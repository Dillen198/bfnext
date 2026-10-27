//! The voice and LLM worker (f120 part 2, docs/plans/bfdb-process-split.md).
//!
//! By default nothing here changes anything: bfdb runs GCI, campaign ATC,
//! the range's military ATC, the HQ strategist and the war diary's model
//! calls in its own process, as it always has.
//!
//! With `--worker-mode external` bfdb starts none of them. A second process,
//! the same exe started as `bfdb --worker`, runs them instead and connects to
//! bfdb's loopback worker listener (`--worker-listen`, a WebSocket guarded by
//! a shared secret). Over that one link:
//!
//! - the worker's engine calls go to bfdb, which makes them with its own
//!   subscriptions and breaker ([`host`]);
//! - GCI transcript lines and the range ATC live feed come back and are
//!   served from bfdb's routes as before ([`hub`]);
//! - bfdb asks the worker for GCI transmissions from the dashboard, the range
//!   ATC transcript and audio, the diary's prose and its pictures.
//!
//! The worker never opens the database (sled takes an exclusive lock) and
//! never listens on a port. A voice fault, a stuck model call or a misbehaving
//! opus.dll takes down the worker, not the API; and the LLM, image and voice
//! keys need only be given to the worker.

pub(crate) mod client;
pub(crate) mod host;
pub(crate) mod hub;
pub(crate) mod link;

use crate::{gci::GciConfig, instance::InstanceCfg, LogHistory};
use std::{net::SocketAddr, path::PathBuf};
use tokio::sync::broadcast;

/// Where the voice and model services run.
#[derive(clap::ValueEnum, Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum WorkerMode {
    /// In bfdb itself (the default, and how bfdb always ran).
    #[default]
    InProcess,
    /// In a separate `bfdb --worker` process.
    External,
}

#[derive(clap::Args, Debug, Clone)]
pub(crate) struct WorkerArgs {
    /// Where GCI, ATC, the range ATC, the HQ strategist and the war diary's
    /// model calls run: `in-process` (default) or `external`, in a separate
    /// `bfdb --worker` process that connects to --worker-listen.
    #[arg(long = "worker-mode", value_enum, default_value_t = WorkerMode::InProcess)]
    pub(crate) mode: WorkerMode,

    /// Run as the worker instead of as bfdb: no database, no web server; the
    /// voice and model services, connected to bfdb at --worker-url. Takes
    /// the same --instances / --gci-config / --config / --hq-* / --news-llm-*
    /// / --news-image-* flags bfdb would.
    #[arg(long = "worker")]
    pub(crate) run_worker: bool,

    /// External mode: the loopback address bfdb takes the worker's link on.
    /// Only loopback callers holding the secret are accepted.
    #[arg(long = "worker-listen", default_value = "127.0.0.1:8890")]
    pub(crate) listen: SocketAddr,

    /// The worker: bfdb's worker link.
    #[arg(long = "worker-url", default_value = "ws://127.0.0.1:8890/internal/worker")]
    pub(crate) url: String,

    /// The secret both ends share, from this file (or $BFDB_WORKER_SECRET).
    /// bfdb creates the file with a fresh secret when it is missing. Default
    /// `<db>.worker-secret`, beside the --db path (which the worker is given
    /// for this and never opens).
    #[arg(long = "worker-secret-file")]
    pub(crate) secret_file: Option<PathBuf>,
}

/// The shared secret. bfdb (`create`) makes one when there is none yet; the
/// worker waits for it to appear.
pub(crate) fn secret(
    args: &WorkerArgs,
    db: &std::path::Path,
    create: bool,
) -> anyhow::Result<Option<String>> {
    if let Some(s) = std::env::var("BFDB_WORKER_SECRET")
        .ok()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
    {
        return Ok(Some(s));
    }
    let path = args
        .secret_file
        .clone()
        .unwrap_or_else(|| crate::beside_db(db, ".worker-secret"));
    match std::fs::read_to_string(&path) {
        Ok(s) if !s.trim().is_empty() => Ok(Some(s.trim().to_string())),
        _ if create => {
            let s = format!(
                "{}{}",
                uuid::Uuid::new_v4().simple(),
                uuid::Uuid::new_v4().simple()
            );
            if let Some(dir) = path.parent().filter(|d| !d.as_os_str().is_empty()) {
                std::fs::create_dir_all(dir)?;
            }
            std::fs::write(&path, &s).map_err(|e| {
                anyhow::anyhow!("writing the worker secret to {}: {e}", path.display())
            })?;
            log::info!("worker: new shared secret written to {}", path.display());
            Ok(Some(s))
        }
        _ => Ok(None),
    }
}

/// The GCI config that applies to one instance: its own `gci_config` file,
/// else (for the default instance only, never a range) the box-wide one from
/// --gci-config or campaign.json's `gci` block.
pub(crate) fn gci_config_for(
    cfg: &InstanceCfg,
    is_default: bool,
    global: &Option<GciConfig>,
) -> Option<GciConfig> {
    cfg.gci_config.as_ref().and_then(|p| crate::gci::from_file(p)).or_else(|| {
        if is_default && !cfg.is_range() {
            global.clone()
        } else {
            None
        }
    })
}

/// Start one instance's voice services -- the range's military ATC, or a
/// campaign's GCI and towers -- wherever this process is. Shared by bfdb's
/// in-process mode and the worker so the two can never start different sets.
pub(crate) fn spawn_voice(
    host: host::EngineHost,
    gci: Option<GciConfig>,
    tx: broadcast::Sender<String>,
    hist: LogHistory,
) {
    let cfg = host.cfg.clone();
    match (cfg.base.is_some(), gci) {
        // A range has no GCI picture or campaign towers; its voice service
        // is the military ATC.
        (true, Some(c)) if cfg.is_range() => match c.range_atc.clone() {
            Some(r) if r.enabled => {
                log::info!("[{}] range ATC enabled", cfg.id);
                tokio::spawn(crate::milatc::run(host, c, r));
            }
            _ => log::info!("[{}] range instance has a gci_config but no rangeAtc block", cfg.id),
        },
        (true, Some(c)) => {
            log::info!("[{}] live GCI enabled", cfg.id);
            // ATC rides the same config file and the same voice bus, on its
            // own SRS clients so a busy tower can never delay a threat call
            // on the GCI net.
            if let Some(atc_cfg) = c.atc.clone() {
                let atc_transcript: crate::atc::AtcTranscript = Default::default();
                tokio::spawn(crate::atc::run(host.clone(), c.clone(), atc_cfg, atc_transcript));
            }
            tokio::spawn(crate::gci::run(host, c, tx, hist));
        }
        (false, Some(_)) => log::warn!(
            "[{}] GCI configured but disabled: this instance has no netidx base (no live engine to query)",
            cfg.id
        ),
        _ => {}
    }
}

/// The war diary's writer as `news_generator` sees it on one tick.
#[derive(Clone)]
pub(crate) enum DiaryWriter {
    Local(crate::news_llm::WriterCfg),
    /// The worker's, by model name.
    Worker {
        model: String,
    },
}

impl DiaryWriter {
    pub(crate) fn model(&self) -> &str {
        match self {
            DiaryWriter::Local(w) => &w.model,
            DiaryWriter::Worker { model } => model,
        }
    }

    /// `news_llm::write_dispatch`, here or in the worker. Blocking.
    pub(crate) fn write(
        &self,
        d: &crate::news::NewsDigest,
        history: &[crate::news::NewsDigest],
    ) -> anyhow::Result<crate::news_llm::Written> {
        match self {
            DiaryWriter::Local(w) => crate::news_llm::write_dispatch(w, d, history),
            DiaryWriter::Worker { .. } => hub::news_write(d, history),
        }
    }
}

/// Where the diary's writer comes from.
#[derive(Clone)]
pub(crate) enum DiarySource {
    /// bfdb's own flags; `None` is the template bank.
    Local(Option<crate::news_llm::WriterCfg>),
    /// External mode: whatever writer the worker last said it has. Kept
    /// while the worker is away, so a restart costs a failed call (which
    /// keeps the day's prose), not a day filed from templates.
    Worker,
}

impl DiarySource {
    /// Whether the writer is known yet. In external mode it is not until the
    /// worker has connected once: a pass before that would take "no writer"
    /// for an answer and refile a day's prose from templates.
    pub(crate) fn ready(&self) -> bool {
        match self {
            DiarySource::Local(_) => true,
            DiarySource::Worker => hub::hub().map_or(false, |h| h.seen()),
        }
    }

    pub(crate) fn current(&self) -> Option<DiaryWriter> {
        match self {
            DiarySource::Local(w) => w.clone().map(DiaryWriter::Local),
            DiarySource::Worker => hub::hub()
                .and_then(|h| h.writer_model())
                .map(|model| DiaryWriter::Worker { model }),
        }
    }
}
