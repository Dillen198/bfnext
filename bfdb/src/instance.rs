//! Multi-instance registry.
//!
//! One bfdb process can front several DCS server instances running on the same
//! machine. Everything that used to be a single `--base` / `--stats-jsonl` /
//! `--engine-config` / UDP-export-port becomes a per-instance record here, and
//! every live-engine query, ingestion loop and round in the DB is tagged with
//! the `InstanceId` it belongs to.
//!
//! Configured with `--instances <file.json>`:
//!
//! ```json
//! {
//!   "default": "vs1",
//!   "instances": [
//!     { "id": "vs1", "label": "Vector Strike #1",
//!       "base": "/local/fowl/vs1",
//!       "stats_jsonl": "C:/.../DCS.vectorstrike_1/Logs/stats.jsonl",
//!       "stats_dir":   "C:/.../DCS.vectorstrike_1/Logs/stats",
//!       "engine_config": "C:/.../DCS.vectorstrike_1/ODFv2_CFG",
//!       "export_port": 42001, "srs_url": "http://127.0.0.1:5002",
//!       "dcs_server_name": "[VS] Vector Strike #1" },
//!     { "id": "vs2", "label": "Vector Strike #2",
//!       "base": "/local/fowl/vs2",
//!       "stats_jsonl": "C:/.../DCS.vectorstrike_2/Logs/stats.jsonl",
//!       "stats_dir":   "C:/.../DCS.vectorstrike_2/Logs/stats",
//!       "engine_config": "C:/.../DCS.vectorstrike_2/ODFv2_CFG",
//!       "export_port": 42002, "srs_url": "http://127.0.0.1:5003",
//!       "dcs_server_name": "[VS] Vector Strike #2" }
//!   ]
//! }
//! ```
//!
//! Without `--instances`, the legacy single-server flags synthesize exactly one
//! instance whose id is [`DEFAULT_INSTANCE`] -- so an existing deployment (and
//! its existing Sled DB, whose rounds carry no instance tag) keeps working
//! unchanged. See `deploy/multi-instance.md`.
use anyhow::{bail, Context, Result};
use netidx::path::Path as NetidxPath;
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    sync::Arc,
};

/// The instance id assumed for data written before multi-instance support, and
/// for a bfdb started with the legacy single-server flags. Rounds in the DB
/// with no `round_instance` entry are treated as belonging to this instance.
pub(crate) const DEFAULT_INSTANCE: &str = "default";

/// Default UDP port the DCS `Export.lua` feed arrives on. Historically
/// hardcoded; now the default for the first instance only -- every additional
/// instance must pick its own (and set the matching `BF_PORT` in its copy of
/// `scripts/Export.lua`).
pub(crate) const DEFAULT_EXPORT_PORT: u16 = 42001;

pub(crate) type InstanceId = Arc<str>;

/// Static, startup-time description of one DCS server instance.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct InstanceCfg {
    /// Stable, short, URL-safe key (`?instance=<id>`). Never change it once a
    /// campaign has run under it -- rounds in the DB are tagged with this.
    pub id: String,
    /// Human-readable name for the dashboard's instance selector. Falls back
    /// to `id`.
    #[serde(default)]
    pub label: Option<String>,
    /// `netidx_base` from this instance's engine CFG. bflib publishes under
    /// `<base>/<sortie>`; two instances MUST NOT share a base.
    #[serde(default)]
    pub base: Option<NetidxPath>,
    /// Pin the live engine subscriptions to this sortie instead of learning it
    /// from the stats stream (the old global `--sortie`, per instance now).
    #[serde(default)]
    pub sortie: Option<String>,
    /// This instance's `Logs/stats.jsonl`.
    #[serde(default)]
    pub stats_jsonl: Option<PathBuf>,
    /// This instance's `Logs/stats` netidx-archive directory.
    #[serde(default)]
    pub stats_dir: Option<PathBuf>,
    /// UDP port this instance's `Export.lua` sends live unit positions to.
    #[serde(default)]
    pub export_port: Option<u16>,
    /// This instance's engine CFG json, for the admin config editor.
    #[serde(default)]
    pub engine_config: Option<PathBuf>,
    /// SRS client list (URL to proxy, or a local CLIENT_EXPORT_FILE_PATH).
    #[serde(default)]
    pub srs_url: Option<String>,
    /// Live GCI config json for this instance. Each instance needs its own
    /// (its own SRS port, frequencies and callsigns).
    #[serde(default)]
    pub gci_config: Option<PathBuf>,
    /// The DCSServerBot server name this instance corresponds to. Purely
    /// informational to bfdb; the fowlengine plugin uses it to map a Discord
    /// command on a given DCS server to the right `?instance=`.
    #[serde(default)]
    pub dcs_server_name: Option<String>,
    /// Whether this instance is part of the *public* picture. Default true.
    ///
    /// A `false` instance -- a test/staging server -- is:
    ///   * left out of `GET /api/instances` for anyone who isn't a dashboard
    ///     admin, so it never appears in the public server selector, and
    ///   * excluded from the all-time pilot totals: the leaderboard, a pilot's
    ///     lifetime profile, and `/api/stats` global counters are computed as
    ///     if its rounds did not exist, so messing about on the test server
    ///     cannot inflate anyone's record.
    ///
    /// Its rounds are still recorded normally and an admin can select it and
    /// use every per-instance view (TACMAP, objectives, engine log, commander).
    ///
    /// This is a visibility and accounting rule, NOT an authorization boundary:
    /// a caller who knows the id can still read that instance's per-round data
    /// on the public routes. The routes that actually expose sensitive things
    /// (`/ws/units`, `/api/admin/*`, `/api/commander/*`, the coalition-locked
    /// intel and briefing) keep their own admin/coalition gates regardless.
    #[serde(default = "default_true")]
    pub public: bool,
}

fn default_true() -> bool {
    true
}

impl InstanceCfg {
    pub(crate) fn label(&self) -> &str {
        self.label.as_deref().unwrap_or(&self.id)
    }
}

/// On-disk shape of `--instances <file.json>`.
#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct InstancesFile {
    /// Which instance answers a request that names none. Defaults to the first
    /// entry in `instances`.
    #[serde(default)]
    pub default: Option<String>,
    pub instances: Vec<InstanceCfg>,
}

/// The resolved set of instances this bfdb fronts, in configured order.
#[derive(Debug, Clone)]
pub(crate) struct Registry {
    instances: Vec<Arc<InstanceCfg>>,
    default: InstanceId,
}

impl Registry {
    /// Build from a parsed `--instances` file, validating the invariants that
    /// silently corrupt data if broken: duplicate ids, shared netidx bases,
    /// shared stats files, and shared UDP export ports.
    pub(crate) fn new(file: InstancesFile) -> Result<Self> {
        if file.instances.is_empty() {
            bail!("instances file lists no instances");
        }
        let mut ids: HashSet<String> = HashSet::new();
        let mut bases: HashSet<String> = HashSet::new();
        let mut ports: HashSet<u16> = HashSet::new();
        let mut jsonls: HashSet<PathBuf> = HashSet::new();
        for i in &file.instances {
            if i.id.is_empty() {
                bail!("an instance has an empty id");
            }
            if !i
                .id
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
            {
                bail!(
                    "instance id {:?} must be ASCII alphanumeric, '-' or '_' (it appears in URLs and DB keys)",
                    i.id
                );
            }
            if !ids.insert(i.id.clone()) {
                bail!("duplicate instance id {:?}", i.id);
            }
            if let Some(b) = &i.base {
                if !bases.insert(format!("{b}")) {
                    bail!(
                        "instance {:?} reuses netidx base {b} -- each DCS instance needs its own \
                         `netidx_base` in its engine CFG, or their stats and RPCs will cross",
                        i.id
                    );
                }
            }
            if let Some(p) = i.export_port {
                if !ports.insert(p) {
                    bail!(
                        "instance {:?} reuses UDP export port {p} -- give each instance its own \
                         port and set the matching BF_PORT in its Export.lua",
                        i.id
                    );
                }
            }
            if let Some(p) = &i.stats_jsonl {
                if !jsonls.insert(p.clone()) {
                    bail!(
                        "instance {:?} reuses stats.jsonl {} -- each instance writes its own",
                        i.id,
                        p.display()
                    );
                }
            }
        }
        let default: InstanceId = match &file.default {
            Some(d) => {
                if !ids.contains(d.as_str()) {
                    bail!("default instance {d:?} is not in the instances list");
                }
                Arc::from(d.as_str())
            }
            None => Arc::from(file.instances[0].id.as_str()),
        };
        Ok(Self {
            instances: file.instances.into_iter().map(Arc::new).collect(),
            default,
        })
    }

    /// Load and validate `--instances <path>`.
    pub(crate) fn load(path: &Path) -> Result<Self> {
        let txt = std::fs::read_to_string(path)
            .with_context(|| format!("reading instances file {}", path.display()))?;
        let file: InstancesFile = serde_json::from_str(&txt)
            .with_context(|| format!("parsing instances file {}", path.display()))?;
        Self::new(file)
    }

    /// The legacy single-server shape: one instance built from the flat CLI
    /// flags, so an existing deployment is untouched.
    pub(crate) fn single(cfg: InstanceCfg) -> Self {
        let default: InstanceId = Arc::from(cfg.id.as_str());
        Self {
            instances: vec![Arc::new(cfg)],
            default,
        }
    }

    pub(crate) fn all(&self) -> &[Arc<InstanceCfg>] {
        &self.instances
    }

    /// True when at least one instance is non-public, i.e. the all-time stats
    /// have to be filtered. Lets the common all-public case keep using the
    /// pre-aggregated pilot totals instead of re-summing per round.
    pub(crate) fn has_private(&self) -> bool {
        self.instances.iter().any(|i| !i.public)
    }

    /// True when this bfdb fronts exactly one instance -- the legacy shape,
    /// where `?instance=` can be ignored entirely.
    pub(crate) fn is_single(&self) -> bool {
        self.instances.len() == 1
    }

    pub(crate) fn default_id(&self) -> &InstanceId {
        &self.default
    }

    pub(crate) fn get(&self, id: &str) -> Option<&Arc<InstanceCfg>> {
        self.instances.iter().find(|i| i.id == id)
    }

    /// Resolve a `?instance=` query value. `None` (or an empty string) picks
    /// the default instance; an unknown id is an error rather than a silent
    /// fallback, so a stale bookmark doesn't quietly show the wrong server.
    ///
    /// `"all"` also resolves to the default instance. It is not a real
    /// instance -- it is the aggregate mode of the handful of routes that
    /// support one (`/api/rounds`), and those read the raw query themselves.
    /// Resolving it here rather than erroring keeps `with_instance` usable on
    /// those routes without a second, separate filter.
    pub(crate) fn resolve(&self, requested: Option<&str>) -> Result<&Arc<InstanceCfg>> {
        match requested.map(str::trim).filter(|s| !s.is_empty() && *s != "all") {
            None => self
                .get(&self.default)
                .ok_or_else(|| anyhow::anyhow!("default instance missing")),
            Some(id) => self
                .get(id)
                .ok_or_else(|| anyhow::anyhow!("unknown instance {id:?}")),
        }
    }

    /// Find the instance that owns a DCSServerBot server name.
    pub(crate) fn by_dcs_server_name(&self, name: &str) -> Option<&Arc<InstanceCfg>> {
        self.instances
            .iter()
            .find(|i| i.dcs_server_name.as_deref() == Some(name))
    }
}
