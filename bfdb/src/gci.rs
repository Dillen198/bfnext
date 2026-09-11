// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `gci` module. See the repository NOTICE file.

//! Live GCI — proactive AWACS-style callouts over SRS.
//!
//! Phase 1: broadcast only. A background task polls the engine's `query-gci`
//! RPC (one call per coalition, every few seconds), diffs each flight's hostile
//! BRAA picture against a per-player ledger, and turns the changes into spoken
//! NATO-brevity calls — THREAT, SAM threat, new group, aspect/range updates,
//! MERGED, FADED and a periodic PICTURE / "picture clean". Calls are addressed
//! to the player's DCS flight callsign, prefixed with the coalition
//! controller's callsign on the first contact.
//!
//! Voice is carried by two persistent SRS clients (one per coalition, see
//! [`srs`]) that stay connected — visible in the SRS client list — and
//! authenticate via External AWACS Mode. TTS ([`tts`]) is Piper or Windows
//! SAPI; Opus encoding uses the `opus.dll` that ships with DCS-SRS.
//!
//! Units are per-player: the engine reports each flight's explicit
//! metric/imperial choice (set in-game via `-gci metric` or F10 → EWR → GCI
//! Voice); `None` means "use the server default" from config.
//!
//! The picture is strict fog of war: the engine only reports contacts the
//! player's own coalition sensors are painting. AI flights are never
//! controlled — the engine only lists human-occupied slots.
//!
//! Later phases (also here): extended broadcast calls (splash / chute /
//! tumbleweed / support / split / converge), speech recognition so players can
//! *request* BOGEY DOPE / PICTURE / DECLARE / COMMIT ([`stt`]), GCI-flown
//! intercept vectoring after a commit, coalition-wide group naming, and ID
//! ripening (bogey → bandit → hostile).

use crate::db::StatsDb;
use crate::voice::{radios_from, srs, stt, tts, FreqSpec, Radio};
use bfprotocols::gci::{
    GciAspect, GciContact, GciControlPicture, GciFlight, GciRef, GciSamThreat, GciUnits,
};
use dcso3::coalition::Side;
use serde::Deserialize;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::PathBuf,
    sync::{Arc, Mutex, RwLock},
    time::{Duration, Instant},
};
use tokio::sync::{broadcast, mpsc};

/// Rolling transcript for the dashboard (`/api/gci/transcript`, `/ws/gci`).
pub(crate) type Transcript = Arc<Mutex<VecDeque<String>>>;
const TRANSCRIPT_CAP: usize = 200;

/// Latest `query-gci` picture per side, shared with the speech-recognition
/// worker so it can answer requests against the same data the broadcast uses.
type PictureCache = Arc<RwLock<HashMap<&'static str, GciControlPicture>>>;

// ─── Config ────────────────────────────────────────────────────────────────

fn d_srs_host() -> String {
    "127.0.0.1".into()
}
fn d_srs_port() -> u16 {
    5002
}
fn d_modulation() -> String {
    "AM".into()
}
fn d_blue_cs() -> String {
    "Magic".into()
}
fn d_red_cs() -> String {
    "Overlord".into()
}
fn d_units() -> String {
    "imperial".into()
}
fn d_reference() -> String {
    "braa".into()
}
fn d_true() -> bool {
    true
}
fn d_threat_range() -> u32 {
    40
}
fn d_cooldown() -> u64 {
    25
}
fn d_picture_interval() -> u64 {
    300
}
fn d_poll() -> u64 {
    5
}

/// `gci` block / `gci.json` file. Only `blueFreqMhz` and `redFreqMhz` are
/// strictly required; on an External AWACS Mode server you also need
/// `blueEamPassword` / `redEamPassword`.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct GciConfig {
    /// SRS server host. TCP control + UDP voice both use `srsPort`.
    #[serde(default = "d_srs_host")]
    pub srs_host: String,
    #[serde(default = "d_srs_port")]
    pub srs_port: u16,
    /// Legacy single-frequency form. Still honoured; `blueFreqs`/`redFreqs`
    /// win when present.
    #[serde(default)]
    pub blue_freq_mhz: f64,
    #[serde(default)]
    pub red_freq_mhz: f64,
    /// Every frequency the controller transmits on, per side — a UHF channel,
    /// a VHF channel and an FM combat net can all be listed, and one
    /// transmission is heard on all of them. Modulation defaults to FM below
    /// 108 MHz and AM above.
    ///
    /// ```json
    /// "blueFreqs": [
    ///   { "mhz": 251.0, "modulation": "AM" },
    ///   { "mhz": 124.0, "modulation": "AM" },
    ///   { "mhz": 30.0,  "modulation": "FM" }
    /// ]
    /// ```
    #[serde(default)]
    pub blue_freqs: Vec<FreqSpec>,
    #[serde(default)]
    pub red_freqs: Vec<FreqSpec>,
    #[serde(default = "d_modulation")]
    pub modulation: String,
    /// External AWACS Mode coalition passwords. Leave blank on a server that
    /// does not require EAM auth.
    #[serde(default)]
    pub blue_eam_password: String,
    #[serde(default)]
    pub red_eam_password: String,
    /// Explicit path to `opus.dll` (ships with DCS-SRS). If unset, bfdb looks
    /// for `opus.dll` on the standard search path / next to `bfdb.exe`.
    #[serde(default)]
    pub opus_dll_path: Option<PathBuf>,
    /// Piper TTS binary (shared by both sides). With a per-side model set,
    /// calls use the Piper neural voice; otherwise Windows SAPI is used.
    #[serde(default)]
    pub piper_exe: Option<PathBuf>,
    /// Per-side Piper voice model (`.onnx`). Falls back to `piperModel`.
    /// Point `redPiperModel` at a Russian-accent model for red-side calls.
    #[serde(default)]
    pub piper_model: Option<PathBuf>,
    #[serde(default)]
    pub blue_piper_model: Option<PathBuf>,
    #[serde(default)]
    pub red_piper_model: Option<PathBuf>,
    /// Per-side named SAPI voice (used only when Piper is not configured for
    /// that side). Falls back to `ttsVoice`.
    #[serde(default)]
    pub tts_voice: Option<String>,
    #[serde(default)]
    pub blue_tts_voice: Option<String>,
    #[serde(default)]
    pub red_tts_voice: Option<String>,
    /// Phase 3: speech recognition. `whisper-cli.exe` / `main.exe` from a
    /// whisper.cpp release + a ggml model (e.g. `ggml-base.en.bin`). Both set →
    /// players can key up and request BOGEY DOPE / PICTURE / DECLARE / etc.
    #[serde(default)]
    pub whisper_exe: Option<PathBuf>,
    #[serde(default)]
    pub whisper_model: Option<PathBuf>,
    /// Base URL of a running `whisper-server` (e.g. `http://127.0.0.1:8910`).
    /// Preferred over `whisperExe`/`whisperModel` when set — the model stays
    /// resident instead of being reloaded for every transmission, which matters
    /// as soon as more than one position is listening.
    #[serde(default)]
    pub whisper_server_url: Option<String>,
    /// Spoken air traffic control and ATIS. Shares this file's SRS, TTS and
    /// whisper settings; omit the block to leave ATC off.
    #[serde(default)]
    pub atc: Option<crate::atc::AtcConfig>,
    /// Discord webhook URL. When set, every GCI call is also posted there as a
    /// text transcript, both coalitions in one channel, tagged 🔵/🔴. Create it
    /// in a channel's Integrations → Webhooks.
    #[serde(default)]
    pub discord_webhook_url: Option<String>,
    /// Split the transcript across two channels instead. When either of these
    /// is set that side posts here and not to `discordWebhookUrl`, which keeps
    /// working as the fallback for a side without its own webhook — so you can
    /// run one channel, two channels, or one channel per coalition with a
    /// shared overflow.
    #[serde(default)]
    pub blue_discord_webhook_url: Option<String>,
    #[serde(default)]
    pub red_discord_webhook_url: Option<String>,
    /// Prefix every Discord line with the DCS server's name. Defaults to on
    /// when bfdb is fronting more than one instance, so two servers posting
    /// into one channel stay tellable apart.
    #[serde(default)]
    pub discord_tag_instance: Option<bool>,
    /// Spoken controller callsign + SRS client-list name, per side.
    #[serde(default = "d_blue_cs")]
    pub blue_controller_callsign: String,
    #[serde(default = "d_red_cs")]
    pub red_controller_callsign: String,
    /// Spoken name for the bullseye reference point, per side — a mission
    /// codeword (e.g. "WHISKEY", "DALLAS") when the briefing gives one. Blank
    /// → the literal word "bullseye".
    #[serde(default)]
    pub blue_bullseye_name: Option<String>,
    #[serde(default)]
    pub red_bullseye_name: Option<String>,
    /// Default spoken units when a player has not chosen — "imperial" or "metric".
    #[serde(default = "d_units")]
    pub units: String,
    /// Default position reference when a player has not chosen — "braa" (from
    /// the flight's own jet) or "bullseye".
    #[serde(default = "d_reference")]
    pub reference: String,
    /// Announce live enemy SAM threats covering a flight.
    #[serde(default = "d_true")]
    pub sam_threat_calls: bool,
    /// "Splash" when a hostile a flight was warned about is killed.
    #[serde(default = "d_true")]
    pub splash_calls: bool,
    /// "Chute observed" on a friendly ejection (CSAR cue).
    #[serde(default = "d_true")]
    pub chute_calls: bool,
    /// "Tumbleweed" when the coalition's radar net goes down.
    #[serde(default = "d_true")]
    pub tumbleweed_calls: bool,
    /// Periodic friendly tanker / AWACS location broadcast.
    #[serde(default = "d_true")]
    pub support_calls: bool,
    /// "New tasking" broadcast when a player posts a task (CAP / CAS /
    /// CAPTURE / ...) to the coalition tasking board in game.
    #[serde(default = "d_true")]
    pub tasking_calls: bool,
    #[serde(default = "d_support_interval")]
    pub support_interval_secs: u64,
    /// A hostile group beyond this range is not called as "new" (it still
    /// counts for THREAT once it closes).
    #[serde(default = "d_threat_range")]
    pub threat_range_nm: u32,
    /// Minimum seconds between calls to one flight. THREAT calls ignore it.
    #[serde(default = "d_cooldown")]
    pub per_player_cooldown_secs: u64,
    /// Seconds between unsolicited PICTURE broadcasts to a flight.
    #[serde(default = "d_picture_interval")]
    pub picture_interval_secs: u64,
    /// Seconds between `query-gci` polls per coalition.
    #[serde(default = "d_poll")]
    pub poll_secs: u64,
    /// Minimum quiet time on the frequency between GCI transmissions, so
    /// players have a window to make their own radio calls. Threat / SAM calls
    /// use half this.
    #[serde(default = "d_inter_gap")]
    pub inter_call_gap_secs: u64,
}

fn d_inter_gap() -> u64 {
    6
}
fn d_support_interval() -> u64 {
    240
}

impl GciConfig {
    fn default_units(&self) -> GciUnits {
        if self.units.eq_ignore_ascii_case("metric") {
            GciUnits::Metric
        } else {
            GciUnits::Imperial
        }
    }
    fn controller(&self, side: Side) -> &str {
        match side {
            Side::Red => &self.red_controller_callsign,
            _ => &self.blue_controller_callsign,
        }
    }
    /// Spoken bullseye reference name for a side ("bullseye" unless a codeword
    /// is configured).
    fn bullseye_name(&self, side: Side) -> &str {
        let n = match side {
            Side::Red => self.red_bullseye_name.as_deref(),
            _ => self.blue_bullseye_name.as_deref(),
        };
        n.map(str::trim).filter(|s| !s.is_empty()).unwrap_or("bullseye")
    }
    fn default_reference(&self) -> GciRef {
        if self.reference.eq_ignore_ascii_case("bullseye") || self.reference.eq_ignore_ascii_case("bulls") {
            GciRef::Bullseye
        } else {
            GciRef::Braa
        }
    }
    fn modulation_byte(&self) -> u8 {
        crate::voice::modulation_byte(&self.modulation)
    }

    /// Every frequency a side's controller transmits on.
    fn radios(&self, side: Side) -> Vec<Radio> {
        let (specs, legacy, label) = match side {
            Side::Red => (
                &self.red_freqs,
                self.red_freq_mhz,
                self.red_controller_callsign.as_str(),
            ),
            _ => (
                &self.blue_freqs,
                self.blue_freq_mhz,
                self.blue_controller_callsign.as_str(),
            ),
        };
        radios_from(specs, legacy, self.modulation_byte(), label)
    }
}

/// Pull the `gci` object out of the parsed campaign.json, if present and valid.
pub(crate) fn from_campaign_json(v: &serde_json::Value) -> Option<GciConfig> {
    let raw = v.get("gci")?;
    match serde_json::from_value::<GciConfig>(raw.clone()) {
        Ok(c) => Some(c),
        Err(e) => {
            log::warn!("gci: campaign.json `gci` block is invalid, GCI disabled: {e}");
            None
        }
    }
}

/// Load a dedicated GCI config file (the whole file is the config object).
/// The canonical place to configure GCI — see `gci.sample.json` and the
/// `$gciConfigPath` variable in `bfsystem.ps1`.
pub(crate) fn from_file(path: &std::path::Path) -> Option<GciConfig> {
    let raw = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            log::warn!("gci: could not read --gci-config {path:?}, GCI disabled: {e}");
            return None;
        }
    };
    match serde_json::from_str::<GciConfig>(&raw) {
        Ok(c) => {
            log::info!("gci: config loaded from {path:?}");
            Some(c)
        }
        Err(e) => {
            log::warn!("gci: --gci-config {path:?} is invalid, GCI disabled: {e}");
            None
        }
    }
}

// ─── Public entry point ────────────────────────────────────────────────────

/// Run the GCI poller for one DCS server instance until the process exits.
/// Spawned from `main` only for an instance that has a netidx base (there is a
/// live engine to query) and a `gci` config of its own.
pub(crate) async fn run(
    db: StatsDb,
    inst: crate::Inst,
    cfg: GciConfig,
    transcript_tx: broadcast::Sender<String>,
    transcript: Transcript,
) {
    log::info!(
        "[{}] GCI enabled: blue {} MHz ({}) / red {} MHz ({}) {}, default units {}, poll {}s",
        inst.id,
        cfg.blue_freq_mhz,
        cfg.blue_controller_callsign,
        cfg.red_freq_mhz,
        cfg.red_controller_callsign,
        cfg.modulation,
        cfg.units,
        cfg.poll_secs,
    );

    let discord = DiscordRoute::build(&cfg, &inst, db.instances().all().len() > 1);
    discord.log();

    let (tx, rx) = mpsc::channel::<QueuedCall>(64);

    // Phase 3: speech recognition. When configured, each persistent SRS client
    // gets a sender for completed inbound transmissions, and a worker turns
    // them into answers.
    let picture_cache: PictureCache = Default::default();
    let stt = stt::Stt::from_cfg(
        nonempty_path(&cfg.whisper_exe).as_ref(),
        nonempty_path(&cfg.whisper_model).as_ref(),
        cfg.whisper_server_url.as_deref(),
    );
    // Prime the recogniser with the names it will actually hear. The controller
    // callsigns are the wake words — getting those decoded is most of the job.
    if let Some(s) = stt.as_ref() {
        s.set_vocabulary(&[
            cfg.blue_controller_callsign.clone(),
            cfg.red_controller_callsign.clone(),
            cfg.bullseye_name(Side::Blue).to_string(),
            cfg.bullseye_name(Side::Red).to_string(),
        ]);
    }
    let (rx_for_srs, rx_recv) = if stt.is_some() {
        let (t, r) = tokio::sync::mpsc::unbounded_channel::<srs::Transmission>();
        (Some(t), Some(r))
    } else {
        (None, None)
    };
    let (commit_tx, mut commit_rx) = tokio::sync::mpsc::unbounded_channel::<(Side, String)>();
    if stt.is_none() {
        log::info!(
            "gci: speech recognition disabled (set whisperExe + whisperModel to let              players call '{}' / '{}'); outbound calls still work",
            cfg.blue_controller_callsign,
            cfg.red_controller_callsign,
        );
    }
    if let (Some(stt), Some(rx_recv)) = (stt.clone(), rx_recv) {
        log::info!("gci: speech recognition enabled — {}", stt.describe());
        tokio::spawn(stt_worker(
            stt,
            cfg.clone(),
            picture_cache.clone(),
            tx.clone(),
            commit_tx,
            rx_recv,
        ));
    }

    tokio::spawn(transmit_worker(
        build_voice(&cfg, rx_for_srs),
        discord,
        cfg.inter_call_gap_secs,
        transcript_tx,
        transcript,
        rx,
    ));

    let mut ledger: HashMap<String, PlayerGci> = HashMap::new();
    let mut side_state: HashMap<&'static str, SideState> = HashMap::new();
    let mut tick: u64 = 0;
    let mut engine_ok = true; // start optimistic so the first failure warns once
    let heartbeat_ticks = (60 / cfg.poll_secs.max(1)).max(1);
    let mut poll = tokio::time::interval(Duration::from_secs(cfg.poll_secs.max(1)));
    poll.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);

    loop {
        poll.tick().await;
        tick = tick.wrapping_add(1);

        // Drain commit requests from the speech worker.
        let mut committed: HashMap<String, Side> = HashMap::new();
        while let Ok((s, ucid)) = commit_rx.try_recv() {
            committed.insert(ucid, s);
        }

        let mut any_ok = false;
        let mut last_err: Option<String> = None;
        let mut total_flights = 0usize;
        let mut flights_in_combat = 0usize;

        for (side, side_str) in [(Side::Blue, "blue"), (Side::Red, "red")] {
            let picture = match fetch_picture(&db, &inst, side_str).await {
                Ok(p) => {
                    any_ok = true;
                    p
                }
                Err(e) => {
                    last_err = Some(e);
                    continue;
                }
            };
            if let Ok(mut c) = picture_cache.write() {
                c.insert(side_str, picture.clone());
            }
            let be = picture.bullseye;
            let named = name_side_groups(&picture);
            let seen: Vec<&str> = picture.flights.iter().map(|f| f.ucid.as_str()).collect();
            total_flights += picture.flights.len();
            let mut nonurgent_this_side = 0u8;
            for flight in &picture.flights {
                if !flight.contacts.is_empty() || !flight.sam_threats.is_empty() {
                    flights_in_combat += 1;
                    log::info!(
                        "gci: {} sees {} group(s): {}",
                        flight.player_name,
                        flight.contacts.len(),
                        flight
                            .contacts
                            .iter()
                            .map(|c| format!(
                                "{}nm {}x {}",
                                c.rng_m / 1852,
                                c.group_size,
                                c.type_name.as_deref().unwrap_or("unknown-type")
                            ))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
                // Honor a fresh COMMIT for this flight.
                if committed.get(&flight.ucid) == Some(&side) {
                    if let Some(pg) = ledger.get_mut(&flight.ucid) {
                        if let Some(c) = flight.contacts.first() {
                            pg.intercept = Some(Intercept {
                                key: group_key(c),
                                last_vector: None,
                                last_hdg: c.brg,
                                merged: false,
                            });
                        }
                    }
                }
                if let Some((urgent, text)) =
                    decide(&mut ledger, &cfg, side, flight, be, &named, tick)
                {
                    // Multi-flight deconfliction: at most 2 non-urgent GCI
                    // transmissions queued per side per tick.
                    if !urgent {
                        nonurgent_this_side += 1;
                        if nonurgent_this_side > 2 {
                            continue;
                        }
                    }
                    let _ = tx.try_send(QueuedCall {
                        queued: Instant::now(),
                        side,
                        urgent,
                        text,
                    });
                }
            }

            // Coalition-wide calls: tumbleweed, chute, support location.
            let ss = side_state.entry(side_str).or_default();
            for (urgent, text) in coalition_calls(ss, &cfg, side, &picture, tick) {
                let _ = tx.try_send(QueuedCall {
                    queued: Instant::now(),
                    side,
                    urgent,
                    text,
                });
            }

            ledger.retain(|ucid, pg| {
                pg.side != side
                    || seen.contains(&ucid.as_str())
                    || tick.wrapping_sub(pg.last_seen_tick) < 120
            });
        }

        // Surface engine reachability at default log level — GCI shares the
        // `query-*` RPC channel, so if this is down, so is everything else.
        if any_ok && !engine_ok {
            engine_ok = true;
            log::info!("gci: engine reachable again");
        } else if !any_ok && engine_ok {
            engine_ok = false;
            log::warn!(
                "gci: engine unreachable, no calls until query-gci works ({})",
                last_err.as_deref().unwrap_or("unknown")
            );
        }
        if tick % heartbeat_ticks == 0 {
            if engine_ok {
                log::info!(
                    "gci: alive — {total_flights} airborne flight(s), {flights_in_combat} with contacts"
                );
            } else {
                // Keep saying it — a one-time warning is easy to miss when the
                // RPC channel is down for a whole mission.
                log::warn!(
                    "gci: still waiting on the engine — query-gci not answering ({}). \
                     Check the netidx resolver / bfdb --base; GCI makes no calls until this clears.",
                    last_err.as_deref().unwrap_or("unknown")
                );
            }
        }
    }
}

async fn fetch_picture(
    db: &StatsDb,
    inst: &crate::db::InstanceState,
    side: &'static str,
) -> Result<GciControlPicture, String> {
    // Be generous: on a loaded engine the RPC handler runs on the DCS
    // scripting thread and can lag well past a few seconds. A missed poll is
    // cheaper than a spurious "unreachable".
    let res = tokio::time::timeout(
        Duration::from_secs(GCI_RPC_TIMEOUT_SECS),
        crate::call_engine_rpc_str(
            db,
            inst,
            "query-gci",
            vec![("side", netidx::publisher::Value::from(side))],
        ),
    )
    .await;
    match res {
        Ok(Ok(json)) => serde_json::from_str::<GciControlPicture>(&json)
            .map_err(|e| format!("bad query-gci JSON: {e}")),
        Ok(Err(e)) => Err(format!("query-gci RPC error: {e:?}")),
        Err(_) => Err(format!(
            "query-gci timed out after {GCI_RPC_TIMEOUT_SECS}s (engine unreachable or overloaded)"
        )),
    }
}

const GCI_RPC_TIMEOUT_SECS: u64 = 8;

// ─── Per-player ledger + decision ──────────────────────────────────────────

#[derive(Clone, Copy)]
struct Tracked {
    brg: u16,
    rng_m: u32,
    aspect: GciAspect,
    alt_band: i32,
    size: u8,
    threat: bool,
    missing_ticks: u8,
    /// Poll tick this group was first held — drives ID ripening
    /// (bogey → bandit → hostile).
    first_tick: u64,
}

/// How long a group has been held, mapped to the spoken identity.
#[derive(Clone, Copy, PartialEq)]
enum Maturity {
    /// Just appeared — "bogey", no type.
    Bogey,
    /// Held a little while — "bandit" + type.
    Bandit,
    /// Established track — "hostile" + type.
    Hostile,
}

impl Maturity {
    /// `held_secs` = (now − first_tick) × poll interval.
    fn from_secs(held_secs: u64) -> Self {
        match held_secs {
            0..=14 => Maturity::Bogey,
            15..=44 => Maturity::Bandit,
            _ => Maturity::Hostile,
        }
    }
}

/// A hostile group given a coalition-wide name ("north group") so every flight
/// that sees it hears the same label. Absolute position of the cluster centroid.
struct NamedGroup {
    lat: f64,
    lon: f64,
    name: &'static str,
}

#[derive(Default)]
struct SideState {
    radar_up: bool,
    last_support: Option<Instant>,
    ejections: HashMap<(i64, i64), u64>,
    last_tumbleweed_tick: u64,
    /// Tasking board entries already read out, so each is spoken once. The
    /// engine keeps a posted task in the picture for a couple of minutes.
    tasks_called: HashSet<i64>,
    initialized: bool,
}

/// Phase 4: an active GCI-controlled intercept for one flight.
struct Intercept {
    /// The hostile group we are steering the flight onto.
    key: (u16, u16),
    last_vector: Option<Instant>,
    last_hdg: u16,
    merged: bool,
}

struct PlayerGci {
    side: Side,
    last_tx: Option<Instant>,
    last_picture: Option<Instant>,
    last_seen_tick: u64,
    groups: HashMap<(u16, u16), Tracked>,
    /// SAM threats already called, bucketed by (bearing/15, range/10km),
    /// with the tick they were last seen so they can time out.
    sams: HashMap<(u16, u16), u64>,
    /// SAM launches already called, same bucketing.
    launches: HashMap<(u16, u16), u64>,
    /// Splashes already called, same bucketing.
    splashes: HashMap<(u16, u16), u64>,
    /// Clustered-contact count last tick, for split / converge calls.
    prev_contacts: usize,
    /// Phase 4: active intercept, if the pilot committed.
    intercept: Option<Intercept>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
enum Prio {
    SamLaunch,
    Threat,
    SamThreat,
    Splash,
    Vector,
    NewGroup,
    Update,
    Merged,
    Faded,
    Picture,
}

fn group_key(c: &GciContact) -> (u16, u16) {
    (c.brg / 15, (c.rng_m / 5000) as u16)
}

/// nm from metres.
fn nm(m: u32) -> u32 {
    ((m as f64) / 1852.0).round() as u32
}

fn is_threat(c: &GciContact) -> bool {
    let r = nm(c.rng_m);
    r <= 5 || (r <= 10 && matches!(c.aspect, GciAspect::Hot | GciAspect::Flank))
}

/// Keep the highest-priority (lowest `Prio`) call seen so far.
fn consider(best: &mut Option<(Prio, String)>, prio: Prio, text: String) {
    if best.as_ref().map_or(true, |(p, _)| prio < *p) {
        *best = Some((prio, text));
    }
}

/// Coalition-wide broadcast calls (not tied to one flight): tumbleweed, chute,
/// periodic support location. Addressed to "all players".
fn coalition_calls(
    ss: &mut SideState,
    cfg: &GciConfig,
    side: Side,
    picture: &GciControlPicture,
    tick: u64,
) -> Vec<(bool, String)> {
    let mut out: Vec<(bool, String)> = vec![];
    let ctrl = cfg.controller(side);
    let units = cfg.default_units();

    // "<bullseye name> <brg> <range>" from the coalition bullseye to a lat/lon.
    let bname = cfg.bullseye_name(side);
    let bulls = |lat: f64, lon: f64| -> String {
        match picture.bullseye {
            Some((blat, blon)) => {
                let (b, d) = bearing_range(blat, blon, lat, lon);
                format!("{bname} {} {}", digit_string(b as u16), range_phrase(d as u32, units))
            }
            None => format!("{bname} unknown"),
        }
    };

    // TUMBLEWEED — radar net just went down.
    if cfg.tumbleweed_calls && ss.initialized {
        if ss.radar_up && !picture.radar_up && tick.wrapping_sub(ss.last_tumbleweed_tick) > 24 {
            ss.last_tumbleweed_tick = tick;
            out.push((false, format!("all players, {ctrl}, tumbleweed, negative radar")));
        }
    }
    ss.radar_up = picture.radar_up;

    // CHUTE — friendly ejection(s).
    if cfg.chute_calls {
        ss.ejections.retain(|_, seen| tick.wrapping_sub(*seen) < 24);
        for (elat, elon) in &picture.ejections {
            let k = ((elat * 50.0) as i64, (elon * 50.0) as i64);
            if ss.ejections.insert(k, tick).is_none() {
                out.push((false, format!("all players, {ctrl}, chute observed, {}", bulls(*elat, *elon))));
            }
        }
    }

    // SUPPORT — periodic tanker / AWACS location.
    if cfg.support_calls && !picture.support.is_empty() {
        let due = ss
            .last_support
            .map_or(true, |t| t.elapsed().as_secs() >= cfg.support_interval_secs);
        if due {
            ss.last_support = Some(Instant::now());
            let mut s = format!("all players, {ctrl}, support.");
            for a in picture.support.iter().take(3) {
                let cs = a.callsign.as_deref().unwrap_or_else(|| {
                    if a.kind == "tanker" { "tanker" } else { "AWACS" }
                });
                s.push_str(&format!(
                    " {}, {}, {}.",
                    cs,
                    bulls(a.lat, a.lon),
                    altitude_phrase(a.alt_m, units),
                ));
            }
            out.push((false, s));
        }
    }

    // TASKING -- a player posted a task on the F10 tasking board. Read it
    // out once, to everyone, the way a controller would pass new tasking.
    if cfg.tasking_calls {
        for t in &picture.tasks {
            if ss.tasks_called.insert(t.id) && ss.initialized {
                let mut call = format!(
                    "all players, {ctrl}, new tasking, {}, {}",
                    t.kind.to_lowercase(),
                    bulls(t.lat, t.lon)
                );
                if let Some(by) = t.by.as_ref() {
                    call.push_str(&format!(", requested by {by}"));
                }
                out.push((false, call));
            }
        }
        // A task the engine has stopped advertising is off the board; let
        // its id go so a re-post is called again.
        let live: HashSet<i64> = picture.tasks.iter().map(|t| t.id).collect();
        ss.tasks_called.retain(|id| live.contains(id));
    }

    ss.initialized = true;
    out
}

/// Cluster every hostile contact the whole coalition is painting and give each
/// cluster a shared cardinal name, so multiple flights near the same groups
/// hear one consistent picture ("north group" / "south group") instead of each
/// getting an independent "single group". Returns empty when there is only one
/// cluster (nothing to disambiguate) or more than three (too busy to name).
fn name_side_groups(picture: &GciControlPicture) -> Vec<NamedGroup> {
    let mut pts: Vec<(f64, f64)> = vec![];
    for f in &picture.flights {
        for c in &f.contacts {
            pts.push(project(f.lat, f.lon, c.brg as f64, c.rng_m as f64));
        }
    }
    if pts.len() < 2 {
        return vec![];
    }
    // Greedy single-link clustering at ~15 nm.
    let mut clusters: Vec<Vec<(f64, f64)>> = vec![];
    for p in pts {
        match clusters.iter_mut().find(|cl| {
            let (_, d) = bearing_range(cl[0].0, cl[0].1, p.0, p.1);
            d < 28_000.0
        }) {
            Some(cl) => cl.push(p),
            None => clusters.push(vec![p]),
        }
    }
    if !(2..=3).contains(&clusters.len()) {
        return vec![];
    }
    let cents: Vec<(f64, f64)> = clusters
        .iter()
        .map(|cl| {
            let n = cl.len() as f64;
            (
                cl.iter().map(|p| p.0).sum::<f64>() / n,
                cl.iter().map(|p| p.1).sum::<f64>() / n,
            )
        })
        .collect();
    // Name along the dominant separation axis.
    let (mut min_la, mut max_la, mut min_lo, mut max_lo) = (f64::MAX, f64::MIN, f64::MAX, f64::MIN);
    for c in &cents {
        min_la = min_la.min(c.0);
        max_la = max_la.max(c.0);
        min_lo = min_lo.min(c.1);
        max_lo = max_lo.max(c.1);
    }
    let ns_axis = (max_la - min_la) >= (max_lo - min_lo) * cents[0].0.to_radians().cos();
    let mut idx: Vec<usize> = (0..cents.len()).collect();
    if ns_axis {
        idx.sort_by(|&a, &b| cents[b].0.total_cmp(&cents[a].0)); // north first
    } else {
        idx.sort_by(|&a, &b| cents[a].1.total_cmp(&cents[b].1)); // west first
    }
    let (first, last, mid): (&str, &str, &str) = if ns_axis {
        ("north group", "south group", "center group")
    } else {
        ("west group", "east group", "center group")
    };
    let n = idx.len();
    idx.iter()
        .enumerate()
        .map(|(rank, &ci)| NamedGroup {
            lat: cents[ci].0,
            lon: cents[ci].1,
            name: if rank == 0 {
                first
            } else if rank == n - 1 {
                last
            } else {
                mid
            },
        })
        .collect()
}

fn decide(
    ledger: &mut HashMap<String, PlayerGci>,
    cfg: &GciConfig,
    side: Side,
    flight: &GciFlight,
    bullseye: Option<(f64, f64)>,
    named: &[NamedGroup],
    tick: u64,
) -> Option<(bool, String)> {
    // This flight runs its own comms — the controller answers when called but
    // never opens. Still tick the ledger below so that turning callouts back on
    // does not dump a backlog of stale contacts; just say nothing.
    let quiet = !flight.auto;
    let controller = cfg.controller(side);
    // Address the flight by pilot name (falling back to flight callsign) — most
    // players don't set a DCS flight callsign.
    let who: &str = if !flight.player_name.is_empty() {
        &flight.player_name
    } else {
        &flight.callsign
    };
    let rc = RCtx {
        units: flight.units.unwrap_or_else(|| cfg.default_units()),
        refm: flight.reference.unwrap_or_else(|| cfg.default_reference()),
        flight,
        be: bullseye,
        bulls_name: cfg.bullseye_name(side),
        groups: named,
    };

    let pg = ledger.entry(flight.ucid.clone()).or_insert_with(|| PlayerGci {
        side,
        last_tx: None,
        last_picture: None,
        last_seen_tick: tick,
        groups: HashMap::new(),
        sams: HashMap::new(),
        launches: HashMap::new(),
        splashes: HashMap::new(),
        prev_contacts: 0,
        intercept: None,
    });
    pg.side = side;
    pg.last_seen_tick = tick;
    for g in pg.groups.values_mut() {
        g.missing_ticks = g.missing_ticks.saturating_add(1);
    }

    let mut best: Option<(Prio, String)> = None;
    let is_first_contact = pg.groups.is_empty();

    let poll = cfg.poll_secs.max(1);
    for c in &flight.contacts {
        let key = group_key(c);
        let alt_band = c.alt_m / 5000;
        let now_threat = is_threat(c);
        let prev = pg.groups.get(&key).copied();
        let first_tick = prev.map_or(tick, |t| t.first_tick);
        let mat = Maturity::from_secs(tick.wrapping_sub(first_tick) * poll);
        match prev {
            None => {
                if nm(c.rng_m) <= cfg.threat_range_nm {
                    if now_threat {
                        consider(
                            &mut best,
                            Prio::Threat,
                            threat_call(controller, who, c, &rc, mat),
                        );
                    } else {
                        let lead = if is_first_contact { Some(controller) } else { None };
                        consider(
                            &mut best,
                            Prio::NewGroup,
                            braa_call(lead, who, c, &rc, mat),
                        );
                    }
                }
            }
            Some(t) => {
                let crossed_in = nm(t.rng_m) >= 10 && nm(c.rng_m) < 10;
                let went_hot = c.aspect == GciAspect::Hot && t.aspect != GciAspect::Hot;
                let went_cold = t.threat
                    && c.aspect == GciAspect::Cold
                    && t.aspect != GciAspect::Cold;
                let fillin_changed = t.size >= 1 && c.group_size >= 2 && c.group_size != t.size;
                // "bogey" that has now ripened to "bandit" — re-declare once.
                let ripened = Maturity::from_secs(tick.wrapping_sub(t.first_tick).saturating_sub(1) * poll)
                    == Maturity::Bogey
                    && mat != Maturity::Bogey;
                if now_threat && !t.threat {
                    consider(
                        &mut best,
                        Prio::Threat,
                        threat_call(controller, who, c, &rc, mat),
                    );
                } else if nm(c.rng_m) < 3 && nm(t.rng_m) >= 3 {
                    consider(
                        &mut best,
                        Prio::Merged,
                        format!("{}, merged", spoken_callsign(who)),
                    );
                } else if went_cold {
                    consider(&mut best, Prio::Update, cold_call(who, c, &rc));
                } else if fillin_changed {
                    consider(&mut best, Prio::Update, fillin_call(who, c, &rc));
                } else if went_hot || crossed_in || alt_band != t.alt_band || ripened {
                    consider(&mut best, Prio::Update, braa_call(None, who, c, &rc, mat));
                }
            }
        }
        pg.groups.insert(
            key,
            Tracked {
                brg: c.brg,
                rng_m: c.rng_m,
                aspect: c.aspect,
                alt_band,
                size: c.group_size,
                threat: now_threat,
                missing_ticks: 0,
                first_tick,
            },
        );
    }

    // FADED: a group we were calling has been absent for two polls.
    let faded: Vec<Tracked> = pg
        .groups
        .values()
        .filter(|t| t.missing_ticks == 2 && nm(t.rng_m) <= 60)
        .copied()
        .collect();
    for t in faded {
        consider(&mut best, Prio::Faded, faded_call(who, &t, &rc));
    }
    pg.groups.retain(|_, t| t.missing_ticks < 4);

    // SAM launches — highest priority, always called.
    pg.launches.retain(|_, seen| tick.wrapping_sub(*seen) < 12);
    for s in &flight.sam_launches {
        let lkey = (s.brg / 20, (s.rng_m / 10000) as u16);
        let fresh = !pg.launches.contains_key(&lkey);
        pg.launches.insert(lkey, tick);
        if fresh {
            consider(&mut best, Prio::SamLaunch, sam_launch_call(controller, who, s, &rc));
        }
    }

    // SPLASH.
    if cfg.splash_calls {
        pg.splashes.retain(|_, seen| tick.wrapping_sub(*seen) < 6);
        for s in &flight.splashes {
            let skey = (s.brg / 20, (s.rng_m / 10000) as u16);
            if pg.splashes.insert(skey, tick).is_none() {
                consider(&mut best, Prio::Splash, splash_call(controller, who, s, &rc));
            }
        }
    }

    // SAM threats (envelope entry).
    if cfg.sam_threat_calls {
        pg.sams.retain(|_, seen| tick.wrapping_sub(*seen) < 24);
        for s in &flight.sam_threats {
            let skey = (s.brg / 15, (s.rng_m / 10000) as u16);
            let fresh = !pg.sams.contains_key(&skey);
            pg.sams.insert(skey, tick);
            if fresh {
                consider(&mut best, Prio::SamThreat, sam_call(controller, who, s, &rc));
            }
        }
    }

    // SPLIT / CONVERGING (clustered-contact count changed).
    let n_now = flight.contacts.len();
    if best.is_none() && pg.prev_contacts >= 1 && n_now >= 1 && n_now != pg.prev_contacts {
        if n_now > pg.prev_contacts {
            consider(
                &mut best,
                Prio::Update,
                format!(
                    "{}, {}, groups splitting, {} groups",
                    spoken_callsign(who),
                    controller,
                    number_word(n_now as u32)
                ),
            );
        } else {
            consider(
                &mut best,
                Prio::Update,
                format!("{}, groups converging, {}", spoken_callsign(who), fill_word(1)),
            );
        }
    }
    pg.prev_contacts = n_now;

    // PHASE 4 — active intercept vectoring.
    if let Some(mut ic) = pg.intercept.take() {
        let target = flight.contacts.iter().find(|c| {
            let k = group_key(c);
            (k.0 as i32 - ic.key.0 as i32).abs() <= 1 && (k.1 as i32 - ic.key.1 as i32).abs() <= 3
        });
        match target {
            None => {
                consider(
                    &mut best,
                    Prio::Vector,
                    format!("{}, {}, clean, resume CAP", spoken_callsign(who), controller),
                );
                // intercept ends (ic dropped)
            }
            Some(c) if nm(c.rng_m) <= 3 && !ic.merged => {
                ic.merged = true;
                consider(
                    &mut best,
                    Prio::Vector,
                    format!("{}, merged, merged", spoken_callsign(who)),
                );
                pg.intercept = Some(ic);
            }
            Some(c) if nm(c.rng_m) > 60 => {
                consider(
                    &mut best,
                    Prio::Vector,
                    format!("{}, {}, clean, resume CAP", spoken_callsign(who), controller),
                );
            }
            Some(c) => {
                ic.key = group_key(c);
                let due = ic
                    .last_vector
                    .map_or(true, |t| t.elapsed().as_secs() >= 10);
                if due {
                    if let Some(dh) = intercept_heading(flight, c) {
                        ic.last_vector = Some(Instant::now());
                        ic.last_hdg = dh;
                        consider(
                            &mut best,
                            Prio::Vector,
                            format!(
                                "{}, {}, {} {}, {}, {}, {}, {}",
                                spoken_callsign(who),
                                controller,
                                turn_dir(flight.heading, dh),
                                digit_string(dh),
                                rc.group_label(c.brg, c.rng_m, c.group_size),
                                rc.pos(c.brg, c.rng_m, Some(c.alt_m)),
                                altitude_phrase(c.alt_m, rc.units),
                                aspect_word(c.aspect),
                            ),
                        );
                    }
                }
                pg.intercept = Some(ic);
            }
        }
    }

    // PICTURE: only when nothing more urgent fired this tick.
    if best.is_none() {
        let due = pg
            .last_picture
            .map_or(true, |t| t.elapsed().as_secs() >= cfg.picture_interval_secs);
        if due {
            pg.last_picture = Some(Instant::now());
            consider(&mut best, Prio::Picture, picture_call(who, &rc));
        }
    }

    let (prio, text) = best?;
    // Ledger is up to date; this flight just doesn't want to be spoken to.
    if quiet {
        return None;
    }
    let urgent = matches!(
        prio,
        Prio::SamLaunch | Prio::Threat | Prio::SamThreat | Prio::Splash | Prio::Vector
    );
    let cooldown_ok = urgent
        || pg
            .last_tx
            .map_or(true, |t| t.elapsed().as_secs() >= cfg.per_player_cooldown_secs);
    if !cooldown_ok {
        return None;
    }
    pg.last_tx = Some(Instant::now());
    Some((urgent, text))
}

// ─── Brevity rendering ─────────────────────────────────────────────────────
//
// Phrasing follows NATO air-to-air brevity (ATP-3.3.4.2 style): bearings are
// spoken digit-by-digit, range as a whole number of miles, altitude as
// "<N> thousand", contacts are counted with a fill word, the group's motion is
// its cardinal track, and everything ends with a declaration. "BRAA" is never
// spelled out — TTS mangles the acronym — the numbers are given plainly.

/// Fill: how many contacts are in the group.
fn fill_word(size: u8) -> &'static str {
    match size {
        0 | 1 => "single group",
        2 => "two ship",
        3 => "three ship",
        _ => "heavy",
    }
}

/// Aspect relative to the flight being called (for threat / bogey-dope calls).
fn aspect_word(a: GciAspect) -> &'static str {
    match a {
        GciAspect::Hot => "hot",
        GciAspect::Flank => "flanking",
        GciAspect::Beam => "beaming",
        GciAspect::Cold => "cold",
    }
}

/// The group's own cardinal track, from its heading.
fn track_word(hdg: u16) -> &'static str {
    match ((hdg as u32 + 22) / 45) % 8 {
        0 => "north",
        1 => "northeast",
        2 => "east",
        3 => "southeast",
        4 => "south",
        5 => "southwest",
        6 => "west",
        _ => "northwest",
    }
}

/// Declaration — what the group is.
fn declare_word(class: u8) -> &'static str {
    match class {
        2 => "hostile heavy", // bomber
        3 => "hostile rotary",
        _ => "hostile",
    }
}

/// Full declaration. Prefers the type's reporting/common name ("hostile
/// Flankers"); if the type is known but unmapped, speaks a tidied form of the
/// raw type ("hostile MiG twenty nine"); otherwise the coarse class word.
fn declare_phrase(c: &GciContact) -> String {
    declare_phrase_aged(c, Maturity::Hostile)
}

/// Declaration modulated by how long the track has been held: a brand-new
/// contact is a "bogey" with no type; once it settles it becomes "bandit"
/// then "hostile" with the reporting name.
fn declare_phrase_aged(c: &GciContact, mat: Maturity) -> String {
    if mat == Maturity::Bogey {
        return if c.group_size >= 2 { "bogeys".into() } else { "bogey".into() };
    }
    let prefix = if mat == Maturity::Bandit { "bandit" } else { "hostile" };
    match c.type_name.as_deref() {
        Some(raw) => match reporting_name(raw) {
            Some(name) if c.group_size >= 2 => format!("{prefix} {name}s"),
            Some(name) => format!("{prefix} {name}"),
            None => match tidy_type(raw) {
                t if t.is_empty() => prefixed_class(prefix, c.class),
                t => format!("{prefix} {t}"),
            },
        },
        None => prefixed_class(prefix, c.class),
    }
}

/// Class word with a maturity prefix ("bandit heavy", "hostile rotary").
fn prefixed_class(prefix: &str, class: u8) -> String {
    match declare_word(class).strip_prefix("hostile") {
        Some(rest) => format!("{prefix}{rest}"),
        None => prefix.to_string(),
    }
}

/// Turn a raw DCS type into something readable when we have no reporting name:
/// "F-16C_50" → "F 16", "MiG-29S" → "MiG 29", "Su-30MKI" → "Su 30".
fn tidy_type(raw: &str) -> String {
    let mut out = String::new();
    for ch in raw.chars() {
        match ch {
            '_' => break, // drop the "_50" / "_hornet" module suffix
            '-' | ' ' => out.push(' '),
            c if c.is_ascii_alphabetic() => out.push(c),
            c if c.is_ascii_digit() => out.push(c),
            _ => {}
        }
    }
    // keep "<letters> <first number block>" — drop trailing variant letters
    let mut parts = out.split_whitespace();
    let head = parts.next().unwrap_or("").to_string();
    let num: String = parts
        .next()
        .unwrap_or("")
        .chars()
        .take_while(|c| c.is_ascii_digit())
        .collect();
    if head.is_empty() {
        String::new()
    } else if num.is_empty() {
        head
    } else {
        format!("{head} {num}")
    }
}

/// DCS aircraft type name → NATO reporting name (Soviet/Russian/Chinese) or
/// common nickname (Western). Loose substring match on the DCS type string.
fn reporting_name(raw: &str) -> Option<&'static str> {
    let t = raw.to_ascii_lowercase();
    let m = |p: &str| t.contains(p);
    Some(match () {
        _ if m("mig-29") => "Fulcrum",
        _ if m("mig-31") => "Foxhound",
        _ if m("mig-25") => "Foxbat",
        _ if m("mig-23") => "Flogger",
        _ if m("mig-21") => "Fishbed",
        _ if m("mig-19") => "Farmer",
        _ if m("mig-15") => "Fagot",
        _ if m("su-27") || m("j-11") => "Flanker",
        _ if m("su-33") || m("su-30") || m("su-35") => "Flanker",
        _ if m("su-34") => "Fullback",
        _ if m("su-24") => "Fencer",
        _ if m("su-25") => "Frogfoot",
        _ if m("su-17") => "Fitter",
        _ if m("tu-22") => "Backfire",
        _ if m("tu-95") || m("tu-142") => "Bear",
        _ if m("tu-160") => "Blackjack",
        _ if m("il-76") => "Candid",
        _ if m("il-78") => "Midas",
        _ if m("a-50") => "Mainstay",
        _ if m("an-26") => "Curl",
        _ if m("an-30") => "Clank",
        _ if m("yak-40") => "Codling",
        _ if m("f-16") => "Viper",
        _ if m("fa-18") || m("f/a-18") || m("f-18") => "Hornet",
        _ if m("f-15e") => "Strike Eagle",
        _ if m("f-15") => "Eagle",
        _ if m("f-14") => "Tomcat",
        _ if m("f-5") => "Tiger",
        _ if m("f-4") => "Phantom",
        _ if m("a-10") => "Warthog",
        _ if m("av8b") || m("av-8b") => "Harrier",
        _ if m("m-2000") => "Mirage",
        _ if m("mirage-f1") => "Mirage F1",
        _ if m("jf-17") => "Thunder",
        _ if m("jas39") || m("gripen") => "Gripen",
        _ if m("ka-50") || m("ka-52") => "Hokum",
        _ if m("mi-24") => "Hind",
        _ if m("mi-28") => "Havoc",
        _ if m("mi-8") || m("mi-17") => "Hip",
        _ if m("mi-26") => "Halo",
        _ if m("ah-64") => "Apache",
        _ if m("ah-1") => "Cobra",
        _ if m("uh-1") => "Huey",
        _ if m("sa342") => "Gazelle",
        _ if m("oh-58") || m("oh58") => "Kiowa",
        _ if m("ch-47") => "Chinook",
        _ if m("uh-60") => "Blackhawk",
        _ => return None,
    })
}

/// Small cardinal numbers as words (for clock positions).
/// A single digit as it is spoken on the radio.
///
/// Unlike [`number_word`] this is digit-by-digit — callsigns, bearings and
/// headings are never read as quantities. The spellings are the ICAO/NATO
/// ones ("tree", "fife", "niner"), which exist because the ordinary words are
/// the ones that get lost under noise: "five" and "nine" are easily confused,
/// and "three" collapses to "free". They read oddly on the page but come out
/// of a TTS voice sounding like a controller rather than a satnav.
fn digit_word(d: u32) -> &'static str {
    match d {
        0 => "zero",
        1 => "one",
        2 => "two",
        3 => "tree",
        4 => "four",
        5 => "fife",
        6 => "six",
        7 => "seven",
        8 => "eight",
        _ => "niner",
    }
}

/// The everyday spelling of a digit — what a speech recogniser writes down when
/// a player reads a number, as opposed to what we say back.
fn digit_word_plain(d: u32) -> &'static str {
    match d {
        0 => "zero",
        1 => "one",
        2 => "two",
        3 => "three",
        4 => "four",
        5 => "five",
        6 => "six",
        7 => "seven",
        8 => "eight",
        _ => "nine",
    }
}

fn number_word(n: u32) -> &'static str {
    match n {
        1 => "one",
        2 => "two",
        3 => "three",
        4 => "four",
        5 => "five",
        6 => "six",
        7 => "seven",
        8 => "eight",
        9 => "nine",
        10 => "ten",
        11 => "eleven",
        _ => "twelve",
    }
}

/// Vertical-speed trend suffix for a contact call.
fn trend_word(vspd_ms: i16) -> &'static str {
    if vspd_ms > 12 {
        ", climbing"
    } else if vspd_ms < -12 {
        ", descending"
    } else {
        ""
    }
}

/// Spoken range: "<n> miles" / "<n> kilometers".
fn range_phrase(m: u32, units: GciUnits) -> String {
    match units {
        GciUnits::Imperial => {
            let n = nm(m);
            format!("{n} mile{}", if n == 1 { "" } else { "s" })
        }
        GciUnits::Metric => format!("{} kilometers", ((m as f64) / 1000.0).round().max(1.0) as u32),
    }
}

/// Spoken altitude: "<N> thousand" (imperial) / "<N> hundred meters" (metric).
fn altitude_phrase(alt_m: i32, units: GciUnits) -> String {
    match units {
        GciUnits::Imperial => {
            let kft = (((alt_m as f64) * 3.28084) / 1000.0).round() as i32;
            if kft <= 0 {
                "low".to_string()
            } else {
                format!("{kft} thousand")
            }
        }
        GciUnits::Metric => {
            let hm = (((alt_m as f64) / 100.0).round() as i32) * 100;
            if hm <= 0 {
                "low".to_string()
            } else {
                format!("{hm} meters")
            }
        }
    }
}

/// Render context: everything the call renderers need beyond the contact.
struct RCtx<'a> {
    units: GciUnits,
    refm: GciRef,
    flight: &'a GciFlight,
    be: Option<(f64, f64)>,
    /// Spoken name of the bullseye point ("bullseye" or a mission codeword).
    bulls_name: &'a str,
    /// Coalition-wide group names, when the picture warranted them.
    groups: &'a [NamedGroup],
}

impl RCtx<'_> {
    /// The coalition-wide name for the group at this bearing/range, if one was
    /// assigned this tick.
    fn group_name(&self, brg: u16, rng_m: u32) -> Option<&'static str> {
        if self.groups.is_empty() {
            return None;
        }
        let (glat, glon) = project(self.flight.lat, self.flight.lon, brg as f64, rng_m as f64);
        self.groups
            .iter()
            .map(|g| {
                let (_, d) = bearing_range(g.lat, g.lon, glat, glon);
                (d, g.name)
            })
            .filter(|(d, _)| *d < 32_000.0)
            .min_by(|a, b| a.0.total_cmp(&b.0))
            .map(|(_, n)| n)
    }

    /// Group label: the shared name if there is one, otherwise the fill word.
    /// With a name *and* a multi-ship group, both ("north group, two ship").
    fn group_label(&self, brg: u16, rng_m: u32, size: u8) -> String {
        match (self.group_name(brg, rng_m), size) {
            (Some(n), s) if s >= 2 => format!("{n}, {}", fill_word(s)),
            (Some(n), _) => n.to_string(),
            (None, s) => fill_word(s).to_string(),
        }
    }
}

impl RCtx<'_> {
    /// Position phrase from bearing/range relative to the flight, in the
    /// flight's chosen reference. `alt_m` gives clock calls a high/low tag.
    fn pos(&self, brg: u16, rng_m: u32, alt_m: Option<i32>) -> String {
        match self.refm {
            GciRef::Bullseye => {
                if let Some(b) = self.be {
                    let (glat, glon) =
                        project(self.flight.lat, self.flight.lon, brg as f64, rng_m as f64);
                    let (bbrg, bdist) = bearing_range(b.0, b.1, glat, glon);
                    return format!(
                        "{} {} {}",
                        self.bulls_name,
                        digit_string(bbrg as u16),
                        range_phrase(bdist as u32, self.units)
                    );
                }
                format!("{} for {}", digit_string(brg), range_phrase(rng_m, self.units))
            }
            GciRef::Clock => {
                let rel = (brg as i32 - self.flight.heading as i32 + 360) % 360;
                let mut hour = ((rel + 15) / 30) % 12;
                if hour == 0 {
                    hour = 12;
                }
                let fa = self.flight.alt_m as i32;
                let hl = match alt_m {
                    Some(a) if a - fa > 900 => " high",
                    Some(a) if a - fa < -900 => " low",
                    _ => "",
                };
                format!(
                    "{} o'clock{}, {}",
                    number_word(hour as u32),
                    hl,
                    range_phrase(rng_m, self.units)
                )
            }
            GciRef::Braa => {
                format!("{} for {}", digit_string(brg), range_phrase(rng_m, self.units))
            }
        }
    }
}

/// "<callsign>[, <controller>]" — controller callsign only on the first call.
fn head(callsign: &str, controller: Option<&str>) -> String {
    match controller {
        Some(c) => format!("{}, {c}", spoken_callsign(callsign)),
        None => spoken_callsign(callsign),
    }
}

/// A group call: fill, position, altitude, aspect, declaration + trend.
fn braa_call(
    controller: Option<&str>,
    callsign: &str,
    c: &GciContact,
    rc: &RCtx,
    mat: Maturity,
) -> String {
    format!(
        "{}, {}, {}, {}, {}, {}{}",
        head(callsign, controller),
        rc.group_label(c.brg, c.rng_m, c.group_size),
        rc.pos(c.brg, c.rng_m, Some(c.alt_m)),
        altitude_phrase(c.alt_m, rc.units),
        aspect_word(c.aspect),
        declare_phrase_aged(c, mat),
        trend_word(c.vspd_ms),
    )
}

/// Threat call — a hostile is a danger to this flight now.
fn threat_call(
    controller: &str,
    callsign: &str,
    c: &GciContact,
    rc: &RCtx,
    mat: Maturity,
) -> String {
    format!(
        "{}, {}, threat, {}, {}, {}, {}, {}{}",
        spoken_callsign(callsign),
        controller,
        rc.group_label(c.brg, c.rng_m, c.group_size),
        rc.pos(c.brg, c.rng_m, Some(c.alt_m)),
        altitude_phrase(c.alt_m, rc.units),
        aspect_word(c.aspect),
        declare_phrase_aged(c, mat),
        trend_word(c.vspd_ms),
    )
}

/// A short "cold, extending" call.
fn cold_call(callsign: &str, c: &GciContact, rc: &RCtx) -> String {
    format!(
        "{}, {}, cold, extending, {}",
        spoken_callsign(callsign),
        rc.group_label(c.brg, c.rng_m, c.group_size),
        rc.pos(c.brg, c.rng_m, Some(c.alt_m)),
    )
}

/// Group fill-in change ("now two ship").
fn fillin_call(callsign: &str, c: &GciContact, rc: &RCtx) -> String {
    format!(
        "{}, {}, {}, now {}",
        spoken_callsign(callsign),
        rc.group_label(c.brg, c.rng_m, c.group_size),
        rc.pos(c.brg, c.rng_m, Some(c.alt_m)),
        fill_word(c.group_size),
    )
}

/// Splash — a hostile the flight was warned about is down.
fn splash_call(controller: &str, callsign: &str, s: &GciSamThreat, rc: &RCtx) -> String {
    format!(
        "{}, {}, splash, {}",
        spoken_callsign(callsign),
        controller,
        rc.pos(s.brg, s.rng_m, None),
    )
}

/// Faded — a tracked group has dropped off radar.
fn faded_call(callsign: &str, t: &Tracked, rc: &RCtx) -> String {
    format!(
        "{}, {} faded, last {}",
        spoken_callsign(callsign),
        rc.group_label(t.brg, t.rng_m, t.size),
        rc.pos(t.brg, t.rng_m, None),
    )
}

/// SAM launch — an enemy SAM missile is in the air near this flight.
fn sam_launch_call(controller: &str, callsign: &str, s: &GciSamThreat, rc: &RCtx) -> String {
    format!(
        "{}, {}, S-A-M launch, {}, defend, defend",
        spoken_callsign(callsign),
        controller,
        rc.pos(s.brg, s.rng_m, None),
    )
}

/// SAM threat — a live enemy SAM's engagement zone covers this flight.
fn sam_call(controller: &str, callsign: &str, s: &GciSamThreat, rc: &RCtx) -> String {
    let band = match s.site_range_m {
        r if r < 35_000 => "short range S-A-M",
        r if r < 90_000 => "S-A-M",
        _ => "long range S-A-M",
    };
    format!(
        "{}, {}, {} threat, {}, defend",
        spoken_callsign(callsign),
        controller,
        band,
        rc.pos(s.brg, s.rng_m, None),
    )
}

/// Periodic picture.
fn picture_call(callsign: &str, rc: &RCtx) -> String {
    let cs = spoken_callsign(callsign);
    let groups = &rc.flight.contacts;
    if groups.is_empty() {
        return format!("{cs}, picture clean");
    }
    let n = groups.len();
    let mut s = format!("{cs}, picture, {n} group{}.", if n == 1 { "" } else { "s" });
    for (i, c) in groups.iter().take(3).enumerate() {
        // Prefer the coalition-wide group name; fall back to lead/middle/trail.
        let named = rc.group_name(c.brg, c.rng_m);
        let label = match (named, n, i) {
            (Some(nm), _, _) => format!("{nm}, "),
            (None, 1, _) => String::new(),
            (None, _, 0) => "lead group, ".into(),
            (None, _, k) if k == n - 1 || k == 2 => "trail group, ".into(),
            (None, _, _) => "middle group, ".into(),
        };
        s.push_str(&format!(
            " {label}{}, {}, track {}, {}.",
            rc.pos(c.brg, c.rng_m, Some(c.alt_m)),
            altitude_phrase(c.alt_m, rc.units),
            track_word(c.hdg),
            declare_phrase(c),
        ));
    }
    s
}

/// "Enfield11" → "Enfield 1 1"; "Springfield21" → "Springfield 2 1".
/// Also cleans up a raw player name used as the fallback callsign: strips
/// bracketed clan tags and punctuation so TTS doesn't read "open bracket dot
/// I D", and drops the trailing "00" DCS puts on custom group callsigns.
fn spoken_callsign(cs: &str) -> String {
    // "№15 | KillerDog", "=51= | Ivan" — a pipe separates a squadron tag from
    // the name; only the name gets spoken.
    let cs = cs.rsplit('|').next().map(str::trim).filter(|s| !s.is_empty()).unwrap_or(cs);
    // Drop [..] (..) {..} <..> tag groups and stray punctuation.
    let mut cleaned = String::with_capacity(cs.len());
    let mut depth = 0i32;
    for ch in cs.chars() {
        match ch {
            '[' | '(' | '{' | '<' => depth += 1,
            ']' | ')' | '}' | '>' => depth = (depth - 1).max(0),
            _ if depth > 0 => {}
            c if c.is_alphanumeric() || c == ' ' => cleaned.push(c),
            _ => cleaned.push(' '),
        }
    }
    let cleaned = cleaned.split_whitespace().collect::<Vec<_>>().join(" ");
    let cs: &str = if cleaned.is_empty() { cs } else { &cleaned };

    let cs = cs
        .strip_suffix("00")
        .filter(|s| s.chars().last().map_or(false, |c| c.is_alphabetic()))
        .unwrap_or(cs);
    let mut out = String::with_capacity(cs.len() + 4);
    let mut prev_digit = false;
    for (i, ch) in cs.chars().enumerate() {
        if ch.is_ascii_digit() {
            if i > 0 && !out.ends_with(' ') {
                out.push(' ');
            }
            out.push(ch);
            prev_digit = true;
        } else {
            if prev_digit && !out.ends_with(' ') {
                out.push(' ');
            }
            out.push(ch);
            prev_digit = false;
        }
    }
    out
}

/// Bearing/heading as spoken digits: 340 → "three four zero".
fn digit_string(n: u16) -> String {
    format!("{:03}", n % 1000)
        .chars()
        .filter_map(|c| c.to_digit(10))
        .map(digit_word)
        .collect::<Vec<_>>()
        .join(" ")
}

// ─── Geo helpers (equirectangular — fine at tactical ranges) ────────────────

fn project(lat: f64, lon: f64, brg_deg: f64, dist_m: f64) -> (f64, f64) {
    let r = 6_371_000.0_f64;
    let brg = brg_deg.to_radians();
    let dlat = (dist_m * brg.cos()) / r;
    let dlon = (dist_m * brg.sin()) / (r * lat.to_radians().cos());
    (lat + dlat.to_degrees(), lon + dlon.to_degrees())
}

fn bearing_range(lat1: f64, lon1: f64, lat2: f64, lon2: f64) -> (f64, f64) {
    let r = 6_371_000.0_f64;
    let mlat = ((lat1 + lat2) / 2.0).to_radians();
    let dlat = (lat2 - lat1).to_radians();
    let dlon = (lon2 - lon1).to_radians();
    let x = dlon * mlat.cos();
    let dist = ((dlat * r).powi(2) + (x * r).powi(2)).sqrt();
    let brg = (x.atan2(dlat).to_degrees() + 360.0) % 360.0;
    (brg, dist)
}

// ─── Phase 4: intercept geometry ──────────────────────────────────────────

/// Lead-collision heading to intercept a group given as BRAA from the flight,
/// with both speeds and the group's heading. `None` if no solution (the group
/// is faster and running).
fn intercept_heading(flight: &GciFlight, c: &GciContact) -> Option<u16> {
    // Local plane, metres: x = east, y = north, centred on the flight.
    let brg = (c.brg as f64).to_radians();
    let (rx, ry) = ((c.rng_m as f64) * brg.sin(), (c.rng_m as f64) * brg.cos());
    let th = (c.hdg as f64).to_radians();
    let (tvx, tvy) = ((c.spd_ms as f64) * th.sin(), (c.spd_ms as f64) * th.cos());
    let so = (flight.speed_ms.max(60) as f64).max(1.0);

    // |R + Vt·τ| = so·τ  →  a τ² + b τ + c = 0
    let a = tvx * tvx + tvy * tvy - so * so;
    let b = 2.0 * (rx * tvx + ry * tvy);
    let cc = rx * rx + ry * ry;
    let tau = if a.abs() < 1e-6 {
        if b.abs() < 1e-6 {
            return None;
        }
        -cc / b
    } else {
        let disc = b * b - 4.0 * a * cc;
        if disc < 0.0 {
            return None;
        }
        let s = disc.sqrt();
        [(-b - s) / (2.0 * a), (-b + s) / (2.0 * a)]
            .into_iter()
            .filter(|x| *x > 0.0)
            .fold(f64::INFINITY, f64::min)
    };
    if !tau.is_finite() || tau <= 0.0 {
        return None;
    }
    let (ax, ay) = (rx + tvx * tau, ry + tvy * tau);
    Some(((ax.atan2(ay).to_degrees() + 360.0) % 360.0) as u16)
}

/// "come left <hdg>" / "come right <hdg>" / "continue" from a current heading.
fn turn_dir(from: u16, to: u16) -> &'static str {
    let d = (to as i32 - from as i32 + 540) % 360 - 180;
    if d < -7 {
        "come left"
    } else if d > 7 {
        "come right"
    } else {
        "continue heading"
    }
}

// ─── Transmission ──────────────────────────────────────────────────────────

struct QueuedCall {
    queued: Instant,
    side: Side,
    urgent: bool,
    text: String,
}

/// The two persistent SRS clients plus a per-side TTS engine (so red can use a
/// Russian-accented voice while blue uses an English one).
struct Voice {
    blue: Option<srs::SrsClient>,
    red: Option<srs::SrsClient>,
    /// Every frequency each side's controller transmits on. One transmission
    /// goes out on all of them at once (UHF + VHF + FM).
    blue_radios: Vec<Radio>,
    red_radios: Vec<Radio>,
    tts_blue: tts::Tts,
    tts_red: tts::Tts,
}

/// Where each coalition's transcript goes on Discord.
///
/// Three shapes fall out of the config with no extra switch: one webhook for
/// everything (both coalitions, colour-tagged), a webhook per coalition (two
/// channels), or one per coalition with `discordWebhookUrl` catching whichever
/// side has none of its own.
#[derive(Clone, Default)]
struct DiscordRoute {
    blue: Option<String>,
    red: Option<String>,
    /// Server-name prefix, when more than one DCS instance shares a channel.
    prefix: String,
}

impl DiscordRoute {
    fn build(cfg: &GciConfig, inst: &crate::Inst, multi_instance: bool) -> Self {
        let ok = |u: &Option<String>| u.clone().filter(|s| s.starts_with("https://"));
        let shared = ok(&cfg.discord_webhook_url);
        // Tag by default only when this bfdb fronts several servers — a
        // single-server setup does not need its own name on every line.
        let tag = cfg.discord_tag_instance.unwrap_or(multi_instance);
        let prefix = match (tag, inst.cfg.label().trim()) {
            (true, n) if !n.is_empty() => format!("`[{n}]` "),
            _ => String::new(),
        };
        DiscordRoute {
            blue: ok(&cfg.blue_discord_webhook_url).or_else(|| shared.clone()),
            red: ok(&cfg.red_discord_webhook_url).or(shared),
            prefix,
        }
    }

    fn any(&self) -> bool {
        self.blue.is_some() || self.red.is_some()
    }

    fn prefix(&self) -> &str {
        &self.prefix
    }

    fn for_side(&self, side: Side) -> Option<&String> {
        match side {
            Side::Red => self.red.as_ref(),
            _ => self.blue.as_ref(),
        }
    }

    fn log(&self) {
        match (&self.blue, &self.red) {
            (None, None) => {}
            (b, r) if b == r => log::info!("gci: Discord transcript enabled (one channel)"),
            (Some(_), Some(_)) => {
                log::info!("gci: Discord transcript enabled (separate blue/red channels)")
            }
            (Some(_), None) => log::info!("gci: Discord transcript enabled (blue only)"),
            (None, Some(_)) => log::info!("gci: Discord transcript enabled (red only)"),
        }
    }
}

/// Post one GCI call to the Discord transcript webhook. Fire-and-forget —
/// failures are logged at debug and never block a transmission.
async fn post_discord(
    http: &reqwest::Client,
    url: &str,
    side: Side,
    prefix: &str,
    text: &str,
) {
    let tag = match side {
        Side::Blue => "🔵",
        Side::Red => "🔴",
        _ => "⚪",
    };
    let body = serde_json::json!({
        "content": format!("{prefix}{tag} {text}"),
        "allowed_mentions": { "parse": [] }
    });
    match tokio::time::timeout(Duration::from_secs(5), http.post(url).json(&body).send()).await {
        Ok(Ok(r)) if r.status().is_success() || r.status().as_u16() == 204 => {}
        Ok(Ok(r)) => log::debug!("gci: Discord webhook returned {}", r.status()),
        Ok(Err(e)) => log::debug!("gci: Discord webhook error: {e}"),
        Err(_) => log::debug!("gci: Discord webhook timed out"),
    }
}

/// Treat an empty string in an optional path field as "not set" — JSON
/// `""` deserializes to `Some(PathBuf::new())`, not `None`.
fn nonempty_path(p: &Option<PathBuf>) -> Option<PathBuf> {
    p.as_ref()
        .filter(|p| !p.as_os_str().is_empty())
        .map(|p| p.to_path_buf())
}

fn build_voice(
    cfg: &GciConfig,
    rx_tx: Option<tokio::sync::mpsc::UnboundedSender<srs::Transmission>>,
) -> Voice {
    let piper_exe = nonempty_path(&cfg.piper_exe);
    let side_tts = |model: &Option<PathBuf>, voice: &Option<String>| -> tts::Tts {
        let model = nonempty_path(model).or_else(|| nonempty_path(&cfg.piper_model));
        let voice = voice
            .as_ref()
            .filter(|s| !s.is_empty())
            .or_else(|| cfg.tts_voice.as_ref().filter(|s| !s.is_empty()));
        tts::Tts::from_cfg(piper_exe.as_ref(), model.as_ref(), voice)
    };
    let tts_blue = side_tts(&cfg.blue_piper_model, &cfg.blue_tts_voice);
    let tts_red = side_tts(&cfg.red_piper_model, &cfg.red_tts_voice);
    log::info!(
        "gci: TTS — blue: {}, red: {}",
        tts_blue.describe(),
        tts_red.describe()
    );

    let opus_dll = nonempty_path(&cfg.opus_dll_path);
    let opus = match srs::Opus::load(opus_dll.as_deref()) {
        Ok(o) => Some(Arc::new(o)),
        Err(e) => {
            log::error!(
                "gci: could not load opus.dll ({e:#}) — GCI will log calls but NOT transmit. \
                 Copy opus.dll (ships with DCS-SRS) next to bfdb.exe, or set opusDllPath in gci.json."
            );
            None
        }
    };

    let mk = |coalition: u8, name: &str, radios: Vec<Radio>, pw: &str| -> Option<srs::SrsClient> {
        if radios.is_empty() {
            log::error!(
                "gci: no frequency configured for '{name}' — set freqs (or the legacy freqMhz)                  in gci.json; this side will not transmit"
            );
            return None;
        }
        match srs::SrsClient::start(
            &cfg.srs_host,
            cfg.srs_port,
            coalition,
            name,
            radios,
            pw,
            opus.clone(),
            rx_tx.clone(),
        ) {
            Ok(c) => Some(c),
            Err(e) => {
                log::error!("gci: SRS '{name}' client failed to start: {e:#}");
                None
            }
        }
    };

    let blue_radios = cfg.radios(Side::Blue);
    let red_radios = cfg.radios(Side::Red);
    for (side, rs) in [("blue", &blue_radios), ("red", &red_radios)] {
        if !rs.is_empty() {
            log::info!(
                "gci: {side} controller on {}",
                rs.iter().map(|r| r.describe()).collect::<Vec<_>>().join(" + ")
            );
        }
    }

    Voice {
        blue: mk(
            2,
            &cfg.blue_controller_callsign,
            blue_radios.clone(),
            &cfg.blue_eam_password,
        ),
        red: mk(
            1,
            &cfg.red_controller_callsign,
            red_radios.clone(),
            &cfg.red_eam_password,
        ),
        blue_radios,
        red_radios,
        tts_blue,
        tts_red,
    }
}

async fn transmit_worker(
    voice: Voice,
    discord: DiscordRoute,
    gap_secs: u64,
    transcript_tx: broadcast::Sender<String>,
    transcript: Transcript,
    mut rx: mpsc::Receiver<QueuedCall>,
) {
    let http = discord.any().then(reqwest::Client::new);
    let mut last_tx_end: Option<Instant> = None;
    while let Some(call) = rx.recv().await {
        if call.queued.elapsed() > Duration::from_secs(12) {
            log::debug!("gci: dropping stale call: {}", call.text);
            continue;
        }

        // Dashboard transcript.
        let entry = serde_json::json!({
            "time": chrono::Utc::now().to_rfc3339(),
            "side": if call.side == Side::Red { "red" } else { "blue" },
            "text": call.text,
        })
        .to_string();
        {
            let mut h = transcript.lock().unwrap();
            h.push_back(entry.clone());
            while h.len() > TRANSCRIPT_CAP {
                h.pop_front();
            }
        }
        let _ = transcript_tx.send(entry);

        // Global inter-call gap: hold the frequency quiet for a while between
        // GCI transmissions so players can make their own calls. Urgent calls
        // (threat / SAM) wait half as long.
        let gap = Duration::from_secs(if call.urgent { gap_secs.max(1) / 2 } else { gap_secs });
        if let Some(end) = last_tx_end {
            let since = end.elapsed();
            if since < gap {
                tokio::time::sleep(gap - since).await;
            }
        }
        // Re-check staleness after the wait.
        if call.queued.elapsed() > Duration::from_secs(20) {
            log::debug!("gci: dropping stale call after gap: {}", call.text);
            continue;
        }

        log::info!("gci[{:?}]: {}", call.side, call.text);

        if let (Some(url), Some(http)) = (discord.for_side(call.side), &http) {
            post_discord(http, url, call.side, discord.prefix(), &call.text).await;
        }

        let (client, radios) = match call.side {
            Side::Blue => (voice.blue.clone(), voice.blue_radios.clone()),
            Side::Red => (voice.red.clone(), voice.red_radios.clone()),
            _ => (None, Vec::new()),
        };
        let Some(client) = client else {
            log::debug!("gci: no SRS client for {:?}; call not transmitted", call.side);
            continue;
        };
        if !client.is_connected() {
            log::warn!("gci: SRS not connected for {:?}; call dropped", call.side);
            continue;
        }

        let tts = match call.side {
            Side::Red => voice.tts_red.clone(),
            _ => voice.tts_blue.clone(),
        };
        let text = call.text.clone();
        let res = tokio::task::spawn_blocking(move || -> anyhow::Result<usize> {
            let pcm = tts.synthesize(&text)?;
            let n = pcm.len();
            client.transmit(&radios, &pcm)?;
            Ok(n)
        })
        .await;
        last_tx_end = Some(Instant::now());
        match res {
            Ok(Ok(n)) => log::info!(
                "gci[{:?}]: transmitted ({:.1}s audio)",
                call.side,
                n as f64 / 16_000.0
            ),
            Ok(Err(e)) => log::warn!("gci: transmit failed: {e:#}"),
            Err(e) => log::warn!("gci: transmit task panicked: {e}"),
        }
    }
}

// ─── Phase 3: player → GCI (speech recognition) ────────────────────────────

async fn stt_worker(
    stt: stt::Stt,
    cfg: GciConfig,
    cache: PictureCache,
    tx: mpsc::Sender<QueuedCall>,
    commit_tx: tokio::sync::mpsc::UnboundedSender<(Side, String)>,
    mut rx: tokio::sync::mpsc::UnboundedReceiver<srs::Transmission>,
) {
    while let Some(mut tr) = rx.recv().await {
        let (side, side_str) = if tr.coalition == 1 {
            (Side::Red, "red")
        } else {
            (Side::Blue, "blue")
        };
        let wake = cfg.controller(side).to_lowercase();
        let s = stt.clone();
        // The audio moves into the blocking transcribe task; everything else
        // about the transmission is still needed to answer it.
        let pcm = std::mem::take(&mut tr.pcm);
        let text = match tokio::task::spawn_blocking(move || s.transcribe(&pcm)).await {
            Ok(Ok(t)) => t,
            Ok(Err(e)) => {
                log::debug!("gci: whisper failed: {e}");
                continue;
            }
            Err(_) => continue,
        };
        if text.is_empty() {
            continue;
        }
        log::info!("gci[{side:?}] heard '{}': \"{text}\"", tr.name);
        // The control picture is only needed for calls that report on
        // contacts; a radio check / check-in is answered even when the engine
        // poll has not produced one yet.
        let picture = cache.read().ok().and_then(|c| c.get(side_str).cloned());
        let flight = match_flight(&tr, &text, picture.as_ref());

        let req = match stt::parse_request_dbg(&text, &wake) {
            (Some(r), _) => r,
            (None, Some(stt::Reject::NotAddressed)) => {
                // Not our callsign — someone talking to another agency, or to
                // another player. Staying off the air is the right answer.
                log::info!("gci[{side:?}] ignoring \"{text}\": not addressed to '{wake}'");
                continue;
            }
            (None, _) => {
                // They called us and we could not make out the request. Say so
                // — silence leaves the player wondering whether the whole
                // system is broken.
                let ctrl = cfg.controller(side);
                let who = flight.map(|f| {
                    if f.player_name.is_empty() {
                        f.callsign.as_str()
                    } else {
                        f.player_name.as_str()
                    }
                });
                let answer = match who {
                    Some(w) => format!("{}, {ctrl}, say again", spoken_callsign(w)),
                    None => format!("station calling {ctrl}, say again"),
                };
                log::info!("gci[{side:?}] unparsed \"{text}\" — asking for a repeat");
                let _ = tx.try_send(QueuedCall {
                    queued: Instant::now(),
                    side,
                    urgent: true,
                    text: answer,
                });
                continue;
            }
        };

        if flight.is_none() {
            log::info!(
                "gci[{side:?}] {req:?} from '{}' (unit {:?}): no matching flight in the picture \
                 ({} known)",
                tr.name,
                tr.unit_id,
                picture.as_ref().map_or(0, |p| p.flights.len())
            );
        }

        if req == stt::Request::Commit {
            if let Some(f) = flight {
                let _ = commit_tx.send((side, f.ucid.clone()));
            }
        }

        // A request we understood always gets an answer on the air, even when
        // we can't build the real one — an unanswered call is indistinguishable
        // from a dead system.
        let answer = build_answer(&cfg, side, &req, flight, picture.as_ref()).unwrap_or_else(|| {
            let ctrl = cfg.controller(side);
            match flight {
                Some(f) => {
                    let w = if f.player_name.is_empty() {
                        &f.callsign
                    } else {
                        &f.player_name
                    };
                    format!("{}, {ctrl}, unable", spoken_callsign(w))
                }
                None => format!("station calling {ctrl}, no radar contact, say your position"),
            }
        });
        log::info!("gci[{side:?}] answering {req:?}: {answer}");
        let _ = tx.try_send(QueuedCall {
            queued: Instant::now(),
            side,
            urgent: true,
            text: answer,
        });
    }
}

/// Tie an inbound transmission to a flight.
///
/// The DCS unit id an in-cockpit SRS client reports is exact, so try that
/// first; fall back to name matching for players flying with an external SRS
/// client (which has no unit), and finally to the callsign the pilot spoke.
fn match_flight<'a>(
    tr: &srs::Transmission,
    heard: &str,
    picture: Option<&'a GciControlPicture>,
) -> Option<&'a GciFlight> {
    let p = picture?;
    if let Some(uid) = tr.unit_id {
        if let Some(f) = p.flights.iter().find(|f| f.unit_id == Some(uid)) {
            return Some(f);
        }
    }
    if let Some(f) = p.flights.iter().find(|f| name_matches(&tr.name, f)) {
        return Some(f);
    }
    // "Magic, Colt one one, bogey dope" — the caller said who they are.
    p.flights.iter().find(|f| spoken_callsign_in(heard, f))
}

/// Did the transmission contain this flight's callsign, spoken the way a pilot
/// says it? "Colt11" is read "Colt one one", so compare against the digits
/// written out as words as well as as digits.
fn spoken_callsign_in(heard: &str, f: &GciFlight) -> bool {
    let tokens: Vec<String> = heard
        .split(|c: char| !c.is_alphanumeric())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_lowercase())
        .collect();
    if tokens.is_empty() {
        return false;
    }
    for src in [&f.callsign, &f.player_name] {
        // Split "Colt11" / "Enfield 1-1" into a name and its digits.
        let name: String = src
            .chars()
            .take_while(|c| !c.is_ascii_digit())
            .filter(|c| c.is_alphanumeric())
            .collect::<String>()
            .to_lowercase();
        if name.chars().count() < 3 {
            continue;
        }
        let digits: Vec<char> = src.chars().filter(|c| c.is_ascii_digit()).collect();
        let Some(at) = tokens.iter().position(|t| t == &name) else {
            continue;
        };
        if digits.is_empty() {
            return true;
        }
        // Digits may follow as words ("one one"), as a run ("eleven" is not
        // used on the radio), or glued to the name token.
        let rest: String = tokens[at + 1..].join(" ");
        // We *speak* ICAO ("tree", "fife", "niner") but players say the
        // ordinary words, and whisper writes down what they said — so accept
        // either spelling when matching a callsign we heard.
        let say = |f: fn(u32) -> &'static str| -> String {
            digits
                .iter()
                .filter_map(|d| d.to_digit(10))
                .map(f)
                .collect::<Vec<_>>()
                .join(" ")
        };
        let plain: String = digits.iter().collect();
        if rest.starts_with(&say(digit_word))
            || rest.starts_with(&say(digit_word_plain))
            || rest.starts_with(&plain)
            || tokens[at].ends_with(&plain)
        {
            return true;
        }
    }
    false
}

/// Match an SRS transmitter name to a flight (by pilot name or callsign,
/// tag/punctuation-insensitive, either-contains-either). Players decorate
/// their names differently in SRS and DCS ("№15 | KillerDog" vs "KillerDog"),
/// so a shared significant word counts as a match too.
fn name_matches(srs_name: &str, f: &GciFlight) -> bool {
    let norm = |s: &str| {
        s.chars()
            .filter(|c| c.is_alphanumeric())
            .collect::<String>()
            .to_lowercase()
    };
    // Words of 4+ letters, ignoring the clan tags and squadron numbers that
    // tend to differ between the two names.
    let words = |s: &str| -> Vec<String> {
        s.split(|c: char| !c.is_alphanumeric())
            .map(|w| w.to_lowercase())
            .filter(|w| w.chars().count() >= 4 && w.chars().any(|c| c.is_alphabetic()))
            .collect()
    };
    let a = norm(srs_name);
    if a.is_empty() {
        return false;
    }
    let aw = words(srs_name);
    for b in [&f.player_name, &f.callsign] {
        let bn = norm(b);
        if bn.is_empty() {
            continue;
        }
        if a.contains(&bn) || bn.contains(&a) {
            return true;
        }
        if words(b).iter().any(|w| aw.contains(w)) {
            return true;
        }
    }
    false
}

fn build_answer(
    cfg: &GciConfig,
    side: Side,
    req: &stt::Request,
    flight: Option<&GciFlight>,
    picture: Option<&GciControlPicture>,
) -> Option<String> {
    let ctrl = cfg.controller(side);
    let who = flight.map(|f| {
        if f.player_name.is_empty() {
            f.callsign.as_str()
        } else {
            f.player_name.as_str()
        }
    });
    let addr = |w: Option<&str>| match w {
        Some(w) => format!("{}, {ctrl}", spoken_callsign(w)),
        None => format!("station calling {ctrl}, {ctrl}"),
    };

    match req {
        stt::Request::RadioCheck => Some(format!("{}, loud and clear", addr(who))),
        stt::Request::CheckIn => {
            let n = flight.map_or(0, |f| f.contacts.len());
            let _ = picture;
            Some(format!(
                "{}, radar contact, {}",
                addr(who),
                if n == 0 {
                    "picture clean".to_string()
                } else {
                    format!("{} group{} on the picture", n, if n == 1 { "" } else { "s" })
                }
            ))
        }
        stt::Request::AlphaCheck => {
            let (f, b) = (flight?, picture?.bullseye?);
            let (brg, dist) = bearing_range(b.0, b.1, f.lat, f.lon);
            let units = f.units.unwrap_or_else(|| cfg.default_units());
            Some(format!(
                "{}, your position {} {} {}",
                addr(who),
                cfg.bullseye_name(side),
                digit_string(brg as u16),
                range_phrase(dist as u32, units),
            ))
        }
        stt::Request::Picture => {
            let f = flight?;
            let rc = RCtx {
                units: f.units.unwrap_or_else(|| cfg.default_units()),
                refm: f.reference.unwrap_or_else(|| cfg.default_reference()),
                flight: f,
                be: picture.and_then(|p| p.bullseye),
                bulls_name: cfg.bullseye_name(side),
                groups: &[],
            };
            Some(picture_call(who?, &rc))
        }
        stt::Request::BogeyDope | stt::Request::Snaplock | stt::Request::Commit => {
            let f = flight?;
            let rc = RCtx {
                units: f.units.unwrap_or_else(|| cfg.default_units()),
                refm: f.reference.unwrap_or_else(|| cfg.default_reference()),
                flight: f,
                be: picture.and_then(|p| p.bullseye),
                bulls_name: cfg.bullseye_name(side),
                groups: &[],
            };
            match f.contacts.first() {
                None => Some(format!("{}, clean", addr(who))),
                Some(c) => {
                    let base = braa_call(Some(ctrl), who?, c, &rc, Maturity::Hostile);
                    Some(match req {
                        stt::Request::Commit => format!("{base}, your control"),
                        _ => base,
                    })
                }
            }
        }
        stt::Request::Declare(be_point) => {
            let f = flight?;
            let units = f.units.unwrap_or_else(|| cfg.default_units());
            // A "declare" only makes sense against a point; we only hold
            // hostile tracks, so: hostile if a group is near, else clean.
            let near = match (be_point, picture.and_then(|p| p.bullseye)) {
                (Some((pb, pr)), Some(b)) => {
                    let (plat, plon) = project(b.0, b.1, *pb as f64, (*pr as f64) * 1852.0);
                    f.contacts.iter().any(|c| {
                        let (glat, glon) =
                            project(f.lat, f.lon, c.brg as f64, c.rng_m as f64);
                        let (_, d) = bearing_range(plat, plon, glat, glon);
                        d < 18_520.0 // 10 nm
                    })
                }
                _ => !f.contacts.is_empty(),
            };
            let _ = units;
            Some(format!(
                "{}, {}",
                addr(who),
                if near { "hostile" } else { "clean" }
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::GciConfig;

    /// `gci.sample.json` is the documentation for this struct, and the shape
    /// DCSServerBot's procman renders. If a field is renamed here and not
    /// there, the sample silently stops meaning what it says — serde ignores
    /// unknown keys, so the only symptom on a live server is a setting that
    /// quietly does nothing.
    #[test]
    fn sample_config_parses() {
        let raw = include_str!("../../gci.sample.json");
        let cfg: GciConfig = serde_json::from_str(raw).expect("gci.sample.json must parse");
        assert_eq!(cfg.blue_freqs.len(), 3, "sample should show the multi-band form");
        assert!(cfg.blue_freqs.iter().any(|f| f.mhz < 108.0), "sample should include an FM net");
        let atc = cfg.atc.expect("sample should carry an atc block");
        assert!(atc.tower_base_mhz > 0.0);
        assert!(
            atc.fields.contains_key("Incirlik"),
            "sample should show a per-field frequency override"
        );
    }
}
