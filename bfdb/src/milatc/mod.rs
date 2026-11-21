// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! Military air traffic control for the training range, by voice over SRS.
//!
//! A full set of positions per airfield, each on its own frequency and with
//! its own voice: ATIS, Clearance Delivery, Ground, Tower, Departure,
//! Approach, and a precision (PAR) Final controller. Pilots talk to them the
//! way they would at a real base: they get a clearance, taxi on named
//! taxiways to a hold-short line, are cleared for takeoff when the runway and
//! final are clear, are handed to Departure and released; and on the way home
//! Approach vectors them to the overhead, a straight-in or a talk-down, Tower
//! sequences and clears them, and Ground taxis them in.
//!
//! - [`airport`]: the airfield: runways, taxi network, hold-short points.
//! - [`controller`]: the positions and flight strips: all the decisions.
//! - [`intent`]: what a pilot said, by phraseology rules.
//! - [`llm`]: the language-model fallback for what the rules miss.
//! - [`speech`]: written and spoken phraseology.
//! - [`voices`]: a different voice per frequency, others for AI pilots.
//! - [`live`]: the live audio feed for listeners on the range site.
//!
//! Beyond the airfield, the same plumbing runs the range's other controllers
//! ([`facility`] is what they share):
//!
//! - [`carrier`]: each carrier's Marshal (stack slots), Approach and Paddles.
//! - [`tanker`]: each tanker's crew, running the line of receivers.
//! - [`rangectl`]: Range Control, booking the ranges in slots.
//! - [`airfield`]: the airfield's controllers as a facility.
//!
//! This module is the plumbing: it polls the range engine's `query-atc` and
//! `query-range` pictures, brings facilities on the air as carriers and
//! tankers appear, listens on every position's frequency through a pool of
//! SRS clients ([`crate::voice`]), runs heard calls through whisper and the
//! parsers, and speaks the controllers' replies with Piper.
//!
//! The campaign's spoken tower ([`crate::atc`]) is a different, simpler thing
//! and is untouched by this.

pub(crate) mod airfield;
pub(crate) mod airport;
pub(crate) mod carrier;
pub(crate) mod controller;
pub(crate) mod facility;
pub(crate) mod intent;
pub(crate) mod live;
pub(crate) mod llm;
pub(crate) mod rangectl;
pub(crate) mod speech;
pub(crate) mod tanker;
pub(crate) mod voices;

use crate::{
    voice::{srs, stt, FreqSpec, Radio},
    worker::host::EngineHost,
};
use airport::{Airport, BUILTIN_ANDERSEN};
use bfprotocols::{atc::AtcPicture, range::RangeLive};
use controller::{Caller, Controller, CtlCfg, Tx};
use facility::{Facility, Picture};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, VecDeque},
    path::PathBuf,
    sync::{Arc, Mutex, OnceLock},
    time::{Duration, Instant},
};
use voices::Cast;

/// A controlling position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum Pos {
    Atis,
    Clearance,
    Ground,
    Tower,
    Departure,
    Approach,
    Final,
    /// A carrier's Marshal: check-in and the holding stack.
    Marshal,
    /// A carrier's LSO. Talks on the carrier's approach frequency ("button").
    Paddles,
    /// A tanker's crew, on the tanker's frequency.
    Tanker,
    /// Range Control: who works which range when.
    Range,
}

impl Pos {
    /// An airfield's positions, in the order they are set up.
    pub(crate) const ALL: [Pos; 7] = [
        Pos::Atis,
        Pos::Clearance,
        Pos::Ground,
        Pos::Tower,
        Pos::Departure,
        Pos::Approach,
        Pos::Final,
    ];
    const EVERY: [Pos; 11] = [
        Pos::Atis,
        Pos::Clearance,
        Pos::Ground,
        Pos::Tower,
        Pos::Departure,
        Pos::Approach,
        Pos::Final,
        Pos::Marshal,
        Pos::Paddles,
        Pos::Tanker,
        Pos::Range,
    ];

    /// What the position calls itself after the field's name.
    pub(crate) fn title(self) -> &'static str {
        match self {
            Pos::Atis => "ATIS",
            Pos::Clearance => "Clearance",
            Pos::Ground => "Ground",
            Pos::Tower => "Tower",
            Pos::Departure => "Departure",
            Pos::Approach => "Approach",
            Pos::Final => "Final",
            Pos::Marshal => "Marshal",
            Pos::Paddles => "Paddles",
            Pos::Tanker => "",
            Pos::Range => "Range Control",
        }
    }

    pub(crate) fn key(self) -> &'static str {
        match self {
            Pos::Atis => "atis",
            Pos::Clearance => "clearance",
            Pos::Ground => "ground",
            Pos::Tower => "tower",
            Pos::Departure => "departure",
            Pos::Approach => "approach",
            Pos::Final => "final",
            Pos::Marshal => "marshal",
            Pos::Paddles => "paddles",
            Pos::Tanker => "tanker",
            Pos::Range => "range",
        }
    }

    fn from_key(s: &str) -> Option<Pos> {
        Pos::EVERY.into_iter().find(|p| p.key() == s)
    }
}

// ─── config ────────────────────────────────────────────────────────────────

fn d_true() -> bool {
    true
}
fn d_poll_ms() -> u64 {
    1000
}
fn d_gca() -> f64 {
    5.
}
fn d_atis() -> f64 {
    60.
}

/// One airfield to control.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct FieldCfg {
    /// An airfield file (see `scripts/atc_field_from_osm.py`)...
    #[serde(default)]
    pub data: Option<PathBuf>,
    /// ...or a built-in one: "andersen".
    #[serde(default)]
    pub builtin: Option<String>,
    /// Frequency overrides by position ("tower": [{"mhz": 340.2}]). An empty
    /// list switches a position off; its traffic goes to the next one along.
    #[serde(default)]
    pub positions: HashMap<Pos, Vec<FreqSpec>>,
}

/// A voice for one position, so Ground and Tower do not sound like the same
/// person. Falls back to the shared `piperModel` / `ttsVoice`.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct VoiceCfg {
    #[serde(default)]
    pub piper_model: Option<PathBuf>,
    #[serde(default)]
    pub tts_voice: Option<String>,
    /// A speaker of a multi-speaker Piper model.
    #[serde(default)]
    pub speaker: Option<u32>,
    /// Speaking speed, 1 = normal, 1.2 = a fifth faster.
    #[serde(default)]
    pub speed: Option<f32>,
    /// A voice of the speech service (`tts`): "onyx", "am_michael", or an
    /// ElevenLabs voice id.
    #[serde(default)]
    pub voice: Option<String>,
}

/// The `tts` block: a speech service for voices that sound like people.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct NeuralCfg {
    /// "openai", "elevenlabs", or "kokoro" (Kokoro-FastAPI on the box).
    pub engine: String,
    #[serde(default)]
    pub url: Option<String>,
    #[serde(default)]
    pub model: Option<String>,
    /// Prefer `BFDB_ATC_TTS_KEY` in the environment.
    #[serde(default)]
    pub key: Option<String>,
    /// The voices to cast from, busiest positions first. Default: the
    /// service's own.
    #[serde(default)]
    pub voices: Vec<String>,
    /// "always", or "audience": only while a player is on or someone is
    /// listening on the site, the local voices otherwise. Default: always for
    /// Kokoro, audience for the paid services.
    #[serde(default)]
    pub when: Option<String>,
    /// AI pilots get the service's voices too.
    #[serde(default = "d_true")]
    pub pilots: bool,
}

/// Who Departure hands released traffic to.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct HandoffCfg {
    pub name: String,
    pub mhz: f64,
}

/// The `rangeAtc` block of `gci.json`. SRS, EAM, opus, whisper and Piper come
/// from the rest of that file, shared with the other voice services.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct RangeAtcConfig {
    #[serde(default = "d_true")]
    pub enabled: bool,
    /// The SRS coalition the controllers sit on: "blue" (default) or "red".
    #[serde(default)]
    pub coalition: Option<String>,
    #[serde(default = "d_poll_ms")]
    pub poll_ms: u64,
    /// Empty: Andersen AFB.
    #[serde(default)]
    pub fields: Vec<FieldCfg>,
    #[serde(default)]
    pub llm: Option<llm::LlmCfg>,
    #[serde(default)]
    pub handoff_out: Option<HandoffCfg>,
    #[serde(default)]
    pub require_clearance: bool,
    #[serde(default = "d_gca")]
    pub gca_interval_secs: f64,
    #[serde(default = "d_atis")]
    pub atis_interval_secs: f64,
    #[serde(default)]
    pub voices: HashMap<Pos, VoiceCfg>,
    /// Put players' own transmissions on the range site's ATC radio page:
    /// the live feed and the replays (on by default). Off: their calls show
    /// as text only, and only the controllers and AI pilots can be heard.
    #[serde(default = "d_true")]
    pub publish_pilot_audio: bool,
    /// A speech service for the voices (default: Piper or SAPI, on the box).
    #[serde(default)]
    pub tts: Option<NeuralCfg>,
    /// Range Control (on by default, 348.6).
    #[serde(default)]
    pub range_control: Option<rangectl::RangeControlCfg>,
    /// Marshal, Approach and Paddles on every carrier.
    #[serde(default = "d_true")]
    pub carrier_atc: bool,
    /// Carrier frequency overrides, by the engine's carrier id. Default:
    /// Marshal on the carrier's own frequency, Approach 0.5 MHz above.
    #[serde(default)]
    pub carriers: HashMap<String, carrier::CarrierFreqCfg>,
    /// A crew voice on every tanker's frequency.
    #[serde(default = "d_true")]
    pub tanker_atc: bool,
}

fn load_airport(f: &FieldCfg) -> anyhow::Result<Airport> {
    let json = match (&f.data, f.builtin.as_deref()) {
        (Some(p), _) => std::fs::read_to_string(p)?,
        (None, None) | (None, Some("andersen")) => BUILTIN_ANDERSEN.to_string(),
        (None, Some(other)) => anyhow::bail!("no built-in airfield '{other}'"),
    };
    let mut ap = Airport::from_json(&json)?;
    for (pos, freqs) in &f.positions {
        ap.positions.insert(*pos, freqs.clone());
    }
    ap.positions.retain(|_, v| !v.is_empty());
    Ok(ap)
}

// ─── runtime ───────────────────────────────────────────────────────────────

/// One line of the radio transcript.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct Line {
    /// Unix milliseconds.
    pub t: i64,
    /// The station's id ("0:tower").
    pub station: String,
    /// Who spoke: the facility ("Andersen Tower") or the aircraft.
    pub who: String,
    pub text: String,
    /// A pilot (player or AI) rather than the controller.
    pub pilot: bool,
    pub ai: bool,
    /// The aircraft the transmission is from or to ("C-17").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub aircraft: Option<String>,
    /// Its audio, at `/api/range/atc/audio/<id>`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub audio: Option<u64>,
}

/// A frequency on the transcript page.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct StationInfo {
    pub id: String,
    pub field: String,
    pub position: Pos,
    pub name: String,
    /// What the site's tab says ("Tower", "Lincoln Marshal", "Texaco 1-1").
    pub label: String,
    pub freq: String,
}

#[derive(Default)]
struct Board {
    stations: Vec<StationInfo>,
    lines: VecDeque<Line>,
    /// Recent transmissions as WAV, newest last, for listening on the web.
    audio: VecDeque<(u64, Arc<Vec<u8>>)>,
    audio_bytes: usize,
}

/// How much transmission audio to keep per instance: about 25 minutes of
/// speech at 16 kHz mono.
const AUDIO_KEEP_BYTES: usize = 48 << 20;

/// 16 kHz mono PCM as a WAV file.
fn wav(pcm: &[i16]) -> Vec<u8> {
    let spec = hound::WavSpec {
        channels: 1,
        sample_rate: crate::voice::tts::SRS_RATE,
        bits_per_sample: 16,
        sample_format: hound::SampleFormat::Int,
    };
    let mut c = std::io::Cursor::new(Vec::with_capacity(44 + pcm.len() * 2));
    if let Ok(mut w) = hound::WavWriter::new(&mut c, spec) {
        for s in pcm {
            let _ = w.write_sample(*s);
        }
        let _ = w.finalize();
    }
    c.into_inner()
}

/// Keep a transmission's audio; returns its id. Ids are unique across bfdb
/// restarts (millisecond clock plus a counter), so a browser's cached copy of
/// an old id can never be a different transmission.
fn store_audio(inst: &str, pcm: &[i16]) -> Option<u64> {
    if pcm.is_empty() {
        return None;
    }
    static SEQ: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let n = SEQ.fetch_add(1, std::sync::atomic::Ordering::Relaxed) & 0xff;
    let id = ((now_ms().max(0) as u64) << 8) | n;
    let bytes = Arc::new(wav(pcm));
    let mut b = boards().lock().ok()?;
    let board = b.entry(inst.to_string()).or_default();
    board.audio_bytes += bytes.len();
    board.audio.push_back((id, bytes));
    while board.audio_bytes > AUDIO_KEEP_BYTES {
        match board.audio.pop_front() {
            Some((_, a)) => board.audio_bytes -= a.len(),
            None => break,
        }
    }
    Some(id)
}

/// A kept transmission, as WAV.
pub(crate) fn audio_wav(inst: &str, id: u64) -> Option<Arc<Vec<u8>>> {
    let b = boards().lock().ok()?;
    b.get(inst)?.audio.iter().find(|(i, _)| *i == id).map(|(_, a)| a.clone())
}

/// Every range instance's transcript, for `/api/range/atc`.
fn boards() -> &'static Mutex<HashMap<String, Board>> {
    static B: OnceLock<Mutex<HashMap<String, Board>>> = OnceLock::new();
    B.get_or_init(Default::default)
}

fn note(inst: &str, line: Line) {
    log::info!("milatc [{}] {}: {}", line.station, line.who, line.text);
    if let Ok(mut b) = boards().lock() {
        let board = b.entry(inst.to_string()).or_default();
        board.lines.push_back(line);
        while board.lines.len() > 600 {
            board.lines.pop_front();
        }
    }
}

/// The transcript for the web: the stations and the lines newer than
/// `since` (unix ms), oldest first.
pub(crate) fn transcript_json(inst: &str, since: i64) -> serde_json::Value {
    let b = boards().lock().unwrap();
    match b.get(inst) {
        Some(board) => serde_json::json!({
            "stations": board.stations,
            "lines": board.lines.iter().filter(|l| l.t > since).collect::<Vec<_>>(),
        }),
        None => serde_json::json!({ "stations": [], "lines": [] }),
    }
}

fn now_ms() -> i64 {
    chrono::Utc::now().timestamp_millis()
}

/// One facility and its radio plumbing.
struct Station {
    /// Index for station ids; never reused while bfdb runs.
    index: usize,
    /// "airfield:Andersen AFB", "carrier:cvn72", "tanker:texaco", "range".
    key: String,
    ctl: Mutex<Box<dyn Facility>>,
    /// Per position: its transmit queue. Dropping the station ends the
    /// transmitters.
    queues: HashMap<Pos, tokio::sync::mpsc::UnboundedSender<Tx>>,
    /// Which pool client carries its radios.
    client: usize,
}

/// Radio labels carry "station index:position" so an inbound transmission
/// routes straight to its controller.
fn label(index: usize, pos: Pos) -> String {
    format!("{index}:{}", pos.key())
}

fn parse_label(l: &str) -> Option<(usize, Pos)> {
    let (a, b) = l.split_once(':')?;
    Some((a.parse().ok()?, Pos::from_key(b)?))
}

/// SRS clients shared by all the facilities. SRS allows 11 radios a client
/// and a client cannot be shut down, so facilities that come and go (an
/// on-demand tanker) have their radios added to and taken off a client
/// rather than getting their own.
struct Pool {
    clients: Vec<(srs::SrsClient, Vec<Radio>)>,
}

const RADIOS_PER_CLIENT: usize = 10;

impl Pool {
    fn sync(c: &srs::SrsClient, radios: &[Radio]) {
        if radios.is_empty() {
            // a client must carry a radio: park it somewhere nobody is
            c.set_radios(vec![Radio::new(1.0, 0, "idle")]);
        } else {
            c.set_radios(radios.to_vec());
        }
    }
}

/// Everything the runtime shares between the facilities.
struct Ctx {
    inst_id: String,
    gci: crate::gci::GciConfig,
    coalition: u8,
    eam: String,
    opus: Option<Arc<srs::Opus>>,
    rx_tx: Option<tokio::sync::mpsc::UnboundedSender<srs::Transmission>>,
    cast: Arc<Cast>,
    engine: Engine,
    pool: Mutex<Pool>,
    stations: std::sync::RwLock<Vec<Arc<Station>>>,
    next_index: std::sync::atomic::AtomicUsize,
    start: Instant,
    /// A player is in the air picture (someone may be listening on SRS).
    players_on: std::sync::atomic::AtomicBool,
    /// Players' own voices go on the site's feed.
    publish_pilot_audio: bool,
}

impl Ctx {
    fn clock(&self) -> f64 {
        self.start.elapsed().as_secs_f64()
    }

    fn station(&self, index: usize) -> Option<Arc<Station>> {
        self.stations.read().unwrap().iter().find(|s| s.index == index).cloned()
    }

    fn has(&self, key: &str) -> bool {
        self.stations.read().unwrap().iter().any(|s| s.key == key)
    }

    /// Bring a facility on the air: its radios on a pool client, a
    /// transmitter per position, and its frequencies on the site's tabs.
    fn add(self: &Arc<Self>, key: String, f: Box<dyn Facility>) {
        let index = self.next_index.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let mut radios: HashMap<Pos, Vec<Radio>> = HashMap::new();
        for (pos, specs) in f.radios() {
            let title = f.facility(pos);
            let rs: Vec<Radio> = specs
                .iter()
                .map(|s| {
                    let mut r = s.to_radio(&title);
                    r.label = label(index, pos);
                    r
                })
                .collect();
            if !rs.is_empty() {
                radios.insert(pos, rs);
            }
        }
        let all: Vec<Radio> = radios.values().flatten().cloned().collect();
        // a pool client with room, or a new one
        let (ci, client) = {
            let mut pool = self.pool.lock().unwrap();
            let ci = match pool
                .clients
                .iter()
                .position(|(_, r)| r.len() + all.len() <= RADIOS_PER_CLIENT)
            {
                Some(i) => i,
                None => {
                    let n = pool.clients.len();
                    let name = if n == 0 {
                        "Range ATC".to_string()
                    } else {
                        format!("Range ATC {}", n + 1)
                    };
                    match srs::SrsClient::start(
                        &self.gci.srs_host,
                        self.gci.srs_port,
                        self.coalition,
                        &name,
                        all.clone(),
                        &self.eam,
                        self.opus.clone(),
                        self.rx_tx.clone(),
                    ) {
                        Ok(c) => {
                            // players' voices onto the site's live feed as they talk
                            if self.publish_pilot_audio {
                                let inst = self.inst_id.clone();
                                c.set_live(Arc::new(move |v| live::stream(&inst, v)));
                            }
                            pool.clients.push((c, vec![]));
                            n
                        }
                        Err(e) => {
                            log::error!(
                                "[{}] range ATC: SRS client '{name}' failed: {e:#}",
                                self.inst_id
                            );
                            return;
                        }
                    }
                }
            };
            let (c, rs) = &mut pool.clients[ci];
            rs.extend(all.iter().cloned());
            Pool::sync(c, rs);
            (ci, c.clone())
        };
        let mut queues = HashMap::new();
        let mut infos = vec![];
        for (pos, rs) in &radios {
            let (qtx, qrx) = tokio::sync::mpsc::unbounded_channel::<Tx>();
            queues.insert(*pos, qtx);
            let me = Speaker {
                station: label(index, *pos),
                name: f.facility(*pos),
                pos: *pos,
                inst: self.inst_id.clone(),
                salt: index as u64,
            };
            tokio::spawn(transmitter(
                client.clone(),
                rs.clone(),
                self.cast.clone(),
                qrx,
                self.clone(),
                me,
            ));
            infos.push(StationInfo {
                id: label(index, *pos),
                field: f.name(),
                position: *pos,
                name: f.facility(*pos),
                label: f.label(*pos),
                freq: rs
                    .first()
                    .map(|r| speech::freq(r.freq_hz / 1e6))
                    .unwrap_or_default(),
            });
        }
        // keep the airfield's positions in their natural order on the site
        infos.sort_by_key(|i| Pos::EVERY.iter().position(|p| *p == i.position));
        log::info!(
            "[{}] range ATC: {} on the air: {}",
            self.inst_id,
            f.name(),
            infos
                .iter()
                .map(|i| format!("{} {}", i.name, i.freq))
                .collect::<Vec<_>>()
                .join(", ")
        );
        if let Ok(mut b) = boards().lock() {
            b.entry(self.inst_id.clone()).or_default().stations.extend(infos);
        }
        self.stations.write().unwrap().push(Arc::new(Station {
            index,
            key,
            ctl: Mutex::new(f),
            queues,
            client: ci,
        }));
    }

    /// Take a facility off the air.
    fn remove(&self, index: usize) {
        let st = {
            let mut v = self.stations.write().unwrap();
            let Some(i) = v.iter().position(|s| s.index == index) else { return };
            v.remove(i)
        };
        let prefix = format!("{index}:");
        {
            let mut pool = self.pool.lock().unwrap();
            if let Some((c, rs)) = pool.clients.get_mut(st.client) {
                rs.retain(|r| !r.label.starts_with(&prefix));
                Pool::sync(c, rs);
            }
        }
        if let Ok(mut b) = boards().lock() {
            if let Some(board) = b.get_mut(&self.inst_id) {
                board.stations.retain(|s| !s.id.starts_with(&prefix));
            }
        }
        log::info!(
            "[{}] range ATC: {} off the air",
            self.inst_id,
            st.ctl.lock().unwrap().name()
        );
    }
}

/// Run the range's controllers for one instance until the process exits.
pub(crate) async fn run(
    inst: EngineHost,
    gci: crate::gci::GciConfig,
    cfg: RangeAtcConfig,
) {
    if !cfg.enabled {
        return;
    }
    let fields = if cfg.fields.is_empty() {
        vec![FieldCfg::default()]
    } else {
        cfg.fields.clone()
    };
    let ctl_cfg = CtlCfg {
        require_clearance: cfg.require_clearance,
        handoff_out: cfg.handoff_out.as_ref().map(|h| (h.name.clone(), h.mhz)),
        gca_interval_s: cfg.gca_interval_secs.max(2.),
        atis_interval_s: cfg.atis_interval_secs.max(20.),
    };
    let coalition: u8 = match cfg.coalition.as_deref() {
        Some(c) if c.eq_ignore_ascii_case("red") => 1,
        _ => 2,
    };
    let eam = if coalition == 1 {
        gci.red_eam_password.clone()
    } else {
        gci.blue_eam_password.clone()
    };
    let opus = match srs::Opus::load(gci.opus_dll_path.as_deref()) {
        Ok(o) => Some(Arc::new(o)),
        Err(e) => {
            log::error!("[{}] range ATC: could not load opus.dll ({e:#}) — controllers cannot transmit", inst.id);
            None
        }
    };
    let stt = stt::Stt::from_cfg(
        gci.whisper_exe.as_ref(),
        gci.whisper_model.as_ref(),
        gci.whisper_server_url.as_deref(),
    );
    let llm = llm::Llm::from_cfg(cfg.llm.as_ref());
    let (rx_tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<srs::Transmission>();
    // Who sounds like whom (reads model folders / asks Windows for voices).
    let cast = {
        let (g, v, n) = (gci.clone(), cfg.voices.clone(), cfg.tts.clone());
        match tokio::task::spawn_blocking(move || Cast::build(&g, &v, n.as_ref())).await {
            Ok(c) => Arc::new(c),
            Err(e) => {
                log::error!("[{}] range ATC: voice casting failed: {e}", inst.id);
                return;
            }
        }
    };
    log::info!(
        "[{}] range ATC: voices: {}{}{}",
        inst.id,
        cast.describe(),
        match &stt {
            Some(s) => format!("; listening via {}", s.describe()),
            None => "; broadcast only (no whisper configured)".into(),
        },
        match &llm {
            Some(l) => format!("; language model {}", l.describe()),
            None => String::new(),
        }
    );
    let ctx = Arc::new(Ctx {
        inst_id: inst.id.to_string(),
        gci: gci.clone(),
        coalition,
        eam,
        opus,
        rx_tx: stt.as_ref().map(|_| rx_tx.clone()),
        cast,
        engine: Engine { inst: inst.clone() },
        pool: Mutex::new(Pool { clients: vec![] }),
        stations: std::sync::RwLock::new(vec![]),
        next_index: std::sync::atomic::AtomicUsize::new(0),
        start: Instant::now(),
        players_on: std::sync::atomic::AtomicBool::new(false),
        publish_pilot_audio: cfg.publish_pilot_audio,
    });

    // The airfields, and Range Control, are on the air from the start.
    for f in &fields {
        match load_airport(f) {
            Ok(ap) => {
                let key = format!("airfield:{}", ap.dcs_name);
                ctx.add(
                    key,
                    Box::new(airfield::Airfield::new(
                        Controller::new(ap, ctl_cfg.clone()),
                        &inst.id,
                    )),
                );
            }
            Err(e) => log::error!(
                "[{}] range ATC: airfield {:?} not loaded: {e:#}",
                inst.id,
                f.data.as_ref().or(None::<&PathBuf>)
            ),
        }
    }
    let range_cfg = cfg.range_control.clone().unwrap_or_default();
    if range_cfg.enabled {
        ctx.add("range".into(), Box::new(rangectl::RangeControl::new(range_cfg)));
    }

    // Heard transmissions.
    if let Some(stt) = stt.clone() {
        let ctx = ctx.clone();
        let llm = llm.clone();
        let publish_pilot_audio = cfg.publish_pilot_audio;
        tokio::spawn(async move {
            while let Some(mut tr) = rx.recv().await {
                let Some((index, pos)) = parse_label(&tr.label) else { continue };
                let Some(st) = ctx.station(index) else { continue };
                if pos == Pos::Atis {
                    continue;
                }
                let pcm = std::mem::take(&mut tr.pcm);
                // (already streamed live from SRS as it was said; kept for replays)
                let keep = publish_pilot_audio.then(|| pcm.clone());
                let stt = stt.clone();
                let llm = llm.clone();
                let ctx = ctx.clone();
                tokio::spawn(async move {
                    let s = stt.clone();
                    let heard =
                        match tokio::task::spawn_blocking(move || s.transcribe(&pcm))
                            .await
                        {
                            Ok(Ok(t)) if !t.trim().is_empty() => t,
                            Ok(Err(e)) => {
                                log::debug!("milatc: whisper failed: {e:#}");
                                return;
                            }
                            _ => return,
                        };
                    let audio = keep.and_then(|p| store_audio(&ctx.inst_id, &p));
                    heard_call(&ctx, st, pos, tr.unit_id, tr.name, heard, llm, audio)
                        .await;
                });
            }
        });
    }

    // The pictures.
    let mut ticker = tokio::time::interval(Duration::from_millis(cfg.poll_ms.max(250)));
    ticker.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
    let mut last_vocab = f64::NEG_INFINITY;
    let mut last_range = f64::NEG_INFINITY;
    let mut range: Option<RangeLive> = None;
    let mut fails = 0u32;
    loop {
        ticker.tick().await;
        let atc = match fetch(&inst).await {
            Ok(p) => {
                fails = 0;
                p
            }
            Err(e) => {
                fails += 1;
                if fails == 10 {
                    log::warn!(
                        "[{}] range ATC: no picture from the engine: {e}",
                        inst.id
                    );
                }
                continue;
            }
        };
        let now = ctx.clock();
        ctx.players_on.store(
            atc.traffic.iter().any(|t| !t.ai),
            std::sync::atomic::Ordering::Relaxed,
        );
        // carriers, tankers and range stations change slowly: every 2 s
        if now - last_range >= 2. {
            last_range = now;
            match fetch_range(&inst).await {
                Ok(r) => range = Some(r),
                Err(e) => log::debug!("[{}] range ATC: no range picture: {e}", inst.id),
            }
        }

        // Carriers and tankers come on the air as the engine reports them.
        if let Some(r) = &range {
            if cfg.carrier_atc {
                for c in &r.carriers {
                    let key = format!("carrier:{}", c.id);
                    if !ctx.has(&key) {
                        ctx.add(
                            key,
                            Box::new(carrier::Carrier::new(c, cfg.carriers.get(&c.id))),
                        );
                    }
                }
            }
            if cfg.tanker_atc {
                for t in &r.tankers {
                    let key = format!("tanker:{}", t.id);
                    if t.state == bfprotocols::range::TankerState::OnStation
                        && t.freq_mhz > 0.
                        && !ctx.has(&key)
                    {
                        ctx.add(key, Box::new(tanker::Tanker::new(t)));
                    }
                }
            }
        }

        let pic = Picture {
            atc: &atc,
            range: range.as_ref(),
            now,
            mission_s: range.as_ref().and_then(facility::mission_seconds),
        };
        let stations: Vec<Arc<Station>> = ctx.stations.read().unwrap().clone();
        let mut gone = vec![];
        for st in &stations {
            let out = {
                let mut ctl = st.ctl.lock().unwrap();
                let out = ctl.update(&pic);
                if ctl.gone() {
                    gone.push(st.index);
                }
                out
            };
            dispatch(st, out);
        }
        for i in gone {
            ctx.remove(i);
        }

        // Keep whisper primed with what it will hear.
        if now - last_vocab > 30. {
            last_vocab = now;
            if let Some(stt) = &stt {
                let mut words: Vec<String> = vec![];
                for st in &stations {
                    words.extend(st.ctl.lock().unwrap().vocabulary());
                }
                words.extend(
                    [
                        "request taxi",
                        "holding short",
                        "ready for departure",
                        "request clearance",
                        "initial",
                        "break",
                        "base, gear down",
                        "request PAR",
                        "request ILS",
                        "full stop",
                        "touch and go",
                        "low approach",
                        "closed",
                        "clear of the runway",
                        "squawk",
                        "Alpha",
                        "Bravo",
                        "Charlie",
                        "Delta",
                        "Echo",
                        "Foxtrot",
                        "Golf",
                        "Hotel",
                        "Kilo",
                    ]
                    .map(String::from),
                );
                stt.set_vocabulary(&words);
            }
        }
    }
}

/// Process one recognised transmission: rules first, the language model for
/// what the rules miss, then the facility.
#[allow(clippy::too_many_arguments)]
async fn heard_call(
    ctx: &Ctx,
    st: Arc<Station>,
    pos: Pos,
    unit_id: Option<u64>,
    srs_name: String,
    heard: String,
    llm: Option<llm::Llm>,
    audio: Option<u64>,
) {
    let caller = Caller { unit_id, srs_name: srs_name.clone() };
    let wake = st.ctl.lock().unwrap().wake();
    let mut parsed = intent::parse(&heard, &wake);
    // The transcript names the flight, and its type, when we know them.
    let (who, aircraft) = st.ctl.lock().unwrap().caller(&caller, parsed.as_ref());
    note(
        &ctx.inst_id,
        Line {
            t: now_ms(),
            station: label(st.index, pos),
            who: who.unwrap_or_else(|| srs_name.clone()),
            text: heard.clone(),
            pilot: true,
            ai: false,
            aircraft,
            audio,
        },
    );
    if parsed.is_none() {
        if let Some(llm) = llm {
            let c = st.ctl.lock().unwrap().llm_context(pos, &caller, &heard);
            match tokio::task::spawn_blocking(move || llm.classify(&c)).await {
                Ok(Ok(p)) => {
                    log::info!("milatc: model read \"{heard}\" as {:?}", p.intent);
                    parsed = Some(p);
                }
                Ok(Err(e)) => log::warn!("milatc: language model failed: {e:#}"),
                Err(_) => {}
            }
        }
    }
    let out = st.ctl.lock().unwrap().respond(pos, &caller, &heard, parsed, ctx.clock());
    dispatch(&st, out);
}

fn dispatch(st: &Station, out: Vec<Tx>) {
    for tx in out {
        match st.queues.get(&tx.pos) {
            Some(q) => {
                let _ = q.send(tx);
            }
            None => log::debug!("milatc: no transmitter for {:?}: {}", tx.pos, tx.text),
        }
    }
}

/// The range engine, for the controllers' commands back to DCS.
#[derive(Clone)]
struct Engine {
    inst: EngineHost,
}

impl Engine {
    async fn command(&self, action: &str, target: &str, text: &str) {
        let r = tokio::time::timeout(
            Duration::from_secs(5),
            self.inst.rpc_str(
                "range-atc",
                vec![
                    ("action", netidx::publisher::Value::from(action.to_string())),
                    ("target", netidx::publisher::Value::from(target.to_string())),
                    ("text", netidx::publisher::Value::from(text.to_string())),
                ],
            ),
        )
        .await;
        match r {
            Ok(Ok(_)) => {}
            Ok(Err(e)) => log::warn!("milatc: range-atc {action} {target}: {e:?}"),
            Err(_) => log::warn!("milatc: range-atc {action} {target}: timed out"),
        }
    }
}

/// Which frequency a transmitter speaks for.
struct Speaker {
    station: String,
    name: String,
    pos: Pos,
    inst: String,
    /// Tells apart facilities sharing a position (two tankers' crews).
    salt: u64,
}

/// One position's transmitter. One utterance at a time; a human pause
/// before each (counted from when the frequency last went quiet, so a reply
/// follows the call it answers the way a person's would); never over a pilot
/// (the SRS client waits for a clear frequency); stale calls dropped. AI
/// pilots, and the LSO on a carrier's approach frequency, speak through here
/// too, in their own voices.
async fn transmitter(
    client: srs::SrsClient,
    radios: Vec<Radio>,
    cast: Arc<Cast>,
    mut q: tokio::sync::mpsc::UnboundedReceiver<Tx>,
    ctx: Arc<Ctx>,
    me: Speaker,
) {
    let mut quiet_since = Instant::now();
    while let Some(tx) = q.recv().await {
        let got = Instant::now();
        if ctx.clock() > tx.expires {
            log::debug!("milatc: dropped stale {} call: {}", me.name, tx.text);
            continue;
        }
        let voice = match (&tx.pilot, tx.voice) {
            (Some(_), _) => cast.pilot(tx.to.unwrap_or(0)),
            (None, Some(v)) => cast.controller(v),
            // every tanker crew sounds different
            (None, None) if me.pos == Pos::Tanker => {
                cast.pilot(me.salt.wrapping_mul(0x5bd1_e995) ^ 0xa5a5)
            }
            (None, None) => cast.controller(me.pos),
        };
        // A paid speech service only speaks for an audience.
        let heard = cast.always_neural
            || live::listening(&me.inst)
            || ctx.players_on.load(std::sync::atomic::Ordering::Relaxed);
        let voice = if heard { voice } else { voice.local().clone() };
        let who = match (&tx.pilot, tx.voice) {
            (Some(p), _) => p.clone(),
            (None, Some(Pos::Paddles)) => "Paddles".to_string(),
            _ => me.name.clone(),
        };
        // Synthesis overlaps the pause.
        let spoken = speech::speak(&tx.text);
        let pcm =
            match tokio::task::spawn_blocking(move || voice.synthesize(&spoken)).await {
                Ok(Ok(p)) => Some(p),
                Ok(Err(e)) => {
                    log::warn!("milatc: {} speech synthesis failed: {e:#}", me.name);
                    None
                }
                Err(_) => None,
            };
        let from = quiet_since.max(got);
        let due = from + Duration::from_secs_f64(tx.delay.clamp(0., 6.));
        tokio::time::sleep_until(tokio::time::Instant::from_std(due)).await;
        let audio = pcm.as_deref().and_then(|p| store_audio(&me.inst, p));
        note(
            &me.inst,
            Line {
                t: now_ms(),
                station: me.station.clone(),
                who: who.clone(),
                text: tx.text.clone(),
                pilot: tx.pilot.is_some(),
                ai: tx.pilot.is_some(),
                aircraft: tx.aircraft.clone(),
                audio,
            },
        );
        if let Some(p) = &pcm {
            live::play(&me.inst, &me.station, &who, &tx.text, p);
        }
        if let (Some(pcm), true) = (pcm, client.is_connected()) {
            let client = client.clone();
            let radios = radios.clone();
            let res =
                tokio::task::spawn_blocking(move || client.transmit(&radios, &pcm)).await;
            if let Ok(Err(e)) = res {
                log::warn!("milatc: {} transmit failed: {e:#}", me.name);
            }
        }
        quiet_since = Instant::now();
        if let Some(unit) = tx.subtitle {
            let e = ctx.engine.clone();
            let text = format!("{}: {}", who.to_uppercase(), tx.text);
            tokio::spawn(
                async move { e.command("text", &unit.to_string(), &text).await },
            );
        }
        if let Some((group, action)) = &tx.action {
            ctx.engine.command(action, group, "").await;
        }
    }
}

async fn fetch_range(inst: &EngineHost) -> Result<RangeLive, String> {
    let res =
        tokio::time::timeout(Duration::from_secs(4), inst.rpc_str("query-range", vec![]))
            .await;
    match res {
        Ok(Ok(json)) => serde_json::from_str::<RangeLive>(&json)
            .map_err(|e| format!("bad query-range JSON: {e}")),
        Ok(Err(e)) => Err(format!("query-range RPC error: {e:?}")),
        Err(_) => Err("query-range timed out".to_string()),
    }
}

async fn fetch(inst: &EngineHost) -> Result<AtcPicture, String> {
    let res = tokio::time::timeout(
        Duration::from_secs(4),
        inst.rpc_str("query-atc", vec![("side", netidx::publisher::Value::from("all"))]),
    )
    .await;
    match res {
        Ok(Ok(json)) => serde_json::from_str::<AtcPicture>(&json)
            .map_err(|e| format!("bad query-atc JSON: {e}")),
        Ok(Err(e)) => Err(format!("query-atc RPC error: {e:?}")),
        Err(_) => Err("query-atc timed out".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn config_defaults_and_labels() {
        let c: RangeAtcConfig = serde_json::from_str("{}").unwrap();
        assert!(c.enabled && c.fields.is_empty() && c.poll_ms == 1000);
        let c: RangeAtcConfig = serde_json::from_str(
            r#"{"fields":[{"builtin":"andersen","positions":{"final":[],"tower":[{"mhz":251.1}]}}],"voices":{"tower":{"ttsVoice":"x"}},"handoffOut":{"name":"Overlord","mhz":251.5}}"#,
        )
        .unwrap();
        let ap = load_airport(&c.fields[0]).unwrap();
        assert!(!ap.positions.contains_key(&Pos::Final));
        assert_eq!(ap.positions[&Pos::Tower][0].mhz, 251.1);
        assert_eq!(parse_label(&label(0, Pos::Ground)), Some((0, Pos::Ground)));
    }

    #[test]
    fn audio_is_kept_as_wav_and_bounded() {
        let inst = "test-audio";
        let pcm = vec![1000i16; 16_000]; // a second
        let id = store_audio(inst, &pcm).unwrap();
        let w = audio_wav(inst, id).unwrap();
        assert_eq!(&w[..4], b"RIFF");
        assert_eq!(w.len(), 44 + 32_000);
        assert!(audio_wav(inst, id + 1).is_none());
        // keeping far more than the cap drops the oldest first
        let big = vec![0i16; 8_000_000]; // 16 MB each
        for _ in 0..5 {
            store_audio(inst, &big);
        }
        assert!(audio_wav(inst, id).is_none(), "the first clip aged out");
        let b = boards().lock().unwrap();
        assert!(b[inst].audio_bytes <= AUDIO_KEEP_BYTES);
    }
}
