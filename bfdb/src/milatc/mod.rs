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
//!
//! This module is the plumbing: it polls the range engine's `query-atc`
//! picture once a second, listens on every position's frequency through the
//! shared voice bus ([`crate::voice`]), runs heard calls through whisper and
//! the parsers, and speaks the controllers' replies with Piper.
//!
//! The campaign's spoken tower ([`crate::atc`]) is a different, simpler thing
//! and is untouched by this.

pub(crate) mod airport;
pub(crate) mod controller;
pub(crate) mod intent;
pub(crate) mod llm;
pub(crate) mod speech;
pub(crate) mod voices;

use crate::{
    db::StatsDb,
    voice::{srs, stt, FreqSpec, Radio},
};
use airport::{Airport, P2, BUILTIN_ANDERSEN};
use bfprotocols::atc::AtcPicture;
use controller::{Caller, Controller, CtlCfg, Tx};
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
}

impl Pos {
    pub(crate) const ALL: [Pos; 7] =
        [Pos::Atis, Pos::Clearance, Pos::Ground, Pos::Tower, Pos::Departure, Pos::Approach, Pos::Final];

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
        }
    }

    fn key(self) -> &'static str {
        match self {
            Pos::Atis => "atis",
            Pos::Clearance => "clearance",
            Pos::Ground => "ground",
            Pos::Tower => "tower",
            Pos::Departure => "departure",
            Pos::Approach => "approach",
            Pos::Final => "final",
        }
    }

    fn from_key(s: &str) -> Option<Pos> {
        Pos::ALL.into_iter().find(|p| p.key() == s)
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
}

/// A frequency on the transcript page.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct StationInfo {
    pub id: String,
    pub field: String,
    pub position: Pos,
    pub name: String,
    pub freq: String,
}

#[derive(Default)]
struct Board {
    stations: Vec<StationInfo>,
    lines: VecDeque<Line>,
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

struct Station {
    /// Index into the instance's fields, for station ids.
    index: usize,
    ctl: Mutex<Controller>,
    /// Per position: its transmit queue.
    queues: HashMap<Pos, tokio::sync::mpsc::UnboundedSender<Tx>>,
    /// Logged once: the field is not in the engine's picture.
    missing_logged: Mutex<bool>,
    /// Aligning the taxi network to DCS was tried (and failed, if the airport
    /// is still unaligned): do not retry every second.
    align_tried: Mutex<bool>,
}

/// Radio labels carry "field index:position" so an inbound transmission
/// routes straight to its controller.
fn label(field: usize, pos: Pos) -> String {
    format!("{field}:{}", pos.key())
}

fn parse_label(l: &str) -> Option<(usize, Pos)> {
    let (a, b) = l.split_once(':')?;
    Some((a.parse().ok()?, Pos::from_key(b)?))
}

/// Run the range's controllers for one instance until the process exits.
pub(crate) async fn run(db: StatsDb, inst: crate::Inst, gci: crate::gci::GciConfig, cfg: RangeAtcConfig) {
    if !cfg.enabled {
        return;
    }
    let fields = if cfg.fields.is_empty() { vec![FieldCfg::default()] } else { cfg.fields.clone() };
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
    let eam = if coalition == 1 { gci.red_eam_password.clone() } else { gci.blue_eam_password.clone() };

    // Airfields and their radios.
    let mut airports = vec![];
    for f in &fields {
        match load_airport(f) {
            Ok(ap) => airports.push(ap),
            Err(e) => log::error!("[{}] range ATC: airfield {:?} not loaded: {e:#}", inst.id, f.data.as_ref().or(None::<&PathBuf>)),
        }
    }
    if airports.is_empty() {
        log::error!("[{}] range ATC: no airfields, not starting", inst.id);
        return;
    }

    let opus = match srs::Opus::load(gci.opus_dll_path.as_deref()) {
        Ok(o) => Some(Arc::new(o)),
        Err(e) => {
            log::error!("[{}] range ATC: could not load opus.dll ({e:#}) — controllers cannot transmit", inst.id);
            None
        }
    };
    let stt = stt::Stt::from_cfg(gci.whisper_exe.as_ref(), gci.whisper_model.as_ref(), gci.whisper_server_url.as_deref());
    let llm = llm::Llm::from_cfg(cfg.llm.as_ref());
    let (rx_tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<srs::Transmission>();

    let start = Instant::now();
    let clock = move || start.elapsed().as_secs_f64();
    // Who sounds like whom (reads model folders / asks Windows for voices).
    let cast = {
        let (g, v) = (gci.clone(), cfg.voices.clone());
        match tokio::task::spawn_blocking(move || Cast::build(&g, &v)).await {
            Ok(c) => Arc::new(c),
            Err(e) => {
                log::error!("[{}] range ATC: voice casting failed: {e}", inst.id);
                return;
            }
        }
    };
    let engine = Engine { db: db.clone(), inst: inst.clone() };
    let inst_id = inst.id.to_string();
    let mut board_stations = vec![];

    let mut stations: Vec<Arc<Station>> = vec![];
    let mut clients: Vec<srs::SrsClient> = vec![];
    for (fi, ap) in airports.into_iter().enumerate() {
        let mut radios: HashMap<Pos, Vec<Radio>> = HashMap::new();
        for pos in Pos::ALL {
            let Some(specs) = ap.positions.get(&pos) else { continue };
            let title = format!("{} {}", ap.callsign, pos.title());
            let rs: Vec<Radio> = specs
                .iter()
                .map(|s| {
                    let mut r = s.to_radio(&title);
                    r.label = label(fi, pos);
                    r
                })
                .collect();
            radios.insert(pos, rs);
        }
        // SRS carries at most 11 radios per client: split if needed.
        let all: Vec<Radio> = Pos::ALL.iter().filter_map(|p| radios.get(p)).flatten().cloned().collect();
        let mut field_clients = vec![];
        for (ci, chunk) in all.chunks(10).enumerate() {
            let name = if ci == 0 { format!("{} ATC", ap.callsign) } else { format!("{} ATC {}", ap.callsign, ci + 1) };
            // ATIS is broadcast only: nothing heard on it needs whisper.
            match srs::SrsClient::start(&gci.srs_host, gci.srs_port, coalition, &name, chunk.to_vec(), &eam, opus.clone(), stt.as_ref().map(|_| rx_tx.clone())) {
                Ok(c) => field_clients.push((chunk.to_vec(), c)),
                Err(e) => log::error!("[{}] range ATC: SRS client '{name}' failed: {e:#}", inst.id),
            }
        }
        let mut queues = HashMap::new();
        for (pos, rs) in &radios {
            let Some((_, client)) = field_clients.iter().find(|(chunk, _)| chunk.iter().any(|r| rs.iter().any(|x| x.key() == r.key())))
            else {
                continue;
            };
            let (qtx, qrx) = tokio::sync::mpsc::unbounded_channel::<Tx>();
            queues.insert(*pos, qtx);
            let me = Speaker {
                station: label(fi, *pos),
                name: format!("{} {}", ap.callsign, pos.title()),
                pos: *pos,
                inst: inst_id.clone(),
            };
            tokio::spawn(transmitter(client.clone(), rs.clone(), cast.clone(), qrx, clock, me, engine.clone()));
        }
        for p in Pos::ALL {
            if let Some(r) = radios.get(&p).and_then(|r| r.first()) {
                board_stations.push(StationInfo {
                    id: label(fi, p),
                    field: ap.callsign.clone(),
                    position: p,
                    name: format!("{} {}", ap.callsign, p.title()),
                    freq: speech::freq(r.freq_hz / 1e6),
                });
            }
        }
        clients.extend(field_clients.into_iter().map(|(_, c)| c));
        log::info!(
            "[{}] range ATC: {} ({}) on {}; voices: {}{}{}",
            inst.id,
            ap.callsign,
            ap.icao,
            Pos::ALL
                .iter()
                .filter_map(|p| radios.get(p).and_then(|r| r.first()).map(|r| format!("{} {}", p.title(), r.describe())))
                .collect::<Vec<_>>()
                .join(", "),
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
        stations.push(Arc::new(Station {
            index: fi,
            ctl: Mutex::new(Controller::new(ap, ctl_cfg.clone())),
            queues,
            missing_logged: Mutex::new(false),
            align_tried: Mutex::new(false),
        }));
    }
    let stations = Arc::new(stations);
    if let Ok(mut b) = boards().lock() {
        b.entry(inst_id.clone()).or_default().stations = board_stations;
    }

    // Heard transmissions.
    if let Some(stt) = stt.clone() {
        let stations = stations.clone();
        let llm = llm.clone();
        let inst_id = inst_id.clone();
        tokio::spawn(async move {
            while let Some(mut tr) = rx.recv().await {
                let Some((fi, pos)) = parse_label(&tr.label) else { continue };
                let Some(st) = stations.get(fi).cloned() else { continue };
                if pos == Pos::Atis {
                    continue;
                }
                let pcm = std::mem::take(&mut tr.pcm);
                let stt = stt.clone();
                let llm = llm.clone();
                let inst_id = inst_id.clone();
                tokio::spawn(async move {
                    let s = stt.clone();
                    let heard = match tokio::task::spawn_blocking(move || s.transcribe(&pcm)).await {
                        Ok(Ok(t)) if !t.trim().is_empty() => t,
                        Ok(Err(e)) => {
                            log::debug!("milatc: whisper failed: {e:#}");
                            return;
                        }
                        _ => return,
                    };
                    heard_call(st, pos, tr.unit_id, tr.name, heard, llm, clock, &inst_id).await;
                });
            }
        });
    }

    // The picture.
    let mut ticker = tokio::time::interval(Duration::from_millis(cfg.poll_ms.max(250)));
    ticker.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
    let mut last_vocab = f64::NEG_INFINITY;
    let mut fails = 0u32;
    loop {
        ticker.tick().await;
        let picture = match fetch(&db, &inst).await {
            Ok(p) => {
                fails = 0;
                p
            }
            Err(e) => {
                fails += 1;
                if fails == 10 {
                    log::warn!("[{}] range ATC: no picture from the engine: {e}", inst.id);
                }
                continue;
            }
        };
        let now = clock();
        for st in stations.iter() {
            let out = {
                let mut ctl = st.ctl.lock().unwrap();
                let Some(field) = picture.airfields.iter().find(|a| ctl.ap.is_named(&a.name)).cloned() else {
                    let mut logged = st.missing_logged.lock().unwrap();
                    if !*logged {
                        *logged = true;
                        log::warn!(
                            "[{}] range ATC: {} is not in the engine's picture (has: {})",
                            inst.id,
                            ctl.ap.dcs_name,
                            picture.airfields.iter().map(|a| a.name.as_str()).collect::<Vec<_>>().join(", ")
                        );
                    }
                    continue;
                };
                let mut tried = st.align_tried.lock().unwrap();
                if ctl.ap.aligned.is_none() && !*tried {
                    let dcs: Vec<(String, P2)> = field
                        .runways
                        .iter()
                        .filter_map(|r| Some((r.name.clone(), ctl.ap.local(r.thr_lat?, r.thr_lon?))))
                        .collect();
                    if !dcs.is_empty() {
                        *tried = true;
                        match ctl.ap.align(&dcs) {
                            Some(m) => log::info!("[{}] range ATC: {} taxi network aligned to DCS ({m:.0} m shift)", inst.id, ctl.ap.callsign),
                            None => {
                                log::warn!(
                                    "[{}] range ATC: {} taxi network does not match the DCS runways; taxi clearances will not name taxiways",
                                    inst.id,
                                    ctl.ap.callsign
                                );
                            }
                        }
                    }
                }
                drop(tried);
                ctl.update(&field, &picture.traffic, now)
            };
            dispatch(&st, out);
        }
        // Keep whisper primed with what it will hear.
        if now - last_vocab > 30. {
            last_vocab = now;
            if let Some(stt) = &stt {
                let mut words: Vec<String> = vec![];
                for st in stations.iter() {
                    let ctl = st.ctl.lock().unwrap();
                    for p in Pos::ALL {
                        words.push(ctl.facility(p));
                    }
                    for f in ctl.flights.values() {
                        words.push(f.call.clone());
                    }
                    words.extend(ctl.ap.runways.iter().map(|r| format!("runway {}", r.name)));
                }
                words.extend(
                    [
                        "request taxi", "holding short", "ready for departure", "request clearance", "initial",
                        "break", "base, gear down", "request PAR", "request ILS", "full stop", "touch and go",
                        "low approach", "closed", "clear of the runway", "squawk", "Alpha", "Bravo", "Charlie",
                        "Delta", "Echo", "Foxtrot", "Golf", "Hotel", "Kilo",
                    ]
                    .map(String::from),
                );
                stt.set_vocabulary(&words);
            }
        }
    }
}

/// Process one recognised transmission: rules first, the language model for
/// what the rules miss, then the controller.
#[allow(clippy::too_many_arguments)]
async fn heard_call(
    st: Arc<Station>,
    pos: Pos,
    unit_id: Option<u64>,
    srs_name: String,
    heard: String,
    llm: Option<llm::Llm>,
    clock: impl Fn() -> f64,
    inst: &str,
) {
    let callsign = st.ctl.lock().unwrap().ap.callsign.clone();
    let mut parsed = intent::parse(&heard, &callsign);
    // The transcript names the flight when we know it.
    let who = {
        let mut ctl = st.ctl.lock().unwrap();
        ctl.identify(&Caller { unit_id, srs_name: srs_name.clone() }, parsed.as_ref())
            .and_then(|k| ctl.flights.get(&k).map(|f| f.call.clone()))
            .unwrap_or_else(|| srs_name.clone())
    };
    note(inst, Line { t: now_ms(), station: label(st.index, pos), who, text: heard.clone(), pilot: true, ai: false });
    if parsed.is_none() {
        if let Some(llm) = llm {
            let ctx = {
                let mut ctl = st.ctl.lock().unwrap();
                let key = ctl.identify(&Caller { unit_id, srs_name: srs_name.clone() }, None);
                ctl.llm_context(pos, key, &heard)
            };
            match tokio::task::spawn_blocking(move || llm.classify(&ctx)).await {
                Ok(Ok(p)) => {
                    log::info!("milatc: model read \"{heard}\" as {:?}", p.intent);
                    parsed = Some(p);
                }
                Ok(Err(e)) => log::warn!("milatc: language model failed: {e:#}"),
                Err(_) => {}
            }
        }
    }
    let out = {
        let mut ctl = st.ctl.lock().unwrap();
        ctl.respond(pos, &Caller { unit_id, srs_name }, &heard, parsed, clock())
    };
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
    db: StatsDb,
    inst: crate::Inst,
}

impl Engine {
    async fn command(&self, action: &str, target: &str, text: &str) {
        let r = tokio::time::timeout(
            Duration::from_secs(5),
            crate::call_engine_rpc_str(
                &self.db,
                &self.inst,
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
}

/// One position's transmitter. One utterance at a time; a human pause
/// before each (counted from when the frequency last went quiet, so a reply
/// follows the call it answers the way a person's would); never over a pilot
/// (the SRS client waits for a clear frequency); stale calls dropped. AI
/// pilots speak through here too, in their own voices.
async fn transmitter(
    client: srs::SrsClient,
    radios: Vec<Radio>,
    cast: Arc<Cast>,
    mut q: tokio::sync::mpsc::UnboundedReceiver<Tx>,
    clock: impl Fn() -> f64,
    me: Speaker,
    engine: Engine,
) {
    let mut quiet_since = Instant::now();
    while let Some(tx) = q.recv().await {
        let got = Instant::now();
        if clock() > tx.expires {
            log::debug!("milatc: dropped stale {} call: {}", me.name, tx.text);
            continue;
        }
        let voice = match &tx.pilot {
            Some(_) => cast.pilot(tx.to.unwrap_or(0)),
            None => cast.controller(me.pos),
        };
        // Synthesis overlaps the pause.
        let spoken = speech::speak(&tx.text);
        let pcm = match tokio::task::spawn_blocking(move || voice.synthesize(&spoken)).await {
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
        note(
            &me.inst,
            Line {
                t: now_ms(),
                station: me.station.clone(),
                who: tx.pilot.clone().unwrap_or_else(|| me.name.clone()),
                text: tx.text.clone(),
                pilot: tx.pilot.is_some(),
                ai: tx.pilot.is_some(),
            },
        );
        if let (Some(pcm), true) = (pcm, client.is_connected()) {
            let client = client.clone();
            let radios = radios.clone();
            let res = tokio::task::spawn_blocking(move || client.transmit(&radios, &pcm)).await;
            if let Ok(Err(e)) = res {
                log::warn!("milatc: {} transmit failed: {e:#}", me.name);
            }
        }
        quiet_since = Instant::now();
        if let Some(unit) = tx.subtitle {
            let e = engine.clone();
            let text = format!("{}: {}", me.name.to_uppercase(), tx.text);
            tokio::spawn(async move { e.command("text", &unit.to_string(), &text).await });
        }
        if let Some((group, action)) = &tx.action {
            engine.command(action, group, "").await;
        }
    }
}

async fn fetch(db: &StatsDb, inst: &crate::db::InstanceState) -> Result<AtcPicture, String> {
    let res = tokio::time::timeout(
        Duration::from_secs(4),
        crate::call_engine_rpc_str(db, inst, "query-atc", vec![("side", netidx::publisher::Value::from("all"))]),
    )
    .await;
    match res {
        Ok(Ok(json)) => serde_json::from_str::<AtcPicture>(&json).map_err(|e| format!("bad query-atc JSON: {e}")),
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
}
