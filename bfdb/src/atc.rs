// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `atc` module. See the repository NOTICE file.

//! Spoken air traffic control — ATIS on a loop, and a tower that answers.
//!
//! Runs beside [`crate::gci`] on the same voice bus ([`crate::voice`]): one
//! persistent SRS client per coalition, carrying a radio for every field that
//! side holds. A transmission is routed to a position by the frequency it
//! arrived on, so "Incirlik Tower, Colt one one, taxi" reaches Incirlik and
//! nothing else.
//!
//! Two kinds of traffic:
//!
//! - **ATIS** is a broadcast. Each open field repeats a recorded-style report
//!   on its own frequency — information letter, wind, active runway, altimeter,
//!   temperature, cloud, and the campaign facts DCS has no idea about: whether
//!   the field is under threat and whether it can rearm and refuel you. The
//!   letter only advances when something material changes, the way a real ATIS
//!   does, so pilots can say "with information Charlie" and have it mean
//!   something.
//! - **Tower** is a conversation. Taxi, takeoff, pattern entry, landing,
//!   traffic advisories and altimeter, in military phraseology.
//!
//! Deliberately *advisory*: DCS players and AI do not hold short reliably, and
//! a controller that withholds clearance produces dead air and frustration
//! rather than realism. Tower sequences traffic, warns about it, and always
//! eventually clears you.

use crate::db::StatsDb;
use crate::voice::{modulation_byte, srs, stt, tts, FreqSpec, Radio};
use bfprotocols::atc::{AtcAirfield, AtcPicture, AtcTraffic};
use dcso3::coalition::Side;
use serde::Deserialize;
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

// ─── Config ────────────────────────────────────────────────────────────────

fn d_true() -> bool {
    true
}
fn d_tower_base() -> f64 {
    // Clear of the comms plan's blue UHF 251-270 and red UHF 228-237 blocks.
    340.0
}
fn d_step() -> f64 {
    0.5
}
fn d_atis_base() -> f64 {
    370.0
}
fn d_atis_interval() -> u64 {
    45
}
fn d_poll() -> u64 {
    20
}
fn d_pattern_alt() -> u32 {
    1500
}

/// Explicit frequencies for one field, when the automatic plan is not what you
/// want — e.g. to match the stock DCS ATC frequency for that airfield.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct FieldFreqCfg {
    #[serde(default)]
    pub tower: Option<FreqSpec>,
    #[serde(default)]
    pub atis: Option<FreqSpec>,
}

/// The `atc` block of `gci.json`. Shares that file's SRS host, EAM passwords,
/// opus, TTS and whisper settings — ATC is another position on the same bus,
/// not a separate service to configure twice.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct AtcConfig {
    #[serde(default = "d_true")]
    pub enabled: bool,
    /// Tower frequencies are handed out from here, one `towerStepMhz` apart,
    /// in a stable order (by field name) so a given field keeps its frequency
    /// for the life of the campaign. Override any of them under `fields`.
    #[serde(default = "d_tower_base")]
    pub tower_base_mhz: f64,
    #[serde(default = "d_step")]
    pub tower_step_mhz: f64,
    #[serde(default)]
    pub tower_modulation: Option<String>,
    /// ATIS frequencies, same scheme. Kept in a separate block so a pilot can
    /// listen to ATIS on one radio while working tower on the other.
    #[serde(default = "d_atis_base")]
    pub atis_base_mhz: f64,
    #[serde(default = "d_step")]
    pub atis_step_mhz: f64,
    #[serde(default)]
    pub atis_modulation: Option<String>,
    /// Seconds between ATIS repeats on a field's frequency.
    #[serde(default = "d_atis_interval")]
    pub atis_interval_secs: u64,
    /// Seconds between `query-atc` polls per coalition.
    #[serde(default = "d_poll")]
    pub poll_secs: u64,
    /// Pattern altitude AGL, feet — quoted when clearing an aircraft into the
    /// pattern.
    #[serde(default = "d_pattern_alt")]
    pub pattern_altitude_ft: u32,
    /// Per-field overrides, keyed by the field's name as the campaign knows it
    /// ("Incirlik").
    #[serde(default)]
    pub fields: HashMap<String, FieldFreqCfg>,
    /// Fields whose ATIS should not be broadcast (a rear-area field nobody
    /// flies from just adds chatter).
    #[serde(default)]
    pub atis_exclude: Vec<String>,
}

impl AtcConfig {
    fn tower_modulation_byte(&self) -> u8 {
        self.tower_modulation.as_deref().map_or(0, modulation_byte)
    }
    fn atis_modulation_byte(&self) -> u8 {
        self.atis_modulation.as_deref().map_or(0, modulation_byte)
    }
}

// ─── Frequency plan ────────────────────────────────────────────────────────

/// What each field is on. Assigned once per poll from the field list, so a
/// captured field keeps the same numbers whichever side is holding it.
#[derive(Debug, Clone)]
pub(crate) struct FieldFreqs {
    pub name: String,
    pub tower: Radio,
    pub atis: Option<Radio>,
}

/// Assign every field a tower and ATIS frequency.
///
/// The order is by **name**, not by objective id or by who owns what, so the
/// assignment is stable: a field does not change frequency when it is captured,
/// when another field falls, or when the server restarts. Explicit `fields`
/// entries win and are excluded from the automatic run so they cannot collide
/// with it.
fn plan_frequencies(cfg: &AtcConfig, fields: &[&AtcAirfield]) -> Vec<FieldFreqs> {
    let mut names: Vec<&str> = fields.iter().map(|f| f.name.as_str()).collect();
    names.sort_unstable();
    names.dedup();

    let mut out = Vec::with_capacity(names.len());
    let mut auto_slot = 0usize;
    for name in names {
        let over = cfg.fields.get(name);
        let tower = match over.and_then(|o| o.tower.as_ref()) {
            Some(spec) => spec.to_radio(name),
            None => {
                let f = cfg.tower_base_mhz + cfg.tower_step_mhz * auto_slot as f64;
                Radio::new(f, cfg.tower_modulation_byte(), name)
            }
        };
        let atis = if cfg.atis_exclude.iter().any(|x| x == name) {
            None
        } else {
            Some(match over.and_then(|o| o.atis.as_ref()) {
                Some(spec) => spec.to_radio(name),
                None => {
                    let f = cfg.atis_base_mhz + cfg.atis_step_mhz * auto_slot as f64;
                    Radio::new(f, cfg.atis_modulation_byte(), name)
                }
            })
        };
        // Only an automatically-assigned field consumes a slot.
        if over.map_or(true, |o| o.tower.is_none() && o.atis.is_none()) {
            auto_slot += 1;
        }
        out.push(FieldFreqs {
            name: name.to_string(),
            tower,
            atis,
        });
    }
    out
}

// ─── ATIS ──────────────────────────────────────────────────────────────────

/// The ATIS state for one field: which information letter is current, and what
/// it was issued for.
#[derive(Debug, Clone)]
struct AtisState {
    letter: u8, // 0 = Alpha
    text: String,
    /// The values the current letter was issued against — a new letter is only
    /// cut when one of these moves materially, the way a real ATIS works.
    wind_from: u16,
    wind_kts: u16,
    runway: Option<String>,
    qnh_hpa: i32,
    open: bool,
    threatened: bool,
    last_broadcast: Option<Instant>,
}

fn letter_word(i: u8) -> &'static str {
    const L: [&str; 26] = [
        "alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel", "india",
        "juliet", "kilo", "lima", "mike", "november", "oscar", "papa", "quebec", "romeo",
        "sierra", "tango", "uniform", "victor", "whiskey", "xray", "yankee", "zulu",
    ];
    L[(i % 26) as usize]
}

/// Has anything changed enough to justify a new information letter? Wind
/// direction inside 20° and speed inside 3 kt is noise, not news.
fn atis_materially_changed(s: &AtisState, a: &AtcAirfield) -> bool {
    let dir_delta = {
        let d = (s.wind_from as i32 - a.wind_from_deg as i32).abs() % 360;
        d.min(360 - d)
    };
    dir_delta > 20
        || (s.wind_kts as i32 - a.wind_speed_kts as i32).abs() > 3
        || s.runway.as_deref() != a.active_runway.as_deref()
        || (s.qnh_hpa - a.qnh_hpa.round() as i32).abs() >= 2
        || s.open != a.open
        || s.threatened != a.threatened
}

/// Render a field's ATIS.
///
/// Follows the real running order — identification, letter, wind, runway,
/// altimeter, temperature, weather — and then adds the two things a campaign
/// knows and a real ATIS never would: whether the field is under threat, and
/// whether it can actually rearm and refuel you.
fn atis_text(a: &AtcAirfield, letter: u8, pattern_alt_ft: u32) -> String {
    let mut s = String::with_capacity(320);
    s.push_str(&format!(
        "{} information {}. ",
        a.name,
        letter_word(letter)
    ));

    if !a.open {
        s.push_str("Field is closed. ");
    }

    // Wind. Calm below 3 kt, as on a real ATIS.
    if a.wind_speed_kts < 3 {
        s.push_str("Wind calm. ");
    } else {
        s.push_str(&format!(
            "Wind {} at {}. ",
            digits(a.wind_from_deg),
            a.wind_speed_kts
        ));
    }

    match (&a.active_runway, &a.brc) {
        (_, Some(brc)) => s.push_str(&format!("Base recovery course {}. ", digits(*brc))),
        (Some(r), _) => s.push_str(&format!("Runway in use {}. ", spell_runway(r))),
        _ => {}
    }

    s.push_str(&format!(
        "Altimeter {}, Q N H {} hectopascals. ",
        inhg_spoken(a.qnh_inhg),
        a.qnh_hpa.round() as i32
    ));
    s.push_str(&format!(
        "Temperature {}, dewpoint {}. ",
        temp_spoken(a.temp_c),
        temp_spoken(a.dewpoint_c)
    ));

    if a.visibility_m < 8000 {
        s.push_str(&format!("Visibility {} kilometres. ", a.visibility_m / 1000));
    }
    if let Some(base) = a.cloud_base_ft {
        if base > 0 {
            s.push_str(&format!("Cloud base {} feet. ", round_to(base, 100)));
        }
    }
    if a.precipitation {
        s.push_str("Precipitation in the area. ");
    }
    if let Some(c) = a.recovery_case {
        s.push_str(&format!("Case {} recovery. ", roman(c)));
    } else {
        s.push_str(&format!("Pattern altitude {} feet. ", pattern_alt_ft));
    }

    // The campaign-aware part.
    if a.threatened {
        s.push_str("Caution, field is under threat, expect hostile activity in the vicinity. ");
    }
    match (a.supply, a.fuel) {
        (0, 0) => s.push_str("Rearming and refuelling unavailable. "),
        (0, _) => s.push_str("Rearming unavailable. "),
        (_, 0) => s.push_str("Refuelling unavailable. "),
        _ => {}
    }
    if a.logi < 30 {
        s.push_str("Field services degraded. ");
    }

    s.push_str(&format!("Advise on initial contact you have {}.", letter_word(letter)));
    s
}

// ─── Tower ─────────────────────────────────────────────────────────────────

/// What a pilot asked the tower for.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AtcRequest {
    RadioCheck,
    /// Engine start / ramp.
    Startup,
    Taxi,
    Takeoff,
    /// Joining the pattern or running straight in.
    Inbound,
    Landing,
    /// Overhead break.
    Initial,
    GoAround,
    Altimeter,
    /// Leaving the frequency.
    Departing,
}

/// Parse a tower call. The wake word is the field name plus any of the position
/// names a pilot might use for it.
pub(crate) fn parse_atc(text: &str, field: &str) -> Option<AtcRequest> {
    let t = text.to_lowercase();
    let addressed = stt::mentions(&t, field)
        || t.contains("tower")
        || t.contains("ground")
        || t.contains("approach");
    if !addressed {
        return None;
    }
    let has = |p: &str| stt::contains_phrase(&t, p);
    Some(if has("radio check") || has("how do you read") {
        AtcRequest::RadioCheck
    } else if has("request startup") || has("start up") || has("request start") {
        AtcRequest::Startup
    } else if has("taxi") {
        AtcRequest::Taxi
    } else if has("takeoff") || has("take off") || has("departure") && has("request") {
        AtcRequest::Takeoff
    } else if has("initial") || has("overhead break") || has("the break") {
        AtcRequest::Initial
    } else if has("go around") || has("going around") {
        AtcRequest::GoAround
    } else if has("gear down") || has("cleared to land") || has("request landing") || has("land") {
        AtcRequest::Landing
    } else if has("inbound") || has("recovery") || has("straight in") {
        AtcRequest::Inbound
    } else if has("altimeter") || has("qnh") {
        AtcRequest::Altimeter
    } else if has("switching") || has("off station") || has("departing") {
        AtcRequest::Departing
    } else {
        return None;
    })
}

/// Build the tower's reply.
fn tower_reply(
    cfg: &AtcConfig,
    a: &AtcAirfield,
    req: &AtcRequest,
    who: &str,
    traffic: &[&AtcTraffic],
) -> String {
    let field = &a.name;
    let head = format!("{who}, {field} tower");
    let rwy = a
        .active_runway
        .as_deref()
        .map(spell_runway)
        .unwrap_or_else(|| "the active".to_string());
    let wind = if a.wind_speed_kts < 3 {
        "wind calm".to_string()
    } else {
        format!("wind {} at {}", digits(a.wind_from_deg), a.wind_speed_kts)
    };
    let altim = format!("altimeter {}", inhg_spoken(a.qnh_inhg));

    // Who else is in the way. Airborne inside 10 nm, or anything moving on the
    // ground, is worth a word.
    let airborne = traffic.iter().filter(|t| !t.on_ground && t.field_rng_m < 18_520).count();
    let rolling = traffic.iter().filter(|t| t.on_ground && t.speed_kts > 15).count();

    match req {
        AtcRequest::RadioCheck => format!("{head}, loud and clear"),
        AtcRequest::Startup => {
            if a.open {
                format!("{head}, start up approved, {altim}")
            } else {
                format!("{head}, field is closed, start up at your discretion")
            }
        }
        AtcRequest::Taxi => {
            let caution = if rolling > 0 {
                format!(", caution {} aircraft taxiing", number_word(rolling))
            } else {
                String::new()
            };
            format!("{head}, taxi to runway {rwy}, {wind}, {altim}{caution}")
        }
        AtcRequest::Takeoff => {
            let hold = if rolling > 1 {
                ", traffic on the runway, your discretion"
            } else {
                ""
            };
            format!("{head}, runway {rwy}, cleared for takeoff, {wind}{hold}")
        }
        AtcRequest::Initial => format!(
            "{head}, cleared for the overhead break, runway {rwy}, {altim}, report the ninety"
        ),
        AtcRequest::Inbound => {
            let caution = if airborne > 1 {
                format!(", {} in the pattern", number_word(airborne - 1))
            } else {
                String::new()
            };
            format!(
                "{head}, join the pattern runway {rwy}, {} feet, {altim}, report the break{caution}",
                cfg.pattern_altitude_ft
            )
        }
        AtcRequest::Landing => {
            let caution = if airborne > 1 {
                ", caution traffic in the pattern"
            } else {
                ""
            };
            format!("{head}, runway {rwy}, cleared to land, {wind}{caution}")
        }
        AtcRequest::GoAround => format!(
            "{head}, roger your go around, climb to {} feet, report the downwind runway {rwy}",
            cfg.pattern_altitude_ft
        ),
        AtcRequest::Altimeter => format!("{head}, {altim}, {wind}, runway {rwy}"),
        AtcRequest::Departing => format!("{head}, roger, frequency change approved, good hunting"),
    }
}

// ─── Speech helpers ────────────────────────────────────────────────────────

/// Digit-by-digit, ICAO pronunciation. Bearings and runway numbers are never
/// read as quantities.
fn digits(n: u16) -> String {
    format!("{:03}", n % 1000)
        .chars()
        .filter_map(|c| c.to_digit(10))
        .map(digit_word)
        .collect::<Vec<_>>()
        .join(" ")
}

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

fn number_word(n: usize) -> &'static str {
    match n {
        0 => "no",
        1 => "one",
        2 => "two",
        3 => "tree",
        4 => "four",
        _ => "multiple",
    }
}

/// "13L" → "one tree left". DCS designators carry L/R/C suffixes.
fn spell_runway(r: &str) -> String {
    let mut out: Vec<String> = Vec::new();
    for c in r.chars() {
        match c {
            d if d.is_ascii_digit() => out.push(digit_word(d.to_digit(10).unwrap()).to_string()),
            'L' | 'l' => out.push("left".into()),
            'R' | 'r' => out.push("right".into()),
            'C' | 'c' => out.push("center".into()),
            _ => {}
        }
    }
    if out.is_empty() {
        r.to_string()
    } else {
        out.join(" ")
    }
}

/// "29.92" spoken as "two niner niner two".
fn inhg_spoken(inhg: f64) -> String {
    let hundredths = (inhg * 100.0).round() as u32;
    hundredths
        .to_string()
        .chars()
        .filter_map(|c| c.to_digit(10))
        .map(digit_word)
        .collect::<Vec<_>>()
        .join(" ")
}

fn temp_spoken(c: i16) -> String {
    if c < 0 {
        format!("minus {}", -c)
    } else {
        c.to_string()
    }
}

fn roman(n: u8) -> &'static str {
    match n {
        1 => "one",
        2 => "two",
        _ => "tree",
    }
}

fn round_to(v: i32, step: i32) -> i32 {
    ((v + step / 2) / step) * step
}

// ─── Runtime ───────────────────────────────────────────────────────────────

/// One queued ATC transmission.
struct AtcCall {
    side: Side,
    radios: Vec<Radio>,
    text: String,
    urgent: bool,
    queued: Instant,
}

/// Per-side ATC state.
#[derive(Default)]
struct SideAtc {
    atis: HashMap<String, AtisState>,
    /// The radio set currently registered on this side's SRS client, so we only
    /// retune when the field list actually changes.
    tuned: Vec<u64>,
}

pub(crate) type AtcTranscript = Arc<Mutex<std::collections::VecDeque<String>>>;

/// Decide what, if anything, each field's ATIS should say this tick.
fn atis_calls(
    cfg: &AtcConfig,
    state: &mut SideAtc,
    side: Side,
    picture: &AtcPicture,
    plan: &[FieldFreqs],
) -> Vec<AtcCall> {
    let mut out = vec![];
    for a in &picture.airfields {
        let Some(freqs) = plan.iter().find(|f| f.name == a.name) else {
            continue;
        };
        let Some(atis_radio) = freqs.atis.clone() else {
            continue;
        };
        let entry = state.atis.get(&a.name).cloned();
        let (letter, text) = match &entry {
            Some(s) if !atis_materially_changed(s, a) => (s.letter, s.text.clone()),
            Some(s) => {
                let l = (s.letter + 1) % 26;
                let t = atis_text(a, l, cfg.pattern_altitude_ft);
                log::info!("atc[{side:?}] {}: new ATIS information {}", a.name, letter_word(l));
                (l, t)
            }
            None => {
                let t = atis_text(a, 0, cfg.pattern_altitude_ft);
                (0, t)
            }
        };
        let due = entry
            .as_ref()
            .and_then(|s| s.last_broadcast)
            .map_or(true, |t| t.elapsed().as_secs() >= cfg.atis_interval_secs);
        let changed = entry.as_ref().map_or(true, |s| s.letter != letter);
        state.atis.insert(
            a.name.clone(),
            AtisState {
                letter,
                text: text.clone(),
                wind_from: a.wind_from_deg,
                wind_kts: a.wind_speed_kts,
                runway: a.active_runway.clone(),
                qnh_hpa: a.qnh_hpa.round() as i32,
                open: a.open,
                threatened: a.threatened,
                last_broadcast: if due || changed {
                    Some(Instant::now())
                } else {
                    entry.as_ref().and_then(|s| s.last_broadcast)
                },
            },
        );
        if due || changed {
            out.push(AtcCall {
                side,
                radios: vec![atis_radio],
                text,
                urgent: false,
                queued: Instant::now(),
            });
        }
    }
    out
}

/// Find the flight a tower transmission came from.
fn match_traffic<'a>(
    tr: &srs::Transmission,
    heard: &str,
    picture: &'a AtcPicture,
) -> Option<&'a AtcTraffic> {
    if let Some(uid) = tr.unit_id {
        if let Some(t) = picture.traffic.iter().find(|t| t.unit_id == Some(uid)) {
            return Some(t);
        }
    }
    let norm = |s: &str| {
        s.chars()
            .filter(|c| c.is_alphanumeric())
            .collect::<String>()
            .to_lowercase()
    };
    let a = norm(&tr.name);
    if !a.is_empty() {
        if let Some(t) = picture.traffic.iter().find(|t| {
            let n = norm(&t.player_name);
            !n.is_empty() && (a.contains(&n) || n.contains(&a))
        }) {
            return Some(t);
        }
    }
    let h = heard.to_lowercase();
    picture
        .traffic
        .iter()
        .find(|t| !t.callsign.is_empty() && stt::mentions(&h, &t.callsign))
}

/// Run ATC for one DCS server instance until the process exits.
///
/// Owns its own pair of SRS clients — separate from GCI's so the client list
/// reads sensibly ("Magic", "Blue ATC") and so a busy tower frequency can never
/// delay a threat call on the GCI net.
pub(crate) async fn run(
    db: StatsDb,
    inst: crate::Inst,
    gci_cfg: crate::gci::GciConfig,
    cfg: AtcConfig,
    transcript: AtcTranscript,
) {
    if !cfg.enabled {
        return;
    }
    let (rx_tx, rx_recv) = tokio::sync::mpsc::unbounded_channel::<srs::Transmission>();
    let stt = stt::Stt::from_cfg(
        gci_cfg.whisper_exe.as_ref(),
        gci_cfg.whisper_model.as_ref(),
        gci_cfg.whisper_server_url.as_deref(),
    );
    let voice = match AtcVoice::build(&gci_cfg, stt.is_some().then_some(rx_tx)) {
        Some(v) => v,
        None => {
            log::error!("[{}] ATC: no voice client could be started", inst.id);
            return;
        }
    };
    log::info!(
        "[{}] ATC enabled: towers from {:.3} MHz step {:.3}, ATIS from {:.3} MHz, poll {}s{}",
        inst.id,
        cfg.tower_base_mhz,
        cfg.tower_step_mhz,
        cfg.atis_base_mhz,
        cfg.poll_secs,
        match &stt {
            Some(s) => format!(", speech recognition via {}", s.describe()),
            None => ", broadcast only (no whisper configured)".to_string(),
        }
    );

    let (tx, mut rx) = tokio::sync::mpsc::channel::<AtcCall>(64);

    // Answer inbound tower calls.
    if let Some(stt) = stt {
        let voice_rx = voice.clone();
        let cfg_rx = cfg.clone();
        let tx_rx = tx.clone();
        tokio::spawn(stt_worker(stt, cfg_rx, voice_rx, tx_rx, rx_recv));
    }

    // Transmitter: one utterance at a time per side, paced so ATIS repeats
    // never queue up behind each other.
    {
        let voice = voice.clone();
        let transcript = transcript.clone();
        tokio::spawn(async move {
            while let Some(call) = rx.recv().await {
                if !call.urgent && call.queued.elapsed() > Duration::from_secs(30) {
                    continue; // a stale ATIS repeat is worth nothing
                }
                {
                    let mut h = transcript.lock().unwrap();
                    h.push_back(call.text.clone());
                    while h.len() > 200 {
                        h.pop_front();
                    }
                }
                let Some(client) = voice.client(call.side) else {
                    continue;
                };
                if !client.is_connected() {
                    continue;
                }
                let engine = voice.tts(call.side);
                let text = call.text.clone();
                let radios = call.radios.clone();
                let res = tokio::task::spawn_blocking(move || -> anyhow::Result<()> {
                    let pcm = engine.synthesize(&text)?;
                    client.transmit(&radios, &pcm)
                })
                .await;
                if let Ok(Err(e)) = res {
                    log::warn!("atc: transmit failed: {e:#}");
                }
            }
        });
    }

    let mut state: HashMap<&'static str, SideAtc> = HashMap::new();
    state.insert("blue", SideAtc::default());
    state.insert("red", SideAtc::default());
    let mut ticker = tokio::time::interval(Duration::from_secs(cfg.poll_secs.max(5)));

    loop {
        ticker.tick().await;
        for (side, key) in [(Side::Blue, "blue"), (Side::Red, "red")] {
            let picture = match fetch_atc(&db, &inst, if side == Side::Red { "red" } else { "blue" })
                .await
            {
                Ok(p) => p,
                Err(e) => {
                    log::debug!("atc[{side:?}] poll failed: {e}");
                    continue;
                }
            };
            let refs: Vec<&AtcAirfield> = picture.airfields.iter().collect();
            let plan = plan_frequencies(&cfg, &refs);
            let st = state.get_mut(key).unwrap();

            // Retune the client when the set of fields changes (captures,
            // losses) so we are listening on exactly the towers we control.
            let mut keys: Vec<u64> = plan.iter().map(|f| f.tower.key()).collect();
            keys.sort_unstable();
            if keys != st.tuned {
                st.tuned = keys;
                let mut radios: Vec<Radio> = plan.iter().map(|f| f.tower.clone()).collect();
                radios.extend(plan.iter().filter_map(|f| f.atis.clone()));
                if let Some(c) = voice.client(side) {
                    c.set_radios(radios);
                }
                log::info!(
                    "atc[{side:?}] controlling {} field(s): {}",
                    plan.len(),
                    plan.iter()
                        .map(|f| format!("{} {}", f.name, f.tower.describe()))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }

            for call in atis_calls(&cfg, st, side, &picture, &plan) {
                let _ = tx.try_send(call);
            }
            voice.cache_picture(side, picture, plan);
        }
    }
}

/// Answer one inbound transmission on a tower frequency.
pub(crate) fn answer(
    cfg: &AtcConfig,
    tr: &srs::Transmission,
    heard: &str,
    picture: &AtcPicture,
    plan: &[FieldFreqs],
) -> Option<(Vec<Radio>, String)> {
    // Which field owns the frequency it came in on?
    let field = plan
        .iter()
        .find(|f| (f.tower.freq_hz - tr.freq_hz).abs() <= 500.0)?;
    let a = picture.airfields.iter().find(|a| a.name == field.name)?;
    let req = parse_atc(heard, &a.name)?;

    let flight = match_traffic(tr, heard, picture);
    let who = flight
        .map(|t| {
            if t.callsign.is_empty() {
                t.player_name.clone()
            } else {
                t.callsign.clone()
            }
        })
        .unwrap_or_else(|| "station calling".to_string());

    // Everything else in this field's area, for traffic advisories.
    let traffic: Vec<&AtcTraffic> = picture
        .traffic
        .iter()
        .filter(|t| t.field.as_deref() == Some(a.id.as_str()))
        .filter(|t| flight.map_or(true, |f| f.ucid != t.ucid))
        .collect();

    Some((
        vec![field.tower.clone()],
        tower_reply(cfg, a, &req, &who, &traffic),
    ))
}

/// The SRS clients and TTS engines ATC speaks through, plus the latest picture
/// each side's tower answers from.
#[derive(Clone)]
pub(crate) struct AtcVoice {
    blue: Option<srs::SrsClient>,
    red: Option<srs::SrsClient>,
    tts_blue: tts::Tts,
    tts_red: tts::Tts,
    cache: Arc<Mutex<HashMap<&'static str, (AtcPicture, Vec<FieldFreqs>)>>>,
}

impl AtcVoice {
    pub(crate) fn new(
        blue: Option<srs::SrsClient>,
        red: Option<srs::SrsClient>,
        tts_blue: tts::Tts,
        tts_red: tts::Tts,
    ) -> Self {
        AtcVoice {
            blue,
            red,
            tts_blue,
            tts_red,
            cache: Default::default(),
        }
    }

    fn client(&self, side: Side) -> Option<srs::SrsClient> {
        match side {
            Side::Red => self.red.clone(),
            _ => self.blue.clone(),
        }
    }

    fn tts(&self, side: Side) -> tts::Tts {
        match side {
            Side::Red => self.tts_red.clone(),
            _ => self.tts_blue.clone(),
        }
    }

    fn cache_picture(&self, side: Side, picture: AtcPicture, plan: Vec<FieldFreqs>) {
        let key = if side == Side::Red { "red" } else { "blue" };
        if let Ok(mut c) = self.cache.lock() {
            c.insert(key, (picture, plan));
        }
    }

    /// The current picture and frequency plan for a side, for answering a call.
    pub(crate) fn picture(&self, side: Side) -> Option<(AtcPicture, Vec<FieldFreqs>)> {
        let key = if side == Side::Red { "red" } else { "blue" };
        self.cache.lock().ok()?.get(key).cloned()
    }
}

/// Poll the engine for one coalition's ATC picture.
async fn fetch_atc(
    db: &StatsDb,
    inst: &crate::db::InstanceState,
    side: &'static str,
) -> Result<AtcPicture, String> {
    let res = tokio::time::timeout(
        Duration::from_secs(8),
        crate::call_engine_rpc_str(
            db,
            inst,
            "query-atc",
            vec![("side", netidx::publisher::Value::from(side))],
        ),
    )
    .await;
    match res {
        Ok(Ok(json)) => {
            serde_json::from_str::<AtcPicture>(&json).map_err(|e| format!("bad query-atc JSON: {e}"))
        }
        Ok(Err(e)) => Err(format!("query-atc RPC error: {e:?}")),
        Err(_) => Err("query-atc timed out".to_string()),
    }
}

/// Turn heard tower calls into answers.
async fn stt_worker(
    stt: stt::Stt,
    cfg: AtcConfig,
    voice: AtcVoice,
    tx: tokio::sync::mpsc::Sender<AtcCall>,
    mut rx: tokio::sync::mpsc::UnboundedReceiver<srs::Transmission>,
) {
    while let Some(mut tr) = rx.recv().await {
        let side = if tr.coalition == 1 { Side::Red } else { Side::Blue };
        let pcm = std::mem::take(&mut tr.pcm);
        let s = stt.clone();
        let heard = match tokio::task::spawn_blocking(move || s.transcribe(&pcm)).await {
            Ok(Ok(t)) if !t.is_empty() => t,
            _ => continue,
        };
        log::info!("atc[{side:?}] heard on {}: \"{heard}\"", tr.label);

        let Some((picture, plan)) = voice.picture(side) else {
            continue;
        };
        // Keep the recogniser primed with the field names actually in play.
        s_prime(&stt, &picture);

        match answer(&cfg, &tr, &heard, &picture, &plan) {
            Some((radios, text)) => {
                log::info!("atc[{side:?}] {}: {text}", tr.label);
                let _ = tx.try_send(AtcCall {
                    side,
                    radios,
                    text,
                    urgent: true,
                    queued: Instant::now(),
                });
            }
            None => log::info!(
                "atc[{side:?}] nothing actionable in \"{heard}\" on {}",
                tr.label
            ),
        }
    }
}

fn s_prime(stt: &stt::Stt, picture: &AtcPicture) {
    stt.set_vocabulary(&atc_vocabulary(picture));
}

impl AtcVoice {
    /// Start the pair of SRS clients ATC speaks through. Frequencies are set
    /// later, per poll, from the field list — the client opens on a single
    /// placeholder radio and is retuned as soon as the first picture lands.
    fn build(
        gci: &crate::gci::GciConfig,
        rx_tx: Option<tokio::sync::mpsc::UnboundedSender<srs::Transmission>>,
    ) -> Option<Self> {
        let opus = match srs::Opus::load(gci.opus_dll_path.as_deref()) {
            Ok(o) => Some(Arc::new(o)),
            Err(e) => {
                log::error!("atc: could not load opus.dll ({e:#}) — ATC cannot transmit");
                None
            }
        };
        let side_tts = |model: &Option<std::path::PathBuf>, voice: &Option<String>| -> tts::Tts {
            tts::Tts::from_cfg(
                gci.piper_exe.as_ref(),
                model.as_ref().or(gci.piper_model.as_ref()),
                voice.as_ref().or(gci.tts_voice.as_ref()),
            )
        };
        let mk = |coalition: u8, name: &str, pw: &str| -> Option<srs::SrsClient> {
            srs::SrsClient::start(
                &gci.srs_host,
                gci.srs_port,
                coalition,
                name,
                // Placeholder until the first poll retunes us.
                vec![Radio::new(1.0, 0, name)],
                pw,
                opus.clone(),
                rx_tx.clone(),
            )
            .map_err(|e| log::error!("atc: SRS '{name}' failed to start: {e:#}"))
            .ok()
        };
        let blue = mk(2, "Blue ATC", &gci.blue_eam_password);
        let red = mk(1, "Red ATC", &gci.red_eam_password);
        if blue.is_none() && red.is_none() {
            return None;
        }
        Some(AtcVoice::new(
            blue,
            red,
            side_tts(&gci.blue_piper_model, &gci.blue_tts_voice),
            side_tts(&gci.red_piper_model, &gci.red_tts_voice),
        ))
    }
}

/// Everything the recogniser should expect to hear on an ATC frequency.
pub(crate) fn atc_vocabulary(picture: &AtcPicture) -> Vec<String> {
    let mut v: Vec<String> = vec![
        "tower".into(),
        "ground".into(),
        "approach".into(),
        "request taxi".into(),
        "request takeoff".into(),
        "cleared to land".into(),
        "inbound".into(),
        "initial".into(),
        "overhead break".into(),
        "go around".into(),
        "altimeter".into(),
        "with information".into(),
    ];
    v.extend(picture.airfields.iter().map(|a| a.name.clone()));
    v
}
