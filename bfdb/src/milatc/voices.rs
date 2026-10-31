// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! Who sounds like whom: a different voice on every frequency, and a pool of
//! others for the AI pilots.
//!
//! Casting is automatic from whatever is installed:
//!
//! - **Piper with a multi-speaker model** (`en_US-libritts_r-medium` has
//!   hundreds of speakers): every position gets its own speaker.
//! - **Several Piper models in the model's folder**: they are dealt out to
//!   the positions in turn.
//! - **Windows SAPI**: the installed voices are dealt out, and each position
//!   also gets its own speaking rate, so even one voice sounds like several
//!   people.
//!
//! Each position also has its own pace: ATIS reads slowly, Clearance rattles
//! the clearance off, the PAR controller is deliberate. The `voices` config
//! block overrides any of it.
//!
//! With a speech service configured (`tts`: OpenAI, ElevenLabs, or Kokoro on
//! the box), every position and AI pilot gets one of its voices, which sound
//! like people, with the local voice above as the fallback. OpenAI's gpt-4o
//! voices are also told how each position talks ([`style`]).

use super::{NeuralCfg, Pos, VoiceCfg};
use crate::voice::tts::{self, Api, Neural, Tts};
use std::{collections::HashMap, path::PathBuf, sync::Arc};

/// Positions in casting order: the busiest get the first (usually the
/// best) voices.
const ORDER: [Pos; 11] = [
    Pos::Tower,
    Pos::Ground,
    Pos::Approach,
    Pos::Departure,
    Pos::Clearance,
    Pos::Final,
    Pos::Atis,
    Pos::Marshal,
    Pos::Paddles,
    Pos::Range,
    Pos::Tanker,
];

/// Piper `length_scale` (above 1 is slower) and SAPI rate per position.
fn pace(pos: Pos) -> (f32, i32) {
    match pos {
        Pos::Atis => (1.08, -1),
        Pos::Clearance => (0.88, 2),
        Pos::Ground => (0.98, 0),
        Pos::Tower => (0.94, 1),
        Pos::Departure => (0.92, 2),
        Pos::Approach => (0.96, 1),
        Pos::Final => (1.04, 0),
        Pos::Marshal => (0.95, 1),
        // the LSO talks fast in the groove
        Pos::Paddles => (0.86, 3),
        Pos::Tanker => (1.0, 0),
        Pos::Range => (0.97, 1),
    }
}

#[derive(Clone, Debug)]
enum Candidate {
    Piper { model: PathBuf, speaker: Option<u32> },
    Sapi { voice: Option<String> },
}

/// How each position talks, for a voice that takes direction.
fn style(pos: Pos) -> &'static str {
    match pos {
        Pos::Atis => "Read it like a recorded ATIS broadcast: even, measured, a little flat, clear pauses between items.",
        Pos::Clearance => "Rattle the clearance off quickly and evenly, like someone who has read it a thousand times today.",
        Pos::Ground => "Easygoing and conversational, unhurried, friendly but professional.",
        Pos::Tower => "Brisk and alert: a busy tower controller, calm, clipped, confident.",
        Pos::Departure | Pos::Approach => "Fast, smooth radar-controller cadence, words run together on the routine parts.",
        Pos::Final => "Calm, steady and deliberate, the even rhythm of a PAR controller talking a pilot down to the runway.",
        Pos::Marshal => "A Navy carrier controller: businesslike, quick, matter-of-fact.",
        Pos::Paddles => "A Navy LSO watching a jet in the groove: short, punchy calls, urgent and loud when correcting.",
        Pos::Range => "A laid-back range control officer: relaxed, dry, professional.",
        Pos::Tanker => "A tanker crew member: friendly, casual, relaxed.",
    }
}

const STYLE_BASE: &str = "You are a real US military air traffic controller on a UHF radio. Sound like a person at work, never an announcer or narrator: natural rhythm and breathing, slight run-together of routine phrases, numbers said crisply. Say exactly the words given.";
const STYLE_PILOT: &str = "You are a military pilot talking on the radio from the cockpit: quick, terse, a little breathy, matter-of-fact, the way pilots really sound on frequency. Say exactly the words given.";

/// Voices of each service, controllers' first: the busiest positions get the
/// most natural ones.
const OPENAI_VOICES: [&str; 13] = [
    "ash", "onyx", "echo", "verse", "ballad", "sage", "alloy", "cedar", "coral", "fable",
    "marin", "nova", "shimmer",
];
const KOKORO_VOICES: [&str; 19] = [
    "am_michael",
    "am_adam",
    "bm_george",
    "am_eric",
    "am_liam",
    "af_heart",
    "bf_emma",
    "am_onyx",
    "am_puck",
    "bm_lewis",
    "af_sarah",
    "am_fenrir",
    "bm_daniel",
    "am_echo",
    "af_bella",
    "bm_fable",
    "af_nicole",
    "af_sky",
    "bf_isabella",
];

/// The speech service, set up: its voices, and whether it speaks whatever
/// happens or only when someone can hear it.
struct Service {
    api: Api,
    url: String,
    key: Option<String>,
    model: String,
    voices: Vec<String>,
    always: bool,
    pilots: bool,
}

fn env(k: &str) -> Option<String> {
    std::env::var(k).ok().filter(|v| !v.trim().is_empty())
}

/// Blocking (lists an ElevenLabs account's voices).
fn service(cfg: &NeuralCfg) -> Option<Service> {
    let engine = cfg.engine.trim().to_lowercase();
    let (api, url, model, cloud) = match engine.as_str() {
        "openai" => (Api::OpenAi, "https://api.openai.com", "gpt-4o-mini-tts", true),
        "elevenlabs" | "eleven" => {
            (Api::ElevenLabs, "https://api.elevenlabs.io", "eleven_flash_v2_5", true)
        }
        "kokoro" | "local" => (Api::OpenAi, "http://127.0.0.1:8880", "kokoro", false),
        "" | "none" | "off" => return None,
        other => {
            log::warn!(
                "range ATC: unknown tts engine {other:?} (openai, elevenlabs or kokoro)"
            );
            return None;
        }
    };
    let url = cfg
        .url
        .clone()
        .filter(|u| !u.trim().is_empty())
        .unwrap_or_else(|| url.to_string());
    let model = cfg
        .model
        .clone()
        .filter(|m| !m.trim().is_empty())
        .unwrap_or_else(|| model.to_string());
    let key = cfg
        .key
        .clone()
        .filter(|k| !k.trim().is_empty())
        .or_else(|| env("BFDB_ATC_TTS_KEY"))
        .or_else(|| match api {
            Api::OpenAi if cloud => env("OPENAI_API_KEY"),
            Api::ElevenLabs => env("ELEVENLABS_API_KEY"),
            _ => None,
        });
    if cloud && key.is_none() {
        log::warn!("range ATC: tts engine {engine} needs an API key (bfdb.atc_tts_key in the bot's YAML) -- using the local voices");
        return None;
    }
    let mut voices: Vec<String> =
        cfg.voices.iter().filter(|v| !v.trim().is_empty()).cloned().collect();
    if voices.is_empty() {
        voices = match (engine.as_str(), api) {
            (_, Api::ElevenLabs) => {
                tts::elevenlabs_voices(&url, key.as_deref().unwrap_or_default())
                    .into_iter()
                    .map(|(id, _)| id)
                    .collect()
            }
            ("openai", _) => OPENAI_VOICES.iter().map(|v| v.to_string()).collect(),
            _ => KOKORO_VOICES.iter().map(|v| v.to_string()).collect(),
        };
    }
    if voices.is_empty() {
        log::warn!(
            "range ATC: tts engine {engine} has no voices -- using the local voices"
        );
        return None;
    }
    let always = match cfg.when.as_deref().map(|w| w.trim().to_lowercase()) {
        Some(w) if w == "always" => true,
        Some(w) if w == "audience" => false,
        _ => !cloud,
    };
    Some(Service { api, url, key, model, voices, always, pilots: cfg.pilots })
}

#[derive(Clone)]
pub(crate) struct Cast {
    controllers: HashMap<Pos, Tts>,
    pilots: Vec<Tts>,
    summary: String,
    /// The speech service speaks even when nobody can hear it (a local
    /// service costs nothing); otherwise only for an audience.
    pub always_neural: bool,
}

/// How many speakers a Piper model has, from its `<model>.json`.
fn speakers(model: &std::path::Path) -> u32 {
    let cfg = PathBuf::from(format!("{}.json", model.display()));
    std::fs::read_to_string(cfg)
        .ok()
        .and_then(|s| serde_json::from_str::<serde_json::Value>(&s).ok())
        .and_then(|v| v.get("num_speakers").and_then(|n| n.as_u64()))
        .unwrap_or(1)
        .max(1) as u32
}

/// Every Piper voice on offer: the configured model first, then the other
/// models beside it; a multi-speaker model contributes a spread of speakers.
fn piper_candidates(model: &std::path::Path) -> Vec<Candidate> {
    let mut models = vec![model.to_path_buf()];
    if let Some(dir) = model.parent() {
        if let Ok(rd) = std::fs::read_dir(dir) {
            let mut more: Vec<PathBuf> = rd
                .filter_map(|e| e.ok().map(|e| e.path()))
                .filter(|p| {
                    p.extension().is_some_and(|x| x.eq_ignore_ascii_case("onnx"))
                        && p != model
                })
                .collect();
            more.sort();
            models.extend(more);
        }
    }
    let mut out = vec![];
    for m in models {
        let n = speakers(&m);
        if n <= 1 {
            out.push(Candidate::Piper { model: m, speaker: None });
        } else {
            // A spread across the speaker list: neighbours in a libritts
            // model often sound alike.
            let k = n.min(32);
            for i in 0..k {
                let s = ((i as u64 * 7919) % n as u64) as u32;
                out.push(Candidate::Piper { model: m.clone(), speaker: Some(s) });
            }
        }
    }
    out
}

impl Cast {
    /// Blocking: reads model folders, asks Windows for its SAPI voices, or a
    /// speech service for its.
    pub(crate) fn build(
        gci: &crate::gci::GciConfig,
        overrides: &HashMap<Pos, VoiceCfg>,
        neural: Option<&NeuralCfg>,
    ) -> Self {
        let cands: Vec<Candidate> = match (&gci.piper_exe, &gci.piper_model) {
            (Some(_), Some(m)) => piper_candidates(m),
            _ => {
                let mut v: Vec<Candidate> = tts::sapi_voices()
                    .into_iter()
                    .map(|n| Candidate::Sapi { voice: Some(n) })
                    .collect();
                if let Some(pref) = &gci.tts_voice {
                    v.retain(
                        |c| !matches!(c, Candidate::Sapi { voice: Some(n) } if n == pref),
                    );
                    v.insert(0, Candidate::Sapi { voice: Some(pref.clone()) });
                }
                if v.is_empty() {
                    v.push(Candidate::Sapi { voice: None });
                }
                v
            }
        };
        let make = |c: &Candidate, length: f32, rate: i32| -> Tts {
            match c {
                Candidate::Piper { model, speaker } => Tts::Piper {
                    exe: gci.piper_exe.clone().unwrap_or_default(),
                    model: model.clone(),
                    speaker: *speaker,
                    length_scale: Some(length),
                },
                Candidate::Sapi { voice } => Tts::Sapi { voice: voice.clone(), rate },
            }
        };
        let mut controllers = HashMap::new();
        for (i, pos) in ORDER.iter().enumerate() {
            let (length, rate) = pace(*pos);
            let mut t = make(&cands[i % cands.len()], length, rate);
            if let Some(o) = overrides.get(pos) {
                t = match t {
                    Tts::Piper { exe, model, speaker, length_scale } => Tts::Piper {
                        exe,
                        model: o.piper_model.clone().unwrap_or(model),
                        speaker: o.speaker.or(if o.piper_model.is_some() {
                            None
                        } else {
                            speaker
                        }),
                        length_scale: o.speed.map(|s| 1. / s.max(0.3)).or(length_scale),
                    },
                    Tts::Sapi { voice, rate } => Tts::Sapi {
                        voice: o.tts_voice.clone().or(voice),
                        rate: o
                            .speed
                            .map(|s| ((s - 1.) * 10.).round() as i32)
                            .unwrap_or(rate),
                    },
                    // the local cast is never neural
                    n @ Tts::Neural { .. } => n,
                };
            }
            controllers.insert(*pos, t);
        }
        // Pilots: whatever the controllers did not take first, then the rest
        // again at a different pace, so a pilot rarely sounds like a
        // controller.
        let mut pilots = vec![];
        let n = cands.len();
        for j in 0..n.max(4) {
            let c = &cands[(ORDER.len() + j) % n];
            let length = 0.9 + 0.04 * (j % 4) as f32;
            let rate = 1 + (j % 3) as i32;
            pilots.push(make(c, length, rate));
        }
        let distinct = n.min(ORDER.len());
        let summary = match cands.first() {
            Some(Candidate::Piper { .. }) => format!(
                "Piper, {n} voice(s) available, {distinct} distinct across the positions"
            ),
            _ => format!("Windows SAPI, {n} voice(s) installed, varied by rate"),
        };
        let Some(svc) = neural.and_then(service) else {
            if n < ORDER.len() {
                log::info!(
                    "milatc: only {n} voice(s) for {} positions -- install a multi-speaker Piper model (en_US-libritts_r-medium) for a different voice on every frequency",
                    ORDER.len()
                );
            }
            return Cast { controllers, pilots, summary, always_neural: false };
        };
        // The service's voices over the local ones, which stay as the fallback.
        let wrap = |local: Tts, voice: &str, speed: f32, style: String| Tts::Neural {
            voice: Arc::new(Neural::new(
                svc.api,
                &svc.url,
                svc.key.clone(),
                &svc.model,
                voice,
                speed,
                Some(style),
            )),
            fallback: Box::new(local),
        };
        let nv = svc.voices.len();
        for (i, pos) in ORDER.iter().enumerate() {
            let local =
                controllers.remove(pos).unwrap_or(Tts::Sapi { voice: None, rate: 0 });
            let o = overrides.get(pos);
            let voice = o
                .and_then(|o| o.voice.clone())
                .unwrap_or_else(|| svc.voices[i % nv].clone());
            let speed = o.and_then(|o| o.speed).unwrap_or_else(|| 1. / pace(*pos).0);
            controllers.insert(
                *pos,
                wrap(local, &voice, speed, format!("{STYLE_BASE} {}", style(*pos))),
            );
        }
        if svc.pilots {
            pilots = pilots
                .into_iter()
                .enumerate()
                .map(|(j, local)| {
                    // the voices the controllers did not take first
                    let voice = svc.voices[(ORDER.len() + j) % nv].clone();
                    wrap(
                        local,
                        &voice,
                        1.0 + 0.04 * (j % 3) as f32,
                        STYLE_PILOT.to_string(),
                    )
                })
                .collect();
        }
        let summary = format!(
            "{} with {nv} voice(s){}{}; {summary} as the fallback",
            svc.model,
            if svc.always {
                ""
            } else {
                ", only while someone is listening (a player on, or a listener on the site)"
            },
            if svc.pilots { ", AI pilots too" } else { "" }
        );
        Cast { controllers, pilots, summary, always_neural: svc.always }
    }

    pub(crate) fn controller(&self, pos: Pos) -> Tts {
        self.controllers.get(&pos).cloned().unwrap_or(Tts::Sapi { voice: None, rate: 0 })
    }

    /// The voice of the AI pilot of flight `key`: the same one every time it
    /// keys up.
    pub(crate) fn pilot(&self, key: u64) -> Tts {
        let h = key.wrapping_mul(0x9E37_79B9_7F4A_7C15) >> 32;
        self.pilots[(h as usize) % self.pilots.len()].clone()
    }

    pub(crate) fn describe(&self) -> &str {
        &self.summary
    }
}
