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

use super::{Pos, VoiceCfg};
use crate::voice::tts::{self, Tts};
use std::{collections::HashMap, path::PathBuf};

/// Positions in casting order: the busiest get the first (usually the
/// best) voices.
const ORDER: [Pos; 7] = [Pos::Tower, Pos::Ground, Pos::Approach, Pos::Departure, Pos::Clearance, Pos::Final, Pos::Atis];

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
    }
}

#[derive(Clone, Debug)]
enum Candidate {
    Piper { model: PathBuf, speaker: Option<u32> },
    Sapi { voice: Option<String> },
}

#[derive(Clone)]
pub(crate) struct Cast {
    controllers: HashMap<Pos, Tts>,
    pilots: Vec<Tts>,
    summary: String,
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
                .filter(|p| p.extension().map_or(false, |x| x.eq_ignore_ascii_case("onnx")) && p != model)
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
            let k = n.min(16);
            for i in 0..k {
                let s = ((i as u64 * 7919) % n as u64) as u32;
                out.push(Candidate::Piper { model: m.clone(), speaker: Some(s) });
            }
        }
    }
    out
}

impl Cast {
    /// Blocking: reads model folders, or asks Windows for its SAPI voices.
    pub(crate) fn build(gci: &crate::gci::GciConfig, overrides: &HashMap<Pos, VoiceCfg>) -> Self {
        let cands: Vec<Candidate> = match (&gci.piper_exe, &gci.piper_model) {
            (Some(_), Some(m)) => piper_candidates(m),
            _ => {
                let mut v: Vec<Candidate> = tts::sapi_voices().into_iter().map(|n| Candidate::Sapi { voice: Some(n) }).collect();
                if let Some(pref) = &gci.tts_voice {
                    v.retain(|c| !matches!(c, Candidate::Sapi { voice: Some(n) } if n == pref));
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
                        speaker: o.speaker.or(if o.piper_model.is_some() { None } else { speaker }),
                        length_scale: o.speed.map(|s| 1. / s.max(0.3)).or(length_scale),
                    },
                    Tts::Sapi { voice, rate } => Tts::Sapi {
                        voice: o.tts_voice.clone().or(voice),
                        rate: o.speed.map(|s| ((s - 1.) * 10.).round() as i32).unwrap_or(rate),
                    },
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
            Some(Candidate::Piper { .. }) => format!("Piper, {n} voice(s) available, {distinct} distinct across the positions"),
            _ => format!("Windows SAPI, {n} voice(s) installed, varied by rate"),
        };
        if n < ORDER.len() {
            log::info!(
                "milatc: only {n} voice(s) for {} positions -- install a multi-speaker Piper model (en_US-libritts_r-medium) for a different voice on every frequency",
                ORDER.len()
            );
        }
        Cast { controllers, pilots, summary }
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
