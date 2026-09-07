// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential — no license granted. See the repository NOTICE.

//! Phase 3: player → GCI. Speech-to-text via the whisper.cpp CLI
//! (`whisper-cli.exe` / `main.exe` from a whisper.cpp release — no build deps),
//! plus a NATO-brevity request parser.

use anyhow::{Context, Result};
use std::{
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Clone)]
pub(crate) struct Stt {
    exe: PathBuf,
    model: PathBuf,
}

impl Stt {
    pub(crate) fn from_cfg(exe: Option<&PathBuf>, model: Option<&PathBuf>) -> Option<Self> {
        Some(Stt {
            exe: exe?.clone(),
            model: model?.clone(),
        })
    }

    pub(crate) fn describe(&self) -> String {
        format!("whisper.cpp ({})", self.model.display())
    }

    /// Transcribe 16 kHz mono `i16` PCM to lowercased text. Blocking.
    pub(crate) fn transcribe(&self, pcm: &[i16]) -> Result<String> {
        let base = std::env::temp_dir().join(format!("bfgci-stt-{}", uuid::Uuid::new_v4()));
        let wav = base.with_extension("wav");
        write_wav(&wav, pcm)?;
        let out = Command::new(&self.exe)
            .arg("-m")
            .arg(&self.model)
            .arg("-f")
            .arg(&wav)
            .args(["-l", "en", "-nt", "-np", "-otxt", "-of"])
            .arg(&base)
            .output()
            .context("spawning whisper-cli")?;
        let _ = std::fs::remove_file(&wav);
        let txt = base.with_extension("txt");
        let text = std::fs::read_to_string(&txt).unwrap_or_default();
        let _ = std::fs::remove_file(&txt);
        if text.trim().is_empty() && !out.status.success() {
            anyhow::bail!(
                "whisper-cli failed: {}",
                String::from_utf8_lossy(&out.stderr).trim()
            );
        }
        Ok(text.trim().to_lowercase())
    }
}

fn write_wav(path: &Path, pcm: &[i16]) -> Result<()> {
    // Minimal 16-bit mono 16 kHz PCM WAV — avoids pulling hound just for this.
    let data_len = (pcm.len() * 2) as u32;
    let mut f = std::fs::File::create(path)?;
    f.write_all(b"RIFF")?;
    f.write_all(&(36 + data_len).to_le_bytes())?;
    f.write_all(b"WAVEfmt ")?;
    f.write_all(&16u32.to_le_bytes())?; // fmt chunk size
    f.write_all(&1u16.to_le_bytes())?; // PCM
    f.write_all(&1u16.to_le_bytes())?; // mono
    f.write_all(&16_000u32.to_le_bytes())?; // sample rate
    f.write_all(&32_000u32.to_le_bytes())?; // byte rate = 16000*1*2
    f.write_all(&2u16.to_le_bytes())?; // block align
    f.write_all(&16u16.to_le_bytes())?; // bits per sample
    f.write_all(b"data")?;
    f.write_all(&data_len.to_le_bytes())?;
    for s in pcm {
        f.write_all(&s.to_le_bytes())?;
    }
    Ok(())
}

// ─── request parsing ──────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Request {
    BogeyDope,
    Picture,
    /// Declare a contact — optional bullseye bearing/range the caller gave.
    Declare(Option<(u16, u32)>),
    Snaplock,
    AlphaCheck,
    RadioCheck,
    CheckIn,
    Commit,
}

/// Parse a recognised transmission. Returns `None` if it isn't addressed to the
/// controller (`wake` = its callsign, lowercased) or isn't a known request.
pub(crate) fn parse_request(text: &str, wake: &str) -> Option<Request> {
    let t = text;
    // Must name the controller (fuzzy — whisper mangles proper nouns).
    let wake_ok = !wake.is_empty()
        && (t.contains(wake)
            || wake
                .split_whitespace()
                .next()
                .map_or(false, |w| w.len() > 3 && t.contains(w)));
    if !wake_ok {
        return None;
    }
    if t.contains("radio check") || t.contains("comm check") || t.contains("how do you read") {
        return Some(Request::RadioCheck);
    }
    if t.contains("checking in") || t.contains("check in") || t.contains("on station") {
        return Some(Request::CheckIn);
    }
    if t.contains("alpha check") {
        return Some(Request::AlphaCheck);
    }
    if t.contains("bogey dope") || t.contains("bogeydope") || t.contains("bogey") {
        return Some(Request::BogeyDope);
    }
    if t.contains("snaplock") || t.contains("snap lock") {
        return Some(Request::Snaplock);
    }
    if t.contains("declare") {
        return Some(Request::Declare(parse_bullseye(t)));
    }
    if t.contains("picture") {
        return Some(Request::Picture);
    }
    if t.contains("commit") {
        return Some(Request::Commit);
    }
    None
}

/// Pull "bullseye 270 for 45" / "bullseye 270 45" out of recognised text.
fn parse_bullseye(t: &str) -> Option<(u16, u32)> {
    let idx = t.find("bullseye")? + "bullseye".len();
    let nums: Vec<u32> = t[idx..]
        .split(|c: char| !c.is_ascii_digit())
        .filter(|s| !s.is_empty())
        .filter_map(|s| s.parse().ok())
        .collect();
    match nums.as_slice() {
        [b, r, ..] => Some(((*b as u16) % 360, *r)),
        _ => None,
    }
}
