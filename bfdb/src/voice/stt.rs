// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential — no license granted. See the repository NOTICE.

//! Player → controller speech recognition, plus a NATO-brevity request parser.
//!
//! Two whisper.cpp backends:
//!
//! - **CLI** (`whisper-cli.exe` / `main.exe` from a release): no setup beyond
//!   dropping in a binary and a ggml model, but it reloads the model on every
//!   single transmission. Fine for one GCI channel, poor once ATC and JTAC are
//!   also listening.
//! - **Server** (`whisper-server.exe --host … --port …`): the model stays
//!   resident and transcription is an HTTP round trip. Strongly preferred when
//!   more than one position is live.
//!
//! Both are primed with a **vocabulary prompt**. Whisper decodes proper nouns
//! badly on radio-quality audio, and proper nouns are exactly what matters
//! here — the controller callsign that addresses a call, and the brevity term
//! that identifies it. Feeding the expected words in as an initial prompt
//! biases the decoder toward them and is the single cheapest accuracy win
//! available.

use anyhow::{Context, Result};
use std::{
    io::Write,
    path::{Path, PathBuf},
    process::Command,
    sync::{Arc, RwLock},
    time::Duration,
};

/// Brevity and procedure words every position shares. Position-specific words
/// (controller callsigns, airfield names, flight callsigns on the server) are
/// appended at runtime by [`Stt::set_vocabulary`].
const BASE_VOCABULARY: &str = "Radio check. Bogey dope. Picture. Declare. Snaplock. Alpha check. \
Commit. Bullseye. Tally. Contact. Judy. Merged. Faded. Cleared hot. Continue dry. Abort, abort. \
In from the south. IP inbound. Nine line. Laser on. Shift. Cleared to land. Cleared for takeoff. \
Taxi to the active. Request startup. Inbound for the overhead break. Gear down and locked. \
Say again. Wilco. Roger.";

#[derive(Clone)]
enum Backend {
    Cli { exe: PathBuf, model: PathBuf },
    Server { url: String, http: reqwest::blocking::Client },
}

#[derive(Clone)]
pub(crate) struct Stt {
    backend: Backend,
    /// Initial prompt handed to the decoder — the words we expect to hear.
    vocabulary: Arc<RwLock<String>>,
}

impl Stt {
    /// Build from config. A `server_url` wins over the CLI pair when both are
    /// set; `None` when neither is configured (speech recognition disabled).
    pub(crate) fn from_cfg(
        exe: Option<&PathBuf>,
        model: Option<&PathBuf>,
        server_url: Option<&str>,
    ) -> Option<Self> {
        let backend = match server_url.map(str::trim).filter(|s| !s.is_empty()) {
            Some(url) => {
                let http = reqwest::blocking::Client::builder()
                    .timeout(Duration::from_secs(20))
                    .build()
                    .ok()?;
                Backend::Server {
                    url: format!("{}/inference", url.trim_end_matches('/')),
                    http,
                }
            }
            None => Backend::Cli {
                exe: exe?.clone(),
                model: model?.clone(),
            },
        };
        Some(Stt {
            backend,
            vocabulary: Arc::new(RwLock::new(BASE_VOCABULARY.to_string())),
        })
    }

    pub(crate) fn describe(&self) -> String {
        match &self.backend {
            Backend::Cli { model, .. } => format!("whisper.cpp CLI ({})", model.display()),
            Backend::Server { url, .. } => format!("whisper.cpp server ({url})"),
        }
    }

    /// Add the names this server actually uses — controller callsigns, airfield
    /// names, the flight callsigns currently flying — to the decoder's prompt.
    pub(crate) fn set_vocabulary(&self, words: &[String]) {
        let mut v = BASE_VOCABULARY.to_string();
        for w in words {
            let w = w.trim();
            if !w.is_empty() {
                v.push(' ');
                v.push_str(w);
                v.push('.');
            }
        }
        if let Ok(mut cur) = self.vocabulary.write() {
            *cur = v;
        }
    }

    fn prompt(&self) -> String {
        self.vocabulary
            .read()
            .map(|v| v.clone())
            .unwrap_or_else(|_| BASE_VOCABULARY.to_string())
    }

    /// Transcribe 16 kHz mono `i16` PCM to lowercased text. Blocking.
    pub(crate) fn transcribe(&self, pcm: &[i16]) -> Result<String> {
        let pcm = normalize(pcm);
        match &self.backend {
            Backend::Cli { exe, model } => self.transcribe_cli(exe, model, &pcm),
            Backend::Server { url, http } => self.transcribe_server(url, http, &pcm),
        }
    }

    fn transcribe_cli(&self, exe: &Path, model: &Path, pcm: &[i16]) -> Result<String> {
        let base = std::env::temp_dir().join(format!("bfvoice-stt-{}", uuid::Uuid::new_v4()));
        let wav = base.with_extension("wav");
        write_wav(&wav, pcm)?;
        let out = Command::new(exe)
            .arg("-m")
            .arg(model)
            .arg("-f")
            .arg(&wav)
            // `--prompt` biases the decoder toward our brevity vocabulary;
            // `-bs 5` (beam search) recovers noticeably more of a clipped or
            // noisy radio call than the default greedy decode.
            .args(["-l", "en", "-nt", "-np", "-bs", "5", "--prompt"])
            .arg(self.prompt())
            .args(["-otxt", "-of"])
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
        Ok(clean(&text))
    }

    fn transcribe_server(
        &self,
        url: &str,
        http: &reqwest::blocking::Client,
        pcm: &[i16],
    ) -> Result<String> {
        let wav = wav_bytes(pcm);
        let part = reqwest::blocking::multipart::Part::bytes(wav)
            .file_name("call.wav")
            .mime_str("audio/wav")
            .context("building whisper request")?;
        let form = reqwest::blocking::multipart::Form::new()
            .part("file", part)
            .text("temperature", "0.0")
            .text("response_format", "json")
            .text("language", "en")
            .text("prompt", self.prompt());
        let resp = http
            .post(url)
            .multipart(form)
            .send()
            .with_context(|| format!("posting to whisper server at {url}"))?;
        if !resp.status().is_success() {
            anyhow::bail!("whisper server returned {}", resp.status());
        }
        let v: serde_json::Value = resp.json().context("decoding whisper server reply")?;
        let text = v
            .get("text")
            .and_then(|t| t.as_str())
            .unwrap_or_default()
            .to_string();
        Ok(clean(&text))
    }
}

/// Whisper emits bracketed non-speech annotations ("[BLANK_AUDIO]", "(engine
/// noise)") that would otherwise be parsed as words.
fn clean(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    let mut depth = 0i32;
    for ch in raw.chars() {
        match ch {
            '[' | '(' => depth += 1,
            ']' | ')' => depth = (depth - 1).max(0),
            _ if depth == 0 => out.push(ch),
            _ => {}
        }
    }
    out.trim().to_lowercase()
}

/// Bring a quiet transmission up to a consistent level. Mic gain varies wildly
/// between players and whisper degrades badly on very low-amplitude audio.
fn normalize(pcm: &[i16]) -> Vec<i16> {
    let peak = pcm.iter().map(|s| s.unsigned_abs() as u32).max().unwrap_or(0);
    // Already healthy, or pure silence — leave it alone.
    if peak == 0 || peak > 20_000 {
        return pcm.to_vec();
    }
    let gain = (22_000.0 / peak as f64).min(12.0);
    pcm.iter()
        .map(|s| (*s as f64 * gain).clamp(-32_768.0, 32_767.0) as i16)
        .collect()
}

/// Minimal 16-bit mono 16 kHz PCM WAV — avoids pulling hound just for this.
fn wav_bytes(pcm: &[i16]) -> Vec<u8> {
    let data_len = (pcm.len() * 2) as u32;
    let mut b = Vec::with_capacity(44 + pcm.len() * 2);
    b.extend_from_slice(b"RIFF");
    b.extend_from_slice(&(36 + data_len).to_le_bytes());
    b.extend_from_slice(b"WAVEfmt ");
    b.extend_from_slice(&16u32.to_le_bytes()); // fmt chunk size
    b.extend_from_slice(&1u16.to_le_bytes()); // PCM
    b.extend_from_slice(&1u16.to_le_bytes()); // mono
    b.extend_from_slice(&16_000u32.to_le_bytes()); // sample rate
    b.extend_from_slice(&32_000u32.to_le_bytes()); // byte rate = 16000*1*2
    b.extend_from_slice(&2u16.to_le_bytes()); // block align
    b.extend_from_slice(&16u16.to_le_bytes()); // bits per sample
    b.extend_from_slice(b"data");
    b.extend_from_slice(&data_len.to_le_bytes());
    for s in pcm {
        b.extend_from_slice(&s.to_le_bytes());
    }
    b
}

fn write_wav(path: &Path, pcm: &[i16]) -> Result<()> {
    std::fs::File::create(path)?.write_all(&wav_bytes(pcm))?;
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

/// Why a transmission was ignored — logged so a player who gets no answer can
/// see whether the wake word or the request keyword was the problem.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Reject {
    /// The controller was never named (or `wake` is blank).
    NotAddressed,
    /// Addressed to us, but no known brevity request in it.
    UnknownRequest,
}

/// Parse a recognised transmission, with the reason it declined: `None` if it
/// isn't addressed to the controller (`wake` = its callsign) or isn't a known
/// request.
pub(crate) fn parse_request_dbg(text: &str, wake: &str) -> (Option<Request>, Option<Reject>) {
    let tokens = tokenize(text);
    let joined: String = tokens.concat();
    let wake_tokens = tokenize(wake);
    if wake_tokens.is_empty() || !wake_matches(&tokens, &joined, &wake_tokens) {
        return (None, Some(Reject::NotAddressed));
    }
    let has = |p: &str| phrase_in(&tokens, &joined, p);
    let req = if has("radio check") || has("comm check") || has("how do you read") || has("how copy")
    {
        Request::RadioCheck
    } else if has("checking in") || has("check in") || has("on station") {
        Request::CheckIn
    } else if has("alpha check") {
        Request::AlphaCheck
    } else if has("bogey dope") || has("bogeydope") || has("bogey") || has("bogie") {
        Request::BogeyDope
    } else if has("snaplock") || has("snap lock") {
        Request::Snaplock
    } else if has("declare") {
        Request::Declare(parse_bullseye(text))
    } else if has("picture") {
        Request::Picture
    } else if has("commit") {
        Request::Commit
    } else {
        return (None, Some(Reject::UnknownRequest));
    };
    (Some(req), None)
}

/// Is `phrase` present in a recognised transmission, allowing for the words
/// whisper tends to mangle and lost word boundaries? The shared entry point for
/// every position's grammar.
pub(crate) fn contains_phrase(text: &str, phrase: &str) -> bool {
    let tokens = tokenize(text);
    let joined: String = tokens.concat();
    phrase_in(&tokens, &joined, phrase)
}

/// Was this name spoken? Same tolerance as the controller wake word — a field
/// or flight callsign survives being mis-heard or split in two.
pub(crate) fn mentions(text: &str, name: &str) -> bool {
    let tokens = tokenize(text);
    let joined: String = tokens.concat();
    let want = tokenize(name);
    if want.is_empty() {
        return false;
    }
    wake_matches(&tokens, &joined, &want)
}

// ─── fuzzy matching ───────────────────────────────────────────────────────
//
// Whisper mangles proper nouns and brevity terms ("Overlord" → "overload" /
// "over lord", "bogey dope" → "bogey dope."), so every keyword comparison is
// edit-distance tolerant and word-boundary agnostic rather than a plain
// `contains`.

/// Lowercase alphanumeric words.
fn tokenize(t: &str) -> Vec<String> {
    t.split(|c: char| !c.is_alphanumeric())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_lowercase())
        .collect()
}

/// Allowed edit distance for a word of this length — short words must be
/// exact, so a 4-letter callsign doesn't fire on every similar word.
fn tol(len: usize) -> usize {
    match len {
        0..=4 => 0,
        5..=7 => 1,
        _ => 2,
    }
}

fn close(a: &str, b: &str) -> bool {
    if a == b {
        return true;
    }
    let (av, bv): (Vec<char>, Vec<char>) = (a.chars().collect(), b.chars().collect());
    let t = tol(bv.len());
    if av.len().abs_diff(bv.len()) > t {
        return false;
    }
    lev(&av, &bv) <= t
}

fn lev(a: &[char], b: &[char]) -> usize {
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut cur = vec![0usize; b.len() + 1];
    for (i, ca) in a.iter().enumerate() {
        cur[0] = i + 1;
        for (j, cb) in b.iter().enumerate() {
            let sub = prev[j] + usize::from(ca != cb);
            cur[j + 1] = sub.min(prev[j + 1] + 1).min(cur[j] + 1);
        }
        std::mem::swap(&mut prev, &mut cur);
    }
    prev[b.len()]
}

/// Is `phrase` (one or more words) present in the transmission, allowing for
/// mis-heard words and lost/added word boundaries?
fn phrase_in(tokens: &[String], joined: &str, phrase: &str) -> bool {
    let want = tokenize(phrase);
    if want.is_empty() {
        return false;
    }
    // Boundaries lost entirely ("radiocheck", "bogeydope").
    if joined.contains(&want.concat()) {
        return true;
    }
    tokens
        .windows(want.len())
        .any(|w| w.iter().zip(&want).all(|(a, b)| close(a, b)))
}

/// The controller's callsign, allowing for a mis-heard or split-up name
/// ("overload", "over lord") — a single-word callsign is also compared against
/// adjacent word pairs joined back together.
fn wake_matches(tokens: &[String], joined: &str, wake: &[String]) -> bool {
    if phrase_in(tokens, joined, &wake.join(" ")) {
        return true;
    }
    let head = &wake[0];
    if head.chars().count() <= 3 {
        return false; // too short to fuzz safely
    }
    tokens.iter().any(|t| close(t, head))
        || tokens
            .windows(2)
            .any(|w| close(&format!("{}{}", w[0], w[1]), head))
        || tokens
            .windows(3)
            .any(|w| close(&format!("{}{}{}", w[0], w[1], w[2]), head))
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
