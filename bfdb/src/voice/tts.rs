// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential — no license granted. See the repository NOTICE.

//! Text-to-speech → 16 kHz mono `i16` PCM (the format the SRS Opus stream
//! wants). Two engines: Piper (neural, offline) or Windows SAPI via a tiny
//! PowerShell one-liner. Both are blocking subprocess calls — run them from
//! `tokio::task::spawn_blocking`.

use anyhow::{Context, Result};
use std::{
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

/// SRS network audio is 16 kHz mono.
pub(crate) const SRS_RATE: u32 = 16_000;

#[derive(Clone)]
pub(crate) enum Tts {
    Piper { exe: PathBuf, model: PathBuf },
    Sapi { voice: Option<String> },
}

impl Tts {
    pub(crate) fn from_cfg(
        piper_exe: Option<&PathBuf>,
        piper_model: Option<&PathBuf>,
        voice: Option<&String>,
    ) -> Self {
        match (piper_exe, piper_model) {
            (Some(e), Some(m)) => Tts::Piper {
                exe: e.clone(),
                model: m.clone(),
            },
            _ => Tts::Sapi {
                voice: voice.cloned(),
            },
        }
    }

    pub(crate) fn describe(&self) -> String {
        match self {
            Tts::Piper { model, .. } => format!("Piper ({})", model.display()),
            Tts::Sapi { voice: Some(v) } => format!("Windows SAPI ({v})"),
            Tts::Sapi { voice: None } => "Windows SAPI (default voice)".into(),
        }
    }

    /// Synthesize `text` to 16 kHz mono `i16` PCM. Blocking.
    pub(crate) fn synthesize(&self, text: &str) -> Result<Vec<i16>> {
        let tmp = std::env::temp_dir().join(format!("bfgci-{}.wav", uuid::Uuid::new_v4()));
        let r = (|| -> Result<Vec<i16>> {
            match self {
                Tts::Piper { exe, model } => run_piper(exe, model, text, &tmp)?,
                Tts::Sapi { voice } => run_sapi(voice.as_deref(), text, &tmp)?,
            }
            let (samples, rate) = read_wav_mono(&tmp)?;
            Ok(resample_to(&samples, rate, SRS_RATE))
        })();
        let _ = std::fs::remove_file(&tmp);
        r
    }
}

fn run_piper(exe: &Path, model: &Path, text: &str, out: &Path) -> Result<()> {
    let mut child = Command::new(exe)
        .arg("--model")
        .arg(model)
        .arg("--output_file")
        .arg(out)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .context("spawning piper")?;
    child
        .stdin
        .take()
        .context("piper stdin")?
        .write_all(text.as_bytes())?;
    let status = child.wait()?;
    if !status.success() {
        anyhow::bail!("piper exited {status}");
    }
    Ok(())
}

fn run_sapi(voice: Option<&str>, text: &str, out: &Path) -> Result<()> {
    // System.Speech is present on every Windows install. Text is passed on
    // stdin so it never has to be escaped into the command line.
    let select_voice = match voice {
        Some(v) => format!("$s.SelectVoice('{}');", v.replace('\'', "''")),
        None => String::new(),
    };
    let script = format!(
        "Add-Type -AssemblyName System.Speech; \
         $s = New-Object System.Speech.Synthesis.SpeechSynthesizer; \
         {select_voice} \
         $s.SetOutputToWaveFile('{}'); \
         $s.Speak([Console]::In.ReadToEnd()); \
         $s.Dispose()",
        out.display().to_string().replace('\'', "''"),
    );
    let mut child = Command::new("powershell")
        .args(["-NoProfile", "-NonInteractive", "-Command", &script])
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .context("spawning powershell for SAPI")?;
    child
        .stdin
        .take()
        .context("powershell stdin")?
        .write_all(text.as_bytes())?;
    let status = child.wait()?;
    if !status.success() {
        anyhow::bail!("SAPI (powershell) exited {status}");
    }
    if !out.exists() {
        anyhow::bail!("SAPI produced no output file");
    }
    Ok(())
}

/// Read a WAV file, downmixing to mono `f`-free `i16`. Returns (samples, rate).
fn read_wav_mono(path: &Path) -> Result<(Vec<i16>, u32)> {
    let mut reader = hound::WavReader::open(path).context("opening synth WAV")?;
    let spec = reader.spec();
    let ch = spec.channels.max(1) as usize;
    let raw: Vec<i32> = match spec.sample_format {
        hound::SampleFormat::Int => reader
            .samples::<i32>()
            .collect::<Result<_, _>>()
            .context("reading WAV samples")?,
        hound::SampleFormat::Float => reader
            .samples::<f32>()
            .map(|s| s.map(|v| (v.clamp(-1.0, 1.0) * 32767.0) as i32))
            .collect::<Result<_, _>>()
            .context("reading WAV samples")?,
    };
    // Normalize bit depth to i16 range.
    let shift = (spec.bits_per_sample as i32 - 16).max(0);
    let mono: Vec<i16> = raw
        .chunks(ch)
        .map(|frame| {
            let sum: i64 = frame.iter().map(|&s| (s >> shift) as i64).sum();
            (sum / ch as i64).clamp(i16::MIN as i64, i16::MAX as i64) as i16
        })
        .collect();
    Ok((mono, spec.sample_rate))
}

/// Linear-interpolation resample. Adequate for speech.
fn resample_to(input: &[i16], from: u32, to: u32) -> Vec<i16> {
    if from == to || input.is_empty() {
        return input.to_vec();
    }
    let ratio = from as f64 / to as f64;
    let out_len = ((input.len() as f64) / ratio).ceil() as usize;
    let mut out = Vec::with_capacity(out_len);
    for i in 0..out_len {
        let src = i as f64 * ratio;
        let j = src.floor() as usize;
        let frac = src - j as f64;
        let a = input.get(j).copied().unwrap_or(0) as f64;
        let b = input.get(j + 1).copied().unwrap_or(a as i16) as f64;
        out.push((a + (b - a) * frac).round() as i16);
    }
    out
}
