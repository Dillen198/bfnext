// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential — no license granted. See the repository NOTICE.

//! Text-to-speech → 16 kHz mono `i16` PCM (the format the SRS Opus stream
//! wants). Engines: Piper (neural, offline), Windows SAPI via a tiny
//! PowerShell one-liner, or a speech service over HTTP ([`Neural`]: OpenAI's
//! voices, ElevenLabs, or Kokoro running locally) for voices that sound like
//! people. All are blocking — run them from `tokio::task::spawn_blocking`.

use anyhow::{Context, Result};
use std::{
    collections::{HashMap, VecDeque},
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::{Arc, Mutex},
    time::Duration,
};

/// SRS network audio is 16 kHz mono.
pub(crate) const SRS_RATE: u32 = 16_000;

#[derive(Clone)]
pub(crate) enum Tts {
    /// `speaker` picks one voice of a multi-speaker model (libritts has
    /// hundreds); `length_scale` is Piper's speed (above 1 is slower).
    Piper { exe: PathBuf, model: PathBuf, speaker: Option<u32>, length_scale: Option<f32> },
    /// `rate` is SAPI's -10..10.
    Sapi { voice: Option<String>, rate: i32 },
    /// A speech service, with a local voice to fall back on when it fails
    /// (or when it is not worth paying for: see [`Tts::local`]).
    Neural { voice: Arc<Neural>, fallback: Box<Tts> },
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
                speaker: None,
                length_scale: None,
            },
            _ => Tts::Sapi {
                voice: voice.cloned(),
                rate: 0,
            },
        }
    }

    pub(crate) fn describe(&self) -> String {
        match self {
            Tts::Piper { model, speaker: Some(s), .. } => format!("Piper ({} speaker {s})", model.display()),
            Tts::Piper { model, .. } => format!("Piper ({})", model.display()),
            Tts::Sapi { voice: Some(v), .. } => format!("Windows SAPI ({v})"),
            Tts::Sapi { voice: None, .. } => "Windows SAPI (default voice)".into(),
            Tts::Neural { voice, .. } => voice.describe(),
        }
    }

    /// The local voice: itself, or a neural voice's fallback.
    pub(crate) fn local(&self) -> &Tts {
        match self {
            Tts::Neural { fallback, .. } => fallback.local(),
            t => t,
        }
    }

    /// Synthesize `text` to 16 kHz mono `i16` PCM. Blocking.
    ///
    /// Retried once. Piper is an external binary and has been seen to die with
    /// an access violation (0xc0000005) mid-run; that dropped the GCI call
    /// outright even though synthesis is pure and a second attempt succeeds.
    /// Each attempt gets its own temp file so a half-written wav from a crashed
    /// run can't be picked up by the retry.
    pub(crate) fn synthesize(&self, text: &str) -> Result<Vec<i16>> {
        if let Tts::Neural { voice, fallback } = self {
            return match voice.synthesize(text) {
                Ok(pcm) => Ok(pcm),
                Err(e) => {
                    voice.failed(&e);
                    fallback.synthesize(text)
                }
            };
        }
        let mut last_err = None;
        for attempt in 0..2 {
            let tmp = std::env::temp_dir().join(format!("bfgci-{}.wav", uuid::Uuid::new_v4()));
            let r = (|| -> Result<Vec<i16>> {
                match self {
                    Tts::Piper { exe, model, speaker, length_scale } => {
                        run_piper(exe, model, *speaker, *length_scale, text, &tmp)?
                    }
                    Tts::Sapi { voice, rate } => run_sapi(voice.as_deref(), *rate, text, &tmp)?,
                    Tts::Neural { .. } => unreachable!(),
                }
                let (samples, rate) = read_wav_mono(hound::WavReader::open(&tmp).context("opening synth WAV")?)?;
                Ok(resample_to(&samples, rate, SRS_RATE))
            })();
            let _ = std::fs::remove_file(&tmp);
            match r {
                Ok(pcm) => return Ok(pcm),
                Err(e) => {
                    if attempt == 0 {
                        log::debug!("tts: synthesis failed ({e}), retrying once");
                    }
                    last_err = Some(e);
                }
            }
        }
        Err(last_err.expect("loop runs at least once"))
    }
}

fn run_piper(exe: &Path, model: &Path, speaker: Option<u32>, length_scale: Option<f32>, text: &str, out: &Path) -> Result<()> {
    let mut cmd = Command::new(exe);
    cmd.arg("--model").arg(model).arg("--output_file").arg(out);
    if let Some(s) = speaker {
        cmd.arg("--speaker").arg(s.to_string());
    }
    if let Some(l) = length_scale {
        cmd.arg("--length_scale").arg(format!("{l:.2}"));
    }
    let mut child = cmd
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

/// The SAPI voices installed on this machine ("Microsoft David Desktop", ...).
pub(crate) fn sapi_voices() -> Vec<String> {
    let out = Command::new("powershell")
        .args([
            "-NoProfile",
            "-NonInteractive",
            "-Command",
            "Add-Type -AssemblyName System.Speech; (New-Object System.Speech.Synthesis.SpeechSynthesizer).GetInstalledVoices() | Where-Object { $_.Enabled } | ForEach-Object { $_.VoiceInfo.Name }",
        ])
        .stderr(Stdio::null())
        .output();
    match out {
        Ok(o) => String::from_utf8_lossy(&o.stdout).lines().map(|l| l.trim().to_string()).filter(|l| !l.is_empty()).collect(),
        Err(_) => vec![],
    }
}

fn run_sapi(voice: Option<&str>, rate: i32, text: &str, out: &Path) -> Result<()> {
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
         $s.Rate = {rate}; \
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

/// Read a WAV, downmixing to mono `i16`. Returns (samples, rate).
fn read_wav_mono<R: std::io::Read>(mut reader: hound::WavReader<R>) -> Result<(Vec<i16>, u32)> {
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

/// Which speech service.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Api {
    /// `POST /v1/audio/speech`: OpenAI itself, or anything speaking its API
    /// (Kokoro-FastAPI on the box).
    OpenAi,
    /// `POST /v1/text-to-speech/<voice id>`.
    ElevenLabs,
}

/// One voice of a speech service.
pub(crate) struct Neural {
    pub api: Api,
    pub url: String,
    pub key: Option<String>,
    pub model: String,
    pub voice: String,
    /// 1 = normal.
    pub speed: f32,
    /// How to say it (OpenAI's gpt-4o voices): "a tower controller, brisk".
    pub style: Option<String>,
    http: reqwest::blocking::Client,
    /// The last few things said: ATIS repeats itself every minute.
    cache: Mutex<VecDeque<(String, Arc<Vec<i16>>)>>,
    /// When the service last failed, for not logging every failure.
    last_fail: Mutex<Option<std::time::Instant>>,
}

const CACHE: usize = 24;

impl Neural {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(api: Api, url: &str, key: Option<String>, model: &str, voice: &str, speed: f32, style: Option<String>) -> Self {
        Neural {
            api,
            url: url.trim_end_matches('/').to_string(),
            key,
            model: model.to_string(),
            voice: voice.to_string(),
            speed,
            style,
            http: reqwest::blocking::Client::builder().timeout(Duration::from_secs(20)).build().unwrap_or_default(),
            cache: Mutex::new(VecDeque::new()),
            last_fail: Mutex::new(None),
        }
    }

    pub(crate) fn describe(&self) -> String {
        let what = match self.api {
            Api::OpenAi if self.url.contains("api.openai.com") => "OpenAI",
            Api::OpenAi => "OpenAI-compatible",
            Api::ElevenLabs => "ElevenLabs",
        };
        format!("{what} {} ({})", self.model, self.voice)
    }

    fn failed(&self, e: &anyhow::Error) {
        let mut l = self.last_fail.lock().unwrap();
        if l.map_or(true, |t| t.elapsed() > Duration::from_secs(300)) {
            log::warn!("tts: {} failed, using the local voice: {e:#}", self.describe());
            *l = Some(std::time::Instant::now());
        }
    }

    pub(crate) fn synthesize(&self, text: &str) -> Result<Vec<i16>> {
        if let Some((_, pcm)) = self.cache.lock().unwrap().iter().find(|(t, _)| t == text) {
            return Ok(pcm.as_ref().clone());
        }
        let pcm = match self.api {
            Api::OpenAi => self.openai(text)?,
            Api::ElevenLabs => self.elevenlabs(text)?,
        };
        let mut c = self.cache.lock().unwrap();
        c.push_back((text.to_string(), Arc::new(pcm.clone())));
        while c.len() > CACHE {
            c.pop_front();
        }
        Ok(pcm)
    }

    fn openai(&self, text: &str) -> Result<Vec<i16>> {
        let mut body = serde_json::json!({
            "model": self.model,
            "input": text,
            "voice": self.voice,
            "response_format": "wav",
        });
        // gpt-4o voices take their pace from the instructions, not `speed`
        if self.model.starts_with("gpt-4o") {
            if let Some(st) = &self.style {
                body["instructions"] = serde_json::Value::from(st.clone());
            }
        } else if (self.speed - 1.).abs() > 0.01 {
            body["speed"] = serde_json::Value::from(self.speed.clamp(0.5, 2.));
        }
        let mut req = self.http.post(format!("{}/v1/audio/speech", self.url)).json(&body);
        if let Some(k) = &self.key {
            req = req.bearer_auth(k);
        }
        let res = req.send()?;
        let status = res.status();
        if !status.is_success() {
            anyhow::bail!("{status} {}", res.text().unwrap_or_default().chars().take(300).collect::<String>());
        }
        let bytes = res.bytes()?;
        let (samples, rate) = read_wav_mono(hound::WavReader::new(std::io::Cursor::new(bytes.to_vec())).context("speech service WAV")?)?;
        Ok(resample_to(&samples, rate, SRS_RATE))
    }

    fn elevenlabs(&self, text: &str) -> Result<Vec<i16>> {
        let body = serde_json::json!({
            "text": text,
            "model_id": self.model,
            "voice_settings": {
                "stability": 0.4,
                "similarity_boost": 0.75,
                "style": 0.15,
                "use_speaker_boost": true,
                "speed": self.speed.clamp(0.7, 1.2),
            },
        });
        let mut req = self
            .http
            .post(format!("{}/v1/text-to-speech/{}?output_format=pcm_16000", self.url, self.voice))
            .json(&body);
        if let Some(k) = &self.key {
            req = req.header("xi-api-key", k);
        }
        let res = req.send()?;
        let status = res.status();
        if !status.is_success() {
            anyhow::bail!("{status} {}", res.text().unwrap_or_default().chars().take(300).collect::<String>());
        }
        let b = res.bytes()?;
        Ok(b.chunks_exact(2).map(|c| i16::from_le_bytes([c[0], c[1]])).collect())
    }
}

/// The voices of an ElevenLabs account, American ones first: (id, name).
pub(crate) fn elevenlabs_voices(url: &str, key: &str) -> Vec<(String, String)> {
    #[derive(serde::Deserialize)]
    struct V {
        voice_id: String,
        name: String,
        #[serde(default)]
        labels: HashMap<String, String>,
    }
    #[derive(serde::Deserialize)]
    struct R {
        voices: Vec<V>,
    }
    let http = reqwest::blocking::Client::builder().timeout(Duration::from_secs(10)).build().unwrap_or_default();
    let r = http.get(format!("{}/v1/voices", url.trim_end_matches('/'))).header("xi-api-key", key).send();
    let mut v: Vec<V> = match r.and_then(|r| r.error_for_status()).and_then(|r| r.json::<R>()) {
        Ok(r) => r.voices,
        Err(e) => {
            log::warn!("tts: could not list ElevenLabs voices: {e}");
            return vec![];
        }
    };
    v.sort_by_key(|x| !x.labels.get("accent").map_or(false, |a| a.to_lowercase().contains("american")));
    v.into_iter().map(|x| (x.voice_id, x.name)).collect()
}
