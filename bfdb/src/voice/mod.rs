// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential — no license granted. See the repository NOTICE.

//! Shared voice bus — the radio plumbing behind every spoken service
//! (GCI, ATC, JTAC).
//!
//! A *position* is a controlling agency with a callsign on a frequency: "Magic"
//! on 251.0, "Batumi Tower" on 260.0, "Axeman" on 30.0 FM. Positions belonging
//! to one coalition share a single persistent SRS client ([`srs::SrsClient`])
//! carrying a radio per frequency, so the SRS client list stays readable and
//! one TCP connection serves the lot.
//!
//! - [`srs`]: the DCS-SimpleRadio-Standalone protocol client — multi-frequency,
//!   transmit and receive.
//! - [`tts`]: text → 16 kHz mono PCM (Piper or Windows SAPI).
//! - [`stt`]: 16 kHz mono PCM → text (whisper.cpp) plus brevity parsing.

pub(crate) mod srs;
pub(crate) mod stt;
pub(crate) mod tts;

/// One tuned frequency on a voice client. `label` is the service's own name for
/// the radio ("Batumi Tower"); it rides along on inbound transmissions so a
/// router knows which position a player was calling.
#[derive(Debug, Clone)]
pub(crate) struct Radio {
    pub freq_hz: f64,
    /// 0 = AM, 1 = FM.
    pub modulation: u8,
    pub label: String,
}

impl Radio {
    pub(crate) fn new(freq_mhz: f64, modulation: u8, label: impl Into<String>) -> Self {
        Radio {
            freq_hz: freq_mhz * 1e6,
            modulation,
            label: label.into(),
        }
    }

    /// Does this radio match a frequency heard on the wire? SRS frequencies are
    /// doubles that do not round-trip exactly, so compare with the same
    /// tolerance the receive path uses.
    pub(crate) fn matches(&self, hz: f64, modulation: u8) -> bool {
        modulation == self.modulation && (hz - self.freq_hz).abs() <= 500.0
    }

    /// Key for per-frequency bookkeeping (busy timers, dedup) — whole Hz.
    pub(crate) fn key(&self) -> u64 {
        self.freq_hz.max(0.0) as u64
    }

    /// "251.000 UHF AM" — for logs and spoken briefings.
    pub(crate) fn describe(&self) -> String {
        format!(
            "{:.3} {} {}",
            self.freq_hz / 1e6,
            band_of(self.freq_hz),
            if self.modulation == 1 { "FM" } else { "AM" }
        )
    }
}

/// Which radio band a frequency falls in, by the aviation convention DCS uses.
pub(crate) fn band_of(freq_hz: f64) -> &'static str {
    match freq_hz / 1e6 {
        f if f < 30.0 => "HF",
        f if f < 108.0 => "VHF",  // 30–87.995 is the FM combat-net band
        f if f < 156.0 => "VHF",  // 108–151.995 AM aviation band
        f if f < 400.0 => "UHF",  // 225–399.975
        _ => "SHF",
    }
}

/// `"fm"` → 1, anything else → 0 (AM). SRS only carries these two for voice.
pub(crate) fn modulation_byte(s: &str) -> u8 {
    if s.trim().eq_ignore_ascii_case("fm") {
        1
    } else {
        0
    }
}

/// One frequency as written in a config file: `{ "mhz": 251.0, "modulation":
/// "AM" }`. `modulation` may be omitted — it defaults to AM above 108 MHz and
/// FM below, which is right for every band DCS actually uses.
#[derive(Debug, Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct FreqSpec {
    pub mhz: f64,
    #[serde(default)]
    pub modulation: Option<String>,
    /// Optional label override; otherwise the position's name is used.
    #[serde(default)]
    pub label: Option<String>,
}

impl FreqSpec {
    pub(crate) fn to_radio(&self, default_label: &str) -> Radio {
        let modulation = match self.modulation.as_deref() {
            Some(m) => modulation_byte(m),
            // Below the AM aviation band, DCS radios are FM combat nets.
            None if self.mhz < 108.0 => 1,
            None => 0,
        };
        Radio::new(
            self.mhz,
            modulation,
            self.label.clone().unwrap_or_else(|| default_label.to_string()),
        )
    }
}

/// Build a position's radio list from the new multi-frequency form, falling
/// back to a single legacy `freqMhz` + `modulation` pair when no list is given.
pub(crate) fn radios_from(
    specs: &[FreqSpec],
    legacy_mhz: f64,
    legacy_modulation: u8,
    label: &str,
) -> Vec<Radio> {
    if !specs.is_empty() {
        return specs.iter().map(|s| s.to_radio(label)).collect();
    }
    if legacy_mhz > 0.0 {
        return vec![Radio::new(legacy_mhz, legacy_modulation, label)];
    }
    Vec::new()
}
