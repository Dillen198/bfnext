/*
Copyright 2024 Eric Stokes.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
for more details.
*/

//! GCI and situation briefings, the comms plan.

use super::*;

/// Short slot-entry radio briefing shown to a player when they take a slot,
/// so they know where to tune for the live GCI controller. Purely cosmetic —
/// the operational GCI configuration lives in bfdb's `gci.json`. Omit the
/// whole block to show nothing on slot entry.
#[derive(Debug, Clone, Serialize, Deserialize, Default, schemars::JsonSchema)]
pub struct GciBriefingCfg {
    /// Blue GCI/AWACS radio, spoken as written (e.g. "251.0 AM" or
    /// "251.0 AM / 33X"). Omit to skip the frequency line for blue.
    #[serde(default)]
    pub blue_freq: Option<String>,
    /// Red GCI/AWACS radio.
    #[serde(default)]
    pub red_freq: Option<String>,
    /// Blue controller callsign shown in the briefing. Defaults to "Magic".
    #[serde(default)]
    pub blue_callsign: Option<String>,
    /// Red controller callsign shown in the briefing. Defaults to "Overlord".
    #[serde(default)]
    pub red_callsign: Option<String>,
    /// Optional extra line appended to the briefing (e.g. "Broadcast only —
    /// no check-in required" or a SRS server note).
    #[serde(default)]
    pub note: Option<String>,
    /// Seconds the panel message stays on screen. Default 20.
    #[serde(default = "default_gci_briefing_secs")]
    pub display_secs: i64,
}

pub(super) fn default_gci_briefing_secs() -> i64 {
    20
}

impl GciBriefingCfg {
    /// Render the briefing panel text for a side, or `None` if there is
    /// nothing configured to show that side.
    pub fn render(&self, side: Side) -> Option<std::string::String> {
        let (freq, callsign, default_cs) = match side {
            Side::Red => (&self.red_freq, &self.red_callsign, "Overlord"),
            _ => (&self.blue_freq, &self.blue_callsign, "Magic"),
        };
        let str_opt = |o: &Option<String>| -> Option<std::string::String> {
            o.as_ref().map(|s| s.as_str().to_string()).filter(|s| !s.is_empty())
        };
        let freq = str_opt(freq)?;
        let callsign = str_opt(callsign).unwrap_or_else(|| default_cs.to_string());
        let mut out = format!("GCI: {callsign} on {freq} (SRS)");
        if let Some(note) = str_opt(&self.note) {
            out.push('\n');
            out.push_str(&note);
        }
        Some(out)
    }
}

/// The auto-generated situational briefing: a short "what is going on right
/// now" panel on slot entry, the paged F10 -> Info -> Situation report, and
/// the dashboard BRIEFING page. All three render the same engine-built
/// [`bfprotocols::situation::SituationReport`], so they cannot disagree.
///
/// Omit the whole block to keep the slot-entry panel off; the F10 report and
/// the dashboard page are always available (they are pull, not push).
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct SituationBriefingCfg {
    /// Show the condensed briefing panel when a player takes a slot.
    /// Default true.
    #[serde(default = "default_true")]
    pub on_slot_entry: bool,
    /// Seconds the slot-entry panel stays on screen. Default 40 -- long
    /// enough to read a dozen lines while the engines spool.
    #[serde(default = "default_situation_display_secs")]
    pub display_secs: i64,
    /// Seconds after slot entry before the panel fires. Sequenced after the
    /// ATIS (15s) so the two don't overwrite each other. Default 22.
    #[serde(default = "default_situation_delay_secs")]
    pub delay_secs: u32,
    /// Tasking lines on the slot-entry panel. The full list is always on the
    /// F10 report and the dashboard. Default 3.
    #[serde(default = "default_situation_panel_tasks")]
    pub panel_tasks: usize,
    /// Tasking lines the engine will generate at all, across every category.
    /// Default 12.
    #[serde(default = "default_situation_max_tasks")]
    pub max_tasks: usize,
    /// Include known enemy air-defence areas (from this side's own recon /
    /// ELINT / JTAC intel -- never omniscient) in the report. Default true.
    #[serde(default = "default_true")]
    pub include_threats: bool,
    /// Free-form line appended to the briefing, e.g. a campaign premise or a
    /// Discord link.
    #[serde(default)]
    pub note: Option<String>,
}

pub(super) fn default_situation_display_secs() -> i64 {
    40
}

pub(super) fn default_situation_delay_secs() -> u32 {
    22
}

pub(super) fn default_situation_panel_tasks() -> usize {
    3
}

pub(super) fn default_situation_max_tasks() -> usize {
    12
}

impl Default for SituationBriefingCfg {
    fn default() -> Self {
        Self {
            on_slot_entry: true,
            display_secs: default_situation_display_secs(),
            delay_secs: default_situation_delay_secs(),
            panel_tasks: default_situation_panel_tasks(),
            max_tasks: default_situation_max_tasks(),
            include_threats: true,
            note: None,
        }
    }
}

/// One line of the coalition comms card.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CommsChannelCfg {
    /// Radio preset/channel number a pilot dials up, when the airframe has
    /// presets. Purely for the card -- nothing writes it into the .miz.
    #[serde(default)]
    pub preset: Option<u8>,
    /// Who is on it: "AWACS / GCI -- MAGIC".
    pub label: String,
    pub freq_mhz: f64,
    /// "AM" or "FM". UHF and VHF-air are AM; the 30-76 MHz ground net is FM.
    #[serde(default = "default_modulation")]
    pub modulation: String,
    /// What it is for, one short line. Shown under the label on the briefing.
    #[serde(default)]
    pub purpose: Option<String>,
}

pub(super) fn default_modulation() -> String {
    String::from("AM")
}

/// Coalition frequency allocation -- the comms card every pilot flies with.
///
/// The defaults are a deconflicted plan for a mixed DCS module set: blue lives
/// in UHF 251-270 (every western jet's 225-400 AM radio) plus VHF-FM 30-31 for
/// the helo/A-10 ground net; red lives in VHF-AM 124-145 and UHF 228-237,
/// inside the 100-150 / 220-400 coverage of the Russian R-862/R-863 radios.
/// The two sides never share a working channel, and 243.000 / 121.500 are
/// reserved as guard on both.
///
/// The engine overlays the *live* frequencies it actually knows -- the AWACS
/// and tanker radios from the running action specs, the JTAC laser codes --
/// on top of this plan when it builds a briefing, so a channel that is really
/// up shows as on-station and one that is only planned shows as planned.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CommsPlanCfg {
    #[serde(default = "default_blue_comms")]
    pub blue: Vec<CommsChannelCfg>,
    #[serde(default = "default_red_comms")]
    pub red: Vec<CommsChannelCfg>,
    /// First intra-flight frequency, MHz. FLIGHT 1 gets this, FLIGHT 2 gets
    /// `+ flight_step_mhz`, and so on. Kept clear of every channel above.
    #[serde(default = "default_blue_flight_base")]
    pub blue_flight_base_mhz: f64,
    #[serde(default = "default_red_flight_base")]
    pub red_flight_base_mhz: f64,
    #[serde(default = "default_flight_step")]
    pub flight_step_mhz: f64,
    /// How many numbered intra-flight channels to publish. Default 8.
    #[serde(default = "default_flight_count")]
    pub flight_count: u8,
}

pub(super) fn default_modulation_fm() -> String {
    String::from("FM")
}

pub(super) fn ch(preset: u8, label: &str, freq_mhz: f64, purpose: &str) -> CommsChannelCfg {
    CommsChannelCfg {
        preset: Some(preset),
        label: String::from(label),
        freq_mhz,
        modulation: default_modulation(),
        purpose: Some(String::from(purpose)),
    }
}

pub(super) fn ch_fm(preset: u8, label: &str, freq_mhz: f64, purpose: &str) -> CommsChannelCfg {
    CommsChannelCfg {
        preset: Some(preset),
        label: String::from(label),
        freq_mhz,
        modulation: default_modulation_fm(),
        purpose: Some(String::from(purpose)),
    }
}

pub(super) fn default_blue_comms() -> Vec<CommsChannelCfg> {
    vec![
        ch(1, "AWACS / GCI -- MAGIC", 251.0, "Primary control: picture, bogey dope, commit, declare"),
        ch(2, "GCI VHF relay -- MAGIC", 119.0, "Same controller for VHF-only airframes"),
        ch(3, "AWACS alternate -- DARKSTAR", 252.0, "Second controller / overflow when MAGIC is saturated"),
        ch(4, "Tanker TEXACO (boom)", 253.0, "Pre-strike and post-strike refuel, fixed-wing boom"),
        ch(5, "Tanker ARCO (boom)", 254.0, "Second boom track"),
        ch(6, "Tanker SHELL (drogue)", 255.0, "Probe-and-drogue: Hornet, Tomcat, Harrier, Viggen"),
        ch(7, "JTAC 1", 256.0, "Nine-line, talk-on, laser for the first active JTAC"),
        ch(8, "JTAC 2", 257.0, "Second active JTAC"),
        ch(9, "JTAC 3", 258.0, "Third active JTAC"),
        ch(10, "JTAC 4", 259.0, "Fourth active JTAC"),
        ch(11, "CSAR -- SANDY", 260.0, "Downed-pilot pickup: on-scene commander and the helo"),
        ch(12, "Package common -- STRIKE", 265.0, "Strike package internal, all flights"),
        ch(13, "Package common -- SEAD", 266.0, "SEAD/DEAD package internal"),
        ch(14, "Package common -- CAP", 267.0, "Sweep and escort internal"),
        ch(15, "Package common -- CAS", 268.0, "CAS stack check-in and deconfliction"),
        ch(16, "Carrier MARSHAL", 270.0, "Carrier approach control, marshal stack, case II/III"),
        ch(17, "Carrier TOWER / LSO", 127.5, "Ball call, paddles, deck ops"),
        ch(18, "GUARD (UHF)", 243.0, "Emergency only -- never used for traffic"),
        ch(19, "GUARD (VHF)", 121.5, "Emergency only"),
        ch_fm(20, "Ground / logistics net", 30.0, "Convoy, crate and warehouse coordination"),
        ch_fm(21, "Troop & crate ops", 31.0, "Helo lift working channel, troop drops"),
    ]
}

pub(super) fn default_red_comms() -> Vec<CommsChannelCfg> {
    vec![
        ch(1, "GCI -- OVERLORD", 124.0, "Primary control: picture, bogey dope, commit, declare"),
        ch(2, "GCI UHF relay -- OVERLORD", 228.0, "Same controller for UHF-preferred airframes"),
        ch(3, "AWACS A-50 -- DRAGNET", 125.0, "Airborne early warning when an A-50 is up"),
        ch(4, "Tanker IL-78 -- KUZNETS", 126.0, "Probe-and-drogue refuel"),
        ch(5, "JTAC 1", 133.0, "Nine-line, talk-on, laser for the first active JTAC"),
        ch(6, "JTAC 2", 134.0, "Second active JTAC"),
        ch(7, "JTAC 3", 135.0, "Third active JTAC"),
        ch(8, "JTAC 4", 136.0, "Fourth active JTAC"),
        ch(9, "CSAR -- rescue", 137.0, "Downed-pilot pickup"),
        ch(10, "Package common -- STRIKE", 142.0, "Strike package internal, all flights"),
        ch(11, "Package common -- SEAD", 143.0, "SEAD/DEAD package internal"),
        ch(12, "Package common -- CAP", 144.0, "Sweep and escort internal"),
        ch(13, "Package common -- CAS", 145.0, "CAS stack check-in and deconfliction"),
        ch(14, "GUARD (UHF)", 243.0, "Emergency only -- never used for traffic"),
        ch(15, "GUARD (VHF)", 121.5, "Emergency only"),
        ch_fm(16, "Ground / logistics net", 40.0, "Convoy, crate and warehouse coordination"),
        ch_fm(17, "Troop & crate ops", 41.0, "Helo lift working channel, troop drops"),
    ]
}

pub(super) fn default_blue_flight_base() -> f64 {
    305.0
}

pub(super) fn default_red_flight_base() -> f64 {
    230.0
}

pub(super) fn default_flight_step() -> f64 {
    1.0
}

pub(super) fn default_flight_count() -> u8 {
    8
}

impl Default for CommsPlanCfg {
    fn default() -> Self {
        Self {
            blue: default_blue_comms(),
            red: default_red_comms(),
            blue_flight_base_mhz: default_blue_flight_base(),
            red_flight_base_mhz: default_red_flight_base(),
            flight_step_mhz: default_flight_step(),
            flight_count: default_flight_count(),
        }
    }
}

impl CommsPlanCfg {
    pub fn for_side(&self, side: Side) -> &[CommsChannelCfg] {
        match side {
            Side::Red => &self.red,
            _ => &self.blue,
        }
    }

    /// The numbered intra-flight channels for a side, as
    /// `("FLIGHT 3", 307.0)`.
    pub fn flight_channels(&self, side: Side) -> Vec<(std::string::String, f64)> {
        let base = match side {
            Side::Red => self.red_flight_base_mhz,
            _ => self.blue_flight_base_mhz,
        };
        (0..self.flight_count)
            .map(|i| {
                (
                    format!("FLIGHT {}", i + 1),
                    base + self.flight_step_mhz * i as f64,
                )
            })
            .collect()
    }
}
