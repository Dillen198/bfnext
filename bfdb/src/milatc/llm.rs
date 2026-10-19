// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! The language-model fallback for transmissions the rule parser could not
//! place: a pilot who says "Andersen, Viper one-one, we'd like to come in and
//! do some patterns" still gets an answer.
//!
//! One Claude Messages API call, structured output constrained to the
//! [`Parsed`] schema, thinking off and a short timeout: a controller that
//! takes five seconds to answer is worse than one that says "say again". The
//! model only classifies the call. It never writes what the controller says,
//! so it cannot invent a clearance.

use super::intent::{Approach, Intent, LandingOption, Parsed};
use anyhow::{anyhow, bail, Result};
use serde::Deserialize;
use std::time::Duration;

const DEFAULT_URL: &str = "https://api.anthropic.com/v1/messages";
const DEFAULT_MODEL: &str = "claude-haiku-5-5";

/// The `llm` block of the range ATC config.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LlmCfg {
    #[serde(default = "d_true")]
    pub enabled: bool,
    #[serde(default)]
    pub url: Option<String>,
    /// The API key. Prefer the `BFDB_ATC_LLM_KEY` (or `ANTHROPIC_API_KEY`)
    /// environment variable: the bot passes secrets that way, never on disk.
    #[serde(default)]
    pub key: Option<String>,
    #[serde(default)]
    pub model: Option<String>,
    #[serde(default = "d_timeout")]
    pub timeout_ms: u64,
}

fn d_true() -> bool {
    true
}
fn d_timeout() -> u64 {
    4000
}

/// A resolved, ready-to-call client.
#[derive(Clone)]
pub(crate) struct Llm {
    url: String,
    key: String,
    model: String,
    http: reqwest::blocking::Client,
}

impl Llm {
    pub(crate) fn from_cfg(cfg: Option<&LlmCfg>) -> Option<Self> {
        let cfg = cfg?;
        if !cfg.enabled {
            return None;
        }
        let env = |k: &str| std::env::var(k).ok().filter(|v| !v.trim().is_empty());
        let key = cfg.key.clone().filter(|k| !k.trim().is_empty()).or_else(|| env("BFDB_ATC_LLM_KEY")).or_else(|| env("ANTHROPIC_API_KEY"))?;
        let http = reqwest::blocking::Client::builder().timeout(Duration::from_millis(cfg.timeout_ms.max(500))).build().ok()?;
        Some(Llm {
            url: cfg.url.clone().unwrap_or_else(|| DEFAULT_URL.into()),
            key,
            model: cfg.model.clone().unwrap_or_else(|| DEFAULT_MODEL.into()),
            http,
        })
    }

    pub(crate) fn describe(&self) -> String {
        self.model.clone()
    }

    /// Classify one transmission. Blocking; call from `spawn_blocking`.
    pub(crate) fn classify(&self, ctx: &Context) -> Result<Parsed> {
        let body = serde_json::json!({
            "model": self.model,
            "max_tokens": 400,
            "thinking": { "type": "disabled" },
            "output_config": {
                "effort": "low",
                "format": { "type": "json_schema", "schema": schema() },
            },
            "system": SYSTEM,
            "messages": [{ "role": "user", "content": ctx.prompt() }],
        });
        let res = self
            .http
            .post(&self.url)
            .header("x-api-key", &self.key)
            .header("anthropic-version", "2023-06-01")
            .json(&body)
            .send()?;
        let status = res.status();
        if !status.is_success() {
            let txt = res.text().unwrap_or_default();
            bail!("language model: {status} {}", txt.chars().take(300).collect::<String>());
        }
        #[derive(Deserialize)]
        struct Block {
            #[serde(rename = "type")]
            kind: String,
            #[serde(default)]
            text: String,
        }
        #[derive(Deserialize)]
        struct Reply {
            content: Vec<Block>,
            #[serde(default)]
            stop_reason: Option<String>,
        }
        let r: Reply = res.json()?;
        if r.stop_reason.as_deref() == Some("refusal") {
            bail!("language model declined");
        }
        let text: String = r.content.into_iter().filter(|b| b.kind == "text").map(|b| b.text).collect();
        let w: Wire = serde_json::from_str(text.trim()).map_err(|e| anyhow!("unparseable model reply ({e}): {text}"))?;
        Ok(w.into_parsed())
    }
}

/// What the model is told about the transmission.
pub(crate) struct Context {
    /// "Andersen Tower"
    pub position: String,
    /// What the aircraft is doing as far as ATC knows: "taxiing to 06R",
    /// "on a 6 mile final", "unknown".
    pub situation: String,
    /// The last thing this controller said to the aircraft, if anything.
    pub last_instruction: Option<String>,
    pub heard: String,
}

impl Context {
    fn prompt(&self) -> String {
        format!(
            "Frequency: {}\nAircraft situation: {}\nController's last transmission to this aircraft: {}\nPilot transmission (speech-to-text, may contain recognition errors): \"{}\"",
            self.position,
            self.situation,
            self.last_instruction.as_deref().unwrap_or("none"),
            self.heard
        )
    }
}

const SYSTEM: &str = "\
You classify pilot radio transmissions for an automated military air traffic \
control system on a DCS World flight simulator training server. Each \
transmission arrives on one controller's frequency (clearance delivery, \
ground, tower, departure, approach, or the precision approach final \
controller). You output only the structured classification; you never reply \
to the pilot.

Choose the single intent that best describes what the pilot wants from ATC:
- radio_check: asks how they are heard.
- request_clearance: asks for an IFR or departure clearance.
- clearance_readback: reads back a clearance (squawk, altitude, departure frequency).
- request_startup: asks to start engines.
- request_taxi: asks to taxi for departure.
- request_taxi_parking: asks to taxi to parking, the ramp, or back after landing.
- ready_for_departure: holding short or lined up and ready to take off.
- request_crossing: asks to cross a runway.
- check_in: first call on a radar frequency giving position or altitude, without a specific request.
- request_approach: asks for an approach or recovery (overhead, straight-in, visual, PAR/GCA, ILS, TACAN, ASR), or says they are inbound to land.
- report_initial / report_break / report_downwind / report_base / report_final: position reports in the visual pattern (base usually with gear down).
- going_around: going around or executing a missed approach.
- request_closed: asks for closed traffic / a closed pull-up.
- clear_of_runway: reports having vacated the runway.
- request_frequency_change: leaving the frequency.
- request_weather: asks for wind, altimeter or weather.
- say_again: asks the controller to repeat.
- readback: acknowledges or reads back an instruction with no new request (\"roger\", \"wilco\", \"cleared for takeoff, Viper 1-1\").
- unable: cannot comply with the last instruction.
- emergency: mayday, pan-pan, or any declared emergency or serious malfunction.
- request_range: asks Range Control for a range, station or range time (destination: the range or station named).
- ip_inbound: running in on a range target (\"IP inbound\", \"in from the north\", \"in hot\").
- off_target: coming off a range target (\"off safe\", \"off dry\").
- departing_range: leaving the range, finished or winchester.
- request_fuel: asks a tanker to join for fuel.
- fuel_complete: finished refuelling, disconnecting, leaving the tanker.
- ball: the carrier ball call (\"Hornet ball, 5.2\"), or \"clara\" (no ball in sight).
- commencing: leaving the carrier's Marshal stack to start the approach.
- none: not addressed to ATC (intra-flight chatter, noise, or unintelligible).

Use the controller's last transmission to tell a readback from a request. \
callsign: the aircraft callsign as \"Name 1-1\" (name word, then each digit \
separated by hyphens), or \"\" if none was said. runway: a designator such as \
\"06R\" if one was named, else \"\". approach: overhead, straight_in, visual, \
par, ils, tacan, asr, or \"\". option: full_stop, touch_and_go, low_approach, \
option, or \"\". altitude_ft: an altitude the pilot reported or read back in \
feet, else 0. squawk: a four-digit transponder code the pilot read back, else 0. \
destination: a parking area or clearance limit the pilot named, else \"\". \
Speech recognition errors are common: interpret aviation phraseology \
generously (\"tree\" is three, \"niner\" is nine, \"Anderson\" is Andersen).";

fn schema() -> serde_json::Value {
    let intents = [
        "radio_check",
        "request_clearance",
        "clearance_readback",
        "request_startup",
        "request_taxi",
        "request_taxi_parking",
        "ready_for_departure",
        "request_crossing",
        "check_in",
        "request_approach",
        "report_initial",
        "report_break",
        "report_downwind",
        "report_base",
        "report_final",
        "going_around",
        "request_closed",
        "clear_of_runway",
        "request_frequency_change",
        "request_weather",
        "say_again",
        "readback",
        "unable",
        "emergency",
        "request_range",
        "ip_inbound",
        "off_target",
        "departing_range",
        "request_fuel",
        "fuel_complete",
        "ball",
        "commencing",
        "none",
    ];
    serde_json::json!({
        "type": "object",
        "additionalProperties": false,
        "required": ["intent", "callsign", "runway", "approach", "option", "altitude_ft", "squawk", "destination"],
        "properties": {
            "intent": { "type": "string", "enum": intents.to_vec() },
            "callsign": { "type": "string" },
            "runway": { "type": "string" },
            "approach": { "type": "string", "enum": ["", "overhead", "straight_in", "visual", "par", "ils", "tacan", "asr"] },
            "option": { "type": "string", "enum": ["", "full_stop", "touch_and_go", "low_approach", "option"] },
            "altitude_ft": { "type": "integer" },
            "squawk": { "type": "integer" },
            "destination": { "type": "string" },
        }
    })
}

/// The model's reply, before validation. Kept loose so one odd field does not
/// throw away an otherwise good classification.
#[derive(Deserialize)]
struct Wire {
    intent: String,
    #[serde(default)]
    callsign: String,
    #[serde(default)]
    runway: String,
    #[serde(default)]
    approach: String,
    #[serde(default)]
    option: String,
    #[serde(default)]
    altitude_ft: i64,
    #[serde(default)]
    squawk: i64,
    #[serde(default)]
    destination: String,
}

impl Wire {
    fn into_parsed(self) -> Parsed {
        let intent: Intent =
            serde_json::from_value(serde_json::Value::String(self.intent)).unwrap_or(Intent::None);
        let mut p = Parsed::of(intent);
        p.callsign = self.callsign.trim().to_string();
        p.runway = self.runway.trim().to_uppercase();
        p.approach = serde_json::from_value(serde_json::Value::String(self.approach)).unwrap_or(Approach::Unspecified);
        p.option = serde_json::from_value(serde_json::Value::String(self.option)).unwrap_or(LandingOption::Unspecified);
        p.altitude_ft = self.altitude_ft.clamp(0, 60_000) as i32;
        p.squawk = if (0..=7777).contains(&self.squawk) { self.squawk as u16 } else { 0 };
        p.destination = self.destination.trim().to_string();
        p
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wire_reply_maps_onto_parsed() {
        let w: Wire = serde_json::from_str(
            r#"{"intent":"request_approach","callsign":"Viper 1-1","runway":"06r","approach":"par","option":"full_stop","altitude_ft":15000,"squawk":0,"destination":""}"#,
        )
        .unwrap();
        let p = w.into_parsed();
        assert_eq!(p.intent, Intent::RequestApproach);
        assert_eq!(p.approach, Approach::Par);
        assert_eq!(p.option, LandingOption::FullStop);
        assert_eq!(p.runway, "06R");
        let bad: Wire = serde_json::from_str(r#"{"intent":"dance","approach":"x"}"#).unwrap();
        let p = bad.into_parsed();
        assert_eq!(p.intent, Intent::None);
        assert_eq!(p.approach, Approach::Unspecified);
    }
}
