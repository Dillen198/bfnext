// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! What a pilot said: one transmission in, one [`Intent`] out.
//!
//! The rule parser below handles standard phraseology fast and for free. What
//! it cannot place goes to the language model ([`super::llm`]), which returns
//! the same [`Parsed`] shape. Neither ever decides what the controller does
//! about it: that is [`super::controller`]'s job, from the traffic picture.

use super::speech::format_callsign;
use crate::voice::stt;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Intent {
    RadioCheck,
    RequestClearance,
    /// Reading back an IFR clearance (squawk, altitude, frequency).
    ClearanceReadback,
    RequestStartup,
    /// Taxi for departure.
    RequestTaxi,
    /// Taxi to parking after landing.
    RequestTaxiParking,
    /// Holding short, ready to go.
    ReadyForDeparture,
    RequestCrossing,
    /// First call on a radar frequency: "passing 3,000 for 4,000",
    /// "30 miles north, 15,000, inbound".
    CheckIn,
    RequestApproach,
    ReportInitial,
    ReportBreak,
    ReportDownwind,
    ReportBase,
    ReportFinal,
    GoingAround,
    RequestClosed,
    ClearOfRunway,
    RequestFrequencyChange,
    RequestWeather,
    /// The pilot wants the last transmission repeated.
    SayAgain,
    /// Reading back an instruction: nothing to answer.
    Readback,
    Unable,
    Emergency,
    /// Range Control: a slot on a range ("request R-7201", "request range time").
    RequestRange,
    /// Range Control: running in ("IP inbound", "in from the north").
    IpInbound,
    /// Range Control: off the target ("off safe", "off dry").
    OffTarget,
    /// Range Control: leaving the range ("departing the range", "winchester").
    DepartingRange,
    /// Tanker: wants fuel ("request rejoin", "request fuel").
    RequestFuel,
    /// Tanker: finished ("fuel complete", "topped off", "disconnect").
    FuelComplete,
    /// Carrier: the ball call ("Hornet ball, 5.2", "clara").
    Ball,
    /// Carrier: leaving the Marshal stack at the approach time ("commencing").
    Commencing,
    /// Nothing for ATC (intra-flight chatter, noise).
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Approach {
    #[default]
    #[serde(rename = "")]
    Unspecified,
    Overhead,
    StraightIn,
    Visual,
    Par,
    Ils,
    Tacan,
    Asr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub(crate) enum LandingOption {
    #[default]
    #[serde(rename = "")]
    Unspecified,
    FullStop,
    TouchAndGo,
    LowApproach,
    #[serde(rename = "option")]
    TheOption,
}

/// One understood transmission.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct Parsed {
    pub intent: Intent,
    /// "Viper 1-1", when the pilot said one.
    #[serde(default)]
    pub callsign: String,
    /// "06R", when named.
    #[serde(default)]
    pub runway: String,
    #[serde(default)]
    pub approach: Approach,
    #[serde(default)]
    pub option: LandingOption,
    /// Altitude the pilot reported or read back, feet; 0 when none.
    #[serde(default)]
    pub altitude_ft: i32,
    /// Squawk read back; 0 when none.
    #[serde(default)]
    pub squawk: u16,
    /// A named destination: a ramp for taxi-in, a clearance limit.
    #[serde(default)]
    pub destination: String,
}

impl Parsed {
    pub(crate) fn of(intent: Intent) -> Self {
        Parsed {
            intent,
            callsign: String::new(),
            runway: String::new(),
            approach: Approach::Unspecified,
            option: LandingOption::Unspecified,
            altitude_ft: 0,
            squawk: 0,
            destination: String::new(),
        }
    }
}

// ─── numbers in speech ─────────────────────────────────────────────────────

fn word_digit(w: &str) -> Option<char> {
    Some(match w {
        "zero" => '0',
        "one" => '1',
        "two" => '2',
        "three" | "tree" => '3',
        "four" | "fower" => '4',
        "five" | "fife" => '5',
        "six" => '6',
        "seven" => '7',
        "eight" => '8',
        "nine" | "niner" => '9',
        _ => return None,
    })
}

/// Lowercase words, with digit words turned into digits and runs of digits
/// split one per token ("one one" → ["1","1"], "11" → ["1","1"], "4,000" →
/// ["4","0","0","0"]). Whisper writes numbers either way.
fn tokens(text: &str) -> Vec<String> {
    let mut out = vec![];
    let lower = text.to_lowercase().replace(',', "");
    for raw in lower.split(|c: char| !(c.is_alphanumeric() || c == '.')) {
        let w = raw.trim_matches('.');
        if w.is_empty() {
            continue;
        }
        if let Some(d) = word_digit(w) {
            out.push(d.to_string());
        } else if w.chars().all(|c| c.is_ascii_digit() || c == '.') {
            for c in w.chars() {
                if c == '.' {
                    out.push("point".into());
                } else {
                    out.push(c.to_string());
                }
            }
        } else {
            // "06r" / "24l": a runway stuck together
            let digits: String = w.chars().take_while(|c| c.is_ascii_digit()).collect();
            if !digits.is_empty() && digits.len() <= 2 {
                for c in digits.chars() {
                    out.push(c.to_string());
                }
                out.push(w[digits.len()..].to_string());
            } else {
                out.push(w.to_string());
            }
        }
    }
    out
}

fn is_digit_tok(t: &str) -> bool {
    t.len() == 1 && t.as_bytes()[0].is_ascii_digit()
}

/// Words that are never a callsign's name part.
const NOT_A_CALLSIGN: &[&str] = &[
    "tower", "ground", "approach", "departure", "clearance", "delivery", "final", "gca", "radar", "runway", "with",
    "information", "flight", "level", "passing", "for", "climbing", "descending", "squawk", "miles", "mile",
    "heading", "at", "is", "of", "and", "the", "request", "requesting", "inbound", "point", "angels", "taxi",
    "holding", "short", "ready", "number", "on", "to", "by", "via", "cleared", "contact", "frequency", "maintain",
    "altitude", "feet", "thousand", "hundred", "north", "south", "east", "west", "left", "right", "center",
    "alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel", "india", "kilo", "lima", "x",
    "decimal", "copy", "roger", "wilco", "readback", "this", "it", "we", "are", "two", "ship", "flight",
];

/// The callsign a pilot gave: a name word followed by two or three digits.
/// `facility` words (the controllers' own callsign) are skipped.
pub(crate) fn callsign(text: &str, facility: &str) -> Option<String> {
    // digits never run on past a comma: "Rhino 11, 30 miles" is Rhino 1-1
    text.split([',', ';', '!', '?']).find_map(|seg| callsign_in(seg, facility))
}

fn callsign_in(text: &str, facility: &str) -> Option<String> {
    let t = tokens(text);
    let fac = facility.to_lowercase();
    for i in 0..t.len() {
        let w = &t[i];
        if is_digit_tok(w) || w.len() < 3 || NOT_A_CALLSIGN.contains(&w.as_str()) || fac.contains(w.as_str()) {
            continue;
        }
        let mut digits = String::new();
        let mut j = i + 1;
        while j < t.len() && is_digit_tok(&t[j]) && digits.len() < 3 {
            digits.push_str(&t[j]);
            j += 1;
        }
        if (2..=3).contains(&digits.len()) {
            return Some(format_callsign(w, &digits));
        }
    }
    None
}

/// A runway designator mentioned: "runway zero six right" → "06R".
pub(crate) fn runway(text: &str) -> Option<String> {
    let t = tokens(text);
    let i = t.iter().position(|w| w == "runway")?;
    let mut num = String::new();
    let mut j = i + 1;
    while j < t.len() && is_digit_tok(&t[j]) && num.len() < 2 {
        num.push_str(&t[j]);
        j += 1;
    }
    if num.is_empty() {
        return None;
    }
    let n: u32 = num.parse().ok()?;
    if !(1..=36).contains(&n) {
        return None;
    }
    let sfx = match t.get(j).map(|s| s.as_str()) {
        Some("left" | "l") => "L",
        Some("right" | "r") => "R",
        Some("center" | "centre" | "c") => "C",
        _ => "",
    };
    Some(format!("{n:02}{sfx}"))
}

/// An altitude: "4,000", "four thousand", "angels 15", "flight level 220",
/// "passing 3 thousand 5 hundred".
pub(crate) fn altitude(text: &str) -> Option<i32> {
    let t = tokens(text);
    let num_at = |mut j: usize, max: usize| -> (u32, usize) {
        let mut s = String::new();
        while j < t.len() && is_digit_tok(&t[j]) && s.len() < max {
            s.push_str(&t[j]);
            j += 1;
        }
        (s.parse().unwrap_or(0), j)
    };
    for i in 0..t.len() {
        match t[i].as_str() {
            "level" if i > 0 && t[i - 1] == "flight" => {
                let (n, _) = num_at(i + 1, 3);
                if n > 0 {
                    return Some(n as i32 * 100);
                }
            }
            "fl" => {
                let (n, _) = num_at(i + 1, 3);
                if n > 0 {
                    return Some(n as i32 * 100);
                }
            }
            "angels" => {
                let (n, _) = num_at(i + 1, 2);
                if n > 0 {
                    return Some(n as i32 * 1000);
                }
            }
            "thousand" if i > 0 && is_digit_tok(&t[i - 1]) => {
                // walk back over up to two digits
                let mut k = i;
                while k > 0 && is_digit_tok(&t[k - 1]) && i - k < 2 {
                    k -= 1;
                }
                let (th, _) = num_at(k, 2);
                let mut ft = th as i32 * 1000;
                if t.get(i + 2).map(|s| s == "hundred").unwrap_or(false) {
                    if let Some(h) = t.get(i + 1).filter(|s| is_digit_tok(s)) {
                        ft += h.parse::<i32>().unwrap_or(0) * 100;
                    }
                }
                return Some(ft);
            }
            _ => {}
        }
    }
    // A bare "4000" / "12500" after an altitude word.
    for i in 0..t.len() {
        if matches!(t[i].as_str(), "passing" | "maintain" | "maintaining" | "at" | "level" | "climbing" | "descending")
        {
            let (n, j) = num_at(i + 1, 5);
            if j - (i + 1) >= 4 && n >= 1000 {
                return Some(n as i32);
            }
        }
    }
    None
}

/// A squawk code read back: "squawk 4215".
pub(crate) fn squawk(text: &str) -> Option<u16> {
    let t = tokens(text);
    let i = t.iter().position(|w| w == "squawk" || w == "squawking")?;
    let mut s = String::new();
    let mut j = i + 1;
    while j < t.len() && is_digit_tok(&t[j]) && s.len() < 4 {
        s.push_str(&t[j]);
        j += 1;
    }
    (s.len() == 4).then(|| s.parse().ok()).flatten()
}

// ─── the rule parser ───────────────────────────────────────────────────────

fn has(text: &str, p: &str) -> bool {
    stt::contains_phrase(text, p)
}
fn any(text: &str, ps: &[&str]) -> bool {
    ps.iter().any(|p| has(text, p))
}
/// An exact word, for the short ones fuzzy matching would find inside longer
/// words ("par" in "departure", "ils" in "details").
fn word(text: &str, w: &str) -> bool {
    text.split(|c: char| !c.is_alphanumeric()).any(|x| x.eq_ignore_ascii_case(w))
}
/// A spelled-out abbreviation ("p a r", "i l s"), or the word itself.
fn spelled(text: &str, abbr: &str) -> bool {
    if word(text, abbr) {
        return true;
    }
    let toks: Vec<String> = text.split(|c: char| !c.is_alphanumeric()).filter(|x| !x.is_empty()).map(|x| x.to_lowercase()).collect();
    let want: Vec<String> = abbr.chars().map(|c| c.to_string()).collect();
    toks.windows(want.len()).any(|w| w == want.as_slice())
}

/// What a request for an approach asked for.
fn approach_kind(t: &str) -> Approach {
    if any(t, &["precision approach", "precision radar", "talk down", "talkdown"]) || spelled(t, "par") || spelled(t, "gca") {
        Approach::Par
    } else if spelled(t, "ils") || any(t, &["localizer"]) {
        Approach::Ils
    } else if any(t, &["tacan", "tackan", "tack an"]) {
        Approach::Tacan
    } else if spelled(t, "asr") || any(t, &["surveillance approach"]) {
        Approach::Asr
    } else if any(t, &["overhead", "the break", "initial"]) {
        Approach::Overhead
    } else if any(t, &["straight in", "straight-in"]) {
        Approach::StraightIn
    } else if any(t, &["visual"]) {
        Approach::Visual
    } else {
        Approach::Unspecified
    }
}

fn option_kind(t: &str) -> LandingOption {
    if any(t, &["touch and go", "touch-and-go", "touch and goes"]) {
        LandingOption::TouchAndGo
    } else if any(t, &["low approach", "low pass"]) {
        LandingOption::LowApproach
    } else if any(t, &["full stop", "full-stop"]) {
        LandingOption::FullStop
    } else if has(t, "the option") {
        LandingOption::TheOption
    } else {
        LandingOption::Unspecified
    }
}

/// Parse a transmission with the phraseology rules. `None` means "not sure":
/// the caller may try the language model before giving up.
/// "request R-7201 bomb circle": a request naming a range target, not a
/// taxi or a clearance to go to the range.
fn range_request(t: &str) -> bool {
    let t = t.replace("range control", "");
    has(&t, "request")
        && any(&t, &["range", "circle", "pit", "strafe", "bomb", "array", "target", "lane", "convoy", "r-72", "r72"])
        && !any(&t, &["taxi", "clearance", "startup", "start up", "takeoff", "take off", "departure", "approach", "landing"])
}

pub(crate) fn parse(text: &str, facility: &str) -> Option<Parsed> {
    let t = text.to_lowercase();
    let t = t.trim();
    if t.split_whitespace().count() < 2 {
        return Option::None;
    }
    use Intent::*;
    let intent = if any(t, &["mayday", "pan pan", "pan-pan", "declaring an emergency", "declare emergency", "emergency"]) {
        Emergency
    } else if word(t, "ball") || word(t, "clara") || has(t, "no ball") {
        Ball
    } else if word(t, "commencing") || has(t, "commencing approach") {
        Commencing
    } else if any(t, &["ip inbound", "i p inbound", "in from the", "in hot", "rolling in", "in dry", "inbound from the ip", "turning in"]) {
        IpInbound
    } else if any(t, &["off safe", "off target", "off dry", "off hot", "coming off", "off the target"]) {
        OffTarget
    } else if any(t, &["departing the range", "leaving the range", "off the range", "out of the range", "winchester", "clearing the range", "exiting the range"]) {
        DepartingRange
    } else if any(t, &["request range", "range time", "request the range", "request into", "request entry", "request station", "request the circle", "request the pit", "request bomb", "request strafe", "request a slot", "request slot", "cleared into"]) && !has(t, "cleared into the")
        || range_request(t)
    {
        RequestRange
    } else if any(t, &["request rejoin", "request fuel", "request gas", "request a drink", "need fuel", "need gas", "for gas", "for fuel", "request to join", "request join", "request tanking", "on the boom", "for the boom", "request contact"]) {
        RequestFuel
    } else if any(t, &["fuel complete", "topped off", "complete with fuel", "disconnecting", "full of gas", "thanks for the gas", "thanks for the fuel"]) && !has(t, "fuel state") {
        FuelComplete
    } else if word(t, "marshal") && any(t, &["angels", "state", " miles", " dme"]) {
        // the carrier check-in: "Marshal, 401, 30 miles, angels 20, state 6.2"
        CheckIn
    } else if any(t, &["radio check", "how do you read", "how do you hear", "comm check"]) {
        RadioCheck
    } else if any(t, &["say again", "repeat your last", "say last", "didn't copy", "did not copy"]) {
        SayAgain
    } else if any(t, &["going around", "go around", "missed approach", "executing missed"]) {
        GoingAround
    } else if any(t, &["unable"]) {
        Unable
    } else if any(t, &["request clearance", "ready to copy", "clearance to", "ifr clearance", "request ifr", "copy clearance"]) {
        RequestClearance
    } else if squawk(t).is_some() && any(t, &["cleared", "maintain", "climb", "expect", "departure frequency"]) {
        ClearanceReadback
    } else if any(t, &["request startup", "request start up", "request start", "ready to start", "request engine start", "start up"]) {
        RequestStartup
    } else if any(t, &["clear of the runway", "clear of runway", "clear the runway", "off the runway", "vacated", "clear of the active"]) {
        ClearOfRunway
    } else if any(t, &["request crossing", "request to cross", "cross runway", "request cross"]) && !has(t, "cleared to cross") {
        RequestCrossing
    } else if has(t, "taxi") && any(t, &["parking", "ramp", "the line", "transient", "chocks", "hangar", "spot", "back"]) {
        RequestTaxiParking
    } else if any(t, &["request taxi", "ready to taxi", "ready for taxi", "taxi for departure", "request to taxi", "taxi to the active", "taxi to runway"])
        && !has(t, "cleared")
    {
        RequestTaxi
    } else if any(t, &["ready for departure", "ready for takeoff", "ready for take off", "request takeoff", "request take off", "ready to go", "holding short ready", "number one ready", "ready in sequence"]) {
        ReadyForDeparture
    } else if any(t, &["initial"]) && !any(t, &["request", "expect"]) {
        ReportInitial
    } else if any(t, &["the break", "breaking", "in the break", "break"]) && !has(t, "approved") {
        ReportBreak
    } else if any(t, &["base", "gear down", "wheels down", "three green", "gear check"]) && !has(t, "cleared to land") {
        ReportBase
    } else if any(t, &["downwind"]) && !has(t, "report") {
        ReportDownwind
    } else if any(t, &["on final", "short final", "mile final", "miles final"]) {
        ReportFinal
    } else if any(t, &["request closed", "closed traffic", "request closed traffic", "closed pull"]) {
        RequestClosed
    } else if any(t, &["request tacan", "request visual", "request overhead", "request straight in", "request precision", "request approach", "request the overhead", "request a visual", "request the break", "approach request", "for the overhead", "for an overhead", "for a visual"])
        || (has(t, "request") && (spelled(t, "par") || spelled(t, "ils") || spelled(t, "gca")))
    {
        RequestApproach
    } else if any(t, &["request frequency change", "frequency change", "switching", "request switch", "leaving frequency", "going to", "push", "off frequency"]) {
        RequestFrequencyChange
    } else if any(t, &["request weather", "say weather", "request altimeter", "say altimeter", "request winds", "say winds", "request the numbers", "request field conditions"]) {
        RequestWeather
    } else if any(t, &["check in", "checking in", "inbound", "with you", "passing", "climbing", "leveling", "level at", "radar contact"]) {
        CheckIn
    } else if any(t, &["cleared for takeoff", "cleared to land", "line up and wait", "hold short", "cleared to cross", "wilco", "roger", "copy", "will report", "contact", "switching to", "taxi via", "cleared the option", "cleared touch and go", "cleared low approach", "readback", "turn left", "turn right", "heading", "descend", "maintain"]) {
        Readback
    } else {
        return Option::None;
    };
    let mut p = Parsed::of(intent);
    p.callsign = callsign(text, facility).unwrap_or_default();
    p.runway = runway(text).unwrap_or_default();
    p.approach = approach_kind(t);
    p.option = option_kind(t);
    p.altitude_ft = altitude(text).unwrap_or(0);
    p.squawk = squawk(text).unwrap_or(0);
    // "inbound for the overhead" is an approach request on a radar frequency
    if p.intent == CheckIn && p.approach != Approach::Unspecified {
        p.intent = RequestApproach;
    }
    Some(p)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(s: &str) -> Parsed {
        parse(s, "Andersen").unwrap_or_else(|| panic!("no parse: {s}"))
    }

    #[test]
    fn callsigns() {
        assert_eq!(callsign("Andersen Ground, Viper one one, request taxi", "Andersen").as_deref(), Some("Viper 1-1"));
        assert_eq!(callsign("andersen tower viper 11 ready for departure", "Andersen").as_deref(), Some("Viper 1-1"));
        assert_eq!(callsign("Enfield 2-1, request startup", "Andersen").as_deref(), Some("Enfield 2-1"));
        assert_eq!(callsign("Andersen tower, runway 06R", "Andersen"), None);
        assert_eq!(callsign("Marshal, Rhino 11, 30 miles, angels 20", "Lincoln").as_deref(), Some("Rhino 1-1"));
    }

    #[test]
    fn numbers() {
        assert_eq!(runway("holding short runway zero six right").as_deref(), Some("06R"));
        assert_eq!(runway("runway 24L").as_deref(), Some("24L"));
        assert_eq!(altitude("passing two thousand for four thousand"), Some(2000));
        assert_eq!(altitude("flight level two two zero"), Some(22000));
        assert_eq!(altitude("angels 15"), Some(15000));
        assert_eq!(altitude("passing 3,500"), Some(3500));
        assert_eq!(altitude("level at one two thousand five hundred"), Some(12500));
        assert_eq!(squawk("squawk four two one five, departure 363.8"), Some(4215));
    }

    #[test]
    fn standard_calls() {
        assert_eq!(p("Andersen Ground, Viper 11, request taxi").intent, Intent::RequestTaxi);
        assert_eq!(p("Andersen Clearance, Viper 11, request clearance to the range").intent, Intent::RequestClearance);
        assert_eq!(
            p("cleared to the range complex, maintain 4000, departure 363.8, squawk 4215, Viper 11").intent,
            Intent::ClearanceReadback
        );
        assert_eq!(p("Andersen Tower, Viper 11, holding short 06R, ready for departure").intent, Intent::ReadyForDeparture);
        assert_eq!(p("Viper 11 cleared for takeoff runway 06R").intent, Intent::Readback);
        let a = p("Andersen Approach, Viper 11, 30 miles north, angels 15, request PAR full stop");
        assert_eq!(a.intent, Intent::RequestApproach);
        assert_eq!(a.approach, Approach::Par);
        assert_eq!(a.option, LandingOption::FullStop);
        assert_eq!(p("Andersen Approach, Viper 11, inbound for the overhead").approach, Approach::Overhead);
        assert_eq!(p("Andersen Departure, Viper 11, passing 2000 for 4000").intent, Intent::CheckIn);
        assert_eq!(p("Andersen Tower, Viper 11, initial runway 06R").intent, Intent::ReportInitial);
        assert_eq!(p("Viper 11 base, gear down").intent, Intent::ReportBase);
        assert_eq!(p("Viper 11 going around").intent, Intent::GoingAround);
        assert_eq!(p("Andersen Ground, Viper 11, clear of the runway, taxi to parking").intent, Intent::ClearOfRunway);
        assert_eq!(p("Andersen Ground, Viper 11, request taxi to parking").intent, Intent::RequestTaxiParking);
        assert_eq!(p("Andersen Tower Viper 11 radio check").intent, Intent::RadioCheck);
        assert_eq!(p("Mayday mayday Viper 11 engine failure").intent, Intent::Emergency);
        assert_eq!(p("Range Control, Viper 11, request R-7201 bomb circle").intent, Intent::RequestRange);
        assert_eq!(p("Viper 11, IP inbound").intent, Intent::IpInbound);
        assert_eq!(p("Viper 11 off safe").intent, Intent::OffTarget);
        assert_eq!(p("Range Control, Viper 11, winchester, departing the range").intent, Intent::DepartingRange);
        assert_eq!(p("Texaco 11, Viper 11, request rejoin, 4000 pounds").intent, Intent::RequestFuel);
        assert_eq!(p("Texaco 11, Viper 11, fuel complete").intent, Intent::FuelComplete);
        assert_eq!(p("Viper 11, Hornet ball, 5.2").intent, Intent::Ball);
        assert_eq!(p("Viper 11 commencing").intent, Intent::Commencing);
    }
}
