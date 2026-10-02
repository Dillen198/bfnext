//! Landing Signal Officer shorthand: parse DCS's `LANDING_QUALITY_MARK`
//! comment and turn the LSO's notation into plain English.
//!
//! A DCS comment looks like one of
//!
//! ```text
//! LSO: GRADE:_OK_ : WIRE# 3
//! LSO: GRADE:C _SLOX_ _LURX_ 3PTSIW LNFIW WIRE #1
//! GRADE:WO  AAX FIM (SLO)AR _HAW_
//! ```
//!
//! The grade comes first, then zero or more deviation calls, then optionally
//! the wire. Each call is `ERROR` + `POSITION`, wrapped to say how big it was:
//! `(x)` a little, `x` as written, `_x_` a lot. The wrapper can sit around
//! the whole call (`_HAW_`) or just the error (`(SLO)AR`).
//!
//! Both the engine (when it builds a result) and bfdb (when it re-reads one)
//! use this, so the description a pilot reads in Discord and on the range
//! site is always the same text.

use serde::{Deserialize, Serialize};

/// How large a deviation the LSO called.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Magnitude {
    /// `(x)`
    Little,
    /// `x`
    Normal,
    /// `_x_`
    Lot,
}

/// One parsed deviation call, e.g. `_HAW_` = { error "H", position "AW", Lot }.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LsoCall {
    /// The token as the LSO wrote it.
    pub raw: String,
    pub error: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub position: Option<String>,
    pub magnitude: Magnitude,
    /// Plain-English rendering, e.g. "VERY high all the way".
    pub text: String,
}

/// A parsed DCS LSO comment.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LsoComment {
    /// The comment exactly as DCS sent it.
    pub raw: String,
    /// Normalised grade: `_OK_`, `OK`, `(OK)`, `--`, `C`, `B`, `WO`, `OWO`,
    /// `WOP`, `WOFD`, `NC` ...
    pub grade: String,
    /// The deviation calls as written, grade and wire removed.
    pub details: String,
    pub calls: Vec<LsoCall>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub wire: Option<u8>,
}

/// Positions, longest first so `IM` wins over `M`-anything and `AW` over `W`.
const POSITIONS: &[(&str, &str)] = &[
    ("IW", "in the wires"),
    ("IM", "in the middle (second third)"),
    ("IC", "in close (last third)"),
    ("AR", "at the ramp"),
    ("AW", "all the way"),
    ("TL", "to landing"),
    ("BC", "at the ball call"),
    ("X", "at the start (first third)"),
];

/// Errors, longest first. Matched against what is left after the position
/// suffix is removed, so these are whole-string matches.
const ERRORS: &[(&str, &str)] = &[
    ("TMRD", "too much rate of descent"),
    ("NERD", "not enough rate of descent"),
    ("NESA", "not enough straightaway"),
    ("3PTS", "three-point landing"),
    ("LLWD", "left wing down a lot"),
    ("LRWD", "right wing down a lot"),
    ("SLO", "slow"),
    ("LUL", "lined up left"),
    ("LUR", "lined up right"),
    ("LIG", "long in the groove"),
    ("LNF", "landed nose first"),
    ("PNU", "pulled nose up"),
    ("NEP", "not enough power"),
    ("TMP", "too much power"),
    ("SRD", "stopped rate of descent"),
    ("DEC", "decelerating"),
    ("ACC", "accelerating"),
    ("LWD", "left wing down"),
    ("RWD", "right wing down"),
    ("TWA", "too wide abeam"),
    ("TCA", "too close abeam"),
    ("NSU", "not set up"),
    ("OSCB", "overshoot coming back"),
    ("AFU", "all fouled up"),
    ("CB", "climbing"),
    ("CD", "come down"),
    ("DL", "drifted left"),
    ("DR", "drifted right"),
    ("AA", "angling approach"),
    ("OS", "overshoot"),
    ("EG", "eased gun"),
    ("LO", "low"),
    ("WU", "wings up"),
    ("ST", "settled"),
    ("P", "power"),
    ("H", "high"),
    ("F", "fast"),
];

fn lookup(tbl: &[(&str, &'static str)], k: &str) -> Option<&'static str> {
    tbl.iter().find(|(c, _)| *c == k).map(|(_, v)| *v)
}

/// Human name of a grade, e.g. `WO` -> "Waveoff".
pub fn grade_name(grade: &str) -> &'static str {
    match grade {
        "_OK_" => "Perfect pass",
        "OK" => "Good pass",
        "(OK)" => "Fair pass",
        "--" => "No grade",
        "C" => "Cut pass",
        "B" => "Bolter",
        "WO" => "Waveoff",
        "OWO" => "Own waveoff",
        "WOP" => "Pattern waveoff",
        "WOFD" => "Waveoff, foul deck",
        "NC" => "No count",
        _ => "Ungraded",
    }
}

/// Greenie board points for a grade (MOOSE/NATOPS convention). `None` means
/// the pass does not count toward an average: a foul-deck waveoff is not the
/// pilot's fault and a no-count was never graded.
pub fn grade_points(grade: &str) -> Option<f64> {
    Some(match grade {
        "_OK_" => 5.0,
        "OK" => 4.0,
        "(OK)" => 3.0,
        "B" => 2.5,
        "--" => 2.0,
        "OWO" => 2.0,
        "WO" | "WOP" => 1.0,
        "C" => 0.0,
        _ => return None,
    })
}

/// Normalise the grade token DCS writes to the set above.
pub fn normalize_grade(g: &str) -> String {
    let g = g.trim().trim_end_matches(':').trim();
    match g {
        "---" | "--" | "-" => "--".into(),
        "CUT" => "C".into(),
        "BOLTER" => "B".into(),
        "WOAF" | "TWO" | "TLU" => "WO".into(),
        s => s.to_string(),
    }
}

/// Split `inner` (no magnitude wrapper) into error + position.
fn split_call(inner: &str) -> (String, Option<String>) {
    for (p, _) in POSITIONS {
        if inner.len() > p.len() && inner.ends_with(p) {
            let e = &inner[..inner.len() - p.len()];
            if lookup(ERRORS, e).is_some() {
                return (e.to_string(), Some(p.to_string()));
            }
        }
    }
    (inner.to_string(), None)
}

fn render(error: &str, position: Option<&str>, mag: Magnitude) -> String {
    let e = lookup(ERRORS, error).map(|s| s.to_string()).unwrap_or_else(|| error.to_string());
    let e = match mag {
        Magnitude::Little => format!("a little {e}"),
        Magnitude::Normal => e,
        Magnitude::Lot => format!("VERY {e}"),
    };
    match position.and_then(|p| lookup(POSITIONS, p)) {
        Some(p) => format!("{e} {p}"),
        None => e,
    }
}

/// Parse one call token such as `_HAW_`, `(SLO)AR`, `(DRX)`, `FIM`.
pub fn parse_call(tok: &str) -> LsoCall {
    let raw = tok.to_string();
    let (mag, error, position) = if tok.len() > 2 && tok.starts_with('_') && tok.ends_with('_') {
        let (e, p) = split_call(&tok[1..tok.len() - 1]);
        (Magnitude::Lot, e, p)
    } else if let Some(rest) = tok.strip_prefix('(') {
        match rest.find(')') {
            Some(i) => {
                let inside = &rest[..i];
                let after = &rest[i + 1..];
                if after.is_empty() {
                    let (e, p) = split_call(inside);
                    (Magnitude::Little, e, p)
                } else {
                    (Magnitude::Little, inside.to_string(), Some(after.to_string()))
                }
            }
            None => {
                let (e, p) = split_call(rest);
                (Magnitude::Little, e, p)
            }
        }
    } else if let Some(rest) = tok.strip_prefix('_') {
        // `_SLO_AR`: underscores around the error only
        match rest.find('_') {
            Some(i) => {
                let after = &rest[i + 1..];
                (
                    Magnitude::Lot,
                    rest[..i].to_string(),
                    (!after.is_empty()).then(|| after.to_string()),
                )
            }
            None => {
                let (e, p) = split_call(rest);
                (Magnitude::Lot, e, p)
            }
        }
    } else {
        let (e, p) = split_call(tok);
        (Magnitude::Normal, e, p)
    };
    let text = render(&error, position.as_deref(), mag);
    LsoCall { raw, error, position, magnitude: mag, text }
}

/// Pull the wire number out of the details, accepting `WIRE# 3`, `WIRE #3`,
/// `WIRE#3` and `WIRE 3`. Returns the details with the wire removed.
fn take_wire(s: &str) -> (String, Option<u8>) {
    let up = s.to_ascii_uppercase();
    if let Some(i) = up.find("WIRE") {
        let tail = &s[i + 4..];
        let digits: String = tail
            .chars()
            .skip_while(|c| *c == '#' || c.is_whitespace())
            .take_while(|c| c.is_ascii_digit())
            .collect();
        let consumed = tail
            .chars()
            .take_while(|c| *c == '#' || c.is_whitespace() || c.is_ascii_digit())
            .count();
        let wire = digits.parse::<u8>().ok().filter(|w| (1..=4).contains(w));
        let mut rest = String::from(&s[..i]);
        rest.push_str(&tail[consumed.min(tail.len())..]);
        return (rest, wire);
    }
    (s.to_string(), None)
}

/// Parse a DCS LANDING_QUALITY_MARK comment. Returns `None` when it has no
/// `GRADE:` in it at all.
pub fn parse_comment(raw: &str) -> Option<LsoComment> {
    let up = raw.to_ascii_uppercase();
    let gi = up.find("GRADE:")?;
    let after = raw[gi + 6..].trim_start();
    // the grade is the first whitespace/colon-delimited token
    let end = after
        .find(|c: char| c.is_whitespace() || c == ':')
        .unwrap_or(after.len());
    let grade = normalize_grade(&after[..end]);
    let rest = after[end..].trim_start_matches(|c: char| c == ':' || c.is_whitespace());
    let (rest, wire) = take_wire(rest);
    let details: Vec<&str> = rest
        .split_whitespace()
        .filter(|t| *t != ":" && !t.is_empty())
        .collect();
    let calls = details.iter().map(|t| parse_call(t)).collect();
    Some(LsoComment { raw: raw.to_string(), grade, details: details.join(" "), calls, wire })
}

/// The plain-English lines for a details string (no grade, no wire).
pub fn describe(details: &str) -> Vec<String> {
    details.split_whitespace().map(|t| parse_call(t).text).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn screenshot_example() {
        let c = parse_comment("LSO: GRADE:WO AAX FIM (SLO)AR _HAW_").unwrap();
        assert_eq!(c.grade, "WO");
        assert_eq!(grade_name(&c.grade), "Waveoff");
        let t: Vec<_> = c.calls.iter().map(|c| c.text.as_str()).collect();
        assert_eq!(
            t,
            vec![
                "angling approach at the start (first third)",
                "fast in the middle (second third)",
                "a little slow at the ramp",
                "VERY high all the way",
            ]
        );
    }

    #[test]
    fn wire_forms() {
        let c = parse_comment("LSO: GRADE:_OK_ : WIRE# 3").unwrap();
        assert_eq!(c.grade, "_OK_");
        assert_eq!(c.wire, Some(3));
        assert!(c.calls.is_empty());
        let c = parse_comment("LSO: GRADE:C _SLOX_ _LURX_ 3PTSIW LNFIW WIRE #1").unwrap();
        assert_eq!(c.grade, "C");
        assert_eq!(c.wire, Some(1));
        assert_eq!(c.calls.len(), 4);
        assert_eq!(c.calls[0].text, "VERY slow at the start (first third)");
        assert_eq!(c.calls[1].text, "VERY lined up right at the start (first third)");
        assert_eq!(c.calls[2].text, "three-point landing in the wires");
        assert_eq!(c.calls[3].text, "landed nose first in the wires");
    }

    #[test]
    fn no_count_and_unknown() {
        let c = parse_comment("GRADE:--- : _LOIC_ _LOAR_").unwrap();
        assert_eq!(c.grade, "--");
        assert_eq!(c.calls[0].text, "VERY low in close (last third)");
        assert_eq!(grade_points("WOFD"), None);
        assert_eq!(parse_call("ZZZ").text, "ZZZ");
    }
}
