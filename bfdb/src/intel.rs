//! Parser for F-14 TARPS screenshot filenames.
//!
//! DCS writes TARPS captures with the shot metadata encoded in the filename,
//! e.g.
//!
//! ```text
//! TARPS KS-87D 07-16-22L 01-02-2005 N25-14-13 E055-24-12 ALT+08780 DRIFT+00 HDG301 PITCH+01 ROLL+01.png
//! ```
//!
//! - `07-16-22L`  in-mission time `HH-MM-SS` + timezone letter
//! - `01-02-2005` in-mission date `MM-DD-YYYY`
//! - `N25-14-13`  latitude DMS   (hemisphere letter prefix)
//! - `E055-24-12` longitude DMS
//! - `ALT+08780`  altitude, feet
//! - `HDG301`     heading, degrees true
//! - `PITCH+01` / `ROLL+01` attitude, degrees
//!
//! The parser is deliberately tolerant: each token is matched independently
//! so reordering, extra fields, or a missing attitude value doesn't lose the
//! rest. A filename with no recognisable coordinates yields `lat`/`lon` of
//! `None`, and the caller stores the capture unplaced for manual dropping.

use chrono::{DateTime, NaiveDate, NaiveTime, TimeZone, Utc};
use regex::Regex;
use std::sync::OnceLock;

#[derive(Debug, Default, Clone, PartialEq)]
pub(crate) struct ParsedCapture {
    pub lat: Option<f64>,
    pub lon: Option<f64>,
    pub alt_ft: Option<f64>,
    pub heading_deg: Option<f64>,
    pub pitch_deg: Option<f64>,
    pub roll_deg: Option<f64>,
    pub captured_at: Option<DateTime<Utc>>,
}

impl ParsedCapture {
    /// True when we recovered a usable ground position.
    pub fn has_position(&self) -> bool {
        self.lat.is_some() && self.lon.is_some()
    }
}

fn re(src: &str) -> Regex {
    Regex::new(src).expect("static intel regex")
}

fn dms_to_deg(d: f64, m: f64, s: f64, negative: bool) -> f64 {
    let v = d + m / 60.0 + s / 3600.0;
    if negative {
        -v
    } else {
        v
    }
}

/// Parse a TARPS screenshot filename. Never fails -- unrecognised fields are
/// simply left `None`.
pub(crate) fn parse_filename(name: &str) -> ParsedCapture {
    static LAT: OnceLock<Regex> = OnceLock::new();
    static LON: OnceLock<Regex> = OnceLock::new();
    static ALT: OnceLock<Regex> = OnceLock::new();
    static HDG: OnceLock<Regex> = OnceLock::new();
    static PITCH: OnceLock<Regex> = OnceLock::new();
    static ROLL: OnceLock<Regex> = OnceLock::new();
    static DATE: OnceLock<Regex> = OnceLock::new();
    static TIME: OnceLock<Regex> = OnceLock::new();

    let lat_re = LAT.get_or_init(|| re(r"(?i)\b([NS])\s*(\d{1,3})-(\d{1,2})-(\d{1,2}(?:\.\d+)?)"));
    let lon_re = LON.get_or_init(|| re(r"(?i)\b([EW])\s*(\d{1,3})-(\d{1,2})-(\d{1,2}(?:\.\d+)?)"));
    let alt_re = ALT.get_or_init(|| re(r"(?i)\bALT\s*([+-]?\d+(?:\.\d+)?)"));
    let hdg_re = HDG.get_or_init(|| re(r"(?i)\bHDG\s*([+-]?\d+(?:\.\d+)?)"));
    let pitch_re = PITCH.get_or_init(|| re(r"(?i)\bPITCH\s*([+-]?\d+(?:\.\d+)?)"));
    let roll_re = ROLL.get_or_init(|| re(r"(?i)\bROLL\s*([+-]?\d+(?:\.\d+)?)"));
    // date: MM-DD-YYYY (4-digit year disambiguates from the DMS groups)
    let date_re = DATE.get_or_init(|| re(r"\b(\d{1,2})-(\d{1,2})-(\d{4})\b"));
    // time: HH-MM-SS followed by a single timezone letter
    let time_re = TIME.get_or_init(|| re(r"\b(\d{1,2})-(\d{2})-(\d{2})([A-Za-z])\b"));

    let mut out = ParsedCapture::default();

    if let Some(c) = lat_re.captures(name) {
        let neg = c[1].eq_ignore_ascii_case("S");
        let (d, m, s) = (
            c[2].parse().unwrap_or(0.0),
            c[3].parse().unwrap_or(0.0),
            c[4].parse().unwrap_or(0.0),
        );
        let v = dms_to_deg(d, m, s, neg);
        if (-90.0..=90.0).contains(&v) {
            out.lat = Some(v);
        }
    }
    if let Some(c) = lon_re.captures(name) {
        let neg = c[1].eq_ignore_ascii_case("W");
        let (d, m, s) = (
            c[2].parse().unwrap_or(0.0),
            c[3].parse().unwrap_or(0.0),
            c[4].parse().unwrap_or(0.0),
        );
        let v = dms_to_deg(d, m, s, neg);
        if (-180.0..=180.0).contains(&v) {
            out.lon = Some(v);
        }
    }
    out.alt_ft = alt_re
        .captures(name)
        .and_then(|c| c[1].parse::<f64>().ok());
    out.heading_deg = hdg_re
        .captures(name)
        .and_then(|c| c[1].parse::<f64>().ok())
        .map(|h| h.rem_euclid(360.0));
    out.pitch_deg = pitch_re.captures(name).and_then(|c| c[1].parse::<f64>().ok());
    out.roll_deg = roll_re.captures(name).and_then(|c| c[1].parse::<f64>().ok());

    let date = date_re.captures(name).and_then(|c| {
        NaiveDate::from_ymd_opt(
            c[3].parse().ok()?,
            c[1].parse().ok()?,
            c[2].parse().ok()?,
        )
    });
    let time = time_re.captures(name).and_then(|c| {
        NaiveTime::from_hms_opt(c[1].parse().ok()?, c[2].parse().ok()?, c[3].parse().ok()?)
    });
    if let (Some(d), Some(t)) = (date, time) {
        // In-mission clock, treated as UTC -- there's no reliable timezone
        // mapping and it's only used for relative ordering / age display.
        out.captured_at = Utc.from_utc_datetime(&d.and_time(t)).into();
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_reference_filename() {
        let p = parse_filename(
            "TARPS KS-87D 07-16-22L 01-02-2005 N25-14-13 E055-24-12 ALT+08780 DRIFT+00 HDG301 PITCH+01 ROLL+01.png",
        );
        assert!((p.lat.unwrap() - (25.0 + 14.0 / 60.0 + 13.0 / 3600.0)).abs() < 1e-9);
        assert!((p.lon.unwrap() - (55.0 + 24.0 / 60.0 + 12.0 / 3600.0)).abs() < 1e-9);
        assert_eq!(p.alt_ft, Some(8780.0));
        assert_eq!(p.heading_deg, Some(301.0));
        assert_eq!(p.pitch_deg, Some(1.0));
        assert_eq!(p.roll_deg, Some(1.0));
        let ts = p.captured_at.unwrap();
        assert_eq!(ts.format("%Y-%m-%d %H:%M:%S").to_string(), "2005-01-02 07:16:22");
        assert!(p.has_position());
    }

    #[test]
    fn southern_western_hemisphere_is_negative() {
        let p = parse_filename("TARPS S34-30-00 W070-00-00 ALT+10000 HDG090 PITCH-05 ROLL+10.png");
        assert!((p.lat.unwrap() - (-34.5)).abs() < 1e-9);
        assert!((p.lon.unwrap() - (-70.0)).abs() < 1e-9);
        assert_eq!(p.pitch_deg, Some(-5.0));
    }

    #[test]
    fn unparseable_name_has_no_position() {
        let p = parse_filename("Screen_250102_071622.png");
        assert!(!p.has_position());
        assert_eq!(p.lat, None);
    }

    #[test]
    fn missing_attitude_still_parses_coords() {
        let p = parse_filename("TARPS N40-00-00 E010-00-00 ALT+05000.png");
        assert!(p.has_position());
        assert_eq!(p.heading_deg, None);
        assert_eq!(p.alt_ft, Some(5000.0));
    }
}
