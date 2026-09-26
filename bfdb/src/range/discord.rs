// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! Discord embeds for range results, posted verbatim by the DCSServerBot
//! range extension.
//!
//! Shape (flat on purpose, the bot maps it onto its own `discord.Embed`):
//! `{"title","description","color","fields":[{"name","value","inline"}],
//!   "footer","image","url"}` where `image` is the absolute card PNG URL and
//! `url` the result's page on the range site.
//!
//! The trap embed mirrors the classic "Trap Attempt Detected" LSO card:
//! title, a bold `pilot | airframe | CASE n | Day` line, then the grade, the
//! raw LSO comment and its plain-English decoding.

use super::RangeCtx;
use bfprotocols::range::{
    lso, BombQuality, EngagementOutcome, MissileOutcome, PrecisionQuality, RangeRecord,
    RangeResult, StrafeQuality,
};
use serde_json::{json, Value};

const GREEN: u32 = 0x57f287;
const DARK_GREEN: u32 = 0x2ecc71;
const YELLOW: u32 = 0xfee75c;
const ORANGE: u32 = 0xe67e22;
const RED: u32 = 0xed4245;
const GREY: u32 = 0x99aab5;
const BLUE: u32 = 0x5865f2;

/// The short airframe name pilots use: `FA-18C_hornet` -> `F18`.
pub(crate) fn short_airframe(t: &str) -> String {
    let u = t.to_ascii_uppercase();
    let known: &[(&str, &str)] = &[
        ("FA-18", "F18"),
        ("F/A-18", "F18"),
        ("F-14", "F14"),
        ("T-45", "T45"),
        ("AV8B", "AV8B"),
        ("AV-8B", "AV8B"),
        ("F-35", "F35"),
        ("A-4E", "A4"),
        ("SU-33", "SU33"),
        ("SU-27", "SU27"),
        ("MIG-29", "MIG29"),
        ("F-16", "F16"),
        ("F-15E", "F15E"),
        ("F-15", "F15"),
        ("A-10", "A10"),
        ("F-5", "F5"),
        ("M-2000", "M2000"),
        ("MIRAGE-F1", "F1"),
        ("JF-17", "JF17"),
        ("AH-64", "AH64"),
        ("UH-1H", "UH1"),
        ("UH-60", "UH60"),
        ("CH-47", "CH47"),
        ("MI-8", "MI8"),
        ("MI-24", "MI24"),
        ("KA-50", "KA50"),
        ("SA342", "SA342"),
        ("OH58", "OH58"),
    ];
    for (k, v) in known {
        if u.starts_with(k) {
            return v.to_string();
        }
    }
    let s: String = u.chars().filter(|c| c.is_ascii_alphanumeric()).take(6).collect();
    if s.is_empty() {
        t.to_string()
    } else {
        s
    }
}

fn bomb_color(q: BombQuality) -> u32 {
    match q {
        BombQuality::Shack => DARK_GREEN,
        BombQuality::Excellent => GREEN,
        BombQuality::Good => YELLOW,
        BombQuality::Ineffective => ORANGE,
        BombQuality::Poor => RED,
    }
}

fn precision_color(q: PrecisionQuality) -> u32 {
    match q {
        PrecisionQuality::Perfect => DARK_GREEN,
        PrecisionQuality::Excellent => GREEN,
        PrecisionQuality::Good => YELLOW,
        PrecisionQuality::Fair => ORANGE,
        PrecisionQuality::Poor => RED,
    }
}

fn grade_color(g: &str) -> u32 {
    match g {
        "_OK_" => DARK_GREEN,
        "OK" => GREEN,
        "(OK)" => YELLOW,
        "B" | "--" | "OWO" => ORANGE,
        "WO" | "WOP" | "C" => RED,
        _ => GREY,
    }
}

/// Embed title and colour for a record.
pub(crate) fn title_and_color(r: &RangeRecord) -> (String, u32) {
    match &r.result {
        RangeResult::Trap(t) => ("Trap Attempt Detected".into(), grade_color(&t.grade)),
        RangeResult::Bomb(b) => ("Bombing result".into(), bomb_color(b.quality)),
        RangeResult::Strafe(s) => (
            "Strafe result".into(),
            match s.quality {
                StrafeQuality::Deadeye => DARK_GREEN,
                StrafeQuality::Excellent => GREEN,
                StrafeQuality::Good => YELLOW,
                StrafeQuality::Ineffective => ORANGE,
                StrafeQuality::Poor | StrafeQuality::Invalid => RED,
            },
        ),
        RangeResult::Aar(a) => (
            "Air-to-air refuelling".into(),
            match a.grade.as_str() {
                "A" => DARK_GREEN,
                "B" => GREEN,
                "C" => YELLOW,
                "D" => ORANGE,
                _ => RED,
            },
        ),
        RangeResult::Missile(m) => (
            "Missile trainer".into(),
            match (m.outcome, m.perspective.as_str()) {
                (MissileOutcome::Kill | MissileOutcome::Hit, "target") => RED,
                (MissileOutcome::Kill | MissileOutcome::Hit, _) => GREEN,
                (_, "target") => GREEN,
                _ => ORANGE,
            },
        ),
        RangeResult::Engagement(e) => (
            "Engagement result".into(),
            match e.outcome {
                EngagementOutcome::Win => GREEN,
                EngagementOutcome::Loss => RED,
                EngagementOutcome::Draw => YELLOW,
                EngagementOutcome::Abort => GREY,
            },
        ),
        RangeResult::AntiShip(a) => ("Anti-ship strike".into(), if a.hit { GREEN } else { RED }),
        RangeResult::Sling(s) => (
            if s.method == "internal" { "Cargo delivery (internal)".into() } else { "Sling-load delivery".into() },
            precision_color(s.quality),
        ),
        RangeResult::Landing(l) => ("Precision landing".into(), precision_color(l.quality)),
        RangeResult::Troops(t) => ("Troop insertion".into(), precision_color(t.quality)),
        RangeResult::Gunnery(_) => ("Gunnery lane".into(), BLUE),
        RangeResult::Cas(c) => ("Close air support".into(), if c.correct_target { GREEN } else { RED }),
    }
}

fn f(name: &str, value: impl Into<String>, inline: bool) -> (String, String, bool) {
    let v: String = value.into();
    (name.to_string(), if v.is_empty() { "-".into() } else { v }, inline)
}

/// The embed fields for a record, in display order: (name, value, inline).
/// The generic result card reuses these.
pub(crate) fn fields(r: &RangeRecord) -> Vec<(String, String, bool)> {
    const FT: f64 = 3.28084;
    let mut o = vec![];
    match &r.result {
        RangeResult::Trap(t) => {
            o.push(f("LSO Grade", format!("{} => {}", t.grade, lso::grade_name(&t.grade)), false));
            o.push(f("LSO Comment", t.lso_comment.clone(), false));
            let desc = if t.lso_description.is_empty() {
                lso::describe(&t.lso_comment)
            } else {
                t.lso_description.clone()
            };
            o.push(f("LSO Comment Description", desc.join("\n"), false));
            if let Some(w) = t.wire {
                o.push(f("Wire", format!("#{w}"), true));
            }
            if let Some(g) = t.groove_time_s {
                o.push(f("Groove time", format!("{g:.1} s"), true));
            }
            if let Some(w) = t.wind_over_deck_kts {
                o.push(f("Wind over deck", format!("{w:.0} kts"), true));
            }
            if let Some(p) = t.points {
                o.push(f("Points", format!("{p:.1}"), true));
            }
        }
        RangeResult::Bomb(b) => {
            let w = if b.weapon_display.is_empty() { &b.weapon } else { &b.weapon_display };
            o.push(f("Weapon", w.clone(), true));
            o.push(f("Range", format!("{}: {}", b.range, b.target), true));
            o.push(f("Quality", format!("**{}**", b.quality.label()), true));
            o.push(f("Miss (r)", format!("{:.1} m", b.miss_m), true));
            o.push(f("Radial (φ)", format!("{:.1}°", b.radial_deg.rem_euclid(360.)), true));
            o.push(f("Clock", format!("{} o'clock", b.clock), true));
            let alt = if b.release.pos.alt_m != 0. { b.release.pos.alt_m } else { b.release.alt_agl_m };
            o.push(f("Release alt", format!("{:.0} ft", alt * FT), true));
            o.push(f("Release speed", format!("{:.0} kts", b.release.tas_kts), true));
            o.push(f("Release heading", format!("{:03.0}°", b.release.heading_deg.rem_euclid(360.)), true));
            o.push(f("Dive", format!("{:.0}°", b.release.dive_deg), true));
        }
        RangeResult::Strafe(s) => {
            o.push(f("Range", format!("{}: {}", s.range, s.pit), true));
            o.push(f("Gun", s.gun.clone(), true));
            o.push(f("Quality", format!("**{}**", s.quality.label()), true));
            o.push(f("Hits", format!("{} / {}", s.hits, s.rounds_fired), true));
            o.push(f("Accuracy", format!("{:.1}%", s.accuracy_pct), true));
            o.push(f("Foul line", if s.foul_line_crossed { "CROSSED" } else { "ok" }, true));
            if let Some(why) = &s.invalid_reason {
                o.push(f("Invalid", why.clone(), false));
            }
        }
        RangeResult::Aar(a) => {
            o.push(f("Tanker", format!("{} ({})", a.tanker, a.tanker_type), true));
            o.push(f("Grade", format!("**{}**", a.grade), true));
            o.push(f("Contacts", format!("{} ({} disconnects)", a.contacts, a.disconnects), true));
            o.push(f("Connected", format!("{:.0} s", a.time_connected_s), true));
            o.push(f("Fuel", format!("{:.0} lb", a.fuel_lbs), true));
            o.push(f(
                "Stability",
                format!(
                    "±{:.1} / ±{:.1} / ±{:.1} m",
                    a.stability.fore_aft_sd_m, a.stability.lateral_sd_m, a.stability.vertical_sd_m
                ),
                true,
            ));
            if !a.calls.is_empty() {
                o.push(f("Calls", a.calls.join("\n"), false));
            }
        }
        RangeResult::Missile(m) => {
            o.push(f("Shooter", format!("{} ({})", m.shooter.name, m.shooter_type), true));
            o.push(f("Target", format!("{} ({})", m.target.name, m.target_type), true));
            o.push(f("Weapon", m.weapon.clone(), true));
            o.push(f("Outcome", format!("**{}**", m.outcome.label()), true));
            o.push(f("Launch range", format!("{:.1} nm", m.launch.range_m / 1852.), true));
            o.push(f("Aspect", format!("{:.0}°", m.launch.aspect_deg), true));
            o.push(f("Miss distance", format!("{:.0} m", m.min_distance_m), true));
            o.push(f("Time of flight", format!("{:.1} s", m.time_of_flight_s), true));
            if m.perspective == "target" {
                o.push(f(
                    "Reaction",
                    m.defense.reaction_s.map(|x| format!("{x:.1} s")).unwrap_or_else(|| "none".into()),
                    true,
                ));
            }
        }
        RangeResult::Engagement(e) => {
            o.push(f("Setup", e.setup.clone(), true));
            o.push(f("Adversary", e.adversary.clone(), true));
            o.push(f("Outcome", format!("**{}**", e.outcome.label()), true));
            o.push(f("Duration", format!("{:.0} s", e.duration_s), true));
            o.push(f("Shots", e.shots_fired.to_string(), true));
            o.push(f("Kills", format!("{} (+{} gun hits)", e.trainer_kills, e.gun_hits), true));
            if !e.notes.is_empty() {
                o.push(f("Notes", e.notes.join("\n"), false));
            }
        }
        RangeResult::AntiShip(a) => {
            o.push(f("Ship", format!("{} ({})", a.ship, a.ship_type), true));
            o.push(f("Weapon", a.weapon.clone(), true));
            o.push(f("Result", if a.hit { "**HIT**" } else { "**MISS**" }, true));
            o.push(f("Damage", format!("{:.0}%{}", a.damage * 100., if a.ship_sunk { ", sunk" } else { "" }), true));
            o.push(f("Launch range", format!("{:.1} nm", a.launch_range_m / 1852.), true));
            o.push(f("Time of flight", format!("{:.0} s", a.time_of_flight_s), true));
        }
        RangeResult::Sling(s) => {
            o.push(f("Course", s.course.clone(), true));
            o.push(f("Cargo", format!("{} ({:.0} kg)", s.cargo, s.mass_kg), true));
            o.push(f("Quality", format!("**{}**", s.quality.label()), true));
            o.push(f("Distance", format!("{:.1} m", s.distance_m), true));
            o.push(f("Time", format!("{:.0} s", s.time_s), true));
            o.push(f("Damage", format!("{:.0}%", s.damage * 100.), true));
        }
        RangeResult::Landing(l) => {
            o.push(f("Drill", l.drill.clone(), true));
            o.push(f("Pad", l.pad.clone(), true));
            o.push(f("Quality", format!("**{}**", l.quality.label()), true));
            o.push(f("Distance", format!("{:.1} m", l.distance_m), true));
            o.push(f("Touchdown", format!("{:.0} fpm", l.touchdown_fpm), true));
            if let Some(h) = l.heading_error_deg {
                o.push(f("Heading error", format!("{h:.0}°"), true));
            }
        }
        RangeResult::Troops(t) => {
            o.push(f("LZ", t.lz.clone(), true));
            o.push(f("Troops", t.troops.to_string(), true));
            o.push(f("Quality", format!("**{}**", t.quality.label()), true));
            o.push(f("Load time", format!("{:.0} s", t.load_time_s), true));
            o.push(f("Total time", format!("{:.0} s", t.total_time_s), true));
            o.push(f("Landing", format!("{:.1} m off", t.landing_distance_m), true));
        }
        RangeResult::Gunnery(g) => {
            o.push(f("Lane", g.lane.clone(), true));
            o.push(f("Targets", format!("{}/{}", g.targets_killed, g.targets_total), true));
            o.push(f("Hits", format!("{}/{}", g.hits, g.shots), true));
            o.push(f("Time", format!("{:.0} s", g.time_s), true));
            o.push(f("First-round hits", g.first_round_hits.to_string(), true));
        }
        RangeResult::Cas(c) => {
            o.push(f("JTAC", c.jtac.clone(), true));
            o.push(f("Target", c.target.clone(), true));
            o.push(f("Weapon", c.weapon.clone(), true));
            o.push(f("Correct target", if c.correct_target { "yes" } else { "**NO**" }, true));
            o.push(f("Miss", format!("{:.0} m", c.miss_m), true));
            o.push(f("Time to impact", format!("{:.0} s", c.time_to_impact_s), true));
            if c.danger_close {
                o.push(f(
                    "Danger close",
                    c.nearest_friendly_m.map(|d| format!("{d:.0} m from friendlies")).unwrap_or_else(|| "yes".into()),
                    true,
                ));
            }
        }
    }
    o
}

/// The embed for one record.
pub(crate) fn embed(ctx: &RangeCtx, r: &RangeRecord) -> Value {
    let (title, color) = title_and_color(r);
    let air = short_airframe(&r.unit_type);
    let description = match &r.result {
        RangeResult::Trap(t) => format!(
            "**{} | {} | CASE {} | {}**",
            r.pilot.name,
            air,
            t.case,
            if t.night { "Night" } else { "Day" }
        ),
        RangeResult::Missile(m) if m.perspective == "target" => {
            format!("**{} | {} | defending**", r.pilot.name, air)
        }
        _ => format!("**{} | {}**", r.pilot.name, air),
    };
    let footer = if r.theatre.is_empty() {
        "Vector Strike Range".to_string()
    } else {
        format!("{}: {} ({})  ·  Vector Strike Range", r.theatre, r.mission_date, r.mission_time)
    };
    let fields: Vec<Value> = fields(r)
        .into_iter()
        .map(|(name, value, inline)| {
            // Discord caps a field value at 1024 characters.
            let value: String = value.chars().take(1024).collect();
            json!({ "name": name, "value": value, "inline": inline })
        })
        .collect();
    let id = urlencoding::encode(&r.id);
    json!({
        "title": title,
        "description": description,
        "color": color,
        "fields": fields,
        "footer": footer,
        "image": format!("{}{}", ctx.api_url, RangeCtx::card_url(&r.id, "png")),
        "url": format!("{}/result/{}", ctx.site_url, id),
        "timestamp": r.ts.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn airframes() {
        assert_eq!(short_airframe("FA-18C_hornet"), "F18");
        assert_eq!(short_airframe("F-14B"), "F14");
        assert_eq!(short_airframe("T-45"), "T45");
        assert_eq!(short_airframe("AV8BNA"), "AV8B");
        assert_eq!(short_airframe("Su-33"), "SU33");
        assert_eq!(short_airframe("VSN_F4B"), "VSNF4B");
    }

    #[test]
    fn trap_fields_match_reference() {
        let r = super::super::cards::tests::trap();
        let f = fields(&r);
        assert_eq!(f[0].0, "LSO Grade");
        assert_eq!(f[0].1, "WO => Waveoff");
        assert_eq!(f[1].1, "AAX FIM (SLO)AR _HAW_");
        assert_eq!(f[2].0, "LSO Comment Description");
        assert!(f[2].1.starts_with("angling approach at the start"));
        let (t, _) = title_and_color(&r);
        assert_eq!(t, "Trap Attempt Detected");
    }
}
