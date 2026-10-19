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
//! raw LSO comment and its plain-English decoding, and the ship and its class
//! (from the same deck file the cards and the site draw).
//!
//! A field landing is grouped the way an instructor reads it out -- the
//! touchdown, the approach, the pattern, the rollout and taxi -- each one
//! field of a few short lines, so it reads the same on a phone (where
//! Discord stacks inline fields one under the other) as on a desktop.
//!
//! `links` (`[{"label","url"}]`) are the debrief's own ways in: the debrief,
//! the 3D replay, the dashboard's web Tacview and the Tacview clip. The bot
//! shows them as link buttons under the post. Web Tacview goes through the
//! debrief (`?open=tacview`), which asks bfdb where the result is in the
//! replay when it is clicked: when the post goes out the server is still
//! writing the recording, so there is no replay link to give yet.

use super::{deckplan, icons, RangeCtx};
use bfprotocols::range::{
    lso, BombQuality, CsarOutcome, EngagementOutcome, FieldLandingOutcome, HotZoneOutcome,
    MissileOutcome, PrecisionQuality, RangeRecord, RangeResult, StrafeQuality,
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
        RangeResult::Landing(l) => (
            if l.drill == "ship" { "Deck landing".into() } else { "Precision landing".into() },
            precision_color(l.quality),
        ),
        RangeResult::Troops(t) => ("Troop insertion".into(), precision_color(t.quality)),
        RangeResult::Gunnery(_) => ("Gunnery lane".into(), BLUE),
        RangeResult::Cas(c) => ("Close air support".into(), if c.correct_target { GREEN } else { RED }),
        RangeResult::Sead(s) => (
            if s.site_destroyed { "SEAD: site destroyed".into() } else { "SEAD / DEAD kill".into() },
            match (s.site_destroyed, s.site_was_emitting) {
                (true, _) => DARK_GREEN,
                (false, true) => GREEN,
                (false, false) => YELLOW,
            },
        ),
        RangeResult::HotZone(h) => (
            "Hot zone sortie".into(),
            match h.outcome {
                HotZoneOutcome::ShotDown => RED,
                _ if h.trainer_deaths > 0 => ORANGE,
                HotZoneOutcome::Left => GREY,
                _ if h.air_kills + h.ground_kills > 0 => GREEN,
                _ => YELLOW,
            },
        ),
        RangeResult::LowLevel(l) => ("Low-level route".into(), precision_color(l.quality)),
        RangeResult::FieldLanding(fl) => (
            "Field landing".into(),
            if fl.outcome == FieldLandingOutcome::Undershoot { RED } else { precision_color(fl.quality) },
        ),
        RangeResult::Csar(c) => (
            "Combat search and rescue".into(),
            if c.outcome == CsarOutcome::Failed { RED } else { precision_color(c.quality) },
        ),
    }
}

/// Seconds as "m:ss".
pub(crate) fn mmss(s: f64) -> String {
    if !s.is_finite() {
        return "-".into();
    }
    let t = s.abs().round() as u64;
    format!("{}{}:{:02}", if s < 0. { "-" } else { "" }, t / 60, t % 60)
}

/// Where a runway touchdown was, in words.
pub(crate) fn from_threshold(m: f64) -> String {
    if m >= 0. {
        format!("{m:.0} m past the threshold")
    } else {
        format!("{:.0} m short of the threshold", -m)
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
            let plan = deckplan::plan_for(&t.carrier_type);
            let ship = if t.carrier.is_empty() { plan.hull_no.clone().unwrap_or_default() } else { t.carrier.clone() };
            // the deck file's names are capitals for the drawings: "Nimitz class (CVN)" here
            let class = plan
                .name
                .split(' ')
                .map(|w| if w.starts_with('(') { w.to_string() } else if w.eq_ignore_ascii_case("class") || w.eq_ignore_ascii_case("de") { w.to_lowercase() } else { cap(&w.to_lowercase()) })
                .collect::<Vec<_>>()
                .join(" ");
            o.push(f("Ship", if ship.is_empty() { class } else { format!("{ship} · {class}") }, true));
        }
        RangeResult::Bomb(b) => {
            let w = if b.weapon_display.is_empty() { &b.weapon } else { &b.weapon_display };
            let rel = &b.release;
            let alt = if rel.pos.alt_m != 0. { rel.pos.alt_m } else { rel.alt_agl_m };
            o.push(f("Range", format!("{}: {}", b.range, b.target), true));
            o.push(f("Quality", format!("**{}**", b.quality.label()), true));
            o.push(f("Miss", format!("**{:.1} m** at {} o'clock", b.miss_m, b.clock), true));
            o.push(f(
                "Impact",
                format!(
                    "{:.1} m @ {:03.0}°\n{:.0} m {} · {:.0} m {}\nfall {:.1} s{}",
                    b.miss_m,
                    b.radial_deg.rem_euclid(360.),
                    b.long_m.abs(),
                    if b.long_m >= 0. { "long" } else { "short" },
                    b.cross_m.abs(),
                    if b.cross_m >= 0. { "right" } else { "left" },
                    b.time_of_flight_s,
                    if b.target_hit { " · target destroyed" } else { "" }
                ),
                true,
            ));
            o.push(f(
                "Release",
                format!(
                    "{:.0} ft ({:.0} AGL)\n{:.0} kts TAS · M {:.2}\n{:03.0}° · {:.0}° dive\n{:.0} m slant",
                    alt * FT,
                    rel.alt_agl_m * FT,
                    rel.tas_kts,
                    rel.mach,
                    rel.heading_deg.rem_euclid(360.),
                    rel.dive_deg,
                    rel.slant_range_m
                ),
                true,
            ));
            let mut wl = vec![w.clone(), b.guidance.to_uppercase()];
            if let Some(t) = &b.tier {
                wl.push(format!("tier {}", t.to_uppercase()));
            }
            if let Some(c) = b.laser_code {
                wl.push(format!("code {c}"));
            }
            if let Some(j) = &b.gps_denied {
                wl.push(format!("**GPS denied** ({j})"));
            }
            o.push(f("Weapon", wl.join("\n"), true));
        }
        RangeResult::Strafe(s) => {
            o.push(f("Range", format!("{}: {}", s.range, s.pit), true));
            o.push(f("Quality", format!("**{}**", s.quality.label()), true));
            o.push(f("Hits", format!("**{} / {}** · {:.1}%", s.hits, s.rounds_fired, s.accuracy_pct), true));
            o.push(f(
                "Run",
                format!(
                    "{}\nin on {:03.0}° at {:.0} ft AGL\nclosest {:.0} m (foul line {:.0} m)\nfoul line {}",
                    s.gun,
                    s.run_in_heading_deg.rem_euclid(360.),
                    s.entry_alt_agl_m * FT,
                    s.min_range_m,
                    s.foul_line_m,
                    if s.foul_line_crossed { "**CROSSED**" } else { "clear" }
                ),
                true,
            ));
            if let Some(why) = &s.invalid_reason {
                o.push(f("Invalid", why.clone(), false));
            }
        }
        RangeResult::Aar(a) => {
            o.push(f("Tanker", format!("{} ({})", a.tanker, a.tanker_type), true));
            o.push(f("Grade", format!("**{}**", a.grade), true));
            o.push(f("Fuel", format!("**{:.0} lb** · {:.0} lb/min", a.fuel_lbs, a.onload_rate_lbs_min), true));
            o.push(f(
                "Contacts",
                format!(
                    "{} contact{}, {} disconnect{}\n{:.0} s connected of {}\n{}{}",
                    a.contacts,
                    if a.contacts == 1 { "" } else { "s" },
                    a.disconnects,
                    if a.disconnects == 1 { "" } else { "s" },
                    a.time_connected_s,
                    mmss(a.session_s),
                    a.precontact_closure_kts.map(|c| format!("closure {c:.1} kts")).unwrap_or_else(|| "-".into()),
                    if a.overshoot { " · **overshoot**" } else { "" }
                ),
                true,
            ));
            o.push(f(
                "Station-keeping (1σ)",
                format!(
                    "fore/aft ±{:.1} m\nlateral ±{:.1} m\nvertical ±{:.1} m\n{:.0} ft · {:.0} kts",
                    a.stability.fore_aft_sd_m, a.stability.lateral_sd_m, a.stability.vertical_sd_m, a.alt_ft, a.speed_kts
                ),
                true,
            ));
            if !a.calls.is_empty() {
                o.push(f("Calls", a.calls.iter().map(|c| format!("• {}", cap(c))).collect::<Vec<_>>().join("\n"), false));
            }
        }
        RangeResult::Missile(m) => {
            let l = &m.launch;
            let d = &m.defense;
            o.push(f("Shooter", format!("{} ({})", m.shooter.name, short_airframe(&m.shooter_type)), true));
            o.push(f("Target", format!("{} ({})", m.target.name, short_airframe(&m.target_type)), true));
            o.push(f("Outcome", format!("**{}** · miss {:.0} m", m.outcome.label(), m.min_distance_m), true));
            o.push(f(
                "Launch",
                format!(
                    "{}\n{:.1} nm · {:.0}° aspect\n{:.0} / {:.0} ft · {:.0} kts closure\n{:.1} s flight",
                    m.weapon,
                    l.range_m / 1852.,
                    l.aspect_deg,
                    l.shooter_alt_m * FT,
                    l.target_alt_m * FT,
                    l.closure_kts,
                    m.time_of_flight_s
                ),
                true,
            ));
            if m.perspective == "target" {
                o.push(f(
                    "Defence",
                    format!(
                        "reaction {}\nbeam {:.0} s · drag {:.0} s\n{:+.0} ft{}",
                        d.reaction_s.map(|x| format!("{x:.1} s")).unwrap_or_else(|| "**none**".into()),
                        d.beam_s,
                        d.drag_s,
                        d.alt_change_m * FT,
                        if d.went_low { ", went low" } else { "" }
                    ),
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
            if let Some(v) = l.ship_speed_kts {
                o.push(f("Ship speed", format!("{v:.0} kts"), true));
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
        RangeResult::Sead(s) => {
            let g = if s.guidance.is_empty() { String::new() } else { format!(" ({})", s.guidance.to_uppercase()) };
            o.push(f("Site", format!("{} · {}", s.site, s.system), true));
            o.push(f("Killed", format!("{} ({})", s.role, s.unit_type), true));
            o.push(f("Site status", if s.site_destroyed { "**DESTROYED**".into() } else { "radars still alive".to_string() }, true));
            o.push(f(
                "Shot",
                format!(
                    "{}{g}\n{}\nradar {}",
                    s.weapon,
                    s.launch_range_m.map(|r| format!("{:.1} nm", r / 1852.)).unwrap_or_else(|| "range not recorded".into()),
                    if s.site_was_emitting { "**UP** (emitting)" } else { "dark" }
                ),
                true,
            ));
            o.push(f(
                "Threat",
                if s.trainer_deaths > 0 {
                    format!("{} SAM{} at you\n**{} would have killed you**", s.shots_at_you, if s.shots_at_you == 1 { "" } else { "s" }, s.trainer_deaths)
                } else {
                    format!("{} SAM{} at you", s.shots_at_you, if s.shots_at_you == 1 { "" } else { "s" })
                },
                true,
            ));
            o.push(f("Network", s.network.clone(), true));
        }
        RangeResult::HotZone(h) => {
            o.push(f("Zone", h.zone.clone(), true));
            o.push(f("Outcome", format!("**{}**", h.outcome.label()), true));
            o.push(f("Time in zone", mmss(h.time_in_zone_s), true));
            o.push(f("Air kills", h.air_kills.to_string(), true));
            o.push(f("Ground kills", h.ground_kills.to_string(), true));
            o.push(f("Shots fired", h.shots_fired.to_string(), true));
            o.push(f("Missiles defeated", h.missiles_defeated.to_string(), true));
            o.push(f(
                "Trainer deaths",
                if h.trainer_deaths > 0 { format!("**{}**", h.trainer_deaths) } else { "0".into() },
                true,
            ));
            if !h.kills.is_empty() {
                o.push(f("Kills", h.kills.join(", "), false));
            }
        }
        RangeResult::LowLevel(l) => {
            let when = if l.tot_error_s > 0.5 {
                " late"
            } else if l.tot_error_s < -0.5 {
                " early"
            } else {
                ""
            };
            o.push(f("Route", l.route.clone(), true));
            o.push(f("Quality", format!("**{}**", l.quality.label()), true));
            o.push(f("Gates", format!("**{}/{}**", l.gates_hit, l.gates_total), true));
            o.push(f(
                "Timing",
                format!("{} (plan {})\non target {:+.0} s{when}\n{:.0} kts average", mmss(l.time_s), mmss(l.planned_s), l.tot_error_s, l.avg_speed_kts),
                true,
            ));
            o.push(f(
                "Height AGL",
                format!(
                    "{:.0} / {:.0} / {:.0} ft avg/min/max\n{:.0}% under {:.0} ft{}",
                    l.avg_agl_ft,
                    l.min_agl_ft,
                    l.max_agl_ft,
                    l.pct_below_ceiling,
                    l.max_allowed_agl_ft,
                    if l.below_floor_s > 0. { format!("\n**{:.0} s under the floor**", l.below_floor_s) } else { String::new() }
                ),
                true,
            ));
            if !l.calls.is_empty() {
                o.push(f("Calls", l.calls.iter().map(|c| format!("• {}", cap(c))).collect::<Vec<_>>().join("\n"), false));
            }
        }
        RangeResult::FieldLanding(fl) => field_landing(fl, &mut o),
        RangeResult::Csar(c) => {
            o.push(f("Area", c.area.clone(), true));
            o.push(f("Outcome", format!("**{}**", c.outcome.label()), true));
            o.push(f("Quality", format!("**{}**", c.quality.label()), true));
            o.push(f("Threat", if c.hostile { "hostile (troops hunting him)" } else { "permissive" }, true));
            o.push(f("MAYDAY to pickup", c.time_to_pickup_s.map(mmss).unwrap_or_else(|| "-".into()), true));
            o.push(f("MAYDAY to home", c.time_total_s.map(mmss).unwrap_or_else(|| "-".into()), true));
            if !c.pickup_method.is_empty() {
                o.push(f("Pickup", c.pickup_method.clone(), true));
            }
            if let Some(d) = &c.delivered_to {
                o.push(f("Delivered to", d.clone(), true));
            }
        }
    }
    o
}

/// A field landing in four fields of short lines, then every call.
fn field_landing(fl: &bfprotocols::range::FieldLandingResult, o: &mut Vec<(String, String, bool)>) {
    const FT: f64 = 3.28084;
    const NM: f64 = 1852.;
    let deg = |v: Option<f64>| v.map(|v| format!("{v:+.1}°")).unwrap_or_else(|| "-".into());
    o.push(f("Runway", format!("{} {}", fl.airfield, fl.runway), true));
    o.push(f("Outcome", fl.outcome.label(), true));
    o.push(f("Quality", format!("**{}**", fl.quality.label()), true));
    let cl = fl.centreline_m.abs();
    o.push(f(
        "Touchdown",
        format!(
            "{}\n{:.0} m {} of the aim point\n{} m {} of centre\n{:.0} fpm · {:.0} kts",
            from_threshold(fl.touchdown_from_threshold_m),
            fl.aim_error_m.abs(),
            if fl.aim_error_m >= 0. { "long" } else { "short" },
            if cl < 10. { format!("{cl:.1}") } else { format!("{cl:.0}") },
            if fl.centreline_m >= 0. { "right" } else { "left" },
            fl.touchdown_fpm,
            fl.touchdown_gs_kts
        ),
        true,
    ));
    let mut ap = vec![if fl.stable { "stable from 1 nm".to_string() } else { "**NOT stable** from 1 nm".to_string() }];
    for (what, a, b) in [("glideslope", fl.gs_error_1nm_deg, fl.gs_error_half_nm_deg), ("lineup", fl.lineup_1nm_deg, fl.lineup_half_nm_deg)] {
        if a.is_some() || b.is_some() {
            ap.push(format!("{what} {} at 1 nm, {} at ½", deg(a), deg(b)));
        }
    }
    o.push(f("Approach", ap.join("\n"), true));
    if let Some(p) = &fl.pattern {
        let mut l = vec![match (p.kind.as_str(), p.side.as_deref()) {
            ("straight_in", _) => "**Straight-in**".to_string(),
            ("other", _) => "No standard pattern".to_string(),
            (k, Some(sd)) => format!("**{}**, {sd}-hand", cap(&k.replace('_', "-"))),
            (k, None) => format!("**{}**", cap(&k.replace('_', "-"))),
        }];
        if let Some(a) = p.initial_alt_ft {
            l.push(format!("initial {a:.0} ft"));
        }
        if let (Some(a), Some(m)) = (p.break_alt_ft, p.break_at_m) {
            l.push(format!("break {a:.0} ft, {m:+.0} m"));
        }
        if let Some(a) = p.downwind_alt_ft {
            l.push(match p.abeam_nm {
                Some(ab) => format!("downwind {a:.0} ft, {ab:.1} nm abeam"),
                None => format!("downwind {a:.0} ft"),
            });
        }
        if let Some(b) = p.base_at_m {
            l.push(format!("base {:.2} nm {}", b.abs() / NM, if b < 0. { "past" } else { "short" }));
        }
        if let Some(r) = p.rollout_nm {
            l.push(format!("wings level {r:.2} nm, {:.0} ft", p.rollout_alt_ft.unwrap_or(0.)));
        }
        let mut last = vec![];
        if let Some(g) = p.final_gs_kts {
            last.push(format!("final {g:.0} kts"));
        }
        if let Some(t) = p.pattern_time_s {
            last.push(mmss(t));
        }
        if !last.is_empty() {
            l.push(last.join(" · "));
        }
        o.push(f("Pattern", l.join("\n"), true));
    }
    match &fl.taxi {
        Some(t) => {
            let end = match t.end.as_str() {
                "parked" => "parked",
                "stopped_on_runway" => "**stopped on the runway**",
                "left" => "left the aircraft",
                "took_off" => "took off again",
                "timeout" => "lost after 10 min",
                e => e,
            };
            let top = if t.max_taxi_kts > bfprotocols::range::pattern_legs::TAXI_LIMIT_KTS {
                format!("**{:.0} kts**", t.max_taxi_kts)
            } else {
                format!("{:.0} kts", t.max_taxi_kts)
            };
            o.push(f(
                "Rollout and taxi",
                format!(
                    "rollout {:.0} m, {:.0} s\n{}\ntaxi {:.0} m in {}\ntop {top} · {} stop{} · {end}",
                    t.rollout_m,
                    t.rollout_s,
                    match (t.runway_clear_s, t.exit_at_m) {
                        (Some(c), Some(e)) => format!("clear in {c:.0} s, {e:.0} m down"),
                        (Some(c), None) => format!("clear in {c:.0} s"),
                        _ => "**never cleared the runway**".into(),
                    },
                    t.taxi_distance_m,
                    mmss(t.taxi_time_s),
                    t.stops,
                    if t.stops == 1 { "" } else { "s" },
                ),
                true,
            ));
        }
        None if fl.outcome == bfprotocols::range::FieldLandingOutcome::TouchAndGo => {
            o.push(f("Rollout and taxi", "touch-and-go", true));
        }
        None => (),
    }
    let calls: Vec<&String> = fl
        .calls
        .iter()
        .chain(fl.pattern.iter().flat_map(|p| p.calls.iter()))
        .chain(fl.taxi.iter().flat_map(|t| t.calls.iter()))
        .collect();
    if !calls.is_empty() {
        o.push(f("Calls", calls.iter().map(|c| format!("• {}", cap(c))).collect::<Vec<_>>().join("\n"), false));
    }
    let _ = FT;
}

fn cap(s: &str) -> String {
    let mut c = s.chars();
    c.next().map(|f| f.to_uppercase().collect::<String>() + c.as_str()).unwrap_or_default()
}

/// The ways into a result from Discord, as link buttons: (label, url).
pub(crate) fn links(ctx: &RangeCtx, r: &RangeRecord) -> Vec<(&'static str, String)> {
    let id = urlencoding::encode(&r.id);
    let page = format!("{}/result/{}", ctx.site_url, id);
    let mut o = vec![("Debrief", page.clone()), ("3D replay", format!("{page}?view=3d"))];
    if r.track.is_some() || matches!(r.result, RangeResult::Trap(_) | RangeResult::FieldLanding(_) | RangeResult::Bomb(_)) {
        o.push(("Web Tacview", format!("{page}?open=tacview")));
        o.push(("Tacview clip", format!("{}/api/range/result/{}/clip.acmi", ctx.api_url, id)));
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
        RangeResult::FieldLanding(fl) => format!(
            "**{} | {} | RWY {} | {}**",
            r.pilot.name,
            air,
            fl.runway,
            fl.pattern.as_ref().map(|p| p.kind.replace('_', "-").to_uppercase()).unwrap_or_else(|| fl.outcome.label().to_uppercase())
        ),
        _ => format!("**{} | {}**", r.pilot.name, air),
    };
    // the discipline's icon, as the bot's {icon:key}: it becomes the custom
    // emoji (titles do not render custom emoji, descriptions do)
    let description = format!("{{icon:{}}} {description}", icons::kind_icon(&r.result));
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
    let links: Vec<Value> = links(ctx, r).into_iter().map(|(label, url)| json!({ "label": label, "url": url })).collect();
    json!({
        "title": title,
        "description": description,
        "color": color,
        "fields": fields,
        "footer": footer,
        "image": format!("{}{}", ctx.api_url, RangeCtx::card_url(&r.id, "png")),
        "url": format!("{}/result/{}", ctx.site_url, id),
        "timestamp": r.ts.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
        "links": links,
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
        let ship = f.iter().find(|x| x.0 == "Ship").unwrap();
        assert_eq!(ship.1, "CVN-73 · Nimitz class (CVN)");
    }

    /// `CARD_OUT=<dir>`: every fixture's embed fields as JSON next to the card samples.
    #[test]
    fn write_embed_samples() {
        let Ok(dir) = std::env::var("CARD_OUT") else { return };
        std::fs::create_dir_all(&dir).unwrap();
        let mut recs = vec![super::super::cards::tests::trap()];
        recs.extend(super::super::cards::tests::all_cards());
        for (i, r) in recs.iter().enumerate() {
            let (title, color) = title_and_color(r);
            let fields: Vec<Value> = fields(r).into_iter().map(|(n, v, inl)| json!({"name": n, "value": v, "inline": inl})).collect();
            let v = json!({"title": title, "color": color, "fields": fields});
            std::fs::write(format!("{dir}/embed-{i:02}-{}.json", r.kind()), serde_json::to_string_pretty(&v).unwrap()).unwrap();
        }
    }

    #[test]
    fn field_landing_groups_fit_discord() {
        for r in super::super::cards::tests::all_cards() {
            let RangeResult::FieldLanding(fl) = &r.result else { continue };
            let o = fields(&r);
            assert!(o.len() <= 25);
            for (k, v, _) in &o {
                assert!(k.len() <= 256 && v.chars().count() <= 1024, "{k}");
            }
            let names: Vec<&str> = o.iter().map(|x| x.0.as_str()).collect();
            assert!(names.contains(&"Touchdown") && names.contains(&"Approach"), "{names:?}");
            if fl.pattern.is_some() {
                let p = &o.iter().find(|x| x.0 == "Pattern").unwrap().1;
                assert!(p.starts_with("**Overhead**, left-hand") && p.contains("downwind 1000 ft, 1.2 nm abeam"), "{p}");
                let t = &o.iter().find(|x| x.0 == "Rollout and taxi").unwrap().1;
                assert!(t.contains("clear in 27 s, 1580 m down") && t.contains("**38 kts**") && t.contains("parked"), "{t}");
                assert!(o.iter().any(|x| x.0 == "Calls" && x.1.contains("Fast taxi")));
            }
        }
    }
}
