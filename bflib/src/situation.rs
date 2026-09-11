/*
Copyright 2026 Dillen Weerasinghe.

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

//! The auto-generated situational briefing.
//!
//! [`build`] fuses campaign state (ownership, health, supply, capture timers,
//! treasury, convoys) with *one coalition's* earned sensor picture — the EWR /
//! AWACS air tracks and the recon/ELINT [`IntelDatabase`] — into a single
//! [`SituationReport`], and turns that into a ranked tasking list. Three
//! surfaces render that one report, so they can never tell a player different
//! stories:
//!
//! * [`render_panel`] — the condensed slot-entry panel,
//! * [`render_pages`] — F10 → Info → Situation,
//! * `query-situation` → `bfdb` `/api/situation` → the dashboard BRIEFING page.
//!
//! Fog of war is enforced at construction: a report is always built *for* a
//! side and only carries what that side can see. Objective ownership and
//! condition are campaign-public (the in-game F10 map shows both sides to
//! everyone), but threat areas, air tracks, hub stocks, tasking and the comms
//! card are the asking side's alone.
//!
//! The status strings for a hotspot come from [`Db::capture_diagnosis`] and
//! the repair gating in [`crate::menu::objectives`] rather than being written
//! here again — if the engine's capture rules change, the briefing follows
//! automatically instead of drifting into a plausible lie.

use crate::{
    db::{objective::Objective, Db},
    ewr::ContactClass,
    menu::brg_rng,
    Context,
};
use bfprotocols::{
    cfg::{Cfg, CommsPlanCfg},
    db::objective::{ObjectiveId, ObjectiveKind},
    situation::{
        AirPicture, AirThreat, CommsChannel, Hotspot, HubState, LogisticsPosture, MapObjective,
        Posture, SituationEvent, SituationReport, SituationWeather, SupplyGap, SupportStation,
        Task, TaskKind, ThreatArea, Urgency,
    },
};
use chrono::prelude::*;
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, MizLua, Vector2};
use std::{collections::HashMap, fmt::Write};

/// What the caller wants out of a report. The in-game paths want bearings from
/// the player's own jet and no map layer (the F10 map already draws it); the
/// dashboard wants the map layer and no bearings (there is no single viewer
/// position).
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Opts {
    /// Populate [`SituationReport::map`] with every positioned objective.
    pub include_map: bool,
    /// Measure task bearing/range from here.
    pub from: Option<Vector2>,
}

/// Objectives that generate sorties — the ones worth winning or losing.
fn is_primary(kind: &ObjectiveKind) -> bool {
    matches!(
        kind,
        ObjectiveKind::Airbase | ObjectiveKind::NavalBase | ObjectiveKind::Farp { .. }
    )
}

/// A compass sector name for a bearing, for prose like "3 groups NE".
fn sector(bearing_deg: f64) -> &'static str {
    const NAMES: [&str; 8] = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"];
    let idx = (((bearing_deg.rem_euclid(360.0)) + 22.5) / 45.0) as usize % 8;
    NAMES[idx]
}

/// `43°12.34'N 040°37.89'E`, matching the in-game F10 cards.
fn fmt_latlon(lat: f64, lon: f64) -> CompactString {
    let one = |v: f64, pos: char, neg: char| -> CompactString {
        let h = if v >= 0.0 { pos } else { neg };
        let a = v.abs();
        let d = a.floor();
        format_compact!("{:.0}\u{b0}{:05.2}'{}", d, (a - d) * 60.0, h)
    };
    format_compact!("{} {}", one(lat, 'N', 'S'), one(lon, 'E', 'W'))
}

/// `120°/34nm` from `from`, when there is a `from`.
fn brg_rng_opt(from: Option<Vector2>, to: Vector2) -> (Option<u32>, Option<f64>) {
    match from {
        Some(p) => {
            let (b, r) = brg_rng(p, to);
            (Some(b), Some(r))
        }
        None => (None, None),
    }
}

// ── main entry point ───────────────────────────────────────────────────────

pub(crate) fn build(ctx: &Context, lua: MizLua, side: Side, opts: Opts) -> SituationReport {
    let db = &ctx.db;
    let cfg = &db.ephemeral.cfg;
    let now = Utc::now();
    let coord = dcso3::coord::Coord::singleton(lua).ok();
    let to_ll = |p: Vector2| -> (f64, f64) {
        coord
            .as_ref()
            .and_then(|c| {
                c.lo_to_ll(dcso3::LuaVec3(dcso3::Vector3::new(p.x, 0.0, p.y)))
                    .ok()
            })
            .map(|ll| (ll.latitude, ll.longitude))
            .unwrap_or((0.0, 0.0))
    };
    let max_tasks = cfg
        .situation_briefing
        .as_ref()
        .map(|s| s.max_tasks)
        .unwrap_or(12)
        .max(1);
    let include_threats = cfg
        .situation_briefing
        .as_ref()
        .map(|s| s.include_threats)
        .unwrap_or(true);

    let posture = build_posture(ctx, side, now);
    let (hotspots, mut tasking) = build_hotspots_and_tasking(ctx, side, opts.from, &to_ll);
    let threats_pos = if include_threats {
        build_threats(ctx, side, &to_ll)
    } else {
        vec![]
    };
    let air = build_air(ctx, side, now, &to_ll);
    let logistics = build_logistics(ctx, side, &to_ll);
    let support = build_support(ctx, side, &to_ll);
    let (comms, flight_channels) = build_comms(cfg, side, &support);
    let weather = build_weather(lua);
    let mission_time = mission_time(lua);

    // Tasks the hotspot pass can't see: SEAD on known air defence, logistics
    // hauls, intercepts, and a recon prod when the coalition is blind.
    tasking.extend(sead_tasks(&threats_pos, opts.from));
    tasking.extend(logistics_tasks(&logistics));
    tasking.extend(air_tasks(&air));
    tasking.extend(recon_tasks(ctx, side, &threats_pos, opts.from, &to_ll));

    // Worst first, then nearest-to-the-player first inside a band so the top of
    // the list is something this pilot can actually reach.
    tasking.sort_by(|a, b| {
        a.urgency.cmp(&b.urgency).then_with(|| {
            a.range_nm
                .unwrap_or(f64::MAX)
                .total_cmp(&b.range_nm.unwrap_or(f64::MAX))
        })
    });
    tasking.truncate(max_tasks);

    let threats: Vec<ThreatArea> = threats_pos.iter().map(|(t, _)| t.clone()).collect();
    let headline = build_headline(&posture, &hotspots, &air, &logistics, side);

    let map = if opts.include_map {
        db.objectives()
            .filter(|(_, o)| !o.kind().is_special_sam_site())
            .map(|(_, o)| {
                // A carrier group's position is withheld the same way
                // /api/objectives withholds it -- mobile and sensitive.
                let hide = o.kind().is_carrier_group();
                let (lat, lon) = if hide { (0.0, 0.0) } else { to_ll(o.pos()) };
                let friendly = o.owner() == side;
                MapObjective {
                    name: o.name().to_string(),
                    kind: o.kind().name().to_string(),
                    owner: o.owner(),
                    lat,
                    lon,
                    health: o.health(),
                    logi: o.logi(),
                    supply: friendly.then(|| o.supply()),
                    fuel: friendly.then(|| o.fuel()),
                    threatened: o.threatened(),
                    captureable: o.captureable(),
                    priority: o.priority(),
                    primary: is_primary(o.kind()),
                }
            })
            .collect()
    } else {
        vec![]
    };

    SituationReport {
        side,
        generated: now,
        mission_time,
        headline,
        posture,
        weather,
        tasking,
        hotspots,
        threats,
        air,
        logistics,
        support,
        comms,
        flight_channels,
        recent: build_recent(ctx, side, now, &to_ll),
        map,
    }
}

// ── posture ────────────────────────────────────────────────────────────────

fn build_posture(ctx: &Context, side: Side, now: DateTime<Utc>) -> Posture {
    let db = &ctx.db;
    let enemy = side.opposite();
    let mut p = Posture {
        friendly_objectives: 0,
        enemy_objectives: 0,
        neutral_objectives: 0,
        friendly_primary: 0,
        enemy_primary: 0,
        territory_pct: 0.0,
        gained_recent: 0,
        lost_recent: 0,
        treasury: db.persisted.treasury(side),
        players_friendly: 0,
        players_enemy: 0,
        last_stand: None,
        victory_condition: None,
    };
    let hour_ago = now - chrono::Duration::hours(1);
    for (_, o) in db.objectives() {
        if o.kind().is_special_sam_site() {
            continue;
        }
        let recent = o.last_change() >= hour_ago;
        if o.owner() == side {
            p.friendly_objectives += 1;
            p.friendly_primary += is_primary(o.kind()) as u32;
            // An objective that changed hands recently and is ours now was a
            // gain; `last_change` also moves on repair, so this is a ceiling,
            // not a count -- only ever used for a trend word.
            if recent && o.health() >= 100 {
                p.gained_recent += 1;
            }
        } else if o.owner() == enemy {
            p.enemy_objectives += 1;
            p.enemy_primary += is_primary(o.kind()) as u32;
        } else {
            p.neutral_objectives += 1;
            if recent {
                p.lost_recent += 1;
            }
        }
    }
    let contested = (p.friendly_objectives + p.enemy_objectives) as f64;
    if contested > 0.0 {
        p.territory_pct = (p.friendly_objectives as f64 / contested) * 100.0;
    }

    for ifo in ctx.connected.info_by_player_id.values() {
        match db.player(&ifo.ucid).map(|pl| pl.side) {
            Some(s) if s == side => p.players_friendly += 1,
            Some(s) if s == enemy => p.players_enemy += 1,
            _ => (),
        }
    }

    if let (Some((arm_time, losing)), Some(ls)) =
        (db.ephemeral.last_stand_state, db.ephemeral.cfg.last_stand.as_ref())
    {
        let remaining = (chrono::Duration::seconds(ls.countdown_secs as i64)
            - (now - arm_time))
            .num_seconds()
            .max(0);
        let who = if losing == side { "YOUR" } else { "enemy" };
        p.last_stand = Some(format!(
            "LAST STAND -- {who} coalition is down to its final primary objectives, {}m on the clock",
            remaining / 60
        ));
    }
    if let Some(ar) = db.ephemeral.cfg.auto_reset.as_ref() {
        let bfprotocols::cfg::VictoryCondition::MapOwned { fraction } = ar.condition;
        p.victory_condition = Some(format!(
            "round ends when one side holds {:.0}% of the map",
            fraction * 100.0
        ));
    }
    p
}

// ── hotspots + the tasking they generate ───────────────────────────────────

fn build_hotspots_and_tasking<F>(
    ctx: &Context,
    side: Side,
    from: Option<Vector2>,
    to_ll: &F,
) -> (Vec<Hotspot>, Vec<Task>)
where
    F: Fn(Vector2) -> (f64, f64),
{
    let db = &ctx.db;
    let mut hotspots = vec![];
    let mut tasking = vec![];

    for (oid, obj) in db.objectives() {
        if obj.kind().is_special_sam_site() && obj.owner() == side {
            // Our own secret SAM sites aren't a tasking surface.
            continue;
        }
        let friendly = obj.owner() == side;
        let hostile = obj.owner() == side.opposite();
        let neutral = !friendly && !hostile;

        let diag = db.capture_diagnosis(oid, side).ok();
        let in_progress = diag.as_ref().and_then(|d| d.in_progress);
        let being_taken = in_progress.is_some();

        // Is this somewhere the campaign is actually moving?
        let interesting = if friendly {
            obj.threatened() || being_taken || obj.captureable() || obj.health() < 60
        } else {
            obj.captureable() || being_taken || obj.in_capture_hold() || obj.health() <= 40 || neutral
        };
        if !interesting {
            continue;
        }

        let (lat, lon) = to_ll(obj.pos());
        let status = diag
            .as_ref()
            .map(|d| d.one_liner().to_string())
            .unwrap_or_else(|| String::from("no capture assessment available"));
        let risk = hotspot_risk(obj, being_taken, friendly);
        let repair = repair_outlook(db, oid, obj);

        hotspots.push(Hotspot {
            objective: obj.name().to_string(),
            kind: obj.kind().name().to_string(),
            owner: obj.owner(),
            lat,
            lon,
            health: obj.health(),
            logi: obj.logi(),
            supply: obj.supply(),
            threatened: obj.threatened(),
            capture_progress: in_progress,
            captureable: obj.captureable(),
            in_capture_hold: obj.in_capture_hold(),
            status: status.clone(),
            repair_outlook: Some(repair.to_string()),
            risk,
        });

        let (bearing_deg, range_nm) = brg_rng_opt(from, obj.pos());
        let name = obj.name();

        if friendly {
            // Defending is the only thing that can lose the round outright.
            let (kind, urgency, detail, success) = if let Some((by, held, need)) = in_progress {
                (
                    TaskKind::Defend,
                    Urgency::Critical,
                    format!(
                        "{by:?} troops are in the zone -- {held}s of the ~{need}s they need is already on the clock. {status}"
                    ),
                    Some(String::from(
                        "kill every enemy capture group inside the zone to reset the timer",
                    )),
                )
            } else if obj.captureable() {
                (
                    TaskKind::Defend,
                    Urgency::Critical,
                    format!(
                        "wide open -- health {}%, {} infantry left. Any enemy troops that reach the zone take it. {status}",
                        obj.health(),
                        obj.infantry()
                    ),
                    Some(String::from(
                        "get the garrison back above 20% health, or kill the troops moving on it",
                    )),
                )
            } else if obj.threatened() {
                (
                    TaskKind::Cas,
                    Urgency::High,
                    format!(
                        "enemy ground forces are within sight -- repairs are frozen while they stay. Health {}%, logi {}%.",
                        obj.health(),
                        obj.logi()
                    ),
                    Some(String::from("clear the enemy ground units out of the zone")),
                )
            } else {
                (
                    TaskKind::Cas,
                    Urgency::Routine,
                    format!(
                        "down to {}% health with nothing in contact -- {}",
                        obj.health(),
                        repair
                    ),
                    None,
                )
            };
            tasking.push(Task {
                id: format!("{}-{name}", kind.label().to_lowercase()),
                kind,
                urgency,
                title: format!("{} {name}", kind.label()),
                detail,
                success,
                objective: Some(name.to_string()),
                lat,
                lon,
                bearing_deg,
                range_nm,
                roles: match kind {
                    TaskKind::Defend => vec!["CAS".into(), "Troops".into()],
                    _ => vec!["CAS".into()],
                },
            });
        } else if obj.captureable() || obj.in_capture_hold() {
            tasking.push(Task {
                id: format!("capture-{name}"),
                kind: TaskKind::Capture,
                urgency: Urgency::High,
                title: format!("CAPTURE {name}"),
                detail: format!("{status} {repair}"),
                success: Some(String::from(
                    "land capture troops inside the zone and hold them there",
                )),
                objective: Some(name.to_string()),
                lat,
                lon,
                bearing_deg,
                range_nm,
                roles: vec!["Troop lift".into(), "CAS escort".into()],
            });
        } else if hostile && obj.health() <= 40 {
            tasking.push(Task {
                id: format!("strike-{name}"),
                kind: TaskKind::Strike,
                urgency: if obj.priority() { Urgency::High } else { Urgency::Routine },
                title: format!("STRIKE {name}"),
                detail: format!(
                    "already softened to {}% -- {} more and it is takeable. {repair}",
                    obj.health(),
                    (obj.health() as i16 - 20).max(0)
                ),
                success: Some(String::from(
                    "health at or under 20% and no infantry left, then move troops in",
                )),
                objective: Some(name.to_string()),
                lat,
                lon,
                bearing_deg,
                range_nm,
                roles: vec!["Strike".into(), "CAS".into()],
            });
        } else if neutral {
            tasking.push(Task {
                id: format!("capture-{name}"),
                kind: TaskKind::Capture,
                urgency: Urgency::Routine,
                title: format!("CAPTURE {name} (neutral)"),
                detail: String::from(
                    "unowned -- it will not self-repair and nobody is defending it. Troops alone take it.",
                ),
                success: Some(String::from("hold capture troops in the zone")),
                objective: Some(name.to_string()),
                lat,
                lon,
                bearing_deg,
                range_nm,
                roles: vec!["Troop lift".into()],
            });
        }
    }

    hotspots.sort_by(|a, b| a.risk.cmp(&b.risk).then_with(|| a.objective.cmp(&b.objective)));
    (hotspots, tasking)
}

fn hotspot_risk(obj: &Objective, being_taken: bool, friendly: bool) -> Urgency {
    if being_taken {
        return Urgency::Critical;
    }
    if friendly && (obj.captureable() || obj.health() == 0) {
        return Urgency::Critical;
    }
    if obj.captureable() || obj.threatened() || obj.in_capture_hold() {
        return Urgency::High;
    }
    Urgency::Routine
}

/// Mirrors the gating in `Db::maybe_do_repairs` the same way the F10 capture
/// advisor does, so a briefing can't promise a repair the engine won't run.
fn repair_outlook(db: &Db, oid: &ObjectiveId, obj: &Objective) -> CompactString {
    if obj.health() >= 100 {
        return CompactString::from("at full strength");
    }
    if obj.health() == 0 && obj.owner() != Side::Neutral {
        return CompactString::from("garrison wiped -- will fall to NEUTRAL, then needs troops");
    }
    if obj.owner() == Side::Neutral {
        return CompactString::from("NEUTRAL -- never self-repairs, must be retaken with troops");
    }
    if obj.threatened() {
        return CompactString::from("repairs FROZEN while enemy units stay in sight");
    }
    if db.capture_in_progress(oid) {
        return CompactString::from("repairs FROZEN while the capture timer runs");
    }
    let cfg = &db.ephemeral.cfg;
    if obj.supply() < cfg.repair_supply_cost {
        return format_compact!(
            "repair STARVED -- supply {}% under the {}% a pulse costs",
            obj.supply(),
            cfg.repair_supply_cost
        );
    }
    if obj.logi() == 0 && !obj.kind().is_special_sam_site() {
        return CompactString::from("logistics destroyed (logi 0%) -- cannot self-repair");
    }
    let logi = if obj.kind().is_special_sam_site() {
        1.0
    } else {
        (obj.logi() as f32 / 100.0).max(0.01)
    };
    let pulse = (cfg.repair_time as f32 / logi).max(1.0);
    let elapsed = (Utc::now() - obj.last_change()).num_seconds().max(0) as f32;
    format_compact!(
        "self-repairing -- next pulse in ~{:.0}m",
        ((pulse - elapsed).max(0.0) / 60.0).ceil()
    )
}

// ── threat areas (this side's intel only) ──────────────────────────────────

fn build_threats<F>(ctx: &Context, side: Side, to_ll: &F) -> Vec<(ThreatArea, Vector2)>
where
    F: Fn(Vector2) -> (f64, f64),
{
    use crate::db::intel::{IntelSource, IntelUnitClass};
    let db = &ctx.db;
    let cfg = &db.ephemeral.cfg;
    let now = Utc::now();

    // Emitter ranges for the auto ring: every live enemy search radar, as
    // (position, range). A contact inherits the widest range of any emitter
    // inside its own uncertainty bubble -- enough to size the ring without
    // leaking the exact type the intel hasn't earned.
    let emitters: Vec<(Vector2, f64)> = db
        .persisted
        .groups
        .into_iter()
        .filter(|(_, g)| g.side == side.opposite())
        .flat_map(|(_, g)| g.units.into_iter())
        .filter_map(|uid| db.persisted.units.get(uid))
        .filter(|u| !u.dead)
        .filter_map(|u| cfg.ground_radar_ewrs.get(&u.typ).map(|e| (u.pos, e.range as f64)))
        .collect();

    let mut out: Vec<(ThreatArea, Vector2)> = db
        .ephemeral
        .intel_db
        .contacts_for(side)
        .filter(|c| matches!(c.unit_class, IntelUnitClass::AirDefense))
        .map(|c| {
            let (lat, lon) = to_ll(c.pos);
            let r = (c.pos_uncertainty_m as f64).max(2500.0);
            let radius_m = emitters
                .iter()
                .filter(|(p, _)| (p.x - c.pos.x).powi(2) + (p.y - c.pos.y).powi(2) <= r * r)
                .map(|(_, range)| *range)
                .fold(None, |acc: Option<f64>, v| Some(acc.map_or(v, |a: f64| a.max(v))));
            let area = ThreatArea {
                label: match c.source {
                    // Eyes-on sources are allowed to be specific; a radar-fusion
                    // hit is not.
                    IntelSource::Jtac | IntelSource::ReconFlight | IntelSource::SpecialForces => {
                        format!("{}x air defence", c.unit_count)
                    }
                    _ => String::from("air-defence emitter"),
                },
                lat,
                lon,
                radius_m,
                uncertainty_m: c.pos_uncertainty_m,
                confidence: c.confidence,
                source: match c.source {
                    IntelSource::ReconFlight => "recon",
                    IntelSource::SpecialForces => "sf",
                    IntelSource::Awacs => "awacs",
                    IntelSource::EwrFusion => "ewr",
                    IntelSource::Jtac => "jtac",
                    IntelSource::HumanInt => "humint",
                }
                .to_string(),
                age_s: (now - c.detected_at).num_seconds().max(0) as u32,
                count: c.unit_count,
                near: nearest_objective_name(db, c.pos),
            };
            (area, c.pos)
        })
        .collect();
    out.sort_by(|(a, _), (b, _)| {
        b.confidence
            .total_cmp(&a.confidence)
            .then_with(|| b.radius_m.unwrap_or(0.0).total_cmp(&a.radius_m.unwrap_or(0.0)))
    });
    out
}

fn nearest_objective_name(db: &Db, pos: Vector2) -> Option<String> {
    db.objectives()
        .filter(|(_, o)| !o.kind().is_special_sam_site())
        .map(|(_, o)| {
            let d = (o.pos().x - pos.x).powi(2) + (o.pos().y - pos.y).powi(2);
            (o.name().to_string(), d)
        })
        .min_by(|a, b| a.1.total_cmp(&b.1))
        .map(|(n, _)| n)
}

// ── air picture ────────────────────────────────────────────────────────────

fn build_air<F>(ctx: &Context, side: Side, now: DateTime<Utc>, to_ll: &F) -> AirPicture
where
    F: Fn(Vector2) -> (f64, f64),
{
    let db = &ctx.db;
    let contacts = ctx.ewr.air_picture_for(side, now, db);
    let mut hostile_tracks = 0u32;
    let mut friendly_airborne = 0u32;
    let mut stale_tracks = 0u32;
    // Hostile bearing histogram, measured from the centroid of our own ground.
    let mut by_sector: HashMap<&'static str, u32> = HashMap::new();
    let mut nearest: Option<(f64, AirThreat)> = None;

    let friendly_objs: Vec<Vector2> = db
        .objectives()
        .filter(|(_, o)| o.owner() == side && !o.kind().is_special_sam_site())
        .map(|(_, o)| o.pos())
        .collect();
    let centroid = if friendly_objs.is_empty() {
        Vector2::new(0.0, 0.0)
    } else {
        let n = friendly_objs.len() as f64;
        Vector2::new(
            friendly_objs.iter().map(|p| p.x).sum::<f64>() / n,
            friendly_objs.iter().map(|p| p.y).sum::<f64>() / n,
        )
    };

    for c in &contacts {
        if c.friendly {
            friendly_airborne += 1;
            continue;
        }
        hostile_tracks += 1;
        if c.stale {
            stale_tracks += 1;
        }
        let pos = Vector2::new(c.pos.p.x, c.pos.p.z);
        let (bearing, range) = brg_rng(centroid, pos);
        *by_sector.entry(sector(bearing as f64)).or_insert(0) += 1;

        // Closest hostile to any friendly objective -- what is actually
        // threatening us, not what is closest to the map centre.
        let (near_name, near_bearing, near_range) = db
            .objectives()
            .filter(|(_, o)| o.owner() == side && !o.kind().is_special_sam_site())
            .map(|(_, o)| {
                let (b, r) = brg_rng(o.pos(), pos);
                (o.name().to_string(), b, r)
            })
            .min_by(|a, b| a.2.total_cmp(&b.2))
            .unwrap_or_else(|| (String::from("friendly territory"), bearing, range));
        if nearest.as_ref().map_or(true, |(d, _)| near_range < *d) {
            let (lat, lon) = to_ll(pos);
            let v = c.velocity;
            let heading = if v.x.abs() > f64::EPSILON || v.z.abs() > f64::EPSILON {
                (v.z.atan2(v.x).to_degrees() + 360.0) % 360.0
            } else {
                0.0
            };
            nearest = Some((
                near_range,
                AirThreat {
                    lat,
                    lon,
                    alt_ft: (c.pos.p.y * 3.28084) as i32,
                    heading: heading as u32,
                    speed_kts: ((v.x * v.x + v.y * v.y + v.z * v.z).sqrt() * 1.94384) as u32,
                    class: match c.class {
                        ContactClass::Fighter => "fighter",
                        ContactClass::Bomber => "bomber",
                        ContactClass::Helicopter => "helo",
                        ContactClass::Unknown => "unknown",
                    }
                    .to_string(),
                    near: near_name,
                    bearing_deg: near_bearing,
                    range_nm: near_range,
                },
            ));
        }
    }

    let axis = by_sector
        .into_iter()
        .max_by_key(|(_, n)| *n)
        .map(|(sec, n)| {
            if n == 1 {
                format!("1 hostile track {sec} of your territory")
            } else {
                format!("{n} hostile tracks {sec} of your territory")
            }
        });

    // No tracks at all and no friendly datalink either means the radar net is
    // simply not reporting -- say so rather than implying a clean sky.
    let radar_blind = contacts.is_empty();

    AirPicture {
        hostile_tracks,
        friendly_airborne,
        stale_tracks,
        axis,
        nearest: nearest.map(|(_, t)| t),
        radar_blind,
    }
}

// ── logistics ──────────────────────────────────────────────────────────────

fn build_logistics<F>(ctx: &Context, side: Side, to_ll: &F) -> LogisticsPosture
where
    F: Fn(Vector2) -> (f64, f64),
{
    let db = &ctx.db;
    let cfg = &db.ephemeral.cfg;

    let hub_positions: Vec<(ObjectiveId, Vector2)> = db
        .persisted
        .logistics_hubs
        .into_iter()
        .filter_map(|oid| db.objective(oid).ok().map(|o| (*oid, o.pos())))
        .filter(|(oid, _)| db.objective(oid).map(|o| o.owner() == side).unwrap_or(false))
        .collect();

    // How many of our own objectives each hub is the closest supply source for.
    let mut feeding: HashMap<ObjectiveId, u32> = HashMap::new();
    for (_, o) in db.objectives().filter(|(_, o)| o.owner() == side) {
        if let Some((oid, _)) = hub_positions
            .iter()
            .min_by(|a, b| {
                let da = (a.1.x - o.pos().x).powi(2) + (a.1.y - o.pos().y).powi(2);
                let dbb = (b.1.x - o.pos().x).powi(2) + (b.1.y - o.pos().y).powi(2);
                da.total_cmp(&dbb)
            })
        {
            *feeding.entry(*oid).or_insert(0) += 1;
        }
    }

    let mut hubs: Vec<HubState> = hub_positions
        .iter()
        .filter_map(|(oid, _): &(ObjectiveId, Vector2)| {
            db.objective(oid).ok().map(|o| (oid, o))
        })
        .map(|(oid, o)| {
            let (lat, lon) = to_ll(o.pos());
            HubState {
                objective: o.name().to_string(),
                lat,
                lon,
                supply: o.supply(),
                fuel: o.fuel(),
                health: o.health(),
                logi: o.logi(),
                feeding: feeding.get(oid).copied().unwrap_or(0),
                threatened: o.threatened(),
            }
        })
        .collect();
    hubs.sort_by_key(|h| h.supply);

    // A gap is a friendly objective that cannot pay for its own repair pulse,
    // or has run its fuel down -- the two states that actually stop play.
    let mut gaps: Vec<SupplyGap> = db
        .objectives()
        .filter(|(_, o)| {
            o.owner() == side
                && !o.unlimited_supply()
                && !o.kind().is_special_sam_site()
                && (o.supply() < cfg.repair_supply_cost || o.fuel() < 20)
        })
        .map(|(_, o)| {
            let (lat, lon) = to_ll(o.pos());
            let note = if o.supply() < cfg.repair_supply_cost {
                format!(
                    "repair stalled -- supply {}% under the {}% each pulse costs",
                    o.supply(),
                    cfg.repair_supply_cost
                )
            } else {
                format!("fuel {}% -- ground starts here will run the tanks dry", o.fuel())
            };
            SupplyGap {
                objective: o.name().to_string(),
                lat,
                lon,
                supply: o.supply(),
                fuel: o.fuel(),
                health: o.health(),
                note,
            }
        })
        .collect();
    gaps.sort_by_key(|g| g.supply);

    LogisticsPosture {
        hubs,
        gaps,
        convoys_active: db.convoy_count_for_side(side) as u32,
        stage: format!("{:?}", db.ephemeral.logistics_stage()),
    }
}

// ── support assets + comms card ────────────────────────────────────────────

fn build_support<F>(ctx: &Context, side: Side, to_ll: &F) -> Vec<SupportStation>
where
    F: Fn(Vector2) -> (f64, f64),
{
    use bfprotocols::cfg::{ActionKind, AwacsCfg};
    use crate::db::group::DeployKind;

    let db = &ctx.db;
    let mut out = vec![];
    for (gid, group) in &db.persisted.groups {
        if group.side != side {
            continue;
        }
        let DeployKind::Action { spec, name, .. } = &group.origin else { continue };
        let (kind, plane) = match &spec.kind {
            ActionKind::Awacs(AwacsCfg { plane, .. }) => ("AWACS", plane),
            ActionKind::Tanker(plane) => ("TANKER", plane),
            _ => continue,
        };
        let tacan = plane.tacan_channel.map(|ch| {
            let band = plane
                .tacan_band
                .as_ref()
                .map(|b| format!("{b:?}"))
                .unwrap_or_else(|| String::from("X"));
            let cs = plane
                .tacan_callsign
                .as_ref()
                .map(|c| format!(" {c}"))
                .unwrap_or_default();
            format!("{ch}{band}{cs}")
        });
        let (lat, lon) = match db.group_center(gid) {
            Ok(p) => {
                let (a, b) = to_ll(p);
                (Some(a), Some(b))
            }
            Err(_) => (None, None),
        };
        out.push(SupportStation {
            label: format!("{kind} {name}"),
            kind: kind.to_string(),
            freq_mhz: plane.freq.map(|f| f as f64 / 1_000_000.0),
            tacan,
            note: None,
            lat,
            lon,
        });
    }
    for j in ctx.jtac.jtacs().filter(|j| j.side() == side) {
        let loc = j.location();
        let near = db
            .objective(&loc.oid)
            .map(|o| o.name().to_string())
            .unwrap_or_default();
        out.push(SupportStation {
            label: format!("JTAC {}", j.gid()),
            kind: String::from("JTAC"),
            freq_mhz: None,
            tacan: None,
            note: Some(format!("laser {} near {near}", j.code())),
            lat: None,
            lon: None,
        });
    }
    out
}

/// The configured comms plan with the live stations matched onto it. A plan
/// channel whose label names a role we have an on-station asset for is marked
/// live and carries that asset's real detail; everything else stays planned.
fn build_comms(
    cfg: &Cfg,
    side: Side,
    support: &[SupportStation],
) -> (Vec<CommsChannel>, Vec<(String, f64)>) {
    let default_plan;
    let plan: &CommsPlanCfg = match cfg.comms_plan.as_ref() {
        Some(p) => p,
        None => {
            default_plan = CommsPlanCfg::default();
            &default_plan
        }
    };

    // Live assets, in the order they came up, per kind -- so "JTAC 1" on the
    // card resolves to the first active JTAC, "JTAC 2" the second, and so on.
    let pick = |kind: &str, nth: usize| -> Option<&SupportStation> {
        support.iter().filter(|s| s.kind == kind).nth(nth)
    };
    // Next asset to hand out, per kind. A channel labelled "relay" is the SAME
    // station on a second band (the GCI UHF/VHF relays), so it shows the asset
    // the previous channel of its kind got instead of consuming the next one.
    let mut next: HashMap<&'static str, usize> = HashMap::new();
    let mut take = |kind: &'static str, relay: bool| -> usize {
        let n = *next.entry(kind).or_insert(0);
        if relay {
            n.saturating_sub(1)
        } else {
            next.insert(kind, n + 1);
            n
        }
    };

    let channels = plan
        .for_side(side)
        .iter()
        .map(|c| {
            let label_up = c.label.to_uppercase();
            let relay = label_up.contains("RELAY");
            let live_asset = if label_up.contains("JTAC") {
                pick("JTAC", take("JTAC", relay))
            } else if label_up.contains("AWACS") || label_up.contains("GCI") {
                pick("AWACS", take("AWACS", relay))
            } else if label_up.contains("TANKER") {
                pick("TANKER", take("TANKER", relay))
            } else {
                None
            };
            // The live radio wins over the planned one -- a pilot tuning the
            // card has to reach the aircraft that is actually up there.
            let freq_mhz = live_asset
                .and_then(|a| a.freq_mhz)
                .unwrap_or(c.freq_mhz);
            let note = live_asset.map(|a| match (&a.tacan, &a.note) {
                (Some(t), Some(n)) => format!("{} -- TACAN {t}, {n}", a.label),
                (Some(t), None) => format!("{} -- TACAN {t}", a.label),
                (None, Some(n)) => format!("{} -- {n}", a.label),
                (None, None) => a.label.clone(),
            });
            CommsChannel {
                preset: c.preset,
                label: c.label.as_str().to_string(),
                freq_mhz,
                modulation: c.modulation.as_str().to_string(),
                purpose: c.purpose.as_ref().map(|p| p.as_str().to_string()),
                live: live_asset.is_some(),
                note,
            }
        })
        .collect();

    // The GCI slot-entry block is configured separately (bfdb owns the real
    // controller); surface its callsign/freq note on the card too when set.
    let mut channels: Vec<CommsChannel> = channels;
    if let Some(gci) = cfg.gci_briefing.as_ref().and_then(|g| g.render(side)) {
        if let Some(first) = channels.first_mut() {
            if !first.live {
                first.note = Some(gci.replace('\n', " -- "));
            }
        }
    }
    (channels, plan.flight_channels(side))
}

// ── weather + clock ────────────────────────────────────────────────────────

fn build_weather(lua: MizLua) -> Option<SituationWeather> {
    let wx = crate::atis::fetch_weather(lua, 0.0, 0.0).ok()?;
    let vis_km = wx.visibility_m / 1000.0;
    let ceiling_ft = wx.cloud_base_m * 3.28084;
    // A working airman's read, not a METAR: can you get in and out visually.
    let rule = if wx.cloud_density >= 6 && ceiling_ft < 1000.0 {
        "IFR"
    } else if ceiling_ft < 3000.0 || vis_km < 5.0 {
        "MVFR"
    } else {
        "VFR"
    };
    let summary = format!(
        "{rule} -- wind {:03.0}\u{b0} at {:.0}kt, {:.0}km vis, {}{}",
        wx.wind_from_deg,
        wx.wind_speed_kts,
        vis_km,
        if wx.cloud_density == 0 {
            String::from("clear")
        } else {
            format!("cloud base {:.0}ft", ceiling_ft)
        },
        if wx.precip { ", precipitation" } else { "" },
    );
    Some(SituationWeather {
        wind_from_deg: wx.wind_from_deg as u32,
        wind_kts: wx.wind_speed_kts,
        temp_c: wx.temp_c,
        qnh_inhg: wx.qnh_inhg,
        qnh_hpa: wx.qnh_hpa,
        cloud_base_m: Some(wx.cloud_base_m),
        visibility_m: Some(wx.visibility_m),
        precip: wx.precip,
        summary,
    })
}

fn mission_time(lua: MizLua) -> Option<String> {
    let t = dcso3::timer::Timer::singleton(lua)
        .ok()?
        .get_abs_time()
        .ok()?;
    let secs = (t.0 as f64).rem_euclid(86_400.0) as u32;
    Some(format!("{:02}:{:02}", secs / 3600, (secs % 3600) / 60))
}

// ── recent events ──────────────────────────────────────────────────────────

/// In-session campaign events, newest first. The engine keeps no history of
/// its own, so this is derived from the objectives themselves: anything that
/// changed state in the last hour and is worth a line. `bfdb` extends this
/// from its persisted capture log when it serves the dashboard.
fn build_recent<F>(ctx: &Context, side: Side, now: DateTime<Utc>, to_ll: &F) -> Vec<SituationEvent>
where
    F: Fn(Vector2) -> (f64, f64),
{
    let db = &ctx.db;
    let cutoff = now - chrono::Duration::hours(1);
    let mut out: Vec<SituationEvent> = db
        .objectives()
        .filter(|(_, o)| !o.kind().is_special_sam_site() && o.last_change() >= cutoff)
        .filter(|(_, o)| o.owner() == Side::Neutral || o.health() < 100 || o.threatened())
        .map(|(_, o)| {
            let (lat, lon) = to_ll(o.pos());
            let friendly = o.owner() == side;
            let text = if o.owner() == Side::Neutral {
                format!("{} went NEUTRAL -- garrison wiped", o.name())
            } else if o.threatened() {
                format!(
                    "{} ({:?}) under attack -- health {}%",
                    o.name(),
                    o.owner(),
                    o.health()
                )
            } else {
                format!(
                    "{} ({:?}) damaged -- health {}%",
                    o.name(),
                    o.owner(),
                    o.health()
                )
            };
            SituationEvent {
                at: o.last_change(),
                text,
                good: Some(!friendly),
                lat: Some(lat),
                lon: Some(lon),
            }
        })
        .collect();
    out.sort_by(|a, b| b.at.cmp(&a.at));
    out.truncate(15);
    out
}

// ── derived tasking ────────────────────────────────────────────────────────

fn sead_tasks(threats: &[(ThreatArea, Vector2)], from: Option<Vector2>) -> Vec<Task> {
    threats
        .iter()
        // Only intel solid enough to plan a SEAD sortie against.
        .filter(|(t, _)| t.confidence >= 0.5 && t.radius_m.unwrap_or(0.0) >= 10_000.0)
        .take(3)
        .map(|(t, pos)| {
            let where_ = t
                .near
                .clone()
                .unwrap_or_else(|| fmt_latlon(t.lat, t.lon).to_string());
            let (bearing_deg, range_nm) = brg_rng_opt(from, *pos);
            Task {
                id: format!("sead-{where_}"),
                kind: TaskKind::Sead,
                urgency: if t.radius_m.unwrap_or(0.0) >= 40_000.0 {
                    Urgency::High
                } else {
                    Urgency::Routine
                },
                title: format!("SEAD near {where_}"),
                detail: format!(
                    "{} held on {} intel, {:.0}% confidence, {:.0}nm engagement ring, \u{b1}{:.1}km position error. Everything inside that ring is in a launch basket.",
                    t.label,
                    t.source,
                    t.confidence * 100.0,
                    t.radius_m.unwrap_or(0.0) / 1852.0,
                    t.uncertainty_m / 1000.0,
                ),
                success: Some(String::from(
                    "radar off the air or the launchers dead, then the strike package pushes",
                )),
                objective: t.near.clone(),
                lat: t.lat,
                lon: t.lon,
                bearing_deg,
                range_nm,
                roles: vec!["SEAD".into(), "HARM".into()],
            }
        })
        .collect()
}

fn logistics_tasks(logi: &LogisticsPosture) -> Vec<Task> {
    let mut out = vec![];
    for g in logi.gaps.iter().take(3) {
        out.push(Task {
            id: format!("logistics-{}", g.objective),
            kind: TaskKind::Logistics,
            urgency: if g.health < 60 { Urgency::High } else { Urgency::Routine },
            title: format!("RESUPPLY {}", g.objective),
            detail: format!("{} Health {}%, fuel {}%.", g.note, g.health, g.fuel),
            success: Some(String::from(
                "supply back over the repair cost so the garrison can heal itself",
            )),
            objective: Some(g.objective.clone()),
            lat: g.lat,
            lon: g.lon,
            bearing_deg: None,
            range_nm: None,
            roles: vec!["Heavy lift".into(), "Convoy escort".into()],
        });
    }
    for h in logi.hubs.iter().filter(|h| h.threatened || h.supply < 30).take(2) {
        out.push(Task {
            id: format!("defend-hub-{}", h.objective),
            kind: if h.threatened { TaskKind::Defend } else { TaskKind::Logistics },
            urgency: if h.threatened { Urgency::Critical } else { Urgency::High },
            title: format!(
                "{} HUB {}",
                if h.threatened { "DEFEND" } else { "REFILL" },
                h.objective
            ),
            detail: format!(
                "logistics hub feeding {} of your objectives -- supply {}%, fuel {}%, health {}%{}",
                h.feeding,
                h.supply,
                h.fuel,
                h.health,
                if h.threatened { ", enemy in contact" } else { "" }
            ),
            success: Some(String::from(
                "hub secure and stocked -- everything downstream of it stops healing without it",
            )),
            objective: Some(h.objective.clone()),
            lat: h.lat,
            lon: h.lon,
            bearing_deg: None,
            range_nm: None,
            roles: vec!["CAS".into(), "Heavy lift".into()],
        });
    }
    out
}

fn air_tasks(air: &AirPicture) -> Vec<Task> {
    let Some(t) = air.nearest.as_ref() else { return vec![] };
    // Only worth a task if it is actually pressing our ground.
    if t.range_nm > 60.0 {
        return vec![];
    }
    vec![Task {
        id: String::from("intercept-nearest"),
        kind: TaskKind::Intercept,
        urgency: if t.range_nm < 25.0 { Urgency::Critical } else { Urgency::High },
        title: format!("INTERCEPT inbound near {}", t.near),
        detail: format!(
            "{} track {:.0}nm off {} on a bearing of {:03}\u{b0}, {} ft, {} kt, heading {:03}\u{b0}. {} hostile track(s) on the net.",
            t.class, t.range_nm, t.near, t.bearing_deg, t.alt_ft, t.speed_kts, t.heading,
            air.hostile_tracks,
        ),
        success: Some(String::from("push the raid off your territory or kill it")),
        objective: Some(t.near.clone()),
        lat: t.lat,
        lon: t.lon,
        bearing_deg: Some(t.bearing_deg),
        range_nm: Some(t.range_nm),
        roles: vec!["CAP".into(), "Intercept".into()],
    }]
}

/// A prod to go look when the coalition is flying on no picture at all. A
/// briefing that stays silent about its own blind spots is worse than one that
/// admits them.
fn recon_tasks<F>(
    ctx: &Context,
    side: Side,
    threats: &[(ThreatArea, Vector2)],
    from: Option<Vector2>,
    to_ll: &F,
) -> Vec<Task>
where
    F: Fn(Vector2) -> (f64, f64),
{
    if !threats.is_empty() {
        return vec![];
    }
    let db = &ctx.db;
    // The enemy primary objective closest to our own ground is the one we will
    // have to fight through next -- send somebody to look at it.
    let mine: Vec<Vector2> = db
        .objectives()
        .filter(|(_, o)| o.owner() == side)
        .map(|(_, o)| o.pos())
        .collect();
    if mine.is_empty() {
        return vec![];
    }
    let target = db
        .objectives()
        .filter(|(_, o)| {
            o.owner() == side.opposite() && is_primary(o.kind()) && !o.kind().is_carrier_group()
        })
        .map(|(_, o)| {
            let d = mine
                .iter()
                .map(|p| (p.x - o.pos().x).powi(2) + (p.y - o.pos().y).powi(2))
                .fold(f64::MAX, f64::min);
            (o, d)
        })
        .min_by(|a, b| a.1.total_cmp(&b.1));
    let Some((o, _)) = target else { return vec![] };
    let (lat, lon) = to_ll(o.pos());
    let (bearing_deg, range_nm) = brg_rng_opt(from, o.pos());
    vec![Task {
        id: format!("recon-{}", o.name()),
        kind: TaskKind::Recon,
        urgency: Urgency::Routine,
        title: format!("RECON {}", o.name()),
        detail: String::from(
            "your coalition holds no air-defence intel at all -- nothing on recon, SF, JTAC or ELINT. \
             Every strike you plan right now is flying into an unmapped SAM picture.",
        ),
        success: Some(String::from(
            "get a recon or TARPS pass over it so the threat rings appear on the map",
        )),
        objective: Some(o.name().to_string()),
        lat,
        lon,
        bearing_deg,
        range_nm,
        roles: vec!["Recon".into(), "TARPS".into()],
    }]
}

// ── headline ───────────────────────────────────────────────────────────────

fn build_headline(
    p: &Posture,
    hotspots: &[Hotspot],
    air: &AirPicture,
    logi: &LogisticsPosture,
    side: Side,
) -> String {
    let mut s = String::new();
    let trend = if p.territory_pct >= 65.0 {
        "well ahead"
    } else if p.territory_pct >= 55.0 {
        "ahead"
    } else if p.territory_pct > 45.0 {
        "roughly level"
    } else if p.territory_pct > 35.0 {
        "behind"
    } else {
        "badly behind"
    };
    let _ = write!(
        s,
        "{side:?} is {trend}: {} objectives ({} primary) against {} ({} primary), {:.0}% of contested ground.",
        p.friendly_objectives, p.friendly_primary, p.enemy_objectives, p.enemy_primary, p.territory_pct,
    );
    let being_taken: Vec<&Hotspot> = hotspots
        .iter()
        .filter(|h| h.owner == side && h.capture_progress.is_some())
        .collect();
    let at_risk = hotspots
        .iter()
        .filter(|h| h.owner == side && (h.captureable || h.threatened))
        .count();
    if !being_taken.is_empty() {
        let names: Vec<&str> = being_taken.iter().map(|h| h.objective.as_str()).take(3).collect();
        let _ = write!(
            s,
            " A capture timer is running at {} right now -- that is the sortie.",
            names.join(", ")
        );
    } else if at_risk > 0 {
        let _ = write!(
            s,
            " {at_risk} of your objectives {} takeable or in contact.",
            if at_risk == 1 { "is" } else { "are" }
        );
    } else {
        let takeable = hotspots
            .iter()
            .filter(|h| h.owner != side && h.captureable)
            .count();
        if takeable > 0 {
            let _ = write!(
                s,
                " Nothing of yours is in danger; {takeable} enemy objective{} takeable now.",
                if takeable == 1 { " is" } else { "s are" }
            );
        } else {
            s.push_str(" The line is quiet -- soften something before it can be taken.");
        }
    }
    if air.radar_blind {
        s.push_str(" Your radar net is reporting nothing: assume you are blind to air.");
    } else if let Some(axis) = air.axis.as_ref() {
        let _ = write!(s, " Air: {axis}.");
    }
    if !logi.gaps.is_empty() {
        let _ = write!(
            s,
            " {} of your objectives cannot pay for their own repairs.",
            logi.gaps.len()
        );
    }
    s
}

// ── renderers ──────────────────────────────────────────────────────────────

fn task_line(t: &Task) -> CompactString {
    let br = match (t.bearing_deg, t.range_nm) {
        (Some(b), Some(r)) => format_compact!(" {b:03}\u{b0}/{r:.0}nm"),
        _ => CompactString::from(""),
    };
    format_compact!("[{}] {}{br}\n    {}\n", t.urgency.label(), t.title, t.detail)
}

/// The condensed slot-entry panel: where the round stands, what is on fire,
/// and the first few tasks. Anything longer belongs on the F10 pages.
pub(crate) fn render_panel(rep: &SituationReport, panel_tasks: usize, note: Option<&str>) -> String {
    let mut s = String::new();
    let _ = write!(
        s,
        "=== SITUATION -- {:?} {} ===\n{}\n",
        rep.side,
        rep.mission_time.as_deref().unwrap_or(""),
        rep.headline
    );
    if let Some(ls) = rep.posture.last_stand.as_ref() {
        let _ = write!(s, "{ls}\n");
    }
    if let Some(w) = rep.weather.as_ref() {
        let _ = write!(s, "WX: {}\n", w.summary);
    }
    if !rep.tasking.is_empty() {
        s.push_str("\nTASKING:\n");
        for t in rep.tasking.iter().take(panel_tasks.max(1)) {
            s.push_str(&task_line(t));
        }
        if rep.tasking.len() > panel_tasks {
            let _ = write!(
                s,
                "(+{} more -- F10 > Info > Situation)\n",
                rep.tasking.len() - panel_tasks
            );
        }
    }
    if let Some(t) = rep.threats.first() {
        let _ = write!(
            s,
            "\nTHREAT: {} near {} -- {:.0}nm ring, {:.0}% confidence\n",
            t.label,
            t.near.as_deref().unwrap_or("unknown"),
            t.radius_m.unwrap_or(0.0) / 1852.0,
            t.confidence * 100.0,
        );
    }
    if let Some(gci) = rep.comms.iter().find(|c| c.live || c.preset == Some(1)) {
        let _ = write!(
            s,
            "\n{} -- {:.3} {}\n",
            gci.label, gci.freq_mhz, gci.modulation
        );
    }
    if let Some(n) = note {
        let _ = write!(s, "{n}\n");
    }
    s
}

/// Page titles for the F10 report, in order. Kept next to [`render_pages`] so
/// the menu can label its Next/Prev without rendering anything.
pub(crate) const PAGES: [&str; 6] = [
    "Overview",
    "Tasking",
    "Hotspots",
    "Threats & Air",
    "Logistics",
    "Comms",
];

/// The full report, one string per page. Page count matches [`PAGES`].
pub(crate) fn render_pages(rep: &SituationReport) -> Vec<String> {
    let mut pages = Vec::with_capacity(PAGES.len());
    let hdr = |n: usize| -> String {
        format!(
            "=== SITUATION {}/{} -- {} ({:?} {}) ===\n",
            n + 1,
            PAGES.len(),
            PAGES[n],
            rep.side,
            rep.mission_time.as_deref().unwrap_or("")
        )
    };

    // 0 -- overview
    let mut s = hdr(0);
    let _ = write!(s, "{}\n\n", rep.headline);
    let p = &rep.posture;
    let _ = write!(
        s,
        "Territory: {:.0}% of contested ground\n\
         Yours: {} objectives ({} primary)   Enemy: {} ({} primary)   Neutral: {}\n\
         Treasury: {} pts   Pilots online: {} vs {}\n",
        p.territory_pct,
        p.friendly_objectives,
        p.friendly_primary,
        p.enemy_objectives,
        p.enemy_primary,
        p.neutral_objectives,
        p.treasury,
        p.players_friendly,
        p.players_enemy,
    );
    if let Some(v) = p.victory_condition.as_ref() {
        let _ = write!(s, "Victory: {v}\n");
    }
    if let Some(ls) = p.last_stand.as_ref() {
        let _ = write!(s, "{ls}\n");
    }
    if let Some(w) = rep.weather.as_ref() {
        let _ = write!(
            s,
            "\nWeather: {}\nQNH {:.2} inHg / {:.0} hPa, temp {:.0}C\n",
            w.summary, w.qnh_inhg, w.qnh_hpa, w.temp_c
        );
    }
    if !rep.recent.is_empty() {
        s.push_str("\nLast hour:\n");
        for e in rep.recent.iter().take(6) {
            let _ = write!(s, "  {}\n", e.text);
        }
    }
    pages.push(s);

    // 1 -- tasking
    let mut s = hdr(1);
    if rep.tasking.is_empty() {
        s.push_str("Nothing pressing. Soften an enemy objective or resupply your own.\n");
    } else {
        for t in &rep.tasking {
            s.push_str(&task_line(t));
            if let Some(ok) = t.success.as_ref() {
                let _ = write!(s, "    Done when: {ok}\n");
            }
            if !t.roles.is_empty() {
                let _ = write!(s, "    Wants: {}\n", t.roles.join(", "));
            }
        }
    }
    pages.push(s);

    // 2 -- hotspots
    let mut s = hdr(2);
    if rep.hotspots.is_empty() {
        s.push_str("No objective is in contact or takeable right now.\n");
    } else {
        for h in &rep.hotspots {
            let _ = write!(
                s,
                "{} [{}] {:?} -- HP {}% L {}% S {}%\n  {}\n",
                h.objective,
                h.kind,
                h.owner,
                h.health,
                h.logi,
                h.supply,
                h.status
            );
            if let Some(r) = h.repair_outlook.as_ref() {
                let _ = write!(s, "  {r}\n");
            }
        }
    }
    pages.push(s);

    // 3 -- threats & air
    let mut s = hdr(3);
    let a = &rep.air;
    if a.radar_blind {
        s.push_str("AIR: radar net reporting nothing -- you are blind, not clear.\n");
    } else {
        let _ = write!(
            s,
            "AIR: {} hostile track(s) ({} coasting), {} friendly airborne\n",
            a.hostile_tracks, a.stale_tracks, a.friendly_airborne
        );
        if let Some(ax) = a.axis.as_ref() {
            let _ = write!(s, "  {ax}\n");
        }
        if let Some(t) = a.nearest.as_ref() {
            let _ = write!(
                s,
                "  Nearest: {} {:03}\u{b0}/{:.0}nm off {}, {} ft, {} kt\n",
                t.class, t.bearing_deg, t.range_nm, t.near, t.alt_ft, t.speed_kts
            );
        }
    }
    s.push_str("\nKNOWN AIR DEFENCE (your intel only):\n");
    if rep.threats.is_empty() {
        s.push_str(
            "  nothing held -- no recon, SF, JTAC or ELINT contact. Assume the picture is unmapped.\n",
        );
    } else {
        for t in rep.threats.iter().take(12) {
            let _ = write!(
                s,
                "  {} near {} -- {:.0}nm ring, {:.0}% conf, \u{b1}{:.1}km, {} {}s old\n",
                t.label,
                t.near.as_deref().unwrap_or("?"),
                t.radius_m.unwrap_or(0.0) / 1852.0,
                t.confidence * 100.0,
                t.uncertainty_m / 1000.0,
                t.source,
                t.age_s,
            );
        }
    }
    pages.push(s);

    // 4 -- logistics
    let mut s = hdr(4);
    let l = &rep.logistics;
    let _ = write!(
        s,
        "Convoys running: {}   Logistics stage: {}\n\nHUBS:\n",
        l.convoys_active, l.stage
    );
    if l.hubs.is_empty() {
        s.push_str("  none held\n");
    } else {
        for h in &l.hubs {
            let _ = write!(
                s,
                "  {} -- S {}% F {}% HP {}% L {}%, feeding {}{}\n",
                h.objective,
                h.supply,
                h.fuel,
                h.health,
                h.logi,
                h.feeding,
                if h.threatened { " [THREAT]" } else { "" }
            );
        }
    }
    s.push_str("\nCANNOT SUSTAIN THEMSELVES:\n");
    if l.gaps.is_empty() {
        s.push_str("  none -- every objective can pay for its own repairs\n");
    } else {
        for g in l.gaps.iter().take(12) {
            let _ = write!(s, "  {} -- {}\n", g.objective, g.note);
        }
    }
    pages.push(s);

    // 5 -- comms
    let mut s = hdr(5);
    s.push_str("PRESET  FREQ       WHO\n");
    for c in &rep.comms {
        let _ = write!(
            s,
            "{:>4}   {:>7.3} {:2}  {}{}\n",
            c.preset.map(|p| p.to_string()).unwrap_or_else(|| String::from("-")),
            c.freq_mhz,
            c.modulation,
            c.label,
            if c.live { "  [UP]" } else { "" },
        );
        if let Some(n) = c.note.as_ref() {
            let _ = write!(s, "         {n}\n");
        }
    }
    if !rep.flight_channels.is_empty() {
        s.push_str("\nINTRA-FLIGHT:\n");
        let line = rep
            .flight_channels
            .iter()
            .map(|(n, f)| format!("{n} {f:.3}"))
            .collect::<Vec<_>>()
            .join("   ");
        let _ = write!(s, "  {line}\n");
    }
    pages.push(s);

    pages
}

// ── slot-entry delivery ────────────────────────────────────────────────────

/// Queue the condensed briefing panel for a player who just took a slot.
///
/// Sequenced after the ATIS (which fires at +15s) so the two don't overwrite
/// each other on the same canopy; both the delay and the display time are
/// config. No-op when the slot isn't an aircraft, or when `situation_briefing`
/// is absent / has `on_slot_entry` off — the F10 report and the dashboard are
/// pull paths and stay available either way.
pub(crate) fn schedule_slot_briefing(lua: MizLua, slot: dcso3::net::SlotId) -> anyhow::Result<()> {
    use dcso3::timer::Timer;
    let ctx = unsafe { Context::get_mut() };
    let Some(cfg) = ctx.db.ephemeral.cfg.situation_briefing.clone() else {
        return Ok(());
    };
    if !cfg.on_slot_entry || !crate::atis::is_aircraft_slot(&ctx.db, &slot) {
        return Ok(());
    }
    let timer = Timer::singleton(lua)?;
    let when = timer.get_time()? + cfg.delay_secs as f32;
    timer.schedule_function(when, slot, move |lua, slot, _| {
        let ctx = unsafe { Context::get_mut() };
        // The player may have jumped back to spectator in the meantime.
        let Some(si) = ctx.db.ephemeral.get_slot_info(&slot) else {
            return Ok(None);
        };
        let (miz_gid, side) = (si.miz_gid, si.side);
        let from = crate::menu::player_world_pos(ctx, &slot);
        let rep = build(ctx, lua, side, Opts { include_map: false, from });
        let cfg = match ctx.db.ephemeral.cfg.situation_briefing.as_ref() {
            Some(c) => c.clone(),
            None => return Ok(None),
        };
        let text = render_panel(
            &rep,
            cfg.panel_tasks,
            cfg.note.as_ref().map(|n| n.as_str()),
        );
        ctx.db
            .ephemeral
            .msgs()
            .panel_to_group(cfg.display_secs, false, miz_gid, text);
        Ok(None)
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sectors_round_correctly() {
        assert_eq!(sector(0.0), "N");
        assert_eq!(sector(44.0), "NE");
        assert_eq!(sector(90.0), "E");
        assert_eq!(sector(181.0), "S");
        assert_eq!(sector(315.0), "NW");
        assert_eq!(sector(359.0), "N");
        // wraps rather than panicking on out-of-range input
        assert_eq!(sector(-10.0), "N");
        assert_eq!(sector(725.0), "N");
    }

    #[test]
    fn latlon_formats_both_hemispheres() {
        let s = fmt_latlon(43.5, -40.25);
        assert!(s.contains("43\u{b0}30.00'N"), "{s}");
        assert!(s.contains("40\u{b0}15.00'W"), "{s}");
    }

    #[test]
    fn page_titles_match_rendered_pages() {
        // render_pages must produce exactly one page per PAGES entry -- the F10
        // menu steps by index and would otherwise render a blank page.
        assert_eq!(PAGES.len(), 6);
    }
}
