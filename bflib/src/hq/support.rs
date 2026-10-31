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

//! The side's AWACS and tankers once they are up, and fuel for its AI
//! flights.
//!
//! - `refresh_front`: the front line the F10 overlay draws, traced again
//!   when a base changes hands, for the stations (`Picture::support_station`).
//! - `manage`: one AWACS (and tanker) per station. A spare the HQ sent is
//!   sent home, or given its own station when the limits allow two; one
//!   being relieved goes home once its relief is on station; one whose
//!   station the front has moved up to is pulled back. Players' and
//!   commanders' flights are left where they put them.
//! - `fuel_watch`: an AI flight that can take fuel in the air and is running
//!   low is sent (DCS `Refueling` task) to a friendly tanker of its kind --
//!   boom or drogue -- that is on station within reach. An AWACS with no
//!   such tanker is marked for relief instead.

use super::{
    Handle, OpStatus, dist,
    picture::{self, Picture, Station, StationRules, SupportFlight},
};
use crate::{Context, db::group::DeployKind, spawnctx::SpawnCtx};
use bfprotocols::{
    cfg::{ActionKind, HqCfg},
    db::group::GroupId,
    hq::OpKind,
    range::RefuelMethod,
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{MizLua, Vector2, coalition::Side, controller::Task, group::Group};
use fxhash::FxHashSet;
use log::{debug, info, warn};

/// How often each side's AI flights have their fuel read.
const FUEL_CHECK_SECS: i64 = 60;
/// A flight sent to a tanker isn't sent again for this long.
const REFUEL_RETRY_SECS: i64 = 15 * 60;
/// An AWACS or tanker the HQ moved isn't moved again for this long.
const RESTATION_SECS: i64 = 10 * 60;
/// A station this far inside a threat ring is moved.
const RESTATION_INSIDE_M: f64 = 10_000.;
/// Spacing of the front line's points kept for the station geometry.
const FRONT_STEP_M: f64 = 5_000.;

// ---------------------------------------------------------------------------
// Front line
// ---------------------------------------------------------------------------

/// Trace the front (`bfprotocols::frontline`, the F10 overlay's white line)
/// when a base has changed hands since the last time.
pub(super) fn refresh_front(ctx: &mut Context) {
    use std::hash::{Hash, Hasher};
    let objs = crate::frontline::frontline_objectives(&ctx.db.persisted);
    let mut h = std::collections::hash_map::DefaultHasher::new();
    for (x, y, s) in objs.iter() {
        (x.to_bits(), y.to_bits(), s.to_bits()).hash(&mut h);
    }
    let hash = h.finish();
    if ctx.hq.front.as_ref().is_some_and(|(old, _)| *old == hash) {
        return;
    }
    let fl = bfprotocols::frontline::compute(
        &objs,
        &bfprotocols::frontline::Params::default(),
    );
    let mut segs = vec![];
    for line in fl.mid.iter() {
        let mut last: Option<Vector2> = None;
        let n = line.len();
        for (i, p) in line.iter().enumerate() {
            let p = Vector2::new(p[0], p[1]);
            match last {
                None => last = Some(p),
                Some(l) if dist(l, p) >= FRONT_STEP_M || i + 1 == n => {
                    segs.push((l, p));
                    last = Some(p);
                }
                Some(_) => (),
            }
        }
    }
    info!("hq: front line for AWACS and tanker stations: {} segment(s)", segs.len());
    ctx.hq.front = Some((hash, segs));
}

// ---------------------------------------------------------------------------
// Stations
// ---------------------------------------------------------------------------

fn label(kind: OpKind) -> &'static str {
    if kind == OpKind::Tanker { "tanker" } else { "AWACS" }
}

/// End `f`'s HQ operation, if it has one.
fn end_op(
    lua: MizLua,
    ctx: &mut Context,
    side: Side,
    gid: GroupId,
    status: OpStatus,
    detail: &str,
    now: DateTime<Utc>,
) {
    let i = ctx.hq.side(side).ops.iter().position(|o| {
        o.status == OpStatus::Active && matches!(o.handle, Handle::Group(g) if g == gid)
    });
    if let Some(i) = i {
        super::finish(lua, ctx, side, i, status, detail.into(), now);
    }
}

fn stand_down(
    lua: MizLua,
    ctx: &mut Context,
    side: Side,
    f: &SupportFlight,
    status: OpStatus,
    why: CompactString,
    now: DateTime<Utc>,
) {
    info!("hq: {side:?} sending {} {} ({}) home: {why}", label(f.kind), f.name, f.typ);
    crate::airlife::send_home(lua, ctx, f.gid, now);
    ctx.hq
        .side(side)
        .note(now, format_compact!("HQ: {} {} sent home: {why}", label(f.kind), f.name));
    end_op(lua, ctx, side, f.gid, status, &why, now);
}

fn restation(
    lua: MizLua,
    ctx: &mut Context,
    side: Side,
    f: &SupportFlight,
    st: &Station,
    why: CompactString,
    now: DateTime<Utc>,
) {
    let res =
        SpawnCtx::new(lua).and_then(|spctx| ctx.db.restation_air(&spctx, f.gid, st.pos));
    match res {
        Err(e) => warn!(
            "hq: {side:?} could not move {} {} to a new station: {e:?}",
            label(f.kind),
            f.name
        ),
        Ok(()) => {
            info!(
                "hq: {side:?} moving {} {} ({}) {:.0} km to a new station {:.0} km behind the front \
                 (clearance {:.0} km): {why}",
                label(f.kind),
                f.name,
                f.typ,
                dist(f.station, st.pos) / 1000.,
                st.front_m / 1000.,
                st.clearance / 1000.
            );
            let rt = ctx.hq.side(side);
            rt.restationed.insert(f.gid, now);
            rt.note(
                now,
                format_compact!(
                    "HQ: {} {} moved to a new station: {why}",
                    label(f.kind),
                    f.name
                ),
            );
            for o in rt.ops.iter_mut().filter(|o| o.status == OpStatus::Active) {
                if matches!(o.handle, Handle::Group(g) if g == f.gid) {
                    o.pos = st.pos;
                }
            }
        }
    }
}

/// One AWACS (and one tanker of each kind) per station; pull back the ones
/// the front has moved up to. Runs on each planning pass, before the
/// planner, which reads the same picture: what is sent home here isn't
/// replaced until the next pass.
pub(super) fn manage(
    lua: MizLua,
    ctx: &mut Context,
    cfg: &HqCfg,
    side: Side,
    pic: &Picture,
    now: DateTime<Utc>,
) {
    let s = &cfg.support;
    let lead = s.relief_secs;
    let near = ctx
        .hq
        .sides
        .get(&side)
        .and_then(|rt| rt.plan.as_ref())
        .and_then(|p| p.main_effort)
        .and_then(|id| pic.get(&id))
        .map(|o| o.pos)
        .or_else(|| pic.front_point());
    for kind in [OpKind::Awacs, OpKind::Tanker] {
        let rules = StationRules::of(cfg, kind);
        let limit =
            if kind == OpKind::Tanker { cfg.limits.tanker } else { cfg.limits.awacs };
        let mut flights: Vec<SupportFlight> = pic.support_of(kind).cloned().collect();
        flights.sort_by_key(|f| f.launched);
        let mut gone: Vec<GroupId> = vec![];
        for i in 0..flights.len() {
            for j in (i + 1)..flights.len() {
                let (a, b) = (&flights[i], &flights[j]);
                if gone.contains(&a.gid) || gone.contains(&b.gid) {
                    continue;
                }
                // A boom and a drogue tanker serve different jets.
                if kind == OpKind::Tanker
                    && tanker_method(&a.typ) != tanker_method(&b.typ)
                {
                    continue;
                }
                if dist(a.station, b.station) > s.same_station_m {
                    continue;
                }
                // One being relieved: it goes home once its relief is on
                // station. A player's runs out its time on its own.
                let (a_due, b_due) = (a.relief_due(now, lead), b.relief_due(now, lead));
                if a_due || b_due {
                    let (old, new) = if a_due { (a, b) } else { (b, a) };
                    if new.on_station() && !old.player {
                        let why = format_compact!("relieved on station by {}", new.name);
                        stand_down(lua, ctx, side, old, OpStatus::Succeeded, why, now);
                        gone.push(old.gid);
                    }
                    continue;
                }
                // Two on one station. The HQ's newer one is the spare; the
                // HQ's gives way to a player's; two players' are theirs.
                let (keep, spare) = match (a.player, b.player) {
                    (_, false) => (a, b),
                    (false, true) => (b, a),
                    (true, true) => {
                        if ctx.hq.side(side).shared_noted.insert((a.gid, b.gid)) {
                            info!(
                                "hq: {side:?} players' {} {} and {} share a station; the HQ leaves them be",
                                label(kind),
                                a.name,
                                b.name
                            );
                        }
                        continue;
                    }
                };
                let others: Vec<(Vector2, f64)> = flights
                    .iter()
                    .filter(|f| f.gid != spare.gid && !gone.contains(&f.gid))
                    .map(|f| (f.station, 2. * s.same_station_m))
                    .collect();
                let elsewhere = if limit >= 2 {
                    pic.support_station(&rules, near, &others)
                        .filter(|st| st.clearance >= 0.)
                } else {
                    None
                };
                match elsewhere {
                    Some(st) => {
                        let why = format_compact!(
                            "{} already holds its station; covering a second one",
                            keep.name
                        );
                        restation(lua, ctx, side, spare, &st, why, now);
                    }
                    None => {
                        let why =
                            format_compact!("{} already holds that station", keep.name);
                        stand_down(lua, ctx, side, spare, OpStatus::Cancelled, why, now);
                        gone.push(spare.gid);
                    }
                }
            }
        }
        // More AWACS up than the side keeps, wherever they orbit (an A-50 off
        // the coast and an E-3A inland are still two): the HQ's own go home,
        // newest first. Tankers serve boom or drogue jets, so two on
        // different stations may both be wanted; they are only thinned by
        // station above.
        if kind == OpKind::Awacs {
            let up: Vec<&SupportFlight> = flights
                .iter()
                .filter(|f| !gone.contains(&f.gid) && !f.relief_due(now, lead))
                .collect();
            let mut extra = up.len().saturating_sub(limit.max(1) as usize);
            let keep = up
                .iter()
                .find(|f| f.player)
                .or(up.first())
                .map(|f| f.name.clone())
                .unwrap_or_default();
            let spares: Vec<SupportFlight> =
                up.iter().rev().filter(|f| !f.player).map(|f| (*f).clone()).collect();
            for f in spares {
                if extra == 0 {
                    break;
                }
                if f.name == keep {
                    continue;
                }
                let why = format_compact!(
                    "the side keeps {limit} AWACS up and {keep} is on station"
                );
                stand_down(lua, ctx, side, &f, OpStatus::Cancelled, why, now);
                gone.push(f.gid);
                extra -= 1;
            }
        }
        // The HQ's own whose station the front (or a newly found SAM) has
        // come up to: back to a safe one, away from the others.
        for f in flights.iter().filter(|f| !f.player && !gone.contains(&f.gid)) {
            if f.relief_due(now, lead) {
                continue;
            }
            let here = pic.clearance(f.station, &rules);
            if here >= -RESTATION_INSIDE_M {
                continue;
            }
            let recent = ctx
                .hq
                .side(side)
                .restationed
                .get(&f.gid)
                .is_some_and(|t| (now - *t).num_seconds() < RESTATION_SECS);
            if recent {
                continue;
            }
            let others: Vec<(Vector2, f64)> = flights
                .iter()
                .filter(|o| o.gid != f.gid && !gone.contains(&o.gid))
                .map(|o| (o.station, s.same_station_m))
                .collect();
            let Some(st) = pic.support_station(&rules, near, &others) else { continue };
            if st.clearance <= here + RESTATION_INSIDE_M
                || dist(st.pos, f.station) < 20_000.
            {
                continue;
            }
            let why = format_compact!(
                "its station is {:.0} km inside a threat ring",
                -here / 1000.
            );
            restation(lua, ctx, side, f, &st, why, now);
        }
    }
    ctx.hq.side(side).restationed.retain(|_, t| (now - *t).num_seconds() < 3600);
}

// ---------------------------------------------------------------------------
// Air refuelling
// ---------------------------------------------------------------------------

/// How a tanker of DCS type `typ` passes fuel.
pub(crate) fn tanker_method(typ: &str) -> Option<RefuelMethod> {
    bfprotocols::range::cfg::tanker_type(typ).map(|t| t.method)
}

/// How an aircraft of DCS type `typ` takes fuel in the air: a receptacle
/// for the boom, or a probe for the drogue. None if it can't.
pub(crate) fn receiver_method(typ: &str) -> Option<RefuelMethod> {
    const BOOM: &[&str] = &[
        "E-3A",
        "F-15C",
        "F-15E",
        "F-15ESE",
        "F-16A",
        "F-16A MLU",
        "F-16C_50",
        "F-16C bl.50",
        "F-16C bl.52d",
        "A-10A",
        "A-10C",
        "A-10C_2",
        "F-117A",
        "B-1B",
        "B-52H",
        "C-17A",
        "F-4E",
        "F-4E-45MC",
    ];
    const DROGUE: &[&str] = &[
        "A-50",
        "FA-18C_hornet",
        "F/A-18A",
        "F/A-18C",
        "F-14A",
        "F-14A-135-GR",
        "F-14B",
        "AV8BNA",
        "S-3B",
        "Su-24M",
        "Su-24MR",
        "Su-33",
        "Su-34",
        "MiG-31",
        "Tu-95MS",
        "Tu-142",
        "Tu-160",
        "M-2000C",
        "Mirage 2000-5",
        "JF-17",
        "Tornado IDS",
        "Tornado GR4",
    ];
    if BOOM.contains(&typ) {
        Some(RefuelMethod::Boom)
    } else if DROGUE.contains(&typ) {
        Some(RefuelMethod::Drogue)
    } else {
        None
    }
}

fn method_label(m: RefuelMethod) -> &'static str {
    match m {
        RefuelMethod::Boom => "boom",
        RefuelMethod::Drogue => "drogue",
    }
}

struct Thirsty {
    gid: GroupId,
    name: CompactString,
    typ: CompactString,
    method: RefuelMethod,
    awacs: bool,
}

/// Send the side's AI flights that are low on fuel, can take it in the air
/// and have a compatible tanker on station within reach to that tanker. DCS
/// picks the tanker itself (the nearest that suits the receiver) and goes
/// back to the flight's mission when it is full. Many of the engine's
/// missions set unlimited fuel; their fuel never drops and this never fires
/// for them.
pub(super) fn fuel_watch(
    lua: MizLua,
    ctx: &mut Context,
    cfg: &HqCfg,
    side: Side,
    now: DateTime<Utc>,
) {
    let s = &cfg.support;
    if s.refuel_below <= 0. {
        return;
    }
    {
        let rt = ctx.hq.side(side);
        if rt.last_fuel_check.is_some_and(|t| (now - t).num_seconds() < FUEL_CHECK_SECS) {
            return;
        }
        rt.last_fuel_check = Some(now);
    }
    let tankers: Vec<SupportFlight> = picture::support_flights(ctx, side)
        .into_iter()
        .filter(|f| f.kind == OpKind::Tanker && f.on_station())
        .collect();
    let thirsty: Vec<Thirsty> = ctx
        .db
        .actions()
        .filter(|g| g.side == side && !ctx.airlife.sent_home(&g.id))
        .filter_map(|g| {
            let DeployKind::Action { spec, .. } = &g.origin else { return None };
            if matches!(spec.kind, ActionKind::Tanker(_) | ActionKind::Rtb) {
                return None;
            }
            let lead = g
                .units
                .into_iter()
                .filter_map(|uid| ctx.db.persisted.units.get(uid))
                .find(|u| !u.dead)?;
            lead.airborne_velocity?;
            let method = receiver_method(lead.typ.0.as_str())?;
            Some(Thirsty {
                gid: g.id,
                name: CompactString::from(g.name.as_str()),
                typ: CompactString::from(lead.typ.0.as_str()),
                method,
                awacs: matches!(spec.kind, ActionKind::Awacs(_)),
            })
        })
        .collect();
    let mut low_awacs: FxHashSet<GroupId> = FxHashSet::default();
    for f in thirsty {
        let state = (|| -> anyhow::Result<Option<(f64, Vector2, bool, Group)>> {
            let g = Group::get_by_name(lua, f.name.as_str())?;
            let u = g.get_unit(1)?;
            if !u.in_air()? {
                return Ok(None);
            }
            let fuel = u.get_fuel()? as f64;
            let p = u.get_point()?;
            let refuelable = u.has_attribute("Refuelable").unwrap_or(true);
            Ok(Some((fuel, Vector2::new(p.x, p.z), refuelable, g)))
        })();
        let (fuel, pos, refuelable, group) = match state {
            Ok(Some(s)) => s,
            Ok(None) => continue,
            Err(e) => {
                debug!("hq: {side:?} reading {}'s fuel: {e:?}", f.name);
                continue;
            }
        };
        let rt = ctx.hq.side(side);
        if fuel >= s.refuel_below {
            // Topped up (or never low): forget it.
            if fuel >= (s.refuel_below + 0.2).min(0.9) {
                rt.refuel_sent.remove(&f.gid);
                rt.refuel_none.remove(&f.gid);
            }
            continue;
        }
        if !refuelable {
            debug!(
                "hq: {side:?} {} ({}) is low but DCS says it can't refuel in the air",
                f.name, f.typ
            );
            continue;
        }
        if rt
            .refuel_sent
            .get(&f.gid)
            .is_some_and(|t| (now - *t).num_seconds() < REFUEL_RETRY_SECS)
        {
            continue;
        }
        let tanker = tankers
            .iter()
            .filter(|t| tanker_method(&t.typ) == Some(f.method))
            .map(|t| (t, dist(t.pos, pos)))
            .filter(|(_, d)| *d <= s.refuel_reach_m)
            .min_by(|a, b| a.1.total_cmp(&b.1));
        let Some((t, d)) = tanker else {
            if f.awacs {
                low_awacs.insert(f.gid);
            }
            let said = rt
                .refuel_none
                .get(&f.gid)
                .is_some_and(|t| (now - *t).num_seconds() < REFUEL_RETRY_SECS);
            if !said {
                rt.refuel_none.insert(f.gid, now);
                info!(
                    "hq: {side:?} {} ({}) is at {:.0}% fuel and no friendly {} tanker is on station within {:.0} km{}",
                    f.name,
                    f.typ,
                    fuel * 100.,
                    method_label(f.method),
                    s.refuel_reach_m / 1000.,
                    if f.awacs { "; its relief is due" } else { "" }
                );
            }
            continue;
        };
        let pushed = group.get_controller().and_then(|c| c.push_task(Task::Refuelling));
        match pushed {
            Ok(()) => {
                info!(
                    "hq: {side:?} {} ({}) is at {:.0}% fuel: sent to refuel from {} ({}, {} tanker, {:.0} km away)",
                    f.name,
                    f.typ,
                    fuel * 100.,
                    t.name,
                    t.typ,
                    method_label(f.method),
                    d / 1000.
                );
                let rt = ctx.hq.side(side);
                rt.refuel_sent.insert(f.gid, now);
                rt.note(
                    now,
                    format_compact!(
                        "HQ: {} sent to the tanker {} to refuel",
                        f.name,
                        t.name
                    ),
                );
            }
            Err(e) => warn!("hq: {side:?} could not send {} to refuel: {e:?}", f.name),
        }
    }
    let rt = ctx.hq.side(side);
    rt.low_fuel = low_awacs;
    let live: FxHashSet<GroupId> = ctx.db.actions().map(|g| g.id).collect();
    let rt = ctx.hq.side(side);
    rt.refuel_sent.retain(|g, _| live.contains(g));
    rt.refuel_none.retain(|g, _| live.contains(g));
}
