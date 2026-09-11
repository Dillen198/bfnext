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

use super::{brg_rng, player_world_pos, slot_for_group, ArgQuad, ArgTriple, ArgTuple, Pager};
use crate::{
    db::{objective::Objective, Db},
    Context,
};
use anyhow::{Context as ErrContext, Result};
use bfprotocols::db::objective::{ObjectiveId, ObjectiveKind};
use chrono::Utc;
use compact_str::{format_compact, CompactString};
use dcso3::{
    coalition::Side,
    coord::Coord,
    env::miz::GroupId,
    mission_commands::{GroupSubMenu, MissionCommands},
    net::{SlotId, Ucid},
    LuaVec3, MizLua, Vector2, Vector3,
};
use mlua::prelude::{FromLua, IntoLua};
use std::fmt::Write;

const PAGE_SIZE: usize = 10;

/// Which paged status report a menu command drives.
const RPT_FRIENDLY: u8 = 0;
const RPT_ENEMY: u8 = 1;

/// What a paged status report command does to the page cursor.
const PG_FIRST: u8 = 0;
const PG_NEXT: u8 = 1;
const PG_PREV: u8 = 2;

/// Page cursor for the paged status reports, one per menu group. DCS radio
/// menu entries can't be relabeled once created, so instead of listing a
/// command per page (which eats the 10-entry-per-level budget and grows with
/// the map) the menu carries fixed Next/Previous Page commands that move this
/// cursor and re-send the report.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct StatusPages {
    friendly: usize,
    enemy: usize,
}

/// Advance/rewind/reset a cursor, wrapping at the ends and tolerating a page
/// count that shrank since the last click (bases get captured mid-sortie).
fn step_page(cur: usize, pages: usize, how: u8) -> usize {
    let pages = pages.max(1);
    let cur = cur.min(pages - 1);
    match how {
        PG_NEXT => (cur + 1) % pages,
        PG_PREV => (cur + pages - 1) % pages,
        _ => 0,
    }
}

fn fmt_kind(kind: &ObjectiveKind) -> &'static str {
    match kind {
        ObjectiveKind::Airbase => "AB",
        ObjectiveKind::Fob => "FOB",
        ObjectiveKind::Farp { .. } => "FARP",
        ObjectiveKind::Logistics => "LOGI",
        ObjectiveKind::NavalBase => "NAVAL",
        ObjectiveKind::CarrierGroup { .. } => "CVN",
        ObjectiveKind::Factory { .. } => "FACT",
        ObjectiveKind::SpecialSamSite => "SAM",
        ObjectiveKind::CommandCenter => "CC",
    }
}

fn side_tag(side: Side) -> &'static str {
    match side {
        Side::Blue => "BLU",
        Side::Red => "RED",
        Side::Neutral => "NEU",
    }
}

fn fmt_latlon(lat: f64, lon: f64) -> CompactString {
    let ns = if lat >= 0.0 { 'N' } else { 'S' };
    let ew = if lon >= 0.0 { 'E' } else { 'W' };
    let (lat, lon) = (lat.abs(), lon.abs());
    format_compact!(
        "{ns}{:02}\u{b0}{:05.2}' {ew}{:03}\u{b0}{:05.2}'",
        lat.trunc() as u32,
        lat.fract() * 60.0,
        lon.trunc() as u32,
        lon.fract() * 60.0,
    )
}

/// LL and MGRS strings for a map position. `None` if the coord library isn't reachable.
fn fmt_position(lua: MizLua, pos: Vector2) -> Option<(CompactString, CompactString)> {
    let coord = Coord::singleton(lua).ok()?;
    let ll = coord
        .lo_to_ll(LuaVec3(Vector3::new(pos.x, 0.0, pos.y)))
        .ok()?;
    let ll_s = fmt_latlon(ll.latitude, ll.longitude);
    let mgrs_s = coord
        .ll_to_mgrs(ll.latitude, ll.longitude)
        .ok()
        .map(|m| {
            format_compact!(
                "{}{} {:05.0} {:05.0}",
                m.utm_zone,
                m.mgrs_digraph,
                m.easting,
                m.northing
            )
        })
        .unwrap_or_else(|| CompactString::from("--"));
    Some((ll_s, mgrs_s))
}

fn from_pos(ctx: &Context, lua: MizLua, gid: &GroupId) -> Option<Vector2> {
    let (_, slot) = slot_for_group(lua, ctx, gid).ok()?;
    player_world_pos(ctx, &slot)
}

fn flags(db: &Db, oid: &ObjectiveId, obj: &Objective) -> CompactString {
    let mut s = CompactString::from("");
    if obj.threatened() {
        s.push_str(" [THREAT]");
    }
    if obj.in_capture_hold() {
        if obj.capture_hold_stalled() {
            s.push_str(" [CONSOLIDATION PAUSED]");
        } else {
            let pct = obj
                .capture_hold_pct(db.ephemeral.cfg.capture_consolidation_secs)
                .map(|(p, _)| p)
                .unwrap_or(0);
            let _ = write!(s, " [CONSOLIDATING {pct}%]");
        }
    } else if obj.captureable() {
        s.push_str(" [CAP]");
    }
    if db.capture_in_progress(oid) {
        s.push_str(" [BEING TAKEN]");
    }
    if obj.priority() {
        s.push_str(" [PRIORITY]");
    }
    s
}

/// One line for the paged side reports. `full` adds supply/fuel (friendly only).
fn objective_line(db: &Db, oid: &ObjectiveId, obj: &Objective, from: Option<Vector2>, full: bool) -> CompactString {
    let br = match from {
        Some(p) => {
            let (b, r) = brg_rng(p, obj.pos());
            format_compact!(" {b:03}\u{b0}/{r:.0}nm")
        }
        None => CompactString::from(""),
    };
    let stock = if full {
        format_compact!(
            " S:{:>3}% F:{:>3}% A:{:>3}%",
            obj.supply(),
            obj.fuel(),
            obj.aircraft()
        )
    } else {
        CompactString::from("")
    };
    format_compact!(
        "{} [{}]{br} HP:{:>3}% L:{:>3}%{stock}{}\n",
        obj.name(),
        fmt_kind(obj.kind()),
        obj.health(),
        obj.logi(),
        flags(db, oid, obj),
    )
}

fn build_side_report(
    db: &Db,
    viewer: Side,
    want: Side,
    from: Option<Vector2>,
    page: usize,
    full: bool,
) -> CompactString {
    let mut objectives: Vec<_> = db.objectives().filter(|(_, o)| o.owner() == want).collect();
    match from {
        Some(p) => objectives.sort_by(|(_, a), (_, b)| {
            brg_rng(p, a.pos()).1.total_cmp(&brg_rng(p, b.pos()).1)
        }),
        None => objectives.sort_by(|(_, a), (_, b)| a.name().cmp(b.name())),
    }
    let total = objectives.len();
    let total_pages = total.div_ceil(PAGE_SIZE).max(1);
    let slice: Vec<_> = objectives
        .into_iter()
        .skip(page * PAGE_SIZE)
        .take(PAGE_SIZE)
        .collect();
    let heading = if want == viewer { "Friendly" } else if want == Side::Neutral { "Neutral" } else { "Enemy" };
    let sort = if from.is_some() { "by range" } else { "by name" };
    let mut report = format_compact!("=== {heading} Objectives ({sort}) pg {}/{} ===\n", page + 1, total_pages);
    for &(oid, obj) in &slice {
        report.push_str(&objective_line(db, oid, obj, from, full));
    }
    if slice.is_empty() {
        report.push_str("(none)\n");
    }
    report
}

fn repair_state(db: &Db, oid: &ObjectiveId, obj: &Objective) -> CompactString {
    if obj.health() >= 100 {
        return CompactString::from("at full strength");
    }
    let cfg = &db.ephemeral.cfg;
    if obj.threatened() || db.capture_in_progress(oid) {
        return CompactString::from("suppressed (enemy in contact)");
    }
    if obj.supply() < cfg.repair_supply_cost {
        return format_compact!(
            "stalled -- supply {}% below the {}% each pulse needs",
            obj.supply(),
            cfg.repair_supply_cost
        );
    }
    if obj.logi() == 0 {
        return CompactString::from("stalled -- logistics defense destroyed");
    }
    let logi = (obj.logi() as f32 / 100.0).max(0.01);
    let pulse = (cfg.repair_time as f32 / logi).max(1.0);
    let elapsed = (chrono::Utc::now() - obj.last_change()).num_seconds().max(0) as f32;
    let remaining = (pulse - elapsed).max(0.0);
    format_compact!("active -- next pulse in ~{:.0}m", (remaining / 60.0).ceil())
}

/// Attacker-side reading of whether a knocked-down objective will heal back
/// out of capturable range, and what is stopping it if not. Mirrors the
/// gating in `maybe_do_repairs` / `repair_objective`.
fn repair_outlook(db: &Db, oid: &ObjectiveId, obj: &Objective) -> CompactString {
    if obj.health() >= 100 {
        return CompactString::from("at full strength");
    }
    if obj.health() == 0 && obj.owner() != Side::Neutral {
        return CompactString::from(
            "garrison wiped -- base will fall to NEUTRAL, then must be retaken with troops",
        );
    }
    if obj.owner() == Side::Neutral {
        return CompactString::from("NEUTRAL -- does not self-repair, must be retaken with troops");
    }
    if obj.threatened() {
        return CompactString::from("FROZEN while you keep units within sight of the base");
    }
    if db.capture_in_progress(oid) {
        return CompactString::from("FROZEN while the capture timer is running");
    }
    let cfg = &db.ephemeral.cfg;
    if obj.supply() < cfg.repair_supply_cost {
        return format_compact!(
            "STARVED -- supply {}% is below the {}% a repair pulse costs; cannot heal until resupplied",
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
    let elapsed = (chrono::Utc::now() - obj.last_change()).num_seconds().max(0) as f32;
    let remaining = (pulse - elapsed).max(0.0);
    format_compact!(
        "WILL self-repair -- next pulse ~{:.0}m; act fast or it heals back above 20%",
        (remaining / 60.0).ceil()
    )
}

fn capture_state(frac: f64, consolidation: u32, obj: &Objective) -> CompactString {
    if obj.in_capture_hold() {
        if obj.capture_hold_stalled() {
            return CompactString::from(
                "HELD, but consolidation is PAUSED -- the holding troops are outside \
                 the zone; get them back in, or wipe them to force it Neutral",
            );
        }
        return match obj.capture_hold_pct(consolidation) {
            Some((pct, remaining)) => format_compact!(
                "HELD -- consolidating {pct}% ({remaining}s left); you can't start a \
                 capture timer here, wipe the holding troops to force it Neutral"
            ),
            None => CompactString::from(
                "HELD -- new owner is consolidating; you can't start a capture timer \
                 here, wipe the holding troops to force it Neutral",
            ),
        };
    }
    if obj.kind().is_special_sam_site() {
        return if obj.health() == 0 {
            CompactString::from("eligible NOW -- SAM destroyed, capture is instant")
        } else {
            format_compact!("destroy the site first (health {}%)", obj.health())
        };
    }
    if obj.kind().is_carrier_group() {
        return if obj.logi() == 0 {
            CompactString::from("dead in the water -- board with capture troops")
        } else {
            CompactString::from("sink the SUPPLY ship first (Logi must reach 0%)")
        };
    }
    if obj.captureable() {
        return CompactString::from("eligible NOW -- move capture troops into the zone");
    }
    let frac_note = if frac > 0.0 {
        format_compact!(", plus ~{:.0}% of defenders destroyed", frac * 100.0)
    } else {
        CompactString::from("")
    };
    match (obj.health() > 20, obj.infantry() > 0) {
        (true, true) => format_compact!(
            "not eligible -- need health <=20% (now {}%) and 0 infantry (now {}){frac_note}",
            obj.health(),
            obj.infantry()
        ),
        (true, false) => format_compact!("not eligible -- need health <=20% (now {}%){frac_note}", obj.health()),
        (false, true) => format_compact!("not eligible -- clear {} infantry defender(s){frac_note}", obj.infantry()),
        (false, false) => CompactString::from("eligible NOW"),
    }
}

fn build_detail_card(ctx: &Context, lua: MizLua, oid: &ObjectiveId, viewer: Side, from: Option<Vector2>) -> CompactString {
    let db = &ctx.db;
    let obj = match db.objective(oid) {
        Ok(o) => o,
        Err(_) => return CompactString::from("that objective no longer exists"),
    };
    let friendly = obj.owner() == viewer;
    let mut s = format_compact!("========= {} =========\n", obj.name());
    let _ = write!(
        s,
        "{} - owned by {}{}\n",
        obj.kind().name(),
        match obj.owner() {
            Side::Blue => "BLUE",
            Side::Red => "RED",
            Side::Neutral => "NEUTRAL",
        },
        if obj.priority() { " - COMMANDER PRIORITY" } else { "" }
    );
    if let Some((ll, mgrs)) = fmt_position(lua, obj.pos()) {
        let _ = write!(s, "LL:   {ll}\nMGRS: {mgrs}\n");
    }
    if let Some(p) = from {
        let (b, r) = brg_rng(p, obj.pos());
        let _ = write!(s, "From you: {b:03}\u{b0} / {r:.1} nm\n");
    }
    let _ = write!(s, "Zone radius: {:.0} m\n", obj.radius());
    let _ = write!(s, "----------------------------------\n");
    let _ = write!(s, "Health {:>3}%   Logi {:>3}%\n", obj.health(), obj.logi());
    if friendly {
        let _ = write!(
            s,
            "Munitions {:>3}%   Fuel {:>3}%   Aircraft {:>3}%{}\n",
            obj.supply(),
            obj.fuel(),
            obj.aircraft(),
            if obj.unlimited_supply() { "  (UNLIMITED)" } else { "" }
        );
        let _ = write!(s, "Infantry defenders: {}\n", obj.infantry());
        let _ = write!(s, "Repair: {}\n", repair_state(db, oid, obj));
        if obj.logistics_detached() {
            let _ = write!(s, "NOTE: logistics detached -- no automatic resupply\n");
        }
    }
    if obj.kind().is_carrier_group() {
        let brc = crate::atis::carrier_brc(db, obj.kind());
        let _ = write!(s, "BRC: {brc:03}\u{b0}   (recovery case: Info > Weather)\n");
    }
    let frac = db
        .ephemeral
        .cfg
        .campaign_events
        .as_ref()
        .map(|c| c.capture_min_unit_pct_destroyed)
        .unwrap_or(0.0);
    let consolidation = db.ephemeral.cfg.capture_consolidation_secs;
    let _ = write!(s, "Capture: {}\n", capture_state(frac, consolidation, obj));
    if obj.threatened() {
        let _ = write!(s, "THREAT: enemy units within sight of the base\n");
    }
    match db.persisted.navaids.get(oid) {
        Some(navs) if !navs.is_empty() => {
            let _ = write!(s, "Navaids:\n{}\n", crate::navaids::summarize(navs));
        }
        _ => {
            let _ = write!(s, "Navaids: none assigned\n");
        }
    }
    s
}

// ---------------------------------------------------------------------------
// menu callbacks
// ---------------------------------------------------------------------------

/// Show a page of the friendly/enemy status report. `trd` picks the report,
/// `fth` says whether to jump to the first page or step the cursor.
fn status_page(lua: MizLua, arg: ArgQuad<GroupId, Side, u8, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let enemy = arg.trd == RPT_ENEMY;
    let want = if enemy { arg.snd.opposite() } else { arg.snd };
    let pages = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == want)
        .count()
        .div_ceil(PAGE_SIZE)
        .max(1);
    let page = {
        let cursor = ctx.objective_pages.entry(arg.fst).or_default();
        let slot = if enemy { &mut cursor.enemy } else { &mut cursor.friendly };
        *slot = step_page(*slot, pages, arg.fth);
        *slot
    };
    let from = from_pos(ctx, lua, &arg.fst);
    let report = build_side_report(&ctx.db, arg.snd, want, from, page, !enemy);
    ctx.db.ephemeral.msgs().panel_to_group(30, false, arg.fst, report);
    Ok(())
}

fn nearest_base(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let report = match from_pos(ctx, lua, &arg.fst) {
        None => CompactString::from("Get airborne first -- nearest base needs your position."),
        Some(p) => {
            let nearest = ctx
                .db
                .objectives()
                .filter(|(_, o)| o.owner() != Side::Neutral)
                .min_by(|(_, a), (_, b)| brg_rng(p, a.pos()).1.total_cmp(&brg_rng(p, b.pos()).1))
                .map(|(oid, _)| *oid);
            match nearest {
                None => CompactString::from("no objectives on the map"),
                Some(oid) => build_detail_card(ctx, lua, &oid, arg.snd, Some(p)),
            }
        }
    };
    ctx.db.ephemeral.msgs().panel_to_group(45, false, arg.fst, report);
    Ok(())
}

fn contested(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let from = from_pos(ctx, lua, &arg.fst);
    let mut lines: Vec<(f64, CompactString)> = vec![];
    for (oid, obj) in ctx.db.objectives() {
        let being_taken = ctx.db.capture_in_progress(oid);
        if !obj.captureable() && !being_taken {
            continue;
        }
        let (b, r) = from.map(|p| brg_rng(p, obj.pos())).unwrap_or((0, f64::MAX));
        let br = if from.is_some() {
            format_compact!(" {b:03}\u{b0}/{r:.0}nm")
        } else {
            CompactString::from("")
        };
        let tag = if being_taken { "BEING TAKEN" } else { "capturable" };
        let why = ctx
            .db
            .capture_diagnosis(oid, arg.snd)
            .ok()
            .map(|d| d.one_liner())
            .unwrap_or_default();
        lines.push((
            r,
            format_compact!(
                "{} [{}] {}{br} HP:{}% -- {tag}\n    {why}\n",
                obj.name(),
                fmt_kind(obj.kind()),
                side_tag(obj.owner()),
                obj.health()
            ),
        ));
    }
    lines.sort_by(|a, b| a.0.total_cmp(&b.0));
    let mut report = CompactString::from("=== Capturable / Contested ===\n");
    for (_, l) in &lines {
        report.push_str(l);
    }
    if lines.is_empty() {
        report.push_str("Nothing is capturable right now.\n");
    }
    ctx.db.ephemeral.msgs().panel_to_group(30, false, arg.fst, report);
    Ok(())
}

fn under_attack(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let from = from_pos(ctx, lua, &arg.fst);
    let mut lines: Vec<(chrono::DateTime<chrono::Utc>, CompactString)> = vec![];
    for (_, obj) in ctx.db.objectives() {
        if obj.owner() != arg.snd || !obj.threatened() {
            continue;
        }
        let br = match from {
            Some(p) => {
                let (b, r) = brg_rng(p, obj.pos());
                format_compact!(" {b:03}\u{b0}/{r:.0}nm")
            }
            None => CompactString::from(""),
        };
        lines.push((
            obj.last_threatened(),
            format_compact!(
                "{} [{}]{br} HP:{}% L:{}%\n",
                obj.name(),
                fmt_kind(obj.kind()),
                obj.health(),
                obj.logi()
            ),
        ));
    }
    lines.sort_by(|a, b| b.0.cmp(&a.0));
    let mut report = CompactString::from("=== Friendly Bases Under Threat ===\n");
    for (_, l) in &lines {
        report.push_str(l);
    }
    if lines.is_empty() {
        report.push_str("No friendly bases are under threat.\n");
    }
    ctx.db.ephemeral.msgs().panel_to_group(30, false, arg.fst, report);
    Ok(())
}

fn build_capture_advisor_card(
    ctx: &Context,
    lua: MizLua,
    oid: &ObjectiveId,
    viewer: Side,
    from: Option<Vector2>,
) -> CompactString {
    let db = &ctx.db;
    let obj = match db.objective(oid) {
        Ok(o) => o,
        Err(_) => return CompactString::from("that objective no longer exists"),
    };
    let diag = match db.capture_diagnosis(oid, viewer) {
        Ok(d) => d,
        Err(_) => return CompactString::from("could not read capture status"),
    };
    let mut s = format_compact!("===== CAPTURE ADVISOR: {} =====\n", diag.obj_name);
    let _ = write!(s, "{} - owned by {}\n", fmt_kind(obj.kind()), side_tag(diag.owner));
    if let Some((ll, mgrs)) = fmt_position(lua, obj.pos()) {
        let _ = write!(s, "LL:   {ll}\nMGRS: {mgrs}\n");
    }
    if let Some(p) = from {
        let (b, r) = brg_rng(p, obj.pos());
        let _ = write!(s, "From you: {b:03}\u{b0} / {r:.1} nm\n");
    }
    let _ = write!(s, "Zone radius: {:.0} m\n", obj.radius());
    let _ = write!(
        s,
        "Health {}%   Logi {}%   Supply {}%   Infantry {}\n",
        obj.health(),
        obj.logi(),
        obj.supply(),
        obj.infantry()
    );
    let _ = write!(s, "----------------------------------\n");

    if diag.owner == viewer {
        let _ = write!(s, "You already own this objective.\n");
        return s;
    }

    if diag.in_capture_hold {
        let _ = write!(
            s,
            "STATUS: post-capture hold -- takeable NOW by either side.\n"
        );
        let consolidation = db.ephemeral.cfg.capture_consolidation_secs;
        if obj.capture_hold_stalled() {
            let _ = write!(
                s,
                "  the holder's troops are OUT of the zone -- their clock is stopped.\n"
            );
        } else if let Some((pct, remaining)) = obj.capture_hold_pct(consolidation) {
            let _ = write!(
                s,
                "  consolidation {pct}% -- about {remaining}s before the garrison spawns.\n"
            );
        }
    } else if diag.obj_eligible {
        let _ = write!(s, "STATUS: objective is ELIGIBLE for capture.\n");
    } else {
        let _ = write!(s, "STATUS: objective NOT eligible yet --\n");
        for b in &diag.obj_blockers {
            let _ = write!(s, "  - {b}\n");
        }
    }

    if let Some(cd) = diag.cooldown_secs {
        let _ = write!(
            s,
            "COOLDOWN: base changed hands recently -- no capture timer can start for {cd}s.\n"
        );
    }

    if obj.health() < 100 && !diag.in_capture_hold {
        let _ = write!(s, "REPAIR: {}\n", repair_outlook(db, oid, obj));
    }

    if let Some((side, held, base)) = diag.in_progress {
        if side == viewer {
            let _ = write!(
                s,
                "IN PROGRESS: your side is capturing -- held {held}s (~{base}s needed, less with more squads). Hold the zone.\n"
            );
        } else {
            let _ = write!(
                s,
                "IN PROGRESS: {side:?} is capturing this base -- held {held}s. Kill their troops to stop it.\n"
            );
        }
    }

    let _ = write!(s, "----------------------------------\n");
    if diag.troops.is_empty() {
        let _ = write!(s, "YOUR CAPTURE TROOPS: none within 30 nm.\n");
        let _ = write!(
            s,
            "  Deploy capture-capable troops and move them into the zone.\n"
        );
    } else {
        let _ = write!(s, "YOUR TROOPS NEAR THIS BASE:\n");
        for t in &diag.troops {
            let loc = if t.in_zone {
                CompactString::from("in the zone")
            } else {
                format_compact!("{:.0} m outside the zone edge", t.outside_by)
            };
            let cap = if t.can_capture {
                ""
            } else {
                "  <-- CANNOT capture (troop type)"
            };
            let _ = write!(s, "  - {}: {}{}\n", t.name, loc, cap);
        }
    }

    let _ = write!(s, "----------------------------------\n");
    let _ = write!(s, "BOTTOM LINE: {}\n", diag.one_liner());
    s
}

fn capture_advisor_by_oid(lua: MizLua, arg: ArgTuple<GroupId, ObjectiveId>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (viewer, from) = match slot_for_group(lua, ctx, &arg.fst) {
        Ok((side, slot)) => (side, player_world_pos(ctx, &slot)),
        Err(_) => (Side::Neutral, None),
    };
    let report = build_capture_advisor_card(ctx, lua, &arg.snd, viewer, from);
    ctx.db.ephemeral.msgs().panel_to_group(45, false, arg.fst, report);
    Ok(())
}

fn capture_advisor_nearest(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let from = from_pos(ctx, lua, &arg.fst);
    let side = arg.snd;
    let pick = {
        let mut best: Option<(u8, f64, ObjectiveId)> = None;
        for (oid, obj) in ctx.db.objectives() {
            if obj.owner() == side {
                continue;
            }
            let hot = obj.captureable() || ctx.db.capture_in_progress(oid);
            let rank = if hot { 0u8 } else { 1u8 };
            let dist = from.map(|p| brg_rng(p, obj.pos()).1).unwrap_or(0.0);
            if best.map(|(br, bd, _)| (rank, dist) < (br, bd)).unwrap_or(true) {
                best = Some((rank, dist, *oid));
            }
        }
        best.map(|(_, _, oid)| oid)
    };
    let report = match pick {
        None => CompactString::from("No enemy or neutral objectives on the map."),
        Some(oid) => build_capture_advisor_card(ctx, lua, &oid, side, from),
    };
    ctx.db.ephemeral.msgs().panel_to_group(45, false, arg.fst, report);
    Ok(())
}

/// Resolve the calling player's `Ucid` from their F10-menu group id, or
/// message them and return `None` if it can't be done (e.g. they're not
/// actually in a slot right now).
fn ucid_for_menu_call(ctx: &Context, lua: MizLua, gid: &GroupId) -> Option<Ucid> {
    let (_, slot) = slot_for_group(lua, ctx, gid).ok()?;
    ctx.db.ephemeral.player_in_slot(&slot).copied()
}

/// Whether a helo mission carries troops (`true`) or supply (`false`).
const HELO_TROOPS: u8 = 0;
const HELO_SUPPLY: u8 = 1;

/// Dispatch an AI helo mission to `oid` on behalf of whoever is flying `gid`,
/// and describe the outcome for the panel message.
fn dispatch_helo(
    ctx: &mut Context,
    lua: MizLua,
    gid: GroupId,
    side: Side,
    oid: ObjectiveId,
    kind: u8,
) -> CompactString {
    let Some(ucid) = ucid_for_menu_call(ctx, lua, &gid) else {
        return CompactString::from("could not identify you as a player -- are you in a slot?");
    };
    let now = Utc::now();
    let (what, res) = if kind == HELO_TROOPS {
        (
            "Helo troop insertion",
            ctx.db.call_helo_troop_insertion(lua, side, ucid, oid, now),
        )
    } else {
        (
            "Helo resupply run",
            ctx.db.call_helo_resource_delivery(lua, side, ucid, oid, now),
        )
    };
    match res {
        Ok(id) => format_compact!("{what} dispatched ({id})."),
        Err(e) => format_compact!("Could not dispatch: {e}"),
    }
}

/// F10: dispatch an AI helo to a specific objective the player picked off the
/// menu. `trd` is `HELO_TROOPS` or `HELO_SUPPLY`.
fn helo_mission_by_oid(lua: MizLua, arg: ArgTriple<GroupId, ObjectiveId, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let side = match slot_for_group(lua, ctx, &arg.fst) {
        Ok((side, _)) => side,
        Err(e) => {
            ctx.db.ephemeral.msgs().panel_to_group(
                15,
                false,
                arg.fst,
                format_compact!("could not work out which side you are on: {e}"),
            );
            return Ok(());
        }
    };
    let report = dispatch_helo(ctx, lua, arg.fst, side, arg.snd, arg.trd);
    ctx.db.ephemeral.msgs().panel_to_group(15, false, arg.fst, report);
    Ok(())
}

/// F10: dispatch an AI helo to insert a fresh troop group at the nearest
/// capturable objective (falling back to the nearest non-owned one if
/// nothing is currently capturable, so the failure reason is explicit).
fn helo_troop_insertion_nearest(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let side = arg.snd;
    let from = from_pos(ctx, lua, &arg.fst);
    let pick = {
        let mut best: Option<(u8, f64, ObjectiveId)> = None;
        for (oid, obj) in ctx.db.objectives() {
            if obj.owner() == side {
                continue;
            }
            let rank = if obj.captureable() { 0u8 } else { 1u8 };
            let dist = from.map(|p| brg_rng(p, obj.pos()).1).unwrap_or(0.0);
            if best.map(|(br, bd, _)| (rank, dist) < (br, bd)).unwrap_or(true) {
                best = Some((rank, dist, *oid));
            }
        }
        best.map(|(_, _, oid)| oid)
    };
    let report = match pick {
        None => CompactString::from("No enemy or neutral objectives on the map."),
        Some(oid) => dispatch_helo(ctx, lua, arg.fst, side, oid, HELO_TROOPS),
    };
    ctx.db.ephemeral.msgs().panel_to_group(15, false, arg.fst, report);
    Ok(())
}

/// F10: dispatch an AI helo loaded with surplus supply from the nearest
/// friendly hub to the nearest friendly objective (the one closest to the
/// player -- presumably the one they're worried about).
fn helo_resource_delivery_nearest(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let side = arg.snd;
    let from = from_pos(ctx, lua, &arg.fst);
    let pick = {
        let mut best: Option<(f64, ObjectiveId)> = None;
        for (oid, obj) in ctx.db.objectives() {
            if obj.owner() != side {
                continue;
            }
            let dist = from.map(|p| brg_rng(p, obj.pos()).1).unwrap_or(0.0);
            if best.map(|(bd, _)| dist < bd).unwrap_or(true) {
                best = Some((dist, *oid));
            }
        }
        best.map(|(_, oid)| oid)
    };
    let report = match pick {
        None => CompactString::from("No friendly objectives on the map."),
        Some(oid) => dispatch_helo(ctx, lua, arg.fst, side, oid, HELO_SUPPLY),
    };
    ctx.db.ephemeral.msgs().panel_to_group(15, false, arg.fst, report);
    Ok(())
}

fn detail_by_oid(lua: MizLua, arg: ArgTuple<GroupId, ObjectiveId>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let (viewer, from) = match slot_for_group(lua, ctx, &arg.fst) {
        Ok((side, slot)) => (side, player_world_pos(ctx, &slot)),
        Err(_) => (Side::Neutral, None),
    };
    let report = build_detail_card(ctx, lua, &arg.snd, viewer, from);
    ctx.db.ephemeral.msgs().panel_to_group(45, false, arg.fst, report);
    Ok(())
}

// ---------------------------------------------------------------------------
// menu construction
// ---------------------------------------------------------------------------

/// A paged status report: three fixed commands (show / next / previous) that
/// drive the group's page cursor, instead of one command per page.
fn add_status_report(
    mc: &MissionCommands,
    gid: GroupId,
    parent: &GroupSubMenu,
    label: &str,
    side: Side,
    kind: u8,
) -> Result<()> {
    let root = mc.add_submenu_for_group(gid, label.into(), Some(parent.clone()))?;
    for (text, how) in [
        ("Show (First Page)", PG_FIRST),
        ("Next Page >>", PG_NEXT),
        ("<< Previous Page", PG_PREV),
    ] {
        mc.add_command_for_group(
            gid,
            text.into(),
            Some(root.clone()),
            status_page,
            ArgQuad { fst: gid, snd: side, trd: kind, fth: how },
        )?;
    }
    Ok(())
}

/// Objective picks for the helo menus: labelled with range from the player
/// when we know where they are, and ordered nearest-first, so the targets a
/// helo could actually reach are on the first page instead of whatever happens
/// to sort first alphabetically across the whole map.
fn helo_picks(
    ctx: &Context,
    from: Option<Vector2>,
    keep: impl Fn(&ObjectiveId, &Objective) -> bool,
) -> Vec<(ObjectiveId, CompactString)> {
    let mut v: Vec<(f64, ObjectiveId, CompactString)> = ctx
        .db
        .objectives()
        .filter(|(oid, o)| keep(oid, o))
        .map(|(oid, o)| {
            let rng = from.map(|p| brg_rng(p, o.pos()).1).unwrap_or(0.0);
            let cap = if o.captureable() { " [CAP]" } else { "" };
            let label = match from {
                Some(_) => format_compact!("{} {rng:.0}nm{cap}", o.name()),
                None => format_compact!("{}{cap}", o.name()),
            };
            (rng, *oid, label)
        })
        .collect();
    match from {
        Some(_) => v.sort_by(|a, b| a.0.total_cmp(&b.0)),
        None => v.sort_by(|a, b| a.2.cmp(&b.2)),
    }
    v.into_iter().map(|(_, oid, l)| (oid, l)).collect()
}

/// The AI helo dispatch commands. Lives here with its callbacks, but is hung
/// off the Actions menu (see `menu::action`) -- these are player-triggered
/// dispatches, not objective reports.
pub(super) fn add_helo_mission_menu(
    mc: &MissionCommands,
    ctx: &Context,
    lua: MizLua,
    gid: GroupId,
    parent: &GroupSubMenu,
    side: Side,
) -> Result<()> {
    let from = from_pos(ctx, lua, &gid);
    let root = mc.add_submenu_for_group(gid, "AI Helo Missions".into(), Some(parent.clone()))?;
    mc.add_command_for_group(
        gid,
        "Insert Troops: Nearest Capturable".into(),
        Some(root.clone()),
        helo_troop_insertion_nearest,
        ArgTriple { fst: gid, snd: side, trd: 0u8 },
    )?;
    // The short list of objectives troops can actually take right now. Absent
    // from the menu when nothing is capturable.
    let capturable = helo_picks(ctx, from, |oid, o| {
        o.owner() != side && (o.captureable() || ctx.db.capture_in_progress(oid))
    });
    add_base_list(
        mc,
        gid,
        &root,
        "Insert Troops: Capturable Now",
        capturable,
        helo_mission_by_oid,
        |oid| ArgTriple { fst: gid, snd: oid, trd: HELO_TROOPS },
    )?;
    // Everything else that isn't ours, for softening a base up ahead of time.
    let any_target = helo_picks(ctx, from, |_, o| o.owner() != side);
    add_base_list(
        mc,
        gid,
        &root,
        "Insert Troops: Any Objective",
        any_target,
        helo_mission_by_oid,
        |oid| ArgTriple { fst: gid, snd: oid, trd: HELO_TROOPS },
    )?;
    mc.add_command_for_group(
        gid,
        "Resupply: Nearest Friendly".into(),
        Some(root.clone()),
        helo_resource_delivery_nearest,
        ArgTriple { fst: gid, snd: side, trd: 0u8 },
    )?;
    let friendly = helo_picks(ctx, from, |_, o| o.owner() == side);
    add_base_list(
        mc,
        gid,
        &root,
        "Resupply: Friendly Base",
        friendly,
        helo_mission_by_oid,
        |oid| ArgTriple { fst: gid, snd: oid, trd: HELO_SUPPLY },
    )?;
    Ok(())
}

/// Leading characters of a base name, for chunk labels.
fn short_name(name: &str) -> CompactString {
    name.chars().take(10).collect()
}

/// A list of per-base commands, split into `1. Abu Su - Damasc` submenus so no
/// menu level goes past the ~10 entries DCS radio menus handle. Splits again
/// one level down when a map has more bases than a single split can hold, so
/// nothing falls off the end of the list.
fn add_base_tree<'lua, A>(
    mc: &MissionCommands<'lua>,
    gid: GroupId,
    parent: &GroupSubMenu,
    bases: &[(ObjectiveId, CompactString)],
    cb: fn(MizLua, A) -> Result<()>,
    mk_arg: &impl Fn(ObjectiveId) -> A,
) -> Result<()>
where
    A: IntoLua<'lua> + FromLua<'lua> + 'static,
{
    if bases.len() <= PAGE_SIZE {
        for (oid, name) in bases {
            mc.add_command_for_group(
                gid,
                name.clone().into(),
                Some(parent.clone()),
                cb,
                mk_arg(*oid),
            )?;
        }
        return Ok(());
    }
    // Smallest chunk size that keeps this level down to PAGE_SIZE submenus.
    let mut chunk = PAGE_SIZE;
    while bases.len().div_ceil(chunk) > PAGE_SIZE {
        chunk *= PAGE_SIZE;
    }
    for (n, group) in bases.chunks(chunk).enumerate() {
        // The index leads so two chunks can't collide on the same label when a
        // run of bases shares a name prefix.
        let label = format_compact!(
            "{}. {} - {}",
            n + 1,
            short_name(&group[0].1),
            short_name(&group[group.len() - 1].1)
        );
        let sub = mc.add_submenu_for_group(gid, label.into(), Some(parent.clone()))?;
        add_base_tree(mc, gid, &sub, group, cb, mk_arg)?;
    }
    Ok(())
}

/// `add_base_tree` under its own named submenu. Adds nothing at all when the
/// list is empty, so an empty submenu never shows up in the menu.
pub(super) fn add_base_list<'lua, A>(
    mc: &MissionCommands<'lua>,
    gid: GroupId,
    parent: &GroupSubMenu,
    label: &str,
    bases: Vec<(ObjectiveId, CompactString)>,
    cb: fn(MizLua, A) -> Result<()>,
    mk_arg: impl Fn(ObjectiveId) -> A,
) -> Result<()>
where
    A: IntoLua<'lua> + FromLua<'lua> + 'static,
{
    if bases.is_empty() {
        return Ok(());
    }
    let root = mc.add_submenu_for_group(gid, label.into(), Some(parent.clone()))?;
    add_base_tree(mc, gid, &root, &bases, cb, &mk_arg)
}

pub(super) fn init_objectives_menu_for_slot(ctx: &mut Context, lua: MizLua, slot: &SlotId) -> Result<()> {
    let mc = MissionCommands::singleton(lua)?;
    let si = ctx.db.ephemeral.get_slot_info(slot).context("getting slot info")?;
    let miz_gid = si.miz_gid;
    let side = si.side;

    let mut friendly: Vec<(ObjectiveId, CompactString)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == side)
        .map(|(oid, o)| (*oid, CompactString::from(o.name())))
        .collect();
    friendly.sort_by(|a, b| a.1.cmp(&b.1));
    // Enemy + neutral objectives, for the per-base Capture Advisor cards.
    let mut takeable: Vec<(ObjectiveId, CompactString)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() != side)
        .map(|(oid, o)| (*oid, CompactString::from(o.name())))
        .collect();
    takeable.sort_by(|a, b| a.1.cmp(&b.1));

    mc.remove_submenu_for_group(miz_gid, GroupSubMenu::from(vec!["Objectives".into()]))?;
    // The rebuilt menu starts at page 1 of every report.
    ctx.objective_pages.remove(&miz_gid);
    let root = mc.add_submenu_for_group(miz_gid, "Objectives".into(), None)?;
    // Eight entries today against DCS's cap of ten -- paged so the next report
    // added here lands on a "More >>" page instead of silently vanishing.
    let mut p = Pager::new(miz_gid, root);

    for (label, cb) in [
        ("Nearest Base (detail)", nearest_base as fn(MizLua, ArgTriple<GroupId, Side, u8>) -> Result<()>),
        ("Capturable / Contested", contested),
        ("Capture Advisor: Nearest", capture_advisor_nearest),
        ("Bases Under Threat", under_attack),
    ] {
        p.command(
            &mc,
            label.into(),
            cb,
            ArgTriple { fst: miz_gid, snd: side, trd: 0u8 },
        )?;
    }

    let page = p.page(&mc)?;
    add_status_report(&mc, miz_gid, &page, "Friendly Status", side, RPT_FRIENDLY)?;
    let page = p.page(&mc)?;
    add_status_report(&mc, miz_gid, &page, "Enemy Status", side, RPT_ENEMY)?;

    let page = p.page(&mc)?;
    add_base_list(&mc, miz_gid, &page, "Base Detail", friendly, detail_by_oid, |oid| {
        ArgTuple { fst: miz_gid, snd: oid }
    })?;
    // Capture Advisor cards for enemy / neutral objectives. "Capture Advisor:
    // Nearest" and "Capturable / Contested" cover anything past the list cap or
    // that flips owner mid-slot.
    let page = p.page(&mc)?;
    add_base_list(
        &mc,
        miz_gid,
        &page,
        "Capture Advisor",
        takeable,
        capture_advisor_by_oid,
        |oid| ArgTuple { fst: miz_gid, snd: oid },
    )?;

    Ok(())
}
