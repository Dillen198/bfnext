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

use super::{brg_rng, player_world_pos, slot_for_group, ArgTriple, ArgTuple};
use crate::{
    db::{objective::Objective, Db},
    Context,
};
use anyhow::{Context as ErrContext, Result};
use bfprotocols::db::objective::{ObjectiveId, ObjectiveKind};
use compact_str::{format_compact, CompactString};
use dcso3::{
    coalition::Side,
    coord::Coord,
    env::miz::GroupId,
    mission_commands::{GroupSubMenu, MissionCommands},
    net::SlotId,
    LuaVec3, MizLua, Vector2, Vector3,
};
use std::fmt::Write;

const PAGE_SIZE: usize = 10;

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
    if obj.captureable() {
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
        format_compact!(" S:{:>3}% F:{:>3}%", obj.supply(), obj.fuel())
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

fn capture_state(frac: f64, obj: &Objective) -> CompactString {
    if obj.in_capture_hold() {
        return CompactString::from("HELD post-capture -- takeable now by either side");
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
            "Supply {:>3}%   Fuel {:>3}%{}\n",
            obj.supply(),
            obj.fuel(),
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
    let _ = write!(s, "Capture: {}\n", capture_state(frac, obj));
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

fn friendly_status_page(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let from = from_pos(ctx, lua, &arg.fst);
    let report = build_side_report(&ctx.db, arg.snd, arg.snd, from, arg.trd as usize, true);
    ctx.db.ephemeral.msgs().panel_to_group(30, false, arg.fst, report);
    Ok(())
}

fn enemy_status_page(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let from = from_pos(ctx, lua, &arg.fst);
    let report = build_side_report(&ctx.db, arg.snd, arg.snd.opposite(), from, arg.trd as usize, false);
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
        lines.push((
            r,
            format_compact!(
                "{} [{}] {}{br} HP:{}% -- {tag}\n",
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

fn add_paged(
    mc: &MissionCommands,
    gid: GroupId,
    parent: &GroupSubMenu,
    label: &str,
    count: usize,
    side: Side,
    cb: fn(MizLua, ArgTriple<GroupId, Side, u8>) -> Result<()>,
) -> Result<()> {
    let pages = count.div_ceil(PAGE_SIZE).max(1);
    if pages <= 1 {
        mc.add_command_for_group(
            gid,
            label.into(),
            Some(parent.clone()),
            cb,
            ArgTriple { fst: gid, snd: side, trd: 0u8 },
        )?;
        return Ok(());
    }
    let root = mc.add_submenu_for_group(gid, label.into(), Some(parent.clone()))?;
    for page in 0..pages {
        let start = page * PAGE_SIZE + 1;
        let end = ((page + 1) * PAGE_SIZE).min(count);
        mc.add_command_for_group(
            gid,
            format_compact!("Page {} ({}-{})", page + 1, start, end).into(),
            Some(root.clone()),
            cb,
            ArgTriple { fst: gid, snd: side, trd: page as u8 },
        )?;
    }
    Ok(())
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
    let enemy_count = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == side.opposite())
        .count();

    mc.remove_submenu_for_group(miz_gid, GroupSubMenu::from(vec!["Objectives".into()]))?;
    let root = mc.add_submenu_for_group(miz_gid, "Objectives".into(), None)?;

    mc.add_command_for_group(
        miz_gid,
        "Nearest Base (detail)".into(),
        Some(root.clone()),
        nearest_base,
        ArgTriple { fst: miz_gid, snd: side, trd: 0u8 },
    )?;
    mc.add_command_for_group(
        miz_gid,
        "Capturable / Contested".into(),
        Some(root.clone()),
        contested,
        ArgTriple { fst: miz_gid, snd: side, trd: 0u8 },
    )?;
    mc.add_command_for_group(
        miz_gid,
        "Bases Under Threat".into(),
        Some(root.clone()),
        under_attack,
        ArgTriple { fst: miz_gid, snd: side, trd: 0u8 },
    )?;

    add_paged(&mc, miz_gid, &root, "Friendly Status", friendly.len(), side, friendly_status_page)?;
    add_paged(&mc, miz_gid, &root, "Enemy Status", enemy_count, side, enemy_status_page)?;

    // Per-base detail cards, paged so no single submenu gets wide (DCS radio
    // menus misbehave past ~10 entries per level). Capped so a pathological map
    // can't flood the menu tree -- Nearest Base + the Status lists still cover
    // everything past the cap.
    const DETAIL_CAP: usize = 50;
    if !friendly.is_empty() {
        friendly.truncate(DETAIL_CAP);
        let detail_root = mc.add_submenu_for_group(miz_gid, "Base Detail".into(), Some(root.clone()))?;
        let pages = friendly.len().div_ceil(PAGE_SIZE);
        for page in 0..pages {
            let parent = if pages > 1 {
                mc.add_submenu_for_group(
                    miz_gid,
                    format_compact!("Page {}", page + 1).into(),
                    Some(detail_root.clone()),
                )?
            } else {
                detail_root.clone()
            };
            for (oid, name) in friendly.iter().skip(page * PAGE_SIZE).take(PAGE_SIZE) {
                mc.add_command_for_group(
                    miz_gid,
                    name.clone().into(),
                    Some(parent.clone()),
                    detail_by_oid,
                    ArgTuple { fst: miz_gid, snd: *oid },
                )?;
            }
        }
    }

    Ok(())
}
