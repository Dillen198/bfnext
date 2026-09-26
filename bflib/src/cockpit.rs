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

//! Backing for the in-DCS cockpit overlay (`bfcockpit/Scripts/Hooks/bfcockpit.lua`).
//!
//! Two things live here:
//!
//! **Context** -- who the caller is and what they are flying, right now. The
//! overlay never asks the player to tell it their aircraft, their coalition or
//! where they are; the engine already knows all of that, so the panel reads it
//! and adapts itself (show the helo crate list to a helo, CARP to a Herc, the
//! objective you are actually parked at rather than a list of all of them).
//!
//! **The menu mirror** -- the whole F10 menu tree for the caller's slot, read
//! back out of `dcso3`'s mirror and invokable by path. This is deliberately
//! *not* a hand-written port of each menu: `menu/action.rs`, `cargo.rs`,
//! `jtac.rs`, `objectives.rs`, `troop.rs`, `info.rs` and `recon.rs` are ~6500
//! lines between them and they change constantly. Re-implementing them for a
//! second UI would mean two copies of every rule, drifting apart from the
//! first day. Instead the overlay renders the same tree DCS renders, and
//! clicking an item calls the very same handler -- so the cockpit UI is
//! automatically complete, and stays that way when menus are added or changed.
use crate::Context;
use anyhow::{anyhow, Result};
use dcso3::{
    coalition::Side,
    coord::Coord,
    mission_commands::{invoke_mirrored_menu, mirrored_menu, MirroredMenuItem},
    net::Ucid,
    LuaVec3, MizLua, Vector2, Vector3,
};
use serde_derive::Serialize;

/// Everything the overlay needs to draw itself for one player without asking
/// them a single question.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct CockpitContext {
    pub ucid: String,
    pub name: String,
    /// "blue" / "red" / "neutral".
    pub side: &'static str,
    pub points: i32,
    /// Lives remaining by life type, e.g. `[["standard", 3]]`. Empty when the
    /// campaign runs with lives disabled.
    pub lives: Vec<(String, u8)>,
    /// Crates this player is currently carrying or has spawned and not yet
    /// unpacked.
    pub crates: usize,
    /// `None` when the player is in spectators or a non-flying slot -- the
    /// overlay shows its "get in a jet" state rather than an empty panel.
    pub slot: Option<SlotContext>,
}

/// The live picture of the aircraft the player is sitting in.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct SlotContext {
    pub unit_name: String,
    /// DCS airframe type, e.g. "UH-1H", "Hercules", "F-16C_50". The panel keys
    /// its layout off this.
    pub airframe: String,
    pub in_air: bool,
    pub lat: f64,
    pub lon: f64,
    pub alt_ft: i32,
    pub heading_deg: u32,
    pub speed_kts: u32,
    /// The objective this slot belongs to (where you spawned).
    pub home_objective: Option<String>,
    /// The objective you are physically at, if you are sitting at one.
    pub at_objective: Option<String>,
    /// Closest objective of any owner, for orientation.
    pub nearest: Option<NearbyObjective>,
    /// Seconds left on the takeoff hold, if one is running.
    pub takeoff_ok_in_secs: Option<i64>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct NearbyObjective {
    pub name: String,
    pub owner: &'static str,
    pub distance_m: u32,
    /// Bearing from the player to the objective, degrees true.
    pub bearing_deg: u32,
}

fn side_str(side: Side) -> &'static str {
    match side {
        Side::Blue => "blue",
        Side::Red => "red",
        Side::Neutral => "neutral",
    }
}

/// Build the context for `ucid`. Everything past the pilot's identity is
/// best-effort: a player in spectators still gets a valid answer, just with no
/// `slot`.
pub(crate) fn context(ctx: &Context, lua: MizLua, ucid: &Ucid) -> Result<CockpitContext> {
    let player = ctx
        .db
        .player(ucid)
        .ok_or_else(|| anyhow!("unknown player {ucid}"))?;

    let lives = player
        .lives
        .into_iter()
        .map(|(lt, (_, n))| (format!("{lt:?}").to_lowercase(), *n))
        .collect();

    let slot = player.current_slot.as_ref().and_then(|(slot, inst)| {
        let inst = inst.as_ref()?;
        Some(slot_context(ctx, lua, slot, inst))
    });

    Ok(CockpitContext {
        ucid: ucid.to_string(),
        name: player.name.to_string(),
        side: side_str(player.side),
        points: player.points,
        lives,
        crates: player.crates.into_iter().count(),
        slot,
    })
}

fn slot_context(
    ctx: &Context,
    lua: MizLua,
    slot: &dcso3::net::SlotId,
    inst: &crate::db::player::InstancedPlayer,
) -> SlotContext {
    let pos3 = &inst.position;
    let ground = Vector2::new(pos3.p.x, pos3.p.z);

    let (lat, lon) = Coord::singleton(lua)
        .ok()
        .and_then(|c| c.lo_to_ll(LuaVec3(Vector3::new(ground.x, 0.0, ground.y))).ok())
        .map(|ll| (ll.latitude, ll.longitude))
        .unwrap_or((0.0, 0.0));

    // Heading off the airframe's own nose vector rather than its velocity, so
    // it stays correct for a helicopter hovering or sliding sideways.
    let nose = pos3.x;
    let heading_deg = if nose.x.abs() > f64::EPSILON || nose.z.abs() > f64::EPSILON {
        ((nose.z.atan2(nose.x).to_degrees() + 360.0) % 360.0) as u32
    } else {
        0
    };

    let v = inst.velocity;
    let speed_kts = ((v.x * v.x + v.y * v.y + v.z * v.z).sqrt() * 1.94384) as u32;

    let objective_name =
        |oid: &bfprotocols::db::objective::ObjectiveId| -> Option<String> {
            ctx.db
                .persisted
                .objectives
                .get(oid)
                .map(|o| o.name.to_string())
        };

    let nearest = crate::db::Db::objective_near_point(&ctx.db.persisted.objectives, ground, |_| true)
        .map(|(dist, bearing_from_obj, obj)| NearbyObjective {
            name: obj.name.to_string(),
            owner: side_str(obj.owner),
            distance_m: dist as u32,
            // `objective_near_point` reports the bearing from the objective to
            // the point; the pilot wants the reciprocal.
            bearing_deg: ((bearing_from_obj + 180.0).rem_euclid(360.0)) as u32,
        });

    let takeoff_ok_in_secs = inst.takeoff_ok_at.map(|t| {
        (t - chrono::Utc::now()).num_seconds().max(0)
    });

    SlotContext {
        unit_name: inst.unit_name.to_string(),
        airframe: inst.typ.to_string(),
        in_air: inst.in_air,
        lat,
        lon,
        alt_ft: (pos3.p.y * 3.28084) as i32,
        heading_deg,
        speed_kts,
        home_objective: ctx
            .db
            .player_current_objective_id(slot)
            .ok()
            .and_then(|oid| objective_name(&oid)),
        at_objective: inst.landed_at_objective.as_ref().and_then(objective_name),
        nearest,
        takeoff_ok_in_secs,
    }
}

/// The DCS group whose F10 menu belongs to this player right now.
fn menu_group(ctx: &Context, ucid: &Ucid) -> Result<dcso3::env::miz::GroupId> {
    let player = ctx
        .db
        .player(ucid)
        .ok_or_else(|| anyhow!("unknown player {ucid}"))?;
    let (slot, _) = player
        .current_slot
        .as_ref()
        .ok_or_else(|| anyhow!("you must be in a slot"))?;
    let si = ctx
        .db
        .ephemeral
        .get_slot_info(slot)
        .ok_or_else(|| anyhow!("no slot info"))?;
    Ok(si.miz_gid)
}

/// The player's whole F10 menu, in the order they see it.
pub(crate) fn menu(ctx: &Context, lua: MizLua, ucid: &Ucid) -> Result<Vec<MirroredMenuItem>> {
    mirrored_menu(lua, menu_group(ctx, ucid)?)
}

/// Click one F10 menu item on the player's behalf.
///
/// Resolves the group *first* and drops every borrow of `ctx` before the
/// handler runs, because the handler will take its own `Context::get_mut()`
/// exactly as it does when DCS calls it.
pub(crate) fn invoke(ctx: &Context, lua: MizLua, ucid: &Ucid, path: &[String]) -> Result<bool> {
    let group = menu_group(ctx, ucid)?;
    invoke_mirrored_menu(lua, group, path)
}
