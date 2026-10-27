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
use crate::{db::cargo::SlotStats, msgq::FeedTo, Context};
use anyhow::{anyhow, Result};
use dcso3::{
    coalition::Side,
    coord::Coord,
    env::miz::{GroupId, UnitId},
    mission_commands::{invoke_mirrored_menu, mirrored_menu, MirroredMenuItem},
    net::{PlayerId, Ucid},
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


// ── Message feed ──────────────────────────────────────────────────────────
//
// F10 handlers answer the player with DCS text messages, not return values:
// "Need 3 crates", "you must land to spawn crates", the cargo list, the
// nearby-crate bearings. A panel that only showed "ok" after a click left out
// everything the click was for. So the panel reads the same messages back out
// of the message queue's feed (msgq::MsgQ::feed), filtered to the ones this
// player would have seen.

/// One message the player was sent.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct CockpitMsg {
    pub seq: u64,
    /// RFC 3339, UTC.
    pub at: String,
    /// "you" (to this player, their group or unit), "side", "all" or "chat".
    pub scope: &'static str,
    pub text: String,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct CockpitMsgs {
    /// Pass back as `since` to get only what is newer.
    pub next: u64,
    pub messages: Vec<CockpitMsg>,
}

/// Everything a message can be addressed to that means this player.
struct Audience {
    side: Side,
    player: Option<PlayerId>,
    group: Option<GroupId>,
    unit: Option<UnitId>,
}

fn audience(ctx: &Context, ucid: &Ucid) -> Result<Audience> {
    let player = ctx
        .db
        .player(ucid)
        .ok_or_else(|| anyhow!("unknown player {ucid}"))?;
    let (group, unit) = match player.current_slot.as_ref() {
        Some((slot, _)) => (
            ctx.db.ephemeral.get_slot_info(slot).map(|si| si.miz_gid),
            slot.as_unit_id(),
        ),
        None => (None, None),
    };
    Ok(Audience {
        side: player.side,
        player: ctx.connected.id_by_ucid.get(ucid).copied(),
        group,
        unit,
    })
}

fn scope_for(a: &Audience, to: &FeedTo) -> Option<&'static str> {
    match to {
        FeedTo::All => Some("all"),
        FeedTo::Side(s) if *s == a.side => Some("side"),
        FeedTo::Group(g) if Some(*g) == a.group => Some("you"),
        FeedTo::Unit(u) if Some(*u) == a.unit => Some("you"),
        FeedTo::Chat(None) => Some("chat"),
        FeedTo::Chat(Some(p)) if Some(*p) == a.player => Some("you"),
        _ => None,
    }
}

/// How much history a panel gets when it first asks (`since` = 0).
const FIRST_LOOK: usize = 25;

/// Messages for `ucid` newer than `since`.
pub(crate) fn messages(ctx: &Context, ucid: &Ucid, since: u64) -> Result<CockpitMsgs> {
    let a = audience(ctx, ucid)?;
    let q = ctx.db.ephemeral.msgs_ref();
    let mut out: Vec<CockpitMsg> = q
        .feed_since(since)
        .filter_map(|m| {
            scope_for(&a, &m.to).map(|scope| CockpitMsg {
                seq: m.seq,
                at: m.at.to_rfc3339(),
                scope,
                text: m.text.to_string(),
            })
        })
        .collect();
    if since == 0 && out.len() > FIRST_LOOK {
        out.drain(..out.len() - FIRST_LOOK);
    }
    Ok(CockpitMsgs { next: q.feed_seq(), messages: out })
}

/// Click a menu item on the player's behalf and return what its handler told
/// the player, joined -- the text the player would have read in the top right
/// of the screen.
///
/// Must be called with nothing holding the campaign state (see
/// `admin::run_menu_invokes`): the handler takes its own, exactly as when DCS
/// calls it, so this only borrows the state before and after it runs.
pub(crate) fn invoke_and_report(lua: MizLua, ucid: &Ucid, path: &[String]) -> Result<Option<String>> {
    let (group, before) = {
        let ctx = Context::borrow("cockpit menu click")?;
        (menu_group(&ctx, ucid)?, ctx.db.ephemeral.msgs_ref().feed_seq())
    };
    if !invoke_mirrored_menu(lua, group, path)? {
        return Ok(None);
    }
    // The handler took the state itself while it ran; borrow it again.
    let ctx = Context::borrow("cockpit menu click")?;
    let said = messages(&ctx, ucid, before)?
        .messages
        .into_iter()
        .filter(|m| m.scope == "you")
        .map(|m| m.text)
        .collect::<Vec<_>>();
    Ok(Some(if said.is_empty() { "Done.".to_string() } else { said.join("\n") }))
}

// ── Cargo ─────────────────────────────────────────────────────────────────
//
// What the slot can spawn and how many of each crate a deployable needs, as
// data rather than menu labels ("SA 3 Launcher(4)"), plus what is on board and
// what is lying nearby. Each crate carries the path of its own F10 spawn
// command, so the panel's spawn button fires exactly what F10 fires --
// same rules, same messages -- instead of a second implementation.

#[derive(Debug, Clone, Serialize)]
pub(crate) struct CargoCrate {
    pub name: String,
    /// How many of this crate the deployable needs.
    pub required: u32,
    pub weight_kg: u32,
    /// F10 path of the "spawn one" command, if this slot's menu has one.
    pub spawn: Option<Vec<String>>,
    /// F10 path of the "<crate> xN" submenu (C-130 / dynamic cargo), whose
    /// children "1".."9" spawn that many.
    pub spawn_n: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct CargoDeployable {
    pub name: String,
    pub path: Vec<String>,
    pub cost: u32,
    pub crates: Vec<CargoCrate>,
    pub repair: Option<CargoCrate>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct NearbyCrateInfo {
    pub name: String,
    pub distance_m: u32,
    pub bearing_deg: u32,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct CargoInfo {
    /// False in spectators: everything below is then empty.
    pub in_slot: bool,
    pub crate_slots: Option<u8>,
    pub troop_slots: Option<u8>,
    pub carrying: Vec<String>,
    pub troops: Vec<String>,
    pub nearby: Vec<NearbyCrateInfo>,
    /// Base supply crates: repair kit, fuel / weapons transfer, emergency.
    pub supply: Vec<CargoCrate>,
    pub deployables: Vec<CargoDeployable>,
}

/// The F10 paths to spawn `name`, found in the slot's mirrored menu by the
/// labels the cargo menus give them (menu/cargo.rs): the single command is
/// "<name>" or "<name>(<required>)" somewhere under a "Crates" submenu, and the
/// quantity submenu is "<name> xN".
fn spawn_paths(
    menu: &[MirroredMenuItem],
    name: &str,
    required: u32,
) -> (Option<Vec<String>>, Option<Vec<String>>) {
    let titled = format!("{name}({required})");
    let under_crates = |it: &MirroredMenuItem| it.path.iter().any(|p| p == "Crates");
    let one = menu
        .iter()
        .find(|it| it.command && under_crates(it) && (it.name == name || it.name == titled))
        .map(|it| it.path.clone());
    let xn = format!("{name} xN");
    let n = menu
        .iter()
        .find(|it| !it.command && under_crates(it) && it.name == xn)
        .map(|it| it.path.clone());
    (one, n)
}

fn cargo_crate(menu: &[MirroredMenuItem], cr: &bfprotocols::cfg::Crate) -> CargoCrate {
    let (spawn, spawn_n) = spawn_paths(menu, cr.name.as_str(), cr.required);
    CargoCrate {
        name: cr.name.to_string(),
        required: cr.required,
        weight_kg: cr.weight,
        spawn,
        spawn_n,
    }
}

pub(crate) fn cargo(ctx: &Context, lua: MizLua, ucid: &Ucid) -> Result<CargoInfo> {
    let player = ctx
        .db
        .player(ucid)
        .ok_or_else(|| anyhow!("unknown player {ucid}"))?;
    let side = player.side;
    let Some((slot, _)) = player.current_slot.clone() else {
        return Ok(CargoInfo {
            in_slot: false,
            crate_slots: None,
            troop_slots: None,
            carrying: vec![],
            troops: vec![],
            nearby: vec![],
            supply: vec![],
            deployables: vec![],
        });
    };
    let menu = menu(ctx, lua, ucid).unwrap_or_default();
    let cfg = &ctx.db.ephemeral.cfg;

    let capacity = ctx
        .db
        .ephemeral
        .get_slot_info(&slot)
        .and_then(|si| ctx.db.cargo_capacity(&si.typ).ok());
    let (carrying, troops) = match ctx.db.list_cargo(&slot) {
        Some(c) => (
            c.crates.iter().map(|(_, cr)| cr.name.to_string()).collect(),
            c.troops.iter().map(|t| t.troop.name.to_string()).collect(),
        ),
        None => (vec![], vec![]),
    };
    let nearby = SlotStats::get(&ctx.db, lua, &slot)
        .ok()
        .and_then(|st| ctx.db.list_nearby_crates(&st).ok())
        .map(|v| {
            v.iter()
                .map(|nc| NearbyCrateInfo {
                    name: nc.crate_def.name.to_string(),
                    distance_m: nc.distance as u32,
                    bearing_deg: nc.heading as u32,
                })
                .collect()
        })
        .unwrap_or_default();

    let mut supply = vec![];
    if let Some(rep) = cfg.repair_crate.get(&side) {
        supply.push(cargo_crate(&menu, rep));
    }
    if let Some(w) = cfg.warehouse.as_ref() {
        for cr in [
            w.supply_transfer_fuel_crate.get(&side),
            w.supply_transfer_weapons_crate.get(&side),
            w.carrier_repair_crate.get(&side),
        ]
        .into_iter()
        .flatten()
        {
            supply.push(cargo_crate(&menu, cr));
        }
    }
    // Only offer what this slot's menu actually has: a supply crate with no
    // spawn command here is not spawnable from this airframe.
    supply.retain(|c| c.spawn.is_some() || c.spawn_n.is_some());

    let deployables = cfg
        .deployables
        .get(&side)
        .map(|deps| {
            deps.iter()
                .filter(|d| !d.crates.is_empty() || d.repair_crate.is_some())
                .map(|d| CargoDeployable {
                    name: d.path.last().map(|s| s.to_string()).unwrap_or_default(),
                    path: d.path.iter().map(|s| s.to_string()).collect(),
                    cost: d.cost,
                    crates: d.crates.iter().map(|cr| cargo_crate(&menu, cr)).collect(),
                    repair: d.repair_crate.as_ref().map(|cr| cargo_crate(&menu, cr)),
                })
                .collect()
        })
        .unwrap_or_default();

    Ok(CargoInfo {
        in_slot: true,
        crate_slots: capacity.as_ref().map(|c| c.crate_slots),
        troop_slots: capacity.as_ref().map(|c| c.troop_slots),
        carrying,
        troops,
        nearby,
        supply,
        deployables,
    })
}

// ── Overlay keys ──────────────────────────────────────────────────────────
//
// The overlay page used to identify its player by `?playerid=` alone -- the
// small integer DCS assigns each connection. That is not a secret: anyone who
// could reach bfdb could walk the ids and click any connected player's F10
// menu, spawn crates in their name and read their coalition's radar picture.
//
// So the overlay now proves it is running inside that player's own DCS. It
// makes up a random key, hands it to the engine over the one channel only that
// client can use -- a chat line sent *as* that player, which the engine
// swallows before anyone sees it (see `KEY_CMD` in lib.rs's
// onPlayerTrySendChat) -- and then quotes the key on every request.
// `resolve-player-id` only answers when the key matches the one registered for
// that player id and ucid.

/// The chat command the overlay registers its key with. Never logged.
pub(crate) const KEY_CMD: &str = "-cockpitkey ";

/// Registered keys by player id, with the ucid that registered them so a key
/// cannot outlive its connection: DCS hands a disconnected player's id to the
/// next person to join.
static KEYS: std::sync::LazyLock<std::sync::Mutex<fxhash::FxHashMap<PlayerId, (Ucid, String)>>> =
    std::sync::LazyLock::new(|| std::sync::Mutex::new(fxhash::FxHashMap::default()));

fn valid_key(key: &str) -> bool {
    (16..=128).contains(&key.len()) && key.bytes().all(|b| b.is_ascii_alphanumeric())
}

/// Record the overlay key `id` just sent us. Replaces any earlier one, so a
/// reloaded overlay simply re-registers.
pub(crate) fn register_key(ctx: &Context, id: PlayerId, key: &str) -> Result<()> {
    if !valid_key(key) {
        return Err(anyhow!("malformed cockpit key"));
    }
    let ucid = ctx
        .connected
        .get(&id)
        .map(|ifo| ifo.ucid)
        .ok_or_else(|| anyhow!("player {id} is not connected"))?;
    let mut keys = KEYS.lock().unwrap_or_else(|e| e.into_inner());
    keys.insert(id, (ucid, key.to_string()));
    Ok(())
}

/// Whether `key` is the one `id` registered while connected as `ucid`.
/// Compared in constant time: this is the only thing standing between a
/// guessed key and someone else's F10 menu.
pub(crate) fn check_key(id: PlayerId, ucid: &Ucid, key: &str) -> bool {
    let keys = KEYS.lock().unwrap_or_else(|e| e.into_inner());
    match keys.get(&id) {
        Some((owner, want)) if owner == ucid && want.len() == key.len() => {
            want.bytes().zip(key.bytes()).fold(0u8, |acc, (a, b)| acc | (a ^ b)) == 0
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::valid_key;

    #[test]
    fn key_shape() {
        assert!(valid_key("abcdefghijklmnop0123456789ABCDEF"));
        assert!(!valid_key("short"));
        assert!(!valid_key("has space in it 0123456789"));
        assert!(!valid_key(&"a".repeat(129)));
    }
}
