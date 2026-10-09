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

use super::{Db, MapS, SetS, ephemeral::SlotInfo, group::DeployKind};
use crate::{maybe, maybe_mut, objective_mut};
use anyhow::{Context, Result, anyhow, bail};
use bfprotocols::{
    cfg::{LifeType, PointsCfg, UnitTag, Vehicle},
    db::{group::GroupId, objective::{ObjectiveId, ObjectiveKind}},
    shots::{Dead, Who},
    stats::{self, EnId, Stat},
};
use chrono::{Duration, prelude::*};
use compact_str::{CompactString, format_compact};
use dcso3::{
    MizLua, Position3, String, Vector2, Vector3,
    airbase::Airbase,
    coalition::Side,
    coord::Coord,
    net::{SlotId, Ucid},
    object::{DcsObject, DcsOid},
    unit::{ClassUnit, Unit},
};
use log::{debug, error, info};
use netidx::utils::Either;
use serde_derive::{Deserialize, Serialize};
use smallvec::{SmallVec, smallvec};
use std::cmp::{max, min};

struct VictimInfo {
    ucid: Ucid,
    name: String,
    ai_deployable: bool,
    life_type: Option<LifeType>,
}

#[derive(Debug, Clone)]
pub enum SlotAuth {
    Yes(Option<stats::Unit>),
    ObjectiveNotOwned(Side),
    ObjectiveHasNoLogistics,
    NoLives(LifeType),
    NoPoints {
        vehicle: Vehicle,
        cost: u32,
        balance: i32,
    },
    NotRegistered(Side),
    VehicleNotAvailable(Vehicle),
    Denied,
    EraRestricted { vehicle: Vehicle, era: compact_str::CompactString },
    /// The objective holds an aircraft type its own side doesn't normally
    /// produce -- salvage kept from the previous owner on capture (see
    /// capture_warehouse) -- and it hasn't been repaired far enough for the
    /// captors to put it in the air yet.
    CapturedNotReady(Vehicle),
    /// The objective was just taken and is still in its post-capture
    /// consolidation hold. Carries the seconds left on that hold so the denial
    /// can tell the player how long, rather than the bare "is capturable" the
    /// overloaded `captureable()` check used to produce.
    Consolidating(i64),
}

pub enum RegErr {
    AlreadyRegistered(Option<u8>, Side),
    AlreadyOn(Side),
}

#[derive(Debug, Clone)]
pub enum TakeoffRes {
    TookLife(LifeType),
    NoLifeTaken,
    OutOfLives,
    OutOfPoints,
    /// Got airborne before the `takeoff_delay_secs` hold expired -- carries the
    /// seconds that were still remaining.
    TooEarly(i64),
    /// The unit that took off isn't in a player slot at all. AI flights reach
    /// the takeoff handler whenever they were spawned more than a few seconds
    /// before they rolled -- a ground-started CAP taxis for over a minute, so
    /// the `recently_born` guard (5s, there to swallow the takeoff DCS fires
    /// for a unit spawned already airborne) has long since let go of them.
    /// Nothing to charge and nobody to tell: not an error.
    NotPlayerSlot,
}

#[derive(Debug, Clone, Default)]
pub struct InstancedPlayer {
    pub unit_name: String,
    pub position: Position3,
    pub velocity: Vector3,
    pub typ: Vehicle,
    pub in_air: bool,
    pub landed_at_objective: Option<ObjectiveId>,
    pub stopped_at_objective: bool,
    pub moved: Option<DateTime<Utc>>,
    /// Earliest time this player is cleared to get airborne (slot-entry time +
    /// `cfg.takeoff_delay_secs`). `None` when the delay is disabled.
    pub takeoff_ok_at: Option<DateTime<Utc>>,
}

/// One points debit split between a player and an objective's fund, kept so
/// that a later refund hands back what was actually paid -- to whoever paid
/// it -- and never more. This is the start of a charge ledger: anything that
/// charges through `charge_for_item` and may refund later can hold one of
/// these and settle it with `Db::refund_charge`.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct PointsCharge {
    /// The side that paid. An objective that has since changed hands does not
    /// get the fund's share back -- that would be a gift to the captors.
    pub side: Side,
    /// The objective whose fund covered whatever the player's points didn't.
    pub oid: ObjectiveId,
    pub cost: u32,
    /// Fraction of `cost` the player paid; the objective paid the rest.
    pub frac: f32,
}

impl PointsCharge {
    /// Split a refund of `amount` -- capped at what was charged -- into the
    /// (player, objective) shares, in the proportion they paid.
    pub fn refund_split(&self, amount: u32) -> (i32, i32) {
        let amount = amount.min(self.cost);
        let player = ((amount as f32 * self.frac.clamp(0., 1.)).round() as u32).min(amount);
        (player as i32, (amount - player) as i32)
    }
}

/// What the current flight actually cost the pilot: the life `takeoff` took
/// (if any) and the points it charged (if any). Landing at a friendly
/// objective, or a restart while airborne, hands back exactly this record and
/// clears it. Before it existed both simply assumed a life and the flight's
/// full cost had been taken, so a takeoff from anywhere that isn't a friendly
/// objective (or an air start) followed by a landing at one minted a free life
/// and a refund of points that were never charged.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FlightCharge {
    #[serde(default)]
    pub life: Option<LifeType>,
    #[serde(default)]
    pub points: Option<PointsCharge>,
    #[serde(default)]
    pub tally: SortieTally,
}

/// What the pilot did on the flight in progress, for the debrief they get on
/// landing. It lives on `FlightCharge` so it lasts exactly as long as the
/// flight -- across a stop at a field that isn't friendly, and a restart.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SortieTally {
    #[serde(default)]
    pub took_off: Option<DateTime<Utc>>,
    #[serde(default)]
    pub from: Option<ObjectiveId>,
    #[serde(default)]
    pub fired: u32,
    /// Victim type and count, in the order first killed.
    #[serde(default)]
    pub kills: Vec<(String, u16)>,
    #[serde(default)]
    pub kill_points: i32,
}

impl SortieTally {
    fn add_kill(&mut self, typ: &str, points: i32) {
        match self.kills.iter_mut().find(|(t, _)| t.as_str() == typ) {
            Some((_, n)) => *n = n.saturating_add(1),
            None => self.kills.push((String::from(typ), 1)),
        }
        self.kill_points = self.kill_points.saturating_add(points);
    }

    pub fn kill_count(&self) -> u32 {
        self.kills.iter().map(|(_, n)| *n as u32).sum()
    }
}

/// Everything a processed landing settled, for the pilot's debrief.
#[derive(Debug, Clone)]
pub struct LandReport {
    pub ucid: Ucid,
    /// The friendly objective landed at; `None` is a landing that earns nothing.
    pub at: Option<ObjectiveId>,
    pub life_type: Option<LifeType>,
    pub life_returned: bool,
    /// What the takeoff charged, and how much of it came back for stores
    /// still aboard.
    pub charged: u32,
    pub refunded: i32,
    /// Provisional kill points banked by this landing, or still waiting for
    /// a friendly one.
    pub banked: i32,
    pub pending: i32,
    pub points: i32,
    pub tally: SortieTally,
}

/// Split `cost` into the (player, objective) shares actually paid. The
/// player's positive balance goes first, the objective's positive fund covers
/// what it can of the rest, and anything left over is the player's debt. The
/// fund is never driven below zero: a negative fund used to shut every player
/// without the points to cover it out of that base's slots.
fn split_charge(player_balance: i32, obj_balance: i32, cost: u32) -> (u32, u32) {
    let player_first = (player_balance.max(0) as u32).min(cost);
    let obj = (obj_balance.max(0) as u32).min(cost - player_first);
    (cost - obj, obj)
}

/// Team kills older than this many `tk_window` periods are forgotten. By then
/// the halving decay has taken their point penalty below 1/256 of a fresh
/// one; before this they were kept, and each still added lives, forever.
const TK_MEMORY_WINDOWS: i64 = 8;

/// How many whole `window`-hour periods ago a team kill at `ts` was, or
/// `None` once it has been forgotten. A window of 0 means past team kills are
/// forgotten immediately (it used to divide by zero).
fn tk_windows(now: DateTime<Utc>, ts: DateTime<Utc>, window: i64) -> Option<i64> {
    if window <= 0 {
        return None;
    }
    let windows = (now - ts).num_hours().max(0) / window;
    (windows < TK_MEMORY_WINDOWS).then_some(windows)
}

/// `points` halved once per elapsed window. `checked_shr` because a plain
/// shift by 32 or more is masked in release builds and wraps back round to
/// the full penalty.
fn tk_decayed(points: u32, windows: i64) -> u32 {
    u32::try_from(windows)
        .ok()
        .and_then(|w| points.checked_shr(w))
        .unwrap_or(0)
}

/// The extra points an AI team kill costs on top of `points`, from the
/// shooter's remembered AI team kills.
fn ai_tk_penalty(
    history: impl IntoIterator<Item = DateTime<Utc>>,
    now: DateTime<Utc>,
    window: i64,
    points: u32,
) -> u32 {
    history
        .into_iter()
        .filter_map(|ts| tk_windows(now, ts, window))
        .fold(0u32, |acc, w| acc.saturating_add(tk_decayed(points, w)))
}

/// Total (points, lives) a player team kill costs, including `points` and
/// the one life for this kill itself, from the shooter's remembered player
/// team kills.
fn player_tk_penalty(
    history: impl IntoIterator<Item = DateTime<Utc>>,
    now: DateTime<Utc>,
    window: i64,
    points: u32,
) -> (u32, f32) {
    history
        .into_iter()
        .filter_map(|ts| tk_windows(now, ts, window))
        .fold((points, 1.), |(pp, pl), windows| {
            let pp = pp.saturating_add(tk_decayed(points, windows));
            let pl = pl + (1. / (max(1, windows * 2) as f32));
            (pp, pl)
        })
}

impl Player {
    /// Forget team kills that no longer count towards the penalty, so the
    /// history doesn't grow for the life of the campaign.
    fn prune_team_kills(&mut self, now: DateTime<Utc>, window: i64) {
        let stale_ai: SmallVec<[DateTime<Utc>; 8]> = self
            .ai_team_kills
            .into_iter()
            .filter(|ts| tk_windows(now, **ts, window).is_none())
            .copied()
            .collect();
        for ts in &stale_ai {
            self.ai_team_kills.remove_cow(ts);
        }
        let stale_player: SmallVec<[DateTime<Utc>; 8]> = self
            .player_team_kills
            .into_iter()
            .filter(|(ts, _)| tk_windows(now, **ts, window).is_none())
            .map(|(ts, _)| *ts)
            .collect();
        for ts in &stale_player {
            self.player_team_kills.remove_cow(ts);
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Player {
    pub name: String,
    pub alts: SetS<String>,
    pub side: Side,
    pub side_switches: Option<u8>,
    pub lives: MapS<LifeType, (DateTime<Utc>, u8)>,
    pub crates: SetS<GroupId>,
    #[serde(default)]
    pub airborne: Option<LifeType>,
    #[serde(default)]
    pub points: i32,
    #[serde(default)]
    pub ai_team_kills: SetS<DateTime<Utc>>,
    #[serde(default)]
    pub player_team_kills: MapS<DateTime<Utc>, Ucid>,
    /// Kills in the current sortie (resets on death)
    #[serde(default)]
    pub kill_streak: u8,
    /// Total career kills
    #[serde(default)]
    pub total_kills: u32,
    /// The charge record for the flight in progress, see `FlightCharge`.
    /// Always `Some` from takeoff until the flight ends; `None` on a save
    /// written before it existed.
    #[serde(default)]
    pub flight: Option<FlightCharge>,
    #[serde(skip)]
    pub current_slot: Option<(SlotId, Option<InstancedPlayer>)>,
    #[serde(skip)]
    pub changing_slots: bool,
    #[serde(skip)]
    pub jtac_or_spectators: bool,
    #[serde(skip)]
    pub provisional_points: i32,
    /// The late-joiner head start above `new_player_join` (see
    /// `Db::late_joiner_start`). It can be spent but not `-transfer`red:
    /// only points above it can be given away, so an alt account can't be
    /// made just to hand its start to a main.
    #[serde(default)]
    pub untransferable: i32,
}

impl Db {
    pub fn player_deslot(&mut self, ucid: &Ucid) {
        if let Some(player) = self.persisted.players.get_mut_cow(ucid) {
            player.airborne = None;
            // Leaving the slot ends the flight: a pilot who deslots in the
            // air has lost the aircraft, and one on the ground was already
            // settled by `land`. Nothing left to refund either way.
            player.flight = None;
            player.provisional_points = 0;
            if let Some((slot, _)) = player.current_slot.take() {
                let _ = self
                    .ephemeral
                    .player_deslot(&self.persisted, &slot, Some(*ucid));
            }
            // Close the sortie this slot session opened. Only a session that
            // ended on the ground counts as landed -- a pilot who died,
            // ejected or jumped to spectators mid-flight leaves it open, which
            // is how the dashboard shows a sortie as not landed. This has to go
            // out before `Stat::Deslot`: bfdb drops the slot, and with it the
            // open SortieId, the moment it sees the deslot.
            if let Some(Some(_landed)) = self.ephemeral.open_sorties.remove(ucid) {
                self.ephemeral.stat(Stat::Land { id: *ucid });
            }
            self.ephemeral.stat(Stat::Deslot { id: *ucid });
            self.ephemeral.dirty()
        }
    }

    /// Close every sortie still open with its pilot on the ground. Shutdown
    /// ends the slot session for everyone still slotted but never runs
    /// `player_deslot`, so without this a pilot who landed and stayed in the
    /// airframe would leave the sortie open forever -- never landed, no hours
    /// credited. Pilots still airborne are left open, exactly as a mid-flight
    /// deslot leaves them.
    pub fn close_open_sorties(&mut self) {
        let open = std::mem::take(&mut self.ephemeral.open_sorties);
        for (ucid, landed) in open {
            if landed.is_some() {
                self.ephemeral.stat(Stat::Land { id: ucid });
            }
        }
    }

    pub fn player(&self, ucid: &Ucid) -> Option<&Player> {
        self.persisted.players.get(ucid)
    }

    pub fn player_mut(&mut self, ucid: &Ucid) -> Option<&mut Player> {
        self.persisted.players.get_mut_cow(ucid)
    }

    pub fn transfer_points(
        &mut self,
        source: &Ucid,
        target: Either<&Ucid, ObjectiveId>,
        amount: u32,
    ) -> Result<()> {
        let sp = self
            .persisted
            .players
            .get_mut_cow(source)
            .ok_or_else(|| anyhow!("source player not found"))?;
        if sp.points < amount as i32 {
            bail!(
                "insufficient balance, you have {}, you requested {}",
                sp.points,
                amount
            )
        }
        // Whatever of the head start has been spent is spent: the lock only
        // ever shrinks to the balance.
        sp.untransferable = sp.untransferable.clamp(0, sp.points.max(0));
        let transferable = sp.points - sp.untransferable;
        if transferable < amount as i32 {
            bail!(
                "you can transfer {} -- the other {} is your starting grant, which you can spend but not give away",
                transferable,
                sp.untransferable
            )
        }
        sp.points -= amount as i32;
        let sp_name = sp.name.clone();
        match target {
            Either::Left(target) => match self.persisted.players.get_mut_cow(target) {
                Some(tp) => {
                    tp.points += amount as i32;
                    let msg = format_compact!(
                        "{}(+{}) you received points from {}",
                        tp.points,
                        amount,
                        sp_name
                    );
                    self.ephemeral
                        .panel_to_player(&self.persisted, 10, target, msg);
                    self.ephemeral.stat(Stat::PointsTransfer {
                        from: *source,
                        to: *target,
                        points: amount,
                    });
                    self.ephemeral.dirty();
                    Ok(())
                }
                None => {
                    self.persisted.players[source].points += amount as i32;
                    bail!("target player not found")
                }
            },
            Either::Right(target) => match self.persisted.objectives.get_mut_cow(&target) {
                Some(obj) => {
                    obj.points += amount as i32;
                    self.ephemeral.stat(Stat::PointsTransferToObjective {
                        from: *source,
                        to: target,
                        points: amount,
                    });
                    self.ephemeral.dirty();
                    Ok(())
                }
                None => {
                    self.persisted.players[source].points += amount as i32;
                    bail!("target objective not found")
                }
            },
        }
    }

    pub fn player_reset_lives(&mut self, ucid: &Ucid) -> Result<()> {
        maybe_mut!(self.persisted.players, ucid, "player")?.lives = MapS::new();
        self.ephemeral.stat(Stat::Life {
            id: *ucid,
            lives: MapS::new(),
        });
        self.ephemeral.dirty();
        Ok(())
    }

    /// Reset every player's lives. Returns the number of players who actually
    /// had lives consumed.
    pub fn reset_all_lives(&mut self) -> Result<usize> {
        let ucids: SmallVec<[Ucid; 64]> = self
            .persisted
            .players
            .into_iter()
            .filter(|(_, player)| player.lives.len() > 0)
            .map(|(ucid, _)| *ucid)
            .collect();
        for ucid in &ucids {
            if let Some(player) = self.persisted.players.get_mut_cow(ucid) {
                player.lives = MapS::new();
            }
            self.ephemeral.stat(Stat::Life {
                id: *ucid,
                lives: MapS::new(),
            });
        }
        if !ucids.is_empty() {
            self.ephemeral.dirty();
        }
        Ok(ucids.len())
    }

    pub fn instanced_players(&self) -> impl Iterator<Item = (&Ucid, &Player, &InstancedPlayer)> {
        self.ephemeral.players_by_slot.values().filter_map(|ucid| {
            self.persisted.players.get(ucid).and_then(|player| {
                player
                    .current_slot
                    .as_ref()
                    .and_then(|(_, inst)| inst.as_ref())
                    .map(|inst| (ucid, player, inst))
            })
        })
    }

    pub fn player_in_unit(&self, include_deployed: bool, id: &DcsOid<ClassUnit>) -> Option<Ucid> {
        match self
            .ephemeral
            .get_slot_by_object_id(id)
            .and_then(|s| self.ephemeral.players_by_slot.get(s))
        {
            Some(ucid) => Some(ucid.clone()),
            None => {
                if !include_deployed {
                    None
                } else {
                    self.ephemeral
                        .uid_by_object_id
                        .get(id)
                        .and_then(|uid| self.persisted.units.get(uid))
                        .and_then(|unit| self.persisted.groups.get(&unit.group))
                        .and_then(|group| match &group.origin {
                            DeployKind::Deployed {
                                player,
                                spec: _,
                                moved_by: _,
                                cost_fraction: _,
                                origin: _,
                                jtac: _,
                            } => Some(player.clone()),
                            DeployKind::Troop {
                                player,
                                spec: _,
                                moved_by: _,
                                origin: _,
                                cost_fraction: _,
                                ..
                            } => Some(*player),
                            DeployKind::Action { player, .. } => player.clone(),
                            DeployKind::Crate { .. }
                            | DeployKind::Objective { .. }
                            | DeployKind::ObjectiveDeprecated
                            | DeployKind::DownedPilot { .. }
                            | DeployKind::Dismount { .. } => None,
                        })
                }
            }
        }
    }

    /// The bill for a flight: (label, count, points each) per line -- the
    /// airframe first, then every priced store on the aircraft -- and whether
    /// the cost is strictly enforced. `None` when points are off.
    fn flight_cost_items(
        &self,
        sifo: &SlotInfo,
        unit: &Unit,
    ) -> Result<Option<(SmallVec<[(CompactString, u32, u32); 8]>, bool)>> {
        let Some(points) = self.ephemeral.cfg.points.as_ref() else {
            return Ok(None);
        };
        let mut items: SmallVec<[(CompactString, u32, u32); 8]> = smallvec![];
        let airframe = *points.airframe_cost.get(&sifo.typ).unwrap_or(&0);
        items.push((format_compact!("{}", sifo.typ), 1, airframe));
        if !points.weapon_cost.is_empty() {
            for ammo in unit.get_ammo().context("getting ammo")? {
                let ammo = ammo.context("unwrapping ammo")?;
                let typ = ammo.type_name().context("getting ammo type name")?;
                if let Some(each) = points.weapon_cost.get(&typ) {
                    let n = ammo.count().context("getting ammo count")?;
                    // DCS's display name ("AIM-120C AMRAAM") reads better than
                    // the type key ("AIM_120C") the price list is keyed by.
                    let label = match ammo.display_name() {
                        Ok(d) if !d.trim().is_empty() => format_compact!("{}", d.trim()),
                        _ => format_compact!("{typ}"),
                    };
                    items.push((label, n, *each));
                }
            }
        }
        Ok(Some((items, points.strict)))
    }

    fn compute_flight_cost(&self, sifo: &SlotInfo, unit: &Unit) -> Result<(u32, bool, String)> {
        use std::fmt::Write;
        let mut m = String::from("");
        match self.flight_cost_items(sifo, unit)? {
            None => Ok((0, false, m)),
            Some((items, strict)) => {
                let mut cost = 0;
                for (i, (label, n, each)) in items.iter().enumerate() {
                    let c = n * each;
                    cost += c;
                    if i == 0 {
                        write!(m, "{c} for {label}").unwrap();
                    } else {
                        write!(m, ", {c} for {n}x{label}").unwrap();
                    }
                }
                Ok((cost, strict, m))
            }
        }
    }

    /// The panel shown when a pilot starts to taxi: an itemised bill, what
    /// they can pay with, and -- when the cost is enforced and they can't
    /// cover it -- a plain warning not to take off, because takeoff then
    /// destroys the aircraft. `fund` is the base they are sitting at and its
    /// points, which `takeoff` also lets pay for the flight.
    fn flight_cost_panel(
        &self,
        sifo: &SlotInfo,
        unit: &Unit,
        balance: i32,
        fund: Option<(&str, i32)>,
    ) -> Result<Option<CompactString>> {
        use std::fmt::Write;
        let Some((items, strict)) = self.flight_cost_items(sifo, unit)? else {
            return Ok(None);
        };
        if let Some(points) = self.ephemeral.cfg.points.as_ref()
            && points.lifeline_applies(balance, &sifo.typ)
        {
            let stores: u32 = items.iter().skip(1).map(|(_, n, each)| n * each).sum();
            let charge = points.lifeline_store_charge(stores);
            let mut m = format_compact!(
                "LIFELINE FLIGHT\n\
                 You have {balance} points, so this {} flies free",
                sifo.typ
            );
            match points.lifeline.as_ref().and_then(|l| l.store_budget) {
                None => m.push_str(", weapons included."),
                Some(budget) => {
                    let _ = write!(m, ", with up to {budget} points of weapons.");
                    if charge > 0 {
                        let available = balance.max(0) + fund.map(|(_, p)| p.max(0)).unwrap_or(0);
                        let _ = write!(m, "\nYour loadout is {stores}: {charge} over the budget.");
                        if strict && charge as i32 > available {
                            m.push_str(
                                "\n\n!! NOT ENOUGH POINTS -- DO NOT TAKE OFF !!\n\
                                 Taking off will DESTROY your aircraft. Unload some weapons \
                                 at the rearm menu.",
                            );
                        }
                    }
                }
            }
            m.push_str("\nKills, logistics and captures earn your way back.");
            return Ok(Some(m));
        }
        let total: u32 = items.iter().map(|(_, n, each)| n * each).sum();
        if total == 0 {
            return Ok(None);
        }
        let mut m = format_compact!("FLIGHT COST\n");
        for (i, (label, n, each)) in items.iter().enumerate() {
            let c = n * each;
            if i == 0 {
                let _ = write!(m, "  {label} airframe: {c}\n");
            } else if c > 0 {
                let _ = write!(m, "  {n} x {label}: {c}  ({each} each)\n");
            }
        }
        let _ = write!(m, "  TOTAL: {total}\n");
        let fund_pts = fund.map(|(_, p)| p.max(0)).unwrap_or(0);
        let available = balance.max(0) + fund_pts;
        let _ = write!(m, "Your points: {balance}");
        if let Some((name, p)) = fund {
            let _ = write!(m, "  |  {name} base fund: {p}");
        }
        let total = total as i32;
        if strict && total > available {
            let _ = write!(
                m,
                "\n\n!! NOT ENOUGH POINTS -- DO NOT TAKE OFF !!\n\
                 You are {} short. Taking off will DESTROY your aircraft.\n\
                 Unload weapons at the rearm menu or pick a cheaper airframe.",
                total - available
            );
        } else if total > balance.max(0) {
            match fund {
                Some((name, _)) if strict || fund_pts > 0 => {
                    let _ = write!(
                        m,
                        "\n{} of it will come out of the {name} base fund.",
                        total - balance.max(0)
                    );
                }
                _ => {
                    let _ = write!(m, "\nThis takes you to {} points.", balance - total);
                }
            }
        } else {
            let _ = write!(m, "\nAfter takeoff: {} points.", balance - total);
        }
        Ok(Some(m))
    }

    pub fn takeoff(
        &mut self,
        time: DateTime<Utc>,
        slot: SlotId,
        unit: &Unit,
        position: Vector2,
    ) -> Result<TakeoffRes> {
        let Some(sifo) = self.ephemeral.slot_info.get(&slot) else {
            return Ok(TakeoffRes::NotPlayerSlot);
        };
        let (mut cost, strict, cost_msg) = match self.compute_flight_cost(&sifo, unit) {
            Ok(cost) => cost,
            Err(e) => {
                error!("failed to compute flight cost {e:?}");
                (0, false, String::from(""))
            }
        };
        let typ = sifo.typ.clone();
        // An AI unit can occupy a slot the miz also offers to players, so this
        // is the second place a non-player takeoff can land.
        let Some((ucid, player)) = self
            .ephemeral
            .players_by_slot
            .get(&slot)
            .and_then(|ucid| self.persisted.players.get_mut_cow(ucid).map(|p| (*ucid, p)))
        else {
            return Ok(TakeoffRes::NotPlayerSlot);
        };
        // Checked against the pilot's own points before anything is charged;
        // a lifeline flight costs nothing at all, so it can't be "out of
        // points" either.
        let lifeline = self
            .ephemeral
            .cfg
            .points
            .as_ref()
            .is_some_and(|p| p.lifeline_applies(player.points, &typ));
        if lifeline && let Some(points) = self.ephemeral.cfg.points.as_ref() {
            // The airframe is free; only weapons over the budget are charged.
            let airframe = points.airframe_cost.get(&typ).copied().unwrap_or(0);
            cost = points.lifeline_store_charge(cost.saturating_sub(airframe));
        }
        // Enforce the post-slot-entry takeoff hold.
        if let Some((_, Some(inst))) = &player.current_slot {
            if let Some(ok_at) = inst.takeoff_ok_at {
                if time < ok_at {
                    return Ok(TakeoffRes::TooEarly((ok_at - time).num_seconds().max(1)));
                }
            }
        }
        let owned_objective = self
            .persisted
            .objectives
            .into_iter()
            .find_map(|(oid, obj)| {
                if obj.owner == player.side && obj.zone.contains(position) {
                    Some((oid, obj))
                } else {
                    None
                }
            });
        let life_type = match self.ephemeral.cfg.life_types.get(&sifo.typ) {
            None => bail!("no life type for vehicle {:?}", sifo.typ),
            Some(typ) => *typ,
        };
        if let Some((_, Some(inst))) = &mut player.current_slot {
            inst.landed_at_objective = None;
        }
        // A negative fund is just an empty one (see `split_charge`).
        let obj_balance = owned_objective.as_ref().map(|(_, o)| max(0, o.points)).unwrap_or(0);
        if strict && cost as i32 > max(0, player.points) + obj_balance {
            return Ok(TakeoffRes::OutOfPoints);
        }
        let took_life = if !self.ephemeral.cfg.limited_lives || owned_objective.is_none() {
            false
        } else {
            let (_, player_lives) = player.lives.get_or_insert_cow(life_type, || {
                (time, self.ephemeral.cfg.default_lives[&life_type].0)
            });
            // paranoia
            if *player_lives == 0 {
                return Ok(TakeoffRes::OutOfLives);
            }
            *player_lives -= 1;
            self.ephemeral.stat(Stat::Life {
                id: ucid,
                lives: player.lives.clone(),
            });
            true
        };
        player.airborne = Some(life_type);
        player.kill_streak = 0; // reset streak on new sortie
        // Record what this takeoff took so the landing hands back exactly
        // that. A takeoff that took nothing (from a road, an enemy field, a
        // stop at a neutral base) keeps whatever an earlier leg of the same
        // flight took -- that life is still owed back when the aircraft
        // comes home.
        let from = owned_objective.as_ref().map(|(id, _)| **id);
        let flight = player.flight.get_or_insert_with(FlightCharge::default);
        if took_life {
            flight.life = Some(life_type);
        }
        if flight.tally.took_off.is_none() {
            flight.tally.took_off = Some(time);
            flight.tally.from = from;
        }
        let side = player.side;
        self.ephemeral.dirty();
        let res = if took_life {
            Ok(TakeoffRes::TookLife(life_type))
        } else {
            Ok(TakeoffRes::NoLifeTaken)
        };
        if lifeline {
            info!("[POINTS] {ucid} took off on the lifeline in a {typ}");
        }
        if cost > 0
            && let Some(oid) = owned_objective.map(|(id, _)| *id)
        {
            let frac = self.charge_for_item(&ucid, oid, cost, cost_msg.as_str());
            if let Some(player) = self.persisted.players.get_mut_cow(&ucid) {
                player.flight.get_or_insert_with(FlightCharge::default).points =
                    Some(PointsCharge { side, oid, cost, frac });
            }
        };
        // One sortie per slot session, not per takeoff. A pilot who lands to
        // rearm and launches again in the same airframe is still flying the
        // same sortie, so only the first takeoff opens one -- the rest still
        // take a life and charge points above, they just do not mint a second
        // SortieId in bfdb. See `Ephemeral::open_sorties`.
        if self.ephemeral.open_sorties.insert(ucid, None).is_none() {
            self.ephemeral.stat(Stat::Takeoff { id: ucid });
        }
        res
    }

    pub fn charge_for_item(&mut self, ucid: &Ucid, oid: ObjectiveId, cost: u32, msg: &str) -> f32 {
        match self.player(ucid) {
            None => 1.,
            Some(player) => {
                let player_balance = player.points;
                let (adj, frac) = match self.persisted.objectives.get_mut_cow(&oid) {
                    None => (-(cost as i32), 1.),
                    Some(obj) => {
                        let (player_pays, obj_pays) = split_charge(player_balance, obj.points, cost);
                        obj.points -= obj_pays as i32;
                        let frac = if cost == 0 { 1. } else { player_pays as f32 / cost as f32 };
                        (-(player_pays as i32), frac)
                    }
                };
                self.adjust_points(&ucid, adj, msg);
                self.ephemeral.dirty();
                frac
            }
        }
    }

    pub fn refund_points(
        &mut self,
        ucid: &Ucid,
        oid: ObjectiveId,
        cost: u32,
        frac: f32,
        msg: &str,
    ) {
        // The objective's share only goes back while the refunding player's
        // side still holds it -- otherwise a -delete or troop return would pay
        // into the fund of whoever captured the base since.
        let side = self.persisted.players.get(ucid).map(|p| p.side);
        if let Some(obj) = self.persisted.objectives.get_mut_cow(&oid)
            && Some(obj.owner) == side
        {
            let cost = (cost as f32 * (1. - frac)).round() as i32;
            obj.points += cost;
        }
        let cost = (cost as f32 * frac).round() as i32;
        self.adjust_points(ucid, cost, msg);
    }

    /// Settle a recorded charge: hand back up to `amount` (never more than
    /// was charged) to the player and the objective fund in the proportion
    /// they paid. The fund's share is dropped if that objective is no longer
    /// held by the side that paid.
    pub fn refund_charge(
        &mut self,
        ucid: &Ucid,
        charge: &PointsCharge,
        amount: u32,
        msg: &str,
        announce: bool,
    ) {
        let (to_player, to_obj) = charge.refund_split(amount);
        if to_obj > 0
            && let Some(obj) = self.persisted.objectives.get_mut_cow(&charge.oid)
            && obj.owner == charge.side
        {
            obj.points += to_obj;
            self.ephemeral.dirty();
        }
        if announce {
            self.adjust_points(ucid, to_player, msg);
        } else {
            self.adjust_points_silent(ucid, to_player, msg);
        }
    }

    /// Count a weapon a pilot fired, for their landing debrief.
    pub fn note_shot(&mut self, ucid: &Ucid) {
        if let Some(p) = self.persisted.players.get_mut_cow(ucid)
            && let Some(f) = p.flight.as_mut()
        {
            f.tally.fired = f.tally.fired.saturating_add(1);
        }
    }

    /// Settle a landing and say what it settled. `None` only when the slot
    /// isn't a known player's.
    pub fn land(&mut self, slot: SlotId, position: Vector2, unit: &Unit) -> Option<LandReport> {
        // Record the touchdown before any of the bookkeeping below can bail
        // out. `player_deslot` reads this to decide whether the slot session
        // ended on the ground, so every landing has to be marked -- not just
        // the ones at an owned objective that hand a life back.
        if let Some(ucid) = self.ephemeral.players_by_slot.get(&slot).copied()
            && let Some(landed) = self.ephemeral.open_sorties.get_mut(&ucid)
        {
            *landed = Some(Utc::now());
        }
        let sifo = match self.ephemeral.slot_info.get(&slot) {
            Some(sifo) => sifo,
            None => return None,
        };
        let life_type = self.ephemeral.cfg.life_types.get(&sifo.typ).copied();
        let (cost, _, cost_msg) = match self.compute_flight_cost(&sifo, unit) {
            Ok(cost) => cost,
            Err(e) => {
                error!("failed to compute flight cost {e:?}");
                (0, false, String::from(""))
            }
        };
        let (ucid, player) = match self
            .ephemeral
            .players_by_slot
            .get(&slot)
            .and_then(|ucid| self.persisted.players.get_mut_cow(ucid).map(|p| (*ucid, p)))
        {
            Some(player) => player,
            None => return None,
        };
        let owned_objective = self.persisted.objectives.into_iter().find_map(|(oid, o)| {
            if o.owner == player.side && o.zone.contains(position) {
                Some(*oid)
            } else {
                None
            }
        });
        // The sortie is deliberately NOT closed here: the pilot is still in
        // the airframe and may rearm and launch again on the same sortie.
        // `player_deslot` emits `Stat::Land` once the slot session really ends.
        if let Some(oid) = owned_objective {
            player.airborne = None;
            // Hand back exactly what this flight's takeoff took, and nothing
            // it didn't -- see `FlightCharge`.
            let flight = player.flight.take();
            let mut returned = None;
            if let Some(life_type) = flight.as_ref().and_then(|f| f.life)
                && let Some((_, player_lives)) = player.lives.get_mut_cow(&life_type)
            {
                *player_lives = player_lives.saturating_add(1);
                if *player_lives >= self.ephemeral.cfg.default_lives[&life_type].0 {
                    player.lives.remove_cow(&life_type);
                }
                returned = Some(life_type);
            }
            if let Some((_, Some(inst))) = &mut player.current_slot {
                inst.position.p.x = position.x;
                inst.position.p.z = position.y;
                inst.landed_at_objective = Some(oid);
            }
            let lives = player.lives.clone();
            let (mut charged, mut refunded, mut banked) = (0, 0, 0);
            if let Some(points) = self.ephemeral.cfg.points.as_ref() {
                let is_provisional = points.provisional;
                let provisional_points = player.provisional_points;
                player.provisional_points = 0;
                // The refund is what is still on the aircraft (so expended
                // stores stay paid for), capped at what was actually charged
                // and returned to whoever paid it -- the fund charged at
                // departure, not the one landed at. Quietly: the debrief
                // says it, along with everything else.
                if let Some(charge) = flight.as_ref().and_then(|f| f.points) {
                    charged = charge.cost;
                    if cost > 0 {
                        refunded = charge.refund_split(cost).0;
                        self.refund_charge(&ucid, &charge, cost, cost_msg.as_str(), false);
                    }
                }
                if is_provisional && provisional_points > 0 {
                    banked = provisional_points;
                    self.adjust_points_silent(
                        &ucid,
                        provisional_points as i32,
                        "provisional points committed",
                    );
                }
            }
            self.ephemeral.dirty();
            if returned.is_some() && self.ephemeral.cfg.limited_lives {
                self.ephemeral.stat(Stat::Life { id: ucid, lives });
            }
            let points = self.persisted.players.get(&ucid).map(|p| p.points).unwrap_or(0);
            Some(LandReport {
                ucid,
                at: Some(oid),
                life_type,
                life_returned: returned.is_some() && self.ephemeral.cfg.limited_lives,
                charged,
                refunded,
                banked,
                pending: 0,
                points,
                tally: flight.map(|f| f.tally).unwrap_or_default(),
            })
        } else {
            let flight = player.flight.as_ref();
            Some(LandReport {
                ucid,
                at: None,
                life_type,
                life_returned: false,
                charged: flight.and_then(|f| f.points).map(|c| c.cost).unwrap_or(0),
                refunded: 0,
                banked: 0,
                pending: player.provisional_points,
                points: player.points,
                tally: flight.map(|f| f.tally.clone()).unwrap_or_default(),
            })
        }
    }

    /// Restore one life of the given type to a player (e.g. after CSAR delivery).
    /// If the player is already at max for that tier, cascade down via `LifeType::down()`.
    /// Returns the new life count, or None if nothing to restore.
    pub fn restore_life(&mut self, ucid: &Ucid, life_type: LifeType) -> Option<u8> {
        if !self.ephemeral.cfg.limited_lives {
            return None;
        }
        // Find the first tier (starting from life_type, cascading down) that has a deficit
        let mut current = life_type;
        let (current, max_lives) = loop {
            let max = self.ephemeral.cfg.default_lives.get(&current).map(|(n, _)| *n);
            let has_deficit = self
                .persisted
                .players
                .get(ucid)
                .map(|p| p.lives.get(&current).is_some())
                .unwrap_or(false);
            match (max, has_deficit) {
                (Some(max), true) => break (current, max),
                _ => match current.down() {
                    Some(lower) => current = lower,
                    None => return None,
                },
            }
        };
        let player = self.persisted.players.get_mut_cow(ucid)?;
        let new_count = match player.lives.get_mut_cow(&current) {
            None => return None,
            Some((_, count)) => {
                *count = (*count + 1).min(max_lives);
                *count
            }
        };
        if new_count >= max_lives {
            player.lives.remove_cow(&current);
        }
        self.ephemeral.dirty();
        Some(new_count)
    }

    pub fn maybe_reset_lives(&mut self, ucid: &Ucid, now: DateTime<Utc>) -> Result<()> {
        let mut lt_to_reset: SmallVec<[LifeType; 2]> = smallvec![];
        let player = self
            .persisted
            .players
            .get_mut_cow(ucid)
            .ok_or_else(|| anyhow!("no such player {:?}", ucid))?;
        for (lt, (reset, _n)) in player.lives.into_iter() {
            let reset_after = Duration::seconds(
                maybe!(self.ephemeral.cfg.default_lives, lt, "default life")?.1 as i64,
            );
            if now - reset >= reset_after {
                lt_to_reset.push(*lt);
            }
        }
        let mut reset = false;
        for lt in lt_to_reset {
            player.lives.remove_cow(&lt);
            reset = true;
            self.ephemeral.dirty();
        }
        if reset {
            self.ephemeral.stat(Stat::Life {
                id: *ucid,
                lives: player.lives.clone(),
            });
        }
        Ok(())
    }

    pub fn try_occupy_slot(
        &mut self,
        time: DateTime<Utc>,
        slot_side: Side,
        slot: SlotId,
        ucid: &Ucid,
    ) -> SlotAuth {
        let player = match self.persisted.players.get_mut_cow(ucid) {
            Some(player) => player,
            None => {
                if slot.is_spectator() {
                    return SlotAuth::Yes(None);
                }
                return SlotAuth::NotRegistered(slot_side);
            }
        };
        if slot.is_spectator() {
            player.jtac_or_spectators = true;
            return SlotAuth::Yes(None);
        }
        // With sides unlocked, taking the other side's slot IS a side switch,
        // so it spends one exactly as `-switch` does. It used to flip
        // `player.side` for free, which made `side_switches` meaningless and
        // never told bfdb. The side is flipped first because the slot checks
        // below are made against it, and put back if the slot is refused so a
        // rejected slot doesn't cost a switch.
        let switched_from = if slot_side != player.side {
            if self.ephemeral.cfg.lock_sides {
                return SlotAuth::ObjectiveNotOwned(player.side);
            }
            if let Some(0) = player.side_switches {
                return SlotAuth::ObjectiveNotOwned(player.side);
            }
            let from = player.side;
            player.side = slot_side;
            Some(from)
        } else {
            None
        };
        let auth = match slot {
            SlotId::Spectator => unreachable!(),
            SlotId::Instructor(_, _) => {
                if self.ephemeral.cfg.admins.contains_key(ucid) {
                    player.jtac_or_spectators = true;
                    SlotAuth::Yes(None)
                } else {
                    SlotAuth::Denied
                }
            }
            SlotId::ArtilleryCommander(_, _)
            | SlotId::ForwardObserver(_, _)
            | SlotId::Observer(_, _) => {
                if self.ephemeral.cfg.rules.ca.check(ucid) {
                    player.jtac_or_spectators = true;
                    SlotAuth::Yes(None)
                } else {
                    SlotAuth::Denied
                }
            }
            SlotId::Unit(_) | SlotId::MultiCrew(_, _) => {
                if self.ephemeral.slot_info.contains_key(&slot) {
                    self.try_occupy_slot_deferred(time, ucid, slot)
                } else {
                    player.changing_slots = true;
                    player.jtac_or_spectators = false;
                    SlotAuth::Yes(None)
                }
            }
        };
        if let Some(from) = switched_from
            && let Some(player) = self.persisted.players.get_mut_cow(ucid)
        {
            if matches!(auth, SlotAuth::Yes(_)) {
                if let Some(n) = &mut player.side_switches {
                    *n = n.saturating_sub(1);
                }
                // A flight record from the old side must not be settled
                // against the new side's bases.
                player.flight = None;
                self.ephemeral.stat(Stat::Sideswitch { id: *ucid, side: slot_side });
                self.ephemeral.dirty();
            } else {
                player.side = from;
            }
        }
        auth
    }

    pub fn try_occupy_slot_deferred(
        &mut self,
        time: DateTime<Utc>,
        ucid: &Ucid,
        slot: SlotId,
    ) -> SlotAuth {
        let sifo = match self.ephemeral.slot_info.get(&slot) {
            None => return SlotAuth::Denied,
            Some(sifo) => sifo,
        };
        let player = match self.persisted.players.get_mut_cow(ucid) {
            Some(player) => player,
            None => {
                if slot.is_spectator() {
                    return SlotAuth::Yes(None);
                }
                return SlotAuth::NotRegistered(sifo.side);
            }
        };
        let objective = match self.persisted.objectives.get(&sifo.objective) {
            Some(o) if o.owner != Side::Neutral => o,
            Some(_) | None => return SlotAuth::ObjectiveNotOwned(player.side),
        };
        if objective.owner != player.side {
            return SlotAuth::ObjectiveNotOwned(player.side);
        }
        if objective.in_capture_hold() {
            let total = self.ephemeral.cfg.capture_consolidation_secs;
            let remaining = objective.capture_hold_pct(total).map(|(_, s)| s).unwrap_or(0);
            return SlotAuth::Consolidating(remaining);
        }
        if objective.captureable() {
            return SlotAuth::ObjectiveHasNoLogistics;
        }
        let life_type = self.ephemeral.cfg.life_types[&sifo.typ];
        macro_rules! yes {
            () => {
                if let Some(whcfg) = self.ephemeral.cfg.warehouse.as_ref() {
                    let typ = sifo.typ.as_str();
                    if !whcfg.exempt_airframes.contains(typ) {
                        match objective.warehouse.equipment.get(typ) {
                            Some(inv) if inv.stored > 0 => (),
                            Some(_) | None => {
                                break SlotAuth::VehicleNotAvailable(sifo.typ.clone());
                            }
                        }
                    }
                    // An objective can end up holding an aircraft type its
                    // own side doesn't produce: kept from the previous owner
                    // on capture so the captors can operate it (see
                    // capture_warehouse -- the carrier branch, and the land
                    // salvage branch under `captured_airframes`). Captured
                    // aircraft aren't flyable the moment the last defender
                    // dies; the objective has to be consolidated and repaired
                    // first.
                    let is_own_roster = self
                        .ephemeral
                        .production_by_side
                        .get(&objective.owner)
                        .map(|p| p.equipment.contains_key(typ))
                        .unwrap_or(true);
                    if !is_own_roster {
                        let min_health =
                            if matches!(objective.kind, ObjectiveKind::CarrierGroup { .. }) {
                                Some(100)
                            } else {
                                whcfg.captured_airframes.as_ref().map(|c| c.min_health)
                            };
                        if let Some(min) = min_health {
                            if objective.health < min {
                                break SlotAuth::CapturedNotReady(sifo.typ.clone());
                            }
                        }
                    }
                }
                player.changing_slots = false;
                player.jtac_or_spectators = false;
                break SlotAuth::Yes(Some(stats::Unit {
                    typ: sifo.typ.clone(),
                    tags: self
                        .ephemeral
                        .cfg
                        .unit_classification
                        .get(&sifo.typ)
                        .map(|t| *t)
                        .unwrap_or_default(),
                }));
            };
        }
        if let Some(points) = self.ephemeral.cfg.points.as_ref() {
            let cost = *points.airframe_cost.get(&sifo.typ).unwrap_or(&0) as i32;
            // The same pot `takeoff` lets pay for the flight. Neither side of
            // it counts below zero: a fund left negative by an older save (or
            // a pilot's own debt) used to be netted against the other and
            // lock out players who could in fact cover the airframe.
            let balance = max(0, player.points) + max(0, objective.points);
            if cost > 0 && balance < cost && !points.lifeline_applies(player.points, &sifo.typ) {
                return SlotAuth::NoPoints {
                    cost: cost as u32,
                    vehicle: sifo.typ.clone(),
                    balance,
                };
            }
        }
        if let Some(era_cfg) = &self.ephemeral.cfg.era {
            let allowed = era_cfg.eras.get(era_cfg.current.as_str()).map(|v| v.as_slice()).unwrap_or(&[]);
            if !allowed.is_empty() && !allowed.contains(&sifo.typ) {
                return SlotAuth::EraRestricted {
                    vehicle: sifo.typ.clone(),
                    era: compact_str::format_compact!("{}", era_cfg.current),
                };
            }
        }
        loop {
            match player.lives.get(&life_type).map(|t| *t) {
                None => {
                    yes!();
                }
                Some((reset, n)) => {
                    let reset_after =
                        Duration::seconds(self.ephemeral.cfg.default_lives[&life_type].1 as i64);
                    if time - reset >= reset_after {
                        player.lives.remove_cow(&life_type);
                        self.ephemeral.stat(Stat::Life {
                            id: *ucid,
                            lives: player.lives.clone(),
                        });
                        self.ephemeral.dirty = true;
                    } else if n == 0 {
                        break SlotAuth::NoLives(life_type);
                    }
                    yes!();
                }
            }
        }
    }

    pub fn player_connected(&mut self, ucid: Ucid, name: String) {
        if let Some(player) = self.persisted.players.get(&ucid) {
            if player.current_slot.is_some() {
                self.player_deslot(&ucid)
            }
        }
        if let Some(player) = self.persisted.players.get_mut_cow(&ucid) {
            if player.name != name {
                player.alts.insert(name.clone());
                player.name = name;
                self.ephemeral.dirty()
            }
        }
        // Register only fires once, the first time a ucid is ever seen, so a
        // returning player reconnecting into a new round would otherwise never
        // get a side recorded for that round in bfdb, leaving them out of the
        // per-round registered/online counts. Reaffirm their known side on
        // every connect so bfdb always has a current-round side for them.
        if let Some(player) = self.persisted.players.get(&ucid) {
            self.ephemeral.stat(Stat::Sideswitch { id: ucid, side: player.side });
        }
    }

    pub fn register_player(&mut self, ucid: Ucid, name: String, side: Side) -> Result<(), RegErr> {
        match self.persisted.players.get(&ucid) {
            Some(p) if p.side != side => Err(RegErr::AlreadyRegistered(p.side_switches, p.side)),
            Some(_) => Err(RegErr::AlreadyOn(side)),
            None => {
                // Late joiners start at a share of what their side's pilots
                // typically hold, not at the bare join grant.
                let (points, untransferable) = self.late_joiner_start(side);
                if untransferable > 0 {
                    info!(
                        "[ECONOMY] {name} joins {side:?} with {points} points ({untransferable} late-joiner start)"
                    );
                }
                self.persisted.players.insert_cow(
                    ucid,
                    Player {
                        name: name.clone(),
                        alts: SetS::from_iter([name.clone()]),
                        side,
                        side_switches: self.ephemeral.cfg.side_switches,
                        lives: MapS::new(),
                        crates: SetS::new(),
                        airborne: None,
                        points,
                        provisional_points: 0,
                        current_slot: None,
                        changing_slots: false,
                        jtac_or_spectators: true,
                        ai_team_kills: SetS::new(),
                        player_team_kills: MapS::new(),
                        kill_streak: 0,
                        total_kills: 0,
                        flight: None,
                        untransferable,
                    },
                );
                self.ephemeral.stat(Stat::Register {
                    initial_points: points,
                    name,
                    side,
                    id: ucid,
                });
                self.ephemeral.dirty();
                Ok(())
            }
        }
    }

    pub fn force_sideswitch_player(&mut self, ucid: &Ucid, side: Side) -> Result<()> {
        let player = maybe_mut!(self.persisted.players, ucid, "no such player")?;
        player.side = side;
        player.flight = None;
        self.ephemeral.stat(Stat::Sideswitch { id: *ucid, side });
        self.ephemeral.dirty();
        Ok(())
    }

    pub fn sideswitch_player(&mut self, ucid: &Ucid, side: Side) -> Result<(), &'static str> {
        match self.persisted.players.get_mut_cow(ucid) {
            None => Err("You are not registered. Take a slot on the side you want to fly"),
            Some(player) => {
                if side == player.side {
                    Err("you are already on the requested side")
                } else if let Some(0) = player.side_switches {
                    Err("you can't switch sides again this round")
                } else if side == Side::Neutral {
                    Err("you can't switch to neutral")
                } else {
                    match &mut player.side_switches {
                        Some(n) => {
                            *n -= 1;
                        }
                        None => (),
                    }
                    player.side = side;
                    // see try_occupy_slot
                    player.flight = None;
                    self.ephemeral.stat(Stat::Sideswitch { id: *ucid, side });
                    self.ephemeral.dirty();
                    Ok(())
                }
            }
        }
    }

    pub fn update_player_positions<'a>(
        &mut self,
        lua: MizLua,
        now: DateTime<Utc>,
        ids: impl IntoIterator<Item = &'a Ucid>,
    ) -> Result<Vec<DcsOid<ClassUnit>>> {
        let mut dead: Vec<DcsOid<ClassUnit>> = vec![];
        let mut unit: Option<Unit> = None;
        let coord = Coord::singleton(lua)?;
        for ucid in ids {
            let mut inform_cost = None;
            if let Some(player) = self.persisted.players.get_mut_cow(ucid) {
                if let Some((slot, Some(inst))) = &mut player.current_slot {
                    if let Some(id) = self.ephemeral.object_id_by_slot.get(slot) {
                        let instance = match unit.take() {
                            Some(unit) => unit.change_instance(id),
                            None => Unit::get_instance(lua, id),
                        };
                        let instance = match instance {
                            Ok(i) => Ok(i),
                            Err(_) => {
                                // Routine: a player who left the slot between the poll
                                // starting and this lookup has no unit by id
                                // any more. The by-name fallback usually finds
                                // it, and when it does not the skip below says
                                // so -- this was the middle of three warnings
                                // for one benign race.
                                debug!("failed to get unit by id, trying by name");
                                Unit::get_by_name(lua, &inst.unit_name)
                            }
                        };
                        match instance {
                            Ok(instance) => {
                                let pos = instance.get_position()?;
                                if (inst.position.p.0 - pos.p.0).magnitude_squared() > 1.0 {
                                    if inst.stopped_at_objective {
                                        inform_cost = Some((*slot, player.points));
                                    }
                                    inst.stopped_at_objective = false;
                                    inst.position = pos;
                                    inst.velocity = instance.get_velocity()?.0;
                                    inst.in_air = instance.in_air()?;
                                    inst.moved = Some(now);
                                } else if inst.landed_at_objective.is_some() {
                                    inst.stopped_at_objective = true;
                                }
                                unit = Some(instance);
                                self.ephemeral.stat(Stat::Position {
                                    id: EnId::Player(*ucid),
                                    pos: stats::Pos {
                                        pos: coord.lo_to_ll(inst.position.p)?,
                                        velocity: inst.velocity,
                                    },
                                });
                            }
                            Err(e) => {
                                // a shot-down player's unit is gone before the
                                // Dead event arrives; the kill is still recorded
                                info!(
                                    "updating player positions, skipping invalid unit {ucid:?}, {id:?}, player {e:?}",
                                );
                                dead.push(id.clone())
                            }
                        }
                    }
                }
            }
            if let (Some(unit), Some((slot, balance))) = (&unit, inform_cost) {
                let sifo = self
                    .ephemeral
                    .slot_info
                    .get(&slot)
                    .ok_or_else(|| anyhow!("could not find slot {:?}", slot))?;
                // The base the pilot is taxiing out of pays whatever their
                // own points don't -- `takeoff` counts it, so the warning has
                // to as well or it cries wolf at every well-funded base.
                let side = self.persisted.players.get(ucid).map(|p| p.side);
                let pos = Vector2::new(unit.get_point()?.x, unit.get_point()?.z);
                let fund = self
                    .persisted
                    .objectives
                    .into_iter()
                    .find(|(_, o)| Some(o.owner) == side && o.zone.contains(pos))
                    .map(|(_, o)| (o.name.clone(), o.points));
                let fund = fund.as_ref().map(|(n, p)| (n.as_str(), *p));
                if let Some(m) = self.flight_cost_panel(sifo, &unit, balance, fund)? {
                    self.ephemeral.panel_to_player(&self.persisted, 60, ucid, m)
                }
            }
        }
        Ok(dead)
    }

    pub fn update_player_positions_incremental(
        &mut self,
        lua: MizLua,
        now: DateTime<Utc>,
        i: usize,
    ) -> Result<(usize, Vec<DcsOid<ClassUnit>>)> {
        let total = self.ephemeral.players_by_slot.len();
        if i < total {
            let stop = min(total, i + max(1, total / 10));
            let players: SmallVec<[Ucid; 64]> = self.ephemeral.players_by_slot.as_slice()[i..stop]
                .into_iter()
                .map(|(_, ucid)| *ucid)
                .collect();
            let dead = self.update_player_positions(lua, now, &players)?;
            Ok((stop, dead))
        } else {
            Ok((0, vec![]))
        }
    }

    pub fn player_entered_slot(
        &mut self,
        lua: MizLua,
        id: DcsOid<ClassUnit>,
        unit: &Unit,
        slot: SlotId,
        oid: ObjectiveId,
        ucid: Ucid,
    ) -> Result<()> {
        if let Some(old_ucid) = self.ephemeral.players_by_slot.get(&slot) {
            let old_ucid = *old_ucid;
            if old_ucid != ucid {
                self.player_deslot(&old_ucid)
            }
        }
        let obj = objective_mut!(self, oid)?;
        let sifo = maybe!(self.ephemeral.slot_info, slot, "slot")?;
        let player = maybe!(self.persisted.players, ucid, "player")?;
        let life_typ = self.ephemeral.cfg.life_types[&sifo.typ];
        match player.lives.get(&life_typ) {
            Some((_, n)) if *n == 0 => {
                info!("player {ucid} has no lives for this unit type");
                self.player_deslot(&ucid);
                unit.clone().destroy()?;
                return Ok(());
            }
            None | Some((_, _)) => (),
        }
        self.ephemeral.players_by_slot.insert(slot, ucid);
        self.ephemeral
            .slot_by_object_id
            .insert(id.clone(), slot.clone());
        self.ephemeral
            .object_id_by_slot
            .insert(slot.clone(), id.clone());
        let mut adjust_warehouse = || -> Result<()> {
            let id = maybe!(self.ephemeral.airbase_by_oid, obj.id, "airbase")?;
            let wh = Airbase::get_instance(lua, id)
                .context("getting airbase")?
                .get_warehouse()
                .context("getting warehouse")?;
            let mut drawn: SmallVec<[CompactString; 8]> = smallvec![];
            if sifo.ground_start {
                wh.remove_item(sifo.typ.0.clone(), 1)
                    .with_context(|| format_compact!("removing {} from warehouse", sifo.typ.0))?;
                for wep in unit.get_ammo()? {
                    let wep = wep?;
                    let count = wep.count()?;
                    let typ = wep.type_name()?;
                    let whcnt = wh.get_item_count(typ.clone())?;
                    drawn.push(format_compact!("{typ} x{count} (had {whcnt})"));
                    wh.remove_item(typ.clone(), count)?;
                    if let Some(inv) = obj.warehouse.equipment.get_mut_cow(&typ) {
                        // DCS can report fewer in stock than the loadout
                        // carries; u32 subtraction would wrap to ~4 billion.
                        inv.stored = whcnt.saturating_sub(count);
                    }
                }
                // Debit the fuel it launches with, so landing it somewhere else
                // is a real transfer of the airframe *and* its fuel, not free
                // supply appearing at the destination (see player_left_unit).
                if let Ok(frac) = unit.get_fuel() {
                    let max_kg = unit
                        .get_desc()
                        .ok()
                        .and_then(|d| d.raw_get::<_, f64>("fuelMassMax").ok())
                        .unwrap_or(0.0);
                    let kg = (frac.clamp(0.0, 1.0) as f64 * max_kg).round() as u32;
                    if kg > 0 {
                        drawn.push(format_compact!("jet fuel {kg} kg"));
                        let have = wh
                            .get_liquid_amount(dcso3::warehouse::LiquidType::JetFuel)
                            .unwrap_or(0);
                        let _ = wh.remove_liquid(
                            dcso3::warehouse::LiquidType::JetFuel,
                            kg.min(have),
                        );
                        if let Some(inv) = obj
                            .warehouse
                            .liquids
                            .get_mut_cow(&dcso3::warehouse::LiquidType::JetFuel)
                        {
                            inv.stored = wh
                                .get_liquid_amount(dcso3::warehouse::LiquidType::JetFuel)
                                .unwrap_or(inv.stored);
                        }
                    }
                }
            }
            let left = wh
                .get_item_count(sifo.typ.0.clone())
                .with_context(|| format_compact!("getting warehouse count for {}", sifo.typ.0))?;
            // Ground starts are the campaign's main consumption path -- every
            // sortie is an airframe, a loadout and a tank of fuel out of that
            // base. Log the draw so a base running dry can be traced back to
            // what actually drained it.
            if sifo.ground_start {
                info!(
                    "[WAREHOUSE_DRAW] {ucid} took a {} from {} ({left} left); stores: {}",
                    sifo.typ.0,
                    obj.name,
                    if drawn.is_empty() {
                        CompactString::from("none")
                    } else {
                        CompactString::from(drawn.join(", "))
                    }
                );
            } else {
                debug!(
                    "[WAREHOUSE_DRAW] {ucid} slotted an air-start {} at {} -- nothing drawn",
                    sifo.typ.0, obj.name
                );
            }
            maybe_mut!(obj.warehouse.equipment, sifo.typ.0, "equip")?.stored = left;
            Ok(())
        };
        if let Err(e) = adjust_warehouse() {
            error!("couldn't adjust warehouse {:?}", e)
        }
        let player = maybe_mut!(self.persisted.players, ucid, "player")?;
        let player_side = player.side;
        let slot_uid = slot.as_unit_id();
        let position = unit.get_position()?;
        let point = Vector2::new(position.p.x, position.p.z);
        let landed_at_objective = self
            .persisted
            .objectives
            .into_iter()
            .find(|(_, obj)| obj.zone.contains(point))
            .map(|(oid, _)| *oid);
        let in_air = unit.in_air()?;
        // Ground-start slots get a hold before they may get airborne; air starts
        // are already flying, so there's nothing to hold.
        let takeoff_ok_at = match self.ephemeral.cfg.takeoff_delay_secs {
            secs if secs > 0 && !in_air => {
                Some(Utc::now() + Duration::seconds(secs as i64))
            }
            _ => None,
        };
        player.current_slot = Some((
            slot,
            Some(InstancedPlayer {
                unit_name: unit.get_name()?,
                position,
                velocity: unit.get_velocity()?.0,
                in_air,
                typ: Vehicle::from(unit.get_type_name()?),
                landed_at_objective,
                stopped_at_objective: true,
                moved: None,
                takeoff_ok_at,
            }),
        ));
        player.changing_slots = false;
        player.provisional_points = 0;
        // Slot-entry GCI radio briefing ("GCI: Magic on 251.0 AM (SRS)").
        let gci_brief = self
            .ephemeral
            .cfg
            .gci_briefing
            .as_ref()
            .and_then(|b| b.render(player_side).map(|t| (t, b.display_secs)));
        if let (Some((text, secs)), Some(uid)) = (gci_brief, slot_uid) {
            self.ephemeral
                .msgs()
                .panel_to_unit(secs, false, uid, String::from(text));
        }
        self.ephemeral.dirty();
        Ok(())
    }

    pub fn player_left_unit(
        &mut self,
        lua: MizLua,
        now: DateTime<Utc>,
        objid: &DcsOid<ClassUnit>,
    ) -> Result<Vec<DcsOid<ClassUnit>>> {
        let mut dead = vec![];
        if let Some(uid) = self.ephemeral.uid_by_object_id.get(objid) {
            let uid = *uid;
            match self.update_unit_positions(lua, now, &[uid]) {
                Ok(v) => dead = v,
                Err(e) => error!("could not sync final CA unit position {e}"),
            }
            self.ephemeral.units_able_to_move.swap_remove(&uid);
        }
        if let Some(slot) = self.ephemeral.slot_by_object_id.get(&objid).cloned() {
            if let Some(ucid) = self.ephemeral.player_in_slot(&slot).cloned() {
                let player_side = self.persisted.players.get(&ucid).map(|p| p.side);
                // Only ground-start slots have their airframe / stores debited
                // from the departure base (see adjust_warehouse), so only they
                // are credited back on landing.
                let ground_start = self
                    .ephemeral
                    .slot_info
                    .get(&slot)
                    .map(|s| s.ground_start)
                    .unwrap_or(false);
                let player = maybe_mut!(self.persisted.players, ucid, "player")?;
                if let Some((_, Some(inst))) = player.current_slot.as_mut() {
                    let typ = inst.typ.clone();
                    if let Some(oid) = inst.landed_at_objective {
                        let mut fix_warehouse = || -> Result<()> {
                            let obj = objective_mut!(self, oid).context("get objective")?;
                            // The base can change hands while a player sits
                            // parked in it. `landed_at_objective` was friendly
                            // when it was set; re-check now so a captured base
                            // doesn't absorb the departing (enemy) player's
                            // airframe and stores into what is now the new
                            // owner's warehouse.
                            if player_side != Some(obj.owner()) {
                                return Ok(());
                            }
                            // A zone-only FOB or command center, or a carrier
                            // whose deck mapping went with a capture, has no
                            // DCS warehouse: there is nothing to credit, and
                            // takeoff from there debited nothing either. This
                            // used to log "unable to fix warehouse no such
                            // airbase ObjectiveId(N)" as an ERROR every time.
                            let Some(id) = self.ephemeral.airbase_by_oid.get(&oid).cloned() else {
                                debug!("{} has no DCS warehouse, nothing to credit for {typ}", obj.name);
                                return Ok(());
                            };
                            let airbase = Airbase::get_instance(lua, &id).context("get airbase")?;
                            let wh = airbase.get_warehouse().context("get warehouse")?;
                            let mut sync: SmallVec<[String; 4]> = smallvec![typ.0.clone()];
                            // Return the airframe + its remaining stores to the
                            // base it landed at -- for every objective type
                            // (FARP, FOB, airbase, naval). This is the credit
                            // half of the takeoff debit in adjust_warehouse.
                            if ground_start && let Ok(unit) = Unit::get_instance(lua, &objid) {
                                wh.add_item(typ.0.clone(), 1)?;
                                for ammo in unit.get_ammo().context("get ammo")? {
                                    let ammo = ammo.context("ammo")?;
                                    let count = ammo.count().context("ammo count")?;
                                    let atyp = ammo.type_name().context("ammo typ")?;
                                    sync.push(atyp.clone());
                                    wh.add_item(atyp, count).context("add item to warehouse")?;
                                }
                                // Remaining internal fuel comes back as jet fuel.
                                if let Ok(frac) = unit.get_fuel() {
                                    let max_kg = unit
                                        .get_desc()
                                        .ok()
                                        .and_then(|d| d.raw_get::<_, f64>("fuelMassMax").ok())
                                        .unwrap_or(0.0);
                                    let kg = (frac.clamp(0.0, 1.0) as f64 * max_kg).round() as u32;
                                    if kg > 0 {
                                        let _ = wh.add_liquid(
                                            dcso3::warehouse::LiquidType::JetFuel,
                                            kg,
                                        );
                                        if let Some(inv) = obj
                                            .warehouse
                                            .liquids
                                            .get_mut_cow(&dcso3::warehouse::LiquidType::JetFuel)
                                        {
                                            inv.stored = wh
                                                .get_liquid_amount(
                                                    dcso3::warehouse::LiquidType::JetFuel,
                                                )
                                                .unwrap_or(inv.stored);
                                        }
                                    }
                                }
                            }
                            for typ in sync {
                                if let Some(inv) = obj.warehouse.equipment.get_mut_cow(&typ) {
                                    inv.stored = wh.get_item_count(typ).context("getting item")?;
                                    self.ephemeral.dirty();
                                }
                            }
                            Ok(())
                        };
                        if let Err(e) = fix_warehouse() {
                            error!("unable to fix warehouse {:?}", e)
                        }
                    }
                }
                self.return_troops_on_deslot(&slot);
                self.player_deslot(&ucid)
            }
        }
        Ok(dead)
    }

    pub fn player_disconnected(&mut self, ucid: &Ucid) {
        if let Some((_, Some(inst))) = self
            .persisted
            .players
            .get(&ucid)
            .and_then(|p| p.current_slot.as_ref())
        {
            if let Some(oid) = inst.landed_at_objective {
                self.ephemeral.push_sync_warehouse(oid, inst.typ.clone());
            }
        }
        self.ephemeral.stat(Stat::Disconnect { id: *ucid });
        if let Some(slot) = self.persisted.players.get(ucid).and_then(|p| p.current_slot.as_ref().map(|(s, _)| s.clone())) {
            self.return_troops_on_deslot(&slot);
        }
        self.player_deslot(ucid);
    }

    fn apply_teamkill_penalty(
        &mut self,
        shooter: Ucid,
        total_points: u32,
        victim_info: &Option<VictimInfo>,
    ) -> CompactString {
        let player = &mut self.persisted.players[&shooter];
        let window = self
            .ephemeral
            .cfg
            .points
            .as_ref()
            .map(|p| p.tk_window as i64)
            .unwrap_or(0);
        let now = Utc::now();
        player.prune_team_kills(now, window);
        match victim_info.as_ref() {
            None => {
                let penalty = ai_tk_penalty(player.ai_team_kills.into_iter().copied(), now, window, total_points);
                let total_points = total_points.saturating_add(penalty);
                player.points -= total_points as i32;
                player.ai_team_kills.insert_cow(now);
                let tp = player.points;
                format_compact!("{tp}(-{total_points}) points, you have killed a friendly unit")
            }
            Some(VictimInfo {
                name,
                life_type: None,
                ai_deployable: false,
                ucid: _,
            }) => {
                player.points -= total_points as i32;
                format_compact!(
                    "{}(-{total_points})you have team killed {name} on the ground",
                    player.points
                )
            }
            Some(VictimInfo {
                name,
                life_type: None,
                ai_deployable: true,
                ucid: _,
            }) => {
                player.points -= total_points as i32;
                format_compact!(
                    "{}(-{total_points})you have team killed {name}'s ai unit",
                    player.points
                )
            }
            Some(VictimInfo {
                ucid,
                name,
                life_type: Some(life_type),
                ..
            }) => {
                let (penalty_points, penalty_lives) = player_tk_penalty(
                    player.player_team_kills.into_iter().map(|(ts, _)| *ts),
                    now,
                    window,
                    total_points,
                );
                let deplane_possible = penalty_lives > 1.5;
                let mut penalty_lives = penalty_lives.round() as u32;
                let mut lost: SmallVec<[(LifeType, u8); 5]> = smallvec![];
                let mut life_type = *life_type;
                let deplane = loop {
                    let (_, player_lives) = player.lives.get_or_insert_cow(life_type, || {
                        (Utc::now(), self.ephemeral.cfg.default_lives[&life_type].0)
                    });
                    if *player_lives as u32 >= penalty_lives {
                        lost.push((life_type, penalty_lives as u8));
                        *player_lives -= penalty_lives as u8;
                        break false;
                    } else {
                        if *player_lives > 0 {
                            lost.push((life_type, *player_lives));
                        }
                        penalty_lives -= *player_lives as u32;
                        *player_lives = 0;
                        match life_type.up() {
                            None => break deplane_possible,
                            Some(lt) => {
                                life_type = lt;
                            }
                        }
                    }
                };
                self.ephemeral.stat(Stat::Life {
                    id: shooter,
                    lives: player.lives.clone(),
                });
                player.points -= penalty_points as i32;
                player.player_team_kills.insert_cow(now, *ucid);
                let tp = player.points;
                self.ephemeral.dirty();
                use std::fmt::Write;
                let mut msg = CompactString::from("");
                write!(
                    msg,
                    "{tp}(-{penalty_points}) points, you have team killed {name}.\n",
                )
                .unwrap();
                if lost.len() > 0 {
                    write!(msg, "\nYou have lost\n").unwrap();
                    for (ty, n) in lost {
                        write!(msg, "{n} {ty} life\n").unwrap()
                    }
                };
                if deplane {
                    write!(msg, "Shortly you will be deplaned\n").unwrap();
                    write!(
                        msg,
                        "your death may be monitored for quality assurance purposes\n"
                    )
                    .unwrap();
                    write!(msg, "have a nice day").unwrap();
                    self.ephemeral
                        .force_player_to_spectators_at(&shooter, now + Duration::seconds(30));
                }
                msg
            }
        }
    }

    pub fn award_kill_points(&mut self, cfg: &PointsCfg, dead: &Dead) {
        let econ = self.ephemeral.cfg.economy.clone();
        let valid_shots = || {
            // why are you hitting yourself
            dead.shots
                .iter()
                .filter(|shot| match (&shot.shooter, &shot.target) {
                    (Who::AI { gid: g0, .. }, Who::AI { gid: g1, .. }) => g0 != g1,
                    (Who::Player { ucid: u0, .. }, Who::Player { ucid: u1, .. }) => u0 != u1,
                    (
                        Who::AI {
                            ucid: Some(u0),
                            side: s0,
                            ..
                        },
                        Who::Player {
                            side: s1, ucid: u1, ..
                        },
                    ) => u0 != u1 && s0 != s1,
                    (Who::Player { ucid: u1, .. }, Who::AI { ucid: Some(u0), .. }) => u0 != u1,
                    (Who::AI { .. }, Who::Player { .. }) | (Who::Player { .. }, Who::AI { .. }) => {
                        true
                    }
                })
        };
        // Who gets credit, keyed by (player, credited through their deployed
        // AI), weighted by hits; the killing blow counts extra. It used to be
        // `ceil(total / shooters)` each, which minted points on every shared
        // kill and paid a single gun hit the same as the missile that killed.
        let credit_key = |shooter: &Who| match shooter {
            Who::Player { ucid, .. } => Some((*ucid, false)),
            Who::AI { ucid: Some(ucid), .. } => Some((*ucid, true)),
            Who::AI { ucid: None, .. } => None,
        };
        fn add_credit(
            credit: &mut SmallVec<[((Ucid, bool), f64); 8]>,
            k: (Ucid, bool),
            w: f64,
        ) {
            match credit.iter_mut().find(|(c, _)| *c == k) {
                Some(e) => e.1 += w,
                None => credit.push((k, w)),
            }
        }
        let mut credit: SmallVec<[((Ucid, bool), f64); 8]> = smallvec![];
        let mut last_hit: Option<((Ucid, bool), DateTime<Utc>)> = None;
        for shot in valid_shots().filter(|s| s.hit) {
            let Some(k) = credit_key(&shot.shooter) else { continue };
            add_credit(&mut credit, k, 1.);
            if last_hit.map_or(true, |(_, t)| shot.time >= t) {
                last_hit = Some((k, shot.time));
            }
        }
        if let Some((k, _)) = last_hit {
            add_credit(&mut credit, k, econ.killing_blow_weight.max(0.));
        }
        if credit.is_empty() {
            // Nobody registered a hit: everyone who fired at it in the last
            // three minutes shares it evenly.
            for shot in valid_shots() {
                let Some(k) = credit_key(&shot.shooter) else { continue };
                if dead.time - shot.time <= Duration::minutes(3)
                    && !credit.iter().any(|(c, _)| *c == k)
                {
                    credit.push((k, 1.));
                }
            }
        }
        if !credit.is_empty() {
            let victim_typ = dead
                .shots
                .iter()
                .find(|s| s.target_typ.trim() != "")
                .map(|s| s.target_typ.clone());
            let base_points = victim_typ
                .as_ref()
                .and_then(|typ| self.ephemeral.cfg.unit_classification.get(typ.as_str()))
                .map(|tags| {
                    if tags.contains(UnitTag::Aircraft) || tags.contains(UnitTag::Helicopter) {
                        cfg.air_kill
                    } else {
                        // Priced by what it was: a rifleman is worth less
                        // than a tank, a radar more.
                        let v = (cfg.ground_kill as f64 * econ.ground_kill_multiplier(tags.0))
                            .round() as u32;
                        let v = if cfg.ground_kill > 0 { v.max(1) } else { v };
                        if tags.contains(UnitTag::LR | UnitTag::TrackRadar | UnitTag::SAM) {
                            v + cfg.lr_sam_bonus
                        } else {
                            v
                        }
                    }
                })
                .unwrap_or(cfg.ground_kill);
            // Apply night kill bonus if configured. Night is the mission's
            // night, not the server's wall clock.
            let total_points = if let Some(tod_cfg) = self.ephemeral.cfg.time_of_day_effects.as_ref() {
                let hour = self
                    .ephemeral
                    .mission_hour
                    .unwrap_or_else(|| dead.time.hour() as u8);
                let is_night = if tod_cfg.night_start_hour > tod_cfg.night_end_hour {
                    // Wraps midnight (e.g. 22-06)
                    hour >= tod_cfg.night_start_hour || hour < tod_cfg.night_end_hour
                } else {
                    hour >= tod_cfg.night_start_hour && hour < tod_cfg.night_end_hour
                };
                if is_night {
                    (base_points as f64 * tod_cfg.night_kill_bonus).ceil() as u32
                } else {
                    base_points
                }
            } else {
                base_points
            };
            let weights: SmallVec<[f64; 8]> = credit.iter().map(|(_, w)| *w).collect();
            let shares = bfprotocols::cfg::split_by_weight(total_points as i32, &weights);
            let victim_info = match &dead.victim {
                Who::Player { ucid, .. } => self.persisted.players.get(ucid).map(|p| VictimInfo {
                    ucid: *ucid,
                    name: p.name.clone(),
                    life_type: p.airborne,
                    ai_deployable: false,
                }),
                Who::AI { ucid: None, .. } => None,
                Who::AI { ucid: Some(i), .. } => {
                    self.persisted.players.get(i).map(|p| VictimInfo {
                        ucid: *i,
                        name: p.name.clone(),
                        life_type: None,
                        ai_deployable: true,
                    })
                }
            };
            for (((ucid, via_ai), _), share) in credit.iter().copied().zip(shares) {
                let Some((side, streak)) = self
                    .persisted
                    .players
                    .get(&ucid)
                    .map(|p| (p.side, p.kill_streak))
                else {
                    continue;
                };
                let msg = if side == *dead.victim.side() {
                    self.apply_teamkill_penalty(ucid, total_points, &victim_info)
                } else {
                    // A kill by the player's deployed SAMs/troops pays a
                    // share, less while they aren't flying -- it used to pay
                    // in full, offline included, which out-earned actually
                    // hauling the stuff. Streaks are the pilot's own.
                    let ai_frac = if via_ai {
                        econ.owned_ai_fraction(self.is_slotted(&ucid))
                    } else {
                        1.
                    };
                    let streak_mult = if via_ai {
                        1.0
                    } else {
                        cfg.kill_streak_bonuses
                            .iter()
                            .rev()
                            .find(|(min_streak, _)| streak >= *min_streak)
                            .map(|(_, mult)| *mult)
                            .unwrap_or(1.0)
                    };
                    let raw = (share as f64 * ai_frac * streak_mult).round() as i32;
                    let earned = self.scale_earning(&ucid, raw);
                    let pts = earned.amount;
                    let provisional = !via_ai && cfg.provisional;
                    let player = match self.persisted.players.get_mut_cow(&ucid) {
                        Some(p) => p,
                        None => continue,
                    };
                    let tp = if provisional {
                        player.provisional_points += pts;
                        player.provisional_points
                    } else {
                        player.points += pts;
                        player.points
                    };
                    // Only a pilot's own kills go on their debrief -- not
                    // kills their deployed AI made while they flew.
                    if !via_ai
                        && let Some(f) = player.flight.as_mut()
                    {
                        let typ = victim_typ.as_ref().map(|t| t.as_str()).unwrap_or("unknown");
                        f.tally.add_kill(typ, pts);
                    }
                    player.total_kills = player.total_kills.saturating_add(1);
                    if !via_ai {
                        player.kill_streak = player.kill_streak.saturating_add(1);
                        if player.kill_streak == 5 {
                            self.ephemeral.pending_achievements.push(format!("{} is now an Ace! (5 kill streak)", player.name).into());
                        } else if player.kill_streak == 10 {
                            self.ephemeral.pending_achievements.push(format!("{} is unstoppable! (10 kill streak)", player.name).into());
                        } else if player.kill_streak == 15 {
                            self.ephemeral.pending_achievements.push(format!("{} is a god of war! (15 kill streak)", player.name).into());
                        }
                    }
                    let pm = if provisional { " provisional" } else { "" };
                    let streak_msg = if streak_mult > 1.0 {
                        format_compact!(" [x{:.1} streak]", streak_mult)
                    } else {
                        format_compact!("")
                    };
                    let who = if via_ai { "your deployed ai " } else { "" };
                    let note = earned.note();
                    match &victim_info {
                        None => format_compact!("{tp}(+{pts}){pm}{streak_msg}{note} points, {who}kill"),
                        Some(vi) => {
                            if vi.ai_deployable {
                                format_compact!(
                                    "{tp}(+{pts}){pm}{streak_msg}{note} points, {who}killed {}'s deployed ai unit",
                                    vi.name
                                )
                            } else {
                                format_compact!("{tp}(+{pts}){pm}{streak_msg}{note} points, {who}killed {}", vi.name)
                            }
                        }
                    }
                };
                debug!("{ucid} kill message: {msg}");
                self.ephemeral
                    .panel_to_player(&self.persisted, 10, &ucid, msg)
            }
        }
    }

    pub fn adjust_points(&mut self, ucid: &Ucid, amount: i32, why: &str) {
        if let Some(player) = self.persisted.players.get_mut_cow(ucid) {
            player.points += amount;
            let pp = player.points;
            if amount != 0 {
                let m = format_compact!("{}({}) points {}", pp, amount, why);
                self.ephemeral.stat(Stat::Points {
                    points: amount,
                    reason: m.clone().into(),
                    id: *ucid,
                });
                self.ephemeral.panel_to_player(&self.persisted, 10, ucid, m);
                self.ephemeral.dirty();
            }
        }
    }

    pub fn adjust_points_silent(&mut self, ucid: &Ucid, amount: i32, why: &str) {
        if let Some(player) = self.persisted.players.get_mut_cow(ucid) {
            player.points += amount;
            if amount != 0 {
                self.ephemeral.stat(Stat::Points {
                    points: amount,
                    reason: compact_str::format_compact!("{}({}) points {}", player.points, amount, why).into(),
                    id: *ucid,
                });
                self.ephemeral.dirty();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sortie_tally_groups_kills_by_type() {
        let mut t = SortieTally::default();
        t.add_kill("T-72B", 50);
        t.add_kill("Su-25T", 100);
        t.add_kill("T-72B", 50);
        assert_eq!(t.kill_count(), 3);
        assert_eq!(t.kill_points, 200);
        assert_eq!((t.kills[0].0.as_str(), t.kills[0].1), ("T-72B", 2));
        assert_eq!((t.kills[1].0.as_str(), t.kills[1].1), ("Su-25T", 1));
    }

    fn charge(cost: u32, frac: f32) -> PointsCharge {
        PointsCharge {
            side: Side::Blue,
            oid: ObjectiveId::from(1),
            cost,
            frac,
        }
    }

    #[test]
    fn split_charge_never_drives_the_fund_negative() {
        // player covers it all
        assert_eq!(split_charge(500, 100, 200), (200, 0));
        // player covers part, fund the rest
        assert_eq!(split_charge(50, 1000, 200), (50, 150));
        // player broke, fund covers it
        assert_eq!(split_charge(-40, 1000, 200), (0, 200));
        // fund short: it empties, the player owes the remainder
        assert_eq!(split_charge(0, 120, 200), (80, 120));
        // negative fund counts as empty, not as a debt the player inherits
        assert_eq!(split_charge(0, -500, 200), (200, 0));
        assert_eq!(split_charge(10, 0, 0), (0, 0));
    }

    #[test]
    fn refund_split_is_capped_at_the_charge() {
        // full refund of a fully player-paid flight
        assert_eq!(charge(200, 1.).refund_split(200), (200, 0));
        // landing with more aboard than was charged refunds only the charge
        assert_eq!(charge(100, 1.).refund_split(400), (100, 0));
        // split in the proportion paid, and the shares add up
        assert_eq!(charge(200, 0.25).refund_split(200), (50, 150));
        let (p, o) = charge(333, 0.5).refund_split(101);
        assert_eq!(p + o, 101);
        // expended stores stay paid for
        assert_eq!(charge(200, 0.5).refund_split(80), (40, 40));
        // nothing charged, nothing back
        assert_eq!(charge(0, 1.).refund_split(500), (0, 0));
    }

    #[test]
    fn tk_decay_halves_per_window_and_never_wraps() {
        assert_eq!(tk_decayed(100, 0), 100);
        assert_eq!(tk_decayed(100, 1), 50);
        assert_eq!(tk_decayed(100, 3), 12);
        // a plain `>>` masks the shift to 0 in release and gives 100 back
        assert_eq!(tk_decayed(100, 32), 0);
        assert_eq!(tk_decayed(100, 64), 0);
        assert_eq!(tk_decayed(u32::MAX, 1_000_000), 0);
        assert_eq!(tk_decayed(100, -1), 0);
    }

    #[test]
    fn tk_windows_forgets_old_kills_and_survives_a_zero_window() {
        let now = Utc::now();
        let h = |n: i64| now - Duration::hours(n);
        assert_eq!(tk_windows(now, h(0), 24), Some(0));
        assert_eq!(tk_windows(now, h(23), 24), Some(0));
        assert_eq!(tk_windows(now, h(24), 24), Some(1));
        assert_eq!(tk_windows(now, h(24 * 8 - 1), 24), Some(7));
        assert_eq!(tk_windows(now, h(24 * 8), 24), None);
        // a kill stamped in the future (clock step) counts as fresh
        assert_eq!(tk_windows(now, now + Duration::hours(5), 24), Some(0));
        // window 0 used to divide by zero
        assert_eq!(tk_windows(now, h(0), 0), None);
    }

    #[test]
    fn tk_penalties_only_count_remembered_kills() {
        let now = Utc::now();
        let h = |n: i64| now - Duration::hours(n);
        assert_eq!(ai_tk_penalty([], now, 24, 100), 0);
        assert_eq!(ai_tk_penalty([h(1), h(25), h(24 * 40)], now, 24, 100), 150);
        assert_eq!(ai_tk_penalty([h(1)], now, 0, 100), 0);
        let (pts, lives) = player_tk_penalty([h(1), h(49), h(24 * 40)], now, 24, 100);
        assert_eq!(pts, 100 + 100 + 25);
        assert!((lives - (1. + 1. + 0.25)).abs() < 1e-6);
        let (pts, lives) = player_tk_penalty([h(1)], now, 0, 100);
        assert_eq!(pts, 100);
        assert!((lives - 1.).abs() < 1e-6);
    }
}
