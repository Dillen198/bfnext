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

//! Real orders of battle for AI aircraft (`Cfg::ai_air_stock`, Red Storm
//! Rising ENGINE.md 3.1).
//!
//! Every AI flight goes through `Db::add_and_spawn_ai_air_from`, which now
//! asks this module what the flight needs (`air_stock_need`), only launches
//! from a field that holds it (`air_stock_field_ok`) and books it out once
//! the flight is in DCS (`air_stock_draw`). The draw is remembered in
//! `Persisted::air_stock` until the flight leaves the map:
//!
//! - an aircraft shot down, crashed or ejected is gone: nothing comes back
//!   for it (`air_stock_unit_lost` only counts it for the dashboard);
//! - with `return_on_land`, an aircraft that touches down at a friendly base
//!   is noted there (`air_stock_landed`, cleared again if it takes off);
//!   once every live aircraft of the flight has been down `SETTLE`, the slow
//!   tick removes the flight and each goes into the stock of the base it
//!   landed at (`air_stock_tick`);
//! - whenever the flight is removed for any other reason (package lifetime,
//!   the return-to-base cull, a restart dropping it, an admin delete, a
//!   flight stuck on the ramp), `delete_group` calls
//!   `air_stock_group_removed`: every aircraft still alive goes back to the
//!   base it landed at, else to the field it launched from while its side
//!   holds it, else to the side's nearest field that can operate it, else it
//!   is lost. Never more of a type than the flight drew.
//!
//! DCS takes the airframes out of its own warehouse when a flight starts
//! from parking (see `stock_for_ground_start`), so the draw only mirrors
//! that into the model and leaves `Inventory::unsynced` alone; a return is
//! the engine's own doing (the aircraft is removed by script, which DCS does
//! not book back in), so it goes through `Inventory::credit` and is pushed
//! to DCS at the next reconcile. Removing a landed flight after `SETTLE`
//! rather than leaving it to taxi in and shut down keeps DCS from booking
//! the same aircraft in a second time when it would despawn it.

use super::{
    Db, group::is_launch_kind, logistics::is_airframe_item, objective::Objective,
};
use crate::spawnctx::SpawnCtx;
use anyhow::Result;
use bfprotocols::{
    cfg::{ActionKind, AiAirStockCfg, Cfg, StoryTrigger, UnitTag},
    db::{
        group::{GroupId, UnitId},
        objective::ObjectiveId,
    },
    stats::{AirOobStat, AirframeCount, Stat},
};
use chrono::{DateTime, Duration, Utc};
use dcso3::{
    String, Vector2,
    coalition::Side,
    env::miz::{GroupKind, MizIndex},
    object::DcsOid,
    unit::ClassUnit,
};
use fxhash::{FxHashMap, FxHashSet};
use log::{debug, info};
use serde_derive::{Deserialize, Serialize};
use smallvec::SmallVec;
use std::{collections::BTreeMap, sync::Arc};

/// How long every live aircraft of a flight has to have been down at a
/// friendly base before it is taken off the map and booked back in.
pub(crate) const SETTLE: Duration = Duration::seconds(30);
/// The order of battle is looked at, and sent if it changed, at most this
/// often.
const OOB_MIN_GAP: Duration = Duration::seconds(60);
/// ... and sent at least this often while it doesn't change.
const OOB_REFRESH: Duration = Duration::minutes(10);

/// What a flight takes out of stock: airframes by DCS type.
pub(crate) type AirNeed = SmallVec<[(String, u32); 2]>;

/// One flight that drew its airframes from a base.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AirDraw {
    pub gid: GroupId,
    pub side: Side,
    /// The field it launched from.
    pub origin: ObjectiveId,
    /// What it took out of that field's stock, by type: at most this much
    /// ever goes back.
    pub drawn: Vec<(String, u32)>,
    /// Its aircraft that came down at a friendly base (`return_on_land`),
    /// the base, and when.
    #[serde(default)]
    pub landed: Vec<(UnitId, ObjectiveId, DateTime<Utc>)>,
}

#[derive(Debug, Clone, Default)]
struct AirStockRt {
    /// Each .miz template's airframes by type, learned from the mission.
    templates: FxHashMap<std::string::String, Vec<(String, u32)>>,
    /// Player aircraft already counted as lost (a death raises several
    /// events).
    counted: FxHashSet<DcsOid<ClassUnit>>,
    last_look: Option<DateTime<Utc>>,
    last_sent: Option<(DateTime<Utc>, AirOobStat)>,
}

/// The round's air order of battle bookkeeping (`Persisted::air_stock`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AirStock {
    /// Flights in the air (or on the ground) that drew their airframes.
    #[serde(default)]
    pub draws: Vec<AirDraw>,
    /// Airframes lost this round, by type: player aircraft, and AI aircraft
    /// that came out of the stock.
    #[serde(default)]
    pub blue_lost: BTreeMap<std::string::String, u32>,
    #[serde(default)]
    pub red_lost: BTreeMap<std::string::String, u32>,
    #[serde(skip)]
    rt: AirStockRt,
}

impl AirStock {
    fn lost_mut(
        &mut self,
        side: Side,
    ) -> Option<&mut BTreeMap<std::string::String, u32>> {
        match side {
            Side::Blue => Some(&mut self.blue_lost),
            Side::Red => Some(&mut self.red_lost),
            Side::Neutral => None,
        }
    }

    fn lost(&self, side: Side) -> Option<&BTreeMap<std::string::String, u32>> {
        match side {
            Side::Blue => Some(&self.blue_lost),
            Side::Red => Some(&self.red_lost),
            Side::Neutral => None,
        }
    }

    fn draw_mut(&mut self, gid: &GroupId) -> Option<&mut AirDraw> {
        self.draws.iter_mut().find(|d| d.gid == *gid)
    }

    /// Does flight `gid` hold airframes from the stock?
    pub fn has_draw(&self, gid: &GroupId) -> bool {
        self.draws.iter().any(|d| d.gid == *gid)
    }
}

/// A side's airframes of one type over its bases: what they hold
/// (`unlimited_aircraft` bases left out), and whether an unlimited base
/// holds it.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct SideStock {
    pub(crate) stock: u32,
    pub(crate) unlimited: bool,
}

/// Each side's airframes by type, Blue then Red. Keys are lower case.
pub(crate) type StockTotals = [FxHashMap<std::string::String, SideStock>; 2];

fn side_idx(side: Side) -> Option<usize> {
    match side {
        Side::Blue => Some(0),
        Side::Red => Some(1),
        Side::Neutral => None,
    }
}

/// The .miz template an action's aircraft come from, for those that fly.
pub(crate) fn action_template(kind: &ActionKind) -> Option<&str> {
    let plane = match kind {
        ActionKind::Tanker(p)
        | ActionKind::Fighters(p)
        | ActionKind::Attackers(p)
        | ActionKind::Sead(p)
        | ActionKind::CruiseMissileSpawn(p)
        | ActionKind::LogisticsRepair(p)
        | ActionKind::LogisticsTransfer(p) => p,
        ActionKind::Awacs(a) => &a.plane,
        ActionKind::Bomber(b) => &b.plane,
        ActionKind::Strike(s) => &s.plane,
        ActionKind::Drone(d) => &d.plane,
        ActionKind::Recon(r) => &r.plane,
        ActionKind::Paratrooper(d) | ActionKind::Deployable(d) => d.plane.as_ref()?,
        _ => return None,
    };
    Some(plane.template.as_str())
}

/// "2 MiG-29S and 1 Su-24M".
pub(crate) fn describe(need: &[(String, u32)]) -> std::string::String {
    need.iter().map(|(t, n)| format!("{n} {t}")).collect::<Vec<_>>().join(" and ")
}

/// Can `obj` launch `need` now? An `unlimited_aircraft` base always can.
pub(crate) fn field_holds(obj: &Objective, need: &[(String, u32)]) -> bool {
    obj.unlimited_aircraft
        || need.iter().all(|(t, n)| {
            obj.warehouse.equipment.get(t.as_str()).is_some_and(|i| i.stored >= *n)
        })
}

/// Of `types`, how many airframes the side holds; None when an unlimited
/// base holds one of them (it never runs out).
pub(crate) fn stock_of(
    totals: &FxHashMap<std::string::String, SideStock>,
    types: &[std::string::String],
) -> Option<u64> {
    let mut n = 0u64;
    for t in types {
        if let Some(s) = totals.get(&t.to_lowercase()) {
            if s.unlimited {
                return None;
            }
            n += s.stock as u64;
        }
    }
    Some(n)
}

impl Db {
    /// The config, when `ai_air_stock` is on (and there is a warehouse to
    /// draw on).
    fn air_stock_on(&self) -> Option<Arc<Cfg>> {
        let cfg = &self.ephemeral.cfg;
        (cfg.warehouse.is_some() && cfg.ai_air_stock.as_ref().is_some_and(|a| a.enabled))
            .then(|| Arc::clone(cfg))
    }

    fn air_stock_cfg(cfg: &Cfg) -> Option<&AiAirStockCfg> {
        cfg.ai_air_stock.as_ref().filter(|a| a.enabled)
    }

    /// The airframes `template` flies, by type, as the mission has it.
    /// Learned once per template; Err if the mission has no such template.
    pub(crate) fn air_stock_template(
        &mut self,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        template: &str,
    ) -> Result<Vec<(String, u32)>> {
        if let Some(t) = self.persisted.air_stock.rt.templates.get(template) {
            return Ok(t.clone());
        }
        let info = spctx.get_template_ref(idx, GroupKind::Any, side, template)?;
        let mut types: Vec<(String, u32)> = vec![];
        for u in info.group.units()?.into_iter() {
            let typ = u?.typ()?;
            match types.iter_mut().find(|(t, _)| *t == typ) {
                Some((_, n)) => *n += 1,
                None => types.push((typ, 1)),
            }
        }
        self.persisted.air_stock.rt.templates.insert(template.into(), types.clone());
        Ok(types)
    }

    /// Learn every action's and HQ roster's template, so the HQ can tell
    /// what each side can still fly from its first pass. Once per mission
    /// load; does nothing with `ai_air_stock` off.
    pub(crate) fn air_stock_learn_templates(&mut self, spctx: &SpawnCtx, idx: &MizIndex) {
        let Some(cfg) = self.air_stock_on() else { return };
        let mut want: Vec<(Side, std::string::String)> = vec![];
        for (side, actions) in &cfg.actions {
            for a in actions.values() {
                if let Some(t) = action_template(&a.kind) {
                    want.push((*side, t.into()));
                }
            }
        }
        if let Some(hq) = cfg.smart_commander.as_ref().and_then(|s| s.hq.as_ref()) {
            for (side, air) in &hq.air {
                for t in air.cap.iter().chain(&air.strike).chain(&air.sead) {
                    want.push((*side, t.plane.template.as_str().into()));
                }
            }
        }
        for (side, t) in want {
            if let Err(e) = self.air_stock_template(spctx, idx, side, &t) {
                debug!("ai_air_stock: no template {t} for {side:?}: {e:#}")
            }
        }
    }

    /// What a flight of `template` for `action` takes out of stock, given
    /// the template's airframes `types`. None when it takes nothing: the
    /// feature off, the action or template exempt, or only exempt airframes.
    pub(crate) fn air_stock_need(
        &self,
        action: &str,
        template: &str,
        types: &[(String, u32)],
    ) -> Option<AirNeed> {
        let cfg = self.air_stock_on()?;
        let c = Self::air_stock_cfg(&cfg)?;
        if c.exempt(action, template) {
            return None;
        }
        let wh = cfg.warehouse.as_ref()?;
        let need: AirNeed = types
            .iter()
            .filter(|(t, n)| *n > 0 && !wh.exempt_airframes.contains(t.as_str()))
            .cloned()
            .collect();
        (!need.is_empty()).then_some(need)
    }

    /// Does field `oid` hold `need`?
    pub(crate) fn air_stock_field_ok(
        &self,
        oid: &ObjectiveId,
        need: &[(String, u32)],
    ) -> bool {
        self.persisted.objectives.get(oid).is_some_and(|o| field_holds(o, need))
    }

    /// Could `side` fly `template` (for `action`) from any of its bases
    /// right now? True when the stock doesn't apply, and when the template
    /// hasn't been learned (the launch then decides). For the HQ, so it
    /// picks an aircraft its bases still hold.
    pub(crate) fn air_stock_side_can_fly(
        &self,
        side: Side,
        action: &str,
        template: &str,
    ) -> bool {
        let Some(types) = self.persisted.air_stock.rt.templates.get(template) else {
            return true;
        };
        let Some(need) = self.air_stock_need(action, template, types) else {
            return true;
        };
        let extra = &self.ephemeral.cfg.extra_fixed_wing_objectives;
        self.persisted.objectives.into_iter().any(|(_, o)| {
            o.owner == side
                && !o.captureable()
                && is_launch_kind(&o.kind, true, extra.contains(&o.name))
                && field_holds(o, &need)
        })
    }

    /// Flight `gid` is in DCS: take `need` out of `field`'s stock and
    /// remember the draw. Nothing is taken at an `unlimited_aircraft` base.
    pub(crate) fn air_stock_draw(
        &mut self,
        gid: GroupId,
        side: Side,
        field: ObjectiveId,
        need: &[(String, u32)],
    ) {
        let Some(obj) = self.persisted.objectives.get_mut_cow(&field) else { return };
        if obj.unlimited_aircraft {
            return;
        }
        let id = obj.id;
        let name = obj.name.clone();
        let mut drawn: Vec<(String, u32)> = vec![];
        let mut left: SmallVec<[(String, u32); 2]> = SmallVec::new();
        for (t, n) in need {
            if let Some(inv) = obj.warehouse.equipment.get_mut_cow(t) {
                // DCS takes these out of its own warehouse as the flight
                // starts from parking: mirror it, leave `unsynced` alone.
                let took = (*n).min(inv.stored);
                inv.stored -= took;
                if took > 0 {
                    drawn.push((t.clone(), took));
                    left.push((t.clone(), inv.stored));
                }
            }
        }
        for (item, amount) in left {
            self.ephemeral.stat(Stat::EquipmentInventory { id, item, amount });
        }
        if !drawn.is_empty() {
            info!("[AIR_STOCK] {gid:?} ({side:?}) drew {} from {name}", describe(&drawn));
            self.persisted.air_stock.draws.push(AirDraw {
                gid,
                side,
                origin: field,
                drawn,
                landed: vec![],
            });
        }
        self.ephemeral.dirty();
    }

    /// Put `n` airframes of `typ` into `oid`'s stock, up to its capacity (a
    /// base that never stocked the type takes them all). Returns how many
    /// went in; an `unlimited_aircraft` base needs none and "takes" them.
    pub(crate) fn air_stock_credit(
        &mut self,
        oid: ObjectiveId,
        typ: &String,
        n: u32,
    ) -> u32 {
        let Some(obj) = self.persisted.objectives.get_mut_cow(&oid) else { return 0 };
        if obj.unlimited_aircraft {
            return n;
        }
        let id = obj.id;
        let inv = obj.warehouse.equipment.get_or_default_cow(typ.clone());
        if inv.capacity == 0 {
            inv.capacity = n;
        }
        let took = inv.credit(n);
        let amount = inv.stored;
        self.ephemeral.stat(Stat::EquipmentInventory { id, item: typ.clone(), amount });
        self.ephemeral.dirty();
        took
    }

    /// Where an aircraft of `side` that is being retired at `pos` goes: the
    /// base it landed at, else the field it launched from, while its side
    /// holds them; else the side's nearest field that can operate it.
    fn air_stock_home(
        &self,
        side: Side,
        landed: Option<ObjectiveId>,
        origin: ObjectiveId,
        pos: Vector2,
        rotary: bool,
    ) -> Option<ObjectiveId> {
        let held = |oid: &ObjectiveId| {
            self.persisted.objectives.get(oid).is_some_and(|o| o.owner == side)
        };
        if let Some(b) = landed.filter(held) {
            return Some(b);
        }
        if held(&origin) {
            return Some(origin);
        }
        let extra = &self.ephemeral.cfg.extra_fixed_wing_objectives;
        self.persisted
            .objectives
            .into_iter()
            .filter(|(_, o)| {
                o.owner == side
                    && !o.captureable()
                    && is_launch_kind(&o.kind, rotary, extra.contains(&o.name))
            })
            .min_by(|(_, a), (_, b)| {
                (a.zone.pos() - pos).norm().total_cmp(&(b.zone.pos() - pos).norm())
            })
            .map(|(id, _)| *id)
    }

    /// Flight `gid` is being removed (`delete_group`, before the group and
    /// its units go): book its live aircraft back in. See the module notes.
    pub(crate) fn air_stock_group_removed(&mut self, gid: &GroupId) {
        let Some(i) = self.persisted.air_stock.draws.iter().position(|d| d.gid == *gid)
        else {
            return;
        };
        let draw = self.persisted.air_stock.draws.remove(i);
        self.ephemeral.dirty();
        let Some(group) = self.persisted.groups.get(gid) else { return };
        let rotary = group.tags.contains(UnitTag::Helicopter);
        let mut owed = draw.drawn.clone();
        let mut back: SmallVec<[(ObjectiveId, String); 4]> = SmallVec::new();
        let mut lost = 0;
        for uid in &group.units {
            let Some(u) = self.persisted.units.get(uid) else { continue };
            if u.dead {
                continue;
            }
            let Some(slot) =
                owed.iter_mut().find(|(t, n)| *n > 0 && t.as_str() == u.typ.0.as_str())
            else {
                continue;
            };
            slot.1 -= 1;
            let landed =
                draw.landed.iter().find(|(l, _, _)| l == uid).map(|(_, b, _)| *b);
            match self.air_stock_home(draw.side, landed, draw.origin, u.pos, rotary) {
                Some(oid) => back.push((oid, slot.0.clone())),
                None => lost += 1,
            }
        }
        for (oid, typ) in &back {
            self.air_stock_credit(*oid, typ, 1);
        }
        if !back.is_empty() || lost > 0 {
            info!(
                "[AIR_STOCK] {gid:?} ({:?}) retired: {} airframe(s) back in stock, {lost} with no friendly field to go to",
                draw.side,
                back.len()
            );
        }
    }

    /// AI unit `id` touched down at `pos`. With `return_on_land`, an
    /// aircraft of a flight that drew from stock is noted at the friendly
    /// base whose zone it is in.
    pub fn air_stock_landed(
        &mut self,
        id: &DcsOid<ClassUnit>,
        pos: Vector2,
        now: DateTime<Utc>,
    ) {
        let Some(cfg) = self.air_stock_on() else { return };
        if !Self::air_stock_cfg(&cfg).is_some_and(|c| c.return_on_land) {
            return;
        }
        let Some(uid) = self.ephemeral.get_uid_by_object_id(id).copied() else { return };
        self.air_stock_landed_uid(uid, pos, now)
    }

    pub(crate) fn air_stock_landed_uid(
        &mut self,
        uid: UnitId,
        pos: Vector2,
        now: DateTime<Utc>,
    ) {
        let Some(u) = self.persisted.units.get(&uid) else { return };
        let gid = u.group;
        let Some(side) =
            self.persisted.air_stock.draws.iter().find(|d| d.gid == gid).map(|d| d.side)
        else {
            return;
        };
        let Some(base) = self
            .persisted
            .objectives
            .into_iter()
            .find(|(_, o)| o.owner == side && o.zone.contains(pos))
            .map(|(id, _)| *id)
        else {
            return;
        };
        if let Some(d) = self.persisted.air_stock.draw_mut(&gid) {
            d.landed.retain(|(l, _, _)| *l != uid);
            d.landed.push((uid, base, now));
            self.ephemeral.dirty();
        }
    }

    /// AI unit `id` took off again: it is not down anywhere any more.
    pub fn air_stock_took_off(&mut self, id: &DcsOid<ClassUnit>) {
        if self.persisted.air_stock.draws.is_empty() {
            return;
        }
        let Some(uid) = self.ephemeral.get_uid_by_object_id(id).copied() else { return };
        for d in self.persisted.air_stock.draws.iter_mut() {
            d.landed.retain(|(l, _, _)| *l != uid);
        }
    }

    /// Unit `id` died; called before the death is processed. A player's
    /// aircraft, or an AI aircraft that came out of the stock, counts as a
    /// loss of its side this round. Nothing goes back for it.
    pub(crate) fn air_stock_unit_lost(&mut self, id: &DcsOid<ClassUnit>) {
        if self.air_stock_on().is_none() {
            return;
        }
        let what = if let Some(slot) = self.ephemeral.get_slot_by_object_id(id) {
            let Some(ifo) = self.ephemeral.slot_info.get(slot) else { return };
            let tags = self
                .ephemeral
                .cfg
                .unit_classification
                .get(&ifo.typ)
                .copied()
                .unwrap_or_default();
            if !(tags.contains(UnitTag::Aircraft) || tags.contains(UnitTag::Helicopter)) {
                return;
            }
            let what = (ifo.side, std::string::String::from(ifo.typ.0.as_str()));
            if !self.persisted.air_stock.rt.counted.insert(id.clone()) {
                return;
            }
            what
        } else if let Some(uid) = self.ephemeral.get_uid_by_object_id(id) {
            let Some(u) = self.persisted.units.get(uid) else { return };
            if u.dead || !self.persisted.air_stock.has_draw(&u.group) {
                return;
            }
            (u.side, std::string::String::from(u.typ.0.as_str()))
        } else {
            return;
        };
        self.air_stock_count_loss(what.0, &what.1);
    }

    pub(crate) fn air_stock_count_loss(&mut self, side: Side, typ: &str) {
        if let Some(m) = self.persisted.air_stock.lost_mut(side) {
            *m.entry(typ.into()).or_default() += 1;
            self.ephemeral.dirty();
        }
    }

    /// Each side's airframes by type over the bases it holds, from the
    /// warehouses (works with `ai_air_stock` off too: it is what players fly
    /// from). Keys are lower case.
    pub(crate) fn air_stock_totals(&self) -> StockTotals {
        let mut out: StockTotals = Default::default();
        for (_, o) in &self.persisted.objectives {
            let Some(i) = side_idx(o.owner) else { continue };
            for (name, inv) in &o.warehouse.equipment {
                if !is_airframe_item(name.as_str()) {
                    continue;
                }
                let e = out[i].entry(name.as_str().to_lowercase()).or_default();
                if o.unlimited_aircraft {
                    e.unlimited |= inv.stored > 0 || inv.capacity > 0;
                } else {
                    e.stock = e.stock.saturating_add(inv.stored);
                }
            }
        }
        out
    }

    /// How many airframes of the types `flights` fly -- each an action or
    /// roster label and its .miz template -- the bases `side` holds have,
    /// for the HQ's posture (E7). None when that can't be told or never
    /// runs out: `ai_air_stock` off, no flights, a template not learned yet,
    /// one that draws nothing (exempt), or a type at an
    /// `unlimited_aircraft` base.
    pub(crate) fn air_stock_airframes(
        &self,
        side: Side,
        flights: &[(std::string::String, std::string::String)],
    ) -> Option<u64> {
        self.air_stock_on()?;
        let i = side_idx(side)?;
        let mut types: Vec<std::string::String> = vec![];
        for (action, template) in flights {
            let known = self.persisted.air_stock.rt.templates.get(template.as_str())?;
            for (typ, _) in self.air_stock_need(action, template, known)? {
                let t = std::string::String::from(typ.as_str());
                if !types.contains(&t) {
                    types.push(t);
                }
            }
        }
        if types.is_empty() {
            return None;
        }
        stock_of(&self.air_stock_totals()[i], &types)
    }

    /// Does the story wait on a stock (`StockBelow`)? Only then does a story
    /// tick add the stock totals up.
    pub(crate) fn story_needs_stock(&self) -> bool {
        self.ephemeral.cfg.story.as_ref().is_some_and(|s| {
            s.enabled
                && s.events
                    .iter()
                    .any(|e| matches!(e.trigger, StoryTrigger::StockBelow { .. }))
        })
    }

    /// The order of battle as bfdb gets it.
    pub(crate) fn air_stock_oob(&self) -> AirOobStat {
        // The display name of each type, as the warehouses spell it.
        let mut names: FxHashMap<std::string::String, String> = FxHashMap::default();
        let mut per: [BTreeMap<std::string::String, AirframeCount>; 2] =
            Default::default();
        for (_, o) in &self.persisted.objectives {
            let Some(i) = side_idx(o.owner) else { continue };
            for (name, inv) in &o.warehouse.equipment {
                if !is_airframe_item(name.as_str()) {
                    continue;
                }
                let key = name.as_str().to_lowercase();
                names.entry(key.clone()).or_insert_with(|| name.clone());
                let e = per[i].entry(key).or_insert_with(|| AirframeCount {
                    typ: name.clone(),
                    ..Default::default()
                });
                if o.unlimited_aircraft {
                    e.unlimited |= inv.stored > 0 || inv.capacity > 0;
                } else {
                    e.stock = e.stock.saturating_add(inv.stored);
                }
            }
        }
        for (i, side) in [Side::Blue, Side::Red].into_iter().enumerate() {
            let Some(lost) = self.persisted.air_stock.lost(side) else { continue };
            for (typ, n) in lost {
                let key = typ.to_lowercase();
                let e = per[i].entry(key.clone()).or_insert_with(|| AirframeCount {
                    typ: names.get(&key).cloned().unwrap_or_else(|| typ.as_str().into()),
                    ..Default::default()
                });
                e.lost = e.lost.saturating_add(*n);
            }
        }
        let [blue, red] = per;
        AirOobStat {
            blue: blue.into_values().collect(),
            red: red.into_values().collect(),
        }
    }

    /// The slow tick: retire flights whose aircraft are all down at friendly
    /// bases, drop draws whose flight is gone, and send the order of battle
    /// when it has changed.
    pub(crate) fn air_stock_tick(&mut self, now: DateTime<Utc>) {
        if self
            .persisted
            .air_stock
            .draws
            .iter()
            .any(|d| self.persisted.groups.get(&d.gid).is_none())
        {
            self.persisted
                .air_stock
                .draws
                .retain(|d| self.persisted.groups.get(&d.gid).is_some());
            self.ephemeral.dirty();
        }
        let Some(cfg) = self.air_stock_on() else { return };
        if Self::air_stock_cfg(&cfg).is_some_and(|c| c.return_on_land) {
            let done: Vec<GroupId> = self
                .persisted
                .air_stock
                .draws
                .iter()
                .filter(|d| self.air_stock_all_down(d, now))
                .map(|d| d.gid)
                .collect();
            for gid in done {
                info!(
                    "[AIR_STOCK] {gid:?} is down at friendly bases, booking it back in"
                );
                if let Err(e) = self.delete_group(&gid) {
                    log::error!("ai_air_stock: removing landed flight {gid:?}: {e:?}");
                }
            }
        }
        let rt = &self.persisted.air_stock.rt;
        if rt.last_look.is_some_and(|t| now - t < OOB_MIN_GAP) {
            return;
        }
        let oob = self.air_stock_oob();
        let rt = &mut self.persisted.air_stock.rt;
        rt.last_look = Some(now);
        let due = match &rt.last_sent {
            None => true,
            Some((t, last)) => *last != oob || now - *t >= OOB_REFRESH,
        };
        if due {
            rt.last_sent = Some((now, oob.clone()));
            self.ephemeral.stat(Stat::AirOrderOfBattle(oob));
        }
    }

    /// Every live aircraft of `d`'s flight has been down at a friendly base
    /// for `SETTLE`.
    fn air_stock_all_down(&self, d: &AirDraw, now: DateTime<Utc>) -> bool {
        let Some(group) = self.persisted.groups.get(&d.gid) else { return false };
        let mut live = 0;
        for uid in &group.units {
            let Some(u) = self.persisted.units.get(uid) else { continue };
            if u.dead {
                continue;
            }
            live += 1;
            match d.landed.iter().find(|(l, _, _)| l == uid) {
                Some((_, _, t)) if now - *t >= SETTLE => (),
                _ => return false,
            }
        }
        live > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::{
        group::{DeployKind, SpawnedGroup, SpawnedUnit},
        logistics::Inventory,
        objective::Zone,
    };
    use bfprotocols::{
        cfg::{UnitTags, Vehicle},
        db::group::GroupId,
    };
    use dcso3::Position3;

    fn t0() -> DateTime<Utc> {
        DateTime::parse_from_rfc3339("2026-10-31T12:00:00Z").unwrap().with_timezone(&Utc)
    }

    fn base(
        db: &mut Db,
        id: i64,
        name: &str,
        owner: &str,
        x: f64,
        stock: &[(&str, u32)],
        unlimited: bool,
    ) -> ObjectiveId {
        let mut o: Objective = serde_json::from_value(serde_json::json!({
            "id": id, "name": name, "owner": owner, "kind": "Airbase", "groups": {},
            "health": 100, "logi": 100, "threatened": false,
            "last_threatened_ts": "2026-10-31T00:00:00Z", "last_change_ts": "2026-10-31T00:00:00Z",
        }))
        .unwrap_or_else(|e| panic!("objective: {e}"));
        o.zone = Zone::Circle { pos: Vector2::new(x, 0.), radius: 2000. };
        o.unlimited_aircraft = unlimited;
        for (t, n) in stock {
            o.warehouse.equipment.insert_cow(
                (*t).into(),
                Inventory { stored: *n, capacity: 20, ..Default::default() },
            );
        }
        let oid = ObjectiveId::from(id);
        db.persisted.objectives.insert_cow(oid, o);
        db.persisted.objectives_by_name.insert_cow(name.into(), oid);
        oid
    }

    fn stored(db: &Db, oid: ObjectiveId, typ: &str) -> u32 {
        db.persisted
            .objectives
            .get(&oid)
            .unwrap()
            .warehouse
            .equipment
            .get(typ)
            .map(|i| i.stored)
            .unwrap_or(0)
    }

    /// A Db with a warehouse and `ai_air_stock` as given (None = absent),
    /// Red's Mozdok (4 Tu-22M3, 6 MiG-29S) and Kutaisi (2 MiG-29S) 50 km
    /// apart, and Blue's Batumi.
    fn db(stock: Option<&str>) -> (Db, ObjectiveId, ObjectiveId, ObjectiveId) {
        let mut cfg = Cfg::default();
        let mut wh = cfg.warehouse.clone().expect("a default warehouse");
        wh.exempt_airframes.insert("Su-25T".into());
        cfg.warehouse = Some(wh);
        cfg.ai_air_stock = stock.map(|s| serde_json::from_str(s).unwrap());
        let mut db = Db::default();
        db.ephemeral.cfg = Arc::new(cfg);
        let mozdok = base(
            &mut db,
            1,
            "Mozdok",
            "Red",
            0.,
            &[("Tu-22M3", 4), ("MiG-29S", 6)],
            false,
        );
        let kutaisi =
            base(&mut db, 2, "Kutaisi", "Red", 50_000., &[("MiG-29S", 2)], false);
        let batumi =
            base(&mut db, 3, "Batumi", "Blue", 200_000., &[("F-14A", 10)], false);
        (db, mozdok, kutaisi, batumi)
    }

    /// A flight of `typ` x `n` in the db, at `x`.
    fn flight(db: &mut Db, side: Side, typ: &str, n: u32, x: f64) -> GroupId {
        let gid = GroupId::new();
        let mut units = crate::db::SetS::new();
        for i in 0..n {
            let uid = UnitId::new();
            let name: String = format!("{typ}-{gid}-{i}").into();
            let u = SpawnedUnit {
                name: name.clone(),
                id: uid,
                group: gid,
                side,
                typ: Vehicle(typ.into()),
                tags: UnitTags(UnitTag::Aircraft.into()),
                template_name: name.clone(),
                spawn_pos: Vector2::new(x, 0.),
                spawn_heading: 0.,
                spawn_position: Position3::default(),
                pos: Vector2::new(x, 0.),
                heading: 0.,
                position: Position3::default(),
                dead: false,
                moved: None,
                airborne_velocity: None,
            };
            db.persisted.units.insert_cow(uid, u);
            db.persisted.units_by_name.insert_cow(name, uid);
            units.insert_cow(uid);
        }
        let name: String = format!("flight-{gid}").into();
        let g = SpawnedGroup {
            id: gid,
            name: name.clone(),
            template_name: name.clone(),
            side,
            kind: Some(dcso3::group::GroupCategory::Airplane),
            class: crate::db::objective::ObjGroupClass::Other,
            origin: DeployKind::Objective { origin: ObjectiveId::from(1) },
            units,
            tags: UnitTags(UnitTag::Aircraft.into()),
        };
        db.persisted.groups.insert_cow(gid, g);
        db.persisted.groups_by_name.insert_cow(name, gid);
        gid
    }

    fn units_of(db: &Db, gid: GroupId) -> Vec<UnitId> {
        db.persisted.groups.get(&gid).unwrap().units.into_iter().copied().collect()
    }

    fn kill(db: &mut Db, uid: UnitId) {
        db.persisted.units.get_mut_cow(&uid).unwrap().dead = true;
    }

    fn need(db: &Db, types: &[(&str, u32)]) -> Option<AirNeed> {
        let t: Vec<(String, u32)> =
            types.iter().map(|(t, n)| ((*t).into(), *n)).collect();
        db.air_stock_need("Backfire raid", "RBACKFIRE", &t)
    }

    #[test]
    fn config_absent_is_the_old_behaviour() {
        let (mut db, mozdok, ..) = db(None);
        assert!(need(&db, &[("Tu-22M3", 2)]).is_none());
        // nothing is drawn, nothing is recorded, every field "can" launch
        db.persisted
            .air_stock
            .rt
            .templates
            .insert("RBACKFIRE".into(), vec![("Tu-22M3".into(), 9)]);
        assert!(db.air_stock_side_can_fly(Side::Red, "Backfire raid", "RBACKFIRE"));
        let gid = flight(&mut db, Side::Red, "Tu-22M3", 2, 0.);
        db.air_stock_group_removed(&gid);
        assert_eq!(stored(&db, mozdok, "Tu-22M3"), 4);
        db.air_stock_tick(t0());
        assert!(db.persisted.air_stock.draws.is_empty());
        // disabled is the same as absent
        let (db, ..) = super::tests::db(Some(r#"{"enabled": false}"#));
        assert!(need(&db, &[("Tu-22M3", 2)]).is_none());
    }

    #[test]
    fn launch_debits_the_field_it_launches_from() {
        let (mut db, mozdok, kutaisi, _) = db(Some("{}"));
        let n = need(&db, &[("MiG-29S", 2)]).unwrap();
        assert!(
            db.air_stock_field_ok(&mozdok, &n) && db.air_stock_field_ok(&kutaisi, &n)
        );
        let gid = flight(&mut db, Side::Red, "MiG-29S", 2, 50_000.);
        db.air_stock_draw(gid, Side::Red, kutaisi, &n);
        assert_eq!(stored(&db, kutaisi, "MiG-29S"), 0);
        assert_eq!(stored(&db, mozdok, "MiG-29S"), 6);
        // mirrors DCS's own draw: nothing left to push at the next reconcile
        let inv = *db
            .persisted
            .objectives
            .get(&kutaisi)
            .unwrap()
            .warehouse
            .equipment
            .get("MiG-29S")
            .unwrap();
        assert_eq!(inv.unsynced, 0);
        assert!(db.persisted.air_stock.has_draw(&gid));
        // Kutaisi can't launch another pair; Mozdok still can
        assert!(!db.air_stock_field_ok(&kutaisi, &n));
        assert!(db.air_stock_field_ok(&mozdok, &n));
    }

    #[test]
    fn no_stock_no_launch_and_the_hq_falls_back() {
        let (mut db, mozdok, ..) = db(Some("{}"));
        let rt = &mut db.persisted.air_stock.rt;
        rt.templates.insert("RBACKFIRE".into(), vec![("Tu-22M3".into(), 2)]);
        rt.templates.insert("RMIG29".into(), vec![("MiG-29S".into(), 2)]);
        rt.templates.insert("RMIG21".into(), vec![("MiG-21Bis".into(), 2)]);
        assert!(db.air_stock_side_can_fly(Side::Red, "Backfire raid", "RBACKFIRE"));
        // never stocked anywhere: the HQ can't send it
        assert!(!db.air_stock_side_can_fly(Side::Red, "HQ CAP 1", "RMIG21"));
        // Blue holds no MiGs
        assert!(!db.air_stock_side_can_fly(Side::Blue, "x", "RMIG29"));
        // the Backfires go: two pairs and there are none left
        for _ in 0..2 {
            let n = need(&db, &[("Tu-22M3", 2)]).unwrap();
            let gid = flight(&mut db, Side::Red, "Tu-22M3", 2, 0.);
            db.air_stock_draw(gid, Side::Red, mozdok, &n);
        }
        assert!(!db.air_stock_side_can_fly(Side::Red, "Backfire raid", "RBACKFIRE"));
        assert!(!db.air_stock_field_ok(&mozdok, &need(&db, &[("Tu-22M3", 2)]).unwrap()));
        // a template it has never seen is left to the launch to decide
        assert!(db.air_stock_side_can_fly(Side::Red, "x", "UNKNOWN"));
    }

    #[test]
    fn losses_stay_lost_and_survivors_go_home() {
        let (mut db, mozdok, ..) = db(Some("{}"));
        let n = need(&db, &[("Tu-22M3", 2)]).unwrap();
        let gid = flight(&mut db, Side::Red, "Tu-22M3", 2, 150_000.);
        db.air_stock_draw(gid, Side::Red, mozdok, &n);
        assert_eq!(stored(&db, mozdok, "Tu-22M3"), 2);
        let u = units_of(&db, gid);
        kill(&mut db, u[0]);
        db.air_stock_count_loss(Side::Red, "Tu-22M3");
        // the engine retires it (RTB cull): the survivor goes back to Mozdok
        db.delete_group(&gid).unwrap();
        assert_eq!(stored(&db, mozdok, "Tu-22M3"), 3);
        assert!(db.persisted.air_stock.draws.is_empty());
        let oob = db.air_stock_oob();
        let tu = oob.red.iter().find(|a| a.typ.as_str() == "Tu-22M3").unwrap();
        assert_eq!((tu.stock, tu.lost), (3, 1));
        // a flight shot down to the last aircraft gives nothing back
        let gid = flight(&mut db, Side::Red, "Tu-22M3", 2, 150_000.);
        db.air_stock_draw(gid, Side::Red, mozdok, &n);
        for u in units_of(&db, gid) {
            kill(&mut db, u);
        }
        db.delete_group(&gid).unwrap();
        assert_eq!(stored(&db, mozdok, "Tu-22M3"), 1);
    }

    #[test]
    fn landing_returns_to_the_base_it_landed_at() {
        let (mut db, mozdok, kutaisi, _) = db(Some("{}"));
        let n = need(&db, &[("MiG-29S", 2)]).unwrap();
        let gid = flight(&mut db, Side::Red, "MiG-29S", 2, 0.);
        db.air_stock_draw(gid, Side::Red, mozdok, &n);
        assert_eq!(stored(&db, mozdok, "MiG-29S"), 4);
        let u = units_of(&db, gid);
        // one lands at Kutaisi (bingo fuel), the other is still flying
        db.air_stock_landed_uid(u[0], Vector2::new(50_500., 0.), t0());
        db.air_stock_tick(t0() + Duration::minutes(5));
        assert!(db.persisted.groups.get(&gid).is_some(), "not all of it is down");
        // the second touches down at Kutaisi too, then takes off again
        db.air_stock_landed_uid(
            u[1],
            Vector2::new(49_800., 0.),
            t0() + Duration::minutes(6),
        );
        for d in db.persisted.air_stock.draws.iter_mut() {
            d.landed.retain(|(l, _, _)| *l != u[1]);
        }
        db.air_stock_tick(t0() + Duration::minutes(8));
        assert!(db.persisted.groups.get(&gid).is_some());
        // down again; not yet settled, then settled
        db.air_stock_landed_uid(
            u[1],
            Vector2::new(49_800., 0.),
            t0() + Duration::minutes(9),
        );
        db.air_stock_tick(t0() + Duration::minutes(9) + Duration::seconds(10));
        assert!(db.persisted.groups.get(&gid).is_some());
        db.air_stock_tick(t0() + Duration::minutes(10));
        assert!(!db.persisted.groups.get(&gid).is_some(), "retired once all are down");
        assert_eq!(stored(&db, kutaisi, "MiG-29S"), 4);
        assert_eq!(stored(&db, mozdok, "MiG-29S"), 4);
        // landing at an enemy field is not landing at home
        let gid = flight(&mut db, Side::Red, "MiG-29S", 1, 0.);
        db.air_stock_draw(gid, Side::Red, mozdok, &need(&db, &[("MiG-29S", 1)]).unwrap());
        let u = units_of(&db, gid);
        db.air_stock_landed_uid(u[0], Vector2::new(200_000., 0.), t0());
        assert!(db.persisted.air_stock.draws[0].landed.is_empty());
    }

    #[test]
    fn without_return_on_land_only_the_retirement_returns() {
        let (mut db, mozdok, kutaisi, _) = db(Some(r#"{"return_on_land": false}"#));
        let n = need(&db, &[("MiG-29S", 1)]).unwrap();
        let gid = flight(&mut db, Side::Red, "MiG-29S", 1, 50_000.);
        db.air_stock_draw(gid, Side::Red, mozdok, &n);
        // the event hook ignores the landing with return_on_land off; even
        // a noted one is not retired by the tick
        let u = units_of(&db, gid);
        db.air_stock_landed_uid(u[0], Vector2::new(50_000., 0.), t0());
        db.air_stock_tick(t0() + Duration::minutes(5));
        assert!(db.persisted.groups.get(&gid).is_some());
        db.delete_group(&gid).unwrap();
        // it landed at Kutaisi before it was retired, so that is where it is
        assert_eq!(stored(&db, kutaisi, "MiG-29S"), 3);
        assert_eq!(stored(&db, mozdok, "MiG-29S"), 5);
    }

    #[test]
    fn a_lost_origin_sends_survivors_to_the_nearest_field() {
        let (mut db, mozdok, kutaisi, batumi) = db(Some("{}"));
        let n = need(&db, &[("MiG-29S", 2)]).unwrap();
        let gid = flight(&mut db, Side::Red, "MiG-29S", 2, 60_000.);
        db.air_stock_draw(gid, Side::Red, mozdok, &n);
        db.persisted.objectives.get_mut_cow(&mozdok).unwrap().owner = Side::Blue;
        db.delete_group(&gid).unwrap();
        assert_eq!(stored(&db, kutaisi, "MiG-29S"), 4);
        // with nowhere friendly to go they are lost
        db.persisted.objectives.get_mut_cow(&mozdok).unwrap().owner = Side::Red;
        let gid = flight(&mut db, Side::Red, "MiG-29S", 2, 60_000.);
        db.air_stock_draw(gid, Side::Red, mozdok, &n);
        for oid in [mozdok, kutaisi] {
            db.persisted.objectives.get_mut_cow(&oid).unwrap().owner = Side::Blue;
        }
        let before: u32 =
            [mozdok, kutaisi, batumi].iter().map(|o| stored(&db, *o, "MiG-29S")).sum();
        db.delete_group(&gid).unwrap();
        let after: u32 =
            [mozdok, kutaisi, batumi].iter().map(|o| stored(&db, *o, "MiG-29S")).sum();
        assert_eq!(before, after);
    }

    #[test]
    fn unlimited_bases_and_exempt_airframes_are_never_drawn() {
        let (mut db, ..) = db(Some(r#"{"exempt_actions": ["Tanker"]}"#));
        let free = base(&mut db, 9, "Sochi", "Red", 300_000., &[("MiG-29S", 1)], true);
        // exempt airframe types are left out of the need; all exempt = none
        assert!(need(&db, &[("Su-25T", 4)]).is_none());
        let n = need(&db, &[("Su-25T", 2), ("MiG-29S", 2)]).unwrap();
        assert_eq!(n.as_slice(), &[(String::from("MiG-29S"), 2)]);
        // exempt actions and templates
        let t = [(String::from("MiG-29S"), 2)];
        assert!(db.air_stock_need("Tanker", "RKC", &t).is_none());
        // an unlimited base launches anything and gives nothing
        assert!(db.air_stock_field_ok(&free, &n));
        let gid = flight(&mut db, Side::Red, "MiG-29S", 2, 300_000.);
        db.air_stock_draw(gid, Side::Red, free, &n);
        assert!(!db.persisted.air_stock.has_draw(&gid));
        assert_eq!(stored(&db, free, "MiG-29S"), 1);
        // and the totals don't count its stock, but know it is unlimited
        let tot = db.air_stock_totals();
        assert_eq!(tot[1]["mig-29s"], SideStock { stock: 8, unlimited: true });
        assert_eq!(stock_of(&tot[1], &["MiG-29S".into()]), None);
        assert_eq!(stock_of(&tot[1], &["Tu-22M3".into()]), Some(4));
    }

    #[test]
    fn losses_count_once_and_only_stock_aircraft() {
        let (mut db, mozdok, ..) = db(Some("{}"));
        db.air_stock_count_loss(Side::Blue, "F-14A");
        db.air_stock_count_loss(Side::Blue, "F-14A");
        db.air_stock_count_loss(Side::Red, "Tu-22M3");
        let oob = db.air_stock_oob();
        let f14 = oob.blue.iter().find(|a| a.typ.as_str() == "F-14A").unwrap();
        assert_eq!((f14.stock, f14.lost, f14.unlimited), (10, 2, false));
        // a type a side has lost all of still shows
        db.persisted
            .objectives
            .get_mut_cow(&mozdok)
            .unwrap()
            .warehouse
            .equipment
            .remove_cow(&String::from("Tu-22M3"));
        let oob = db.air_stock_oob();
        let tu = oob.red.iter().find(|a| a.typ.as_str() == "Tu-22M3").unwrap();
        assert_eq!((tu.stock, tu.lost), (0, 1));
        // the bookkeeping survives a save
        let back: AirStock = serde_json::from_value(
            serde_json::to_value(&db.persisted.air_stock).unwrap(),
        )
        .unwrap();
        assert_eq!(back.blue_lost.get("F-14A"), Some(&2));
    }

    #[test]
    fn the_oob_is_sent_when_it_changes_and_now_and_then() {
        let (mut db, ..) = db(Some("{}"));
        db.air_stock_tick(t0());
        let first = db.persisted.air_stock.rt.last_sent.clone().unwrap();
        assert_eq!(first.0, t0());
        // unchanged: not again until the refresh
        db.air_stock_tick(t0() + Duration::minutes(2));
        assert_eq!(db.persisted.air_stock.rt.last_sent.as_ref().unwrap().0, t0());
        db.air_stock_count_loss(Side::Red, "MiG-29S");
        // changed, but too soon after the last look
        db.air_stock_tick(t0() + Duration::minutes(2) + Duration::seconds(10));
        assert_eq!(db.persisted.air_stock.rt.last_sent.as_ref().unwrap().0, t0());
        db.air_stock_tick(t0() + Duration::minutes(4));
        assert_eq!(
            db.persisted.air_stock.rt.last_sent.as_ref().unwrap().0,
            t0() + Duration::minutes(4)
        );
        // off: nothing is sent
        let (mut db, ..) = super::tests::db(None);
        db.air_stock_tick(t0());
        assert!(db.persisted.air_stock.rt.last_sent.is_none());
    }
}
