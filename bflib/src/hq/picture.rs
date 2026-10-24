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

//! What one side can see, which is all the HQ plans from.
//!
//! - Its own objectives, forces, transports and players, in full.
//! - The enemy's objectives: who holds them and their state are public --
//!   every label on the F10 map shows them to both sides.
//! - Enemy aircraft: only what the side's radars hold (`Ewr`).
//! - Enemy ground: only what its intel holds (recon, JTACs, special forces),
//!   plus enemy formations its own forces are in contact with.
//!
//! Nothing here reads an enemy unit's true position.

use super::{dist, Handle};
use crate::{
    db::{intel::IntelUnitClass, objective::ObjGroupClass},
    Context,
};
use bfprotocols::{
    cfg::{HqCfg, UnitTag},
    db::{group::GroupId, objective::{ObjectiveId, ObjectiveKind}},
    hq::OpKind,
};
use chrono::prelude::*;
use compact_str::CompactString;
use dcso3::{coalition::Side, Vector2};
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub(crate) struct Obj {
    pub(crate) id: ObjectiveId,
    pub(crate) name: CompactString,
    pub(crate) pos: Vector2,
    pub(crate) own: bool,
    pub(crate) neutral: bool,
    pub(crate) kind: ObjectiveKind,
    pub(crate) health: u8,
    pub(crate) logi: u8,
    pub(crate) supply: u8,
    pub(crate) fuel: u8,
    pub(crate) threatened: bool,
    /// Ours, and the enemy is taking it.
    pub(crate) being_captured: bool,
    /// Theirs, and we are taking it.
    pub(crate) capturing: bool,
    pub(crate) capturable: bool,
    pub(crate) airbase: bool,
    /// Nearest objective of the other side.
    pub(crate) front_m: f64,
    /// Ours, with artillery (armour, MR or LR groups alive) that can fire.
    pub(crate) fires: bool,
    /// Something of ours (convoy, cargo flight, helo) is bound for it.
    pub(crate) inbound: bool,
}

impl Obj {
    /// What taking (or holding) it is worth, around 1.
    pub(crate) fn value(&self) -> f64 {
        match self.kind {
            ObjectiveKind::Logistics => 1.6,
            ObjectiveKind::Factory { .. } => 1.4,
            ObjectiveKind::Airbase => 1.3,
            ObjectiveKind::CommandCenter => 1.2,
            ObjectiveKind::Fob | ObjectiveKind::Farp { .. } => 1.0,
            ObjectiveKind::NavalBase => 0.9,
            ObjectiveKind::SpecialSamSite { .. } => 0.6,
            ObjectiveKind::CarrierGroup { .. } => 0.,
        }
    }

    pub(crate) fn sam_site(&self) -> bool {
        matches!(self.kind, ObjectiveKind::SpecialSamSite { .. })
    }

    pub(crate) fn at_sea(&self) -> bool {
        crate::groundwar::at_sea(&self.kind)
    }

    pub(crate) fn kind_label(&self) -> &'static str {
        match self.kind {
            ObjectiveKind::Airbase => "airbase",
            ObjectiveKind::Fob => "fob",
            ObjectiveKind::Farp { .. } => "farp",
            ObjectiveKind::Logistics => "logistics",
            ObjectiveKind::Factory { .. } => "factory",
            ObjectiveKind::CommandCenter => "command_center",
            ObjectiveKind::NavalBase => "naval_base",
            ObjectiveKind::SpecialSamSite { .. } => "sam_site",
            ObjectiveKind::CarrierGroup { .. } => "carrier_group",
        }
    }

    /// Lowest of supply and fuel, percent.
    pub(crate) fn stores(&self) -> u8 {
        self.supply.min(self.fuel)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Contact {
    pub(crate) pos: Vector2,
    pub(crate) class: IntelUnitClass,
    pub(crate) count: u32,
    pub(crate) age_mins: u32,
}

#[derive(Debug, Clone)]
pub(crate) struct Picture {
    pub(crate) objs: Vec<Obj>,
    pub(crate) humans: u32,
    pub(crate) humans_fw_air: u32,
    pub(crate) humans_helo_air: u32,
    /// Enemy aircraft the side's sensors hold.
    pub(crate) enemy_air: Vec<Vector2>,
    pub(crate) enemy_ground: Vec<Contact>,
    /// Enemy formations in contact with ours.
    pub(crate) enemy_formations: Vec<Vector2>,
    pub(crate) formations: u32,
    pub(crate) formations_idle: u32,
    /// The side's deployed cruise-missile / Scud groups.
    pub(crate) missile_groups: SmallVec<[(GroupId, Vector2); 4]>,
    /// Enemy convoys on the road within reach of our ground. A column on a
    /// road through the front is the one thing both sides can see.
    pub(crate) enemy_convoys_near: u32,
    /// Ops of ours under way, by kind and target.
    pub(crate) under_way: Vec<(OpKind, Option<ObjectiveId>)>,
    /// Our JTACs that are lasing a target: (which, where the target is).
    pub(crate) jtac_targets: SmallVec<[(crate::jtac::JtId, Vector2); 8]>,
    /// Our carrier groups still afloat.
    pub(crate) carriers: SmallVec<[Vector2; 2]>,
    /// Ground battles our formations are fighting.
    pub(crate) battles: SmallVec<[Vector2; 4]>,
}

impl Picture {
    pub(crate) fn own(&self) -> impl Iterator<Item = &Obj> {
        self.objs.iter().filter(|o| o.own)
    }

    pub(crate) fn enemy(&self) -> impl Iterator<Item = &Obj> {
        self.objs.iter().filter(|o| !o.own && !o.neutral)
    }

    pub(crate) fn get(&self, id: &ObjectiveId) -> Option<&Obj> {
        self.objs.iter().find(|o| o.id == *id)
    }

    /// Distance from `p` to the nearest ground we hold.
    pub(crate) fn gap(&self, p: Vector2) -> f64 {
        self.own().map(|o| dist(o.pos, p)).fold(f64::INFINITY, f64::min)
    }

    /// Distance from `p` to the nearest friendly airbase.
    pub(crate) fn air_gap(&self, p: Vector2) -> f64 {
        self.own().filter(|o| o.airbase).map(|o| dist(o.pos, p)).fold(f64::INFINITY, f64::min)
    }

    pub(crate) fn sams(&self) -> impl Iterator<Item = &Contact> {
        self.enemy_ground.iter().filter(|c| c.class == IntelUnitClass::AirDefense)
    }

    /// Known air defence within `r` of `p`: intel contacts and enemy SAM
    /// site objectives.
    pub(crate) fn air_defence_near(&self, p: Vector2, r: f64) -> u32 {
        let intel = self.sams().filter(|c| dist(c.pos, p) <= r).count();
        let sites = self.enemy().filter(|o| o.sam_site() && dist(o.pos, p) <= r).count();
        (intel + sites) as u32
    }

    pub(crate) fn enemy_air_near(&self, p: Vector2, r: f64) -> u32 {
        self.enemy_air.iter().filter(|a| dist(**a, p) <= r).count() as u32
    }

    pub(crate) fn busy(&self, kind: OpKind, target: Option<ObjectiveId>) -> bool {
        self.under_way.iter().any(|(k, t)| *k == kind && *t == target)
    }

    /// Where the front is: the midpoint of the closest pair of our and the
    /// enemy's objectives.
    pub(crate) fn front_point(&self) -> Option<Vector2> {
        let mut best: Option<(f64, Vector2)> = None;
        for o in self.own().filter(|o| !o.at_sea()) {
            for e in self.enemy().filter(|e| !e.at_sea()) {
                let d = dist(o.pos, e.pos);
                if best.map_or(true, |(b, _)| d < b) {
                    best = Some((d, (o.pos + e.pos) * 0.5));
                }
            }
        }
        best.map(|(_, p)| p)
    }

    /// A station `back` metres behind the front, toward the middle of our
    /// ground (never past it): where an AWACS or tanker orbits.
    pub(crate) fn station_behind_front(&self, back: f64) -> Option<Vector2> {
        let front = self.front_point()?;
        let own: Vec<Vector2> = self.own().filter(|o| !o.at_sea()).map(|o| o.pos).collect();
        if own.is_empty() {
            return None;
        }
        let centre = own.iter().fold(Vector2::new(0., 0.), |a, p| a + p) / own.len() as f64;
        let d = centre - front;
        let n = d.norm();
        if n < 1. {
            return Some(centre);
        }
        Some(front + d / n * back.min(n))
    }
}

fn has_fire_groups(ctx: &Context, side: Side, oid: &ObjectiveId) -> bool {
    let Some(obj) = ctx.db.persisted.objectives.get(oid) else { return false };
    obj.groups().get(&side).map_or(false, |gids| {
        gids.into_iter().any(|gid| {
            ctx.db.persisted.groups.get(gid).map_or(false, |g| {
                matches!(g.class, ObjGroupClass::Armor | ObjGroupClass::Mr | ObjGroupClass::Lr)
                    && g.units.into_iter().any(|uid| {
                        ctx.db.persisted.units.get(uid).map_or(false, |u| !u.dead)
                    })
            })
        })
    })
}

pub(crate) fn build(ctx: &Context, cfg: &HqCfg, side: Side, now: DateTime<Utc>) -> Picture {
    let enemy_side = side.opposite();
    let ours_capturing: Vec<ObjectiveId> = ctx.db.objectives_being_captured_by(enemy_side);
    let theirs_capturing: Vec<ObjectiveId> = ctx.db.objectives_being_captured_by(side);
    let inbound: Vec<ObjectiveId> = ctx
        .db
        .ephemeral
        .transport_destinations(side)
        .collect();
    let raw: Vec<(ObjectiveId, &crate::db::objective::Objective)> =
        ctx.db.objectives().map(|(id, o)| (*id, o)).collect();
    let mut objs: Vec<Obj> = raw
        .iter()
        .map(|(id, o)| {
            let owner = o.owner();
            let own = owner == side;
            Obj {
                id: *id,
                name: CompactString::from(o.name()),
                pos: o.pos(),
                own,
                neutral: owner == Side::Neutral,
                kind: o.kind().clone(),
                health: o.health(),
                logi: o.logi(),
                supply: o.supply(),
                fuel: o.fuel(),
                threatened: own && o.threatened(),
                being_captured: own && theirs_capturing.contains(id),
                capturing: !own && ours_capturing.contains(id),
                capturable: o.captureable(),
                airbase: o.is_airbase(),
                front_m: f64::INFINITY,
                fires: own && has_fire_groups(ctx, side, id),
                inbound: own && inbound.contains(id),
            }
        })
        .collect();
    let positions: Vec<(bool, bool, Vector2)> = objs.iter().map(|o| (o.own, o.neutral, o.pos)).collect();
    for o in objs.iter_mut() {
        o.front_m = positions
            .iter()
            .filter(|(own, neutral, _)| !*neutral && *own != o.own)
            .map(|(_, _, p)| dist(*p, o.pos))
            .fold(f64::INFINITY, f64::min);
    }

    let (mut fw, mut helo, mut humans) = (0, 0, 0);
    for (_, p, i) in ctx.db.instanced_players() {
        if p.side != side {
            continue;
        }
        humans += 1;
        if !i.in_air {
            continue;
        }
        let tags = ctx.db.ephemeral.cfg.unit_classification.get(&i.typ);
        match tags {
            Some(t) if t.contains(UnitTag::Helicopter) => helo += 1,
            Some(t) if t.contains(UnitTag::Aircraft) => fw += 1,
            _ => (),
        }
    }
    // A player in the lobby or on the slot screen is still a player the
    // side has; count whichever is bigger.
    let connected = ctx
        .connected
        .info_by_player_id
        .values()
        .filter(|ifo| ctx.db.persisted.players.get(&ifo.ucid).map_or(false, |p| p.side == side))
        .count() as u32;
    humans = humans.max(connected);

    let enemy_air = ctx.ewr.detected_enemy_positions(side, now);
    let enemy_ground: Vec<Contact> = ctx
        .db
        .ephemeral
        .intel_db
        .contacts_for(side)
        .map(|c| Contact {
            pos: c.pos,
            class: c.unit_class,
            count: c.unit_count as u32,
            age_mins: ((now - c.detected_at).num_seconds().max(0) / 60) as u32,
        })
        .collect();

    let ours: Vec<Vector2> = ctx.db.formations().filter(|f| f.side == side).map(|f| f.pos).collect();
    let own_obj_pos: Vec<Vector2> = objs.iter().filter(|o| o.own).map(|o| o.pos).collect();
    // Enemy formations as the side's own forces see them right now (the
    // ground war's spotting), never where they really are.
    let rt = &ctx.groundwar.rt;
    let enemy_formations: Vec<Vector2> = rt
        .sightings(side)
        .filter(|(_, s)| rt.in_sight(s))
        .map(|(_, s)| s.pos)
        .collect();
    // Battles our formations are in.
    let battles: SmallVec<[Vector2; 4]> = rt
        .battles()
        .iter()
        .filter(|b| b.formations.iter().any(|id| ctx.db.formation(*id).map_or(false, |f| f.side == side)))
        .map(|b| b.pos)
        .collect();
    let formations = ours.len() as u32;
    let formations_idle = ctx
        .db
        .formations()
        .filter(|f| f.side == side && f.posture == crate::db::formation::Posture::Holding)
        .count() as u32;

    let missile_groups: SmallVec<[(GroupId, Vector2); 4]> = ctx
        .db
        .deployed()
        .chain(ctx.db.actions())
        .filter(|g| g.side == side && g.tags.contains(UnitTag::ALCM))
        .filter_map(|g| ctx.db.group_center(&g.id).ok().map(|p| (g.id, p)))
        .collect();

    let reach = cfg.max_ground_range_m;
    let enemy_convoys_near = ctx
        .db
        .convoys_for_side(enemy_side)
        .filter(|c| {
            ctx.db
                .group_center(&c.group_id)
                .map(|p| own_obj_pos.iter().any(|q| dist(p, *q) <= reach))
                .unwrap_or(false)
        })
        .count() as u32;

    let jtac_targets = ctx
        .jtac
        .jtacs()
        .filter(|j| j.side() == side)
        .filter_map(|j| j.target().as_ref().map(|t| (j.gid(), Vector2::new(t.pos.x, t.pos.z))))
        .collect();
    let carriers = objs
        .iter()
        .filter(|o| o.own && o.at_sea() && o.health > 0)
        .map(|o| o.pos)
        .collect();
    let under_way = ctx
        .hq
        .sides
        .get(&side)
        .map(|rt| {
            rt.active()
                .filter(|o| !matches!(o.handle, Handle::Fired))
                .map(|o| (o.kind, o.target))
                .collect()
        })
        .unwrap_or_default();

    Picture {
        objs,
        humans,
        humans_fw_air: fw,
        humans_helo_air: helo,
        enemy_air,
        enemy_ground,
        enemy_formations,
        formations,
        formations_idle,
        missile_groups,
        enemy_convoys_near,
        under_way,
        jtac_targets,
        carriers,
        battles,
    }
}
