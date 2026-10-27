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

use super::{Handle, dist};
use crate::{
    Context,
    db::{intel::IntelUnitClass, objective::ObjGroupClass},
};
use bfprotocols::{
    cfg::{HqCfg, UnitTag},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    hq::OpKind,
};
use chrono::prelude::*;
use compact_str::CompactString;
use dcso3::{Vector2, coalition::Side};
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
    /// The front line as segments (`super::support::refresh_front`).
    pub(crate) front: Vec<(Vector2, Vector2)>,
    /// Our AWACS and tankers on the map, whoever sent them.
    pub(crate) support: Vec<SupportFlight>,
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
        self.own()
            .filter(|o| o.airbase)
            .map(|o| dist(o.pos, p))
            .fold(f64::INFINITY, f64::min)
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

    /// Distance from `p` to the front line (the F10 overlay's white line, or
    /// the midpoints between facing bases where there is none).
    pub(crate) fn front_dist(&self, p: Vector2) -> f64 {
        self.front.iter().map(|(a, b)| seg_dist(p, *a, *b)).fold(f64::INFINITY, f64::min)
    }

    /// The racetrack an AWACS or tanker flies around `p`: the centre and both
    /// ends, 30 km either side across the line to the nearest enemy
    /// objective -- the same track `ai_loiter_point_mission` lays out.
    fn orbit_points(&self, p: Vector2) -> [Vector2; 3] {
        let nearest =
            self.enemy().min_by(|a, b| dist(a.pos, p).total_cmp(&dist(b.pos, p)));
        let Some(e) = nearest else { return [p, p, p] };
        let d = p - e.pos;
        let n = d.norm();
        if n < 1. {
            return [p, p, p];
        }
        let across = Vector2::new(-d.y, d.x) / n * RACETRACK_HALF_M;
        [p, p + across, p - across]
    }

    fn point_clearance(&self, q: Vector2, r: &StationRules) -> f64 {
        let mut c = self.front_dist(q) - r.standoff;
        for e in self.enemy() {
            let ring = if e.sam_site() { r.sam_site.max(r.standoff) } else { r.standoff };
            c = c.min(dist(e.pos, q) - ring);
        }
        for s in self.sams() {
            c = c.min(dist(s.pos, q) - r.sam_intel);
        }
        c
    }

    /// How far an orbit at `p` stays outside every threat ring (the front's
    /// stand-off, enemy bases, SAM sites, intel's air defence), for its
    /// worst racetrack point. Negative: that far inside one.
    pub(crate) fn clearance(&self, p: Vector2, r: &StationRules) -> f64 {
        self.orbit_points(p)
            .iter()
            .map(|q| self.point_clearance(*q, r))
            .fold(f64::INFINITY, f64::min)
    }

    /// Where an AWACS or tanker should orbit: over our own ground, every
    /// point of its racetrack outside the threat rings, and of those the one
    /// closest to the front (radar cover, a short hop for the jets) and to
    /// `near` (the sector that matters). Candidates are spread around every
    /// base we hold on land. When nothing clears every ring, the one that
    /// clears them by the most. `away` keeps it at least `.1` from each
    /// `.0` (the other flights' stations). None with no ground to orbit over.
    pub(crate) fn support_station(
        &self,
        r: &StationRules,
        near: Option<Vector2>,
        away: &[(Vector2, f64)],
    ) -> Option<Station> {
        let enemy_land: Vec<Vector2> =
            self.enemy().filter(|o| !o.at_sea()).map(|o| o.pos).collect();
        let own_land: Vec<Vector2> =
            self.own().filter(|o| !o.at_sea()).map(|o| o.pos).collect();
        let nearest = |v: &[Vector2], p: Vector2| {
            v.iter().map(|q| dist(*q, p)).fold(f64::INFINITY, f64::min)
        };
        let mut cands: Vec<Vector2> = vec![];
        for o in own_land.iter() {
            cands.push(*o);
            for ring in [25_000., 50_000., 75_000.] {
                for i in 0..12 {
                    let a = i as f64 * std::f64::consts::PI / 6.;
                    cands.push(*o + Vector2::new(a.cos(), a.sin()) * ring);
                }
            }
        }
        let mut scored: Vec<Station> = cands
            .into_iter()
            // Our side of the front, near our ground, in reach of a field.
            .filter(|p| nearest(&own_land, *p) < nearest(&enemy_land, *p))
            .filter(|p| nearest(&own_land, *p) <= 100_000.)
            .filter(|p| self.air_gap(*p) <= r.reach)
            .filter(|p| away.iter().all(|(q, m)| dist(*p, *q) >= *m))
            .map(|p| Station {
                pos: p,
                clearance: self.clearance(p, r),
                front_m: self.front_dist(p),
            })
            .collect();
        if scored.is_empty() {
            return None;
        }
        let covered = |s: &Station| s.front_m <= r.cover;
        let near_m = |s: &Station| near.map_or(0., |n| dist(s.pos, n));
        let safe: Vec<&Station> =
            scored.iter().filter(|s| s.clearance >= 0. && covered(s)).collect();
        if let Some(best) = safe.iter().min_by(|a, b| {
            (a.front_m + 0.25 * near_m(a)).total_cmp(&(b.front_m + 0.25 * near_m(b)))
        }) {
            return Some(**best);
        }
        // Nothing clears every ring: the safest place that still covers the
        // front, else the safest place at all.
        if scored.iter().any(covered) {
            scored.retain(covered);
        }
        scored.into_iter().max_by(|a, b| a.clearance.total_cmp(&b.clearance))
    }

    /// The side's AWACS (or tankers) on the map.
    pub(crate) fn support_of(
        &self,
        kind: OpKind,
    ) -> impl Iterator<Item = &SupportFlight> {
        self.support.iter().filter(move |f| f.kind == kind)
    }
}

/// Half the length of an AWACS / tanker racetrack (`ai_loiter_point_mission`).
const RACETRACK_HALF_M: f64 = 30_000.;

fn seg_dist(p: Vector2, a: Vector2, b: Vector2) -> f64 {
    let ab = b - a;
    let len2 = ab.norm_squared();
    if len2 < 1. {
        return dist(p, a);
    }
    let t = ((p - a).dot(&ab) / len2).clamp(0., 1.);
    dist(a + ab * t, p)
}

/// What an AWACS or tanker orbit keeps clear of (`HqSupportCfg`).
#[derive(Debug, Clone, Copy)]
pub(crate) struct StationRules {
    /// From the front line and from every enemy objective.
    pub(crate) standoff: f64,
    pub(crate) sam_site: f64,
    pub(crate) sam_intel: f64,
    /// Furthest from the front the station may be.
    pub(crate) cover: f64,
    /// Furthest from a friendly airbase.
    pub(crate) reach: f64,
}

impl StationRules {
    pub(crate) fn of(cfg: &HqCfg, kind: OpKind) -> Self {
        let s = &cfg.support;
        Self {
            standoff: if kind == OpKind::Tanker {
                s.tanker_standoff_m
            } else {
                s.awacs_standoff_m
            },
            sam_site: s.sam_site_ring_m,
            sam_intel: s.sam_intel_ring_m,
            cover: s.awacs_cover_m,
            reach: cfg.max_air_range_m,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Station {
    pub(crate) pos: Vector2,
    /// `Picture::clearance` there.
    pub(crate) clearance: f64,
    pub(crate) front_m: f64,
}

/// One of the side's AWACS or tankers: launched by the HQ, bought by a
/// player, ordered by a commander, or respawned by a restart -- the HQ's own
/// operations don't survive a restart, the flights do.
#[derive(Debug, Clone)]
pub(crate) struct SupportFlight {
    pub(crate) gid: GroupId,
    /// Awacs or Tanker.
    pub(crate) kind: OpKind,
    pub(crate) name: CompactString,
    /// DCS type of its lead aircraft.
    pub(crate) typ: CompactString,
    pub(crate) pos: Vector2,
    /// The orbit it was given.
    pub(crate) station: Vector2,
    pub(crate) launched: DateTime<Utc>,
    /// When its action's duration runs out and it is removed.
    pub(crate) expires: Option<DateTime<Utc>>,
    /// Bought or ordered by a player (the HQ leaves those alone).
    pub(crate) player: bool,
    pub(crate) airborne: bool,
    /// Low on fuel with no tanker to take it from (`super::support`).
    pub(crate) low_fuel: bool,
}

impl SupportFlight {
    /// Time to launch its relief: its time is nearly up, or its fuel is.
    pub(crate) fn relief_due(&self, now: DateTime<Utc>, lead_secs: u32) -> bool {
        self.low_fuel
            || self.expires.map_or(false, |e| (e - now).num_seconds() <= lead_secs as i64)
    }

    /// Airborne and orbiting the station it was given.
    pub(crate) fn on_station(&self) -> bool {
        self.airborne && dist(self.pos, self.station) <= 45_000.
    }
}

/// The side's AWACS and tankers on the map, alive and not sent home.
pub(crate) fn support_flights(ctx: &Context, side: Side) -> Vec<SupportFlight> {
    use crate::{db::group::DeployKind, spawnctx::SpawnLoc};
    use bfprotocols::cfg::{ActionKind, AwacsCfg};
    let low = ctx.hq.sides.get(&side).map(|rt| &rt.low_fuel);
    ctx.db
        .actions()
        .filter(|g| g.side == side)
        .filter_map(|g| {
            let DeployKind::Action { spec, loc, player, time, .. } = &g.origin else {
                return None;
            };
            let (kind, plane) = match &spec.kind {
                ActionKind::Awacs(AwacsCfg { plane, .. }) => (OpKind::Awacs, plane),
                ActionKind::Tanker(plane) => (OpKind::Tanker, plane),
                _ => return None,
            };
            if ctx.airlife.sent_home(&g.id) {
                return None;
            }
            let lead = g
                .units
                .into_iter()
                .filter_map(|uid| ctx.db.persisted.units.get(uid))
                .find(|u| !u.dead)?;
            let station = match loc {
                SpawnLoc::InAir { pos, .. } => *pos,
                _ => lead.pos,
            };
            Some(SupportFlight {
                gid: g.id,
                kind,
                name: CompactString::from(g.name.as_str()),
                typ: CompactString::from(lead.typ.0.as_str()),
                pos: lead.pos,
                station,
                launched: *time,
                expires: plane
                    .duration
                    .map(|h| *time + chrono::Duration::hours(h as i64)),
                player: player.is_some(),
                airborne: lead.airborne_velocity.is_some(),
                low_fuel: low.map_or(false, |l| l.contains(&g.id)),
            })
        })
        .collect()
}

fn has_fire_groups(ctx: &Context, side: Side, oid: &ObjectiveId) -> bool {
    let Some(obj) = ctx.db.persisted.objectives.get(oid) else { return false };
    obj.groups().get(&side).map_or(false, |gids| {
        gids.into_iter().any(|gid| {
            ctx.db.persisted.groups.get(gid).map_or(false, |g| {
                matches!(
                    g.class,
                    ObjGroupClass::Armor | ObjGroupClass::Mr | ObjGroupClass::Lr
                ) && g
                    .units
                    .into_iter()
                    .any(|uid| ctx.db.persisted.units.get(uid).map_or(false, |u| !u.dead))
            })
        })
    })
}

pub(crate) fn build(
    ctx: &Context,
    cfg: &HqCfg,
    side: Side,
    now: DateTime<Utc>,
) -> Picture {
    let enemy_side = side.opposite();
    let ours_capturing: Vec<ObjectiveId> =
        ctx.db.objectives_being_captured_by(enemy_side);
    let theirs_capturing: Vec<ObjectiveId> = ctx.db.objectives_being_captured_by(side);
    let inbound: Vec<ObjectiveId> =
        ctx.db.ephemeral.transport_destinations(side).collect();
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
    let positions: Vec<(bool, bool, Vector2)> =
        objs.iter().map(|o| (o.own, o.neutral, o.pos)).collect();
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
        .filter(|ifo| {
            ctx.db.persisted.players.get(&ifo.ucid).map_or(false, |p| p.side == side)
        })
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

    let ours: Vec<Vector2> =
        ctx.db.formations().filter(|f| f.side == side).map(|f| f.pos).collect();
    let own_obj_pos: Vec<Vector2> =
        objs.iter().filter(|o| o.own).map(|o| o.pos).collect();
    // Enemy formations as the side's own forces see them right now (the
    // ground war's spotting), never where they really are.
    let rt = &ctx.groundwar.rt;
    let enemy_formations: Vec<Vector2> =
        rt.sightings(side).filter(|(_, s)| rt.in_sight(s)).map(|(_, s)| s.pos).collect();
    // Battles our formations are in.
    let battles: SmallVec<[Vector2; 4]> = rt
        .battles()
        .iter()
        .filter(|b| {
            b.formations
                .iter()
                .any(|id| ctx.db.formation(*id).map_or(false, |f| f.side == side))
        })
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
        .filter_map(|j| {
            j.target().as_ref().map(|t| (j.gid(), Vector2::new(t.pos.x, t.pos.z)))
        })
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

    // The front line the F10 overlay draws, else (no contested stretch) the
    // midpoint between each of our bases and the enemy's nearest to it.
    let mut front: Vec<(Vector2, Vector2)> =
        ctx.hq.front.as_ref().map(|(_, f)| f.clone()).unwrap_or_default();
    if front.is_empty() {
        let land = |own: bool| {
            objs.iter()
                .filter(move |o| o.own == own && !o.neutral && !o.at_sea())
                .map(|o| o.pos)
        };
        for p in land(true) {
            if let Some(q) =
                land(false).min_by(|a, b| dist(*a, p).total_cmp(&dist(*b, p)))
            {
                let m = (p + q) * 0.5;
                front.push((m, m));
            }
        }
    }
    let support = support_flights(ctx, side);

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
        front,
        support,
    }
}

// ---------------------------------------------------------------------------
// The objective summary the rules mode scores (`super::rules`)
// ---------------------------------------------------------------------------

/// One objective, as the rules mode sees it. Built straight from the db
/// (`side_objectives`), without the rest of the HQ picture: the rules mode
/// runs on servers with no HQ config, and its numbers are the ones the old
/// Smart Commander event picker used.
#[derive(Debug, Clone)]
pub(crate) struct ObjSummary {
    pub(crate) oid: ObjectiveId,
    pub(crate) pos: Vector2,
    pub(crate) health: u8,
    pub(crate) supply: u8,
    pub(crate) threatened: bool,
    /// Enemy troops actively in the capture zone right now.
    pub(crate) being_captured: bool,
    /// logi == 0 -- no logistics units, can be captured immediately.
    pub(crate) capturable: bool,
    /// A logistics hub -- extra strategic value.
    pub(crate) is_logi_hub: bool,
    /// A factory -- extra strategic value.
    pub(crate) is_factory: bool,
    /// Has alive Armor/Mr/Lr groups that can fire on enemies.
    pub(crate) has_fire_groups: bool,
    /// Display name, for messages.
    pub(crate) name: CompactString,
}

/// Every objective `side` owns, summarised for the rules mode.
pub(crate) fn side_objectives(db: &crate::db::Db, side: Side) -> Vec<ObjSummary> {
    let being_captured: Vec<ObjectiveId> = db.objectives_being_captured_by(side);
    db.persisted
        .objectives
        .into_iter()
        .filter(|(_, o)| o.owner() == side)
        .map(|(id, obj)| {
            let has_fire_groups = obj
                .groups()
                .get(&side)
                .map(|gids| {
                    gids.into_iter().any(|gid| {
                        db.persisted
                            .groups
                            .get(gid)
                            .map(|g| {
                                matches!(
                                    g.class,
                                    ObjGroupClass::Armor
                                        | ObjGroupClass::Mr
                                        | ObjGroupClass::Lr
                                ) && g.units.into_iter().any(|uid| {
                                    db.persisted
                                        .units
                                        .get(uid)
                                        .map(|u| !u.dead)
                                        .unwrap_or(false)
                                })
                            })
                            .unwrap_or(false)
                    })
                })
                .unwrap_or(false);
            ObjSummary {
                oid: *id,
                pos: obj.pos(),
                health: obj.health(),
                supply: obj.supply(),
                threatened: obj.threatened(),
                being_captured: being_captured.contains(id),
                capturable: obj.captureable(),
                is_logi_hub: matches!(obj.kind(), ObjectiveKind::Logistics),
                is_factory: matches!(obj.kind(), ObjectiveKind::Factory { .. }),
                has_fire_groups,
                name: CompactString::from(obj.name()),
            }
        })
        .collect()
}

/// `side`'s deployed or action groups that can fire cruise missiles (tagged
/// `ALCM`), with where they are (`None` when the db can't place one).
pub(crate) fn missile_groups(
    db: &crate::db::Db,
    side: Side,
) -> SmallVec<[(GroupId, Option<Vector2>); 4]> {
    db.deployed()
        .chain(db.actions())
        .filter(|g| g.side == side && g.tags.contains(UnitTag::ALCM))
        .map(|g| (g.id, db.group_center(&g.id).ok()))
        .collect()
}

/// `side`'s players in fixed-wing aircraft right now (helicopters don't
/// count; a slot whose type isn't classified does).
pub(crate) fn fixedwing_pilots(db: &crate::db::Db, side: Side) -> u32 {
    db.persisted
        .players()
        .into_iter()
        .filter(|(_, p)| {
            if p.side != side {
                return false;
            }
            if let Some((_, Some(inst))) = &p.current_slot {
                match db.ephemeral.cfg.unit_classification.get(&inst.typ) {
                    Some(tags) => {
                        tags.contains(UnitTag::Aircraft)
                            && !tags.contains(UnitTag::Helicopter)
                    }
                    None => true,
                }
            } else {
                false
            }
        })
        .count() as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    fn obj(id: i64, own: bool, x_km: f64, y_km: f64, kind: ObjectiveKind) -> Obj {
        Obj {
            id: ObjectiveId::from(id),
            name: format!("obj{id}").into(),
            pos: Vector2::new(x_km * 1000., y_km * 1000.),
            own,
            neutral: false,
            airbase: matches!(kind, ObjectiveKind::Airbase),
            kind,
            health: 100,
            logi: 100,
            supply: 100,
            fuel: 100,
            threatened: false,
            being_captured: false,
            capturing: false,
            capturable: true,
            front_m: f64::INFINITY,
            fires: false,
            inbound: false,
        }
    }

    /// Ours west of x = 0, theirs east of it, a front line running north-south
    /// at x = 0.
    fn picture(objs: Vec<Obj>) -> Picture {
        Picture {
            objs,
            humans: 0,
            humans_fw_air: 0,
            humans_helo_air: 0,
            enemy_air: vec![],
            enemy_ground: vec![],
            enemy_formations: vec![],
            formations: 0,
            formations_idle: 0,
            missile_groups: SmallVec::new(),
            enemy_convoys_near: 0,
            under_way: vec![],
            jtac_targets: SmallVec::new(),
            carriers: SmallVec::new(),
            battles: SmallVec::new(),
            front: vec![(Vector2::new(0., -300_000.), Vector2::new(0., 300_000.))],
            support: vec![],
        }
    }

    fn rules() -> StationRules {
        StationRules {
            standoff: 120_000.,
            sam_site: 110_000.,
            sam_intel: 60_000.,
            cover: 230_000.,
            reach: 400_000.,
        }
    }

    #[test]
    fn the_awacs_orbits_well_behind_the_front() {
        let mut objs = vec![];
        for (i, x) in [-20., -60., -100., -140., -180., -220.].iter().enumerate() {
            objs.push(obj(i as i64, true, *x, 0., ObjectiveKind::Airbase));
        }
        objs.push(obj(10, false, 25., 0., ObjectiveKind::Airbase));
        objs.push(obj(11, false, 60., 80., ObjectiveKind::Fob));
        let pic = picture(objs);
        let st = pic.support_station(&rules(), None, &[]).unwrap();
        assert!(st.clearance >= 0., "{st:?}");
        // Every racetrack point at least the stand-off from the front.
        for p in pic.orbit_points(st.pos) {
            assert!(-p.x >= 119_000., "{p:?} too close to the front");
        }
        // And no further back than it has to be.
        assert!(st.front_m <= 160_000., "{st:?}");
    }

    #[test]
    fn the_awacs_keeps_out_of_a_sam_site_ring() {
        let mut objs = vec![];
        for (i, x) in [-20., -60., -100., -140., -180., -220.].iter().enumerate() {
            objs.push(obj(i as i64, true, *x, 0., ObjectiveKind::Airbase));
            objs.push(obj(20 + i as i64, true, *x, 150., ObjectiveKind::Fob));
        }
        objs.push(obj(10, false, 25., 0., ObjectiveKind::Airbase));
        // A long-range SAM site just over the line, opposite the southern bases.
        objs.push(obj(11, false, 10., 0., ObjectiveKind::SpecialSamSite));
        let pic = picture(objs);
        let st = pic.support_station(&rules(), None, &[]).unwrap();
        assert!(st.clearance >= 0., "{st:?}");
        for p in pic.orbit_points(st.pos) {
            assert!(
                dist(p, Vector2::new(10_000., 0.)) >= 120_000.,
                "{p:?} inside the SAM ring"
            );
        }
    }

    #[test]
    fn a_second_station_keeps_away_from_the_first() {
        let mut objs = vec![];
        for (i, x) in [-20., -100., -180.].iter().enumerate() {
            for (j, y) in [-200., 0., 200.].iter().enumerate() {
                objs.push(obj((i * 3 + j) as i64, true, *x, *y, ObjectiveKind::Airbase));
            }
        }
        objs.push(obj(50, false, 25., 0., ObjectiveKind::Airbase));
        let pic = picture(objs);
        let first = pic.support_station(&rules(), None, &[]).unwrap();
        let second =
            pic.support_station(&rules(), None, &[(first.pos, 200_000.)]).unwrap();
        assert!(dist(first.pos, second.pos) >= 200_000.);
    }
}
