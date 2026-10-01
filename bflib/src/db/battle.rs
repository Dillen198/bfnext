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

//! Where the ground war is being fought.
//!
//! A battle is wherever a formation is in contact: with an enemy formation,
//! or with the objective it is attacking. Contacts close together are one
//! battle, and a battle outlives its last contact by `LINGER_SECS` so a
//! lull in the shooting doesn't wipe it off the map.
//!
//! Every battle gets a ring and a label on the F10 map for both sides --
//! both sides are in it, so neither learns anything -- and the dashboard
//! shows the same. A battle being fought in DCS (a formation in it is live)
//! also gets a column of smoke over it, and every formation vehicle killed
//! in DCS burns where it died for a while: the front can be found by eye
//! from the cockpit.

use super::{
    formation::{FormationId, FormationRt, Order, ENGAGED_M},
    Db,
};
use bfprotocols::{
    cfg::GroundWarCfg,
    db::group::{GroupId, UnitId},
};
use chrono::{prelude::*, Duration};
use compact_str::format_compact;
use dcso3::{
    land::Land,
    trigger::{CircleSpec, LineType, MarkId, SideFilter, SmokePreset, TextSpec, Trigger},
    Color, LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3,
};
use fxhash::{FxHashMap, FxHashSet};
use log::{info, warn};
use smallvec::SmallVec;
use std::collections::VecDeque;

/// Contacts this close together are one battle.
const MERGE_M: f64 = 5_000.;
/// A battle seen again within this of where it was is the same battle.
const SAME_M: f64 = 6_000.;
/// How long a battle stays on the map after its last contact.
const LINGER_SECS: i64 = 300;
/// The ring drawn round a battle.
const RING_M: f64 = 2_500.;
/// Redraw a battle's marks once it has drifted this far.
const REMARK_M: f64 = 2_500.;
/// An objective this close names the battle.
const NAME_M: f64 = 20_000.;

fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

fn battle_color(a: f32) -> Color {
    Color::new(1., 0.55, 0.1, a)
}

#[derive(Debug, Clone)]
pub struct Battle {
    pub id: u32,
    pub pos: Vector2,
    pub started: DateTime<Utc>,
    pub last_seen: DateTime<Utc>,
    /// Being fought in DCS.
    pub live: bool,
    pub formations: SmallVec<[FormationId; 4]>,
    pub near: Option<String>,
    marks: Option<(MarkId, MarkId, Vector2)>,
    smoke: Option<String>,
}

#[derive(Debug, Default)]
pub struct BattleRt {
    pub(super) battles: Vec<Battle>,
    next_id: u32,
    /// Burning wrecks: effect name, when it goes out.
    fires: VecDeque<(String, DateTime<Utc>)>,
    /// Where each live formation vehicle was last seen alive. The engine
    /// resets a dead unit to its spawn point, so this is the only record of
    /// where it died.
    last_pos: FxHashMap<UnitId, Vector2>,
    alive: FxHashSet<UnitId>,
}

impl BattleRt {
    pub fn battles(&self) -> &[Battle] {
        &self.battles
    }
}

/// Merge contact points into battles: (centre, live, formations).
fn cluster(contacts: &[(Vector2, bool, FormationId)]) -> Vec<(Vector2, bool, SmallVec<[FormationId; 4]>)> {
    let mut out: Vec<(Vector2, bool, SmallVec<[FormationId; 4]>, u32)> = vec![];
    for (p, live, f) in contacts {
        match out.iter_mut().find(|(c, ..)| dist(*c, *p) <= MERGE_M) {
            Some((c, l, fs, n)) => {
                *c = (*c * *n as f64 + p) / (*n + 1) as f64;
                *n += 1;
                *l |= *live;
                if !fs.contains(f) {
                    fs.push(*f);
                }
            }
            None => out.push((*p, *live, smallvec::smallvec![*f], 1)),
        }
    }
    out.into_iter().map(|(c, l, fs, _)| (c, l, fs)).collect()
}

impl Db {
    /// Every formation's contacts this tick: (where, live, which formation).
    fn contacts(&self, rt: &FormationRt) -> Vec<(Vector2, bool, FormationId)> {
        let forms: SmallVec<[(FormationId, dcso3::coalition::Side, Vector2, Order, bool); 16]> = self
            .formations()
            .map(|f| (f.id, f.side, f.pos, f.order, rt.is_live(f)))
            .collect();
        let mut out = vec![];
        for (i, (id, side, pos, order, live)) in forms.iter().enumerate() {
            for (jd, s2, p2, _, l2) in forms.iter().skip(i + 1) {
                if s2 != side && dist(*pos, *p2) <= ENGAGED_M {
                    let mid = (pos + p2) / 2.;
                    out.push((mid, *live || *l2, *id));
                    out.push((mid, *live || *l2, *jd));
                }
            }
            if let Order::Attack(oid) = order {
                if let Some(o) = self.persisted.objectives.get(oid) {
                    let op = o.zone.pos();
                    if o.owner != *side && dist(*pos, op) <= ENGAGED_M {
                        out.push((pos * 0.4 + op * 0.6, *live, *id));
                    }
                }
            }
        }
        out
    }

    fn battle_name(&self, pos: Vector2) -> Option<String> {
        self.objectives()
            .map(|(_, o)| (dist(o.pos(), pos), o))
            .filter(|(d, _)| *d <= NAME_M)
            .min_by(|a, b| a.0.total_cmp(&b.0))
            .map(|(_, o)| o.name.clone())
    }

    fn unmark_battle(&mut self, b: &mut Battle) {
        if let Some((ring, text, _)) = b.marks.take() {
            self.ephemeral.msgs().delete_mark(ring);
            self.ephemeral.msgs().delete_mark(text);
        }
    }

    fn mark_battle(&mut self, b: &mut Battle) {
        self.unmark_battle(b);
        let v3 = |p: Vector2| LuaVec3(Vector3::new(p.x, 0., p.y));
        let ring = MarkId::new();
        self.ephemeral.msgs().circle_to_all(
            SideFilter::All,
            ring,
            CircleSpec {
                center: v3(b.pos),
                radius: RING_M,
                color: battle_color(0.9),
                fill_color: battle_color(0.12),
                line_type: LineType::Dashed,
                read_only: true,
            },
            None,
        );
        let label = match &b.near {
            Some(n) => format_compact!("GROUND BATTLE near {n}"),
            None => format_compact!("GROUND BATTLE"),
        };
        let text = MarkId::new();
        self.ephemeral.msgs().text_to_all(
            SideFilter::All,
            text,
            TextSpec {
                pos: v3(b.pos + Vector2::new(RING_M, 0.)),
                color: battle_color(1.),
                fill_color: crate::mapcolor::text_plate(),
                font_size: 10,
                read_only: true,
                text: label.into(),
            },
        );
        b.marks = Some((ring, text, b.pos));
    }

    fn start_smoke(lua: MizLua, name: &str, pos: Vector2, preset: SmokePreset, density: f32) -> bool {
        let res = Land::singleton(lua).and_then(|land| {
            let h = land.get_height(LuaVec2(pos))?;
            Trigger::singleton(lua)?.action()?.effect_smoke_big(
                LuaVec3(Vector3::new(pos.x, h, pos.y)),
                preset,
                density,
                String::from(name),
            )
        });
        match res {
            Ok(()) => true,
            Err(e) => {
                warn!("ground war: smoke {name}: {e:?}");
                false
            }
        }
    }

    fn stop_smoke(lua: MizLua, name: &str) {
        let res = Trigger::singleton(lua).and_then(|t| t.action()?.effect_smoke_stop(String::from(name)));
        if let Err(e) = res {
            warn!("ground war: stopping smoke {name}: {e:?}");
        }
    }

    /// Find this tick's battles, keep their marks and smoke in step.
    pub(super) fn track_battles(&mut self, rt: &mut FormationRt, cfg: &GroundWarCfg, lua: MizLua, now: DateTime<Utc>) {
        let found = cluster(&self.contacts(rt));
        let mut battles = std::mem::take(&mut rt.battle.battles);
        let mut seen: FxHashSet<u32> = FxHashSet::default();
        for (pos, live, forms) in found {
            match battles
                .iter_mut()
                .filter(|b| !seen.contains(&b.id))
                .min_by(|a, b| dist(a.pos, pos).total_cmp(&dist(b.pos, pos)))
                .filter(|b| dist(b.pos, pos) <= SAME_M)
            {
                Some(b) => {
                    b.pos = pos;
                    b.last_seen = now;
                    b.live = live;
                    b.formations = forms;
                    seen.insert(b.id);
                }
                None => {
                    rt.battle.next_id += 1;
                    let id = rt.battle.next_id;
                    let near = self.battle_name(pos);
                    info!("ground war: battle {id} near {near:?} (live: {live})");
                    battles.push(Battle {
                        id,
                        pos,
                        started: now,
                        last_seen: now,
                        live,
                        formations: forms,
                        near,
                        marks: None,
                        smoke: None,
                    });
                    seen.insert(id);
                }
            }
        }
        let linger = Duration::seconds(LINGER_SECS);
        let mut kept = vec![];
        for mut b in battles {
            if !seen.contains(&b.id) {
                // Nobody in contact any more: smoke out now, marks after the lull.
                b.live = false;
                b.formations.clear();
            }
            if !b.live {
                if let Some(name) = b.smoke.take() {
                    Self::stop_smoke(lua, &name);
                }
            }
            if now - b.last_seen > linger {
                info!("ground war: battle {} near {:?} over", b.id, b.near);
                self.unmark_battle(&mut b);
                continue;
            }
            if cfg.battle_marks {
                let stale = b.marks.as_ref().map_or(true, |(_, _, at)| dist(*at, b.pos) > REMARK_M);
                if stale {
                    self.mark_battle(&mut b);
                }
            } else {
                self.unmark_battle(&mut b);
            }
            if cfg.smoke && b.live && b.smoke.is_none() {
                let name = format_compact!("GW battle {}", b.id);
                if Self::start_smoke(lua, &name, b.pos, SmokePreset::LargeSmoke, 0.6) {
                    b.smoke = Some(String::from(name));
                }
            }
            kept.push(b);
        }
        rt.battle.battles = kept;
    }

    /// Set a fire where every live formation vehicle that died since the
    /// last tick was, and put out the old ones.
    pub(super) fn wreck_fires(&mut self, rt: &mut FormationRt, cfg: &GroundWarCfg, lua: MizLua, now: DateTime<Utc>) {
        let mut alive: FxHashSet<UnitId> = FxHashSet::default();
        let mut died: SmallVec<[Vector2; 8]> = smallvec::smallvec![];
        let live: SmallVec<[GroupId; 32]> = self
            .formations()
            .flat_map(|f| f.groups.iter().copied())
            .filter(|g| rt.live_group(g))
            .collect();
        {
            for gid in &live {
                let Some(g) = self.persisted.groups.get(gid) else { continue };
                for uid in &g.units {
                    let Some(u) = self.persisted.units.get(uid) else { continue };
                    if u.dead {
                        if rt.battle.alive.contains(uid) {
                            if let Some(p) = rt.battle.last_pos.get(uid) {
                                died.push(*p);
                            }
                        }
                    } else {
                        alive.insert(*uid);
                        rt.battle.last_pos.insert(*uid, u.pos);
                    }
                }
            }
        }
        rt.battle.last_pos.retain(|u, _| alive.contains(u));
        rt.battle.alive = alive;
        while let Some((name, out)) = rt.battle.fires.front().cloned() {
            if out > now {
                break;
            }
            Self::stop_smoke(lua, &name);
            rt.battle.fires.pop_front();
        }
        if !cfg.smoke || cfg.max_wreck_fires == 0 {
            return;
        }
        for pos in died {
            while rt.battle.fires.len() >= cfg.max_wreck_fires as usize {
                if let Some((name, _)) = rt.battle.fires.pop_front() {
                    Self::stop_smoke(lua, &name);
                }
            }
            rt.battle.next_id += 1;
            let name = format_compact!("GW wreck {}", rt.battle.next_id);
            if Self::start_smoke(lua, &name, pos, SmokePreset::SmallSmokeAndFire, 0.75) {
                let out = now + Duration::seconds(cfg.wreck_fire_secs as i64);
                rt.battle.fires.push_back((String::from(name), out));
            }
        }
    }

    /// Put out every fire and smoke and clear every battle mark (the ground
    /// war has been switched off).
    pub fn clear_battles(&mut self, rt: &mut FormationRt, lua: MizLua) {
        for mut b in std::mem::take(&mut rt.battle.battles) {
            self.unmark_battle(&mut b);
            if let Some(name) = b.smoke.take() {
                Self::stop_smoke(lua, &name);
            }
        }
        for (name, _) in std::mem::take(&mut rt.battle.fires) {
            Self::stop_smoke(lua, &name);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nearby_contacts_are_one_battle() {
        let c = [
            (Vector2::new(0., 0.), false, 1),
            (Vector2::new(1_000., 0.), true, 2),
            (Vector2::new(50_000., 0.), false, 3),
        ];
        let b = cluster(&c);
        assert_eq!(b.len(), 2);
        let first = b.iter().find(|(p, ..)| p.x < 10_000.).unwrap();
        assert!(first.1, "a battle is live if any contact in it is");
        assert_eq!(first.2.len(), 2);
        assert!((first.0.x - 500.).abs() < 1e-6);
    }
}
