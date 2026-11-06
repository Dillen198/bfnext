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

//! Last-defender reveal. A base can only be captured once every infantry
//! defender is dead, and finding the last few in a town was the slowest,
//! least fun part of an attack -- "hunting the last unit" is the longest
//! running complaint on rival PvP servers too. Once a base is battered down
//! to capture health, still has infantry alive and is under attack, the
//! attacking side gets a rough mark on each survivor, refreshed every
//! minute and gone as soon as it isn't needed.

use super::Db;
use bfprotocols::{cfg::UnitTag, db::objective::ObjectiveId};
use chrono::{DateTime, Duration, Utc};
use compact_str::format_compact;
use dcso3::{Vector2, coalition::Side, trigger::MarkId};
use rand::{Rng, thread_rng};
use smallvec::SmallVec;

/// How often the marks move to where the survivors are now.
const REFRESH_SECS: i64 = 60;
/// Only at or under the health a capture needs anyway.
const MAX_HEALTH: u8 = 20;
/// Marks per base at most; past this many survivors it isn't "the last few".
const MAX_MARKS: usize = 8;
/// Marks are rough: the attacker still has to find the man, just not the town.
const JITTER_M: f64 = 150.;

impl Db {
    pub(crate) fn reveal_last_defenders(&mut self, now: DateTime<Utc>) {
        let mut want: Vec<(ObjectiveId, Side, SmallVec<[Vector2; MAX_MARKS]>)> = vec![];
        for (oid, obj) in &self.persisted.objectives {
            if obj.owner == Side::Neutral
                || !obj.threatened
                || obj.health > MAX_HEALTH
                || obj.infantry == 0
            {
                continue;
            }
            let mut pts: SmallVec<[Vector2; MAX_MARKS]> = SmallVec::new();
            if let Some(gids) = obj.groups.get(&obj.owner) {
                for gid in gids {
                    let Some(g) = self.persisted.groups.get(gid) else { continue };
                    for uid in &g.units {
                        let Some(u) = self.persisted.units.get(uid) else { continue };
                        if !u.dead && u.tags.contains(UnitTag::Infantry) {
                            pts.push(u.pos);
                        }
                    }
                }
            }
            if !pts.is_empty() && pts.len() <= MAX_MARKS {
                want.push((*oid, obj.owner, pts));
            }
        }
        // Bases that no longer need them: captured, healed, or quiet.
        let stale: Vec<ObjectiveId> = self
            .ephemeral
            .last_defender_marks
            .keys()
            .filter(|o| !want.iter().any(|w| w.0 == **o))
            .copied()
            .collect();
        for oid in stale {
            self.clear_last_defender_marks(&oid);
        }
        let mut rng = thread_rng();
        for (oid, owner, pts) in want {
            let fresh = self
                .ephemeral
                .last_defender_marks
                .get(&oid)
                .is_some_and(|(_, t)| now - *t < Duration::seconds(REFRESH_SECS));
            if fresh {
                continue;
            }
            self.clear_last_defender_marks(&oid);
            let name = self
                .persisted
                .objectives
                .get(&oid)
                .map(|o| o.name.to_string())
                .unwrap_or_else(|| "the objective".to_string());
            let n = pts.len();
            let mut ids: SmallVec<[MarkId; MAX_MARKS]> = SmallVec::new();
            for p in pts {
                let a: f64 = rng.gen_range(0. ..std::f64::consts::TAU);
                let r: f64 = rng.gen_range(0. ..JITTER_M);
                let at = p + Vector2::new(a.cos(), a.sin()) * r;
                ids.push(self.ephemeral.msgs().mark_to_side(
                    owner.opposite(),
                    at,
                    true,
                    format_compact!("Last defenders of {name}: infantry here ({n} left)"),
                ));
            }
            self.ephemeral.last_defender_marks.insert(oid, (ids, now));
        }
    }

    fn clear_last_defender_marks(&mut self, oid: &ObjectiveId) {
        if let Some((ids, _)) = self.ephemeral.last_defender_marks.remove(oid) {
            for id in ids {
                self.ephemeral.msgs().delete_mark(id);
            }
        }
    }
}
