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

use super::ground_insertion::GroundInsertion;
use super::logistics::PendingCargo;
use super::tasks::{Task, TaskId};
use super::{
    Map, MapM, MapS, Set, SetM, SetS,
    group::{SpawnedGroup, SpawnedUnit},
    objective::Objective,
    player::Player,
};
use crate::navaids::Navaid;
use bfprotocols::db::{
    group::{GroupId, UnitId},
    objective::ObjectiveId,
};
use chrono::prelude::*;
use compact_str::CompactString;
use dcso3::{String, coalition::Side, net::Ucid};
use serde_derive::{Deserialize, Serialize};

/// The save format this build writes. Bump it whenever a change to
/// `Persisted` (or anything inside it) means an older engine would read a new
/// save wrongly or drop data from it -- not for a plain `#[serde(default)]`
/// addition, which older engines already ignore. Saves from before this
/// field existed load as version 0.
///
/// 2: campaign timestamps are campaign time (`crate::clock`), and the save
/// carries the clock (`Persisted::clock`) so the next mission resumes it.
/// Before 2 they were wall clock; such a save has no clock, and the campaign
/// clock then starts at the wall clock -- what those timestamps mean -- so
/// they need no rewriting. An older engine reading a schema 2 save compares
/// its timestamps with the wall clock, which is at most the paused time of
/// the last session off.
pub const SAVE_SCHEMA_VERSION: u32 = 2;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Persisted {
    /// The `SAVE_SCHEMA_VERSION` of the engine that wrote this save (0 for
    /// saves older than the field). Set on every snapshot.
    #[serde(default)]
    pub schema_version: u32,
    pub groups: Map<GroupId, SpawnedGroup>,
    pub units: Map<UnitId, SpawnedUnit>,
    pub groups_by_name: Map<String, GroupId>,
    pub units_by_name: Map<String, UnitId>,
    pub groups_by_side: MapS<Side, Set<GroupId>>,
    pub deployed: SetM<GroupId>,
    pub farps: SetS<ObjectiveId>,
    pub crates: SetM<GroupId>,
    pub troops: SetM<GroupId>,
    pub jtacs: SetM<GroupId>,
    pub ewrs: SetS<GroupId>,
    #[serde(default)]
    pub actions: SetS<GroupId>,
    pub objectives: MapM<ObjectiveId, Objective>,
    pub objectives_by_name: MapM<String, ObjectiveId>,
    pub objectives_by_group: MapM<GroupId, ObjectiveId>,
    pub players: Map<Ucid, Player>,
    #[serde(default)]
    pub logistics_hubs: SetS<ObjectiveId>,
    #[serde(default)]
    pub naval_bases: SetS<ObjectiveId>,
    #[serde(default)]
    pub carrier_groups: SetS<ObjectiveId>,
    #[serde(default)]
    pub factories: SetS<ObjectiveId>,
    #[serde(default)]
    pub special_sam_sites: SetS<ObjectiveId>,
    #[serde(default)]
    pub command_centers: SetS<ObjectiveId>,
    /// SAM site -> its auto-linked nearest friendly CommandCenter (set at
    /// mission init, same pattern as CarrierGroup.parent_naval_base). A SAM
    /// site missing an entry here, or whose linked command center is dead
    /// or enemy-owned, loses IADN network cueing and falls back to plain
    /// DCS AI.
    #[serde(default)]
    pub sam_command_center_link: MapS<ObjectiveId, ObjectiveId>,
    #[serde(default)]
    pub downed_pilots: SetS<GroupId>,
    #[serde(default)]
    pub dismounts: SetS<GroupId>,
    /// Spawn UTC timestamp for each downed pilot (used for capture timer)
    #[serde(default)]
    pub downed_pilot_spawn_times: MapS<GroupId, DateTime<Utc>>,
    #[serde(default)]
    pub nukes_used: u32,
    #[serde(default)]
    pub logistics_ticks_since_delivery: u32,
    #[serde(default)]
    pub oid: i64,
    #[serde(default)]
    pub gid: i64,
    #[serde(default)]
    pub uid: i64,
    #[serde(default)]
    pub migrated_v0: bool,
    /// Coalition treasury balances (Smart Commander).
    #[serde(default)]
    pub blue_treasury: i64,
    #[serde(default)]
    pub red_treasury: i64,
    /// When this round began (first seen by an engine that tracks it), for
    /// `Cfg::max_round_days`.
    #[serde(default)]
    pub round_started: Option<DateTime<Utc>>,
    /// The round's war-effort bar (`db::war_effort`).
    #[serde(default)]
    pub war_effort: super::war_effort::WarEffort,
    /// Sides whose base funds have been seeded from `objective_start_points`
    /// this round. Load used to re-seed every base whose fund was exactly 0
    /// on every restart -- and size the share against only those bases, so a
    /// single drained base got the side's whole start budget each restart.
    /// Seeding now happens once per side per round.
    #[serde(default)]
    pub funds_seeded: Vec<Side>,
    /// Auto-generated navaids per objective (see `crate::navaids`). One entry
    /// for a ground objective; one per aircraft-carrying ship for a carrier
    /// task force. Rebuilt deterministically whenever the objective set
    /// changes, so it's safe if this loads empty (or single-valued) from an
    /// older save.
    #[serde(default, deserialize_with = "de_navaids_compat")]
    pub navaids: MapS<ObjectiveId, Vec<Navaid>>,
    /// Cargo taken out of a warehouse and handed to an in-flight convoy /
    /// air route / sea route, keyed by that route's id. The routes
    /// themselves are ephemeral (their DCS groups don't survive a mission
    /// load), so this ledger is what lets a restart refund the stock that
    /// was on the road instead of deleting it. See
    /// `Db::reconcile_pending_cargo`.
    #[serde(default)]
    pub pending_cargo: MapS<CompactString, PendingCargo>,
    /// Each side's territory score at the start of the campaign. Production
    /// scaling is the ratio of a side's current score to this, so it has to
    /// survive restarts -- re-baselining on load would hand a side that has
    /// lost half the map its full production back.
    #[serde(default)]
    pub production_baseline: MapS<Side, f64>,
    /// How many of each objective's logistics buildings have been bombed out,
    /// under `logi_from_scenery`.
    ///
    /// DCS's own terrain destruction does not survive a server restart, so
    /// without this the campaign's whole logistics picture healed itself every
    /// time the mission reloaded: a hub you spent a night flattening was back
    /// at 100% after the next scheduled restart. The rubble does come back
    /// standing in-game -- this is the campaign remembering that it shouldn't
    /// count.
    #[serde(default)]
    pub scenery_destroyed: MapS<ObjectiveId, u32>,
    /// The coalition tasking boards -- player posted CAP / CAS / LOGISTICS
    /// requests drawn on the F10 map. See `crate::db::tasks`.
    #[serde(default)]
    pub tasks: MapS<TaskId, Task>,
    /// When each dismount squad was spawned, for `dismount_ttl_secs`. A
    /// squad missing an entry (older save) starts its clock on the first
    /// check. See `Db::expire_dismounts`.
    #[serde(default)]
    pub dismount_spawned: MapS<GroupId, DateTime<Utc>>,
    /// Mirror of `Ephemeral::last_stand_state`, written on snapshot and read
    /// back on load. The live value is ephemeral so readers outside the db
    /// module keep working; persisting it is what stops a scheduled restart
    /// from quietly cancelling a losing side's last-stand countdown.
    #[serde(default)]
    pub last_stand_state: Option<(DateTime<Utc>, Side)>,
    /// The campaign clock at the snapshot (schema 2 on). `None` in older
    /// saves, see `SAVE_SCHEMA_VERSION`.
    #[serde(default)]
    pub clock: Option<crate::clock::SavedClock>,
    /// Mirror of `Ephemeral::last_owner_change` (same arrangement as
    /// `last_stand_state`), so the post-capture cooldown survives a restart
    /// instead of a restart re-opening every freshly flipped base at once.
    #[serde(default)]
    pub last_owner_change: MapS<ObjectiveId, DateTime<Utc>>,
    /// When each AI Fat Cow packs up. The helicopter on station is not
    /// respawned after a restart, so without this its FARP would stay up for
    /// good. See `crate::db::fat_cow`.
    #[serde(default)]
    pub fat_cow_expiry: MapS<ObjectiveId, DateTime<Utc>>,
    /// Troop insertions on their way in by road after the helo failed, by
    /// id. Persisted so a restart can refund them: the vehicle itself is not
    /// respawned. See `crate::db::ground_insertion`.
    #[serde(default)]
    pub ground_insertions: MapS<CompactString, GroundInsertion>,
    /// Ground formations in the field (`Cfg::ground_war`), by id. Their
    /// groups are out of their home objective's `groups` while they are
    /// here. See `crate::db::formation`.
    #[serde(default)]
    pub formations: MapS<super::formation::FormationId, super::formation::Formation>,
    /// The theatre HQ's standing orders and record (`crate::hq`): human
    /// overrides, the strategist's directive and how each kind of operation
    /// has gone, so a scheduled restart neither drops a human commander's
    /// orders nor forgets what the HQ has learned.
    #[serde(default)]
    pub hq: crate::hq::HqSaved,
}

impl Persisted {
    /// What to say about a loaded save's schema version, if anything: a save
    /// from a newer engine still loads (unknown fields are ignored), but
    /// whatever that engine added is lost at the next save, and anything it
    /// changed may be misread.
    pub fn schema_warning(&self) -> Option<std::string::String> {
        (self.schema_version > SAVE_SCHEMA_VERSION).then(|| {
            format!(
                "the save file was written by a newer engine (save schema {}, this build                  understands up to {SAVE_SCHEMA_VERSION}). It is loaded anyway, but anything                  that engine added will be dropped at the next save and anything it changed                  may be misread. Roll forward to the newer engine, or restore a backup from                  before the upgrade, to be safe.",
                self.schema_version
            )
        })
    }
}

/// Backward compatibility: saves written before the per-ship rework stored
/// `navaids` as `MapS<ObjectiveId, Navaid>` (one navaid per objective), and
/// the `Navaid` struct itself has changed shape a few times. Read the field
/// as raw JSON and convert best-effort — anything that doesn't parse is
/// dropped, which is safe because `navaids::reallocate` regenerates the
/// whole table on the next objective-set change anyway.
fn de_navaids_compat<'de, D>(
    d: D,
) -> std::result::Result<MapS<ObjectiveId, Vec<Navaid>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let raw: serde_json::Value = serde::Deserialize::deserialize(d)?;
    let Some(obj) = raw.as_object() else {
        return Ok(MapS::default());
    };
    let mut out: Vec<(ObjectiveId, Vec<Navaid>)> = Vec::new();
    for (k, v) in obj {
        let Ok(id) = k.parse::<i64>() else { continue };
        let oid = ObjectiveId::from(id);
        let navs: Vec<Navaid> = if v.is_array() {
            serde_json::from_value(v.clone()).unwrap_or_default()
        } else if v.is_object() {
            match serde_json::from_value::<Navaid>(v.clone()) {
                Ok(n) => vec![n],
                Err(_) => continue,
            }
        } else {
            continue;
        };
        if !navs.is_empty() {
            out.push((oid, navs));
        }
    }
    Ok(out.into_iter().collect())
}

impl Persisted {
    pub fn players(&self) -> &Map<Ucid, Player> {
        &self.players
    }

    pub fn treasury(&self, side: Side) -> i64 {
        match side {
            Side::Blue => self.blue_treasury,
            Side::Red => self.red_treasury,
            _ => 0,
        }
    }

    pub fn adjust_treasury(&mut self, side: Side, delta: i64) -> i64 {
        let t = match side {
            Side::Blue => &mut self.blue_treasury,
            Side::Red => &mut self.red_treasury,
            _ => return 0,
        };
        *t = (*t + delta).max(0);
        *t
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    #[test]
    fn the_clock_round_trips() {
        let mut p = Persisted::default();
        let wall = Utc.with_ymd_and_hms(2026, 10, 27, 12, 0, 0).unwrap();
        p.clock = Some(crate::clock::SavedClock {
            campaign: wall - chrono::Duration::minutes(5),
            wall,
        });
        let back: Persisted =
            serde_json::from_value(serde_json::to_value(&p).unwrap()).unwrap();
        assert_eq!(back.clock, p.clock);
    }

    /// A save in the format written before `schema_version` existed.
    const V0: &str = include_str!("testdata/persisted_v0.json");

    #[test]
    fn old_save_loads_as_schema_0() {
        let p: Persisted = serde_json::from_str(V0).unwrap();
        assert_eq!(p.schema_version, 0);
        // no campaign clock: it resumes at the wall clock, which is what an
        // old save's timestamps were measured on
        assert!(p.clock.is_none());
        assert!(p.schema_warning().is_none());
        assert_eq!((p.oid, p.gid, p.uid), (2, 10, 20));
        assert_eq!((p.blue_treasury, p.red_treasury), (1000, 900));
        assert!(p.migrated_v0);
        let obj = p.objectives.get(&ObjectiveId::from(1)).unwrap();
        assert_eq!(
            (obj.name.as_str(), obj.owner, obj.points),
            ("Batumi", Side::Blue, 250)
        );
        let ucid: Ucid = "0123456789abcdef0123456789abcdef".parse().unwrap();
        let player = p.players.get(&ucid).unwrap();
        assert_eq!(
            (player.name.as_str(), player.points, player.sorties),
            ("Test Pilot", 42, 7)
        );
        assert_eq!(p.funds_seeded, vec![Side::Blue, Side::Red]);
    }

    /// Every field in the golden file comes back out exactly as it went in.
    /// A rename, a changed representation or a dropped field fails here; a
    /// new `#[serde(default)]` field only adds a key, which is allowed.
    #[test]
    fn golden_round_trip() {
        let golden: Value = serde_json::from_str(V0).unwrap();
        let p: Persisted = serde_json::from_str(V0).unwrap();
        let out = serde_json::to_value(&p).unwrap();
        fn check(path: &str, want: &Value, got: &Value) {
            match (want, got) {
                (Value::Object(w), Value::Object(g)) => {
                    for (k, wv) in w {
                        let gv = g.get(k).unwrap_or_else(|| {
                            panic!("{path}.{k} missing after round trip")
                        });
                        check(&format!("{path}.{k}"), wv, gv);
                    }
                }
                (w, g) => assert_eq!(w, g, "{path} changed in the round trip"),
            }
        }
        check("save", &golden, &out);
        // and what we write reads back to the same thing
        let again: Persisted = serde_json::from_value(out.clone()).unwrap();
        assert_eq!(serde_json::to_value(&again).unwrap(), out);
    }

    #[test]
    fn snapshot_stamps_the_current_schema() {
        let mut p: Persisted = serde_json::from_str(V0).unwrap();
        p.schema_version = SAVE_SCHEMA_VERSION;
        let out = serde_json::to_value(&p).unwrap();
        assert_eq!(out["schema_version"], Value::from(SAVE_SCHEMA_VERSION));
    }

    #[test]
    fn newer_save_still_loads_with_a_warning() {
        let mut v: Value = serde_json::from_str(V0).unwrap();
        v["schema_version"] = Value::from(SAVE_SCHEMA_VERSION + 1);
        v["a_field_from_the_future"] = Value::from(true);
        let p: Persisted = serde_json::from_value(v).unwrap();
        assert_eq!(p.schema_version, SAVE_SCHEMA_VERSION + 1);
        assert!(p.schema_warning().unwrap().contains("newer engine"));
        assert_eq!(p.blue_treasury, 1000);
    }
}
