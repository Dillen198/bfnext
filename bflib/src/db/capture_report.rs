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

//! "Why can't this be captured?" -- the report behind the dashboard's
//! objective drawer.
//!
//! Players kept filing "this base can't be captured" bugs because nothing
//! told them what was in the way. [`Db::capture_report`] gathers the same
//! state `Db::check_capture` gates on, and [`build`] turns it into a list of
//! blockers in plain language plus the things still standing that have to
//! die. `build` is pure so the rules can be tested without DCS.
//!
//! The gating here mirrors `check_capture` (and `compute_objective_status`
//! for what counts toward health). When either changes, change this too.
//!
//! Fog of war: an attacker sees every logistics building (they are pinned on
//! the F10 map for both sides) but only the garrison units its own intel has
//! a contact on. The rest are reported by class and count, with no position.

use super::{Db, group::DeployKind, objective::ObjGroupClass};
use crate::{group, objective, unit};
use anyhow::{Result, anyhow};
use bfprotocols::{
    api::{
        CaptureBlocker, CaptureBlockerKind, CapturePerspective, CaptureProgressInfo,
        CaptureReport, CaptureTarget, CaptureTargetKind, CaptureTroopInfo,
    },
    cfg::UnitTag,
    db::objective::{ObjectiveId, ObjectiveKind},
};
use dcso3::{Vector2, coalition::Side};
use fxhash::FxHashSet;

/// `captureable()` wants health at or under this.
pub(crate) const HEALTH_THRESHOLD: u8 = 20;

/// Located entries past this are left off; a garrison is a few dozen units.
const MAX_TARGETS: usize = 120;

/// Health as `compute_objective_status` works it out: truncated for units,
/// rounded when the objective's buildings stand in for its health.
fn pct(alive: u32, total: u32, round: bool) -> u8 {
    if total == 0 {
        return 100;
    }
    let f = alive as f32 / total as f32 * 100.;
    if round { f.round() as u8 } else { f.trunc() as u8 }
}

/// How many more of `alive` (out of `total`) must be destroyed before health
/// is at or under [`HEALTH_THRESHOLD`].
pub(crate) fn kills_to_threshold(alive: u32, total: u32, round: bool) -> u32 {
    (0..=alive)
        .find(|k| pct(alive - k, total, round) <= HEALTH_THRESHOLD)
        .unwrap_or(alive)
}

fn class_name(c: ObjGroupClass) -> &'static str {
    match c {
        ObjGroupClass::Logi => "logistics",
        ObjGroupClass::Aaa => "AAA",
        ObjGroupClass::Lr => "long-range SAM",
        ObjGroupClass::Mr => "medium-range SAM",
        ObjGroupClass::Sr => "short-range SAM",
        ObjGroupClass::Armor => "armor",
        ObjGroupClass::Services => "services",
        ObjGroupClass::Naval => "ship",
        ObjGroupClass::Infantry => "infantry",
        ObjGroupClass::Other => "other",
    }
}

fn plural(n: u32, one: &str, many: &str) -> String {
    if n == 1 { format!("1 {one}") } else { format!("{n} {many}") }
}

/// A live garrison unit that counts toward health.
#[derive(Debug, Clone)]
pub(crate) struct UnitFact {
    pub typ: String,
    pub class: ObjGroupClass,
    pub group: String,
    pub pos: Vector2,
    /// The asking side may see where it is.
    pub located: bool,
}

/// A logistics building still standing.
#[derive(Debug, Clone)]
pub(crate) struct BuildingFact {
    pub label: String,
    pub pos: Vector2,
}

/// Everything [`build`] needs, gathered by [`Db::capture_report`].
#[derive(Debug, Clone)]
pub(crate) struct Facts {
    pub perspective: CapturePerspective,
    pub owner: Side,
    pub sam_site: bool,
    /// The objective's cached health and infantry, the figures the capture
    /// tick actually gates on.
    pub health: u8,
    pub infantry_pct: u8,
    pub in_hold: bool,
    /// Seconds of consolidation left, while `in_hold`.
    pub hold_left_secs: Option<i64>,
    pub cooldown_secs: Option<i64>,
    pub in_progress: Option<CaptureProgressInfo>,
    /// `capture_min_unit_pct_destroyed` (0-1) and the share destroyed now.
    pub min_destroyed: f64,
    pub destroyed: f64,
    /// Garrison units that count toward health, dead or alive.
    pub unit_total: u32,
    pub live_units: Vec<UnitFact>,
    /// Logistics buildings found at load, and the ones still standing.
    pub buildings_total: u32,
    pub buildings: Vec<BuildingFact>,
    /// `logi_from_scenery` is on and applies to this objective.
    pub scenery_mode: bool,
    /// The garrison is in DCS right now (players are near).
    pub spawned: bool,
    /// The asking side's capture squads nearby (attacker view only).
    pub troops: Vec<CaptureTroopInfo>,
    /// Capture-capable troops of more than one side are in the zone.
    pub contested: bool,
}

/// Turn gathered facts into the report. `locate` gives lat/lon and an MGRS
/// grid for a map position.
pub(crate) fn build<F>(f: &Facts, locate: F) -> CaptureReport
where
    F: Fn(Vector2) -> (Option<(f64, f64)>, Option<String>),
{
    let attacker = f.perspective == CapturePerspective::Attacker;
    let neutral = f.owner == Side::Neutral;
    // Which of the two things health is made of applies here, as in
    // `compute_objective_status`.
    let buildings_are_health =
        f.scenery_mode && f.unit_total == 0 && f.buildings_total > 0;
    let nothing_to_destroy =
        !f.sam_site && !neutral && f.unit_total == 0 && !buildings_are_health;
    let live = f.live_units.len() as u32;
    let infantry_left =
        f.live_units.iter().filter(|u| u.class.is_infantry()).count() as u32;
    let kills_needed = if f.sam_site || f.health <= HEALTH_THRESHOLD {
        0
    } else if buildings_are_health {
        kills_to_threshold(f.buildings.len() as u32, f.buildings_total, true)
    } else {
        kills_to_threshold(live, f.unit_total, false)
    };
    let threshold_met =
        f.min_destroyed <= 0. || f.sam_site || f.destroyed >= f.min_destroyed;
    let captureable = f.in_hold || (f.health <= HEALTH_THRESHOLD && f.infantry_pct == 0);
    // `check_capture`: a base in its hold or cooldown starts no timer; past
    // those, it wants `captureable() || special SAM site` and the threshold.
    let open = !f.in_hold
        && f.cooldown_secs.is_none()
        && (captureable || f.sam_site)
        && threshold_met;

    let mut blockers: Vec<CaptureBlocker> = vec![];
    let mut push = |kind, text: String, action: String| {
        blockers.push(CaptureBlocker { kind, text, action })
    };
    if f.in_hold {
        let left = f
            .hold_left_secs
            .map(|s| format!(", about {} left", fmt_secs(s)))
            .unwrap_or_default();
        push(
            CaptureBlockerKind::ConsolidationHold,
            format!(
                "Just captured by {:?} and still held by its assault troops while they consolidate{left}.",
                f.owner
            ),
            if attacker {
                "Kill the holding troops in the zone. That drops it to neutral, and then either side can capture it.".into()
            } else {
                "Keep the holding troops alive and inside the zone until it consolidates."
                    .into()
            },
        );
    } else {
        if let Some(cd) = f.cooldown_secs {
            push(
                CaptureBlockerKind::Cooldown,
                format!(
                    "It just changed hands. No new capture can start for {}.",
                    fmt_secs(cd)
                ),
                "Wait for the cooldown, with capture troops ready.".into(),
            );
        }
        if nothing_to_destroy && f.health > HEALTH_THRESHOLD {
            push(
                CaptureBlockerKind::NothingToDestroy,
                format!(
                    "Nothing here counts toward its health: it has no garrison units and no logistics buildings in its zone. Its health is stuck at {}%, so it can't be captured.",
                    f.health
                ),
                String::new(),
            );
        } else if !f.sam_site && f.health > HEALTH_THRESHOLD {
            let what = if buildings_are_health {
                plural(
                    kills_needed,
                    "more logistics building",
                    "more logistics buildings",
                )
            } else {
                plural(kills_needed, "more garrison unit", "more garrison units")
            };
            push(
                CaptureBlockerKind::Health,
                format!(
                    "Health is {}%. A capture needs {HEALTH_THRESHOLD}% or less.",
                    f.health
                ),
                if kills_needed == 0 {
                    // The cached figure lags a kill by a tick.
                    "Enough is destroyed; health updates as the kills register.".into()
                } else {
                    format!("Destroy {what}.")
                },
            );
        }
        if !f.sam_site && f.infantry_pct > 0 {
            push(
                CaptureBlockerKind::Infantry,
                if infantry_left > 0 {
                    format!(
                        "{} still alive. Every one must die.",
                        plural(
                            infantry_left,
                            "infantry defender is",
                            "infantry defenders are"
                        )
                    )
                } else {
                    "Infantry defenders are still alive. Every one must die.".into()
                },
                "Hunt down the infantry: strafe them, or bring ground forces.".into(),
            );
        }
        if !threshold_met {
            push(
                CaptureBlockerKind::DefendersDestroyed,
                format!(
                    "This campaign needs {:.0}% of the defending units destroyed first (now {:.0}%).",
                    f.min_destroyed * 100.,
                    f.destroyed * 100.
                ),
                "Destroy more of the garrison.".into(),
            );
        }
    }
    if attacker && !f.in_hold && f.in_progress.is_none() {
        let in_zone = f.troops.iter().any(|t| t.can_capture && t.in_zone);
        if f.contested {
            push(
                CaptureBlockerKind::Contested,
                "Capture troops from more than one side are in the zone, so neither side's capture runs.".into(),
                "Clear the other side's troops out of the zone.".into(),
            );
        } else if !in_zone {
            let nearest = f
                .troops
                .iter()
                .filter(|t| t.can_capture)
                .min_by(|a, b| a.outside_by_m.total_cmp(&b.outside_by_m));
            let idle_in_zone = f.troops.iter().find(|t| !t.can_capture && t.in_zone);
            let (text, action) = match (nearest, idle_in_zone) {
                (Some(t), _) => (
                    format!(
                        "Your nearest capture squad ({}) is {:.1} km outside the zone.",
                        t.name,
                        t.outside_by_m / 1000.
                    ),
                    "Move them into the zone and hold.".into(),
                ),
                (None, Some(t)) => (
                    format!("{} is in the zone but can't capture.", t.name),
                    "Bring a capture-capable squad.".into(),
                ),
                (None, None) => (
                    "None of your capture troops are near it.".into(),
                    "Fly or drive capture-capable troops into the zone and hold it."
                        .into(),
                ),
            };
            push(CaptureBlockerKind::NoTroopsInZone, text, action);
        }
    }

    let mut targets: Vec<CaptureTarget> = vec![];
    let mut unlocated = 0u32;
    if !f.in_hold && !f.sam_site {
        let mut units = f.live_units.clone();
        // Infantry first while it blocks: those are the ones that must all die.
        units.sort_by(|a, b| {
            b.class
                .is_infantry()
                .cmp(&a.class.is_infantry())
                .then(a.class.cmp(&b.class))
                .then(a.group.cmp(&b.group))
        });
        let mut hidden: Vec<(String, ObjGroupClass, u32)> = vec![];
        for u in &units {
            if u.located {
                let (ll, grid) = locate(u.pos);
                targets.push(CaptureTarget {
                    kind: CaptureTargetKind::Unit,
                    label: Some(u.typ.clone()),
                    class: class_name(u.class).into(),
                    count: 1,
                    infantry: u.class.is_infantry(),
                    lat: ll.map(|l| l.0),
                    lon: ll.map(|l| l.1),
                    grid,
                });
            } else {
                unlocated += 1;
                match hidden.iter_mut().find(|(g, c, _)| *g == u.group && *c == u.class) {
                    Some((_, _, n)) => *n += 1,
                    None => hidden.push((u.group.clone(), u.class, 1)),
                }
            }
        }
        for (_, class, n) in hidden {
            targets.push(CaptureTarget {
                kind: CaptureTargetKind::Unit,
                label: None,
                class: class_name(class).into(),
                count: n,
                infantry: class.is_infantry(),
                lat: None,
                lon: None,
                grid: None,
            });
        }
        for b in &f.buildings {
            let (ll, grid) = locate(b.pos);
            targets.push(CaptureTarget {
                kind: CaptureTargetKind::Building,
                label: Some(b.label.clone()),
                class: "building".into(),
                count: 1,
                infantry: false,
                lat: ll.map(|l| l.0),
                lon: ll.map(|l| l.1),
                grid,
            });
        }
        targets.truncate(MAX_TARGETS);
    }

    let mut notes = vec![];
    if !f.spawned && live > 0 {
        notes.push(
            "The garrison is only in the game while players are near, so the base can look empty from a distance. Its units still count.".into(),
        );
    }
    if f.scenery_mode && f.buildings_total > 0 && !buildings_are_health {
        notes.push(
            "Destroying its logistics buildings lowers logistics, not health.".into(),
        );
    }
    if f.sam_site {
        notes.push(
            "A special SAM site can be captured at any time: get capture troops into the zone.".into(),
        );
    }
    if unlocated > 0 && attacker {
        notes.push(format!(
            "{} not on your intel picture. Recon, a JTAC or a closer look will place them.",
            plural(unlocated, "defender is", "defenders are")
        ));
    }

    let summary = if let Some(p) = f.in_progress {
        format!(
            "Being captured by {:?}: held {} of about {}.",
            p.side,
            fmt_secs(p.held_secs),
            fmt_secs(p.needed_secs)
        )
    } else if f.in_hold {
        "Just captured and consolidating. Not open to a new capture yet.".into()
    } else if nothing_to_destroy && f.health > HEALTH_THRESHOLD {
        "Can't be captured: nothing here counts toward its health.".into()
    } else if let Some(cd) = f.cooldown_secs {
        format!("Just changed hands: {} before a new capture can start.", fmt_secs(cd))
    } else if !open {
        let mut parts = vec![];
        if kills_needed > 0 {
            parts.push(format!(
                "destroy {}",
                plural(kills_needed, "more target", "more targets")
            ));
        }
        if f.infantry_pct > 0 && !f.sam_site {
            parts.push("kill the infantry".into());
        }
        if !threshold_met {
            parts.push("destroy more of the garrison".into());
        }
        if parts.is_empty() {
            "Not capturable yet.".into()
        } else {
            format!("Not capturable yet: {}.", parts.join(", then "))
        }
    } else if !attacker {
        "Open to capture: enemy troops in the zone would take it.".into()
    } else if f.troops.iter().any(|t| t.can_capture && t.in_zone) && !f.contested {
        "Capturable now: your troops are in the zone. Hold position.".into()
    } else {
        "Capturable now: get capture troops into the zone.".into()
    };

    CaptureReport {
        perspective: f.perspective,
        open,
        summary,
        health_threshold: HEALTH_THRESHOLD,
        kills_needed,
        infantry_left,
        blockers,
        targets,
        unlocated,
        cooldown_secs: f.cooldown_secs,
        in_progress: f.in_progress,
        troops: f.troops.clone(),
        notes,
    }
}

fn fmt_secs(s: i64) -> String {
    let s = s.max(0);
    if s >= 3600 {
        format!("{}h{:02}m", s / 3600, (s % 3600) / 60)
    } else if s >= 60 {
        format!("{}m{:02}s", s / 60, s % 60)
    } else {
        format!("{s}s")
    }
}

impl Db {
    /// Gather [`Facts`] for one objective, as seen by `for_side` (`None`: an
    /// unscoped/admin query), and build the report.
    pub fn capture_report<F>(
        &self,
        oid: &ObjectiveId,
        for_side: Option<Side>,
        locate: F,
    ) -> Result<CaptureReport>
    where
        F: Fn(Vector2) -> (Option<(f64, f64)>, Option<String>),
    {
        let obj = objective!(self, oid)?;
        let perspective = match for_side {
            None => CapturePerspective::Admin,
            Some(s) if s == obj.owner => CapturePerspective::Defender,
            Some(_) => CapturePerspective::Attacker,
        };
        // The side whose troops would take it, for the timer and troop scan.
        let attacker = match for_side {
            Some(s) if s != obj.owner => s,
            _ => match obj.owner {
                Side::Neutral => Side::Red,
                o => o.opposite(),
            },
        };
        let diag = self.capture_diagnosis(oid, attacker)?;

        // Intel contacts the asking side holds, for placing enemy units.
        let contacts: Vec<(Vector2, f64)> = match (perspective, for_side) {
            (CapturePerspective::Attacker, Some(s)) => self
                .ephemeral
                .intel_db
                .contacts_for(s)
                .map(|c| (c.pos, (c.pos_uncertainty_m as f64).max(500.)))
                .collect(),
            _ => vec![],
        };
        let located = |p: Vector2| {
            perspective != CapturePerspective::Attacker
                || contacts
                    .iter()
                    .any(|(c, r)| na::distance(&(*c).into(), &p.into()) <= *r)
        };

        // What counts toward health: the owner's garrison, minus invincible
        // units -- the same walk `compute_objective_status` does.
        let mut unit_total = 0u32;
        let mut live_units = vec![];
        if let Some(gids) = obj.groups.get(&obj.owner) {
            for gid in gids {
                let group = group!(self, gid)?;
                for uid in &group.units {
                    let unit = unit!(self, uid)?;
                    if unit.tags.contains(UnitTag::Invincible) {
                        continue;
                    }
                    unit_total += 1;
                    if !unit.dead {
                        live_units.push(UnitFact {
                            typ: unit.typ.to_string(),
                            class: group.class,
                            group: group.name.to_string(),
                            pos: unit.pos,
                            located: located(unit.pos),
                        });
                    }
                }
            }
        }

        let scenery_mode = self.ephemeral.cfg.logi_from_scenery.is_some()
            && !matches!(obj.kind, ObjectiveKind::CarrierGroup { .. })
            && obj.fat_cow().is_none();
        let buildings_total =
            self.ephemeral.scenery_total_by_objective.get(oid).copied().unwrap_or(0);
        let mut buildings: Vec<BuildingFact> = self
            .ephemeral
            .tracked_scenery
            .values()
            .filter(|t| t.objective == *oid)
            .map(|t| BuildingFact { label: t.label.to_string(), pos: t.pos })
            .collect();
        buildings.sort_by(|a, b| a.label.cmp(&b.label));

        // Capture-capable groups of a non-owner side with a live unit in the
        // zone, by side: more than one and `check_capture` does nothing.
        let mut sides_in_zone: FxHashSet<Side> = FxHashSet::default();
        for gid in self.persisted.troops.into_iter().chain(&self.persisted.dismounts) {
            let Ok(group) = group!(self, gid) else { continue };
            let can_capture = match &group.origin {
                DeployKind::Troop { spec, .. } => spec.can_capture,
                DeployKind::Dismount { can_capture, .. } => *can_capture,
                _ => false,
            };
            if !can_capture || group.side == obj.owner {
                continue;
            }
            let in_zone = group
                .units
                .into_iter()
                .filter_map(|uid| self.persisted.units.get(uid))
                .any(|u| !u.dead && obj.zone.contains(u.pos));
            if in_zone {
                sides_in_zone.insert(group.side);
            }
        }

        let total = self.ephemeral.cfg.capture_consolidation_secs;
        let facts = Facts {
            perspective,
            owner: obj.owner,
            sam_site: obj.kind.is_special_sam_site(),
            health: obj.health,
            infantry_pct: obj.infantry,
            in_hold: obj.in_capture_hold(),
            hold_left_secs: obj.capture_hold_pct(total).map(|(_, left)| left),
            cooldown_secs: diag.cooldown_secs,
            in_progress: diag.in_progress.map(|(side, held_secs, needed_secs)| {
                CaptureProgressInfo { side, held_secs, needed_secs }
            }),
            min_destroyed: self
                .ephemeral
                .cfg
                .campaign_events
                .as_ref()
                .map(|c| c.capture_min_unit_pct_destroyed)
                .unwrap_or(0.),
            destroyed: self.defender_destruction_ratio(obj),
            unit_total,
            live_units,
            buildings_total,
            buildings,
            scenery_mode,
            spawned: obj.spawned,
            troops: if perspective == CapturePerspective::Attacker {
                diag.troops
                    .iter()
                    .map(|t| CaptureTroopInfo {
                        name: t.name.to_string(),
                        can_capture: t.can_capture,
                        in_zone: t.in_zone,
                        outside_by_m: t.outside_by,
                    })
                    .collect()
            } else {
                vec![]
            },
            contested: sides_in_zone.len() > 1,
        };
        Ok(build(&facts, locate))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unit(class: ObjGroupClass, located: bool) -> UnitFact {
        UnitFact {
            typ: match class {
                ObjGroupClass::Infantry => "Soldier AK".into(),
                ObjGroupClass::Naval => "speedboat".into(),
                _ => "T-72B".into(),
            },
            class,
            group: format!("{class:?}-1"),
            pos: Vector2::new(1000., 2000.),
            located,
        }
    }

    fn facts() -> Facts {
        Facts {
            perspective: CapturePerspective::Attacker,
            owner: Side::Blue,
            sam_site: false,
            health: 100,
            infantry_pct: 0,
            in_hold: false,
            hold_left_secs: None,
            cooldown_secs: None,
            in_progress: None,
            min_destroyed: 0.,
            destroyed: 0.,
            unit_total: 0,
            live_units: vec![],
            buildings_total: 0,
            buildings: vec![],
            scenery_mode: true,
            spawned: true,
            troops: vec![],
            contested: false,
        }
    }

    fn locate(_: Vector2) -> (Option<(f64, f64)>, Option<String>) {
        (Some((33.5, 36.3)), Some("37S BA 12345 67890".into()))
    }

    fn kinds(r: &CaptureReport) -> Vec<CaptureBlockerKind> {
        r.blockers.iter().map(|b| b.kind).collect()
    }

    fn squad(in_zone: bool, can_capture: bool, outside_by_m: f64) -> CaptureTroopInfo {
        CaptureTroopInfo {
            name: "Capture squad".into(),
            can_capture,
            in_zone,
            outside_by_m,
        }
    }

    #[test]
    fn kills_to_threshold_matches_the_health_rounding() {
        // Units truncate: 2 of 10 is 20%, so 8 must die from full.
        assert_eq!(kills_to_threshold(10, 10, false), 8);
        // 21 of 100 is 21%: one more.
        assert_eq!(kills_to_threshold(21, 100, false), 1);
        assert_eq!(kills_to_threshold(20, 100, false), 0);
        // 1 of 4 buildings rounds to 25%, so all four must go.
        assert_eq!(kills_to_threshold(4, 4, true), 4);
        // 1 of 5 is exactly 20%.
        assert_eq!(kills_to_threshold(5, 5, true), 4);
        assert_eq!(kills_to_threshold(0, 0, false), 0);
    }

    #[test]
    fn an_empty_zone_on_bare_terrain_reports_nothing_to_destroy() {
        // HJ01/HJ04 on vs1: no garrison, no logistics building in the zone.
        let r = build(&facts(), locate);
        assert!(!r.open);
        assert_eq!(kinds(&r)[0], CaptureBlockerKind::NothingToDestroy);
        assert!(r.blockers[0].action.is_empty());
        assert!(r.summary.contains("nothing here counts"));
        assert!(r.targets.is_empty());
    }

    #[test]
    fn buildings_are_the_health_when_there_is_no_garrison() {
        let mut f = facts();
        f.buildings_total = 4;
        f.buildings = (0..4)
            .map(|i| BuildingFact {
                label: format!("Warehouse {i}"),
                pos: Vector2::zeros(),
            })
            .collect();
        let r = build(&f, locate);
        assert_eq!(kinds(&r)[0], CaptureBlockerKind::Health);
        assert_eq!(r.kills_needed, 4);
        assert!(r.blockers[0].action.contains("4 more logistics buildings"));
        assert_eq!(r.targets.len(), 4);
        assert!(r.targets.iter().all(|t| t.kind == CaptureTargetKind::Building));
        assert_eq!(r.targets[0].grid.as_deref(), Some("37S BA 12345 67890"));
    }

    #[test]
    fn a_garrison_needs_health_down_and_every_infantryman_dead() {
        let mut f = facts();
        f.unit_total = 10;
        f.health = 60;
        f.infantry_pct = 50;
        f.live_units = vec![
            unit(ObjGroupClass::Armor, true),
            unit(ObjGroupClass::Armor, true),
            unit(ObjGroupClass::Armor, true),
            unit(ObjGroupClass::Infantry, true),
            unit(ObjGroupClass::Infantry, false),
            unit(ObjGroupClass::Naval, false),
        ];
        let r = build(&f, locate);
        assert_eq!(
            kinds(&r),
            vec![
                CaptureBlockerKind::Health,
                CaptureBlockerKind::Infantry,
                CaptureBlockerKind::NoTroopsInZone
            ]
        );
        assert_eq!(r.kills_needed, 4);
        assert_eq!(r.infantry_left, 2);
        assert_eq!(r.unlocated, 2);
        // Located infantry leads; the unlocated come as counts, no position.
        assert_eq!(r.targets[0].class, "infantry");
        assert!(r.targets[0].grid.is_some());
        let hidden: Vec<_> = r.targets.iter().filter(|t| t.label.is_none()).collect();
        assert_eq!(hidden.len(), 2);
        assert!(hidden.iter().all(|t| t.lat.is_none() && t.count == 1));
        assert!(r.notes.iter().any(|n| n.contains("intel")));
    }

    #[test]
    fn open_with_troops_in_the_zone_has_no_blockers() {
        let mut f = facts();
        f.unit_total = 10;
        f.health = 20;
        f.live_units =
            vec![unit(ObjGroupClass::Armor, true), unit(ObjGroupClass::Armor, true)];
        f.troops = vec![squad(true, true, 0.)];
        let r = build(&f, locate);
        assert!(r.open);
        assert!(r.blockers.is_empty());
        assert!(r.summary.starts_with("Capturable now"));
    }

    #[test]
    fn open_but_troops_outside_says_how_far() {
        let mut f = facts();
        f.owner = Side::Neutral;
        f.health = 0;
        f.troops = vec![squad(false, true, 2500.), squad(true, false, 0.)];
        let r = build(&f, locate);
        assert!(r.open);
        assert_eq!(kinds(&r), vec![CaptureBlockerKind::NoTroopsInZone]);
        assert!(r.blockers[0].text.contains("2.5 km"));
    }

    #[test]
    fn hold_and_cooldown_mirror_check_capture() {
        let mut f = facts();
        f.unit_total = 4;
        f.health = 0;
        f.in_hold = true;
        f.hold_left_secs = Some(300);
        let r = build(&f, locate);
        assert!(!r.open);
        assert_eq!(kinds(&r), vec![CaptureBlockerKind::ConsolidationHold]);
        assert!(r.blockers[0].text.contains("5m00s"));

        let mut f = facts();
        f.unit_total = 4;
        f.health = 0;
        f.cooldown_secs = Some(90);
        let r = build(&f, locate);
        assert!(!r.open);
        assert_eq!(kinds(&r)[0], CaptureBlockerKind::Cooldown);
    }

    #[test]
    fn special_sam_sites_are_always_open() {
        let mut f = facts();
        f.sam_site = true;
        f.unit_total = 6;
        f.health = 100;
        f.infantry_pct = 100;
        f.live_units = vec![unit(ObjGroupClass::Sr, true)];
        f.troops = vec![squad(true, true, 0.)];
        let r = build(&f, locate);
        assert!(r.open);
        assert!(r.blockers.is_empty());
        assert!(r.targets.is_empty());
    }

    #[test]
    fn defenders_destroyed_threshold_blocks() {
        let mut f = facts();
        f.unit_total = 10;
        f.health = 10;
        f.min_destroyed = 0.95;
        f.destroyed = 0.9;
        let r = build(&f, locate);
        assert!(!r.open);
        assert!(kinds(&r).contains(&CaptureBlockerKind::DefendersDestroyed));
    }

    #[test]
    fn contested_zone_is_named() {
        let mut f = facts();
        f.owner = Side::Neutral;
        f.health = 0;
        f.contested = true;
        f.troops = vec![squad(true, true, 0.)];
        let r = build(&f, locate);
        assert_eq!(kinds(&r), vec![CaptureBlockerKind::Contested]);
    }

    #[test]
    fn the_defender_view_has_no_troop_advice() {
        let mut f = facts();
        f.perspective = CapturePerspective::Defender;
        f.unit_total = 5;
        f.health = 100;
        f.spawned = false;
        f.live_units = vec![unit(ObjGroupClass::Armor, true); 5];
        let r = build(&f, locate);
        assert_eq!(kinds(&r), vec![CaptureBlockerKind::Health]);
        assert_eq!(r.unlocated, 0);
        assert!(r.notes.iter().any(|n| n.contains("players are near")));
    }
}
