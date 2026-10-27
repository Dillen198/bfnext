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

//! Paid tasking. At slot entry every pilot is given one task from the side's
//! ranked tasking list -- the most urgent one their airframe can do -- with a
//! zone marked on their own F10 map (`situation::schedule_slot_briefing`).
//! Inside that zone kills pay `TASK_POINTS_MULT` times the points, and kills,
//! captures and logistics delivered to an objective in it earn bonus XP
//! (`Stat::PilotXp`, scored by bfdb's progression). It pulls play toward what
//! the side actually needs instead of the nearest furball.

use super::Db;
use bfprotocols::stats::Stat;
use chrono::{DateTime, Duration, Utc};
use compact_str::format_compact;
use dcso3::{String, Vector2, env::miz::GroupId, net::Ucid, trigger::MarkId};

/// Points multiplier for a kill inside the pilot's task zone.
pub(crate) const TASK_POINTS_MULT: f64 = 1.5;
/// Bonus XP for a kill inside the zone.
pub(crate) const TASK_KILL_XP: u32 = 150;
/// Bonus XP for a capture, repair, supply run or FARP at an objective in it.
pub(crate) const TASK_JOB_XP: u32 = 300;
/// How long a task stays assigned if the pilot never leaves the slot.
const TASK_HOURS: i64 = 2;

#[derive(Debug, Clone)]
pub(crate) struct TaskedZone {
    pub pos: Vector2,
    pub radius: f64,
    pub label: String,
    pub until: DateTime<Utc>,
    pub mark: Option<MarkId>,
}

impl Db {
    /// Give `ucid` (flying group `group`) the task `label` at `pos`, replacing
    /// any earlier one and its mark.
    pub(crate) fn assign_task(
        &mut self,
        ucid: Ucid,
        group: GroupId,
        pos: Vector2,
        radius: f64,
        label: &str,
        now: DateTime<Utc>,
    ) {
        self.clear_task(&ucid);
        let mark = self.ephemeral.msgs().mark_to_group(
            group,
            pos,
            true,
            format_compact!(
                "YOUR TASK: {label} -- x{TASK_POINTS_MULT} points and bonus XP within {:.0} km",
                radius / 1000.
            ),
        );
        self.ephemeral.tasked.insert(
            ucid,
            TaskedZone {
                pos,
                radius,
                label: String::from(label),
                until: now + Duration::hours(TASK_HOURS),
                mark: Some(mark),
            },
        );
    }

    pub(crate) fn clear_task(&mut self, ucid: &Ucid) {
        if let Some(z) = self.ephemeral.tasked.remove(ucid) {
            log::debug!("task zone {} for {ucid} cleared", z.label);
            if let Some(m) = z.mark {
                self.ephemeral.msgs().delete_mark(m);
            }
        }
    }

    /// Whether `pos` is inside `ucid`'s current task zone.
    pub(crate) fn in_task_zone(
        &self,
        ucid: &Ucid,
        pos: Vector2,
        now: DateTime<Utc>,
    ) -> bool {
        self.ephemeral
            .tasked
            .get(ucid)
            .map_or(false, |z| now < z.until && (z.pos - pos).norm() <= z.radius)
    }

    /// Bonus XP for work in the task zone; scored by bfdb.
    pub(crate) fn task_bonus_xp(&self, ucid: Ucid, xp: u32, what: &str) {
        self.ephemeral.stat(Stat::PilotXp {
            id: ucid,
            xp,
            reason: String::from(format_compact!("task zone: {what}").as_str()),
        });
    }

    /// Drop tasks that ran out, and those of pilots no longer in a slot.
    pub(crate) fn expire_tasks(&mut self, now: DateTime<Utc>) {
        let gone: Vec<Ucid> = self
            .ephemeral
            .tasked
            .iter()
            .filter(|(u, z)| now >= z.until || !self.is_slotted(u))
            .map(|(u, _)| *u)
            .collect();
        for u in gone {
            self.clear_task(&u);
        }
    }
}
