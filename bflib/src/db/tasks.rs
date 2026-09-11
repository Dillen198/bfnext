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

//! The coalition tasking board.
//!
//! Players post tasks -- CAP, CAS, LOGISTICS, CAPTURE, SUPPLY, whatever the
//! mission config offers -- from the Actions menu. Every player on that
//! coalition then sees the task area circle and its map pin on the F10 map
//! (drawn by [`crate::db::map_layer`]), and any of them can take the task
//! back off the board again.
//!
//! A task is posted either at one of the player's own F10 map marks
//! (`TaskTarget::Position`) or against an objective
//! (`TaskTarget::CaptureObjective` / `TaskTarget::SupplyObjective`).
//! Objective tasks need no further player input to close out: the engine
//! watches the objective and marks the task complete the moment the
//! coalition has actually captured or resupplied it.
//!
//! Tasks are persisted, so a board a coalition built up over an evening
//! survives a server restart. Anything still open expires on its own after
//! `TaskCfg::ttl_secs` so nobody has to garbage collect stale requests.

use super::Db;
use anyhow::{Result, anyhow, bail};
use bfprotocols::{
    cfg::{RemoveTaskCfg, TaskCfg, TaskTarget},
    db::objective::ObjectiveId,
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{String, Vector2, atomic_id, coalition::Side, net::Ucid, radians_to_degrees};
use serde_derive::{Deserialize, Serialize};
use smallvec::{SmallVec, smallvec};

atomic_id!(TaskId);

/// One posted task on a coalition's board.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    pub id: TaskId,
    pub side: Side,
    /// The task type name from the config, e.g. "CAP".
    pub kind: String,
    /// Longer description from the config, if any.
    pub description: Option<String>,
    /// How this task completes, copied from the config when it was posted.
    pub target: TaskTarget,
    /// The objective this task is about, for the objective targets.
    pub oid: Option<ObjectiveId>,
    pub pos: Vector2,
    pub radius: f64,
    /// RGBA, resolved from the config when the task was posted. Cached here
    /// so editing the config can't repaint tasks that are already up.
    pub color: [f32; 4],
    /// Human readable "12km NE of Batumi", computed when the task is posted.
    pub location: String,
    pub created_by: Option<Ucid>,
    pub created_by_name: String,
    pub created: DateTime<Utc>,
    pub expires: Option<DateTime<Utc>>,
}

impl Task {
    /// One line label for menus and chat, e.g. `CAP 14 (12km NE of Batumi)`.
    pub fn label(&self) -> CompactString {
        format_compact!("{} {} ({})", self.kind, self.id, self.location)
    }

    /// Multi line text for the F10 map pin. Deliberately static -- a pin
    /// can't be edited in place, and re-dropping it every tick to count a
    /// timer down would flicker the map.
    pub fn pin_text(&self) -> CompactString {
        let mut s = format_compact!("TASK {} #{}", self.kind, self.id);
        if let Some(d) = self.description.as_ref() {
            s.push_str(&format_compact!("\n{d}"));
        }
        s.push_str(&format_compact!("\n{}", self.location));
        match &self.target {
            TaskTarget::Position => (),
            TaskTarget::CaptureObjective => s.push_str("\ncompletes when captured"),
            TaskTarget::SupplyObjective { threshold } => {
                s.push_str(&format_compact!("\ncompletes at {threshold}% supply"))
            }
        }
        s.push_str(&format_compact!("\nposted by {}", self.created_by_name));
        s.push_str(&format_compact!(
            "\nposted {}",
            self.created.format("%H:%MZ")
        ));
        if let Some(exp) = self.expires.as_ref() {
            s.push_str(&format_compact!("\nexpires {}", exp.format("%H:%MZ")));
        }
        s
    }
}

fn cardinal(deg: f64) -> &'static str {
    const DIRS: [&str; 8] = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"];
    let i = (((deg % 360. + 360.) % 360.) / 45.).round() as usize;
    DIRS[i % 8]
}

impl Db {
    /// Every open task belonging to `side`, oldest first.
    pub fn tasks(&self, side: Side) -> impl Iterator<Item = &Task> {
        self.persisted
            .tasks
            .into_iter()
            .map(|(_, t)| t)
            .filter(move |t| t.side == side)
    }

    #[allow(dead_code)]
    pub fn task(&self, id: &TaskId) -> Option<&Task> {
        self.persisted.tasks.get(id)
    }

    /// Describe a point as "<distance> <cardinal> of <nearest objective>",
    /// falling back to raw coordinates when there are no objectives at all.
    fn describe_point(&self, pos: Vector2) -> CompactString {
        match Db::objective_near_point(&self.persisted.objectives, pos, |_| true) {
            None => format_compact!("{:.0} {:.0}", pos.x, pos.y),
            Some((dist, bearing, obj)) => {
                if dist < 1000. {
                    format_compact!("{}", obj.name)
                } else {
                    let dir = cardinal(radians_to_degrees(bearing));
                    format_compact!("{:.0}km {} of {}", dist / 1000., dir, obj.name)
                }
            }
        }
    }

    /// Post a task at a map mark position. Returns the new task's id.
    pub fn add_task(
        &mut self,
        cfg: &TaskCfg,
        side: Side,
        ucid: Option<Ucid>,
        kind: &str,
        pos: Vector2,
        now: DateTime<Utc>,
    ) -> Result<TaskId> {
        let location = self.describe_point(pos);
        self.post_task(cfg, side, ucid, kind, None, pos, location, now)
    }

    /// Post a task against an objective. The objective has to make sense for
    /// the task type -- you can't ask the coalition to capture something it
    /// already owns, or to resupply an enemy base.
    pub fn add_objective_task(
        &mut self,
        cfg: &TaskCfg,
        side: Side,
        ucid: Option<Ucid>,
        kind: &str,
        oid: ObjectiveId,
        now: DateTime<Utc>,
    ) -> Result<TaskId> {
        let spec = cfg
            .types
            .iter()
            .find(|t| t.name.eq_ignore_ascii_case(kind))
            .ok_or_else(|| anyhow!("no such task type {kind}"))?;
        let obj = self.objective(&oid)?;
        match &spec.target {
            TaskTarget::Position => bail!("{} is not an objective task", spec.name),
            TaskTarget::CaptureObjective => {
                if obj.owner == side {
                    bail!("{} is already ours", obj.name)
                }
            }
            TaskTarget::SupplyObjective { .. } => {
                if obj.owner != side {
                    bail!("{} is not ours to supply", obj.name)
                }
            }
        }
        let pos = obj.zone.pos();
        let location = obj.name.clone();
        self.post_task(
            cfg,
            side,
            ucid,
            kind,
            Some(oid),
            pos,
            location.as_str().into(),
            now,
        )
    }

    fn post_task(
        &mut self,
        cfg: &TaskCfg,
        side: Side,
        ucid: Option<Ucid>,
        kind: &str,
        oid: Option<ObjectiveId>,
        pos: Vector2,
        location: CompactString,
        now: DateTime<Utc>,
    ) -> Result<TaskId> {
        let spec = cfg
            .types
            .iter()
            .find(|t| t.name.eq_ignore_ascii_case(kind))
            .ok_or_else(|| anyhow!("no such task type {kind}"))?
            .clone();
        let open = self.tasks(side).count();
        if open >= cfg.max_per_side {
            bail!("{side} already has {open} tasks on the board, remove one before posting another")
        }
        // Two people noticing the same problem shouldn't double post it.
        if let Some(oid) = oid.as_ref()
            && let Some(dup) = self
                .tasks(side)
                .find(|t| t.oid.as_ref() == Some(oid) && t.kind == spec.name)
        {
            bail!("{} is already on the board as task {}", dup.location, dup.id)
        }
        let created_by_name = ucid
            .as_ref()
            .and_then(|ucid| self.persisted.players.get(ucid))
            .map(|p| p.name.clone())
            .unwrap_or_else(|| String::from("command"));
        let color = spec.color.unwrap_or(match side {
            Side::Red => [1., 0., 0., 0.7],
            Side::Blue => [0., 0., 1., 0.7],
            Side::Neutral => [1., 1., 1., 0.7],
        });
        let id = TaskId::new();
        let task = Task {
            id,
            side,
            kind: spec.name.clone(),
            description: spec.description.clone(),
            target: spec.target.clone(),
            oid,
            pos,
            radius: spec.radius_m,
            color,
            location: location.as_str().into(),
            created_by: ucid,
            created_by_name,
            created: now,
            expires: if cfg.ttl_secs == 0 {
                None
            } else {
                Some(now + chrono::Duration::seconds(cfg.ttl_secs as i64))
            },
        };
        let msg = format_compact!(
            "NEW TASK: {} {} posted by {}",
            task.kind,
            task.location,
            task.created_by_name
        );
        if cfg.announce_gci {
            self.ephemeral.gci_tasks.push((now, task.clone()));
        }
        self.persisted.tasks.insert_cow(id, task);
        self.ephemeral.dirty();
        self.ephemeral.msgs().panel_to_side(15, false, side, msg);
        Ok(id)
    }

    /// Take a task off `side`'s board.
    pub fn remove_task(
        &mut self,
        cfg: &RemoveTaskCfg,
        side: Side,
        ucid: Option<Ucid>,
        id: &TaskId,
    ) -> Result<Task> {
        let task = self
            .persisted
            .tasks
            .get(id)
            .ok_or_else(|| anyhow!("no such task {id}"))?;
        if task.side != side {
            bail!("task {id} does not belong to {side}")
        }
        if cfg.owner_only
            && let Some(ucid) = ucid.as_ref()
            && task.created_by.as_ref() != Some(ucid)
            && !self.ephemeral.cfg.admins.contains_key(ucid)
        {
            bail!(
                "task {id} was posted by {}, only they or an admin can remove it",
                task.created_by_name
            )
        }
        let by = ucid
            .as_ref()
            .and_then(|ucid| self.persisted.players.get(ucid))
            .map(|p| p.name.clone())
            .unwrap_or_else(|| String::from("command"));
        let task = self
            .persisted
            .tasks
            .remove_cow(id)
            .ok_or_else(|| anyhow!("no such task {id}"))?;
        self.ephemeral.dirty();
        let msg = format_compact!("TASK CANCELLED: {} {} by {by}", task.kind, task.location);
        self.ephemeral.msgs().panel_to_side(15, false, side, msg);
        Ok(task)
    }

    /// Close out objective tasks the coalition has finished, and drop tasks
    /// whose ttl has run out. Called from the slow tick.
    pub fn tick_tasks(&mut self, now: DateTime<Utc>) {
        // The GCI broadcast queue only needs to survive long enough for
        // bfdb's next poll to see it.
        self.ephemeral
            .gci_tasks
            .retain(|(t, _)| now - *t < chrono::Duration::seconds(180));
        let mut done: SmallVec<[(TaskId, bool); 4]> = smallvec![];
        for (id, task) in self.persisted.tasks.into_iter() {
            let complete = match (&task.target, task.oid.as_ref()) {
                (TaskTarget::Position, _) | (_, None) => false,
                (TaskTarget::CaptureObjective, Some(oid)) => self
                    .persisted
                    .objectives
                    .get(oid)
                    .map(|o| o.owner == task.side)
                    .unwrap_or(false),
                (TaskTarget::SupplyObjective { threshold }, Some(oid)) => self
                    .persisted
                    .objectives
                    .get(oid)
                    .map(|o| {
                        o.owner == task.side && o.supply >= *threshold && o.fuel >= *threshold
                    })
                    .unwrap_or(false),
            };
            if complete {
                done.push((*id, true));
            } else if task.expires.map(|exp| now >= exp).unwrap_or(false) {
                done.push((*id, false));
            }
        }
        if done.is_empty() {
            return;
        }
        for (id, complete) in done {
            if let Some(task) = self.persisted.tasks.remove_cow(&id) {
                let msg = if complete {
                    format_compact!("TASK COMPLETE: {} {}", task.kind, task.location)
                } else {
                    format_compact!("TASK EXPIRED: {} {}", task.kind, task.location)
                };
                self.ephemeral
                    .msgs()
                    .panel_to_side(15, false, task.side, msg);
            }
        }
        self.ephemeral.dirty();
    }

    /// A short board summary for chat / panel reports.
    #[allow(dead_code)]
    pub fn task_board(&self, side: Side) -> SmallVec<[CompactString; 8]> {
        let mut board: SmallVec<[CompactString; 8]> = smallvec![];
        for task in self.tasks(side) {
            board.push(format_compact!(
                "{} -- posted by {}",
                task.label(),
                task.created_by_name
            ));
        }
        board
    }
}
