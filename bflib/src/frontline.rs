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

//! F10-map frontline overlay. The geometry lives in
//! `bfprotocols::frontline` so bfdb can serve the identical line to the web
//! dashboard; this module just gathers the objectives, calls it, and draws
//! the three lines (blue-edge / centre / red-edge) via the message queue.

use crate::{db::persisted::Persisted, msgq::MsgQ};
use bfprotocols::{cfg::FrontLineConfig, db::objective::ObjectiveKind, frontline as fl};
use chrono::{DateTime, Utc};
use dcso3::{
    coalition::Side,
    trigger::{LineSpec, LineType, MarkId, SideFilter},
    Color, LuaVec3, Vector3,
};
use log::*;

const LINE_ALPHA: f32 = 0.9;
/// Centre line ("no man's land") style.
const MID_LINE: LineType = LineType::Dotted;
/// Blue-edge and red-edge line style.
const EDGE_LINE: LineType = LineType::Dashed;

/// Objectives that count toward the front: owned, on the ground. Carrier
/// groups move and sit at sea, so they're excluded.
pub fn frontline_objectives(persisted: &Persisted) -> Vec<(f64, f64, f64)> {
    persisted
        .objectives
        .into_iter()
        .filter(|(_, o)| matches!(o.owner, Side::Blue | Side::Red))
        .filter(|(_, o)| !matches!(o.kind(), ObjectiveKind::CarrierGroup { .. }))
        .map(|(_, o)| {
            let p = o.pos();
            (p.x, p.y, if o.owner == Side::Blue { 1.0 } else { -1.0 })
        })
        .collect()
}

/// Stores frontline drawing state.
#[derive(Debug, Clone)]
pub struct FrontLine {
    marks: Vec<MarkId>,
    config: FrontLineConfig,
    objective_ownership_hash: u64,
}

impl FrontLine {
    pub fn new(config: FrontLineConfig) -> Self {
        Self {
            marks: Vec::new(),
            config,
            objective_ownership_hash: 0,
        }
    }

    fn clear_marks(&mut self, msgq: &mut MsgQ) {
        for mark_id in self.marks.drain(..) {
            msgq.delete_mark(mark_id);
        }
    }

    fn calculate_ownership_hash(persisted: &Persisted) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        let mut objectives: Vec<_> = persisted.objectives.into_iter().collect();
        objectives.sort_by_key(|(id, _)| *id);
        for (id, obj) in objectives {
            id.hash(&mut hasher);
            (obj.owner as u8).hash(&mut hasher);
        }
        hasher.finish()
    }

    fn params(&self) -> fl::Params {
        fl::Params {
            grid_res: self.config.samples_per_boundary,
            ..fl::Params::default()
        }
    }

    fn draw_frontline(&mut self, persisted: &Persisted, msgq: &mut MsgQ) {
        let objs = frontline_objectives(persisted);
        let fronts = fl::compute(&objs, &self.params());
        if fronts.is_empty() {
            return;
        }

        let draw = |line: &[[f64; 2]], color: Color, lt: LineType, marks: &mut Vec<MarkId>, msgq: &mut MsgQ| {
            for w in line.windows(2) {
                let (a, b) = (w[0], w[1]);
                let mark_id = MarkId::new();
                msgq.line_to_all(
                    SideFilter::All,
                    mark_id,
                    LineSpec {
                        start: LuaVec3(Vector3::new(a[0], 0., a[1])),
                        end: LuaVec3(Vector3::new(b[0], 0., b[1])),
                        color,
                        line_type: lt,
                        read_only: true,
                    },
                    None,
                );
                marks.push(mark_id);
            }
        };

        for f in &fronts {
            draw(&f.blue, Color::new(0.0, 0.4, 1.0, LINE_ALPHA), EDGE_LINE, &mut self.marks, msgq);
            draw(&f.red, Color::new(1.0, 0.2, 0.2, LINE_ALPHA), EDGE_LINE, &mut self.marks, msgq);
            draw(&f.mid, Color::new(1.0, 1.0, 1.0, LINE_ALPHA), MID_LINE, &mut self.marks, msgq);
        }
    }

    /// Redraw the frontline from current objective ownership.
    /// Returns true if it was redrawn.
    pub fn update(&mut self, persisted: &Persisted, msgq: &mut MsgQ, _now: DateTime<Utc>) -> bool {
        if !self.config.enabled {
            if !self.marks.is_empty() {
                info!("Frontline: disabled, clearing {} marks", self.marks.len());
                self.clear_marks(msgq);
                self.objective_ownership_hash = 0;
            }
            return false;
        }

        let new_hash = Self::calculate_ownership_hash(persisted);
        let is_initial = self.objective_ownership_hash == 0;
        if self.config.update_on_objective_change_only && !is_initial && new_hash == self.objective_ownership_hash {
            debug!("Frontline: no ownership change, skipping redraw");
            return false;
        }
        self.objective_ownership_hash = new_hash;

        self.clear_marks(msgq);
        self.draw_frontline(persisted, msgq);

        if self.marks.len() > 5000 {
            warn!("Frontline: excessive segment count {} — bug?", self.marks.len());
        } else {
            info!("Frontline: drew {} segment(s)", self.marks.len());
        }
        true
    }

    /// Dummy method for compatibility (no longer used)
    pub fn collect_unit_pressure(&mut self, _persisted: &Persisted, _now: DateTime<Utc>) {}

    /// Remove all frontline segments
    pub fn remove(mut self, msgq: &mut MsgQ) {
        self.clear_marks(msgq);
    }
}
