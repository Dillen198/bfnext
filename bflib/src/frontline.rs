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

use crate::{db::persisted::Persisted, msgq::MsgQ};
use bfprotocols::cfg::FrontLineConfig;
use chrono::{DateTime, Utc};
use dcso3::{
    coalition::Side,
    trigger::{LineSpec, LineType, MarkId, SideFilter},
    Color, LuaVec3, Vector2, Vector3,
};
use log::*;
use rstar::{RTree, RTreeObject, AABB, PointDistance};

/// Point wrapper for R-tree spatial indexing of objectives
#[derive(Debug, Clone, Copy)]
struct ObjectivePoint {
    pos: [f64; 2],
    side: Side,
}

impl RTreeObject for ObjectivePoint {
    type Envelope = AABB<[f64; 2]>;
    fn envelope(&self) -> Self::Envelope {
        AABB::from_point(self.pos)
    }
}

impl PointDistance for ObjectivePoint {
    fn distance_2(&self, point: &[f64; 2]) -> f64 {
        let dx = self.pos[0] - point[0];
        let dy = self.pos[1] - point[1];
        dx * dx + dy * dy
    }
}

// Line styling is deliberately kept out of FrontLineConfig: `Cfg` is
// snapshotted positionally (bincode) into bfdb's session tree, so adding
// fields to it breaks decoding of older snapshots. These constants give the
// look from the reference drawing (thick coloured dashes, dotted where
// contested) without touching the config layout.
const LINE_ALPHA: f32 = 0.9;
/// Line style for a segment where one side clearly holds the adjacent ground.
const HELD_LINE: LineType = LineType::Dashed;
/// Line style for a segment that is genuinely contested.
const CONTESTED_LINE: LineType = LineType::Dotted;
/// |red - blue| / (red + blue) within the sample window below which a
/// boundary segment is treated as contested (drawn white / dotted).
const CONTESTED_THRESHOLD: f64 = 0.22;

/// Stores front line drawing state
#[derive(Debug, Clone)]
pub struct FrontLine {
    /// Stored mark IDs for the drawn frontline segments
    marks: Vec<MarkId>,
    /// Configuration
    config: FrontLineConfig,
    /// Hash of objective ownership to detect changes
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

    /// Clear all existing territory zone marks
    fn clear_marks(&mut self, msgq: &mut MsgQ) {
        for mark_id in self.marks.drain(..) {
            msgq.delete_mark(mark_id);
        }
    }

    /// Calculate a simple hash of objective ownership for change detection
    fn calculate_ownership_hash(persisted: &Persisted) -> u64 {
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;

        let mut hasher = DefaultHasher::new();
        let mut objectives: Vec<_> = persisted.objectives.into_iter().collect();
        objectives.sort_by_key(|(id, _)| *id);

        for (id, obj) in objectives {
            id.hash(&mut hasher);
            (obj.owner as u8).hash(&mut hasher);
        }

        hasher.finish()
    }

    /// Calculate map bounds with padding
    fn calculate_bounds(&self, objectives: &[(Vector2, Side)]) -> (Vector2, Vector2) {
        let first = objectives[0].0;
        let mut min = first;
        let mut max = first;

        for (pos, _) in objectives.iter().skip(1) {
            min.x = min.x.min(pos.x);
            min.y = min.y.min(pos.y);
            max.x = max.x.max(pos.x);
            max.y = max.y.max(pos.y);
        }

        // Add 15% padding
        let width = max.x - min.x;
        let height = max.y - min.y;
        let padding = width.max(height) * 0.15;

        min -= Vector2::new(padding, padding);
        max += Vector2::new(padding, padding);

        (min, max)
    }

    /// Build Voronoi tessellation grid using R-tree for O(log n) nearest-neighbor lookups
    fn build_voronoi_grid(&self, objectives: &[(Vector2, Side)], min: Vector2, max: Vector2) -> Vec<Vec<Side>> {
        // Adaptive resolution based on config
        let grid_size = self.config.samples_per_boundary.clamp(50, 200);
        let cell_width = (max.x - min.x) / grid_size as f64;
        let cell_height = (max.y - min.y) / grid_size as f64;

        info!("Frontline: Building {}x{} Voronoi grid (cell: {:.0}m x {:.0}m) using R-tree",
              grid_size, grid_size, cell_width, cell_height);

        // Build R-tree from objectives for O(log n) nearest-neighbor queries
        let points: Vec<ObjectivePoint> = objectives.iter()
            .map(|(pos, side)| ObjectivePoint {
                pos: [pos.x, pos.y],
                side: *side,
            })
            .collect();
        let rtree = RTree::bulk_load(points);

        let mut grid = vec![vec![Side::Neutral; grid_size]; grid_size];

        // For each grid cell, find nearest objective via R-tree (O(log n) per query)
        for i in 0..grid_size {
            for j in 0..grid_size {
                let x = min.x + (j as f64 + 0.5) * cell_width;
                let y = min.y + (i as f64 + 0.5) * cell_height;

                if let Some(nearest) = rtree.nearest_neighbor(&[x, y]) {
                    grid[i][j] = nearest.side;
                }
            }
        }

        grid
    }

    /// Locally dominant side around boundary cell `(i, j)`: sample a square
    /// window of the ownership grid and see which side holds more of the
    /// surrounding ground. A roughly even split is reported as `Neutral`
    /// (genuinely contested → drawn white / dotted).
    fn segment_side(grid: &[Vec<Side>], i: usize, j: usize, window: i64) -> Side {
        let rows = grid.len() as i64;
        let cols = grid[0].len() as i64;
        let (mut red, mut blue) = (0i64, 0i64);
        for di in -window..=window {
            for dj in -window..=window {
                let ii = i as i64 + di;
                let jj = j as i64 + dj;
                if ii < 0 || jj < 0 || ii >= rows || jj >= cols {
                    continue;
                }
                match grid[ii as usize][jj as usize] {
                    Side::Red => red += 1,
                    Side::Blue => blue += 1,
                    Side::Neutral => {}
                }
            }
        }
        let total = red + blue;
        if total == 0 {
            return Side::Neutral;
        }
        if (red - blue).abs() as f64 / total as f64 <= CONTESTED_THRESHOLD {
            Side::Neutral
        } else if red > blue {
            Side::Red
        } else {
            Side::Blue
        }
    }

    /// Walk the ownership grid and, for every cell edge where a Red cell
    /// borders a Blue cell, emit that shared edge as a line segment tagged
    /// with the locally dominant side. Honours `config.max_marks`.
    fn extract_frontline_segments(
        &self,
        grid: &[Vec<Side>],
        min: Vector2,
        max: Vector2,
    ) -> Vec<(Side, Vector2, Vector2)> {
        let rows = grid.len();
        if rows == 0 {
            return Vec::new();
        }
        let cols = grid[0].len();
        let cw = (max.x - min.x) / cols as f64;
        let ch = (max.y - min.y) / rows as f64;
        // Sample window for local advantage: ~1/12 of the grid, clamped.
        let window = ((rows.min(cols) / 12).max(2).min(8)) as i64;

        let opposite = |a: Side, b: Side| {
            matches!(
                (a, b),
                (Side::Red, Side::Blue) | (Side::Blue, Side::Red)
            )
        };

        let mut segs: Vec<(Side, Vector2, Vector2)> = Vec::new();
        for i in 0..rows {
            for j in 0..cols {
                let here = grid[i][j];
                if here == Side::Neutral {
                    continue;
                }
                // Eastern neighbour → vertical shared edge.
                if j + 1 < cols && opposite(here, grid[i][j + 1]) {
                    let x = min.x + (j + 1) as f64 * cw;
                    let y0 = min.y + i as f64 * ch;
                    let side = Self::segment_side(grid, i, j, window);
                    segs.push((side, Vector2::new(x, y0), Vector2::new(x, y0 + ch)));
                }
                // Southern neighbour → horizontal shared edge.
                if i + 1 < rows && opposite(here, grid[i + 1][j]) {
                    let y = min.y + (i + 1) as f64 * ch;
                    let x0 = min.x + j as f64 * cw;
                    let side = Self::segment_side(grid, i, j, window);
                    segs.push((side, Vector2::new(x0, y), Vector2::new(x0 + cw, y)));
                }
            }
        }

        // Respect the mark budget: thin the segment list to fit.
        let budget = self.config.max_marks.max(1);
        if segs.len() > budget {
            let step = (segs.len() + budget - 1) / budget;
            segs = segs.into_iter().step_by(step).collect();
        }
        segs
    }

    /// Draw the frontline as coloured dashed segments along the Red/Blue
    /// territory boundary.
    fn draw_frontline(&mut self, persisted: &Persisted, msgq: &mut MsgQ) {
        let all_objectives: Vec<(Vector2, Side)> = persisted
            .objectives
            .into_iter()
            .map(|(_, obj)| (obj.pos(), obj.owner))
            .collect();

        if all_objectives.len() < 2 {
            return;
        }

        let red_count = all_objectives.iter().filter(|(_, s)| *s == Side::Red).count();
        let blue_count = all_objectives.iter().filter(|(_, s)| *s == Side::Blue).count();
        if red_count == 0 || blue_count == 0 {
            info!("Frontline: one side holds no objectives, nothing to draw");
            return;
        }

        let (min, max) = self.calculate_bounds(&all_objectives);
        let grid = self.build_voronoi_grid(&all_objectives, min, max);
        let segments = self.extract_frontline_segments(&grid, min, max);

        info!(
            "Frontline: drawing {} boundary segment(s) (Red obj: {}, Blue obj: {})",
            segments.len(), red_count, blue_count
        );

        for (side, a, b) in segments {
            let (color, line_type) = match side {
                Side::Red => (Color::new(1.0, 0.0, 0.0, LINE_ALPHA), HELD_LINE),
                Side::Blue => (Color::new(0.0, 0.0, 1.0, LINE_ALPHA), HELD_LINE),
                Side::Neutral => (Color::new(1.0, 1.0, 1.0, LINE_ALPHA), CONTESTED_LINE),
            };
            let mark_id = MarkId::new();
            msgq.line_to_all(
                SideFilter::All,
                mark_id,
                LineSpec {
                    start: LuaVec3(Vector3::new(a.x, 0., a.y)),
                    end: LuaVec3(Vector3::new(b.x, 0., b.y)),
                    color,
                    line_type,
                    read_only: true,
                },
                None,
            );
            self.marks.push(mark_id);
        }
    }

    /// Redraw the frontline from current objective ownership.
    /// Returns true if it was redrawn.
    pub fn update(&mut self, persisted: &Persisted, msgq: &mut MsgQ, _now: DateTime<Utc>) -> bool {
        if !self.config.enabled {
            // Clear marks if disabled
            if !self.marks.is_empty() {
                info!("Frontline: Feature disabled, clearing {} existing marks", self.marks.len());
                self.clear_marks(msgq);
                self.objective_ownership_hash = 0;
            }
            return false;
        }

        // Check if objectives have changed ownership
        let new_hash = Self::calculate_ownership_hash(persisted);
        let is_initial_calculation = self.objective_ownership_hash == 0;

        if self.config.update_on_objective_change_only && !is_initial_calculation && new_hash == self.objective_ownership_hash {
            debug!("Frontline: No objective ownership changes detected, skipping update");
            return false;
        }

        if is_initial_calculation {
            info!("Frontline: performing initial frontline draw");
        } else {
            info!("Frontline: objective ownership changed, redrawing frontline");
        }

        self.objective_ownership_hash = new_hash;

        // Clear old marks
        if !self.marks.is_empty() {
            debug!("Frontline: clearing {} old segments before redraw", self.marks.len());
        }
        self.clear_marks(msgq);

        self.draw_frontline(persisted, msgq);

        // Safety check: warn if we somehow drew an absurd number of segments.
        if self.marks.len() > 5000 {
            warn!("Frontline: WARNING - excessive segment count: {} created! This may indicate a bug.",
                  self.marks.len());
        } else {
            info!("Frontline: drew {} frontline segment(s)", self.marks.len());
        }

        true
    }

    /// Dummy method for compatibility (no longer used)
    pub fn collect_unit_pressure(&mut self, _persisted: &Persisted, _now: DateTime<Utc>) {
        // No-op: pressure system removed for polygon-only mode
    }

    /// Remove all frontline segments
    pub fn remove(mut self, msgq: &mut MsgQ) {
        self.clear_marks(msgq);
    }
}
