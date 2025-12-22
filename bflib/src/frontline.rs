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
    trigger::{LineType, MarkId, QuadSpec, SideFilter},
    Color, LuaVec3, Vector2, Vector3,
};
use log::*;

/// Stores front line drawing state
#[derive(Debug, Clone)]
pub struct FrontLine {
    /// Stored mark IDs for territory zone quads
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

    /// Build Voronoi tessellation grid
    fn build_voronoi_grid(&self, objectives: &[(Vector2, Side)], min: Vector2, max: Vector2) -> Vec<Vec<Side>> {
        // Adaptive resolution based on config
        let grid_size = self.config.samples_per_boundary.clamp(50, 200);
        let cell_width = (max.x - min.x) / grid_size as f64;
        let cell_height = (max.y - min.y) / grid_size as f64;

        info!("Frontline: Building {}x{} Voronoi grid (cell: {:.0}m x {:.0}m)",
              grid_size, grid_size, cell_width, cell_height);

        let mut grid = vec![vec![Side::Neutral; grid_size]; grid_size];

        // For each grid cell, find nearest objective (Voronoi assignment)
        for i in 0..grid_size {
            for j in 0..grid_size {
                let x = min.x + (j as f64 + 0.5) * cell_width;
                let y = min.y + (i as f64 + 0.5) * cell_height;
                let pos = Vector2::new(x, y);

                // Find closest objective
                let (_, owner) = objectives.iter()
                    .map(|(obj_pos, side)| ((*obj_pos - pos).magnitude_squared(), side))
                    .min_by(|a, b| a.0.partial_cmp(&b.0).unwrap())
                    .unwrap();

                grid[i][j] = *owner;
            }
        }

        grid
    }

    /// Extract territory zones as filled quads for visualization
    /// Returns (side, list of quads) for each territory
    fn extract_territory_zones(&self, grid: &[Vec<Side>], min: Vector2, max: Vector2) -> Vec<(Side, Vec<(Vector2, Vector2, Vector2, Vector2)>)> {
        let rows = grid.len();
        if rows == 0 {
            return Vec::new();
        }
        let cols = grid[0].len();

        let cell_width = (max.x - min.x) / cols as f64;
        let cell_height = (max.y - min.y) / rows as f64;

        // Sample grid at lower resolution for performance (every Nth cell)
        let sample_step = 4;  // Sample every 4th cell

        let mut red_quads = Vec::new();
        let mut blue_quads = Vec::new();

        for i in (0..rows).step_by(sample_step) {
            for j in (0..cols).step_by(sample_step) {
                let side = grid[i][j];

                if side == Side::Neutral {
                    continue;
                }

                // Create a quad for this cell
                let x1 = min.x + (j as f64) * cell_width;
                let y1 = min.y + (i as f64) * cell_height;
                let x2 = min.x + ((j + sample_step).min(cols) as f64) * cell_width;
                let y2 = min.y + ((i + sample_step).min(rows) as f64) * cell_height;

                let quad = (
                    Vector2::new(x1, y1),
                    Vector2::new(x2, y1),
                    Vector2::new(x2, y2),
                    Vector2::new(x1, y2),
                );

                match side {
                    Side::Red => red_quads.push(quad),
                    Side::Blue => blue_quads.push(quad),
                    Side::Neutral => {},
                }
            }
        }

        let mut zones = Vec::new();
        if !red_quads.is_empty() {
            zones.push((Side::Red, red_quads));
        }
        if !blue_quads.is_empty() {
            zones.push((Side::Blue, blue_quads));
        }

        zones
    }

    /// Draw filled territory zones to show areas of control
    fn draw_territory_zones(&mut self, persisted: &Persisted, msgq: &mut MsgQ) {
        // Collect all objectives
        let all_objectives: Vec<(Vector2, Side)> = persisted.objectives
            .into_iter()
            .map(|(_, obj)| (obj.pos(), obj.owner))
            .collect();

        if all_objectives.len() < 2 {
            return;
        }

        // Count objectives by side
        let red_count = all_objectives.iter().filter(|(_, side)| *side == Side::Red).count();
        let blue_count = all_objectives.iter().filter(|(_, side)| *side == Side::Blue).count();
        let neutral_count = all_objectives.iter().filter(|(_, side)| *side == Side::Neutral).count();

        info!("Frontline: Calculating territory zones for {} objectives (Red: {}, Blue: {}, Neutral: {})",
              all_objectives.len(), red_count, blue_count, neutral_count);

        // Calculate bounds and build Voronoi grid
        let (min, max) = self.calculate_bounds(&all_objectives);
        info!("Frontline: Map bounds: ({:.0}, {:.0}) to ({:.0}, {:.0})",
              min.x, min.y, max.x, max.y);

        let grid = self.build_voronoi_grid(&all_objectives, min, max);

        // Extract territory zones
        let zones = self.extract_territory_zones(&grid, min, max);

        info!("Frontline: Drawing {} territory zone(s)", zones.len());

        // Draw quads for each territory
        for (side, quads) in zones {
            let (fill_r, fill_g, fill_b) = match side {
                Side::Red => (1.0, 0.0, 0.0),
                Side::Blue => (0.0, 0.0, 1.0),
                Side::Neutral => continue,  // Don't draw neutral zones
            };

            let fill_color = Color::new(fill_r, fill_g, fill_b, self.config.territory_zone_alpha);
            let border_color = Color::new(fill_r, fill_g, fill_b, 0.0);  // Invisible border

            info!("Frontline: Drawing {} quads for {:?} territory", quads.len(), side);

            for (p0, p1, p2, p3) in quads {
                let mark_id = MarkId::new();

                msgq.quad_to_all(
                    SideFilter::All,
                    mark_id,
                    QuadSpec {
                        p0: LuaVec3(Vector3::new(p0.x, 0., p0.y)),
                        p1: LuaVec3(Vector3::new(p1.x, 0., p1.y)),
                        p2: LuaVec3(Vector3::new(p2.x, 0., p2.y)),
                        p3: LuaVec3(Vector3::new(p3.x, 0., p3.y)),
                        color: border_color,
                        fill_color,
                        line_type: LineType::NoLine,
                        read_only: true,
                    },
                    None,
                );

                self.marks.push(mark_id);
            }
        }
    }

    /// Update territory zones based on current objective ownership
    /// Returns true if the zones were updated
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
            info!("Frontline: Performing initial territory zones calculation");
        } else {
            info!("Frontline: Objective ownership changed, recalculating territory zones");
        }

        self.objective_ownership_hash = new_hash;

        // Clear old marks
        if !self.marks.is_empty() {
            debug!("Frontline: Clearing {} old marks before redraw", self.marks.len());
        }
        self.clear_marks(msgq);

        // Draw territory zones
        self.draw_territory_zones(persisted, msgq);

        // Safety check: Warn if we have an excessive number of marks
        if self.marks.len() > 5000 {
            warn!("Frontline: WARNING - Excessive mark count: {} marks created! This may indicate a bug.",
                  self.marks.len());
        } else {
            info!("Frontline: Created {} territory zone marks", self.marks.len());
        }

        true
    }

    /// Dummy method for compatibility (no longer used)
    pub fn collect_unit_pressure(&mut self, _persisted: &Persisted, _now: DateTime<Utc>) {
        // No-op: pressure system removed for polygon-only mode
    }

    /// Remove all territory zone marks
    pub fn remove(mut self, msgq: &mut MsgQ) {
        self.clear_marks(msgq);
    }
}
