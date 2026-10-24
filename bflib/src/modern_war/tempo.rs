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

//! Campaign tempo. Each side alternates an offensive with a regroup, and
//! the rest of `modern_war` (and the air_life packages) read it: raids come
//! faster and aim along the offensive's axis while it lasts, and slower while
//! the side regroups. The two sides run half a cycle out of phase, so one is
//! usually pushing while the other digs in.
//!
//! The phase is a pure function of the wall clock -- no state to save, and a
//! restart mid-offensive picks the same offensive back up. The axis is
//! chosen from the map as it stands when the offensive (or this session)
//! starts: the enemy objective the side is best placed to hit.

use crate::{airlife::dist, Context};
use bfprotocols::{
    cfg::TempoCfg,
    db::objective::{ObjectiveId, ObjectiveKind},
};
use chrono::prelude::*;
use compact_str::format_compact;
use dcso3::coalition::Side;
use fxhash::FxHashMap;
use log::info;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Phase {
    Offensive,
    Regroup,
}

#[derive(Debug, Default)]
pub(crate) struct Tempo {
    /// Per side: the phase last announced, and the offensive's axis.
    state: FxHashMap<Side, (Phase, Option<ObjectiveId>)>,
}

/// (phase, seconds left in it) for `side` at `now`.
pub(crate) fn phase_at(cfg: &TempoCfg, side: Side, now: DateTime<Utc>) -> (Phase, i64) {
    let off = (cfg.offensive_hours.max(0.1) * 3600.) as i64;
    let reg = (cfg.regroup_hours.max(0.1) * 3600.) as i64;
    let cycle = off + reg;
    let shift = match side {
        Side::Red => cycle / 2,
        _ => 0,
    };
    let t = (now.timestamp() + shift).rem_euclid(cycle);
    if t < off {
        (Phase::Offensive, off - t)
    } else {
        (Phase::Regroup, cycle - t)
    }
}

impl Tempo {
    /// Interval multiplier for `side`'s raids and packages right now. 1.0
    /// when tempo is off.
    pub(crate) fn factor(&self, cfg: Option<&TempoCfg>, side: Side, now: DateTime<Utc>) -> f64 {
        match cfg.filter(|c| c.enabled) {
            None => 1.,
            Some(c) => match phase_at(c, side, now).0 {
                Phase::Offensive => c.offensive_factor,
                Phase::Regroup => c.regroup_factor,
            },
        }
    }

    /// The enemy objective `side`'s current offensive is aimed at.
    pub(crate) fn axis(&self, side: Side) -> Option<ObjectiveId> {
        match self.state.get(&side) {
            Some((Phase::Offensive, axis)) => *axis,
            _ => None,
        }
    }
}

/// The enemy objective `side` is best placed to push on: close to its own
/// ground, weighted toward what matters (logistics, factories, airbases) and
/// away from SAM sites and carriers.
fn pick_axis(ctx: &Context, side: Side) -> Option<ObjectiveId> {
    let own: Vec<_> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() == side)
        .map(|(_, o)| o.pos())
        .collect();
    ctx.db
        .objectives()
        .filter(|(_, o)| o.owner() == side.opposite())
        .filter(|(_, o)| {
            !matches!(o.kind(), ObjectiveKind::SpecialSamSite | ObjectiveKind::CarrierGroup { .. })
        })
        .filter_map(|(id, o)| {
            let gap = own.iter().map(|p| dist(*p, o.pos())).fold(f64::INFINITY, f64::min);
            if !gap.is_finite() {
                return None;
            }
            let value = match o.kind() {
                ObjectiveKind::Logistics => 1.6,
                ObjectiveKind::Factory { .. } => 1.4,
                ObjectiveKind::Airbase => 1.3,
                _ => 1.0,
            };
            Some((*id, gap / value))
        })
        .min_by(|a, b| a.1.total_cmp(&b.1))
        .map(|(id, _)| id)
}

pub(crate) fn tick(ctx: &mut Context, cfg: &TempoCfg, now: DateTime<Utc>) {
    for side in [Side::Blue, Side::Red] {
        let (phase, left) = phase_at(cfg, side, now);
        let prev = ctx.modern_war.tempo.state.get(&side).map(|(p, _)| *p);
        // An axis that has changed hands is no longer a target.
        let axis_stale = ctx
            .modern_war
            .tempo
            .state
            .get(&side)
            .and_then(|(_, a)| *a)
            .and_then(|a| ctx.db.persisted.objectives.get(&a))
            .map(|o| o.owner() != side.opposite())
            .unwrap_or(false);
        if prev == Some(phase) && !axis_stale {
            continue;
        }
        let axis = match phase {
            Phase::Offensive => pick_axis(ctx, side),
            Phase::Regroup => None,
        };
        ctx.modern_war.tempo.state.insert(side, (phase, axis));
        let hours = left as f64 / 3600.;
        let axis_name = axis
            .and_then(|a| ctx.db.persisted.objectives.get(&a))
            .map(|o| o.name.clone());
        info!("tempo: {side:?} {phase:?} for {hours:.1} h, axis {axis_name:?}");
        // Only announce real transitions, not the first look after a
        // restart (the phase was already under way) or a retargeted axis
        // without a phase change -- except to name the new axis.
        if !cfg.announce || prev.is_none() {
            continue;
        }
        let msg = match (phase, &axis_name) {
            (Phase::Offensive, Some(name)) if prev == Some(Phase::Offensive) => format_compact!(
                "OPERATIONAL ORDERS: the offensive shifts to {name}. Strikes will concentrate there."
            ),
            (Phase::Offensive, Some(name)) => format_compact!(
                "OPERATIONAL ORDERS: offensive operations toward {name} begin. Raids and air \
                 packages concentrate there for the next {hours:.0} hours."
            ),
            (Phase::Offensive, None) => format_compact!(
                "OPERATIONAL ORDERS: offensive operations begin for the next {hours:.0} hours."
            ),
            (Phase::Regroup, _) => format_compact!(
                "OPERATIONAL ORDERS: the offensive is over. Regroup and resupply for the next \
                 {hours:.0} hours; strike tempo drops."
            ),
        };
        ctx.db.ephemeral.msgs().panel_to_side(20, false, side, msg);
    }
}
