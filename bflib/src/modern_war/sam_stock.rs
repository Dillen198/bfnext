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

//! Finite SAM interceptors. DCS limits a launcher's missiles, but the engine
//! culls idle garrisons and respawns them when enemies come near, and every
//! respawn refills DCS's magazines -- so a site that fired everything at a
//! raid was full again by the next one. This keeps the count across that:
//! each SAM launch spends one from the group's magazine, a group at zero goes
//! weapons-hold ("Winchester"), and only logistics bring it back -- a few
//! missiles per period, and only while the objective it belongs to has supply
//! and is not under attack. Saturation raids now wear a defence down.

use super::home_objective;
use crate::{airlife::alive, Context};
use bfprotocols::{
    cfg::{SamStockCfg, UnitTag},
    db::group::GroupId,
};
use chrono::prelude::*;
use compact_str::format_compact;
use dcso3::{
    controller::{AiOption, GroundOption, GroundRoe},
    group::Group,
    MizLua,
};
use fxhash::FxHashMap;
use log::info;

/// How often a Winchester group's weapons-hold is re-applied: a cull and
/// respawn brings the template's ROE back.
const REAPPLY_SECS: i64 = 60;

#[derive(Debug)]
struct Magazine {
    remaining: u32,
    max: u32,
    winchester: bool,
    last_applied: Option<DateTime<Utc>>,
}

#[derive(Debug, Default)]
pub(crate) struct SamStock {
    groups: FxHashMap<GroupId, Magazine>,
    last_resupply: Option<DateTime<Utc>>,
}

impl SamStock {
    /// Missiles left in `gid`'s magazine, if it has been counted.
    #[allow(dead_code)]
    pub(crate) fn remaining(&self, gid: &GroupId) -> Option<u32> {
        self.groups.get(gid).map(|m| m.remaining)
    }

    /// Spend `n` interceptors from `gid` (raid interceptions resolved by the
    /// engine rather than by a DCS Shot event).
    pub(crate) fn spend(&mut self, ctx_db: &crate::db::Db, cfg: &SamStockCfg, gid: GroupId, n: u32) {
        let max = magazine_size(ctx_db, cfg, &gid);
        let m = self.groups.entry(gid).or_insert(Magazine {
            remaining: max,
            max,
            winchester: false,
            last_applied: None,
        });
        m.remaining = m.remaining.saturating_sub(n);
    }
}

fn magazine_size(db: &crate::db::Db, cfg: &SamStockCfg, gid: &GroupId) -> u32 {
    let launchers = db
        .persisted
        .groups
        .get(gid)
        .map(|g| {
            g.units
                .into_iter()
                .filter_map(|uid| db.persisted.units.get(uid))
                .filter(|u| u.tags.contains(UnitTag::Launcher) && u.tags.contains(UnitTag::SAM))
                .count()
        })
        .unwrap_or(0)
        .max(1);
    ((launchers as f32) * cfg.missiles_per_launcher as f32 * cfg.reload_multiplier.max(1.)).round()
        as u32
}

fn site_name(ctx: &Context, gid: &GroupId) -> dcso3::String {
    home_objective(ctx, gid)
        .and_then(|o| ctx.db.persisted.objectives.get(&o))
        .map(|o| o.name.clone())
        .unwrap_or_else(|| dcso3::String::from("a SAM site"))
}

fn set_roe(lua: MizLua, ctx: &Context, gid: &GroupId, roe: GroundRoe) -> bool {
    let Some(name) = ctx.db.persisted.groups.get(gid).map(|g| g.name.clone()) else {
        return false;
    };
    Group::get_by_name(lua, name.as_str())
        .and_then(|g| g.get_controller())
        .and_then(|c| c.set_option(AiOption::Ground(GroundOption::Roe(roe))))
        .is_ok()
}

pub(crate) fn on_shot(ctx: &mut Context, cfg: &SamStockCfg, gid: GroupId, _now: DateTime<Utc>) {
    let max = magazine_size(&ctx.db, cfg, &gid);
    let m = ctx.modern_war.sam.groups.entry(gid).or_insert(Magazine {
        remaining: max,
        max,
        winchester: false,
        last_applied: None,
    });
    m.remaining = m.remaining.saturating_sub(1);
    let went_dry = m.remaining == 0 && !m.winchester;
    if went_dry {
        m.winchester = true;
        m.last_applied = None;
    }
    if went_dry {
        let side = ctx.db.persisted.groups.get(&gid).map(|g| g.side);
        let name = site_name(ctx, &gid);
        info!("sam stock: {gid} at {name} is Winchester");
        if let (Some(side), true) = (side, cfg.announce) {
            ctx.db.ephemeral.msgs().panel_to_side(
                15,
                false,
                side,
                format_compact!(
                    "SAM site at {name} is out of missiles (Winchester). Keep it supplied to rearm."
                ),
            );
        }
    }
}

pub(crate) fn tick(lua: MizLua, ctx: &mut Context, cfg: &SamStockCfg, now: DateTime<Utc>) {
    // Forget groups that are gone.
    let gone: Vec<GroupId> = ctx
        .modern_war
        .sam
        .groups
        .keys()
        .filter(|g| !alive(ctx, g))
        .copied()
        .collect();
    for g in gone {
        ctx.modern_war.sam.groups.remove(&g);
    }
    // Keep empty sites silent through a cull and respawn.
    let dry: Vec<GroupId> = ctx
        .modern_war
        .sam
        .groups
        .iter()
        .filter(|(_, m)| {
            m.winchester
                && m.last_applied.map(|t| (now - t).num_seconds() >= REAPPLY_SECS).unwrap_or(true)
        })
        .map(|(g, _)| *g)
        .collect();
    for g in dry {
        if set_roe(lua, ctx, &g, GroundRoe::WeaponHold) {
            if let Some(m) = ctx.modern_war.sam.groups.get_mut(&g) {
                m.last_applied = Some(now);
            }
        }
    }
    // Resupply.
    let due = ctx
        .modern_war
        .sam
        .last_resupply
        .map(|t| (now - t).num_seconds() >= cfg.resupply_period_secs as i64)
        .unwrap_or(true);
    if !due {
        return;
    }
    ctx.modern_war.sam.last_resupply = Some(now);
    let short: Vec<GroupId> = ctx
        .modern_war
        .sam
        .groups
        .iter()
        .filter(|(_, m)| m.remaining < m.max)
        .map(|(g, _)| *g)
        .collect();
    for g in short {
        let supplied = home_objective(ctx, &g)
            .and_then(|o| ctx.db.persisted.objectives.get(&o))
            .map(|o| (o.unlimited_supply() || o.supply() >= cfg.min_supply) && !o.threatened())
            .unwrap_or(false);
        if !supplied {
            continue;
        }
        let rearmed = {
            let Some(m) = ctx.modern_war.sam.groups.get_mut(&g) else { continue };
            m.remaining = (m.remaining + cfg.resupply_per_period).min(m.max);
            let rearmed = m.winchester && m.remaining > 0;
            if rearmed {
                m.winchester = false;
            }
            if m.remaining == m.max && !m.winchester {
                ctx.modern_war.sam.groups.remove(&g);
            }
            rearmed
        };
        if rearmed {
            set_roe(lua, ctx, &g, GroundRoe::WeaponFree);
            let name = site_name(ctx, &g);
            info!("sam stock: {g} at {name} rearmed");
            if cfg.announce {
                if let Some(side) = ctx.db.persisted.groups.get(&g).map(|x| x.side) {
                    ctx.db.ephemeral.msgs().panel_to_side(
                        15,
                        false,
                        side,
                        format_compact!("SAM site at {name} has been rearmed."),
                    );
                }
            }
        }
    }
}
