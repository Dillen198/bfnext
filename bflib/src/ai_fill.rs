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

//! PvP with the AI filling in (`Cfg::ai_fill`): counts the heads and hands
//! them to the rule in `bfprotocols::cfg::AiFillCfg`.
//!
//! Who counts: players **in a slot** (`Db::active_players`, the same count
//! empty-side protection uses). Someone on the slot screen or in the lobby
//! is not fighting, so the other side's AI does not step up for them.
//!
//! The one exception is the HQ's own-side fade, which keeps counting
//! everyone connected and registered to the side (`hq::humans_on`) as it
//! always has: its `gap_fill` curve and per-job cuts were tuned on that, and
//! a player between sorties is still about to fly for the side. The enemy
//! match multiplied onto it uses the in-a-slot count like everything else.
//!
//! The own-side fade for the packages and the ground AI is the HQ's
//! `smart_commander.hq.gap_fill` when one is configured, its defaults
//! otherwise.
//!
//! With the block absent (or `enabled: false`) every caller gets `None` and
//! behaves exactly as before.

use crate::Context;
use bfprotocols::{
    cfg::{AiFillCfg, Cfg, HqGapFillCfg},
    hq::AiFillInfo,
};
use dcso3::coalition::Side;

/// The block, when it is present and switched on.
pub(crate) fn cfg(cfg: &Cfg) -> Option<&AiFillCfg> {
    cfg.ai_fill.as_ref().filter(|a| a.enabled)
}

/// The own-side fade the packages and the ground AI use.
fn gap_curve(cfg: &Cfg) -> HqGapFillCfg {
    cfg.smart_commander
        .as_ref()
        .and_then(|s| s.hq.as_ref())
        .map(|h| h.gap_fill.clone())
        .unwrap_or_default()
}

/// Pilots in a slot on `side` and on the other side.
pub(crate) fn heads(ctx: &Context, side: Side) -> (u32, u32) {
    (ctx.db.active_players(side), ctx.db.active_players(side.opposite()))
}

/// The HQ's enemy match for `side`, or None when ai_fill (or its `hq`
/// switch) is off.
pub(crate) fn hq_match(ctx: &Context, side: Side) -> Option<f64> {
    let a = cfg(&ctx.db.ephemeral.cfg).filter(|a| a.hq)?;
    Some(a.enemy_match(heads(ctx, side).1))
}

/// AI pressure for `side`'s packages, or None when ai_fill (or its
/// `packages` switch) is off.
pub(crate) fn packages(ctx: &Context, side: Side) -> Option<f64> {
    let c = &ctx.db.ephemeral.cfg;
    let a = cfg(c).filter(|a| a.packages)?;
    let (own, enemy) = heads(ctx, side);
    Some(a.pressure(&gap_curve(c), own, enemy))
}

/// AI pressure for `side`'s ground AI attacks, or None when ai_fill (or its
/// `ground_ai` switch) is off.
pub(crate) fn ground_ai(ctx: &Context, side: Side) -> Option<f64> {
    let c = &ctx.db.ephemeral.cfg;
    let a = cfg(c).filter(|a| a.ground_ai)?;
    let (own, enemy) = heads(ctx, side);
    Some(a.pressure(&gap_curve(c), own, enemy))
}

/// What the HQ dashboard shows for `side`: `hq_pressure` is the HQ's
/// combined factor (`SideRt::gap`, own fade times enemy match).
pub(crate) fn info(ctx: &Context, side: Side, hq_pressure: f64) -> Option<AiFillInfo> {
    let c = &ctx.db.ephemeral.cfg;
    let a = cfg(c)?;
    let (pilots, enemy_pilots) = heads(ctx, side);
    let gap = gap_curve(c);
    Some(AiFillInfo {
        pilots,
        enemy_pilots,
        enemy_match: a.enemy_match(enemy_pilots),
        pressure: a.pressure(&gap, pilots, enemy_pilots),
        hq_pressure,
        ai_led: AiFillCfg::ai_led(&gap, pilots),
    })
}
