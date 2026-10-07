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

//! Who commands (`Cfg::command`).
//!
//! Commander access is earned by rank, which bfdb works out from the pilots'
//! campaign scores, or granted by an admin. bfdb pushes the result here with
//! the `set-commanders` RPC; every order the engine takes from a player --
//! ground formations from F10, the theatre HQ override from chat -- checks it.
//! Until bfdb has pushed a list the engine doesn't know who has earned it, so
//! it lets orders through on the older per-feature rules instead of locking
//! everyone out of a server running without bfdb.

use crate::Context;
use bfprotocols::{cfg::rank_min_score, command::Commanders};
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, net::Ucid};
use log::info;

/// Rank titles by tier, for messages (the NATO service's; the VKS ladder has
/// the same tiers, and the dashboard shows each side its own).
const RANK_TITLES: [&str; 8] = [
    "2nd Lieutenant",
    "1st Lieutenant",
    "Captain",
    "Major",
    "Lieutenant Colonel",
    "Colonel",
    "Brigadier General",
    "Major General",
];

pub(crate) fn rank_title(tier: u8) -> &'static str {
    RANK_TITLES[(tier.clamp(1, 8) - 1) as usize]
}

/// bfdb's latest roster.
pub(crate) fn set(ctx: &mut Context, commanders: Commanders) {
    if ctx.commanders.as_ref() != Some(&commanders) {
        info!(
            "command: {} Blue and {} Red commander(s) from bfdb",
            commanders.blue.len(),
            commanders.red.len()
        );
    }
    ctx.commanders = Some(commanders);
}

/// Is `ucid` a commander of `side`? None if bfdb hasn't said who is.
pub(crate) fn is_commander(ctx: &Context, ucid: &Ucid, side: Side) -> Option<bool> {
    ctx.commanders.as_ref().map(|c| c.side_of(ucid) == Some(side))
}

/// May `ucid`, on `side`, give orders? Server admins always may; so does
/// everyone when the server doesn't require commanders, or when bfdb hasn't
/// told the engine who they are. Err is the reason, for the player.
pub(crate) fn may_order(ctx: &Context, ucid: &Ucid, side: Side) -> Result<(), CompactString> {
    let cfg = &ctx.db.ephemeral.cfg;
    if !cfg.command.require_commander || cfg.admins.contains_key(ucid) {
        return Ok(());
    }
    match is_commander(ctx, ucid, side) {
        None | Some(true) => Ok(()),
        Some(false) => Err(not_commander(ctx)),
    }
}

/// What to tell a pilot who isn't a commander.
pub(crate) fn not_commander(ctx: &Context) -> CompactString {
    let tier = ctx.db.ephemeral.cfg.command.commander_rank;
    format_compact!(
        "orders need a commander: reach {} (a campaign score of {:.0}), or ask an admin",
        rank_title(tier),
        rank_min_score(tier)
    )
}
