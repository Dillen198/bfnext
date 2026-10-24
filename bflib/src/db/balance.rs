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

//! Empty-server protection (`Cfg::population_scaling`).
//!
//! When one side has (almost) nobody online, the other side could otherwise
//! roll its bases up unopposed overnight. While a base's owner has fewer than
//! `min_defenders` players in a slot, a capture there takes a multiple of the
//! normal time, and optionally the owner's bases self-repair faster. The
//! multipliers themselves are pure functions on `PopulationScalingCfg`; this
//! module only counts heads and feeds them in.
//!
//! "Active" means in a slot: `Ephemeral::players_by_slot`, which a player
//! joins when their unit is born and leaves when they deslot, die or
//! disconnect. Spectators -- connected, side picked, but not flying -- do not
//! defend anything and are not counted. The count is taken live every time,
//! so a defender slotting in shortens a running capture straight away, and a
//! defender briefly between aircraft can only make a capture slower, never
//! faster.

use super::{Db, ephemeral::Ephemeral, objective::Objective, persisted::Persisted};
use bfprotocols::cfg::{Cfg, PopulationScalingCfg, fmt_mult};
use compact_str::{CompactString, format_compact};
use dcso3::coalition::Side;

/// Players in a slot on `side`.
pub(super) fn active_players(eph: &Ephemeral, persisted: &Persisted, side: Side) -> u32 {
    eph.players_by_slot
        .values()
        .filter(|ucid| persisted.players.get(ucid).map_or(false, |p| p.side == side))
        .count() as u32
}

fn scaling(cfg: &Cfg) -> Option<&PopulationScalingCfg> {
    cfg.population_scaling.as_ref().filter(|p| p.enabled)
}

/// Auto-repair speed multiplier for a base owned by `owner` right now.
pub(super) fn repair_speed_mult(eph: &Ephemeral, persisted: &Persisted, owner: Side) -> f64 {
    match scaling(&eph.cfg) {
        Some(ps) if owner != Side::Neutral => {
            ps.repair_speed_mult(active_players(eph, persisted, owner))
        }
        _ => 1.,
    }
}

/// Seconds between auto-repair pulses for `obj`: `repair_time` scaled by its
/// logistics (special SAM sites have none and run the flat `repair_time`),
/// divided by the undefended speed-up. Infinite for a base with no logistics
/// left, which never self-repairs. `maybe_do_repairs` and every repair ETA
/// shown to players go through this, so the text can't drift from the rule.
pub(crate) fn repair_pulse_secs(cfg: &Cfg, obj: &Objective, speed_mult: f64) -> f64 {
    let base = if obj.kind().is_special_sam_site() {
        cfg.repair_time as f64
    } else {
        cfg.repair_time as f64 / (obj.logi() as f64 / 100.)
    };
    base / speed_mult.max(1.)
}

impl Db {
    /// Players in a slot on `side` -- see the module docs for what counts.
    pub fn active_players(&self, side: Side) -> u32 {
        active_players(&self.ephemeral, &self.persisted, side)
    }

    /// How many times longer `attacker`'s capture of a base owned by `owner`
    /// takes right now. Always 1 for a Neutral base, or with the feature off.
    pub fn capture_time_mult(&self, owner: Side, attacker: Side) -> f64 {
        match scaling(&self.ephemeral.cfg) {
            Some(ps) if owner != Side::Neutral => {
                ps.capture_time_mult(self.active_players(owner), self.active_players(attacker))
            }
            _ => 1.,
        }
    }

    /// See `repair_pulse_secs`.
    pub fn repair_pulse_secs(&self, obj: &Objective) -> f64 {
        let mult = repair_speed_mult(&self.ephemeral, &self.persisted, obj.owner());
        repair_pulse_secs(&self.ephemeral.cfg, obj, mult)
    }

    /// One line on the undefended state of `owner` for the Capture Advisor
    /// and the repair outlook, or None when the feature is off or `owner`
    /// has enough players up that normal rules apply.
    pub fn undefended_note(&self, owner: Side, attacker: Side) -> Option<CompactString> {
        let ps = scaling(&self.ephemeral.cfg)?;
        if owner == Side::Neutral {
            return None;
        }
        let have = self.active_players(owner);
        if !ps.undefended(have) {
            return None;
        }
        let cap = self.capture_time_mult(owner, attacker);
        let rep = ps.repair_speed_mult(have);
        let mut s = format_compact!(
            "{owner:?} has {have} pilot(s) in a slot (under {}) -- ",
            ps.min_defenders
        );
        if cap > 1. {
            s.push_str(&format_compact!("captures take {} longer", fmt_mult(cap)));
        } else {
            s.push_str("captures run at normal speed");
        }
        if rep > 1. {
            s.push_str(&format_compact!(", repairs run {} faster", fmt_mult(rep)));
        }
        Some(s)
    }
}
