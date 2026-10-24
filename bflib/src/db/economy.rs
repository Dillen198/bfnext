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

//! Who gets paid, and how much they actually bank. The rules and their
//! arithmetic live in `bfprotocols::cfg::EconomyCfg`; this is the part that
//! needs the campaign state: territory and head counts for underdog pay, the
//! side's balances for the wealth taper and the late-joiner start, haul
//! distance and the front line for logistics pay, and the fund a captured
//! base hands over.

use super::{objective::Objective, Db};
use bfprotocols::db::objective::ObjectiveId;
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, net::Ucid};
use log::info;

/// What a payout came to after the economy had its say, for the message.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Earning {
    pub amount: i32,
    pub underdog: f64,
    pub tapered: bool,
}

impl Earning {
    /// " [x1.3 underdog, taper]" or "" -- appended to the points message so
    /// a pilot can see why the number isn't the one on the wiki.
    pub(crate) fn note(&self) -> CompactString {
        let mut parts: smallvec::SmallVec<[CompactString; 2]> = smallvec::smallvec![];
        if self.underdog > 1.005 {
            parts.push(format_compact!("x{:.2} underdog", self.underdog));
        }
        if self.tapered {
            parts.push("high-balance rate".into());
        }
        if parts.is_empty() {
            CompactString::new("")
        } else {
            format_compact!(" [{}]", parts.join(", "))
        }
    }
}

impl Db {
    /// Share of the contested (Blue + Red held) objectives `side` holds.
    /// 0.5 when nothing is held by either side.
    pub(crate) fn territory_share(&self, side: Side) -> f64 {
        let (mut mine, mut contested) = (0u32, 0u32);
        for (_, obj) in &self.persisted.objectives {
            match obj.owner() {
                Side::Neutral => (),
                s => {
                    contested += 1;
                    if s == side {
                        mine += 1
                    }
                }
            }
        }
        if contested == 0 {
            0.5
        } else {
            mine as f64 / contested as f64
        }
    }

    /// Players in a slot (spectators excluded) on `side`, and on both sides.
    pub(crate) fn slotted_counts(&self, side: Side) -> (u32, u32) {
        let (mut mine, mut total) = (0u32, 0u32);
        for ucid in self.ephemeral.players_by_slot.values() {
            if let Some(p) = self.persisted.players.get(ucid) {
                if p.side == Side::Neutral {
                    continue;
                }
                total += 1;
                if p.side == side {
                    mine += 1
                }
            }
        }
        (mine, total)
    }

    /// Whether this player is in a slot right now (not spectating, not
    /// offline).
    pub(crate) fn is_slotted(&self, ucid: &Ucid) -> bool {
        self.persisted
            .players
            .get(ucid)
            .map(|p| p.current_slot.is_some())
            .unwrap_or(false)
    }

    /// The earnings multiplier for `side` right now (>= 1).
    pub(crate) fn underdog_multiplier(&self, side: Side) -> f64 {
        let econ = &self.ephemeral.cfg.economy;
        if !econ.enabled || side == Side::Neutral {
            return 1.;
        }
        let (mine, total) = self.slotted_counts(side);
        econ.underdog_multiplier(self.territory_share(side), mine, total)
    }

    fn new_player_join(&self) -> i64 {
        self.ephemeral
            .cfg
            .points
            .as_ref()
            .map(|p| p.new_player_join as i64)
            .unwrap_or(0)
    }

    /// The side's typical balance: the median of its pilots who have earned
    /// or spent anything this round (a pilot still sitting on exactly the
    /// join grant hasn't played, and counting the hundreds who joined once
    /// would pin the median to the grant). Never less than the join grant.
    pub(crate) fn reference_balance(&self, side: Side) -> i64 {
        let join = self.new_player_join();
        let mut balances: Vec<i64> = self
            .persisted
            .players
            .into_iter()
            .filter(|(_, p)| p.side == side && p.points as i64 != join)
            .map(|(_, p)| p.points as i64)
            .collect();
        if balances.is_empty() {
            return join.max(1);
        }
        balances.sort_unstable();
        let mid = balances.len() / 2;
        let median = if balances.len() % 2 == 0 {
            (balances[mid - 1] + balances[mid]) / 2
        } else {
            balances[mid]
        };
        median.max(join).max(1)
    }

    /// What `base` points of earnings come to for this pilot: raised by their
    /// side's underdog multiplier, then tapered past the wealth cap. Costs,
    /// refunds, penalties and transfers never go through here.
    pub(crate) fn scale_earning(&self, ucid: &Ucid, base: i32) -> Earning {
        let econ = &self.ephemeral.cfg.economy;
        let Some(player) = self.persisted.players.get(ucid) else {
            return Earning { amount: base, underdog: 1., tapered: false };
        };
        if !econ.enabled || base <= 0 {
            return Earning { amount: base, underdog: 1., tapered: false };
        }
        let underdog = self.underdog_multiplier(player.side);
        let raised = (base as f64 * underdog).round() as i64;
        let balance = player.points as i64 + player.provisional_points as i64;
        let cap = econ.wealth_cap(self.reference_balance(player.side));
        let amount = econ.taper(balance, raised, cap);
        Earning {
            amount: amount.clamp(0, i32::MAX as i64) as i32,
            underdog,
            tapered: amount < raised,
        }
    }

    /// Pay a pilot for something they did. Applies the economy (see
    /// `scale_earning`) and tells them, with the reason the amount differs
    /// from the base rate if it does.
    pub fn earn_points(&mut self, ucid: &Ucid, base: i32, why: &str) -> i32 {
        let e = self.scale_earning(ucid, base);
        self.adjust_points(ucid, e.amount, &format_compact!("{why}{}", e.note()));
        e.amount
    }

    /// `earn_points` without the panel message (frequent small payments).
    pub fn earn_points_silent(&mut self, ucid: &Ucid, base: i32, why: &str) -> i32 {
        let e = self.scale_earning(ucid, base);
        self.adjust_points_silent(ucid, e.amount, why);
        e.amount
    }

    /// The balance a brand-new pilot on `side` starts with: the join grant,
    /// or a share of the side's typical balance if that is more, so a pilot
    /// who joins on day three isn't starting from nothing against people
    /// with tens of thousands. Returns (start, untransferable part).
    pub(crate) fn late_joiner_start(&self, side: Side) -> (i32, i32) {
        let join = self.new_player_join();
        let econ = &self.ephemeral.cfg.economy;
        if !econ.enabled || econ.late_joiner_fraction <= 0. {
            return (join as i32, 0);
        }
        let share = (self.reference_balance(side) as f64 * econ.late_joiner_fraction) as i64;
        let start = share.max(join).min(i32::MAX as i64);
        (start as i32, (start - join).max(0) as i32)
    }

    /// A front-line objective: threatened right now, or within
    /// `front_line_km` of an enemy-held objective.
    pub(crate) fn is_front_line(&self, oid: &ObjectiveId) -> bool {
        let Some(obj) = self.persisted.objectives.get(oid) else {
            return false;
        };
        if obj.threatened {
            return true;
        }
        let side = obj.owner();
        let reach = self.ephemeral.cfg.economy.front_line_km * 1000.;
        let pos = obj.pos();
        self.persisted.objectives.into_iter().any(|(_, o)| {
            let s = o.owner();
            s != side && s != Side::Neutral && (o.pos() - pos).magnitude() <= reach
        })
    }

    /// Pay for a logistics delivery (repair kit, supply transfer) unpacked at
    /// `dest` by `unpacker` from a crate loaded at `origin` by `hauler`.
    /// The pay scales with the haul and the front line, and goes to the
    /// pilot who hauled it -- the unpacker only gets a share when that's
    /// someone else. Before this, whoever pressed "unpack" got all of it.
    pub(crate) fn pay_delivery(
        &mut self,
        base: u32,
        hauler: Option<Ucid>,
        unpacker: Ucid,
        origin: Option<ObjectiveId>,
        dest: ObjectiveId,
        what: &str,
    ) {
        if base == 0 {
            return;
        }
        let econ = self.ephemeral.cfg.economy.clone();
        if !econ.enabled {
            self.adjust_points(&unpacker, base as i32, &format_compact!("for {what}"));
            return;
        }
        let distance = match (origin, self.persisted.objectives.get(&dest)) {
            (Some(o), Some(d)) => self
                .persisted
                .objectives
                .get(&o)
                .map(|o: &Objective| (o.pos() - d.pos()).magnitude())
                .unwrap_or(0.),
            _ => 0.,
        };
        let front = self.is_front_line(&dest);
        let mult = econ.delivery_multiplier(distance, front);
        let total = (base as f64 * mult).round() as i32;
        let detail = format_compact!(
            "for {what} ({:.0} km{})",
            distance / 1000.,
            if front { ", front line" } else { "" }
        );
        // The hauler only counts if they're still a pilot on the same side.
        let unpacker_side = self.persisted.players.get(&unpacker).map(|p| p.side);
        let hauler = hauler.filter(|h| {
            *h != unpacker
                && self.persisted.players.get(h).map(|p| Some(p.side)) == Some(unpacker_side)
        });
        match hauler {
            None => {
                self.earn_points(&unpacker, total, &detail);
            }
            Some(h) => {
                let share = econ.unpacker_share.clamp(0., 1.);
                let unpacker_pts = (total as f64 * share).round() as i32;
                let hauler_pts = total - unpacker_pts;
                self.earn_points(&h, hauler_pts, &detail);
                if unpacker_pts > 0 {
                    self.earn_points(&unpacker, unpacker_pts, &format_compact!("{detail}, unpacked"));
                }
            }
        }
    }

    /// A base just changed hands (or went neutral). The fund it held was the
    /// loser's money: the captor keeps `capture_fund_keep` of it, capped at
    /// the ceiling its own bases of that kind get; a neutral base holds
    /// nothing. Capturing a fat base used to hand the whole fund over -- an
    /// 11M fund on vs2 came from that.
    pub(crate) fn settle_captured_fund(&mut self, oid: ObjectiveId) {
        let keep = self.ephemeral.cfg.economy.capture_fund_keep.clamp(0., 1.);
        let ceiling = self.objective_fund_ceilings().get(&oid).copied();
        let Some(obj) = self.persisted.objectives.get_mut_cow(&oid) else {
            return;
        };
        let before = obj.points;
        let after = if obj.owner() == Side::Neutral || before <= 0 {
            0
        } else {
            let kept = (before as f64 * keep) as i64;
            ceiling.map_or(kept, |c| kept.min(c)).clamp(0, i32::MAX as i64) as i32
        };
        if after != before {
            obj.points = after;
            info!(
                "[ECONOMY] {} changed hands: fund {} -> {} (owner {:?})",
                obj.name,
                before,
                after,
                obj.owner()
            );
            self.ephemeral.dirty();
        }
    }
}
