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

//! The order of battle: each side's formations grouped into regiments.
//!
//! A formation is a battalion and its DCS groups are its companies. A
//! regiment is up to `MAX_BATTALIONS` formations of one side. A formation
//! is given its regiment once, the first tick after it is raised, and keeps
//! it for life, so "2nd Regiment / 1st Bn" means the same unit all round. It
//! joins a regiment already on its task (same objective), else the nearest
//! one within `JOIN_M`, else a new one. Regiment numbers are per side and
//! freed when the last battalion is gone. Nothing here touches DCS: it is
//! bookkeeping on top of the formations the ground war already runs.

use crate::db::{Db, formation::FormationId};
use bfprotocols::{
    db::objective::ObjectiveId,
    groundwar::{FormationInfo, LatLon, RegimentInfo},
};
use dcso3::{Vector2, coalition::Side};
use std::collections::BTreeMap;

/// Battalions in a full regiment.
pub(crate) const MAX_BATTALIONS: usize = 3;
/// Farthest a new battalion joins a regiment that isn't on its task, metres.
const JOIN_M: f64 = 40_000.;

/// What assignment needs to know about a formation.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Bn {
    pub id: FormationId,
    pub side: Side,
    /// 0 = none yet.
    pub regiment: u32,
    pub target: Option<ObjectiveId>,
    pub pos: Vector2,
}

/// A regiment for every battalion in `bns` that has none, oldest
/// formation first. Returns only the new assignments.
pub(crate) fn assign(bns: &[Bn]) -> Vec<(FormationId, u32)> {
    let mut cur: Vec<Bn> = bns.to_vec();
    cur.sort_by_key(|b| b.id);
    let mut out = vec![];
    for i in 0..cur.len() {
        let b = cur[i];
        if b.regiment != 0 {
            continue;
        }
        let mut regs: Vec<u32> = cur
            .iter()
            .filter(|x| x.side == b.side && x.regiment != 0)
            .map(|x| x.regiment)
            .collect();
        regs.sort_unstable();
        regs.dedup();
        let open: Vec<(u32, Vec<Bn>)> = regs
            .iter()
            .map(|r| {
                let m: Vec<Bn> = cur
                    .iter()
                    .filter(|x| x.side == b.side && x.regiment == *r)
                    .copied()
                    .collect();
                (*r, m)
            })
            .filter(|(_, m)| m.len() < MAX_BATTALIONS)
            .collect();
        let from_b = |m: &[Bn]| {
            let c = m.iter().fold(Vector2::zeros(), |a, x| a + x.pos) / m.len() as f64;
            (c - b.pos).norm()
        };
        let on_task = open
            .iter()
            .filter(|(_, m)| b.target.is_some() && m.iter().any(|x| x.target == b.target))
            .min_by(|(_, a), (_, c)| from_b(a).total_cmp(&from_b(c)))
            .map(|(r, _)| *r);
        let near = open
            .iter()
            .map(|(r, m)| (*r, from_b(m)))
            .filter(|(_, d)| *d <= JOIN_M)
            .min_by(|a, c| a.1.total_cmp(&c.1))
            .map(|(r, _)| r);
        let r = on_task
            .or(near)
            .unwrap_or_else(|| (1..).find(|n| !regs.contains(n)).unwrap_or(1));
        cur[i].regiment = r;
        out.push((b.id, r));
    }
    out
}

/// Give every formation without a regiment one (the ground war's tick).
pub(crate) fn assign_regiments(db: &mut Db) {
    let bns: Vec<Bn> = db
        .formations()
        .map(|f| Bn {
            id: f.id,
            side: f.side,
            regiment: f.regiment,
            target: f.order.target(),
            pos: f.pos,
        })
        .collect();
    let new = assign(&bns);
    if new.is_empty() {
        return;
    }
    for (id, r) in new {
        if let Some(f) = db.persisted.formations.get_mut_cow(&id) {
            f.regiment = r;
        }
    }
    db.ephemeral.dirty();
}

pub(crate) fn ordinal(n: u32) -> String {
    let suffix = match (n % 10, n % 100) {
        (_, 11..=13) => "th",
        (1, _) => "st",
        (2, _) => "nd",
        (3, _) => "rd",
        _ => "th",
    };
    format!("{n}{suffix}")
}

/// The task text for an order on `name`.
fn task_text(order: &str, name: Option<&str>) -> String {
    let at = name.unwrap_or("its objective");
    match order {
        "attack" => format!("attacking {at}"),
        "defend" => format!("defending {at}"),
        "withdraw" => format!("withdrawing to {at}"),
        _ => "holding".into(),
    }
}

/// The side's regiments from its formations, and each formation's place
/// in its regiment (`FormationInfo::battalion`). `main_effort` is the HQ's.
pub(crate) fn regiments(
    formations: &mut [FormationInfo],
    main_effort: Option<u64>,
) -> Vec<RegimentInfo> {
    let mut by_reg: BTreeMap<u32, Vec<usize>> = BTreeMap::new();
    for (i, f) in formations.iter().enumerate() {
        if f.regiment != 0 {
            by_reg.entry(f.regiment).or_default().push(i);
        }
    }
    let mut out = vec![];
    for (reg, mut idx) in by_reg {
        idx.sort_by_key(|i| formations[*i].id);
        for (n, i) in idx.iter().enumerate() {
            formations[*i].battalion = format!("{} Bn", ordinal(n as u32 + 1));
        }
        // the task most of its battalions are on, the first raised's on a tie
        let mut tasks: Vec<((String, Option<u64>), usize)> = vec![];
        for i in &idx {
            let f = &formations[*i];
            let key = (f.order.clone(), f.target);
            match tasks.iter_mut().find(|(k, _)| *k == key) {
                Some((_, c)) => *c += 1,
                None => tasks.push((key, 1)),
            }
        }
        let best = tasks.iter().map(|(_, c)| *c).max().unwrap_or(0);
        let (order, objective) = tasks
            .into_iter()
            .find(|(_, c)| *c == best)
            .map(|(k, _)| k)
            .unwrap_or_default();
        let objective_name = idx
            .iter()
            .map(|i| &formations[*i])
            .find(|f| f.target == objective)
            .and_then(|f| f.target_name.clone());
        let n = idx.len() as f64;
        let pos: LatLon = idx.iter().fold([0., 0.], |a, i| {
            let p = formations[*i].pos;
            [a[0] + p[0] / n, a[1] + p[1] / n]
        });
        out.push(RegimentInfo {
            id: reg,
            name: format!("{} Regiment", ordinal(reg)),
            task: task_text(&order, objective_name.as_deref()),
            main_effort: order == "attack"
                && objective.is_some()
                && objective == main_effort,
            order,
            objective,
            objective_name,
            battalions: idx.iter().map(|i| formations[*i].id).collect(),
            alive: idx.iter().map(|i| formations[*i].alive).sum(),
            total: idx.iter().map(|i| formations[*i].total).sum(),
            pos,
            engaged: idx.iter().filter(|i| formations[**i].engaged).count() as u32,
        });
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bn(id: u32, side: Side, regiment: u32, target: Option<i64>, x: f64) -> Bn {
        Bn {
            id,
            side,
            regiment,
            target: target.map(ObjectiveId::from),
            pos: Vector2::new(x, 0.),
        }
    }

    #[test]
    fn joins_on_task_then_near_then_new() {
        let bns = [
            bn(1, Side::Blue, 1, Some(10), 0.),
            // same objective: joins the 1st, wherever it is
            bn(2, Side::Blue, 0, Some(10), 10_000.),
            // no task in common, but near the 1st (2 of 3): joins it
            bn(3, Side::Blue, 0, Some(20), 5_000.),
            // 1st is full, nothing near: a new regiment, number 2
            bn(4, Side::Blue, 0, Some(30), 500_000.),
            // the other side numbers its own
            bn(5, Side::Red, 0, Some(10), 0.),
        ];
        let got = assign(&bns);
        assert_eq!(got, vec![(2, 1), (3, 1), (4, 2), (5, 1)]);
    }

    #[test]
    fn reuses_freed_numbers_and_skips_assigned() {
        let bns = [bn(7, Side::Red, 3, None, 0.), bn(8, Side::Red, 0, None, 900_000.)];
        // 1 is free (its battalions are gone), so the new one is the 1st
        assert_eq!(assign(&bns), vec![(8, 1)]);
        assert!(assign(&[bn(9, Side::Red, 2, None, 0.)]).is_empty());
        // on the same task it joins however far apart they are
        let far = [
            bn(1, Side::Blue, 1, Some(10), 0.),
            bn(2, Side::Blue, 0, Some(10), 300_000.),
        ];
        assert_eq!(assign(&far), vec![(2, 1)]);
    }

    #[test]
    fn names() {
        assert_eq!(ordinal(1), "1st");
        assert_eq!(ordinal(2), "2nd");
        assert_eq!(ordinal(3), "3rd");
        assert_eq!(ordinal(11), "11th");
        assert_eq!(ordinal(22), "22nd");
        assert_eq!(task_text("attack", Some("HJ01")), "attacking HJ01");
        assert_eq!(task_text("withdraw", Some("H4")), "withdrawing to H4");
        assert_eq!(task_text("hold", None), "holding");
    }
}
