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

//! The plan: posture, main effort, what to hold and what to resupply.
//!
//! The HQ's own rules always produce a full plan. A strategist directive
//! then replaces whichever fields it sets, and a human override replaces
//! whichever fields *it* sets on top of that -- so either can be as terse as
//! "main effort: Gori" and the rest still gets decided.

use super::{Override, picture::Picture, posture_weights};
use bfprotocols::{
    cfg::{HqCfg, HqPostureCfg},
    db::objective::ObjectiveId,
    hq::{Directive, Line, OpKind, Posture},
};
use compact_str::{CompactString, format_compact};
use std::collections::BTreeMap;

/// A new target must beat the standing main effort by this much to replace
/// it, so the HQ does not flip its whole war between two similar bases
/// every pass.
const STICKINESS: f64 = 1.35;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Source {
    Rules,
    /// A story event's standing orders (`Cfg::story`).
    Story,
    Strategist,
    Human,
}

impl Source {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Rules => "rules",
            Self::Story => "story",
            Self::Strategist => "strategist",
            Self::Human => "human",
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Plan {
    pub(crate) posture: Posture,
    pub(crate) source: Source,
    pub(crate) main_effort: Option<ObjectiveId>,
    pub(crate) defend: Vec<ObjectiveId>,
    pub(crate) supply: Vec<ObjectiveId>,
    pub(crate) avoid: Vec<ObjectiveId>,
    pub(crate) weights: BTreeMap<Line, f64>,
    /// As players read it.
    pub(crate) intent: CompactString,
    /// Why the rules chose what they chose.
    pub(crate) reasons: Vec<CompactString>,
    pub(crate) disabled: Vec<OpKind>,
    pub(crate) paused: bool,
    /// What the side's own state does to the plan (`HqCfg::posture`).
    pub(crate) strain: Strain,
}

/// What the side's own state -- will, fuel, fighters (`HqCfg::posture`,
/// E7) -- does to the plan besides its posture. These are the side's means,
/// not an opinion, so they stand whatever posture a story event, the
/// strategist or a human sets. All 1 (and no kinds) without `hq.posture`.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Strain {
    /// On top of the line weights, per kind of operation: low will avoids
    /// the costly and risky ones, depleted fighters fly less SEAD.
    pub(crate) kinds: BTreeMap<OpKind, f64>,
    /// CAP far forward, over the base the main effort is pushed from.
    pub(crate) cap_forward: f64,
    /// CAP over the bases being held; above 1, the side's own SAM sites get
    /// CAP too.
    pub(crate) cap_home: f64,
    /// How often the ground war's AI sends its attack waves, 0..1.
    pub(crate) ground_attacks: f64,
    /// The state that turned the posture, for the announcement.
    pub(crate) note: Option<CompactString>,
}

impl Default for Strain {
    fn default() -> Self {
        Self {
            kinds: BTreeMap::new(),
            cap_forward: 1.,
            cap_home: 1.,
            ground_attacks: 1.,
            note: None,
        }
    }
}

impl Plan {
    pub(crate) fn weight(&self, line: Line) -> f64 {
        self.weights.get(&line).copied().unwrap_or(1.)
    }

    /// The side's own state's weight on `kind` (`Strain`).
    pub(crate) fn kind_weight(&self, kind: OpKind) -> f64 {
        self.strain.kinds.get(&kind).copied().unwrap_or(1.)
    }

    pub(crate) fn avoided(&self, id: Option<ObjectiveId>) -> bool {
        id.is_some_and(|id| self.avoid.contains(&id))
    }
}

fn oid(id: u64) -> ObjectiveId {
    ObjectiveId::from(id as i64)
}

/// How good a target an enemy objective is for the main effort: worth,
/// weakness, and how close it is to ground we already hold.
fn effort_score(pic: &Picture, cfg: &HqCfg, id: &ObjectiveId) -> Option<f64> {
    let o = pic.get(id)?;
    if o.own || o.neutral || o.at_sea() || o.sam_site() || o.value() <= 0. {
        return None;
    }
    let gap = pic.gap(o.pos);
    if gap > cfg.max_ground_range_m {
        return None;
    }
    let weakness = 1.5 - o.health as f64 / 200. - o.logi as f64 / 400.;
    let capturable = if o.capturable { 1.8 } else { 1. };
    let underway = if o.capturing { 1.5 } else { 1. };
    // Every 20 km of separation halves the appeal.
    let near = 1. / (1. + gap / 20_000.);
    Some(o.value() * weakness * capturable * underway * near)
}

fn rules(cfg: &HqCfg, pic: &Picture, prev: Option<&Plan>) -> Plan {
    let own: Vec<_> = pic.own().collect();
    let enemy: Vec<_> = pic.enemy().collect();
    let total = (own.len() + enemy.len()).max(1) as f64;
    let territory = own.len() as f64 / total;
    let being_captured = own.iter().filter(|o| o.being_captured).count();
    let threatened = own.iter().filter(|o| o.threatened).count();
    let capturable_own = own.iter().filter(|o| o.capturable).count();
    let pressure = (being_captured * 3 + threatened + capturable_own * 2) as f64;
    let opportunities = enemy
        .iter()
        .filter(|o| {
            pic.gap(o.pos) <= cfg.max_ground_range_m && !o.sam_site() && !o.at_sea()
        })
        .map(|o| {
            if o.capturable {
                2.
            } else if o.health < 50 {
                1.
            } else {
                0.
            }
        })
        .sum::<f64>();
    let mut reasons: Vec<CompactString> = vec![format_compact!(
        "holding {} of {} objectives ({:.0}%)",
        own.len(),
        total as usize,
        territory * 100.
    )];
    if pressure > 0. {
        reasons.push(format_compact!(
            "{being_captured} base(s) being captured, {threatened} threatened, {capturable_own} with no logistics left"
        ));
    }
    if opportunities > 0. {
        reasons.push(format_compact!(
            "{opportunities:.0} point(s) of opportunity within reach"
        ));
    }
    // Pressure scales with the size of the side: two threatened bases out of
    // forty is a quiet night, two out of five is a crisis.
    let size = (own.len() as f64 / 6.).max(1.);
    let mut score = (opportunities - pressure) / size + (territory - 0.5) * 2.;
    let state = cfg.posture.as_ref().filter(|p| p.enabled);
    let mut strain = Strain::default();
    if let Some(pc) = state {
        score += will(pc, pic, &mut strain, &mut reasons);
    }
    let mut posture = if being_captured > 0 && pressure / size >= 2. {
        Posture::Defensive
    } else if score >= 0.5 {
        Posture::Offensive
    } else if score <= -1. {
        Posture::Defensive
    } else {
        Posture::Balanced
    };
    if let Some(pc) = state {
        posture = fuel(pc, pic, posture, &mut strain, &mut reasons);
        air(pc, pic, &mut strain, &mut reasons);
    }
    reasons.push(format_compact!("posture {} (score {score:.1})", posture.label()));

    // Main effort: best target, but the standing one keeps it unless beaten
    // clearly.
    let mut best: Option<(ObjectiveId, f64)> = None;
    for o in enemy.iter() {
        if let Some(s) = effort_score(pic, cfg, &o.id)
            && best.is_none_or(|(_, b)| s > b)
        {
            best = Some((o.id, s));
        }
    }
    let standing = prev
        .and_then(|p| p.main_effort)
        .and_then(|id| effort_score(pic, cfg, &id).map(|s| (id, s)));
    let main_effort = match (standing, best) {
        (Some((sid, ss)), Some((bid, bs))) if bid != sid && bs > ss * STICKINESS => {
            Some(bid)
        }
        (Some((sid, _)), _) => Some(sid),
        (None, b) => b.map(|(id, _)| id),
    };
    if let Some(name) = main_effort.and_then(|id| pic.get(&id)).map(|o| o.name.clone()) {
        reasons.push(format_compact!("main effort {name}"));
    }

    // Hold: being captured, then threatened, then wide open; front first.
    let mut defend: Vec<(u8, f64, ObjectiveId)> = own
        .iter()
        .filter(|o| o.being_captured || o.threatened || o.capturable)
        .map(|o| {
            let rank = if o.being_captured {
                0
            } else if o.threatened {
                1
            } else {
                2
            };
            (rank, o.front_m, o.id)
        })
        .collect();
    defend.sort_by(|a, b| {
        (a.0, a.1).partial_cmp(&(b.0, b.1)).unwrap_or(std::cmp::Ordering::Equal)
    });
    let defend: Vec<ObjectiveId> =
        defend.into_iter().take(4).map(|(_, _, id)| id).collect();

    // Resupply: the emptiest first, weighted toward the front and anything
    // we are holding or staging from.
    let staging = main_effort.and_then(|me| {
        let p = pic.get(&me)?.pos;
        own.iter()
            .min_by(|a, b| super::dist(a.pos, p).total_cmp(&super::dist(b.pos, p)))
            .map(|o| o.id)
    });
    let mut supply: Vec<(f64, ObjectiveId)> = own
        .iter()
        .filter(|o| !o.at_sea() && o.stores() < cfg.resupply_below_pct)
        .map(|o| {
            let need = (100 - o.stores().min(100)) as f64;
            let front = 1. + 1. / (1. + o.front_m / 30_000.);
            let key =
                if defend.contains(&o.id) || Some(o.id) == staging { 1.5 } else { 1. };
            (need * front * key, o.id)
        })
        .collect();
    supply.sort_by(|a, b| b.0.total_cmp(&a.0));
    let supply: Vec<ObjectiveId> = supply.into_iter().take(6).map(|(_, id)| id).collect();

    Plan {
        posture,
        source: Source::Rules,
        main_effort,
        defend,
        supply,
        avoid: vec![],
        weights: posture_weights(posture),
        intent: CompactString::new(""),
        reasons,
        disabled: vec![],
        paused: false,
        strain,
    }
}

/// Will (`Picture::will`): the posture score it adds, pushing toward
/// offensive above `will_neutral` and defensive below; under `will_low` the
/// costly and risky kinds are worth less.
fn will(
    pc: &HqPostureCfg,
    pic: &Picture,
    strain: &mut Strain,
    reasons: &mut Vec<CompactString>,
) -> f64 {
    let Some(w) = pic.will.filter(|w| w.is_finite()) else { return 0. };
    let shift = (w - pc.will_neutral) / 50. * pc.will_weight;
    if w < pc.will_low {
        let k = pc.will_low_risky_weight.clamp(0., 1.);
        for kind in pc.risky_ops.iter() {
            strain.kinds.insert(*kind, k);
        }
        reasons.push(format_compact!(
            "will {w:.0} -- avoiding costly operations (x{k:.1}), score {shift:+.1}"
        ));
        if shift < 0. {
            strain.note = Some(format_compact!("will {w:.0}"));
        }
    } else {
        reasons.push(format_compact!("will {w:.0}, score {shift:+.1}"));
    }
    shift
}

/// Fuel (`Picture::fuel_pct`): short of it the side holds whatever the map
/// says -- the offensive stalls -- and critical, its ground attacks thin out
/// too.
fn fuel(
    pc: &HqPostureCfg,
    pic: &Picture,
    posture: Posture,
    strain: &mut Strain,
    reasons: &mut Vec<CompactString>,
) -> Posture {
    let Some(f) = pic.fuel_pct.filter(|f| f.is_finite()) else { return posture };
    if f < pc.fuel_critical_pct {
        let k = pc.fuel_critical_ground_attacks.clamp(0., 1.);
        strain.ground_attacks = k;
        let note = format_compact!("fuel {f:.0}% -- critical, holding");
        reasons.push(format_compact!("{note}, ground attacks x{k:.1}"));
        strain.note = Some(note);
        Posture::Defensive
    } else if f < pc.fuel_low_pct {
        let note = format_compact!("fuel {f:.0}% -- holding");
        reasons.push(note.clone());
        strain.note = Some(note);
        Posture::Defensive
    } else {
        reasons.push(format_compact!("fuel {f:.0}%"));
        posture
    }
}

/// Air strength (`Picture::fighters` against `Picture::enemy_air`, what
/// the radars hold): depleted, the fighters stay home -- less CAP far
/// forward and SEAD, more over the bases being held and the SAM sites.
fn air(
    pc: &HqPostureCfg,
    pic: &Picture,
    strain: &mut Strain,
    reasons: &mut Vec<CompactString>,
) {
    let Some(own) = pic.fighters else { return };
    let seen = pic.enemy_air.len();
    let ratio = own as f64 / seen.max(pc.air_min_enemy as usize).max(1) as f64;
    if ratio < pc.air_depleted_ratio {
        let fwd = pc.air_depleted_forward_weight.max(0.);
        strain.cap_forward = fwd;
        strain.cap_home = pc.air_depleted_home_weight.max(0.);
        let sead = strain.kinds.entry(OpKind::Sead).or_insert(1.);
        *sead *= fwd;
        reasons.push(format_compact!(
            "{own} fighters against {seen} enemy aircraft on radar -- depleted: CAP kept over our bases and SAMs, sweeps x{fwd:.1}"
        ));
    } else {
        reasons.push(format_compact!(
            "{own} fighters against {seen} enemy aircraft on radar"
        ));
    }
}

/// Lay `d` over `plan`: every field it sets wins.
fn apply(plan: &mut Plan, pic: &Picture, d: &Directive, source: Source) {
    let mut touched = false;
    if let Some(p) = d.posture {
        if p != plan.posture {
            // A posture change carries its own weights; explicit ones below
            // still win.
            plan.weights = posture_weights(p);
        }
        plan.posture = p;
        touched = true;
    }
    if let Some(me) =
        d.main_effort.map(oid).filter(|id| pic.get(id).is_some_and(|o| !o.own))
    {
        plan.main_effort = Some(me);
        touched = true;
    }
    let ours = |id: &ObjectiveId| pic.get(id).is_some_and(|o| o.own);
    if !d.defend.is_empty() {
        let mut v: Vec<ObjectiveId> =
            d.defend.iter().copied().map(oid).filter(|i| ours(i)).collect();
        // Anything actually being captured stays on the list whatever the
        // directive says.
        for id in plan.defend.iter().copied() {
            if pic.get(&id).is_some_and(|o| o.being_captured) && !v.contains(&id) {
                v.push(id);
            }
        }
        plan.defend = v;
        touched = true;
    }
    if !d.supply_priority.is_empty() {
        let mut v: Vec<ObjectiveId> =
            d.supply_priority.iter().copied().map(oid).filter(|i| ours(i)).collect();
        for id in plan.supply.iter().copied() {
            if !v.contains(&id) {
                v.push(id);
            }
        }
        v.truncate(8);
        plan.supply = v;
        touched = true;
    }
    if !d.avoid.is_empty() {
        plan.avoid = d.avoid.iter().copied().map(oid).collect();
        if plan.main_effort.is_some_and(|m| plan.avoid.contains(&m)) {
            plan.main_effort = None;
        }
        touched = true;
    }
    for (line, w) in d.weights.iter() {
        plan.weights.insert(*line, *w);
        touched = true;
    }
    if let Some(i) = d.intent.as_ref().filter(|i| !i.trim().is_empty()) {
        plan.intent = i.trim().into();
        touched = true;
    }
    if touched {
        plan.source = source;
    }
}

fn names(pic: &Picture, ids: &[ObjectiveId]) -> CompactString {
    let v: Vec<&str> =
        ids.iter().filter_map(|id| pic.get(id)).map(|o| o.name.as_str()).collect();
    CompactString::from(v.join(", "))
}

/// The intent paragraph the HQ writes for itself when nobody gave it one.
fn write_intent(pic: &Picture, plan: &Plan) -> CompactString {
    let mut s = CompactString::new("");
    match (plan.posture, plan.main_effort.and_then(|id| pic.get(&id))) {
        (Posture::Defensive, Some(me)) => s.push_str(&format_compact!(
            "Hold the line; strike back at {} when it is cheap.",
            me.name
        )),
        (Posture::Defensive, None) => s.push_str("Hold the line."),
        (Posture::Offensive, Some(me)) => {
            s.push_str(&format_compact!("Take {}.", me.name))
        }
        (Posture::Balanced, Some(me)) => {
            s.push_str(&format_compact!("Hold what we have; press {}.", me.name))
        }
        (_, None) => s.push_str("Hold what we have and keep it supplied."),
    }
    if !plan.defend.is_empty() {
        s.push_str(&format_compact!(
            " Defend {}.",
            names(pic, &plan.defend[..plan.defend.len().min(3)])
        ));
    }
    if !plan.supply.is_empty() {
        s.push_str(&format_compact!(
            " Resupply {} first.",
            names(pic, &plan.supply[..plan.supply.len().min(3)])
        ));
    }
    s
}

pub(crate) fn plan(
    cfg: &HqCfg,
    pic: &Picture,
    prev: Option<&Plan>,
    story: Option<&Directive>,
    directive: Option<&Directive>,
    human: Option<&Override>,
) -> Plan {
    let mut plan = rules(cfg, pic, prev);
    if let Some(d) = story {
        apply(&mut plan, pic, d, Source::Story);
    }
    if let Some(d) = directive.filter(|_| cfg.strategist.enabled) {
        apply(&mut plan, pic, d, Source::Strategist);
    }
    if let Some(h) = human {
        apply(&mut plan, pic, &h.directive, Source::Human);
        plan.disabled = h.disabled_ops.clone();
        plan.paused = h.paused;
        plan.source = Source::Human;
    }
    let max = cfg.strategist.max_weight.max(1.);
    for w in plan.weights.values_mut() {
        *w = w.clamp(0., max);
    }
    if plan.intent.is_empty() {
        plan.intent = write_intent(pic, &plan);
    }
    plan
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hq::picture::{Obj, Picture};
    use bfprotocols::db::objective::ObjectiveKind;
    use dcso3::Vector2;

    fn obj(id: i64, x: f64, own: bool, health: u8, capturable: bool) -> Obj {
        Obj {
            id: ObjectiveId::from(id),
            name: format_compact!("o{id}"),
            pos: Vector2::new(x, 0.),
            own,
            neutral: false,
            kind: ObjectiveKind::Fob,
            health,
            logi: if capturable { 0 } else { 100 },
            supply: 100,
            fuel: 100,
            threatened: false,
            being_captured: false,
            capturing: false,
            capturable,
            airbase: false,
            front_m: 20_000.,
            fires: false,
            inbound: false,
        }
    }

    fn pic(objs: Vec<Obj>) -> Picture {
        Picture {
            objs,
            humans: 0,
            humans_fw_air: 0,
            humans_helo_air: 0,
            enemy_air: vec![],
            enemy_ground: vec![],
            enemy_formations: vec![],
            formations: 0,
            formations_idle: 0,
            missile_groups: Default::default(),
            enemy_convoys_near: 0,
            under_way: vec![],
            jtac_targets: Default::default(),
            carriers: Default::default(),
            battles: Default::default(),
            front: vec![],
            support: vec![],
            will: None,
            fuel_pct: None,
            fighters: None,
        }
    }

    fn with_state() -> HqCfg {
        HqCfg { posture: Some(HqPostureCfg::default()), ..HqCfg::default() }
    }

    /// One of ours, three enemy bases in reach and one of them with no
    /// logistics left: the map says attack.
    fn good_map() -> Picture {
        pic(vec![
            obj(1, 0., true, 100, false),
            obj(4, -15_000., true, 100, false),
            obj(2, 20_000., false, 40, false),
            obj(3, 25_000., false, 100, true),
        ])
    }

    /// As many bases as the enemy, nothing to take: balanced.
    fn even_map() -> Picture {
        pic(vec![obj(1, 0., true, 100, false), obj(2, 20_000., false, 100, false)])
    }

    /// Three of ours, one threatened, against two healthy enemy bases:
    /// balanced, leaning defensive.
    fn threatened_map() -> Picture {
        let mut objs = vec![
            obj(1, 0., true, 100, false),
            obj(4, -10_000., true, 100, false),
            obj(5, -20_000., true, 100, false),
            obj(2, 20_000., false, 100, false),
            obj(6, 30_000., false, 100, false),
        ];
        objs[0].threatened = true;
        pic(objs)
    }

    fn said(p: &Plan, what: &str) -> bool {
        p.reasons.iter().any(|r| r.contains(what))
    }

    #[test]
    fn without_the_posture_block_the_sides_state_changes_nothing() {
        let cfg = HqCfg::default();
        for base in [good_map(), even_map(), threatened_map()] {
            let a = plan(&cfg, &base, None, None, None, None);
            let mut state = base.clone();
            state.will = Some(5.);
            state.fuel_pct = Some(3.);
            state.fighters = Some(0);
            state.enemy_air = vec![Vector2::new(0., 0.); 10];
            let b = plan(&cfg, &state, None, None, None, None);
            assert_eq!(a.posture, b.posture);
            assert_eq!(a.main_effort, b.main_effort);
            assert_eq!(a.weights, b.weights);
            assert_eq!(a.reasons, b.reasons);
            assert_eq!(b.strain, Strain::default());
            // The block on with nothing to read: the map alone, as before.
            let c = plan(&with_state(), &base, None, None, None, None);
            assert_eq!(a.posture, c.posture);
            assert_eq!(a.weights, c.weights);
            assert_eq!(a.reasons, c.reasons);
            assert_eq!(c.strain, Strain::default());
        }
    }

    #[test]
    fn short_of_fuel_the_side_holds_whatever_the_map_says() {
        let cfg = with_state();
        let mut p = good_map();
        assert_eq!(plan(&cfg, &p, None, None, None, None).posture, Posture::Offensive);
        p.fuel_pct = Some(80.);
        let full = plan(&cfg, &p, None, None, None, None);
        assert_eq!(full.posture, Posture::Offensive);
        assert!(said(&full, "fuel 80%"), "{:?}", full.reasons);
        p.fuel_pct = Some(22.);
        let low = plan(&cfg, &p, None, None, None, None);
        assert_eq!(low.posture, Posture::Defensive);
        assert!(said(&low, "fuel 22% -- holding"), "{:?}", low.reasons);
        assert_eq!(low.strain.ground_attacks, 1.);
        assert_eq!(low.weights, posture_weights(Posture::Defensive));
        // Critical: the ground attacks thin out too, and that holds even
        // when a human orders the side forward.
        p.fuel_pct = Some(6.);
        let h = Override {
            by: "cdr".into(),
            set: chrono::Utc::now(),
            expires: chrono::Utc::now(),
            directive: Directive {
                posture: Some(Posture::Offensive),
                ..Default::default()
            },
            disabled_ops: vec![],
            paused: false,
        };
        let crit = plan(&cfg, &p, None, None, None, Some(&h));
        assert_eq!(crit.posture, Posture::Offensive);
        assert_eq!(crit.source, Source::Human);
        assert_eq!(crit.strain.ground_attacks, 0.5);
        assert!(said(&crit, "critical"), "{:?}", crit.reasons);
    }

    #[test]
    fn high_will_takes_an_even_map_onto_the_offensive() {
        let cfg = with_state();
        let mut p = even_map();
        assert_eq!(plan(&cfg, &p, None, None, None, None).posture, Posture::Balanced);
        p.will = Some(50.);
        assert_eq!(plan(&cfg, &p, None, None, None, None).posture, Posture::Balanced);
        p.will = Some(90.);
        p.fuel_pct = Some(90.);
        let a = plan(&cfg, &p, None, None, None, None);
        assert_eq!(a.posture, Posture::Offensive);
        assert!(said(&a, "will 90"), "{:?}", a.reasons);
        assert!(a.strain.kinds.is_empty());
        // The good map with high will but no fuel still holds.
        let mut g = good_map();
        g.will = Some(90.);
        g.fuel_pct = Some(20.);
        assert_eq!(plan(&cfg, &g, None, None, None, None).posture, Posture::Defensive);
    }

    #[test]
    fn low_will_and_a_threatened_base_put_the_side_on_the_defensive() {
        let cfg = with_state();
        let mut p = threatened_map();
        let a = plan(&cfg, &p, None, None, None, None);
        assert_eq!(a.posture, Posture::Balanced);
        p.will = Some(20.);
        let b = plan(&cfg, &p, None, None, None, None);
        assert_eq!(b.posture, Posture::Defensive);
        // ... and it avoids its costly operations.
        assert_eq!(b.kind_weight(OpKind::Bomber), 0.6);
        assert_eq!(b.kind_weight(OpKind::Strike), 0.6);
        assert_eq!(b.kind_weight(OpKind::Cap), 1.);
        assert!(said(&b, "avoiding costly operations"), "{:?}", b.reasons);
    }

    #[test]
    fn a_depleted_air_force_flies_fewer_sweeps() {
        let cfg = with_state();
        let mut p = even_map();
        p.enemy_air = vec![Vector2::new(10_000., 0.); 12];
        p.fighters = Some(40);
        let strong = plan(&cfg, &p, None, None, None, None);
        assert_eq!(strong.kind_weight(OpKind::Sead), 1.);
        assert_eq!(strong.strain.cap_forward, 1.);
        p.fighters = Some(6);
        let weak = plan(&cfg, &p, None, None, None, None);
        assert_eq!(weak.kind_weight(OpKind::Sead), 0.5);
        assert_eq!(weak.strain.cap_forward, 0.5);
        assert_eq!(weak.strain.cap_home, 1.3);
        assert!(said(&weak, "6 fighters against 12"), "{:?}", weak.reasons);
        // A quiet radar doesn't make a handful of fighters look strong.
        p.enemy_air.clear();
        p.fighters = Some(3);
        assert_eq!(plan(&cfg, &p, None, None, None, None).strain.cap_forward, 0.5);
        // Low will and depleted: both lower SEAD.
        p.will = Some(10.);
        let both = plan(&cfg, &p, None, None, None, None);
        assert!((both.kind_weight(OpKind::Sead) - 0.3).abs() < 1e-9);
    }

    #[test]
    fn the_main_effort_goes_where_it_is_cheap_and_sticks() {
        let cfg = HqCfg::default();
        let p = pic(vec![
            obj(1, 0., true, 100, false),
            obj(2, 20_000., false, 100, false),
            obj(3, 25_000., false, 100, true),
        ]);
        let a = plan(&cfg, &p, None, None, None, None);
        assert_eq!(a.main_effort, Some(ObjectiveId::from(3)));
        // Objective 2 gets a little weaker: not enough to switch.
        let mut objs = p.objs.clone();
        objs[1].health = 70;
        let b = plan(&cfg, &pic(objs.clone()), Some(&a), None, None, None);
        assert_eq!(b.main_effort, Some(ObjectiveId::from(3)));
        // 3 is taken (now ours): the effort moves on.
        objs[2].own = true;
        let c = plan(&cfg, &pic(objs), Some(&b), None, None, None);
        assert_eq!(c.main_effort, Some(ObjectiveId::from(2)));
    }

    #[test]
    fn a_base_being_taken_puts_the_side_on_the_defensive() {
        let cfg = HqCfg::default();
        let mut objs = vec![
            obj(1, 0., true, 100, false),
            obj(2, 20_000., false, 100, false),
            obj(4, -10_000., true, 100, false),
        ];
        objs[0].being_captured = true;
        objs[0].threatened = true;
        let a = plan(&cfg, &pic(objs), None, None, None, None);
        assert_eq!(a.posture, Posture::Defensive);
        assert_eq!(a.defend.first(), Some(&ObjectiveId::from(1)));
    }

    #[test]
    fn a_directive_overrides_only_what_it_names_and_a_human_beats_it() {
        let cfg = HqCfg::default();
        let p = pic(vec![
            obj(1, 0., true, 100, false),
            obj(2, 20_000., false, 100, false),
            obj(3, 25_000., false, 100, true),
        ]);
        let d = Directive { main_effort: Some(2), ..Default::default() };
        let a = plan(&cfg, &p, None, None, Some(&d), None);
        assert_eq!(a.main_effort, Some(ObjectiveId::from(2)));
        assert_eq!(a.source, Source::Strategist);
        // A directive naming our own base as the main effort is ignored.
        let bad = Directive { main_effort: Some(1), ..Default::default() };
        assert_eq!(
            plan(&cfg, &p, None, None, Some(&bad), None).main_effort,
            Some(ObjectiveId::from(3))
        );
        let h = Override {
            by: "cdr".into(),
            set: chrono::Utc::now(),
            expires: chrono::Utc::now(),
            directive: Directive {
                posture: Some(Posture::Defensive),
                ..Default::default()
            },
            disabled_ops: vec![OpKind::MissileStrike],
            paused: false,
        };
        let b = plan(&cfg, &p, None, None, Some(&d), Some(&h));
        assert_eq!(b.posture, Posture::Defensive);
        assert_eq!(b.main_effort, Some(ObjectiveId::from(2)));
        assert_eq!(b.source, Source::Human);
        assert_eq!(b.disabled, vec![OpKind::MissileStrike]);
    }
}
