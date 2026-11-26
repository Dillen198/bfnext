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

//! War will (`Cfg::war_will`, Red Storm Rising ENGINE.md 3.2): each side's
//! will to fight, 0-100, saved with the campaign.
//!
//! Moved by what happens, with the weights in `WarWillCfg`:
//!
//! - **losses** (`Db::unit_dead`): aircraft by type, ships;
//! - **formations** destroyed (`Db::cleanup` in the ground war);
//! - **captures** (`Db::check_capture`): the loser by the kind of base, the
//!   captor gains;
//! - **objective health** (`Db::update_objective_status`): a carrier group
//!   damaged or sunk, a factory or command centre destroyed;
//! - **fuel** (the slow tick): a side whose warehouses hold little fuel
//!   drains by the hour.
//!
//! Every loss gives the other side `enemy_loss_share` of it. Will recovers
//! slowly while nothing bad happens. A side below `threshold` for
//! `hold_hours` asks for terms: `Db::check_victory` ends the round with the
//! other side the winner. Nothing here is scripted or clock-driven: only
//! the recovery and the fuel drain run on the tick, and both come from the
//! state play leaves.
//!
//! `WarWill` is the pure state machine (tested below without a mission);
//! the `Db` methods feed it and announce what it says.

use super::{
    Db,
    objective::{countdown_announce_due, fmt_countdown},
};
use bfprotocols::{
    cfg::{UnitTag, UnitTags, WarWillCfg},
    db::objective::{ObjectiveId, ObjectiveKind},
    stats::{Stat, WarWillStat, WillCause},
};
use chrono::{DateTime, Duration, Utc};
use compact_str::format_compact;
use dcso3::{
    String, coalition::Side, group::GroupCategory, object::DcsOid, unit::ClassUnit,
};
use fxhash::FxHashMap;
use log::info;
use serde_derive::{Deserialize, Serialize};

/// The longest list of causes kept.
const MAX_CAUSES: usize = 10;
/// Causes older than this are dropped.
const CAUSE_MAX_AGE: Duration = Duration::hours(24);
/// The same cause again within this long is added to the first.
const CAUSE_MERGE: Duration = Duration::hours(1);
/// How often the trend history takes a sample.
const SAMPLE_EVERY: Duration = Duration::minutes(10);
/// The trend's window.
const TREND_WINDOW: Duration = Duration::hours(3);
/// The least history a trend is shown on.
const TREND_MIN: Duration = Duration::hours(1);
/// A tick never accounts for more than this: a long gap (a stopped server)
/// is not an hour-by-hour drain.
const MAX_TICK: Duration = Duration::hours(1);
/// Publish at most this often unless a side moved by a point or more.
const PUBLISH_MIN: Duration = Duration::seconds(60);
/// And at least this often, so bfdb has a value soon after it restarts.
const PUBLISH_HEARTBEAT: Duration = Duration::minutes(15);

/// One side's will.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SideWill {
    pub value: f64,
    /// The last loss, for recovery.
    #[serde(default)]
    pub last_loss: Option<DateTime<Utc>>,
    /// Since when the side has been below the threshold.
    #[serde(default)]
    pub below_since: Option<DateTime<Utc>>,
    /// 0 fuel fine, 1 low, 2 critical: a cause is noted on a step down.
    #[serde(default)]
    pub fuel_band: u8,
}

/// What a tick has to say.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WillNote {
    /// The side has dropped below the threshold.
    Breaking(Side),
    /// The side is back above it.
    Rallied(Side),
}

/// Not saved: what the engine needs between ticks.
#[derive(Debug, Clone, Default)]
struct WillRt {
    /// When the last `Stat::WarWill` went out, and both values in it.
    published: Option<(DateTime<Utc>, f64, f64)>,
    /// Something moved or a cause was added since then.
    changed: bool,
    /// Each objective's owner and health as last seen, so a health change
    /// is read against the same owner's previous health. Empty after a
    /// restart: the first look only records.
    seen: FxHashMap<ObjectiveId, (Side, u8)>,
    /// The threshold countdown's last announcement, Blue then Red.
    countdown_last: [Option<i64>; 2],
}

/// Both sides' will, the trend history and the recent causes.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WarWill {
    pub blue: SideWill,
    pub red: SideWill,
    /// The last tick, for recovery and fuel.
    #[serde(default)]
    pub last_tick: Option<DateTime<Utc>>,
    /// (time, blue, red) every `SAMPLE_EVERY`, for the trend.
    #[serde(default)]
    pub samples: Vec<(DateTime<Utc>, f64, f64)>,
    /// The biggest recent causes, oldest first.
    #[serde(default)]
    pub causes: Vec<WillCause>,
    /// Factories and command centres already counted as destroyed, so a
    /// capture of the wreck doesn't count it again. Cleared when the
    /// objective is repaired or changes hands.
    #[serde(default)]
    pub destroyed: Vec<ObjectiveId>,
    #[serde(skip)]
    rt: WillRt,
}

fn side_idx(side: Side) -> Option<usize> {
    match side {
        Side::Blue => Some(0),
        Side::Red => Some(1),
        Side::Neutral => None,
    }
}

impl WarWill {
    pub fn new(cfg: &WarWillCfg) -> Self {
        let start = cfg.start();
        let s = SideWill { value: start, ..Default::default() };
        Self { blue: s.clone(), red: s, ..Default::default() }
    }

    pub fn side(&self, side: Side) -> Option<&SideWill> {
        match side {
            Side::Blue => Some(&self.blue),
            Side::Red => Some(&self.red),
            Side::Neutral => None,
        }
    }

    fn side_mut(&mut self, side: Side) -> Option<&mut SideWill> {
        match side {
            Side::Blue => Some(&mut self.blue),
            Side::Red => Some(&mut self.red),
            Side::Neutral => None,
        }
    }

    pub fn value(&self, side: Side) -> Option<f64> {
        self.side(side).map(|s| s.value)
    }

    /// Move `side`'s will by `delta`, clamped to 0-100, noting `text` as the
    /// cause. Returns the change actually made.
    pub fn apply(
        &mut self,
        side: Side,
        delta: f64,
        text: &str,
        now: DateTime<Utc>,
    ) -> f64 {
        if !delta.is_finite() {
            return 0.;
        }
        let Some(s) = self.side_mut(side) else { return 0. };
        let before = s.value;
        s.value = (s.value + delta).clamp(0., 100.);
        let done = s.value - before;
        if delta < 0. {
            s.last_loss = Some(now);
        }
        if done.abs() > 1e-9 {
            self.rt.changed = true;
            self.push_cause(side, text, done, now);
        }
        done
    }

    /// `side` lost something worth `amount`: it loses that, and the other
    /// side gains its share.
    pub fn loss(
        &mut self,
        cfg: &WarWillCfg,
        side: Side,
        amount: f64,
        text: &str,
        now: DateTime<Utc>,
    ) {
        if side == Side::Neutral || !(amount > 0.) {
            return;
        }
        self.apply(side, -amount, text, now);
        let gain = amount * cfg.enemy_loss_share();
        if gain > 0. {
            self.apply(side.opposite(), gain, text, now);
        }
    }

    /// `captor` took `name` (a `kind`) from `loser`.
    pub fn capture(
        &mut self,
        cfg: &WarWillCfg,
        oid: ObjectiveId,
        kind: &ObjectiveKind,
        name: &str,
        loser: Side,
        captor: Side,
        now: DateTime<Utc>,
    ) {
        if captor == loser || captor == Side::Neutral {
            return;
        }
        // A destroyed factory or command centre was counted when it fell to
        // 0; the wreck changing hands is not a second loss.
        let counted = self.destroyed.contains(&oid);
        self.destroyed.retain(|o| *o != oid);
        self.rt.seen.remove(&oid);
        if loser == Side::Neutral {
            return;
        }
        let loss = if counted { 0. } else { cfg.captured_loss(kind) };
        if loss > 0. {
            self.apply(loser, -loss, &format!("{name} lost"), now);
        }
        let gain =
            if counted { cfg.objective_captured() } else { cfg.capture_gain(loss) };
        if gain > 0. {
            self.apply(captor, gain, &format!("{name} taken"), now);
        }
    }

    /// An objective's status was recomputed: `owner` holds it at `health`.
    pub fn objective_status(
        &mut self,
        cfg: &WarWillCfg,
        oid: ObjectiveId,
        kind: &ObjectiveKind,
        name: &str,
        owner: Side,
        health: u8,
        now: DateTime<Utc>,
    ) {
        let prev = self.rt.seen.insert(oid, (owner, health));
        if health > 0 {
            self.destroyed.retain(|o| *o != oid);
        }
        let Some((prev_owner, prev_health)) = prev else { return };
        if owner == Side::Neutral || prev_owner != owner || prev_health == 0 {
            return;
        }
        match kind {
            ObjectiveKind::CarrierGroup { .. } => {
                let line = cfg.carrier_damaged_pct;
                if health == 0 {
                    self.loss(
                        cfg,
                        owner,
                        cfg.carrier_destroyed(),
                        &format!("{name} sunk"),
                        now,
                    )
                } else if prev_health >= line && health < line {
                    self.loss(
                        cfg,
                        owner,
                        cfg.carrier_damaged(),
                        &format!("{name} hit"),
                        now,
                    )
                }
            }
            ObjectiveKind::Factory { .. } | ObjectiveKind::CommandCenter
                if health == 0 && !self.destroyed.contains(&oid) =>
            {
                self.destroyed.push(oid);
                let amount = if kind.is_factory() {
                    cfg.factory_lost()
                } else {
                    cfg.command_center_lost()
                };
                self.loss(cfg, owner, amount, &format!("{name} destroyed"), now)
            }
            _ => (),
        }
    }

    /// `side` lost a unit of type `typ`. Aircraft and ships count; ground
    /// units count through their formation.
    pub fn unit_lost(
        &mut self,
        cfg: &WarWillCfg,
        side: Side,
        typ: &str,
        tags: UnitTags,
        ship: bool,
        now: DateTime<Utc>,
    ) {
        if tags.contains(UnitTag::Aircraft) || tags.contains(UnitTag::Helicopter) {
            let amount = cfg.aircraft_loss(typ, tags);
            self.loss(cfg, side, amount, &format!("{typ} lost"), now)
        } else if ship || tags.contains(UnitTag::Boat) {
            self.loss(cfg, side, cfg.naval_unit_lost(), &format!("{typ} sunk"), now)
        }
    }

    /// The slow tick: recovery, the fuel drain (`fuel` is each side's stock
    /// in percent of capacity, Blue then Red; None when no fuel is tracked),
    /// the threshold clock and the trend history.
    pub fn tick(
        &mut self,
        cfg: &WarWillCfg,
        fuel: [Option<f64>; 2],
        now: DateTime<Utc>,
    ) -> Vec<WillNote> {
        let dt = match self.last_tick {
            Some(t) => (now - t).clamp(Duration::zero(), MAX_TICK),
            None => Duration::zero(),
        };
        self.last_tick = Some(now);
        let hours = dt.num_milliseconds() as f64 / 3_600_000.;
        let start = cfg.start();
        let quiet = Duration::seconds(cfg.quiet_secs());
        for side in [Side::Blue, Side::Red] {
            let pct = fuel[side_idx(side).unwrap_or(0)];
            let drain = cfg.fuel_drain_per_hour(pct);
            let band = match pct {
                Some(p) if p < cfg.fuel_critical_pct => 2,
                Some(p) if p < cfg.fuel_low_pct => 1,
                _ => 0,
            };
            let Some(s) = self.side_mut(side) else { continue };
            let worse = band > s.fuel_band;
            s.fuel_band = band;
            if drain > 0. {
                let before = s.value;
                s.value = (s.value - drain * hours).clamp(0., 100.);
                self.rt.changed |= s.value != before;
            } else if s.last_loss.is_none_or(|t| now - t >= quiet) && s.value < start {
                let before = s.value;
                s.value = (s.value + cfg.recovery_per_hour() * hours).min(start);
                self.rt.changed |= s.value != before;
            }
            if worse && drain > 0. {
                let line =
                    if band == 2 { cfg.fuel_critical_pct } else { cfg.fuel_low_pct };
                self.push_cause(
                    side,
                    &format!("Fuel stocks under {line:.0}% ({:+.0}/h)", -drain),
                    -drain,
                    now,
                );
                self.rt.changed = true;
            }
        }
        let notes = self.update_threshold(cfg, now);
        if self.samples.last().is_none_or(|(t, _, _)| now - *t >= SAMPLE_EVERY) {
            self.samples.push((now, self.blue.value, self.red.value));
        }
        self.samples.retain(|(t, _, _)| now - *t <= TREND_WINDOW + SAMPLE_EVERY);
        self.causes.retain(|c| now - c.time <= CAUSE_MAX_AGE);
        notes
    }

    /// Start or stop each side's threshold clock.
    fn update_threshold(
        &mut self,
        cfg: &WarWillCfg,
        now: DateTime<Utc>,
    ) -> Vec<WillNote> {
        let threshold = cfg.threshold();
        let mut notes = vec![];
        for side in [Side::Blue, Side::Red] {
            let Some(s) = self.side_mut(side) else { continue };
            if s.value < threshold {
                if s.below_since.is_none() {
                    s.below_since = Some(now);
                    notes.push(WillNote::Breaking(side));
                }
            } else if s.below_since.take().is_some() {
                notes.push(WillNote::Rallied(side));
            }
        }
        notes
    }

    /// Seconds until `side` asks for terms, while it is below the
    /// threshold.
    pub fn terms_in(
        &self,
        cfg: &WarWillCfg,
        side: Side,
        now: DateTime<Utc>,
    ) -> Option<i64> {
        let since = self.side(side)?.below_since?;
        Some((cfg.hold_secs() - (now - since).num_seconds()).max(0))
    }

    /// The side that asks for terms now, if any: below the threshold for
    /// `hold_hours`. Both: the one that broke first.
    pub fn terms(&self, cfg: &WarWillCfg, now: DateTime<Utc>) -> Option<Side> {
        let hold = Duration::seconds(cfg.hold_secs());
        [Side::Blue, Side::Red]
            .into_iter()
            .filter_map(|side| {
                let since = self.side(side)?.below_since?;
                (now - since >= hold).then_some((since, side))
            })
            .min_by_key(|(since, _)| *since)
            .map(|(_, side)| side)
    }

    /// `side`'s change over the last 3 h, or over as much of it as is known
    /// if that is at least an hour.
    pub fn trend(&self, side: Side, now: DateTime<Utc>) -> Option<f64> {
        let cur = self.value(side)?;
        let (_, b, r) = self.samples.iter().find(|(t, _, _)| {
            let age = now - *t;
            age <= TREND_WINDOW + SAMPLE_EVERY / 2 && age >= TREND_MIN
        })?;
        Some(cur - if side == Side::Blue { *b } else { *r })
    }

    /// The record bfdb keeps.
    pub fn stat(&self, cfg: &WarWillCfg, now: DateTime<Utc>) -> WarWillStat {
        WarWillStat {
            blue: self.blue.value,
            red: self.red.value,
            blue_trend: self.trend(Side::Blue, now),
            red_trend: self.trend(Side::Red, now),
            threshold: cfg.threshold(),
            causes: self.causes.iter().rev().cloned().collect(),
        }
    }

    /// Whether a `Stat::WarWill` is due: the first since a restart, a side
    /// moved by a point, anything changed and a minute has passed, or the
    /// heartbeat.
    pub fn publish_due(&self, now: DateTime<Utc>) -> bool {
        let Some((t, b, r)) = self.rt.published else { return true };
        let big = (self.blue.value - b).abs() >= 1. || (self.red.value - r).abs() >= 1.;
        big || (self.rt.changed && now - t >= PUBLISH_MIN) || now - t >= PUBLISH_HEARTBEAT
    }

    pub fn published(&mut self, now: DateTime<Utc>) {
        self.rt.published = Some((now, self.blue.value, self.red.value));
        self.rt.changed = false;
    }

    fn push_cause(&mut self, side: Side, text: &str, delta: f64, now: DateTime<Utc>) {
        if let Some(c) = self.causes.iter_mut().find(|c| {
            c.side == side && c.text.as_str() == text && now - c.time < CAUSE_MERGE
        }) {
            c.delta += delta;
            c.time = now;
        } else {
            self.causes.push(WillCause {
                side,
                text: String::from(text),
                delta,
                time: now,
            });
        }
        self.causes.sort_by_key(|c| c.time);
        while self.causes.len() > MAX_CAUSES {
            // the smallest swing goes; the oldest of equals
            let Some(i) = (0..self.causes.len()).min_by(|a, b| {
                let (a, b) = (&self.causes[*a], &self.causes[*b]);
                a.delta.abs().total_cmp(&b.delta.abs()).then(a.time.cmp(&b.time))
            }) else {
                break;
            };
            self.causes.remove(i);
        }
    }
}

impl Db {
    /// The war will config, when it is on.
    fn will_cfg(&self) -> Option<std::sync::Arc<bfprotocols::cfg::Cfg>> {
        let cfg = self.ephemeral.cfg.clone();
        cfg.war_will.as_ref().is_some_and(|w| w.enabled).then_some(cfg)
    }

    /// `side`'s will, 0-100; None with `war_will` off.
    pub(crate) fn will_of(&self, side: Side) -> Option<f64> {
        let cfg = self.will_cfg()?;
        let wc = cfg.war_will.as_ref()?;
        match self.persisted.war_will.as_ref() {
            Some(w) => w.value(side),
            None => (side != Side::Neutral).then(|| wc.start()),
        }
    }

    /// Run `f` on the will state (made on first use), if `war_will` is on.
    fn with_will(&mut self, f: impl FnOnce(&WarWillCfg, &mut WarWill)) {
        let Some(cfg) = self.will_cfg() else { return };
        let Some(wc) = cfg.war_will.as_ref() else { return };
        let w = self.persisted.war_will.get_or_insert_with(|| WarWill::new(wc));
        f(wc, w);
        self.ephemeral.dirty();
    }

    /// `captor` took `oid` from `loser`.
    pub(super) fn will_capture(
        &mut self,
        oid: ObjectiveId,
        loser: Side,
        captor: Side,
        now: DateTime<Utc>,
    ) {
        let Some(obj) = self.persisted.objectives.get(&oid) else { return };
        let (kind, name) = (obj.kind.clone(), obj.name.clone());
        self.with_will(|c, w| w.capture(c, oid, &kind, &name, loser, captor, now));
    }

    /// `oid`'s status was recomputed.
    pub(super) fn will_objective_status(&mut self, oid: ObjectiveId, now: DateTime<Utc>) {
        if self.will_cfg().is_none() {
            return;
        }
        let Some(obj) = self.persisted.objectives.get(&oid) else { return };
        let (kind, name, owner, health) =
            (obj.kind.clone(), obj.name.clone(), obj.owner, obj.health);
        self.with_will(|c, w| {
            w.objective_status(c, oid, &kind, &name, owner, health, now)
        });
    }

    /// The unit `id` died. Called before the death is processed, while
    /// the engine still knows what it was.
    pub(super) fn will_unit_lost(&mut self, id: &DcsOid<ClassUnit>, now: DateTime<Utc>) {
        if self.will_cfg().is_none() {
            return;
        }
        let what = if let Some(slot) = self.ephemeral.get_slot_by_object_id(id) {
            // a player's own aircraft or vehicle
            self.ephemeral.slot_info.get(slot).map(|ifo| {
                let tags = self
                    .ephemeral
                    .cfg
                    .unit_classification
                    .get(&ifo.typ)
                    .copied()
                    .unwrap_or_default();
                (ifo.side, ifo.typ.0.clone(), tags, false)
            })
        } else if let Some(uid) = self.ephemeral.get_uid_by_object_id(id) {
            self.persisted.units.get(uid).map(|u| {
                let ship = self
                    .persisted
                    .groups
                    .get(&u.group)
                    .is_some_and(|g| g.kind == Some(GroupCategory::Ship));
                (u.side, u.typ.0.clone(), u.tags, ship)
            })
        } else {
            None
        };
        let Some((side, typ, tags, ship)) = what else { return };
        self.with_will(|c, w| w.unit_lost(c, side, &typ, tags, ship, now));
    }

    /// A ground war formation of `side`'s was destroyed.
    pub(super) fn will_formation_destroyed(
        &mut self,
        side: Side,
        name: &str,
        now: DateTime<Utc>,
    ) {
        self.with_will(|c, w| {
            let amount = c.formation_destroyed();
            w.loss(c, side, amount, &format!("{name} destroyed"), now)
        });
    }

    /// A story event (`Cfg::story`) moves `side`'s will by `delta`, noted
    /// as `text`. Returns the change made; None with `war_will` off.
    pub(super) fn will_story(
        &mut self,
        side: Side,
        delta: f64,
        text: &str,
        now: DateTime<Utc>,
    ) -> Option<f64> {
        let mut done = None;
        self.with_will(|_, w| done = Some(w.apply(side, delta, text, now)));
        done
    }

    /// Each side's fuel stock, in percent of capacity, over the warehouses
    /// of the bases it holds. None when it holds no fuel capacity at all
    /// (warehouses off). The HQ's posture reads the same measure
    /// (`fuel_of`).
    pub(super) fn will_fuel(&self) -> [Option<f64>; 2] {
        let mut sums = [(0u64, 0u64); 2];
        for (_, obj) in &self.persisted.objectives {
            let Some(i) = side_idx(obj.owner) else { continue };
            for (_, inv) in &obj.warehouse.liquids {
                sums[i].0 += inv.stored as u64;
                sums[i].1 += inv.capacity as u64;
            }
        }
        sums.map(|(stored, cap)| {
            (cap > 0).then(|| (stored as f64 / cap as f64 * 100.).min(100.))
        })
    }

    /// `side`'s fuel stock in percent of capacity (`will_fuel`), whether or
    /// not `war_will` is on: what the HQ's posture holds on (E7).
    pub(crate) fn fuel_of(&self, side: Side) -> Option<f64> {
        self.will_fuel()[side_idx(side)?]
    }

    /// The slow tick: recovery, fuel, the threshold warnings and countdown,
    /// and the stat for bfdb.
    pub(crate) fn will_tick(&mut self, now: DateTime<Utc>) {
        let Some(cfg) = self.will_cfg() else { return };
        let Some(wc) = cfg.war_will.as_ref() else { return };
        let fuel = self.will_fuel();
        let w = self.persisted.war_will.get_or_insert_with(|| WarWill::new(wc));
        let notes = w.tick(wc, fuel, now);
        let threshold = wc.threshold();
        for note in notes {
            match note {
                WillNote::Breaking(side) => {
                    // the countdown below makes the first announcement
                    w.rt.countdown_last[side_idx(side).unwrap_or(0)] = None;
                }
                WillNote::Rallied(side) => {
                    w.rt.countdown_last[side_idx(side).unwrap_or(0)] = None;
                    let v = w.value(side).unwrap_or(0.);
                    let msgs = self.ephemeral.msgs();
                    msgs.panel_to_side(
                        15,
                        false,
                        side,
                        format_compact!(
                            "WILL TO FIGHT {v:.0}/100: back above {threshold:.0}. We fight on."
                        ),
                    );
                    msgs.panel_to_side(
                        15,
                        false,
                        side.opposite(),
                        format_compact!(
                            "The enemy's will to fight has recovered to {v:.0}/100. They will not ask for terms yet."
                        ),
                    );
                }
            }
        }
        for side in [Side::Blue, Side::Red] {
            let Some(remaining) = w.terms_in(wc, side, now) else { continue };
            let i = side_idx(side).unwrap_or(0);
            let prev = w.rt.countdown_last[i].replace(remaining);
            if remaining > 0 && countdown_announce_due(prev, remaining) {
                let v = w.value(side).unwrap_or(0.);
                let left = fmt_countdown(remaining);
                let msgs = self.ephemeral.msgs();
                msgs.panel_to_side(
                    15,
                    prev.is_none(),
                    side,
                    format_compact!(
                        "WILL TO FIGHT {v:.0}/100, below {threshold:.0}: we ask for terms in {left} unless it recovers."
                    ),
                );
                msgs.panel_to_side(
                    15,
                    false,
                    side.opposite(),
                    format_compact!(
                        "The enemy's will to fight is breaking ({v:.0}/100). They ask for terms in {left} unless it recovers."
                    ),
                );
            }
        }
        if w.publish_due(now) {
            let stat = w.stat(wc, now);
            w.published(now);
            self.ephemeral.stat(Stat::WarWill(stat));
        }
        self.ephemeral.dirty();
    }

    /// The side that wins because the other asks for terms now, announced.
    pub(super) fn will_terms(&mut self, now: DateTime<Utc>) -> Option<Side> {
        let cfg = self.will_cfg()?;
        let wc = cfg.war_will.as_ref()?;
        let loser = self.persisted.war_will.as_ref()?.terms(wc, now)?;
        let winner = loser.opposite();
        let msg = format_compact!(
            "{loser:?} asks for terms: its will to fight has broken. {winner:?} wins the war."
        );
        info!("war will: {loser:?} asks for terms, {winner:?} wins");
        self.ephemeral.msgs().send(crate::msgq::MsgTyp::Chat(None), msg.clone());
        self.ephemeral.msgs().panel_to_all(30, true, msg);
        Some(winner)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn t0() -> DateTime<Utc> {
        DateTime::parse_from_rfc3339("2026-10-31T12:00:00Z").unwrap().with_timezone(&Utc)
    }

    fn h(n: f64) -> Duration {
        Duration::milliseconds((n * 3_600_000.) as i64)
    }

    fn cfg() -> WarWillCfg {
        WarWillCfg::default()
    }

    fn near(a: f64, b: f64) -> bool {
        (a - b).abs() < 1e-6
    }

    fn oid(n: i64) -> ObjectiveId {
        ObjectiveId::from(n)
    }

    #[test]
    fn starts_at_start() {
        let w = WarWill::new(&cfg());
        assert_eq!(w.value(Side::Blue), Some(70.));
        assert_eq!(w.value(Side::Red), Some(70.));
        assert_eq!(w.value(Side::Neutral), None);
    }

    #[test]
    fn losses_cost_the_side_and_pay_the_enemy_half() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        w.loss(&c, Side::Red, 5., "Tuapse refinery destroyed", t0());
        assert!(near(w.red.value, 65.));
        assert!(near(w.blue.value, 72.5));
        assert_eq!(w.red.last_loss, Some(t0()));
        assert_eq!(w.blue.last_loss, None);
        let s = w.stat(&c, t0());
        assert_eq!(s.causes.len(), 2);
        assert!(s.causes.iter().any(|c| c.side == Side::Red && near(c.delta, -5.)));
        assert!(s.causes.iter().any(|c| c.side == Side::Blue && near(c.delta, 2.5)));
    }

    #[test]
    fn clamped_to_0_100() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        for _ in 0..10 {
            w.loss(&c, Side::Blue, 25., "carrier sunk", t0());
        }
        assert_eq!(w.blue.value, 0.);
        assert_eq!(w.red.value, 100.);
        // a clamped change adds no cause
        let before = w.causes.len();
        assert_eq!(w.apply(Side::Blue, -3., "more", t0()), 0.);
        assert_eq!(w.causes.len(), before);
        assert_eq!(w.apply(Side::Blue, f64::NAN, "nan", t0()), 0.);
    }

    #[test]
    fn captures_by_kind() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        w.capture(
            &c,
            oid(1),
            &ObjectiveKind::Airbase,
            "Senaki",
            Side::Red,
            Side::Blue,
            t0(),
        );
        assert!(near(w.red.value, 66.) && near(w.blue.value, 72.));
        let mut w = WarWill::new(&c);
        w.capture(
            &c,
            oid(2),
            &ObjectiveKind::CommandCenter,
            "HQ",
            Side::Red,
            Side::Blue,
            t0(),
        );
        assert!(near(w.red.value, 60.) && near(w.blue.value, 75.));
        // from neutral: no loser, no gain
        let mut w = WarWill::new(&c);
        w.capture(
            &c,
            oid(3),
            &ObjectiveKind::Fob,
            "FOB",
            Side::Neutral,
            Side::Blue,
            t0(),
        );
        assert!(near(w.blue.value, 70.) && near(w.red.value, 70.));
    }

    #[test]
    fn carrier_hit_then_sunk() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        let cg = ObjectiveKind::CarrierGroup {
            carrier_template: "CVN".into(),
            waypoint: None,
            parent_naval_base: None,
            repair_start_time: None,
        };
        let st = |w: &mut WarWill, hp| {
            w.objective_status(&c, oid(9), &cg, "CVBG", Side::Blue, hp, t0())
        };
        // the first look only records
        st(&mut w, 60);
        assert!(near(w.blue.value, 70.));
        st(&mut w, 100);
        st(&mut w, 80);
        assert!(near(w.blue.value, 70.));
        st(&mut w, 70);
        assert!(near(w.blue.value, 62.) && near(w.red.value, 74.));
        // still under the line: not again
        st(&mut w, 50);
        assert!(near(w.blue.value, 62.));
        st(&mut w, 0);
        assert!(near(w.blue.value, 37.) && near(w.red.value, 86.5));
        st(&mut w, 0);
        assert!(near(w.blue.value, 37.));
        // repaired above the line and hit again: counts again
        st(&mut w, 90);
        st(&mut w, 60);
        assert!(near(w.blue.value, 29.));
    }

    #[test]
    fn destroyed_factory_is_not_lost_twice() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        let f = ObjectiveKind::Factory { production_rate: 0, last_production_ts: None };
        w.objective_status(&c, oid(4), &f, "Tuapse refinery", Side::Red, 100, t0());
        w.objective_status(&c, oid(4), &f, "Tuapse refinery", Side::Red, 0, t0());
        assert!(near(w.red.value, 65.) && near(w.blue.value, 72.5));
        w.capture(&c, oid(4), &f, "Tuapse refinery", Side::Red, Side::Blue, t0());
        assert!(near(w.red.value, 65.));
        assert!(near(w.blue.value, 74.5));
        // a change of owner is not read as damage
        w.objective_status(&c, oid(4), &f, "Tuapse refinery", Side::Blue, 0, t0());
        w.objective_status(&c, oid(4), &f, "Tuapse refinery", Side::Blue, 0, t0());
        assert!(near(w.blue.value, 74.5));
    }

    #[test]
    fn units() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        let fighter = UnitTags::from(vec![UnitTag::Aircraft]);
        w.unit_lost(&c, Side::Blue, "F-16C_50", fighter, false, t0());
        assert!(near(w.blue.value, 69.7) && near(w.red.value, 70.15));
        w.unit_lost(&c, Side::Red, "Tu-22M3", fighter, false, t0());
        assert!(near(w.red.value, 69.15));
        w.unit_lost(&c, Side::Red, "MOSCOW", UnitTags::default(), true, t0());
        assert!(near(w.red.value, 68.15));
        // tanks count through their formation, not one by one
        let tank = UnitTags::from(vec![UnitTag::Armor]);
        w.unit_lost(&c, Side::Red, "T-72B", tank, false, t0());
        assert!(near(w.red.value, 68.15));
    }

    #[test]
    fn recovery_waits_for_quiet_and_stops_at_start() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        w.tick(&c, [None, None], t0());
        w.loss(&c, Side::Blue, 10., "x", t0());
        w.tick(&c, [None, None], t0() + h(0.5));
        assert!(near(w.blue.value, 60.), "{}", w.blue.value);
        w.tick(&c, [None, None], t0() + h(1.));
        // the half hour since the last tick, now that it is quiet
        assert!(near(w.blue.value, 60.5), "{}", w.blue.value);
        w.tick(&c, [None, None], t0() + h(2.));
        assert!(near(w.blue.value, 61.5), "{}", w.blue.value);
        for i in 3..30 {
            w.tick(&c, [None, None], t0() + h(i as f64));
        }
        assert!(near(w.blue.value, 70.));
        // above start (from enemy losses) it does not decay
        assert!(near(w.red.value, 75.));
    }

    #[test]
    fn fuel_drains_and_blocks_recovery() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        w.tick(&c, [Some(50.), Some(25.)], t0());
        assert_eq!(w.red.fuel_band, 1);
        w.tick(&c, [Some(50.), Some(25.)], t0() + h(1.));
        assert!(near(w.red.value, 69.) && near(w.blue.value, 70.));
        w.tick(&c, [Some(50.), Some(5.)], t0() + h(2.));
        assert!(near(w.red.value, 66.), "{}", w.red.value);
        assert_eq!(w.red.fuel_band, 2);
        let fuel: Vec<_> =
            w.causes.iter().filter(|c| c.text.starts_with("Fuel")).collect();
        assert_eq!(fuel.len(), 2);
        // a long gap counts as an hour at most
        w.tick(&c, [Some(50.), Some(5.)], t0() + h(12.));
        assert!(near(w.red.value, 63.), "{}", w.red.value);
    }

    #[test]
    fn threshold_held_for_hold_hours_asks_for_terms() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        assert_eq!(w.tick(&c, [None, None], t0()), vec![]);
        w.apply(Side::Red, -60., "collapse", t0());
        let notes = w.tick(&c, [None, None], t0());
        assert_eq!(notes, vec![WillNote::Breaking(Side::Red)]);
        assert_eq!(w.terms(&c, t0() + h(5.9)), None);
        assert_eq!(w.terms_in(&c, Side::Red, t0() + h(5.)), Some(3600));
        // recovery is too slow to save it: +1/h from 10 is 15 at 6 h
        for i in 1..=6 {
            w.tick(&c, [None, None], t0() + h(i as f64));
        }
        assert!(w.red.value < 20.);
        assert_eq!(w.terms(&c, t0() + h(6.)), Some(Side::Red));
        assert_eq!(w.terms(&c, t0() + h(6.)).map(|s| s.opposite()), Some(Side::Blue));
    }

    #[test]
    fn rallying_resets_the_clock() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        w.apply(Side::Blue, -55., "x", t0());
        w.tick(&c, [None, None], t0());
        w.apply(Side::Blue, 10., "y", t0() + h(3.));
        let notes = w.tick(&c, [None, None], t0() + h(3.));
        assert_eq!(notes, vec![WillNote::Rallied(Side::Blue)]);
        assert_eq!(w.terms(&c, t0() + h(7.)), None);
        assert_eq!(w.terms_in(&c, Side::Blue, t0() + h(7.)), None);
    }

    #[test]
    fn trend_over_three_hours() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        assert_eq!(w.trend(Side::Blue, t0()), None);
        let mut t = t0();
        while t <= t0() + h(4.) {
            w.tick(&c, [None, Some(20.)], t);
            t += Duration::minutes(10);
        }
        let now = t0() + h(4.);
        // red drains 1/h
        let tr = w.trend(Side::Red, now).unwrap();
        assert!(near(tr, -3.), "{tr}");
        assert!(near(w.trend(Side::Blue, now).unwrap(), 0.));
        // the record bfdb gets
        let s = w.stat(&c, now);
        assert_eq!(s.threshold, 20.);
        assert!(near(s.red_trend.unwrap(), -3.));
    }

    #[test]
    fn causes_merge_and_keep_the_biggest() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        w.loss(&c, Side::Blue, 0.3, "F-16C_50 lost", t0());
        w.loss(&c, Side::Blue, 0.3, "F-16C_50 lost", t0() + Duration::minutes(5));
        let f16: Vec<_> = w
            .causes
            .iter()
            .filter(|c| c.side == Side::Blue && c.text.as_str() == "F-16C_50 lost")
            .collect();
        assert_eq!(f16.len(), 1);
        assert!(near(f16[0].delta, -0.6));
        for i in 0..20 {
            w.loss(&c, Side::Red, 0.1 * (i + 1) as f64, &format!("loss {i}"), t0());
        }
        assert_eq!(w.causes.len(), MAX_CAUSES);
        assert!(w.causes.iter().any(|c| c.text.as_str() == "loss 19"));
        assert!(!w.causes.iter().any(|c| c.text.as_str() == "F-16C_50 lost"));
        // newest first in the stat
        let s = w.stat(&c, t0());
        assert!(s.causes.windows(2).all(|p| p[0].time >= p[1].time));
    }

    #[test]
    fn publish_rate_limit() {
        let c = cfg();
        let mut w = WarWill::new(&c);
        assert!(w.publish_due(t0()));
        w.published(t0());
        assert!(!w.publish_due(t0() + Duration::seconds(10)));
        w.loss(&c, Side::Blue, 0.3, "a", t0());
        assert!(!w.publish_due(t0() + Duration::seconds(10)));
        assert!(w.publish_due(t0() + Duration::seconds(61)));
        w.loss(&c, Side::Blue, 1.5, "b", t0());
        assert!(w.publish_due(t0() + Duration::seconds(11)));
        w.published(t0() + Duration::seconds(11));
        assert!(!w.publish_due(t0() + Duration::minutes(10)));
        assert!(w.publish_due(t0() + Duration::minutes(16)));
    }

    #[test]
    fn old_saves_and_configs() {
        // a save from before war will: no field, no state
        let mut json =
            serde_json::to_value(super::super::persisted::Persisted::default()).unwrap();
        json.as_object_mut().unwrap().remove("war_will");
        let p: super::super::persisted::Persisted = serde_json::from_value(json).unwrap();
        assert!(p.war_will.is_none());
        // a save from this build, minus the optional fields
        let w: WarWill =
            serde_json::from_str(r#"{"blue":{"value":40},"red":{"value":55}}"#).unwrap();
        assert_eq!((w.blue.value, w.red.value), (40., 55.));
        assert!(w.samples.is_empty() && w.causes.is_empty() && w.last_tick.is_none());
        // round trip
        let mut w = WarWill::new(&cfg());
        w.loss(&cfg(), Side::Red, 3., "x", t0());
        w.tick(&cfg(), [None, None], t0());
        let back: WarWill =
            serde_json::from_str(&serde_json::to_string(&w).unwrap()).unwrap();
        assert_eq!(back.red.value, w.red.value);
        assert_eq!(back.causes, w.causes);
        assert_eq!(back.samples.len(), 1);
    }
}
