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

//! Player-flown reconnaissance ("Recon Pass").
//!
//! A player in a `UnitTag::Recon` aircraft starts a timed pass from the F10
//! menu while within range of an enemy objective. Holding station for the
//! configured dwell time scans every enemy unit around that objective that the
//! aircraft has terrain line-of-sight to and feeds the detections into the
//! ELINT/SIGINT [`IntelDatabase`](super::intel::IntelDatabase), which renders
//! the decaying F10 map contacts and the "Ground Intel" radio picture.

use super::{intel::{IntelSource, IntelUnitClass}, Db};
use crate::landcache::LandCache;
use anyhow::{Context as ErrContext, Result};
use bfprotocols::{
    cfg::{ElintConfig, PlayerReconCfg, UnitTag, Vehicle},
    db::objective::ObjectiveId,
};
use chrono::prelude::*;
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, land::Land, net::Ucid, MizLua, Vector2, Vector3};

/// An in-progress player recon pass.
#[derive(Debug, Clone)]
pub struct ReconSession {
    /// Objective being reconnoitred.
    pub target: ObjectiveId,
    /// Objective centre (cached so it survives the objective moving/flipping).
    pub target_pos: Vector2,
    /// Coalition of the reconning player.
    pub side: Side,
    /// Seconds of valid station time accumulated so far.
    pub accumulated_secs: f64,
    pub last_tick: DateTime<Utc>,
    /// Highest dwell fraction at which contacts have already been revealed
    /// (drives the progressive 25/50/75/100% reveals).
    pub last_reveal_frac: f32,
}

impl Db {
    /// Is this aircraft type cleared to run a recon pass?
    pub fn recon_capable(&self, typ: &Vehicle) -> bool {
        self.ephemeral.cfg.player_recon.is_some()
            && self
                .ephemeral
                .cfg
                .unit_classification
                .get(typ)
                .map_or(false, |tags| tags.0.contains(UnitTag::Recon))
    }

    fn player_recon_cfg(&self) -> Option<PlayerReconCfg> {
        self.ephemeral.cfg.player_recon.clone()
    }

    fn elint_cfg(&self) -> ElintConfig {
        self.ephemeral
            .cfg
            .elint
            .clone()
            .unwrap_or_else(ElintConfig::default)
    }

    /// The shared intel database (ELINT/SIGINT, player recon, JTAC eyes-on)
    /// only decays and renders marks when at least one of its feeders is
    /// configured -- see `Ephemeral::tick_intel_decay`.
    fn intel_active(&self) -> bool {
        self.ephemeral.cfg.elint.is_some() || self.ephemeral.cfg.player_recon.is_some()
    }

    /// Feed a unit a JTAC currently has (or just had) eyes-on into the intel
    /// database as `IntelSource::Jtac`. Called every tick a JTAC still sees
    /// the unit, so confidence stays pinned at 1.0 while tracked and only
    /// starts decaying -- over a long, separately configured half-life --
    /// once the JTAC loses it. No-op unless the intel system is active.
    pub fn note_jtac_contact(&mut self, friendly_side: Side, unit: &super::group::SpawnedUnit, now: DateTime<Utc>) {
        if !self.intel_active() {
            return;
        }
        let class = self
            .ephemeral
            .cfg
            .unit_classification
            .get(&unit.typ)
            .map(|tags| IntelUnitClass::from_tags(*tags))
            .unwrap_or(IntelUnitClass::Unknown);
        let elint_cfg = self.elint_cfg();
        self.ephemeral.intel_db.upsert(
            friendly_side,
            unit.side,
            unit.pos,
            class,
            1,
            IntelSource::Jtac,
            &elint_cfg,
            now,
        );
    }

    /// Scan enemy units within `scan_radius_m` of `target_pos` and insert
    /// clustered detections into the intel database as `source`. When `los` is
    /// `Some((land, landcache, observer_pos))` only units with terrain
    /// line-of-sight from `observer_pos` are counted. Returns the number of
    /// individual units detected.
    pub fn apply_recon_scan(
        &mut self,
        target_pos: Vector2,
        scan_radius_m: f64,
        side: Side,
        source: IntelSource,
        los: Option<(&Land, &mut LandCache, Vector3)>,
        draw_area: bool,
        now: DateTime<Utc>,
    ) -> usize {
        let enemy_side = side.opposite();
        let scan_sq = scan_radius_m.powi(2);
        // (2d pos, 3d pos, class) for every candidate enemy unit in range.
        let mut detected: Vec<(Vector2, Vector3, IntelUnitClass)> = self
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, obj)| obj.owner == enemy_side)
            .flat_map(|(_, obj)| {
                obj.groups
                    .get(&enemy_side)
                    .into_iter()
                    .flat_map(|gs| gs.into_iter())
            })
            .filter_map(|gid| self.persisted.groups.get(gid))
            .flat_map(|g| g.units.into_iter())
            .filter_map(|uid| self.persisted.units.get(uid))
            .filter(|u| {
                !u.dead && na::distance_squared(&target_pos.into(), &u.pos.into()) <= scan_sq
            })
            .map(|u| {
                let class = self
                    .ephemeral
                    .cfg
                    .unit_classification
                    .get(&u.typ)
                    .map(|tags| IntelUnitClass::from_tags(*tags))
                    .unwrap_or(IntelUnitClass::Unknown);
                (u.pos, u.position.p.0, class)
            })
            .collect();

        if let Some((land, landcache, observer)) = los {
            detected.retain(|(_, unit3, _)| {
                let d = na::distance(&observer.into(), &(*unit3).into());
                landcache
                    .is_visible(land, d, observer, *unit3)
                    .unwrap_or(false)
            });
        }

        let count = detected.len();
        let elint_cfg = self.elint_cfg();
        let cluster_sq = elint_cfg.contact_cluster_radius_m.powi(2);
        // Cluster by class, then spatially.
        let mut clusters: Vec<(Vector2, IntelUnitClass, u8)> = Vec::new();
        for (pos, _, class) in &detected {
            let existing = clusters.iter_mut().find(|(cpos, cclass, _)| {
                *cclass == *class
                    && na::distance_squared(&(*cpos).into(), &(*pos).into()) <= cluster_sq
            });
            if let Some((cpos, _, n)) = existing {
                cpos.x = cpos.x * 0.7 + pos.x * 0.3;
                cpos.y = cpos.y * 0.7 + pos.y * 0.3;
                *n = n.saturating_add(1);
            } else {
                clusters.push((*pos, *class, 1));
            }
        }
        for (pos, class, n) in clusters {
            self.ephemeral.intel_db.upsert(
                side, enemy_side, pos, class, n, source, &elint_cfg, now,
            );
        }

        // Dotted scan-area box + count label on the F10 map.
        if draw_area {
            self.ephemeral
                .on_recon_result(target_pos, scan_radius_m, count, side, now);
        }
        count
    }

    /// Begin a recon pass for `ucid`. Returns the player-facing status message.
    pub fn recon_start(&mut self, ucid: &Ucid, now: DateTime<Utc>) -> CompactString {
        let cfg = match self.player_recon_cfg() {
            Some(c) => c,
            None => return "Recon is not enabled on this server".into(),
        };
        if self.ephemeral.recon_sessions.contains_key(ucid) {
            return "Recon pass already in progress -- cancel it first".into();
        }
        if let Some(until) = self.ephemeral.recon_cooldown.get(ucid).copied() {
            if now < until {
                let secs = (until - now).num_seconds().max(1);
                return format_compact!("Recon on cooldown for {secs}s");
            }
        }
        let player = match self.player(ucid) {
            Some(p) => p,
            None => return "You are not registered".into(),
        };
        let side = player.side;
        let points = player.points;
        let inst = match player.current_slot.as_ref().and_then(|(_, i)| i.as_ref()) {
            Some(i) => i,
            None => return "You must be in an aircraft to run a recon pass".into(),
        };
        if !self.recon_capable(&inst.typ) {
            return "This airframe is not equipped for reconnaissance".into();
        }
        let ac_pos = Vector2::new(inst.position.p.x, inst.position.p.z);
        if cfg.cost > 0 && points < cfg.cost as i32 {
            return format_compact!("Recon pass costs {} points (you have {points})", cfg.cost);
        }
        let (dist, oid, oname) = match Db::objective_near_point(
            &self.persisted.objectives,
            ac_pos,
            |o| o.owner != side && o.owner != Side::Neutral,
        ) {
            Some((dist, _, obj)) => (dist, obj.id, obj.name().to_string()),
            None => return "No enemy objective nearby".into(),
        };
        if dist > cfg.range_m {
            return format_compact!(
                "Nearest enemy objective ({oname}) is {:.0}km away -- must be within {:.0}km",
                dist / 1000.0,
                cfg.range_m / 1000.0
            );
        }
        let target_pos = self
            .persisted
            .objectives
            .get(&oid)
            .map(|o| o.pos())
            .unwrap_or(ac_pos);
        if cfg.cost > 0 {
            self.adjust_points(ucid, -(cfg.cost as i32), "recon pass");
        }
        self.ephemeral.recon_sessions.insert(
            *ucid,
            ReconSession {
                target: oid,
                target_pos,
                side,
                accumulated_secs: 0.0,
                last_tick: now,
                last_reveal_frac: 0.0,
            },
        );
        format_compact!(
            "Recon pass started on {oname} -- hold within {:.0}km for {}s",
            cfg.range_m / 1000.0,
            cfg.dwell_secs
        )
    }

    /// Cancel an in-progress pass (player command). Refunds the point cost.
    pub fn recon_cancel(&mut self, ucid: &Ucid, now: DateTime<Utc>) -> CompactString {
        match self.ephemeral.recon_sessions.remove(ucid) {
            None => "No recon pass in progress".into(),
            Some(_) => {
                if let Some(cfg) = self.player_recon_cfg() {
                    if cfg.cost > 0 {
                        self.adjust_points(ucid, cfg.cost as i32, "recon pass cancelled");
                    }
                    self.ephemeral
                        .recon_cooldown
                        .insert(*ucid, now + chrono::Duration::seconds(cfg.cooldown_secs as i64));
                }
                "Recon pass cancelled".into()
            }
        }
    }

    /// Player-facing status line for the Recon menu.
    pub fn recon_status(&self, ucid: &Ucid, now: DateTime<Utc>) -> CompactString {
        let cfg = match self.player_recon_cfg() {
            Some(c) => c,
            None => return "Recon is not enabled on this server".into(),
        };
        if let Some(s) = self.ephemeral.recon_sessions.get(ucid) {
            let oname = self
                .persisted
                .objectives
                .get(&s.target)
                .map(|o| o.name().to_string())
                .unwrap_or_else(|| "target".to_string());
            let pct = ((s.accumulated_secs / cfg.dwell_secs.max(1) as f64) * 100.0).min(100.0);
            return format_compact!("Recon on {oname}: {pct:.0}% ({:.0}/{}s)", s.accumulated_secs, cfg.dwell_secs);
        }
        if let Some(until) = self.ephemeral.recon_cooldown.get(ucid).copied() {
            if now < until {
                return format_compact!("Recon on cooldown for {}s", (until - now).num_seconds().max(1));
            }
        }
        match self.player(ucid).and_then(|p| p.current_slot.as_ref()).and_then(|(_, i)| i.as_ref()) {
            None => "Not in an aircraft".into(),
            Some(inst) if !self.recon_capable(&inst.typ) => {
                "This airframe is not equipped for reconnaissance".into()
            }
            Some(inst) => {
                let ac_pos = Vector2::new(inst.position.p.x, inst.position.p.z);
                let side = self.player(ucid).map(|p| p.side).unwrap_or(Side::Neutral);
                match Db::objective_near_point(&self.persisted.objectives, ac_pos, |o| {
                    o.owner != side && o.owner != Side::Neutral
                }) {
                    Some((dist, _, obj)) if dist <= cfg.range_m => {
                        format_compact!("Ready -- nearest enemy objective {} at {:.0}km", obj.name(), dist / 1000.0)
                    }
                    Some((dist, _, obj)) => format_compact!(
                        "Nearest enemy objective {} is {:.0}km away (need {:.0}km)",
                        obj.name(),
                        dist / 1000.0,
                        cfg.range_m / 1000.0
                    ),
                    None => "No enemy objective nearby".into(),
                }
            }
        }
    }

    /// Per-tick advance of all active recon sessions. Cheap no-op when the
    /// feature is disabled or no sessions are running.
    pub fn tick_recon_sessions(
        &mut self,
        lua: MizLua,
        landcache: &mut LandCache,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let cfg = match self.player_recon_cfg() {
            Some(c) if !self.ephemeral.recon_sessions.is_empty() => c,
            _ => return Ok(()),
        };
        let land = Land::singleton(lua).context("getting land")?;
        let ucids: Vec<Ucid> = self.ephemeral.recon_sessions.keys().copied().collect();
        // Small tolerance so a brief GPS/altitude wobble at the edge of the
        // range ring doesn't abort a pass.
        let range_limit = cfg.range_m * 1.1;
        for ucid in ucids {
            let (target_pos, side, accumulated, last_tick, last_frac) =
                match self.ephemeral.recon_sessions.get(&ucid) {
                    Some(s) => (s.target_pos, s.side, s.accumulated_secs, s.last_tick, s.last_reveal_frac),
                    None => continue,
                };
            // Resolve the live aircraft (copy out everything we need so the
            // shared borrow of `self` is released before the `&mut self` calls).
            let ac_state: Option<(Vector2, Vector3, f64, Vehicle)> = self
                .player(&ucid)
                .filter(|p| p.side == side)
                .and_then(|p| p.current_slot.as_ref())
                .and_then(|(_, i)| i.as_ref())
                .map(|i| {
                    (
                        Vector2::new(i.position.p.x, i.position.p.z),
                        i.position.p.0,
                        i.position.p.y,
                        i.typ.clone(),
                    )
                });
            let (ac_pos, observer3, alt_msl, typ) = match ac_state {
                Some(v) => v,
                None => {
                    self.recon_abort(&ucid, now, &cfg, "Recon aborted -- left the aircraft");
                    continue;
                }
            };
            let capable = self.recon_capable(&typ);
            if !capable {
                self.recon_abort(&ucid, now, &cfg, "Recon aborted -- switched airframe");
                continue;
            }
            let dist = na::distance(&ac_pos.into(), &target_pos.into());
            if dist > range_limit {
                self.recon_abort(&ucid, now, &cfg, "Recon aborted -- out of range of the target");
                continue;
            }
            if cfg.max_altitude_m > 0.0 && alt_msl > cfg.max_altitude_m {
                self.recon_abort(&ucid, now, &cfg, "Recon aborted -- above the sensor altitude ceiling");
                continue;
            }
            // Accumulate valid station time.
            let dt = (now - last_tick).num_milliseconds().max(0) as f64 / 1000.0;
            let new_acc = accumulated + dt;
            let dwell = cfg.dwell_secs.max(1) as f64;
            let frac = (new_acc / dwell) as f32;
            if let Some(s) = self.ephemeral.recon_sessions.get_mut(&ucid) {
                s.accumulated_secs = new_acc;
                s.last_tick = now;
            }
            // Decide whether to reveal now: on completion, or when we've crossed
            // the next 25% boundary (progressive mode only).
            let done = frac >= 1.0;
            let next_boundary = ((last_frac / 0.25f32).floor() + 1.0) * 0.25f32;
            let progressive_hit = cfg.progressive && frac >= next_boundary && next_boundary < 1.0;
            if done || progressive_hit {
                let los = cfg
                    .require_los
                    .then_some((&land, &mut *landcache, observer3));
                let n = self.apply_recon_scan(
                    target_pos,
                    cfg.scan_radius_m,
                    side,
                    IntelSource::ReconFlight,
                    los,
                    done,
                    now,
                );
                if let Some(s) = self.ephemeral.recon_sessions.get_mut(&ucid) {
                    s.last_reveal_frac = frac.min(1.0);
                }
                if done {
                    self.ephemeral.recon_sessions.remove(&ucid);
                    self.ephemeral.recon_cooldown.insert(
                        ucid,
                        now + chrono::Duration::seconds(cfg.cooldown_secs as i64),
                    );
                    self.ephemeral.panel_to_player(
                        &self.persisted,
                        15,
                        &ucid,
                        format_compact!("Recon complete -- {n} contact(s) on the F10 map"),
                    );
                } else {
                    self.ephemeral.panel_to_player(
                        &self.persisted,
                        8,
                        &ucid,
                        format_compact!("Recon {:.0}% -- {n} contact(s)", (frac * 100.0f32).min(100.0f32)),
                    );
                }
            }
        }
        Ok(())
    }

    fn recon_abort(&mut self, ucid: &Ucid, now: DateTime<Utc>, cfg: &PlayerReconCfg, msg: &str) {
        if self.ephemeral.recon_sessions.remove(ucid).is_some() {
            if cfg.cost > 0 {
                self.adjust_points(ucid, cfg.cost as i32, "recon pass aborted");
            }
            self.ephemeral
                .recon_cooldown
                .insert(*ucid, now + chrono::Duration::seconds(cfg.cooldown_secs as i64));
            self.ephemeral
                .panel_to_player(&self.persisted, 10, ucid, msg.to_string());
        }
    }
}
