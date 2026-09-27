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

//! Ground electronic warfare. Objectives of the configured kinds host a
//! jammer truck (`GPS_Spoofer_Blue`/`_Red`, DCS's only `Jammer` units):
//!
//! - It follows the objective: a captured site loses its truck and the new
//!   owner gets its own after `respawn_secs`; a destroyed truck is replaced
//!   the same way, and only while the objective still stands.
//! - It is dark until enemy aircraft come inside `activation_radius_m`,
//!   then jams GPS/GLONASS (or spoofs them, at `spoof_kinds`) and the radio
//!   band, and stays on at least `min_on_secs`.
//! - Emitting gives it away: the enemy gets an intel mark near it.
//! - While it jams radio, enemy flights inside `comms_jam_radius_m` get
//!   broken GCI (see `comms_jammed`, read by `admin::query_gci`).

use super::{enemy_air, land_near};
use crate::{airlife::dist, spawnctx::SpawnCtx, Context};
use bfprotocols::{
    cfg::{EwCfg, GnssMode, RadioJamMode},
    db::{group::GroupId, objective::ObjectiveId},
    perf::PerfInner,
};
use chrono::prelude::*;
use compact_str::format_compact;
use dcso3::{
    coalition::Side,
    controller::{Command, GnssJamming, RadioJamming},
    group::Group,
    land::Land,
    trigger::MarkId,
    MizLua, Vector2,
};
use fxhash::FxHashMap;
use log::{info, warn};
use rand::{thread_rng, Rng};

#[derive(Debug)]
struct Site {
    side: Side,
    name: dcso3::String,
    pos: Vector2,
    gid: Option<GroupId>,
    /// When the last truck died (or the site changed hands); a new one is
    /// not spawned before `respawn_secs` after it.
    down_since: Option<DateTime<Utc>>,
    on_since: Option<DateTime<Utc>>,
    spoof: bool,
    spoof_point: Vector2,
    intel_mark: Option<MarkId>,
}

#[derive(Debug, Default)]
pub(crate) struct Ew {
    sites: FxHashMap<ObjectiveId, Site>,
}

fn gnss(m: GnssMode, spoof_site: bool) -> GnssJamming {
    match m {
        GnssMode::Off => GnssJamming::Off,
        // A spoofing site spoofs whatever the config says to jam.
        GnssMode::Jam if spoof_site => GnssJamming::Spoofing,
        GnssMode::Jam => GnssJamming::Jamming,
        GnssMode::Spoof => GnssJamming::Spoofing,
    }
}

fn radio(m: RadioJamMode) -> RadioJamming {
    match m {
        RadioJamMode::Off => RadioJamming::Off,
        RadioJamMode::Simple => RadioJamming::Simple,
        RadioJamMode::Adaptive => RadioJamming::Adaptive,
    }
}

impl Ew {
    pub(crate) fn comms_jammed(&self, cfg: &EwCfg, victim: Side, pos: Vector2) -> bool {
        cfg.radio != RadioJamMode::Off
            && self.sites.values().any(|s| {
                s.side == victim.opposite()
                    && s.on_since.is_some()
                    && dist(s.pos, pos) <= cfg.comms_jam_radius_m
            })
    }
}

fn set_jammer(lua: MizLua, ctx: &Context, gid: &GroupId, cmd: Command) -> anyhow::Result<()> {
    let name = ctx
        .db
        .persisted
        .groups
        .get(gid)
        .map(|g| g.name.clone())
        .ok_or_else(|| anyhow::anyhow!("jammer group gone"))?;
    Group::get_by_name(lua, name.as_str())?.get_controller()?.set_command(cmd)?;
    Ok(())
}

fn go_dark(lua: MizLua, ctx: &mut Context, oid: ObjectiveId) {
    let Some(site) = ctx.modern_war.ew.sites.get_mut(&oid) else { return };
    site.on_since = None;
    let mark = site.intel_mark.take();
    let gid = site.gid;
    if let Some(m) = mark {
        ctx.db.ephemeral.msgs().delete_mark(m);
    }
    if let Some(gid) = gid {
        let _ = set_jammer(lua, ctx, &gid, Command::DeactivateJammer);
    }
}

pub(crate) fn tick(
    lua: MizLua,
    ctx: &mut Context,
    perf: &mut PerfInner,
    cfg: &EwCfg,
    now: DateTime<Utc>,
) {
    // Which objectives host a site now.
    let hosts: Vec<(ObjectiveId, Side, dcso3::String, Vector2, bool, u8)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| o.owner() != Side::Neutral)
        .filter(|(_, o)| cfg.host_kinds.iter().any(|k| k.as_str() == o.kind().name()))
        .map(|(id, o)| {
            let spoof = cfg.spoof_kinds.iter().any(|k| k.as_str() == o.kind().name());
            (*id, o.owner(), o.name.clone(), o.pos(), spoof, o.health())
        })
        .collect();
    // Sites whose objective no longer qualifies (neutralised) go away.
    let gone: Vec<ObjectiveId> = ctx
        .modern_war
        .ew
        .sites
        .keys()
        .filter(|id| !hosts.iter().any(|h| h.0 == **id))
        .copied()
        .collect();
    for oid in gone {
        go_dark(lua, ctx, oid);
        if let Some(site) = ctx.modern_war.ew.sites.remove(&oid) {
            if let Some(gid) = site.gid {
                let _ = ctx.db.delete_group(&gid);
            }
        }
    }
    let land = Land::singleton(lua).ok();
    let spctx = SpawnCtx::new(lua).ok();
    for (oid, owner, name, pos, spoof, health) in hosts {
        // New site, or one that changed hands: the old owner's truck goes.
        let changed = ctx.modern_war.ew.sites.get(&oid).map(|s| s.side != owner).unwrap_or(false);
        if changed {
            go_dark(lua, ctx, oid);
            if let Some(gid) = ctx.modern_war.ew.sites.get_mut(&oid).and_then(|s| s.gid.take()) {
                let _ = ctx.db.delete_group(&gid);
            }
            ctx.modern_war.ew.sites.remove(&oid);
        }
        if !ctx.modern_war.ew.sites.contains_key(&oid) {
            let a: f64 = thread_rng().gen_range(0.0..std::f64::consts::TAU);
            ctx.modern_war.ew.sites.insert(
                oid,
                Site {
                    side: owner,
                    name: name.clone(),
                    pos,
                    gid: None,
                    down_since: changed.then_some(now),
                    on_since: None,
                    spoof,
                    spoof_point: pos + Vector2::new(a.cos(), a.sin()) * cfg.spoof_offset_m,
                    intel_mark: None,
                },
            );
        }
        // A dead truck: tell the side that killed it, and start the clock
        // on its replacement.
        let dead = ctx.modern_war.ew.sites[&oid]
            .gid
            .map(|g| !crate::airlife::alive(ctx, &g))
            .unwrap_or(false);
        if dead {
            go_dark(lua, ctx, oid);
            let site = ctx.modern_war.ew.sites.get_mut(&oid).expect("present");
            let gid = site.gid.take();
            site.down_since = Some(now);
            if let Some(gid) = gid {
                let _ = ctx.db.delete_group(&gid);
            }
            info!("ew: {owner:?} jammer at {name} destroyed");
            ctx.db.ephemeral.msgs().panel_to_side(
                15,
                false,
                owner.opposite(),
                format_compact!("Enemy jammer at {name} destroyed -- GPS and comms clear there."),
            );
            ctx.db.ephemeral.msgs().panel_to_side(
                15,
                false,
                owner,
                format_compact!("Our jammer at {name} has been destroyed."),
            );
        }
        // Replace a missing truck once the site has been down long enough
        // and the objective is still standing.
        let site = &ctx.modern_war.ew.sites[&oid];
        let due = site
            .down_since
            .map(|t| (now - t).num_seconds() >= cfg.respawn_secs as i64)
            .unwrap_or(true);
        if site.gid.is_none() && due && health > 0 {
            if let (Some(land), Some(spctx)) = (land.as_ref(), spctx.as_ref()) {
                // A naval base's centre is often water.
                match land_near(land, pos + Vector2::new(0., 300.), 3_000.) {
                    None => warn!("ew: no dry land near {name} for a jammer"),
                    Some(at) => match ctx.db.spawn_jammer_truck(perf, spctx, &ctx.idx, owner, at, 0.) {
                        Ok(gid) => {
                            info!("ew: {owner:?} jammer {gid} up at {name}");
                            let site = ctx.modern_war.ew.sites.get_mut(&oid).expect("present");
                            site.gid = Some(gid);
                            site.pos = at;
                            site.down_since = None;
                        }
                        Err(e) => {
                            warn!("ew: jammer at {name} would not spawn: {e:?}");
                            ctx.modern_war.ew.sites.get_mut(&oid).expect("present").down_since =
                                Some(now);
                        }
                    },
                }
            }
        }
        // On or off.
        let site = &ctx.modern_war.ew.sites[&oid];
        let Some(gid) = site.gid else { continue };
        let threat = enemy_air(ctx, owner, now)
            .into_iter()
            .any(|p| dist(p, site.pos) <= cfg.activation_radius_m);
        match (threat, site.on_since) {
            (true, None) => {
                let cmd = Command::ActivateJammer {
                    gps: gnss(cfg.gps, site.spoof),
                    glonass: gnss(cfg.glonass, site.spoof),
                    radio: radio(cfg.radio),
                    band_mhz: Some(cfg.band_mhz),
                    spoof_point: Some(site.spoof_point),
                };
                let (spos, sname, spoofing) = (site.pos, site.name.clone(), site.spoof);
                match set_jammer(lua, ctx, &gid, cmd) {
                    Err(e) => warn!("ew: could not switch on the jammer at {sname}: {e:?}"),
                    Ok(()) => {
                        info!("ew: {owner:?} jammer at {sname} ON");
                        let mark = cfg.intel_marks.then(|| {
                            let a: f64 = thread_rng().gen_range(0.0..std::f64::consts::TAU);
                            let r: f64 = thread_rng().gen_range(0.0..cfg.intel_uncertainty_m.max(1.));
                            let at = spos + Vector2::new(a.cos(), a.sin()) * r;
                            ctx.db.ephemeral.msgs().mark_to_side(
                                owner.opposite(),
                                at,
                                true,
                                if spoofing {
                                    "ELINT: GPS spoofing and comms jamming in this area"
                                } else {
                                    "ELINT: GPS and comms jamming in this area"
                                },
                            )
                        });
                        let site = ctx.modern_war.ew.sites.get_mut(&oid).expect("present");
                        site.on_since = Some(now);
                        site.intel_mark = mark;
                    }
                }
            }
            (false, Some(since)) if (now - since).num_seconds() >= cfg.min_on_secs as i64 => {
                info!("ew: {owner:?} jammer at {} off", site.name);
                go_dark(lua, ctx, oid);
            }
            _ => (),
        }
    }
}
