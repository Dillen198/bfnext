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

use std::sync::Arc;

use super::{Db, ephemeral::SlotInfo, group::DeployKind, objective::ObjGroup};
use crate::{
    bg::Task,
    db::{
        MapS,
        logistics::Warehouse,
        objective::{Objective, Zone},
    },
    group, group_health, group_mut,
    landcache::LandCache,
    objective, objective_mut,
    spawnctx::{Despawn, SpawnCtx, SpawnLoc},
    unit, unit_mut,
};
use anyhow::{Context, Result, anyhow, bail};
use bfprotocols::{
    cfg::{Cfg, SpecialSamSiteCfg, SpecialSamUnitCfg, Vehicle},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::PerfInner,
    stats::Stat,
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{
    centroid2d, coalition::Side, controller::PointType, coord::Coord,
    country::Country,
    env::miz::{Group, GroupKind, Miz, MizIndex, Skill, TriggerZone, TriggerZoneTyp},
    land::Land, net::Net, trigger::Trigger, LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3
};
use enumflags2::BitFlags;
use fxhash::{FxHashMap, FxHashSet};
use dcso3::land::SurfaceType;
use log::{debug, error, info, warn};
use smallvec::SmallVec;
use tokio::sync::mpsc::UnboundedSender;

impl Db {
    /// objectives are just trigger zones named according to type codes
    /// the first caracter is the type of the zone
    /// O - Objective
    /// G - Group within an objective
    /// T - Generic trigger zone, ignored by the engine
    ///
    /// Then a 2 character type code
    /// - AB: Airbase
    /// - FO: Fob
    /// - SA: Sam site
    /// - LO: Logistics Objective
    /// - CC: Command Center (IADN network node -- see mizinit's SAM-site
    ///   auto-linking pass)
    ///
    /// Then a 1 character code for the default owner
    /// followed by the display name
    /// - R: Red
    /// - B: Blue
    /// - N: Neutral
    ///
    /// So e.g. Tblisi would be OABBTBLISI -> Objective, Airbase, Default to Blue, named Tblisi
    fn init_objective(&mut self, lua: MizLua, zone: TriggerZone, name: &str) -> Result<()> {
        fn side_and_name(s: &str) -> Result<(Side, String)> {
            if let Some(name) = s.strip_prefix("R") {
                Ok((Side::Red, String::from(name)))
            } else if let Some(name) = s.strip_prefix("B") {
                Ok((Side::Blue, String::from(name)))
            } else if let Some(name) = s.strip_prefix("N") {
                Ok((Side::Neutral, String::from(name)))
            } else {
                bail!("invalid default coalition {s} expected B, R, or N prefix")
            }
        }
        let (kind, owner, name) = if let Some(name) = name.strip_prefix("AB") {
            let (side, name) = side_and_name(name)?;
            (ObjectiveKind::Airbase, side, name)
        } else if let Some(name) = name.strip_prefix("FO") {
            let (side, name) = side_and_name(name)?;
            (ObjectiveKind::Fob, side, name)
        } else if let Some(name) = name.strip_prefix("LO") {
            let (side, name) = side_and_name(name)?;
            (ObjectiveKind::Logistics, side, name)
        } else if let Some(name) = name.strip_prefix("NB") {
            let (side, name) = side_and_name(name)?;
            (ObjectiveKind::NavalBase, side, name)
        } else if let Some(name) = name.strip_prefix("CG") {
            info!("[CARRIER_OBJ_INIT] Parsing carrier group objective, remaining name: {}", name);
            let (side, name) = side_and_name(name)?;
            info!("[CARRIER_OBJ_INIT] Side: {:?}, Name: {}", side, name);
            (ObjectiveKind::CarrierGroup {
                carrier_template: String::default(),
                waypoint: None,
                parent_naval_base: None,
                repair_start_time: None,
            }, side, name)
        } else if let Some(name) = name.strip_prefix("FAC") {
            let (side, name) = side_and_name(name)?;
            let production_rate = self.ephemeral.cfg.factory
                .as_ref()
                .map(|f| f.production_rate)
                .unwrap_or(100);
            (ObjectiveKind::Factory {
                production_rate,
                last_production_ts: None,
            }, side, name)
        } else if let Some(name) = name.strip_prefix("CC") {
            let (side, name) = side_and_name(name)?;
            (ObjectiveKind::CommandCenter, side, name)
        } else {
            bail!("invalid objective type for {name}, expected AB, FO, LO, NB, CG, FAC, or CC")
        };
        let id = ObjectiveId::new();
        let mut logistics_detached = false;
        let mut unlimited_supply = false;
        let mut unlimited_aircraft = false;
        for pr in zone.properties()? {
            let pr = pr?;
            if &*pr.key == "LOGISTICS_DETACHED" {
                let v = pr.value.to_ascii_lowercase();
                if &*v == "true" {
                    logistics_detached = true;
                } else if &*v == "false" {
                    logistics_detached = false;
                } else {
                    bail!("invalid value of LOGISTICS_DETACHED {v}")
                }
            } else if &*pr.key == "UNLIMITED_SUPPLY" {
                let v = pr.value.to_ascii_lowercase();
                if &*v == "true" {
                    unlimited_supply = true;
                } else if &*v == "false" {
                    unlimited_supply = false;
                } else {
                    bail!("invalid value of UNLIMITED_SUPPLY {v}")
                }
            } else if &*pr.key == "UNLIMITED_AIRCRAFTS" {
                let v = pr.value.to_ascii_lowercase();
                if &*v == "true" {
                    unlimited_aircraft = true;
                } else if &*v == "false" {
                    unlimited_aircraft = false;
                } else {
                    bail!("invalid value of UNLIMITED_AIRCRAFTS {v}")
                }
            } else {
                bail!("invalid objective property {pr:?}")
            }
        }
        let zone = match zone.typ()? {
            TriggerZoneTyp::Quad(points) => Zone::Quad {
                pos: centroid2d([points.p0.0, points.p1.0, points.p2.0, points.p3.0]),
                points,
            },
            TriggerZoneTyp::Circle { radius } => Zone::Circle {
                pos: zone.pos()?,
                radius,
            },
        };
        let obj = Objective {
            id,
            spawned: false,
            enabled: false,
            threatened: false,
            zone,
            name: name.clone(),
            kind,
            owner,
            groups: MapS::new(),
            health: 0,
            logi: 0,
            infantry: 0,
            supply: 0,
            fuel: 0,
            last_change_ts: Utc::now(),
            last_threatened_ts: Utc::now(),
            warehouse: Warehouse::default(),
            points: 0,
            capture_hold: vec![],
            capture_hold_ts: None,
            logistics_detached,
            unlimited_supply,
            unlimited_aircraft,
            priority: false,
            last_activate: DateTime::<Utc>::default(),
            // initialized by load
            threat_pos3: Vector3::default(),
        };
        match obj.kind {
            ObjectiveKind::Logistics => {
                self.persisted.logistics_hubs.insert_cow(id);
            }
            ObjectiveKind::NavalBase => {
                self.persisted.naval_bases.insert_cow(id);
            }
            ObjectiveKind::CarrierGroup { .. } => {
                info!("[CARRIER_OBJ] Created carrier group objective: {} (id: {:?}, owner: {:?})", name, id, owner);
                self.persisted.carrier_groups.insert_cow(id);
            }
            ObjectiveKind::Factory { .. } => {
                self.persisted.factories.insert_cow(id);
            }
            ObjectiveKind::CommandCenter => {
                self.persisted.command_centers.insert_cow(id);
            }
            _ => {}
        }
        let pos = zone.pos();
        let llpos = Coord::singleton(lua)?.lo_to_ll(LuaVec3(Vector3::new(pos.x, 0., pos.y)))?;
        self.ephemeral.stat(Stat::Objective {
            name: name.clone(),
            id,
            kind: obj.kind.clone(),
            owner: obj.owner,
            pos: llpos,
        });
        self.persisted.objectives.insert_cow(id, obj);
        self.persisted.objectives_by_name.insert_cow(name, id);
        Ok(())
    }

    /// Objective groups are trigger zones with the first character set to G. They are then a template
    /// name, followed by # and a number. They are associated with an objective by proximity.
    /// e.g. GRIRSRAD#001 would be the 1st instantiation of the template RIRSRAD, which must
    /// correspond to a group in the miz file. There is one special template name called (R|B|N)LOGI
    /// which corresponds to the logistics template for objectives
    fn init_objective_group(
        &mut self,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        _miz: &Miz,
        zone: TriggerZone,
        side: Side,
        name: &str,
    ) -> Result<()> {
        let pos = zone.pos()?;
        let obj = {
            let mut iter = self.persisted.objectives.into_iter();
            loop {
                match iter.next() {
                    None => bail!("group {:?} isn't associated with an objective", name),
                    Some((id, obj)) => {
                        if obj.zone.contains(pos) {
                            break *id;
                        }
                    }
                }
            }
        };
        let gid = self.add_group(
            spctx,
            idx,
            side,
            SpawnLoc::AtPos {
                pos,
                offset_direction: Vector2::default(),
                group_heading: 0.,
            },
            name,
            DeployKind::Objective { origin: obj },
            BitFlags::empty(),
        )?;
        let o = objective_mut!(self, obj)?;
        o.groups.get_or_default_cow(side).insert_cow(gid);
        let owner = o.owner;
        self.persisted.objectives_by_group.insert_cow(gid, obj);
        if side != owner {
            for uid in group!(self, gid)?.units.clone().into_iter() {
                unit_mut!(self, uid)?.dead = true;
            }
        }
        Ok(())
    }

    /// Safety net for the "G" trigger-zone coverage pass above: if an
    /// objective that needs a logistics-defense group (Airbase/Fob/Farp/
    /// Logistics) has no G<template> coverage zone placed for it -- meaning
    /// the proximity pass above never associated one with it -- spawn one
    /// directly from the standard RLOGI/BLOGI template at the objective's
    /// own zone center instead of leaving it permanently defenseless.
    ///
    /// A missing coverage zone is an easy mission-authoring mistake (add a
    /// new base, forget its logistics marker) and otherwise leaves that
    /// objective's Logi stat stuck at 0% forever -- there's no group there
    /// for a repair crate or auto-repair to ever revive. This makes correct
    /// logistics coverage a property of the engine, not something every
    /// mission version has to get exactly right by hand.
    pub fn ensure_default_logi_coverage(&mut self, spctx: &SpawnCtx, idx: &MizIndex) -> Result<()> {
        let targets: SmallVec<[(ObjectiveId, Side, Vector2); 64]> = self
            .persisted
            .objectives
            .into_iter()
            .filter(|(_, obj)| {
                matches!(
                    obj.kind,
                    ObjectiveKind::Airbase
                        | ObjectiveKind::Fob
                        | ObjectiveKind::Farp { .. }
                        | ObjectiveKind::Logistics
                        | ObjectiveKind::NavalBase
                )
            })
            .map(|(oid, obj)| (*oid, obj.owner, obj.zone.pos()))
            .collect();

        let land = Land::singleton(spctx.lua())?;
        for (oid, owner, pos) in targets {
            // The objective zone centre can sit over water (naval bases,
            // coastal airfields). Ground logi units can't spawn there, so
            // probe outward on a ring for the nearest dry point.
            let land_pos = {
                let dry = |p: Vector2| {
                    matches!(
                        land.get_surface_type(LuaVec2(p)),
                        Ok(SurfaceType::Land | SurfaceType::Road | SurfaceType::Runway)
                    )
                };
                if dry(pos) {
                    Some(pos)
                } else {
                    let mut found = None;
                    'search: for r in [200.0, 500.0, 1000.0, 2000.0, 3500.0] {
                        for k in 0..12 {
                            let a = k as f64 * std::f64::consts::TAU / 12.0;
                            let p = pos + Vector2::new(a.cos() * r, a.sin() * r);
                            if dry(p) {
                                found = Some(p);
                                break 'search;
                            }
                        }
                    }
                    found
                }
            };
            let Some(land_pos) = land_pos else {
                warn!("[LOGI_FALLBACK] objective {:?} has no dry ground near its zone centre -- skipping default logi coverage", oid);
                continue;
            };
            for side in [Side::Red, Side::Blue] {
                let obj = objective!(self, oid)?;
                let has_logi = obj.groups.get(&side).is_some_and(|gids| {
                    gids.into_iter()
                        .any(|gid| group!(self, *gid).map(|g| g.class.is_logi()).unwrap_or(false))
                });
                if has_logi {
                    continue;
                }
                let template_name = match side {
                    Side::Red => "RLOGI",
                    Side::Blue => "BLOGI",
                    Side::Neutral => continue,
                };
                if spctx
                    .get_template_ref(idx, GroupKind::Any, side, template_name)
                    .is_err()
                {
                    continue;
                }
                let gid = match self.add_group(
                    spctx,
                    idx,
                    side,
                    SpawnLoc::AtPos {
                        pos: land_pos,
                        offset_direction: Vector2::default(),
                        group_heading: 0.,
                    },
                    template_name,
                    DeployKind::Objective { origin: oid },
                    BitFlags::empty(),
                ) {
                    Ok(gid) => gid,
                    Err(e) => {
                        warn!("[LOGI_FALLBACK] objective {:?} side {:?}: could not place {}: {:?}",
                              oid, side, template_name, e);
                        continue;
                    }
                };
                let o = objective_mut!(self, oid)?;
                o.groups.get_or_default_cow(side).insert_cow(gid);
                self.persisted.objectives_by_group.insert_cow(gid, oid);
                if side != owner {
                    for uid in group!(self, gid)?.units.clone().into_iter() {
                        unit_mut!(self, uid)?.dead = true;
                    }
                }
                info!(
                    "[LOGI_FALLBACK] objective {:?} had no coverage zone for side {:?}, spawned default {} group",
                    oid, side, template_name
                );
            }
        }
        Ok(())
    }

    fn init_carrier_template_groups(
        &mut self,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        miz: &Miz,
        lua: MizLua,
        create_objectives: bool,
    ) -> Result<()> {
        use dcso3::coord::Coord;

        // Index carrier template groups from the miz ship groups
        // These are late-activated ship groups with CARRIER in their name
        // Also create the carrier group objective at the ship's location (if create_objectives is true)
        for side in Side::ALL {
            let coa = miz.coalition(side)?;
            for country in coa.countries()? {
                let country = country?;
                for ship_group in country.ships()? {
                    let ship_group = ship_group?;
                    let name = ship_group.name()?;
                    // Only index groups with CARRIER in the name (template groups)
                    if name.contains("CARRIER") {
                        info!("[CARRIER_TEMPLATE] Indexing ship group: {}", name);

                        // Check if a group with this template name already exists in the database
                        // This prevents duplicate groups when re-indexing after load
                        // Exact match: RCARRIER and RCARRIER2 are distinct
                        // task forces -- a `starts_with` here would make one
                        // suppress creation of the other.
                        let already_exists = self.persisted.groups.into_iter()
                            .any(|(_, g)| g.template_name.as_str() == name.as_str());
                        if already_exists {
                            info!("[CARRIER_TEMPLATE] Skipping {} - already exists in database", name);
                            continue;
                        }

                        // Get the group's position from the first unit
                        let first_unit = ship_group.units()?.into_iter().next();
                        let first_unit = match first_unit {
                            Some(Ok(unit)) => unit,
                            Some(Err(e)) => {
                                error!("[CARRIER_TEMPLATE] Failed to get first unit: {}", e);
                                continue;
                            },
                            None => {
                                error!("[CARRIER_TEMPLATE] Ship group has no units");
                                continue;
                            },
                        };
                        let pos = first_unit.pos()?;

                        // Create the carrier group objective at this location (only on initial load)
                        // But first check if one already exists for this side (from trigger zone parsing)
                        let obj_id = if create_objectives {
                            // Check if a carrier group objective already exists for this side
                            let existing_cg = self.persisted.carrier_groups.into_iter()
                                .find(|cg_id| {
                                    self.persisted.objectives.get(cg_id)
                                        .map(|obj| obj.owner == side)
                                        .unwrap_or(false)
                                })
                                .cloned();

                            if let Some(existing_id) = existing_cg {
                                info!("[CARRIER_TEMPLATE] Using existing carrier group objective for {:?} side", side);
                                Some(existing_id)
                            } else {
                                let obj_id = ObjectiveId::new();
                                let obj_name = format!("{} Strike Group", if side == Side::Red { "Red" } else { "Blue" });

                                // Create a circular zone around the carrier's position (5000m radius)
                                let zone = Zone::Circle {
                                    pos,
                                    radius: 5000.0,
                                };

                                let obj = Objective {
                                    id: obj_id,
                                    spawned: false,
                                    enabled: true,
                                    threatened: false,
                                    zone,
                                    name: obj_name.clone().into(),
                                    kind: ObjectiveKind::CarrierGroup {
                                        carrier_template: name.clone(),
                                        waypoint: None,
                                        parent_naval_base: None,
                                        repair_start_time: None,
                                    },
                                    owner: side,
                                    groups: MapS::new(),
                                    health: 100,
                                    logi: 100,
                                    infantry: 0,
                                    supply: 100,
                                    fuel: 100,
                                    last_change_ts: Utc::now(),
                                    last_threatened_ts: Utc::now(),
                                    warehouse: Warehouse::default(),
                                    points: 0,
                                    capture_hold: vec![],
                                    capture_hold_ts: None,
                                    logistics_detached: true,
                                    unlimited_supply: false,
                                    unlimited_aircraft: false,
                                    priority: false,
                                    last_activate: DateTime::<Utc>::default(),
                                    threat_pos3: Vector3::default(),
                                };

                                info!("[CARRIER_OBJ] Created carrier group objective: {}", obj_name);

                                // Register the objective
                                self.persisted.carrier_groups.insert_cow(obj_id);

                                let llpos = Coord::singleton(lua)?.lo_to_ll(LuaVec3(Vector3::new(pos.x, 0., pos.y)))?;
                                self.ephemeral.stat(Stat::Objective {
                                    name: obj_name.clone().into(),
                                    id: obj_id,
                                    kind: obj.kind.clone(),
                                    owner: obj.owner,
                                    pos: llpos,
                                });

                                self.persisted.objectives.insert_cow(obj_id, obj);
                                self.persisted.objectives_by_name.insert_cow(obj_name.into(), obj_id);

                                Some(obj_id)
                            }
                        } else {
                            None
                        };

                        // Add the ship group to the database
                        info!("[CARRIER_TEMPLATE] About to add ship group {} to database", name);
                        let gid = match self.add_group(
                            spctx,
                            idx,
                            side,
                            SpawnLoc::AtPos {
                                pos,
                                offset_direction: Vector2::default(),
                                group_heading: 0.,
                            },
                            &name,
                            if let Some(oid) = obj_id {
                                DeployKind::Objective { origin: oid }
                            } else {
                                DeployKind::ObjectiveDeprecated
                            },
                            BitFlags::empty(),
                        ) {
                            Ok(gid) => gid,
                            Err(e) => {
                                error!("[CARRIER_TEMPLATE] Failed to add group {}: {}", name, e);
                                continue;
                            }
                        };

                        info!("[CARRIER_TEMPLATE] Added ship group {} with GroupId {:?}", name, gid);

                        // If we created an objective, link the group to it immediately
                        if let Some(oid) = obj_id {
                            let obj = objective_mut!(self, &oid)?;
                            obj.groups.get_or_default_cow(side).insert_cow(gid);
                            self.persisted.objectives_by_group.insert_cow(gid, oid);
                            info!("[CARRIER_TEMPLATE] Linked group {:?} to objective {:?}", gid, oid);
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn init_carrier_groups(&mut self, miz: &Miz) -> Result<()> {
        use super::objective::ObjGroupClass;

        info!("[CARRIER_INIT] Starting init_carrier_groups");

        // Valid carrier task-force template names, straight from the miz ship
        // groups (e.g. RCARRIER, RCARRIER2, BCARRIER, BCARRIER2). Any
        // persisted carrier group whose template_name isn't one of these is
        // a leftover from an older buggy build (some carried a mangled
        // template_name like "RCARRIER-2023"); it gets deleted here and
        // init_carrier_template_groups (already run) recreates a clean copy.
        let mut valid_names: FxHashSet<String> = FxHashSet::default();
        for side in Side::ALL {
            let coa = miz.coalition(side)?;
            for country in coa.countries()? {
                let country = country?;
                for ship_group in country.ships()? {
                    let name = ship_group?.name()?;
                    if name.contains("CARRIER") {
                        valid_names.insert(name);
                    }
                }
            }
        }
        info!("[CARRIER_INIT] valid carrier templates from miz: {:?}", valid_names);

        // (gid, side, template_name, unit centroid) for every persisted
        // carrier task-force group.
        let mut cgs: Vec<(GroupId, Side, String, Vector2)> = self
            .persisted
            .groups
            .into_iter()
            .filter_map(|(gid, group)| {
                if group.name.contains("CARRIER") && matches!(group.class, ObjGroupClass::Naval) {
                    let mut sum = Vector2::default();
                    let mut n = 0u32;
                    for uid in &group.units {
                        if let Some(u) = self.persisted.units.get(uid) {
                            sum += u.pos;
                            n += 1;
                        }
                    }
                    let pos = if n > 0 { sum / n as f64 } else { Vector2::default() };
                    info!("[CARRIER_INIT] found carrier group {} tmpl={} side={:?} at ({:.0},{:.0})",
                          group.name, group.template_name, group.side, pos.x, pos.y);
                    Some((*gid, group.side, group.template_name.clone(), pos))
                } else {
                    None
                }
            })
            .collect();

        let carrier_ids: Vec<ObjectiveId> = self.persisted.carrier_groups.into_iter().copied().collect();
        info!("[CARRIER_INIT] {} carrier task-force group(s), {} carrier objective(s): {:?}",
              cgs.len(), carrier_ids.len(), carrier_ids);

        // Home side per carrier objective, from its NAME ("Red Strike Group"
        // / "Blue Strike Group") -- drift-immune, unlike the zone position.
        let mut home_by_oid: FxHashMap<ObjectiveId, Side> = FxHashMap::default();
        for cg_id in &carrier_ids {
            let obj = objective!(self, cg_id)?;
            let home = if obj.name.starts_with("Red") {
                Side::Red
            } else if obj.name.starts_with("Blue") {
                Side::Blue
            } else {
                Side::Neutral
            };
            home_by_oid.insert(*cg_id, home);
        }

        // --- Delete corrupt / duplicate carrier task-force groups ---
        // Keep a group only if its template_name is a current miz carrier
        // template AND it's the lowest-GroupId group with that template.
        // Everything else (mangled template_name from an old build, or a
        // second copy of a template) is deleted -- two groups from one
        // template collide on DCS deck-airbase unit names. Skip the whole
        // pass if the miz gave us no carrier templates (parse failure) so we
        // don't wipe every carrier group.
        if !valid_names.is_empty() {
            let mut best: FxHashMap<String, GroupId> = FxHashMap::default();
            for (gid, _, tmpl, _) in &cgs {
                if valid_names.contains(tmpl) {
                    best.entry(tmpl.clone())
                        .and_modify(|b| {
                            if gid < b {
                                *b = *gid;
                            }
                        })
                        .or_insert(*gid);
                }
            }
            let keep = |gid: &GroupId, tmpl: &String| {
                valid_names.contains(tmpl) && best.get(tmpl) == Some(gid)
            };
            let doomed: Vec<(GroupId, String)> = cgs
                .iter()
                .filter(|(g, _, t, _)| !keep(g, t))
                .map(|(g, _, t, _)| (*g, t.clone()))
                .collect();
            for (gid, tmpl) in doomed {
                let gname = group!(self, &gid).map(|g| g.name.to_string()).unwrap_or_default();
                info!("[CARRIER_INIT] deleting stale carrier group {:?} ({} / tmpl {})", gid, gname, tmpl);
                // Despawn by name ONLY for a mangled/unknown template name --
                // that name is unique so it can't hit a live carrier. A
                // duplicate of a VALID template (e.g. a second "RCARRIER")
                // shares its DCS group name with the real one, so despawning
                // by name could kill the wrong ship; just drop it from the DB
                // (nothing is spawned yet at init time anyway) and let the
                // real group spawn.
                if !valid_names.contains(&tmpl) {
                    self.ephemeral.push_despawn(gid, Despawn::GroupByName(tmpl.to_string()));
                    if !gname.is_empty() && gname != tmpl.as_str() {
                        self.ephemeral.push_despawn(gid, Despawn::GroupByName(gname));
                    }
                }
                self.persisted.objectives_by_group.remove_cow(&gid);
                if let Err(e) = self.delete_group(&gid) {
                    error!("[CARRIER_INIT] failed to delete stale carrier group {:?}: {:?}", gid, e);
                }
            }
            cgs.retain(|(g, _, t, _)| keep(g, t));
        }

        // Wipe every carrier objective's task-force group list (any naval
        // CARRIER group, plus dead references) -- rebuilt below.
        for cg_id in &carrier_ids {
            let stale: SmallVec<[(Side, GroupId); 8]> = {
                let obj = objective!(self, cg_id)?;
                (&obj.groups)
                    .into_iter()
                    .flat_map(|(s, set)| {
                        let s = *s;
                        set.into_iter()
                            .copied()
                            .filter(|g| {
                                self.persisted
                                    .groups
                                    .get(g)
                                    .map(|grp| {
                                        grp.name.contains("CARRIER")
                                            && matches!(grp.class, ObjGroupClass::Naval)
                                    })
                                    .unwrap_or(true)
                            })
                            .map(move |g| (s, g))
                    })
                    .collect()
            };
            for (s, g) in stale {
                if let Some(set) = objective_mut!(self, cg_id)?.groups.get_mut_cow(&s) {
                    set.remove_cow(&g);
                }
            }
        }

        // --- Assign each task force to a carrier objective BY NAME ---
        // Each carrier objective holds BOTH sides' task forces (co-located
        // in the miz). The naming convention picks which objective a group
        // belongs to, drift- and zone-collapse-immune:
        //   "<X>CARRIER"  -> the objective whose home side is X
        //   "<X>CARRIER2" -> the objective whose home side is the OTHER side
        //                    (X's reserve, pre-placed to take over when X
        //                    captures the enemy carrier station)
        // Falls back to the nearest carrier objective if the name doesn't
        // resolve. The group whose side == the objective's owner is LIVE
        // (spawned); the other side's group is a RESERVE (units dead) that
        // flip_carrier_group_owner resurrects in place on capture.
        let mut live_taken: FxHashSet<(ObjectiveId, Side)> = FxHashSet::default();
        for (gid, g_side, tmpl, g_pos) in cgs.iter().cloned() {
            let suffixed = tmpl
                .rsplit("CARRIER")
                .next()
                .map(|s| s.chars().any(|c| c.is_ascii_digit()))
                .unwrap_or(false);
            let want_home = if suffixed { g_side.opposite() } else { g_side };
            let target = carrier_ids
                .iter()
                .copied()
                .find(|oid| home_by_oid.get(oid).copied() == Some(want_home))
                .or_else(|| {
                    carrier_ids.iter().copied().min_by(|a, b| {
                        let pa = objective!(self, a).map(|o| o.zone.pos()).unwrap_or_default();
                        let pb = objective!(self, b).map(|o| o.zone.pos()).unwrap_or_default();
                        na::distance_squared(&pa.into(), &g_pos.into())
                            .partial_cmp(&na::distance_squared(&pb.into(), &g_pos.into()))
                            .unwrap()
                    })
                });
            let Some(cg_id) = target else { continue };

            let owner = objective!(self, &cg_id)?.owner;
            let cg_name = objective!(self, &cg_id)?.name.clone();
            let effective_owner = if owner == Side::Neutral {
                home_by_oid.get(&cg_id).copied().unwrap_or(Side::Neutral)
            } else {
                owner
            };
            let live = g_side == effective_owner && live_taken.insert((cg_id, g_side));

            objective_mut!(self, &cg_id)?
                .groups
                .get_or_default_cow(g_side)
                .insert_cow(gid);
            self.persisted.objectives_by_group.insert_cow(gid, cg_id);
            group_mut!(self, &gid)?.origin = DeployKind::Objective { origin: cg_id };

            let uids: SmallVec<[bfprotocols::db::group::UnitId; 8]> =
                group!(self, &gid)?.units.into_iter().copied().collect();
            for uid in &uids {
                unit_mut!(self, uid)?.dead = !live;
            }

            if live {
                if let Zone::Circle { pos, .. } = &mut objective_mut!(self, &cg_id)?.zone {
                    *pos = g_pos;
                }
                self.ephemeral.push_spawn(gid);
                let template_name = group!(self, &gid)?.template_name.clone();
                if let ObjectiveKind::CarrierGroup { carrier_template, .. } =
                    &mut objective_mut!(self, &cg_id)?.kind
                {
                    *carrier_template = template_name.clone();
                }
                info!("[CARRIER_INIT] {} LIVE {} {:?} ({:?})", cg_name, tmpl, gid, g_side);
            } else {
                info!("[CARRIER_INIT] {} RESERVE {} {:?} ({:?})", cg_name, tmpl, gid, g_side);
            }
        }

        // Mark every carrier objective spawned so cull_or_respawn_objectives
        // leaves it alone (spawned is #[serde(skip)], defaults false on load).
        for cg_id in &carrier_ids {
            objective_mut!(self, cg_id)?.spawned = true;
        }

        // Link carrier groups to their nearest naval base
        for cg_id in &self.persisted.carrier_groups.clone() {
            let cg_obj = objective!(self, &cg_id)?;
            let cg_pos = cg_obj.zone.pos();
            let cg_owner = cg_obj.owner;

            // Find the nearest naval base of the same owner
            let mut nearest_nb: Option<(ObjectiveId, f64)> = None;
            for nb_id in &self.persisted.naval_bases {
                let nb_obj = objective!(self, nb_id)?;
                if nb_obj.owner == cg_owner {
                    let nb_pos = nb_obj.zone.pos();
                    let dist_sq = na::distance_squared(&cg_pos.into(), &nb_pos.into());
                    if let Some((_, best_dist)) = nearest_nb {
                        if dist_sq < best_dist {
                            nearest_nb = Some((*nb_id, dist_sq));
                        }
                    } else {
                        nearest_nb = Some((*nb_id, dist_sq));
                    }
                }
            }

            // Set the parent naval base
            if let Some((nb_id, _)) = nearest_nb {
                let cg_name = objective!(self, cg_id)?.name.clone();
                let nb_name = objective!(self, nb_id)?.name.clone();
                let cg_obj = objective_mut!(self, cg_id)?;
                if let ObjectiveKind::CarrierGroup { parent_naval_base, .. } = &mut cg_obj.kind {
                    *parent_naval_base = Some(nb_id);
                    info!("linked carrier group {} to naval base {}", cg_name, nb_name);
                }
            } else {
                let cg_name = objective!(self, cg_id)?.name.clone();
                debug!("no naval base found for carrier group {}", cg_name);
            }
        }

        // IADN: link SAM sites to their nearest friendly command center.
        // A SAM site with no link (or whose linked command center later
        // dies/changes hands) falls back to plain DCS AI -- see the EMCON
        // gating in ewr.rs.
        for sam_id in &self.persisted.special_sam_sites.clone() {
            let sam_obj = objective!(self, sam_id)?;
            let sam_pos = sam_obj.zone.pos();
            let sam_owner = sam_obj.owner;

            let mut nearest_cc: Option<(ObjectiveId, f64)> = None;
            for cc_id in &self.persisted.command_centers {
                let cc_obj = objective!(self, cc_id)?;
                if cc_obj.owner == sam_owner {
                    let cc_pos = cc_obj.zone.pos();
                    let dist_sq = na::distance_squared(&sam_pos.into(), &cc_pos.into());
                    if let Some((_, best_dist)) = nearest_cc {
                        if dist_sq < best_dist {
                            nearest_cc = Some((*cc_id, dist_sq));
                        }
                    } else {
                        nearest_cc = Some((*cc_id, dist_sq));
                    }
                }
            }

            if let Some((cc_id, _)) = nearest_cc {
                let sam_name = objective!(self, sam_id)?.name.clone();
                let cc_name = objective!(self, cc_id)?.name.clone();
                self.persisted.sam_command_center_link.insert_cow(*sam_id, cc_id);
                info!("linked SAM site {} to command center {}", sam_name, cc_name);
            } else {
                let sam_name = objective!(self, sam_id)?.name.clone();
                debug!("no command center found for SAM site {}", sam_name);
            }
        }

        Ok(())
    }

    fn init_special_sam_sites(
        &mut self,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        lua: MizLua,
    ) -> Result<()> {
        use super::ephemeral::SyntheticGroupSpec;
        use dcso3::group::GroupCategory;
        let sites: Vec<SpecialSamSiteCfg> = self.ephemeral.cfg.special_sam_sites.clone();
        for cfg_site in &sites {
            // On load, re-register synthetic templates for any inline groups so that
            // spawn_group can find them in ephemeral.synthetic_templates (which is cleared on restart).
            if self.persisted.objectives_by_name.get(&cfg_site.name).is_some() {
                let sides_info: [(Side, &Vec<SpecialSamUnitCfg>, Country); 2] = [
                    (Side::Red, &cfg_site.red_units, cfg_site.red_country),
                    (Side::Blue, &cfg_site.blue_units, cfg_site.blue_country),
                ];
                for (side, inline_units, country) in &sides_info {
                    if inline_units.is_empty() {
                        continue;
                    }
                    if let Some(oid) = self.persisted.objectives_by_name.get(&cfg_site.name) {
                        if let Some(obj) = self.persisted.objectives.get(oid) {
                            if let Some(groups) = obj.groups.get(side) {
                                for gid in groups {
                                    if let Some(g) = self.persisted.groups.get(gid) {
                                        if g.template_name.starts_with("@synthetic:") {
                                            self.ephemeral.synthetic_templates.insert(
                                                g.template_name.clone(),
                                                SyntheticGroupSpec {
                                                    country: *country,
                                                    category: GroupCategory::Ground,
                                                },
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                continue;
            }
            let id = ObjectiveId::new();
            let site_pos = Vector2::new(cfg_site.pos.x, cfg_site.pos.y);
            let zone = Zone::Circle {
                pos: site_pos,
                radius: self.ephemeral.cfg.special_sam_capture_radius_m,
            };
            let obj = Objective {
                id,
                spawned: false,
                enabled: false,
                threatened: false,
                zone,
                name: cfg_site.name.clone(),
                kind: ObjectiveKind::SpecialSamSite {},
                owner: cfg_site.coalition,
                groups: MapS::new(),
                health: 0,
                logi: 0,
                infantry: 0,
                supply: 0,
                fuel: 0,
                last_change_ts: Utc::now(),
                last_threatened_ts: Utc::now(),
                warehouse: Warehouse::default(),
                points: 0,
                capture_hold: vec![],
                capture_hold_ts: None,
                logistics_detached: false,
                unlimited_supply: false,
                unlimited_aircraft: false,
                priority: false,
                last_activate: DateTime::<Utc>::default(),
                threat_pos3: Vector3::default(),
            };
            self.persisted.special_sam_sites.insert_cow(id);
            let llpos = Coord::singleton(lua)?
                .lo_to_ll(LuaVec3(Vector3::new(cfg_site.pos.x, 0., cfg_site.pos.y)))?;
            self.ephemeral.stat(Stat::Objective {
                name: cfg_site.name.clone(),
                id,
                kind: obj.kind.clone(),
                owner: obj.owner,
                pos: llpos,
            });
            self.persisted.objectives.insert_cow(id, obj);
            self.persisted.objectives_by_name.insert_cow(cfg_site.name.clone(), id);

            // Register a group for BOTH coalitions, just like standard objectives.
            // The non-owner side's units are marked dead so they don't count toward health.
            // When the site is captured, cull_or_respawn_objectives finds the new owner's
            // group in obj.groups and spawns it — this is the standard capture flow.
            let sides: [(Side, Option<String>, &Vec<SpecialSamUnitCfg>, Country); 2] = [
                (Side::Red, cfg_site.red_template.clone(), &cfg_site.red_units, cfg_site.red_country),
                (Side::Blue, cfg_site.blue_template.clone(), &cfg_site.blue_units, cfg_site.blue_country),
            ];
            for (side, template, inline_units, country) in &sides {
                let side = *side;
                let country = *country;
                let gid = if !inline_units.is_empty() {
                    self.add_group_from_units(
                        spctx,
                        side,
                        country,
                        inline_units,
                        DeployKind::Objective { origin: id },
                    )?
                } else if let Some(tmpl) = template {
                    // AtPosWithCenter with pos==center is a pure translation that preserves each
                    // unit's exact template position and heading (no rotation applied).
                    self.add_group(
                        spctx,
                        idx,
                        side,
                        SpawnLoc::AtPosWithCenter {
                            pos: site_pos,
                            center: site_pos,
                        },
                        tmpl,
                        DeployKind::Objective { origin: id },
                        BitFlags::empty(),
                    )?
                } else {
                    bail!(
                        "special SAM site '{}': {} side needs either inline units or a template",
                        cfg_site.name,
                        side
                    )
                };
                let o = objective_mut!(self, id)?;
                o.groups.get_or_default_cow(side).insert_cow(gid);
                self.persisted.objectives_by_group.insert_cow(gid, id);
                // Mark non-owner side's units dead (same as init_objective_group)
                if side != cfg_site.coalition {
                    for uid in group!(self, gid)?.units.clone().into_iter() {
                        unit_mut!(self, uid)?.dead = true;
                    }
                }
            }
            self.update_objective_status(&id, Utc::now())?;
        }
        Ok(())
    }

    /// Find every neutral-coalition static object placed inside an objective zone
    /// and register it so that if it's ever destroyed, `respawn_protected_static`
    /// (called from the death event handlers) puts it right back — this is the
    /// only way to make a DCS static object effectively immortal, since the
    /// StaticObject class has no controller and no setImmortal/setLife API.
    fn init_protected_statics(&mut self, miz: &Miz, lua: MizLua) -> Result<()> {
        use dcso3::{coalition::Static, object::DcsObject, static_object::StaticObject};
        let zones: Vec<Zone> = self
            .persisted
            .objectives
            .into_iter()
            .map(|(_, o)| o.zone)
            .collect();
        if zones.is_empty() {
            return Ok(());
        }
        for country in miz.coalition(Side::Neutral)?.countries()? {
            let country = country?;
            for g in country.statics()? {
                let g = g?;
                let pos = match g.pos() {
                    Ok(p) => p,
                    Err(_) => continue,
                };
                if !zones.iter().any(|z| z.contains(pos)) {
                    continue;
                }
                let name = g.name()?;
                match StaticObject::get_by_name(lua, name.as_str()) {
                    Ok(Static::Static(obj)) => {
                        let id = obj.object_id()?;
                        self.ephemeral.protected_statics.insert(
                            id,
                            super::ephemeral::ProtectedStatic {
                                template_name: name,
                                side: Side::Neutral,
                            },
                        );
                    }
                    Ok(Static::Airbase(_)) => (),
                    Err(e) => {
                        debug!("protected static '{name}' not found live in the world: {e:?}")
                    }
                }
            }
        }
        info!(
            "[PROTECTED_STATICS] registered {} neutral statics inside objective zones",
            self.ephemeral.protected_statics.len()
        );
        Ok(())
    }

    /// Find real map-terrain buildings (DCS "Scenery" category -- baked into
    /// the terrain, not placed via the mission editor) inside each objective's
    /// zone, draw a small box on each one (plus a warning marker on objectives
    /// where none were found), and register them in `ephemeral.tracked_scenery`
    /// so `check_scenery_buildings` can later detect when they're destroyed and
    /// dock that objective's `logi` rating accordingly. Resets and re-registers
    /// from scratch every time it runs (every mission load) -- this state is
    /// intentionally not persisted, matching the fact that DCS's own terrain
    /// destruction doesn't survive a server restart either.
    ///
    /// Raw "Scenery" search radius around an airbase sweeps in whole nearby
    /// towns (tens of thousands of houses/props), so results are filtered to
    /// type names that look like actual logistics infrastructure -- otherwise
    /// this would flood the F10 map with tens of thousands of markers.
    pub fn scan_objective_scenery(&mut self, lua: MizLua) -> Result<()> {
        use dcso3::{
            object::{ClassObject, DcsObject, DcsOid, ObjectCategory},
            world::{SearchVolume, World},
        };
        use mlua::Value;
        use std::{cell::RefCell, rc::Rc};
        const RELEVANT_KEYWORDS: &[&str] = &["WAREHOUSE", "INDUSTRIAL", "DEPOT", "FUEL", "STORAGE"];
        // Only the few buildings closest to the objective count as its logistics
        // infrastructure -- a scan radius over a town otherwise pulls in dozens
        // of unrelated structures, so destroying any one barely moves the logi
        // bar. This is both the tracked-for-penalty count and the marker count.
        const BUILDINGS_PER_OBJECTIVE: usize = 4;
        let world = World::singleton(lua)?;
        let land = Land::singleton(lua)?;

        self.ephemeral.tracked_scenery.clear();
        self.ephemeral.scenery_check_queue.clear();
        self.ephemeral.scenery_destroyed_by_objective.clear();
        self.ephemeral.scenery_total_by_objective.clear();

        // Collect first so this loop doesn't hold a borrow of self.persisted
        // while we need &mut self.ephemeral below to draw markers/register.
        let objectives: Vec<(ObjectiveId, String, CompactString, Vector2, f64)> = self
            .persisted
            .objectives
            .into_iter()
            .map(|(oid, o)| {
                (
                    *oid,
                    o.name.clone(),
                    CompactString::from(o.kind.name()),
                    o.zone.pos(),
                    o.zone.radius().max(200.),
                )
            })
            .collect();
        for (oid, name, kind_name, center, radius) in objectives {
            let alt = land.get_height(LuaVec2(center))?;
            let point = LuaVec3(Vector3::new(center.x, alt, center.y));
            let vol = SearchVolume::Sphere { point, radius };
            let seen: Rc<RefCell<usize>> = Rc::new(RefCell::new(0));
            let found: Rc<RefCell<SmallVec<[(CompactString, LuaVec3, DcsOid<ClassObject>); 8]>>> =
                Rc::new(RefCell::new(SmallVec::new()));
            let seen_cb = Rc::clone(&seen);
            let found_cb = Rc::clone(&found);
            world.search_objects(ObjectCategory::Scenery, vol, Value::Nil, move |_, o, _| {
                *seen_cb.borrow_mut() += 1;
                let typ = o.get_type_name().unwrap_or_else(|_| "?".into());
                let typ_upper = typ.to_uppercase();
                if !RELEVANT_KEYWORDS.iter().any(|kw| typ_upper.contains(kw)) {
                    return Ok(true);
                }
                let id = match o.object_id() {
                    Ok(id) => id,
                    Err(_) => return Ok(true),
                };
                let oname = o.get_name().unwrap_or_else(|_| "?".into());
                let label = CompactString::from(format!("{oname} ({typ})"));
                let pos = o
                    .get_point()
                    .unwrap_or(LuaVec3(Vector3::new(0., 0., 0.)));
                found_cb.borrow_mut().push((label, pos, id));
                Ok(true)
            })?;
            let seen = *seen.borrow();
            let mut found = found.borrow().clone();
            // Keep only the closest BUILDINGS_PER_OBJECTIVE to the objective centre.
            found.sort_by(|(_, a, _), (_, b, _)| {
                let da = (a.x - center.x).powi(2) + (a.z - center.y).powi(2);
                let db = (b.x - center.x).powi(2) + (b.z - center.y).powi(2);
                da.total_cmp(&db)
            });
            found.truncate(BUILDINGS_PER_OBJECTIVE);
            if found.is_empty() {
                info!(
                    "[SCENERY_SCAN] {name} ({kind_name}): no relevant buildings within {radius:.0}m ({seen} scenery objects scanned)"
                );
            } else {
                info!(
                    "[SCENERY_SCAN] {name} ({kind_name}): {} relevant building(s) within {radius:.0}m ({seen} scenery objects scanned): {}",
                    found.len(),
                    found
                        .iter()
                        .map(|(l, _, _)| l.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                self.ephemeral
                    .scenery_total_by_objective
                    .insert(oid, found.len() as u32);
                for (label, pos, id) in found.iter() {
                    // Registered for logi tracking always; the F10 marker itself is
                    // created/removed by sync_scenery_markers, tied to the parent
                    // objective's spawn state exactly like its units (culled when
                    // no player is near) so the map isn't carrying hundreds of pins.
                    self.ephemeral.tracked_scenery.insert(
                        id.clone(),
                        super::ephemeral::TrackedScenery {
                            objective: oid,
                            label: label.clone().into(),
                            pos: Vector2::new(pos.x, pos.z),
                            marker: None,
                            marker_side: None,
                        },
                    );
                    self.ephemeral.scenery_check_queue.push_back(id.clone());
                }
            }
        }
        info!(
            "[SCENERY_SCAN] registered {} logistics-relevant scenery buildings across {} objectives",
            self.ephemeral.tracked_scenery.len(),
            self.ephemeral.scenery_total_by_objective.len()
        );
        // Draw markers for whatever's already spawned (the cull loop keeps them
        // in sync from here on).
        let with_buildings: SmallVec<[ObjectiveId; 64]> =
            self.ephemeral.scenery_total_by_objective.keys().copied().collect();
        for oid in with_buildings {
            self.sync_scenery_markers(oid);
        }
        Ok(())
    }

    /// Reconcile `oid`'s logistics-building F10 markers with the objective's
    /// current spawn state and owner. Markers exist only while the parent
    /// objective is spawned (culled the same way its units are, so the F10 map
    /// isn't carrying a pin for every building on the map at once) and are
    /// scoped to the owning coalition (everyone if Neutral). Call on every
    /// spawn/cull transition and on capture.
    pub(super) fn sync_scenery_markers(&mut self, oid: ObjectiveId) {
        let (side, oname, want) = match self.persisted.objectives.get(&oid) {
            Some(o) => (o.owner, o.name.clone(), o.spawned && !o.kind.is_special_sam_site()),
            None => return,
        };
        let ids: SmallVec<[_; 8]> = self
            .ephemeral
            .tracked_scenery
            .iter()
            .filter(|(_, ts)| ts.objective == oid)
            .map(|(id, _)| id.clone())
            .collect();
        for id in ids {
            let (label, pos, old, old_side) = match self.ephemeral.tracked_scenery.get(&id) {
                Some(ts) => (ts.label.clone(), ts.pos, ts.marker, ts.marker_side),
                None => continue,
            };
            let up_to_date = old.is_some() == want && (!want || old_side == Some(side));
            if up_to_date {
                continue;
            }
            if let Some(mk) = old {
                self.ephemeral.msgs().delete_mark(mk);
            }
            let (marker, marker_side) = if want {
                let text = format_compact!("{oname} logi: {label}");
                let mk = match side {
                    Side::Neutral => self.ephemeral.msgs().mark_to_all(pos, true, text),
                    s => self.ephemeral.msgs().mark_to_side(s, pos, true, text),
                };
                (Some(mk), Some(side))
            } else {
                (None, None)
            };
            if let Some(ts) = self.ephemeral.tracked_scenery.get_mut(&id) {
                ts.marker = marker;
                ts.marker_side = marker_side;
            }
        }
    }

    /// Poll a small round-robin batch of `tracked_scenery` for destruction
    /// (Scenery objects don't reliably fire death events, unlike units/statics,
    /// so this is a periodic `is_exist()` check rather than event-driven).
    /// Confirmed-destroyed buildings are removed from tracking and counted in
    /// `scenery_destroyed_by_objective`, then that objective's status is
    /// recomputed so the `logi` penalty in `compute_objective_status` applies.
    pub fn check_scenery_buildings(&mut self, lua: MizLua, ts: DateTime<Utc>) -> Result<()> {
        use dcso3::object::{DcsObject, Object};
        const BATCH_SIZE: usize = 25;

        if self.ephemeral.scenery_check_queue.is_empty() {
            // Start a fresh round once everything still standing has been checked.
            self.ephemeral
                .scenery_check_queue
                .extend(self.ephemeral.tracked_scenery.keys().cloned());
            if self.ephemeral.scenery_check_queue.is_empty() {
                return Ok(());
            }
        }

        let mut affected: SmallVec<[ObjectiveId; 8]> = SmallVec::new();
        for _ in 0..BATCH_SIZE {
            let Some(id) = self.ephemeral.scenery_check_queue.pop_front() else {
                break;
            };
            let Some(tracked) = self.ephemeral.tracked_scenery.get(&id) else {
                continue; // already removed by a previous check this round
            };
            let alive = match Object::get_instance(lua, &id) {
                Ok(obj) => obj.is_exist().unwrap_or(true),
                Err(_) => false,
            };
            if alive {
                self.ephemeral.scenery_check_queue.push_back(id);
            } else {
                let oid = tracked.objective;
                let label = tracked.label.clone();
                let marker = tracked.marker;
                self.ephemeral.tracked_scenery.remove(&id);
                if let Some(mk) = marker {
                    self.ephemeral.msgs().delete_mark(mk);
                }
                *self
                    .ephemeral
                    .scenery_destroyed_by_objective
                    .entry(oid)
                    .or_insert(0) += 1;
                info!("[SCENERY_SCAN] destroyed: {label} (objective {:?})", oid);
                affected.push(oid);
            }
        }
        for oid in affected {
            if let Err(e) = self.update_objective_status(&oid, ts) {
                error!("failed to update objective status after scenery loss {oid:?} {e:?}");
            }
        }
        Ok(())
    }

    pub fn init_objective_slots(&mut self, side: Side, slot: Group) -> Result<()> {
        if slot.raw_get::<_, bool>("dynSpawnTemplate").unwrap_or(false) {
            return Ok(());
        }
        let mut ground_start = false;
        let mut has_link_unit = false;

        for point in slot.route()?.points()? {
            let point = point?;
            match point.typ {
                PointType::TakeOffGround | PointType::TakeOffGroundHot | PointType::TakeOffParkingHot => ground_start = true,
                PointType::Land
                | PointType::TakeOff
                | PointType::Custom(_)
                | PointType::Nil
                | PointType::TakeOffParking
                | PointType::TurningPoint => (),
            }
            // Check for link_unit (carrier-based slots have this set to the carrier unit)
            if point.link_unit.is_some() {
                has_link_unit = true;
            }
        }

        for unit in slot.units()? {
            let unit = unit?;
            let vehicle = Vehicle::from(unit.typ()?);
            self.ephemeral
                .cfg
                .check_vehicle_has_threat_distance(&vehicle)?;
            if unit.skill()? != Skill::Client {
                continue;
            }
            let id = unit.slot()?;
            let pos = unit.pos()?;

            // For carrier-based slots, find the objective by matching the link_unit to carrier objective
            let obj = if has_link_unit {
                // This is a carrier-based slot - find the carrier group objective for this side
                // At init time, carriers haven't spawned yet so we just match by side
                let mut found_obj: Option<ObjectiveId> = None;
                for (oid, obj) in self.persisted.objectives.into_iter() {
                    if let ObjectiveKind::CarrierGroup { .. } = &obj.kind {
                        if obj.owner == side {
                            found_obj = Some(*oid);
                            break;
                        }
                    }
                }
                match found_obj {
                    Some(oid) => {
                        info!("[CARRIER_SLOT] Slot {:?} linked to carrier objective {:?}", slot.name(), oid);
                        oid
                    }
                    None => {
                        info!("[CARRIER_SLOT] slot {:?} has link_unit but no matching carrier objective found for side {:?}", slot.name(), side);
                        return Ok(());
                    }
                }
            } else {
                // Standard zone-based matching for non-carrier slots
                let mut iter = self.persisted.objectives.into_iter();
                loop {
                    match iter.next() {
                        None => {
                            info!("slot {:?} not associated with an objective", slot);
                            return Ok(());
                        }
                        Some((id, obj)) => {
                            if obj.zone.contains(pos) {
                                break *id;
                            }
                        }
                    }
                }
            };

            self.ephemeral.cfg.check_vehicle_has_life_type(&vehicle)?;
            self.ephemeral.slot_info.insert(
                id.clone(),
                SlotInfo {
                    typ: vehicle,
                    unit_name: unit.name()?,
                    objective: obj,
                    ground_start,
                    miz_gid: slot.id()?,
                    side,
                },
            );
        }
        Ok(())
    }

    pub fn init(
        lua: MizLua,
        cfg: Arc<Cfg>,
        idx: &MizIndex,
        miz: &Miz,
        to_bg: UnboundedSender<Task>,
    ) -> Result<Self> {
        let spctx = SpawnCtx::new(lua)?;
        let mut t = Self::default();
        t.ephemeral.set_cfg(miz, idx, cfg, to_bg)?;
        if let Some(sc) = t.ephemeral.cfg.smart_commander.as_ref() {
            if sc.treasury_start > 0 {
                t.persisted.blue_treasury = sc.treasury_start;
                t.persisted.red_treasury = sc.treasury_start;
            }
        }
        let sc_obj_start = t
            .ephemeral
            .cfg
            .smart_commander
            .as_ref()
            .map(|sc| sc.objective_start_points)
            .unwrap_or(0);
        let mut objective_names = FxHashSet::default();
        for zone in miz.triggers()? {
            let zone = zone?;
            let name = zone.name()?;
            if name.starts_with('O') {
                info!("[ZONE] Processing trigger zone: {}", name);
                if name.len() > 4 {
                    if !objective_names.insert(CompactString::from(&name[3..])) {
                        bail!("duplicate objective name {name}")
                    }
                } else {
                    bail!("malformed objective name {name}")
                }
                let name = name.strip_prefix("O").unwrap();
                info!("[ZONE] After stripping O prefix: {}", name);
                t.init_objective(lua, zone, name)?
            }
        }
        for side in Side::ALL {
            // Neutral objectives are never given defenders, regardless of
            // what's placed in the trigger zone (e.g. a "Neut" AAA/SR/LOGI
            // template) — they're meant to be undefended and immediately
            // capturable by whichever side gets there first.
            if side == Side::Neutral {
                continue;
            }
            let _coa = miz.coalition(side)?;
            for zone in miz.triggers()? {
                let zone = zone?;
                let name = zone.name()?;
                if let Some(name) = name.strip_prefix("G") {
                    let (template_side, name) = name.parse::<ObjGroup>()?.template(side);
                    if template_side == side {
                        // Skip if the template doesn't exist in the miz for this side
                        // (e.g. unprefixed zones like "GLOGIA" generate "NLOGIA" for Neutral
                        // but missions typically don't define Neutral-coalition variants)
                        if spctx
                            .get_template_ref(idx, GroupKind::Any, side, name.as_str())
                            .is_err()
                        {
                            continue;
                        }
                        t.init_objective_group(&spctx, idx, miz, zone, side, name.as_str())?
                    }
                } else if name.starts_with("T") || name.starts_with("O") {
                    () // ignored
                } else {
                    bail!("invalid trigger zone type code {name}, expected O, G, or T prefix")
                }
            }
        }

        t.ensure_default_logi_coverage(&spctx, idx)
            .context("ensure_default_logi_coverage failed")?;

        // Index carrier template groups BEFORE slot initialization
        // This ensures carrier objectives exist so slots on carriers can be associated with them
        info!("[CARRIER_SETUP] About to init carrier template groups");
        t.init_carrier_template_groups(&spctx, idx, miz, lua, true)
            .context("init_carrier_template_groups failed")?;
        info!("[CARRIER_SETUP] About to init carrier groups");
        t.init_carrier_groups(miz)
            .context("init_carrier_groups failed")?;
        info!("[CARRIER_SETUP] Carrier initialization complete");
        t.init_special_sam_sites(&spctx, idx, lua)
            .context("init_special_sam_sites failed")?;
        t.init_protected_statics(miz, lua)
            .context("init_protected_statics failed")?;
        t.scan_objective_scenery(lua)
            .context("scan_objective_scenery failed")?;

        // Now initialize slots - carrier objectives are available for slot association
        for side in Side::ALL {
            let coa = miz.coalition(side)?;
            for country in coa.countries()? {
                let country = country?;
                for plane in country.planes()? {
                    let plane = plane?;
                    t.init_objective_slots(side, plane)?
                }
                for heli in country.helicopters()? {
                    let heli = heli?;
                    t.init_objective_slots(side, heli)?
                }
            }
        }
        let now = Utc::now();
        let ids = t
            .persisted
            .objectives
            .into_iter()
            .map(|(id, _)| *id)
            .collect::<Vec<_>>();
        for id in ids {
            // Skip carrier groups during initial status update - they'll be updated after spawning
            let obj = objective!(&t, id)?;
            if matches!(obj.kind, ObjectiveKind::CarrierGroup { .. } | ObjectiveKind::SpecialSamSite { .. }) {
                continue;
            }
            t.update_objective_status(&id, now)?
        }
        // Seed objective points from cfg (per-side budget distributed by kind weight).
        // cfg.objective_start_points takes priority; falls back to sc_obj_start
        // (which was the old flat equal-per-objective value from smart_commander).
        for (side, budget) in t.ephemeral.cfg.objective_start_points.clone() {
            if budget > 0 {
                t.seed_objective_points(side, budget, false);
            }
        }
        if t.ephemeral.cfg.objective_start_points.is_empty() && sc_obj_start > 0 {
            for side in dcso3::coalition::Side::ALL {
                t.seed_objective_points(side, sc_obj_start, false);
            }
        }
        t.init_warehouses(lua).context("initializing warehouses")?;
        t.ephemeral.dirty();
        Ok(t)
    }

    pub fn respawn_after_load(
        &mut self,
        lua: MizLua,
        perf: &mut PerfInner,
        idx: &MizIndex,
        miz: &Miz,
        landcache: &mut LandCache,
        spctx: &SpawnCtx,
    ) -> Result<()> {
        debug!("init slots");
        // migrate format changes
        if !self.persisted.migrated_v0 {
            self.persisted.migrated_v0 = true;
            self.ephemeral.dirty();
            for (oid, obj) in &self.persisted.objectives {
                for (_, groups) in &obj.groups {
                    for gid in groups {
                        let g = group_mut!(self, gid)?;
                        match &g.origin {
                            DeployKind::ObjectiveDeprecated => {
                                g.origin = DeployKind::Objective { origin: *oid };
                            }
                            _ => (),
                        }
                        for uid in &g.units {
                            let unit = unit_mut!(self, uid)?;
                            if unit.side != obj.owner {
                                unit.dead = true;
                            }
                        }
                    }
                }
            }
        }
        for side in Side::ALL {
            let coa = miz.coalition(side)?;
            for country in coa.countries()? {
                let country = country?;
                for plane in country.planes()? {
                    let plane = plane?;
                    self.init_objective_slots(side, plane)?
                }
                for heli in country.helicopters()? {
                    let heli = heli?;
                    self.init_objective_slots(side, heli)?
                }
            }
        }
        for name in &self.ephemeral.cfg.extra_fixed_wing_objectives {
            if !self.persisted.objectives_by_name.get(name).is_some() {
                bail!("extra_fixed_wing_objectives {name} does not match any objective")
            }
        }
        // Initialize and spawn carriers FIRST so they exist in DCS before crates/troops
        // that need to be linked to them. Without this, carrier-linked crates fail to
        // re-establish their linkUnit because Unit::get_by_name() can't find the carrier.
        info!("[CARRIER_LOAD] Re-indexing carrier template groups after load");
        self.init_carrier_template_groups(spctx, idx, miz, lua, false)
            .context("re-initializing carrier template groups")?;
        self.init_carrier_groups(miz)
            .context("re-initializing carrier groups")?;
        self.reconcile_carrier_task_forces(lua, idx)
            .context("reconciling carrier task forces")?;
        self.init_special_sam_sites(spctx, idx, lua)
            .context("re-init special sam sites")?;
        self.init_protected_statics(miz, lua)
            .context("re-init protected statics")?;
        self.scan_objective_scenery(lua)
            .context("scan_objective_scenery failed")?;
        info!("[CARRIER_LOAD] Spawning carrier groups before other entities");
        while self.ephemeral.spawnq_len() > 0 {
            self.ephemeral.process_spawn_queue(perf, &self.persisted, Utc::now(), idx, spctx)?
        }
        info!("[CARRIER_LOAD] Carrier groups spawned, now spawning deployed/logistics/crates");

        let mut spawn_deployed_and_logistics = || -> Result<()> {
            debug!("queue respawn deployables");
            let land = Land::singleton(spctx.lua())?;
            for gid in &self.persisted.deployed {
                self.ephemeral.push_spawn(*gid);
            }
            for gid in &self.persisted.crates {
                self.ephemeral.push_spawn(*gid);
            }
            for gid in &self.persisted.troops {
                self.ephemeral.push_spawn(*gid);
            }
            let actions: SmallVec<[GroupId; 16]> =
                SmallVec::from_iter(self.persisted.actions.into_iter().map(|g| *g));
            debug!("respawn actions");
            for gid in actions {
                if let Err(e) = self.respawn_action(perf, spctx, idx, gid) {
                    error!("failed to respawn action {e:?}");
                }
            }
            debug!("respawning farps");
            for (_, obj) in self.persisted.objectives.iter_mut_cow() {
                let pos = obj.zone.pos();
                let alt = land.get_height(LuaVec2(pos))? + 50.;
                obj.threat_pos3 = Vector3::new(pos.x, alt, pos.y);
                if let ObjectiveKind::Farp {
                    spec: _,
                    mobile: _,
                    pad_template,
                } = &obj.kind
                {
                    if let Some(uid) = self.persisted.units_by_name.get(pad_template)
                        && let Some(unit) = self.persisted.units.get(uid)
                    {
                        self.ephemeral.push_spawn(unit.group);
                    } else {
                        spctx
                            .move_farp_pad(idx, obj.owner, &pad_template, pos)
                            .context("moving farp pad")?;
                    }
                    self.ephemeral.set_pad_template_used(pad_template.clone());
                }
                if let Some(groups) = obj.groups.get(&obj.owner) {
                    for gid in groups {
                        let group = group!(self, gid)?;
                        if obj.kind.is_farp() || group.class.is_services() {
                            self.ephemeral.push_spawn(*gid)
                        }
                    }
                }
                // spawn left behind base defenses
                if let Some(groups) = obj.groups.get(&obj.owner.opposite()) {
                    for gid in groups {
                        if group_health!(self, gid)?.0 > 0 {
                            self.ephemeral.push_spawn(*gid);
                        }
                    }
                }
            }
            Ok(())
        };
        spawn_deployed_and_logistics().context("spawning deployed and logistics")?;

        // spawn everything before setting up warehouses, so that ship warehouses will also be set up correctly
        while self.ephemeral.spawnq_len() > 0 {
            self.ephemeral.process_spawn_queue(perf, &self.persisted, Utc::now(), idx, spctx)?
        }
        self.setup_warehouses_after_load(spctx.lua())
            .context("setting up warehouses")?;
        let mut mark_deployed_and_logistics = || -> Result<()> {
            let groups = self
                .persisted
                .groups
                .into_iter()
                .map(|(gid, _)| *gid)
                .collect::<Vec<_>>();
            for gid in groups {
                self.mark_group(&gid)?
            }
            for (_, obj) in &self.persisted.objectives {
                self.ephemeral.create_objective_markup(&self.persisted, obj)
            }
            Ok(())
        };
        mark_deployed_and_logistics().context("marking deployed and logistics")?;
        let net = Net::singleton(lua)?;
        let act = Trigger::singleton(lua)?.action()?;
        // spawn all the markup
        while self.ephemeral.msgs.len() > 0 {
            self.ephemeral.msgs.process(100, &net, &act);
        }
        let mut queue_check_close_enemies = || -> Result<()> {
            for (uid, unit) in &self.persisted.units {
                if !unit.dead {
                    self.ephemeral
                        .units_potentially_close_to_enemies
                        .insert(*uid);
                }
            }
            Ok(())
        };
        queue_check_close_enemies().context("queuing unit pos checks")?;
        self.cull_or_respawn_objectives(spctx.lua(), landcache, Utc::now())
            .context("initial cull or respawn")?;
        // return lives to pilots who were airborne on the last restart
        let airborne_players = self
            .persisted
            .players
            .into_iter()
            .filter_map(|(ucid, p)| p.airborne.and_then(|lt| Some((ucid.clone(), lt))))
            .collect::<Vec<_>>();
        for (ucid, lt) in airborne_players {
            let player = &mut self.persisted.players[&ucid];
            player.airborne = None;
            if let Some((_, lives)) = player.lives.get_mut_cow(&lt) {
                *lives += 1;
                if *lives >= self.ephemeral.cfg.default_lives[&lt].0 {
                    player.lives.remove_cow(&lt);
                }
                self.ephemeral.stat(Stat::Life {
                    id: ucid,
                    lives: player.lives.clone(),
                });
                self.ephemeral.dirty();
            }
        }
        Ok(())
    }
}
