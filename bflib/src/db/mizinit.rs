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
    spawnctx::{SpawnCtx, SpawnLoc},
    unit, unit_mut,
};
use anyhow::{Context, Result, anyhow, bail};
use bfprotocols::{
    cfg::{Cfg, Vehicle},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::PerfInner,
    stats::Stat,
};
use chrono::prelude::*;
use compact_str::CompactString;
use dcso3::{
    centroid2d, coalition::Side, controller::PointType, coord::Coord, env::miz::{Group, Miz, MizIndex, Skill, TriggerZone, TriggerZoneTyp}, land::Land, net::Net, trigger::Trigger, LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3
};
use enumflags2::BitFlags;
use fxhash::FxHashSet;
use log::{debug, error, info};
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
        } else {
            bail!("invalid objective type for {name}, expected AB, FO, LO, NB, CG, or FAC")
        };
        let id = ObjectiveId::new();
        let mut logistics_detached = false;
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
            supply: 0,
            fuel: 0,
            last_change_ts: Utc::now(),
            last_threatened_ts: Utc::now(),
            warehouse: Warehouse::default(),
            points: 0,
            logistics_detached,
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
                        let already_exists = self.persisted.groups.into_iter()
                            .any(|(_, g)| g.template_name.starts_with(name.as_str()));
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
                                    supply: 100,
                                    fuel: 100,
                                    last_change_ts: Utc::now(),
                                    last_threatened_ts: Utc::now(),
                                    warehouse: Warehouse::default(),
                                    points: 0,
                                    logistics_detached: true,
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

    fn init_carrier_groups(&mut self) -> Result<()> {
        use super::objective::ObjGroupClass;

        info!("[CARRIER_INIT] Starting init_carrier_groups");

        // Carrier groups are initialized by spawning their template groups
        // Template groups should be named like: BCARRIER, RCARRIER, etc.
        // They match carrier group objectives by the side prefix (B/R)

        // Find all groups with CARRIER in the name (these are the carrier task force templates)
        let carrier_template_groups: Vec<(GroupId, Side, String)> = self
            .persisted
            .groups
            .into_iter()
            .filter_map(|(gid, group)| {
                if group.name.contains("CARRIER") && matches!(group.class, ObjGroupClass::Naval) {
                    info!("[CARRIER_INIT] Found carrier template group: {} (side: {:?})", group.name, group.side);
                    Some((*gid, group.side, group.name.clone()))
                } else {
                    None
                }
            })
            .collect();

        info!("[CARRIER_INIT] Found {} carrier template groups total", carrier_template_groups.len());
        info!("[CARRIER_INIT] Found {} carrier group objectives total", self.persisted.carrier_groups.len());

        // Match carrier template groups to carrier group objectives
        let carrier_ids: Vec<ObjectiveId> = self.persisted.carrier_groups.into_iter().copied().collect();
        info!("[CARRIER_INIT] Carrier group IDs: {:?}", carrier_ids);

        for cg_id in &carrier_ids {
            info!("[CARRIER_INIT] About to process carrier group ID: {:?}", cg_id);
            let obj = objective!(self, cg_id)?;
            let cg_name = obj.name.clone();
            let cg_owner = obj.owner;

            info!("[CARRIER_INIT] Processing carrier group: {} (owner: {:?})", cg_name, cg_owner);
            info!("[CARRIER_INIT] Available template groups: {:?}", carrier_template_groups);

            // Find all carrier template groups that match this carrier group's side
            let matching_groups: Vec<(GroupId, String)> = carrier_template_groups
                .iter()
                .filter_map(|(gid, side, name)| {
                    info!("[CARRIER_INIT] Checking template {} (side: {:?}) against {} (owner: {:?})", name, side, cg_name, cg_owner);
                    if *side == cg_owner {
                        info!("[CARRIER_INIT] MATCH! Adding {} to carrier group {}", name, cg_name);
                        Some((*gid, name.clone()))
                    } else {
                        None
                    }
                })
                .collect();

            if matching_groups.is_empty() {
                info!("[CARRIER_INIT] No carrier template groups found for {} ({:?})", cg_name, cg_owner);
                continue;
            }

            // Assign all matching groups to this carrier group and queue them for spawning
            let mut carrier_template_set = false;
            for (gid, group_name) in &matching_groups {
                let obj = objective_mut!(self, cg_id)?;
                obj.groups.get_or_default_cow(cg_owner).insert_cow(*gid);
                info!("[CARRIER_INIT] After insert, objective {} has groups: {:?}", obj.name, obj.groups);
                self.persisted.objectives_by_group.insert_cow(*gid, *cg_id);

                // Update the group's origin
                let group = group_mut!(self, gid)?;
                group.origin = DeployKind::Objective { origin: *cg_id };

                // Queue group for spawning
                self.ephemeral.push_spawn(*gid);

                // If this group has CARRIER in the name (not just escort/supply), set carrier_template
                // Use the TEMPLATE name (e.g. "BCARRIER") not the bflib group name (e.g. "BCARRIER-490"),
                // since Group.activate() keeps the original DCS group name for carrier_waypoint lookups
                if !carrier_template_set && group_name.contains("CARRIER") && !group_name.contains("ESCORT") && !group_name.contains("SUPPLY") {
                    let template_name = group!(self, gid)?.template_name.clone();
                    let obj = objective_mut!(self, cg_id)?;
                    if let ObjectiveKind::CarrierGroup { carrier_template, .. } = &mut obj.kind {
                        *carrier_template = template_name.clone();
                        carrier_template_set = true;
                        info!("[CARRIER_INIT] Set carrier_template for {} to DCS template name {}", cg_name, template_name);
                    }
                }
            }

            info!("[CARRIER_INIT] {} initialized with {} naval groups (owner: {:?})",
                  cg_name,
                  matching_groups.len(),
                  cg_owner);
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

        Ok(())
    }

    pub fn init_objective_slots(&mut self, side: Side, slot: Group) -> Result<()> {
        let mut ground_start = false;
        let mut has_link_unit = false;

        for point in slot.route()?.points()? {
            let point = point?;
            match point.typ {
                PointType::TakeOffGround | PointType::TakeOffGroundHot => ground_start = true,
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
            let coa = miz.coalition(side)?;
            for zone in miz.triggers()? {
                let zone = zone?;
                let name = zone.name()?;
                if let Some(name) = name.strip_prefix("G") {
                    let (template_side, name) = name.parse::<ObjGroup>()?.template(side);
                    if template_side == side {
                        t.init_objective_group(&spctx, idx, miz, zone, side, name.as_str())?
                    }
                } else if name.starts_with("T") || name.starts_with("O") {
                    () // ignored
                } else {
                    bail!("invalid trigger zone type code {name}, expected O, G, or T prefix")
                }
            }
        }

        // Index carrier template groups BEFORE slot initialization
        // This ensures carrier objectives exist so slots on carriers can be associated with them
        info!("[CARRIER_SETUP] About to init carrier template groups");
        t.init_carrier_template_groups(&spctx, idx, miz, lua, true)
            .context("init_carrier_template_groups failed")?;
        info!("[CARRIER_SETUP] About to init carrier groups");
        t.init_carrier_groups()
            .context("init_carrier_groups failed")?;
        info!("[CARRIER_SETUP] Carrier initialization complete");

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
            if matches!(obj.kind, ObjectiveKind::CarrierGroup { .. }) {
                continue;
            }
            t.update_objective_status(&id, now)?
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

        // Re-initialize carrier template groups from miz file (they're not persisted, but objectives are)
        info!("[CARRIER_LOAD] Re-indexing carrier template groups after load");
        self.init_carrier_template_groups(spctx, idx, miz, lua, false)
            .context("re-initializing carrier template groups")?;
        self.init_carrier_groups()
            .context("re-initializing carrier groups")?;

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
