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

use anyhow::{Context, Result, anyhow};
use bfprotocols::perf::PerfInner;
use chrono::Utc;
use compact_str::format_compact;
use dcso3::{
    DeepClone, LuaEnv, LuaVec2, LuaVec3, MizLua, String, Vector2, Vector3,
    coalition::{Coalition, Side, Static},
    env::miz::{self, GroupInfo, GroupKind, Miz, MizIndex, TriggerZone},
    group::{Group, GroupCategory},
    land::Land,
    object::{ClassObject, DcsObject, DcsOid, ObjectCategory},
    perf::record_perf,
    unit::Unit,
    world::{SearchVolume, World},
};
use fxhash::FxHashMap;
use log::{info, warn};
use mlua::Value;
use serde_derive::{Deserialize, Serialize};

fn default_speed() -> f64 {
    220.
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SpawnLoc {
    /// only for air units, obviously
    InAir {
        pos: Vector2,
        heading: f64,
        altitude: f64,
        #[serde(default = "default_speed")]
        speed: f64,
    },
    AtPos {
        /// the position of the player. the group will be offset in the
        /// direction offset_direction from this point by the group radius + 10 meters
        pos: Vector2,
        /// this should be a unit vector pointing in the direction
        /// you want to offset the group
        offset_direction: Vector2,
        /// rotate the group to this heading in radians
        group_heading: f64,
    },
    /// like AtPos, but places the group's center exactly at pos with no
    /// added clearance offset. Use this when the caller has already picked
    /// a specific, deconflicted spawn point (e.g. a crate grid scan) and an
    /// extra automatic offset would defeat that placement.
    AtPosExact {
        pos: Vector2,
        /// rotate the group to this heading in radians
        group_heading: f64,
    },
    AtPosWithComponents {
        pos: Vector2,
        /// the position of sub components of the group by unit type
        component_pos: FxHashMap<String, Vector2>,
        /// rotate the group to this heading in radians
        group_heading: f64,
    },
    /// spawn the group as a direct translation from an original (provided) center
    /// to a new center. This is useful if you have statics, or multiple groups,
    /// and you want their relative positions to be preserved
    AtPosWithCenter {
        /// pos is the new center position of the group
        pos: Vector2,
        /// center is the original center of the group
        center: Vector2,
    },
    AtTrigger {
        name: String,
        /// rotate the group to this heading in radians
        group_heading: f64,
    },
}

impl Default for SpawnLoc {
    fn default() -> Self {
        Self::AtPos {
            pos: Vector2::new(0., 0.),
            offset_direction: Vector2::new(0., 0.),
            group_heading: 0.,
        }
    }
}

pub struct SpawnCtx<'lua> {
    coalition: Coalition<'lua>,
    miz: Miz<'lua>,
    lua: MizLua<'lua>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Despawn {
    Group(DcsOid<ClassObject>),
    /// Destroy a group by name when the DCS object ID is not yet tracked (e.g. the
    /// group spawned so recently that no DCS event has fired to populate object_id_by_gid).
    GroupByName(std::string::String),
    Static(String),
    /// Destroy a static object by its DCS object ID. Used when the DCS name may
    /// differ from the bflib name (e.g. after C-130 cargo load/drop renames).
    StaticObject(DcsOid<ClassObject>),
}

#[derive(Debug, Clone)]
pub enum Spawned<'lua> {
    Group(Group<'lua>),
    Static,
}

impl<'lua> SpawnCtx<'lua> {
    pub fn new(lua: MizLua<'lua>) -> Result<Self> {
        Ok(Self {
            coalition: Coalition::singleton(lua)?,
            miz: Miz::singleton(lua)?,
            lua,
        })
    }

    pub fn lua(&self) -> MizLua<'lua> {
        self.lua
    }

    pub fn get_template(
        &self,
        idx: &MizIndex,
        kind: GroupKind,
        side: Side,
        template_name: &str,
    ) -> Result<GroupInfo<'lua>> {
        let mut template = self
            .miz
            .get_group_by_name(idx, kind, side, template_name)?
            .ok_or_else(|| anyhow!("no such template {template_name}"))?;
        template.group = template.group.deep_clone(self.lua.inner())?;
        Ok(template)
    }

    /// get at template that you pinky promise not to modify
    pub fn get_template_ref<'a>(
        &'a self,
        idx: &MizIndex,
        kind: GroupKind,
        side: Side,
        template_name: &str,
    ) -> Result<GroupInfo<'a>> {
        self.miz
            .get_group_by_name(idx, kind, side, template_name)?
            .ok_or_else(|| anyhow!("no such template {template_name}"))
    }

    pub fn get_trigger_zone<'a>(&'a self, idx: &MizIndex, name: &str) -> Result<TriggerZone<'a>> {
        Ok(self
            .miz
            .get_trigger_zone(idx, name)?
            .ok_or_else(|| anyhow!("no such trigger zone {name}"))?)
    }


    pub fn move_farp_pad(
        &self,
        idx: &MizIndex,
        side: Side,
        pad_template: &str,
        pos: Vector2,
    ) -> Result<Spawned<'lua>> {
        let pad = {
            let pad = self
                .get_template(idx, GroupKind::Any, side, &pad_template)
                .context("getting the pad")?;
            pad.group.set("hidden", false)?;
            pad.group.set("lateActivation", false)?;
            let pad_unit = pad
                .group
                .units()
                .context("getting pad units")?
                .get(1)
                .context("getting pad unit")?;
            pad_unit.set_pos(pos).context("setting pad pos")?;
            drop(pad_unit);
            pad
        };
        self.spawn(pad).context("moving the pad")
    }

    pub fn spawn(&self, template: GroupInfo<'lua>) -> Result<Spawned<'lua>> {
        self.spawn_with_link(template, None)
    }

    /// Spawn a group/static with optional linking to a ship unit.
    /// If link_unit_id is provided and the template is a static object,
    /// the static will be linked to that ship and move with it.
    pub fn spawn_with_link(&self, template: GroupInfo<'lua>, link_unit_name: Option<String>) -> Result<Spawned<'lua>> {
        match GroupCategory::from_kind(template.category) {
            Some(category) => Ok(Spawned::Group(
                self.coalition
                    .add_group(template.country, category, template.group.clone())
                    .with_context(|| {
                        format_compact!("spawning group from template {:?}", template)
                    })?,
            )),
            None => {
                // static objects are not fed to addStaticObject as groups
                let unit: miz::Unit<'lua> = template
                    .group
                    .units()
                    .context("getting static group units")?
                    .first()
                    .context("getting first unit in static group")?
                    .clone();
                // If linking to a ship, compute the offset from the carrier's position
                // to the crate's intended position, then set the linkUnit field.
                // This is non-fatal: if linking fails, we still spawn the crate at its
                // world position so it's visible to the player.
                if let Some(ref name) = link_unit_name {
                    let link_result = (|| -> Result<()> {
                        let crate_pos = unit.pos().context("getting crate template position")?;
                        let carrier_unit = Unit::get_by_name(self.lua, name)
                            .with_context(|| format_compact!("getting carrier unit '{}'", name))?;
                        let carrier_pos_3d = carrier_unit.get_point()
                            .context("getting carrier unit position")?;
                        // DCS 3D (x,y,z) -> miz 2D (x,y): miz.x = 3d.x, miz.y = 3d.z
                        let offset_x = crate_pos.x - carrier_pos_3d.0.x;
                        let offset_y = crate_pos.y - carrier_pos_3d.0.z;
                        info!("[CARRIER_LINK] Linking static to ship '{}' (crate pos: {:.0},{:.0}, carrier pos: {:.0},{:.0}, offset: {:.1},{:.1})",
                              name, crate_pos.x, crate_pos.y, carrier_pos_3d.0.x, carrier_pos_3d.0.z, offset_x, offset_y);
                        unit.set_link_unit(name, offset_x, offset_y)
                            .context("setting linkUnit")?;
                        Ok(())
                    })();
                    match link_result {
                        Ok(()) => {},
                        Err(e) => {
                            warn!("[CARRIER_LINK] Failed to link static to carrier '{}': {:#}. Spawning without link.", name, e);
                        }
                    }
                }
                self.coalition
                    .add_static_object(template.country, unit)
                    .with_context(|| {
                        format_compact!("spawning static object from template {:?}", template)
                    })?;
                Ok(Spawned::Static)
            }
        }
    }

    pub fn despawn(&self, perf: &mut PerfInner, name: Despawn) -> Result<()> {
        let ts = Utc::now();
        match name {
            Despawn::Group(oid) => {
                match dcso3::object::Object::get_instance(self.lua, &oid) {
                    Ok(obj) => {
                        match obj.as_unit() {
                            Ok(unit) => {
                                match unit.get_group() {
                                    Ok(group) => group.destroy()?,
                                    Err(e) => info!("attempt to despawn unit without group {e:?}"),
                                }
                            }
                            Err(_) => {
                                // oid is a Group, not a unit — destroy directly
                                if let Err(e) = obj.destroy() {
                                    info!("attempt to despawn group directly failed: {e:?}");
                                }
                            }
                        }
                    }
                    Err(e) => info!("attempt to despawn invalid object {e:?}"),
                }
                record_perf(&mut perf.despawn, ts);
                Ok(())
            }
            Despawn::GroupByName(name) => {
                match Group::get_by_name(self.lua, &*name) {
                    Ok(group) => {
                        if let Err(e) = group.destroy() {
                            info!("attempt to despawn group by name '{}' failed: {e:?}", name);
                        }
                    }
                    Err(e) => info!("attempt to despawn unknown group by name '{}' {e:?}", name),
                }
                record_perf(&mut perf.despawn, ts);
                Ok(())
            }
            Despawn::Static(name) => {
                match dcso3::static_object::StaticObject::get_by_name(self.lua, &*name) {
                    Ok(Static::Airbase(obj)) => obj.destroy()?,
                    Ok(Static::Static(obj)) => obj.destroy()?,
                    Err(e) => info!("attempt to despawn unknown static {} {}", name, e),
                }
                record_perf(&mut perf.despawn, ts);
                Ok(())
            }
            Despawn::StaticObject(oid) => {
                match dcso3::object::Object::get_instance(self.lua, &oid) {
                    Ok(obj) => obj.destroy()?,
                    Err(e) => info!("attempt to despawn static by oid {:?} {}", oid, e),
                }
                record_perf(&mut perf.despawn, ts);
                Ok(())
            }
        }
    }

    /*
    pub fn remove_junk(&self, point: Vector2, radius: f64) -> Result<()> {
        let alt = Land::singleton(self.lua)?.get_height(LuaVec2(point))?;
        let point = LuaVec3(Vector3::new(point.x, alt, point.y));
        let vol = SearchVolume::Sphere { point, radius };
        World::singleton(self.lua)?.remove_junk(vol)?;
        Ok(())
    }
    */

    pub fn remove_scenery(&self, point: Vector2, radius: f64) -> Result<()> {
        let alt = Land::singleton(self.lua)?.get_height(LuaVec2(point))?;
        let point = LuaVec3(Vector3::new(point.x, alt, point.y));
        let vol = SearchVolume::Sphere { point, radius };
        World::singleton(self.lua)?.search_objects(
            ObjectCategory::Scenery,
            vol,
            Value::Nil,
            |_, o, _| {
                o.destroy()?;
                Ok(true)
            },
        )?;
        Ok(())
    }
}
