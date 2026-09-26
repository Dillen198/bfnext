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

CARP (Computed Air Release Point) support for the C-130J CNI-MU airdrop
pages. This module does NOT replicate the CNI-MU's ballistics solution
(CARP INIT 5/5) -- that's computed onboard the aircraft from its own
G-12D/G-12E parachute database once given load weight/chutes. What this
module supplies is the real-mission data the CNI-MU can't get on its
own: the PI's exact position (from an F10 map mark), terrain elevation
around it, and wind/temperature sampled at the surface and at the drop
altitude -- i.e. the auto-fillable fields on CARP INIT 1/5, 3/5 and 4/5.
See the field-by-field breakdown in the DCS C-130J User Manual, CNI-MU
section, "Computed Air Release Point (CARP) Overview" onward.
*/

use crate::atis::{temp_pressure_at, wind_at};
use anyhow::{anyhow, Result};
use dcso3::{coord::Coord, land::Land, world::World, LuaVec2, MizLua, Vector2};
use serde_derive::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct CarpSolution {
    pub pi_lat: f64,
    pub pi_lon: f64,
    pub pi_mgrs: std::string::String,
    pub pi_elevation_ft: f64,
    pub dz_elevation_ft: f64,
    pub obstr_elevation_ft: f64,
    pub drop_altitude_ft: f64,
    pub alt_wind_dir_deg: f64,
    pub alt_wind_speed_kt: f64,
    pub sfc_wind_dir_deg: f64,
    pub sfc_wind_speed_kt: f64,
    pub bal_wind_dir_deg: f64,
    pub bal_wind_speed_kt: f64,
    pub alt_temp_c: f64,
    pub sfc_temp_c: f64,
}

const M_TO_FT: f64 = 3.28084;
const MS_TO_KT: f64 = 1.94384;
/// Surface wind reference height above the PI. getWind at or below the
/// terrain mesh returns nothing useful, and 10 m is the standard surface-wind
/// height anyway (the ATIS samples the same).
const SURFACE_WIND_AGL_M: f64 = 10.0;

/// Live DCS wind at a point: (FROM bearing, degrees TRUE; knots). This had
/// its own copy with the atan2 arguments swapped -- DCS x is north and z is
/// east -- which reported every wind mirrored about the NE-SW axis (a
/// westerly came out as a southerly). It now reuses the ATIS reading.
fn wind_kt(lua: MizLua, x: f64, y: f64, z: f64) -> Result<(f64, f64)> {
    let (from_deg, speed_ms) = wind_at(lua, x, y, z)?;
    Ok((from_deg, speed_ms * MS_TO_KT))
}

// Vector-average of two "wind from" direction/speed pairs, matching the
// CNI-MU's BAL W/V (CARP INIT 3/5, L4): the vector average of ALT W/V and
// SFC W/V.
fn vector_avg(a: (f64, f64), b: (f64, f64)) -> (f64, f64) {
    let to_components = |dir_from_deg: f64, spd: f64| {
        let dir_to = (dir_from_deg + 180.0).to_radians();
        (spd * dir_to.sin(), spd * dir_to.cos())
    };
    let (ax, ay) = to_components(a.0, a.1);
    let (bx, by) = to_components(b.0, b.1);
    let mx = (ax + bx) / 2.0;
    let my = (ay + by) / 2.0;
    let spd = (mx * mx + my * my).sqrt();
    let dir_to = mx.atan2(my).to_degrees();
    let dir_from = (dir_to + 180.0).rem_euclid(360.0);
    (dir_from, spd)
}

// Approximates DZ ELEV (highest elevation within the drop zone) and
// OBSTR ELEV (highest obstacle in the drop zone area) by sampling
// terrain height in a ring around the PI, since DCS has no "highest
// point in an area" query and the DZ's actual extent/heading isn't
// known server-side. Returns (dz_elevation_m, obstr_elevation_m).
fn sample_elevations(lua: MizLua, x: f64, z: f64, pi_elev_m: f64) -> Result<(f64, f64)> {
    let land = Land::singleton(lua)?;
    const NEAR_M: f64 = 400.0;
    const FAR_M: f64 = 1200.0;
    let mut dz_elev = pi_elev_m;
    let mut obstr_elev = pi_elev_m;
    for deg in (0..360).step_by(45) {
        let rad = (deg as f64).to_radians();
        let (dx, dz) = (rad.cos(), rad.sin());
        let near = land.get_height(LuaVec2(Vector2::new(x + dx * NEAR_M, z + dz * NEAR_M)))?;
        let far = land.get_height(LuaVec2(Vector2::new(x + dx * FAR_M, z + dz * FAR_M)))?;
        if near > dz_elev {
            dz_elev = near;
        }
        if far > obstr_elev {
            obstr_elev = far;
        }
    }
    Ok((dz_elev, obstr_elev))
}

fn format_mgrs(pos: &dcso3::coord::MGRSPos) -> std::string::String {
    format!(
        "{}{} {:05.0} {:05.0}",
        pos.utm_zone, pos.mgrs_digraph, pos.easting, pos.northing
    )
}

/// Core solve, shared by both cockpit-UI entry points below -- everything
/// past "get a world x/z for the PI" is identical either way.
fn solve_at_world_pos(
    lua: MizLua,
    x: f64,
    z: f64,
    drop_altitude_agl_ft: f64,
) -> Result<CarpSolution> {
    let coord = Coord::singleton(lua)?;
    let ll = coord.lo_to_ll(dcso3::LuaVec3(dcso3::Vector3::new(x, 0.0, z)))?;
    let mgrs = coord.ll_to_mgrs(ll.latitude, ll.longitude)?;

    let pi_elev_m = Land::singleton(lua)?.get_height(LuaVec2(Vector2::new(x, z)))?;
    let (dz_elev_m, obstr_elev_m) = sample_elevations(lua, x, z, pi_elev_m)?;

    let drop_alt_agl_m = drop_altitude_agl_ft / M_TO_FT;
    let drop_alt_y = pi_elev_m + drop_alt_agl_m;

    // Surface wind 10 m above the PI, not at y=0 -- that is underground
    // anywhere above sea level.
    let (sfc_dir, sfc_spd) = wind_kt(lua, x, pi_elev_m + SURFACE_WIND_AGL_M, z)?;
    let (alt_dir, alt_spd) = wind_kt(lua, x, drop_alt_y, z)?;
    let (bal_dir, bal_spd) = vector_avg((alt_dir, alt_spd), (sfc_dir, sfc_spd));

    // Both temperatures live from DCS's atmosphere at the PI and at the drop
    // altitude. They used to be the mission's sea-level season temperature
    // with an ISA lapse on top -- an estimate, and wrong over high ground.
    let (sfc_temp_c, _) = temp_pressure_at(lua, x, pi_elev_m, z)?;
    let (alt_temp_c, _) = temp_pressure_at(lua, x, drop_alt_y, z)?;

    Ok(CarpSolution {
        pi_lat: ll.latitude,
        pi_lon: ll.longitude,
        pi_mgrs: format_mgrs(&mgrs),
        pi_elevation_ft: pi_elev_m * M_TO_FT,
        dz_elevation_ft: dz_elev_m * M_TO_FT,
        obstr_elevation_ft: obstr_elev_m * M_TO_FT,
        drop_altitude_ft: (pi_elev_m + drop_alt_agl_m) * M_TO_FT,
        alt_wind_dir_deg: alt_dir,
        alt_wind_speed_kt: alt_spd,
        sfc_wind_dir_deg: sfc_dir,
        sfc_wind_speed_kt: sfc_spd,
        bal_wind_dir_deg: bal_dir,
        bal_wind_speed_kt: bal_spd,
        alt_temp_c,
        sfc_temp_c,
    })
}

/// Builds a CARP solution for the PI marked on the F10 map with the text
/// `mark_key`, at the given drop altitude (feet AGL).
pub(crate) fn build_carp_solution_from_mark(
    lua: MizLua,
    mark_key: &str,
    drop_altitude_agl_ft: f64,
) -> Result<CarpSolution> {
    let mark = World::singleton(lua)?
        .get_mark_panels()
        .map_err(|e| anyhow!("getting marks: {e:?}"))?
        .into_iter()
        .filter_map(|m| m.ok())
        .find(|m| m.text.as_str() == mark_key)
        .ok_or_else(|| anyhow!("no F10 map mark found with text '{mark_key}'"))?;
    solve_at_world_pos(lua, mark.pos.0.x, mark.pos.0.z, drop_altitude_agl_ft)
}

/// Builds a CARP solution for a PI given directly as lat/long -- e.g. a
/// click on the cockpit dashboard's map, no F10 mark required.
pub(crate) fn build_carp_solution_from_latlon(
    lua: MizLua,
    lat: f64,
    lon: f64,
    drop_altitude_agl_ft: f64,
) -> Result<CarpSolution> {
    let coord = Coord::singleton(lua)?;
    let world_pos = coord.ll_to_lo(dcso3::coord::LLPos { latitude: lat, longitude: lon, altitude: 0.0 })?;
    solve_at_world_pos(lua, world_pos.0.x, world_pos.0.z, drop_altitude_agl_ft)
}
