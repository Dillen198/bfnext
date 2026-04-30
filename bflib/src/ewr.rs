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

EWR SYSTEM CONFIGURATION:
The EWR system supports two modes controlled by the 'ewr_mode' configuration option:
- EwrMode::Original: Original implementation with immediate track updates and complex reporting timing
- EwrMode::Delayed: Modified implementation with configurable delay on track updates and simplified reporting

The delay is controlled by the 'ewr_delay' configuration option (in seconds, default: 60).
The default mode is EwrMode::Original to maintain backward compatibility.
*/

use crate::{
    db::{
        Db,
        player::{InstancedPlayer, Player},
    },
    landcache::LandCache,
};
use anyhow::Result;
use bfprotocols::{
    cfg::EwrMode,
    stats::{DetectionSource, EnId, Stat},
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{
    MizLua, Position3, Vector2, Vector3, azumith2d_to, azumith3d, azumith3d_to, coalition::Side,
    land::Land, net::Ucid, radians_to_degrees,
};
use fxhash::FxHashMap;
use smallvec::{SmallVec, smallvec};
use std::fmt;

/// Bitmask recording which sensor type(s) detected a track.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DetectedBy(u8);

impl DetectedBy {
    pub const GROUND: DetectedBy = DetectedBy(0b01);
    pub const AIRBORNE: DetectedBy = DetectedBy(0b10);

    pub fn with(self, other: DetectedBy) -> Self {
        DetectedBy(self.0 | other.0)
    }

    fn label(self) -> &'static str {
        match self.0 {
            0b01 => "[G]",
            0b10 => "[A]",
            _    => "[GA]",
        }
    }
}

/// Aspect of the contact relative to the observing player.
/// HOT = nose-on, COLD = tail-on, FLANK/BEAM = crossing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Aspect {
    Hot,
    FlankLeft,
    FlankRight,
    BeamLeft,
    BeamRight,
    Cold,
}

impl Aspect {
    /// Compute aspect from the contact's heading and the bearing from player to contact.
    /// `bearing_to_contact` and `contact_heading` are both in degrees (0–360, north-up).
    /// `pos` is the player's 2D position, `cpos` is the contact's 2D position (DCS XZ plane).
    fn compute(bearing_to_contact: f64, contact_heading: f64, pos: Vector2, cpos: Vector2) -> Self {
        // Reciprocal: direction FROM contact TO player
        let reciprocal = (bearing_to_contact + 180.0) % 360.0;
        // Angular difference between contact heading and the line back to the player (0–180°)
        let mut diff = (contact_heading - reciprocal).abs() % 360.0;
        if diff > 180.0 {
            diff = 360.0 - diff;
        }
        // Left/right: 2D cross product of contact heading vector × contact-to-player vector.
        // In DCS: north=+Z, east=+X, Vector2.x=world_x, Vector2.y=world_z.
        let hdg_rad = contact_heading.to_radians();
        let hx = hdg_rad.sin(); // east component of heading
        let hz = hdg_rad.cos(); // north component of heading
        let ctp_x = pos.x - cpos.x;
        let ctp_z = pos.y - cpos.y; // Vector2.y is world Z
        // cross > 0 → player is to the LEFT of contact's heading
        let cross = hx * ctp_z - hz * ctp_x;
        let left = cross > 0.0;
        match diff as u32 {
            0..=30   => Aspect::Hot,
            31..=60  => if left { Aspect::FlankLeft  } else { Aspect::FlankRight  },
            61..=120 => if left { Aspect::BeamLeft   } else { Aspect::BeamRight   },
            _        => if diff >= 150.0 { Aspect::Cold }
                        else if left { Aspect::FlankLeft } else { Aspect::FlankRight },
        }
    }

    fn label(self) -> &'static str {
        match self {
            Aspect::Hot        => "HOT      ",
            Aspect::FlankLeft  => "FLANK L  ",
            Aspect::FlankRight => "FLANK R  ",
            Aspect::BeamLeft   => "BEAM  L  ",
            Aspect::BeamRight  => "BEAM  R  ",
            Aspect::Cold       => "COLD     ",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GibBraa {
    pub bearing: u16,
    pub range: u32,
    pub altitude: u32,
    pub heading: u16,
    pub speed: u16,
    pub age: u16,
    pub aspect: Aspect,
    pub units: EwrUnits,
    pub stale: bool,
    pub detected_by: DetectedBy,
    converted: bool,
}

/// Age at which a track is considered stale (data may be old) but still reported.
pub const STALE_AGE_SECS: i64 = 60;
/// Age at which a track is dropped from the table entirely.
pub const DROP_AGE_SECS: i64 = 120;

pub const HEADER: &'static str = "     BRG      RNG      ALT      SPD        HDG      AGE  ASPECT    SRC";

impl fmt::Display for GibBraa {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (range_u, altitude_u, speed_u) = match self.units {
            EwrUnits::Imperial => ("nm", "ft", "kts "),
            EwrUnits::Metric => ("km", "m ", "km/h"),
        };
        let stale = if self.stale { "*" } else { " " };
        write!(
            f,
            "{}{:>6} {:>6}{} {:>6}{} {:>6}{} {:>6} {:>6}s {} {}",
            stale,
            self.bearing,
            self.range,
            range_u,
            self.altitude,
            altitude_u,
            self.speed,
            speed_u,
            self.heading,
            self.age,
            self.aspect.label(),
            self.detected_by.label(),
        )
    }
}

impl GibBraa {
    fn convert(&mut self, unit: EwrUnits) {
        if self.converted {
            return;
        }
        self.converted = true;
        match unit {
            EwrUnits::Metric => {
                self.range = self.range / 1000;
                self.speed = ((((self.speed as f64) * 3.6) / 100.0).round() * 100.0) as u16;
                if self.altitude < 1000 {
                    self.altitude = ((self.altitude as f64 / 100.0).round() * 100.0) as u32;
                } else {
                    self.altitude = ((self.altitude as f64 / 1000.0).round() * 1000.0) as u32;
                }
            }
            EwrUnits::Imperial => {
                self.range = self.range / 1852;
                self.altitude = (self.altitude as f64 * 3.38084) as u32;
                self.speed = ((((self.speed as f64) * 1.94384) / 100.0).round() * 100.0) as u16;
                if self.altitude < 1000 {
                    self.altitude = ((self.altitude as f64 / 100.0).round() * 100.0) as u32;
                } else {
                    self.altitude = ((self.altitude as f64 / 1000.0).round() * 1000.0) as u32;
                }
            }
        }
        self.units = unit;
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct Track {
    pos: Position3,
    velocity: Vector3,
    last: DateTime<Utc>,
    last_update: DateTime<Utc>,
    side: Side,
    detected_by: DetectedBy,
    was_detected: bool,
    detected: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum EwrUnits {
    Imperial,
    Metric,
}

impl Default for EwrUnits {
    fn default() -> Self {
        Self::Metric
    }
}

#[derive(Debug, Clone, Copy)]
struct PlayerState {
    enabled: bool,
    units: EwrUnits,
    last: DateTime<Utc>,
    last_spike_warned: DateTime<Utc>,
}

impl Default for PlayerState {
    fn default() -> Self {
        Self {
            enabled: true,
            units: EwrUnits::default(),
            last: DateTime::default(),
            last_spike_warned: DateTime::default(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Ewr {
    tracks: FxHashMap<Side, FxHashMap<EnId, Track>>,
    player_state: FxHashMap<Ucid, PlayerState>,
    /// Snapshot of all active radar donors, rebuilt each tick in update_tracks.
    /// Stored so spike_warnings can query enemy donors without re-iterating db.
    donor_snapshot: Vec<crate::db::RadarDonor>,
}

impl Ewr {
    pub fn update_tracks(
        &mut self,
        lua: MizLua,
        landcache: &mut LandCache,
        db: &Db,
        now: DateTime<Utc>,
        ewr_mode: EwrMode,
        ewr_delay: u32,
    ) -> Result<()> {
        let land = Land::singleton(lua)?;
        let aircraft: SmallVec<[(EnId, Side, Position3, Vector3); 128]> = {
            let players = db
                .instanced_players()
                .filter(|(_, _, inst)| inst.in_air)
                .map(|(ucid, player, inst)| {
                    (
                        EnId::Player(*ucid),
                        player.side,
                        inst.position,
                        inst.velocity,
                    )
                });
            let actions = db
                .persisted
                .actions
                .into_iter()
                .filter_map(|gid| db.persisted.groups.get(gid))
                .flat_map(|sg| {
                    sg.units
                        .into_iter()
                        .filter_map(|uid| db.persisted.units.get(uid).map(|u| (*uid, u)))
                        .filter_map(|(uid, su)| {
                            su.airborne_velocity
                                .map(|v| (EnId::Unit(uid), sg.side, su.position, v))
                        })
                });
            players.chain(actions).collect()
        };
        for tracks in self.tracks.values_mut() {
            for track in tracks.values_mut() {
                track.detected = false;
                track.detected_by = DetectedBy::default();
            }
        }

        // Snapshot donors for spike_warnings use later in the same tick
        self.donor_snapshot = db.radar_donors().collect();

        for donor in &self.donor_snapshot {
            let range_sq = (donor.range as f64).powi(2);
            let tracks = self.tracks.entry(donor.side).or_default();
            let mut donor_pos = donor.pos.p.0;
            donor_pos.y += 10.; // factor in antenna height
            let sensor = if donor.airborne { DetectedBy::AIRBORNE } else { DetectedBy::GROUND };
            for (id, obj_side, pos, velocity) in &aircraft {
                let track = tracks.entry(*id).or_default();
                if track.last != now {
                    let dist_sq = na::distance_squared(&donor_pos.into(), &pos.p.0.into());
                    if dist_sq <= range_sq {
                        let in_cone = match donor.aspect_half_angle {
                            None => true,
                            Some(half_deg) => {
                                let donor_heading = azumith3d(donor.pos.x.0);
                                let bearing_to_target = azumith3d_to(donor_pos, pos.p.0);
                                let mut diff = (bearing_to_target - donor_heading).abs();
                                if diff > std::f64::consts::PI {
                                    diff = 2. * std::f64::consts::PI - diff;
                                }
                                diff <= (half_deg as f64).to_radians()
                            }
                        };
                        if in_cone && landcache.is_visible(&land, dist_sq.sqrt(), donor_pos, pos.p.0)? {
                            match ewr_mode {
                                EwrMode::Original => {
                                    track.pos = *pos;
                                    track.velocity = *velocity;
                                    track.last_update = now;
                                }
                                EwrMode::Delayed => {
                                    let time_since_update = (now - track.last_update).num_seconds();
                                    if time_since_update >= ewr_delay as i64 || track.last_update == DateTime::<Utc>::UNIX_EPOCH {
                                        track.pos = *pos;
                                        track.velocity = *velocity;
                                        track.last_update = now;
                                    }
                                }
                            }
                            track.last = now;
                            track.side = *obj_side;
                            if donor.side != *obj_side {
                                track.detected = true;
                                track.detected_by = track.detected_by.with(sensor);
                            }
                        }
                    }
                }
            }
        }

        // BFT: register friendly airborne players in their own side's track table
        // so that "Friendly Report" always has current data even without a ground EWR.
        for (ucid, player, inst) in db.instanced_players().filter(|(_, _, i)| i.in_air) {
            let tracks = self.tracks.entry(player.side).or_default();
            let id = EnId::Player(*ucid);
            let track = tracks.entry(id).or_default();
            if track.last != now {
                track.pos = inst.position;
                track.velocity = inst.velocity;
                track.last = now;
                track.last_update = now;
                track.side = player.side;
                // detected/detected_by stay false/empty for own-side entries
            }
        }

        for tracks in self.tracks.values_mut() {
            for (id, track) in tracks.iter_mut() {
                if track.was_detected != track.detected {
                    track.was_detected = track.detected;
                    db.ephemeral.stat(Stat::Detected {
                        id: *id,
                        detected: track.was_detected,
                        source: DetectionSource::EWR,
                    })
                }
            }
        }
        Ok(())
    }

    pub fn toggle(&mut self, ucid: &Ucid) -> bool {
        let st = self.player_state.entry(ucid.clone()).or_default();
        st.enabled = !st.enabled;
        st.enabled
    }

    pub fn set_units(&mut self, ucid: &Ucid, units: EwrUnits) {
        self.player_state.entry(ucid.clone()).or_default().units = units;
    }

    pub fn where_chicken(
        &mut self,
        now: DateTime<Utc>,
        friendly: bool,
        force: bool,
        ucid: &Ucid,
        player: &Player,
        inst: &InstancedPlayer,
        ewr_mode: EwrMode,
        ewr_delay: u32,
    ) -> SmallVec<[GibBraa; 64]> {
        let side = player.side;
        let pos = Vector2::new(inst.position.p.x, inst.position.p.z);
        let mut reports: SmallVec<[GibBraa; 64]> = smallvec![];
        let tracks = match self.tracks.get_mut(&side) {
            Some(t) => t,
            None => return reports,
        };
        let state = self.player_state.entry(ucid.clone()).or_default();
        if !force && !state.enabled {
            return reports;
        }
        let ownship = EnId::Player(*ucid);
        tracks.retain(|tucid, track| {
            let age = (now - track.last).num_seconds();
            let include = (friendly && track.side == side) || (!friendly && track.side != side);
            if include && age <= DROP_AGE_SECS && tucid != &ownship {
                let cpos = Vector2::new(track.pos.p.x, track.pos.p.z);
                let range = na::distance(&pos.into(), &cpos.into());
                let bearing = radians_to_degrees(azumith2d_to(pos, cpos));
                let heading = radians_to_degrees(azumith3d(track.pos.x.0));
                let speed = track.velocity.magnitude();
                let altitude = track.pos.p.y;
                let aspect = Aspect::compute(bearing, heading, pos, cpos);
                reports.push(GibBraa {
                    range: range as u32,
                    heading: heading as u16,
                    altitude: altitude as u32,
                    bearing: bearing as u16,
                    age: age as u16,
                    speed: speed as u16,
                    aspect,
                    units: EwrUnits::Metric,
                    stale: age >= STALE_AGE_SECS,
                    detected_by: track.detected_by,
                    converted: false,
                })
            }
            age <= DROP_AGE_SECS
        });
        if reports.is_empty() {
            return reports;
        }
        reports.sort_by_key(|r| r.range);
        while reports.len() > 10 {
            reports.pop();
        }
        let since_last = (now - state.last).num_seconds();
        // For proximity-based auto-reports, skip stale tracks as the threat may have moved.
        let closest_fresh = reports.iter().find(|r| !r.stale);
        match ewr_mode {
            EwrMode::Original => {
                let urgent = closest_fresh.map_or(false, |r| {
                    (r.range <= 20000 && r.age <= 10)
                        || (r.range <= 40000 && r.age <= 10 && since_last >= 30)
                });
                if force || since_last >= 60 || urgent {
                    state.last = now;
                    reports.iter_mut().for_each(|r| r.convert(state.units));
                    reports
                } else {
                    smallvec![]
                }
            }
            EwrMode::Delayed => {
                if force || since_last >= ewr_delay as i64 {
                    state.last = now;
                    reports.iter_mut().for_each(|r| r.convert(state.units));
                    reports
                } else {
                    smallvec![]
                }
            }
        }
    }

    /// Check whether any enemy radar donor has the player in its detection cone.
    /// Returns a warning message for each spiking radar (max one per enemy donor type
    /// per 30-second window to avoid spam). Only fires when the player is in the air.
    pub fn spike_warnings(
        &mut self,
        now: DateTime<Utc>,
        ucid: &Ucid,
        player: &Player,
        inst: &InstancedPlayer,
    ) -> SmallVec<[CompactString; 4]> {
        let mut warnings: SmallVec<[CompactString; 4]> = smallvec![];
        if !inst.in_air {
            return warnings;
        }
        let state = self.player_state.entry(ucid.clone()).or_default();
        let since_last = (now - state.last_spike_warned).num_seconds();
        if since_last < 30 {
            return warnings;
        }
        let player_pos = inst.position.p.0;
        let mut spiked = false;
        for donor in &self.donor_snapshot {
            if donor.side == player.side {
                continue;
            }
            let half_angle = match donor.aspect_half_angle {
                // Omnidirectional donors don't produce a directional spike
                None => continue,
                Some(h) => h,
            };
            let range_sq = (donor.range as f64).powi(2);
            let donor_pos = donor.pos.p.0;
            let dist_sq = na::distance_squared(&donor_pos.into(), &player_pos.into());
            if dist_sq > range_sq {
                continue;
            }
            let donor_heading = azumith3d(donor.pos.x.0);
            let bearing_to_player = azumith3d_to(donor_pos, player_pos);
            let mut diff = (bearing_to_player - donor_heading).abs();
            if diff > std::f64::consts::PI {
                diff = 2. * std::f64::consts::PI - diff;
            }
            if diff <= (half_angle as f64).to_radians() {
                spiked = true;
                let label = if donor.airborne { "airborne radar" } else { "ground radar" };
                warnings.push(format_compact!("⚠ SPIKE: enemy {label} has you in cone"));
            }
        }
        if spiked {
            state.last_spike_warned = now;
        }
        warnings
    }
}
