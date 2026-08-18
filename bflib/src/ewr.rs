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
    cfg::{EwrMode, IadnConfig, RadarBand, RadarPhysicsCfg, SensorType, UnitTag},
    db::group::GroupId,
    stats::{DetectionSource, EnId, Stat},
};
use chrono::prelude::*;
use compact_str::{CompactString, format_compact};
use dcso3::{
    MizLua, Position3, Vector2, Vector3, azumith2d_to, azumith3d, azumith3d_to, coalition::Side,
    land::Land, net::Ucid, radians_to_degrees,
    object::{DcsObject, DcsOid},
    unit::Unit,
    weapon::{ClassWeapon, Weapon},

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

// ─── IADN: Fused multi-sensor air picture ────────────────────────────────────

/// Stable ID for a fused track (survives across multiple detection updates).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FusedTrackId(u64);

impl FusedTrackId {
    fn new() -> Self {
        use std::sync::atomic::{AtomicU64, Ordering};
        static SEQ: AtomicU64 = AtomicU64::new(1);
        Self(SEQ.fetch_add(1, Ordering::Relaxed))
    }
}

/// Coarse classification of a fused contact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContactClass {
    Fighter,
    Bomber,
    Helicopter,
    Unknown,
}

/// Identification-friend-or-foe state for a fused contact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum IffState {
    Friendly,
    Hostile,
    Unknown,
}

/// A fused track combining contributions from multiple sensors.
#[derive(Debug, Clone)]
pub struct FusedTrack {
    pub id: FusedTrackId,
    pub side: Side,
    pub pos: Position3,
    pub velocity: Vector3,
    /// 1-sigma position uncertainty (meters). Decreases with more sensor coverage.
    pub pos_uncertainty_m: f32,
    /// Detection confidence 0.0–1.0; decays without sensor hits.
    pub confidence: f32,
    pub last_detection: DateTime<Utc>,
    pub last_update: DateTime<Utc>,
    pub classification: ContactClass,
    pub iff: IffState,
    /// Accumulated sensor type mask.
    pub detected_by: DetectedBy,
    /// Number of independent sensors currently tracking this contact.
    pub sensor_count: u8,
    /// True when the track is old enough to be marked stale but not yet dropped.
    pub stale: bool,
}

impl FusedTrack {
    fn is_stale(&self, now: DateTime<Utc>, stale_secs: u32) -> bool {
        (now - self.last_detection).num_seconds() >= stale_secs as i64
    }

    fn is_expired(&self, cfg: &IadnConfig) -> bool {
        self.confidence < cfg.sam_cue_confidence_threshold * 0.1
    }
}

// ─── Radar physics ───────────────────────────────────────────────────────────

/// Compute the detection probability for one (donor, target) pair.
/// Returns 0.0–1.0.  The caller does a Bernoulli trial against this value.
fn compute_detection_probability(
    donor_range: u32,
    range_m: f64,
    aspect: Aspect,
    contact_alt_agl_m: f64,
    contact_velocity: Vector3,
    donor_pos: nalgebra::Point3<f64>,
    sensor_type: SensorType,
    pulse_doppler: bool,
    look_down_capable: bool,
    frequency_band: RadarBand,
    cfg: &RadarPhysicsCfg,
) -> f32 {
    // 1. Inverse fourth-power radar range equation.
    let range_factor = ((donor_range as f64) / range_m).powi(4) as f32;

    // 2. Aspect / RCS factor, compressed toward 1.0 for low-frequency radars.
    // VHF/UHF radars operate near the Rayleigh scattering regime for aircraft-sized
    // targets — aspect and shaping matter much less, making stealth less effective.
    let base_rcs = match aspect {
        Aspect::Hot                             => cfg.rcs_hot,
        Aspect::FlankLeft | Aspect::FlankRight  => cfg.rcs_flank,
        Aspect::BeamLeft  | Aspect::BeamRight   => cfg.rcs_beam,
        Aspect::Cold                            => cfg.rcs_cold,
    };
    // band_sharpness: 1.0 = full aspect variation (X-band), lower = compressed toward 1.0
    let band_sharpness = match frequency_band {
        RadarBand::Vhf           => 0.3,
        RadarBand::Uhf           => 0.5,
        RadarBand::Lband         => 0.7,
        RadarBand::Sband         => 0.85,
        RadarBand::Cband         => 0.95,
        RadarBand::Xband | RadarBand::Kuband => 1.0,
    };
    let rcs_factor = 1.0 - (1.0 - base_rcs) * band_sharpness;

    // 3. Doppler notch: beam-aspect target whose closure rate toward the donor
    //    is below the threshold is almost invisible to pulse-Doppler radars.
    let notch_factor = if pulse_doppler
        && matches!(aspect, Aspect::BeamLeft | Aspect::BeamRight)
    {
        let to_donor_dir = (donor_pos.coords - nalgebra::Vector3::new(
            contact_velocity.x, contact_velocity.y, contact_velocity.z,
        )).normalize();
        let closure = contact_velocity.dot(&to_donor_dir).abs() as f32;
        if closure < cfg.notch_closure_threshold_ms {
            cfg.notch_attenuation
        } else {
            1.0
        }
    } else {
        1.0
    };

    // 4. Altitude / clutter factor.
    // Radars that cannot look down have a much steeper clutter penalty — they are
    // nearly blind below the terrain masking threshold regardless of tuning.
    let alt_factor = match sensor_type {
        SensorType::Awacs | SensorType::AirborneFighter => {
            if look_down_capable {
                1.0
            } else {
                // Non-look-down fighter/early AWACS: severe penalty below 500 m AGL.
                (contact_alt_agl_m as f32 / 500.0).clamp(0.0, 1.0)
            }
        }
        SensorType::NavalRadar => {
            let base = (contact_alt_agl_m as f32 / 150.0).clamp(0.3, 1.0);
            if look_down_capable { base } else { base * 0.4 }
        }
        SensorType::GroundEwr | SensorType::SamSearchRadar => {
            if look_down_capable {
                (contact_alt_agl_m as f32 / cfg.ground_radar_low_alt_threshold_m).clamp(0.1, 1.0)
            } else {
                // Non-look-down ground radar: near-zero below 3× threshold.
                (contact_alt_agl_m as f32 / (cfg.ground_radar_low_alt_threshold_m * 3.0)).clamp(0.0, 1.0)
            }
        }
    };

    (range_factor * rcs_factor * notch_factor * alt_factor).clamp(0.0, 1.0)
}

// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default)]
pub struct Ewr {
    tracks: FxHashMap<Side, FxHashMap<EnId, Track>>,
    player_state: FxHashMap<Ucid, PlayerState>,
    /// Snapshot of all active radar donors, rebuilt each tick in update_tracks.
    /// Stored so spike_warnings can query enemy donors without re-iterating db.
    donor_snapshot: Vec<crate::db::RadarDonor>,
    /// IADN: fused multi-sensor air picture, keyed by owning coalition.
    /// Only populated when `cfg.iadn` is Some.
    pub fused_tracks: FxHashMap<Side, Vec<FusedTrack>>,
    /// IADN HARM defense: anti-radiation missiles currently in flight,
    /// tracked so nearby SAM sites of the threatened side can be warned to
    /// go dark before impact. (weapon object id, side under threat, launch
    /// time -- used for a safety expiry if the weapon object outlives any
    /// reasonable ARM flight time).
    tracked_arms: Vec<(DcsOid<ClassWeapon>, Side, DateTime<Utc>)>,
    /// SAM site group -> timestamp its radar is forced dark until, due to a
    /// detected ARM threat. Consulted by the EMCON logic in update_tracks,
    /// which overrides the normal cue-based decision while an entry here is
    /// still in the future.
    harm_dark_until: FxHashMap<GroupId, DateTime<Utc>>,
    /// IADN engagement doctrine/hysteresis: per-SAM-site hot/dark state,
    /// so a site doesn't flicker between AlarmState values every tick and
    /// doesn't snap hot the instant a cue appears -- see decide_hot_state.
    sam_emcon: FxHashMap<GroupId, SamEmconState>,
}

/// Per-SAM-site engagement doctrine state (see Ewr::decide_hot_state).
#[derive(Debug, Clone, Copy, Default)]
struct SamEmconState {
    /// Whether this site is currently commanded hot (AlarmState::Auto),
    /// tracked independently of DCS's own state so hysteresis doesn't need
    /// a round-trip Lua call to know its own last decision.
    hot: bool,
    /// When this site last transitioned dark -> hot, for the minimum
    /// engagement dwell check.
    went_hot_at: Option<DateTime<Utc>>,
    /// Scheduled time this site is allowed to go hot after first detecting
    /// a qualifying cue, for the randomized per-site reaction delay.
    pending_hot_at: Option<DateTime<Utc>>,
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
        let radar_physics = db.ephemeral.cfg.radar_physics.clone();
        let iadn_cfg = db.ephemeral.cfg.iadn.clone();
        let mut rng = rand::thread_rng();
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
            // Action groups (player-deployed AWACS, drones, etc.) — use stored airborne_velocity.
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
            // AI aircraft (spawned CAP, AI AWACS, any AI airborne group).
            // These are NOT in `db.persisted.actions` and do NOT have `airborne_velocity` set
            // because they never go through `update_unit_positions()`.  We query DCS directly
            // here — the same way player positions are queried — using the object-id map
            // populated at birth time.
            let mut ai_aircraft: SmallVec<[(EnId, Side, Position3, Vector3); 32]> =
                SmallVec::new();
            for (uid, oid, side) in db.ai_aircraft_unit_ids() {
                // Query DCS for live position and in_air state.
                let unit_instance: Unit<'_> = match Unit::get_instance(lua, oid) {
                    Ok(u) => u,
                    Err(_) => continue, // unit may have died but not yet been cleaned up
                };
                let in_air = match unit_instance.in_air() {
                    Ok(b) => b,
                    Err(_) => continue,
                };
                if !in_air {
                    continue;
                }
                let pos = match unit_instance.get_position() {
                    Ok(p) => p,
                    Err(_) => continue,
                };
                let vel = match unit_instance.get_velocity() {
                    Ok(v) => v.0,
                    Err(_) => Vector3::zeros(),
                };
                ai_aircraft.push((EnId::Unit(uid), side, pos, vel));
            }
            players.chain(actions).chain(ai_aircraft.into_iter()).collect()
        };

        for tracks in self.tracks.values_mut() {
            for track in tracks.values_mut() {
                track.detected = false;
                track.detected_by = DetectedBy::default();
            }
        }

        // Snapshot donors for spike_warnings use later in the same tick
        self.donor_snapshot = db.radar_donors().collect();

        // IADN jamming: snapshot every UnitTag::Jammer-tagged aircraft's
        // position per side, once per tick, reused by every donor below
        // (jamming_factor_for_donor only cares about the nearest enemy
        // jammer to a given donor, not per-contact).
        let jammer_positions: FxHashMap<Side, SmallVec<[Vector3; 8]>> = if iadn_cfg
            .as_ref()
            .map(|c| c.jamming_enabled)
            .unwrap_or(false)
        {
            let mut m: FxHashMap<Side, SmallVec<[Vector3; 8]>> = FxHashMap::default();
            for (id, side, pos, _vel) in &aircraft {
                if Self::is_jammer(id, db) {
                    m.entry(*side).or_default().push(pos.p.0);
                }
            }
            m
        } else {
            FxHashMap::default()
        };

        // Accumulate per-sensor contributions for IADN fusion this tick.
        // Maps (donor_side, EnId) -> Vec<(snr, pos, velocity, sensor_type, detected_by)>
        let mut fusion_contributions: FxHashMap<(Side, EnId), SmallVec<[(f32, Position3, Vector3, SensorType, DetectedBy); 4]>> =
            FxHashMap::default();

        for donor in &self.donor_snapshot {
            let range_sq = (donor.range as f64).powi(2);
            let tracks = self.tracks.entry(donor.side).or_default();
            let mut donor_pos = donor.pos.p.0;
            donor_pos.y += 10.; // factor in antenna height
            let jamming_factor = iadn_cfg
                .as_ref()
                .filter(|c| c.jamming_enabled)
                .map(|c| Self::jamming_factor_for_donor(donor, donor_pos, &jammer_positions, c))
                .unwrap_or(1.0);
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
                            // Scan interval gate: slow-rotating radars only refresh tracks
                            // every scan_interval_secs. Between sweeps the track position is
                            // held but the contact is still considered detected (track memory).
                            let scan_due = donor.scan_interval_secs == 0
                                || (now - track.last_update).num_seconds() >= donor.scan_interval_secs as i64
                                || track.last_update == DateTime::<Utc>::UNIX_EPOCH;

                            // Probabilistic detection gate: with radar_physics config we roll
                            // against the computed probability; without it we use legacy binary.
                            let detected = if let Some(rp) = &radar_physics {
                                let bearing = radians_to_degrees(azumith3d_to(donor_pos, pos.p.0));
                                let heading  = radians_to_degrees(azumith3d(pos.x.0));
                                let contact_pos2 = Vector2::new(pos.p.x, pos.p.z);
                                let donor_pos2   = Vector2::new(donor_pos.x, donor_pos.z);
                                let aspect = Aspect::compute(bearing, heading, donor_pos2, contact_pos2);
                                let alt_agl = pos.p.y;
                                let prob = compute_detection_probability(
                                    donor.range,
                                    dist_sq.sqrt(),
                                    aspect,
                                    alt_agl,
                                    *velocity,
                                    nalgebra::Point3::from(donor_pos),
                                    donor.sensor_type,
                                    donor.pulse_doppler,
                                    donor.look_down_capable,
                                    donor.frequency_band,
                                    rp,
                                ) * jamming_factor;
                                // Collect SNR for IADN fusion
                                if iadn_cfg.is_some() && donor.side != *obj_side {
                                    fusion_contributions
                                        .entry((donor.side, *id))
                                        .or_default()
                                        .push((prob, *pos, *velocity, donor.sensor_type, sensor));
                                }
                                rand::Rng::r#gen::<f32>(&mut rng) < prob
                            } else {
                                true // legacy: always detected if in range/cone/LOS
                            };

                            if detected {
                                // Only refresh position/velocity when a scan sweep is due.
                                // This models slow-rotating radars: detection is remembered
                                // between sweeps but position is only updated each rotation.
                                if scan_due {
                                    match ewr_mode {
                                    EwrMode::Original => {
                                        if let Some(rp) = &radar_physics {
                                            let a = rp.track_smoothing_alpha as f64;
                                            let op = track.pos.p.0;
                                            let np = pos.p.0;
                                            let sp = nalgebra::Vector3::new(
                                                op.x * (1.0 - a) + np.x * a,
                                                op.y * (1.0 - a) + np.y * a,
                                                op.z * (1.0 - a) + np.z * a,
                                            );
                                            track.pos.p.0 = sp;
                                            track.velocity = track.velocity * (1.0 - a) + velocity * a;
                                        } else {
                                            track.pos = *pos;
                                            track.velocity = *velocity;
                                        }
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
                                    } // end scan_due
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
        }

        // IADN fusion pass: merge per-sensor contributions into FusedTrack table.
        if let Some(iadn) = &iadn_cfg {
            self.fuse_tracks(&fusion_contributions, iadn, db, now);
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
        // IADN SAM cueing + EMCON: use the fused, multi-sensor track picture
        // (not each SAM's own organic radar alone) to decide whether a SAM
        // site's radar should be hot. No qualifying fused hostile in range ->
        // AlarmState::Green (radar dark, doesn't light up for nothing).
        // Otherwise -> AlarmState::Auto (radar on, DCS's native SAM AI takes
        // it from there using its own seeker/engagement logic). This is the
        // "smart" half of IADN; the fusion feeding it already ran above.
        //
        // Every donor is resolved fresh by DCS group name each tick via
        // Group::get_by_name -- never a cached handle -- so a SAM site whose
        // objective is currently culled/despawned just fails that lookup and
        // is silently skipped this tick, with zero interaction with the
        // culling system: nothing here holds a reference across a
        // despawn/respawn cycle, so there's nothing to go stale.
        if let (Some(iadn), Some(_rp)) = (&iadn_cfg, &radar_physics) {
            if iadn.sam_cue_enabled {
                // HARM defense: poll in-flight tracked ARMs and mark any SAM
                // site within harm_defense_radius_m as threatened. Must run
                // before the cue loop below so a fresh threat overrides this
                // tick's targeting decision, not next tick's.
                self.update_harm_threats(lua, iadn, now);
                // Clone the snapshot (RadarDonor is Copy) so the loop body
                // is free to call &mut self methods (decide_hot_state,
                // apply_layered_radar_emission) without borrow conflicts.
                let donors = self.donor_snapshot.clone();
                for donor in &donors {
                    if !matches!(donor.sensor_type, SensorType::SamSearchRadar) {
                        continue;
                    }
                    let Some(gid) = donor.gid else { continue };
                    let Some(group) = db.persisted.groups.get(&gid) else { continue };
                    let sam_pos = Vector2::new(donor.pos.p.x, donor.pos.p.z);
                    let under_harm_threat = self
                        .harm_dark_until
                        .get(&gid)
                        .map(|until| now < *until)
                        .unwrap_or(false);
                    // IADN network gating: this site only gets smart,
                    // network-fused cueing if its linked command center is
                    // alive and still friendly. Cut off (never linked, or
                    // the command center died/was captured) -> it doesn't
                    // just get abandoned in whatever EMCON state it was
                    // last forced into; it's explicitly returned to Auto,
                    // DCS's own always-on default, same as a site with no
                    // IADN involvement at all.
                    let networked = db
                        .persisted
                        .objectives_by_group
                        .get(&gid)
                        .and_then(|sam_oid| db.persisted.sam_command_center_link.get(sam_oid))
                        .and_then(|cc_oid| db.persisted.objectives.get(cc_oid))
                        .map(|cc| cc.owner == donor.side && cc.health() > 0)
                        .unwrap_or(false);
                    let mut nearest_cue_dist: Option<f64> = None;
                    let desired = if under_harm_threat {
                        // Survival overrides everything else, networked or
                        // not: go dark even if a good cue exists, rather
                        // than trading the site for one more shot. Also
                        // reset engagement-doctrine state so coming off a
                        // HARM threat re-enters the reaction-delay process
                        // fresh rather than snapping straight back hot.
                        self.sam_emcon.remove(&gid);
                        dcso3::controller::AlarmState::Green
                    } else if !networked {
                        self.sam_emcon.remove(&gid);
                        dcso3::controller::AlarmState::Auto
                    } else {
                        let cues = self.sam_cue_targets(donor.side, sam_pos, donor.range as f64, iadn);
                        nearest_cue_dist = cues
                            .iter()
                            .map(|(_, pos, _)| na::distance(&sam_pos.into(), &(*pos).into()))
                            .fold(None, |acc: Option<f64>, d| Some(acc.map_or(d, |a| a.min(d))));
                        self.decide_hot_state(gid, !cues.is_empty(), now, iadn)
                    };
                    let group_name = group.name.clone();
                    if let Ok(live) = dcso3::group::Group::get_by_name(lua, &group_name) {
                        if let Ok(con) = live.get_controller() {
                            let _ = con.set_option(dcso3::controller::AiOption::Ground(
                                dcso3::controller::GroundOption::AlarmState(desired),
                            ));
                        }
                    }
                    // Layered search/track radar: an optional second control
                    // layer on top of the group AlarmState above, only for
                    // sites with units tagged SearchRadar/TrackRadar. Search
                    // radar mirrors the site's hot/dark state; the separate
                    // tracking/engagement radar only powers up once a cue is
                    // within track_radar_range_fraction of the search
                    // radar's own range, mirroring real layered SAM systems
                    // (SA-10, Patriot) that keep the higher-exposure
                    // engagement radar dark until close to actually firing.
                    let hot = matches!(desired, dcso3::controller::AlarmState::Auto | dcso3::controller::AlarmState::Red);
                    self.apply_layered_radar_emission(
                        lua, db, gid, hot, nearest_cue_dist, donor.range as f64, iadn,
                    );
                    // IADN jamming: degrade this donor's effective detection
                    // range against airborne contacts near a UnitTag::Jammer
                    // unit -- see jamming_factor_for_donor.
                }
            }
        }
        Ok(())
    }

    // ─── IADN: fuse sensor contributions into FusedTrack table ──────────────

    /// Derive a ContactClass from unit tags for a given tracked entity.
    /// IADN jamming: does this contact's vehicle type carry
    /// UnitTag::Jammer? Same type-lookup shape as classify_contact just
    /// below, kept separate since the callers care about different things
    /// (contact classification vs. "is this a jamming platform").
    fn is_jammer(id: &EnId, db: &Db) -> bool {
        let typ = match id {
            EnId::Player(ucid) => db
                .persisted
                .players
                .get(ucid)
                .and_then(|p| p.current_slot.as_ref())
                .and_then(|(_, inst)| inst.as_ref())
                .map(|inst| inst.typ.clone()),
            EnId::Unit(uid) => db.persisted.units.get(uid).map(|u| u.typ.clone()),
        };
        typ.as_ref()
            .and_then(|t| db.ephemeral.cfg.unit_classification.get(t))
            .map(|tags| tags.contains(UnitTag::Jammer))
            .unwrap_or(false)
    }

    /// IADN jamming: detection-probability multiplier for a donor given the
    /// nearest enemy jammer, if any is within cfg.jamming_range_m. Linear
    /// falloff from full effect at 0m to none at jamming_range_m, scaled by
    /// jamming_detection_penalty and the donor's own ecm_susceptibility.
    /// 1.0 (no degradation) if no enemy jammer is in range.
    fn jamming_factor_for_donor(
        donor: &crate::db::RadarDonor,
        donor_pos: Vector3,
        jammer_positions: &FxHashMap<Side, SmallVec<[Vector3; 8]>>,
        cfg: &IadnConfig,
    ) -> f32 {
        let Some(enemy_jammers) = jammer_positions.get(&donor.side.opposite()) else {
            return 1.0;
        };
        let range_sq = cfg.jamming_range_m.powi(2);
        let mut worst_proximity = 0.0f64; // 0 = no effect, 1 = jammer right on top
        for jpos in enemy_jammers {
            let dist_sq = na::distance_squared(&donor_pos.into(), &(*jpos).into());
            if dist_sq <= range_sq {
                let proximity = 1.0 - (dist_sq.sqrt() / cfg.jamming_range_m);
                if proximity > worst_proximity {
                    worst_proximity = proximity;
                }
            }
        }
        let effect = worst_proximity * cfg.jamming_detection_penalty as f64 * donor.ecm_susceptibility as f64;
        (1.0 - effect).clamp(0.0, 1.0) as f32
    }

    fn classify_contact(id: &EnId, db: &Db) -> ContactClass {
        let typ = match id {
            EnId::Player(ucid) => db
                .persisted
                .players
                .get(ucid)
                .and_then(|p| p.current_slot.as_ref())
                .and_then(|(_, inst)| inst.as_ref())
                .map(|inst| inst.typ.clone()),
            EnId::Unit(uid) => db
                .persisted
                .units
                .get(uid)
                .map(|u| u.typ.clone()),
        };
        let tags = typ.as_ref().and_then(|t| db.ephemeral.cfg.unit_classification.get(t));
        match tags {
            Some(tags) if tags.contains(UnitTag::Helicopter) => ContactClass::Helicopter,
            Some(tags) if tags.contains(UnitTag::Aircraft) && tags.contains(UnitTag::AWACS) => ContactClass::Bomber,
            Some(tags) if tags.contains(UnitTag::Aircraft) => ContactClass::Fighter,
            _ => ContactClass::Unknown,
        }
    }

    fn fuse_tracks(
        &mut self,
        contributions: &FxHashMap<(Side, EnId), SmallVec<[(f32, Position3, Vector3, SensorType, DetectedBy); 4]>>,
        cfg: &IadnConfig,
        db: &Db,
        now: DateTime<Utc>,
    ) {
        let assoc_sq = cfg.track_association_radius_m.powi(2);

        for ((side, id), sensor_hits) in contributions {
            if sensor_hits.is_empty() {
                continue;
            }
            // Weighted centroid from all sensors that hit this tick.
            let total_snr: f32 = sensor_hits.iter().map(|(snr, ..)| snr).sum();
            if total_snr < cfg.detection_snr_threshold {
                continue;
            }
            // Classify by unit tags from the cfg unit_classification table.
            let contact_class = Self::classify_contact(id, db);
            let mut fused_pos = Position3::default();
            let mut fused_vel = Vector3::zeros();
            let mut fused_db  = DetectedBy::default();
            for (snr, pos, vel, _, db) in sensor_hits {
                let w = snr / total_snr;
                fused_pos.p.0 += pos.p.0 * w as f64;
                fused_vel     += vel * w as f64;
                fused_db       = fused_db.with(*db);
            }
            let fused_2d = Vector2::new(fused_pos.p.x, fused_pos.p.z);
            let tracks = self.fused_tracks.entry(*side).or_default();

            // Find the closest existing track within association radius.
            let existing = tracks
                .iter_mut()
                .filter(|t| t.iff == IffState::Hostile)
                .find(|t| {
                    let t2d = Vector2::new(t.pos.p.x, t.pos.p.z);
                    na::distance_squared(&fused_2d.into(), &t2d.into()) <= assoc_sq
                });

            let confidence_gain = (total_snr / sensor_hits.len() as f32).clamp(0.0, 0.3);
            if let Some(track) = existing {
                // Update existing track with smoothed position.
                let op = track.pos.p.0;
                let np = fused_pos.p.0;
                track.pos.p.0 = nalgebra::Vector3::new(
                    op.x * 0.6 + np.x * 0.4,
                    op.y * 0.6 + np.y * 0.4,
                    op.z * 0.6 + np.z * 0.4,
                );
                track.velocity = track.velocity * 0.7 + fused_vel * 0.3;
                track.confidence = (track.confidence + confidence_gain).clamp(0.0, 1.0);
                let n = sensor_hits.len() as u8;
                track.pos_uncertainty_m = (track.pos_uncertainty_m * 0.8)
                    .max(1000.0 / (n as f32 + 1.0));
                track.detected_by = track.detected_by.with(fused_db);
                track.sensor_count = sensor_hits.len() as u8;
                track.last_detection = now;
                track.last_update = now;
                // Refine classification when we get a better identification.
                if track.classification == ContactClass::Unknown && contact_class != ContactClass::Unknown {
                    track.classification = contact_class;
                }
            } else {
                // Tentative new track.
                tracks.push(FusedTrack {
                    id: FusedTrackId::new(),
                    side: *side,
                    pos: fused_pos,
                    velocity: fused_vel,
                    pos_uncertainty_m: 5000.0,
                    confidence: confidence_gain,
                    last_detection: now,
                    last_update: now,
                    classification: contact_class,
                    iff: IffState::Hostile,
                    detected_by: fused_db,
                    sensor_count: sensor_hits.len() as u8,
                    stale: false,
                });
            }
        }

        // Decay and drop stale fused tracks.
        for tracks in self.fused_tracks.values_mut() {
            tracks.retain_mut(|t| {
                let age_secs = (now - t.last_detection).num_seconds() as f32;
                // Exponential decay: half-life ~60 s
                t.confidence *= (-age_secs * std::f32::consts::LN_2 / 60.0).exp();
                t.stale = t.is_stale(now, cfg.track_stale_secs);
                !t.is_expired(cfg)
            });
        }
    }

    /// IADN HARM defense: register a freshly-launched weapon for in-flight
    /// tracking, once the caller (Event::Shot handler) has already matched
    /// its type name against cfg.iadn.anti_radiation_weapons. `threatened_side`
    /// is the side whose SAM sites this missile might be homing on -- i.e.
    /// the opposite of whoever fired it.
    pub fn track_potential_arm(
        &mut self,
        oid: DcsOid<ClassWeapon>,
        threatened_side: Side,
        now: DateTime<Utc>,
    ) {
        self.tracked_arms.push((oid, threatened_side, now));
    }

    /// IADN HARM defense: poll all in-flight tracked ARMs and, for any
    /// within `cfg.harm_defense_radius_m` of a live SAM search radar on the
    /// threatened side, force that site's radar dark until
    /// `now + cfg.harm_defense_cooldown_secs`. Drops a tracking entry once
    /// its weapon object no longer exists (impacted, or DCS cleaned it up)
    /// or a safety flight-time cap is exceeded -- this is a "might be
    /// targeting us" proximity heuristic, not true seeker/guidance physics.
    ///
    /// Each tracked weapon is resolved fresh via Weapon::get_instance every
    /// call -- never a cached handle across ticks beyond the DcsOid itself,
    /// which is a lightweight identifier, not a live reference. A weapon
    /// that no longer exists just fails that lookup and gets dropped from
    /// tracking; no interaction with SAM culling either way, since this only
    /// ever reads donor_snapshot (already culling-safe) and writes
    /// timestamps keyed by GroupId, never touching a live DCS handle for the
    /// SAM site itself.
    fn update_harm_threats(&mut self, lua: MizLua, cfg: &IadnConfig, now: DateTime<Utc>) {
        const MAX_ARM_FLIGHT_SECS: i64 = 90;
        let range_sq = cfg.harm_defense_radius_m.powi(2);
        let cooldown = chrono::Duration::seconds(cfg.harm_defense_cooldown_secs as i64);
        let Self { tracked_arms, donor_snapshot, harm_dark_until, .. } = self;
        tracked_arms.retain(|(oid, threatened_side, launched_at)| {
            if (now - *launched_at).num_seconds() > MAX_ARM_FLIGHT_SECS {
                return false;
            }
            let weapon = match Weapon::get_instance(lua, oid) {
                Ok(w) => w,
                Err(_) => return false, // impacted or otherwise gone
            };
            let pos = match weapon.as_object().and_then(|o| o.get_point()) {
                Ok(p) => p,
                Err(_) => return false,
            };
            let arm_pos = Vector2::new(pos.x, pos.z);
            for donor in donor_snapshot.iter() {
                if donor.side != *threatened_side
                    || !matches!(donor.sensor_type, SensorType::SamSearchRadar)
                {
                    continue;
                }
                let Some(gid) = donor.gid else { continue };
                let donor_pos = Vector2::new(donor.pos.p.x, donor.pos.p.z);
                if na::distance_squared(&arm_pos.into(), &donor_pos.into()) <= range_sq {
                    harm_dark_until.insert(gid, now + cooldown);
                }
            }
            true // still in flight, keep tracking
        });
    }

    /// IADN engagement doctrine: decide whether a SAM site should be hot
    /// (AlarmState::Auto) or dark (AlarmState::Green) given whether it
    /// currently has a qualifying cue, applying two pieces of hysteresis so
    /// the decision doesn't look mechanically instant:
    ///
    /// - On first detecting a cue, the site waits a random
    ///   0..=reaction_delay_max_secs before actually going hot (rolled once
    ///   per activation), so a cluster of networked sites doesn't snap to
    ///   Auto in the same tick.
    /// - Once hot, the site stays hot for at least min_hot_dwell_secs even
    ///   if the cue drops in the meantime, so it actually gets a chance to
    ///   engage instead of flickering dark the instant track quality dips.
    ///
    /// Per-site state lives in `sam_emcon`, keyed by GroupId -- Rust-side
    /// bookkeeping only, never a live DCS handle, so it has no interaction
    /// with SAM culling; a culled/despawned site's state just sits idle
    /// until it's relevant again.
    fn decide_hot_state(
        &mut self,
        gid: GroupId,
        has_cue: bool,
        now: DateTime<Utc>,
        cfg: &IadnConfig,
    ) -> dcso3::controller::AlarmState {
        use dcso3::controller::AlarmState;
        let state = self.sam_emcon.entry(gid).or_default();
        if has_cue {
            if state.hot {
                return AlarmState::Auto;
            }
            match state.pending_hot_at {
                None => {
                    let mut rng = rand::thread_rng();
                    let delay_secs =
                        rand::Rng::gen_range(&mut rng, 0..=cfg.reaction_delay_max_secs.max(1));
                    state.pending_hot_at = Some(now + chrono::Duration::seconds(delay_secs as i64));
                    AlarmState::Green
                }
                Some(t) if now >= t => {
                    state.hot = true;
                    state.went_hot_at = Some(now);
                    state.pending_hot_at = None;
                    AlarmState::Auto
                }
                Some(_) => AlarmState::Green, // still waiting out the reaction delay
            }
        } else {
            state.pending_hot_at = None; // cue gone -- cancel any pending activation
            if state.hot {
                let dwell_ok = state
                    .went_hot_at
                    .map(|t| (now - t).num_seconds() >= cfg.min_hot_dwell_secs as i64)
                    .unwrap_or(true);
                if dwell_ok {
                    state.hot = false;
                    AlarmState::Green
                } else {
                    AlarmState::Auto // still within the minimum engagement dwell
                }
            } else {
                AlarmState::Green
            }
        }
    }

    /// IADN layered search/track radar: an optional second control layer on
    /// top of the group-level AlarmState. Units tagged UnitTag::SearchRadar
    /// mirror the site's overall hot/dark state; units tagged
    /// UnitTag::TrackRadar only emit once a cue is within
    /// `cfg.track_radar_range_fraction` of the search radar's own range --
    /// real layered SAM systems (SA-10, Patriot) keep the higher-exposure
    /// engagement/tracking radar dark until close to actually firing, not
    /// lit the whole time the site is merely alert. Sites with no unit
    /// tagged either role are untouched -- this adds a layer, it doesn't
    /// replace the group AlarmState control.
    ///
    /// Each unit is resolved fresh via Unit::get_by_name every call, same
    /// culling-safe pattern as the group-level lookup: a dead/despawned
    /// unit just fails that lookup and is silently skipped.
    fn apply_layered_radar_emission(
        &self,
        lua: MizLua,
        db: &Db,
        gid: GroupId,
        hot: bool,
        nearest_cue_dist_m: Option<f64>,
        search_range_m: f64,
        cfg: &IadnConfig,
    ) {
        let Some(group) = db.persisted.groups.get(&gid) else { return };
        let track_range_m = search_range_m * cfg.track_radar_range_fraction as f64;
        let track_should_be_on =
            hot && nearest_cue_dist_m.map(|d| d <= track_range_m).unwrap_or(false);
        for uid in &group.units {
            let Some(unit) = db.persisted.units.get(uid) else { continue };
            if unit.dead {
                continue;
            }
            let want_on = if unit.tags.contains(UnitTag::SearchRadar) {
                hot
            } else if unit.tags.contains(UnitTag::TrackRadar) {
                track_should_be_on
            } else {
                continue; // not a radar-role unit -- leave it alone entirely
            };
            if let Ok(live) = dcso3::unit::Unit::get_by_name(lua, &unit.name) {
                let _ = live.enable_emission(want_on);
            }
        }
    }

    /// Returns SAM cue targets for a given SAM position and range.
    /// Returns up to 3 (FusedTrackId, position, priority) sorted by priority descending.
    pub fn sam_cue_targets(
        &self,
        side: Side,
        sam_pos: Vector2,
        sam_range_m: f64,
        cfg: &IadnConfig,
    ) -> SmallVec<[(FusedTrackId, Vector2, f32); 3]> {
        let mut result: SmallVec<[(FusedTrackId, Vector2, f32); 3]> = smallvec![];
        let tracks = match self.fused_tracks.get(&side) {
            Some(t) => t,
            None => return result,
        };
        let range_sq = sam_range_m.powi(2);
        for track in tracks {
            if track.side != side || track.iff != IffState::Hostile || track.confidence < cfg.sam_cue_confidence_threshold {
                continue;
            }
            let t2d = Vector2::new(track.pos.p.x, track.pos.p.z);
            let dist_sq = na::distance_squared(&sam_pos.into(), &t2d.into());
            if dist_sq > range_sq {
                continue;
            }
            // Priority: high confidence × close × fighter bonus.
            let class_bonus = match track.classification {
                ContactClass::Fighter  => 1.5,
                ContactClass::Bomber   => 1.2,
                ContactClass::Helicopter => 0.8,
                ContactClass::Unknown  => 1.0,
            };
            let priority = track.confidence * class_bonus * (sam_range_m as f32 / dist_sq.sqrt() as f32);
            if result.len() < 3 {
                result.push((track.id, t2d, priority));
                result.sort_by(|a, b| b.2.partial_cmp(&a.2).unwrap_or(std::cmp::Ordering::Equal));
            } else if let Some(last) = result.last() {
                if priority > last.2 {
                    result.pop();
                    result.push((track.id, t2d, priority));
                    result.sort_by(|a, b| b.2.partial_cmp(&a.2).unwrap_or(std::cmp::Ordering::Equal));
                }
            }
        }
        result
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

    /// Return the 2D (x,z) positions of all enemy aircraft currently detected by
    /// `defending_side`'s EWR network (`Track.detected == true`, enemy side, fresh age).
    /// Only contacts that a radar has actually seen are included — players on the ground
    /// or outside radar range do NOT appear here.
    /// This is the authoritative source for reactive CAP spawning.
    pub fn detected_enemy_positions(
        &self,
        defending_side: Side,
        now: DateTime<Utc>,
    ) -> Vec<Vector2> {
        let tracks = match self.tracks.get(&defending_side) {
            Some(t) => t,
            None => return vec![],
        };
        tracks
            .values()
            .filter(|t| {
                t.detected
                    && t.side != defending_side
                    && (now - t.last).num_seconds() <= DROP_AGE_SECS
            })
            .map(|t| Vector2::new(t.pos.p.x, t.pos.p.z))
            .collect()
    }

    /// Ground intel radio picture from the ELINT/SIGINT database.
    /// Returns up to 5 highest-confidence ground contacts near the player, formatted
    /// for radio broadcast: "BOGEY 3 ARMOR, 047 FOR 32, CONFIDENCE HIGH".
    pub fn intel_picture(
        &self,
        side: Side,
        player_pos: Vector2,
        intel_db: &crate::db::intel::IntelDatabase,
    ) -> SmallVec<[CompactString; 8]> {
        let mut lines: SmallVec<[CompactString; 8]> = smallvec![];
        let contacts = intel_db.top_contacts_for_side(side, player_pos, 5);
        if contacts.is_empty() {
            lines.push(format_compact!("No current ground intel"));
            return lines;
        }
        for contact in contacts {
            let bearing = radians_to_degrees(azumith2d_to(player_pos, contact.pos)) as u16;
            let range_km = (na::distance(&player_pos.into(), &contact.pos.into()) / 1000.0) as u32;
            let class_str = match contact.unit_class {
                crate::db::intel::IntelUnitClass::Armor      => "ARMOR",
                crate::db::intel::IntelUnitClass::AirDefense => "ADS",
                crate::db::intel::IntelUnitClass::Artillery  => "ARTY",
                crate::db::intel::IntelUnitClass::Infantry   => "INF",
                crate::db::intel::IntelUnitClass::AirBase    => "AIRBASE",
                crate::db::intel::IntelUnitClass::Naval      => "NAVAL",
                crate::db::intel::IntelUnitClass::Unknown    => "UNK",
            };
            let conf_str = if contact.confidence >= 0.8 {
                "HIGH"
            } else if contact.confidence >= 0.4 {
                "MEDIUM"
            } else {
                "LOW"
            };
            lines.push(format_compact!(
                "{} {}×{class_str}, {bearing:03} FOR {range_km}km, CONF {conf_str}",
                contact.unit_count,
                contact.unit_count,
            ));
        }
        lines
    }

    /// Count the number of **enemy player** contacts currently detected by
    /// `defending_side`'s EWR network that are flying **fixed-wing aircraft only**
    /// (i.e. `UnitTag::Aircraft`, NOT `UnitTag::Helicopter`).
    ///
    /// Helicopters are excluded because SAMs are expected to handle them.
    /// AI aircraft are excluded because they should not trigger reactive CAP
    /// (a lone AI scout or logistics plane does not constitute an air threat).
    ///
    /// Returns the count of qualifying fresh tracks.
    pub fn detected_enemy_fixedwing_player_count(
        &self,
        defending_side: Side,
        now: DateTime<Utc>,
        db: &crate::db::Db,
    ) -> usize {
        use bfprotocols::cfg::UnitTag;
        use bfprotocols::stats::EnId;
        let tracks = match self.tracks.get(&defending_side) {
            Some(t) => t,
            None => return 0,
        };
        tracks
            .iter()
            .filter(|(id, t)| {
                if !t.detected
                    || t.side == defending_side
                    || (now - t.last).num_seconds() > DROP_AGE_SECS
                {
                    return false;
                }
                // Only player contacts — AI aircraft don't trigger CAP.
                let ucid = match id {
                    EnId::Player(ucid) => ucid,
                    EnId::Unit(_) => return false,
                };
                // Look up what they're flying and exclude helicopters.
                let tags = db
                    .persisted
                    .players
                    .get(ucid)
                    .and_then(|p| p.current_slot.as_ref())
                    .and_then(|(_, inst)| inst.as_ref())
                    .and_then(|inst| {
                        db.ephemeral
                            .cfg
                            .unit_classification
                            .get(&inst.typ)
                    });
                match tags {
                    Some(tags) => {
                        tags.contains(UnitTag::Aircraft)
                            && !tags.contains(UnitTag::Helicopter)
                    }
                    // If we can't determine the type, be conservative and include it.
                    None => true,
                }
            })
            .count()
    }
}
