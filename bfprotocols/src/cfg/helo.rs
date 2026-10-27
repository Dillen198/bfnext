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

//! Logistics from scenery and the AI helicopter insertion settings.

use super::*;

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct LogiFromSceneryCfg {
    /// Logi rating (0-100) for an objective where the scenery scan found no
    /// trackable buildings at all. Default 100 (treated as fully supplied).
    #[serde(default = "default_scenery_fallback_logi")]
    pub fallback_logi: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HeloInsertionCfg {
    /// Helicopter group template name per side (must be a Helicopter group in
    /// the miz file). Deliberately separate from
    /// `warehouse.air_logistics.aircraft_template` -- that feature flies
    /// fixed-wing cargo planes between logistics hubs and shouldn't be
    /// repointed at a helicopter, and these missions land at arbitrary
    /// objectives in the open, not just at airfields, so they need an
    /// airframe that can actually do that. A side with no entry here can't
    /// launch either mission -- the F10 command tells the player why.
    #[serde(default)]
    pub aircraft_template: FxHashMap<Side, String>,
    /// Absolute floor (metres BARO) for the transit leg. The route's own
    /// altitude comes from the terrain under it -- see `terrain_clearance_m`
    /// -- and this only raises it where the ground is low, so keep it low
    /// unless you want the helo held high over flat country.
    #[serde(default = "default_helo_altitude_m")]
    pub altitude_m: f64,
    /// Cruise speed in km/h for the transit leg.
    #[serde(default = "default_helo_speed_kph")]
    pub speed_kph: f64,
    /// Which `troops[side]` entry (looked up by name) the troop-insertion
    /// mission carries. Must exist for both sides; wants `can_capture: true`
    /// to actually be useful at the destination.
    #[serde(default = "default_helo_troop_name")]
    pub troop_name: String,
    /// Points charged to call the troop-insertion mission, ON TOP of the
    /// troop's own `cost` (which is charged too, same as a normal deploy).
    #[serde(default)]
    pub troop_mission_cost: i32,
    /// Points charged to call the resource-delivery mission.
    #[serde(default = "default_helo_supply_cost")]
    pub supply_mission_cost: i32,
    /// Max units of each equipment/liquid item moved per resource-delivery
    /// run (further capped by what the origin objective actually has stored).
    #[serde(default = "default_helo_supply_per_item")]
    pub supply_amount_per_item: u32,
    /// Neither mission will launch if the chosen origin/destination pair is
    /// farther apart than this (metres). Default 150km.
    #[serde(default = "default_helo_max_range_m")]
    pub max_range_m: f64,
    /// How close to the destination point, while on the ground, counts as
    /// "landed and delivered". Default 200m.
    #[serde(default = "default_helo_landing_radius_m")]
    pub landing_radius_m: f64,
    /// How far above the highest terrain under a leg the route is planned.
    /// `altitude_m` is only a floor -- over mountains the planner climbs the
    /// route to terrain + this, which is what stops the helo flying into a
    /// ridge on the way. Default 250m.
    #[serde(default = "default_helo_terrain_clearance_m")]
    pub terrain_clearance_m: f64,
    /// Terrain the planner considers too high for a loaded helicopter to
    /// climb over. Exceeding it is logged, and is what `lateral_avoidance`
    /// keys off if it is enabled. It never caps the route itself: flying
    /// under a ridge isn't an option, so the route climbs regardless.
    /// Default 3500m.
    #[serde(default = "default_helo_max_altitude_m")]
    pub max_altitude_m: f64,
    /// Terrain sampling resolution for route planning, in metres. Smaller
    /// tracks the ground more closely and catches narrower ridges, at the
    /// cost of more `land.getHeight` calls in the frame the mission is
    /// called. Default 2500m (further subdivided on long routes, so a route
    /// is always cut at least 100 ways).
    #[serde(default = "default_helo_waypoint_spacing_m")]
    pub waypoint_spacing_m: f64,
    /// Let the planner dogleg around high ground instead of climbing over
    /// it. Off by default: a helo that just climbs and descends with the
    /// terrain flies the shortest, most predictable track, and the detour is
    /// only worth its extra exposure on a map with terrain a loaded
    /// helicopter genuinely cannot out-climb. Turn it on for one of those.
    #[serde(default)]
    pub lateral_avoidance: bool,
    /// Refund the points a mission cost if the helicopter is lost before it
    /// delivers. Default true.
    #[serde(default = "default_true")]
    pub refund_on_loss: bool,
    /// Spawn the helicopter cold (engines off, AI runs its own startup)
    /// rather than hot on the ramp. Default true. Turn it off if the AI
    /// startup delay before it gets moving is more than you want to wait.
    #[serde(default = "default_true")]
    pub cold_start: bool,
    /// What happens when a TROOP INSERTION helo fails -- never takes off, is
    /// shot down, or never gets down at the target: instead of a refund the
    /// same squad is driven there by road from the nearest friendly objective.
    /// Absent = on, with the defaults below; `"enabled": false` turns it off
    /// and brings back the plain refund. Supply runs always just refund.
    #[serde(default)]
    pub ground_fallback: HeloGroundFallbackCfg,
    /// Route around the enemy air defence the calling side knows about (its
    /// intel contacts, and a presumed short-range garrison at every enemy
    /// objective on the map), fly nap-of-the-earth near it, land at the spot
    /// in the target zone farthest from known enemies, and re-route in the
    /// air when the helo comes under fire or new air defence is reported
    /// across its path. Absent = on, with the defaults below;
    /// `"enabled": false` brings back the plain terrain-only route to the
    /// zone centre.
    #[serde(default)]
    pub threat_avoidance: HeloThreatAvoidanceCfg,
}

/// See `HeloInsertionCfg::threat_avoidance`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HeloThreatAvoidanceCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Multiplier on every known threat's engagement radius before the route
    /// is planned around it. Default 1.25.
    #[serde(default = "default_helo_threat_margin")]
    pub margin: f64,
    /// Height above the ground flown near known threats and on the final
    /// approach, in metres. Elsewhere the route keeps `terrain_clearance_m`.
    /// Default 40.
    #[serde(default = "default_helo_noe_agl_m")]
    pub noe_agl_m: f64,
    /// Re-route in flight when the helo is shot at or a newly reported
    /// threat lies across the rest of its route. Default true.
    #[serde(default = "default_true")]
    pub replan: bool,
    /// Most in-flight re-routes one mission may make. Default 4.
    #[serde(default = "default_helo_max_replans")]
    pub max_replans: u32,
    /// Least time between two re-routes of one mission, seconds. Default 20.
    #[serde(default = "default_helo_min_replan_secs")]
    pub min_replan_secs: u32,
    /// Engagement radius assumed for an air-defence contact whose weapons the
    /// intel can't size, in metres. Default 10000 (a short-range radar SAM).
    #[serde(default = "default_helo_unknown_threat_m")]
    pub unknown_radius_m: f64,
    /// Short-range air defence (MANPADS, AAA) presumed around every enemy
    /// objective whose garrison is still standing, in metres beyond its
    /// zone. 0 turns the presumption off. Default 3000.
    #[serde(default = "default_helo_garrison_threat_m")]
    pub garrison_radius_m: f64,
}

impl Default for HeloThreatAvoidanceCfg {
    fn default() -> Self {
        Self {
            enabled: true,
            margin: default_helo_threat_margin(),
            noe_agl_m: default_helo_noe_agl_m(),
            replan: true,
            max_replans: default_helo_max_replans(),
            min_replan_secs: default_helo_min_replan_secs(),
            unknown_radius_m: default_helo_unknown_threat_m(),
            garrison_radius_m: default_helo_garrison_threat_m(),
        }
    }
}

pub(super) fn default_helo_threat_margin() -> f64 {
    1.25
}
pub(super) fn default_helo_noe_agl_m() -> f64 {
    40.0
}
pub(super) fn default_helo_max_replans() -> u32 {
    4
}
pub(super) fn default_helo_min_replan_secs() -> u32 {
    20
}
pub(super) fn default_helo_unknown_threat_m() -> f64 {
    10_000.0
}
pub(super) fn default_helo_garrison_threat_m() -> f64 {
    3_000.0
}

/// See `HeloInsertionCfg::ground_fallback`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct HeloGroundFallbackCfg {
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Vehicle group template per side (a ground group in the .miz). A side
    /// with no entry uses its `warehouse.convoy.truck_template`; a side with
    /// neither gets the refund.
    #[serde(default)]
    pub template: FxHashMap<Side, String>,
    /// Road speed in km/h. Default 50.
    #[serde(default = "default_ground_fallback_speed_kph")]
    pub speed_kph: f64,
    /// The squad dismounts once the vehicle is this close to the target's
    /// zone, or has stopped for a minute within twice this. Default 400m.
    #[serde(default = "default_ground_fallback_arrive_m")]
    pub arrive_m: f64,
    /// Only objectives this close to the target (straight line) may send the
    /// vehicle. Default 60km.
    #[serde(default = "default_ground_fallback_max_range_m")]
    pub max_range_m: f64,
    /// Give up (and refund) after this many minutes on the road. Unset =
    /// twice the planned drive plus 10 minutes, never under 20.
    #[serde(default)]
    pub timeout_mins: Option<u32>,
}

impl Default for HeloGroundFallbackCfg {
    fn default() -> Self {
        Self {
            enabled: true,
            template: FxHashMap::default(),
            speed_kph: default_ground_fallback_speed_kph(),
            arrive_m: default_ground_fallback_arrive_m(),
            max_range_m: default_ground_fallback_max_range_m(),
            timeout_mins: None,
        }
    }
}

pub(super) fn default_ground_fallback_speed_kph() -> f64 {
    50.0
}
pub(super) fn default_ground_fallback_arrive_m() -> f64 {
    400.0
}
pub(super) fn default_ground_fallback_max_range_m() -> f64 {
    60_000.0
}

#[cfg(test)]
mod ground_fallback_tests {
    use super::*;

    #[test]
    fn absent_block_means_on_with_defaults() {
        let h: HeloInsertionCfg = serde_json::from_value(serde_json::json!({})).unwrap();
        let g = h.ground_fallback;
        assert!(g.enabled);
        assert!(g.template.is_empty());
        assert_eq!((g.speed_kph, g.arrive_m, g.max_range_m), (50., 400., 60_000.));
        assert_eq!(g.timeout_mins, None);
    }

    #[test]
    fn a_partial_block_keeps_the_other_defaults() {
        let h: HeloInsertionCfg = serde_json::from_value(serde_json::json!({
            "ground_fallback": { "enabled": false, "template": { "Red": "RTRUCK" } }
        }))
        .unwrap();
        let g = h.ground_fallback;
        assert!(!g.enabled);
        assert_eq!(g.template.get(&Side::Red).map(|s| s.as_str()), Some("RTRUCK"));
        assert_eq!(g.arrive_m, 400.);
    }

    #[test]
    fn threat_avoidance_is_on_by_default() {
        let h: HeloInsertionCfg = serde_json::from_value(serde_json::json!({})).unwrap();
        let t = h.threat_avoidance;
        assert!(t.enabled && t.replan);
        assert_eq!((t.margin, t.noe_agl_m), (1.25, 40.));
        assert_eq!((t.max_replans, t.min_replan_secs), (4, 20));
        assert_eq!((t.unknown_radius_m, t.garrison_radius_m), (10_000., 3_000.));
        let h: HeloInsertionCfg = serde_json::from_value(serde_json::json!({
            "threat_avoidance": { "margin": 1.5, "replan": false }
        }))
        .unwrap();
        assert_eq!(h.threat_avoidance.margin, 1.5);
        assert!(!h.threat_avoidance.replan && h.threat_avoidance.enabled);
    }
}

pub(super) fn default_helo_troop_name() -> String {
    String::from("Standard")
}
pub(super) fn default_helo_altitude_m() -> f64 {
    500.0
}
pub(super) fn default_helo_speed_kph() -> f64 {
    220.0
}
pub(super) fn default_helo_supply_cost() -> i32 {
    50
}
pub(super) fn default_helo_supply_per_item() -> u32 {
    50
}
pub(super) fn default_helo_max_range_m() -> f64 {
    150_000.0
}
pub(super) fn default_helo_landing_radius_m() -> f64 {
    200.0
}
pub(super) fn default_helo_terrain_clearance_m() -> f64 {
    250.0
}
pub(super) fn default_helo_max_altitude_m() -> f64 {
    3500.0
}
pub(super) fn default_helo_waypoint_spacing_m() -> f64 {
    2500.0
}

pub(super) fn default_scenery_fallback_logi() -> u8 {
    100
}

impl Default for LogiFromSceneryCfg {
    fn default() -> Self {
        Self { fallback_logi: default_scenery_fallback_logi() }
    }
}

pub(super) fn default_supply_alert_threshold() -> u8 {
    20
}

pub(super) fn default_supply_auto_convoy_delay() -> u32 {
    300
}

pub(super) fn default_treasury_income_period() -> u32 {
    300
}
pub(super) fn default_treasury_income_amount() -> i64 {
    500
}
pub(super) fn default_obj_fund_period() -> u32 {
    120
}
pub(super) fn default_obj_fund_max() -> i32 {
    200
}
pub(super) fn default_commander_period() -> u32 {
    60
}
pub(super) fn default_holding_bonus() -> i32 {
    5
}
pub(super) fn default_objective_start_points() -> i32 {
    500
}
pub(super) fn default_barrage_cost() -> i64 {
    150
}
pub(super) fn default_ambush_cost() -> i64 {
    100
}
pub(super) fn default_cap_cost() -> i64 {
    250
}
pub(super) fn default_cap_min_friendly_pilots() -> u32 {
    2
}
pub(super) fn default_cap_cooldown_secs() -> u32 {
    300
}
