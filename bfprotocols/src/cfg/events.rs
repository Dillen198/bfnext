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

//! Weather and time-of-day effects, dynamic campaign events, CAP templates, pilot experience.

use super::*;

/// Weather effects on logistics and operations
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct WeatherEffectsCfg {
    /// Convoy speed multiplier in rain (0.0-1.0)
    #[serde(default = "default_rain_speed_mult")]
    pub rain_speed_multiplier: f64,
    /// Convoy speed multiplier in thunderstorm (0.0-1.0)
    #[serde(default = "default_storm_speed_mult")]
    pub thunderstorm_speed_multiplier: f64,
    /// Convoy speed multiplier in snow (0.0-1.0)
    #[serde(default = "default_snow_speed_mult")]
    pub snow_speed_multiplier: f64,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// Minimum visibility (meters) for fixed-wing takeoff
    #[serde(default = "default_min_vis_fixed_wing")]
    pub min_visibility_fixed_wing: f64,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// Disable helicopter operations in thunderstorms
    #[serde(default = "default_true")]
    pub no_helo_in_thunderstorm: bool,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// EWR detection range multiplier in bad weather (0.0-1.0)
    #[serde(default = "default_ewr_weather_mult")]
    pub ewr_weather_range_multiplier: f64,
}

pub(super) fn default_rain_speed_mult() -> f64 {
    0.8
}
pub(super) fn default_storm_speed_mult() -> f64 {
    0.6
}
pub(super) fn default_snow_speed_mult() -> f64 {
    0.5
}
pub(super) fn default_min_vis_fixed_wing() -> f64 {
    800.0
}
pub(super) fn default_ewr_weather_mult() -> f64 {
    0.85
}

impl Default for WeatherEffectsCfg {
    fn default() -> Self {
        Self {
            rain_speed_multiplier: default_rain_speed_mult(),
            thunderstorm_speed_multiplier: default_storm_speed_mult(),
            snow_speed_multiplier: default_snow_speed_mult(),
            min_visibility_fixed_wing: default_min_vis_fixed_wing(),
            no_helo_in_thunderstorm: true,
            ewr_weather_range_multiplier: default_ewr_weather_mult(),
        }
    }
}

/// Time-of-day effects on gameplay
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct TimeOfDayEffectsCfg {
    /// Points multiplier for kills during night hours (e.g. 1.5 = 50% bonus)
    #[serde(default = "default_night_kill_bonus")]
    pub night_kill_bonus: f64,
    /// Hour when night begins (0-23, local mission time)
    #[serde(default = "default_night_start")]
    pub night_start_hour: u8,
    /// Hour when night ends (0-23, local mission time)
    #[serde(default = "default_night_end")]
    pub night_end_hour: u8,
}

pub(super) fn default_night_kill_bonus() -> f64 {
    1.5
}
pub(super) fn default_night_start() -> u8 {
    22
}
pub(super) fn default_night_end() -> u8 {
    6
}

impl Default for TimeOfDayEffectsCfg {
    fn default() -> Self {
        Self {
            night_kill_bonus: default_night_kill_bonus(),
            night_start_hour: default_night_start(),
            night_end_hour: default_night_end(),
        }
    }
}

/// Dynamic campaign events configuration
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct CampaignEventsCfg {
    /// Enable the dynamic events system
    #[serde(default)]
    pub enabled: bool,
    /// Minimum seconds between event checks
    #[serde(default = "default_event_check_interval")]
    pub check_interval_secs: u32,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// Probability (0.0-1.0) of spawning an event per check
    #[serde(default = "default_event_probability")]
    pub event_probability: f64,
    /// Maximum number of concurrent active events per side
    #[serde(default = "default_max_events")]
    pub max_concurrent_events: u32,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// Points awarded for successful VIP extraction
    #[serde(default = "default_vip_reward")]
    pub vip_reward_points: i32,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// Points per civilian evacuated
    #[serde(default = "default_evac_reward")]
    pub evacuation_reward_per_civilian: i32,
    /// Enable artillery barrage events
    #[serde(default = "default_true")]
    pub barrage_enabled: bool,
    /// Duration in seconds of a barrage event
    #[serde(default = "default_barrage_duration")]
    pub barrage_duration_secs: u32,
    /// FireAtPoint scatter radius (metres) used by the auto-barrage event. Default: 500.
    #[serde(default = "default_barrage_radius_m")]
    pub barrage_radius_m: f64,
    /// Maximum number of groups the auto-barrage simultaneously orders. Default: 5.
    #[serde(default = "default_barrage_max_groups")]
    pub barrage_max_groups: usize,
    /// Enable convoy ambush events
    #[serde(default = "default_true")]
    pub ambush_enabled: bool,
    /// Duration in seconds before an ambush expires without contact
    #[serde(default = "default_ambush_duration")]
    pub ambush_duration_secs: u32,
    /// Enable automatic enemy CAP intercept spawns when players are deep in enemy territory
    #[serde(default)]
    pub enemy_cap_enabled: bool,
    /// Roster of CAP templates for red / blue: the .miz group names a scramble
    /// may spawn. One entry is picked at random by weight, from those whose
    /// `min_threat` the incursion actually meets -- a pair of MiG-21s for a
    /// two-ship probe, Flankers when six players push -- instead of the same
    /// flight every single time. A roster of one behaves exactly like the
    /// single `cap_template_red` / `cap_template_blue` names these replaced.
    ///
    /// Required when `enemy_cap_enabled` is set: an empty roster is rejected at
    /// config load rather than discovered as a silently cancelled scramble
    /// hours into a session.
    #[serde(default)]
    pub cap_templates_red: Vec<CapTemplateCfg>,
    #[serde(default)]
    pub cap_templates_blue: Vec<CapTemplateCfg>,
    /// How long (seconds) a CAP orbit lasts before despawning. Default: 600.
    #[serde(default = "default_cap_duration")]
    pub cap_duration_secs: u32,
    /// Barometric altitude (metres MSL) AI CAP flights hold and intercept at.
    /// Default 7000 (~FL230) -- without this DCS parks them low and slow.
    #[serde(default = "default_cap_altitude_m")]
    pub cap_altitude_m: f64,
    /// True airspeed (m/s) for AI CAP orbit and intercept legs. Default 230
    /// (~450 kt).
    #[serde(default = "default_cap_speed_ms")]
    pub cap_speed_ms: f64,
    /// How far (metres) a CAP flight may push from the objective it is
    /// defending toward a detected threat. Keeps interceptors leaning forward
    /// without chasing contacts deep across the front line. Default 60000.
    #[serde(default = "default_cap_max_push_m")]
    pub cap_max_push_m: f64,
    /// Radius (metres) of a CAP flight's engage-in-zone order around its
    /// station. This is the leash: the flight will chase an air contact this
    /// far from where it is stationed and no further. Keep it modest -- a large
    /// value plus `cap_max_push_m` lets interceptors wander into enemy SAM
    /// belts and get swatted, which reads in-game as CAP that spawns, dies, and
    /// respawns on a loop. Default 45000.
    #[serde(default = "default_cap_engage_radius_m")]
    pub cap_engage_radius_m: f64,
    /// If a CAP flight's side has painted no workable threat for this many
    /// seconds, it is sent home (RTB) and despawned early rather than loitering
    /// out its full `cap_duration_secs`. Keeps the air picture matched to the
    /// actual threat instead of accumulating idle flights. Default 240.
    #[serde(default = "default_cap_idle_rtb_secs")]
    pub cap_idle_rtb_secs: u32,
    /// UNUSED: nothing in the engine reads this, so it has no effect. Kept so
    /// existing configs still load; `Cfg::load` warns when it is set.
    /// Distance from an objective (metres) within which a CAP orbit is placed. Default: 15000.
    #[serde(default = "default_cap_orbit_radius")]
    pub cap_orbit_radius_m: f64,
    /// Chance (0.0-1.0) that a reactive CAP answers an incursion at once;
    /// otherwise it launches 2-8 minutes later, so fighters don't always
    /// arrive on the same beat. 1 = always at once. Default: 0.35.
    #[serde(default = "default_cap_probability")]
    pub cap_probability: f64,
    /// Seconds a troop must continuously occupy an objective before it is captured.
    /// Set to 0 to disable momentum (instant capture). Default: 60.
    #[serde(default = "default_capture_time")]
    pub capture_time_secs: u32,
    /// Fraction (0.0–1.0) of an objective's defending units that must be destroyed
    /// before it becomes capturable, in addition to logi == 0. Default: 0.0 (disabled).
    /// Example: 0.30 requires 30% of defenders to be killed first.
    #[serde(default)]
    pub capture_min_unit_pct_destroyed: f64,
    /// Seconds after a base changes hands (captured, or fell to Neutral) during
    /// which no new capture timer can start against it. Gives the new owner a
    /// window to defend / reinforce and stops a zone with capture troops from
    /// both sides in it from flipping on a loop. Default 120.
    #[serde(default = "default_capture_cooldown_secs")]
    pub capture_cooldown_secs: u32,
    /// Fraction (0.0–1.0) of the new owner's *non-SAM* combat garrison (AAA,
    /// infantry, armour) brought back the moment a base is captured -- spent
    /// AAA/infantry-first. The rest, and all of the SAMs, rebuild gradually
    /// through normal auto-repair (or must be delivered by crate). 1.0 = full
    /// non-SAM garrison; lower = a freshly-taken base is a light target you
    /// have to build up. Default 0.25.
    #[serde(default = "default_capture_garrison_revive_fraction")]
    pub capture_garrison_revive_fraction: f64,
    /// Also bring back a share of the new owner's SAM garrison (short/medium/
    /// long-range) on capture. Default false -- a freshly-taken base has no
    /// working SAM cover until it's rebuilt or resupplied, which is both more
    /// realistic and stops "fly over a wrecked base, get killed by an
    /// instant Pantsir".
    #[serde(default)]
    pub capture_garrison_revive_include_sam: bool,
    /// Distance (metres) from an enemy-owned objective within which an in-air player aircraft
    /// is considered a threat and triggers a reactive CAP spawn. Default: 60000 (60 km).
    #[serde(default = "default_cap_trigger_radius")]
    pub cap_trigger_radius_m: f64,
    /// Maximum number of CAP events active simultaneously across both sides. Default: 3.
    #[serde(default = "default_cap_max_concurrent")]
    pub cap_max_concurrent: usize,
    /// Maximum reactive EnemyCap events active for a single side at the same time.
    /// Prevents one side from consuming all concurrent CAP slots. Default: 2.
    #[serde(default = "default_cap_max_per_side")]
    pub cap_max_per_side: usize,
    /// Minimum number of enemy aircraft within `cap_trigger_radius_m` of an objective
    /// before a reactive CAP spawn is triggered. A lone scout will not trigger CAP;
    /// a real incursion of 2+ aircraft will. Default: 2.
    #[serde(default = "default_cap_min_threat")]
    pub cap_min_threat_count: u32,
    /// Cooldown (seconds) after a reactive CAP wave ends -- shot down OR flown
    /// its full duration and RTB'd -- before ANOTHER WAVE MAY LAUNCH FROM THE
    /// SAME FIELD. Per airbase, not per side: one wave ending on the east flank
    /// no longer locks the entire coalition out of the air, which is what made
    /// `cap_max_per_side = 2` effectively mean one. Side-wide throttling is
    /// `cap_max_sorties_per_hour`. Default: 1800 (30 minutes).
    #[serde(default = "default_cap_respawn_cooldown")]
    pub cap_respawn_cooldown_secs: u64,
    /// Side-wide ceiling on reactive CAP launches per rolling hour. This is the
    /// coalition's sortie budget -- it stops a long busy night turning into an
    /// endless conveyor of AI fighters now that the per-field cooldown no
    /// longer gates the whole side. 0 = unlimited. Default: 6.
    #[serde(default = "default_cap_max_sorties_per_hour")]
    pub cap_max_sorties_per_hour: u32,
    /// Grace (seconds) granted to a CAP flight that still has a live contact
    /// inside its engage radius when its clock runs out. Without this, a flight
    /// in the middle of a merge is told to go home, which reads in-game as the
    /// AI disengaging for no reason. The flight keeps earning extensions while
    /// it is in contact, up to `cap_duration_secs` of total overtime, then goes
    /// home regardless. 0 disables. Default: 180.
    #[serde(default = "default_cap_engaged_extension_secs")]
    pub cap_engaged_extension_secs: u32,
    /// When true, reactive CAP triggers on the actual count of enemy fixed-wing
    /// PLAYERS airborne on the attacking side, not on what the defending side's
    /// radar network has painted. Use this if a side (usually Red) never
    /// scrambles because its EWR / SAM search radars are down, dark (EMCON), or
    /// have coverage gaps. Costs the fog-of-war realism -- CAP will vector
    /// toward players the defender can't "see". Default: false.
    #[serde(default)]
    pub cap_trigger_on_known_players: bool,
    /// Air-balance CAP: also scramble for whichever side has this many FEWER
    /// fixed-wing players airborne than the other -- so if Blue puts up 4 and
    /// Red 1, Red gets a CAP even without a specific detected incursion. Uses
    /// actual player counts (radar-independent). 0 disables. Default: 2.
    #[serde(default = "default_cap_balance_gap")]
    pub cap_balance_gap: u32,
    /// Scramble CAP COLD -- engines off on the ramp, AI runs its own startup --
    /// instead of hot with rotors/engines already turning. Cold is the default
    /// because the startup + taxi + takeoff time is what gives a defending
    /// flight a realistic reaction delay, and it is the warning the attacking
    /// side gets. Set false if you want intercepts airborne sooner.
    #[serde(default = "default_true")]
    pub cap_cold_start: bool,

    // ── Reactive helicopter patrol (the rotary mirror of reactive CAP) ──────
    // Fixed-wing CAP deliberately ignores helicopters: fighters hunting NOE
    // helos is lopsided, and SAMs are expected to cover that. So a session
    // where only helo pilots are flying gets no AI response at all. These
    // knobs answer helicopter players with armed AI helicopters instead --
    // same trigger logic, same spawn/station/RTB machinery, rotary numbers.
    /// Enable reactive AI helicopter patrols. Requires a non-empty
    /// `helo_templates_red`/`_blue` roster naming helicopter-section groups
    /// that exist in the .miz. Default: false.
    #[serde(default)]
    pub enemy_helo_enabled: bool,
    /// As `cap_templates_red` / `_blue`, for the helicopter patrol. Every entry
    /// must name a HELICOPTER-section group in the mission file -- the
    /// ground-start rewrite picks helipads (and open ground at padless FOBs)
    /// only when the template's category is Helicopter.
    ///
    /// Required when `enemy_helo_enabled` is set.
    #[serde(default)]
    pub helo_templates_red: Vec<CapTemplateCfg>,
    #[serde(default)]
    pub helo_templates_blue: Vec<CapTemplateCfg>,
    /// How long (seconds) a helicopter patrol stays up before it RTBs and
    /// despawns. Longer than CAP's -- helos cruise at a third of the speed and
    /// need the time to reach their station. Default: 1800.
    #[serde(default = "default_helo_duration")]
    pub helo_duration_secs: u32,
    /// Patrol height in metres ABOVE THE TERRAIN at the station (unlike
    /// `cap_altitude_m`, which is absolute MSL). Terrain height under the
    /// orbit point is sampled at task time and this is added to it, so the
    /// same number works over the coast and over Hermon. Default: 300.
    #[serde(default = "default_helo_altitude_agl_m")]
    pub helo_altitude_agl_m: f64,
    /// True airspeed (m/s) for patrol orbit and intercept legs. Default: 60
    /// (~115 kt) -- a speed every attack helo in DCS can actually hold.
    #[serde(default = "default_helo_speed_ms")]
    pub helo_speed_ms: f64,
    /// How far (metres) a patrol may push from the objective it defends toward
    /// a contact. Much shorter than CAP's leash -- a helo that wanders is a
    /// helo that runs out of fuel or finds a SAM. Default: 30000.
    #[serde(default = "default_helo_max_push_m")]
    pub helo_max_push_m: f64,
    /// Engage-in-zone radius (metres) around the patrol's station. Roughly the
    /// reach of helicopter weapons plus a short chase. Default: 15000.
    #[serde(default = "default_helo_engage_radius_m")]
    pub helo_engage_radius_m: f64,
    /// Send the patrol home if its side has had nothing to work for this many
    /// seconds. Default: 420.
    #[serde(default = "default_helo_idle_rtb_secs")]
    pub helo_idle_rtb_secs: u32,
    /// Distance (metres) within which two enemy helicopter contacts count as
    /// one incursion, and the radius the patrol is placed relative to. Smaller
    /// than CAP's because the rotary fight happens at much shorter range.
    /// Default: 40000.
    #[serde(default = "default_helo_trigger_radius")]
    pub helo_trigger_radius_m: f64,
    /// Maximum helicopter patrols active across both sides at once. Counted
    /// separately from CAP, so a busy air war doesn't starve the rotary
    /// response (or vice versa). Default: 2.
    #[serde(default = "default_helo_max_concurrent")]
    pub helo_max_concurrent: usize,
    /// Maximum helicopter patrols one side may have up at once. Default: 1.
    #[serde(default = "default_helo_max_per_side")]
    pub helo_max_per_side: usize,
    /// Enemy helicopter players near an owned objective needed to scramble a
    /// patrol. Default: 2.
    #[serde(default = "default_helo_min_threat")]
    pub helo_min_threat_count: u32,
    /// Cooldown (seconds) after a patrol ends before ANOTHER MAY LAUNCH FROM
    /// THE SAME FIELD (per airbase/FARP/FOB, not per side -- see
    /// `cap_respawn_cooldown_secs`). Default: 900 (15 minutes).
    #[serde(default = "default_helo_respawn_cooldown")]
    pub helo_respawn_cooldown_secs: u64,
    /// Side-wide ceiling on helicopter-patrol launches per rolling hour.
    /// 0 = unlimited. Default: 4.
    #[serde(default = "default_helo_max_sorties_per_hour")]
    pub helo_max_sorties_per_hour: u32,
    /// As `cap_trigger_on_known_players`, for helicopters. Usually wanted:
    /// radar rarely paints a helo flying NOE, so with this off the threat path
    /// almost never fires and only the balance gap does. Default: true.
    #[serde(default = "default_true")]
    pub helo_trigger_on_known_players: bool,
    /// Balance trigger: launch a patrol for whichever side has this many FEWER
    /// helicopter players airborne than the other. This is the one that covers
    /// "tonight it's only helo pilots" -- 0 disables. Default: 2.
    #[serde(default = "default_helo_balance_gap")]
    pub helo_balance_gap: u32,
    /// As `cap_cold_start`, for the helicopter patrol. Cold by default for the
    /// same reason -- and a helo that spools up on its pad is a helo you can
    /// hear coming.
    #[serde(default = "default_true")]
    pub helo_cold_start: bool,
    /// How often (seconds) to call world.removeJunk to clean up debris. Default: 300 (5 min).
    /// Set to 0 to disable.
    #[serde(default = "default_junk_removal_interval")]
    pub junk_removal_interval_secs: u32,
    /// Sphere radius (metres) passed to world.removeJunk. Default: 500_000 (covers most maps).
    #[serde(default = "default_junk_removal_radius")]
    pub junk_removal_radius_m: f64,
}

pub(super) fn default_cap_respawn_cooldown() -> u64 {
    1800
}
pub(super) fn default_cap_balance_gap() -> u32 {
    2
}
pub(super) fn default_cap_max_sorties_per_hour() -> u32 {
    6
}
pub(super) fn default_helo_max_sorties_per_hour() -> u32 {
    4
}
pub(super) fn default_cap_engaged_extension_secs() -> u32 {
    180
}
pub(super) fn default_cap_template_weight() -> u32 {
    1
}

/// One entry in a reactive-response template roster
/// (`cap_templates_red`/`_blue`, `helo_templates_red`/`_blue`).
///
/// A scramble filters the roster to the entries this incursion qualifies for
/// (`min_threat` at or below the size of the detected cluster), then picks one
/// at random weighted by `weight`. If every entry is out of reach -- a roster
/// where the cheapest tier needs 3 contacts and only 2 showed up -- the whole
/// roster is used rather than nothing scrambling.
///
/// Templates are tried in pick order: if the chosen group is missing from the
/// .miz the next candidate is tried, so one typo'd name degrades the roster
/// instead of cancelling every scramble.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct CapTemplateCfg {
    /// Group name in the .miz, exactly as for the single-template knobs.
    pub template: String,
    /// Relative pick weight among eligible entries. 3 is picked three times as
    /// often as 1. Default: 1.
    #[serde(default = "default_cap_template_weight")]
    pub weight: u32,
    /// Smallest detected contact count this entry answers. Use it to hold the
    /// heavy metal back for real pushes: `min_threat: 4` on a Flanker flight
    /// means two players probing get MiGs and eight get Flankers. Default: 0
    /// (always eligible).
    #[serde(default)]
    pub min_threat: u32,
}

pub(super) fn default_helo_duration() -> u32 {
    1800
}
pub(super) fn default_helo_altitude_agl_m() -> f64 {
    300.0
}
pub(super) fn default_helo_speed_ms() -> f64 {
    60.0
}
pub(super) fn default_helo_max_push_m() -> f64 {
    30_000.0
}
pub(super) fn default_helo_engage_radius_m() -> f64 {
    15_000.0
}
pub(super) fn default_helo_idle_rtb_secs() -> u32 {
    420
}
pub(super) fn default_helo_trigger_radius() -> f64 {
    40_000.0
}
pub(super) fn default_helo_max_concurrent() -> usize {
    2
}
pub(super) fn default_helo_max_per_side() -> usize {
    1
}
pub(super) fn default_helo_min_threat() -> u32 {
    2
}
pub(super) fn default_helo_respawn_cooldown() -> u64 {
    900
}
pub(super) fn default_helo_balance_gap() -> u32 {
    2
}

pub(super) fn default_event_check_interval() -> u32 {
    300
}
pub(super) fn default_event_probability() -> f64 {
    0.15
}
pub(super) fn default_max_events() -> u32 {
    3
}
pub(super) fn default_vip_reward() -> i32 {
    300
}
pub(super) fn default_evac_reward() -> i32 {
    50
}
pub(super) fn default_barrage_duration() -> u32 {
    300
}
pub(super) fn default_ambush_duration() -> u32 {
    600
}
pub(super) fn default_cap_altitude_m() -> f64 {
    8000.0
}
pub(super) fn default_cap_speed_ms() -> f64 {
    250.0
}
pub(super) fn default_cap_max_push_m() -> f64 {
    60000.0
}
pub(super) fn default_cap_engage_radius_m() -> f64 {
    45000.0
}
pub(super) fn default_cap_idle_rtb_secs() -> u32 {
    240
}
pub(super) fn default_cap_duration() -> u32 {
    600
}
pub(super) fn default_cap_orbit_radius() -> f64 {
    15_000.0
}
pub(super) fn default_cap_probability() -> f64 {
    0.35
}
pub(super) fn default_capture_time() -> u32 {
    180
}
pub(super) fn default_capture_garrison_revive_fraction() -> f64 {
    0.25
}
pub(super) fn default_capture_cooldown_secs() -> u32 {
    120
}
pub(super) fn default_barrage_radius_m() -> f64 {
    500.0
}
pub(super) fn default_barrage_max_groups() -> usize {
    5
}
pub(super) fn default_cap_trigger_radius() -> f64 {
    90_000.0
}
pub(super) fn default_cap_max_concurrent() -> usize {
    3
}
pub(super) fn default_cap_max_per_side() -> usize {
    2
}
pub(super) fn default_cap_min_threat() -> u32 {
    2
}
pub(super) fn default_junk_removal_interval() -> u32 {
    300
}
pub(super) fn default_junk_removal_radius() -> f64 {
    500_000.0
}

impl Default for CampaignEventsCfg {
    fn default() -> Self {
        Self {
            enabled: false,
            check_interval_secs: default_event_check_interval(),
            event_probability: default_event_probability(),
            max_concurrent_events: default_max_events(),
            vip_reward_points: default_vip_reward(),
            evacuation_reward_per_civilian: default_evac_reward(),
            barrage_enabled: true,
            barrage_duration_secs: default_barrage_duration(),
            ambush_enabled: true,
            ambush_duration_secs: default_ambush_duration(),
            enemy_cap_enabled: false,
            cap_templates_red: Vec::new(),
            cap_templates_blue: Vec::new(),
            cap_duration_secs: default_cap_duration(),
            cap_altitude_m: default_cap_altitude_m(),
            cap_speed_ms: default_cap_speed_ms(),
            cap_max_push_m: default_cap_max_push_m(),
            cap_engage_radius_m: default_cap_engage_radius_m(),
            cap_idle_rtb_secs: default_cap_idle_rtb_secs(),
            cap_orbit_radius_m: default_cap_orbit_radius(),
            cap_probability: default_cap_probability(),
            capture_time_secs: default_capture_time(),
            capture_min_unit_pct_destroyed: 0.0,
            capture_cooldown_secs: default_capture_cooldown_secs(),
            capture_garrison_revive_fraction: default_capture_garrison_revive_fraction(),
            capture_garrison_revive_include_sam: false,
            barrage_radius_m: default_barrage_radius_m(),
            barrage_max_groups: default_barrage_max_groups(),
            cap_trigger_radius_m: default_cap_trigger_radius(),
            cap_max_concurrent: default_cap_max_concurrent(),
            cap_max_per_side: default_cap_max_per_side(),
            cap_min_threat_count: default_cap_min_threat(),
            cap_respawn_cooldown_secs: default_cap_respawn_cooldown(),
            cap_max_sorties_per_hour: default_cap_max_sorties_per_hour(),
            cap_engaged_extension_secs: default_cap_engaged_extension_secs(),
            cap_trigger_on_known_players: false,
            cap_balance_gap: default_cap_balance_gap(),
            cap_cold_start: true,
            enemy_helo_enabled: false,
            helo_templates_red: Vec::new(),
            helo_templates_blue: Vec::new(),
            helo_duration_secs: default_helo_duration(),
            helo_altitude_agl_m: default_helo_altitude_agl_m(),
            helo_speed_ms: default_helo_speed_ms(),
            helo_max_push_m: default_helo_max_push_m(),
            helo_engage_radius_m: default_helo_engage_radius_m(),
            helo_idle_rtb_secs: default_helo_idle_rtb_secs(),
            helo_trigger_radius_m: default_helo_trigger_radius(),
            helo_max_concurrent: default_helo_max_concurrent(),
            helo_max_per_side: default_helo_max_per_side(),
            helo_min_threat_count: default_helo_min_threat(),
            helo_respawn_cooldown_secs: default_helo_respawn_cooldown(),
            helo_max_sorties_per_hour: default_helo_max_sorties_per_hour(),
            helo_trigger_on_known_players: true,
            helo_balance_gap: default_helo_balance_gap(),
            helo_cold_start: true,
            junk_removal_interval_secs: default_junk_removal_interval(),
            junk_removal_radius_m: default_junk_removal_radius(),
        }
    }
}

/// Pilot experience and progression configuration
///
/// UNUSED: nothing in the engine implements pilot experience; see
/// `Cfg::pilot_experience`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
// #[serde(deny_unknown_fields)]
pub struct PilotExperienceCfg {
    /// Enable the pilot experience system
    #[serde(default)]
    pub enabled: bool,
    /// XP awarded per air kill
    #[serde(default = "default_xp_air_kill")]
    pub xp_per_air_kill: u32,
    /// XP awarded per ground kill
    #[serde(default = "default_xp_ground_kill")]
    pub xp_per_ground_kill: u32,
    /// XP awarded per successful sortie (land safely)
    #[serde(default = "default_xp_sortie")]
    pub xp_per_sortie: u32,
    /// XP awarded per cargo delivery
    #[serde(default = "default_xp_delivery")]
    pub xp_per_delivery: u32,
    /// XP thresholds for rank progression
    #[serde(default = "default_rank_thresholds")]
    pub rank_thresholds: Vec<(u32, String)>,
}

pub(super) fn default_xp_air_kill() -> u32 {
    100
}
pub(super) fn default_xp_ground_kill() -> u32 {
    50
}
pub(super) fn default_xp_sortie() -> u32 {
    25
}
pub(super) fn default_xp_delivery() -> u32 {
    75
}
pub(super) fn default_rank_thresholds() -> Vec<(u32, String)> {
    vec![
        (0, "Cadet".into()),
        (500, "2nd Lieutenant".into()),
        (1500, "1st Lieutenant".into()),
        (3000, "Captain".into()),
        (6000, "Major".into()),
        (10000, "Lieutenant Colonel".into()),
        (20000, "Colonel".into()),
    ]
}

impl Default for PilotExperienceCfg {
    fn default() -> Self {
        Self {
            enabled: false,
            xp_per_air_kill: default_xp_air_kill(),
            xp_per_ground_kill: default_xp_ground_kill(),
            xp_per_sortie: default_xp_sortie(),
            xp_per_delivery: default_xp_delivery(),
            rank_thresholds: default_rank_thresholds(),
        }
    }
}
