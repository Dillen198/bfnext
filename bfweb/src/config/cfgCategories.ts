import {
  Shield, Coins, HeartPulse, Truck, Flag, Radar, Crosshair, Gauge, Cloud, Trophy,
  type LucideIcon,
} from 'lucide-react'

export interface CfgCategory {
  key: string
  label: string
  icon: LucideIcon
  fields: string[]
}

/**
 * Groups Cfg's top-level fields into UI categories. Any field present in the
 * schema but not listed here automatically falls into the "Other" bucket
 * (see ConfigEditorPage) — new Cfg fields never silently disappear from the
 * editor, they just show up uncategorized until someone adds them here.
 */
export const CFG_CATEGORIES: CfgCategory[] = [
  {
    key: 'server', label: 'Server & Access', icon: Shield,
    fields: ['admins', 'banned', 'rules', 'name_filter', 'lock_sides', 'side_switches', 'shutdown', 'max_msgs_per_second', 'netidx_base'],
  },
  {
    key: 'economy', label: 'Points & Economy', icon: Coins,
    fields: ['points', 'objective_start_points', 'smart_commander'],
  },
  {
    key: 'lives', label: 'Lives & Combat Rules', icon: HeartPulse,
    fields: ['life_types', 'default_lives', 'limited_lives', 'era', 'last_stand', 'under_attack', 'counter_battery'],
  },
  {
    key: 'logistics', label: 'Cargo & Logistics', icon: Truck,
    fields: [
      'cargo', 'c130_cargo', 'c130_cargo_template', 'helo_cargo', 'helo_cargo_template',
      'crate_template', 'crate_load_distance', 'crate_spread', 'max_crates', 'ground_vehicle_cargo',
      'warehouse', 'logistics_exclusion', 'supply_alert_threshold', 'supply_auto_convoy_delay_secs',
      'factory', 'repair_crate', 'repair_time', 'repair_supply_cost', 'deploy_supply_cost',
    ],
  },
  {
    key: 'objectives', label: 'Objectives & Deployables', icon: Flag,
    fields: ['deployables', 'troops', 'dismount', 'unit_classification', 'extra_fixed_wing_objectives', 'frontline', 'actions', 'capture_consolidation_secs'],
  },
  {
    key: 'airdefense', label: 'SAM Sites & Air Defense', icon: Radar,
    fields: [
      'special_sam_sites', 'special_sam_capture_radius_m', 'special_sam_wake_distance',
      'radar_physics', 'iadn', 'elint', 'player_recon',
      'ground_radar_ewrs', 'airborne_ewrs', 'ewr_mode', 'ewr_delay', 'ewr_cull_distance',
    ],
  },
  {
    key: 'jtac', label: 'JTAC & Targeting', icon: Crosshair,
    fields: [
      'airborne_jtacs', 'jtac_priority', 'artillery', 'artillery_mission_range', 'artillery_min_range', 'alcm_mission_range',
      'weapon_target_exclusions', 'threatened_distance', 'threatened_cooldown',
    ],
  },
  {
    key: 'performance', label: 'Culling & Performance', icon: Gauge,
    fields: [
      'unit_cull_distance', 'ground_vehicle_cull_distance', 'lr_cull_distance', 'cull_after',
      'slow_timed_events_freq', 'weapon_spawn_radius', 'weapon_spawn_expiry_secs',
    ],
  },
  {
    key: 'environment', label: 'Environment', icon: Cloud,
    fields: ['weather_effects', 'time_of_day_effects'],
  },
  {
    key: 'campaign', label: 'Campaign & Events', icon: Trophy,
    fields: ['campaign_events', 'auto_reset', 'carrier', 'pilot_experience', 'csar'],
  },
]
