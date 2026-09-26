/** Spawn catalogue and weapon ballistics fixtures. */
import type { ParamSpec, SpawnCatalog, WeaponCalibration, WeaponDb } from '../types'

const skill: ParamSpec = {
  key: 'skill',
  label: 'Skill',
  kind: {
    type: 'choice',
    options: [
      { value: 'Average', label: 'Average' },
      { value: 'Good', label: 'Good' },
      { value: 'High', label: 'High' },
      { value: 'Excellent', label: 'Excellent' },
    ],
  },
  default: 'High',
}

const count = (max: number, def = '1'): ParamSpec => ({
  key: 'count',
  label: 'Flight size',
  kind: { type: 'number', min: 1, max, step: 1, unit: 'ship' },
  default: def,
})

const range = (def: string, min = 5, max = 80): ParamSpec => ({
  key: 'range_nm',
  label: 'Range from you',
  kind: { type: 'number', min, max, step: 1, unit: 'nm' },
  default: def,
})

const altFt = (def: string, min = 500, max = 40000, step = 500): ParamSpec => ({
  key: 'alt_ft',
  label: 'Altitude',
  kind: { type: 'number', min, max, step, unit: 'ft' },
  default: def,
})

export const CATALOG: SpawnCatalog = {
  max_active_per_player: 3,
  despawn_after_s: 3600,
  items: [
    {
      id: 'bfm_bandit',
      category: 'air_to_air',
      label: 'BFM bandit',
      description: 'A single fighter set up for a 1v1 at the merge. Missile trainer stays on: its shots are destroyed before they reach you.',
      relative_to_player: true,
      instructor_only: false,
      params: [
        {
          key: 'type', label: 'Type', default: 'MiG-29A',
          kind: { type: 'choice', options: [
            { value: 'MiG-29A', label: 'MiG-29A Fulcrum' }, { value: 'Su-27', label: 'Su-27 Flanker' },
            { value: 'F-5E-3', label: 'F-5E Tiger II' }, { value: 'MiG-21Bis', label: 'MiG-21bis' },
          ] },
        },
        {
          key: 'setup', label: 'Setup', default: 'neutral',
          kind: { type: 'choice', options: [
            { value: 'offensive', label: 'Offensive (you at his 6, 3,000 ft)' },
            { value: 'neutral', label: 'Neutral (head-on pass)' },
            { value: 'defensive', label: 'Defensive (him at your 6)' },
          ] },
        },
        skill,
        { key: 'guns_only', label: 'Weapons', default: 'guns', kind: { type: 'choice', options: [
          { value: 'guns', label: 'Guns only' }, { value: 'ir', label: 'Guns + IR missiles' },
        ] } },
      ],
    },
    {
      id: 'bvr_flight',
      category: 'air_to_air',
      label: 'BVR flight',
      description: 'A flight of radar-missile fighters pointed at you from range. Good for launch-and-leave, cranking and notching practice.',
      relative_to_player: true,
      instructor_only: false,
      params: [
        { key: 'type', label: 'Type', default: 'Su-27', kind: { type: 'choice', options: [
          { value: 'Su-27', label: 'Su-27 (R-27ER/ET)' }, { value: 'MiG-29S', label: 'MiG-29S (R-77)' },
          { value: 'J-11A', label: 'J-11A (R-77)' }, { value: 'F-15C', label: 'F-15C (AIM-120C) — red-air' },
        ] } },
        count(4, '2'),
        range('40', 20, 80),
        altFt('25000', 5000, 40000, 1000),
        skill,
      ],
    },
    {
      id: 'sam_threat',
      category: 'air_to_ground',
      label: 'SAM threat (trainer)',
      description: 'A live SAM site that shoots at you with the missile trainer on. Practise defending, SEAD and HARM shots.',
      relative_to_player: true,
      instructor_only: false,
      params: [
        { key: 'system', label: 'System', default: 'SA-8', kind: { type: 'choice', options: [
          { value: 'SA-8', label: 'SA-8 Gecko' }, { value: 'SA-15', label: 'SA-15 Gauntlet' },
          { value: 'SA-11', label: 'SA-11 Gadfly' }, { value: 'SA-6', label: 'SA-6 Gainful' },
        ] } },
        range('15', 5, 40),
      ],
    },
    {
      id: 'target_armor',
      category: 'air_to_ground',
      label: 'Armour column',
      description: 'A static or moving armour group at a chosen range station, scored like the fixed targets.',
      relative_to_player: false,
      instructor_only: false,
      params: [
        { key: 'station', label: 'Where', default: 'range_b_array', kind: { type: 'choice', options: [
          { value: 'range_b_array', label: 'Range B — Tactical Array' }, { value: 'range_b_convoy', label: 'Range B — Convoy road' },
          { value: 'range_c_laser', label: 'Range C' },
        ] } },
        count(8, '4'),
        { key: 'moving', label: 'Movement', default: 'static', kind: { type: 'choice', options: [
          { value: 'static', label: 'Static' }, { value: 'moving', label: 'Moving, 25 km/h' },
        ] } },
      ],
    },
    {
      id: 'tanker_kc135',
      category: 'tanker',
      label: 'KC-135 (boom)',
      description: 'An on-demand boom tanker on a racetrack ahead of you.',
      relative_to_player: true,
      instructor_only: false,
      params: [altFt('22000', 10000, 30000, 1000), { key: 'speed_kts', label: 'Speed', default: '275', kind: { type: 'number', min: 220, max: 320, step: 5, unit: 'kt IAS' } }],
    },
    {
      id: 'tanker_mprs',
      category: 'tanker',
      label: 'KC-135MPRS (drogue)',
      description: 'Wing-pod drogue tanker for probe-equipped jets.',
      relative_to_player: true,
      instructor_only: false,
      params: [altFt('24000', 10000, 30000, 1000)],
    },
    {
      id: 'tanker_kc130',
      category: 'tanker',
      label: 'KC-130 (drogue)',
      description: 'Slow drogue tanker for Harriers and helicopters.',
      relative_to_player: true,
      instructor_only: false,
      params: [altFt('15000', 3000, 20000, 500)],
    },
    {
      id: 'tanker_il78',
      category: 'tanker',
      label: 'IL-78M (drogue)',
      description: 'Red-side drogue tanker.',
      relative_to_player: true,
      instructor_only: true,
      params: [altFt('20000', 10000, 30000, 1000)],
    },
    {
      id: 'recovery_tanker',
      category: 'tanker',
      label: 'Recovery tanker',
      description: 'A carrier recovery tanker overhead the boat at 6,000 ft.',
      relative_to_player: false,
      instructor_only: false,
      params: [
        { key: 'type', label: 'Type', default: 'S-3B Tanker', kind: { type: 'choice', options: [
          { value: 'S-3B Tanker', label: 'S-3B Viking' }, { value: 'A-6E', label: 'A-6E Intruder' },
        ] } },
        { key: 'carrier', label: 'Carrier', default: 'cvn73', kind: { type: 'choice', options: [{ value: 'cvn73', label: 'CVN-73 George Washington' }] } },
      ],
    },
    {
      id: 'ship_group',
      category: 'naval',
      label: 'Surface action group',
      description: 'Enemy ships in the anti-ship box. Choose whether they defend themselves.',
      relative_to_player: false,
      instructor_only: false,
      params: [
        { key: 'composition', label: 'Ships', default: 'molniya_cargo', kind: { type: 'choice', options: [
          { value: 'molniya_cargo', label: 'Molniya + 2 cargo' }, { value: 'grisha', label: 'Grisha x2' },
          { value: 'neustrashimy', label: 'Neustrashimy frigate' },
        ] } },
        { key: 'defended', label: 'Air defence', default: 'off', kind: { type: 'choice', options: [
          { value: 'off', label: 'Weapons hold' }, { value: 'on', label: 'Weapons free (trainer on)' },
        ] } },
      ],
    },
    {
      id: 'ship_moving_target',
      category: 'naval',
      label: 'Fast attack craft',
      description: 'A pair of small boats running at 30 kt, for guns and rockets.',
      relative_to_player: true,
      instructor_only: false,
      params: [range('10', 3, 30)],
    },
    {
      id: 'helo_sling',
      category: 'helo',
      label: 'Sling-load cargo',
      description: 'A cargo crate at a pickup zone with a scored drop zone.',
      relative_to_player: false,
      instructor_only: false,
      params: [
        { key: 'course', label: 'Course', default: 'sling1', kind: { type: 'choice', options: [
          { value: 'sling1', label: 'Sling Course 1' }, { value: 'sling2', label: 'Sling Course 2 (ridge)' },
        ] } },
        { key: 'mass_kg', label: 'Mass', default: '900', kind: { type: 'number', min: 300, max: 5000, step: 100, unit: 'kg' } },
      ],
    },
    {
      id: 'helo_troops',
      category: 'helo',
      label: 'Troop pickup',
      description: 'A squad waiting at a pickup point to be flown to an LZ.',
      relative_to_player: false,
      instructor_only: false,
      params: [
        { key: 'lz', label: 'LZ', default: 'LZ Fox', kind: { type: 'choice', options: [
          { value: 'LZ Fox', label: 'LZ Fox' }, { value: 'LZ Dagger', label: 'LZ Dagger' }, { value: 'LZ Tango', label: 'LZ Tango' },
        ] } },
        { key: 'troops', label: 'Troops', default: '8', kind: { type: 'number', min: 2, max: 33, step: 1, unit: 'pax' } },
      ],
    },
    {
      id: 'ground_opfor',
      category: 'ground',
      label: 'Gunnery lane OPFOR',
      description: 'Pop-up armour in the gunnery lane for Combined Arms crews.',
      relative_to_player: false,
      instructor_only: false,
      params: [count(12, '8'), skill],
    },
    {
      id: 'jtac_cas',
      category: 'jtac',
      label: 'AI JTAC CAS drill',
      description: 'An AI JTAC with friendly troops in contact passes you a 9-line. Scored on time, accuracy and danger-close.',
      relative_to_player: false,
      instructor_only: false,
      params: [
        { key: 'marking', label: 'Mark', default: 'laser', kind: { type: 'choice', options: [
          { value: 'laser', label: 'Laser 1688' }, { value: 'smoke', label: 'Smoke' }, { value: 'ir', label: 'IR pointer (night)' },
        ] } },
        { key: 'danger_close', label: 'Friendlies', default: 'normal', kind: { type: 'choice', options: [
          { value: 'normal', label: '> 1 km away' }, { value: 'close', label: 'Danger close (< 600 m)' },
        ] } },
      ],
    },
    {
      id: 'reset_range',
      category: 'air_to_ground',
      label: 'Reset a range station',
      description: 'Respawn every target on one station. Instructor only while others are working it.',
      relative_to_player: false,
      instructor_only: true,
      params: [
        { key: 'station', label: 'Station', default: 'range_b_array', kind: { type: 'choice', options: [
          { value: 'range_b_array', label: 'Range B — Tactical Array' }, { value: 'range_b_convoy', label: 'Range B — Moving Convoy' },
          { value: 'range_c_laser', label: 'Range C — Laser Target' },
        ] } },
      ],
    },
  ],
}

export const WEAPON_DB: WeaponDb = {
  dcs_version: '2.9.20.14212',
  bombs: [
    { name: 'Mk_82', display_name: 'Mk-82 500 lb GP', mass_kg: 241, caliber_m: 0.273, length_m: 2.21, cx_coeff: [1, 0.29, 0.71, 0.13, 1.28], char_time_s: 20.32, class: 'unguided' },
    { name: 'Mk_83', display_name: 'Mk-83 1000 lb GP', mass_kg: 447, caliber_m: 0.357, length_m: 3.0, cx_coeff: [1, 0.29, 0.71, 0.13, 1.28], char_time_s: 20.27, class: 'unguided' },
    { name: 'Mk_84', display_name: 'Mk-84 2000 lb GP', mass_kg: 894, caliber_m: 0.458, length_m: 3.84, cx_coeff: [1, 0.29, 0.71, 0.13, 1.28], char_time_s: 20.2, class: 'unguided' },
    { name: 'BDU_33', display_name: 'BDU-33 practice', mass_kg: 11.3, caliber_m: 0.102, length_m: 0.58, cx_coeff: [1, 0.35, 0.8, 0.15, 1.3], char_time_s: 20.8, class: 'unguided' },
    { name: 'BDU_50LD', display_name: 'BDU-50LD inert', mass_kg: 241, caliber_m: 0.273, length_m: 2.21, cx_coeff: [1, 0.29, 0.71, 0.13, 1.28], char_time_s: 20.32, class: 'unguided' },
    { name: 'CBU_99', display_name: 'CBU-99 Rockeye', mass_kg: 222, caliber_m: 0.335, length_m: 2.34, cx_coeff: [1, 0.35, 0.8, 0.14, 1.3], char_time_s: 20.6, class: 'unguided' },
    { name: 'GBU_12', display_name: 'GBU-12 Paveway II', mass_kg: 277, caliber_m: 0.273, length_m: 3.33, cx_coeff: [1, 0.39, 0.6, 0.16, 1.31], char_time_s: 20.4, class: 'guided' },
    { name: 'GBU_16', display_name: 'GBU-16 Paveway II', mass_kg: 454, caliber_m: 0.356, length_m: 3.7, cx_coeff: [1, 0.39, 0.6, 0.16, 1.31], char_time_s: 20.35, class: 'guided' },
    { name: 'GBU_38', display_name: 'GBU-38 JDAM', mass_kg: 253, caliber_m: 0.273, length_m: 2.38, cx_coeff: [1, 0.39, 0.6, 0.16, 1.31], char_time_s: 20.3, class: 'guided' },
  ],
}

export const CALIBRATION: WeaponCalibration[] = [
  { weapon: 'Mk_82', samples: 41, drag_scale: 1.18, residual_m: 9.4 },
  { weapon: 'BDU_33', samples: 67, drag_scale: 0.92, residual_m: 6.1 },
  { weapon: 'BDU_50LD', samples: 23, drag_scale: 1.15, residual_m: 11.2 },
  { weapon: 'Mk_84', samples: 6, drag_scale: 1.07, residual_m: 17.8 },
]
