/**
 * The mock range: Caucasus, a carrier group off Batumi, bombing and strafe
 * ranges inland of Kobuleti, tanker tracks over the sea, helicopter courses
 * near Kobuleti. Coordinates are plausible, not surveyed.
 */
import type { GeoPt, RefuelMethod, ResultKind, StationKind } from '../types'

export const THEATRE = 'Caucasus'
export const MISSION_DATE = '2024/8/1'

export const pt = (lat: number, lon: number, alt_m = 0): GeoPt => ({ lat, lon, alt_m })

export interface MockPilot {
  ucid: string
  name: string
  airframes: string[]
  /** relative weights of the disciplines they fly */
  kinds: [ResultKind, number][]
  /** 0 (new) .. 1 (ace); shifts every distribution */
  skill: number
  callsign: string
}

export const PILOTS: MockPilot[] = [
  { ucid: 'c45e7a1f0b2d4c6e8a9b1c3d5e7f9a0b', name: 'Casper', callsign: 'Casper 1-1', airframes: ['T-45', 'FA-18C_hornet'], skill: 0.55,
    kinds: [['trap', 10], ['aar', 3], ['bomb', 2], ['field_landing', 3], ['missile', 1]] },
  { ucid: '5b70c3e1d9f24a8b6c1e3f5a7b9d0c2e', name: 'Sprocket', callsign: 'Sprocket 2-1', airframes: ['FA-18C_hornet'], skill: 0.7,
    kinds: [['bomb', 6], ['trap', 5], ['aar', 3], ['anti_ship', 2], ['strafe', 1], ['low_level', 2], ['sead', 2], ['field_landing', 1]] },
  { ucid: 'a1b2c3d4e5f60718293a4b5c6d7e8f90', name: 'Hollywood', callsign: 'Showtime 1-1', airframes: ['F-14B'], skill: 0.8,
    kinds: [['trap', 6], ['engagement', 4], ['missile', 4], ['aar', 2], ['bomb', 1], ['hot_zone', 3]] },
  { ucid: '0f1e2d3c4b5a69788796a5b4c3d2e1f0', name: 'Viper', callsign: 'Viper 1-1', airframes: ['F-16C_50'], skill: 0.75,
    kinds: [['bomb', 5], ['strafe', 4], ['aar', 4], ['missile', 3], ['engagement', 1], ['sead', 4], ['hot_zone', 2], ['field_landing', 2]] },
  { ucid: 'd00c5d00c5d00c5d00c5d00c5d00c5d0', name: 'Dutch', callsign: 'Hawg 1-1', airframes: ['A-10C_2'], skill: 0.62,
    kinds: [['strafe', 6], ['bomb', 5], ['cas', 4], ['aar', 2], ['hot_zone', 2], ['low_level', 2]] },
  { ucid: '9a8b7c6d5e4f3a2b1c0d9e8f7a6b5c4d', name: 'Nitro', callsign: 'Enfield 1-1', airframes: ['FA-18C_hornet'], skill: 0.66,
    kinds: [['engagement', 5], ['missile', 5], ['trap', 3], ['anti_ship', 2], ['hot_zone', 2]] },
  { ucid: '7e57a11e7e57a11e7e57a11e7e57a11e', name: 'Tuna', callsign: 'Chalk 1', airframes: ['UH-1H', 'CH-47Fbl1'], skill: 0.58,
    kinds: [['sling', 5], ['landing', 5], ['troops', 3], ['csar', 4]] },
  { ucid: '3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f', name: 'Kestrel', callsign: 'Gunfighter 1', airframes: ['AH-64D_BLK_II', 'M-1 Abrams'], skill: 0.68,
    kinds: [['gunnery', 5], ['cas', 3], ['landing', 2]] },
  { ucid: 'b0b0b0b0c1c1c1c1d2d2d2d2e3e3e3e3', name: 'Rook', callsign: 'Nitro 3-1', airframes: ['AV8BNA'], skill: 0.45,
    kinds: [['bomb', 4], ['landing', 3], ['strafe', 2], ['aar', 1], ['field_landing', 3]] },
  { ucid: 'bl4ckj4ckbl4ckj4ckbl4ckj4ck00001', name: 'Blackjack', callsign: 'Victory 2-1', airframes: ['F-14B'], skill: 0.6,
    kinds: [['trap', 5], ['bomb', 3], ['anti_ship', 2]] },
  { ucid: 'm0ng0m0ng0m0ng0m0ng0m0ng0m0ng000', name: 'Mongo', callsign: 'Dude 1-1', airframes: ['F-15ESE'], skill: 0.5,
    kinds: [['bomb', 5], ['aar', 3], ['missile', 2], ['low_level', 3], ['sead', 1], ['field_landing', 2]] },
]

export const pilotByName = (n: string) => PILOTS.find(p => p.name === n)!

export interface MockStation {
  id: string
  name: string
  kind: StationKind
  pos: GeoPt
  targets: number
  rings_m: number[]
  laser_code?: number
  note?: string
  /** display target name used on cards */
  target: string
}

export const STATIONS: MockStation[] = [
  { id: 'range_a_circle', name: 'Range A — Bomb Circle', kind: 'bomb_circle', pos: pt(41.985, 42.04, 22),
    targets: 1, rings_m: [10, 25, 50, 100], target: 'Bomb Circle Centre' },
  { id: 'range_a_strafe_1', name: 'Range A — Strafe Pit 1', kind: 'strafe_pit', pos: pt(41.9735, 42.018, 18),
    targets: 1, rings_m: [], target: 'Strafe Pit 1', note: 'Foul line 610 m (2000 ft)' },
  { id: 'range_a_strafe_2', name: 'Range A — Strafe Pit 2', kind: 'strafe_pit', pos: pt(41.9735, 42.024, 18),
    targets: 1, rings_m: [], target: 'Strafe Pit 2', note: 'Foul line 610 m (2000 ft)' },
  { id: 'range_b_array', name: 'Range B — Tactical Array', kind: 'tactical_array', pos: pt(42.031, 42.112, 140),
    targets: 10, rings_m: [25, 50, 100], target: 'Array Centre (T-72 revetments)' },
  { id: 'range_b_convoy', name: 'Range B — Moving Convoy', kind: 'convoy', pos: pt(42.052, 42.178, 160),
    targets: 6, rings_m: [], target: 'Convoy (BTR-80 x4, Ural x2)', note: 'Road loop, 25 km/h' },
  { id: 'range_c_laser', name: 'Range C — Laser Target', kind: 'laser_target', pos: pt(42.081, 42.262, 310),
    targets: 3, rings_m: [10, 25, 50], laser_code: 1688, target: 'Bunker (lased 1688)' },
  { id: 'range_c_coord', name: 'Range C — Coordinate Target', kind: 'coord_target', pos: pt(42.095, 42.3, 355),
    targets: 1, rings_m: [10, 25, 50], target: 'Comms Hut (coords on F10)' },
  { id: 'ship_box', name: 'Anti-ship Box — Surface Group', kind: 'ship_target', pos: pt(41.75, 40.9, 0),
    targets: 3, rings_m: [], target: 'Molniya + 2 cargo', note: 'Target ships do not shoot back' },
  { id: 'gun_lane_1', name: 'Gunnery Lane 1', kind: 'gunnery_lane', pos: pt(41.938, 41.985, 30),
    targets: 8, rings_m: [], target: 'Pop-up armour' },
  { id: 'sam_pit', name: 'SAM Pit — SA-8 (trainer)', kind: 'sam_site', pos: pt(42.152, 42.352, 420),
    targets: 1, rings_m: [], target: 'SA-8 Gecko', note: 'Missile trainer on: SAM shots are destroyed before impact' },
]

export const stationById = (id: string) => STATIONS.find(s => s.id === id)!

export interface MockCarrier {
  id: string
  name: string
  unit_type: string
  pos: GeoPt
  brc_deg: number
  /** as the engine reports it: FB − BRC, negative = angled to port */
  deck_angle_deg: number
  speed_kts: number
  tacan: string
  icls: number | null
  link4_mhz: number | null
  tower_mhz: number
  deck_height_m: number
}

// Both steam into the mock surface wind (250°/8 kt, src/mock/atmo.ts): the
// carrier on the BRC that puts ~27 kt straight down its angled deck, the
// straight-deck LHA straight into it.
export const CARRIERS: MockCarrier[] = [
  { id: 'cvn73', name: 'CVN-73 George Washington', unit_type: 'CVN_73', pos: pt(41.46, 41.02, 0),
    brc_deg: 282, deck_angle_deg: -9, speed_kts: 20, tacan: '73X GW', icls: 13, link4_mhz: 336.0, tower_mhz: 305.0, deck_height_m: 20.1 },
  { id: 'lha1', name: 'LHA-1 Tarawa', unit_type: 'LHA_Tarawa', pos: pt(41.6, 41.2, 0),
    brc_deg: 250, deck_angle_deg: 0, speed_kts: 14, tacan: '1X TAR', icls: null, link4_mhz: null, tower_mhz: 264.0, deck_height_m: 20 },
]

export interface MockTanker {
  id: string
  callsign: string
  unit_type: string
  method: RefuelMethod
  /** orbit centre, leg bearing, leg length (m) */
  centre: GeoPt
  leg_deg: number
  leg_m: number
  alt_ft: number
  speed_kts: number
  tacan: string | null
  freq_mhz: number
  recovery_for?: string
}

export const TANKERS: MockTanker[] = [
  { id: 'tex11', callsign: 'Texaco 1-1', unit_type: 'KC-135', method: 'boom', centre: pt(42.26, 41.34),
    leg_deg: 90, leg_m: 55_000, alt_ft: 22_000, speed_kts: 275, tacan: '51Y TEX', freq_mhz: 251.0 },
  { id: 'arc11', callsign: 'Arco 1-1', unit_type: 'KC135MPRS', method: 'drogue', centre: pt(42.42, 41.82),
    leg_deg: 60, leg_m: 50_000, alt_ft: 24_000, speed_kts: 280, tacan: '52Y ARC', freq_mhz: 252.0 },
  { id: 'shl11', callsign: 'Shell 1-1', unit_type: 'KC130', method: 'drogue', centre: pt(42.12, 42.3),
    leg_deg: 120, leg_m: 35_000, alt_ft: 15_000, speed_kts: 230, tacan: '53Y SHL', freq_mhz: 253.0 },
  { id: 'arc2', callsign: 'Arco 2', unit_type: 'A-6E', method: 'drogue', centre: pt(41.5, 41.06),
    leg_deg: 44, leg_m: 12_000, alt_ft: 6_000, speed_kts: 250, tacan: '54Y AR2', freq_mhz: 254.0, recovery_for: 'cvn73' },
  { id: 'mauler', callsign: 'Mauler 1', unit_type: 'S-3B Tanker', method: 'drogue', centre: pt(41.7, 40.85),
    leg_deg: 20, leg_m: 18_000, alt_ft: 10_000, speed_kts: 240, tacan: '55Y MAU', freq_mhz: 255.5 },
  { id: 'il78', callsign: 'Kaban 7', unit_type: 'IL-78M', method: 'drogue', centre: pt(42.6, 42.5),
    leg_deg: 100, leg_m: 45_000, alt_ft: 20_000, speed_kts: 290, tacan: null, freq_mhz: 124.5 },
]

export interface Pad {
  id: string
  name: string
  drill: string
  pos: GeoPt
}

export const PADS: Pad[] = [
  { id: 'kob_pad3', name: 'Kobuleti Pad 3', drill: 'precision', pos: pt(41.929, 41.873, 18) },
  { id: 'eagle_rock', name: "Pinnacle 'Eagle Rock'", drill: 'pinnacle', pos: pt(41.902, 42.051, 612) },
  { id: 'clearing_7', name: 'Confined Area 7', drill: 'confined', pos: pt(41.955, 41.962, 95) },
  { id: 'lha_spot4', name: 'LHA-1 Spot 4', drill: 'ship', pos: pt(41.6, 41.2, 20) },
]

export const SLING_COURSES = [
  { id: 'sling1', name: 'Sling Course 1', dz: pt(41.921, 41.951, 60) },
  { id: 'sling2', name: 'Sling Course 2 (ridge)', dz: pt(41.944, 42.012, 240) },
]

export const LZS = ['LZ Fox', 'LZ Dagger', 'LZ Tango']

export const ARENAS = [
  { id: 'bfm', name: 'BFM Box', pos: pt(42.46, 41.28, 6000), radius_m: 18_520 },
  { id: 'bvr', name: 'BVR Lane', pos: pt(42.72, 41.62, 9000), radius_m: 74_000 },
]

// ─── the newer disciplines ─────────────────────────────────────────────────
//
// Placed inside their sectors from bfrange/RANGE_CFG.sample.json (the mock
// sector layer), not next to the older mock stations above.

/** Station difficulty for the tiered-target results (bomb cards). */
export const STATION_TIER: Record<string, 'easy' | 'medium' | 'hard'> = {
  range_a_circle: 'easy',
  range_b_array: 'medium',
  range_b_convoy: 'medium',
  range_c_laser: 'hard',
  range_c_coord: 'hard',
}

export interface MockSamSite { id: string; name: string; system: string; pos: GeoPt; range_m: number; units: number }

/** S-1: the Javakheti plateau IADS (sample `iads[0]`, engages within 25 nm of 41.43, 43.40). */
export const IADS = {
  id: 's1-iads',
  name: 'S-1 Akhalkalaki IADS',
  side: 'red',
  sites: [
    { id: 's1-ewr', name: 'Javakheti EWR', system: 'ewr_55g6', pos: pt(41.47, 43.33, 1900), range_m: 0, units: 1 },
    { id: 's1-sa10', name: 'SA-10 battery', system: 'sa10', pos: pt(41.41, 43.47, 1750), range_m: 75_000, units: 6 },
    { id: 's1-sa2', name: 'SA-2 site', system: 'sa2', pos: pt(41.52, 43.46, 1800), range_m: 40_000, units: 7 },
    { id: 's1-sa11', name: 'SA-11 battery', system: 'sa11', pos: pt(41.36, 43.31, 1700), range_m: 35_000, units: 5 },
    { id: 's1-sa3', name: 'SA-3 site', system: 'sa3', pos: pt(41.45, 43.21, 1850), range_m: 18_000, units: 6 },
    { id: 's1-sa6', name: 'SA-6 battery', system: 'sa6', pos: pt(41.33, 43.42, 1650), range_m: 25_000, units: 5 },
    { id: 's1-shorad', name: 'Pantsir point defence', system: 'pantsir', pos: pt(41.42, 43.44, 1740), range_m: 20_000, units: 1 },
  ] as MockSamSite[],
}

/** What a SEAD kill took out, by system. */
export const SAM_SYSTEM: Record<string, { label: string; roles: [string, string][] }> = {
  ewr_55g6: { label: '55G6 EWR', roles: [['EWR', 'EWR 55G6']] },
  sa10: { label: 'SA-10 Grumble', roles: [['search radar', 'S-300PS 64H6E sr'], ['track radar', 'S-300PS 40B6M tr'], ['launcher', 'S-300PS 5P85C ln']] },
  sa2: { label: 'SA-2 Guideline', roles: [['track radar', 'SNR_75V'], ['search radar', 'p-19 s-125 sr'], ['launcher', 'S_75M_Volhov']] },
  sa11: { label: 'SA-11 Buk', roles: [['search radar', 'SA-11 Buk SR 9S18M1'], ['launcher', 'SA-11 Buk LN 9A310M1'], ['command post', 'SA-11 Buk CC 9S470M1']] },
  sa3: { label: 'SA-3 Goa', roles: [['track radar', 'snr s-125 tr'], ['search radar', 'p-19 s-125 sr'], ['launcher', '5p73 s-125 ln']] },
  sa6: { label: 'SA-6 Kub', roles: [['track radar', 'Kub 1S91 str'], ['launcher', 'Kub 2P25 ln']] },
  pantsir: { label: 'Pantsir-S1', roles: [['launcher', '2S6 Tunguska']] },
}

/** HZ-1 Liakhvi (sample `hot_zones[0]`, 20 nm). */
export const HOT_ZONE = { id: 'hz1', name: 'HZ-1 Liakhvi', pos: pt(42.38, 44.1, 900), radius_m: 20 * 1852, ai_side: 'red', awacs: 'Overlord 1 (251.5)', ground: 9 }

export const HOT_ZONE_KILLS = {
  air: ['MiG-29S', 'Su-27', 'J-11A'],
  ground: ['T-72B', 'BMP-2', 'ZSU-23-4 Shilka', 'Tor 9A331', 'Ural-375', 'SA-18 Igla manpad'],
}

/** EW-1 Tsalka (sample `jammers[0]`, 12 nm), inside the EW-1 sector. */
export const JAMMER = { id: 'ew1', name: 'EW-1 Tsalka jammer', side: 'red', pos: pt(41.69, 44.15, 1500), radius_m: 12 * 1852, gps: 'jam', radio: 'off' }

/** LL-1 Kolkheti: the middle of the sample's corridor, gate by gate. */
export const LOW_LEVEL = {
  id: 'll1',
  name: 'LL-1 Kolkheti',
  max_agl_ft: 500,
  min_agl_ft: 100,
  speed_kts: 420,
  gates: [
    ['ALPHA', pt(42.06, 42.42)], ['BRAVO', pt(42.12, 42.2)], ['CHARLIE', pt(42.12, 41.93)], ['DELTA', pt(42.28, 41.83)],
    ['ECHO', pt(42.42, 41.86)], ['FOXTROT', pt(42.5, 42.05)], ['GOLF', pt(42.52, 42.25)], ['HOTEL', pt(42.45, 42.4)],
  ] as [string, GeoPt][],
}

/** Runways the pattern grades (threshold, landing heading). */
export const FIELDS = [
  { airfield: 'Senaki-Kolkhi', runway: '09', threshold: pt(42.2413, 42.0263, 13), heading_deg: 94 },
  { airfield: 'Kutaisi', runway: '07', threshold: pt(42.1728, 42.4602, 45), heading_deg: 74 },
  { airfield: 'Kobuleti', runway: '07', threshold: pt(41.9282, 41.8543, 18), heading_deg: 70 },
  { airfield: 'Batumi', runway: '13', threshold: pt(41.6103, 41.5997, 10), heading_deg: 126 },
]

/** CS-1 Bakhmaro (sample `csar.areas[0]`). */
export const CSAR_AREA = { id: 'cs1', name: 'CS-1 Bakhmaro', pos: pt(41.83, 42.4, 1600), radius_m: 14_250 }

/** FD-1 Kobuleti (sample `ship_decks`). */
export const SHIP_DECKS = [
  { id: 'fd1-perry', name: 'FFG-7 Perry', unit_type: 'PERRY', pos: pt(42.0, 41.453), heading_deg: 90, speed_kts: 10, leg_m: 4 * 1852, tacan: '41X FFG' },
  { id: 'fd1-burke', name: 'DDG Arleigh Burke', unit_type: 'USS_Arleigh_Burke_IIa', pos: pt(41.965, 41.587), heading_deg: 270, speed_kts: 14, leg_m: 4 * 1852, tacan: '42X DDG' },
]
