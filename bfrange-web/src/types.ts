/**
 * Wire types for the training range, mirrored by hand from
 * `bfprotocols/src/range/mod.rs`. Keep the two in step: every field here is
 * named and shaped exactly as serde writes it.
 *
 * Conventions used below:
 *  - `Option<T>` with `#[serde(default)]` only       -> `T | null` (always written)
 *  - `Option<T>` with `skip_serializing_if = is_none` -> `field?: T` (omitted when None)
 *  - `DateTime<Utc>`                                 -> ISO-8601 string
 *  - `#[serde(tag = "kind")]` enums                   -> discriminated unions on `kind`
 */

export type IsoTime = string

// ─── shared ────────────────────────────────────────────────────────────────

export interface PilotRef {
  ucid?: string
  name: string
}

export interface GeoPt {
  lat: number
  lon: number
  /** metres MSL */
  alt_m: number
}

// ─── A/G range ─────────────────────────────────────────────────────────────

export type WeaponClass = 'unguided' | 'guided' | 'rocket' | 'missile' | 'cluster' | 'gun'

export type BombQuality = 'POOR' | 'INEFFECTIVE' | 'GOOD' | 'EXCELLENT' | 'SHACK'

/**
 * DCS's atmosphere at one height above a point, read from the running
 * mission. Every wind-dependent number the range reports or calculates comes
 * from these, never from the mission file or an assumed standard day.
 */
export interface AtmoLayer {
  /** metres MSL */
  alt_m: number
  /** meteorological: the direction it blows FROM, degrees true */
  wind_from_deg: number
  wind_kts: number
  temp_c: number
  pressure_hpa: number
}

export interface Release {
  pos: GeoPt
  alt_agl_m: number
  tas_kts: number
  gs_kts: number
  /** true heading of the velocity vector (track), degrees */
  heading_deg: number
  /** flight-path dive angle, degrees; positive = descending */
  dive_deg: number
  slant_range_m: number
  ground_range_m: number
  /** meteorological (from) */
  wind_from_deg: number
  wind_kts: number
  /** from DCS's temperature at the release point */
  mach: number
  /**
   * DCS's wind, temperature and pressure over the release point from the
   * impact's ground up to the release altitude, lowest first: what the bomb
   * actually fell through. Missing / empty on records from older engines.
   */
  atmo?: AtmoLayer[]
}

export interface BombResult {
  station_id: string
  range: string
  target: string
  weapon: string
  weapon_display: string
  weapon_class: WeaponClass
  /** "laser", "ins", "tv", "ir", "radar", "none" */
  guidance: string
  release: Release
  target_pos: GeoPt
  impact: GeoPt
  /** impact relative to the target, metres */
  impact_north_m: number
  impact_east_m: number
  miss_m: number
  /** bearing from target to impact, degrees true (the card's φ) */
  radial_deg: number
  /** 1..12 relative to the attack heading; 12 = long, 6 = short */
  clock: number
  /** along-track error, + = long */
  long_m: number
  /** cross-track error, + = right of the run-in line */
  cross_m: number
  time_of_flight_s: number
  quality: BombQuality
  target_hit: boolean
  laser_code?: number
  rings_m: number[]
  good_radius_m: number
}

export type StrafeQuality = 'INVALID' | 'POOR' | 'INEFFECTIVE' | 'GOOD' | 'EXCELLENT' | 'DEADEYE'

export interface StrafeResult {
  station_id: string
  range: string
  pit: string
  gun: string
  rounds_fired: number
  hits: number
  accuracy_pct: number
  quality: StrafeQuality
  foul_line_crossed: boolean
  invalid_reason?: string
  run_in_heading_deg: number
  min_range_m: number
  entry_alt_agl_m: number
  target_pos: GeoPt
  foul_line_m: number
}

// ─── carrier ───────────────────────────────────────────────────────────────

export type PassOutcome =
  | 'trap'
  | 'bolter'
  | 'waveoff'
  | 'own_waveoff'
  | 'touch_and_go'
  | 'crash'
  | 'unknown'

export type GradeSource = 'dcs' | 'engine'

export interface PatternSummary {
  break_alt_ft: number | null
  abeam_distance_nm: number | null
  abeam_alt_ft: number | null
  ninety_alt_ft: number | null
  /** the card's "Wake Alt" */
  wake_alt_ft: number | null
  pattern_time_s: number | null
  notes: string[]
}

export interface EngineGrade {
  grade: string
  points: number | null
  comment: string
}

export interface TrapResult {
  carrier: string
  carrier_type: string
  /** 1, 2 or 3 */
  case: number
  night: boolean
  outcome: PassOutcome
  grade: string
  points: number | null
  lso_comment: string
  lso_description: string[]
  wire: number | null
  wire_from_dcs: boolean
  groove_time_s: number | null
  wind_over_deck_kts: number | null
  final_bearing_deg: number | null
  source: GradeSource
  dcs_comment?: string
  engine_grade?: EngineGrade
  pattern: PatternSummary
  hook_down: boolean | null
}

export interface GrooveSample {
  t: number
  /** metres aft of the landing point along the final bearing (+ = in the groove) */
  x_m: number
  /** metres off the landing-area centreline, + = right */
  y_m: number
  /** feet above the deck */
  alt_ft: number
  /** glideslope error, degrees, + = high */
  gse_deg: number
  /** lineup error, degrees, + = right */
  lue_deg: number
  aoa_deg: number | null
  closure_kts: number
  /** vertical speed, feet per minute, + = climbing (engine: v.y * 60 * M_TO_FT) */
  vs_fpm: number
  lat: number
  lon: number
}

// ─── AAR ───────────────────────────────────────────────────────────────────

export type RefuelMethod = 'boom' | 'drogue'

export interface Stability {
  fore_aft_sd_m: number
  lateral_sd_m: number
  vertical_sd_m: number
  mean_fwd_m: number
  mean_right_m: number
  mean_up_m: number
}

export interface AarResult {
  tanker: string
  tanker_type: string
  method: RefuelMethod
  join_time_s: number | null
  contacts: number
  disconnects: number
  time_connected_s: number
  fuel_kg: number
  fuel_lbs: number
  onload_rate_lbs_min: number
  stability: Stability
  precontact_closure_kts: number | null
  overshoot: boolean
  alt_ft: number
  speed_kts: number
  /** "A" .. "F" */
  grade: string
  calls: string[]
  session_s: number
}

export interface RelSample {
  t: number
  fwd_m: number
  right_m: number
  up_m: number
  connected: boolean
  fuel_kg: number
  closure_kts: number
}

// ─── A/A ───────────────────────────────────────────────────────────────────

export type MissileOutcome = 'kill' | 'defeated' | 'hit' | 'timeout'

export interface LaunchGeom {
  range_m: number
  /** 0 = hot, 180 = cold */
  aspect_deg: number
  shooter_alt_m: number
  target_alt_m: number
  shooter_speed_kts: number
  target_speed_kts: number
  closure_kts: number
  shooter_pos: GeoPt
  target_pos: GeoPt
}

export interface DefenseSummary {
  reaction_s: number | null
  beam_s: number
  drag_s: number
  hot_s: number
  alt_change_m: number
  went_low: boolean
}

export interface MissileResult {
  weapon: string
  /** "aam" / "sam" / "other" */
  weapon_category: string
  shooter: PilotRef
  shooter_type: string
  target: PilotRef
  target_type: string
  outcome: MissileOutcome
  launch: LaunchGeom
  min_distance_m: number
  time_of_flight_s: number
  kill_radius_m: number
  defense: DefenseSummary
  /** "shooter" | "target" */
  perspective: string
}

export type EngagementOutcome = 'win' | 'loss' | 'draw' | 'abort'

export interface EngagementResult {
  setup: string
  adversary: string
  adversary_skill: string
  opponent?: PilotRef
  outcome: EngagementOutcome
  duration_s: number
  shots_fired: number
  trainer_kills: number
  gun_hits: number
  notes: string[]
}

// ─── anti-ship ─────────────────────────────────────────────────────────────

export interface AntiShipResult {
  ship: string
  ship_type: string
  weapon: string
  launch_range_m: number
  weapon_max_range_m: number | null
  hit: boolean
  /** 0..1 */
  damage: number
  ship_sunk: boolean
  time_of_flight_s: number
  intercepted: boolean
  launch_pos: GeoPt
  ship_pos: GeoPt
}

// ─── rotary ────────────────────────────────────────────────────────────────

export type PrecisionQuality = 'POOR' | 'FAIR' | 'GOOD' | 'EXCELLENT' | 'PERFECT'

/** A helicopter cargo delivery: slung, or DCS dynamic cargo carried inside. */
export interface SlingResult {
  /** "sling" or "internal" (DCS dynamic cargo loaded with the cargo loader); older records lack it */
  method?: 'sling' | 'internal'
  course: string
  cargo: string
  mass_kg: number
  time_s: number
  distance_m: number
  damage: number
  quality: PrecisionQuality
  dz_pos: GeoPt
  set_down_pos: GeoPt
}

export interface LandingResult {
  /** "precision", "confined", "pinnacle", "ship", "fclp" */
  drill: string
  pad: string
  distance_m: number
  /** positive = descending */
  touchdown_fpm: number
  heading_error_deg: number | null
  hover_s: number
  quality: PrecisionQuality
  pad_pos: GeoPt
  touchdown_pos: GeoPt
}

export interface TroopResult {
  lz: string
  troops: number
  load_time_s: number
  total_time_s: number
  landing_distance_m: number
  quality: PrecisionQuality
}

// ─── ground / CAS ──────────────────────────────────────────────────────────

export interface GunneryResult {
  lane: string
  targets_total: number
  targets_killed: number
  shots: number
  hits: number
  time_s: number
  first_round_hits: number
}

export interface CasResult {
  jtac: string
  target: string
  weapon: string
  time_to_impact_s: number
  miss_m: number
  correct_target: boolean
  danger_close: boolean
  nearest_friendly_m: number | null
  laser_code: number | null
}

// ─── the record ────────────────────────────────────────────────────────────

/** `RangeResult`, internally tagged by `kind` (snake_case). */
export type RangeResult =
  | ({ kind: 'bomb' } & BombResult)
  | ({ kind: 'strafe' } & StrafeResult)
  | ({ kind: 'trap' } & TrapResult)
  | ({ kind: 'aar' } & AarResult)
  | ({ kind: 'missile' } & MissileResult)
  | ({ kind: 'engagement' } & EngagementResult)
  | ({ kind: 'anti_ship' } & AntiShipResult)
  | ({ kind: 'sling' } & SlingResult)
  | ({ kind: 'landing' } & LandingResult)
  | ({ kind: 'troops' } & TroopResult)
  | ({ kind: 'gunnery' } & GunneryResult)
  | ({ kind: 'cas' } & CasResult)

export type ResultKind = RangeResult['kind']

/** Narrow a `RangeResult` to one kind. */
export type ResultOf<K extends ResultKind> = Extract<RangeResult, { kind: K }>

export const RESULT_KINDS: ResultKind[] = [
  'bomb', 'strafe', 'trap', 'aar', 'missile', 'engagement',
  'anti_ship', 'sling', 'landing', 'troops', 'gunnery', 'cas',
]

export interface TrackPt {
  t: number
  lat: number
  lon: number
  alt_m: number
  speed_kts: number
}

/** `Track`, internally tagged by `kind`. */
export type Track =
  | { kind: 'weapon'; points: TrackPt[] }
  | { kind: 'groove'; samples: GrooveSample[] }
  | { kind: 'aar'; samples: RelSample[] }
  | { kind: 'intercept'; missile: TrackPt[]; target: TrackPt[] }
  | { kind: 'path'; paths: Record<string, TrackPt[]> }

export interface RangeRecord {
  /** `<sortie>-<unix ms>-<seq>` */
  id: string
  v: number
  ts: IsoTime
  mission_time: string
  mission_date: string
  theatre: string
  pilot: PilotRef
  /** DCS type name, e.g. "FA-18C_hornet" */
  unit_type: string
  /** "blue" / "red" / "neutral" */
  side: string
  callsign: string
  /** normalised 0..5; null when the event does not count */
  score: number | null
  result: RangeResult
  track?: Track
}

/** A feed / list entry: the record without its track, plus render hints. */
export type Summary = Omit<RangeRecord, 'track'> & {
  headline: string
  has_track: boolean
  /** relative URL on the API host */
  card_png: string
  card_svg: string
  /** bfdb instance id the result came from */
  instance?: string
}

// ─── live ──────────────────────────────────────────────────────────────────
//
// bfdb passes the engine's live picture through as-is, so fields added to the
// engine after the first release are optional here: an older engine simply
// does not send them.

export interface WindInfo {
  /** DCS's atmosphere over the range reference point, lowest first */
  layers?: AtmoLayer[]
  surface_from_deg: number
  surface_kts: number
  /** ~2000 m / 6600 ft */
  alt_from_deg: number
  alt_kts: number
  temperature_c: number
  qnh_hpa: number
}

export interface LivePlayer {
  ucid: string
  name: string
  unit_type: string
  callsign: string
  side: string
  pos: GeoPt
  heading_deg: number
  speed_kts: number
  in_air: boolean
  activity: string | null
}

export type StationKind =
  | 'bomb_circle'
  | 'strafe_pit'
  | 'tactical_array'
  | 'convoy'
  | 'coord_target'
  | 'laser_target'
  | 'ship_target'
  | 'gunnery_lane'
  | 'sam_site'

export interface LiveStation {
  id: string
  name: string
  kind: StationKind
  pos: GeoPt
  hot_by: string[]
  targets_alive: number
  targets_total: number
  laser_code: number | null
  rings_m: number[]
  note: string | null
  /** ground height at the station's centre, metres MSL, from DCS */
  elev_m?: number | null
  /**
   * DCS's wind, temperature and pressure over the station, lowest first
   * (ground up to 10 km), for the release calculator
   */
  atmo?: AtmoLayer[]
}

export type TankerState = 'spawning' | 'on_station' | 'rtb' | 'dead'

export interface LiveTanker {
  id: string
  callsign: string
  unit_type: string
  method: RefuelMethod
  pos: GeoPt
  heading_deg: number
  speed_kts: number
  alt_ft: number
  /** e.g. "51Y TEX" */
  tacan: string | null
  freq_mhz: number
  state: TankerState
  receivers: string[]
  owner: string | null
  recovery_for: string | null
}

export interface LiveCarrier {
  id: string
  name: string
  unit_type: string
  pos: GeoPt
  /** base recovery course: the ship's live heading, degrees true */
  brc_deg: number
  /** final bearing of the landing area, degrees true */
  fb_deg: number
  /** the ship's live speed, knots */
  speed_kts: number
  /** the engine's own wind over deck */
  wind_over_deck_kts: number
  /** relative wind off the landing-area axis, degrees, + = from starboard */
  wind_over_deck_angle_deg: number
  recovery_open: boolean
  next_window: string | null
  tacan: string | null
  icls: number | null
  link4_mhz: number | null
  tower_mhz: number | null
  recovery_tanker: string | null
  pattern: string[]
  case: number
  /** the true wind at the ship (anemometer height), from DCS */
  true_wind_from_deg?: number
  true_wind_kts?: number
  /**
   * landing area relative to the bow, degrees: FB − BRC, negative = angled
   * to port (about −9 for a Nimitz or Forrestal), 0 for a straight deck
   */
  deck_angle_deg?: number
}

export interface LiveSpawn {
  id: string
  item: string
  label: string
  owner_name: string
  owner_ucid: string | null
  pos: GeoPt
  created: IsoTime
  expires: IsoTime | null
  units: number
}

export interface LiveArena {
  id: string
  name: string
  pos: GeoPt
  radius_m: number
  occupants: string[]
  /** "open" / "duel: A vs B" */
  status: string
}

// ─── sectors (`bfprotocols/src/range/cfg.rs` SectorCfg) ────────────────────

/** What a sector of the theatre is for; each kind has its own F10 colour. */
export type SectorKind =
  | 'air_to_ground'
  | 'tactical'
  | 'threat'
  | 'gunnery'
  | 'helo'
  | 'air_to_air'
  | 'bvr'
  | 'duel'
  | 'aar'
  | 'carrier'
  | 'anti_ship'

export interface SectorPoint {
  lat: number
  lon: number
}

export interface SectorCircle {
  lat: number
  lon: number
  radius_m: number
}

/**
 * A tanker race-track: the leg starts at `lat`/`lon` and runs `leg_m` along
 * `heading_deg` (true); the sector is everything within `width_m / 2` of it.
 */
export interface SectorTrack {
  lat: number
  lon: number
  heading_deg: number
  leg_m: number
  width_m: number
}

/** Exactly one of the three is set. */
export interface SectorShape {
  polygon?: SectorPoint[]
  circle?: SectorCircle
  track?: SectorTrack
}

/** A part of the theatre with one job: a bombing range, a fight area, a tanker track... */
export interface Sector {
  /** `[a-z0-9-]`, e.g. "r-1" */
  id: string
  /** as drawn on the F10 map, e.g. "R-1 SAMGORI" */
  name: string
  kind: SectorKind
  /** who it is laid out for: "blue", "red" or "all" */
  side: 'blue' | 'red' | 'all'
  /** one line of what is in it */
  purpose: string
  shape: SectorShape
  /** the engine tells a player what the sector is for when they fly in */
  announce?: boolean
}

export interface RangeLive {
  server_time: IsoTime
  theatre: string
  mission_time: string
  mission_date: string
  night: boolean
  wind: WindInfo
  players: LivePlayer[]
  stations: LiveStation[]
  tankers: LiveTanker[]
  carriers: LiveCarrier[]
  spawns: LiveSpawn[]
  arenas: LiveArena[]
  /** the range's sectors, as the F10 map draws them; missing from older engines */
  sectors?: Sector[]
  uptime_s: number
}

// ─── spawning ──────────────────────────────────────────────────────────────

export type SpawnCategory =
  | 'air_to_air'
  | 'air_to_ground'
  | 'tanker'
  | 'naval'
  | 'ground'
  | 'helo'
  | 'jtac'

export interface ParamOption {
  value: string
  label: string
}

/** `ParamKind`, internally tagged by `type`. */
export type ParamKind =
  | { type: 'choice'; options: ParamOption[] }
  | { type: 'number'; min: number; max: number; step: number; unit: string }

export interface ParamSpec {
  key: string
  label: string
  kind: ParamKind
  default: string
}

export interface CatalogItem {
  id: string
  category: SpawnCategory
  label: string
  description: string
  params: ParamSpec[]
  instructor_only: boolean
  relative_to_player: boolean
}

export interface SpawnCatalog {
  items: CatalogItem[]
  max_active_per_player: number
  despawn_after_s: number
}

export interface SpawnReply {
  ok: boolean
  message: string
  spawn_id: string | null
}

export interface WeaponBallistics {
  name: string
  display_name: string
  mass_kg: number
  caliber_m: number
  length_m: number
  cx_coeff: number[]
  char_time_s: number | null
  /** "unguided" / "guided" */
  class: string
}

export interface WeaponDb {
  dcs_version: string
  bombs: WeaponBallistics[]
}

// ─── bfdb API responses (range endpoints) ──────────────────────────────────

export interface LiveResponse {
  live: RangeLive | null
  reason: string | null
}

export interface FeedResponse {
  items: Summary[]
}

export interface ResultsResponse {
  items: Summary[]
  total: number
}

export interface Me {
  logged_in: boolean
  ucid: string | null
  name: string | null
  admin: boolean
  discord_name?: string
}

export interface KindStats {
  count: number
  avg_score: number | null
  best_score: number | null
  last_ts: IsoTime | null
}

export interface TrendPoint {
  /** ISO week label, e.g. "2026-W37" */
  week: string
  avg_score: number | null
  count: number
}

export type InsightSeverity = 'info' | 'warn' | 'good'

export interface Insight {
  id: string
  kind: ResultKind | string
  severity: InsightSeverity
  title: string
  detail: string
  /** result ids */
  evidence: string[]
}

export interface Qualification {
  id: string
  name: string
  description: string
  earned: boolean
  /** 0..1 */
  progress: number
  detail: string
}

export interface PilotProfile {
  ucid: string
  name: string
  per_kind: Partial<Record<ResultKind, KindStats>>
  trend: Partial<Record<ResultKind, TrendPoint[]>>
  insights: Insight[]
  quals: Qualification[]
  airframes: { unit_type: string; count: number }[]
  recent: Summary[]
}

export interface PilotHit {
  ucid: string
  name: string
  count: number
}

export interface GreeniePass {
  id: string
  ts: IsoTime
  grade: string
  points: number | null
  wire: number | null
  case: number
  night: boolean
  outcome: PassOutcome
  unit_type: string
}

export interface GreenieRow {
  ucid: string
  name: string
  avg_points: number | null
  count: number
  passes: GreeniePass[]
}

export interface GreenieResponse {
  rows: GreenieRow[]
}

/** bfdb/src/range/boards.rs `leaderboards`; averages are null with no samples. */
export interface Leaderboards {
  bombing: { ucid: string; name: string; count: number; cep_m: number | null; avg_score: number | null }[]
  strafe: { ucid: string; name: string; count: number; avg_accuracy: number | null; avg_score?: number | null }[]
  lso: { ucid: string; name: string; count?: number; avg_points: number | null; traps: number }[]
  aar: { ucid: string; name: string; count: number; avg_score: number | null }[]
  duels: { ucid: string; name: string; count?: number; wins: number; losses: number; elo: number }[]
  missile_defense: { ucid: string; name: string; count?: number; defeated: number; killed: number }[]
}

export interface StationImpact {
  id: string
  north_m: number
  east_m: number
  miss_m: number
  /** display name when the engine had one, else the DCS type name */
  weapon: string
  weapon_class?: WeaponClass
  quality: BombQuality
  ucid: string | null
  name: string
  ts: IsoTime
}

export interface StationImpacts {
  /** the target's position (from the newest drop), null with no drops */
  target: GeoPt | null
  rings_m: number[]
  impacts: StationImpact[]
  cep_m: number | null
}

export interface WeaponCalibration {
  weapon: string
  samples: number
  /** how many of `samples` were fitted through their recorded DCS atmosphere */
  layered?: number
  drag_scale: number
  residual_m: number
  /** the inputs bfdb fitted with (bfdb/src/range/ballistics.rs `Calibration`) */
  cd_ref?: number
  mass_kg?: number
  caliber_m?: number
}

export interface WeaponsResponse {
  db: WeaponDb | null
  calibration: WeaponCalibration[]
}

export interface OkMessage {
  ok: boolean
  message: string
}

export interface FeedQuery {
  limit?: number
  before?: string
  kind?: ResultKind
}

export interface ResultsQuery {
  pilot?: string
  kind?: ResultKind
  unit_type?: string
  station?: string
  days?: number
  limit?: number
  offset?: number
}

export interface GreenieQuery {
  days?: number
  carrier?: string
  unit_type?: string
}
