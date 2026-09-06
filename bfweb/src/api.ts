// ── API/WS base URLs ────────────────────────────────────────────────
// Default: relative '/api' and same-origin WS, for when bfweb is embedded
// directly in bfdb (rust-embed) and served from the same origin. Set
// VITE_API_BASE (e.g. "https://api.example.com") at build time to point a
// standalone-hosted dashboard at a remotely-hosted bfdb instead — see
// deploy/README.md. VITE_API_BASE must NOT have a trailing slash.
export const API_ROOT: string = import.meta.env.VITE_API_BASE ?? ''

function wsUrl(path: string): string {
  if (API_ROOT) {
    return API_ROOT.replace(/^http/, 'ws') + path
  }
  const proto = window.location.protocol === 'https:' ? 'wss' : 'ws'
  return `${proto}://${window.location.host}${path}`
}

// ── JSON Schema (draft-07, as produced by the `schemars` crate) ───────
export interface JsonSchema {
  $schema?: string
  title?: string
  description?: string
  type?: string | string[]
  format?: string
  enum?: (string | number)[]
  const?: string | number | boolean
  default?: unknown
  properties?: Record<string, JsonSchema>
  required?: string[]
  additionalProperties?: JsonSchema | boolean
  items?: JsonSchema
  anyOf?: JsonSchema[]
  oneOf?: JsonSchema[]
  allOf?: JsonSchema[]
  $ref?: string
  minimum?: number
  maximum?: number
  definitions?: Record<string, JsonSchema>
}

export interface Round {
  id: number
  scenario: string
  start: string
  end: string | null
  active: boolean
  winner: string | null
}

export interface Objective {
  id: string
  name: string
  kind: string
  owner: 'Red' | 'Blue' | 'Neutral'
  lat: number
  lon: number
  health: number
  logi: number
  supply: number
  fuel: number
  last_change: string
  priority?: boolean
  threatened?: boolean
  captureable?: boolean
}

/** The frontline as three independent sets of [lat, lon] polylines: the
 *  white centre (no man's land), the blue-dominance edge and the
 *  red-dominance edge. Same geometry the engine draws on the F10 map. */
export interface Frontlines {
  mid: [number, number][][]
  blue: [number, number][][]
  red: [number, number][][]
}

export interface Pilot {
  ucid: string
  name: string
  air_kills: number
  ground_kills: number
  captures: number
  repairs: number
  supply_transfers: number
  troops: number
  farps: number
  deploys: number
  actions: number
  deaths: number
  hours: number
  donated_points: number
}

export interface Kill {
  time: string
  victim: { ucid: string | null; side: string }
  killer: { ucid: string | null; side: string; weapon: string | null; airframe: string | null } | null
  target_type: string | null
}

// ── Briefing (navaids / radios / artillery / deployables / threats) ──
export interface NavaidEntry {
  objective: string
  kind: string
  deck: string | null
  lat: number
  lon: number
  tacan: string | null
  ndb_khz: number | null
  icls: number | null
  link4_mhz: number | null
  acls: boolean
  brc: number | null
}
export interface RadioEntry {
  label: string
  kind: string
  freq_mhz: number | null
  tacan: string | null
  extra: string | null
}
export interface ArtilleryEntry {
  group: string
  typ: string
  lat: number
  lon: number
  min_range_m: number
  max_range_m: number
  alive: number
}
export interface DeployableEntry {
  name: string
  cost: number
  crates_required: number
  limit: number
  deployed: number
  tags: string[]
}
export interface ThreatEntry {
  typ: string
  count: number
  band: string | null
  harm_code: string | null
  max_range_km: number | null
}
export interface Briefing {
  side: 'Blue' | 'Red' | 'Neutral'
  generated: string
  navaids: NavaidEntry[]
  radios: RadioEntry[]
  artillery: ArtilleryEntry[]
  deployables: DeployableEntry[]
  threats: ThreatEntry[]
}

export interface Weather {
  temp_c: number
  wind_speed_kts: number
  wind_from_deg: number
  cloud_base_m: number
  qnh_hpa: number
  cloud_density: number | null
  visibility_m: number | null
}

export interface Stats {
  total_pilots: number
  total_rounds: number
  active_round: { id: number; scenario: string; start: string } | null
  objective_count: number
  total_kills: number
  restart_at: string | null
  weather: Weather | null
  blue_registered: number
  red_registered: number
  blue_online: number
  red_online: number
}

export interface MapUnit {
  id: string
  owner: 'Red' | 'Blue' | 'Neutral'
  typ: string
  tags: string[]
  lat: number
  lon: number
  alt: number
  heading: number
  speed: number
  detected_by: string[]
}

// ── Live unit types (from Export.lua → bfdb UDP → WebSocket) ──────────
export interface LiveUnit {
  id:   string
  nm:   string   // unit name / callsign
  typ:  string   // DCS type string
  cat:  number   // 1=Plane 2=Helo 3=Ground 4=Ship
  coa:  number   // 1=Red 2=Blue
  lat:  number
  lon:  number
  alt:  number   // metres
  hdg:  number   // degrees true
  spd:  number   // knots
  vspd?: number  // vertical speed m/s (positive = climbing)
  pilot?: string // occupying player's name, absent for AI-flown units
}

export interface Bullseye {
  side: number   // 1=Red 2=Blue
  lat:  number
  lon:  number
}

export interface WsUnitsMsg {
  t:     number        // DCS model time (seconds)
  units: LiveUnit[]
  bull:  Bullseye[]
}

export interface LogLine {
  ts:     string   // ISO timestamp with millis
  level:  string   // ERROR | WARN | INFO | DEBUG | TRACE
  target: string   // Rust module path
  msg:    string
}

/** Connect to the admin log WebSocket. Returns a cleanup function. Requires admin session. */
export function connectLiveLogs(
  onLine: (line: LogLine) => void,
  onStatus: (s: 'open' | 'closed' | 'error') => void,
): () => void {
  const ws = new WebSocket(wsUrl('/ws/logs'))
  ws.onopen  = () => onStatus('open')
  ws.onclose = () => onStatus('closed')
  ws.onerror = () => onStatus('error')
  ws.onmessage = (e) => {
    try { onLine(JSON.parse(e.data as string) as LogLine) } catch { /* ignore */ }
  }
  return () => ws.close()
}

/** Connect to the live bflib engine log WebSocket (raw text lines from the
 *  running DCS mission, distinct from bfdb's own process log). Requires
 *  admin session and bfdb started with --base. */
export function connectEngineLogs(
  onLine: (line: string) => void,
  onStatus: (s: 'open' | 'closed' | 'error') => void,
): () => void {
  const ws = new WebSocket(wsUrl('/ws/engine-logs'))
  ws.onopen  = () => onStatus('open')
  ws.onclose = () => onStatus('closed')
  ws.onerror = () => onStatus('error')
  ws.onmessage = (e) => onLine(e.data as string)
  return () => ws.close()
}

/** Connect to the live unit WebSocket.
 *  Returns a cleanup function to close the socket.
 *  @param onMsg  called each time a full unit snapshot arrives
 *  @param onStatus  called with 'open' | 'closed' | 'error'
 */
export function connectLiveUnits(
  onMsg: (msg: WsUnitsMsg) => void,
  onStatus: (s: 'open' | 'closed' | 'error') => void,
): () => void {
  const ws = new WebSocket(wsUrl('/ws/units'))
  ws.onopen  = () => onStatus('open')
  ws.onclose = () => onStatus('closed')
  ws.onerror = () => onStatus('error')
  ws.onmessage = (e) => {
    try { onMsg(JSON.parse(e.data as string) as WsUnitsMsg) } catch { /* ignore */ }
  }
  return () => ws.close()
}

export interface CarpSolution {
  pi_lat: number
  pi_lon: number
  pi_mgrs: string
  pi_elevation_ft: number
  dz_elevation_ft: number
  obstr_elevation_ft: number
  drop_altitude_ft: number
  alt_wind_dir_deg: number
  alt_wind_speed_kt: number
  sfc_wind_dir_deg: number
  sfc_wind_speed_kt: number
  bal_wind_dir_deg: number
  bal_wind_speed_kt: number
  alt_temp_c: number
  sfc_temp_c: number
}

export interface AuthUser {
  discord_id: string
  username:   string
  avatar:     string | null
  is_admin:   boolean
  ucid:       string | null
  /** Coalition in the active round; null if the pilot has no side (or isn't
   *  linked). Gates access to the recon intel page. */
  side:       'Blue' | 'Red' | null
}

// ── Recon intel (TARPS) ─────────────────────────────────────────────────
export interface IntelAdjust {
  /** 4 ground corners (TL, TR, BR, BL) as [lat, lon] the photo is pinned to,
   *  overriding the automatic projection. Set by the warp editor. */
  corners: [[number, number], [number, number], [number, number], [number, number]] | null
  /** Overlay opacity 0..1 for peeling stacked photos. */
  opacity: number | null
}

export interface IntelCapture {
  id:               string
  side:             'Blue' | 'Red'
  image_url:        string   // server path, prefix with API_ROOT
  uploaded_by_name: string
  uploaded_at:      string
  captured_at:      string | null
  filename:         string
  placed:           boolean
  lat:              number
  lon:              number
  alt_ft:           number | null
  heading_deg:      number | null
  pitch_deg:        number | null
  roll_deg:         number | null
  adjust:           IntelAdjust | null
  note:             string | null
  mine:             boolean   // may the current viewer edit/delete it
}

export interface AdminSession {
  discord_id: string
  username:   string
  avatar:     string | null
  is_admin:   boolean
  expires:    string
}

export interface TrailPoint {
  id:  string   // unit id
  lat: number
  lon: number
  alt: number
  hdg: number
  ts:  number   // unix timestamp seconds
}

export interface OnlinePilot {
  ucid: string
  name: string
  side: 'Blue' | 'Red' | 'Neutral'
  aircraft: string | null
}

export interface PilotPoints {
  name: string
  points: number
  side: string
}

export interface CaptureCount {
  name: string
  count: number
}

export interface AircraftUsage {
  vehicle: string
  sorties: number
  hours: number
}

export interface PilotSortie {
  round_id: number
  aircraft: string
  takeoff: string
  land: string | null
  duration_secs: number
  landed: boolean
}

export interface TheaterBreakdown {
  round_id: number
  scenario: string
  air_kills: number
  ground_kills: number
  captures: number
  repairs: number
  supply_transfers: number
  troops: number
  farps: number
  deploys: number
  actions: number
  deaths: number
  hours: number
}

export interface PilotKill {
  round_id: number
  time: string
  victim_ucid: string | null
  victim_side: string
  target_type: string | null
  weapon: string | null
  killer_airframe: string | null
}

export interface PilotDeploy {
  round_id: number
  time: string
  deployable: string
  aircraft: string | null
  method: string | null  // "AirDrop" | "ManualUnpack" | null (unknown / non-physical-cargo deploy)
}

export interface PilotName {
  ucid: string
  name: string
}

export interface SrsRadio {
  freq:       number
  modulation: number  // 0=AM, 1=FM, 2=intercom
  name:       string
  enabled:    boolean
  secFreq:    number
}

export interface SrsClient {
  ClientGuid: string
  Name:       string
  Coalition:  number  // 0=spectator, 1=red, 2=blue
  RadioInfo:  {
    radios:        SrsRadio[]
    inAircraft:    boolean
    intercomHotMic: boolean
  } | null
}

export interface SrsStatus {
  version: string | null
  clients: SrsClient[]
}

export interface PerfRow {
  name:  string
  unit:  string
  n:     number
  mean:  number
  p50:   number
  p90:   number
  p99:   number
  p999:  number
}

export interface PerfData {
  available:       boolean
  time?:           string
  engine?:         PerfRow[]
  api?:            PerfRow[]
  logistics_items?: number
}

export interface BanRecord {
  ucid:      string
  name:      string
  banned_at: string | null
  until:     string | null
  reason:    string
  source:    'web' | 'engine'
}

export interface BotStatus {
  configured: boolean
  name:       string | null
  status:     string | null  // e.g. "running" | "stopped" | "paused", per DCSServerBot
}

export interface BotActionResult {
  message: string
  status:  string
}

export interface PerfTimelinePoint {
  time:            string
  frame:           { mean: number; p99: number }
  timed_events:    { mean: number; p99: number }
  slow_timed:      { mean: number; p99: number }
  dcs_events:      { mean: number; p99: number }
  spawn:           { mean: number; p99: number }
  despawn:         { mean: number; p99: number }
  logistics:       { mean: number; p99: number }
  logistics_deliver: { mean: number; p99: number }
  frontline:       { mean: number; p99: number }
  unit_positions:  { mean: number; p99: number }
  ewr_tracks:      { mean: number; p99: number }
  snapshot:        { mean: number; p99: number }
}

export interface PerfSession {
  time:    string
  metrics: PerfRow[]
}

export interface PerfHistory {
  timeline: PerfTimelinePoint[]
  sessions: PerfSession[]
}

const BASE = `${API_ROOT}/api`

async function errorMessage(res: Response): Promise<string> {
  try {
    const j = await res.json()
    if (j && typeof j.error === 'string') return j.error
  } catch { /* body wasn't JSON */ }
  return `HTTP ${res.status}`
}

async function get<T>(path: string): Promise<T> {
  const res = await fetch(`${BASE}${path}`, { credentials: 'include' })
  if (!res.ok) throw new Error(await errorMessage(res))
  return res.json()
}

async function post<T>(path: string, body: unknown): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(body),
  })
  if (!res.ok) throw new Error(await errorMessage(res))
  return res.json()
}

export const api = {
  rounds: () => get<Round[]>('/rounds'),
  leaderboard: () => get<Pilot[]>('/leaderboard'),
  allPilots: () => get<PilotName[]>('/pilots'),
  objectives: (roundId?: number) =>
    get<Objective[]>(roundId ? `/objectives?round=${roundId}` : '/objectives'),
  frontline: (roundId?: number) =>
    get<Frontlines>(roundId ? `/frontline?round=${roundId}` : '/frontline'),
  // Coalition-locked server-side: omit `side` to get your own coalition's
  // briefing; only admins may request a specific side.
  briefing: (side?: 'Blue' | 'Red') =>
    get<Briefing>(side ? `/briefing?side=${side}` : '/briefing'),
  kills: (roundId?: number, limit = 50) =>
    get<Kill[]>(`/kills?limit=${limit}${roundId ? `&round=${roundId}` : ''}`),
  pilot: (ucid: string) => get<Pilot>(`/pilot/${ucid}`),
  pilotSorties: (ucid: string) => get<PilotSortie[]>(`/pilot/${ucid}/sorties`),
  pilotBreakdown: (ucid: string) => get<TheaterBreakdown[]>(`/pilot/${ucid}/breakdown`),
  pilotKills: (ucid: string) => get<PilotKill[]>(`/pilot/${ucid}/kills`),
  pilotDeploys: (ucid: string) => get<PilotDeploy[]>(`/pilot/${ucid}/deploys`),
  stats: () => get<Stats>('/stats'),
  online: () => get<OnlinePilot[]>('/online'),
  points: () => get<PilotPoints[]>('/points'),
  captures: () => get<CaptureCount[]>('/captures'),
  aircraftUsage: () => get<AircraftUsage[]>('/aircraft-usage'),
  srs:   () => get<SrsStatus>('/srs'),
  units: () => get<MapUnit[]>('/units'),
  trails: () => get<TrailPoint[]>('/trails'),
  auth: {
    me:           () => get<{ user: AuthUser | null }>('/auth/me').then(r => r.user),
    logout:       () => fetch(`${BASE}/auth/logout`, { credentials: 'include' }),
    loginUrl:     () => `${BASE}/auth/login?return_to=${encodeURIComponent(window.location.origin + '/')}`,
    localEnabled: () => get<{ enabled: boolean }>('/auth/local-enabled').then(r => r.enabled),
    localLogin: async (username: string, password: string): Promise<void> => {
      const res = await fetch(`${BASE}/auth/local-login`, {
        method: 'POST',
        credentials: 'include',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ username, password }),
      })
      if (!res.ok) {
        let msg = `Error ${res.status}`
        try { const j = await res.json(); if (j?.error) msg = j.error } catch { /* ignore */ }
        throw new Error(msg)
      }
    },
  },
  admin: {
    sessions:    () => get<AdminSession[]>('/admin/sessions'),
    reset:       () => post<{ ok: boolean }>('/admin/reset', {}),
    perf:        () => get<PerfData>('/admin/perf'),
    perfHistory: () => get<PerfHistory>('/admin/perf-history'),
    banned:      () => get<BanRecord[]>('/admin/banned'),
    engineErrors: () => get<string[]>('/admin/engine-errors'),
    ban:         (ucid: string, name: string, reason = '', until?: string) =>
                   post<{ ok: boolean }>('/admin/ban', { ucid, name, reason, until: until ?? null }),
    unban:       (ucid: string) => post<{ ok: boolean }>('/admin/unban', { ucid }),
    botStatus:          () => get<BotStatus>('/admin/bot/status'),
    botStart:           () => post<BotActionResult>('/admin/bot/start', {}),
    botStop:            () => post<BotActionResult>('/admin/bot/stop', {}),
    botRestart:         () => post<BotActionResult>('/admin/bot/restart', {}),
    botMissionRestart:  () => post<BotActionResult>('/admin/bot/mission/restart', {}),
    botMissionPause:    () => post<BotActionResult>('/admin/bot/mission/pause', {}),
    botMissionUnpause:  () => post<BotActionResult>('/admin/bot/mission/unpause', {}),
    cfg:         () => get<Record<string, unknown>>('/admin/cfg'),
    cfgSchema:   () => get<JsonSchema>('/admin/cfg/schema'),
    cfgSave:     (cfg: Record<string, unknown>) => post<{ ok: boolean }>('/admin/cfg', { cfg }),
  },
  commander: {
    spawnLogistics: (airbase: string, itemType: string) =>
      post<{ ok: boolean }>('/commander/spawn', { airbase, type: itemType }),
  },
  cockpit: {
    // playerId comes from bflib/lua/cockpit.lua's net.get_my_player_id(),
    // passed as ?playerid= on the page URL when loaded inside DCS. When
    // absent (e.g. testing standalone in a browser), these fall back to
    // the Discord-linked session cookie server-side.
    ewrReport: (friendly: boolean, playerId?: string) =>
      get<{ report: string }>(`/cockpit/ewr/report?friendly=${friendly}${playerId ? `&playerid=${playerId}` : ''}`),
    ewrToggle: (playerId?: string) =>
      post<{ state: string }>(`/cockpit/ewr/toggle${playerId ? `?playerid=${playerId}` : ''}`, {}),
    ewrUnits:  (imperial: boolean, playerId?: string) =>
      post<{ units: string }>(`/cockpit/ewr/units${playerId ? `?playerid=${playerId}` : ''}`, { imperial }),
    ewrIntel:  (playerId?: string) =>
      get<{ report: string }>(`/cockpit/ewr/intel${playerId ? `?playerid=${playerId}` : ''}`),
    carpSolve: (markKey: string, dropAltAglFt: number, playerId?: string) => {
      const params = new URLSearchParams({ key: markKey, altft: String(dropAltAglFt) })
      if (playerId) params.set('playerid', playerId)
      return get<CarpSolution>(`/cockpit/carp/solve?${params.toString()}`)
    },
    carpSolveLatLon: (lat: number, lon: number, dropAltAglFt: number, playerId?: string) => {
      const params = new URLSearchParams({ lat: String(lat), lon: String(lon), altft: String(dropAltAglFt) })
      if (playerId) params.set('playerid', playerId)
      return get<CarpSolution>(`/cockpit/carp/solve-latlon?${params.toString()}`)
    },
    // Queues qty copies of a crate for the player's current slot -- same
    // logic the F10 "Spawn N Crates" menu items call, just with a free
    // quantity field instead of a fixed menu list of preset amounts.
    cargoSpawn: (crateName: string, qty: number, c130: boolean, playerId?: string) => {
      const q = playerId ? `?playerid=${playerId}` : ''
      return post<{ message: string }>(`/cockpit/cargo/spawn${q}`, { crate_name: crateName, qty, c130 })
    },
  },
  intel: {
    /** Recon captures visible to the caller's coalition in the active round.
     *  `side` is admin-only ('all' | 'blue' | 'red'). */
    captures: (side?: 'all' | 'blue' | 'red') =>
      get<IntelCapture[]>(`/intel/captures${side ? `?side=${side}` : ''}`),
    /** Absolute URL for a capture's photo (same-coalition gated server-side). */
    imageUrl: (id: string) => `${BASE}/intel/images/${id}`,
    upload: async (file: File, side?: 'blue' | 'red'): Promise<IntelCapture> => {
      const res = await fetch(`${BASE}/intel/upload${side ? `?side=${side}` : ''}`, {
        method: 'POST',
        credentials: 'include',
        headers: {
          'content-type': file.type || 'image/png',
          'x-intel-filename': encodeURIComponent(file.name),
        },
        body: file,
      })
      if (!res.ok) throw new Error(await errorMessage(res))
      return res.json()
    },
    adjust: (body: {
      id: string
      lat?: number
      lon?: number
      placed?: boolean
      note?: string | null
      adjust?: IntelAdjust | null
    }) => post<IntelCapture>('/intel/adjust', body),
    del:   (id: string) => post<{ ok: boolean }>('/intel/delete', { id }),
    purge: () => post<{ ok: boolean }>('/intel/purge', {}),
  },
}
