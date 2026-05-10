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
  killer: { ucid: string | null; side: string; weapon: string | null } | null
  target_type: string | null
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

/** Connect to the live unit WebSocket.
 *  Returns a cleanup function to close the socket.
 *  @param onMsg  called each time a full unit snapshot arrives
 *  @param onStatus  called with 'open' | 'closed' | 'error'
 */
export function connectLiveUnits(
  onMsg: (msg: WsUnitsMsg) => void,
  onStatus: (s: 'open' | 'closed' | 'error') => void,
): () => void {
  const proto = window.location.protocol === 'https:' ? 'wss' : 'ws'
  const ws = new WebSocket(`${proto}://${window.location.host}/ws/units`)
  ws.onopen  = () => onStatus('open')
  ws.onclose = () => onStatus('closed')
  ws.onerror = () => onStatus('error')
  ws.onmessage = (e) => {
    try { onMsg(JSON.parse(e.data as string) as WsUnitsMsg) } catch { /* ignore */ }
  }
  return () => ws.close()
}

export interface AuthUser {
  discord_id: string
  username:   string
  avatar:     string | null
  is_admin:   boolean
  ucid:       string | null
}

export interface AdminLink {
  discord_id: string
  ucid:       string
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
}

export interface PilotName {
  ucid: string
  name: string
}

const BASE = '/api'

async function get<T>(path: string): Promise<T> {
  const res = await fetch(`${BASE}${path}`, { credentials: 'include' })
  if (!res.ok) throw new Error(`HTTP ${res.status}`)
  return res.json()
}

async function post<T>(path: string, body: unknown): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(body),
  })
  if (!res.ok) throw new Error(`HTTP ${res.status}`)
  return res.json()
}

export const api = {
  rounds: () => get<Round[]>('/rounds'),
  leaderboard: () => get<Pilot[]>('/leaderboard'),
  allPilots: () => get<PilotName[]>('/pilots'),
  objectives: (roundId?: number) =>
    get<Objective[]>(roundId ? `/objectives?round=${roundId}` : '/objectives'),
  kills: (roundId?: number, limit = 50) =>
    get<Kill[]>(`/kills?limit=${limit}${roundId ? `&round=${roundId}` : ''}`),
  pilot: (ucid: string) => get<Pilot>(`/pilot/${ucid}`),
  pilotSorties: (ucid: string) => get<PilotSortie[]>(`/pilot/${ucid}/sorties`),
  pilotBreakdown: (ucid: string) => get<TheaterBreakdown[]>(`/pilot/${ucid}/breakdown`),
  pilotKills: (ucid: string) => get<PilotKill[]>(`/pilot/${ucid}/kills`),
  stats: () => get<Stats>('/stats'),
  online: () => get<OnlinePilot[]>('/online'),
  points: () => get<PilotPoints[]>('/points'),
  captures: () => get<CaptureCount[]>('/captures'),
  aircraftUsage: () => get<AircraftUsage[]>('/aircraft-usage'),
  units: () => get<MapUnit[]>('/units'),
  trails: () => get<TrailPoint[]>('/trails'),
  auth: {
    me:           () => get<{ user: AuthUser | null }>('/auth/me').then(r => r.user),
    logout:       () => fetch(`${BASE}/auth/logout`, { credentials: 'include' }),
    link:         (ucid: string) => post<{ ok: boolean }>('/auth/link', { ucid }),
    loginUrl:     () => `${BASE}/auth/login`,
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
    links:    () => get<AdminLink[]>('/admin/links'),
    sessions: () => get<AdminSession[]>('/admin/sessions'),
    unlink:   (discord_id: string) => post<{ ok: boolean }>('/admin/unlink', { discord_id }),
    reset:    () => post<{ ok: boolean }>('/admin/reset', {}),
  },
}
