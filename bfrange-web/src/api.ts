/**
 * bfdb range API client.
 *
 *  - Base URL: `VITE_API_BASE` (no trailing slash). Unset, the site calls a
 *    same-origin `/api` (the Vite dev proxy in development); a production
 *    build gets https://api.vectorstrike.org from `.env.production`.
 *  - Every call sends cookies (`credentials: 'include'`): the session is a
 *    Discord OAuth cookie set on the API host, exactly as bfweb/bfwiki use it.
 *  - `?instance=` is appended only when `VITE_RANGE_INSTANCE` is set;
 *    otherwise bfdb picks its range instance itself.
 *  - `VITE_MOCK=1` swaps the whole client for the in-browser fixtures in
 *    src/mock/ (lazy-loaded, so a production build never ships them).
 */
import type {
  Clip,
  FeedQuery,
  FeedResponse,
  GreenieQuery,
  GreenieResponse,
  Leaderboards,
  LiveResponse,
  Me,
  OkMessage,
  PilotHit,
  PilotProfile,
  RangeRecord,
  ResultsQuery,
  ResultsResponse,
  SpawnCatalog,
  SpawnReply,
  StationImpacts,
  Summary,
  WeaponsResponse,
} from './types'
import { deadline, errorMessage, LONG_TIMEOUT_MS } from '@shared/web/api'
import { VS_URLS } from '@shared/web/urls'

// Same-origin by default, like the other apps: `npm run dev` proxies /api to a
// local bfdb (or production with `--mode live`), and the production API comes
// from .env.production rather than being compiled in here.
export const API_ROOT: string = (import.meta.env.VITE_API_BASE ?? '').replace(/\/+$/, '')
export const RANGE_INSTANCE: string | undefined = import.meta.env.VITE_RANGE_INSTANCE || undefined
export const MOCK: boolean = __RANGE_MOCK__
/** The dashboard: its flight replay is the full web Tacview of a session. */
export const DASHBOARD_URL: string = (import.meta.env.VITE_DASHBOARD_URL || VS_URLS.dashboard).replace(/\/+$/, '')

const BASE = `${API_ROOT}/api`

/** Append `instance=` (only when configured) to a path that may have a query. */
export function withInstance(path: string): string {
  if (!RANGE_INSTANCE || /[?&]instance=/.test(path)) return path
  return path + (path.includes('?') ? '&' : '?') + `instance=${encodeURIComponent(RANGE_INSTANCE)}`
}

/** Resolve a server-relative URL (e.g. a result's `card_png`) against the API host. */
export function apiUrl(rel: string): string {
  if (/^(https?:|data:|blob:)/.test(rel)) return rel
  const p = rel.startsWith('/') ? rel : `/${rel}`
  return withInstance(`${API_ROOT}${p}`)
}

export class ApiError extends Error {
  status: number
  constructor(status: number, message: string) {
    super(message)
    this.status = status
  }
}

function qs(params: object): string {
  const u = new URLSearchParams()
  for (const [k, v] of Object.entries(params)) {
    if (v === undefined || v === null || v === '') continue
    u.set(k, String(v))
  }
  const s = u.toString()
  return s ? `?${s}` : ''
}

async function get<T>(path: string): Promise<T> {
  const res = await fetch(`${BASE}${withInstance(path)}`, { credentials: 'include', signal: deadline() })
  if (!res.ok) throw new ApiError(res.status, await errorMessage(res))
  return res.json() as Promise<T>
}

async function post<T>(path: string, body: unknown): Promise<T> {
  const res = await fetch(`${BASE}${withInstance(path)}`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(body),
    signal: deadline(),
  })
  if (!res.ok) throw new ApiError(res.status, await errorMessage(res))
  return res.json() as Promise<T>
}

/** Everything the site asks bfdb for. The mock implements the same interface. */
export interface RangeApi {
  live(): Promise<LiveResponse>
  feed(q?: FeedQuery): Promise<FeedResponse>
  results(q?: ResultsQuery): Promise<ResultsResponse>
  /** the full record; bfdb also sends the summary fields (headline, card URLs) */
  result(id: string): Promise<RangeRecord & Partial<Pick<Summary, 'headline' | 'card_png' | 'card_svg'>>>
  me(): Promise<Me>
  pilot(ucid: string): Promise<PilotProfile>
  pilots(q: string): Promise<PilotHit[]>
  greenie(q?: GreenieQuery): Promise<GreenieResponse>
  leaderboards(days?: number): Promise<Leaderboards>
  stationImpacts(id: string, q?: { days?: number; pilot?: string }): Promise<StationImpacts>
  catalog(): Promise<SpawnCatalog>
  spawn(item: string, params: Record<string, string>): Promise<SpawnReply>
  despawn(target: { spawn_id: string } | 'all'): Promise<OkMessage>
  /** instructors only: respawn every target on a station */
  resetStation(station: string): Promise<OkMessage>
  weapons(): Promise<WeaponsResponse>
  /** Resolves to a Blob, or null when there is no recording (404). */
  tacview(id: string): Promise<Blob | null>
  /**
   * The cut of the server's recording around a result, for the 3D debrief.
   * Rejects with an ApiError (404 + the reason) when there is none.
   */
  clip(id: string): Promise<Clip>
  /** The same cut as a small Tacview file; null when there is none. */
  clipAcmi(id: string): Promise<Blob | null>
  /**
   * Where the result is in the dashboard's flight replay: the dashboard path
   * to open (recording, the pilot's flight, a moment before the result).
   * Rejects with a 404 until the replay has read the recording.
   */
  replayLink(id: string): Promise<{ rec: string; focus: number; t: number; path: string }>
  /** Card image URLs for a result id (PNG for download, SVG for display). */
  cardUrl(id: string, fmt: 'png' | 'svg'): string
  loginUrl(): string
  logout(): Promise<void>
}

const realApi: RangeApi = {
  live: () => get('/range/live'),
  feed: (q = {}) => get(`/range/feed${qs(q)}`),
  results: (q = {}) => get(`/range/results${qs(q)}`),
  result: id => get(`/range/result/${encodeURIComponent(id)}`),
  me: () => get('/range/me'),
  pilot: ucid => get(`/range/pilot/${encodeURIComponent(ucid)}`),
  pilots: q => get(`/range/pilots${qs({ q })}`),
  greenie: (q = {}) => get(`/range/greenie${qs(q)}`),
  leaderboards: days => get(`/range/leaderboards${qs({ days })}`),
  stationImpacts: (id, q = {}) => get(`/range/stations/${encodeURIComponent(id)}/impacts${qs(q)}`),
  catalog: () => get('/range/catalog'),
  spawn: (item, params) => post('/range/spawn', { item, params }),
  // bfdb takes `{spawn_id}`; the engine treats the id "all" as every spawn you own
  despawn: target => post('/range/despawn', { spawn_id: target === 'all' ? 'all' : target.spawn_id }),
  resetStation: station => post('/range/admin/reset-station', { station }),
  weapons: () => get('/range/weapons'),
  tacview: async id => {
    const res = await fetch(`${BASE}${withInstance(`/range/tacview/${encodeURIComponent(id)}`)}`, {
      credentials: 'include',
      signal: deadline(LONG_TIMEOUT_MS),
    })
    if (res.status === 404) return null
    if (!res.ok) throw new ApiError(res.status, await errorMessage(res))
    return res.blob()
  },
  clip: id => get(`/range/result/${encodeURIComponent(id)}/clip`),
  replayLink: id => get(`/range/result/${encodeURIComponent(id)}/replay`),
  clipAcmi: async id => {
    const res = await fetch(`${BASE}${withInstance(`/range/result/${encodeURIComponent(id)}/clip.acmi`)}`, { credentials: 'include', signal: deadline(LONG_TIMEOUT_MS) })
    if (res.status === 404) return null
    if (!res.ok) throw new ApiError(res.status, await errorMessage(res))
    return res.blob()
  },
  cardUrl: (id, fmt) => apiUrl(`/api/range/result/${encodeURIComponent(id)}/card.${fmt}`),
  loginUrl: () => `${BASE}/auth/login?return_to=${encodeURIComponent(window.location.origin + '/')}`,
  logout: async () => {
    await fetch(`${BASE}/auth/logout`, { method: 'POST', credentials: 'include', signal: deadline() })
  },
}

/**
 * The mock client, loaded on first use. Synchronous members (URLs) are
 * answered from a small sync shim until the module arrives.
 */
function mockApi(): RangeApi {
  let loaded: RangeApi | null = null
  const mod = import('./mock/server').then(m => {
    loaded = m.mockApi
    return m
  })
  const sync = {
    // any page that shows a card has already awaited a mock call, so the
    // module is loaded by then
    cardUrl: (id: string, fmt: 'png' | 'svg') => (loaded ? loaded.cardUrl(id, fmt) : ''),
    loginUrl: () => '#mock-login',
  }
  return new Proxy({} as RangeApi, {
    get(_t, key: string) {
      if (key in sync) return sync[key as keyof typeof sync]
      return (...args: unknown[]) =>
        mod.then(m => (m.mockApi as unknown as Record<string, (...a: unknown[]) => unknown>)[key](...args))
    },
  })
}

export const api: RangeApi = __RANGE_MOCK__ ? mockApi() : realApi
