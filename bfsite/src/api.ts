// Minimal client for the handful of public bfdb endpoints the site shows live.

// Default: relative '/api', for when bfsite is embedded in bfdb (rust-embed)
// and served from the same origin. Set VITE_API_BASE (e.g.
// "https://api.example.com", no trailing slash) at build time to point a
// standalone-hosted site at a remotely-hosted bfdb instead — see deploy/README.md.
export const API_ROOT: string = import.meta.env.VITE_API_BASE ?? ''

export type Side = 'Blue' | 'Red' | 'Neutral'

export interface Round {
  id: number
  scenario: string
  start: string
}

/** One DCS server instance fronted by this bfdb. */
export interface ServerInstance {
  id: string
  label: string
  default: boolean
  live: boolean
  sortie: string | null
  active_round: Round | null
  /** Always true here: bfdb only returns non-public (test) instances to an
   *  admin session, and the public site never has one. */
  public: boolean
}

export interface Objective {
  id: string
  owner: Side
}

/** `/api/online` also carries names and aircraft; the site only counts. */
export interface OnlinePilot {
  side: Side
}

export interface NewsDay {
  /** YYYY-MM-DD, UTC. */
  day: string
  round: number
  headline: string
  body?: string[]
  items?: { text?: string }[]
  /** A path under the API root, or null. */
  image?: string | null
}

/** One bfdb instance's own view of its game server. */
export interface Health {
  ok: boolean
  engine: { reachable: boolean; latency_ms: number | null; error: string | null }
  active_round: { id: number; start: string } | null
}

// bfdb's host can vanish outright (Cloudflare then holds the request open
// before answering 52x), so every call gets a deadline rather than hanging.
const TIMEOUT_MS = 12_000

async function get<T>(path: string): Promise<T> {
  const res = await fetch(`${API_ROOT}${path}`, { signal: AbortSignal.timeout(TIMEOUT_MS) })
  if (!res.ok) throw new Error(`${res.status} ${res.statusText}`)
  return res.json() as Promise<T>
}

/** Scope a path to one DCS server. `undefined` lets bfdb pick its default. */
function scoped(path: string, instance?: string): string {
  if (!instance) return path
  return path + (path.includes('?') ? '&' : '?') + `instance=${encodeURIComponent(instance)}`
}

export const api = {
  /** The DCS servers this bfdb fronts. A single-server bfdb returns one. */
  instances: () => get<{ default: string; instances: ServerInstance[] }>('/api/instances'),
  objectives: (instance?: string) => get<Objective[]>(scoped('/api/objectives', instance)),
  online: (instance?: string) => get<OnlinePilot[]>(scoped('/api/online', instance)),
  health: (instance?: string) => get<Health>(scoped('/api/health', instance)),
  latestNews: (instance?: string) => get<{ days: NewsDay[] }>(scoped('/api/news?limit=1', instance)),
}
