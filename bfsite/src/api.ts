// Minimal client for the handful of public bfdb endpoints the site shows live.

// Default: relative '/api', for when bfsite is embedded in bfdb (rust-embed)
// and served from the same origin. Set VITE_API_BASE (e.g.
// "https://api.example.com", no trailing slash) at build time to point a
// standalone-hosted site at a remotely-hosted bfdb instead — see deploy/README.md.
export const API_ROOT: string = import.meta.env.VITE_API_BASE ?? ''

export type Side = 'Blue' | 'Red' | 'Neutral'

export interface Round {
  /** A database key, not a name: use `roundNo`. */
  id: number
  /** The round's number on its own server, 1 for the first. */
  number?: number | null
  scenario: string
  start: string
}

/** A server's most recently finished round and who won it. */
export interface LastRound extends Round {
  end: string
  /** "Blue", "Red", or null: a draw, or a reset that named no winner. */
  winner: Side | null
}

/** What to call a round: its number on its server (see `withRoundNumbers`). */
export const roundNo = (r: { id: number; number?: number | null }) => r.number ?? r.id

interface RoundRow {
  id: number
  instance: string
  start: string
}

/**
 * Fill in the rounds' `number` where bfdb did not send it -- a bfdb from
 * before v2.7, which a server keeps running until its auto-update applies
 * the new one. Same rule as bfdb: 1, 2, 3 per server in start order, from
 * the round list. Without it an older bfdb's rounds read as their database
 * ids (0, then 108000656).
 */
async function withRoundNumbers(data: { default: string; instances: ServerInstance[] }) {
  const rounds = data.instances.flatMap(i => [i.active_round, i.last_round]).filter((r): r is Round => !!r)
  if (rounds.every(r => r.number != null)) return data
  try {
    const all = await get<RoundRow[]>('/api/rounds?instance=all')
    const n = new Map<number, number>()
    const byServer = new Map<string, RoundRow[]>()
    for (const r of all) byServer.set(r.instance, [...(byServer.get(r.instance) ?? []), r])
    for (const list of byServer.values()) {
      [...list].sort((a, b) => a.start.localeCompare(b.start)).forEach((r, i) => n.set(r.id, i + 1))
    }
    for (const r of rounds) r.number ??= n.get(r.id) ?? null
  } catch {
    // The list is a nicety: without it the ids show, as before.
  }
  return data
}

/** One DCS server instance fronted by this bfdb. */
export interface ServerInstance {
  id: string
  label: string
  default: boolean
  live: boolean
  sortie: string | null
  active_round: Round | null
  last_round?: LastRound | null
  /** The server's own names for its sides, where bfdb has them. */
  blue_label?: string | null
  red_label?: string | null
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
  active_round: { id: number; number?: number | null; start: string } | null
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
  instances: () => get<{ default: string; instances: ServerInstance[] }>('/api/instances').then(withRoundNumbers),
  objectives: (instance?: string) => get<Objective[]>(scoped('/api/objectives', instance)),
  online: (instance?: string) => get<OnlinePilot[]>(scoped('/api/online', instance)),
  health: (instance?: string) => get<Health>(scoped('/api/health', instance)),
  latestNews: (instance?: string) => get<{ days: NewsDay[] }>(scoped('/api/news?limit=1', instance)),
}
