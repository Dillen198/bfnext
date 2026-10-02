// Minimal API client for the campaign website (live stats section)

// Default: relative '/api', for when bfsite is embedded in bfdb (rust-embed)
// and served from the same origin. Set VITE_API_BASE (e.g.
// "https://api.example.com", no trailing slash) at build time to point a
// standalone-hosted site at a remotely-hosted bfdb instead — see deploy/README.md.
const API_ROOT: string = import.meta.env.VITE_API_BASE ?? ''

export interface Stats {
  total_pilots: number
  total_rounds: number
  active_round: { id: number; scenario: string; start: string } | null
  objective_count: number
  total_kills: number
}

export interface Objective {
  id: string
  owner: 'Red' | 'Blue' | 'Neutral'
}

export interface Pilot {
  ucid: string
  name: string
  air_kills: number
  ground_kills: number
  deaths: number
  hours: number
}

/** One DCS server instance fronted by this bfdb. */
export interface ServerInstance {
  id: string
  label: string
  default: boolean
  live: boolean
  sortie: string | null
  active_round: { id: number; scenario: string; start: string } | null
  /** Always true here: bfdb only returns non-public (test) instances to an
   *  admin session, and the public site never has one. */
  public: boolean
}

async function get<T>(path: string): Promise<T> {
  const res = await fetch(`${API_ROOT}${path}`)
  if (!res.ok) throw new Error(`${res.status} ${res.statusText}`)
  return res.json() as Promise<T>
}

/** Scope a path to one DCS server. `undefined` lets bfdb pick its default,
 *  which is what a single-server deployment wants. */
function scoped(path: string, instance?: string): string {
  if (!instance) return path
  return path + (path.includes('?') ? '&' : '?') + `instance=${encodeURIComponent(instance)}`
}

export const api = {
  /** The DCS servers this bfdb fronts. A single-server bfdb returns one. */
  instances: () => get<{ default: string; instances: ServerInstance[] }>('/api/instances'),
  stats: (instance?: string) => get<Stats>(scoped('/api/stats', instance)),
  objectives: (instance?: string) => get<Objective[]>(scoped('/api/objectives', instance)),
  // Pilot totals span every server, so the leaderboard is deliberately global.
  leaderboard: () => get<Pilot[]>('/api/leaderboard'),
}
