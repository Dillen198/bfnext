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

async function get<T>(path: string): Promise<T> {
  const res = await fetch(`${API_ROOT}${path}`)
  if (!res.ok) throw new Error(`${res.status} ${res.statusText}`)
  return res.json() as Promise<T>
}

export const api = {
  stats: () => get<Stats>('/api/stats'),
  objectives: () => get<Objective[]>('/api/objectives'),
  leaderboard: () => get<Pilot[]>('/api/leaderboard'),
}
