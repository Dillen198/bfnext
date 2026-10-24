import { useQuery } from '@tanstack/react-query'

// ─────────────────────────────────────────────────────────────────────────────
// The hand-written server notices ("down for a RAM upgrade, back Sunday").
//
// They live in bfsite's public/status.json, served by the public site's static
// host -- so they are readable when the game host, and bfdb with it, is down.
// That is the one time a dashboard most needs to explain itself. The public
// site's /status page reads the same file. The outage banner and the
// notification bell both read it, through this one query.
// ─────────────────────────────────────────────────────────────────────────────

export const STATUS_URL: string = import.meta.env.VITE_STATUS_URL ?? 'https://www.vectorstrike.org/status.json'
export const STATUS_PAGE = new URL('/status', STATUS_URL).href

export type Level = 'outage' | 'degraded' | 'maintenance' | 'info'

export interface Notice {
  id: string
  level: Level
  title: string
  message?: string
  servers?: string[]
  since?: string
  eta?: string
}

export interface Maintenance {
  title: string
  message?: string
  servers?: string[]
  start: string
  end: string
}

export interface StatusFile {
  notices?: Notice[]
  maintenance?: Maintenance[]
}

export const RANK: Record<Level, number> = { outage: 0, maintenance: 1, degraded: 2, info: 3 }

export function fmtUtc(iso?: string): string | undefined {
  if (!iso) return undefined
  const d = new Date(iso)
  if (Number.isNaN(d.getTime())) return undefined
  return d.toLocaleString('en-GB', { weekday: 'short', day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit', timeZone: 'UTC' }) + ' UTC'
}

export function useStatusFile() {
  return useQuery({
    queryKey: ['status-file'],
    queryFn: async (): Promise<StatusFile> => {
      const res = await fetch(STATUS_URL, { cache: 'no-store' })
      if (!res.ok) throw new Error(`${res.status}`)
      return res.json()
    },
    refetchInterval: 60_000,
    retry: 1,
  })
}

/** Whether a notice scoped to `servers` concerns instance `id` (no scope: every server). */
export const appliesTo = (servers: string[] | undefined, id: string | undefined) =>
  !servers?.length || !id || servers.includes(id)
