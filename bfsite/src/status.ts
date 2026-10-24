import { useQuery } from '@tanstack/react-query'

// ─────────────────────────────────────────────────────────────────────────────
// Server status that does not depend on bfdb.
//
// Two sources, merged:
//   1. public/status.json -- notices an admin writes by hand ("down for a RAM
//      upgrade, back Sunday"). It is served by the site's own static host, so
//      it is readable when the game host and bfdb are both down. The dashboard
//      reads the same file cross-origin.
//   2. Probes of bfdb itself: whether /api answers at all, and per server
//      whether the DCS engine behind it does (see useServerStates in live.ts).
// A notice wins over a probe: a probe can only say "offline", a notice says why.
// ─────────────────────────────────────────────────────────────────────────────

export type NoticeLevel = 'outage' | 'degraded' | 'maintenance' | 'info'

export interface Notice {
  id: string
  level: NoticeLevel
  title: string
  message?: string
  /** Instance ids; absent means every server. */
  servers?: string[]
  since?: string
  /** Expected return, ISO 8601 UTC. */
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

const STATUS_URL: string = import.meta.env.VITE_STATUS_URL ?? '/status.json'

/** Windows show from a week before they start until they end. */
const MAINT_LEAD_MS = 7 * 86_400_000

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

const appliesTo = (servers: string[] | undefined, id: string) => !servers || servers.length === 0 || servers.includes(id)

export function useNotices() {
  const { data, dataUpdatedAt } = useStatusFile()
  // The file is refetched every minute, so its fetch time is "now" to within
  // a minute -- close enough for windows measured in hours, and pure.
  const now = dataUpdatedAt
  const notices = data?.notices ?? []
  const maintenance = (data?.maintenance ?? []).filter(m => {
    const start = Date.parse(m.start)
    const end = Date.parse(m.end)
    return !Number.isNaN(start) && !Number.isNaN(end) && end > now && start - MAINT_LEAD_MS < now
  })
  /** Maintenance windows running right now count as a notice for their servers. */
  const activeMaintenance: Notice[] = maintenance
    .filter(m => Date.parse(m.start) <= now)
    .map(m => ({
      id: `maint-${m.start}`,
      level: 'maintenance',
      title: m.title,
      message: m.message,
      servers: m.servers,
      since: m.start,
      eta: m.end,
    }))
  const all = [...notices, ...activeMaintenance]
  return {
    notices: all,
    upcoming: maintenance.filter(m => Date.parse(m.start) > now),
    forServer: (id: string) => all.filter(n => appliesTo(n.servers, id)),
  }
}

/** The state a server card or the status page shows. Ordered worst first. */
export type ServerState = 'outage' | 'offline' | 'maintenance' | 'engine-down' | 'degraded' | 'live' | 'standby' | 'unknown'

export const STATE_LABEL: Record<ServerState, string> = {
  outage: 'Outage',
  offline: 'Offline',
  maintenance: 'Maintenance',
  'engine-down': 'Server down',
  degraded: 'Degraded',
  live: 'Round live',
  standby: 'Between rounds',
  unknown: 'Checking…',
}

/** Tone for colouring a state: ok (green), warn (amber), bad (red), idle (khaki). */
export function stateTone(s: ServerState): 'ok' | 'warn' | 'bad' | 'idle' {
  switch (s) {
    case 'live': return 'ok'
    case 'maintenance':
    case 'degraded': return 'warn'
    case 'outage':
    case 'offline':
    case 'engine-down': return 'bad'
    default: return 'idle'
  }
}

/** "Sun 19 Oct, 18:00 UTC" */
export function fmtUtc(iso: string | undefined): string | undefined {
  if (!iso) return undefined
  const d = new Date(iso)
  if (Number.isNaN(d.getTime())) return undefined
  return d.toLocaleString('en-GB', {
    weekday: 'short', day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit', timeZone: 'UTC',
  }) + ' UTC'
}
