import { useQuery } from '@tanstack/react-query'
import { appliesTo, fetchStatusFile, maintenanceVisible, type Notice } from '@shared/web/status'

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

export type { Notice, NoticeLevel } from '@shared/web/status'
export { fmtUtc } from '@shared/web/status'

const STATUS_URL: string = import.meta.env.VITE_STATUS_URL ?? '/status.json'

export function useStatusFile() {
  return useQuery({
    queryKey: ['status-file'],
    queryFn: () => fetchStatusFile(STATUS_URL),
    refetchInterval: 60_000,
    retry: 1,
  })
}

export function useNotices() {
  const { data, dataUpdatedAt } = useStatusFile()
  // The file is refetched every minute, so its fetch time is "now" to within
  // a minute -- close enough for windows measured in hours, and pure.
  const now = dataUpdatedAt
  const notices = data?.notices ?? []
  const maintenance = (data?.maintenance ?? []).filter(m => maintenanceVisible(m, now))
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
