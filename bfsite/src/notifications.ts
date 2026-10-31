import { useMemo } from 'react'
import { roundNo } from './api'
import { useTheatres } from './live'
import { fmtUtc, maintenanceVisible, type NoticeLevel } from '@shared/web/status'
import { lasted, ROUND_NEWS_MS, useReadState as useSharedReadState } from '@shared/web/notifications'
import { useStatusFile } from './status'

// ─────────────────────────────────────────────────────────────────────────────
// What the site's notification bell lists, the same as the dashboard's:
//   * status.json: every notice, and maintenance from a week out until it ends
//   * bfdb: each server's last won round, for a week after it ends.
// Read state is per browser (localStorage) and only drives the unread count.
// ─────────────────────────────────────────────────────────────────────────────

export interface SiteNotification {
  id: string
  kind: 'round' | NoticeLevel
  title: string
  message?: string
  time: number
  /** Tone class: b / r for a side, else the notice level. */
  tone: 'b' | 'r' | NoticeLevel
  link?: { href: string; label: string; external?: boolean }
}

const READ_KEY = 'vs-site-notifications-read'

export function useNotifications(dashboard: (path: string) => string): SiteNotification[] {
  const { data, dataUpdatedAt } = useStatusFile()
  const { theatres, checkedAt } = useTheatres()
  return useMemo(() => {
    // Fetch times stand in for "now": both poll every minute, and the list
    // stays pure. The later of the two, as either can be failing.
    const now = Math.max(dataUpdatedAt || 0, checkedAt ?? 0)
    const out: SiteNotification[] = []
    for (const n of data?.notices ?? []) {
      const eta = fmtUtc(n.eta)
      out.push({
        id: `notice:${n.id}`,
        kind: n.level,
        title: n.title,
        message: [n.message, eta && `${n.level === 'maintenance' ? 'Until' : 'Expected back'} ${eta}.`].filter(Boolean).join(' ') || undefined,
        time: Date.parse(n.since ?? '') || now,
        tone: n.level,
        link: { href: '/status', label: 'Status' },
      })
    }
    for (const m of data?.maintenance ?? []) {
      if (!maintenanceVisible(m, now)) continue
      const start = Date.parse(m.start)
      const running = start <= now
      out.push({
        id: `maint:${m.start}`,
        kind: 'maintenance',
        title: `${running ? 'Maintenance now' : 'Maintenance planned'}: ${m.title}`,
        message: [m.message, `${fmtUtc(m.start)} to ${fmtUtc(m.end)}.`].filter(Boolean).join(' '),
        time: running ? start : now,
        tone: 'maintenance',
        link: { href: '/status', label: 'Status' },
      })
    }
    for (const t of theatres) {
      const r = t.instance?.last_round
      if (!r?.end || (r.winner !== 'Blue' && r.winner !== 'Red')) continue
      const end = Date.parse(r.end)
      if (Number.isNaN(end) || now - end > ROUND_NEWS_MS) continue
      out.push({
        id: `round:${t.key}:${r.id}`,
        kind: 'round',
        title: `${t.label}: Round ${roundNo(r)} won by ${r.winner === 'Blue' ? t.blueLabel : t.redLabel}`,
        message: `The round lasted ${lasted(r.start, r.end)} and ended ${fmtUtc(r.end)}.`,
        time: end,
        tone: r.winner === 'Blue' ? 'b' : 'r',
        link: {
          href: dashboard(`/rounds?round=${r.id}&instance=${encodeURIComponent(t.key)}`),
          label: 'Full rundown',
          external: true,
        },
      })
    }
    return out.sort((a, b) => b.time - a.time)
  }, [data, dataUpdatedAt, checkedAt, theatres, dashboard])
}

/** Which notifications this browser has seen, and a way to mark them seen. */
export function useReadState(items: SiteNotification[]) {
  return useSharedReadState(items, READ_KEY)
}
