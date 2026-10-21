import { useCallback, useMemo, useState } from 'react'
import { roundNo } from './api'
import { useTheatres } from './live'
import { fmtUtc, useStatusFile, type NoticeLevel } from './status'

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

const ROUND_NEWS_MS = 7 * 86_400_000
const MAINT_LEAD_MS = 7 * 86_400_000
const READ_KEY = 'vs-site-notifications-read'
const READ_KEEP = 200

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
      const start = Date.parse(m.start)
      const end = Date.parse(m.end)
      if (Number.isNaN(start) || Number.isNaN(end) || end <= now || start - MAINT_LEAD_MS > now) continue
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
      const hours = Math.max(0, (end - Date.parse(r.start)) / 3_600_000)
      const lasted = hours < 48 ? `${hours.toFixed(1)} hours` : `${(hours / 24).toFixed(1)} days`
      out.push({
        id: `round:${t.key}:${r.id}`,
        kind: 'round',
        title: `${t.label}: Round ${roundNo(r)} won by ${r.winner === 'Blue' ? t.blueLabel : t.redLabel}`,
        message: `The round lasted ${lasted} and ended ${fmtUtc(r.end)}.`,
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

function readIds(): string[] {
  try {
    const v = JSON.parse(localStorage.getItem(READ_KEY) ?? '[]')
    return Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string') : []
  } catch {
    return []
  }
}

/** Which notifications this browser has seen, and a way to mark them seen. */
export function useReadState(items: SiteNotification[]) {
  const [read, setRead] = useState<Set<string>>(() => new Set(readIds()))
  const unread = items.filter(n => !read.has(n.id)).length
  const markAllRead = useCallback(() => {
    const next = new Set(read)
    for (const n of items) next.add(n.id)
    const kept = [...next].slice(-READ_KEEP)
    try { localStorage.setItem(READ_KEY, JSON.stringify(kept)) } catch { /* private window */ }
    setRead(new Set(kept))
  }, [items, read])
  return { read, unread, markAllRead }
}
