import { useCallback, useMemo, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api, roundNo, type ServerInstance } from '../api'
import { useInstance } from '../context/InstanceContext'
import { appliesTo, fmtUtc, STATUS_PAGE, useStatusFile, type Level } from './statusFile'

// ─────────────────────────────────────────────────────────────────────────────
// What the notification bell lists. Two sources:
//   * status.json: every hand-written notice, and maintenance windows from a
//     week out until they end -- the ones for the selected server, or for all.
//   * bfdb: each server's last won round, for a week after it ends.
// Read state is per viewer and per browser (localStorage). It only drives the
// unread count; nothing here is lost if storage is unavailable.
// ─────────────────────────────────────────────────────────────────────────────

export type NotificationKind = 'round' | Level

export interface AppNotification {
  /** Stable across polls, so read state survives a refetch. */
  id: string
  kind: NotificationKind
  title: string
  message?: string
  /** When it happened (ms), for ordering and "3h ago". */
  time: number
  /** CSS colour of its marker. */
  color: string
  /** The server it is about, when it is about one. */
  instance?: string
  link?: { href: string; label: string; external?: boolean }
}

const ROUND_NEWS_MS = 7 * 86_400_000
const MAINT_LEAD_MS = 7 * 86_400_000
const READ_KEY = 'vs-notifications-read'
const READ_KEEP = 200

const LEVEL_COLOR: Record<Level, string> = {
  outage: 'var(--red)',
  maintenance: 'var(--yellow)',
  degraded: 'var(--yellow)',
  info: 'var(--accent)',
}

function lasted(start: string, end: string): string {
  const hours = Math.max(0, (Date.parse(end) - Date.parse(start)) / 3_600_000)
  return hours < 48 ? `${hours.toFixed(1)} hours` : `${(hours / 24).toFixed(1)} days`
}

/** A server's last won round, while it is news. */
function roundNotification(inst: ServerInstance, now: number, multi: boolean): AppNotification | null {
  const r = inst.last_round
  if (!r?.end || (r.winner !== 'Blue' && r.winner !== 'Red')) return null
  const end = Date.parse(r.end)
  if (Number.isNaN(end) || now - end > ROUND_NEWS_MS) return null
  const blue = r.winner === 'Blue'
  const side = (blue ? inst.blue_label : inst.red_label) || r.winner_name || r.winner
  return {
    id: `round:${inst.id}:${r.id}`,
    kind: 'round',
    title: `Round ${roundNo(r)} over: ${side} won`,
    message: `${multi ? `${inst.label}. ` : ''}The round lasted ${lasted(r.start, r.end)} and ended ${fmtUtc(r.end)}.`,
    time: end,
    color: blue ? 'var(--blue)' : 'var(--red)',
    instance: inst.id,
    link: { href: `/rounds?round=${r.id}`, label: 'Full rundown' },
  }
}

export function useNotifications(): AppNotification[] {
  const { current, multi } = useInstance()
  const file = useStatusFile()
  // Its own minute poll: the shared instance list is cached for five
  // minutes, and a round result that turns up late is no news.
  const inst = useQuery({
    queryKey: ['instances', 'notifications'],
    queryFn: api.instances,
    refetchInterval: 60_000,
    staleTime: 30_000,
  })
  const fileAt = file.dataUpdatedAt
  const instAt = inst.dataUpdatedAt

  return useMemo(() => {
    const now = Math.max(fileAt, instAt) || 0
    const id = current?.id
    const out: AppNotification[] = []
    for (const n of file.data?.notices ?? []) {
      if (!appliesTo(n.servers, id)) continue
      const eta = fmtUtc(n.eta)
      out.push({
        id: `notice:${n.id}`,
        kind: n.level,
        title: n.title,
        message: [n.message, eta && `${n.level === 'maintenance' ? 'Until' : 'Expected back'} ${eta}.`].filter(Boolean).join(' ') || undefined,
        time: Date.parse(n.since ?? '') || fileAt,
        color: LEVEL_COLOR[n.level] ?? LEVEL_COLOR.info,
        link: { href: STATUS_PAGE, label: 'Status', external: true },
      })
    }
    for (const m of file.data?.maintenance ?? []) {
      const start = Date.parse(m.start)
      const end = Date.parse(m.end)
      if (Number.isNaN(start) || Number.isNaN(end) || end <= now || start - MAINT_LEAD_MS > now) continue
      if (!appliesTo(m.servers, id)) continue
      const running = start <= now
      out.push({
        id: `maint:${m.start}`,
        kind: 'maintenance',
        title: `${running ? 'Maintenance now' : 'Maintenance planned'}: ${m.title}`,
        message: [m.message, `${fmtUtc(m.start)} to ${fmtUtc(m.end)}.`].filter(Boolean).join(' '),
        time: running ? start : now,
        color: LEVEL_COLOR.maintenance,
        link: { href: STATUS_PAGE, label: 'Status', external: true },
      })
    }
    for (const i of inst.data?.instances ?? []) {
      const r = roundNotification(i, now, multi)
      if (r) out.push(r)
    }
    return out.sort((a, b) => b.time - a.time)
  }, [file.data, inst.data, fileAt, instAt, current?.id, multi])
}

function readIds(): string[] {
  try {
    const v = JSON.parse(localStorage.getItem(READ_KEY) ?? '[]')
    return Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string') : []
  } catch {
    return []
  }
}

/** Which notifications this viewer has seen, and a way to mark them seen. */
export function useReadState(items: AppNotification[]) {
  const [read, setRead] = useState<Set<string>>(() => new Set(readIds()))
  const unread = items.filter(n => !read.has(n.id)).length
  const markAllRead = useCallback(() => {
    const next = new Set(read)
    for (const n of items) next.add(n.id)
    // Newest last, trimmed, so the list cannot grow without end.
    const kept = [...next].slice(-READ_KEEP)
    try { localStorage.setItem(READ_KEY, JSON.stringify(kept)) } catch { /* private window */ }
    setRead(new Set(kept))
  }, [items, read])
  return { read, unread, markAllRead }
}
