// ─────────────────────────────────────────────────────────────────────────────
// The hand-written server notices ("down for a RAM upgrade, back Sunday").
//
// They live in bfsite's public/status.json, served by the public site's own
// static host -- so they are readable when the game host, and bfdb with it, is
// down, which is when a page most needs to explain itself. The public site's
// /status page and the dashboard's banner and notification bell all read it.
// ─────────────────────────────────────────────────────────────────────────────

import { deadline } from './api'

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

/** Worst first. */
export const LEVEL_RANK: Record<NoticeLevel, number> = { outage: 0, maintenance: 1, degraded: 2, info: 3 }

/** Maintenance windows show from a week before they start until they end. */
export const MAINT_LEAD_MS = 7 * 86_400_000

/** The status file, uncached. Polled by each app's own react-query hook. */
export async function fetchStatusFile(url: string): Promise<StatusFile> {
  const res = await fetch(url, { cache: 'no-store', signal: deadline(12_000) })
  if (!res.ok) throw new Error(`${res.status}`)
  return res.json()
}

/** Whether a notice scoped to `servers` concerns instance `id` (no scope, or
 *  no instance chosen: every server). */
export function appliesTo(servers: string[] | undefined, id: string | undefined): boolean {
  return !servers?.length || !id || servers.includes(id)
}

/** Whether a maintenance window is worth showing at `now`: it has not ended,
 *  and starts within the lead time. Junk dates never show. */
export function maintenanceVisible(m: Maintenance, now: number): boolean {
  const start = Date.parse(m.start)
  const end = Date.parse(m.end)
  return !Number.isNaN(start) && !Number.isNaN(end) && end > now && start - MAINT_LEAD_MS <= now
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
