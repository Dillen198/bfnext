// What makes an objective worth a look right now. Shared by the Objectives
// page's "Needs attention" list and the SITREP's front-line panel, so both
// rank the same base the same way.

import type { CaptureEvent, Objective } from '../api'
import { ago } from './format'

/** A capture this recent still counts as news. */
export const RECENT_CAPTURE_MS = 6 * 3_600_000

export type Tone = 'crit' | 'warn' | 'info'

export interface Attention {
  obj: Objective
  /** 0 = about to fall ... 4 = short of stock; lower sorts first. */
  rank: number
  reasons: { text: string; tone: Tone }[]
}

/** Reasons an objective needs attention, ranked so a base about to fall
 *  sorts above one that is merely short of fuel; null when there are none. */
export function attentionOf(o: Objective, lastCap: CaptureEvent | undefined, now: number): Attention | null {
  const reasons: Attention['reasons'] = []
  let rank = 9
  if (o.captureable) { reasons.push({ text: 'Ready to capture', tone: 'crit' }); rank = Math.min(rank, 0) }
  if (o.threatened) { reasons.push({ text: 'Under attack', tone: 'crit' }); rank = Math.min(rank, 1) }
  if (o.health < 40) { reasons.push({ text: `Defences ${o.health}%`, tone: 'warn' }); rank = Math.min(rank, 2) }
  if (lastCap && now - Date.parse(lastCap.time) < RECENT_CAPTURE_MS) {
    reasons.push({ text: `Taken by ${lastCap.side} ${ago(lastCap.time, now)}`, tone: 'info' }); rank = Math.min(rank, 3)
  }
  if (o.supply < 25) { reasons.push({ text: `Supply ${o.supply}%`, tone: 'warn' }); rank = Math.min(rank, 4) }
  if (o.fuel < 25) { reasons.push({ text: `Fuel ${o.fuel}%`, tone: 'warn' }); rank = Math.min(rank, 4) }
  return reasons.length ? { obj: o, rank, reasons } : null
}

/** Every objective needing attention, most urgent first. `captures` arrive
 *  newest first, as /api/capture-events serves them. */
export function attentionList(objectives: Objective[], captures: CaptureEvent[], now: number): Attention[] {
  const last = new Map<string, CaptureEvent>()
  for (const e of captures) if (!last.has(e.objective)) last.set(e.objective, e)
  return objectives
    .map(o => attentionOf(o, last.get(o.name), now))
    .filter((a): a is Attention => a !== null)
    .sort((a, b) => a.rank - b.rank || a.obj.health - b.obj.health)
}

export const TONE: Record<Tone, { fg: string; bg: string }> = {
  crit: { fg: 'var(--red)', bg: 'var(--red-bg)' },
  warn: { fg: 'var(--yellow)', bg: 'var(--accent-glow)' },
  info: { fg: 'var(--text-muted)', bg: 'var(--bg-elevated)' },
}
