// Sortie helpers shared by the Flight Log, the sortie detail panel, the
// career overview and the full-page debrief.

import type { PilotSortie, SortieOutcome } from '../api'
import type { FlightRow } from '../replay/data'

/** How each sortie ending reads in the Flight Log, the sortie detail and the
 *  career overview. Checked with the dataviz palette validator in landed /
 *  no-landing / lost / shot-down order on both themes; grey is a deliberate
 *  neutral ("unknown"), and every use carries a text label. */
export const OUTCOME: Record<SortieOutcome, { label: string; color: string }> = {
  landed:     { label: '✈ Landed',     color: '#16a34a' },
  killed:     { label: '✕ Shot down',  color: '#dc2626' },
  lost:       { label: '✕ Lost',       color: '#ca8a04' },
  no_landing: { label: '— No landing', color: '#64748b' },
}

/** A sortie's outcome; an older bfdb sends only `landed`. */
export function outcomeOf(s: Pick<PilotSortie, 'outcome' | 'landed'>): SortieOutcome {
  return s.outcome ?? (s.landed ? 'landed' : 'no_landing')
}

/** The recorded flight a sortie happened in: the one whose time in the
 *  recording spans the takeoff (a minute's slack either side), same airframe
 *  preferred. */
export function replayFor(s: { takeoff: string; aircraft: string }, replays: FlightRow[]): FlightRow | undefined {
  const t = Date.parse(s.takeoff)
  if (Number.isNaN(t)) return undefined
  const hits = replays.filter(r => t >= r.start_ms - 60_000 && t <= r.end_ms + 60_000)
  return hits.find(r => r.aircraft === s.aircraft) ?? hits[0]
}
