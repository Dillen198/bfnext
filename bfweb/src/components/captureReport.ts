import type { CaptureBlockerKind, CaptureReport, CaptureTarget } from '../api'

/**
 * Pure helpers behind the "Why can't this be captured?" panel. The engine
 * decides what blocks a capture and what the asking side may see; this only
 * words and arranges it.
 */

/** Panel heading for the report, from the viewer's side of it. */
export function captureHeading(r: CaptureReport): string {
  if (r.in_progress) return 'Capture in progress'
  if (r.open) return r.perspective === 'defender' ? 'Open to capture' : 'Capturable now'
  return r.perspective === 'defender' ? 'What the enemy needs to capture it' : "Why can't this be captured?"
}

/** Blockers players can do nothing about, as opposed to work to be done. */
export function isDeadEnd(kind: CaptureBlockerKind): boolean {
  return kind === 'nothing_to_destroy'
}

/** Colour for a blocker: red for a dead end, yellow for a wait, orange for work. */
export function blockerTone(kind: CaptureBlockerKind): string {
  if (isDeadEnd(kind)) return 'var(--red)'
  if (kind === 'cooldown' || kind === 'consolidation_hold') return 'var(--yellow)'
  return 'var(--accent-bright)'
}

/** One line per target: what it is, and where if we know. */
export function targetLine(t: CaptureTarget): { what: string; where: string } {
  const name = t.label ?? (t.count === 1 ? `1 ${t.class} unit` : `${t.count} ${t.class} units`)
  const what = t.label ? `${name} (${t.class})` : name
  const where = t.grid ?? (t.lat != null && t.lon != null
    ? `${t.lat.toFixed(4)}, ${t.lon.toFixed(4)}`
    : 'position unknown')
  return { what, where }
}

/** Targets that can go on a map, with their index in `r.targets`. */
export function locatedTargets(r: CaptureReport): { t: CaptureTarget; i: number }[] {
  return r.targets
    .map((t, i) => ({ t, i }))
    .filter(({ t }) => t.lat != null && t.lon != null)
}

/** [[west, south], [east, north]] around the targets, padded so a single
 *  target (or a tight cluster) still shows its surroundings. */
export function targetBounds(
  pts: { lat: number; lon: number }[],
  padDeg = 0.006,
): [[number, number], [number, number]] | null {
  if (!pts.length) return null
  let w = Infinity, s = Infinity, e = -Infinity, n = -Infinity
  for (const p of pts) {
    w = Math.min(w, p.lon); e = Math.max(e, p.lon)
    s = Math.min(s, p.lat); n = Math.max(n, p.lat)
  }
  return [[w - padDeg, s - padDeg], [e + padDeg, n + padDeg]]
}

/** Marker colour for a target on the mini-map. */
export function targetColor(t: CaptureTarget): string {
  if (t.kind === 'building') return '#ffd24a'
  if (t.infantry) return '#f04747'
  return '#f0a030'
}
