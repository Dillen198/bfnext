// The telemetry export: the followed object's whole track as CSV, the way
// Tacview exports a flight's data. Its own module, not Stats.tsx, so that
// file only exports components (react-refresh/only-export-components).

import { type RecMeta, type Seg, isStepChannel } from './data'

/** Save one object's whole track as CSV: time, position, attitude and
 *  every recorded property, one row per sample. */
export function downloadCsv(meta: RecMeta, idx: number, s: Seg) {
  const o = meta.objects[idx]
  const cols: [string, ArrayLike<number> | undefined, number][] = [
    ['lon', s.lon, 7], ['lat', s.lat, 7], ['alt_m', s.alt, 1],
    ['roll', s.roll, 2], ['pitch', s.pitch, 2], ['yaw', s.yaw, 2],
    ['ias_ms', s.ias, 2], ['mach', s.mach, 3], ['aoa', s.aoa, 2], ['agl_m', s.agl, 1],
  ]
  for (const [k, v] of Object.entries(s.ch ?? {})) cols.push([k, v, isStepChannel(k) ? 0 : 3])
  const used = cols.filter(c => c[1])
  const base = meta.start_ms || 0
  const lines = [['time_s', 'utc', ...used.map(c => c[0])].join(',')]
  for (let i = 0; i < s.t.length; i++) {
    const row = [(s.t[i] / 1000).toFixed(2), base ? new Date(base + s.t[i]).toISOString() : '']
    for (const [, v, dp] of used) row.push(v![i].toFixed(dp))
    lines.push(row.join(','))
  }
  const blob = new Blob([lines.join('\n') + '\n'], { type: 'text/csv' })
  const a = document.createElement('a')
  a.href = URL.createObjectURL(blob)
  a.download = `${(o?.p ?? o?.n ?? `object-${idx}`).replace(/[^\w.-]+/g, '_')}.csv`
  a.click()
  setTimeout(() => URL.revokeObjectURL(a.href), 5_000)
}
