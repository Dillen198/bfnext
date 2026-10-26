// Tacview's statistics window: every flight in the recording in one sortable
// table -- shots, hits, kills, how it ended and how long it flew. Click a row
// to follow that flight. And the telemetry export: the followed object's
// whole track as CSV, the way Tacview exports a flight's data.

import { useMemo, useState } from 'react'
import { type RecMeta, type Seg, sideHex, fmtDuration, isStepChannel } from './data'

type Col = 'pilot' | 'aircraft' | 'color' | 'shots' | 'hits' | 'kills' | 'fate' | 'time'

const COLS: [Col, string][] = [
  ['pilot', 'Pilot'], ['aircraft', 'Type'], ['shots', 'Sh'], ['hits', 'Hit'], ['kills', 'K'], ['fate', 'End'], ['time', 'Time'],
]

export default function Stats({ meta, humans, focus, onPick }: {
  meta: RecMeta; humans: Set<number> | null; focus: number | null; onPick: (i: number) => void
}) {
  const [sort, setSort] = useState<{ col: Col; dir: 1 | -1 }>({ col: 'kills', dir: -1 })
  const [players, setPlayers] = useState(!!humans && humans.size > 0)
  const rows = useMemo(() => {
    const val = (f: RecMeta['flights'][number], c: Col): string | number => {
      switch (c) {
        case 'pilot': return f.pilot.toLowerCase()
        case 'aircraft': return f.aircraft
        case 'color': return f.color ?? ''
        case 'shots': return f.shots
        case 'hits': return f.hits ?? 0
        case 'kills': return f.kills
        case 'fate': return f.fate
        case 'time': return f.t1 - f.t0
      }
    }
    return meta.flights
      .filter(f => !players || !humans || humans.has(f.i))
      .sort((a, b) => {
        const x = val(a, sort.col), y = val(b, sort.col)
        const c = typeof x === 'number' && typeof y === 'number' ? x - y : String(x).localeCompare(String(y))
        return c * sort.dir || a.pilot.localeCompare(b.pilot)
      })
  }, [meta, sort, players, humans])
  const tot = rows.reduce((t, f) => ({ shots: t.shots + f.shots, hits: t.hits + (f.hits ?? 0), kills: t.kills + f.kills, lost: t.lost + (f.fate === 'destroyed' ? 1 : 0) }), { shots: 0, hits: 0, kills: 0, lost: 0 })
  return (
    <>
      <div className="rp-search">
        {humans && <button className={`rp-chip ${players ? 'on' : ''}`} onClick={() => setPlayers(p => !p)}>PLAYERS ONLY</button>}
        <span className="rp-stats-tot">{rows.length} flights · {tot.shots} shots · {tot.hits} hits · {tot.kills} kills · {tot.lost} lost</span>
      </div>
      <div className="rp-list">
        <table className="rp-stats">
          <thead>
            <tr>
              {COLS.map(([c, label]) => (
                <th key={c} onClick={() => setSort(s => ({ col: c, dir: s.col === c ? (s.dir === 1 ? -1 : 1) : c === 'pilot' || c === 'aircraft' ? 1 : -1 }))}>
                  {label}{sort.col === c ? (sort.dir === 1 ? ' ▴' : ' ▾') : ''}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rows.map(f => (
              <tr key={f.i} className={f.i === focus ? 'on' : ''} onClick={() => onPick(f.i)}>
                <td><span className="rp-dot" style={{ background: sideHex(f.color) }} /> {f.pilot}</td>
                <td>{f.aircraft.replace(/_/g, ' ')}</td>
                <td>{f.shots || ''}</td>
                <td>{f.hits || ''}</td>
                <td className="k">{f.kills || ''}</td>
                <td className={f.fate === 'destroyed' ? 'x' : ''}>{f.fate === 'destroyed' ? 'lost' : f.fate === 'landed' ? 'landed' : ''}</td>
                <td>{fmtDuration(f.t1 - f.t0)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </>
  )
}

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
