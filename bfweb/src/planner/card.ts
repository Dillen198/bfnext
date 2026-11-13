// The route card: one kneeboard page (768x1024, the size DCS's kneeboard
// draws) as a PNG, so it can go straight into Saved Games\DCS\Kneeboard.

import { fmtClock, fmtDuration, legs, parseClock, type MagVar, wmmMagVar } from './nav'
import { aircraftDef, type Plan } from './types'

const W = 768
const H = 1024

/** "N41°36.576'"-style degrees and decimal minutes, the way the jets enter it. */
export function ddm(v: number, pos: string, neg: string): string {
  const a = Math.abs(v)
  let d = Math.floor(a)
  let m = (a - d) * 60
  if (Number(m.toFixed(3)) >= 60) { d += 1; m = 0 }
  const width = pos === 'E' ? 3 : 2
  return `${v >= 0 ? pos : neg}${String(d).padStart(width, '0')}°${m.toFixed(3).padStart(6, '0')}'`
}

/** One text row per waypoint: what the card prints and the tests read. */
export function cardRows(plan: Plan, magVar: MagVar = wmmMagVar): string[][] {
  const ls = legs(plan, magVar)
  const start = parseClock(plan.start_time)
  return plan.waypoints.map((w, i) => {
    const leg = i > 0 ? ls[i - 1] : null
    return [
      String(i + 1),
      (w.name || `WP${i + 1}`).slice(0, 10) + (w.target ? ' *' : ''),
      `${ddm(w.lat, 'N', 'S')} ${ddm(w.lon, 'E', 'W')}`,
      String(Math.round(w.alt_ft)),
      leg ? String(Math.round(leg.course_mag)).padStart(3, '0') : '',
      leg ? leg.dist_nm.toFixed(1) : '',
      leg ? fmtDuration(leg.ete_s) : '',
      start == null ? '' : fmtClock(start + (leg?.elapsed_s ?? 0)).slice(0, 5),
    ]
  })
}

export function routeCardPng(plan: Plan): Promise<Blob> {
  const c = document.createElement('canvas')
  c.width = W
  c.height = H
  const g = c.getContext('2d')!
  g.fillStyle = '#f4f1e8'
  g.fillRect(0, 0, W, H)
  g.fillStyle = '#111'
  g.textBaseline = 'top'

  let y = 28
  g.font = 'bold 30px monospace'
  g.fillText(plan.name.toUpperCase().slice(0, 36), 28, y)
  y += 40
  g.font = '18px monospace'
  const head = [aircraftDef(plan.aircraft).label, plan.callsign, `${plan.speed_kts} KT GS`, plan.start_time ? `T/O ${plan.start_time}` : '']
  g.fillText(head.filter(Boolean).join('  ·  '), 28, y)
  y += 34

  const cols = [28, 62, 196, 470, 548, 600, 660, 720]
  g.font = 'bold 15px monospace'
  ;['#', 'NAME', 'POSITION', 'ALT', 'CRS°M', 'NM', 'ETE', 'ETA'].forEach((h, i) => g.fillText(h, cols[i], y))
  y += 22
  g.fillRect(28, y - 4, W - 56, 2)

  g.font = '15px monospace'
  const rows = cardRows(plan)
  const rowH = Math.min(30, Math.floor((H - y - 260) / Math.max(rows.length, 1)))
  for (const [i, r] of rows.entries()) {
    if (i % 2 === 1) {
      g.fillStyle = '#e6e1d3'
      g.fillRect(28, y - 3, W - 56, rowH)
      g.fillStyle = '#111'
    }
    // Position is the wide column: lat over lon when the row is tall enough.
    r.forEach((cell, j) => {
      if (j === 2 && rowH >= 30) {
        const [lat, lon] = cell.split(' ')
        g.font = '13px monospace'
        g.fillText(lat, cols[j], y)
        g.fillText(lon, cols[j], y + 13)
        g.font = '15px monospace'
      } else {
        g.fillText(j === 2 ? cell.slice(0, 26) : cell, cols[j], y + (rowH >= 30 ? 6 : 0))
      }
    })
    y += rowH
  }
  const total = legs(plan).reduce((s, l) => ({ d: s.d + l.dist_nm, t: s.t + l.ete_s }), { d: 0, t: 0 })
  y += 6
  g.font = 'bold 15px monospace'
  g.fillText(`TOTAL ${total.d.toFixed(1)} NM  ${fmtDuration(total.t)}   * = target`, 28, y)
  y += 32

  const radios = [['COMM1', plan.comm1], ['COMM2', plan.comm2]] as const
  for (const [label, chans] of radios) {
    if (!chans.length || y > H - 80) continue
    g.font = 'bold 15px monospace'
    g.fillText(label, 28, y)
    g.font = '14px monospace'
    const items = [...chans].sort((a, b) => a.preset - b.preset)
      .map(ch => `${ch.preset}:${ch.freq_mhz.toFixed(3)}${ch.label ? ` ${ch.label.slice(0, 8)}` : ''}`)
    let x = 100
    for (const it of items) {
      const w = g.measureText(it).width + 18
      if (x + w > W - 28) { x = 100; y += 18 }
      g.fillText(it, x, y)
      x += w
    }
    y += 26
  }

  if (plan.notes && y < H - 40) {
    g.font = '14px monospace'
    for (const line of plan.notes.split('\n')) {
      if (y > H - 30) break
      g.fillText(line.slice(0, 84), 28, y)
      y += 18
    }
  }

  return new Promise((res, rej) => c.toBlob(b => (b ? res(b) : rej(new Error('canvas export failed'))), 'image/png'))
}
