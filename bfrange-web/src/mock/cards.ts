/**
 * Stand-ins for bfdb's rendered result cards, so the debrief page's card
 * image has something to show in mock mode. They follow the reference
 * layouts (FunkMan's bomb plot, the "Trap Attempt Detected" sheet) closely
 * enough to review the page against; the real cards come from the backend.
 */
import { airframe, weaponName } from '../lib/format'
import { gradeName } from '../lib/lso'
import { headline } from '../lib/headline'
import type { RangeRecord } from '../types'

const esc = (s: string) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
const svg = (w: number, h: number, body: string) =>
  `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${w} ${h}" width="${w}" height="${h}" font-family="DejaVu Sans, Arial, sans-serif">${body}</svg>`

function bombCard(r: RangeRecord): string {
  if (r.result.kind !== 'bomb') return ''
  const b = r.result
  const W = 640, H = 700
  const R = Math.max(60, Math.ceil((Math.max(b.miss_m * 1.35, b.good_radius_m * 2.2)) / 20) * 20)
  const cx = 320, cy = 360, S = 250 / R
  const X = (e: number) => cx + e * S
  const Y = (n: number) => cy - n * S
  let g = ''
  for (let v = -R; v <= R; v += R / 4) {
    g += `<line x1="${X(v)}" y1="${Y(-R)}" x2="${X(v)}" y2="${Y(R)}" stroke="#ccc" stroke-width="1"/>`
    g += `<line x1="${X(-R)}" y1="${Y(v)}" x2="${X(R)}" y2="${Y(v)}" stroke="#ccc" stroke-width="1"/>`
    g += `<text x="${X(v)}" y="${Y(-R) + 18}" font-size="11" text-anchor="middle" fill="#333">${Math.round(v)}</text>`
    g += `<text x="${X(-R) - 6}" y="${Y(v) + 4}" font-size="11" text-anchor="end" fill="#333">${Math.round(v)}</text>`
  }
  const rings = b.rings_m.map(rr => `<circle cx="${cx}" cy="${cy}" r="${rr * S}" fill="none" stroke="#777" stroke-dasharray="3 4"/>`).join('')
  const ix = X(b.impact_east_m), iy = Y(b.impact_north_m)
  const h = (b.release.heading_deg * Math.PI) / 180
  const ax = cx - Math.sin(h) * R * 0.72 * S, ay = cy + Math.cos(h) * R * 0.72 * S
  const box = (x: number, y: number, lines: string[], stroke: string, fill: string) => {
    const w = 132, lh = 16, hh = lines.length * lh + 10
    const bx = Math.min(W - w - 10, Math.max(10, x)), by = Math.min(H - hh - 50, Math.max(70, y))
    return `<rect x="${bx}" y="${by}" width="${w}" height="${hh}" rx="4" fill="${fill}" stroke="${stroke}"/>` +
      lines.map((l, i) => `<text x="${bx + 8}" y="${by + 18 + i * lh}" font-size="12" fill="#111">${esc(l)}</text>`).join('')
  }
  return svg(W, H, `
    <rect width="${W}" height="${H}" fill="#fff"/>
    <text x="${W / 2}" y="34" font-size="20" text-anchor="middle" font-weight="bold" fill="#111">Bombing result of ${esc(r.pilot.name)}</text>
    <text x="${W / 2}" y="56" font-size="14" text-anchor="middle" fill="#333">${esc(b.range)}: ${esc(b.target)}</text>
    <rect x="${X(-R)}" y="${Y(R)}" width="${2 * R * S}" height="${2 * R * S}" fill="#fafafa" stroke="#222"/>
    ${g}
    <circle cx="${cx}" cy="${cy}" r="${Math.max(6, (b.rings_m[0] ?? 10) * S)}" fill="#e3c98f" stroke="#b39a5c"/>
    ${rings}
    <line x1="${ax}" y1="${ay}" x2="${cx - Math.sin(h) * R * 0.3 * S}" y2="${cy + Math.cos(h) * R * 0.3 * S}" stroke="#2a8f3a" stroke-width="2" marker-end="url(#ah)"/>
    <defs><marker id="ah" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto"><path d="M0 0L10 5L0 10z" fill="#2a8f3a"/></marker>
    <marker id="nh" viewBox="0 0 10 10" refX="5" refY="5" markerWidth="8" markerHeight="8" orient="auto"><path d="M0 0L10 5L0 10z" fill="#111"/></marker></defs>
    <circle cx="${ix}" cy="${iy}" r="6" fill="#d62020" stroke="#fff" stroke-width="1.5"/>
    ${box(ix + 12, iy - 40, [weaponName(b.weapon), `r=${b.miss_m.toFixed(1)} m`, `φ=${b.radial_deg.toFixed(1)}°`, b.quality], '#d62020', '#ffecec')}
    ${box(ax - 140, ay - 20, [airframe(r.unit_type), `h=${Math.round(b.release.alt_agl_m / 0.3048)} ft`, `v=${Math.round(b.release.tas_kts)} kts`, `ψ=${Math.round(b.release.heading_deg)}°`], '#2a8f3a', '#eaf7ea')}
    <line x1="${W - 40}" y1="${Y(R) + 50}" x2="${W - 40}" y2="${Y(R) + 10}" stroke="#111" stroke-width="2" marker-end="url(#nh)"/>
    <text x="${W - 40}" y="${Y(R) + 66}" font-size="13" text-anchor="middle" font-weight="bold">N</text>
    <text x="${W / 2}" y="${Y(-R) + 40}" font-size="12" text-anchor="middle" fill="#333">metres east of target</text>
    <text x="${W / 2}" y="${H - 14}" font-size="12" text-anchor="middle" fill="#555">${esc(r.theatre)}: ${esc(r.mission_date)} (${esc(r.mission_time)})</text>
  `)
}

function trapCard(r: RangeRecord): string {
  if (r.result.kind !== 'trap') return ''
  const t = r.result
  const W = 900, H = 760
  const samples = r.track?.kind === 'groove' ? r.track.samples : []
  const NM = 1852
  // side view
  const sx = (x: number) => 60 + (1 - x / NM) * 780
  const sy = (a: number) => 470 - (a / 700) * 170
  const gs = (d: number) => `M${sx(NM)} ${sy((NM * Math.tan(((3.5 + d) * Math.PI) / 180)) / 0.3048)} L${sx(0)} ${sy(0)}`
  const bands = [[0, '#0a0'], [0.4, '#6a6'], [-0.4, '#6a6'], [0.8, '#cb0'], [-0.8, '#cb0'], [1.5, '#d33'], [-1.5, '#d33']] as const
  const side = samples.filter(s => s.x_m >= -50 && s.x_m <= NM)
  const sidePath = side.map((s, i) => `${i ? 'L' : 'M'}${sx(s.x_m).toFixed(1)} ${sy(s.alt_ft).toFixed(1)}`).join('')
  // top view
  const tx = (x: number) => 60 + (1 - x / (NM * 1.4)) * 780
  const ty = (y: number) => 640 + (y / 2600) * 110
  const top = samples.map((s, i) => `${i ? 'L' : 'M'}${tx(s.x_m).toFixed(1)} ${ty(s.y_m).toFixed(1)}`).join('')
  const arcs = [0.25, 0.5, 0.75, 1].map(f => `<line x1="${tx(f * NM)}" y1="560" x2="${tx(f * NM)}" y2="740" stroke="#bbb" stroke-dasharray="2 4"/><text x="${tx(f * NM)}" y="756" font-size="11" text-anchor="middle">${f} NM</text>`).join('')
  const lines = t.lso_description.length ? t.lso_description : ['(no deviations called)']
  return svg(W, H, `
    <rect width="${W}" height="${H}" fill="#2f3136"/>
    <rect x="12" y="12" width="${W - 24}" height="${H - 24}" rx="6" fill="#36393f"/>
    <text x="36" y="48" font-size="20" font-weight="bold" fill="#fff">Trap Attempt Detected</text>
    <text x="36" y="76" font-size="16" fill="#dcddde">${esc(r.pilot.name)} | ${esc(airframe(r.unit_type).replace(/[^A-Za-z0-9]/g, ''))} | CASE ${['I', 'II', 'III'][t.case - 1] ?? t.case} | ${t.night ? 'Night' : 'Day'}</text>
    <text x="36" y="106" font-size="15" fill="#fff" font-weight="bold">LSO Grade: <tspan font-weight="normal">${esc(t.grade)} =&gt; ${esc(gradeName(t.grade))}${t.wire ? ` · #${t.wire} wire` : ''}</tspan></text>
    <text x="36" y="132" font-size="15" fill="#fff" font-weight="bold">LSO Comment: <tspan font-weight="normal">${esc(t.lso_comment || '—')}</tspan></text>
    <text x="36" y="158" font-size="15" fill="#fff" font-weight="bold">LSO Comment Description</text>
    ${lines.map((l, i) => `<text x="48" y="${182 + i * 20}" font-size="14" fill="#dcddde">${esc(l)}</text>`).join('')}
    <rect x="40" y="270" width="${W - 80}" height="480" fill="#fff"/>
    <text x="60" y="292" font-size="13" font-weight="bold">Side view</text>
    ${bands.map(([d, c]) => `<path d="${gs(d)}" stroke="${c}" stroke-dasharray="${d === 0 ? '' : '3 4'}" fill="none"/>`).join('')}
    <path d="${sidePath}" stroke="#1b5fd1" stroke-width="2.2" fill="none"/>
    <text x="60" y="545" font-size="13" font-weight="bold">Top view</text>
    ${arcs}
    <line x1="${tx(NM * 1.4)}" y1="${ty(0)}" x2="${tx(-100)}" y2="${ty(0)}" stroke="#0a0"/>
    <path d="${top}" stroke="#1b5fd1" stroke-width="2" fill="none"/>
    <text x="${W - 60}" y="560" font-size="12" text-anchor="end">Wake Alt: ${t.pattern.wake_alt_ft ?? '—'}ft</text>
  `)
}

function genericCard(r: RangeRecord): string {
  const W = 640, H = 300
  const words = headline(r).split(' ')
  const lines: string[] = []
  let cur = ''
  for (const w of words) {
    if ((cur + ' ' + w).length > 48) { lines.push(cur); cur = w } else cur = cur ? `${cur} ${w}` : w
  }
  if (cur) lines.push(cur)
  return svg(W, H, `
    <rect width="${W}" height="${H}" fill="#2f3136"/>
    <text x="28" y="44" font-size="20" font-weight="bold" fill="#fff">${esc(r.result.kind.replace('_', ' ').toUpperCase())} RESULT</text>
    ${lines.map((l, i) => `<text x="28" y="${86 + i * 24}" font-size="17" fill="#dcddde">${esc(l)}</text>`).join('')}
    <text x="28" y="${H - 24}" font-size="12" fill="#999">${esc(r.theatre)}: ${esc(r.mission_date)} (${esc(r.mission_time)})</text>
  `)
}

export function mockCardDataUrl(r: RangeRecord): string {
  const body = r.result.kind === 'bomb' ? bombCard(r) : r.result.kind === 'trap' ? trapCard(r) : genericCard(r)
  return `data:image/svg+xml;charset=utf-8,${encodeURIComponent(body)}`
}
