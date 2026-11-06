/**
 * The range chart for the kneeboard: the live picture (sectors, targets with
 * their laser codes, SAM and jammer rings, the carriers, the tankers on
 * station) drawn as plain SVG over Esri tiles (lib/kneeboardInk.ts), with a lat/long grid,
 * a scale in nautical miles and a key. Plain SVG so it prints crisp and
 * rasterises to a PNG for the DCS kneeboard folder (lib/kneeboard.ts).
 *
 * Two inks: light, black on paper for the day, and dark, pale on black for
 * night, so the card does not light up the cockpit.
 */
import type { ReactNode } from 'react'
import { fmt, pad3 } from '../lib/format'
import { NM, type LatLon } from '../lib/geo'
import { drawSectors, SECTOR_KIND_ORDER, kindInfo } from '../lib/sectors'
import type { LiveStation, RangeLive, SectorKind } from '../types'
import { KB_INK, KB_MONO, KB_SANS, TILES, sectorInk, type Ink, type KbTheme } from '../lib/kneeboardInk'

export type { KbTheme }

// ─── projection (web mercator, z0 world units of 256) ─────────────────────

const wx = (lon: number) => ((lon + 180) / 360) * 256
const wy = (lat: number) => {
  const s = Math.sin((lat * Math.PI) / 180)
  return (0.5 - Math.log((1 + s) / (1 - s)) / (4 * Math.PI)) * 256
}
const lonOfW = (x: number) => (x / 256) * 360 - 180
const latOfW = (y: number) => {
  const n = Math.PI - (2 * Math.PI * y) / 256
  return (180 / Math.PI) * Math.atan(Math.sinh(n))
}

interface View {
  W: number
  H: number
  /** screen px per z0 world unit */
  s: number
  x: (p: LatLon) => number
  y: (p: LatLon) => number
  lonAt: (sx: number) => number
  latAt: (sy: number) => number
  /** ground metres per screen px at the chart's middle */
  mpp: number
}

function fit(pts: LatLon[], W: number, H: number, pad: number): View {
  const xs = pts.map(p => wx(p.lon))
  const ys = pts.map(p => wy(p.lat))
  let x0 = Math.min(...xs), x1 = Math.max(...xs), y0 = Math.min(...ys), y1 = Math.max(...ys)
  // never closer in than ~12 km across, so a lone target still has a map round it
  const min = 12_000 / 156_543 / Math.cos(((pts[0]?.lat ?? 0) * Math.PI) / 180)
  if (x1 - x0 < min) { const c = (x0 + x1) / 2; x0 = c - min / 2; x1 = c + min / 2 }
  if (y1 - y0 < min) { const c = (y0 + y1) / 2; y0 = c - min / 2; y1 = c + min / 2 }
  const s = Math.min((W - 2 * pad) / (x1 - x0), (H - 2 * pad) / (y1 - y0))
  const cx = (x0 + x1) / 2, cy = (y0 + y1) / 2
  const midLat = latOfW(cy)
  return {
    W, H, s,
    x: p => (wx(p.lon) - cx) * s + W / 2,
    y: p => (wy(p.lat) - cy) * s + H / 2,
    lonAt: sx => lonOfW((sx - W / 2) / s + cx),
    latAt: sy => latOfW((sy - H / 2) / s + cy),
    mpp: (40_075_016.686 * Math.cos((midLat * Math.PI) / 180)) / (256 * s),
  }
}

/** Esri tiles covering the view, at the zoom that is at least as sharp as the screen. */
function tiles(v: View, ratio: number, theme: KbTheme) {
  const src = TILES[theme]
  let z = Math.min(src.maxzoom, Math.max(2, Math.ceil(Math.log2(v.s * ratio))))
  for (;;) {
    const n = 2 ** z
    const t = 256 / n
    const cx = (v.lonAt(0) + 180) / 360 * 256
    const x0 = Math.floor(cx / t)
    const x1 = Math.floor(((v.lonAt(v.W) + 180) / 360 * 256) / t)
    const y0 = Math.floor(wy(v.latAt(0)) / t)
    const y1 = Math.floor(wy(v.latAt(v.H)) / t)
    if ((x1 - x0 + 1) * (y1 - y0 + 1) <= 140 || z <= 2) {
      const out: { href: string; x: number; y: number; size: number; names?: boolean }[] = []
      const nz = Math.min(z, 13)
      const size = t * v.s
      for (let tx = x0; tx <= x1; tx++) {
        for (let ty = y0; ty <= y1; ty++) {
          if (ty < 0 || ty >= n) continue
          const wrap = ((tx % n) + n) % n
          const at = { x: v.x({ lat: 0, lon: lonOfW(tx * t) }), y: v.y({ lat: latOfW(ty * t), lon: 0 }), size }
          out.push({ href: `${src.base}/${z}/${ty}/${wrap}`, ...at })
          // the place names, on top of the wash (no closer than 13: they stop there)
          if (src.names && nz === z) out.push({ href: `${src.names}/${z}/${ty}/${wrap}`, names: true, ...at })
        }
      }
      return out
    }
    z--
  }
}

// ─── labels that keep out of each other's way ─────────────────────────────

interface Box { x: number; y: number; w: number; h: number }
const hit = (a: Box, b: Box) => a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h

class Labeler {
  boxes: Box[] = []
  W: number
  H: number
  constructor(W: number, H: number) { this.W = W; this.H = H }
  reserve(b: Box) { this.boxes.push(b) }
  /** The first candidate box that is on the chart and clear of the rest. */
  place(cands: Box[]): Box | null {
    for (const b of cands) {
      if (b.x < 2 || b.y < 2 || b.x + b.w > this.W - 2 || b.y + b.h > this.H - 2) continue
      if (this.boxes.some(o => hit(o, b))) continue
      this.boxes.push(b)
      return b
    }
    return null
  }
}

const textW = (s: string, size: number, mono = false) => s.length * size * (mono ? 0.6 : 0.56)

interface Label {
  /** anchor (the symbol) */
  x: number
  y: number
  /** half the symbol's size: the label sits clear of it */
  r: number
  lines: { text: string; size: number; mono?: boolean; weight?: number; color: string }[]
}

/**
 * Where a label may go, best first: hard against its symbol, then further
 * out in a crowd (drawn with a leader line back to the symbol).
 */
function labelCandidates(l: Label): Box[] {
  const w = Math.max(...l.lines.map(t => textW(t.text, t.size, t.mono)))
  const h = l.lines.reduce((s, t) => s + t.size * 1.2, 0)
  const out: Box[] = []
  for (const extra of [0, 22, 44, 70]) {
    const g = l.r + 4 + extra
    const d = extra * 0.7
    out.push(
      { x: l.x + g, y: l.y - h / 2, w, h },
      { x: l.x - g - w, y: l.y - h / 2, w, h },
      { x: l.x - w / 2, y: l.y - g - h, w, h },
      { x: l.x - w / 2, y: l.y + g, w, h },
      { x: l.x + g - d, y: l.y - h - 2 - d, w, h },
      { x: l.x + g - d, y: l.y + 2 + d, w, h },
      { x: l.x - g - w + d, y: l.y - h - 2 - d, w, h },
      { x: l.x - g - w + d, y: l.y + 2 + d, w, h },
    )
  }
  return out
}

function LabelText({ l, b, ink }: { l: Label; b: Box; ink: Ink }) {
  const ys = l.lines.map((_, i) => b.y + l.lines.slice(0, i + 1).reduce((s, t) => s + t.size * 1.2, 0))
  // a label pushed out of a crowd points back at its symbol
  const nx = Math.max(b.x, Math.min(l.x, b.x + b.w)), ny = Math.max(b.y, Math.min(l.y, b.y + b.h))
  const dist = Math.hypot(nx - l.x, ny - l.y)
  const far = dist > l.r + 12
  const sx = l.x + ((nx - l.x) / (dist || 1)) * l.r, sy = l.y + ((ny - l.y) / (dist || 1)) * l.r
  return (
    <g>
      {far && <line x1={sx} y1={sy} x2={nx} y2={ny} stroke={l.lines[0].color} strokeWidth={0.8} strokeOpacity={0.8} />}
      {l.lines.map((t, i) => {
        const y = ys[i]
        return (
          <text
            key={i} x={b.x} y={y - t.size * 0.25} fontSize={t.size} fontFamily={t.mono ? KB_MONO : KB_SANS} fontWeight={t.weight ?? 500}
            fill={t.color} stroke={ink.halo} strokeWidth={3} strokeLinejoin="round" paintOrder="stroke"
          >
            {t.text}
          </text>
        )
      })}
    </g>
  )
}

// ─── the chart ─────────────────────────────────────────────────────────────

const ringPath = (v: View, ring: [number, number][]) =>
  ring.map(([lon, lat], i) => `${i ? 'L' : 'M'}${v.x({ lat, lon }).toFixed(1)},${v.y({ lat, lon }).toFixed(1)}`).join('') + 'Z'
const linePath = (v: View, ring: [number, number][]) =>
  ring.map(([lon, lat], i) => `${i ? 'L' : 'M'}${v.x({ lat, lon }).toFixed(1)},${v.y({ lat, lon }).toFixed(1)}`).join('')

function minutes(d: number, pos: string, neg: string, w: number) {
  const a = Math.abs(d)
  let deg = Math.floor(a)
  let min = Math.round((a - deg) * 60)
  if (min === 60) { deg += 1; min = 0 }
  return `${String(deg).padStart(w, '0')}°${String(min).padStart(2, '0')}′${d >= 0 ? pos : neg}`
}

/** A grid step in minutes giving no more than `max` lines over `spanMin`. */
function gridStep(spanMin: number, max = 6) {
  for (const s of [1, 2, 5, 10, 15, 20, 30, 60, 120, 300, 600]) if (spanMin / s <= max) return s
  return 1200
}

function Ship({ x, y, hdg, len, color, ink }: { x: number; y: number; hdg: number; len: number; color: string; ink: Ink }) {
  const w = len * 0.28
  return (
    <g transform={`translate(${x.toFixed(1)},${y.toFixed(1)}) rotate(${hdg})`}>
      <path d={`M0,${-len / 2} L${w / 2},${-len / 4} L${w / 2},${len / 2} L${-w / 2},${len / 2} L${-w / 2},${-len / 4}Z`} fill={color} stroke={ink.halo} strokeWidth={1.2} />
      <path d={`M0,${-len / 2 - 3} L0,${-len / 2 - 11}`} stroke={color} strokeWidth={1.6} strokeLinecap="round" />
    </g>
  )
}

export interface ChartProps {
  live: RangeLive
  theme: KbTheme
  width: number
  height: number
  /** device px per chart unit the tiles are picked for (2 for the PNG) */
  ratio?: number
  /** the key along the bottom */
  legend?: boolean
}

/** The chart's contents, for an `<svg>` of `width` × `height` units. */
export function ChartBody({ live, theme, width: W, height: H, ratio = 1, legend = true }: ChartProps) {
  const ink = KB_INK[theme]
  const KEY_H = legend ? 46 : 0
  const MH = H - KEY_H
  const sectors = drawSectors(live.sectors)
  const sams = (live.iads ?? []).flatMap(n => n.sites.map(s => ({ n, s })))
  const tankers = live.tankers.filter(t => t.state === 'on_station')
  const pts: LatLon[] = [
    ...live.stations.map(s => s.pos),
    ...sams.map(x => x.s.pos),
    ...live.carriers.map(c => c.pos),
    ...(live.ship_decks ?? []).map(d => d.pos),
    ...(live.jammers ?? []).map(j => j.pos),
    ...sectors.flatMap(d => d.ring.map(([lon, lat]) => ({ lat, lon }))),
  ]
  if (!pts.length) pts.push({ lat: 13.44, lon: 144.79 })
  const v = fit(pts, W, MH, 26)
  const L = new Labeler(W, MH)
  const out: ReactNode[] = []
  const labels: { l: Label; b: Box }[] = []
  const symbols: ReactNode[] = []

  // reserve every symbol first so no label lands on one
  const mark = (x: number, y: number, r: number) => L.reserve({ x: x - r, y: y - r, w: 2 * r, h: 2 * r })
  const label = (l: Label) => {
    const b = L.place(labelCandidates(l))
    if (b) labels.push({ l, b })
  }

  // the grid
  const lon0 = v.lonAt(0), lon1 = v.lonAt(W), lat1 = v.latAt(0), lat0 = v.latAt(MH)
  const gLon = gridStep((lon1 - lon0) * 60)
  const gLat = gridStep((lat1 - lat0) * 60, Math.max(3, Math.round(6 * MH / W)))
  const grid: ReactNode[] = []
  for (let m = Math.ceil((lon0 * 60) / gLon) * gLon; m <= lon1 * 60; m += gLon) {
    const x = v.x({ lat: 0, lon: m / 60 })
    grid.push(<line key={`x${m}`} x1={x} x2={x} y1={0} y2={MH} stroke={ink.grid} strokeWidth={0.8} />)
    grid.push(
      <text key={`xt${m}`} x={x + 3} y={11} fontSize={8.5} fontFamily={KB_MONO} fill={ink.dim} stroke={ink.halo} strokeWidth={2.5} paintOrder="stroke">
        {minutes(m / 60, 'E', 'W', 3)}
      </text>,
    )
  }
  for (let m = Math.ceil((lat0 * 60) / gLat) * gLat; m <= lat1 * 60; m += gLat) {
    const y = v.y({ lat: m / 60, lon: 0 })
    grid.push(<line key={`y${m}`} x1={0} x2={W} y1={y} y2={y} stroke={ink.grid} strokeWidth={0.8} />)
    grid.push(
      <text key={`yt${m}`} x={4} y={y - 3} fontSize={8.5} fontFamily={KB_MONO} fill={ink.dim} stroke={ink.halo} strokeWidth={2.5} paintOrder="stroke">
        {minutes(m / 60, 'N', 'S', 2)}
      </text>,
    )
  }
  L.reserve({ x: 0, y: 0, w: W, h: 14 })
  L.reserve({ x: 0, y: 0, w: 58, h: MH })

  // north arrow and scale bar, reserved before anything is labelled
  L.reserve({ x: W - 40, y: 16, w: 34, h: 46 })
  const target = (W * 0.22 * v.mpp) / NM
  const nm = [0.5, 1, 2, 5, 10, 20, 25, 50, 100, 200].reduce((a, b) => (Math.abs(b - target) < Math.abs(a - target) ? b : a))
  const barPx = (nm * NM) / v.mpp
  L.reserve({ x: 10, y: MH - 34, w: barPx + 40, h: 30 })

  // sectors: outline and a faint fill in the F10 colour, name at the top
  for (const d of sectors) {
    const c = sectorInk(d.info.color, theme)
    const dashed = d.info.dashed || d.info.route
    out.push(
      <path
        key={`s-${d.sector.id}`} d={ringPath(v, d.ring)} fill={c} fillOpacity={d.info.route ? 0.04 : theme === 'light' ? 0.07 : 0.1}
        stroke={c} strokeWidth={d.info.route ? 0.8 : 1.4} strokeOpacity={d.info.route ? 0.6 : 0.95} strokeDasharray={dashed ? '5 3' : undefined}
      />,
    )
    if (d.centreline) out.push(<path key={`c-${d.sector.id}`} d={linePath(v, d.centreline)} fill="none" stroke={c} strokeWidth={2} />)
  }

  // SAM engagement rings and jammer circles
  for (const { n, s } of sams) {
    const dead = s.units_alive === 0
    const c = dead ? ink.dim : ink.red
    const x = v.x(s.pos), y = v.y(s.pos)
    if (s.range_m > 0 && !dead) {
      out.push(<circle key={`r-${n.id}-${s.id}`} cx={x} cy={y} r={s.range_m / v.mpp} fill={c} fillOpacity={0.05} stroke={c} strokeWidth={1.2} strokeDasharray="6 3" />)
    }
    mark(x, y, 6)
    symbols.push(
      <path key={`sam-${n.id}-${s.id}`} d={`M${x},${y - 6} L${x + 6},${y + 5} L${x - 6},${y + 5}Z`} fill={dead ? 'none' : c} stroke={dead ? c : ink.halo} strokeWidth={1.3} />,
    )
  }
  for (const j of live.jammers ?? []) {
    const x = v.x(j.pos), y = v.y(j.pos)
    const c = j.alive ? ink.teal : ink.dim
    if (j.alive) out.push(<circle key={`jr-${j.id}`} cx={x} cy={y} r={j.radius_m / v.mpp} fill="none" stroke={c} strokeWidth={1.2} strokeDasharray="2 3" />)
    mark(x, y, 6)
    symbols.push(<rect key={`j-${j.id}`} x={x - 4.5} y={y - 4.5} width={9} height={9} transform={`rotate(45 ${x} ${y})`} fill={c} stroke={ink.halo} strokeWidth={1.2} />)
  }

  // targets
  for (const s of live.stations) {
    const x = v.x(s.pos), y = v.y(s.pos)
    mark(x, y, 6)
    symbols.push(
      <g key={`st-${s.id}`}>
        <rect x={x - 5.5} y={y - 5.5} width={11} height={11} fill={ink.halo} fillOpacity={0.7} stroke={ink.ink} strokeWidth={1.6} />
        <circle cx={x} cy={y} r={1.8} fill={ink.ink} />
      </g>,
    )
  }

  // ships and tankers
  for (const c of live.carriers) {
    const x = v.x(c.pos), y = v.y(c.pos)
    mark(x, y, 9)
    symbols.push(<Ship key={`cv-${c.id}`} x={x} y={y} hdg={c.brc_deg} len={18} color={ink.sea} ink={ink} />)
  }
  for (const d of live.ship_decks ?? []) {
    const x = v.x(d.pos), y = v.y(d.pos)
    mark(x, y, 6)
    symbols.push(<Ship key={`sd-${d.id}`} x={x} y={y} hdg={d.heading_deg} len={11} color={ink.sea} ink={ink} />)
  }
  for (const t of tankers) {
    const x = v.x(t.pos), y = v.y(t.pos)
    mark(x, y, 6)
    symbols.push(
      <g key={`tk-${t.id}`}>
        <circle cx={x} cy={y} r={5.5} fill={ink.halo} stroke={ink.amber} strokeWidth={1.6} />
        <text x={x} y={y + 3} fontSize={7.5} fontWeight={700} textAnchor="middle" fontFamily={KB_SANS} fill={ink.amber}>T</text>
      </g>,
    )
  }

  // labels: the things you fly to first, the sector names last
  // a range's targets share its name ("TN-2 Tinian MLA - Easy", "... - Medium"):
  // one label for the range, its targets listed under it
  const groups = new Map<string, LiveStation[]>()
  for (const s of live.stations) {
    const i = s.name.indexOf(' - ')
    const k = i > 0 ? s.name.slice(0, i) : s.name
    groups.set(k, [...(groups.get(k) ?? []), s])
  }
  for (const [name, ss] of groups) {
    if (ss.length === 1) {
      const s = ss[0]
      const second = [s.laser_code != null ? `LSR ${s.laser_code}` : null, s.elev_m != null ? `${fmt(s.elev_m / 0.3048)} ft` : null].filter(Boolean).join(' · ')
      label({
        x: v.x(s.pos), y: v.y(s.pos), r: 6,
        lines: [{ text: s.name, size: 10, weight: 700, color: ink.ink }, ...(second ? [{ text: second, size: 8.5, mono: true, color: ink.dim }] : [])],
      })
      continue
    }
    const xs = ss.map(s => v.x(s.pos)), ys = ss.map(s => v.y(s.pos))
    const [x0, x1, y0, y1] = [Math.min(...xs), Math.max(...xs), Math.min(...ys), Math.max(...ys)]
    const parts = ss.map(s => {
      const part = s.name.slice(name.length + 3) || s.name
      return s.laser_code != null ? `${part} (${s.laser_code})` : part
    })
    const wrapped: string[] = []
    for (const p of parts) {
      const last = wrapped.length - 1
      if (last >= 0 && wrapped[last].length + p.length + 3 <= 40) wrapped[last] += ` · ${p}`
      else wrapped.push(p)
    }
    label({
      x: (x0 + x1) / 2, y: (y0 + y1) / 2, r: 6 + Math.max(x1 - x0, y1 - y0) / 2,
      lines: [{ text: name, size: 10, weight: 700, color: ink.ink }, ...wrapped.map(text => ({ text, size: 8.5, mono: true, color: ink.dim }))],
    })
  }
  for (const c of live.carriers) {
    const second = [c.tacan ? `TCN ${c.tacan}` : null, c.icls != null ? `ICLS ${c.icls}` : null, c.tower_mhz != null ? c.tower_mhz.toFixed(3) : null].filter(Boolean).join(' · ')
    label({
      x: v.x(c.pos), y: v.y(c.pos), r: 9,
      lines: [{ text: `${c.name} · BRC ${pad3(c.brc_deg)}`, size: 10, weight: 700, color: ink.sea }, ...(second ? [{ text: second, size: 8.5, mono: true, color: ink.dim }] : [])],
    })
  }
  for (const d of live.ship_decks ?? []) {
    label({ x: v.x(d.pos), y: v.y(d.pos), r: 6, lines: [{ text: d.name, size: 9, weight: 600, color: ink.sea }, ...(d.tacan ? [{ text: `TCN ${d.tacan}`, size: 8, mono: true, color: ink.dim }] : [])] })
  }
  for (const t of tankers) {
    label({
      x: v.x(t.pos), y: v.y(t.pos), r: 6,
      lines: [
        { text: t.callsign, size: 9.5, weight: 700, color: ink.amber },
        { text: [t.tacan, t.freq_mhz.toFixed(3), `FL${Math.round(t.alt_ft / 100)}`].filter(Boolean).join(' · '), size: 8, mono: true, color: ink.dim },
      ],
    })
  }
  for (const { n, s } of sams) {
    const dead = s.units_alive === 0
    label({
      x: v.x(s.pos), y: v.y(s.pos), r: 6,
      lines: [
        { text: s.name, size: 9, weight: 700, color: dead ? ink.dim : ink.red },
        { text: `${s.system}${s.range_m > 0 ? ` · ${fmt(s.range_m / NM)} nm` : ''}${dead ? ' · dead' : ''}${n.weapons_free ? '' : ' · hold'}`, size: 8, mono: true, color: ink.dim },
      ],
    })
  }
  for (const j of live.jammers ?? []) {
    label({ x: v.x(j.pos), y: v.y(j.pos), r: 6, lines: [{ text: j.name, size: 9, weight: 700, color: j.alive ? ink.teal : ink.dim }, { text: `GPS ${j.gps} · ${fmt(j.radius_m / NM)} nm`, size: 8, mono: true, color: ink.dim }] })
  }
  for (const d of sectors) {
    const x = v.x(d.label), y = v.y(d.label)
    const text = d.sector.name
    const size = 9
    const w = textW(text, size) + 2, h = size * 1.2
    const b = L.place([
      { x: x - w / 2, y: y - h / 2, w, h },
      { x: x - w / 2, y: y + h, w, h },
      { x: x - w / 2, y: y - 2 * h, w, h },
    ])
    if (b) labels.push({ l: { x, y, r: 0, lines: [{ text, size, weight: 700, color: sectorInk(d.info.color, theme) }] }, b })
  }

  // the key: the sector kinds on this chart, then the symbols
  const kinds = SECTOR_KIND_ORDER.filter(k => sectors.some(d => d.sector.kind === k))
  const extra = [...new Set(sectors.map(d => d.sector.kind).filter(k => !kinds.includes(k as SectorKind)))]
  const key: { draw: (x: number, y: number) => ReactNode; text: string }[] = [
    ...[...kinds, ...extra].map(k => {
      const info = kindInfo(k)
      const c = sectorInk(info.color, theme)
      return {
        text: info.label,
        draw: (x: number, y: number) => <rect x={x} y={y - 7} width={14} height={8} fill={c} fillOpacity={0.15} stroke={c} strokeWidth={1.2} strokeDasharray={info.dashed || info.route ? '3 2' : undefined} />,
      }
    }),
    ...(live.stations.length ? [{ text: 'TARGET', draw: (x: number, y: number) => <rect x={x + 2} y={y - 8} width={9} height={9} fill="none" stroke={ink.ink} strokeWidth={1.5} /> }] : []),
    ...(sams.length ? [{ text: 'SAM · RING = RANGE', draw: (x: number, y: number) => <path d={`M${x + 7},${y - 9} L${x + 13},${y + 1} L${x + 1},${y + 1}Z`} fill={ink.red} /> }] : []),
    ...((live.jammers ?? []).length ? [{ text: 'JAMMER', draw: (x: number, y: number) => <rect x={x + 4} y={y - 7} width={7} height={7} transform={`rotate(45 ${x + 7.5} ${y - 3.5})`} fill={ink.teal} /> }] : []),
    ...(live.carriers.length + (live.ship_decks ?? []).length ? [{ text: 'SHIP', draw: (x: number, y: number) => <Ship x={x + 7} y={y - 3} hdg={90} len={13} color={ink.sea} ink={ink} /> }] : []),
    ...(tankers.length ? [{ text: 'TANKER', draw: (x: number, y: number) => <circle cx={x + 7} cy={y - 3.5} r={5} fill="none" stroke={ink.amber} strokeWidth={1.5} /> }] : []),
  ]
  const keyItems: ReactNode[] = []
  if (legend) {
    let kx = 10, ky = MH + 18
    for (const [i, k] of key.entries()) {
      const w = 20 + textW(k.text, 8) + 14
      if (kx + w > W - 6) { kx = 10; ky += 16 }
      if (ky > H - 4) break
      keyItems.push(
        <g key={i}>
          {k.draw(kx, ky)}
          <text x={kx + 19} y={ky} fontSize={8} fontWeight={600} letterSpacing={0.4} fontFamily={KB_SANS} fill={ink.dim}>{k.text}</text>
        </g>,
      )
      kx += w
    }
  }

  const clip = `kb-clip-${theme}-${W}x${H}`
  const base = tiles(v, ratio, theme)
  return (
    <g fontFamily={KB_SANS}>
      <defs>
        <clipPath id={clip}><rect x={0} y={0} width={W} height={MH} /></clipPath>
      </defs>
      <rect x={0} y={0} width={W} height={H} fill={ink.paper} />
      <g clipPath={`url(#${clip})`}>
        {base.filter(t => !t.names).map(t => (
          <image key={t.href} href={t.href} x={t.x} y={t.y} width={t.size + 0.5} height={t.size + 0.5} preserveAspectRatio="none" />
        ))}
        <rect x={0} y={0} width={W} height={MH} fill={ink.wash} />
        {base.filter(t => t.names).map(t => (
          <image key={t.href} href={t.href} x={t.x} y={t.y} width={t.size + 0.5} height={t.size + 0.5} preserveAspectRatio="none" opacity={0.85} />
        ))}
        {grid}
        {out}
        {symbols}
        {labels.map(({ l, b }, i) => <LabelText key={i} l={l} b={b} ink={ink} />)}
        {/* north */}
        <g transform={`translate(${W - 23},${22})`}>
          <path d="M0,0 L7,20 L0,15 L-7,20Z" fill={ink.ink} stroke={ink.halo} strokeWidth={1.2} />
          <text x={0} y={34} fontSize={10} fontWeight={700} textAnchor="middle" fill={ink.ink} stroke={ink.halo} strokeWidth={3} paintOrder="stroke">N</text>
        </g>
        {/* scale */}
        <g transform={`translate(14,${MH - 16})`}>
          {[0, 1, 2, 3].map(i => (
            <rect key={i} x={(i * barPx) / 4} y={0} width={barPx / 4} height={5} fill={i % 2 ? ink.halo : ink.ink} stroke={ink.ink} strokeWidth={0.8} />
          ))}
          <text x={0} y={-4} fontSize={8.5} fontFamily={KB_MONO} fill={ink.ink} stroke={ink.halo} strokeWidth={2.5} paintOrder="stroke">0</text>
          <text x={barPx} y={-4} fontSize={8.5} fontFamily={KB_MONO} textAnchor="end" fill={ink.ink} stroke={ink.halo} strokeWidth={2.5} paintOrder="stroke">{nm} NM</text>
        </g>
        <text x={W - 4} y={MH - 4} fontSize={6.5} textAnchor="end" fill={ink.dim} opacity={0.8}>{theme === 'dark' ? 'Imagery Esri, Maxar, Earthstar Geographics' : 'Basemap Esri, GEBCO, NOAA, Garmin, HERE'}</text>
      </g>
      <rect x={0.5} y={0.5} width={W - 1} height={MH - 1} fill="none" stroke={ink.ink} strokeWidth={1} />
      {keyItems}
    </g>
  )
}

/** The chart on its own, scaling to its container. */
export function KneeboardChart(p: ChartProps & { className?: string }) {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg" viewBox={`0 0 ${p.width} ${p.height}`} className={p.className}
      role="img" aria-label={`Range chart, ${p.theme} kneeboard`} style={{ display: 'block', width: '100%', height: 'auto' }}
    >
      <ChartBody {...p} />
    </svg>
  )
}
