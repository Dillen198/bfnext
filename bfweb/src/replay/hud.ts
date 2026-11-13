// The cockpit camera's HUD: a generic head-up display drawn over the 3D view,
// not any one type's real symbology. Conformal where a HUD is conformal --
// the horizon, the pitch ladder and the flight path marker are projected
// through the same camera as the scene, so they sit on the real horizon and
// on where the aircraft is going -- and fixed where a HUD is fixed: the
// heading tape, the speed and altitude boxes, the bank scale.
//
// Speed is the recorded IAS when the recording has it, else an estimate
// from ground speed (marked GS); G is worked out from the track (as the
// flight graph does). Drawn on a 2D canvas each frame by the theatre.

import type { State } from './data'

const DEG = Math.PI / 180
const M_TO_FT = 3.28084
const MS_TO_KT = 1.943844
const M_TO_NM = 1 / 1852

export type Vec3 = [number, number, number]
/** A world direction (east, north, up) -> screen px, or null behind the camera. */
export type Project = (dir: Vec3) => [number, number] | null

export interface HudData {
  st: State
  /** indicated airspeed, m/s (recorded or estimated) */
  ias: number
  iasRecorded: boolean
  mach: number
  g: number | null
  /** velocity east/north/up, m/s, from the track (null: not loaded yet) */
  vel: Vec3 | null
  /** height above the ground, m */
  agl: number | null
  /** the measured object, when there is one: what to call it and how far */
  target: { label: string; range: number } | null
}

// ── pure geometry (tested) ──

/** Unit direction of a heading and an elevation, east/north/up. */
export function dirOf(hdgDeg: number, elDeg: number): Vec3 {
  const h = hdgDeg * DEG, e = elDeg * DEG
  return [Math.sin(h) * Math.cos(e), Math.cos(h) * Math.cos(e), Math.sin(e)]
}

/** Where the aircraft is going: the velocity, or (until the track is in)
 *  the nose heading with the climb angle from the vertical speed. */
export function flightPathDir(st: State, vel: Vec3 | null): Vec3 {
  if (vel && Math.hypot(vel[0], vel[1], vel[2]) > 5) {
    const n = Math.hypot(vel[0], vel[1], vel[2])
    return [vel[0] / n, vel[1] / n, vel[2] / n]
  }
  return dirOf(st.hdg, Math.atan2(st.vs, Math.max(1, st.gs)) / DEG)
}

/** The pitch ladder's rungs: every 5 degrees, labelled every 10 (and at 5 near the horizon). */
export function ladderRungs(): { el: number; label: string | null }[] {
  const out = []
  for (let el = -85; el <= 85; el += 5) {
    if (el === 0) continue
    out.push({ el, label: Math.abs(el) % 10 === 0 || Math.abs(el) === 5 ? String(Math.abs(el)) : null })
  }
  return out
}

/** Heading tape marks within `half` degrees of the heading: offset from the
 *  centre in degrees, and the label (36, 01, 02 ...) on the tens. */
export function headingMarks(hdg: number, half = 20, step = 5): { off: number; label: string | null; major: boolean }[] {
  const out = []
  const first = Math.ceil((hdg - half) / step) * step
  for (let d = first; d <= hdg + half + 1e-9; d += step) {
    const v = ((d % 360) + 360) % 360
    const major = v % 10 === 0
    out.push({ off: d - hdg, label: major ? String((v === 0 ? 360 : v) / 10).padStart(2, '0') : null, major })
  }
  return out
}

/** Bank scale ticks, degrees either side of wings level. */
export const BANK_TICKS = [0, 10, 20, 30, 45, 60]

// ── drawing ──

const INK = '#41ff7e'
const SHADOW = 'rgba(0, 0, 0, 0.55)'
const MONO = "'Share Tech Mono', Consolas, 'Courier New', monospace"

export function drawHud(ctx: CanvasRenderingContext2D, w: number, h: number, d: HudData, project: Project, targetAt: [number, number] | null) {
  ctx.clearRect(0, 0, w, h)
  const st = d.st
  const cx = w / 2, cy = h / 2
  // the combiner: the glass the symbology is drawn on
  // (on a phone, held upright, nearly the whole width and a taller glass)
  const narrow = w < 600
  const cw = narrow ? w * 0.94 : Math.min(w * 0.72, h * 1.05)
  const ch = narrow ? Math.min(h * 0.72, cw * 1.35) : Math.min(h * 0.86, cw * 0.95)
  const left = cx - cw / 2, top = cy - ch / 2
  const small = cw < 420
  const fs = small ? 12 : 13

  ctx.save()
  ctx.lineWidth = small ? 1.4 : 1.7
  ctx.strokeStyle = INK
  ctx.fillStyle = INK
  ctx.shadowColor = SHADOW
  ctx.shadowBlur = 3
  ctx.font = `600 ${fs}px ${MONO}`
  ctx.textBaseline = 'middle'

  // ── conformal: horizon, pitch ladder, flight path marker ──
  ctx.save()
  ctx.beginPath()
  ctx.rect(left, top + 34, cw, ch - 70)
  ctx.clip()
  const hd = st.hdg
  const right: Vec3 = [Math.cos(hd * DEG), -Math.sin(hd * DEG), 0]
  const along = (c: Vec3, k: number): Vec3 => [c[0] + right[0] * k, c[1] + right[1] * k, c[2] + right[2] * k]
  const seg = (a: Vec3, b: Vec3, dash: number[] = []) => {
    const p = project(a), q = project(b)
    if (!p || !q) return null
    ctx.setLineDash(dash)
    ctx.beginPath()
    ctx.moveTo(p[0], p[1])
    ctx.lineTo(q[0], q[1])
    ctx.stroke()
    ctx.setLineDash([])
    return [p, q] as const
  }
  // the horizon, across the glass with a gap at the nose
  const hz = dirOf(hd, 0)
  seg(along(hz, -Math.tan(40 * DEG)), along(hz, -Math.tan(3 * DEG)))
  seg(along(hz, Math.tan(3 * DEG)), along(hz, Math.tan(40 * DEG)))
  // the rungs: solid above the horizon, dashed below, ends bent toward the horizon
  const wide = Math.tan(5.5 * DEG), gap = Math.tan(2 * DEG)
  for (const r of ladderRungs()) {
    if (Math.abs(r.el - st.pitch) > 32) continue
    const c = dirOf(hd, r.el)
    const dash = r.el < 0 ? [6, 4] : []
    for (const side of [-1, 1]) {
      const pq = seg(along(c, side * gap), along(c, side * wide), dash)
      if (!pq) continue
      const [, q] = pq
      // the bent tip, pointing at the horizon
      const tip = project([c[0] + right[0] * side * wide, c[1] + right[1] * side * wide, c[2] - Math.sign(r.el) * 0.012])
      if (tip) {
        ctx.beginPath()
        ctx.moveTo(q[0], q[1])
        ctx.lineTo(tip[0], tip[1])
        ctx.stroke()
      }
      if (r.label) {
        const lp = project(along(c, side * (wide + 0.025)))
        if (lp) {
          ctx.textAlign = 'center'
          ctx.fillText(r.label, lp[0], lp[1])
        }
      }
    }
  }
  // the flight path marker
  const fp = project(flightPathDir(st, d.vel))
  if (fp) {
    const r = small ? 6 : 7
    ctx.beginPath()
    ctx.arc(fp[0], fp[1], r, 0, Math.PI * 2)
    ctx.moveTo(fp[0] - r, fp[1]); ctx.lineTo(fp[0] - r * 2.6, fp[1])
    ctx.moveTo(fp[0] + r, fp[1]); ctx.lineTo(fp[0] + r * 2.6, fp[1])
    ctx.moveTo(fp[0], fp[1] - r); ctx.lineTo(fp[0], fp[1] - r * 2)
    ctx.stroke()
  }
  ctx.restore()

  // the waterline: where the nose points (the camera looks down the nose)
  ctx.beginPath()
  ctx.moveTo(cx - 22, cy); ctx.lineTo(cx - 11, cy); ctx.lineTo(cx - 5.5, cy + 6); ctx.lineTo(cx, cy)
  ctx.lineTo(cx + 5.5, cy + 6); ctx.lineTo(cx + 11, cy); ctx.lineTo(cx + 22, cy)
  ctx.stroke()

  // ── heading tape ──
  const tapeW = Math.min(cw * 0.6, 340)
  const ty = top + 16
  const pxDeg = tapeW / 40
  ctx.textAlign = 'center'
  for (const m of headingMarks(hd)) {
    const x = cx + m.off * pxDeg
    ctx.beginPath()
    ctx.moveTo(x, ty + 12)
    ctx.lineTo(x, ty + (m.major ? 4 : 8))
    ctx.stroke()
    if (m.label && Math.abs(m.off) > 3.5) ctx.fillText(m.label, x, ty - 4)
  }
  const hdgTxt = String(Math.round(((hd % 360) + 360) % 360) || 360).padStart(3, '0')
  boxed(ctx, cx, ty - 4, hdgTxt, fs)
  ctx.beginPath()
  ctx.moveTo(cx - 5, ty + 18); ctx.lineTo(cx, ty + 13); ctx.lineTo(cx + 5, ty + 18)
  ctx.stroke()

  // ── speed (left) and altitude (right) ──
  const sx = left + (small ? 34 : 50), ax = left + cw - (small ? 34 : 50)
  const kt = Math.round(d.ias * MS_TO_KT)
  ctx.textAlign = 'center'
  ctx.fillText(d.iasRecorded ? 'IAS' : 'GS', sx, cy - 26)
  boxed(ctx, sx, cy, String(kt), fs + 2, 64)
  const alt = Math.round(st.alt * M_TO_FT)
  ctx.fillText('ALT', ax, cy - 26)
  boxed(ctx, ax, cy, alt.toLocaleString('en-US'), fs + 2, 74)
  // under the speed: Mach, G, AoA
  const line = (x: number, y: number, txt: string) => { ctx.fillText(txt, x, y) }
  line(sx, cy + 28, `M ${d.mach.toFixed(2)}`)
  if (d.g != null) line(sx, cy + 46, `G ${d.g.toFixed(1)}`)
  if (st.aoa != null) line(sx, cy + 64, `α ${st.aoa.toFixed(1)}`)
  // under the altitude: radar altitude, vertical speed
  if (d.agl != null && d.agl < 5000 / M_TO_FT) line(ax, cy + 28, `R ${Math.max(0, Math.round(d.agl * M_TO_FT)).toLocaleString('en-US')}`)
  line(ax, cy + 46, `VS ${st.vs >= 0 ? '+' : ''}${Math.round((st.vs * M_TO_FT * 60) / 10) * 10}`)

  // ── bank scale, along the bottom ──
  const br = Math.min(cw * 0.32, 150)
  const by = top + ch - 26 - br
  for (const t of BANK_TICKS) {
    for (const side of t === 0 ? [1] : [-1, 1]) {
      const a = (90 + side * t) * DEG
      const len = t === 0 || t === 30 || t === 60 ? 9 : 5
      const x0 = cx + Math.cos(a) * br, y0 = by + Math.sin(a) * br
      const x1 = cx + Math.cos(a) * (br + len), y1 = by + Math.sin(a) * (br + len)
      ctx.beginPath(); ctx.moveTo(x0, y0); ctx.lineTo(x1, y1); ctx.stroke()
    }
  }
  const roll = Math.max(-70, Math.min(70, st.roll))
  const pa = (90 - roll) * DEG
  const px = cx + Math.cos(pa) * (br - 2), py = by + Math.sin(pa) * (br - 2)
  const ta = pa + Math.PI / 2
  ctx.beginPath()
  ctx.moveTo(px, py)
  ctx.lineTo(px - Math.cos(pa) * 9 + Math.cos(ta) * 5, py - Math.sin(pa) * 9 + Math.sin(ta) * 5)
  ctx.lineTo(px - Math.cos(pa) * 9 - Math.cos(ta) * 5, py - Math.sin(pa) * 9 - Math.sin(ta) * 5)
  ctx.closePath()
  ctx.fill()

  // ── the measured target: a box on it, or an arrow toward it ──
  if (d.target) {
    const lbl = `${d.target.label} ${(d.target.range * M_TO_NM).toFixed(1)} NM`
    const inside = targetAt && targetAt[0] > left && targetAt[0] < left + cw && targetAt[1] > top && targetAt[1] < top + ch
    if (inside && targetAt) {
      ctx.strokeRect(targetAt[0] - 11, targetAt[1] - 11, 22, 22)
      // the label under the box, kept on the glass; on a phone along the
      // bottom, where it cannot land on the speed or altitude readouts
      const lw = ctx.measureText(lbl).width
      ctx.textAlign = 'center'
      if (narrow) ctx.fillText(lbl, cx, top + ch - 12)
      else ctx.fillText(lbl, Math.min(left + cw - lw / 2 - 4, Math.max(left + lw / 2 + 4, targetAt[0])), targetAt[1] + 24)
    } else {
      // off the glass (or behind): an arrow from the centre toward it
      const tx = targetAt ? targetAt[0] : cx, tyy = targetAt ? targetAt[1] : cy + ch
      const a = Math.atan2(tyy - cy, tx - cx)
      const rr = Math.min(cw, ch) * 0.28
      const x = cx + Math.cos(a) * rr, y = cy + Math.sin(a) * rr
      ctx.beginPath()
      ctx.moveTo(x + Math.cos(a) * 12, y + Math.sin(a) * 12)
      ctx.lineTo(x + Math.cos(a + 2.6) * 9, y + Math.sin(a + 2.6) * 9)
      ctx.lineTo(x + Math.cos(a - 2.6) * 9, y + Math.sin(a - 2.6) * 9)
      ctx.closePath()
      ctx.fill()
      ctx.textAlign = 'center'
      ctx.fillText(lbl, cx, top + ch - 12)
    }
  }
  ctx.restore()
}

/** Text in a box, centred on (x, y). */
function boxed(ctx: CanvasRenderingContext2D, x: number, y: number, txt: string, size: number, minW = 0) {
  ctx.save()
  ctx.font = `700 ${size}px ${MONO}`
  const tw = Math.max(minW, ctx.measureText(txt).width + 14)
  const th = size + 10
  ctx.strokeRect(x - tw / 2, y - th / 2, tw, th)
  ctx.textAlign = 'center'
  ctx.fillText(txt, x, y + 1)
  ctx.restore()
}
