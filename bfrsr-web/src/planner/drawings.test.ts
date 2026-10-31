// The map drawings ported from the jets' own DTC editors, and the JDAM
// handles: what DCS draws, the pilot drags, and the cartridge keeps.
import { describe, expect, it } from 'vitest'
import { AnnotationIndex, normaliseAnnotations, setAt, withOverride, type Json, type JsonObject, type Overrides, type Seg } from './model'
import { fromDcsXY, terrainNamed, toDcsXY } from './nav'
import { buildNative } from './native'
import { lar, larCentre, larPolygon } from './f14lar'
import { capOrbit, corridorEdges, drawings, moveHandle } from './dtc/drawings'
import type { Ctx } from './dtc/core'
import { newPlan } from './types'
import tomcat from './data/native/F-14BU.defaults.json'
import tomcatA from './data/native/F-14BU.annotations.json'
import hornet from './data/native/FA-18C_hornet.defaults.json'
import hornetA from './data/native/FA-18C_hornet.annotations.json'

const map = terrainNamed('Caucasus')!
function cart(defaults: unknown, ann: unknown): Ctx {
  const ctx: Ctx = {
    idx: new AnnotationIndex(normaliseAnnotations(ann as never)), defaults: defaults as Json, overrides: {},
    value: JSON.parse(JSON.stringify(defaults)) as Json, readOnly: false, hidden: new Set<string>(), points: [],
    set: (segs: Seg[], v: Json) => { setAt(ctx.value, segs, v) },
  }
  return ctx
}
const target = (extra: JsonObject = {}): JsonObject => ({ active: true, x: -280000, y: 640000, drop_alt: 25000, drop_spd: 450, attack_heading: 90, ...extra })
const T: Seg[] = ['JDAM', 'stations', 0, 'targets', 0]

describe('map drawings', () => {
  it('draws the CAP racetrack the way the Hornet editor does', () => {
    const o = capOrbit({ x: 0, y: 0 }, 0, 9260, 18520, 'Left')
    const xs = o.map(p => p.x)
    const ys = o.map(p => p.y)
    // Back along the course (south, -x) by the leg, plus the far half circle.
    expect(Math.min(...xs)).toBeCloseTo(-18520 - 4630, 0)
    expect(Math.max(...xs)).toBeCloseTo(4630, 0)
    // A left turn puts the second leg on the left (west, -y).
    expect(Math.min(...ys)).toBeCloseTo(-9260, 0)
    expect(Math.max(...ys)).toBeCloseTo(0, 0)
    const right = capOrbit({ x: 0, y: 0 }, 0, 9260, 18520, 'Right')
    expect(Math.max(...right.map(p => p.y))).toBeCloseTo(9260, 0)
  })

  it('puts corridor edges 5 NM either side, mitred at the turns', () => {
    const [l, r] = corridorEdges([{ x: 0, y: 0 }, { x: 10000, y: 0 }, { x: 10000, y: 10000 }], 9260)
    expect(l[0]).toEqual({ x: 0, y: 9260 })
    expect(r[0]).toEqual({ x: 0, y: -9260 })
    // The outside corner of a right angle sits half-width out on both axes.
    expect(r[1].x).toBeCloseTo(10000 + 9260, 3)
    expect(r[1].y).toBeCloseTo(-9260, 3)
    expect(l[2]).toEqual({ x: 10000 - 9260, y: 10000 })
  })

  it('draws an F-14 JDAM target with its zone, launch point and heading handle', () => {
    const ctx = cart(tomcat, tomcatA)
    ctx.set(T, target())
    const d = drawings(ctx, map, ['JDAM'])
    expect(d.lines).toHaveLength(2)
    expect(d.handles.map(h => [h.kind, h.label, h.here])).toEqual([['launch', 'STA3 T1 LP', true], ['heading', '90°', true]])
    // The handle sits at Rmax on the reciprocal of the attack heading (west).
    const l = lar(25000, 450)
    const hdg = toDcsXY(d.handles[1].lat, d.handles[1].lon, map.terrain)
    expect(hdg.x).toBeCloseTo(-280000, -1)
    expect(hdg.y).toBeCloseTo(640000 - l.lar_rmax_nmi * 1852, -1)
  })

  it('turns the attack and keeps the launch point in the zone when the handles move', () => {
    const ctx = cart(tomcat, tomcatA)
    ctx.set(T, target())
    const [lp, hdg] = drawings(ctx, map, ['JDAM']).handles
    // Drag the heading handle due south of the target: the attack runs north.
    const southLL = fromDcsXY(-300000, 640000, map.terrain)
    const turned = moveHandle(ctx, hdg, southLL.lat, southLL.lon, map)
    expect(turned.attack_heading).toBe(0)
    // A launch point dragged far outside comes back onto the zone's edge.
    const far = moveHandle(ctx, lp, 42.9, 44.5, map)
    const poly = larPolygon(-280000, 640000, 90, lar(25000, 450))
    const onEdge = poly.some((a, i) => {
      const b = poly[(i + 1) % poly.length]
      const cross = (b.x - a.x) * ((far.launch_y as number) - a.y) - (b.y - a.y) * ((far.launch_x as number) - a.x)
      return Math.abs(cross) / Math.hypot(b.x - a.x, b.y - a.y) < 1
    })
    expect(onEdge).toBe(true)
  })

  it('exports the launch point the pilot set, not the zone centre', () => {
    const l = lar(25000, 450)
    const c = larCentre(-280000, 640000, 90, l)
    const mine = { x: c.x + 300, y: c.y - 200 }
    let ov: Overrides = {}
    ov = withOverride(tomcat as Json, ov, T, target({ launch_x: mine.x, launch_y: mine.y }))
    const plan = { ...newPlan('F-14BU'), waypoints: [{ name: 'A', lat: 42.2, lon: 42.4, alt_ft: 20000, elev_ft: 0, target: false, note: '' }] }
    const out = JSON.parse(buildNative(plan, 'F-14BU', tomcat as Json, ov, () => null).text)
    const t = out.data.JDAM.stations[0].targets[0]
    expect(t.launch_x).toBeCloseTo(mine.x, 3)
    expect(t.launch_y).toBeCloseTo(mine.y, 3)
  })

  it('rings Hornet MEZ threats and draws CAP orbits', () => {
    const ctx = cart(hornet, hornetA)
    ctx.set(['SA', 'MEZ_THRTS'], [{ id: 'MEZ_THRTS_1', num: 1, x: -280000, y: 640000, text: 'SA10', threat_type: 'Custom', threat_ring_radius: 20, threat_level: 1 }])
    ctx.set(['SA', 'CAP_PTS'], [{ id: 'CAP_PTS_1', num: 1, x: -270000, y: 630000, course: 90, length: 18520, diameter: 9260, turn_direction: 'Right', note: '' }])
    const d = drawings(ctx, map, ['SA'])
    expect(d.labels.map(l => l.text).sort()).toEqual(['1 SA10', 'CAP 1'])
    expect(d.lines).toHaveLength(2)
  })
})
