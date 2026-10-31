// Lines and areas on the cartridge map: found in every aircraft that has
// them, drawn as lines, and edited with the ids the jets expect.
import { describe, expect, it } from 'vitest'
import { AnnotationIndex, getAt, normaliseAnnotations, setAt, type Json, type JsonObject, type Seg } from './model'
import { terrainNamed } from './nav'
import { addPoint, addRect, removePoint, shapeLists, shapesOf } from './dtc/shapes'
import { addableLists, positions } from './dtc/places'
import type { Ctx } from './dtc/core'
import f16 from './data/native/F-16C_50.defaults.json'
import f16a from './data/native/F-16C_50.annotations.json'
import hornet from './data/native/FA-18C_hornet.defaults.json'
import hornetA from './data/native/FA-18C_hornet.annotations.json'
import apache from './data/native/AH-64D_BLK_II.defaults.json'
import apacheA from './data/native/AH-64D_BLK_II.annotations.json'
import tomcat from './data/native/F-14BU.defaults.json'
import tomcatA from './data/native/F-14BU.annotations.json'

const map = terrainNamed('Caucasus')!

/** A cartridge whose edits land in its own value, like the workspace's. */
function cart(defaults: unknown, ann: unknown) {
  const ctx: Ctx = {
    idx: new AnnotationIndex(normaliseAnnotations(ann as never)), defaults: defaults as Json, overrides: {},
    value: JSON.parse(JSON.stringify(defaults)) as Json, readOnly: false, hidden: new Set<string>(), points: [],
    set: (segs: Seg[], v: Json) => { setAt(ctx.value, segs, v) },
  }
  return ctx
}
const list = (ctx: Ctx, path: string) => shapeLists(ctx, path.split('/')).find(l => l.segs.join('/') === path)!

describe('lines and areas', () => {
  it('finds the lines of every aircraft', () => {
    const f = cart(f16, f16a)
    expect(list(f, 'MPD/GEO_LINES')).toMatchObject({ mode: 'flags', flags: ['L1', 'L2', 'L3', 'L4'] })
    const h = cart(hornet, hornetA)
    expect(list(h, 'SA/FAOR_FLOT/FLOT')).toMatchObject({ mode: 'shapes', pointsKey: 'points', maxPts: 7, maxItems: 3 })
    const a = cart(apache, apacheA)
    expect(list(a, 'NAV/Mission_1/Areas')).toMatchObject({ mode: 'shapes', pointsKey: 'vertices', rect: true, closed: 'always' })
    expect(list(a, 'NAV/Mission_1/Lines')).toMatchObject({ pointsKey: 'vertices', minPts: 2, maxPts: 4, rect: false })
    const t = cart(tomcat, tomcatA)
    expect(shapeLists(t, ['NAV']).filter(l => l.segs[2] === 'lines')).toHaveLength(12)
    expect(shapeLists(t, ['NAV']).find(l => l.segs[1] === 1)).toMatchObject({ closed: 'flag', maxPts: 9 })
  })

  it('draws F-16 geo lines per L1-L4, numbered GEO 31 up', () => {
    const ctx = cart(f16, f16a)
    const l = list(ctx, 'MPD/GEO_LINES')
    for (const [lat, lon, flag] of [[42, 42, 0], [42.1, 42.2, 0], [42.3, 42.1, 1], [42.2, 42.4, 0]] as const) {
      ctx.set(l.segs, addPoint(ctx, l, { shape: null, flag }, lat, lon, map)!)
    }
    const v = getAt(ctx.value, l.segs) as JsonObject[]
    expect(v.map(p => [p.number, p.id, p.L1, p.L2])).toEqual([
      [1, 'GEO_LINES31', true, false], [2, 'GEO_LINES32', true, false], [3, 'GEO_LINES33', false, true], [4, 'GEO_LINES34', true, false],
    ])
    const drawn = shapesOf(ctx, map, l)
    expect(drawn.map(s => [s.name, s.pts.length])).toEqual([['Line 1', 3], ['Line 2', 1]])
    expect(drawn[0].pts[0].lat).toBeCloseTo(42, 4)
    // Not boxed as points as well.
    expect(positions(ctx, map, ['MPD', 'GEO_LINES']).filter(p => p.segs[1] === 'GEO_LINES')).toHaveLength(0)
    expect(addableLists(ctx, ['MPD', 'GEO_LINES'])).toHaveLength(0)

    ctx.set(l.segs, removePoint(ctx, l, [...l.segs, 0]))
    expect((getAt(ctx.value, l.segs) as JsonObject[]).map(p => p.id)).toEqual(['GEO_LINES31', 'GEO_LINES32', 'GEO_LINES33'])
  })

  it('keeps Hornet FLOT ids in step with their index', () => {
    const ctx = cart(hornet, hornetA)
    const l = list(ctx, 'SA/FAOR_FLOT/FLOT')
    ctx.set(l.segs, addPoint(ctx, l, { shape: null, flag: 0 }, 42, 42, map)!)
    ctx.set(l.segs, addPoint(ctx, l, { shape: 0, flag: 0 }, 42.1, 42.1, map)!)
    ctx.set(l.segs, addPoint(ctx, l, { shape: null, flag: 0 }, 42.5, 42.5, map)!)
    ctx.set(l.segs, addPoint(ctx, l, { shape: 1, flag: 0 }, 42.6, 42.4, map)!)
    let v = getAt(ctx.value, l.segs) as JsonObject[]
    expect(v.map(s => [s.num, s.id, (s.points as JsonObject[]).map(p => p.id)])).toEqual([
      [1, 'FLOT_1', ['FLOT_1_PT_1', 'FLOT_1_PT_2']], [2, 'FLOT_2', ['FLOT_2_PT_1', 'FLOT_2_PT_2']],
    ])
    // Deleting the first line's points removes it and renumbers the second.
    ctx.set(l.segs, removePoint(ctx, l, [...l.segs, 0, 'points', 0]))
    ctx.set(l.segs, removePoint(ctx, l, [...l.segs, 0, 'points', 0]))
    v = getAt(ctx.value, l.segs) as JsonObject[]
    expect(v.map(s => [s.num, s.id, (s.points as JsonObject[]).map(p => p.id)])).toEqual([[1, 'FLOT_1', ['FLOT_1_PT_1', 'FLOT_1_PT_2']]])
    // Seven points at most per line.
    for (let i = 0; i < 5; i++) ctx.set(l.segs, addPoint(ctx, l, { shape: 0, flag: 0 }, 42 + i / 10, 43, map)!)
    expect(addPoint(ctx, l, { shape: 0, flag: 0 }, 43, 43, map)).toBeNull()
  })

  it('draws an Apache area as four grid-square corners, named EA1', () => {
    const ctx = cart(apache, apacheA)
    const l = list(ctx, 'NAV/Mission_1/Areas')
    ctx.set(l.segs, addRect(ctx, l, { lat: 42, lon: 42 }, { lat: 42.1, lon: 42.2 }, map)!)
    const area = (getAt(ctx.value, l.segs) as JsonObject[])[0]
    const c = area.vertices as JsonObject[]
    expect(c).toHaveLength(4)
    expect(c[0].x).toBe(c[1].x)
    expect(c[1].y).toBe(c[2].y)
    expect(c[2].x).toBe(c[3].x)
    expect(area).toHaveProperty('caption_pos')
    expect(area).toHaveProperty('note', '')
    const [ea] = shapesOf(ctx, map, l)
    expect(ea).toMatchObject({ name: 'EA1', closed: true })
    // A corner deleted takes the area with it: DCS drops anything but four.
    ctx.set(l.segs, removePoint(ctx, l, [...l.segs, 0, 'vertices', 2]))
    expect(getAt(ctx.value, l.segs)).toEqual([])
  })

  it('names Apache lines by type and writes F-14 plot points with lat/lon', () => {
    const a = cart(apache, apacheA)
    const l = list(a, 'NAV/Mission_1/Lines')
    a.set(l.segs, addPoint(a, l, { shape: null, flag: 0 }, 42, 42, map)!)
    a.set(l.segs, addPoint(a, l, { shape: 0, flag: 0 }, 42.1, 42.1, map)!)
    const line = (getAt(a.value, l.segs) as JsonObject[])[0]
    expect((line.vertices as JsonObject[])).toHaveLength(2)
    expect(shapesOf(a, map, l)[0].name).toBe('PL')

    const t = cart(tomcat, tomcatA)
    const pl = shapeLists(t, ['NAV']).find(x => x.segs[1] === 2)!
    t.set(pl.segs, addPoint(t, pl, { shape: null, flag: 0 }, 42.25, 42.5, map)!)
    const p = ((getAt(t.value, pl.segs) as JsonObject[])[0].points as JsonObject[])[0]
    expect(p.lat).toBeCloseTo(42.25, 6)
    expect(p.lon).toBeCloseTo(42.5, 6)
    expect(typeof p.x).toBe('number')
    expect(p.x).not.toBe(0)
  })
})
