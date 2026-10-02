import { describe, expect, it } from 'vitest'
import { SECTORS } from '../mock/sectors'
import type { Sector } from '../types'
import { distanceM, NM } from './geo'
import { drawSectors, kindInfo, sectorCentre, sectorLabelPoint, sectorRing, showsFor, trackRing } from './sectors'

/** West-east and south-north extent of a ring, measured with haversine. */
function extent(ring: [number, number][]) {
  const lons = ring.map(p => p[0])
  const lats = ring.map(p => p[1])
  const [w, e, s, n] = [Math.min(...lons), Math.max(...lons), Math.min(...lats), Math.max(...lats)]
  const midLat = (s + n) / 2
  const midLon = (w + e) / 2
  return {
    ew: distanceM({ lat: midLat, lon: w }, { lat: midLat, lon: e }),
    ns: distanceM({ lat: s, lon: midLon }, { lat: n, lon: midLon }),
  }
}

describe('tanker track sectors', () => {
  const leg = 30 * NM
  const width = 8 * NM

  it('an east-west leg makes a closed stadium leg + width long and width wide', () => {
    const ring = trackRing({ lat: 42, lon: 41, heading_deg: 90, leg_m: leg, width_m: width })
    expect(ring[0]).toEqual(ring[ring.length - 1])
    const { ew, ns } = extent(ring)
    expect(Math.abs(ew - (leg + width)) / (leg + width)).toBeLessThan(0.005)
    expect(Math.abs(ns - width) / width).toBeLessThan(0.005)
    // the leg starts at the given point and runs east of it
    const lons = ring.map(p => p[0])
    expect(Math.min(...lons)).toBeLessThan(41)
    expect(Math.max(...lons)).toBeGreaterThan(41 + 0.5)
  })

  it('a north-south leg swaps the extents', () => {
    const ring = trackRing({ lat: 42, lon: 41, heading_deg: 0, leg_m: leg, width_m: width })
    const { ew, ns } = extent(ring)
    expect(Math.abs(ns - (leg + width)) / (leg + width)).toBeLessThan(0.005)
    expect(Math.abs(ew - width) / width).toBeLessThan(0.005)
  })

  it('every point is within half the width of the leg', () => {
    const t = { lat: 42, lon: 41, heading_deg: 37, leg_m: leg, width_m: width }
    const start = { lat: t.lat, lon: t.lon }
    const mid = sectorLabelPoint({ track: t }, trackRing(t))
    // the label sits halfway down the leg (the local plane is good to ~0.1 %
    // over a diagonal this long: tens of metres, nothing on a 70 km track)
    expect(Math.abs(distanceM(start, mid) - leg / 2) / (leg / 2)).toBeLessThan(0.001)
    const reach = leg / 2 + width / 2
    for (const [lon, lat] of trackRing(t)) {
      // no point further from the leg's midpoint than half the leg plus half the width
      expect(distanceM(mid, { lat, lon })).toBeLessThan(reach * 1.001)
    }
  })
})

describe('sector shapes', () => {
  it('closes polygons and rejects unusable shapes', () => {
    const ring = sectorRing({ polygon: [{ lat: 42, lon: 41 }, { lat: 42.1, lon: 41 }, { lat: 42.1, lon: 41.1 }] })!
    expect(ring).toHaveLength(4)
    expect(ring[0]).toEqual(ring[3])
    expect(sectorRing({ polygon: [{ lat: 42, lon: 41 }, { lat: 42.1, lon: 41 }] })).toBeNull()
    expect(sectorRing({ circle: { lat: 42, lon: 41, radius_m: 0 } })).toBeNull()
    expect(sectorRing({ track: { lat: 42, lon: 41, heading_deg: 90, leg_m: 1000, width_m: -1 } })).toBeNull()
    expect(sectorRing({})).toBeNull()
  })

  it('names a polygon or circle above its middle, inside the northern edge', () => {
    const shape = { polygon: [{ lat: 42, lon: 41 }, { lat: 42, lon: 41.2 }, { lat: 42.2, lon: 41.2 }, { lat: 42.2, lon: 41 }] }
    const ring = sectorRing(shape)!
    const c = sectorCentre(ring)
    expect(c.lat).toBeCloseTo(42.1, 3)
    expect(c.lon).toBeCloseTo(41.1, 3)
    const l = sectorLabelPoint(shape, ring)
    expect(l.lon).toBeCloseTo(41.1, 3)
    expect(l.lat).toBeCloseTo(42.1 + 0.7 * 0.1, 3)
    const circle = { lat: 42, lon: 41, radius_m: 10_000 }
    const cl = sectorLabelPoint({ circle }, sectorRing({ circle })!)
    expect(distanceM(circle, cl)).toBeCloseTo(7000, -1)
    expect(cl.lat).toBeGreaterThan(42)
  })

  it('draws all 27 sample sectors, largest first, in their F10 colours', () => {
    const drawn = drawSectors(SECTORS)
    expect(drawn).toHaveLength(27)
    for (let i = 1; i < drawn.length; i++) expect(drawn[i - 1].area_m2).toBeGreaterThanOrEqual(drawn[i].area_m2)
    const r1 = drawn.find(d => d.sector.id === 'r-1')!
    expect(r1.info.color).toBe('#FF9F1A')
    expect(drawn.find(d => d.sector.kind === 'aar')!.info.dashed).toBe(true)
    // a kind from a newer engine still draws
    expect(kindInfo('space_ops').label).toBe('SPACE OPS')
  })

  it('the side filter keeps the shared sectors', () => {
    const s = (side: Sector['side']) => ({ side })
    expect(showsFor(s('blue'), 'blue')).toBe(true)
    expect(showsFor(s('red'), 'blue')).toBe(false)
    expect(showsFor(s('all'), 'red')).toBe(true)
    expect(showsFor(s('red'), 'all')).toBe(true)
    const blue = SECTORS.filter(x => showsFor(x, 'blue'))
    expect(blue.some(x => x.side === 'all')).toBe(true)
    expect(blue.some(x => x.side === 'red')).toBe(false)
  })
})
