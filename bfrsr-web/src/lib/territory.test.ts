import { describe, expect, it } from 'vitest'
import type { LatLon } from '../api'
import { clipPieces, territoryMap, type TerritorySite } from './territory'

const site = (lat: number, lon: number, owner: string, kind = 'FOB'): TerritorySite => ({ lat, lon, owner, kind })

function inside([lat, lon]: LatLon, ring: LatLon[]): boolean {
  let hit = false
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
    const [yi, xi] = ring[i]
    const [yj, xj] = ring[j]
    if ((yi > lat) !== (yj > lat) && lon < ((xj - xi) * (lat - yi)) / (yj - yi) + xi) hit = !hit
  }
  return hit
}

describe('clipPieces', () => {
  it('cuts a U into its two arms, with nothing joining them', () => {
    // A U opening upwards, cut above its base: two separate arms.
    const u: [number, number][] = [[0, 0], [3, 0], [3, 3], [2, 3], [2, 1], [1, 1], [1, 3], [0, 3]]
    const pieces = clipPieces(u, [0, 2], [0, -1])
    expect(pieces).toHaveLength(2)
    for (const piece of pieces) {
      const xs = piece.map((p) => p[0])
      expect(Math.max(...xs) - Math.min(...xs)).toBe(1)
    }
  })

  it('keeps a polygon wholly inside and drops one wholly outside', () => {
    const sq: [number, number][] = [[0, 0], [1, 0], [1, 1], [0, 1]]
    expect(clipPieces(sq, [5, 0], [1, 0])).toEqual([sq])
    expect(clipPieces(sq, [-5, 0], [1, 0])).toEqual([])
  })
})

describe('territoryMap', () => {
  it('splits the ground at the midpoint, out past the bases', () => {
    const t = territoryMap([site(10, 20.0, 'Blue'), site(10, 20.4, 'Red')])
    expect(t.cells).toHaveLength(2)
    const blue = t.cells.find((c) => c.owner === 'Blue')!
    expect(Math.max(...blue.ring.map((p) => p[1]))).toBeCloseTo(20.2, 2)
    // No 40 km cap: the cell runs well past the base.
    expect(Math.min(...blue.ring.map((p) => p[1]))).toBeLessThan(19.5)
  })

  it('draws the front only between different owners', () => {
    const t = territoryMap([site(10, 20.0, 'Blue'), site(10, 20.4, 'Red'), site(10, 20.8, 'Red')])
    expect(t.front).toHaveLength(1)
    const [[, lonA], [, lonB]] = t.front[0]
    expect(lonA).toBeCloseTo(20.2, 2)
    expect(lonB).toBeCloseTo(20.2, 2)
  })

  it('gives carrier groups no ground', () => {
    const t = territoryMap([site(10, 20, 'Blue', 'Carrier Group'), site(10, 21, 'Red', 'Airbase')])
    expect(t.cells.map((c) => c.owner)).toEqual(['Red'])
    expect(t.front).toHaveLength(0)
  })

  it('fills the Caucasus out past the plot, but none of its seas', () => {
    // Gagra and Sochi-Adler face each other on the coast.
    const t = territoryMap([site(43.2965, 40.25, 'Blue'), site(43.4445, 39.9413, 'Red'), site(42.18, 42.48, 'Blue')])
    const held = (p: LatLon) => t.cells.some((c) => inside(p, c.ring))
    // The Black Sea, the Sea of Azov and the Caspian.
    for (const sea of [[42.9, 39.6], [46.0, 37.0], [42.0, 49.5]] as LatLon[]) expect(held(sea)).toBe(false)
    // By the bases, and far from them: Rostov, Crimea, inland Turkey.
    for (const land of [[43.5, 40.6], [47.5, 40.0], [45.0, 34.0], [39.0, 40.0]] as LatLon[]) expect(held(land)).toBe(true)
    // The front runs inland from the shore, none of it out in the open sea.
    expect(t.front.length).toBeGreaterThan(0)
    for (const seg of t.front) {
      for (const [lat, lon] of seg) expect(lat > 41.6 && lat < 42.9 && lon > 37.5 && lon < 40.4).toBe(false)
    }
  })
})
