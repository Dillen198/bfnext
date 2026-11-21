import { describe, expect, it } from 'vitest'
import { braa } from './analysis'
import type { State } from './data'

const M = 1 / 111_195 // ~1 m of latitude, in degrees

/** A jet at (east, north, up) metres from the origin, flying `hdg` level at `v` m/s. */
function jet(e: number, n: number, up: number, hdg: number, v = 200, pitch = 0): State {
  const p = (pitch * Math.PI) / 180
  return {
    lon: e * M, lat: n * M, alt: up, roll: 0, pitch, hdg,
    gs: v * Math.cos(p), vs: v * Math.sin(p),
    path: { pitch, trk: hdg },
  }
}

describe('braa', () => {
  it('reads a tail chase', () => {
    const r = braa(jet(0, 0, 3000, 0, 250), jet(0, 1000, 3000, 0, 200))
    expect(r.range).toBeCloseTo(1000, -1)
    expect(r.closure).toBeCloseTo(50, 0)
    expect(r.ata).toBeLessThan(1)
    expect(r.aspect).toBeLessThan(1)
    expect(r.hca).toBeLessThan(1)
    expect(r.pursuit).toBe('pure')
  })

  it('reads head on', () => {
    const r = braa(jet(0, 0, 3000, 0), jet(0, 5000, 3000, 180))
    expect(r.aspect).toBeGreaterThan(179)
    expect(r.hca).toBeGreaterThan(179)
    expect(r.closure).toBeCloseTo(400, 0)
  })

  it('tells lead from lag', () => {
    // the target crosses left to right (east), 1 km ahead
    const tgt = jet(0, 1000, 3000, 90)
    expect(braa(jet(0, 0, 3000, 20), tgt).pursuit).toBe('lead')
    expect(braa(jet(0, 0, 3000, 340), tgt).pursuit).toBe('lag')
    expect(braa(jet(0, 0, 3000, 180), tgt).pursuit).toBeNull()
  })

  it('measures in three dimensions', () => {
    // straight up at a target 1 km above, which climbs away
    const r = braa(jet(0, 0, 3000, 0, 200, 90), jet(0, 0, 4000, 0, 150, 90))
    expect(r.ata).toBeLessThan(1)
    expect(r.aspect).toBeLessThan(1)
    expect(r.dAlt).toBeCloseTo(1000, 3)
  })
})
