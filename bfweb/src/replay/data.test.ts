import { describe, expect, it } from 'vitest'
import { sampleSeg, type Seg } from './data'

const f64 = (xs: number[]) => Float64Array.from(xs)

describe('sampleSeg path', () => {
  // A bomb released level, heading east, as DCS writes it: attitude at
  // release only, then positions. 1 s apart, ~200 m/s east, falling faster.
  const deg = 1 / 111_195 // ~1 m of latitude, in degrees
  const bomb: Seg = {
    t: f64([0, 1000, 2000, 3000]),
    lon: f64([0, 200 * deg, 400 * deg, 600 * deg]),
    lat: f64([0, 0, 0, 0]),
    alt: f64([1000, 995, 975, 935]),
    roll: f64([0, 0, 0, 0]),
    pitch: f64([0, 0, 0, 0]),
    yaw: f64([0, 0, 0, 0]),
  }

  it('noses down along the fall while the recorded pitch stays level', () => {
    const early = sampleSeg(bomb, 500)!
    const late = sampleSeg(bomb, 2500)!
    expect(late.pitch).toBe(0)
    expect(early.path!.pitch).toBeLessThan(0)
    expect(late.path!.pitch).toBeLessThan(early.path!.pitch)
    // halfway between the sinks at 2 s (30 m/s, centred) and 3 s (40 m/s)
    expect(late.path!.pitch).toBeCloseTo(-Math.atan2(35, 200) * 180 / Math.PI, 1)
    expect(late.path!.trk).toBeCloseTo(90, 0)
  })

  it('turns smoothly across a sample', () => {
    const a = sampleSeg(bomb, 1999)!.path!.pitch
    const b = sampleSeg(bomb, 2001)!.path!.pitch
    expect(Math.abs(a - b)).toBeLessThan(0.1)
  })

  it('has no path standing still', () => {
    const still: Seg = { t: f64([0, 1000]), lon: f64([0, 0]), lat: f64([0, 0]), alt: f64([5, 5]) }
    expect(sampleSeg(still, 500)!.path).toBeUndefined()
  })
})
