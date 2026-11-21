import { describe, expect, it } from 'vitest'
import { fmtCoord, toMgrs, toUtm } from './coords'

describe('coordinates', () => {
  it('puts the zone 31 origin where the grid says', () => {
    // lat 0 lon 0: the textbook check value
    expect(toMgrs({ lat: 0, lon: 0 })).toBe('31N AA 66021 00000')
  })

  it('is exactly 500 km east on a central meridian', () => {
    const u = toUtm({ lat: 42, lon: 45 })
    expect(u.zone).toBe(38)
    expect(u.band).toBe('T')
    expect(u.easting).toBeCloseTo(500000, 3)
  })

  it('formats the cockpit styles', () => {
    const p = { lat: 42.1766, lon: 42.4826 }
    expect(fmtCoord(p, 'ddm')).toBe("N42°10.596' E042°28.956'")
    expect(fmtCoord(p, 'dms')).toBe('N42°10\'35.8" E042°28\'57.4"')
    expect(fmtCoord({ lat: -12.5, lon: -0.25 }, 'ddm')).toBe("S12°30.000' W000°15.000'")
    expect(fmtCoord(p, 'mgrs')).toMatch(/^38T [A-Z]{2} \d{5} \d{5}$/)
  })

  it('rolls minutes over instead of printing 60', () => {
    expect(fmtCoord({ lat: 41.99999999, lon: 43 }, 'ddm')).toBe("N42°00.000' E043°00.000'")
  })
})
