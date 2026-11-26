import { describe, expect, it } from 'vitest'
import { missionClock } from './missionClock'
import { flightCategory, metar } from './weather'
import type { RangeLive, SkyInfo } from '../types'

const sky = (s: Partial<SkyInfo>): SkyInfo => ({ precip: 'none', visibility_m: 80_000, ground_m: 0, ...s })

const live = (s: SkyInfo | null): RangeLive => ({
  theatre: 'MarianaIslands', mission_date: '2024/6/15', mission_time: '08:18:38',
  wind: { surface_from_deg: 92, surface_kts: 11.4, alt_from_deg: 100, alt_kts: 20, temperature_c: 27.6, qnh_hpa: 1013.2 },
  sky: s,
} as unknown as RangeLive)

describe('mission clock', () => {
  it('takes the theatre offset off for Zulu, back across midnight', () => {
    const c = missionClock('MarianaIslands', '08:18:38', '2024/6/15', 0)!
    expect(c.local).toBe('08:18:38')
    expect(c.zulu).toBe('22:18:38')
    expect(c.zuluDay).toBe(14)
  })
  it('runs on between polls, but not for ever', () => {
    expect(missionClock('Caucasus', '23:59:58', '2024/8/1', 5)!.local).toBe('00:00:03')
    expect(missionClock('Caucasus', '10:00:00', '2024/8/1', 600)!.local).toBe('10:00:30')
  })
  it('gives no Zulu for a theatre it does not know', () => {
    expect(missionClock('SomeNewMap', '10:00:00', '2024/8/1', 0)!.zulu).toBeNull()
  })
})

describe('weather', () => {
  it('writes CAVOK on a clear day', () => {
    const l = live(sky({}))
    expect(metar(l, missionClock(l.theatre, l.mission_time, l.mission_date, 0))).toBe('RNG 142218Z 09011KT CAVOK 28/ Q1013 A2992')
  })
  it('writes the layer, the visibility and the weather when there is some', () => {
    const l = live(sky({ cover: 'BKN', base_m: 457, visibility_m: 3000, precip: 'rain' }))
    expect(metar(l, null)).toBe('RNG 09011KT 3000 RA BKN015 28/ Q1013 A2992')
  })
  it('grades the flight category from the ceiling and the visibility', () => {
    expect(flightCategory(sky({}))).toBe('VFR')
    expect(flightCategory(sky({ cover: 'SCT', base_m: 300 }))).toBe('VFR')
    expect(flightCategory(sky({ cover: 'OVC', base_m: 610 }))).toBe('MVFR')
    expect(flightCategory(sky({ cover: 'BKN', base_m: 250 }))).toBe('IFR')
    expect(flightCategory(sky({ visibility_m: 1000 }))).toBe('LIFR')
  })
})
