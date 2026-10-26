import { describe, expect, it } from 'vitest'
import { buildDcsDtc, dcsdtcFor, ddm, dms, shareString, DCSDTC_TARGETS } from './dcsdtc'
import { newPlan, type Plan } from './types'

const plan = (over: Partial<Plan> = {}): Plan => ({
  ...newPlan('F-16C_50'),
  name: 'CAS: north',
  waypoints: [
    { name: 'kutaisi', lat: 42.1772, lon: 42.4807, alt_ft: 0, elev_ft: 147.6, target: false, note: '' },
    { name: 'tgt', lat: 42.5, lon: 43.2, alt_ft: 20000, elev_ft: 1830, target: true, note: '' },
  ],
  ...over,
})

const f16Defaults = {
  Aircraft: 'F16C', Upload: { Waypoints: false, CMS: false },
  Waypoints: { Waypoints: [] }, CMS: { Programs: [{ Number: 1, Chaff: 1 }] }, Radios: { Radio1: {} }, Version: 2, KneeboardNotes: null,
}

describe('DCS-DTC coordinates', () => {
  it('writes zero-padded DDM with the U+2019 minute mark', () => {
    expect(ddm(42.1772, true)).toBe('N 42°10.632’')
    expect(ddm(2.5, false)).toBe('E 002°30.000’')
    expect(ddm(-0.0001, true)).toBe('S 00°00.006’')
    // 59.9997 minutes rounds up into the next degree, not "60.000".
    expect(ddm(41 + 59.9997 / 60, true)).toBe('N 42°00.000’')
  })

  it('writes DMS for the Hornet pre-planned targets', () => {
    expect(dms(42 + 10 / 60 + 37.8 / 3600, true)).toBe('N 42°10’37.80”')
  })
})

describe('DCS-DTC presets', () => {
  const f16 = DCSDTC_TARGETS.find(t => t.code === 'F16C')!

  it('maps DCS unit types to DCS-DTC codes', () => {
    expect(dcsdtcFor('FA-18C_hornet')?.code).toBe('FA18C')
    expect(dcsdtcFor('F-15ESE')?.code).toBe('F15E')
    expect(dcsdtcFor('Su-25T')).toBeUndefined()
  })

  it('writes a whole preset with the route and Version 2', () => {
    const out = buildDcsDtc(plan(), f16, f16Defaults, { '/CMS/Programs/0/Chaff': 4 }, false, () => null)
    const cfg = JSON.parse(out.text)
    expect(out.filename).toBe('CAS north.json')
    expect(cfg.Aircraft).toBe('F16C')
    expect(cfg.Version).toBe(2)
    expect(cfg.Upload.Waypoints).toBe(true)
    expect(cfg.Waypoints.Waypoints).toHaveLength(2)
    expect(cfg.Waypoints.Waypoints[0]).toMatchObject({ Sequence: 1, Name: 'KUTAISI', Elevation: 148, Target: false, UseOA: false })
    expect(cfg.Waypoints.Waypoints[1].Target).toBe(true)
    expect(cfg.CMS.Programs[0].Chaff).toBe(4)
    expect(cfg.Radios).toEqual({ Radio1: {} })
  })

  it('nulls untouched systems for Load from file', () => {
    const cfg = JSON.parse(buildDcsDtc(plan(), f16, f16Defaults, { '/CMS/Programs/0/Chaff': 4 }, true, () => null).text)
    expect(cfg.Radios).toBeNull()
    expect(cfg.CMS.Programs[0].Chaff).toBe(4)
    expect(cfg.Waypoints.Waypoints).toHaveLength(2)
    expect(cfg.Version).toBe(2)
  })

  it('numbers Hornet waypoints from 0 and splits Apache targets', () => {
    const hornet = DCSDTC_TARGETS.find(t => t.code === 'FA18C')!
    const h = JSON.parse(buildDcsDtc(plan(), hornet, { Waypoints: { Waypoints: [] } }, {}, false, () => null).text)
    expect(h.Waypoints.Waypoints.map((w: { Sequence: number }) => w.Sequence)).toEqual([0, 1])
    const apache = DCSDTC_TARGETS.find(t => t.code === 'AH64D')!
    const a = JSON.parse(buildDcsDtc(plan(), apache, { Waypoints: { Waypoints: [] }, Targets: { Waypoints: [] } }, {}, false, () => null).text)
    expect(a.Waypoints.Waypoints).toHaveLength(1)
    expect(a.Targets.Waypoints[0]).toMatchObject({ Sequence: 1, PointType: 'TG', Free: 'TGT' })
  })

  it('keeps C-130 names to six letters or digits', () => {
    const c130 = DCSDTC_TARGETS.find(t => t.code === 'C130')!
    const c = JSON.parse(buildDcsDtc(plan({ waypoints: [{ ...plan().waypoints[0], name: 'stpt 12 x' }] }), c130, { Waypoints: { Waypoints: [] } }, {}, false, () => null).text)
    expect(c.Waypoints.Waypoints[0].Name).toBe('STPT12')
  })

  it('makes the clipboard share string DCS-DTC decodes', async () => {
    const json = '{"Aircraft":"F16C","Version":2}'
    // web APIs only (no node: modules): the app's tsconfig has no Node types
    const raw = Uint8Array.from(atob(await shareString(json)), ch => ch.charCodeAt(0))
    expect(new DataView(raw.buffer).getInt32(0, true)).toBe(json.length)
    const body = new Blob([raw.subarray(4)]).stream().pipeThrough(new DecompressionStream('gzip'))
    expect(await new Response(body).text()).toBe(json)
  })
})
