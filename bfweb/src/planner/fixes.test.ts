// Regression tests for what verification found running the exports through
// DCS's own import code and DCS-DTC's C# model.
import { describe, expect, it } from 'vitest'
import { buildNative } from './native'
import { buildDcsDtc, DCSDTC_TARGETS } from './dcsdtc'
import { pruneOverrides, type Json } from './model'
import { newPlan, type Plan, type PlanWaypoint } from './types'
import hornet from './data/native/FA-18C_hornet.defaults.json'
import apache from './data/native/AH-64D_BLK_II.defaults.json'
import f16 from './data/native/F-16C_50.defaults.json'
import a64dtc from './data/dcsdtc/AH64D.defaults.json'
import f18dtc from './data/dcsdtc/FA18C.defaults.json'
import f16dtc from './data/dcsdtc/F16C.defaults.json'
import kiowa from './data/dcsdtc/OH58D.defaults.json'
import c130 from './data/dcsdtc/C130.defaults.json'

type Obj = Record<string, unknown>
const wp = (name: string, lat: number, lon: number, extra: Partial<PlanWaypoint> = {}): PlanWaypoint => ({
  name, lat, lon, alt_ft: 12000, elev_ft: 100, target: false, note: '', ...extra,
})
const many = (n: number, targets: number[] = []) =>
  Array.from({ length: n }, (_, i) => wp(`POINT${i + 1}`, 41.7 + i * 0.01, 41.8 + i * 0.01, { target: targets.includes(i) }))
const plan = (aircraft: string, over: Partial<Plan> = {}): Plan => ({ ...newPlan(aircraft), name: 'Fix check', waypoints: many(4), ...over })
const data = (t: string) => JSON.parse(t).data
const radios = { include_comms: true, comm1: [{ preset: 2, freq_mhz: 251, label: 'AWACS', modulation: 'AM' as const }], comm2: [{ preset: 2, freq_mhz: 40.5, label: 'JTAC', modulation: 'FM' as const }] }

describe('overrides that do not fit the defaults', () => {
  it('drops typos, keeps real keys, array items and new dict entries', () => {
    const d: Json = { CMDS: { ChaffNum: 10 }, Laser: {}, List: [1, 2] }
    expect(pruneOverrides(d, {
      '/CMDS/Chaff': 12, '/CMDS/ChaffNum': 12, '/Laser/A': { code: 1688 }, '/Laser/A/code': 1511, '/List/1': 5, '/Nope/x': 1,
    })).toEqual({ '/CMDS/ChaffNum': 12, '/Laser/A': { code: 1688 }, '/Laser/A/code': 1511, '/List/1': 5 })
  })

  it('warns when a saved setting is left out', () => {
    const out = buildNative(plan('F-16C_50'), 'F-16C_50', f16 as Json, { '/MPD/CMDS/CMDSBingoSettings/Chaff': 12 }, () => null)
    expect(out.warnings.join(' ')).toMatch(/1 saved setting/)
    expect(data(out.text).MPD.CMDS.CMDSBingoSettings.Chaff).toBeUndefined()
  })
})

describe('native Hornet sequences', () => {
  it('splits a long route across SEQ 1-3 with one TGT each and 5-letter names', () => {
    const out = buildNative(plan('FA-18C_hornet', { waypoints: many(32, [3, 5, 20]) }), 'FA-18C_hornet', hornet as Json, {}, () => null)
    const w = data(out.text).WYPT as { NAV_PTS: Obj[]; NAV_ROUTE: Obj[] }
    expect(w.NAV_PTS[0]).toMatchObject({ R1: true, R2: false, R1_order: 1, text_note: 'POINT' })
    expect(w.NAV_PTS[15]).toMatchObject({ R1: false, R2: true, R2_order: 1 })
    expect(w.NAV_PTS[31]).toMatchObject({ R3: true, R3_order: 2 })
    expect(Object.keys(w.NAV_ROUTE[0])).toHaveLength(15)
    expect(Object.keys(w.NAV_ROUTE[2])).toHaveLength(2)
    const tgts = Object.values(w.NAV_ROUTE[0]).filter(v => (v as Obj).TGT)
    expect(tgts).toHaveLength(1)
    expect((w.NAV_ROUTE[1].STPT21 as Obj).TGT).toBe(true)
    expect(out.warnings.join(' ')).toMatch(/one TGT point/)
  })
})

describe('native Apache radios and altitudes', () => {
  it('writes plan presets per radio and keeps legs above the ground', () => {
    const d = data(buildNative(plan('AH-64D_BLK_II', { ...radios, waypoints: [wp('FARP', 41.7, 41.8, { alt_ft: 0, elev_ft: 500 }), wp('B', 41.8, 41.9)] }),
      'AH-64D_BLK_II', apache as Json, {}, () => null).text)
    const comms = d.Presets.COMM[1].COMMS
    expect(comms.UHF).toMatchObject({ Frequency: 251, Modulation: 0 })
    expect(comms.FM1).toMatchObject({ Frequency: 40.5, Modulation: 1 })
    expect(d.Presets.COMM[1].DoNotUploadTabData).toBe(false)
    expect(d.NAV.Mission_1.Routes[0].POINTS[0].alt).toBeCloseTo(500 * 0.3048, 6)
  })
})

describe('DCS-DTC fixes', () => {
  const tg = (c: string) => DCSDTC_TARGETS.find(t => t.code === c)!

  it('leaves capture settings alone and sequences the Hornet route', () => {
    const cfg = JSON.parse(buildDcsDtc(plan('FA-18C_hornet'), tg('FA18C'), f18dtc as Json, {}, true, () => null).text)
    expect(cfg.WaypointsCapture).toBeNull()
    expect(cfg.Sequences.Seq1.Mode).toBe(1)
    expect(cfg.Sequences.EnableUpload1).toBe(true)
    expect(cfg.Upload.Sequences).toBe(true)
  })

  it('writes plan radios in each preset shape', () => {
    const f = JSON.parse(buildDcsDtc(plan('F-16C_50', radios), tg('F16C'), f16dtc as Json, {}, true, () => null).text)
    expect(f.Radios.Radio1.Presets).toEqual([{ Number: 2, Name: 'AWACS', Frequency: '251.000' }])
    expect(f.Upload.Radios).toBe(true)
    const a = JSON.parse(buildDcsDtc(plan('AH-64D_BLK_II', radios), tg('AH64D'), a64dtc as Json, {}, false, () => null).text)
    expect(a.Radios.Radio.Presets).toEqual([{ Number: 2, Name: 'AWACS', Frequencies: ['251.000', '', '40.500', '', '', ''] }])
    const k = JSON.parse(buildDcsDtc(plan('OH58D', radios), tg('OH58D'), kiowa as Json, {}, false, () => null).text)
    expect(k.Radios.Radio.Presets[0].Frequencies).toHaveLength(4)
    expect(k.Radios.Radio.SelectedModes.map((m: Obj) => [m.Number, m.SelectedMode])).toEqual([[1, 2], [2, 2], [3, 2], [4, 2]])
  })

  it('does not wipe Apache points a plan has none of', () => {
    const cfg = JSON.parse(buildDcsDtc(plan('AH-64D_BLK_II'), tg('AH64D'), a64dtc as Json, {}, true, () => null).text)
    expect(cfg.Targets).toBeNull()
    expect(cfg.Waypoints.Waypoints).toHaveLength(4)
  })

  it('keeps C-130 names unique once cut to six characters', () => {
    const cfg = JSON.parse(buildDcsDtc(plan('C-130J-30', { waypoints: [wp('ALPHA ONE', 41.7, 41.8), wp('ALPHA TWO', 41.8, 41.9)] }), tg('C130'), c130 as Json, {}, false, () => null).text)
    const names = cfg.Waypoints.Waypoints.map((w: Obj) => w.Name)
    expect(names[0]).toBe('ALPHAO')
    expect(new Set(names).size).toBe(2)
    expect((names[1] as string).length).toBeLessThanOrEqual(6)
  })

  it('clamps elevation to what the cockpit takes', () => {
    const cfg = JSON.parse(buildDcsDtc(plan('FA-18C_hornet', { waypoints: [wp('LOW', 41.7, 41.8, { elev_ft: -50 })] }), tg('FA18C'), f18dtc as Json, {}, false, () => null).text)
    expect(cfg.Waypoints.Waypoints[0].Elevation).toBe(0)
  })
})
