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
import a10 from './data/dcsdtc/A10.defaults.json'
import tomcat from './data/native/F-14BU.defaults.json'
import fulcrum from './data/native/MiG-29_Fulcrum.defaults.json'

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
    // Point 15 ends SEQ 1 and starts SEQ 2, so the leg 15 -> 16 is flown.
    expect(w.NAV_PTS[14]).toMatchObject({ R1: true, R1_order: 15, R2: true, R2_order: 1 })
    expect(w.NAV_PTS[15]).toMatchObject({ R1: false, R2: true, R2_order: 2 })
    expect(w.NAV_PTS[28]).toMatchObject({ R2: true, R2_order: 15, R3: true, R3_order: 1 })
    expect(w.NAV_PTS[31]).toMatchObject({ R3: true, R3_order: 4 })
    expect(Object.keys(w.NAV_ROUTE[0])).toHaveLength(15)
    expect(Object.keys(w.NAV_ROUTE[1])).toHaveLength(15)
    expect(Object.keys(w.NAV_ROUTE[2])).toHaveLength(4)
    expect((w.NAV_ROUTE[1].STPT15 as Obj).route_num).toBe(2)
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

describe('second verification pass', () => {
  const tg = (c: string) => DCSDTC_TARGETS.find(t => t.code === c)!
  const ground = [wp('FIELD', 41.7, 41.8, { alt_ft: 0, elev_ft: 150 }), wp('B', 41.8, 41.9), wp('C', 41.9, 42.0)]

  it('writes MiG-29 plan radios and keeps altitudes above the ground', () => {
    const d = data(buildNative(plan('MiG-29 Fulcrum', {
      include_comms: true, waypoints: ground,
      comm1: [{ preset: 3, freq_mhz: 251, label: '', modulation: 'AM' }, { preset: 20, freq_mhz: 124, label: '', modulation: 'AM' }],
      comm2: [{ preset: 4, freq_mhz: 40.5, label: '', modulation: 'FM' }],
    }), 'MiG-29 Fulcrum', fulcrum as Json, {}, () => null).text)
    expect(d.Program_1.Radio.Channel_3).toEqual({ freq: 251, modulation: 1 })
    expect(d.Program_1.Radio.Channel_0).toEqual({ freq: 124, modulation: 1 })
    expect(d.mirror_Radio).toBe(false)
    expect(d.Program_1.Points[0].alt).toBeCloseTo(150 * 0.3048, 6)
    expect(d.Program_1.Route[0].alt).toBeCloseTo(150 * 0.3048, 6)
  })

  it('keeps F-14 elevations and speeds in the cockpit ranges', () => {
    const out = buildNative(plan('F-14BU', { speed_kts: 997, include_comms: true, comm1: radios.comm1, waypoints: [wp('LOW', 41.7, 41.8, { elev_ft: -50 }), wp('B', 41.8, 41.9)] }),
      'F-14BU', tomcat as Json, {}, () => null)
    const pts = data(out.text).NAV[1].waypoints
    expect(pts[0].elev).toBe(0)
    expect(pts[0].spd).toBe(990)
    expect(out.warnings.join(' ')).toMatch(/no radio section/)
  })

  it('skips presets a two-radio aircraft cannot tune', () => {
    const out = buildDcsDtc(plan('C-130J-30', { include_comms: true, comm1: [], comm2: [{ preset: 1, freq_mhz: 40, label: '', modulation: 'FM' }, { preset: 2, freq_mhz: 127.5, label: '', modulation: 'AM' }] }),
      tg('C130'), c130 as Json, {}, false, () => null)
    expect(JSON.parse(out.text).Radios.Radio2.Presets.map((p: Obj) => p.Frequency)).toEqual(['127.500'])
    expect(out.warnings.join(' ')).toMatch(/1 radio preset outside/)
  })

  it('puts A-10 COMM2 presets on the ARC-210 as 21+', () => {
    const cfg = JSON.parse(buildDcsDtc(plan('A-10C_2', { include_comms: true, comm1: [{ preset: 1, freq_mhz: 251, label: '', modulation: 'AM' }], comm2: [{ preset: 1, freq_mhz: 127.5, label: 'FLT', modulation: 'AM' }] }),
      tg('A10'), a10 as Json, {}, false, () => null).text)
    expect(cfg.Radios.Radio1.Presets.map((p: Obj) => [p.Number, p.Frequency])).toEqual([[1, '251.000'], [21, '127.500']])
    expect(cfg.Radios.Radio2.Presets).toBeNull()
  })

  it('keeps Apache VHF to 118-151.975', () => {
    const cfg = JSON.parse(buildDcsDtc(plan('AH-64D_BLK_II', { include_comms: true, comm1: [], comm2: [{ preset: 1, freq_mhz: 110, label: '', modulation: 'AM' }, { preset: 2, freq_mhz: 120, label: '', modulation: 'AM' }] }),
      tg('AH64D'), a64dtc as Json, {}, false, () => null).text)
    expect(cfg.Radios.Radio.Presets.map((p: Obj) => p.Number)).toEqual([2])
  })

  it('refuses an override past the end of a list', () => {
    expect(pruneOverrides({ L: [] }, { '/L/0': 1, '/L/3': 2 })).toEqual({ '/L/0': 1 })
  })

  it('renames C-130 points until the name is free', () => {
    const names = ['KUTAISI 1', 'KUTAISI 2', 'KUTAI3', 'KUTAISI 3'].map((n, i) => wp(n, 41.7 + i * 0.01, 41.8))
    const cfg = JSON.parse(buildDcsDtc(plan('C-130J-30', { waypoints: names }), tg('C130'), c130 as Json, {}, false, () => null).text)
    const got = cfg.Waypoints.Waypoints.map((w: Obj) => w.Name as string)
    expect(new Set(got).size).toBe(4)
    expect(got.every((n: string) => /^[A-Z0-9]{1,6}$/.test(n))).toBe(true)
  })
})

describe('automatic load switches', () => {
  it('switches on the DCS-DTC upload and item flags for what changed', () => {
    const t = DCSDTC_TARGETS.find(x => x.code === 'F16C')!
    const cfg = JSON.parse(buildDcsDtc(plan('F-16C_50'), t, f16dtc as Json,
      { '/CMS/Programs/0/ChaffBurstQty': 4, '/Misc/TGPCode': 1511, '/Misc/Bingo': 3000 }, false, () => null).text)
    expect(cfg.Upload.CMS).toBe(true)
    expect(cfg.Upload.Misc).toBe(true)
    expect(cfg.Upload.Radios).toBe(false)
    expect(cfg.CMS.Programs[0].ToBeUpdated).toBe(true)
    expect(cfg.CMS.Programs[1].ToBeUpdated).toBe(false)
    expect(cfg.Misc.LaserSettingsToBeUpdated).toBe(true)
    expect(cfg.Misc.BingoToBeUpdated).toBe(true)
    expect(cfg.Misc.TACANToBeUpdated).toBe(false)
  })

  it('leaves a switch the pilot set by hand', () => {
    const t = DCSDTC_TARGETS.find(x => x.code === 'F16C')!
    const cfg = JSON.parse(buildDcsDtc(plan('F-16C_50'), t, f16dtc as Json,
      { '/CMS/Programs/0/ChaffBurstQty': 4, '/Upload/CMS': false }, false, () => null).text)
    expect(cfg.Upload.CMS).toBe(false)
  })
})

describe('DCS-DTC Hornet sequences', () => {
  it('splits a long route into waypoint ranges, 15 points each', () => {
    const t = DCSDTC_TARGETS.find(x => x.code === 'FA18C')!
    const cfg = JSON.parse(buildDcsDtc(plan('FA-18C_hornet', { waypoints: many(32) }), t, f18dtc as Json, {}, false, () => null).text)
    // Hornet waypoints are numbered from 0 in DCS-DTC.
    expect(cfg.Sequences.Seq1).toEqual({ Mode: 2, Waypoints: [0, 14] })
    expect(cfg.Sequences.Seq2).toEqual({ Mode: 2, Waypoints: [14, 28] })
    expect(cfg.Sequences.Seq3).toEqual({ Mode: 2, Waypoints: [28, 31] })
    expect([cfg.Sequences.EnableUpload1, cfg.Sequences.EnableUpload2, cfg.Sequences.EnableUpload3]).toEqual([true, true, true])
    const short = JSON.parse(buildDcsDtc(plan('FA-18C_hornet'), t, f18dtc as Json, {}, false, () => null).text)
    expect(short.Sequences.Seq1).toEqual({ Mode: 1, Waypoints: null })
    expect(short.Sequences.EnableUpload2).toBe(false)
  })
})

describe('F-14 JDAM targets placed on the map', () => {
  it('draws an 11-point release zone around the reciprocal of the attack heading', async () => {
    const { lar, larPolygon, larCentre } = await import('./f14lar')
    const l = lar(20000, 450)
    const poly = larPolygon(0, 0, 0, l)
    expect(poly).toHaveLength(11)
    // Attack heading 000 (north): the zone lies to the south (negative x).
    expect(poly.every(p => p.x < 0)).toBe(true)
    expect(Math.hypot(poly[0].x, poly[0].y)).toBeCloseTo(l.lar_rmin_nmi * 1852, 3)
    expect(larCentre(0, 0, 0, l).x).toBeCloseTo(-((l.lar_rmin_nmi + l.lar_rmax_nmi) / 2) * 1852, 3)
  })

  it('exports a placed target with lat/lon and warns about a zero elevation', () => {
    // The map writes the whole slot, as here.
    const slot = (tomcat as { JDAM: { stations: { targets: Obj[] }[] } }).JDAM.stations[1].targets[0]
    const out = buildNative(plan('F-14BU'), 'F-14BU', tomcat as Json, {
      '/JDAM/stations/1/targets/0': { ...slot, active: true, x: -260000, y: 620000, elev: 0 } as Json,
    }, () => null)
    const t = data(out.text).JDAM.stations[1].targets[0]
    expect(t).toMatchObject({ active: true, x: -260000, y: 620000 })
    expect(t.lat).toBeGreaterThan(41)
    expect(t.lon).toBeGreaterThan(41)
    expect(typeof t.launch_x).toBe('number')
    expect(out.warnings.join(' ')).toMatch(/0 m elevation/)
  })

  it('finds every JDAM slot on the page, labelled by station', async () => {
    const { slotsOn } = await import('./dtc/places')
    const { AnnotationIndex, normaliseAnnotations } = await import('./model')
    const ann = (await import('./data/native/F-14BU.annotations.json')).default
    const idx = new AnnotationIndex(normaliseAnnotations(ann as never))
    const ctx = { idx, defaults: tomcat as Json, overrides: {}, value: tomcat as Json, readOnly: false, set: () => {}, hidden: new Set<string>(), points: [] }
    const slots = slotsOn(ctx, ['JDAM'])
    expect(slots).toHaveLength(32)
    expect(slots[0].label).toBe('STA 3 › PP1')
    expect(slots[31].label).toBe('STA 6 › PP8')
  })
})

describe('offset aimpoints and VIP/VRP', () => {
  const east = { range_ft: 3000, bearing_deg: 90, elev_ft: 150 }
  const route = () => {
    const w = many(4, [2])
    w[2] = { ...w[2], oa: [east, { range_ft: 6076.12, bearing_deg: 0, elev_ft: 200 }], vrp: { point: { range_ft: 9000, bearing_deg: 180, elev_ft: 100 }, pup: { range_ft: 18000, bearing_deg: 200, elev_ft: 100 } } }
    w[1] = { ...w[1], vip: { point: { range_ft: 5 * 6076.12, bearing_deg: 45, elev_ft: 100 }, pup: { range_ft: 3 * 6076.12, bearing_deg: 30, elev_ft: 0 } } }
    return w
  }

  it('writes F-16 OAs and VIP/VRP on the map grid', () => {
    const d = data(buildNative(plan('F-16C_50', { waypoints: route() }), 'F-16C_50', f16 as Json, {}, () => null).text)
    const s = d.MPD.NAV_PTS[2]
    expect(s.isOAP_1).toBe(true)
    // 3000 ft east: about 914 m along +y, delta = steerpoint - OAP.
    expect(s.OAP_1_Range).toBeCloseTo(914.4, -1)
    expect(s.OAP_1_Y - s.y).toBeGreaterThan(900)
    expect(s.OAP_1_DeltaY).toBeCloseTo(s.y - s.OAP_1_Y, 6)
    expect(s.OAP_1_Bearing).toBeGreaterThan(1.4)
    expect(s.OAP_1_Bearing).toBeLessThan(1.75)
    expect(s.OAP_1_Alt).toBeCloseTo(150 * 0.3048, 6)
    expect(s.isOAP_2).toBe(true)
    expect(s.OAP_2_Range).toBeCloseTo(1852, -1)
    const v = d.MPD.VIPVRP
    expect(v).toMatchObject({ VIP_Number: 2, VRP_Number: 3, VIP_Enabled: true, VIPPUP_Enabled: true, VRP_Enabled: true, VRPPUP_Enabled: true })
    expect(v.VIPTOTGT_Range).toBeCloseTo(5 * 1852, -1)
    expect(v.TGTTOVRP_X).toBeLessThan(d.MPD.NAV_PTS[2].x)
    expect(d.MPD.mirror_NAV_PTS).toBe(false)
  })

  it('writes the Hornet OA in grid degrees', () => {
    const d = data(buildNative(plan('FA-18C_hornet', { waypoints: route() }), 'FA-18C_hornet', hornet as Json, {}, () => null).text)
    const p = d.WYPT.NAV_PTS[2]
    expect(p.isOA).toBe(true)
    expect(p.OA_Bearing).toBeGreaterThan(80)
    expect(p.OA_Bearing).toBeLessThan(100)
    expect(p.OA_Range).toBeCloseTo(914.4, -1)
    expect(d.WYPT.NAV_PTS[0].isOA).toBe(false)
  })

  it('writes DCS-DTC F-16 offsets in its units', () => {
    const t = DCSDTC_TARGETS.find(x => x.code === 'F16C')!
    const w = JSON.parse(buildDcsDtc(plan('F-16C_50', { waypoints: route() }), t, f16dtc as Json, {}, false, () => null).text).Waypoints.Waypoints
    expect(w[2]).toMatchObject({ UseOA: true, OffsetAimpoint1: { Range: 3000, Bearing: 90, Elevation: 150 }, OffsetAimpoint2: { Range: 6076, Bearing: 0, Elevation: 200 } })
    expect(w[2]).toMatchObject({ UseVRP: true, TGTtoVRP: { Range: 9000, Bearing: 180, Elevation: 100 } })
    expect(w[1]).toMatchObject({ UseVIP: true, VIPtoTGT: { Range: 5, Bearing: 45 }, VIPtoPUP: { Range: 3 } })
    expect(w[0]).toMatchObject({ UseOA: false, OffsetAimpoint1: null, UseVIP: false, VIPtoTGT: null })
  })
})

describe('third verification pass', () => {
  const tg = (c: string) => DCSDTC_TARGETS.find(t => t.code === c)!

  it('moves a MiG-29 preset that clashes on its one radio', () => {
    const out = buildNative(plan('MiG-29 Fulcrum', {
      include_comms: true,
      comm1: [{ preset: 1, freq_mhz: 251, label: '', modulation: 'AM' }],
      comm2: [{ preset: 1, freq_mhz: 127.5, label: '', modulation: 'AM' }],
    }), 'MiG-29 Fulcrum', fulcrum as Json, {}, () => null)
    const radio = data(out.text).Program_1.Radio
    expect(radio.Channel_1.freq).toBe(251)
    expect(radio.Channel_2.freq).toBe(127.5)
    expect(out.warnings.join(' ')).toMatch(/moved to free channels/)
  })

  it('keeps the first of two frequencies on one Apache radio and preset', () => {
    const out = buildNative(plan('AH-64D_BLK_II', {
      include_comms: true,
      comm1: [{ preset: 2, freq_mhz: 251, label: '', modulation: 'AM' }],
      comm2: [{ preset: 2, freq_mhz: 262, label: '', modulation: 'AM' }],
    }), 'AH-64D_BLK_II', apache as Json, {}, () => null)
    expect(data(out.text).Presets.COMM[1].COMMS.UHF.Frequency).toBe(251)
    expect(out.warnings.join(' ')).toMatch(/second frequency/)
  })

  it('never writes a DCS-DTC bearing of 360 and cuts over-long offsets', () => {
    const w = many(3)
    w[1] = { ...w[1], oa: [{ range_ft: 150000, bearing_deg: 359.97, elev_ft: 0 }] }
    const out = buildDcsDtc(plan('F-16C_50', { waypoints: w }), tg('F16C'), f16dtc as Json, {}, false, () => null)
    const oa = JSON.parse(out.text).Waypoints.Waypoints[1].OffsetAimpoint1
    expect(oa).toEqual({ Range: 99999, Bearing: 0, Elevation: 0 })
    expect(out.warnings.join(' ')).toMatch(/longer than the F-16 takes/)
  })
})
