import { describe, expect, it } from 'vitest'
import { buildNative } from './native'
import { newPlan, type Plan, type PlanWaypoint } from './types'
import type { Json } from './model'
import f16 from './data/native/F-16C_50.defaults.json'
import hornet from './data/native/FA-18C_hornet.defaults.json'
import apache from './data/native/AH-64D_BLK_II.defaults.json'
import tomcat from './data/native/F-14BU.defaults.json'
import fulcrum from './data/native/MiG-29_Fulcrum.defaults.json'
import { lar } from './f14lar'

// Each check below is an item from the exporter checklists, which were
// verified by running DCS's own descriptors (setData/returnData) under its
// Lua interpreter.

const wp = (name: string, lat: number, lon: number, extra: Partial<PlanWaypoint> = {}): PlanWaypoint => ({
  name, lat, lon, alt_ft: 15000, elev_ft: 100, target: false, note: '', ...extra,
})
const route = [wp('BATUMI', 41.6096, 41.6002), wp('IP', 42.0, 41.9), wp('SA11', 42.2, 42.1, { target: true, elev_ft: 450 })]
const plan = (aircraft: string, over: Partial<Plan> = {}): Plan => ({ ...newPlan(aircraft), name: 'SEAD north', waypoints: route, ...over })
const noTime = () => null

type Obj = Record<string, unknown>
const file = (text: string) => JSON.parse(text) as { name: string; type: string; data: Obj }

/** No nulls, NaNs or holes anywhere: DCS's JSON reader drops nulls and shifts arrays. */
function clean(v: unknown, path = ''): string[] {
  if (v === null) return [path]
  if (typeof v === 'number' && !Number.isFinite(v)) return [path]
  if (Array.isArray(v)) return v.flatMap((x, i) => clean(x, `${path}/${i}`))
  if (typeof v === 'object') return Object.entries(v as Obj).flatMap(([k, x]) => clean(x, `${path}/${k}`))
  return []
}

describe('native F-16C cartridge', () => {
  const out = buildNative(plan('F-16C_50'), 'F-16C_50', f16 as Json, {}, noTime)
  const f = file(out.text)
  const d = f.data as { MPD: Obj; COMM: Obj; ELINT: { RWR: Obj }; type: string; terrain: string }

  it('carries name, type and the map at both levels', () => {
    expect(f.type).toBe('F-16C_50')
    expect(f.name).toBe('SEAD north')
    expect(d.type).toBe('F-16C_50')
    expect(d.terrain).toBe('Caucasus')
    expect(d.MPD.terrain).toBe('Caucasus')
    expect(out.filename).toBe('SEAD north.dtc')
  })

  it('writes the route as steerpoints 1..n and switches NAV on', () => {
    const pts = d.MPD.NAV_PTS as Obj[]
    expect(pts.map(p => p.id)).toEqual(['STPT1', 'STPT2', 'STPT3'])
    expect(pts[2]).toMatchObject({ number: 3, type: 'TGT', R1: true, R2: false, isTOSEnabled: true, FIX_Time: false, TOS: -1 })
    expect(pts[2].alt as number).toBeCloseTo(450 * 0.3048, 6)
    expect(pts[2].routeAltitude as number).toBeCloseTo(15000 * 0.3048, 6)
    expect(d.MPD.mirror_NAV_PTS).toBe(false)
    // Untouched tabs stay "do not upload".
    expect(d.MPD.mirror_DEST).toBe(true)
    expect(d.COMM.mirror_COMM1).toBe(true)
  })

  it('keeps the wholesale-replaced tables complete', () => {
    expect(Object.keys(d.COMM.COMM1 as Obj).filter(k => k.startsWith('Channel_'))).toHaveLength(20)
    expect(Object.keys(d.COMM.COMM2 as Obj).filter(k => k.startsWith('Channel_'))).toHaveLength(20)
    expect(d.ELINT.RWR.HIGH).toBeTruthy()
    expect(d.ELINT.RWR.LOW).toBeTruthy()
    expect(clean(f.data)).toEqual([])
  })

  it('switches on a tab the pilot changed and writes plan radios', () => {
    const o = file(buildNative(
      plan('F-16C_50', { include_comms: true, comm1: [{ preset: 2, freq_mhz: 251.5, label: '', modulation: 'AM' }] }),
      'F-16C_50', f16 as Json, { '/MPD/DEST': [{ number: 1, id: 'DEST81', x: 0, y: 0, alt: 0, text: 'FRP', note: '' }] }, noTime,
    ).text).data as { MPD: Obj; COMM: Obj }
    expect(o.MPD.mirror_DEST).toBe(false)
    expect(o.COMM.mirror_COMM1).toBe(false)
    expect(o.COMM.mirror_COMM2).toBe(true)
    expect((o.COMM.COMM1 as Obj).Channel_2).toEqual({ freq: 251.5, modulation: 1 })
  })

  it('fixes TOS when the plan is timed', () => {
    const o = file(buildNative(plan('F-16C_50'), 'F-16C_50', f16 as Json, {}, i => 30600 + i * 600).text).data as { MPD: { NAV_PTS: Obj[] } }
    expect(o.MPD.NAV_PTS[1]).toMatchObject({ TOS: 31200, FIX_Time: true })
  })

  it('refuses a route that leaves the map, or no route', () => {
    const crossing = [wp('A', 41.6096, 41.6002), wp('B', 34.5, 36.5)]
    expect(() => buildNative(plan('F-16C_50', { waypoints: crossing }), 'F-16C_50', f16 as Json, {}, noTime)).toThrow(/leaves the Caucasus/)
    expect(() => buildNative(plan('F-16C_50', { waypoints: [] }), 'F-16C_50', f16 as Json, {}, noTime)).toThrow(/no waypoints/)
  })

  it('warns about a target at sea level', () => {
    const out = buildNative(plan('F-16C_50', { waypoints: [wp('T', 41.6096, 41.6002, { target: true, elev_ft: 0 })] }), 'F-16C_50', f16 as Json, {}, noTime)
    expect(out.warnings[0]).toMatch(/elevation 0 ft/)
  })

  it('refuses more than 25 steerpoints', () => {
    const many = Array.from({ length: 26 }, (_, i) => wp(`P${i}`, 41.6 + i * 0.01, 41.6))
    expect(() => buildNative(plan('F-16C_50', { waypoints: many }), 'F-16C_50', f16 as Json, {}, noTime)).toThrow(/25/)
  })
})

describe('native F/A-18C cartridge', () => {
  const f = file(buildNative(plan('FA-18C_hornet'), 'FA-18C_hornet', hornet as Json, {}, noTime).text)
  const w = (f.data as { WYPT: Obj }).WYPT

  it('writes waypoints 1..n in sequence 1 with a route plan', () => {
    const pts = w.NAV_PTS as Obj[]
    expect(pts.map(p => [p.wypt_num, p.R1, p.R1_order])).toEqual([[1, true, 1], [2, true, 2], [3, true, 3]])
    const routes = w.NAV_ROUTE as Obj[]
    expect(routes).toHaveLength(3)
    expect(routes[0].STPT3).toMatchObject({ route_num: 1, wypt_num: 3, TGT: true, FIX_Time: false })
    expect(w.mirror_NAV_PTS).toBe(false)
    expect(w.terrain).toBe('Caucasus')
    expect(clean(f.data)).toEqual([])
  })
})

describe('native AH-64D cartridge', () => {
  const f = file(buildNative(plan('AH-64D_BLK_II', { callsign: 'Lobo 1' }), 'AH-64D_BLK_II', apache as Json, {}, noTime).text)
  const m1 = (f.data as { NAV: { Mission_1: { Points: Record<string, Obj>; Routes: Obj[] } } }).NAV.Mission_1

  it('writes WPTHZ and TGT points and route 1 over them', () => {
    const wpthz = m1.Points.WPTHZ.POINTS as Obj[]
    expect(wpthz.map(p => [p.num, p.id, p.text])).toEqual([[1, 6, 'BAT'], [2, 6, 'IP'], [3, 6, 'SA1']])
    expect(m1.Points.WPTHZ.isEnabled).toBe(true)
    expect(m1.Points.TGT.POINTS as Obj[]).toHaveLength(1)
    expect((m1.Points.TGT.POINTS as Obj[])[0]).toMatchObject({ num: 1, id: 1, text: 'T01' })
    expect(m1.Points.TGT.isEnabled).toBe(true)
    // CTRLM untouched stays off.
    expect(m1.Points.CTRLM.isEnabled).toBe(false)
    expect(m1.Routes).toHaveLength(10)
    const legs = m1.Routes[0].POINTS as Obj[]
    expect(m1.Routes[0]).toMatchObject({ isEnabled: true, Name: 'LOBO1' })
    expect(legs.map(l => l.num)).toEqual([1, 2, 3])
    expect(legs[0]).toMatchObject({ dist: 0, eta: 0, fix: false })
    expect(legs[1].dist as number).toBeGreaterThan(40000)
    expect(legs[1].eta as number).toBeCloseTo((legs[1].dist as number) / (420 * 0.514), 6)
    expect(clean(f.data)).toEqual([])
  })

  it('switches on ASE when the pilot changes it', () => {
    const d = file(buildNative(plan('AH-64D_BLK_II'), 'AH-64D_BLK_II', apache as Json, { '/MISC/ASE/ASE_Settings/ChaffMode': 2 }, noTime).text).data as { MISC: { ASE: Obj } }
    expect(d.MISC.ASE.isEnabled).toBe(true)
  })
})


describe('native F-14B(U) cartridge', () => {
  it('ports the JDAM release zone exactly', () => {
    const l = lar(20000, 450)
    expect(l.lar_rmin_nmi).toBeCloseTo(1.5712031862492, 9)
    expect(l.lar_rmax_nmi).toBeCloseTo(7.2509806211745, 9)
    expect(l.lar_half_angle_deg).toBeCloseTo(44.983734395416, 9)
  })

  it('writes the route as flight plan 2 with no terrain key', () => {
    const f = file(buildNative(plan('F-14BU'), 'F-14BU', tomcat as Json, {}, noTime).text)
    const d = f.data as { NAV: Obj[]; terrain?: string; CMDS: Obj; TIS: Obj; type: string }
    expect(f.type).toBe('F-14BU')
    expect(d.terrain).toBeUndefined()
    expect(d.NAV).toHaveLength(12)
    expect(d.NAV[0].waypoints).toEqual([])
    const pts = d.NAV[1].waypoints as Obj[]
    expect(d.NAV[1].name).toBe('SEAD north')
    expect(pts.map(p => p.name)).toEqual(['BATUM', 'IP', 'SA11'])
    expect(pts[2]).toMatchObject({ elev: 450, spd: 420, tot: '', lat: 42.2, lon: 42.1 })
    expect(d.CMDS).toBeTruthy()
    expect(d.TIS).toBeTruthy()
    expect(clean(f.data)).toEqual([])
  })

  it('places an active JDAM target on the plan target with its release zone', () => {
    const f = file(buildNative(plan('F-14BU'), 'F-14BU', tomcat as Json,
      { '/JDAM/stations/0/targets/0/active': true }, noTime).text)
    const t = ((f.data as { JDAM: { stations: { targets: Obj[] }[] } }).JDAM.stations[0].targets[0])
    expect(t.lat).toBe(42.2)
    expect(typeof t.x).toBe('number')
    expect(t.lar_rmax_nmi as number).toBeGreaterThan(t.lar_rmin_nmi as number)
    expect(typeof t.launch_x).toBe('number')
  })
})

describe('native MiG-29 cartridge', () => {
  it('fills program 1 and switches NAV on', () => {
    const f = file(buildNative(plan('MiG-29 Fulcrum'), 'MiG-29 Fulcrum', fulcrum as Json, {}, noTime).text)
    const d = f.data as { Program_1: Obj; mirror_NAV: boolean; mirror_Radio: boolean; terrain: string; SelectedProgram: string }
    expect(f.type).toBe('MiG-29 Fulcrum')
    expect(d.terrain).toBe('Caucasus')
    expect(d.mirror_NAV).toBe(false)
    expect(d.mirror_Radio).toBe(true)
    expect(d.SelectedProgram).toBe('Program_1')
    const p = d.Program_1
    expect((p.Points as Obj[]).map(x => x.id)).toEqual(['PNT1', 'PNT2', 'PNT3'])
    expect((p.Waypoints as Obj[]).map(x => x.num)).toEqual(['WPT1'])
    expect((p.Route as Obj[]).map(x => x.num)).toEqual(['PNT1', 'WPT1', 'PNT3'])
    expect(p.TargetPoint).toMatchObject({ name: 'PNT3' })
    expect(p.WeaponSettings).toEqual({ trainingMode: false })
    expect(clean(f.data)).toEqual([])
  })
})
