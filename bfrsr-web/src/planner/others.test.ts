import { describe, expect, it } from 'vitest'
import { m2kCoord, mirageDtc, thunderAirspace, thunderRadio, viggenIni } from './others'
import { newPlan, type Plan, type PlanWaypoint } from './types'

const wp = (name: string, lat: number, lon: number, extra: Partial<PlanWaypoint> = {}): PlanWaypoint => ({
  name, lat, lon, alt_ft: 10000, elev_ft: 0, target: false, note: '', ...extra,
})
const plan = (over: Partial<Plan> = {}): Plan => ({
  ...newPlan('M-2000C'),
  name: 'Strike Kobuleti',
  waypoints: [wp('Kutaisi', 42.1775, 42.4819), wp('B1', 42.05, 42.2), wp('Bridge', 41.86, 41.78, { target: true }), wp('Kutaisi', 42.1775, 42.4819)],
  ...over,
})

describe('M-2000C cartridge', () => {
  it('writes the MIP coordinate format', () => {
    expect(m2kCoord(41.927433, true)).toBe('N41:55.646')
    expect(m2kCoord(-45.72, false)).toBe('W45:43.200')
    expect(m2kCoord(41 + 59.99999 / 60, true)).toBe('N42:00.000')
  })

  it('writes a Lua cartridge for the map and warns where the BUTs land', () => {
    const out = mirageDtc(plan(), new Date(2026, 9, 26))
    expect(out.filename).toBe('STRIKE KOBULETI.dtc')
    expect(out.text).toContain('terrain = "Caucasus"')
    expect(out.text).toContain('date = "26/10/2026"')
    expect(out.text).toContain('waypoints[3] = { name = "BRIDGE", lat = "N41:51.600", lon = "E41:46.800", alt = 3048 }')
    expect(out.warnings[0]).toMatch(/BUT 11-14/)
  })
})

describe('AJS37 cartridge', () => {
  it('maps departure, route and arrival to LS, B1-Bn and L1', () => {
    const out = viggenIni(plan({ aircraft: 'AJS37' }), i => i * 600)
    expect(out.filename).toBe('CustomCartridge.ini')
    const t = out.text.replace(/\r\n/g, '\n')
    expect(t).toMatch(/\[LS\]\nlatitude = 42\.177500/)
    expect(t).toMatch(/\[B1\]\nlatitude = 42\.050000/)
    expect(t).toMatch(/\[B2\][^[]*missiontime = 1200\n[^[]*etalocked = true\n[^[]*istargetpoint = true/)
    expect(t).toMatch(/\[L1\]/)
    expect(t).not.toMatch(/\[B3\]/)
  })
})

describe('JF-17 files', () => {
  it('draws the route as an air-space line for the side', () => {
    const out = thunderAirspace(plan({ aircraft: 'JF-17' }), 'Red')
    expect(out.filename).toBe('Custom_AirSpace_Caucasus.lua')
    expect(out.text).toContain("airspace['RKYA'] = {")
    expect(out.text).toContain('[3] = { lat = 41.860000, lon = 41.780000 },')
  })

  it('writes radio presets clear of the mission ones', () => {
    const out = thunderRadio(plan({
      comm1: [{ preset: 1, freq_mhz: 251, label: 'AWACS', modulation: 'AM' }],
      comm2: [{ preset: 2, freq_mhz: 127.5, label: '', modulation: 'AM' }],
    }))
    expect(out.text).toContain('presets[22] = 251000000 -- AWACS')
    expect(out.text).toContain('presets[43] = 127500000')
    expect(out.text.trim().endsWith('return presets')).toBe(true)
  })
})
