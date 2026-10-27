// "From the mission": what each cartridge takes from the campaign, and the
// typed-threat rings the exporter must write.
import { describe, expect, it } from 'vitest'
import { AnnotationIndex, getAt, normaliseAnnotations, withOverride, type Json, type JsonObject, type Overrides } from './model'
import { terrainNamed } from './nav'
import { buildNative } from './native'
import { commsFromCard, missionFills, type MissionData } from './dtc/mission'
import type { Ctx } from './dtc/core'
import { newPlan, type Plan } from './types'
import f16 from './data/native/F-16C_50.defaults.json'
import f16a from './data/native/F-16C_50.annotations.json'
import hornet from './data/native/FA-18C_hornet.defaults.json'
import hornetA from './data/native/FA-18C_hornet.annotations.json'
import apache from './data/native/AH-64D_BLK_II.defaults.json'
import apacheA from './data/native/AH-64D_BLK_II.annotations.json'
import tomcat from './data/native/F-14BU.defaults.json'
import tomcatA from './data/native/F-14BU.annotations.json'

const map = terrainNamed('Caucasus')!
const ctxOf = (defaults: unknown, ann: unknown): Ctx => ({
  idx: new AnnotationIndex(normaliseAnnotations(ann as never)), defaults: defaults as Json, overrides: {},
  value: defaults as Json, readOnly: false, hidden: new Set<string>(), points: [], set: () => {},
})
const target = (code: string) => ({ key: `native:${code}`, tool: 'native' as const, code, label: code })

const mission: MissionData = {
  side: 'Blue',
  comms: [
    { preset: 1, label: 'Tower', freq_mhz: 251, modulation: 'AM', live: false },
    { preset: 2, label: 'AWACS Darkstar', freq_mhz: 264.5, modulation: 'AM', live: true },
    { preset: 1, label: 'Flight', freq_mhz: 124, modulation: 'AM', live: false },
    { preset: 3, label: 'JTAC', freq_mhz: 31.5, modulation: 'FM', live: false },
    { label: 'Guard', freq_mhz: 243, modulation: 'AM', live: false },
  ],
  threats: [
    { label: '4x air defence', lat: 42.86, lon: 41.12, radius_m: 35000, uncertainty_m: 800, confidence: 0.8, source: 'recon', age_s: 10, count: 4 },
    { label: 'air-defence emitter', lat: 42.3, lon: 41.8, uncertainty_m: 3000, confidence: 0.4, source: 'ewr', age_s: 100, count: 1 },
  ],
  objectives: [
    { name: 'Kutaisi', kind: 'Airbase', owner: 'Blue', lat: 42.18, lon: 42.48, health: 100, logi: 100, threatened: false, captureable: false, priority: false, primary: true },
    { name: 'Lentekhi Outpost', kind: 'FOB', owner: 'Blue', lat: 42.79, lon: 42.72, health: 100, logi: 100, threatened: false, captureable: false, priority: false, primary: false },
    { name: 'Gori FARP', kind: 'FARP', owner: 'Blue', lat: 41.98, lon: 44.1, health: 100, logi: 100, threatened: false, captureable: false, priority: false, primary: false },
    { name: 'Sukhumi', kind: 'Airbase', owner: 'Red', lat: 42.86, lon: 41.12, health: 100, logi: 100, threatened: false, captureable: false, priority: false, primary: true },
  ],
  navaids: [
    { objective: 'Kutaisi', kind: 'Airbase', deck: null, lat: 42.18, lon: 42.48, tacan: '44X KTS', ndb_khz: 477, icls: null, link4_mhz: null, acls: false, brc: null },
    { objective: 'Batumi', kind: 'Airbase', deck: null, lat: 41.6, lon: 41.6, tacan: null, ndb_khz: 430, icls: null, link4_mhz: null, acls: false, brc: null },
    { objective: 'CVN', kind: 'Carrier', deck: 'CVN74', lat: 41, lon: 40, tacan: '74X CVN', ndb_khz: null, icls: 1, link4_mhz: 336, acls: true, brc: 0 },
  ],
}
const plan: Plan = { ...newPlan('F-16C_50'), waypoints: [{ name: 'A', lat: 42.2, lon: 42.4, alt_ft: 20000, elev_ft: 0, target: false, note: '' }] }

describe('fill from the mission', () => {
  it('splits the comms card into COMM 1 and COMM 2 by band, numbered presets only', () => {
    const { comm1, comm2 } = commsFromCard(mission.comms)
    expect(comm1.map(c => [c.preset, c.freq_mhz])).toEqual([[1, 251], [2, 264.5]])
    expect(comm2.map(c => [c.preset, c.freq_mhz, c.modulation])).toEqual([[1, 124, 'AM'], [3, 31.5, 'FM']])
  })

  it('offers the F-16 presets, threats and destination points', () => {
    const fills = missionFills(ctxOf(f16, f16a), target('F-16C_50'), plan, map, mission)
    expect(fills.map(f => f.id)).toEqual(['comms', 'threats', 'dest'])
    const threats = fills[1].writes![0][1] as JsonObject[]
    expect(threats.map(t => [t.id, t.def_num, t.threatName, t.radius, t.text, t.ring])).toEqual([
      ['THREAT_PTS56', 1, 'Custom', 35000, '4AD', true],
      ['THREAT_PTS57', 1, 'Custom', 37040, 'AD', true],
    ])
    expect(threats[0].x).not.toBe(0)
    // Own bases only: airfield first, then the FARP, then the FOB.
    const dest = fills[2].writes![0][1] as JsonObject[]
    expect(dest.map(d => [d.id, d.text, d.note])).toEqual([
      ['DEST81', 'KUT', 'Kutaisi'], ['DEST82', 'GOR', 'Gori FARP'], ['DEST83', 'LEN', 'Lentekhi Outpost'],
    ])
  })

  it('fills Hornet MEZ rings in NM and Apache ADF presets from NDBs', () => {
    const h = missionFills(ctxOf(hornet, hornetA), target('FA-18C_hornet'), plan, map, mission)
    expect(h.map(f => f.id)).toEqual(['comms', 'threats'])
    const mez = h[1].writes![0][1] as JsonObject[]
    expect(mez.map(t => [t.id, t.num, t.threat_type, t.threat_ring_radius, t.text])).toEqual([
      ['MEZ_THRTS_1', 1, 'Custom', 18.9, '4AD'], ['MEZ_THRTS_2', 2, 'Custom', 20, 'AD'],
    ])
    const a = missionFills(ctxOf(apache, apacheA), target('AH-64D_BLK_II'), plan, map, mission)
    const adf = a.find(f => f.id === 'adf')!.writes!
    expect(adf).toEqual([
      [['NAV', 'Mission_1', 'ADF', 'Preset_1'], { Freq: 477, ID: 'KTS' }],
      [['NAV', 'Mission_1', 'ADF', 'Preset_2'], { Freq: 430, ID: 'BAT' }],
      [['NAV', 'Mission_1', 'ADF', 'isEnabled'], true],
    ])
    expect(missionFills(ctxOf(tomcat, tomcatA), target('F-14BU'), plan, map, mission)).toEqual([])
  })

  it('knows when the cartridge already says it', () => {
    const ctx = ctxOf(f16, f16a)
    const [comms, threats] = missionFills(ctx, target('F-16C_50'), plan, map, mission)
    expect(comms.done).toBe(false)
    const filled = { ...plan, ...comms.comms!, include_comms: true }
    const value = JSON.parse(JSON.stringify(f16)) as Json
    ;(getAt(value, ['MPD']) as JsonObject).THREAT_PTS = threats.writes![0][1]
    const again = missionFills({ ...ctx, value }, target('F-16C_50'), filled, map, mission)
    expect(again.map(f => f.done)).toEqual([true, true, false])
  })
})

describe('typed threats in the export', () => {
  it('gives a typed F-16 threat its own exact ring and a Hornet MEZ its range', () => {
    let ov: Overrides = {}
    ov = withOverride(f16 as Json, ov, ['MPD', 'THREAT_PTS'], [
      { number: 1, id: 'THREAT_PTS56', x: -280000, y: 640000, def_num: 19, threatName: 'Custom', radius: 5000, alt: 100, elev: 0, text: '', ring: true },
      { number: 2, id: 'THREAT_PTS57', x: -281000, y: 641000, def_num: 1, threatName: 'Custom', radius: 5000, alt: 100, elev: 0, text: 'X', ring: true },
    ])
    const out = JSON.parse(buildNative({ ...plan, systems: { 'native:F-16C_50': ov } }, 'F-16C_50', f16 as Json, ov, () => null).text)
    const [sa10, custom] = out.data.MPD.THREAT_PTS
    expect(sa10).toMatchObject({ threatName: "SAM SA-10 'Grumble'", radius: 120000, alt: 27000, text: '10' })
    expect(custom).toMatchObject({ threatName: 'Custom', radius: 5000, alt: 100, text: 'X' })

    const hp = { ...newPlan('FA-18C_hornet'), waypoints: plan.waypoints }
    let hv: Overrides = {}
    hv = withOverride(hornet as Json, hv, ['SA', 'MEZ_THRTS'], [
      { id: 'MEZ_THRTS_1', num: 1, x: -280000, y: 640000, text: '', threat_type: "SAM SA-11 'Gadfly'", threat_ring_radius: 3, threat_level: 1 },
    ])
    const h = JSON.parse(buildNative(hp, 'FA-18C_hornet', hornet as Json, hv, () => null).text)
    expect(h.data.SA.MEZ_THRTS[0].threat_ring_radius).toBeCloseTo(50000 / 1852, 6)
    expect(h.data.SA.MEZ_THRTS[0].text).toBe('11')
  })
})
