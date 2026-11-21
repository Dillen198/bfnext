// Writes sample cartridges/presets from the planner's real exporters, so they
// can be fed through DCS's own import code (see the research harnesses).
// Usage: npx vite-node scripts/planner-samples.ts <out-dir>

import { mkdirSync, writeFileSync } from 'node:fs'
import { join } from 'node:path'
import { buildNative } from '../src/planner/native'
import { buildDcsDtc, DCSDTC_TARGETS } from '../src/planner/dcsdtc'
import { newPlan, type Plan, type PlanWaypoint, type NativeType } from '../src/planner/types'
import type { Json, Overrides } from '../src/planner/model'

const out = process.argv[2]
if (!out) throw new Error('usage: planner-samples.ts <out-dir>')
mkdirSync(out, { recursive: true })

const wp = (name: string, lat: number, lon: number, extra: Partial<PlanWaypoint> = {}): PlanWaypoint => ({
  name, lat, lon, alt_ft: 15000, elev_ft: 100, target: false, note: '', ...extra,
})

/** Batumi to Kutaisi past a target with offset aimpoints, a VIP before it
 *  and a VRP on it (the F-16 and Hornet take what they can). */
const route = (): PlanWaypoint[] => [
  wp('BATUMI', 41.6096, 41.6002, { alt_ft: 0, elev_ft: 33 }),
  wp('IP', 42.0, 41.9, {
    vip: { point: { range_ft: 5 * 6076.12, bearing_deg: 40, elev_ft: 450 }, pup: { range_ft: 3 * 6076.12, bearing_deg: 20, elev_ft: 450 } },
  }),
  wp('SA11', 42.2, 42.1, {
    target: true, elev_ft: 450,
    oa: [{ range_ft: 3000, bearing_deg: 90, elev_ft: 460 }, { range_ft: 6000, bearing_deg: 180, elev_ft: 440 }],
    vrp: { point: { range_ft: 9000, bearing_deg: 200, elev_ft: 400 }, pup: { range_ft: 18000, bearing_deg: 220, elev_ft: 400 } },
  }),
  wp('KUTAISI', 42.1772, 42.4807, { alt_ft: 0, elev_ft: 148 }),
]

/** 32 points: a Hornet route that runs across all three sequences. */
const longRoute = (): PlanWaypoint[] =>
  Array.from({ length: 32 }, (_, i) => wp(`P${i + 1}`, 41.65 + i * 0.02, 41.65 + i * 0.025, { target: i === 5 || i === 20 }))

const plan = (aircraft: string, waypoints = route(), name?: string): Plan => ({
  ...newPlan(aircraft),
  name: name ?? `Planner sample ${aircraft}`.replace(/[^A-Za-z0-9 _-]/g, ''),
  callsign: 'Enfield 1',
  start_time: '08:30',
  include_comms: true,
  comm1: [{ preset: 1, freq_mhz: 251, label: 'AWACS', modulation: 'AM' }],
  comm2: [{ preset: 1, freq_mhz: 127.5, label: 'FLT', modulation: 'AM' }],
  waypoints,
})
const tos = (i: number) => 30600 + i * 600
const tosText = (i: number) => {
  const s = tos(i)
  return `${String(Math.floor(s / 3600)).padStart(2, '0')}:${String(Math.floor(s / 60) % 60).padStart(2, '0')}:00`
}

const tomcat = (await import('../src/planner/data/native/F-14BU.defaults.json')).default as { JDAM: { stations: { targets: Record<string, Json>[] }[] } }
const natives: [NativeType, Overrides, Plan?][] = [
  ['F-16C_50', { '/MPD/CMDS/CMDSBingoSettings/ChaffNum': 12 }],
  ['FA-18C_hornet', {}],
  ['FA-18C_hornet', {}, plan('FA-18C_hornet', longRoute(), 'Planner sample FA-18C long')],
  ['AH-64D_BLK_II', { '/MISC/ASE/ASE_Settings/ChaffMode': 2 }],
  // One JDAM target from the plan's target waypoint, one placed on the map.
  ['F-14BU', {
    '/JDAM/stations/0/targets/0/active': true,
    '/JDAM/stations/1/targets/0': { ...tomcat.JDAM.stations[1].targets[0], active: true, x: -262000, y: 618000, elev: 120, attack_heading: 90, drop_alt: 30000, drop_spd: 500 },
  }],
  ['MiG-29 Fulcrum', {}],
]
for (const [type, ov, p] of natives) {
  const defaults = (await import(`../src/planner/data/native/${type.replace(/ /g, '_')}.defaults.json`)).default as Json
  const r = buildNative(p ?? plan(type), type, defaults, ov, tos)
  writeFileSync(join(out, r.filename), r.text)
  console.log('native', type, r.filename, r.warnings)
}

/** Changes that must switch their DCS-DTC upload and item flags on. */
const DCSDTC_CHANGES: Record<string, Overrides> = {
  F16C: { '/CMS/Programs/0/ChaffBurstQty': 4, '/Misc/Bingo': 3000, '/Misc/TGPCode': 1511 },
  FA18C: { '/Misc/Bingo': 4000, '/Misc/TGPCode': 1511 },
}
for (const t of DCSDTC_TARGETS) {
  const defaults = (await import(`../src/planner/data/dcsdtc/${t.code}.defaults.json`)).default as Json
  for (const partial of [false, true]) {
    const r = buildDcsDtc(plan(t.units[0]), t, defaults, DCSDTC_CHANGES[t.code] ?? {}, partial, tosText)
    const name = `dcsdtc-${t.code}-${partial ? 'load' : 'preset'}.json`
    writeFileSync(join(out, name), r.text)
    console.log('dcsdtc', name, r.warnings)
  }
}
const longT = DCSDTC_TARGETS.find(t => t.code === 'FA18C')!
const longD = (await import('../src/planner/data/dcsdtc/FA18C.defaults.json')).default as Json
writeFileSync(join(out, 'dcsdtc-FA18C-long-preset.json'), buildDcsDtc(plan('FA-18C_hornet', longRoute()), longT, longD, {}, false, tosText).text)
