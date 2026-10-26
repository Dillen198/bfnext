// Writes sample cartridges/presets from the planner's real exporters, so they
// can be fed through DCS's own import code (see the research harnesses).
// Usage: npx vite-node scripts/planner-samples.ts <out-dir>

import { mkdirSync, writeFileSync } from 'node:fs'
import { join } from 'node:path'
import { buildNative } from '../src/planner/native'
import { buildDcsDtc, DCSDTC_TARGETS } from '../src/planner/dcsdtc'
import { newPlan, type Plan, type PlanWaypoint, type NativeType } from '../src/planner/types'
import type { Json } from '../src/planner/model'

const out = process.argv[2]
if (!out) throw new Error('usage: planner-samples.ts <out-dir>')
mkdirSync(out, { recursive: true })

const wp = (name: string, lat: number, lon: number, extra: Partial<PlanWaypoint> = {}): PlanWaypoint => ({
  name, lat, lon, alt_ft: 15000, elev_ft: 100, target: false, note: '', ...extra,
})
const plan = (aircraft: string): Plan => ({
  ...newPlan(aircraft),
  name: `Planner sample ${aircraft}`.replace(/[^A-Za-z0-9 _-]/g, ''),
  callsign: 'Enfield 1',
  start_time: '08:30',
  include_comms: true,
  comm1: [{ preset: 1, freq_mhz: 251, label: 'AWACS', modulation: 'AM' }],
  comm2: [{ preset: 1, freq_mhz: 127.5, label: 'FLT', modulation: 'AM' }],
  waypoints: [
    wp('BATUMI', 41.6096, 41.6002, { alt_ft: 0, elev_ft: 33 }),
    wp('IP', 42.0, 41.9),
    wp('SA11', 42.2, 42.1, { target: true, elev_ft: 450 }),
    wp('KUTAISI', 42.1772, 42.4807, { alt_ft: 0, elev_ft: 148 }),
  ],
})
const tos = (i: number) => 30600 + i * 600
const tosText = (i: number) => {
  const s = tos(i)
  return `${String(Math.floor(s / 3600)).padStart(2, '0')}:${String(Math.floor(s / 60) % 60).padStart(2, '0')}:00`
}

const natives: [NativeType, Record<string, Json>][] = [
  ['F-16C_50', { '/MPD/CMDS/CMDSBingoSettings/ChaffNum': 12 }],
  ['FA-18C_hornet', {}],
  ['AH-64D_BLK_II', { '/MISC/ASE/ASE_Settings/ChaffMode': 2 }],
  ['F-14BU', { '/JDAM/stations/0/targets/0/active': true }],
  ['MiG-29 Fulcrum', {}],
]
for (const [type, ov] of natives) {
  const defaults = (await import(`../src/planner/data/native/${type}.defaults.json`)).default as Json
  const r = buildNative(plan(type), type, defaults, ov, tos)
  writeFileSync(join(out, r.filename), r.text)
  console.log('native', type, r.filename, r.warnings)
}

for (const t of DCSDTC_TARGETS) {
  const defaults = (await import(`../src/planner/data/dcsdtc/${t.code}.defaults.json`)).default as Json
  for (const partial of [false, true]) {
    const r = buildDcsDtc(plan(t.units[0]), t, defaults, {}, partial, tosText)
    const name = `dcsdtc-${t.code}-${partial ? 'load' : 'preset'}.json`
    writeFileSync(join(out, name), r.text)
    console.log('dcsdtc', name, r.warnings)
  }
}
