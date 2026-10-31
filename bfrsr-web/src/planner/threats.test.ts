// The RWR threat table editor's helpers, and that a reorder reaches the
// exported cartridge as a clean 1..N ranking.
import { describe, expect, it } from 'vitest'
import { buildNative } from './native'
import { applyOverrides, withOverride, type Json, type JsonObject, type Overrides } from './model'
import { newPlan } from './types'
import { isThreatTable, liftToTop, moveTo, renumber, threatKind, threatRows } from './dtc/threats'
import f16 from './data/native/F-16C_50.defaults.json'
import hornet from './data/native/FA-18C_hornet.defaults.json'
import apache from './data/native/AH-64D_BLK_II.defaults.json'

type Obj = Record<string, Record<string, Json>>
const rwr = (f16 as unknown as { ELINT: { RWR: Obj } }).ELINT.RWR

describe('threat tables', () => {
  it('finds every RWR table and nothing else', () => {
    expect(isThreatTable(rwr.HIGH as Json)).toBe(true)
    expect(isThreatTable(rwr.LOW as Json)).toBe(true)
    for (const k of ['NORM', 'AI', 'AAA', 'UNK', 'FRND']) expect(isThreatTable((hornet as unknown as { ALR67: { RWR: Obj } }).ALR67.RWR[k] as Json)).toBe(true)
    const misc = (apache as unknown as { MISC: Record<string, Json> }).MISC
    expect(isThreatTable(misc.RLWR)).toBe(true)
    expect(isThreatTable(misc.RFI)).toBe(true)
    expect(isThreatTable((f16 as unknown as { COMM: Json }).COMM)).toBe(false)
  })

  it('sorts threats into kinds a pilot knows', () => {
    expect(threatKind("SAM SA-10 'Grumble' - Flap Lid")).toBe('SAM')
    expect(threatKind('SPAAA ZSU-23-4 - Gun Dish')).toBe('AAA')
    expect(threatKind('AIM-120')).toBe('Missiles')
    expect(threatKind('EWR 55G6')).toBe('EWR & AWACS')
    expect(threatKind('E-3')).toBe('EWR & AWACS')
    expect(threatKind('Ticonderoga guided missile cruiser')).toBe('Ships')
    expect(threatKind('MiG-29/Su-27/Su-33/J-11A')).toBe('Aircraft')
    expect(threatKind('Unknown radar')).toBe('Other')
    const rows = threatRows(rwr.HIGH as Json, undefined)
    expect(rows.map(r => r.pri)).toEqual(rows.map((_, i) => i + 1))
  })

  it('keeps the ranks 1..N with no gaps or repeats', () => {
    const rows = threatRows(rwr.HIGH as Json, undefined)
    const sams = rows.filter(r => r.kind === 'SAM').map(r => r.key)
    for (const order of [moveTo(rows, rows[50].key, 0), moveTo(rows, rows[0].key, 94), liftToTop(rows, sams)]) {
      expect(order).toHaveLength(95)
      expect(new Set(order.map(String)).size).toBe(95)
      const pri = new Map(rows.map(r => [String(r.key), r.pri]))
      for (const [k, n] of renumber(rows, order)) pri.set(String(k), n)
      expect([...pri.values()].sort((a, b) => a - b)).toEqual(rows.map((_, i) => i + 1))
    }
    expect(liftToTop(rows, sams).slice(0, sams.length)).toEqual(sams)
  })

  it('exports a reordered table whole, ranked 1..95', () => {
    const rows = threatRows(rwr.HIGH as Json, undefined)
    const sa10 = rows.find(r => r.name.includes('Flap Lid'))!
    let ov: Overrides = {}
    for (const [k, n] of renumber(rows, moveTo(rows, sa10.key, 0))) ov = withOverride(f16 as Json, ov, ['ELINT', 'RWR', 'HIGH', k, 'PRI'], n)
    ov = withOverride(f16 as Json, ov, ['ELINT', 'RWR', 'HIGH', 'E-3', 'display'], false)
    const value = applyOverrides(f16 as Json, ov) as unknown as { ELINT: { RWR: Obj } }
    expect((value.ELINT.RWR.HIGH[String(sa10.key)] as JsonObject).PRI).toBe(1)

    const plan = { ...newPlan('F-16C_50'), waypoints: [{ name: 'A', lat: 42, lon: 42, alt_ft: 20000, elev_ft: 0, target: false, note: '' }], systems: { 'native:F-16C_50': ov } }
    const out = JSON.parse(buildNative(plan, 'F-16C_50', f16 as Json, ov, () => null).text) as { data: { ELINT: { RWR: Obj } } }
    const high = out.data.ELINT.RWR.HIGH
    expect(Object.keys(high)).toHaveLength(95)
    expect(Object.keys(out.data.ELINT.RWR.LOW)).toHaveLength(95)
    expect(Object.values(high).map(r => (r as JsonObject).PRI as number).sort((a, b) => a - b)).toEqual(rows.map((_, i) => i + 1))
    expect((high[String(sa10.key)] as JsonObject).PRI).toBe(1)
    expect((high['E-3'] as JsonObject).display).toBe(false)
  })
})
