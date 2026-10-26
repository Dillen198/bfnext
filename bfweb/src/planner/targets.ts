// Export targets: one per (tool, aircraft). Each target's defaults and field
// annotations are bundled per aircraft and fetched only when a pilot opens
// that aircraft -- the native F-16 defaults alone are ~300 KB.

import { normaliseAnnotations, type Annotations, type Json } from './model'
import { aircraftDef, type AircraftDef } from './types'
import { DCSDTC_TARGETS, type DcsDtcTarget } from './dcsdtc'

export type Tool = 'native' | 'dcsdtc'

export interface Target {
  /** Key into Plan.systems: "native:F-16C_50", "dcsdtc:F16C". */
  key: string
  tool: Tool
  /** Native DCS type or DCS-DTC code. */
  code: string
  label: string
}

export interface TargetModel {
  defaults: Json
  annotations: Annotations
}

type Loader = () => Promise<unknown>
const dcsdtcFiles = import.meta.glob('./data/dcsdtc/*.json') as Record<string, Loader>
const nativeFiles = import.meta.glob('./data/native/*.json') as Record<string, Loader>

const unwrap = (m: unknown) => ((m as { default?: unknown }).default ?? m) as never

/** Targets the aircraft can export to, native cartridge first. */
export function targetsFor(a: AircraftDef): Target[] {
  const out: Target[] = []
  if (a.native) out.push({ key: `native:${a.native}`, tool: 'native', code: a.native, label: 'DCS cartridge (.dtc)' })
  if (a.dcsdtc) out.push({ key: `dcsdtc:${a.dcsdtc}`, tool: 'dcsdtc', code: a.dcsdtc, label: 'DCS-DTC preset' })
  return out
}

export function targetsForUnit(unit: string): Target[] {
  return targetsFor(aircraftDef(unit))
}

export function dcsdtcTarget(code: string): DcsDtcTarget | undefined {
  return DCSDTC_TARGETS.find(t => t.code === code)
}

/** Whether the target's data files are in this build. */
export function hasModel(t: Target): boolean {
  const files = t.tool === 'dcsdtc' ? dcsdtcFiles : nativeFiles
  return `./data/${t.tool}/${t.code}.defaults.json` in files && `./data/${t.tool}/${t.code}.annotations.json` in files
}

const cache = new Map<string, Promise<TargetModel>>()

export function loadModel(t: Target): Promise<TargetModel> {
  let p = cache.get(t.key)
  if (!p) {
    const files = t.tool === 'dcsdtc' ? dcsdtcFiles : nativeFiles
    const d = files[`./data/${t.tool}/${t.code}.defaults.json`]
    const a = files[`./data/${t.tool}/${t.code}.annotations.json`]
    if (!d || !a) return Promise.reject(new Error(`No data for ${t.key}`))
    p = Promise.all([d(), a()]).then(([dm, am]) => ({ defaults: unwrap(dm), annotations: normaliseAnnotations(unwrap(am)) }))
    cache.set(t.key, p)
  }
  return p
}
