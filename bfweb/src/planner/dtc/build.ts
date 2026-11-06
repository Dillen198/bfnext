// The cartridge's two ways in and how each builds its file. Not in
// DtcWorkspace.tsx, which only exports its component
// (react-refresh/only-export-components).

import { type Json, type Overrides } from '../model'
import { buildNative } from '../native'
import { buildDcsDtc } from '../dcsdtc'
import { ExportError } from '../exports'
import { dcsdtcTarget, type Target } from '../targets'
import { type NativeType, type Plan } from '../types'

export type Msg = { kind: 'ok' | 'warn' | 'err'; text: string }

/** Plain names for the two ways in: nobody should have to tell "DCS DTC"
 *  from "DCS-DTC". */
export const METHOD = {
  native: { name: 'DCS built-in cartridge', blurb: 'Nothing to install. Load it in the cockpit from the DTC page.' },
  dcsdtc: { name: 'DCS-DTC app', blurb: 'A free app types it into the cockpit for you.' },
} as const

/** Build the file for a target; null with a reason when it can't be built yet. */
export function buildFor(plan: Plan, t: Target, defaults: Json, ov: Overrides, tosSeconds: (i: number) => number | null, tosText: (i: number) => string | null, partial = true) {
  if (t.tool === 'native') return buildNative(plan, t.code as NativeType, defaults, ov, tosSeconds)
  const dt = dcsdtcTarget(t.code)
  if (!dt) throw new ExportError('DCS-DTC does not support this aircraft.')
  return buildDcsDtc(plan, dt, defaults, ov, partial, tosText)
}
