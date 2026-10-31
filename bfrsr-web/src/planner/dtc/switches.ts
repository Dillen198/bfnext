// "Load into the jet" switches. Every cartridge format has per-section flags
// that decide whether a section reaches the aircraft: DCS's cartridges have
// `mirror_*` ("do not upload", default on) or the Apache's `isEnabled`;
// DCS-DTC has `Upload.<system>` plus `*ToBeUpdated` per item. Pilots should
// never have to know any of that: a section they change, or that the route
// fills, switches itself on. These helpers are the one place that knows the
// mapping, for the exporters and the editor alike.

import { fromPointer, getAt, setAt, toPointer, type Json, type JsonObject, type Overrides } from '../model'
import { TABS } from '../native'
import type { NativeType } from '../types'
import type { Target } from '../targets'

export interface LoadSwitch {
  /** JSON Pointer of the flag. */
  flag: string
  /** The flag value that means "loads into the jet". */
  load: boolean
  /** Sections (pointers) this switch governs. */
  covers: string[]
}

const under = (p: string, root: string) => p === root || p.startsWith(root + '/')

/** Does DCS-DTC's Upload key `u` govern system `k`? ("MFDs" covers "MFD",
 *  "HARMHTS" covers "HARM" and "HTS".) */
function uploadCovers(u: string, k: string): boolean {
  return u === k || u === `${k}s` || (k.length >= 3 && u.includes(k) && !u.startsWith('Delete'))
}

export function switchesFor(t: Target, defaults: Json): LoadSwitch[] {
  if (t.tool === 'native') {
    const byFlag = new Map<string, LoadSwitch>()
    for (const tab of TABS[t.code as NativeType] ?? []) {
      const sw = byFlag.get(tab.flag) ?? { flag: tab.flag, load: tab.load ?? false, covers: [] }
      sw.covers.push(tab.path)
      byFlag.set(tab.flag, sw)
    }
    return [...byFlag.values()]
  }
  const upload = getAt(defaults, ['Upload'])
  if (!upload || typeof upload !== 'object' || Array.isArray(upload)) return []
  const systems = Object.keys(defaults as JsonObject).filter(k => k !== 'Upload')
  return Object.entries(upload)
    .filter(([, v]) => typeof v === 'boolean')
    .map(([u]) => ({ flag: `/Upload/${u}`, load: true, covers: systems.filter(k => uploadCovers(u, k)).map(k => `/${k}`) }))
}

/** The switch for a section, if any covers it (or sits inside it). */
export function switchFor(switches: LoadSwitch[], pointer: string): LoadSwitch | undefined {
  return switches.find(s => s.covers.some(c => under(pointer, c) || under(c, pointer)))
}

/** Pointers of every switch flag: the editor hides them as raw fields. */
export function flagPointers(switches: LoadSwitch[]): Set<string> {
  return new Set(switches.map(s => s.flag))
}

/**
 * DCS-DTC per-item flags: a changed CMS program needs its `ToBeUpdated`, a
 * changed bingo its `BingoToBeUpdated`, and so on. For each changed path,
 * walk its ancestors and switch on the flag that governs it, unless the
 * pilot set that flag. Mutates `cfg`.
 */
export function autoItemFlags(cfg: JsonObject, changed: string[], ov: Overrides): void {
  // Fields whose flag is not named after them.
  const alias = (field: string, prefix: string) =>
    prefix === 'LaserSettings' && /^(TGP|LST|Laser)/.test(field)
  for (const p of changed) {
    const segs = fromPointer(p)
    for (let d = 0; d < segs.length; d++) {
      const anc = getAt(cfg, segs.slice(0, d))
      if (!anc || typeof anc !== 'object' || Array.isArray(anc)) continue
      const child = segs[d]
      for (const k of Object.keys(anc)) {
        if (!k.endsWith('ToBeUpdated') || typeof anc[k] !== 'boolean') continue
        const prefix = k.slice(0, -'ToBeUpdated'.length)
        const governs = prefix === '' ? d < segs.length - 1 || child !== k : child.startsWith(prefix) || alias(child, prefix)
        const flag = toPointer([...segs.slice(0, d), k])
        if (governs && !(flag in ov)) setAt(cfg, fromPointer(flag), true)
      }
    }
  }
}

/** Switch every Upload flag on whose systems were changed, unless set by hand. */
export function autoUploadFlags(cfg: JsonObject, switches: LoadSwitch[], changed: string[], ov: Overrides): void {
  for (const sw of switches) {
    if (sw.flag in ov) continue
    if (changed.some(p => sw.covers.some(c => under(p, c)))) setAt(cfg, fromPointer(sw.flag), sw.load)
  }
}

/** Whether a switch is on in an exported file. */
export function isOn(file: Json, sw: LoadSwitch): boolean {
  return getAt(file, fromPointer(sw.flag)) === sw.load
}

