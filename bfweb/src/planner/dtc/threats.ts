// RWR threat tables (F-16 ALR-56M, Hornet ALR-67, Apache RLWR/RFI): a list
// or map of threat groups, each with a priority and a few switches. Plain
// helpers here; the editor is ThreatTable.tsx.

import type { FieldAnnotation, Json, JsonObject, Seg } from '../model'
import { isFlat, isObj } from './core'

export interface ThreatRow {
  key: Seg
  name: string
  /** The DCS unit types in the group, when the cartridge names them. */
  types: string
  kind: ThreatKind
  pri: number
  rec: JsonObject
}

export type ThreatKind = 'SAM' | 'AAA' | 'Aircraft' | 'Missiles' | 'EWR & AWACS' | 'Ships' | 'Other'
export const THREAT_KINDS: ThreatKind[] = ['SAM', 'AAA', 'Aircraft', 'Missiles', 'EWR & AWACS', 'Ships', 'Other']

/** A table of threat groups: every entry a record with a numeric PRI (and
 *  at most the emitter list beside its plain fields). */
export function isThreatTable(v: Json | undefined): boolean {
  const kids = Array.isArray(v) ? v : isObj(v) ? Object.values(v) : null
  return !!kids && kids.length >= 5 && kids.every(k => {
    if (!isObj(k) || typeof k.PRI !== 'number') return false
    return isFlat(withoutEmitters(k))
  })
}

/** A threat record without its emitter list (static, never edited). */
function withoutEmitters(rec: JsonObject): JsonObject {
  const out = { ...rec }
  delete out.threats
  return out
}

const tidy = (s: string) => s.replace(/�/g, '–').replace(/\s+/g, ' ').trim()

/** What kind of threat a group is, from its name and unit types. */
export function threatKind(name: string, types = ''): ThreatKind {
  const s = `${name} ${types}`
  if (/^unknown/i.test(name)) return 'Other'
  if (/^(A-50|E-2|E-3|KJ-2000)\b/.test(name) || /^EWR\b|Sborka|Dog Ear|AN\/FPS/.test(name)) return 'EWR & AWACS'
  if (/^SAM\b|\bSAM\b|Patriot|NASAMS|Hawk|Rapier|Roland|HQ-7|IRIS-T|\bSA-\d+/.test(name)) return 'SAM'
  if (/AAA|SPAAA|LPWS|Gepard|Vulcan|ZSU|Shilka|PGZ/i.test(name)) return 'AAA'
  if (/^(AIM-|R-77|R-27|R-33|R-37|MICA|SD-10|PL-12)|Active R(a)?dr|\bAAM\b/i.test(s)) return 'Missiles'
  if (/frigate|destroyer|cruiser|carrier|corvette|boat|ship|dock|Kuznetsov|Kirov|Ticonderoga|Tarawa|Nimitz|Forrestal|Invincible|Combattante|Condell|Bykov|Tarantul|Slava|Molniya|\bCVN?\b|\bDDG\b|\bFFG\b/i.test(s)) return 'Ships'
  return 'Aircraft'
}

/** The rows in priority order (1 first). */
export function threatRows(v: Json, a: FieldAnnotation | undefined): ThreatRow[] {
  const entries: [Seg, Json][] = Array.isArray(v) ? v.map((x, i) => [i, x]) : isObj(v) ? Object.entries(v) : []
  return entries
    .map(([key, x]) => {
      const rec = withoutEmitters(x as JsonObject)
      const name = tidy(typeof rec.group_name === 'string' && rec.group_name ? rec.group_name : String(key))
      const typesRaw = (typeof key === 'string' ? a?.keyLabels?.[key] : undefined) ?? (typeof rec.hint === 'string' ? rec.hint : '')
      const types = tidy(typesRaw)
      return { key, name, types: types && types !== name ? types : '', kind: threatKind(name, types), pri: rec.PRI as number, rec }
    })
    .sort((x, y) => x.pri - y.pri || String(x.name).localeCompare(String(y.name)))
}

/** The PRI changes that put `keys` in this order: 1, 2, 3... Only the rows
 *  whose number moves are returned. */
export function renumber(rows: ThreatRow[], order: Seg[]): [Seg, number][] {
  const now = new Map(rows.map(r => [String(r.key), r.pri]))
  const out: [Seg, number][] = []
  order.forEach((k, i) => { if (now.get(String(k)) !== i + 1) out.push([k, i + 1]) })
  return out
}

/** The order after moving `key` to position `to` (0-based) of the whole table. */
export function moveTo(rows: ThreatRow[], key: Seg, to: number): Seg[] {
  const keys = rows.map(r => r.key).filter(k => String(k) !== String(key))
  const at = Math.max(0, Math.min(keys.length, to))
  return [...keys.slice(0, at), key, ...keys.slice(at)]
}

/** The order with `keys` lifted to the top, each group keeping its own order. */
export function liftToTop(rows: ThreatRow[], keys: Seg[]): Seg[] {
  const lift = new Set(keys.map(String))
  return [...rows.filter(r => lift.has(String(r.key))), ...rows.filter(r => !lift.has(String(r.key)))].map(r => r.key)
}

/** Short column names for the cockpit switches. */
export const SWITCH_LABELS: Record<string, string> = {
  display: 'Show',
  search: 'Search only',
  unknown: 'Unknown only',
  friend: 'Friendly',
  friendly: 'Friendly',
  hostile: 'Hostile',
  aspj_xmit: 'Jam (ASPJ)',
}
