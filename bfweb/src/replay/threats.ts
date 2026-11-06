// Threat rings, as Tacview draws them round air defences: how far each DCS
// SAM launcher or gun can reach. Tacview's own database gives the range where
// it has one; otherwise the table below, matched on the recorded unit type
// (Tacview's `Name=`): published maximum engagement ranges, rounded --
// a picture of the threat, not DCS's exact numbers. Search and track radars
// have no ring of their own: the launchers carry it.

import type { Kind } from './data'
import { tacviewType } from '../../../shared/models/catalog'

export interface Threat {
  /** NATO name, as a Tacview label shows it */
  label: string
  /** maximum engagement range, metres */
  range: number
  /** gun or short-range IR system (drawn fainter) */
  short: boolean
}

const KM = 1000

const TABLE: [RegExp, string, number][] = [
  [/Patriot ln|Patriot cp|Patriot AMG/i, 'Patriot', 100 * KM],
  [/S-300PS 5P85|S-300PS 40B6M|SA-10|S-300/i, 'SA-10', 75 * KM],
  [/S_75M_Volhov|SA-2|S-75/i, 'SA-2', 43 * KM],
  [/Hawk ln|Hawk tr|Hawk pcp/i, 'Hawk', 45 * KM],
  [/SA-11 Buk LN|Buk LN|SA-11/i, 'SA-11', 35 * KM],
  [/Kub 2P25|SA-6/i, 'SA-6', 25 * KM],
  [/NASAMS_LN/i, 'NASAMS', 25 * KM],
  [/5p73 s-125|S-125|SA-3/i, 'SA-3', 18 * KM],
  [/HQ-7_LN|HQ-7/i, 'HQ-7', 12 * KM],
  [/Tor 9A331|SA-15/i, 'SA-15', 12 * KM],
  [/Osa 9A33|SA-8/i, 'SA-8', 10 * KM],
  [/2S6 Tunguska|SA-19/i, 'SA-19', 8 * KM],
  [/Roland ADS|Roland/i, 'Roland', 8 * KM],
  [/M48 Chaparral|Chaparral/i, 'Chaparral', 8 * KM],
  [/rapier_fsa_launcher|Rapier/i, 'Rapier', 7 * KM],
  [/Strela-10M3|SA-13/i, 'SA-13', 5 * KM],
  [/Strela-1 9P31|SA-9/i, 'SA-9', 4.2 * KM],
  [/M1097 Avenger|Avenger/i, 'Avenger', 5 * KM],
  [/Igla|Stinger|SA-18|SA-24|manpad/i, 'MANPADS', 5 * KM],
  [/Gepard/i, 'Gepard', 4 * KM],
  [/ZSU-23-4 Shilka|Shilka/i, 'ZSU-23-4', 2.5 * KM],
  [/ZSU_57|ZSU-57/i, 'ZSU-57', 4 * KM],
  [/ZU-23|Ural-375 ZU-23/i, 'ZU-23', 2.5 * KM],
  [/Vulcan|M163/i, 'Vulcan', 2 * KM],
  [/flak|KS-19|S-60|bofors|AAA/i, 'AAA', 3 * KM],
]

const SHORT = 6 * KM
const cache = new Map<string, Threat | null>()

/** The threat ring of a recorded object, if it is an air defence system. */
export function threatOf(kind: Kind, name?: string | null): Threat | null {
  if (!name || (kind !== 'sam' && kind !== 'vehicle' && kind !== 'armor' && kind !== 'ship' && kind !== 'static')) return null
  let t = cache.get(name)
  if (t !== undefined) return t
  t = null
  // Tacview's own engagement range first (its database has most air defences)
  const tv = tacviewType(name)
  if (tv && tv.engage > 0 && !/\b(sr|tr|str|cp|pcp|cwar)\b|radar|search/i.test(name)) {
    t = { label: tv.short || name, range: tv.engage, short: tv.engage <= SHORT }
    cache.set(name, t)
    return t
  }
  for (const [re, label, range] of TABLE) {
    if (re.test(name)) { t = { label, range, short: range <= SHORT }; break }
  }
  cache.set(name, t)
  return t
}
