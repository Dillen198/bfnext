// The real 3D models' catalogue, shared by the dashboard's flight replay
// (bfweb/src/replay) and the range site's 3D debrief (bfrange-web): which
// model a DCS unit type is drawn with. The models themselves are
// bfweb/public/models/*.glb (credits and licences in CREDITS.txt there):
// nose on +Y, up on +Z, origin at the middle, real length in metres. The
// range site serves the same files (see its vite.config.ts), so there is
// one copy of each.
//
// Ships also need to sit on the water, and a carrier's deck on the deck its
// grading uses: ship-fit.json (from bfweb/scripts/fit-ship-models.mjs) says
// where each ship model's flight or main deck, keel and ends are, and
// SHIP_DRAFT the published draft of the escorts.

import fit from './ship-fit.json'

/** A file name under public/models (without .glb). */
export type Real = string

// First match wins, so a type with a model of its own comes before the
// lookalike rules. Patterns are tested against the DCS unit type name.
const AIR: [RegExp, Real][] = [
  [/F-14/i, 'F-14'],
  [/F-?A-18|F-18|Hornet|EA-18/i, 'F-18'],
  [/F-35/i, 'F-35'],
  [/F-22|Su-57|J-20|J-31|FC-31/i, 'F-22'],
  [/F-16/i, 'F-16'],
  [/F-15/i, 'F-15'],
  [/F-117/i, 'F-117'],
  [/F-111|F-111/i, 'F-111'],
  [/F-100/i, 'F-100'],
  [/F-4/i, 'F-4'],
  [/EA-6|A-6/i, 'EA-6B'],
  [/A-10/i, 'A-10'],
  [/Su-24/i, 'Su-24'],
  [/Su-25/i, 'Su-25'],
  [/Su-17|Su-22/i, 'Su-17'],
  [/Su-34/i, 'Su-34'],
  [/Su-2[7]|Su-3[0-5]|J-11|J-15|J-16/i, 'Su-35'],
  [/MiG-29/i, 'MiG-29'],
  [/MiG-31/i, 'MiG-31'],
  [/MiG-25/i, 'MiG-25'],
  [/MiG-27/i, 'MiG-27'],
  [/MiG-23/i, 'MiG-23'],
  [/MiG-1[579]|F-86/i, 'MiG-17'],
  [/M-?2000|Mirage|MiG-21|J-7|Kfir|Cheetah/i, 'Mirage2000'],
  [/Rafale/i, 'Rafale'],
  [/Typhoon|Eurofighter|EF-?2000|J-10|JAS39|Gripen|AJS37|Viggen/i, 'EF2000'],
  [/F-5|F-20/i, 'F-5'],
  [/AV-?8|Harrier/i, 'AV-8B'],
  [/Tornado/i, 'Tornado'],
  [/Tu-160/i, 'Tu-160'],
  [/Tu-22/i, 'Tu-22M3'],
  [/B-52/i, 'B-52'],
  [/B-1/i, 'B-1'],
  [/Tu-95|Tu-142/i, 'Tu-95'],
  [/E-3|A-50|KJ-2000/i, 'E-3'],
  [/E-2/i, 'E-2'],
  [/Il-7[68]|IL-7[68]|Y-20|C-17/i, 'Il-76'],
  [/Yak-4[02]/i, 'Yak-40'],
  [/An-2[0-9]|An-30|An-32/i, 'An-26'],
  [/MQ-?[19]|RQ-?1|Predator|Reaper|Wing ?Loong|TB2|Bayraktar|CH-4|UAV|drone/i, 'MQ-1'],
  // other jet transports and tankers read as the KC-135, props as the C-130
  [/KC-?135|KC-?10|KC-?46|C-5|A_?3[23]0|B_?7[0-9]7|Boeing|Airbus|IL-62|Tu-154/i, 'KC-135'],
  [/C-?130|KC-?130|An-12|Y-8|C-47|Hercules|A400/i, 'C-130'],
  // lookalikes for fighters without a model yet
  [/JF-17|L-39|C-101|MB-339|Hawk|T-45|Yak-130|K-8/i, 'F-16'],
]
const HELO: [RegExp, Real][] = [
  [/CH-47|CH-46/i, 'CH-47'],
  [/AH-64|Apache/i, 'AH-64'],
  [/AH-1/i, 'AH-1'],
  [/Mi-24|Mi-35/i, 'Mi-24'],
  [/Mi-28|Ka-50|Ka-52/i, 'Mi-28'],
  [/Mi-8|Mi-17|Mi-26/i, 'Mi-8'],
  [/SH-60|UH-60|MH-60|HH-60|Black ?Hawk/i, 'SH-60'],
  [/UH-1|OH-58|SA ?342|Gazelle|Bell/i, 'UH-1'],
]
const GROUND: [RegExp, Real][] = [
  // launchers (not their radars)
  [/NASAMS_LN|IRISTSLM_LN/i, 'NASAMS'],
  [/Shilka|ZSU|ZU-23|Gepard|Vulcan/i, 'Shilka'],
  [/Osa|9A33|\bTor\b|TorM2|Pantsir|Tunguska|2S6|Strela|Roland|HQ-7_STR_SP|M6 Linebacker|M1097 Avenger/i, 'Osa'],
  [/M142|HIMARS|MLRS|M270|Smerch|Uragan|Grad|BM-2[17]|BM-30|9A52|PHL/i, 'HIMARS'],
  [/\bln\b|_LN\b|LN_|Launcher|Volhov|S_75|5P85|9A310|RAPIER_FSA/i, 'Patriot'],
  [/M-?1 Abrams|M1A[12]|Leopard|Leclerc|Challenger|Merkava|Chieftain|M-60|ZTZ96/i, 'M1A2'],
  [/T-?72|T-?8[04]|T-?90|T-?55|T-?62|T-?64|Type 59|Oplot/i, 'T-90'],
  [/M-?109|SAU|2S1|2S3|2S9|2S19|Msta|Akatsia|Gvozdika|Dana|PLZ05|Firtina|SpGH|M-?110/i, 'M109'],
  [/ZIL|Ural|KAMAZ|GAZ-66|KrAZ|HEMTT|M 818|M818|M978|ATZ|ATMZ|TZ-22|Truck|truck|HX7|HX8|M1083|Tractor|Trailer|Tigr|Blitz|Opel|Spoofer|_CP\b|Command_Post/i, 'ZIL-131'],
]
const SEA: [RegExp, Real][] = [
  [/CV_?1143|Kuznetsov|KUZNECOW/i, 'Kuznetsov'],
  [/CVN|Stennis|Forrestal|Nimitz|Ford|LHA|Tarawa|Carrier/i, 'CVN'],
  [/TICONDEROG|CG-?47/i, 'CG-47'],
  [/Arleigh|DDG/i, 'DDG-51'],
  [/Type_?05[24]|Type_?071|NEUSTRASH|REZKY|MOLNIYA|ALBATROS|MOSCOW|PIOTR|BDK|Grisha|Krivak/i, 'Type054'],
  // warships only: a cargo ship or tanker keeps the plain hull
  [/USS|Arleigh|PERRY|TICONDEROG|FFG|DDG|frigate|destroyer|cruiser|La_Combattante|HMS/i, 'FFG-7'],
]

// Weapons in flight. DCS names them with underscores (AIM_120C, GBU_12).
const MISSILE: [RegExp, Real][] = [
  [/TY-?90/i, 'TY-90'],
  [/HJ-?10|AKD/i, 'HJ-10'],
  [/AGM[_-]?114|Hellfire|Ataka|9M120|TOW|Vikhr|9M127|Kornet|\bHOT|Spike|Brimstone/i, 'AGM-114'],
  [/Kh[_-]?29/i, 'Kh-29'],
  // cruise missiles and other big strike weapons
  [/BGM[_-]?109|Tomahawk|AGM[_-]?158|JASSM|Kh[_-]?(101|55|65|59)|Kalibr|3M14|P[_-]?800|Storm|SCALP|Taurus|\bM48\b|ATACMS|Iskander|9M723|Scud|R-17/i, 'Kh-29'],
  [/AGM[_-]?(65|88|84|154)|HARM|Harpoon|JSOW|Kh[_-]?(25|31|35|58)|ALARM|BK[_-]?90|Maverick/i, 'AGM-65'],
  [/PL-?11/i, 'PL-11'],
  [/R[_-]?77|P[_-]?77|RVV|R[_-]?37/i, 'R-77'],
  [/R[_-]?27|P[_-]?27|R[_-]?24|R[_-]?40|R[_-]?33/i, 'R-27'],
  [/AIM[_-]?(7|120)|MICA|Meteor|PL-?1[25]|SD-10|Super[_-]?530|Skyflash|Aspide|SeaSparrow|ESSM/i, 'AIM-7'],
  [/AIM[_-]?9|R[_-]?73|P[_-]?73|R[_-]?60|R[_-]?74|PL-?[589]\b|Magic|IRIS|ASRAAM|Python|Stinger|Igla|9M3[1-9]|SA9M3|Strela|Mistral|Rapier|Roland|RAM\b/i, 'AIM-9'],
  // long-range SAMs: the biggest missile shape we have
  [/SA5B|SA5V|48N6|5V55|HHQ|HQ-|MIM[_-]?(104|23)|PAC|Patriot|SA57E6|SA9M38|9M3[18]|SM[_-]?[26]|RIM|S-?75|V-?75|5V2|SA-?\d/i, 'R-27'],
]
const BOMB: [RegExp, Real][] = [
  [/KAB|UPAB/i, 'KAB-500L'],
  [/GBU[_-]?(10|12|16|24|27|28)|Paveway|LGB/i, 'GBU-12'],
  [/./, 'JDAM'],
]

export function realOf(kind: string, name?: string | null): Real | null {
  if (!name) return null
  if (kind === 'missile' || kind === 'torpedo') {
    for (const [re, r] of MISSILE) if (re.test(name)) return r
    return null
  }
  if (kind === 'bomb') {
    for (const [re, r] of BOMB) if (re.test(name)) return r
    return null
  }
  const rules =
    kind === 'air' ? AIR
    : kind === 'helo' ? HELO
    : kind === 'armor' || kind === 'vehicle' || kind === 'sam' ? GROUND
    : kind === 'ship' || kind === 'carrier' ? SEA
    : null
  if (!rules) return null
  // a radar is not a launcher
  // (whole words: "Strela" is not a tracking radar)
  if (kind === 'sam' && /\b(sr|tr|str|cwar|pcp)\b|_STR\b|_sr_|radar|search|track|SNR|RLS|EWR|1L13|55G6|9S\d|FPS|Dome|Dog Ear/i.test(name) && !/\bln\b|_LN\b|LN_|launcher/i.test(name)) return null
  for (const [re, r] of rules) if (re.test(name)) return r
  return null
}

export interface ShipFit { stern_y: number; bow_y: number; keel_x: number; deck_z: number; bottom_z: number; top_z: number }
export const SHIP_FIT: Partial<Record<Real, ShipFit>> = fit

/** Published draft (m, keel to waterline, sonar dome included). */
export const SHIP_DRAFT: Partial<Record<Real, number>> = { 'CG-47': 10.2, 'DDG-51': 9.3, 'FFG-7': 7.5, Type054: 5 }

/** Model z of a ship model's waterline, from its draft or (carriers) its deck height. */
export function waterlineZ(r: Real, deckHeightM?: number): number | null {
  const f = SHIP_FIT[r]
  if (!f) return null
  if (deckHeightM != null) return f.deck_z - deckHeightM
  const d = SHIP_DRAFT[r]
  return d != null ? f.bottom_z + d : null
}

// ── the range site: a type's own model only ──
// The dashboard lends a lookalike to types without a model of their own (a
// T-45 flies as an F-16 there, a Ka-50 as a Mi-28). The range debriefs one
// pilot's own sortie, so a borrowed model would be wrong where it matters:
// these types keep the range site's own build to their real dimensions.
const BORROWED = /JF-17|L-39|C-101|MB-339|Hawk|T-45|Yak-130|K-8|Su-57|J-20|J-31|FC-31|F-86|F-20|MiG-1[59]|MiG-21|J-7|Kfir|Cheetah|J-10|JAS39|Gripen|AJS37|Viggen|C-17|Y-20|C-5|A_?3[23]0|B_?7[0-9]7|Boeing|Airbus|IL-62|Tu-154|An-12|Y-8|C-47|A400|KC-?10|KC-?46|MQ-?9|Reaper|Wing ?Loong|TB2|Bayraktar|CH-4|UAV|Mi-26|OH-58|SA ?342|Gazelle|Bell|Ka-5[02]/i

/** Ships: the names each ship model really is (anything else the catalogue matched only looks like it). */
const SHIP_OWN: Partial<Record<Real, RegExp>> = {
  CVN: /CVN|Nimitz|Stennis|Vinson|Roosevelt|Lincoln|Washington|Truman|Reagan|Bush|Enterprise/i,
  Kuznetsov: /CV_?1143|Kuznetsov|KUZNECOW/i,
  'CG-47': /TICONDEROG|CG-?47/i,
  'DDG-51': /Arleigh|DDG/i,
  'FFG-7': /PERRY|FFG-?7/i,
  Type054: /Type_?054/i,
}

/** The real model of a type's own (null where the catalogue would only lend a lookalike). */
export function ownModelOf(kind: string, name?: string | null): Real | null {
  const r = realOf(kind, name)
  if (!r || !name) return null
  if ((kind === 'air' || kind === 'helo') && BORROWED.test(name)) return null
  if (kind === 'ship' || kind === 'carrier') return SHIP_OWN[r]?.test(name) ? r : null
  return r
}
