// The real 3D models' catalogue, shared by the dashboard's flight replay
// (bfweb/src/replay) and the range site's 3D debrief (bfrange-web): which
// model a DCS unit type is drawn with, and what Tacview knows about it.
//
// The models are Tacview's own meshes (GPL-3.0; bfweb/scripts/
// build-tacview-models.mjs makes bfweb/public/models/<mesh>.glb from them:
// nose on +Y, up on +Z, origin at the middle, real size in metres). Which
// type gets which mesh is Tacview's own object database (tacview.json, from
// bfweb/scripts/gen-tacview-db.py), matched the way Tacview matches it: the
// recorded name, ignoring case and separators, else the longest name Tacview
// knows that it starts with (so "F-16C_50" is Tacview's F-16C and "Ka-50_3"
// its Ka-50). Names Tacview does not know -- community mod units, mostly --
// fall back to the lookalike rules below. The range site serves the same
// files (see its vite.config.ts), so there is one copy of each.
//
// Ships also need to sit on the water, and a carrier's deck on the deck its
// grading uses: ship-fit.json (from bfweb/scripts/fit-ship-models.mjs) says
// where each ship model's flight or main deck, keel and ends are, and
// SHIP_DRAFT the published draft of the escorts.

import fit from './ship-fit.json'
import db from './tacview.json'

/** A Tacview mesh: a file name under public/models (without .glb). */
export type Real = string

/** What Tacview's database says about a type. */
export interface TacviewType {
  /** as Tacview labels it, e.g. "F-16C" */
  short: string
  /** e.g. "F-16C Fighting Falcon" */
  long: string
  shape: Real
  /** maximum engagement / detection range, metres (0: not given) */
  engage: number
  detect: number
}

type Row = [string, string, number, number, number]
const SHAPES = db.shapes as string[]
const TYPES = db.types as Row[]
const NAMES = db.names as Record<string, number>

const norm = (s: string) => s.replace(/[\s_.-]+/g, '').toLowerCase()
let byLength: string[] | null = null
const typeCache = new Map<string, TacviewType | null>()

/** Tacview's type for a recorded unit name, or null when it has none. */
export function tacviewType(name?: string | null): TacviewType | null {
  if (!name) return null
  const hit = typeCache.get(name)
  if (hit !== undefined) return hit
  const n = norm(name)
  let i: number | undefined = NAMES[n]
  if (i == null) {
    // The longest known name this one starts with -- short ones too ("A-4"
    // for "A-4E-C", "J-10" for "J-10A") -- but never one that splits a
    // number: "A-4" is not "A-400M", nor "F-1" "F-15".
    byLength ??= Object.keys(NAMES).filter(k => k.length >= 2).sort((a, b) => b.length - a.length)
    const k = byLength.find(k => n.length > k.length && n.startsWith(k) && !(/\d/.test(k[k.length - 1]) && /\d/.test(n[k.length])))
    if (k) i = NAMES[k]
  }
  let t: TacviewType | null = null
  if (i != null) {
    const [short, long, shape, engage, detect] = TYPES[i]
    t = { short, long, shape: SHAPES[shape], engage, detect }
  }
  typeCache.set(name, t)
  return t
}

/** Does a mesh suit an object of this kind (a name match across kinds --
 *  a truck that shares a prefix with a jet -- is not taken)? */
function fits(kind: string, shape: Real): boolean {
  if (shape === 'Core.Empty') return false // Tacview's invisible placeholder
  switch (kind) {
    case 'air': return /^(FixedWing|Core\.FixedWing)/.test(shape)
    case 'helo': return /^(Rotorcraft|Rotocraft|Core\.Rotorcraft)/.test(shape)
    case 'missile': case 'torpedo': return /^(Missile|Rocket|Core\.(Missile|Torpedo|Weapon))/.test(shape)
    case 'rocket': return /^(Rocket|Missile|Core\.Missile)/.test(shape)
    case 'bomb': return /^(Bomb|Core\.Bomb)/.test(shape)
    case 'sam': case 'armor': case 'vehicle': return /^(AntiAircraft|Vehicle|Core\.(Vehicle|Infantry|Artillery|Human))/.test(shape)
    case 'infantry': case 'parachutist': return /Infantry|Manpad|Parachutist|Human/.test(shape)
    case 'ship': case 'carrier': return /^(Watercraft|Core\.(Watercraft|AircraftCarrier))/.test(shape)
    case 'static': return true
    default: return false
  }
}

// ── lookalikes for names Tacview does not know ──
// First match wins. Patterns are tested against the DCS unit type name.

const AIR: [RegExp, Real][] = [
  [/WingLoong|Wing Loong|CH-4\b|TB2|Bayraktar|MQ-?9|Reaper|UAV|drone/i, 'FixedWing.MQ-9'],
  [/MQ-?1|RQ-?1|Predator/i, 'FixedWing.MQ-1'],
  [/KC-?135|KC135/i, 'FixedWing.KC-135C'],
  [/IL-78|Il-78/i, 'FixedWing.Il-78'],
  [/IL-76|Il-76|Y-20/i, 'FixedWing.Il-76'],
  [/Su-35|Su-2[7]|Su-3[0-5]|J-1[156]/i, 'FixedWing.Su-27'],
  [/MiG-29/i, 'FixedWing.MiG-29'],
  [/F-16/i, 'FixedWing.F-16'],
  [/F-?A-18|F-18|Hornet/i, 'FixedWing.F-18C'],
  [/F-15/i, 'FixedWing.F-15'],
  [/F-14/i, 'FixedWing.F-14'],
  [/Mirage|M-?2000/i, 'FixedWing.Mirage 2000-5'],
  [/Tu-160/i, 'FixedWing.B-1'],
  [/C-?130|Hercules|An-12|Y-8/i, 'FixedWing.C-130'],
  [/A_?3[23]0|B_?7[0-9]7|Boeing|Airbus|Yak-4[02]|Tu-154|IL-62/i, 'FixedWing.A320'],
]
const HELO: [RegExp, Real][] = [
  [/CH-47|Chinook/i, 'Rotorcraft.Chinook'],
  [/UH-60|SH-60|MH-60|HH-60|Black ?Hawk/i, 'Rotorcraft.UH-60'],
  [/Ka-5[02]/i, 'Rotorcraft.Ka-50'],
  [/OH-?58|Kiowa/i, 'Rotorcraft.Bell 206'],
  [/AH-6(?!\d)|MH-6(?!\d)|OH-6(?!\d)|Little ?Bird/i, 'Rotorcraft.MD 500'],
  [/SA ?342|Gazelle/i, 'Rotorcraft.SA342M'],
  [/Mi-26|Mi-17|Mi-8/i, 'Rotorcraft.Mi-8'],
  [/AH-64|Apache/i, 'Rotorcraft.AH-64'],
]
/** Air defence radars by what they look like (Tacview shares its shapes the same way). */
const RADAR: [RegExp, Real][] = [
  [/snr|Fan ?Song|Low ?Blow/i, 'AntiAircraft.SAM.SA-3.SNR-125'],
  [/p-19|Flat ?Face|RLS_19J6|Dog Ear/i, 'AntiAircraft.SAM.P-15'],
  [/\b(str|tr)\b|_STR\b|track|MPQ|Sentinel|1S91|9S18/i, 'AntiAircraft.SAM.SA-6.1S91'],
  [/\b(cp|pcp)\b|_CP\b|54K6|command/i, 'Vehicle.Truck.Kamaz.Command'],
  [/./, 'AntiAircraft.SAM.SA-10.76N6'],
]
const GROUND: [RegExp, Real][] = [
  [/S_75|Volhov|S-200|5P72/i, 'AntiAircraft.SAM.SA-2.LN'],
  [/Pantsir|\bTor\b|TorM2|Tunguska|2S6|Osa|9A33|Strela|SA-13|SA-9|Roland|Linebacker|Avenger|HQ-7/i, 'AntiAircraft.SAM.SA-13'],
  [/Igla|Stinger|manpad|Mistral|\bcomm\b/i, 'AntiAircraft.SAM.Manpad'],
  [/\bln\b|_LN\b|LN_|Launcher|NASAMS|Patriot|Hawk|5P85|9A310|Rapier/i, 'AntiAircraft.SAM.Launcher'],
  [/Shilka|ZSU-23-4/i, 'AntiAircraft.AAA.ZSU-23-4'],
  [/ZU-23|ZSU_57|ZSU-57|ZPU/i, 'AntiAircraft.AAA.ZSU-23-2'],
  [/flak|KS-19|S-60\b|bofors|M1939|QF[ _]3/i, 'AntiAircraft.Flak.Flak 18'],
  [/T-?90|T-?72|T-?8[04]|Oplot|ZTZ|Type 59/i, 'Vehicle.Tank.T-80U'],
  [/Abrams|M1A[12]|Leopard|Challenger|Merkava|M-60|Chieftain/i, 'Vehicle.Tank.M1'],
  [/BMP|BMD|ZBD|Marder|Warrior|LAV-25/i, 'Vehicle.APC.BMP-2'],
  [/Bradley|M2A2/i, 'Vehicle.Tank.M2'],
  [/BTR|VAB|Stryker|\bLAV\b|MTLB|MT-LB|Boxer|AAV7|Cobra/i, 'Vehicle.APC.BTR-80'],
  [/MATV|M-ATV|Titan|Tigr|HMMWV|Hummer|JLTV|M1043|M1045/i, 'Vehicle.Car.Humvee'],
  [/M142|HIMARS|M270|MLRS|ATACMS|Smerch|Uragan|Grad|BM-\d/i, 'Vehicle.Truck.BM-21'],
  [/Scud|9P117/i, 'Vehicle.Truck.TEL.Scud'],
  [/M-?109|2S1|2S3|2S19|Msta|Akatsia|Gvozdika|Dana|PLZ|SpGH|M-?110|SAU/i, 'Vehicle.Artillery.M109'],
  [/mortar|2B11|Soldier|Infantry|Paratrooper/i, 'Core.Infantry'],
  [/ATZ|ATMZ|TZ-22|Tanker|fuel|M978|refuel/i, 'Vehicle.Truck.Tanker'],
  [/Spoofer|GCI|station|_CP\b|Command|C-RAM|Phalanx|Generator|Comms/i, 'Vehicle.Truck.Kamaz.Command'],
  [/UAZ|Jeep|Willys/i, 'Vehicle.Car.UAZ-469'],
  [/Pickup|Toyota|Hilux|technical/i, 'Vehicle.Car.Pickup'],
  [/Tractor|Trailer|HX7|HX8|M1083|Truck|KAMAZ|Ural|ZIL|KrAZ|GAZ|HEMTT|M 818|M818|Blitz|Opel|MAZ/i, 'Vehicle.Truck.Kamaz'],
]
const SEA: [RegExp, Real][] = [
  [/CVN|Stennis|Nimitz|Forrestal|Ford|Vinson|Roosevelt|Lincoln|Washington|Truman|Reagan/i, 'Watercraft.CVN-72'],
  [/KUZNECOW|Kuznetsov|CV_?1143/i, 'Watercraft.Admiral Kuznetsov'],
  [/LHA|Tarawa/i, 'Watercraft.LHA-1'],
  [/TICONDEROG|CG-?47/i, 'Watercraft.CG-47'],
  [/Arleigh|DDG/i, 'Watercraft.F710'],
  [/PERRY|FFG/i, 'Watercraft.Frigate'],
  [/Type_?052/i, 'Watercraft.Type 052D'],
  [/Type_?054/i, 'Watercraft.Type 054A'],
  [/REZKY|Krivak/i, 'Watercraft.Krivak'],
  [/MOSCOW|Moskva|Slava/i, 'Watercraft.Moskva'],
  [/Seawise/i, 'Watercraft.Seawise Giant'],
  [/cargo|Supply|conveyor|container|Bulk|HandyWind|ELNYA|tanker|Tilde/i, 'Watercraft.Cargo'],
  [/KILO|submarine|Santa ?Fe|Type_?093/i, 'Watercraft.Project 877'],
  [/U-?boat|Type ?VII/i, 'Watercraft.Type VIIC'],
  [/LST|Higgins|LCVP|Landing/i, 'Watercraft.LST-1'],
  [/speedboat|boat|ZWEZDNY|Zodiac/i, 'Watercraft.Zodiac'],
]
const MISSILE: [RegExp, Real][] = [
  [/AIM[_-]?120|SD-10|PL-?12/i, 'Missile.AIM-120C'],
  [/AIM[_-]?54|Phoenix/i, 'Missile.AIM-54A'],
  [/AIM[_-]?9|R[_-]?73|P[_-]?73|R[_-]?60|Magic|IRIS|Python|ASRAAM|PL-?[589]\b|TY-?90/i, 'Missile.AIM-9M'],
  [/AIM[_-]?7|R[_-]?27|P[_-]?27|R[_-]?77|P[_-]?77|PL-?1[15]|MICA|Meteor|530/i, 'Missile.AIM-7M'],
  [/AGM[_-]?88|HARM|Kh[_-]?(25MP|31P|58)/i, 'Missile.AGM-88'],
  [/AGM[_-]?84|Harpoon|SLAM|Exocet|Kh[_-]?35|P[_-]?800/i, 'Missile.AGM-84E'],
  [/BGM|Tomahawk|Kalibr|3M14|Kh[_-]?(101|55|65|59)|JASSM|SCALP|Storm|Taurus|AGM[_-]?158/i, 'Missile.Kh-101'],
  [/48N6|5V55|SA5|PAC|MIM|SM[_-]?[26]|Aster|9M3[18]|HQ-?9/i, 'Missile.Aster30'],
  [/9M33|Tor|HQ-?7|Crotale|Roland/i, 'Missile.9M330'],
  [/Igla|Stinger|Strela|Mistral|9M3[1-9]|SA9M3/i, 'Missile.9K38'],
  [/./, 'Missile.AGM-65'],
]
const BOMB: [RegExp, Real][] = [
  [/GBU|KAB|UPAB|LGB|Paveway/i, 'Bomb.GBU-12'],
  [/CBU|Rockeye|Mk[_-]?20|RBK|BL[_-]?755|BLU-?97|Cluster/i, 'Bomb.MK-20'],
  [/AN[_-]?M\d|SC[_-]?\d|SD[_-]?\d/i, 'Bomb.AN-M59'],
]

/** Tacview's mesh for a kind of object when nothing more is known. */
const CORE: Record<string, Real> = {
  air: 'Core.FixedWing', helo: 'Core.Rotorcraft', missile: 'Core.Missile', torpedo: 'Core.Torpedo', bomb: 'Core.Bomb',
  rocket: 'Rocket.Hydra 70', sam: 'AntiAircraft.SAM.Launcher', armor: 'Core.Vehicle', vehicle: 'Core.Vehicle',
  infantry: 'Core.Infantry', parachutist: 'Other.Parachutist', ship: 'Core.Watercraft', carrier: 'Core.AircraftCarrier',
}

const RADAR_NAME = /\b(sr|tr|str|cwar|pcp|cp)\b|_STR\b|_sr_|\bsr_|radar|search|track|SNR|RLS|RPC|5N62|EWR|1L13|55G6|9S\d|FPS|Dome|Dog Ear|54K6/i
const LAUNCHER_NAME = /\bln\b|_LN\b|LN_|launcher/i

/** The mesh a recorded object is drawn with (null: draw the built-in shape). */
export function realOf(kind: string, name?: string | null): Real | null {
  if (!name) return CORE[kind] ?? null
  const t = tacviewType(name)
  if (t && fits(kind, t.shape)) return t.shape
  const rules =
    kind === 'air' ? AIR
    : kind === 'helo' ? HELO
    : kind === 'sam' && RADAR_NAME.test(name) && !LAUNCHER_NAME.test(name) ? RADAR
    : kind === 'armor' || kind === 'vehicle' || kind === 'sam' ? GROUND
    : kind === 'ship' || kind === 'carrier' ? SEA
    : kind === 'missile' || kind === 'torpedo' ? MISSILE
    : kind === 'bomb' ? BOMB
    : null
  if (rules) for (const [re, r] of rules) if (re.test(name)) return r
  if (kind === 'rocket') return /S[_-]?(5|8|13|24|25)(?!\d)|B[_-]?8|UB[_-]?(16|32)|C_?8/i.test(name) ? 'Rocket.S-13' : 'Rocket.Hydra 70'
  if (kind === 'infantry' || kind === 'parachutist') return /Igla|Stinger|manpad|Mistral/i.test(name) ? 'AntiAircraft.SAM.Manpad' : CORE[kind]
  return CORE[kind] ?? null
}

/** The short name Tacview labels a type with ("F-16C"), or null. */
export function shortNameOf(name?: string | null): string | null {
  return tacviewType(name)?.short || null
}

export interface ShipFit { stern_y: number; bow_y: number; keel_x: number; deck_z: number; bottom_z: number; top_z: number }
export const SHIP_FIT: Partial<Record<Real, ShipFit>> = fit

/** Published draft (m, keel to waterline, sonar dome included). */
export const SHIP_DRAFT: Partial<Record<Real, number>> = {
  'Watercraft.CG-47': 10.2, 'Watercraft.F710': 4.8, 'Watercraft.Frigate': 7.5, 'Watercraft.Type 054A': 5,
  'Watercraft.Type 052D': 6.5, 'Watercraft.Krivak': 4.6, 'Watercraft.Moskva': 8.4, 'Watercraft.Type23': 7.3,
}

/** Model z of a ship model's waterline, from its draft or (carriers) its deck height. */
export function waterlineZ(r: Real, deckHeightM?: number): number | null {
  const f = SHIP_FIT[r]
  if (!f) return null
  if (deckHeightM != null) return f.deck_z - deckHeightM
  const d = SHIP_DRAFT[r]
  return d != null ? f.bottom_z + d : null
}

// ── the range site: a type's own model only ──
// The dashboard lends a lookalike to types Tacview does not know (a CHAP
// mod tank drawn as a T-80). The range debriefs one pilot's own sortie, so a
// borrowed model would be wrong where it matters: only Tacview's own match
// counts there, and a ship only with its deck and keel measured.

/** The real model of a type's own (null where the catalogue would only lend a lookalike). */
export function ownModelOf(kind: string, name?: string | null): Real | null {
  const t = tacviewType(name)
  if (!t || !fits(kind, t.shape) || t.shape.startsWith('Core.')) return null
  if (kind === 'ship' || kind === 'carrier') return SHIP_FIT[t.shape] ? t.shape : null
  return t.shape
}
