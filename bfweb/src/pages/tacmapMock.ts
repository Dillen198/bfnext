// Fixture for TacMapPage's `?mock` mode (dev builds only -- the page tests
// `import.meta.env.DEV` before it touches this). The ground picture and our
// assets come from the groundwar and command mocks; this adds what the
// TacMap's base drawer reads besides: the public objective list, each base's
// warehouse as the engine would scope it for Blue (ours in full, the enemy's
// from recon or not at all) and the round's captures.
import type { CaptureEvent, CaptureReport, GroundPicture, Objective, Warehouse, WarehouseItem } from '../api'

/** Deterministic 0..1 from a name, so the numbers don't jump between frames. */
function seed(name: string, salt: number): number {
  let h = salt * 2654435761
  for (let i = 0; i < name.length; i++) h = Math.imul(h ^ name.charCodeAt(i), 2246822519)
  return ((h >>> 0) % 1000) / 1000
}
const pct = (name: string, salt: number, lo = 20) => Math.round(lo + seed(name, salt) * (100 - lo))
const isoAgo = (mins: number) => new Date(Date.now() - mins * 60_000).toISOString()

/** A naval base on the coast: in the public list, not in the ground picture. */
const POTI: Objective = {
  id: 'ObjectiveId(40)', name: 'Poti', kind: 'Naval', owner: 'Blue', lat: 42.15, lon: 41.67,
  health: 100, logi: 100, supply: 77, fuel: 83, last_change: isoAgo(60 * 30), unlimited_aircraft: false,
}

export function mockObjectives(pic: GroundPicture): Objective[] {
  return [
    ...pic.objectives.map((o): Objective => ({
      id: `ObjectiveId(${o.id})`,
      name: o.name,
      kind: o.kind.charAt(0).toUpperCase() + o.kind.slice(1),
      owner: o.owner,
      lat: o.pos[0],
      lon: o.pos[1],
      health: o.owner === 'Neutral' ? 0 : o.health ?? pct(o.name, 1, 45),
      logi: o.owner === 'Neutral' ? 0 : pct(o.name, 2, 35),
      supply: o.owner === 'Neutral' ? 0 : o.supply ?? pct(o.name, 3),
      fuel: o.owner === 'Neutral' ? 0 : pct(o.name, 4),
      last_change: isoAgo(30 + Math.round(seed(o.name, 5) * 600)),
      priority: o.name === 'Khashuri' || o.name === 'Sachkhere',
      threatened: o.threatened ?? false,
      captureable: o.being_captured,
      unlimited_supply: o.kind === 'command',
      unlimited_aircraft: o.name === 'Kutaisi',
    })),
    POTI,
  ]
}

const AIRFRAMES: Record<string, string[]> = {
  airbase: ['F-16C_50', 'FA-18C_hornet', 'A-10C_2', 'AH-64D_BLK_II', 'UH-1H', 'Mi-8MT', 'C-130J-30'],
  farp: ['AH-64D_BLK_II', 'UH-1H', 'Mi-8MT', 'SA342M'],
  fob: ['UH-1H', 'Mi-8MT'],
  naval: ['SH-60B'],
}
const KIT = [
  'weapons.missiles.AIM_120C', 'weapons.missiles.AIM_9X', 'weapons.bombs.Mk_82', 'weapons.bombs.GBU_12',
  'weapons.missiles.AGM_65D', 'weapons.nurs.HYDRA_70_M151', 'weapons.missiles.AGM_88C', 'weapons.bombs.GBU_38',
  'weapons.missiles.AIM_7M', 'weapons.droptanks.FuelTank_370', 'weapons.shells.M61_20_HE', 'weapons.nurs.C_8OFP2',
  'vehicles.M-113', 'vehicles.Hummer', 'vehicles.M1043 HMMWV Armament', 'vehicles.Ural-375',
  'Fortifications.Sandbox', 'Fortifications.Barracks 2',
]

function stock(name: string, keys: string[], cap: number, salt: number): Record<string, WarehouseItem> {
  const out: Record<string, WarehouseItem> = {}
  keys.forEach((k, i) => {
    const c = Math.max(2, Math.round(cap * (0.5 + seed(k, salt + i))))
    out[k] = { capacity: c, stored: Math.round(c * seed(name + k, salt)) }
  })
  return out
}


/** The engine's capture report, as it would read for Blue: one enemy base
 *  still defended (recon has its garrison), one whose zone has nothing in it
 *  to destroy, a neutral one waiting on troops, and our own. */
function mockCapture(o: Objective, ours: boolean, seen: boolean): CaptureReport {
  const at = (dn: number, de: number) => ({
    lat: o.lat + dn / 111_000, lon: o.lon + de / (111_000 * Math.cos((o.lat * Math.PI) / 180)),
  })
  const grid = (i: number) => `38T LM ${String(41230 + i * 137).padStart(5, '0')} ${String(65810 + i * 91).padStart(5, '0')}`
  const base = { health_threshold: 20, troops: [], notes: [] as string[], unlocated: 0, kills_needed: 0, infantry_left: 0, targets: [] }
  if (o.owner === 'Neutral') {
    return {
      ...base, perspective: 'attacker', open: true, summary: 'Capturable now: get capture troops into the zone.',
      blockers: [{ kind: 'no_troops_in_zone', text: 'Your nearest capture squad (Rifle squad) is 3.4 km outside the zone.', action: 'Move them into the zone and hold.' }],
      troops: [{ name: 'Rifle squad', can_capture: true, in_zone: false, outside_by_m: 3400 }],
    }
  }
  if (!ours && !seen) {
    return {
      ...base, perspective: 'attacker', open: false, summary: 'Can’t be captured: nothing here counts toward its health.',
      blockers: [{ kind: 'nothing_to_destroy', text: `Nothing here counts toward its health: it has no garrison units and no logistics buildings in its zone. Its health is stuck at ${o.health}%, so it can't be captured.`, action: '' }],
    }
  }
  const targets: CaptureReport['targets'] = [
    { kind: 'unit', label: 'Soldier AK', class: 'infantry', count: 1, infantry: true, ...at(120, -80), grid: grid(0) },
    { kind: 'unit', label: 'Soldier AK', class: 'infantry', count: 1, infantry: true, ...at(140, -60), grid: grid(1) },
    { kind: 'unit', label: 'T-72B', class: 'armor', count: 1, infantry: false, ...at(-300, 250), grid: grid(2) },
    { kind: 'unit', label: 'ZSU-23-4 Shilka', class: 'AAA', count: 1, infantry: false, ...at(420, 380), grid: grid(3) },
    { kind: 'unit', label: null, class: 'short-range SAM', count: 2, infantry: false, lat: null, lon: null, grid: null },
    { kind: 'building', label: 'Warehouse 1 (WAREHOUSE_A)', class: 'building', count: 1, infantry: false, ...at(-90, -420), grid: grid(4) },
  ]
  return {
    ...base, perspective: ours ? 'defender' : 'attacker', open: false,
    summary: 'Not capturable yet: destroy 3 more targets, then kill the infantry.',
    kills_needed: 3, infantry_left: 2, unlocated: ours ? 0 : 2, targets,
    blockers: [
      { kind: 'health', text: `Health is ${o.health}%. A capture needs 20% or less.`, action: 'Destroy 3 more garrison units.' },
      { kind: 'infantry', text: '2 infantry defenders are still alive. Every one must die.', action: 'Hunt down the infantry: strafe them, or bring ground forces.' },
      ...(ours ? [] : [{ kind: 'no_troops_in_zone' as const, text: 'None of your capture troops are near it.', action: 'Fly or drive capture-capable troops into the zone and hold it.' }]),
    ],
    notes: ours ? ['Destroying its logistics buildings lowers logistics, not health.'] : ['2 defenders are not on your intel picture. Recon, a JTAC or a closer look will place them.'],
  }
}

export function mockWarehouse(pic: GroundPicture, name: string): Warehouse {
  const objs = mockObjectives(pic)
  const o = objs.find((x) => x.name === name)
  if (!o) throw new Error(`no objective called ${name}`)
  const ours = o.owner === pic.side
  const kind = o.kind.toLowerCase()
  const base = {
    objective_id: o.id, objective_name: o.name, kind: o.kind, owner: o.owner,
    health: o.health, logi: o.logi, damaged: o.name === 'Sachkhere',
  }
  if (!ours) {
    // Recon has seen two of the enemy's bases; the rest are dark.
    const seen = o.name === 'Khashuri' || o.name === 'Borjomi'
    return {
      ...base, visibility: o.owner === 'Neutral' ? 'intel' : seen ? 'intel' : 'hidden',
      intel_as_of: seen ? isoAgo(23) : null, supply: seen ? o.supply : null, fuel: seen ? o.fuel : null,
      equipment: {}, liquids: {}, supplier: null, destinations: [],
      capture: mockCapture(o, false, seen),
    }
  }
  const hub = objs.find((x) => x.name === 'Kharagauli')!
  const link = (x: Objective, interdicted = false) => ({ id: x.id, name: x.name, owner: x.owner, interdicted })
  const fed = o.name === 'Kharagauli'
    ? objs.filter((x) => x.owner === 'Blue' && x.name !== 'Kharagauli' && x.name !== 'Poti').map((x) => link(x, x.name === 'Sachkhere'))
    : []
  return {
    ...base, visibility: 'full', intel_as_of: null, supply: o.supply, fuel: o.fuel,
    equipment: {
      ...stock(o.name, AIRFRAMES[kind] ?? [], kind === 'airbase' ? 12 : 4, 7),
      ...stock(o.name, KIT, 80, 11),
    },
    liquids: {
      jet_fuel: { capacity: 100_000, stored: Math.round(1000 * (o.fuel ?? 0)) },
      gasoline: { capacity: 40_000, stored: Math.round(400 * pct(o.name, 8)) },
      diesel: { capacity: 40_000, stored: Math.round(400 * pct(o.name, 9)) },
      methanol_mixture: { capacity: 8_000, stored: Math.round(80 * pct(o.name, 10, 5)) },
    },
    supplier: o.name === 'Kharagauli' ? null : link(hub, o.name === 'Sachkhere'),
    destinations: fed,
    capture: mockCapture(o, true, true),
  }
}

export function mockCaptures(): CaptureEvent[] {
  return [
    { time: isoAgo(42), objective: 'Sachkhere', side: 'Blue', by: ['Viper 1-1', 'Hog 2-2'], by_ucid: ['a', 'b'] },
    { time: isoAgo(180), objective: 'Chiatura', side: 'Blue', by: ['Dusty 3-1'], by_ucid: ['c'] },
    { time: isoAgo(260), objective: 'Sachkhere', side: 'Red', by: ['Flanker 1-2'], by_ucid: ['d'] },
    { time: isoAgo(600), objective: 'Borjomi', side: 'Red', by: [], by_ucid: [] },
  ]
}
