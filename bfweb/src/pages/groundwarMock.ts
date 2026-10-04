// Fixture for GroundWarPage's `?mock` mode (dev builds only -- the page tests
// `import.meta.env.DEV` before it touches this). Central Georgia: Blue holds
// the Rioni valley out to Kharagauli and Sachkhere and is assaulting Khashuri;
// Red holds the Kura valley from Borjomi to Gori and is probing Sachkhere
// from the east.
//
// It is a small simulation, not a still: `step()` drives formations along
// their roads (time-accelerated so it shows), deploys them when they stop,
// flies the live players and keeps the battles producing events. `?mock=view`
// renders it read-only, `?mock=god` as an admin looking in.
import type {
  GroundBattle,
  GroundCommand,
  GroundCommandReply,
  GroundEnemyContact,
  GroundEvent,
  GroundFormation,
  GroundKind,
  GroundObjective,
  GroundPicture,
  GroundRole,
  GroundUnit,
  LatLon,
  LivePlayer,
} from '../api'
import { along, bearing, dist, km, lerp, lerpHeading, offset, pathLength } from './groundwar/geo'

/** Formations move this many times faster than their speed, so the mock shows motion. */
const TIME_ACCEL = 6
const COLUMN_SPACING = 30
const LINE_SPACING = 80

const BLUE_TYP: Record<GroundRole, string> = {
  tank: 'T-72B', ifv: 'BMP-2', apc: 'BTR-80', recon: 'BRDM-2', aaa: 'ZSU-23-4 Shilka',
  sam: 'Osa 9A33 ln', artillery: '2S1 Gvozdika', infantry: 'Infantry AK-74', truck: 'Ural-375',
}
const RED_TYP: Record<GroundRole, string> = {
  tank: 'T-72B3', ifv: 'BMP-2', apc: 'BTR-82A', recon: 'BRDM-2', aaa: 'ZSU-23-4 Shilka',
  sam: 'Strela-10M3', artillery: '2S3 Akatsia', infantry: 'Infantry AK-74 Rus', truck: 'KAMAZ Truck',
}

/** Deterministic jitter in [-1, 1). */
function jit(n: number): number {
  const x = Math.sin(n * 127.1 + 311.7) * 43758.5453
  return (x - Math.floor(x)) * 2 - 1
}

const OBJECTIVES: GroundObjective[] = [
  { id: 1, name: 'Kutaisi', pos: [42.176, 42.482], owner: 'Blue', kind: 'airbase', health: 96, threatened: false, can_raise: 2, being_captured: false, supply: 88, garrison: 6 },
  { id: 2, name: 'Tkibuli', pos: [42.35, 42.998], owner: 'Neutral', kind: 'farp', health: null, threatened: null, can_raise: null, being_captured: false, supply: null, garrison: null },
  { id: 3, name: 'Zestaponi', pos: [42.11, 43.045], owner: 'Blue', kind: 'fob', health: 81, threatened: false, can_raise: 1, being_captured: false, supply: 64, garrison: 4 },
  { id: 4, name: 'Chiatura', pos: [42.29, 43.285], owner: 'Blue', kind: 'factory', health: 70, threatened: false, can_raise: 0, being_captured: false, supply: 41, garrison: 2 },
  { id: 5, name: 'Sachkhere', pos: [42.343, 43.415], owner: 'Blue', kind: 'farp', health: 58, threatened: true, can_raise: 0, being_captured: false, supply: 27, garrison: 3 },
  { id: 6, name: 'Kharagauli', pos: [42.021, 43.2], owner: 'Blue', kind: 'logistics', health: 100, threatened: false, can_raise: 1, being_captured: false, supply: 92, garrison: 5 },
  { id: 7, name: 'Khashuri', pos: [41.994, 43.6], owner: 'Red', kind: 'logistics', health: null, threatened: null, can_raise: null, being_captured: true, supply: null, garrison: null },
  { id: 8, name: 'Borjomi', pos: [41.842, 43.387], owner: 'Red', kind: 'factory', health: null, threatened: null, can_raise: null, being_captured: false, supply: null, garrison: null },
  { id: 9, name: 'Kareli', pos: [42.022, 43.898], owner: 'Red', kind: 'fob', health: null, threatened: null, can_raise: null, being_captured: false, supply: null, garrison: null },
  { id: 10, name: 'Gori', pos: [41.984, 44.112], owner: 'Red', kind: 'command', health: null, threatened: null, can_raise: null, being_captured: false, supply: null, garrison: null },
  { id: 11, name: 'Tskhinvali', pos: [42.226, 43.966], owner: 'Red', kind: 'airbase', health: null, threatened: null, can_raise: null, being_captured: false, supply: null, garrison: null },
  { id: 12, name: 'Java', pos: [42.398, 43.928], owner: 'Red', kind: 'sam', health: null, threatened: null, can_raise: null, being_captured: false, supply: null, garrison: null },
]
const objById = (id: number) => OBJECTIVES.find((o) => o.id === id)

interface SimFormation {
  id: number
  name: string
  kind: GroundKind
  /** Alive vehicles in march order. */
  roles: GroundRole[]
  total: number
  order: GroundFormation['order']
  target: number | null
  home: number
  commander: string | null
  locked: number | null
  live: boolean
  /** Road it follows; includes road already covered so the column has somewhere to trail. */
  route: LatLon[]
  at: number
  speed: number
  moving: boolean
  assaulting: boolean
  engaged: boolean
  /** Seconds since it stopped; drives column -> line. */
  stoppedFor: number
  dugIn: boolean
  /** Heading it faces once deployed. */
  face: number
  trail: LatLon[]
  supply: number
  inSupply: boolean
  morale: number
  broken: boolean
  halted: boolean
  losses: number
  kills: number
}

const roles = (spec: [GroundRole, number][]): GroundRole[] => spec.flatMap(([r, n]) => Array<GroundRole>(n).fill(r))

function sim(f: Partial<SimFormation> & Pick<SimFormation, 'id' | 'name' | 'kind' | 'roles' | 'route' | 'home'>): SimFormation {
  return {
    total: f.roles.length,
    order: 'hold',
    target: null,
    commander: null,
    locked: null,
    live: true,
    at: 0,
    speed: 0,
    moving: false,
    assaulting: false,
    engaged: false,
    stoppedFor: 999,
    dugIn: false,
    face: 90,
    trail: [],
    supply: 80,
    inSupply: true,
    morale: 80,
    broken: false,
    halted: false,
    losses: 0,
    kills: 0,
    ...f,
  }
}

interface SimPlayer extends LivePlayer {
  /** Orbit centre and radius, or a leg to fly back and forth. */
  orbit?: { c: LatLon; r: number; ang: number }
  leg?: { a: LatLon; b: LatLon; t: number }
}

interface SimEnemy extends GroundEnemyContact {
  roles: GroundRole[]
  deployed: boolean
  drift?: { to: LatLon; mps: number }
}

export interface GroundMock {
  picture(): GroundPicture
  step(dtSecs: number): void
  command(cmd: GroundCommand): GroundCommandReply
}

export function createGroundwarMock(mode: string): GroundMock {
  let now = Math.floor(Date.now() / 1000)
  let clock = 0
  let nextId = 20
  let nextEventAt = 9

  const objectives = OBJECTIVES.map((o) => ({ ...o }))

  const forms: SimFormation[] = [
    sim({
      id: 1, name: '1st Mech Coy (Kharagauli)', kind: 'mechanised',
      roles: roles([['recon', 1], ['ifv', 4], ['tank', 2], ['ifv', 2], ['aaa', 1], ['truck', 1]]), total: 12,
      order: 'attack', target: 8, home: 6, commander: 'Viper 1-1', locked: 47,
      route: [[42.05, 43.13], [42.035, 43.17], [42.021, 43.2], [41.99, 43.24], [41.955, 43.27], [41.92, 43.31], [41.885, 43.35], [41.855, 43.375]],
      at: 2600, speed: 28, moving: true, supply: 74, morale: 82, losses: 1, kills: 2,
    }),
    sim({
      id: 2, name: '2nd Armd Coy (Zestaponi)', kind: 'armour',
      roles: roles([['tank', 6], ['ifv', 2], ['aaa', 1], ['truck', 1]]), total: 13,
      order: 'attack', target: 7, home: 3, commander: null,
      route: [[42.0, 43.48], [41.996, 43.53], [41.996, 43.567]],
      at: 99999, assaulting: true, engaged: true, face: 95, supply: 46, morale: 61, losses: 3, kills: 7,
    }),
    sim({
      id: 3, name: '3rd Mot Coy (Chiatura)', kind: 'motorised',
      roles: roles([['apc', 5], ['infantry', 3], ['aaa', 1], ['truck', 2]]), total: 12,
      order: 'defend', target: 5, home: 4, commander: 'Dillen', locked: 22,
      route: [[42.33, 43.40], [42.338, 43.43], [42.339, 43.445]],
      at: 99999, engaged: true, dugIn: true, face: 85, supply: 33, inSupply: true, morale: 70, losses: 1, kills: 2,
    }),
    sim({
      id: 4, name: '4th Armd Coy (Kutaisi)', kind: 'armour',
      roles: roles([['recon', 1], ['tank', 7], ['ifv', 2], ['sam', 1], ['truck', 3]]), total: 14,
      order: 'defend', target: 3, home: 1, live: false,
      route: [[42.18, 42.6], [42.172, 42.66], [42.16, 42.73], [42.15, 42.8], [42.14, 42.87], [42.128, 42.94], [42.115, 43.0], [42.11, 43.04]],
      at: 7200, speed: 34, moving: true, supply: 96, morale: 90,
    }),
    sim({
      id: 5, name: '5th Inf Coy (Tkibuli)', kind: 'infantry',
      roles: roles([['infantry', 4], ['apc', 1]]), total: 9,
      order: 'withdraw', target: 4, home: 2, broken: true,
      route: [[42.36, 43.12], [42.34, 43.18], [42.32, 43.23], [42.3, 43.27]],
      at: 1800, speed: 9, moving: true, supply: 12, inSupply: false, morale: 16, losses: 4, kills: 1,
    }),
    sim({
      id: 6, name: '6th Mech Coy (Zestaponi)', kind: 'mechanised',
      roles: roles([['ifv', 6], ['artillery', 2], ['truck', 2]]), total: 10,
      order: 'hold', target: null, home: 3, halted: true,
      route: [[42.1, 43.06], [42.098, 43.085], [42.095, 43.11]],
      at: 99999, face: 105, supply: 90, morale: 85,
    }),
  ]
  for (const f of forms) {
    const len = pathLength(f.route)
    f.at = Math.min(f.at, len)
    f.trail = trailFor(f)
  }

  const enemies: SimEnemy[] = [
    {
      id: 101, pos: [41.995, 43.588], kind: 'mechanised', approx_vehicles: 10, heading: 275, last_seen_secs: 0, moving: false, units: [],
      roles: roles([['tank', 2], ['ifv', 4], ['apc', 1]]), deployed: true,
    },
    {
      id: 102, pos: [42.334, 43.49], kind: 'armour', approx_vehicles: 5, heading: 268, last_seen_secs: 0, moving: true, units: [],
      roles: roles([['tank', 4], ['ifv', 1]]), deployed: false, drift: { to: [42.338, 43.455], mps: 1.4 },
    },
    {
      id: 103, pos: [42.03, 43.79], kind: 'motorised', approx_vehicles: 10, heading: 250, last_seen_secs: 240, moving: true, units: [],
      roles: [], deployed: false,
    },
    {
      id: 104, pos: [41.99, 44.04], kind: 'armour', approx_vehicles: 15, heading: 290, last_seen_secs: 660, moving: false, units: [],
      roles: [], deployed: false,
    },
    {
      id: 105, pos: [41.862, 43.392], kind: 'infantry', approx_vehicles: 5, heading: 0, last_seen_secs: 0, moving: false, units: [],
      roles: [], deployed: false,
    },
  ]

  const battles: GroundBattle[] = [
    {
      id: 41, pos: [41.996, 43.578], radius_m: 2500, near: 'Khashuri', since: now - 1260, live: true, ours: [2],
      intensity: 0.85, our_losses: 3, enemy_losses: 7, kind: 'assault', objective: 7,
    },
    {
      id: 42, pos: [42.337, 43.462], radius_m: 2500, near: 'Sachkhere', since: now - 380, live: true, ours: [3],
      intensity: 0.45, our_losses: 1, enemy_losses: 2, kind: 'meeting', objective: 5,
    },
  ]

  const players: SimPlayer[] = [
    {
      name: 'Viper 1-1', typ: 'F-16C_50', category: 'plane', pos: [42.02, 43.3], alt_m: 6100, heading: 90, speed_kts: 420, in_air: true, is_self: false,
      orbit: { c: [42.05, 43.32], r: 14_000, ang: 0 },
    },
    {
      name: 'Dillen', typ: 'Ka-50_3', category: 'helicopter', pos: [42.2, 43.3], alt_m: 45, heading: 40, speed_kts: 115, in_air: true, is_self: mode !== 'god',
      leg: { a: [42.12, 43.1], b: [42.31, 43.39], t: 0.35 },
    },
    {
      name: 'Hog 2', typ: 'A-10C_2', category: 'plane', pos: [41.98, 43.55], alt_m: 3900, heading: 180, speed_kts: 290, in_air: true, is_self: false,
      orbit: { c: [41.99, 43.53], r: 8000, ang: 2 },
    },
    {
      name: 'Tusker', typ: 'T-72B', category: 'ground', pos: [42.336, 43.438], alt_m: 610, heading: 80, speed_kts: 0, in_air: false, is_self: false,
    },
  ]

  const events: GroundEvent[] = [
    { at: now - 40, kind: 'kill', text: '2nd Armd Coy destroyed an enemy BMP-2 at Khashuri', pos: [41.995, 43.588], formation: 2 },
    { at: now - 95, kind: 'loss', text: '2nd Armd Coy lost a T-72B to an ATGM', pos: [41.996, 43.567], formation: 2 },
    { at: now - 160, kind: 'contact', text: '3rd Mot Coy: enemy armour, 5 vehicles, east of Sachkhere', pos: [42.334, 43.49], formation: 3 },
    { at: now - 300, kind: 'broken', text: '5th Inf Coy has broken and is falling back on Chiatura', pos: [42.33, 43.21], formation: 5 },
    { at: now - 380, kind: 'battle', text: 'Meeting engagement at Sachkhere', pos: [42.337, 43.462], formation: 3 },
    { at: now - 520, kind: 'order', text: 'Viper 1-1 ordered 1st Mech Coy to attack Borjomi', pos: [42.035, 43.17], formation: 1 },
    { at: now - 700, kind: 'supply', text: '5th Inf Coy is out of supply', pos: [42.35, 43.15], formation: 5 },
    { at: now - 840, kind: 'assault', text: '2nd Armd Coy is assaulting Khashuri', pos: [41.994, 43.6], formation: 2 },
    { at: now - 1260, kind: 'battle', text: 'Battle for Khashuri', pos: [41.996, 43.578], formation: 2 },
    { at: now - 1500, kind: 'raised', text: '4th Armd Coy raised at Kutaisi', pos: [42.176, 42.482], formation: 4 },
    { at: now - 2100, kind: 'arrived', text: '6th Mech Coy arrived at Zestaponi', pos: [42.11, 43.045], formation: 6 },
  ]

  function trailFor(f: SimFormation): LatLon[] {
    const out: LatLon[] = []
    for (let m = Math.max(0, f.at - 3000); m < f.at; m += 250) out.push(along(f.route, m).pos)
    return out
  }

  function lead(f: SimFormation) {
    return along(f.route, f.at)
  }

  /** Where each vehicle is: a column back down the road while moving, a line
   *  abreast (support in a second rank) once deployed, in between while deploying. */
  function unitsFor(f: SimFormation): GroundUnit[] {
    const typ = BLUE_TYP
    const head = lead(f)
    const deployK = f.moving ? 0 : Math.min(1, f.stoppedFor / 18)
    const front = f.roles.filter((r) => r !== 'truck' && r !== 'artillery' && r !== 'sam')
    const back = f.roles.filter((r) => r === 'truck' || r === 'artillery' || r === 'sam')
    const linePos = new Map<number, { pos: LatLon; hdg: number }>()
    let fi = 0
    let bi = 0
    f.roles.forEach((r, i) => {
      const isBack = r === 'truck' || r === 'artillery' || r === 'sam'
      const k = isBack ? bi++ : fi++
      const n = isBack ? back.length : front.length
      const across = (k - (n - 1) / 2) * LINE_SPACING * (r === 'infantry' ? 0.6 : 1)
      const behind = isBack ? 220 : 0
      const p = offset(offset(head.pos, f.face + 90, across), f.face + 180, behind + jit(i + f.id * 31) * 12)
      linePos.set(i, { pos: p, hdg: (f.face + jit(i * 7 + f.id) * 12 + 360) % 360 })
    })
    return f.roles.map((role, i) => {
      const col = along(f.route, Math.max(0, f.at - i * COLUMN_SPACING))
      const line = linePos.get(i) ?? col
      const pos = deployK > 0 ? lerp(col.pos, line.pos, deployK) : col.pos
      const hdg = deployK > 0 ? lerpHeading(col.hdg, line.hdg, deployK) : col.hdg
      return { pos, heading: hdg, role, typ: typ[role] }
    })
  }

  function enemyUnits(e: SimEnemy): GroundUnit[] {
    if (e.last_seen_secs > 0 || !e.roles.length) return []
    return e.roles.map((role, i) => {
      if (e.deployed) {
        const across = (i - (e.roles.length - 1) / 2) * LINE_SPACING
        return {
          pos: offset(offset(e.pos, e.heading + 90, across), e.heading + 180, jit(i + e.id) * 25),
          heading: (e.heading + jit(i * 3 + e.id) * 15 + 360) % 360,
          role, typ: RED_TYP[role],
        }
      }
      return { pos: offset(e.pos, e.heading + 180, i * COLUMN_SPACING), heading: e.heading, role, typ: RED_TYP[role] }
    })
  }

  function formationOut(f: SimFormation): GroundFormation {
    const head = lead(f)
    const remaining: LatLon[] = f.moving
      ? [head.pos, ...f.route.filter((_, i) => pathLength(f.route.slice(0, i + 1)) > f.at)]
      : []
    const kmToGo = remaining.length > 1 ? pathLength(remaining) / 1000 : 0
    const tgt = f.target != null ? objById(f.target) : undefined
    const home = objById(f.home)
    const makeUp: Partial<Record<GroundRole, number>> = {}
    for (const r of f.roles) makeUp[r] = (makeUp[r] ?? 0) + 1
    const alive = f.roles.length
    const deployment: GroundFormation['deployment'] = f.moving
      ? 'column'
      : f.dugIn ? 'dug_in' : f.stoppedFor < 18 ? 'deploying' : 'deployed'
    const depMul = deployment === 'column' ? 0.7 : deployment === 'deploying' ? 0.8 : deployment === 'dug_in' ? 1.25 : 1
    const powerFull = f.total * 10
    return {
      id: f.id,
      name: f.name,
      pos: head.pos,
      heading: f.moving ? head.hdg : f.face,
      order: f.order,
      target: f.target,
      target_name: tgt?.name ?? null,
      posture: f.assaulting ? 'assaulting' : f.moving ? 'moving' : 'holding',
      alive,
      total: f.total,
      has_infantry: f.roles.some((r) => r === 'infantry' || r === 'apc' || r === 'ifv'),
      live: f.live,
      halted: f.halted,
      engaged: f.engaged,
      home: f.home,
      home_name: home?.name ?? '?',
      commander: f.commander,
      locked_mins: f.commander ? f.locked : null,
      path: remaining,
      km_to_go: Math.round(kmToGo * 10) / 10,
      eta_mins: f.moving && f.speed > 0 ? Math.round((kmToGo / f.speed) * 60) : null,
      kind: f.kind,
      units: unitsFor(f),
      make_up: makeUp,
      power: Math.round(alive * 10 * (f.supply / 100 * 0.5 + 0.5) * (f.morale / 100 * 0.5 + 0.5) * depMul),
      power_full: powerFull,
      supply_pct: Math.round(f.supply),
      in_supply: f.inSupply,
      morale_pct: Math.round(f.morale),
      broken: f.broken,
      deployment,
      speed_kph: f.moving ? f.speed : 0,
      trail: f.trail,
      losses: f.losses,
      kills: f.kills,
    }
  }

  function pushEvent(e: Omit<GroundEvent, 'at'>) {
    events.unshift({ at: now, ...e })
    if (events.length > 40) events.length = 40
  }

  function stepPlayers(dt: number) {
    for (const p of players) {
      const mps = p.speed_kts * 0.5144
      if (p.orbit) {
        p.orbit.ang += (mps * dt) / p.orbit.r
        const a = p.orbit.ang
        p.pos = offset(p.orbit.c, (a * 180) / Math.PI, p.orbit.r)
        p.heading = ((a * 180) / Math.PI + 90) % 360
      } else if (p.leg) {
        const len = dist(p.leg.a, p.leg.b)
        p.leg.t += (mps * dt) / len
        const phase = p.leg.t % 2
        const fwd = phase < 1
        const t = fwd ? phase : 2 - phase
        p.pos = lerp(p.leg.a, p.leg.b, t)
        p.heading = fwd ? bearing(p.leg.a, p.leg.b) : bearing(p.leg.b, p.leg.a)
        p.alt_m = 40 + 25 * Math.sin(p.leg.t * 9)
      }
    }
  }

  function stepFormations(dt: number) {
    for (const f of forms) {
      if (!f.moving) {
        f.stoppedFor += dt
        continue
      }
      const len = pathLength(f.route)
      f.at += (f.speed / 3.6) * dt * TIME_ACCEL
      const last = f.trail[f.trail.length - 1]
      const head = lead(f).pos
      if (!last || dist(last, head) > 250) {
        f.trail.push(head)
        if (f.trail.length > 40) f.trail.shift()
      }
      if (f.at >= len) {
        f.at = len
        f.moving = false
        f.stoppedFor = 0
        f.face = bearing(f.route[Math.max(0, f.route.length - 2)], f.route[f.route.length - 1])
        const tgt = f.target != null ? objById(f.target) : undefined
        if (f.broken) {
          f.broken = false
          f.morale = 35
          pushEvent({ kind: 'arrived', text: `${f.name} reached ${tgt?.name ?? 'safety'} and is rallying`, pos: head, formation: f.id })
        } else if (f.order === 'attack' && tgt) {
          f.assaulting = true
          f.engaged = true
          pushEvent({ kind: 'assault', text: `${f.name} is assaulting ${tgt.name}`, pos: tgt.pos, formation: f.id })
        } else {
          pushEvent({ kind: 'arrived', text: `${f.name} arrived at ${tgt?.name ?? 'its position'}`, pos: head, formation: f.id })
        }
      }
    }
  }

  function stepEnemies(dt: number) {
    for (const e of enemies) {
      if (e.last_seen_secs > 0) e.last_seen_secs += dt
      if (e.drift && e.last_seen_secs === 0) {
        const d = dist(e.pos, e.drift.to)
        if (d < 30) {
          e.drift = undefined
          e.moving = false
          e.deployed = true
        } else {
          e.heading = bearing(e.pos, e.drift.to)
          e.pos = offset(e.pos, e.heading, Math.min(d, e.drift.mps * dt * TIME_ACCEL))
        }
      }
    }
  }

  function stepBattles() {
    if (clock < nextEventAt) return
    nextEventAt = clock + 10 + Math.abs(jit(clock)) * 10
    const b = battles[Math.floor(Math.abs(jit(clock * 3)) * battles.length) % battles.length]
    const f = forms.find((x) => x.id === b.ours[0])
    if (!f) return
    b.intensity = Math.max(0.25, Math.min(1, b.intensity + jit(clock * 5) * 0.15))
    if (jit(clock * 7) > -0.2) {
      b.enemy_losses++
      f.kills++
      pushEvent({ kind: 'kill', text: `${f.name.split(' (')[0]} destroyed an enemy ${RED_TYP[jit(clock) > 0 ? 'tank' : 'ifv']} near ${b.near}`, pos: b.pos, formation: f.id })
    } else if (f.roles.length > 3) {
      b.our_losses++
      f.losses++
      const lost = f.roles.splice(Math.floor(Math.abs(jit(clock * 11)) * f.roles.length), 1)[0]
      f.morale = Math.max(10, f.morale - 4)
      pushEvent({ kind: 'loss', text: `${f.name.split(' (')[0]} lost a ${BLUE_TYP[lost]} near ${b.near}`, pos: b.pos, formation: f.id })
    }
  }

  function picture(): GroundPicture {
    return {
      side: 'Blue',
      enabled: true,
      max_formations: 8,
      live: forms.filter((f) => f.live).length,
      max_live: 6,
      player_lock_secs: 3600,
      formations: forms.map(formationOut),
      enemy: enemies.map((e) => ({
        id: e.id, pos: e.pos, kind: e.kind, approx_vehicles: e.approx_vehicles, heading: e.heading,
        last_seen_secs: Math.round(e.last_seen_secs), moving: e.moving, units: enemyUnits(e),
      })),
      battles: battles.map((b) => ({ ...b, ours: [...b.ours] })),
      objectives: objectives.map((o) => ({ ...o })),
      players: players.map((p): LivePlayer => ({
        name: p.name, typ: p.typ, category: p.category, pos: [...p.pos] as LatLon, alt_m: p.alt_m,
        heading: p.heading, speed_kts: p.speed_kts, in_air: p.in_air, is_self: p.is_self,
      })),
      events: events.slice(0, 30).map((e) => ({ ...e })),
      time: now,
      spot_m: 6000,
      engage_m: 3000,
      can_command: mode !== 'view' && mode !== 'god',
      god_mode: mode === 'god',
    }
  }

  function step(dt: number) {
    clock += dt
    now = Math.floor(Date.now() / 1000)
    stepPlayers(dt)
    stepFormations(dt)
    stepEnemies(dt)
    stepBattles()
  }

  function routeTo(f: SimFormation, to: LatLon): void {
    const head = lead(f).pos
    const behind = f.trail.slice(-6)
    const mid = offset(lerp(head, to, 0.5), bearing(head, to) + 90, jit(f.id + clock) * 1500)
    const route: LatLon[] = [...behind, head, mid, to]
    f.route = route
    f.at = pathLength([...behind, head])
    f.moving = true
    f.assaulting = false
    f.engaged = false
    f.halted = false
    f.dugIn = false
    f.speed = f.broken ? 9 : f.kind === 'infantry' ? 12 : f.kind === 'armour' ? 32 : 28
  }

  function command(cmd: GroundCommand): GroundCommandReply {
    if (cmd.kind === 'raise') {
      const o = objectives.find((x) => x.id === cmd.objective)
      if (!o || o.owner !== 'Blue') return { ok: false, message: 'You can only raise a formation at one of our own bases.', formation: null }
      if ((o.can_raise ?? 0) <= 0) return { ok: false, message: `${o.name} can't spare the troops: its garrison is too thin.`, formation: null }
      if (forms.length >= 8) return { ok: false, message: 'Every formation slot is in use (8/8).', formation: null }
      o.can_raise = (o.can_raise ?? 1) - 1
      const id = nextId++
      const face = bearing(o.pos, [41.994, 43.6])
      const f = sim({
        id, name: `${id}th Mech Coy (${o.name})`, kind: 'mechanised',
        roles: roles([['ifv', 5], ['tank', 2], ['truck', 1]]), route: [offset(o.pos, face + 180, 800), o.pos], home: o.id,
        commander: 'Dillen', locked: 60, face, stoppedFor: 0,
      })
      f.at = pathLength(f.route)
      forms.push(f)
      pushEvent({ kind: 'raised', text: `${f.name} raised at ${o.name}`, pos: o.pos, formation: id })
      return { ok: true, message: `${f.name} raised at ${o.name}. It is yours for 60 min.`, formation: id }
    }
    const f = forms.find((x) => x.id === cmd.formation)
    if (!f) return { ok: false, message: 'That formation no longer exists.', formation: null }
    const short = f.name.split(' (')[0]
    if (f.broken && cmd.kind !== 'withdraw' && cmd.kind !== 'release') {
      return { ok: false, message: `${short} has broken and is falling back; it won't take orders until it rallies.`, formation: f.id }
    }
    if (cmd.kind === 'hold') {
      f.order = 'hold'
      f.target = null
      if (f.moving) {
        f.moving = false
        f.stoppedFor = 0
        f.face = lead(f).hdg
      }
      f.assaulting = false
      f.commander = 'Dillen'
      f.locked = 60
      pushEvent({ kind: 'order', text: `Dillen ordered ${short} to hold`, pos: lead(f).pos, formation: f.id })
      return { ok: true, message: `${short}: holding where it is.`, formation: f.id }
    }
    if (cmd.kind === 'release') {
      if (!f.commander) return { ok: false, message: `${short} is already under AI command.`, formation: f.id }
      f.commander = null
      f.locked = null
      return { ok: true, message: `${short} handed back to the AI.`, formation: f.id }
    }
    if (!('objective' in cmd)) return { ok: false, message: 'Unknown order.', formation: f.id }
    const o = objectives.find((x) => x.id === cmd.objective)
    if (!o) return { ok: false, message: 'No such objective.', formation: f.id }
    if (cmd.kind === 'attack' && o.owner === 'Blue') {
      return { ok: false, message: `${o.name} is already ours. Defend it instead.`, formation: f.id }
    }
    if ((cmd.kind === 'defend' || cmd.kind === 'withdraw') && o.owner !== 'Blue') {
      return { ok: false, message: `${o.name} isn't ours to ${cmd.kind}. Attack it instead.`, formation: f.id }
    }
    if (km(lead(f).pos, o.pos) > 90) {
      return { ok: false, message: `${o.name} is ${Math.round(km(lead(f).pos, o.pos))} km away; too far to march (90 km max).`, formation: f.id }
    }
    f.order = cmd.kind
    f.target = o.id
    f.commander = 'Dillen'
    f.locked = 60
    routeTo(f, o.pos)
    const k = pathLength(f.route.slice(-3)) / 1000
    const verb = cmd.kind === 'attack' ? 'attacking' : cmd.kind === 'defend' ? 'moving to defend' : 'withdrawing to'
    pushEvent({ kind: 'order', text: `Dillen ordered ${short} to ${cmd.kind} ${o.name}`, pos: lead(f).pos, formation: f.id })
    return {
      ok: true,
      message: `${short}: ${verb} ${o.name}, ${k.toFixed(0)} km, about ${Math.round((k / f.speed) * 60)} min.`,
      formation: f.id,
    }
  }

  return { picture, step, command }
}
