// A dev-only stand-in for /api/command and /api/command/order, set in the
// same corner of Georgia as `groundwarMock`, so the command layer can be
// built and checked with `?mock` and no server. Assets move: the tanker
// orbits, the convoy drives, ordered units go where they are sent.
import type { Asset, AssetUnit, CommandOrder, CommandPicture, CommandReply, LatLon, LaunchOption } from '../api'

export interface CommandMock {
  picture(): CommandPicture
  step(dtSecs: number): void
  order(o: CommandOrder): CommandReply
}

const KM_LAT = 1 / 110.6
const KM_LON = 1 / 82.5

function unit(typ: string, pos: LatLon, heading: number, alt_m = 300, speed_kts = 0): AssetUnit {
  return { typ, pos, heading, alt_m, speed_kts }
}

function offset(p: LatLon, east_km: number, north_km: number): LatLon {
  return [p[0] + north_km * KM_LAT, p[1] + east_km * KM_LON]
}

function bearing(a: LatLon, b: LatLon): number {
  const dx = (b[1] - a[1]) / KM_LON
  const dy = (b[0] - a[0]) / KM_LAT
  return ((Math.atan2(dx, dy) * 180) / Math.PI + 360) % 360
}

function dist_km(a: LatLon, b: LatLon): number {
  return Math.hypot((b[1] - a[1]) / KM_LON, (b[0] - a[0]) / KM_LAT)
}

interface Sim {
  a: Asset
  /** km/s */
  speed: number
  orbit?: { c: LatLon; r: number; t: number }
}

export function createCommandMock(): CommandMock {
  let treasury = 2400
  const sims: Sim[] = []
  const add = (a: Omit<Asset, 'units' | 'alive' | 'total' | 'live'>, units: AssetUnit[], speed = 0, orbit?: Sim['orbit']) => {
    sims.push({ a: { ...a, units, alive: units.length, total: units.length, live: true }, speed, orbit })
  }
  const tankerC: LatLon = [42.05, 42.7]
  add({ id: 501, name: 'Texaco 1', kind: 'air', role: 'Tanker', typ: 'KC-135', pos: tankerC, heading: 90, alt_m: 7000, speed_kts: 420, task: 'Tanker track', dest: null, base: 'Kutaisi', range_m: null, orders: ['station', 'rtb'] },
    [unit('KC-135', tankerC, 90, 7000, 420)], 0, { c: tankerC, r: 18, t: 0 })
  const awacsC: LatLon = [41.95, 42.45]
  add({ id: 502, name: 'Magic 1', kind: 'air', role: 'AWACS', typ: 'E-3A', pos: awacsC, heading: 0, alt_m: 9000, speed_kts: 380, task: 'AWACS orbit', dest: null, base: 'Kutaisi', range_m: null, orders: ['station', 'rtb'] },
    [unit('E-3A', awacsC, 0, 9000, 380)], 0, { c: awacsC, r: 22, t: 1.2 })
  const capP: LatLon = [42.25, 43.35]
  add({ id: 503, name: 'Enfield 2', kind: 'air', role: 'Fighters', typ: 'F-15C', pos: capP, heading: 70, alt_m: 8000, speed_kts: 450, task: 'CAP over Sachkhere', dest: null, base: 'Kutaisi', range_m: null, orders: ['station', 'rtb'] },
    [unit('F-15C', capP, 70, 8000, 450), unit('F-15C', offset(capP, -1.2, -0.8), 70, 8000, 450)], 0, { c: capP, r: 9, t: 2 })
  const convoyP: LatLon = [42.06, 43.12]
  add({ id: 504, name: 'Supply convoy 7', kind: 'convoy', role: 'Supply convoy', typ: 'Ural-375', pos: convoyP, heading: 40, alt_m: 300, speed_kts: 22, task: 'Supplies to Chiatura', dest: [42.29, 43.285], base: 'Chiatura', range_m: null, orders: [] },
    [0, 1, 2, 3].map((i) => unit('Ural-375', offset(convoyP, -0.08 * i, -0.08 * i), 40, 300, 22)), 0.011)
  const arty: LatLon = [42.13, 43.06]
  add({ id: 505, name: 'Zestaponi battery', kind: 'artillery', role: 'Artillery', typ: 'M-109', pos: arty, heading: 90, alt_m: 260, speed_kts: 0, task: null, dest: null, base: 'Zestaponi', range_m: 22000, orders: ['fire'] },
    [unit('M-109', arty, 90), unit('M-109', offset(arty, 0.12, 0), 90), unit('M-109', offset(arty, 0.24, 0), 90)])
  const troops: LatLon = [42.31, 43.38]
  add({ id: 506, name: 'Infantry squad (Viper)', kind: 'troops', role: 'Standard Squad', typ: 'Soldier M4', pos: troops, heading: 120, alt_m: 500, speed_kts: 0, task: null, dest: null, base: null, range_m: null, orders: ['move'] },
    [0, 1, 2, 3, 4].map((i) => unit('Soldier M4', offset(troops, 0.03 * i, 0.02 * (i % 2)), 120, 500)))
  const sam: LatLon = [42.16, 42.9]
  add({ id: 507, name: 'Hawk site (Rook)', kind: 'deployed', role: 'Hawk', typ: 'Hawk ln', pos: sam, heading: 80, alt_m: 200, speed_kts: 0, task: null, dest: null, base: null, range_m: null, orders: ['move'] },
    [unit('Hawk ln', sam, 80), unit('Hawk sr', offset(sam, 0.2, 0.1), 80), unit('Hawk tr', offset(sam, -0.15, 0.12), 80)])
  const cvn: LatLon = [41.9, 41.45]
  add({ id: 508, name: 'CVN-74 group', kind: 'naval', role: 'Carrier group', typ: 'Stennis', pos: cvn, heading: 300, alt_m: 0, speed_kts: 18, task: null, dest: null, base: 'CVN-74 group', range_m: null, orders: ['sail'] },
    [unit('Stennis', cvn, 300, 0, 18), unit('TICONDEROG', offset(cvn, 1.5, 0.8), 300, 0, 18)], 0.005)

  const launch: LaunchOption[] = [
    { kind: 'strike', objective: 8, objective_name: 'Borjomi', pos: [41.842, 43.387], cost: 520, why: 'enemy factory feeding the Khashuri front', ready: true },
    { kind: 'sead', objective: 12, objective_name: 'Java', pos: [42.398, 43.928], cost: 300, why: 'SA-11 covering the northern approach', ready: true },
    { kind: 'cap', objective: 5, objective_name: 'Sachkhere', pos: [42.343, 43.415], cost: 180, why: 'enemy air seen near our base', ready: true },
    { kind: 'artillery', objective: 7, objective_name: 'Khashuri', pos: [41.994, 43.6], cost: 120, why: 'enemy armour assembling', ready: true },
    { kind: 'convoy', objective: 5, objective_name: 'Sachkhere', pos: [42.343, 43.415], cost: 90, why: 'supply at 27%', ready: true },
    { kind: 'bomber', objective: 10, objective_name: 'Gori', pos: [41.984, 44.112], cost: 3400, why: 'enemy command centre', ready: false },
  ]

  const byId = (id: number) => sims.find((s) => s.a.id === id)
  const send = (s: Sim, to: LatLon, speed: number) => {
    s.orbit = undefined
    s.a.dest = to
    s.speed = speed
    s.a.task = 'Under orders'
  }

  return {
    picture(): CommandPicture {
      return {
        side: 'Blue',
        time: Math.floor(Date.now() / 1000),
        treasury,
        assets: sims.map((s) => structuredClone(s.a)),
        launch: launch.map((l) => ({ ...l, ready: l.ready && l.cost <= treasury })),
        hq: true,
        can_command: true,
        god_mode: false,
        commander: null,
      }
    },
    step(dt: number) {
      for (const s of sims) {
        const a = s.a
        if (s.orbit) {
          s.orbit.t += (dt * (a.speed_kts / 3600) * 1.852) / s.orbit.r
          const p = offset(s.orbit.c, Math.sin(s.orbit.t) * s.orbit.r, Math.cos(s.orbit.t) * s.orbit.r)
          const hdg = (((s.orbit.t * 180) / Math.PI) + 90) % 360
          const shift: LatLon = [p[0] - a.pos[0], p[1] - a.pos[1]]
          a.pos = p
          a.heading = hdg
          a.units = a.units.map((u) => ({ ...u, pos: [u.pos[0] + shift[0], u.pos[1] + shift[1]] as LatLon, heading: hdg }))
          continue
        }
        if (!a.dest || !s.speed) continue
        const d = dist_km(a.pos, a.dest)
        if (d < 0.3) {
          a.dest = null
          a.task = a.kind === 'convoy' ? 'Delivered' : 'Holding'
          continue
        }
        const hdg = bearing(a.pos, a.dest)
        const step = Math.min(d, s.speed * dt)
        const rad = (hdg * Math.PI) / 180
        const dE = Math.sin(rad) * step
        const dN = Math.cos(rad) * step
        a.pos = offset(a.pos, dE, dN)
        a.heading = hdg
        a.units = a.units.map((u) => ({ ...u, pos: offset(u.pos, dE, dN), heading: hdg }))
      }
    },
    order(o: CommandOrder): CommandReply {
      const pay = (n: number) => {
        if (n > treasury) return false
        treasury -= n
        return true
      }
      if ('move' in o) {
        const s = byId(o.move.group)
        if (!s) return { ok: false, message: 'no such group' }
        if (!pay(40)) return { ok: false, message: 'the treasury can’t pay for that' }
        send(s, o.move.to, 0.006)
        return { ok: true, message: `${s.a.name} moving (40 from the treasury)` }
      }
      if ('station' in o) {
        const s = byId(o.station.group)
        if (!s) return { ok: false, message: 'no such flight' }
        s.orbit = { c: o.station.at, r: s.a.role === 'Fighters' ? 9 : 18, t: 0 }
        s.a.task = 'New station'
        return { ok: true, message: `${s.a.name} retasked` }
      }
      if ('rtb' in o) {
        const s = byId(o.rtb.group)
        if (!s) return { ok: false, message: 'no such flight' }
        send(s, [42.176, 42.482], 0.2)
        return { ok: true, message: `${s.a.name} returning to base` }
      }
      if ('sail' in o) {
        const s = byId(o.sail.group)
        if (!s) return { ok: false, message: 'no such group' }
        send(s, o.sail.to, 0.01)
        return { ok: true, message: 'carrier group under way' }
      }
      if ('fire' in o) {
        const s = byId(o.fire.group)
        if (!s) return { ok: false, message: 'no such battery' }
        if (dist_km(s.a.pos, o.fire.at) * 1000 > (s.a.range_m ?? 0)) return { ok: false, message: `${s.a.name} can't reach that` }
        if (!pay(120)) return { ok: false, message: 'the treasury can’t pay for that' }
        return { ok: true, message: `${s.a.name} firing (120 from the treasury)` }
      }
      if ('barrage' in o) return pay(120) ? { ok: true, message: 'barrage on the marked point (120 from the treasury)' } : { ok: false, message: 'the treasury can’t pay for that' }
      if ('move_formation' in o) return { ok: true, message: 'formation moving to the marked position' }
      if ('convoy' in o) return pay(90) ? { ok: true, message: 'supplies on the road (90 from the treasury)' } : { ok: false, message: 'the treasury can’t pay for that' }
      if ('helo_supply' in o) return pay(150) ? { ok: true, message: 'helicopter supplies (150 from the treasury)' } : { ok: false, message: 'the treasury can’t pay for that' }
      if ('helo_troops' in o) return pay(200) ? { ok: true, message: 'helicopter troops (200 from the treasury)' } : { ok: false, message: 'the treasury can’t pay for that' }
      if ('launch' in o) {
        const l = launch.find((x) => x.kind === o.launch.kind && x.objective === o.launch.objective)
        if (!l) return { ok: false, message: 'the HQ has nothing like that it can run there now' }
        if (!pay(l.cost)) return { ok: false, message: `that costs ${l.cost} and the treasury has ${treasury}` }
        return { ok: true, message: `you ordered ${l.kind} on ${l.objective_name}` }
      }
      return { ok: false, message: 'unknown order' }
    },
  }
}
