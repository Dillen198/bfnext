// A ground picture from any bfdb/engine, filled out to the current shape.
//
// The dashboard ships ahead of the server: a bfdb or bflib.dll from before the
// realistic ground war (Oct 2026) sends no players, events, units, trails,
// supply or morale, and its enemy contacts have no ids. Every field the screen
// reads gets a neutral default here, once, so nothing downstream has to guess
// (an older picture crashed the page on `pic.players.find`).
import type {
  GroundBattle, GroundEnemyContact, GroundFormation, GroundKind, GroundObjective, GroundPicture,
} from '../../api'

const KINDS: GroundKind[] = ['armour', 'mechanised', 'motorised', 'infantry']
const kindOf = (k: unknown, infantry?: boolean): GroundKind =>
  KINDS.includes(k as GroundKind) ? (k as GroundKind) : infantry ? 'mechanised' : 'armour'
const arr = <T>(v: unknown): T[] => (Array.isArray(v) ? (v as T[]) : [])
const num = (v: unknown, d: number): number => (typeof v === 'number' && Number.isFinite(v) ? v : d)

function formation(raw: Partial<GroundFormation>): GroundFormation {
  const f = raw as GroundFormation
  const total = num(f.total, 0)
  return {
    ...f,
    path: arr(f.path),
    kind: kindOf(f.kind, f.has_infantry),
    units: arr(f.units),
    make_up: f.make_up ?? {},
    power: num(f.power, 0),
    power_full: num(f.power_full, 0),
    // Unknown is shown as fine: an old server can't run anyone out of supply.
    supply_pct: num(f.supply_pct, 100),
    in_supply: f.in_supply ?? true,
    morale_pct: num(f.morale_pct, 100),
    broken: f.broken ?? false,
    deployment: f.deployment ?? 'column',
    speed_kph: num(f.speed_kph, 0),
    trail: arr(f.trail),
    losses: num(f.losses, Math.max(0, total - num(f.alive, total))),
    kills: num(f.kills, 0),
  }
}

function enemy(raw: Partial<GroundEnemyContact>, i: number): GroundEnemyContact {
  const e = raw as GroundEnemyContact
  return {
    ...e,
    // Old contacts had no id; their order is stable enough within a picture.
    id: num(e.id, -1 - i),
    kind: kindOf(e.kind),
    heading: num(e.heading, 0),
    last_seen_secs: num(e.last_seen_secs, 0),
    moving: e.moving ?? false,
    units: arr(e.units),
  }
}

function battle(raw: Partial<GroundBattle>): GroundBattle {
  const b = raw as GroundBattle
  return {
    ...b,
    ours: arr(b.ours),
    intensity: num(b.intensity, 0.5),
    our_losses: num(b.our_losses, 0),
    enemy_losses: num(b.enemy_losses, 0),
    kind: b.kind ?? 'meeting',
    objective: b.objective ?? null,
  }
}

function objective(raw: Partial<GroundObjective>): GroundObjective {
  const o = raw as GroundObjective
  return {
    ...o,
    health: o.health ?? null,
    threatened: o.threatened ?? null,
    can_raise: o.can_raise ?? null,
    being_captured: o.being_captured ?? false,
    supply: o.supply ?? null,
    garrison: o.garrison ?? null,
  }
}

export function normalizePicture(raw: GroundPicture): GroundPicture {
  const p = raw as Partial<GroundPicture>
  return {
    ...(p as GroundPicture),
    formations: arr<Partial<GroundFormation>>(p.formations).map(formation),
    enemy: arr<Partial<GroundEnemyContact>>(p.enemy).map(enemy),
    battles: arr<Partial<GroundBattle>>(p.battles).map(battle),
    objectives: arr<Partial<GroundObjective>>(p.objectives).map(objective),
    players: arr(p.players),
    events: arr(p.events),
    time: num(p.time, Date.now() / 1000),
    spot_m: num(p.spot_m, 7000),
    engage_m: num(p.engage_m, 3000),
    can_command: p.can_command ?? false,
    god_mode: p.god_mode ?? false,
  }
}
