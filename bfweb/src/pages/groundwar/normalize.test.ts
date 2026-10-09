import { describe, expect, it } from 'vitest'
import type { GroundPicture } from '../../api'
import { normalizePicture } from './normalize'

// What a bfdb + bflib.dll from before the realistic ground war sent (the
// bfprotocols GroundPicture at 2aff2f60): no players, events, units or supply.
const OLD = {
  side: 'Blue',
  enabled: true,
  max_formations: 6,
  live: 1,
  max_live: 4,
  player_lock_secs: 3600,
  formations: [{
    id: 3, name: '1st Mech Coy (Senaki)', pos: [42.2, 42.0], heading: 90, order: 'attack', target: 7,
    target_name: 'Gori', posture: 'moving', alive: 9, total: 12, has_infantry: true, live: false,
    halted: false, engaged: false, home: 2, home_name: 'Senaki', commander: null, locked_mins: null,
    path: [[42.2, 42.0], [42.0, 44.1]], km_to_go: 41.5, eta_mins: 80,
  }],
  enemy: [{ pos: [42.0, 44.0], kind: 'armour', approx_vehicles: 10, heading: 270 }],
  battles: [{ id: 1, pos: [42.1, 43.0], radius_m: 2500, near: 'Gori', since: 0, live: false, ours: [3] }],
  objectives: [{ id: 7, name: 'Gori', pos: [42.0, 44.1], owner: 'Red', kind: 'fob', health: null, threatened: null, can_raise: null, being_captured: false }],
  can_command: true,
  god_mode: false,
} as unknown as GroundPicture

describe('normalizePicture', () => {
  it('fills an old picture out to the current shape', () => {
    const p = normalizePicture(OLD)
    expect(p.players).toEqual([])
    expect(p.events).toEqual([])
    expect(p.players.find((x) => x.is_self)).toBeUndefined()
    expect(p.engage_m).toBeGreaterThan(0)
    const f = p.formations[0]
    expect(f.kind).toBe('mechanised')
    expect(f.units).toEqual([])
    expect(f.trail).toEqual([])
    expect(f.in_supply).toBe(true)
    expect(f.broken).toBe(false)
    expect(f.losses).toBe(3)
    const e = p.enemy[0]
    expect(e.id).toBeTypeOf('number')
    expect(e.last_seen_secs).toBe(0)
    expect(e.units).toEqual([])
    expect(p.battles[0].our_losses).toBe(0)
    expect(p.objectives[0].supply).toBeNull()
    expect(p.can_command).toBe(true)
  })

  it('leaves a current picture alone', () => {
    const cur = normalizePicture(OLD)
    expect(normalizePicture(cur)).toEqual(cur)
  })

  it('survives a picture with missing lists', () => {
    const p = normalizePicture({ side: 'Red', enabled: false } as unknown as GroundPicture)
    expect(p.formations).toEqual([])
    expect(p.objectives).toEqual([])
    expect(p.battles).toEqual([])
  })
})
