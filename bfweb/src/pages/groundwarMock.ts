// Fixture for GroundWarPage's `?mock` mode (dev builds only -- the page tests
// `import.meta.env.DEV` before it touches this). Western Georgia, Blue
// pushing east out of Senaki and Kutaisi.
import type { GroundCommand, GroundCommandReply, GroundPicture } from '../api'

const now = Math.floor(Date.now() / 1000)

export const groundwarMock: GroundPicture = {
  side: 'Blue',
  enabled: true,
  max_formations: 6,
  live: 3,
  max_live: 6,
  player_lock_secs: 3600,
  can_command: true,
  god_mode: false,
  formations: [
    {
      id: 1, name: '1st Mech Coy (Senaki)', pos: [42.27, 42.25], heading: 80,
      order: 'attack', target: 7, target_name: 'Kutaisi', posture: 'moving',
      alive: 11, total: 12, has_infantry: true, live: true, halted: false, engaged: false,
      home: 2, home_name: 'Senaki', commander: 'Viper 1-1', locked_mins: 47,
      path: [[42.27, 42.25], [42.26, 42.38], [42.24, 42.48], [42.22, 42.6], [42.18, 42.69]],
      km_to_go: 38, eta_mins: 76,
    },
    {
      id: 2, name: '2nd Armd Coy (Kobuleti)', pos: [42.06, 42.05], heading: 40,
      order: 'attack', target: 8, target_name: 'Lanchkhuti', posture: 'assaulting',
      alive: 5, total: 9, has_infantry: false, live: true, halted: false, engaged: true,
      home: 1, home_name: 'Kobuleti', commander: null, locked_mins: null,
      path: [], km_to_go: 0, eta_mins: null,
    },
    {
      id: 3, name: '3rd Inf Coy (Zugdidi)', pos: [42.5, 41.92], heading: 0,
      order: 'defend', target: 3, target_name: 'Zugdidi', posture: 'holding',
      alive: 8, total: 8, has_infantry: true, live: false, halted: false, engaged: false,
      home: 3, home_name: 'Zugdidi', commander: null, locked_mins: null,
      path: [], km_to_go: 0, eta_mins: null,
    },
  ],
  enemy: [
    { pos: [42.08, 42.08], kind: 'mechanised', approx_vehicles: 10, heading: 220 },
    { pos: [42.3, 42.45], kind: 'armour', approx_vehicles: 5, heading: 270 },
  ],
  battles: [
    { id: 4, pos: [42.07, 42.07], radius_m: 2500, near: 'Lanchkhuti', since: now - 1260, live: true, ours: [2] },
    { id: 5, pos: [42.48, 41.95], radius_m: 2500, near: 'Zugdidi', since: now - 300, live: false, ours: [] },
  ],
  objectives: [
    { id: 1, name: 'Kobuleti', pos: [41.93, 41.86], owner: 'Blue', kind: 'airbase', health: 92, threatened: false, can_raise: 1, being_captured: false },
    { id: 2, name: 'Senaki', pos: [42.24, 42.05], owner: 'Blue', kind: 'airbase', health: 100, threatened: false, can_raise: 2, being_captured: false },
    { id: 3, name: 'Zugdidi', pos: [42.5, 41.87], owner: 'Blue', kind: 'fob', health: 64, threatened: true, can_raise: 0, being_captured: false },
    { id: 4, name: 'Poti', pos: [42.15, 41.67], owner: 'Blue', kind: 'naval', health: 100, threatened: false, can_raise: 0, being_captured: false },
    { id: 7, name: 'Kutaisi', pos: [42.18, 42.48], owner: 'Red', kind: 'airbase', health: null, threatened: null, can_raise: null, being_captured: false },
    { id: 8, name: 'Lanchkhuti', pos: [42.09, 42.03], owner: 'Red', kind: 'fob', health: null, threatened: null, can_raise: null, being_captured: true },
    { id: 9, name: 'Samtredia', pos: [42.16, 42.34], owner: 'Red', kind: 'logistics', health: null, threatened: null, can_raise: null, being_captured: false },
    { id: 10, name: 'Tskaltubo', pos: [42.33, 42.6], owner: 'Neutral', kind: 'farp', health: null, threatened: null, can_raise: null, being_captured: false },
  ],
}

export function groundwarMockCommand(cmd: GroundCommand): GroundCommandReply {
  return {
    ok: true,
    message: `(mock) ${cmd.kind} accepted`,
    formation: 'formation' in cmd ? cmd.formation : 9,
  }
}
