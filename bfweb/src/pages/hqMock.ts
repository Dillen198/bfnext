// Dev-only fixture for the HQ page (`/hq?mock`). Roughly a Georgian front:
// Blue holding the west, pushing on Gori.
import type { HqCommand, HqReply, HqView } from '../api'

const now = Date.now()
const ago = (m: number) => new Date(now - m * 60_000).toISOString()

export const hqMock: HqView = {
  side: 'Blue',
  enabled: true,
  paused: false,
  posture: 'offensive',
  source: 'strategist',
  main_effort: { id: 12, name: 'Gori', pos: [41.98, 44.11] },
  defend: [{ id: 4, name: 'Kutaisi', pos: [42.18, 42.48] }],
  supply_priority: [
    { id: 7, name: 'Khashuri FOB', pos: [41.99, 43.6] },
    { id: 4, name: 'Kutaisi', pos: [42.18, 42.48] },
  ],
  avoid: [],
  weights: { air: 1.2, fires: 1.3, logistics: 1.0, troops: 1.6, ground: 1.2 },
  intent: 'Take Gori while its logistics are down; hold Kutaisi against the armour moving west.',
  reasons: ['holding 22 of 41 objectives (54%)', '1 base(s) being captured, 2 threatened, 0 with no logistics left', 'posture OFFENSIVE (score 0.8)', 'main effort Gori'],
  directive: {
    directive: { posture: 'offensive', main_effort: 12, rationale: 'Gori has no logistics left and sits 18 km from Khashuri; troops can take it before Red resupplies.' },
    received: ago(6),
    expires: ago(-34),
  },
  override: null,
  treasury: 4820,
  reserve: 300,
  gap_factor: 0.82,
  available: { bomber: 350, awacs: 200, tanker: 150, cap: 250, strike: 300, sead: 300, recon: 120, artillery: 150, convoy: 80, helo_supply: 120, helo_troops: 180, reinforce: 250 },
  ops: [
    { id: 41, kind: 'helo_troops', line: 'troops', target: 12, target_name: 'Gori', pos: [41.98, 44.11], started: ago(4), cost: 180, status: 'active', detail: 'helo starting up', request: null, support: [] },
    { id: 40, kind: 'bomber', line: 'air', target: null, target_name: 'JTAC target at Gori', pos: [41.98, 44.11], started: ago(9), cost: 900, status: 'active', detail: 'under way, 1 escort flight(s), SEAD going in first', request: 3, support: ['sead', 'escort'] },
    { id: 38, kind: 'convoy', line: 'logistics', target: 7, target_name: 'Khashuri FOB', pos: [41.99, 43.6], started: ago(15), cost: 80, status: 'active', detail: 'supply convoy dispatched', request: null, support: [] },
    { id: 35, kind: 'artillery', line: 'fires', target: 12, target_name: 'Gori', pos: [41.98, 44.11], started: ago(22), cost: 150, status: 'succeeded', detail: 'fired', request: null, support: [] },
    { id: 31, kind: 'sead', line: 'air', target: null, target_name: 'air defence near Gori', pos: [41.95, 44.2], started: ago(48), cost: 300, status: 'failed', detail: 'package lost', request: null, support: [] },
  ],
  requests: [
    { id: 3, kind: 'cas', by: 'Viper 1-1', target: 12, target_name: 'Gori', created: ago(11), status: 'answered', answer: 'strike tasked (HQ strike 40 airborne)' },
    { id: 5, kind: 'supply', by: 'Hip 2', target: 4, target_name: 'Kutaisi', created: ago(2), status: 'open', answer: '' },
  ],
  record: [
    { kind: 'strike', launched: 6, succeeded: 4, failed: 1 },
    { kind: 'sead', launched: 3, succeeded: 1, failed: 2 },
    { kind: 'convoy', launched: 9, succeeded: 7, failed: 1 },
    { kind: 'helo_troops', launched: 2, succeeded: 1, failed: 0 },
  ],
  log: [
    { at: ago(4), text: 'HQ: helo troop insertion on Gori -- no logistics left, troops will take it (helo starting up)' },
    { at: ago(6), text: 'HQ: strategist directive -- posture OFFENSIVE, main effort #12' },
    { at: ago(9), text: 'HQ: strike on Gori -- main effort (HQ strike 40 airborne)' },
  ],
  picture: {
    territory_pct: 53.7,
    own_objectives: 22,
    enemy_objectives: 19,
    objectives: [
      { id: 4, name: 'Kutaisi', kind: 'airbase', owner: 'own', pos: [42.18, 42.48], health: 74, logi: 100, supply: 51, fuel: 70, threatened: true, being_captured: false, capturable: false, front_km: 31, inbound: false },
      { id: 7, name: 'Khashuri FOB', kind: 'fob', owner: 'own', pos: [41.99, 43.6], health: 88, logi: 80, supply: 34, fuel: 41, threatened: false, being_captured: false, capturable: false, front_km: 18, inbound: true },
      { id: 2, name: 'Senaki', kind: 'airbase', owner: 'own', pos: [42.24, 42.05], health: 100, logi: 100, supply: 90, fuel: 92, threatened: false, being_captured: false, capturable: false, front_km: 60, inbound: false },
      { id: 12, name: 'Gori', kind: 'fob', owner: 'enemy', pos: [41.98, 44.11], health: 31, logi: 0, supply: 40, fuel: 40, threatened: false, being_captured: false, capturable: true, front_km: 18, inbound: false },
      { id: 13, name: 'Tskhinvali', kind: 'logistics', owner: 'enemy', pos: [42.23, 43.97], health: 92, logi: 100, supply: 80, fuel: 80, threatened: false, being_captured: false, capturable: false, front_km: 35, inbound: false },
      { id: 15, name: 'Tbilisi', kind: 'airbase', owner: 'enemy', pos: [41.67, 44.95], health: 100, logi: 100, supply: 100, fuel: 100, threatened: false, being_captured: false, capturable: false, front_km: 72, inbound: false },
    ],
    humans: 5,
    humans_fixed_wing_airborne: 2,
    humans_helo_airborne: 1,
    enemy_air_detected: 3,
    enemy_ground: [{ pos: [42.1, 43.2], class: 'armor', count: 6, near: 'Kutaisi', near_km: 22, age_mins: 7 }],
    enemy_sams: [{ pos: [41.95, 44.2], class: 'ads', count: 4, near: 'Gori', near_km: 8, age_mins: 30 }],
    formations: 4,
    formations_idle: 1,
    enemy_formations_in_contact: 1,
    ai_air_up: 1,
    logistics_out: 1,
    troops_out: 1,
  },
  next_think_secs: 74,
  can_command: true,
  can_request: true,
  god_mode: false,
}

export function hqMockCommand(cmd: HqCommand): HqReply {
  switch (cmd.kind) {
    case 'request':
      return { ok: true, message: `HQ copies: ${cmd.request}. You will hear back when it is tasked (request #9).` }
    case 'override':
      return { ok: true, message: 'orders set' }
    case 'clear_override':
      return { ok: false, message: 'there are no human orders standing' }
    case 'cancel_op':
      return { ok: true, message: 'operation called off' }
    case 'cancel_request':
      return { ok: true, message: 'request withdrawn' }
  }
}
