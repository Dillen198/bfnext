/**
 * `RangeResult::headline` from bfprotocols/src/range/mod.rs, for records that
 * arrive without one (the full `/api/range/result/{id}` record). List
 * endpoints send bfdb's own headline, which wins.
 */
import type { RangeRecord } from '../types'
import { gradeName } from './lso'

const f0 = (n: number) => n.toFixed(0)
const f1 = (n: number) => n.toFixed(1)

const MISSILE_OUTCOME: Record<string, string> = {
  kill: 'SPLASH (trainer kill)',
  defeated: 'DEFEATED',
  hit: 'HIT',
  timeout: 'TIMED OUT',
}

const ENGAGEMENT_OUTCOME: Record<string, string> = {
  win: 'WIN',
  loss: 'LOSS',
  draw: 'DRAW',
  abort: 'ABORTED',
}

export const HOT_ZONE_OUTCOME_LABEL: Record<string, string> = {
  egressed: 'egressed',
  landed: 'landed',
  shot_down: 'shot down',
  left: 'left the aircraft',
}

export const FIELD_LANDING_OUTCOME_LABEL: Record<string, string> = {
  full_stop: 'full stop',
  touch_and_go: 'touch and go',
  undershoot: 'undershoot',
}

export const CSAR_OUTCOME_LABEL: Record<string, string> = {
  rescued: 'RESCUED',
  picked_up: 'picked up, not delivered',
  failed: 'not rescued',
}

const sgn0 = (n: number) => (n >= 0 ? '+' : '') + n.toFixed(0)

export function missileOutcomeLabel(o: string): string {
  return MISSILE_OUTCOME[o] ?? o
}

export function engagementOutcomeLabel(o: string): string {
  return ENGAGEMENT_OUTCOME[o] ?? o
}

export const PASS_OUTCOME_LABEL: Record<string, string> = {
  trap: 'Trap',
  bolter: 'Bolter',
  waveoff: 'Waveoff',
  own_waveoff: 'Own waveoff',
  touch_and_go: 'Touch and go',
  crash: 'Crash',
  unknown: 'Unknown',
}

export function headline(rec: Pick<RangeRecord, 'pilot' | 'unit_type' | 'result'>): string {
  const pilot = rec.pilot.name
  const typ = rec.unit_type
  const r = rec.result
  switch (r.kind) {
    case 'bomb':
      return `${pilot} (${typ}) ${r.weapon} on ${r.target}: ${f0(r.miss_m)} m @ ${r.clock} o'clock, ${r.quality}`
    case 'strafe':
      return `${pilot} (${typ}) strafe ${r.pit}: ${r.hits}/${r.rounds_fired} hits (${f0(r.accuracy_pct)}%), ${r.quality}`
    case 'trap': {
      const wire = r.wire !== null ? ` #${r.wire} wire` : ''
      return `${pilot} (${typ}) ${r.carrier}: ${r.grade} => ${gradeName(r.grade)}${wire}`
    }
    case 'aar':
      return `${pilot} (${typ}) AAR on ${r.tanker}: ${r.contacts} contact(s), ${f0(r.fuel_lbs)} lb, grade ${r.grade}`
    case 'missile':
      return `${r.shooter.name} ${r.weapon} vs ${r.target.name}: ${missileOutcomeLabel(r.outcome)}`
    case 'engagement':
      return `${pilot} (${typ}) ${r.setup} vs ${r.adversary}: ${engagementOutcomeLabel(r.outcome)}`
    case 'anti_ship':
      return `${pilot} (${typ}) ${r.weapon} on ${r.ship}: ${r.hit ? 'HIT' : 'MISS'}`
    case 'sling':
      return `${pilot} (${typ}) ${r.method === 'internal' ? 'cargo' : 'sling'} ${r.cargo} to ${r.course}: ${f1(r.distance_m)} m off, ${r.quality}`
    case 'landing':
      return `${pilot} (${typ}) ${r.drill} landing at ${r.pad}: ${f1(r.distance_m)} m off, ${f0(r.touchdown_fpm)} fpm, ${r.quality}`
    case 'troops':
      return `${pilot} (${typ}) troops to ${r.lz}: ${r.troops} in ${f0(r.total_time_s)} s`
    case 'gunnery':
      return `${pilot} (${typ}) gunnery ${r.lane}: ${r.targets_killed}/${r.targets_total} targets in ${f0(r.time_s)} s`
    case 'cas':
      return `${pilot} (${typ}) CAS with ${r.jtac}: ${r.correct_target ? 'on target' : 'wrong target'}`
    case 'sead':
      return `${pilot} (${typ}) ${r.weapon} killed the ${r.system} ${r.role} at ${r.site} (${r.site_was_emitting ? 'radar up' : 'radar dark'})`
    case 'hot_zone':
      return `${pilot} (${typ}) ${r.zone}: ${r.air_kills} air, ${r.ground_kills} ground kills in ${f0(r.time_in_zone_s / 60)} min, ${HOT_ZONE_OUTCOME_LABEL[r.outcome] ?? r.outcome}`
    case 'low_level':
      return `${pilot} (${typ}) ${r.route}: ${r.gates_hit}/${r.gates_total} gates, ${f0(r.pct_below_ceiling)}% under the ceiling, TOT ${sgn0(r.tot_error_s)} s, ${r.quality}`
    case 'field_landing':
      return `${pilot} (${typ}) landing ${r.airfield} rwy ${r.runway}: ${sgn0(r.aim_error_m)} m from the aim point, ${f1(Math.abs(r.centreline_m))} m off centreline, ${f0(r.touchdown_fpm)} fpm, ${r.quality}`
    case 'csar':
      return `${pilot} (${typ}) CSAR ${r.area}: ${CSAR_OUTCOME_LABEL[r.outcome] ?? r.outcome}${r.time_total_s !== null ? ` in ${f0(r.time_total_s / 60)} min` : ''}`
  }
}
