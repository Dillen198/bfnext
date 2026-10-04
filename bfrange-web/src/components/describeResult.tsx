/** What a result card leads with, per kind (kept apart from the components for fast refresh). */
import type { ReactNode } from 'react'
import {
  airframe,
  fmt,
  fmtClock,
  fmtSecs,
  fmtSigned,
  qualityTone,
  scoreTone,
  weaponName,
  type Tone,
} from '../lib/format'
import {
  CSAR_OUTCOME_LABEL,
  FIELD_LANDING_OUTCOME_LABEL,
  HOT_ZONE_OUTCOME_LABEL,
  engagementOutcomeLabel,
  missileOutcomeLabel,
  PASS_OUTCOME_LABEL,
} from '../lib/headline'
import type { RangeResult } from '../types'
import { GradeBadge } from './Chips'

export interface Headline {
  /** the one number/word that matters */
  big: ReactNode
  /** the supporting line */
  detail: string
  tone: Tone
}

/** What a result card leads with, per kind. */
export function describeResult(r: RangeResult, score: number | null): Headline {
  const t = scoreTone(score)
  switch (r.kind) {
    case 'trap':
      return {
        big: <GradeBadge grade={r.grade} />,
        detail: [
          r.carrier.split(' ')[0],
          `Case ${r.case}`,
          r.night ? 'night' : null,
          r.wire ? `#${r.wire} wire` : PASS_OUTCOME_LABEL[r.outcome],
          r.lso_comment || null,
        ].filter(Boolean).join(' · '),
        tone: t,
      }
    case 'bomb':
      return {
        big: `${fmt(r.miss_m, r.miss_m < 10 ? 1 : 0)} m`,
        detail: [
          weaponName(r.weapon, r.weapon_display),
          `${r.clock} o'clock`,
          r.quality,
          r.range.split(' — ')[0],
          r.tier ? r.tier.toUpperCase() : null,
          r.gps_denied ? 'GPS denied' : null,
        ].filter(Boolean).join(' · '),
        tone: qualityTone(r.quality),
      }
    case 'strafe':
      return {
        big: r.quality === 'INVALID' ? 'FOUL' : `${fmt(r.accuracy_pct)}%`,
        detail: `${r.pit} · ${r.hits}/${r.rounds_fired} rounds · ${r.quality}`,
        tone: qualityTone(r.quality),
      }
    case 'aar':
      return {
        big: r.grade,
        detail: `${r.tanker} (${airframe(r.tanker_type)}) · ${r.contacts} contact${r.contacts === 1 ? '' : 's'} · ${fmt(r.fuel_lbs)} lb`,
        tone: t,
      }
    case 'missile': {
      const mine = r.perspective === 'target'
      return {
        big: r.outcome === 'kill' ? (mine ? 'DEAD' : 'SPLASH') : r.outcome.toUpperCase(),
        detail: mine
          ? `${weaponName(r.weapon)} from ${r.shooter.name} · ${fmt(r.launch.range_m / 1852, 1)} nm · miss ${fmt(r.min_distance_m)} m`
          : `${weaponName(r.weapon)} at ${r.target.name} · ${fmt(r.launch.range_m / 1852, 1)} nm · ${missileOutcomeLabel(r.outcome)}`,
        tone: t,
      }
    }
    case 'engagement':
      return {
        big: engagementOutcomeLabel(r.outcome),
        detail: `${r.setup} vs ${r.adversary} · ${fmtSecs(r.duration_s)}`,
        tone: t,
      }
    case 'anti_ship':
      return {
        big: r.hit ? (r.ship_sunk ? 'SUNK' : 'HIT') : 'MISS',
        detail: `${weaponName(r.weapon)} on ${r.ship} · ${fmt(r.launch_range_m / 1852, 1)} nm`,
        tone: t,
      }
    case 'sling':
      return { big: `${fmt(r.distance_m, 1)} m`, detail: `${r.method === 'internal' ? 'Internal' : 'Sling'} · ${r.course} · ${r.cargo} · ${fmtSecs(r.time_s)} · ${r.quality}`, tone: qualityTone(r.quality) }
    case 'landing':
      return {
        big: `${fmt(r.distance_m, 1)} m`,
        detail: `${r.drill === 'ship' ? 'Deck · ' : ''}${r.pad} · ${fmt(r.touchdown_fpm)} fpm${r.ship_speed_kts !== undefined ? ` · ship ${fmt(r.ship_speed_kts)} kt` : ''} · ${r.quality}`,
        tone: qualityTone(r.quality),
      }
    case 'troops':
      return { big: fmtSecs(r.total_time_s), detail: `${r.troops} troops to ${r.lz} · ${fmt(r.landing_distance_m)} m off · ${r.quality}`, tone: qualityTone(r.quality) }
    case 'gunnery':
      return { big: `${r.targets_killed}/${r.targets_total}`, detail: `${r.lane} · ${r.hits}/${r.shots} hits · ${fmtSecs(r.time_s)}`, tone: t }
    case 'cas':
      return {
        big: r.correct_target ? `${fmt(r.miss_m, r.miss_m < 10 ? 1 : 0)} m` : 'WRONG TGT',
        detail: `${r.jtac} · ${weaponName(r.weapon)} · ${r.target}${r.danger_close ? ' · danger close' : ''}`,
        tone: t,
      }
    case 'sead':
      return {
        big: r.site_destroyed ? 'SITE DOWN' : r.site_was_emitting ? 'SEAD' : 'DEAD',
        detail: `${r.system} ${r.role} · ${weaponName(r.weapon)} · radar ${r.site_was_emitting ? 'up' : 'dark'}${r.trainer_deaths ? ` · ${r.trainer_deaths} trainer save${r.trainer_deaths === 1 ? '' : 's'}` : ''}`,
        tone: r.site_destroyed || r.site_was_emitting ? (r.trainer_deaths ? 'good' : 'great') : 'fair',
      }
    case 'hot_zone':
      return {
        big: `${r.air_kills}A ${r.ground_kills}G`,
        detail: `${r.zone} · ${fmtClock(r.time_in_zone_s)} · ${HOT_ZONE_OUTCOME_LABEL[r.outcome] ?? r.outcome}${r.trainer_deaths ? ` · ${r.trainer_deaths} trainer death${r.trainer_deaths === 1 ? '' : 's'}` : ''}`,
        tone: r.outcome === 'shot_down' ? 'poor' : r.trainer_deaths ? 'fair' : r.air_kills + r.ground_kills > 0 ? 'great' : 'good',
      }
    case 'low_level':
      return {
        big: `${fmtSigned(r.tot_error_s, 0)} s`,
        detail: `${r.route} · ${r.gates_hit}/${r.gates_total} gates · ${fmt(r.pct_below_ceiling)}% low · ${r.quality}`,
        tone: qualityTone(r.quality),
      }
    case 'field_landing':
      return {
        big: r.outcome === 'undershoot' ? 'SHORT' : `${fmtSigned(r.aim_error_m, 0)} m`,
        detail: `${r.airfield} ${r.runway} · ${FIELD_LANDING_OUTCOME_LABEL[r.outcome] ?? r.outcome} · ${fmt(Math.abs(r.centreline_m), 1)} m ${r.centreline_m >= 0 ? 'R' : 'L'} · ${fmt(r.touchdown_fpm)} fpm · ${r.quality}`,
        tone: qualityTone(r.quality),
      }
    case 'csar':
      return {
        big: r.outcome === 'rescued' && r.time_total_s !== null ? fmtClock(r.time_total_s) : r.outcome === 'failed' ? 'LOST' : 'NOT HOME',
        detail: `${r.area} · ${CSAR_OUTCOME_LABEL[r.outcome] ?? r.outcome}${r.hostile ? ' · hostile' : ''}${r.pickup_method ? ` · ${r.pickup_method}` : ''} · ${r.quality}`,
        tone: qualityTone(r.quality),
      }
  }
}
