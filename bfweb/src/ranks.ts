import {
  NatoO1, NatoO2, NatoO3, NatoO4, NatoO5, NatoO6, NatoO7, NatoO8,
  VksO1, VksO2, VksO3, VksO4, VksO5, VksO6, VksO7, VksO8,
  type IconComponent,
} from './icons'

/**
 * Pilot rank.
 *
 * Rank is earned on campaign score, which is already the leaderboard's
 * currency: Air×3 + Ground×2 + Cap×5 + FARP×4 + Deploy×2 + Repair + Supply +
 * Troop×0.5 + Action − Death×2. That matters more than it sounds -- scoring on
 * kills alone would make transport and logistics pilots permanently junior,
 * and scoring on hours alone would reward sitting on the ramp.
 *
 * The thresholds are absolute, not percentile. A rank you earned should not be
 * taken away because somebody else flew a good sortie; and a fixed ladder means
 * "Major" means the same thing in six months as it does today. They are spaced
 * geometrically so the top of the ladder stays out of reach for a long while --
 * as of the current campaign the leading pilot sits around 78, which is O-4.
 *
 * Insignia follow the pilot's coalition. Blue wears NATO, Red wears VKS; a
 * pilot bfdb has no side for yet falls back to NATO shapes but keeps a neutral
 * title, since a rank with no service behind it is the honest reading.
 */
export interface Rank {
  /** 1-8, low to high. */
  tier: number
  /** Minimum campaign score. */
  min: number
  /** Service title for the pilot's coalition. */
  title: string
  /** Shoulder board for the pilot's coalition. */
  icon: IconComponent
}

interface Tier {
  tier: number
  min: number
  nato: { title: string; icon: IconComponent }
  vks: { title: string; icon: IconComponent }
}

const TIERS: Tier[] = [
  { tier: 1, min: 0,   nato: { title: '2nd Lieutenant',    icon: NatoO1 }, vks: { title: 'Lieutenant',           icon: VksO1 } },
  { tier: 2, min: 10,  nato: { title: '1st Lieutenant',    icon: NatoO2 }, vks: { title: 'Senior Lieutenant',    icon: VksO2 } },
  { tier: 3, min: 25,  nato: { title: 'Captain',           icon: NatoO3 }, vks: { title: 'Captain',              icon: VksO3 } },
  { tier: 4, min: 50,  nato: { title: 'Major',             icon: NatoO4 }, vks: { title: 'Major',                icon: VksO4 } },
  { tier: 5, min: 100, nato: { title: 'Lieutenant Colonel',icon: NatoO5 }, vks: { title: 'Lieutenant Colonel',   icon: VksO5 } },
  { tier: 6, min: 200, nato: { title: 'Colonel',           icon: NatoO6 }, vks: { title: 'Colonel',              icon: VksO6 } },
  { tier: 7, min: 400, nato: { title: 'Brigadier General', icon: NatoO7 }, vks: { title: 'Major General',        icon: VksO7 } },
  { tier: 8, min: 800, nato: { title: 'Major General',     icon: NatoO8 }, vks: { title: 'Lieutenant General',   icon: VksO8 } },
]

export type PilotSide = 'Blue' | 'Red' | null | undefined

/** The rank a pilot has earned at `score`, in their coalition's service. */
export function rankFor(score: number, side: PilotSide): Rank {
  let found = TIERS[0]
  for (const t of TIERS) if (score >= t.min) found = t
  const svc = side === 'Red' ? found.vks : found.nato
  return {
    tier: found.tier,
    min: found.min,
    title: side ? svc.title : `Tier ${found.tier}`,
    icon: svc.icon,
  }
}

/** Score still needed for the next tier, or null at the top of the ladder. */
export function nextRankAt(score: number): number | null {
  const next = TIERS.find(t => t.min > score)
  return next ? next.min : null
}

/** The whole ladder, for a legend or a profile page. */
export const RANK_TIERS = TIERS
