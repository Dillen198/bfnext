/**
 * Wind over deck. The relative wind a carrier's deck feels is the true wind
 * plus the wind of the ship's own motion (which blows from dead ahead). On an
 * angled deck the landing area points off the bow, so the wind the pilot
 * cares about is its component straight down the LANDING AREA (axial) and
 * across it; on a straight deck that is simply down the bow.
 *
 * Deck angle convention, everywhere in this file: the wire's
 * `LiveCarrier.deck_angle_deg`, the landing area relative to the bow,
 * FB − BRC. Negative = angled to port (−9.14 for a Nimitz or Forrestal),
 * 0 for a straight deck (LHA, LHD).
 */
import { deg, norm180, norm360, rad } from './geo'

/** Nimitz / Forrestal landing-area angle, degrees to port of the bow. */
export const ANGLED_DECK_DEG = 9.14
/** The same as a `deck_angle_deg` (FB − BRC): the default everywhere. */
export const NIMITZ_DECK_ANGLE_DEG = -ANGLED_DECK_DEG

/** Below this the deck counts as straight. */
const STRAIGHT_DECK_DEG = 0.05

export interface WodInput {
  ship_heading_deg: number
  ship_speed_kts: number
  wind_from_deg: number
  wind_kts: number
  /** landing area relative to the bow, FB − BRC, negative = to port; default Nimitz */
  deck_angle_deg?: number
}

export interface Wod {
  /** relative wind speed over the deck, knots */
  wod_kts: number
  /** direction the relative wind comes from, degrees true */
  wod_from_deg: number
  /** final bearing of the landing area, degrees true */
  fb_deg: number
  /** relative wind off the bow, degrees, + = from starboard */
  rel_bow_deg: number
  /** relative wind off the landing-area axis, degrees, + = from starboard */
  rel_deck_deg: number
  /** component straight down the landing area, knots (+ = headwind for the pilot) */
  axial_kts: number
  /** component across the landing area, knots, + = from starboard */
  cross_kts: number
}

/** Unit "from" vector (east, north) for a bearing. */
function u(brg: number): [number, number] {
  return [Math.sin(rad(brg)), Math.cos(rad(brg))]
}

/** True for a straight-deck ship (LHA, LHD). */
export function isStraightDeck(deck_angle_deg: number): boolean {
  return Math.abs(deck_angle_deg) < STRAIGHT_DECK_DEG
}

export function windOverDeck(i: WodInput): Wod {
  const angle = i.deck_angle_deg ?? NIMITZ_DECK_ANGLE_DEG
  const [we, wn] = u(i.wind_from_deg)
  const [se, sn] = u(i.ship_heading_deg)
  // "from" vectors add: the ship's motion is a wind from its heading
  const fe = i.wind_kts * we + i.ship_speed_kts * se
  const fn = i.wind_kts * wn + i.ship_speed_kts * sn
  const wod = Math.hypot(fe, fn)
  const from = wod < 1e-9 ? i.ship_heading_deg : norm360(deg(Math.atan2(fe, fn)))
  const fb = norm360(i.ship_heading_deg + angle)
  const relDeck = norm180(from - fb)
  return {
    wod_kts: wod,
    wod_from_deg: from,
    fb_deg: fb,
    rel_bow_deg: norm180(from - i.ship_heading_deg),
    rel_deck_deg: relDeck,
    axial_kts: wod * Math.cos(rad(relDeck)),
    cross_kts: wod * Math.sin(rad(relDeck)),
  }
}

export interface BrcAdvice {
  brc_deg: number
  ship_kts: number
  wod: Wod
  /** true when the plan puts 25–30 kt straight down the landing area */
  in_window: boolean
  note: string
}

export interface BrcOptions {
  target_kts?: number
  min_kts?: number
  max_kts?: number
  min_ship_kts?: number
  max_ship_kts?: number
  /** FB − BRC, negative = to port; default Nimitz */
  deck_angle_deg?: number
}

/**
 * Pick a BRC and ship speed that put 25–30 kt of wind straight down the
 * landing area with no crosswind.
 *
 * With the true wind W from θw and the ship doing S along BRC, measure the
 * wind from the final bearing (δ = θw − FB) and let α be the angle of the
 * landing area to PORT of the bow (α = −deck_angle_deg, BRC = FB + α).
 * Zero crosswind needs  W·sin δ + S·sin α = 0, and then the axial wind is
 * W·sin(α − δ) / sin α. Solving for the axial target A gives
 * sin(α − δ) = A·sin α / W, two δ per A; keep the slowest legal S.
 * A straight deck (α = 0) just heads into the wind: δ = 0, S = A − W.
 */
export function recommendBrc(wind_from_deg: number, wind_kts: number, o: BrcOptions = {}): BrcAdvice {
  const target = o.target_kts ?? 27
  const lo = o.min_kts ?? 25
  const hi = o.max_kts ?? 30
  const minS = o.min_ship_kts ?? 0
  const maxS = o.max_ship_kts ?? 30
  const deckAngle = o.deck_angle_deg ?? NIMITZ_DECK_ANGLE_DEG
  const alphaDeg = -deckAngle
  const a = rad(alphaDeg)
  // the ship's own wind is off the landing area on this side
  const side = alphaDeg > 0 ? 'starboard' : 'port'

  const make = (brc: number, s: number, note: string): BrcAdvice => {
    const wod = windOverDeck({
      ship_heading_deg: brc,
      ship_speed_kts: s,
      wind_from_deg,
      wind_kts,
      deck_angle_deg: deckAngle,
    })
    return {
      brc_deg: norm360(brc),
      ship_kts: s,
      wod,
      in_window: wod.axial_kts >= lo - 0.05 && wod.axial_kts <= hi + 0.05 && Math.abs(wod.cross_kts) < 1,
      note,
    }
  }

  if (isStraightDeck(deckAngle)) {
    const s = Math.min(maxS, Math.max(minS, target - wind_kts))
    const note =
      wind_kts <= 0.1 ? `No wind: all of the ${Math.round(s)} kt over the deck comes from the ship.`
      : wind_kts + minS > hi ? 'Strong wind: even at minimum speed the deck sees more than 30 kt.'
      : wind_kts + maxS < lo ? 'Light wind: even at full speed the deck sees less than 25 kt.'
      : `Steam ${Math.round(s)} kt straight into the wind for ${Math.round(wind_kts + s)} kt down the deck.`
    return make(wind_from_deg + alphaDeg, s, note)
  }

  if (wind_kts > 0.1) {
    const targets = [target, ...[25, 26, 28, 29, 30].filter(x => x !== target)]
    for (const A of targets) {
      const r = (A * Math.sin(a)) / wind_kts
      if (Math.abs(r) > 1) continue
      const betas = [Math.asin(r), Math.PI - Math.asin(r)]
      let best: { brc: number; s: number } | null = null
      for (const b of betas) {
        const d = a - b
        const s = (-wind_kts * Math.sin(d)) / Math.sin(a)
        if (s < minS - 1e-9 || s > maxS + 1e-9) continue
        const fb = wind_from_deg - deg(d)
        if (!best || s < best.s) best = { brc: fb + alphaDeg, s }
      }
      if (best) return make(best.brc, best.s, `Steam ${Math.round(best.s)} kt for ${Math.round(A)} kt down the angled deck, no crosswind.`)
    }
    if (wind_kts >= hi) {
      // too much wind: slowest speed, still no crosswind
      const d = Math.asin(Math.max(-1, Math.min(1, (-minS * Math.sin(a)) / wind_kts)))
      const fb = wind_from_deg - deg(d)
      return make(fb + alphaDeg, minS, 'Strong wind: even at minimum speed the deck sees more than 30 kt.')
    }
  }
  // light or no wind: put the true wind straight down the angled deck and
  // make up the rest with speed; the ship's own wind leaves some crosswind
  const s = Math.min(maxS, Math.max(minS, (target - wind_kts) / Math.cos(a)))
  return make(
    wind_from_deg + alphaDeg,
    s,
    wind_kts <= 0.1
      ? `No wind: all of the WOD comes from the ship, so expect a crosswind from ${side}.`
      : `Light wind: a small ${side} crosswind is unavoidable.`,
  )
}
