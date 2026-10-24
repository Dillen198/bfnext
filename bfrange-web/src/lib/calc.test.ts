import { describe, expect, it } from 'vitest'
import { planAar } from './aarPlan'
import { ANGLED_DECK_DEG, NIMITZ_DECK_ANGLE_DEG, recommendBrc, windOverDeck } from './wod'

describe('wind over deck', () => {
  it('ship speed alone blows from dead ahead', () => {
    const w = windOverDeck({ ship_heading_deg: 90, ship_speed_kts: 20, wind_from_deg: 0, wind_kts: 0 })
    expect(w.wod_kts).toBeCloseTo(20, 6)
    expect(w.wod_from_deg).toBeCloseTo(90, 6)
    expect(w.rel_bow_deg).toBeCloseTo(0, 6)
    // the angled deck points 9.14° to port, so that wind is from starboard of it
    expect(w.rel_deck_deg).toBeCloseTo(ANGLED_DECK_DEG, 6)
    expect(w.cross_kts).toBeGreaterThan(0)
  })

  it('a headwind adds straight on', () => {
    const w = windOverDeck({ ship_heading_deg: 0, ship_speed_kts: 15, wind_from_deg: 0, wind_kts: 10 })
    expect(w.wod_kts).toBeCloseTo(25, 6)
  })

  it('recommends a BRC with 25–30 kt straight down the angled deck', () => {
    for (const [from, kts] of [[0, 10], [120, 18], [250, 6], [45, 25]] as const) {
      const r = recommendBrc(from, kts)
      expect(r.in_window).toBe(true)
      expect(Math.abs(r.wod.cross_kts)).toBeLessThan(0.5)
      expect(r.wod.axial_kts).toBeGreaterThanOrEqual(24.9)
      expect(r.wod.axial_kts).toBeLessThanOrEqual(30.1)
      expect(r.ship_kts).toBeGreaterThanOrEqual(0)
      expect(r.ship_kts).toBeLessThanOrEqual(30)
    }
  })

  it('10 kt from north: steer right of the wind', () => {
    const r = recommendBrc(0, 10)
    expect(r.brc_deg).toBeGreaterThan(15)
    expect(r.brc_deg).toBeLessThan(35)
    expect(r.ship_kts).toBeCloseTo(17.7, 0)
  })

  it('handles calm air', () => {
    const r = recommendBrc(0, 0)
    expect(r.ship_kts).toBeGreaterThan(20)
    expect(r.in_window).toBe(false)
  })

  it('takes the deck angle as DCS reports it (FB − BRC, negative = to port)', () => {
    const base = { ship_heading_deg: 90, ship_speed_kts: 20, wind_from_deg: 30, wind_kts: 12 }
    // the default is a Nimitz, so passing its angle changes nothing
    expect(windOverDeck({ ...base, deck_angle_deg: NIMITZ_DECK_ANGLE_DEG })).toEqual(windOverDeck(base))
    expect(windOverDeck({ ...base, deck_angle_deg: -9 }).fb_deg).toBeCloseTo(81, 9)
    expect(recommendBrc(250, 8, { deck_angle_deg: NIMITZ_DECK_ANGLE_DEG })).toEqual(recommendBrc(250, 8))
    // a live Nimitz reading -9 instead of -9.14 still lands in the window
    const r = recommendBrc(250, 8, { deck_angle_deg: -9 })
    expect(r.in_window).toBe(true)
    expect(r.wod.fb_deg).toBeCloseTo(r.brc_deg - 9, 6)
  })

  it('straight deck (LHA): the wind is simply down the bow', () => {
    const w = windOverDeck({ ship_heading_deg: 250, ship_speed_kts: 12, wind_from_deg: 250, wind_kts: 8, deck_angle_deg: 0 })
    expect(w.fb_deg).toBeCloseTo(250, 9)
    expect(w.axial_kts).toBeCloseTo(20, 9)
    expect(w.cross_kts).toBeCloseTo(0, 9)
    const r = recommendBrc(250, 8, { deck_angle_deg: 0 })
    expect(r.brc_deg).toBeCloseTo(250, 9)
    expect(r.ship_kts).toBeCloseTo(19, 9)
    expect(r.wod.axial_kts).toBeCloseTo(27, 9)
    expect(r.in_window).toBe(true)
    expect(Number.isFinite(recommendBrc(0, 0, { deck_angle_deg: 0 }).ship_kts)).toBe(true)
    expect(recommendBrc(0, 40, { deck_angle_deg: 0 }).in_window).toBe(false)
  })
})

describe('AAR planner', () => {
  it('computes transit, onload and time connected', () => {
    const p = planAar({
      fuel_lb: 5000, burn_lb_min: 100, dist_nm: 70, gs_kts: 420,
      bingo_lb: 3000, mission_lb: 5000, capacity_lb: 10_800, transfer_lb_min: 1100,
    })
    expect(p.transit_min).toBeCloseTo(10, 6)
    expect(p.fuel_at_tanker_lb).toBeCloseTo(4000, 6)
    expect(p.onload_lb).toBeCloseTo(4000, 6)
    expect(p.time_connected_min).toBeCloseTo(4, 6)
    expect(p.transferred_lb).toBeCloseTo(4400, 6)
    expect(p.arrive_below_bingo).toBe(false)
  })

  it('caps at tank capacity and flags a low arrival', () => {
    const p = planAar({
      fuel_lb: 3000, burn_lb_min: 100, dist_nm: 70, gs_kts: 420,
      bingo_lb: 3000, mission_lb: 20_000, capacity_lb: 10_800, transfer_lb_min: 1100,
    })
    expect(p.capped).toBe(true)
    expect(p.fill_to_lb).toBe(10_800)
    expect(p.arrive_below_bingo).toBe(true)
  })

  it('is infeasible when the burn beats the transfer', () => {
    const p = planAar({
      fuel_lb: 3000, burn_lb_min: 500, dist_nm: 0, gs_kts: 400,
      bingo_lb: 2000, mission_lb: 3000, capacity_lb: 10_000, transfer_lb_min: 400,
    })
    expect(p.feasible).toBe(false)
  })
})
