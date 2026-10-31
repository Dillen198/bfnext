import { describe, expect, it } from 'vitest'
import { DECKS, landingFromRef } from './carrierDeck'
import { frameFor, passRead, stateAt } from './clipPlay'
import { FT, destination } from './geo'
import type { Clip, ClipObj } from '../types'

const obj = (o: Partial<ClipObj>): ClipObj => ({
  id: 'a', kind: 'air', role: 'self', t: [], lat: [], lon: [], alt: [], roll: [], pitch: [], yaw: [], destroyed: false, ...o,
})

describe('clip playback', () => {
  const me = obj({ t: [0, 10], lat: [42, 42], lon: [41, 41.01], alt: [1000, 1100], roll: [0, 20], pitch: [0, 0], yaw: [90, 90] })
  const clip: Clip = { v: 1, reference_time: null, event_s: 0, t0: 0, t1: 10, complete: true, self_id: 'a', source: '', objects: [me] }
  const f = frameFor(clip)

  it('interpolates position and attitude', () => {
    const s = stateAt(me, f, 5)
    expect(s.x).toBeCloseTo((0.005 * 111_320 * Math.cos((42 * Math.PI) / 180)), 0)
    expect(s.z).toBeCloseTo(1050)
    expect(s.roll).toBeCloseTo(10)
    expect(s.gs_kts).toBeGreaterThan(160)
    expect(s.hidden).toBe(false)
    expect(stateAt(me, f, 11).hidden).toBe(true)
  })

  it('reads a pass on the glideslope as on the ball and lined up', () => {
    const g = DECKS.nimitz
    const brc = 30
    const ship = { lat: 42, lon: 41 }
    const l = landingFromRef(g)
    const lp = destination(destination(ship, brc, l.fwd), brc + 90, l.right)
    const fb = brc + g.angle_deg
    // 0.5 NM out on the final bearing, 3.5° above the deck
    const d = 926
    const at = destination(lp, fb + 180, d)
    const hgt = d * Math.tan((3.5 * Math.PI) / 180)
    const boat = obj({ id: 'b', role: 'boat', kind: 'carrier', name: 'CVN_73', t: [0, 1], lat: [ship.lat, ship.lat], lon: [ship.lon, ship.lon], alt: [0, 0], roll: [0, 0], pitch: [0, 0], yaw: [brc, brc] })
    const jet = obj({ t: [0, 1], lat: [at.lat, at.lat], lon: [at.lon, at.lon], alt: [hgt + g.deck_height_m, hgt + g.deck_height_m], roll: [0, 0], pitch: [0, 0], yaw: [fb, fb] })
    const c2: Clip = { ...clip, objects: [jet, boat] }
    const fr = frameFor(c2)
    const r = passRead(stateAt(jet, fr, 0.5), stateAt(boat, fr, 0.5), g)
    expect(r.x_m).toBeCloseTo(d, -1)
    expect(Math.abs(r.y_m)).toBeLessThan(2)
    expect(Math.abs(r.gse_deg)).toBeLessThan(0.05)
    expect(Math.abs(r.lue_deg)).toBeLessThan(0.1)
    expect(r.hgt_ft).toBeCloseTo(hgt / FT, 0)
  })
})
