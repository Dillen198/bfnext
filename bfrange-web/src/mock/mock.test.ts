import { describe, expect, it } from 'vitest'
import { RESULT_KINDS } from '../types'
import { mockApi } from './server'

// The mock stands in for bfdb in `VITE_MOCK=1`; these check it covers every
// page's needs, so "the site renders in mock mode" means something.
describe('mock range API', () => {
  it('has results of every kind, with tracks where the kind has one', async () => {
    const { items, total } = await mockApi.results({ limit: 5000 })
    expect(total).toBe(items.length)
    for (const k of RESULT_KINDS) {
      const of = items.filter(s => s.result.kind === k)
      expect(of.length, `kind ${k}`).toBeGreaterThan(0)
      if (k !== 'gunnery') expect(of.some(s => s.has_track), `track for ${k}`).toBe(true)
    }
    // list entries never carry the track, but do carry render hints
    for (const s of items.slice(0, 20)) {
      expect('track' in s).toBe(false)
      expect(s.headline.length).toBeGreaterThan(10)
      expect(s.card_png.startsWith('data:image/svg+xml')).toBe(true)
    }
  })

  it('includes the two reference cards from the brief', async () => {
    const { items } = await mockApi.results({ limit: 5000 })
    const trap = items.find(s => s.result.kind === 'trap' && s.result.lso_comment === 'AAX FIM (SLO)AR _HAW_' && s.pilot.name === 'Casper')
    expect(trap).toBeDefined()
    if (trap?.result.kind !== 'trap') throw new Error('no trap')
    expect(trap.result.grade).toBe('WO')
    expect(trap.result.pattern.wake_alt_ft).toBe(451)
    expect(trap.result.lso_description).toEqual([
      'angling approach at the start (first third)',
      'fast in the middle (second third)',
      'a little slow at the ramp',
      'VERY high all the way',
    ])
    const full = await mockApi.result(trap.id)
    expect(full.track?.kind).toBe('groove')

    const bomb = items.find(s => s.result.kind === 'bomb' && s.result.weapon === 'GBU_16' && s.pilot.name === 'Blackjack' && s.result.miss_m === 37)
    if (bomb?.result.kind !== 'bomb') throw new Error('no bomb')
    expect(bomb.result.radial_deg).toBe(260)
    expect(bomb.result.quality).toBe('INEFFECTIVE')
    expect(bomb.unit_type).toBe('F-14B')
    expect(Math.round(bomb.result.release.alt_agl_m / 0.3048)).toBe(10611)
  })

  it('serves the live picture', async () => {
    const { live } = await mockApi.live()
    expect(live).not.toBeNull()
    expect(live!.players.length).toBeGreaterThan(5)
    expect(live!.tankers.map(t => t.unit_type)).toEqual(expect.arrayContaining(['KC-135', 'KC135MPRS', 'KC130', 'S-3B Tanker', 'A-6E', 'IL-78M']))
    expect(live!.carriers[0].recovery_tanker).toMatch(/A-6E/)
    for (const p of live!.players) {
      expect(Number.isFinite(p.pos.lat) && Number.isFinite(p.pos.lon)).toBe(true)
    }
  })

  it('serves the range sectors, each with exactly one shape', async () => {
    const { live } = await mockApi.live()
    const sectors = live!.sectors!
    expect(sectors).toHaveLength(27)
    expect(new Set(sectors.map(s => s.id)).size).toBe(27)
    for (const s of sectors) {
      const shapes = [s.shape.polygon, s.shape.circle, s.shape.track].filter(Boolean)
      expect(shapes, s.id).toHaveLength(1)
      expect(['blue', 'red', 'all']).toContain(s.side)
      expect(s.purpose.length, s.id).toBeGreaterThan(5)
    }
    expect(new Set(sectors.map(s => s.kind)).size).toBe(11)
  })

  it('serves DCS atmosphere and ship data the calculators prefill from', async () => {
    const { live } = await mockApi.live()
    for (const s of live!.stations) {
      expect(s.elev_m).toBe(s.pos.alt_m)
      const a = s.atmo!
      expect(a.length).toBeGreaterThan(5)
      expect(a[0].alt_m).toBe(s.pos.alt_m)
      expect(a[a.length - 1].alt_m).toBe(10_000)
      for (let i = 1; i < a.length; i++) expect(a[i].alt_m).toBeGreaterThan(a[i - 1].alt_m)
      // veers and strengthens with height, cools and thins
      expect(a[a.length - 1].wind_kts).toBeGreaterThan(a[0].wind_kts + 25)
      expect(a[a.length - 1].temp_c).toBeLessThan(a[0].temp_c - 50)
      expect(a[a.length - 1].pressure_hpa).toBeLessThan(a[0].pressure_hpa / 3)
    }
    expect(live!.wind.layers!.length).toBeGreaterThan(5)
    const [cv, lha] = live!.carriers
    expect(cv.deck_angle_deg).toBe(-9)
    expect(cv.fb_deg).toBeCloseTo((cv.brc_deg - 9 + 360) % 360, 9)
    expect(lha.deck_angle_deg).toBe(0)
    expect(lha.fb_deg).toBe(lha.brc_deg)
    for (const c of live!.carriers) {
      expect(c.true_wind_kts).toBeGreaterThan(3)
      expect(c.wind_over_deck_kts).toBeGreaterThan(c.speed_kts)
    }
  })

  it('serves every aggregate', async () => {
    const g = await mockApi.greenie({ days: 365 })
    expect(g.rows.length).toBeGreaterThan(2)
    const lb = await mockApi.leaderboards(365)
    for (const k of ['bombing', 'strafe', 'lso', 'aar', 'duels', 'missile_defense'] as const) expect(lb[k].length, k).toBeGreaterThan(0)
    const imp = await mockApi.stationImpacts('range_a_circle', { days: 365 })
    expect(imp.impacts.length).toBeGreaterThan(3)
    expect(imp.cep_m).toBeGreaterThan(0)
    const me = await mockApi.me()
    const p = await mockApi.pilot(me.ucid!)
    expect(p.insights.length).toBeGreaterThan(0)
    expect(p.quals.length).toBeGreaterThan(0)
    expect(Object.keys(p.per_kind)).toContain('trap')
    const hits = await mockApi.pilots('cas')
    expect(hits[0].name).toBe('Casper')
    const w = await mockApi.weapons()
    expect(w.db?.bombs.length).toBeGreaterThan(3)
  })

  it('spawns and despawns', async () => {
    const cat = await mockApi.catalog()
    const tanker = cat.items.find(i => i.id === 'tanker_kc130')!
    const r = await mockApi.spawn(tanker.id, { alt_ft: '12000' })
    expect(r.ok).toBe(true)
    const { live } = await mockApi.live()
    expect(live!.spawns.some(s => s.id === r.spawn_id)).toBe(true)
    const locked = await mockApi.spawn('tanker_il78', {})
    expect(locked.ok).toBe(false)
    const d = await mockApi.despawn('all')
    expect(d.ok).toBe(true)
    const after = await mockApi.live()
    expect(after.live!.spawns.some(s => s.id === r.spawn_id)).toBe(false)
  })
})
