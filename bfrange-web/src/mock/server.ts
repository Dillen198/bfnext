/**
 * `VITE_MOCK=1`: an in-browser stand-in for bfdb's range endpoints.
 *
 * About 45 days of history is generated from a fixed seed on load (so every
 * reload shows the same pilots, passes and impacts), plus the two reference
 * cards from the design brief at /result/mock-ref-trap and
 * /result/mock-ref-bomb. A new result "lands" every 25 s of wall clock so
 * the live feed animates. Spawns and despawns are kept in memory.
 *
 * Switch who you are with the login button, or set
 * `localStorage['rangeMock.user']` to `casper` (linked pilot, default),
 * `admin` (instructor), `unlinked` (Discord but no UCID) or `none`.
 * `localStorage['rangeMock.offline'] = '1'` makes /live report the server down.
 */
import { ApiError, type RangeApi } from '../api'
import { headline } from '../lib/headline'
import type {
  FeedQuery,
  LiveSpawn,
  Me,
  RangeRecord,
  ResultsQuery,
  Summary,
} from '../types'
import { greenie, leaderboards, pilotProfile, stationImpacts, within } from './aggregate'
import { mockCardDataUrl } from './cards'
import { CALIBRATION, CATALOG, WEAPON_DB } from './catalog'
import { buildLive, spawnPosition } from './live'
import { bombRecord, randomRecord, trapRecord } from './records'
import { Rng } from './rng'
import { PILOTS, pilotByName } from './world'

const DAY = 86_400_000
const SLOT_MS = 25_000
const BOOT = Date.now()
/** when the two reference cards were "flown": just before the page loaded */
const REF = BOOT

// ─── history ───────────────────────────────────────────────────────────────

function buildHistory(): RangeRecord[] {
  const rng = new Rng(20260923)
  const out: RangeRecord[] = []
  const today = new Date(BOOT)
  today.setUTCHours(0, 0, 0, 0)
  for (let d = 44; d >= 0; d--) {
    const dayStart = today.getTime() - d * DAY
    // busier on weekends and recently
    const dow = new Date(dayStart).getUTCDay()
    const n = rng.int(2, 5) + (dow === 0 || dow === 6 ? 4 : 0) + (d < 10 ? 2 : 0)
    for (let i = 0; i < n; i++) {
      const ts = dayStart + rng.range(9, 23) * 3600_000
      // always build (so the seeded stream stays aligned), then drop the future
      const r = randomRecord(rng, Math.round(ts))
      if (ts <= REF - 10 * 60_000) out.push(r)
    }
  }
  // the reference cards from the brief
  const wo = trapRecord(pilotByName('Casper'), new Rng(73), REF - 3 * 60_000, { grade: 'WO', unit_type: 'T-45', night: false })
  if (wo.result.kind === 'trap') {
    wo.result.case = 1
    wo.result.pattern.wake_alt_ft = 451
  }
  // fixed ids, so /result/mock-ref-trap and /result/mock-ref-bomb always open
  wo.id = 'mock-ref-trap'
  out.push(wo)
  const gbu = bombRecord(pilotByName('Blackjack'), new Rng(16), REF - 7 * 60_000, {
    station: 'range_a_circle', weapon: 'GBU_16', cls: 'unguided', guidance: 'none',
    miss: 37, radial: 260, alt_ft: 10_611, tas: 317, heading: 78, dive: 0, unit_type: 'F-14B',
  })
  gbu.id = 'mock-ref-bomb'
  out.push(gbu)
  return out
}

const records: RangeRecord[] = buildHistory()
let lastSlot = Math.floor(BOOT / SLOT_MS)

/** Land a new result for every 25 s slot since the last call (at most 3 at once). */
function tick(now = Date.now()) {
  const slot = Math.floor(now / SLOT_MS)
  for (let s = Math.max(lastSlot + 1, slot - 2); s <= slot; s++) {
    const rng = new Rng(s)
    // live results come from the pilots actually on the range right now
    const p = pilotByName(rng.pick(['Casper', 'Sprocket', 'Viper', 'Dutch', 'Hollywood', 'Tuna', 'Kestrel', 'Nitro']))
    records.push(randomRecord(rng, s * SLOT_MS, p))
  }
  lastSlot = Math.max(lastSlot, slot)
}

const cardCache = new Map<string, string>()
function card(r: RangeRecord): string {
  let c = cardCache.get(r.id)
  if (!c) {
    c = mockCardDataUrl(r)
    cardCache.set(r.id, c)
  }
  return c
}

function toSummary(r: RangeRecord): Summary {
  const { track, ...rest } = r
  const c = card(r)
  return { ...rest, headline: headline(r), has_track: track !== undefined, card_png: c, card_svg: c }
}

const byNewest = (a: RangeRecord, b: RangeRecord) => b.ts.localeCompare(a.ts)
const sleep = (ms: number) => new Promise(res => setTimeout(res, ms))
const latency = () => sleep(90 + Math.random() * 160)

// ─── identity ──────────────────────────────────────────────────────────────

type MockUser = 'casper' | 'admin' | 'unlinked' | 'none'

function mockUser(): MockUser {
  try {
    const v = localStorage.getItem('rangeMock.user')
    if (v === 'admin' || v === 'unlinked' || v === 'none' || v === 'casper') return v
  } catch { /* storage blocked */ }
  return 'casper'
}

export function setMockUser(u: MockUser) {
  try { localStorage.setItem('rangeMock.user', u) } catch { /* storage blocked */ }
}

function me(): Me {
  const u = mockUser()
  const casper = PILOTS[0]
  switch (u) {
    case 'none': return { logged_in: false, ucid: null, name: null, admin: false }
    case 'unlinked': return { logged_in: true, ucid: null, name: 'casper#0001', admin: false }
    case 'admin': return { logged_in: true, ucid: casper.ucid, name: casper.name, admin: true }
    default: return { logged_in: true, ucid: casper.ucid, name: casper.name, admin: false }
  }
}

// ─── spawns ────────────────────────────────────────────────────────────────

let spawns: LiveSpawn[] = [
  {
    id: 'sp-demo-1', item: 'tanker_kc130', label: 'KC-130 (drogue) · 15,000 ft', owner_name: 'Rook',
    owner_ucid: pilotByName('Rook').ucid, pos: { lat: 42.12, lon: 42.3, alt_m: 4572 },
    created: new Date(BOOT - 20 * 60_000).toISOString(), expires: new Date(BOOT + 40 * 60_000).toISOString(), units: 1,
  },
  {
    id: 'sp-demo-2', item: 'bvr_flight', label: 'BVR flight · Su-27 ×2 · 40 nm', owner_name: 'Nitro',
    owner_ucid: pilotByName('Nitro').ucid, pos: { lat: 42.85, lon: 41.2, alt_m: 7620 },
    created: new Date(BOOT - 6 * 60_000).toISOString(), expires: new Date(BOOT + 54 * 60_000).toISOString(), units: 2,
  },
]
let spawnSeq = 1

function liveSpawns(now: number) {
  spawns = spawns.filter(s => !s.expires || new Date(s.expires).getTime() > now)
  return spawns
}

// ─── the API ───────────────────────────────────────────────────────────────

function notFound(what: string): never {
  throw new ApiError(404, `${what} not found`)
}

export const mockApi: RangeApi = {
  async live() {
    await latency()
    try {
      if (localStorage.getItem('rangeMock.offline') === '1')
        return { live: null, reason: 'The range server is not running (mock offline mode).' }
    } catch { /* ignore */ }
    const now = Date.now()
    return { live: buildLive(now, liveSpawns(now)), reason: null }
  },

  async feed(q: FeedQuery = {}) {
    await latency()
    tick()
    let items = [...records].sort(byNewest)
    if (q.kind) items = items.filter(r => r.result.kind === q.kind)
    if (q.before) {
      const ref = records.find(r => r.id === q.before)?.ts ?? q.before
      items = items.filter(r => r.ts < ref)
    }
    return { items: items.slice(0, q.limit ?? 20).map(toSummary) }
  },

  async results(q: ResultsQuery = {}) {
    await latency()
    tick()
    const now = Date.now()
    const items = [...records]
      .filter(within(q.days, now))
      .filter(r => !q.kind || r.result.kind === q.kind)
      .filter(r => !q.unit_type || r.unit_type === q.unit_type)
      .filter(r => !q.pilot || r.pilot.ucid === q.pilot || r.pilot.name.toLowerCase().includes(q.pilot.toLowerCase()))
      .filter(r => !q.station || ('station_id' in r.result && r.result.station_id === q.station))
      .sort(byNewest)
    const off = q.offset ?? 0
    return { items: items.slice(off, off + (q.limit ?? 25)).map(toSummary), total: items.length }
  },

  async result(id) {
    await latency()
    return structuredClone(records.find(r => r.id === id) ?? notFound('Result'))
  },

  async me() {
    await latency()
    return me()
  },

  async pilot(ucid) {
    await latency()
    return pilotProfile(records, ucid, toSummary) ?? notFound('Pilot')
  },

  async pilots(q) {
    await latency()
    const counts = new Map<string, { ucid: string; name: string; count: number }>()
    for (const r of records) {
      if (!r.pilot.ucid) continue
      const c = counts.get(r.pilot.ucid) ?? { ucid: r.pilot.ucid, name: r.pilot.name, count: 0 }
      c.count++
      counts.set(r.pilot.ucid, c)
    }
    const needle = q.trim().toLowerCase()
    return [...counts.values()].filter(p => !needle || p.name.toLowerCase().includes(needle)).sort((a, b) => b.count - a.count)
  },

  async greenie(q = {}) {
    await latency()
    return greenie(records, q, Date.now())
  },

  async leaderboards(days) {
    await latency()
    return leaderboards(records, days, Date.now())
  },

  async stationImpacts(id, q = {}) {
    await latency()
    return stationImpacts(records, id, q, Date.now())
  },

  async catalog() {
    await latency()
    return CATALOG
  },

  async spawn(item, params) {
    await latency()
    const who = me()
    if (!who.logged_in) throw new ApiError(401, 'Log in to spawn')
    if (!who.ucid) return { ok: false, message: 'Link your DCS account first: type -linkme in the Discord bot channel.', spawn_id: null }
    const it = CATALOG.items.find(i => i.id === item)
    if (!it) return { ok: false, message: `Unknown item ${item}`, spawn_id: null }
    if (it.instructor_only && !who.admin) return { ok: false, message: `${it.label} is instructor-only.`, spawn_id: null }
    const now = Date.now()
    const mine = liveSpawns(now).filter(s => s.owner_ucid === who.ucid)
    if (mine.length >= CATALOG.max_active_per_player && !who.admin)
      return { ok: false, message: `You already have ${mine.length} active spawns (max ${CATALOG.max_active_per_player}). Despawn one first.`, spawn_id: null }
    const live = buildLive(now, spawns)
    const pl = live.players.find(p => p.ucid === who.ucid)
    if (it.relative_to_player && !pl?.in_air)
      return { ok: false, message: `${it.label} spawns relative to you: get airborne in DCS first.`, spawn_id: null }
    const id = `sp-${now.toString(36)}-${spawnSeq++}`
    const detail = it.params
      .map(p => {
        const v = params[p.key] ?? p.default
        return p.kind.type === 'choice' ? p.kind.options.find(o => o.value === v)?.label ?? v : `${Number(v).toLocaleString('en-US')} ${p.kind.unit}`
      })
      .join(' · ')
    spawns.push({
      id, item, label: `${it.label}${detail ? ` · ${detail}` : ''}`, owner_name: who.name ?? 'you', owner_ucid: who.ucid,
      pos: spawnPosition(item, pl?.pos ?? live.stations[0].pos), created: new Date(now).toISOString(),
      expires: new Date(now + CATALOG.despawn_after_s * 1000).toISOString(), units: Number(params.count ?? 1) || 1,
    })
    return {
      ok: true,
      message: `${it.label} spawned${it.relative_to_player ? ' relative to your aircraft' : ''}. It despawns in ${Math.round(CATALOG.despawn_after_s / 60)} min.`,
      spawn_id: id,
    }
  },

  async despawn(target) {
    await latency()
    const who = me()
    if (!who.ucid) throw new ApiError(401, 'Log in and link your DCS account to despawn')
    if (target === 'all') {
      const n = spawns.filter(s => s.owner_ucid === who.ucid).length
      spawns = spawns.filter(s => s.owner_ucid !== who.ucid)
      return { ok: true, message: n ? `Despawned ${n} group${n > 1 ? 's' : ''}.` : 'You had nothing spawned.' }
    }
    const s = spawns.find(x => x.id === target.spawn_id)
    if (!s) return { ok: false, message: 'That spawn is already gone.' }
    if (s.owner_ucid !== who.ucid && !who.admin) return { ok: false, message: 'You can only despawn your own groups.' }
    spawns = spawns.filter(x => x !== s)
    return { ok: true, message: `Despawned ${s.label.split(' · ')[0]}.` }
  },

  async resetStation(station) {
    await latency()
    if (!me().admin) throw new ApiError(403, 'instructors only')
    return { ok: true, message: `Respawned every target on ${station}.` }
  },

  async weapons() {
    await latency()
    return { db: WEAPON_DB, calibration: CALIBRATION }
  },

  async tacview(id) {
    await latency()
    const r = records.find(x => x.id === id)
    if (!r?.track || r.track.kind === 'groove' || r.track.kind === 'aar') return null
    const t0 = new Date(r.ts)
    const lines = ['FileType=text/acmi/tacview', 'FileVersion=2.2', `0,ReferenceTime=${t0.toISOString()}`, `0,Title=${r.id}`]
    const series: [string, { t: number; lat: number; lon: number; alt_m: number }[]][] =
      r.track.kind === 'weapon' ? [['weapon', r.track.points]]
      : r.track.kind === 'intercept' ? [['missile', r.track.missile], ['target', r.track.target]]
      : Object.entries(r.track.paths)
    series.forEach(([name, pts], i) => {
      const oid = (i + 1).toString(16)
      pts.forEach((p, j) => {
        lines.push(`#${p.t.toFixed(2)}`)
        lines.push(`${oid},T=${p.lon.toFixed(6)}|${p.lat.toFixed(6)}|${p.alt_m.toFixed(0)}${j === 0 ? `,Name=${name}` : ''}`)
      })
    })
    return new Blob([lines.join('\n')], { type: 'text/plain' })
  },

  cardUrl(id) {
    const r = records.find(x => x.id === id)
    return r ? card(r) : ''
  },

  loginUrl: () => '#mock-login',

  async logout() {
    setMockUser('none')
  },
}
