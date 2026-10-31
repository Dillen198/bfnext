// Tests for shared/web/snapshot.ts (shared/web has no runner of its own).
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import {
  ago, createSnapshotSource, fetchWithSnapshot, offlineStore, offlineText, snapshotKey,
} from '@shared/web/snapshot'

// bfdb's `allowed_key` (bfdb/src/snapshot.rs), copied: every key the client
// asks for must be one bfdb is allowed to publish.
const BFDB_ALLOWED = new RegExp([
  '^(?:',
  'manifest\\.json',
  '|g/instances(?:~kind-range)?\\.json',
  '|g/wiki/pages\\.json',
  '|g/wiki/pages/[A-Za-z0-9._-]+(?:/[A-Za-z0-9._-]+)*\\.json',
  '|i/[A-Za-z0-9_-]+/(?:online|objectives|frontline|rounds|leaderboard|stats|news|state)\\.json',
  '|i/[A-Za-z0-9_-]+/wiki/facts\\.json',
  '|i/[A-Za-z0-9_-]+/rounds/[0-9]+/summary\\.json',
  ')$',
].join(''))

describe('snapshotKey', () => {
  const cases: [string, string | null][] = [
    ['/api/instances', 'g/instances.json'],
    ['/api/instances?kind=range', 'g/instances~kind-range.json'],
    ['/api/instances?all=1', null],
    ['/api/objectives', 'i/vs1/objectives.json'],
    ['/api/objectives?instance=vs2', 'i/vs2/objectives.json'],
    ['/api/objectives?round=12&instance=vs2', null],
    ['/api/frontline?instance=vs1', 'i/vs1/frontline.json'],
    ['/api/online', 'i/vs1/online.json'],
    ['/api/rounds', 'i/vs1/rounds.json'],
    ['/api/rounds?instance=all', null],
    ['/api/rounds/108000656/summary?instance=vs2', 'i/vs2/rounds/108000656/summary.json'],
    ['/api/rounds/12/report', null],
    ['/api/leaderboard', 'i/vs1/leaderboard.json'],
    ['/api/leaderboard?round=5', null],
    ['/api/stats', 'i/vs1/stats.json'],
    ['/api/news?limit=1&instance=vs2', 'i/vs2/news.json'],
    ['/api/wiki/facts?instance=vs2', 'i/vs2/wiki/facts.json'],
    ['/api/wiki/pages?instance=vs2', 'g/wiki/pages.json'],
    ['/api/wiki/pages/gameplay/objectives?instance=vs2', 'g/wiki/pages/gameplay/objectives.json'],
    ['/api/wiki/pages/../../etc', null],
    // Coalition-gated, admin, live-only and per-session routes: never.
    ['/api/briefing', null],
    ['/api/warehouse?objective=Kutaisi', null],
    ['/api/situation', null],
    ['/api/groundwar', null],
    ['/api/hq', null],
    ['/api/intel/captures', null],
    ['/api/command', null],
    ['/api/cockpit/context?server=x', null],
    ['/api/trails', null],
    ['/api/units', null],
    ['/api/logs/bfdb', null],
    ['/api/admin/sessions', null],
    ['/api/auth/me', null],
    ['/api/health', null],
    ['/api/stats?server=Some%20Server', null],
    ['/api/stats?instance=all', null],
    ['/elsewhere/stats', null],
  ]
  it.each(cases)('%s -> %s', (path, key) => {
    expect(snapshotKey(path, 'vs1')).toBe(key)
    if (key) expect(key).toMatch(BFDB_ALLOWED)
  })
})

const MANIFEST = {
  version: 1,
  generated_at: '2026-10-25T12:00:00Z',
  default_instance: 'vs1',
  instances: ['vs1'],
  files: {
    'i/vs1/stats.json': { sha256: 'x', bytes: 2 },
    'i/vs1/news.json': { sha256: 'y', bytes: 2 },
  },
}

function json(body: unknown, status = 200) {
  return new Response(JSON.stringify(body), { status, headers: { 'content-type': 'application/json' } })
}

describe('fetchWithSnapshot', () => {
  let live: (url: string) => Promise<Response>
  beforeEach(() => {
    offlineStore.markOnline()
    vi.stubGlobal('fetch', vi.fn((url: string, init?: RequestInit) => {
      if (url.startsWith('https://snap.test/')) {
        const key = url.slice('https://snap.test/'.length)
        if (key === 'manifest.json') return Promise.resolve(json(MANIFEST))
        if (key === 'i/vs1/stats.json') return Promise.resolve(json({ total_pilots: 7 }))
        if (key === 'i/vs1/news.json') return Promise.resolve(json({ days: [{ day: 'a' }, { day: 'b' }, { day: 'c' }] }))
        return Promise.resolve(json({}, 404))
      }
      void init
      return live(url)
    }))
  })
  afterEach(() => vi.unstubAllGlobals())

  it('passes a live answer through and stays online', async () => {
    live = () => Promise.resolve(json({ total_pilots: 9 }))
    const snap = createSnapshotSource('https://snap.test/')
    const res = await fetchWithSnapshot('https://api.test/api/stats', '/api/stats', {}, snap)
    expect(await res.json()).toEqual({ total_pilots: 9 })
    expect(offlineStore.get()).toEqual({ offline: false, snapshotAt: null })
  })

  it('a 4xx is an answer, not an outage', async () => {
    live = () => Promise.resolve(json({ error: 'not logged in' }, 401))
    const snap = createSnapshotSource('https://snap.test')
    const res = await fetchWithSnapshot('https://api.test/api/stats', '/api/stats', {}, snap)
    expect(res.status).toBe(401)
    expect(offlineStore.get().offline).toBe(false)
  })

  it('a 5xx bfdb wrote itself (its engine is down) is passed through', async () => {
    live = () => Promise.resolve(json({ error: 'engine not answering' }, 503))
    const snap = createSnapshotSource('https://snap.test')
    const res = await fetchWithSnapshot('https://api.test/api/stats', '/api/stats', {}, snap)
    expect(res.status).toBe(503)
    expect(offlineStore.get().offline).toBe(false)
  })

  it('answers a gateway 5xx from the snapshot and marks the app offline', async () => {
    live = () => Promise.resolve(new Response('bad gateway', { status: 522 }))
    const snap = createSnapshotSource('https://snap.test')
    const res = await fetchWithSnapshot('https://api.test/api/stats', '/api/stats', {}, snap)
    expect(await res.json()).toEqual({ total_pilots: 7 })
    expect(res.headers.get('x-snapshot-generated-at')).toBe(MANIFEST.generated_at)
    expect(offlineStore.get()).toEqual({ offline: true, snapshotAt: MANIFEST.generated_at })
  })

  it('answers a network error or timeout from the snapshot, honouring ?limit', async () => {
    live = () => Promise.reject(new TypeError('Failed to fetch'))
    const snap = createSnapshotSource('https://snap.test')
    const res = await fetchWithSnapshot('https://api.test/api/news?limit=1', '/api/news?limit=1', {}, snap)
    expect(await res.json()).toEqual({ days: [{ day: 'a' }] })
  })

  it('without the file, or without a snapshot, fails as before but still marks offline', async () => {
    live = () => Promise.reject(new TypeError('Failed to fetch'))
    const snap = createSnapshotSource('https://snap.test')
    await expect(fetchWithSnapshot('https://api.test/api/online', '/api/online', {}, snap)).rejects.toThrow('Failed to fetch')
    expect(offlineStore.get()).toEqual({ offline: true, snapshotAt: null })
    live = () => Promise.resolve(new Response('', { status: 503 }))
    const res = await fetchWithSnapshot('https://api.test/api/stats', '/api/stats', {}, undefined)
    expect(res.status).toBe(503)
  })

  it('never answers a POST from the snapshot', async () => {
    live = () => Promise.resolve(new Response('', { status: 502 }))
    const snap = createSnapshotSource('https://snap.test')
    const res = await fetchWithSnapshot('https://api.test/api/stats', '/api/stats', { method: 'POST' }, snap)
    expect(res.status).toBe(502)
    expect(offlineStore.get().offline).toBe(false)
  })

  it('is off without a URL', () => {
    expect(createSnapshotSource(undefined)).toBeUndefined()
    expect(createSnapshotSource('  ')).toBeUndefined()
  })
})

describe('offline text', () => {
  const now = Date.parse('2026-10-25T12:12:30Z')
  it('says how old the data is', () => {
    expect(ago('2026-10-25T12:00:00Z', now)).toBe('12 min ago')
    expect(ago('2026-10-25T09:00:00Z', now)).toBe('3 h ago')
    expect(ago('2026-10-22T12:00:00Z', now)).toBe('3 days ago')
    expect(offlineText({ offline: true, snapshotAt: '2026-10-25T12:00:00Z' }, now))
      .toBe('Server offline: showing data from Sun 25 Oct, 12:00 UTC (12 min ago)')
  })
  it('says nothing while online, or with nothing from the snapshot', () => {
    expect(offlineText({ offline: false, snapshotAt: null }, now)).toBeNull()
    expect(offlineText({ offline: true, snapshotAt: null }, now)).toBeNull()
  })
})
