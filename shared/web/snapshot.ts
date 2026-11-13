// ─────────────────────────────────────────────────────────────────────────────
// The public snapshot: the last known campaign state, for when bfdb is down.
//
// bfdb publishes its public read routes to a bucket every minute (see
// bfdb/src/snapshot.rs and deploy/snapshot-fallback.md). When a public GET to
// bfdb fails -- a network error, the request deadline, or a 5xx from in front
// of it (Caddy's 502, Cloudflare's 52x) -- `fetchWithSnapshot` answers it
// from there instead and marks the app offline, so it can say how old the
// data is. A 5xx that bfdb wrote itself (JSON `{"error": ...}`, e.g. its DCS
// engine not answering) means bfdb is up: that one is passed through as is.
//
// Opt-in per app: with no VITE_SNAPSHOT_URL, `createSnapshotSource` returns
// undefined and every request behaves exactly as before.
// ─────────────────────────────────────────────────────────────────────────────

import { useEffect, useState, useSyncExternalStore } from 'react'
import { deadline } from './api'
import { fmtUtc } from './status'

/** `manifest.json`: what the snapshot holds and when it was made. */
export interface SnapshotManifest {
  version: number
  /** When bfdb wrote this snapshot (ISO 8601 UTC): the age the banner shows. */
  generated_at: string
  /** The instance a request naming none is answered for. */
  default_instance: string
  /** Campaign instances with files under `i/`. */
  instances: string[]
  files: Record<string, { sha256: string; bytes: number }>
}

/** `i/<id>/state.json`: one server's last known state. Not an API route. */
export interface ServerLastState {
  instance: string
  label: string
  kind: 'campaign' | 'range'
  /** Whether the DCS engine answered when this was written. */
  online: boolean
  /** The last time bfdb saw the engine answer (ISO 8601 UTC), if ever. */
  last_seen: string | null
  /** The mission (sortie) it was running. */
  mission: string | null
  active_round: { id: number; number: number | null; scenario: string; start: string } | null
  /** The engine's clock on its newest stat, while online. */
  mission_time: string | null
  players: { blue: number; red: number; total: number } | null
  dcs_version: string | null
  bfdb: { version: string; git: string; built: string }
}

// ── Which requests the snapshot can answer ──────────────────────────────────
// Mirrors bfdb's layout (`allowed_key` in bfdb/src/snapshot.rs). A request it
// cannot answer maps to null and fails as it always did.

/** Instance-scoped routes, under `i/<instance>/`. */
const INSTANCE_ROUTES = new Set([
  'online', 'objectives', 'frontline', 'rounds', 'leaderboard', 'stats', 'news', 'wiki/facts',
])
/** Query parameters the snapshot ignores: it holds the default page (news: 30 days). */
const IGNORED_PARAMS = ['limit']
const SAFE_INSTANCE = /^[A-Za-z0-9_-]+$/
const WIKI_PAGE = /^wiki\/pages\/[A-Za-z0-9._-]+(\/[A-Za-z0-9._-]+)*$/

/**
 * The snapshot file for an API request, or null if there is none.
 *
 * @param path The request as the app makes it, from `/api/` on, query and all
 *   (`/api/objectives?instance=vs1`).
 * @param defaultInstance The manifest's `default_instance`, for a request
 *   that names none.
 */
export function snapshotKey(path: string, defaultInstance: string): string | null {
  let url: URL
  try { url = new URL(path, 'http://snapshot.invalid') } catch { return null }
  if (!url.pathname.startsWith('/api/')) return null
  let rest: string
  try { rest = decodeURIComponent(url.pathname.slice('/api/'.length)) } catch { return null }
  rest = rest.replace(/\/+$/, '')
  if (rest.split('/').some(s => s === '' || s === '.' || s === '..')) return null

  const params = url.searchParams
  // The cockpit overlay names its server, not an instance id; only bfdb can
  // resolve that.
  if (params.has('server')) return null
  const instance = params.get('instance') || defaultInstance
  params.delete('instance')
  for (const p of IGNORED_PARAMS) params.delete(p)
  const extra = [...params.entries()]

  if (rest === 'instances') {
    if (extra.length === 0) return 'g/instances.json'
    if (extra.length === 1 && extra[0][0] === 'kind' && extra[0][1] === 'range') return 'g/instances~kind-range.json'
    return null
  }
  if (extra.length) return null
  if (rest === 'wiki/pages' || WIKI_PAGE.test(rest)) return `g/${rest}.json`
  if (instance === 'all' || !SAFE_INSTANCE.test(instance)) return null
  if (INSTANCE_ROUTES.has(rest) || /^rounds\/\d+\/summary$/.test(rest)) return `i/${instance}/${rest}.json`
  return null
}

// ── The offline store ───────────────────────────────────────────────────────

export interface OfflineState {
  /** bfdb did not answer the last request that went to it. */
  offline: boolean
  /** When the snapshot being shown was made, once one has been served. */
  snapshotAt: string | null
}

let state: OfflineState = { offline: false, snapshotAt: null }
const listeners = new Set<() => void>()

function setState(next: OfflineState) {
  if (next.offline === state.offline && next.snapshotAt === state.snapshotAt) return
  state = next
  for (const l of listeners) l()
}

export const offlineStore = {
  get: (): OfflineState => state,
  subscribe(fn: () => void): () => void {
    listeners.add(fn)
    return () => { listeners.delete(fn) }
  },
  /** bfdb answered: whatever came from the snapshot is replaced as it refetches. */
  markOnline: () => setState({ offline: false, snapshotAt: null }),
  /** bfdb did not answer; `snapshotAt` if the snapshot answered instead. */
  markOffline: (snapshotAt?: string) => setState({ offline: true, snapshotAt: snapshotAt ?? state.snapshotAt }),
}

/** The offline state, re-rendering when it changes. */
export function useOffline(): OfflineState {
  return useSyncExternalStore(offlineStore.subscribe, offlineStore.get, offlineStore.get)
}

// ── Fetching ────────────────────────────────────────────────────────────────

export interface SnapshotSource {
  /** Where the snapshot lives, no trailing slash. */
  base: string
  /** The manifest, cached for a short while; null if it cannot be read. */
  manifest(): Promise<SnapshotManifest | null>
}

/** How long a fetched manifest is reused. bfdb rewrites it every minute. */
const MANIFEST_TTL_MS = 30_000
/** Snapshot reads get their own, shorter deadline: the live call already waited. */
const SNAPSHOT_TIMEOUT_MS = 10_000

/** The snapshot at `base` (an app's VITE_SNAPSHOT_URL); undefined when unset. */
export function createSnapshotSource(base: string | undefined): SnapshotSource | undefined {
  const root = base?.trim().replace(/\/+$/, '')
  if (!root) return undefined
  let cached: { at: number; value: Promise<SnapshotManifest | null> } | null = null
  return {
    base: root,
    manifest() {
      const now = Date.now()
      if (cached && now - cached.at < MANIFEST_TTL_MS) return cached.value
      const value = fetch(`${root}/manifest.json`, { cache: 'no-store', signal: deadline(SNAPSHOT_TIMEOUT_MS) })
        .then(r => (r.ok ? (r.json() as Promise<SnapshotManifest>) : null))
        .catch(() => null)
      cached = { at: now, value }
      return value
    },
  }
}

/** One snapshot file as parsed JSON, if the snapshot has it. */
export async function snapshotFile<T>(snap: SnapshotSource, key: string): Promise<{ data: T; manifest: SnapshotManifest } | null> {
  const m = await snap.manifest()
  if (!m || !m.files[key]) return null
  try {
    const r = await fetch(`${snap.base}/${key}`, { signal: deadline(SNAPSHOT_TIMEOUT_MS) })
    if (!r.ok) return null
    return { data: (await r.json()) as T, manifest: m }
  } catch {
    return null
  }
}

/** A server's last known state, from the snapshot. */
export async function serverLastState(snap: SnapshotSource, instance: string): Promise<ServerLastState | null> {
  if (!SAFE_INSTANCE.test(instance)) return null
  return (await snapshotFile<ServerLastState>(snap, `i/${instance}/state.json`))?.data ?? null
}

/** The snapshot's answer to `apiPath` as a Response, or null. */
async function fromSnapshot(snap: SnapshotSource, apiPath: string): Promise<{ res: Response; at: string } | null> {
  const m = await snap.manifest()
  const key = m && snapshotKey(apiPath, m.default_instance)
  if (!m || !key) return null
  const file = await snapshotFile<unknown>(snap, key)
  if (!file) return null
  let data = file.data
  // `?limit=` is ignored by the key; honour it on the one route that has it.
  const limit = Number(new URL(apiPath, 'http://snapshot.invalid').searchParams.get('limit'))
  if (limit > 0 && key.endsWith('/news.json') && data && typeof data === 'object' && Array.isArray((data as { days?: unknown }).days)) {
    const d = data as { days: unknown[] }
    data = { ...d, days: d.days.slice(0, limit) }
  }
  const res = new Response(JSON.stringify(data), {
    status: 200,
    headers: { 'content-type': 'application/json', 'x-snapshot-generated-at': file.manifest.generated_at },
  })
  return { res, at: file.manifest.generated_at }
}

/** A 5xx bfdb wrote itself: its errors are JSON, a gateway's are not. */
function answeredByBfdb(res: Response): boolean {
  return (res.headers.get('content-type') ?? '').includes('application/json')
}

/**
 * `fetch(url, init)` against bfdb, answered from the snapshot when bfdb does
 * not answer. Only GETs fall back; anything else is a plain `fetch`.
 *
 * @param apiPath The same request from `/api/` on (instance applied), which
 *   picks the snapshot file.
 * @param snap The app's snapshot, or undefined for no fallback (the request
 *   still updates the offline state).
 */
export async function fetchWithSnapshot(
  url: string,
  apiPath: string,
  init: RequestInit = {},
  snap?: SnapshotSource,
): Promise<Response> {
  if ((init.method ?? 'GET').toUpperCase() !== 'GET') return fetch(url, init)
  let res: Response | undefined
  let failure: unknown
  try {
    res = await fetch(url, init)
    if (res.status < 500 || answeredByBfdb(res)) {
      offlineStore.markOnline()
      return res
    }
  } catch (e) {
    failure = e
  }
  const fallback = snap ? await fromSnapshot(snap, apiPath) : null
  offlineStore.markOffline(fallback?.at)
  if (fallback) return fallback.res
  if (res) return res
  throw failure
}

// ── What the banner says ────────────────────────────────────────────────────

/** "12 min ago", "3 h ago", "2 days ago". */
export function ago(iso: string, now: number = Date.now()): string {
  const min = Math.max(0, Math.floor((now - Date.parse(iso)) / 60_000))
  if (min < 120) return `${min} min ago`
  const h = Math.floor(min / 60)
  if (h < 48) return `${h} h ago`
  return `${Math.floor(h / 24)} days ago`
}

/** "Server offline: showing data from Sun 19 Oct, 18:00 UTC (12 min ago)", or
 *  null while nothing is being shown from the snapshot. */
export function offlineText(s: OfflineState, now: number = Date.now()): string | null {
  if (!s.offline || !s.snapshotAt || Number.isNaN(Date.parse(s.snapshotAt))) return null
  return `Server offline: showing data from ${fmtUtc(s.snapshotAt)} (${ago(s.snapshotAt, now)})`
}

/** `offlineText` for the current state, kept current every 30 s. */
export function useOfflineText(): string | null {
  const s = useOffline()
  const [now, setNow] = useState(() => Date.now())
  useEffect(() => {
    if (!s.offline) return
    const tick = () => setNow(Date.now())
    // At once too: `now` dates from whenever this component mounted.
    const first = window.setTimeout(tick, 0)
    const t = window.setInterval(tick, 30_000)
    return () => { window.clearTimeout(first); window.clearInterval(t) }
  }, [s.offline, s.snapshotAt])
  return offlineText(s, now)
}
