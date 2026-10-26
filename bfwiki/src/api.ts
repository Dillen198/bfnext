import {
  createInstanceStore, deadline, errorMessage, LONG_TIMEOUT_MS,
  type AuthUser, type InstanceList as SharedInstanceList,
} from '@shared/web/api'
import { createSnapshotSource, fetchWithSnapshot } from '@shared/web/snapshot'

// Default: relative '/api'. Set VITE_API_BASE (e.g. "https://api.example.com")
// at build time to point a standalone-hosted wiki at a remotely-hosted bfdb
// instead — see bfnext-vector/deploy/README.md. No trailing slash.
export const API_ROOT: string = import.meta.env.VITE_API_BASE ?? ''

const BASE = `${API_ROOT}/api`

// bfdb's public snapshot (deploy/snapshot-fallback.md): every page and the
// campaign numbers, read-only, while bfdb is down. Unset: no fallback.
export const snapshot = createSnapshotSource(import.meta.env.VITE_SNAPSHOT_URL)

// ── Selected DCS server instance ────────────────────────────────────
// One bfdb can front several DCS servers (see deploy/multi-instance.md), and
// **each one runs its own engine config** -- different point values, different
// capture timings, a different action list. The wiki's prose is shared across
// them; the numbers are not, which is what `{{cfg:...}}` placeholders and
// `/api/wiki/facts` are for.
//
// Same mechanism as bfweb: `undefined` means "let bfdb pick its default", which
// is what a single-server deployment wants.

const instanceStore = createInstanceStore({ storageKey: 'bfwiki.instance' })

/** The instance facts are currently being read from, if any. */
export const getInstance = instanceStore.get
/** Switch which DCS server the wiki quotes numbers from. Callers invalidate
 *  their query cache afterwards -- nothing here is reactive. */
export const setInstance = instanceStore.set
/** Append `instance=<id>` to a path, respecting any query string it has. */
export const withInstance = instanceStore.withInstance
export { instanceStore }

/** One DCS server instance this bfdb fronts. */
export interface ServerInstance {
  id: string
  label: string
  default: boolean
  /** false for a stats-only instance with no live engine to query. */
  live: boolean
  /** The mission currently publishing, or null when the server is down. */
  sortie: string | null
  dcs_server_name: string | null
  /** false = a test/staging server; only ever returned to an admin. */
  public: boolean
}

export type InstanceList = SharedInstanceList<ServerInstance>

/** The campaign numbers for one instance, as `/api/wiki/facts` returns them.
 *  `facts` mirrors the engine config's shape (an allow-listed subset of it),
 *  so a placeholder path like `points.air_kill` indexes straight into it. */
export interface WikiFacts {
  instance: { id: string; label: string }
  /** When that instance's engine config was last written, or null. */
  updated_at: string | null
  facts: Record<string, unknown>
}

export type { AuthUser }

export interface WikiPageMeta {
  slug:    string
  title:   string
  section: string
  order:   number
}

export interface WikiPageFull extends WikiPageMeta {
  content:    string
  updated_at: string
  updated_by: string
}

async function get<T>(path: string): Promise<T> {
  const p = withInstance(path)
  const res = await fetchWithSnapshot(`${BASE}${p}`, `/api${p}`, { credentials: 'include', signal: deadline() }, snapshot)
  if (!res.ok) throw new Error(await errorMessage(res))
  return res.json()
}

async function post<T>(path: string, body: unknown): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(body),
    signal: deadline(),
  })
  if (!res.ok) throw new Error(await errorMessage(res))
  return res.json()
}

/** A page slug as a URL path: each `/`-separated segment encoded on its own,
 *  so the separators survive and nothing else in a slug can break out of the
 *  path (a `?`, `#` or `..` would otherwise change which URL is requested). */
function slugPath(slug: string): string {
  return slug.split('/').map(encodeURIComponent).join('/')
}

export const api = {
  /** The DCS servers this bfdb fronts. Never instance-scoped itself. */
  instances: async (): Promise<InstanceList> => {
    const res = await fetchWithSnapshot(`${BASE}/instances`, '/api/instances', { credentials: 'include', signal: deadline() }, snapshot)
    if (!res.ok) throw new Error(await errorMessage(res))
    return res.json()
  },
  auth: {
    me:           () => get<{ user: AuthUser | null }>('/auth/me').then(r => r.user),
    // POST: bfdb no longer ends a session on a GET.
    logout:       () => fetch(`${BASE}/auth/logout`, { method: 'POST', credentials: 'include', signal: deadline() }),
    loginUrl:     () => `${BASE}/auth/login?return_to=${encodeURIComponent(window.location.origin + '/')}`,
    // No username/password login here: bfdb's local login is a machine
    // credential for DCSServerBot on the server itself and refuses anyone
    // else. Discord is the only way a person signs in.
  },
  wiki: {
    list: () => get<WikiPageMeta[]>('/wiki/pages'),
    /** Campaign numbers for the selected instance, for `{{cfg:...}}`. */
    facts: () => get<WikiFacts>('/wiki/facts'),
    get:  (slug: string) => get<WikiPageFull>(`/wiki/pages/${slugPath(slug)}`),
    save: (slug: string, page: { title: string; section: string; order: number; content: string }) =>
      post<{ ok: boolean }>(`/wiki/pages/${slugPath(slug)}`, page),
    delete: (slug: string) => post<{ ok: boolean }>('/wiki/delete', { slug }),
    // Returns an absolute-enough URL (prefixed with API_ROOT, which is ''
    // for same-origin dev/embedded mode) so it resolves correctly even when
    // bfwiki and bfdb are hosted on different origins (e.g. Vercel + a
    // separately-hosted bfdb) -- the server only knows its own relative path.
    uploadImage: async (file: File): Promise<{ id: string; url: string }> => {
      const res = await fetch(`${BASE}/wiki/images`, {
        method: 'POST',
        credentials: 'include',
        headers: { 'content-type': file.type || 'application/octet-stream' },
        body: file,
        signal: deadline(LONG_TIMEOUT_MS),
      })
      if (!res.ok) throw new Error(await errorMessage(res))
      const { id } = await res.json() as { id: string }
      return { id, url: `${API_ROOT}/api/wiki/images/${id}` }
    },
  },
}
