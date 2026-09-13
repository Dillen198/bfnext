// Default: relative '/api'. Set VITE_API_BASE (e.g. "https://api.example.com")
// at build time to point a standalone-hosted wiki at a remotely-hosted bfdb
// instead — see bfnext-vector/deploy/README.md. No trailing slash.
export const API_ROOT: string = import.meta.env.VITE_API_BASE ?? ''

const BASE = `${API_ROOT}/api`

// ── Selected DCS server instance ────────────────────────────────────
// One bfdb can front several DCS servers (see deploy/multi-instance.md), and
// **each one runs its own engine config** -- different point values, different
// capture timings, a different action list. The wiki's prose is shared across
// them; the numbers are not, which is what `{{cfg:...}}` placeholders and
// `/api/wiki/facts` are for.
//
// Same mechanism as bfweb: `undefined` means "let bfdb pick its default", which
// is what a single-server deployment wants.

const INSTANCE_STORAGE_KEY = 'bfwiki.instance'

let currentInstance: string | undefined = (() => {
  try {
    // A URL param wins over the remembered choice, so a link can point at one
    // server's numbers: /gameplay/points-and-lives?instance=vs2
    const fromUrl = new URLSearchParams(window.location.search).get('instance')
    if (fromUrl) return fromUrl
    return localStorage.getItem(INSTANCE_STORAGE_KEY) ?? undefined
  } catch {
    return undefined
  }
})()

/** The instance facts are currently being read from, if any. */
export function getInstance(): string | undefined {
  return currentInstance
}

/** Switch which DCS server the wiki quotes numbers from. Callers invalidate
 *  their query cache afterwards -- nothing here is reactive. */
export function setInstance(id: string | undefined): void {
  currentInstance = id
  try {
    if (id) localStorage.setItem(INSTANCE_STORAGE_KEY, id)
    else localStorage.removeItem(INSTANCE_STORAGE_KEY)
  } catch { /* private mode / storage disabled */ }
}

/** Append `instance=<id>` to a path, respecting any query string it has. */
export function withInstance(path: string): string {
  if (!currentInstance) return path
  if (/[?&]instance=/.test(path)) return path
  return path + (path.includes('?') ? '&' : '?') + `instance=${encodeURIComponent(currentInstance)}`
}

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

export interface InstanceList {
  default: string
  instances: ServerInstance[]
}

/** The campaign numbers for one instance, as `/api/wiki/facts` returns them.
 *  `facts` mirrors the engine config's shape (an allow-listed subset of it),
 *  so a placeholder path like `points.air_kill` indexes straight into it. */
export interface WikiFacts {
  instance: { id: string; label: string }
  /** When that instance's engine config was last written, or null. */
  updated_at: string | null
  facts: Record<string, unknown>
}

export interface AuthUser {
  discord_id: string
  username:   string
  avatar:     string | null
  is_admin:   boolean
  ucid:       string | null
}

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

async function errorMessage(res: Response): Promise<string> {
  try {
    const j = await res.json()
    if (j && typeof j.error === 'string') return j.error
  } catch { /* body wasn't JSON */ }
  return `HTTP ${res.status}`
}

async function get<T>(path: string): Promise<T> {
  const res = await fetch(`${BASE}${withInstance(path)}`, { credentials: 'include' })
  if (!res.ok) throw new Error(await errorMessage(res))
  return res.json()
}

async function post<T>(path: string, body: unknown): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(body),
  })
  if (!res.ok) throw new Error(await errorMessage(res))
  return res.json()
}

export const api = {
  /** The DCS servers this bfdb fronts. Never instance-scoped itself. */
  instances: async (): Promise<InstanceList> => {
    const res = await fetch(`${BASE}/instances`, { credentials: 'include' })
    if (!res.ok) throw new Error(await errorMessage(res))
    return res.json()
  },
  auth: {
    me:           () => get<{ user: AuthUser | null }>('/auth/me').then(r => r.user),
    logout:       () => fetch(`${BASE}/auth/logout`, { credentials: 'include' }),
    loginUrl:     () => `${BASE}/auth/login?return_to=${encodeURIComponent(window.location.origin + '/')}`,
    localEnabled: () => get<{ enabled: boolean }>('/auth/local-enabled').then(r => r.enabled),
    localLogin: async (username: string, password: string): Promise<void> => {
      const res = await fetch(`${BASE}/auth/local-login`, {
        method: 'POST',
        credentials: 'include',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ username, password }),
      })
      if (!res.ok) throw new Error(await errorMessage(res))
    },
  },
  wiki: {
    list: () => get<WikiPageMeta[]>('/wiki/pages'),
    /** Campaign numbers for the selected instance, for `{{cfg:...}}`. */
    facts: () => get<WikiFacts>('/wiki/facts'),
    get:  (slug: string) => get<WikiPageFull>(`/wiki/pages/${slug}`),
    save: (slug: string, page: { title: string; section: string; order: number; content: string }) =>
      post<{ ok: boolean }>(`/wiki/pages/${slug}`, page),
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
      })
      if (!res.ok) throw new Error(await errorMessage(res))
      const { id } = await res.json() as { id: string }
      return { id, url: `${API_ROOT}/api/wiki/images/${id}` }
    },
  },
}
