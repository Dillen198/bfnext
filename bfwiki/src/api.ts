// Default: relative '/api'. Set VITE_API_BASE (e.g. "https://api.example.com")
// at build time to point a standalone-hosted wiki at a remotely-hosted bfdb
// instead — see bfnext-vector/deploy/README.md. No trailing slash.
export const API_ROOT: string = import.meta.env.VITE_API_BASE ?? ''

const BASE = `${API_ROOT}/api`

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
  const res = await fetch(`${BASE}${path}`, { credentials: 'include' })
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
  auth: {
    me:           () => get<{ user: AuthUser | null }>('/auth/me').then(r => r.user),
    logout:       () => fetch(`${BASE}/auth/logout`, { credentials: 'include' }),
    loginUrl:     () => `${BASE}/auth/login`,
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
