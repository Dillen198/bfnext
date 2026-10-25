// The parts of a bfdb client every front end shares: request deadlines, error
// text, websocket URLs and the selected DCS server instance. Each app keeps its
// own endpoint list (`api.ts`) on top of these.

/** Deadline for an ordinary API call. bfdb's host can vanish outright
 *  (Cloudflare then holds the request open before answering 52x), and a
 *  request with no deadline leaves a page spinning for minutes. */
export const REQUEST_TIMEOUT_MS = 30_000

/** Deadline for uploads and admin operations that bfdb proxies to the bot,
 *  which allows them five minutes (an update, a restart, a backup). */
export const LONG_TIMEOUT_MS = 300_000

/** An AbortSignal that fires after `ms`. Undefined where the browser has no
 *  `AbortSignal.timeout` (Chromium < 103), so the request still goes out. */
export function deadline(ms: number = REQUEST_TIMEOUT_MS): AbortSignal | undefined {
  return typeof AbortSignal !== 'undefined' && typeof AbortSignal.timeout === 'function'
    ? AbortSignal.timeout(ms)
    : undefined
}

/** bfdb's `{ "error": "..." }` (or `{ "message": "..." }`), else the status. */
export async function errorMessage(res: Response): Promise<string> {
  try {
    const j = await res.json()
    if (j && typeof j.error === 'string') return j.error
    if (j && typeof j.message === 'string') return j.message
  } catch { /* body wasn't JSON */ }
  return `HTTP ${res.status}`
}

/** A websocket URL for `path` on bfdb: `apiRoot` with ws(s) for http(s), or
 *  this page's own host when the app is served same-origin (apiRoot ''). */
export function wsUrl(apiRoot: string, path: string): string {
  if (apiRoot) return apiRoot.replace(/^http/, 'ws') + path
  const proto = window.location.protocol === 'https:' ? 'wss' : 'ws'
  return `${proto}://${window.location.host}${path}`
}

/** Who is signed in, from `/api/auth/me` (the Discord session on the API host). */
export interface AuthUser {
  discord_id: string
  username:   string
  avatar:     string | null
  is_admin:   boolean
  ucid:       string | null
  /** Coalition in the active round; null if the pilot has no side (or isn't
   *  linked). Gates access to the dashboard's recon intel page. */
  side:       'Blue' | 'Red' | null
}

/** `/api/instances`: the DCS servers one bfdb fronts. */
export interface InstanceList<I> {
  default: string
  instances: I[]
}

// ── Selected DCS server instance ────────────────────────────────────────
// One bfdb can front several DCS servers (see deploy/multi-instance.md).
// Every instance-scoped route takes `?instance=<id>`. Rather than thread that
// through every call site, the app keeps the choice in a store like this one
// and runs each path through `withInstance`.
//
// `undefined` means "let bfdb pick its default instance", which is exactly
// what a single-server deployment wants.

export interface InstanceStore {
  /** The instance every request is currently scoped to, if any. */
  get(): string | undefined
  /** Switch servers. Callers drop or invalidate their query cache afterwards:
   *  nothing here is reactive. */
  set(id: string | undefined): void
  /** Append `instance=<id>` to a path, respecting any query string it has.
   *  A path that names an instance itself (`?instance=all`) is left alone. */
  withInstance(path: string): string
}

export function createInstanceStore(opts: {
  /** localStorage key the choice is remembered under. */
  storageKey: string
  /** Also honour `?server=<DCS server name>` (the in-DCS cockpit overlay
   *  knows the server's name, not bfdb's id; bfdb resolves either). */
  serverParam?: boolean
}): InstanceStore {
  const params = (): URLSearchParams | null => {
    try { return new URLSearchParams(window.location.search) } catch { return null }
  }
  let current: string | undefined = (() => {
    // A URL param wins over the remembered choice, so a link can point
    // straight at one server: ?instance=vs2
    const fromUrl = params()?.get('instance')
    if (fromUrl) return fromUrl
    try { return localStorage.getItem(opts.storageKey) ?? undefined } catch { return undefined }
  })()
  const server = opts.serverParam ? params()?.get('server') ?? undefined : undefined

  return {
    get: () => current,
    set(id) {
      current = id
      try {
        if (id) localStorage.setItem(opts.storageKey, id)
        else localStorage.removeItem(opts.storageKey)
      } catch { /* private mode / storage disabled */ }
    },
    withInstance(path) {
      if (/[?&](instance|server)=/.test(path)) return path
      const sep = path.includes('?') ? '&' : '?'
      // An explicit instance id is the precise answer; the DCS server name is
      // the fallback the in-game overlay can supply.
      if (current) return `${path}${sep}instance=${encodeURIComponent(current)}`
      if (server) return `${path}${sep}server=${encodeURIComponent(server)}`
      return path
    },
  }
}
