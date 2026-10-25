// Where the Vector Strike sites live in production. The one list of them: an
// app that links to another, or reads a file from one, takes the address from
// here (each app may still let a VITE_* variable override it at build time).
//
// The API host is not used as a default by any app: each talks to a
// same-origin /api unless VITE_API_BASE is set (see each app's .env files).

export const VS_URLS = {
  /** The public site (bfsite). */
  site: 'https://www.vectorstrike.org',
  /** The campaign dashboard (bfweb). */
  dashboard: 'https://dashboard.vectorstrike.org',
  /** The player wiki (bfwiki). */
  wiki: 'https://wiki.vectorstrike.org',
  /** The training range site (bfrange-web). */
  range: 'https://range.vectorstrike.org',
  /** bfdb, for VITE_API_BASE and the dev servers' `--mode live` proxy. */
  api: 'https://api.vectorstrike.org',
} as const

/** The hand-written server notices, served by the public site's static host
 *  so they stay readable while the game host is down (see status.ts). */
export const STATUS_FILE_URL = `${VS_URLS.site}/status.json`
