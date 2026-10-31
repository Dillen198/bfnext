/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** Absolute origin of a remotely-hosted bfdb API, e.g. "https://api.example.com".
   *  No trailing slash. Leave unset when bfsite is embedded in bfdb (same-origin). */
  readonly VITE_API_BASE?: string
  /** Where status.json lives. Defaults to the site's own copy (public/status.json). */
  readonly VITE_STATUS_URL?: string
  /** bfdb's public snapshot (deploy/snapshot-fallback.md), read while bfdb is
   *  down, e.g. "https://snapshot.vectorstrike.org". Unset: no fallback. */
  readonly VITE_SNAPSHOT_URL?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}
