/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** Absolute origin of the bfdb API, e.g. "https://api.example.com". No
   *  trailing slash. Leave unset for local dev (proxied via vite.config.ts). */
  readonly VITE_API_BASE?: string
  /** bfdb's public snapshot (deploy/snapshot-fallback.md), read while bfdb is
   *  down, e.g. "https://snapshot.vectorstrike.org". Unset: no fallback. */
  readonly VITE_SNAPSHOT_URL?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}
