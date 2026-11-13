/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** Absolute origin of a remotely-hosted bfdb API, e.g. "https://api.example.com".
   *  No trailing slash. Leave unset when bfweb is embedded in bfdb (same-origin). */
  readonly VITE_API_BASE?: string
  /** Server status notices (bfsite's public/status.json). Defaults to the public site's copy. */
  readonly VITE_STATUS_URL?: string
  /** bfdb's public snapshot (deploy/snapshot-fallback.md), read while bfdb is
   *  down, e.g. "https://snapshot.example.org". Unset: no fallback. */
  readonly VITE_SNAPSHOT_URL?: string
  /** The bfdb instance id this dashboard shows (the Red Storm server). Set:
   *  every request is scoped to it and the server selector is hidden.
   *  Unset: the selector works as in bfweb (lib/instanceLock.ts). */
  readonly VITE_INSTANCE?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}
