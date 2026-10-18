/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** Absolute origin of a remotely-hosted bfdb API, e.g. "https://api.example.com".
   *  No trailing slash. Leave unset when bfweb is embedded in bfdb (same-origin). */
  readonly VITE_API_BASE?: string
  /** Server status notices (bfsite's public/status.json). Defaults to the public site's copy. */
  readonly VITE_STATUS_URL?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}
