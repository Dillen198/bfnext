/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** Absolute origin of a remotely-hosted bfdb API, e.g. "https://api.example.com".
   *  No trailing slash. Leave unset when bfsite is embedded in bfdb (same-origin). */
  readonly VITE_API_BASE?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}
