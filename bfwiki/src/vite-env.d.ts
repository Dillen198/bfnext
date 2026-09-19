/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** Absolute origin of the bfdb API, e.g. "https://api.example.com". No
   *  trailing slash. Leave unset for local dev (proxied via vite.config.ts). */
  readonly VITE_API_BASE?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}
