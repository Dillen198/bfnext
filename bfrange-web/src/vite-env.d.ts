/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** bfdb base URL; unset = same-origin /api (.env.production sets the real one) */
  readonly VITE_API_BASE?: string
  /** bfdb instance id for the range; unset = bfdb picks */
  readonly VITE_RANGE_INSTANCE?: string
  /** the dashboard, for its flight replay (web Tacview); default VS_URLS.dashboard */
  readonly VITE_DASHBOARD_URL?: string
  /** "1" = serve everything from src/mock */
  readonly VITE_MOCK?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}

/** Compile-time copy of `VITE_MOCK === '1'` (see vite.config.ts `define`). */
declare const __RANGE_MOCK__: boolean
