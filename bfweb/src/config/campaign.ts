// ─────────────────────────────────────────────────────────────────────────────
// Vector Strike — Campaign Configuration
// Edit this file to customise all branding, links, and copy across the web UI.
// ─────────────────────────────────────────────────────────────────────────────

export const campaign = {
  /** Full campaign name — shown in sidebar, page titles, etc. */
  name: 'Vector Strike',

  /** Short abbreviation — used in logo monogram */
  shortName: 'VS',

  /** Tagline shown beneath the name in the sidebar footer */
  tagline: 'Where every sortie shapes the war.',

  /** Displayed in the sidebar footer version chip */
  version: 'v3.0',

  /** Discord invite URL */
  discord: 'https://discord.gg/XyGkb3WAXS',

  /** Server name shown in status area */
  server: 'The Coop · Operation Vector Strike',

  /** URL of the public campaign website (bfsite) */
  websiteUrl: 'http://localhost:5174',

  /** Accent colour (CSS hex) — used for active-nav left border, badges, etc. */
  accentColor: '#4d7c0f',

  /** Blue faction label */
  blueLabel: 'BLUFOR',

  /** Red faction label */
  redLabel: 'REDFOR',
} as const
