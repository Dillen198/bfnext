// ─────────────────────────────────────────────────────────────────────────────
// Southern Flank 1985: this dashboard's fixed identity.
//
// bfrsr-web shows one campaign, the Red Storm Rising server, so its name,
// papers and colours are not taken from bfdb's /api/config (that config is
// the main campaign's). The config still supplies everything that is not
// branding: links, the server address, map centre, credits.
//
// The names are our own: no real paper, insignia or the novel's title.
// ─────────────────────────────────────────────────────────────────────────────

import { campaign, type CampaignConfig } from './campaign'

export const BRAND = {
  name: 'Southern Flank 1985',
  shortName: 'SF85',
  tagline: 'NATO Southern Command · Black Sea and Caucasus front',
  /** The classification banner across the top of every page. Invented. */
  banner: 'SECRET — SOUTHERN FLANK — 1985',
  /** The Western wire and the Soviet communiqué. */
  papers: { wire: 'The Allied Dispatch', alt: 'Front Bulletin' },
  emblem: { onDark: '/redstorm/emblem-dark.svg', onLight: '/redstorm/emblem-light.svg' },
} as const

export type ThemeName = 'dark' | 'light'

/**
 * Side colours for the places that draw from `campaign.*Color` in script
 * (map markers, charts, inline styles). Hex, because callers append alpha
 * digits. One pair per theme so text in them keeps AA contrast: steel blue
 * for NATO and Soviet red, light on the consoles and dark ink on paper.
 */
export const SIDE_COLORS: Record<ThemeName, { blue: string; red: string; accent: string; accentHover: string }> = {
  dark: { blue: '#8db4d9', red: '#f0685a', accent: '#ffb000', accentHover: '#ffc845' },
  light: { blue: '#24507c', red: '#a3231a', accent: '#8a4b00', accentHover: '#6b3a00' },
}

/** The campaign config with this dashboard's identity laid over it. */
export function withBrand(cfg: Partial<CampaignConfig>, theme: ThemeName): Partial<CampaignConfig> {
  const c = SIDE_COLORS[theme]
  return {
    ...cfg,
    name: BRAND.name,
    shortName: BRAND.shortName,
    tagline: BRAND.tagline,
    // The server line on About, unless it names the other campaign.
    server: cfg.server && !/vector\s*strike/i.test(cfg.server) ? cfg.server : BRAND.name,
    // The main campaign's logo would be its brand, not ours.
    logoUrl: '',
    // A background picture or camouflage is the other dashboard's look.
    backgroundImage: '',
    camoPattern: 'none',
    accentColor: c.accent,
    accentHoverColor: c.accentHover,
    blueColor: c.blue,
    redColor: c.red,
    // Side names the server does not give: the setting's own.
    blueLabel: cfg.blueLabel && !/^BLUFOR$/i.test(cfg.blueLabel) ? cfg.blueLabel : 'NATO',
    redLabel: cfg.redLabel && !/^REDFOR$/i.test(cfg.redLabel) ? cfg.redLabel : 'Soviet',
  }
}

/**
 * Put the theme's side colours into `campaign`, where script reads them.
 * Called while rendering (Layout) as well as when the theme is applied, so a
 * page remounted on a theme change reads the new pair on that same render.
 */
export function syncSideColors(theme: ThemeName) {
  const c = SIDE_COLORS[theme]
  campaign.blueColor = c.blue
  campaign.redColor = c.red
  campaign.accentColor = c.accent
  campaign.accentHoverColor = c.accentHover
}
