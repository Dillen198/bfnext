// ─────────────────────────────────────────────────────────────────────────────
// Vector Strike — the servers
// One entry per DCS server with a page on the site and a field guide in the
// wiki. This is the pitch for each one; the briefing and story are beside it
// in shared/servers/, and the rules (lives, rosters, points) in the wiki.
// ─────────────────────────────────────────────────────────────────────────────

import { VS_URLS } from '../web/urls'

/**
 * Flags blended into a theatre's backdrop: the blue side's, then the red side's.
 * `rsii` is one picture of four: Russia, Syria, Iran and Iraq.
 */
export type FlagId = 'blufor' | 'redfor' | 'georgia' | 'russia' | 'nato' | 'rsii'

/**
 * Each server's page has its own look: `desert` is the modern war's
 * battlefield, `staff` the 1985 staff paper, `chart` the range's aeronautical
 * chart. The theme names the page component and its stylesheet.
 */
export type ServerTheme = 'desert' | 'staff' | 'chart'

/** The class that carries each theme's palette (themes.css). */
export const THEME_CLASS: Record<ServerTheme, string> = {
  desert: 'srv-desert',
  staff: 'srv-redstorm',
  chart: 'srv-range',
}

export interface ServerProfile {
  /** Its page: /servers/<slug>. */
  slug: string
  /**
   * The campaign bfdb instance this profile describes (`?instance=`). The
   * live data comes from `/api/instances`; a profile is matched to it by this
   * id. The range runs on its own box, behind its own bfdb, so it has none.
   */
  instanceId?: string
  theme: ServerTheme
  label: string
  /** The campaign's own name, for the page masthead. */
  title: string
  era: string
  summary: string
  /** Exactly as it appears in the DCS multiplayer server browser. */
  browserName: string
  /**
   * The engine's `limited_lives`. It is the first thing a new pilot asks, so
   * the page says it plainly. Unset where there are no lives at all.
   */
  livesEnforced?: boolean
  /** Coalition names for this campaign — they are not BLUFOR/REDFOR everywhere. */
  blueLabel: string
  redLabel: string
  flags?: [FlagId, FlagId]
  /** The live companion site: this campaign's dashboard, or the range site. */
  site: { label: string; url: string }
}

export const SERVERS: ServerProfile[] = [
  {
    slug: 'syria',
    instanceId: 'vs1',
    theme: 'desert',
    label: 'Modern · Syria',
    title: 'Levant Ablaze',
    era: 'Modern day',
    summary:
      'A Russian-built air defence shield has gone up over Syria and Lebanon, and ' +
      'NATO has gone to war to break it. The Alliance against the RSII coalition, ' +
      'with current-generation jets across the Levant.',
    browserName: '[VS] Vector Strike | Dynamic Campaign PVP Modern',
    livesEnforced: false,
    blueLabel: 'NATO',
    // Russia, Syria, Iran and Iraq: the 2015 four-nation coalition.
    redLabel: 'RSII',
    flags: ['nato', 'rsii'],
    site: { label: 'Campaign dashboard', url: VS_URLS.dashboard },
  },
  {
    slug: 'southern-flank',
    // deploy: miz/Scenarios/Red Storm Rising/DEPLOY.md ("Names used").
    instanceId: 'redstorm',
    theme: 'staff',
    label: '1985 · Caucasus',
    title: 'Southern Flank 1985',
    era: 'Red Storm Rising, 15 June 1985',
    summary:
      'The war in Central Europe is a week old. NATO has come ashore in western ' +
      'Georgia to cut the supply lines of the Soviet Transcaucasus Front, with ' +
      '1985 aircraft and 1985 weapons only.',
    browserName: '[VS] Vector Strike | RSR',
    livesEnforced: true,
    blueLabel: 'NATO',
    redLabel: 'Soviet',
    site: { label: 'Red Storm dashboard', url: VS_URLS.redstorm },
  },
  {
    slug: 'range',
    theme: 'chart',
    label: 'Training · Marianas',
    title: 'Training Range',
    era: 'Mariana Islands Range Complex',
    summary:
      'A practice server. No campaign, no lives, no points: every slot is open ' +
      'and everything you fly is graded, from bomb circles to carrier passes.',
    browserName: '[VS] Vector Strike | Training Range',
    blueLabel: 'Blue',
    redLabel: 'Red',
    site: { label: 'Range results', url: VS_URLS.range },
  },
]

/** The servers that run a campaign on this site's bfdb. */
export const CAMPAIGNS = SERVERS.filter((s): s is ServerProfile & { instanceId: string } => !!s.instanceId)

export const serverBySlug = (slug: string | undefined) => SERVERS.find(s => s.slug === slug)
