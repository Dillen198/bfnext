// ─────────────────────────────────────────────────────────────────────────────
// Vector Strike — the theatres
// One entry per DCS server. This is the pitch for each one; its rules (lives
// tables, aircraft rosters, points) live in the wiki, which quotes them from
// the engine config of whichever server the reader picks.
// ─────────────────────────────────────────────────────────────────────────────

/** Flags blended into a theatre's backdrop: the blue side's, then the red side's. */
export type FlagId = 'blufor' | 'redfor' | 'georgia' | 'russia'

export interface ServerProfile {
  /**
   * The bfdb instance this profile describes (`?instance=`). The live data
   * comes from `/api/instances`; a profile is matched to it by this id.
   */
  instanceId: string
  label: string
  era: string
  summary: string
  /** Exactly as it appears in the DCS multiplayer server browser. */
  browserName: string
  /**
   * The engine's `limited_lives`. The two servers differ on it, and it is the
   * first thing a new pilot asks, so the card says it plainly.
   */
  livesEnforced: boolean
  /** Coalition names for this campaign — they are not BLUFOR/REDFOR everywhere. */
  blueLabel: string
  redLabel: string
  flags: [FlagId, FlagId]
}

export const SERVERS: ServerProfile[] = [
  {
    instanceId: 'vs1',
    label: 'Modern · Syria',
    era: 'Modern day',
    summary:
      'The long-running modern campaign: coalition airpower against a peer air ' +
      'force, fought with current-generation jets across Syria.',
    browserName: '[VS] Vector Strike | Dynamic Campaign PVP Modern',
    livesEnforced: false,
    blueLabel: 'BLUFOR',
    redLabel: 'REDFOR',
    // Generic coalitions on this server, so coalition colours, not a country.
    flags: ['blufor', 'redfor'],
  },
  {
    instanceId: 'vs2',
    label: '2008 · Caucasus',
    era: 'Russo-Georgian War, August 2008',
    summary:
      'August 2008, fought over the ground it happened on: 56 objectives across ' +
      'the Caucasus, 44 hidden SAM sites, and an economy that charges you for ' +
      'the airframe and the loadout.',
    browserName: '[VS] Vector Strike | Dynamic Campaign PVP 2008',
    livesEnforced: true,
    blueLabel: 'Georgia',
    redLabel: 'Russia',
    flags: ['georgia', 'russia'],
  },
]
