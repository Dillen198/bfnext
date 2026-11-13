// Each server's field guide, put together from the same briefing and story
// the public site shows (shared/servers/), so the two cannot disagree. The
// guide is the reference cut: everything on one page, plus the wiki pages that
// explain the rules it mentions.

import { SERVERS, type ServerProfile } from '@shared/servers/profiles'
import type { Orders } from '@shared/servers/types'
import * as syria from '@shared/servers/syria'
import * as redstorm from '@shared/servers/redstorm'
import { STORY, WILL_COSTS } from '@shared/servers/redstormStory'
import * as range from '@shared/servers/range'

export interface GuideSide {
  tone: 'b' | 'r'
  name: string
  members?: string[]
  note: string
  flag?: string
}

export interface GuideEvent {
  side: 'Blue' | 'Red'
  title: string
  when: string
  /** `{by}` is the pilots credited. */
  text: string
  effects?: string[]
}

export interface GuideTable {
  title: string
  head?: string[]
  /** `chip` colours the first cell with a range sector kind. */
  rows: { chip?: string; cells: string[] }[]
}

export interface Guide {
  profile: ServerProfile
  kicker: string
  sides?: GuideSide[]
  flash?: string
  situation: { lead?: string; body: string }[]
  glance?: { k: string; blue?: string; red?: string; both?: string }[]
  orders?: Orders[]
  ending?: { text: string[]; costs: [string, string][] }
  story?: { title: string; intro: string; events: GuideEvent[]; note?: string }
  /** Said plainly where a server has no story. */
  noStory?: string
  rules?: { title: string; body: string }[]
  tables?: GuideTable[]
  /** Wiki pages that explain the rules this server runs on. */
  related: { slug: string; label: string }[]
}

const bySlug = (slug: string) => SERVERS.find(s => s.slug === slug)!

const GUIDES: Guide[] = [
  {
    profile: bySlug('syria'),
    kicker: 'Field guide · Modern day · Syria and the Levant',
    sides: [
      { tone: 'b', name: syria.SIDES.blue.name, members: syria.SIDES.blue.members, note: `From ${syria.SIDES.blue.ground}`, flag: syria.SIDES.blue.flag },
      { tone: 'r', name: syria.SIDES.red.name, members: syria.SIDES.red.members, note: `From ${syria.SIDES.red.ground}`, flag: syria.SIDES.red.flag },
    ],
    flash: syria.FLASH,
    situation: syria.SITUATION,
    glance: syria.GLANCE,
    orders: syria.ORDERS,
    // Syria's war_will uses the Red Storm values (Syria/tools/add_story.py).
    ending: { text: syria.ENDING, costs: WILL_COSTS },
    story: {
      title: 'Turning points',
      intro: 'Fourteen moments the war can turn on. None runs on a clock: each fires once, when players or the AI make its condition true, and is announced to both sides with the names of the pilots who did it.',
      events: syria.TURNING_POINTS,
    },
    rules: [
      ...syria.FRONTS.map(f => ({ title: f.name, body: f.body })),
      { title: 'Hidden air defence', body: `${syria.SAMS.blue.count} NATO sites (${syria.SAMS.blue.types}) and ${syria.SAMS.red.count} RSII sites (${syria.SAMS.red.types}). ${syria.SAMS.note}` },
    ],
    tables: [
      { title: 'Objectives at the start of a round', head: ['Kind', 'NATO', 'RSII'], rows: syria.OOB.map(([k, b, r]) => ({ cells: [k, String(b), String(r)] })) },
    ],
    related: [
      { slug: 'advanced/modern-war', label: 'Modern war and the air defence network' },
      { slug: 'gameplay/objectives', label: 'Objectives' },
      { slug: 'gameplay/war-economy', label: 'The war economy' },
      { slug: 'gameplay/gci', label: 'Live GCI' },
      { slug: 'reference/aircraft-roster', label: 'Aircraft roster' },
    ],
  },
  {
    profile: bySlug('southern-flank'),
    kicker: 'Field guide · Red Storm Rising · 15 June 1985',
    sides: [
      { tone: 'b', name: 'NATO', note: 'Operation GOLDEN FLEECE: western Georgia and Abkhazia, from Kutaisi' },
      { tone: 'r', name: 'Soviet Union', note: 'The Transcaucasus Front: eastern Georgia and the north, from Tbilisi' },
    ],
    situation: redstorm.SITUATION,
    glance: redstorm.GLANCE,
    orders: redstorm.ORDERS,
    ending: { text: redstorm.ENDING, costs: WILL_COSTS },
    story: {
      title: 'Story events',
      intro: 'Eighteen moments the war can turn on. None runs on a clock: each fires once, when players or the AI make its condition true, and is announced to both sides with the names of the pilots who did it.',
      events: STORY,
    },
    rules: redstorm.FLYING,
    tables: [
      {
        title: 'Aircraft',
        head: ['Side', 'Flyable'],
        rows: [
          { cells: [`NATO (${redstorm.AIRCRAFT.blue.note.toLowerCase()})`, redstorm.AIRCRAFT.blue.list.join(', ')] },
          { cells: ['Both sides', redstorm.AIRCRAFT.both.join(', ')] },
          { cells: [`Soviet (${redstorm.AIRCRAFT.red.note.toLowerCase()})`, redstorm.AIRCRAFT.red.list.join(', ')] },
        ],
      },
    ],
    related: [
      { slug: 'advanced/campaign-events', label: 'How story events work' },
      { slug: 'gameplay/round-rules', label: 'Round rules and how a round ends' },
      { slug: 'gameplay/points-and-lives', label: 'Points and lives' },
      { slug: 'gameplay/carrier-ops', label: 'Carrier operations' },
      { slug: 'gameplay/comms-plan', label: 'The comms plan' },
      { slug: 'advanced/theatre-hq', label: 'The theatre HQ' },
    ],
  },
  {
    profile: bySlug('range'),
    kicker: 'Field guide · Mariana Islands Range Complex',
    situation: [{ body: range.INTRO }, { lead: 'The complex.', body: range.COMPLEX }, { lead: 'The markings.', body: range.MARKINGS }],
    noStory:
      'None, by design. The range is a practice server: no campaign, no sides to win and no story events. What it keeps is your record: every bomb, pass and landing, graded, on the range site.',
    rules: [
      { title: 'Fly it like the real thing', body: range.RULES.join(' ') },
      { title: 'Missile trainer', body: range.TRAINER },
    ],
    tables: [
      { title: 'Map colour key', head: ['Colour', 'Used for'], rows: range.LEGEND.map(l => ({ chip: l.kind, cells: [l.colour, l.use] })) },
      ...range.SECTORS.map(g => ({ title: `Sectors: ${g.group}`, rows: g.rows.map(r => ({ chip: r.kind, cells: [r.id, r.what] })) })),
      {
        title: 'Tankers',
        head: ['Track', 'Type', 'TACAN', 'Freq', 'Alt', 'Speed'],
        rows: range.TANKERS.map(t => ({ chip: 'tanker', cells: [t.track, t.note ? `${t.type} (${t.note})` : t.type, t.tacan, t.freq, t.alt, t.speed] })),
      },
      { title: 'Deck spawns', head: ['Ship', 'Aircraft', 'Role', 'Callsign'], rows: range.DECKS.map(d => ({ chip: 'carrier', cells: [d.ship, d.aircraft, d.role, d.callsign] })) },
    ],
    related: [
      { slug: 'gameplay/carrier-ops', label: 'Carrier operations' },
      { slug: 'reference/aircraft-roster', label: 'Aircraft roster' },
    ],
  },
]

export const guideBySlug = (slug: string | undefined) => GUIDES.find(g => g.profile.slug === slug)
export { SERVERS }
