// ─────────────────────────────────────────────────────────────────────────────
// Vector Strike — site copy and links
// The site sells the campaign and gets people flying. Anything that explains
// *how* something works belongs in the wiki: link to a page there (see `wiki`
// below) rather than copying it here, so there is one copy to keep current.
// ─────────────────────────────────────────────────────────────────────────────

import { VS_URLS } from '@shared/web/urls'

export const links = {
  discord: 'https://discord.gg/XyGkb3WAXS',
  dashboard: VS_URLS.dashboard,
  wiki: VS_URLS.wiki,
  dcs: 'https://www.digitalcombatsimulator.com',
} as const

/** A page on the wiki, by its slug (bfdb's seed_wiki path without `.md`). */
export const wiki = (slug: string) => `${links.wiki}/${slug}`

/** A page on the ops dashboard (bfweb routes). */
export const dashboard = (path = '') => `${links.dashboard}${path}`

export const campaign = {
  name: 'Vector Strike',
  tagline: 'Where every sortie shapes the war.',
  pitch:
    'A persistent, player-versus-player campaign for DCS World. The front line ' +
    'moves when you move it, the supply you haul keeps it there, and the war ' +
    'is still running when you log off.',
} as const

export interface Capability {
  icon: 'Comms' | 'Sam' | 'Supply' | 'Health' | 'CommandCenter' | 'Rankings'
  title: string
  body: string
  wiki: string
}

/** What sets the campaign apart -- the hook, not the manual. */
export const capabilities: Capability[] = [
  {
    icon: 'Comms',
    title: 'Live voice GCI',
    body: 'A controller works the radar picture over SRS: threat calls, SAM launches, bogey dope and vectors in NATO brevity.',
    wiki: 'gameplay/gci',
  },
  {
    icon: 'Sam',
    title: 'Air defence that thinks',
    body: 'SAM sites share one sensor picture, stay dark until they have a shot, and cover each other against anti-radiation missiles.',
    wiki: 'advanced/modern-war',
  },
  {
    icon: 'Supply',
    title: 'A war economy',
    body: 'Convoys, factories and airlifted crates feed the front. Starve the enemy’s supply and their objectives start to fall.',
    wiki: 'gameplay/war-economy',
  },
  {
    icon: 'Health',
    title: 'Losses that count',
    body: 'Points buy your support and deployables. Where lives are enforced, each role flies from a limited pool, and a rescue crew can win a lost life back.',
    wiki: 'gameplay/points-and-lives',
  },
  {
    icon: 'CommandCenter',
    title: 'An HQ that fights back',
    body: 'Each side has an AI theatre commander running air packages, fires, convoys and troop lifts, and filling whatever gaps the players leave.',
    wiki: 'advanced/theatre-hq',
  },
  {
    icon: 'Rankings',
    title: 'Every sortie on record',
    body: 'Kills, captures and cargo runs build a pilot career, with sortie debriefs and flight replays to fly back through.',
    wiki: 'playbooks/flight-replay',
  },
]

export interface Role {
  icon: 'Intercept' | 'Cas' | 'Logistics' | 'Csar' | 'Capture' | 'CommandCenter'
  tag: string
  title: string
  body: string
  image: string
  alt: string
  wiki: string
}

export const roles: Role[] = [
  {
    icon: 'Intercept',
    tag: 'Air war',
    title: 'Own the sky',
    body: 'Fly CAP over your front, take vectors from GCI, and dig out the SAM sites that wait for you to come close.',
    image: '/media/air-war.jpg',
    alt: 'A camouflaged SAM battery hidden in a canyon at dusk',
    wiki: 'advanced/modern-war',
  },
  {
    icon: 'Cas',
    tag: 'Strike & CAS',
    title: 'Work with a JTAC',
    body: 'Copy the 9-line, find the laser and break up the armour pushing on your objective before it gets there.',
    image: '/media/cas.jpg',
    alt: 'A JTAC calling in an A-10 strike from a ridge line',
    wiki: 'playbooks/cas-with-jtac',
  },
  {
    icon: 'Logistics',
    tag: 'Airlift',
    title: 'Move the front',
    body: 'C-130 airdrops, helicopter crate runs and troop lifts. Every FARP and every rebuilt base starts in a cargo bay.',
    image: '/media/airlift.jpg',
    alt: 'A C-130 dropping supply crates under parachutes',
    wiki: 'playbooks/running-crates',
  },
  {
    icon: 'Csar',
    tag: 'Rescue',
    title: 'Bring them home',
    body: 'A downed pilot is a life lost until someone lands beside the green smoke. Pick them up and the life comes back.',
    image: '/media/csar.jpg',
    alt: 'A Huey hoisting a downed pilot out of a valley',
    wiki: 'f10-menu/csar',
  },
  {
    icon: 'Capture',
    tag: 'Ground',
    title: 'Take the objective',
    body: 'Insert troops, deploy units and hold the ground until a contested FARP becomes your FARP.',
    image: '/media/capture.jpg',
    alt: 'Troops fast-roping from a helicopter onto an outpost',
    wiki: 'playbooks/capturing-a-base',
  },
  {
    icon: 'CommandCenter',
    tag: 'Command',
    title: 'Run the war',
    body: 'Set the coalition’s priorities on a live map, spend its points, and move formations against the enemy line.',
    image: '/media/command.jpg',
    alt: 'An officer at a desk covered in maps and a glowing laptop',
    wiki: 'advanced/command',
  },
]

export const credits = {
  developers: ['No15 | KillerDog', '[.ID] EagleEye'],
}
