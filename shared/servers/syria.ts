// Levant Ablaze, the modern Syria campaign. The story is original to this
// site, written in the manner of the Southern Flank briefing: no real people,
// and every place, front and number in it is the campaign's own (objectives
// from the mission, fronts and SAM counts from Missions/Vector/Syria/
// README.md). Since mission 2.1 the turning points are the campaign config's
// story events (levantStory.ts, generated from the repo's
// miz/Scenarios/Vector Strike/levant_ablaze.story.json).

import type { Orders } from './types'

/** Who fights, and from where. The RSII's four members fight from Syria and Lebanon. */
export const SIDES = {
  blue: {
    name: 'NATO',
    long: 'The Alliance and its partners',
    members: ['Turkey', 'Israel', 'Jordan', 'Cyprus bases'],
    ground: 'Turkey, Cyprus, Israel and Jordan',
    flag: '/media/flag-nato.png',
  },
  red: {
    name: 'RSII',
    long: 'Russia, Syria, Iran and Iraq',
    members: ['Russia', 'Syria', 'Iran', 'Iraq'],
    ground: 'Syria and Lebanon',
    flag: '/media/flag-rsii.png',
  },
}

/** The opening signal, as the page's terminal types it. */
export const FLASH =
  'FLASH. NATO surveillance aircraft lost off the Syrian coast. Collective defence invoked 0412Z. ' +
  'All bases Incirlik, Akrotiri, Ramat David, Muwaffaq Salti: generate sorties.'

export const SITUATION: { lead?: string; body: string }[] = [
  {
    lead: 'It began with a shield.',
    body:
      'Over one winter a Russian-built air defence network went live across Syria and Lebanon: S-300 ' +
      'batteries at Tabqa and around Damascus, SA-11 and SA-15 layered beneath them, radars reaching deep ' +
      'into Turkish, Cypriot and Israeli airspace. Iranian and Iraqi air regiments flew in behind it. The four ' +
      'governments called it a defensive coalition, Russia, Syria, Iran and Iraq: the RSII.',
  },
  {
    body:
      'On the first night a NATO surveillance aircraft working off the Syrian coast was shot down. The Alliance ' +
      'invoked collective defence before dawn. Turkey opened Incirlik; Israel, Jordan and the British bases on ' +
      'Cyprus followed. NATO’s headquarters is at Incirlik; the coalition fights from the Air Defence Academy ' +
      'and the 159th Regiment’s command post.',
  },
  {
    lead: 'The war has three fronts.',
    body:
      'In the south, Kiryat Shmona and Rosh Pina face the Naqoura line and the Marj as Sultan positions on the ' +
      'road to Damascus, some of them barely 10 km apart. In the north, Hatay and Gaziantep face Taftanaz, ' +
      'Minakh and Aleppo across 60 km of contested ground. At sea, Cyprus faces Latakia across 208 km of open ' +
      'water: NATO’s depth, and the coalition’s only naval base.',
  },
  {
    lead: 'The shield is the war.',
    body:
      'Forty-eight air defence sites are hidden across the map, Patriot, Hawk and IRIS-T on one side and SA-2 ' +
      'to SA-15 on the other, and none of them shows until somebody finds it. Each is networked through its ' +
      'side’s command centres. Kill a command centre and its batteries lose the network picture and fight alone.',
  },
  {
    lead: 'Industry keeps it fed.',
    body:
      'The coalition’s war industry sits in the Aleppo belt, at Al Safira, Kharab Ishk and Tal Siman, and out ' +
      'on the Euphrates at Sharqiyah Mine and Durayhim. NATO’s runs from Ashdod to the hills of Cyprus. A ' +
      'factory produces whether or not the enemy can reach it, so it has to be reached.',
  },
]

export const GLANCE: { k: string; blue?: string; red?: string; both?: string }[] = [
  { k: 'Era', both: 'Modern day, current-generation aircraft and weapons' },
  { k: 'Coalitions', blue: 'NATO with Turkey, Israel, Jordan and the Cyprus bases', red: 'RSII: Russia, Syria, Iran and Iraq' },
  { k: 'Headquarters', blue: 'Incirlik HQ', red: 'Air Defence Academy HQ, SyADF 159th Regiment' },
  { k: 'Naval bases', blue: 'Mersin', red: 'Latakia' },
  { k: 'Air defence', both: '48 hidden SAM sites, networked through the command centres' },
  { k: 'Persistence', both: 'Territory, stocks and aircraft carry over between sessions' },
]

export const ORDERS: Orders[] = [
  {
    side: 'Blue',
    from: 'NATO · Operation BRONZE HELM',
    mission: 'Break the shield.',
    tasks: [
      {
        lead: 'Hold the Galilee.',
        body:
          'Kiryat Shmona, Rosh Pina, Ramat David and Haifa carry the southern front. The coalition’s nearest ' +
          'posts are close enough to see them.',
      },
      {
        lead: 'Blind the network.',
        body:
          'Find the hidden batteries, then the command centres that cue them: the Air Defence Academy and the ' +
          '159th Regiment. Every site they lose fights on its own.',
      },
      {
        lead: 'Open the north.',
        body: 'From Hatay and Gaziantep take Taftanaz and Minakh, then Aleppo.',
      },
      {
        lead: 'Starve the coalition:',
        body: 'the factories of the Aleppo belt, Sharqiyah Mine and Durayhim on the Euphrates, and Latakia.',
      },
    ],
    closing: [
      'You have the depth: 29 airbases from Diyarbakir to the Negev, and a rear nothing can touch.',
      'They have the shield, and the ground under it.',
    ],
  },
  {
    side: 'Red',
    from: 'RSII · Levant Joint Command',
    mission: 'Hold the shield and make the Alliance pay for every mile.',
    tasks: [
      {
        lead: 'Keep the network alive.',
        body:
          'The Air Defence Academy and the 159th Regiment are the shield’s brain. Lose them and every battery ' +
          'fights alone.',
      },
      {
        lead: 'Push into the Galilee.',
        body: 'From Naqoura and Marj as Sultan, take Kiryat Shmona and Rosh Pina.',
      },
      {
        lead: 'Hold Aleppo',
        body: 'and the factories around it. They are why the north matters, to both sides.',
      },
      {
        lead: 'Keep the sea lane and the capital.',
        body: 'Latakia is your only naval base; Damascus is your supply hub. Hold both.',
      },
    ],
    closing: [
      'You fight under your own SAMs, on interior lines, and every NATO strike has to come to you.',
      'Make it cost them.',
    ],
  },
]

/** The story events, as the campaign config carries them (levantStory.ts). */
export { LEVANT_STORY as TURNING_POINTS } from './levantStory'

/** How the war ends: SYRIA_CFG's war_will (Syria/tools/add_story.py). */
export const ENDING: string[] = [
  'Each side has a will to fight, starting at 70 of 100. Losses, lost ground and empty fuel tanks wear it ' +
    'down; enemy losses and quiet hours restore it.',
  'A side whose will stays below 20 for six hours asks for terms. Holding 95% of the map wins outright.',
  'The story has its own endings: once the shield is broken and its will is under 12, the RSII asks for terms; ' +
    'once Incirlik is hit and its will is under 12, NATO stands down.',
]

/** Objectives by kind, Blue then Red. */
export const OOB: [string, number, number][] = [
  ['Airbases', 29, 22],
  ['Logistics hubs', 7, 6],
  ['FOBs', 13, 17],
  ['Factories', 9, 9],
  ['Command centres', 2, 2],
  ['Naval bases', 1, 1],
]

export const SAMS = {
  blue: { count: 25, types: 'Patriot, Hawk, IRIS-T' },
  red: { count: 23, types: 'SA-2, SA-6, SA-10, SA-11, SA-15' },
  note:
    'Hidden until someone finds them. Every site sits at least 4 km outside any objective, so it has to be ' +
    'fought for on its own, and no two are close enough for one package to roll up both.',
}

export const FRONTS: { code: string; name: string; status: 'hot' | 'warm' | 'cold'; body: string }[] = [
  {
    code: 'S',
    name: 'South: Golan, Lebanon, northern Israel',
    status: 'hot',
    body:
      'The real front. Almost every opposing pair of objectives within 80 km of each other is here, some of ' +
      'them barely 10 km apart.',
  },
  {
    code: 'N',
    name: 'North: Hatay and Gaziantep against the Aleppo belt',
    status: 'warm',
    body:
      'A genuine second front at about 60 km, facing Taftanaz, Minakh and Aleppo. The coalition’s newer ' +
      'factories are in this belt, which gives it something worth flying to.',
  },
  {
    code: 'W',
    name: 'Maritime: Cyprus and the Syrian coast',
    status: 'cold',
    body:
      '208 km of open water from Larnaca to Latakia. Nothing on Cyprus can be threatened, which makes it depth ' +
      'and logistics rather than a front.',
  },
]
