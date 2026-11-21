// Red Storm Rising: Southern Flank, 15 June 1985. The briefing is the
// in-game one (redstorm/tools/briefings.py: GENERAL, BLUE, RED), split into
// pieces the page can lay out; the story and the will table are in
// redstormStory.ts.

import type { Orders } from './types'

export const SITUATION: { lead?: string; body: string }[] = [
  {
    body:
      'The war in Central Europe is a week old. In the south the Transcaucasus Front is pushing through ' +
      'Soviet Georgia and Armenia towards Turkey and, beyond it, the oil of the Gulf. NATO’s answer is to ' +
      'go for the Front’s supply lines rather than meet it head on.',
  },
  {
    body:
      'A US carrier battle group has passed the Turkish Straits. Under Operation GOLDEN FLEECE (western ' +
      'Georgia was the Colchis of the old legend), US Marines and Turkish troops came ashore on the Adjarian ' +
      'coast and have broken out: NATO now holds western Georgia and Abkhazia, from Batumi to Kutaisi and up ' +
      'the coast to Gagra, with its headquarters in Kutaisi. The Soviet Union holds eastern Georgia and ' +
      'everything north of the mountains, run from the Front headquarters in Tbilisi.',
  },
  {
    lead: 'The front has three parts.',
    body:
      'On the coast, Gagra and Gudauta face Sochi across the Psou. In the centre, Zestafoni and Sachhere face ' +
      'the Surami tunnel, Gori and Tskhinvali. In the mountains, the Mamison road comes down to Ambrolauri and ' +
      'the Georgian Military Road runs through Kazbegi.',
  },
  {
    lead: 'The supply lines are the mountains.',
    body:
      'Fuel and ammunition reach the Front three ways: the coastal railway from Tuapse through Sochi and ' +
      'Sukhumi, the two roads over the Greater Caucasus from Ordzhonikidze (the Roki road and the Georgian ' +
      'Military Road), and, inside Georgia, the single rail line through the Surami tunnel. Cut them and every ' +
      'Soviet unit west of Surami fights with what it carries.',
  },
  {
    lead: 'Fuel decides this war.',
    body:
      'The refineries at Tuapse and Krasnodar feed the Front; the oil terminal at Batumi feeds NATO. Soviet ' +
      'fuel stocks are already short.',
  },
]

/** The campaign at a glance, NATO first. */
export const GLANCE: { k: string; blue?: string; red?: string; both?: string }[] = [
  { k: 'Date', both: '15 June 1985, 06:00' },
  { k: 'Headquarters', blue: 'Kutaisi city', red: 'Tbilisi' },
  {
    k: 'Fleets',
    blue: 'USS Forrestal (CV-59) battle group off Batumi; USS Tarawa (LHA-1) amphibious ready group off Poti',
    red: 'Black Sea Fleet, the cruiser Moskva, from Novorossiysk',
  },
  { k: 'Air defence', both: '46 hidden SAM and radar sites' },
  { k: 'Persistence', both: 'Territory, stocks, aircraft and ground forces carry over between sessions' },
  { k: 'Manning', both: 'Both sides always open; when one is short of pilots, its AI fills the gap' },
]

export const ORDERS: Orders[] = [
  {
    side: 'Blue',
    from: 'NATO · Operation GOLDEN FLEECE',
    mission: 'Starve the Transcaucasus Front.',
    tasks: [
      {
        lead: 'Hold western Georgia.',
        body:
          'Batumi, Kobuleti, Senaki, Kutaisi, Sukhumi and Gudauta are your airfields; Batumi port, Samtredia and ' +
          'Zestafoni your supply hubs. Zestafoni and Sachhere hold the Surami front, Gagra the coast. Your ' +
          'headquarters is Kutaisi city: lose it and NATO’s will takes a heavy blow.',
      },
      {
        lead: 'Keep the fleet alive.',
        body:
          'Soviet Naval Aviation Backfires from Mozdok and Krymsk will come for the carrier battle group off ' +
          'Batumi and for USS Tarawa’s amphibious ready group off Poti whenever they are in range. Each ship is ' +
          'worth more to NATO’s will than any airfield.',
      },
      {
        lead: 'Cut the supply lines:',
        body:
          'the coastal railway, the Surami tunnel, the Roki and Georgian Military roads. Burn the refineries at ' +
          'Tuapse and Krasnodar.',
      },
      { lead: 'Push east', body: 'through the Surami pass to Gori, and north up the coast to Sochi.' },
    ],
    closing: [
      'You have the better aircraft and the shorter legs. They have the numbers.',
      'Strike packages: B-52H runway raids, F-117A precision strikes, Tornado IDS low-level raids and Tomahawks ' +
        'from the Ticonderoga. E-3A and E-2C on station, KC-135 and S-3B tankers.',
    ],
  },
  {
    side: 'Red',
    from: 'Soviet Union · Transcaucasus Front',
    mission: 'Throw NATO back into the sea before the fuel runs out.',
    tasks: [
      {
        lead: 'Retake western Georgia.',
        body:
          'NATO holds everything west of the Surami ridge and the coast to Gagra. Break through at Zestafoni ' +
          'towards Kutaisi, and push down the coast from Sochi through Gagra and Gudauta.',
      },
      {
        lead: 'Sink the carrier.',
        body:
          'Long Range and Naval Aviation Tu-22M3s with Kh-22 fly from Mozdok and Krymsk; the Black Sea Fleet’s ' +
          'Moskva group sails from Novorossiysk.',
      },
      {
        lead: 'Keep the fuel moving south.',
        body:
          'Defend the refineries at Tuapse and Krasnodar, the Surami tunnel and the passes. Front fuel stocks ' +
          'are already short: every convoy and every tank car counts.',
      },
      {
        lead: 'Take Kutaisi.',
        body: 'It is NATO’s main inland airfield, and Kutaisi city is NATO’s headquarters. Hold Tbilisi, yours.',
      },
    ],
    closing: [
      'You have the numbers: MiG-29 and Su-27 regiments, MiG-23s on the coast, MiG-25 and MiG-31 interceptors, ' +
        'Su-24 and MiG-27 strike regiments, and a dense air defence from SA-6 at the front to S-300 in the rear.',
      'A-50 on station, Il-78 tankers, the 104th Guards Airborne ready to drop.',
    ],
  },
]

/** How the war ends, from the briefing and the will settings in RSR_SF_CFG. */
export const ENDING: string[] = [
  'There is no script. Each side has a will to fight, starting at 70 of 100. Losses, lost ground and empty ' +
    'fuel tanks wear it down; enemy losses and quiet hours restore it.',
  'A side whose will stays below 20 for six hours asks for terms. Holding 95% of the map wins outright; 28 ' +
    'days without a result is a draw.',
  'Fuel counts too: below 20% fuel a side loses 1 will an hour, and below 8% it loses 3 an hour.',
]

export const FLYING: { title: string; body: string }[] = [
  {
    title: 'Dynamic spawn only',
    body:
      'No fixed slots: spawn any aircraft your side has in stock at a field you hold. Every flyable module is ' +
      'open, modern ones included, but only with 1985 weapons.',
  },
  {
    title: 'Era rules',
    body:
      'AMRAAM, AIM-9X, AIM-54C, AIM-7P, R-77, JDAM and laser JDAM, JSOW, SLAM, APKWS, Kh-31, Kh-38, Kh-59M, ' +
      'KAB-500S, the Hellfire, Vikhr and later Mavericks, and targeting pods (LITENING, ATFLIR, Sniper) are ' +
      'blocked at the ramp. Take off with one and you have five minutes to land; fire one and you lose the ' +
      'aircraft. Practice ammunition and smoke pods are blocked too.',
  },
  {
    title: 'Radios',
    body:
      'Your side’s plan is on presets 1-8, the other side’s on 9-16, where your radios can tune them. Flaming ' +
      'Cliffs aircraft have no presets.',
  },
  {
    title: 'Both sides, same tools',
    body:
      'Both sides can build Scud-B launchers (night raids on factories and hubs) and Silkworm anti-ship ' +
      'batteries from crates. Supply trains run the railways and are fair game. You are warned when one of ' +
      'your objectives is attacked.',
  },
]

export const AIRCRAFT = {
  blue: {
    note: 'Every Western module',
    list: [
      'F-14 (carrier)', 'F/A-18C (carrier)', 'F-15C', 'F-15E', 'F-16C', 'F-4E', 'F-5E', 'F-100D', 'A-10A',
      'A-10C', 'AV-8B (Tarawa or ashore)', 'UH-1H', 'OH-58D', 'AH-64D', 'CH-47F',
    ],
  },
  red: {
    note: 'Every Soviet and Chinese module',
    list: [
      'MiG-29 (all versions)', 'Su-27', 'J-11A', 'Su-33', 'Su-25', 'Su-25T', 'MiG-21bis', 'MiG-19P',
      'MiG-15bis', 'L-39C', 'L-39ZA', 'JF-17', 'Mi-24P', 'Mi-8', 'Ka-50',
    ],
  },
  both: ['AJS 37', 'Mirage F1', 'Mirage 2000C', 'C-101', 'MB-339', 'Gazelle', 'C-130J'],
}
