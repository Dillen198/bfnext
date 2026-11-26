// The training range, Mariana Islands. Everything here is from the range
// mission's in-game briefing (bfrange/mission/build_range_miz.py, BRIEFING)
// and its deck spawns (DECK). The range has no story: it is a practice server.

export const INTRO =
  'A practice server. No campaign, no lives, no points: every slot is open and everything you fly is graded. ' +
  'Results, debriefs, the greenie board and the spawner are on the range site.'

export const COMPLEX =
  'The thin outlines on the F10 map are the real special use airspace around Guam and the Northern Marianas: ' +
  'the ATCAAs over the warning areas, W-517, and R-7201 round Farallon de Medinilla. Every block has a job, ' +
  'the one it has in life; the filled areas inside are the ranges. F10 > Range > Sectors lists them nearest ' +
  'first with bearing and range, and you are told what a sector is for when you fly in.'

export const MARKINGS =
  'The ranges are marked on the ground like real ones: tyre rings round the bomb circle and the arrays, a ' +
  'tyre foul line on the strafe pit, flags on the coordinate targets and the gunnery lane, tyre pads and cones ' +
  'for helicopters.'

export type SectorKind =
  | 'ag' | 'cas' | 'threat' | 'gunnery' | 'helo' | 'aa' | 'bvr' | 'tanker' | 'carrier' | 'ship' | 'hot' | 'ew'

/** The F10 map's colour key, as the briefing gives it. */
export const LEGEND: { kind: SectorKind; colour: string; use: string }[] = [
  { kind: 'ag', colour: 'Amber', use: 'Air-to-ground' },
  { kind: 'cas', colour: 'Yellow', use: 'Tactical / CAS' },
  { kind: 'threat', colour: 'Red', use: 'Threat / SEAD' },
  { kind: 'gunnery', colour: 'Tan', use: 'CA gunnery' },
  { kind: 'helo', colour: 'Lime', use: 'Helicopter' },
  { kind: 'aa', colour: 'Cyan', use: 'Air-to-air' },
  { kind: 'bvr', colour: 'Blue', use: 'BVR' },
  { kind: 'tanker', colour: 'Green', use: 'Tanker tracks' },
  { kind: 'carrier', colour: 'White', use: 'Carrier ops' },
  { kind: 'ship', colour: 'Violet', use: 'Anti-ship' },
  { kind: 'hot', colour: 'Hot red', use: 'Hot zone' },
  { kind: 'ew', colour: 'Teal', use: 'EW / GPS jamming' },
]

export const RULES: string[] = [
  'Launch from Andersen or Won Pat (dynamic spawn, hot start) or the decks.',
  'Work inside your airspace; R-7201 round Farallon de Medinilla is the only live impact area at sea.',
  'Tankers sit on the published tracks: fly the entry line from the ARIP, join at the ARCP, leave on the exit line.',
]

export const SECTORS: { group: string; rows: { id: string; kind: SectorKind; what: string }[] }[] = [
  {
    group: 'Air-to-air',
    rows: [
      { id: 'W-517', kind: 'aa', what: 'BFM / ACM, live missile shots (SW of Guam)' },
      { id: 'ATCAA 2 (W-12)', kind: 'aa', what: 'Duels, round W-517' },
      { id: 'ATCAA 5', kind: 'aa', what: 'BFM west of Guam' },
      { id: 'ATCAA 1 (W-11)', kind: 'bvr', what: 'BVR presentations, Fox 1 / Fox 3 defence' },
      { id: 'ATCAA 3C (W-13C)', kind: 'aa', what: 'Intercepts east of the chain' },
      { id: 'ATCAA 3A (W-13A)', kind: 'aa', what: 'Large-force strikes into R-7201 against red air' },
      { id: 'ATCAA 6', kind: 'aa', what: 'Over the islands: intercepts and DCA above FL150' },
    ],
  },
  {
    group: 'Air-to-ground',
    rows: [
      { id: 'R-7201A', kind: 'ag', what: 'The 12 nm ring round FDM: hold, then run in' },
      { id: 'R-7201 FDM', kind: 'ag', what: 'Bomb circle, strafe pit (run in 209 from the NNE), JDAM' },
      { id: 'TN-1 North Field', kind: 'cas', what: 'Tinian: tactical array laser 1688, convoy, JDAM, JTAC Axeman 133.0' },
      { id: 'TN-2 Tinian MLA', kind: 'cas', what: 'Tiered targets: easy / medium (AAA) / hard (SHORAD)' },
      { id: 'EW-1 Tinian South', kind: 'ew', what: 'GPS jamming over coordinate targets' },
    ],
  },
  {
    group: 'Threat',
    rows: [
      { id: 'R-3 Aguijan', kind: 'threat', what: 'SA-8 emitter (radar on, weapons hold)' },
      { id: 'S-1 Rota', kind: 'threat', what: 'Live SEAD / IADS: EWR, SA-2/3/6/11/10, Pantsir, weapons free' },
      { id: 'HZ-1 Saipan', kind: 'hot', what: 'Hot zone: red CAP + SHORAD, AWACS Overlord 1 on 251.5' },
    ],
  },
  {
    group: 'Guam',
    rows: [
      { id: 'H-1 Andersen', kind: 'helo', what: 'Helicopter courses, Andersen South LZ, Mt Santa Rosa' },
      { id: 'G-1 NW Field', kind: 'gunnery', what: 'Combined Arms gunnery lane' },
      { id: 'PT-1 Andersen', kind: 'carrier', what: 'Overhead breaks; every landing everywhere is graded' },
      { id: 'CS-1 South Guam', kind: 'helo', what: 'CSAR, beacon 350 kHz (Helicopter > CSAR)' },
      { id: 'LL-1 Guam coast', kind: 'cas', what: 'Low-level route Ritidian to Orote, 8 gates, below 500 ft' },
    ],
  },
  {
    group: 'At sea',
    rows: [
      { id: 'CV OPAREA', kind: 'carrier', what: 'CVN-72 TACAN 72X ICLS 11 Link-4 336.0, LHA-1 71X, recovery tanker A-6E 63Y 261.0. Every pass graded.' },
      { id: 'FD-1 Apra', kind: 'carrier', what: 'Frigate decks: FFG-7 41X, DDG 42X' },
      { id: 'AS-1 SINKEX', kind: 'ship', what: 'Undefended hulks in W-517' },
      { id: 'ATCAA 3B (W-13B)', kind: 'ship', what: 'Maritime strike; AS-2 escorted convoy, weapons free' },
    ],
  },
]

export const TANKERS: { track: string; type: string; tacan: string; freq: string; alt: string; speed: string; note?: string }[] = [
  { track: 'AR RIO', type: 'KC-135 boom', tacan: '51Y', freq: '251.0', alt: 'FL250', speed: '300 KIAS' },
  { track: 'AR MIRAGE', type: 'KC-135MPRS basket', tacan: '52Y', freq: '252.0', alt: 'FL210', speed: '270 KIAS', note: 'next to FDM' },
  { track: 'AR LUXOR', type: 'KC-135 boom', tacan: '54Y', freq: '254.0', alt: 'FL240', speed: '300 KIAS' },
  { track: 'AR SHELL', type: 'KC-130 basket', tacan: '53Y', freq: '253.0', alt: '14,000 ft', speed: '220 KIAS', note: 'west of Guam' },
  { track: 'Recovery', type: 'A-6E over the carrier', tacan: '63Y', freq: '261.0', alt: '6,000 ft', speed: '250 KIAS' },
]

/** Deck spawns: ship, aircraft, callsign. */
export const DECKS: { ship: string; aircraft: string; role: string; callsign: string }[] = [
  { ship: 'CVN-72', aircraft: 'F/A-18C Hornet', role: 'CAP', callsign: 'Enfield, Springfield' },
  { ship: 'CVN-72', aircraft: 'F-14B Tomcat', role: 'CAP', callsign: 'Uzi' },
  { ship: 'CVN-72', aircraft: 'F-14A Tomcat', role: 'CAP', callsign: 'Colt' },
  { ship: 'LHA-1', aircraft: 'AV-8B Harrier', role: 'CAS', callsign: 'Dodge' },
  { ship: 'LHA-1', aircraft: 'UH-1H Huey', role: 'Transport', callsign: 'Ford' },
]

export const TRAINER =
  'The missile trainer removes any missile that would kill you and tells you so, SAMs included, so the SEAD ' +
  'range, the hot zone and the escorted convoy are live fire you survive. Guns and AAA are real. ' +
  'Chat: -range trainer on|off'
