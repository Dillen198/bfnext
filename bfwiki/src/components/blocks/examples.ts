/**
 * One copy-paste example per mockup block: the editor's Blocks reference
 * shows them, the dev fixture page (/__blocks) draws them, and the tests
 * parse them, so the three can never disagree about what a valid block is.
 */
import type { MockupLang } from './langs'

export interface BlockExample {
  lang: MockupLang
  name: string
  /** What the block is for and which fields it takes. */
  about: string
  body: string
}

const json = (v: unknown) => JSON.stringify(v, null, 2)

export const BLOCK_EXAMPLES: BlockExample[] = [
  {
    lang: 'f10menu',
    name: 'F10 radio menu',
    about:
      'A clickable copy of the DCS radio menu. "items" nest: an entry with its own "items" is a submenu, one without is a command. ' +
      'A command\'s "says" is what the server prints in the message box when it is clicked; "note" is an explanation shown under it. ' +
      'More than ten entries in one menu are split into "Next Page..." the way DCS does it.',
    body: json({
      root: 'F10 Other',
      items: [
        {
          label: 'Cargo',
          items: [
            { label: 'Unpack Nearby Crate(s)', says: 'SA-3 site built from 4 crates.', note: 'Every crate the recipe needs must be on the ground within 200 m.' },
            { label: 'Load Nearby Crate', says: 'FARP Crate loaded (1/2 crate slots)' },
            { label: 'Unload Crate', says: 'Unloaded FARP Crate at Hama' },
            { label: 'List Nearby Crates', says: 'SA-3 Launcher  42 m, bearing 270°' },
            { label: 'List Cargo', says: 'Current Cargo: FARP Crate (1/2 crate slots)' },
            { label: 'Destroy Nearby Crate', says: 'Destroyed SA-3 Launcher crate' },
            {
              label: 'Crates',
              items: [
                {
                  label: 'Air Defense',
                  items: [
                    { label: 'SA-3 Launcher', says: 'SA-3 Launcher crate spawned. 3 more needed to build an SA-3 site here.' },
                    { label: 'SA-3 Radar', says: 'SA-3 Radar crate spawned.' },
                  ],
                },
              ],
            },
          ],
        },
        {
          label: 'Troops',
          items: [
            { label: 'Load Standard Squad', says: 'Standard squad loaded (8 troops)' },
            { label: 'Unload Troops', says: 'Standard squad deployed' },
          ],
        },
        { label: 'Objectives', items: [{ label: 'Nearest Objective', says: 'Senaki (blue) 11.9 nm @ 012°' }] },
      ],
    }),
  },
  {
    lang: 'dcsmessage',
    name: 'DCS message box',
    about: 'Plain text, not JSON: exactly what the server prints in the top-right message box. Line breaks are kept.',
    body: 'SA-3 Launcher crate spawned. 3 more needed to build an SA-3 site here.\nCurrent Cargo: FARP Crate (1/2 crate slots)',
  },
  {
    lang: 'cockpitpanel',
    name: 'Cockpit panel',
    about:
      'The Vector Strike panel. "tab" is MENU, CARGO, STATUS, EWR, LOG, WHEEL or WIKI. Rows are {"heading"}, {"text"} or {"button"}; ' +
      'a button\'s "says" goes in the status line when it is clicked. Optional: "status" (the status line before any click), ' +
      '"crumb" (the "< Back  F10 > ..." row; MENU always has one), a text row\'s "style" (dim, bad, warn, good, blue, mono, answer), ' +
      'a button\'s "sub" (submenu look, the default when the label ends in ">"), "go" (the green SPAWN ALL look) and "enabled": false.',
    body: json({
      tab: 'CARGO',
      rows: [
        { heading: 'ON BOARD' },
        { text: 'Crates 1 / 2    Troops 0 / 8' },
        { text: '  • FARP Crate' },
        { heading: 'NEARBY CRATES' },
        { text: '  SA-3 Launcher  --  42 m, bearing 270°' },
        { button: 'UNPACK NEARBY CRATE(S)', says: 'SA-3 site built from 4 crates.' },
        { button: 'LOAD NEARBY CRATE', says: 'SA-3 Launcher loaded (2/2 crate slots)' },
        { heading: 'BUILD SOMETHING -- pick a category' },
        { button: '  Air Defense   (6)  >', says: 'Opens the Air Defense crates' },
      ],
      status: 'Vector Strike 3.0.0  ·  CTRL+ALT+C panel  ·  CTRL+ALT+W wheel',
    }),
  },
  {
    lang: 'wheel',
    name: 'Radial wheel',
    about:
      '"theme" is green, blue, amber, red or white. Up to ten slots on the ring (more fold into MORE); a slot with "items" is a group, ' +
      'whose children open as an arc outside the ring. A slot\'s "says" shows in the SERVER strip under the wheel. Optional: "icon" ' +
      '(an icon name; picked from the label otherwise), "hub" (the hub\'s second line), "danger" (asks for a second click; guessed ' +
      'from words like Destroy), and the wheel\'s "title" and "subtitle" for the hub.',
    body: json({
      theme: 'green',
      subtitle: 'UH-1H  ·  31108 pts',
      items: [
        {
          label: 'Cargo',
          items: [
            { label: 'Unpack', says: 'SA-3 site built from 4 crates.' },
            { label: 'Load', says: 'FARP Crate loaded (1/2 crate slots)' },
            { label: 'Unload', says: 'Unloaded FARP Crate at Hama' },
            { label: 'Nearby', says: 'SA-3 Launcher  42 m, bearing 270°' },
            { label: 'List Cargo', says: 'Current Cargo: FARP Crate (1/2 crate slots)' },
            { label: 'Destroy', says: 'Destroyed SA-3 Launcher crate' },
          ],
        },
        { label: 'Troops', items: [{ label: 'Load Squad', says: 'Standard squad loaded' }, { label: 'Unload', says: 'Standard squad deployed' }] },
        { label: '1/2 Crates', icon: 'crate', hub: 'FARP Crate', says: 'Current Cargo: FARP Crate (1/2 crate slots)' },
        { label: 'Objectives', items: [{ label: 'Nearest', icon: 'compass', says: 'Senaki (blue) 11.9 nm @ 012°' }] },
        { label: '12NM 012°', icon: 'compass', hub: 'Senaki (blue) 11.9 nm @ 012°', says: 'Senaki (blue) 11.9 nm @ 012°' },
        { label: 'F10 Menu', icon: 'panel', hub: 'Opens the panel', says: 'The panel opens on its MENU tab' },
      ],
    }),
  },
  {
    lang: 'steps',
    name: 'Walkthrough',
    about:
      'A list of steps shown one at a time with Back/Next. "text" is markdown; "image" is optional (an uploaded /api/wiki/images/... ' +
      'path or any URL), with an optional "alt".',
    body: json([
      { title: 'Land near a logistics hub', text: 'Crates spawn next to you, so set down **within 200 m** of a friendly FARP or airbase.' },
      { title: 'Spawn the crates', text: 'F10 > Cargo > Crates > Air Defense > **SA-3 Launcher**, once per crate the recipe needs.' },
      { title: 'Unpack', text: 'With every crate on the ground nearby, pick F10 > Cargo > **Unpack Nearby Crate(s)**.' },
    ]),
  },
]

/** An example as it is written in a page. */
export function fenced(ex: BlockExample): string {
  return '```' + ex.lang + '\n' + ex.body + '\n```'
}
