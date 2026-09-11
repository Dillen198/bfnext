/**
 * Vector Strike icon set.
 *
 * Hand-drawn replacements for the domain glyphs lucide has no real answer for
 * (a runway, an angled flight deck, a suppressed radar). Generic chrome --
 * chevrons, close, search, save -- stays on lucide; there is no identity to be
 * won by redrawing a chevron. Contact symbology on the tactical scope stays on
 * milsymbol, which draws real MIL-STD-2525 symbols.
 *
 * Drawing rules (see createIcon.tsx for the chassis):
 *   - fill the 24x24 box; a glyph that sits timidly in the middle dies at 16px
 *   - six strokes is the budget -- detail that only resolves at 48px is noise
 *   - silhouette first: these are read at a glance on a map, not studied
 */
import { createIcon } from './createIcon'

export type { IconProps, IconComponent } from './createIcon'
export { createIcon } from './createIcon'

// ── Sections ──────────────────────────────────────────────────────────────────

/** SITREP -- a HUD panel: header rail, readout bars, one data block. */
export const Sitrep = createIcon('Sitrep', <>
  <path d="M3 4h18v16H3z" />
  <path d="M3 9h18" />
  <path d="M6 13h5" />
  <path d="M6 16.5h7" />
  <path d="M14.5 12.5h4v4.5h-4z" />
</>)

/** TACMAP -- a gridded chart with one contact box. */
export const Tacmap = createIcon('Tacmap', <>
  <path d="M3 4h18v16H3z" />
  <path d="M3 12h18" />
  <path d="M12 4v16" />
  <path d="M14.5 6.5h4v3h-4z" />
</>)

/** OBJECTIVE -- the hex the campaign draws its objectives as, with a centre pip. */
export const Objective = createIcon('Objective', <>
  <path d="M12 2.5 20 7v10l-8 4.5L4 17V7z" />
  <path d="M10.5 10.5h3v3h-3z" />
</>)

/** BRIEFING -- a briefing sheet with a folded corner. */
export const Briefing = createIcon('Briefing', <>
  <path d="M4 3h11l5 5v13H4z" />
  <path d="M15 3v5h5" />
  <path d="M7.5 12h9M7.5 15.5h9M7.5 19h5" />
</>)

/** RECON INTEL -- a camera; the pip is the reticle, not a lens flare. */
export const ReconIntel = createIcon('ReconIntel', <>
  <path d="M3 7h18v13H3z" />
  <path d="M8.5 7 10 4h4l1.5 3" />
  <path d="M15.5 13.5a3.5 3.5 0 1 1-7 0 3.5 3.5 0 1 1 7 0" />
  <path d="M11.4 12.9h1.2v1.2h-1.2z" />
</>)

/** RANKINGS -- a podium, tallest in the middle. */
export const Rankings = createIcon('Rankings', <>
  <path d="M3.5 13h5v7.5h-5z" />
  <path d="M9.5 8h5v12.5h-5z" />
  <path d="M15.5 15.5h5v5h-5z" />
</>)

/** KILL FEED -- a target struck out. A plain reticle would read as "aim". */
export const KillFeed = createIcon('KillFeed', <>
  <path d="M12 4a8 8 0 1 1 0 16 8 8 0 1 1 0-16" />
  <path d="m8 8 8 8M16 8l-8 8" />
</>)

/** PILOTS -- a flight helmet, visor down. */
export const Pilot = createIcon('Pilot', <>
  <path d="M4.5 13.5a7.5 7.5 0 0 1 15 0v2.5h-15z" />
  <path d="M6.5 16h11v4.5h-11z" />
  <path d="M17.5 20.5 20.5 22" />
</>)

/** ABOUT -- the information mark. */
export const Info = createIcon('Info', <>
  <path d="M12 3.5a8.5 8.5 0 1 1 0 17 8.5 8.5 0 1 1 0-17" />
  <path d="M12 11.5v5.5" />
  <path d="M11.25 6.75h1.5v1.5h-1.5z" />
</>)

/** WIKI -- an open book. */
export const Wiki = createIcon('Wiki', <>
  <path d="M3.5 5h7.5v14H3.5z" />
  <path d="M13 5h7.5v14H13z" />
  <path d="M12 5.5v13" />
</>)

/** SUPPORT -- the donation bolt. */
export const Support = createIcon('Support', <>
  <path d="M13.5 2.5 5.5 13.5h5.5l-1 8 8-11h-5.5z" />
</>)

/** ADMIN -- a key: the panel is access, not settings. */
export const Admin = createIcon('Admin', <>
  <path d="M9.5 10.5a4 4 0 1 1 0 5 4 4 0 1 1 0-5" />
  <path d="M13.5 13H21" />
  <path d="M18 13v3.5M21 13v3" />
</>)

/** CONFIG -- slider bank. */
export const Config = createIcon('Config', <>
  <path d="M3 7h18M3 17h18" />
  <path d="M7.5 4.5h3v5h-3zM14 14.5h3v5h-3z" />
</>)

/** ALERT -- the warning triangle, cut square to match the set. */
export const Alert = createIcon('Alert', <>
  <path d="M12 3 22 20.5H2z" />
  <path d="M12 10v4.5" />
  <path d="M11.25 16.75h1.5v1.5h-1.5z" />
</>)

/** AWARD -- a medal. */
export const Award = createIcon('Award', <>
  <path d="M16.5 9a4.5 4.5 0 1 1-9 0 4.5 4.5 0 1 1 9 0" />
  <path d="M9 12.5 7.5 21l4.5-2.5 4.5 2.5-1.5-8.5" />
</>)

/** COMMS -- a radio set with its antenna. */
export const Comms = createIcon('Comms', <>
  <path d="M4 11h13v9.5H4z" />
  <path d="M14 11 20 4" />
  <path d="M7 14.5h4" />
  <path d="M13.5 14.5h1.5v3h-1.5z" />
</>)

/** PIN -- a map location. */
export const Pin = createIcon('Pin', <>
  <path d="M12 2.5c3.6 0 6.5 2.8 6.5 6.3 0 4.7-6.5 12.7-6.5 12.7S5.5 13.5 5.5 8.8C5.5 5.3 8.4 2.5 12 2.5z" />
  <path d="M10.5 7.5h3v3h-3z" />
</>)

/** SERVER -- the box the campaign runs on. */
export const Server = createIcon('Server', <>
  <path d="M3 4h18v7H3zM3 13h18v7H3z" />
  <path d="M6 7.5h1.5M6 16.5h1.5" />
  <path d="M17 7.5h2M17 16.5h2" />
</>)

/** ACTIVITY -- a trace. */
export const Activity = createIcon('Activity', <>
  <path d="M2.5 12.5h4l3-7.5 4.5 14 3-6.5h4.5" />
</>)

/** SHIELD -- coalition, plain. */
export const Shield = createIcon('Shield', <>
  <path d="M12 2.5 20 5.5v6.5c0 4.5-3.5 7.5-8 9-4.5-1.5-8-4.5-8-9V5.5z" />
</>)

/** HEADING -- a course arrow. */
export const Heading = createIcon('Heading', <>
  <path d="M12 2 20.5 21 12 16.5 3.5 21z" />
</>)

/** WIND */
export const Wind = createIcon('Wind', <>
  <path d="M2.5 8h11a3 3 0 1 0-3-3" />
  <path d="M2.5 12.5h15a3 3 0 1 1-3 3" />
  <path d="M2.5 17h8" />
</>)

/** TEMPERATURE */
export const Temp = createIcon('Temp', <>
  <path d="M10 13.5V4.5a2 2 0 0 1 4 0v9a4.5 4.5 0 1 1-4 0z" />
  <path d="M12 8.5v6" />
</>)

/** BAROMETER -- QNH. */
export const Baro = createIcon('Baro', <>
  <path d="M12 3.5a8.5 8.5 0 1 1 0 17 8.5 8.5 0 1 1 0-17" />
  <path d="M12 12 16.5 8" />
  <path d="M11.25 11.25h1.5v1.5h-1.5z" />
</>)

/** VISIBILITY -- slant range. */
export const Visibility = createIcon('Visibility', <>
  <path d="M2.5 12S6.7 6 12 6s9.5 6 9.5 6-4.2 6-9.5 6-9.5-6-9.5-6z" />
  <path d="M10.5 10.5h3v3h-3z" />
</>)

// ── Objective types ───────────────────────────────────────────────────────────

/** AIRBASE -- the runway seen on final, with the tower standing at its far
    end: centreline down the pavement, mast and cab off the upwind threshold.
    Pavement alone reads as a runway rather than a base, so the tower is what
    makes it a field. */
export const Airbase = createIcon('Airbase', <>
  <path d="M9 6h6l3.5 15h-13.5z" />
  <path d="M11 10v2M11 14v2M11 18v2" />
  <path d="M17 10V4" />
  <path d="M15 4h4V1.5h-4z" />
</>)

/** FARP -- the pad marking, on the objective hex. */
export const Farp = createIcon('Farp', <>
  <path d="M12 2.5 20 7v10l-8 4.5L4 17V7z" />
  <path d="M9.5 9v6M14.5 9v6M9.5 12h5" />
</>)

/** FOB -- a revetted bunker with its entrance. */
export const Fob = createIcon('Fob', <>
  <path d="M3.5 19.5h17" />
  <path d="M5.5 19.5V13L12 9.5l6.5 3.5v6.5" />
  <path d="M10 19.5V15h4v4.5" />
</>)

/** FACTORY -- the saw-tooth plant roofline. */
export const Factory = createIcon('Factory', <>
  <path d="M3.5 20.5V10.5l5.5 4v-4l5.5 4V6.5h5v14z" />
  <path d="M7.5 17.5h9" />
</>)

/** LOGISTICS HUB -- a warehouse with its bay door. */
export const LogiHub = createIcon('LogiHub', <>
  <path d="M2 9.5h20" />
  <path d="M3.5 20.5V9.5h17v11" />
  <path d="M8.5 20.5v-6h7v6" />
  <path d="M8.5 17.5h7" />
</>)

/** NAVAL BASE -- an anchor. */
export const NavalBase = createIcon('NavalBase', <>
  <path d="M12 6v14.5" />
  <path d="M8 10h8" />
  <path d="M13.5 4.5a1.5 1.5 0 1 1-3 0 1.5 1.5 0 1 1 3 0" />
  <path d="M4.5 14a7.5 7.5 0 0 0 15 0" />
</>)

/** CARRIER -- flight deck in plan: raked bow, angled deck, starboard island. */
export const Carrier = createIcon('Carrier', <>
  <path d="M12 3 18.5 9v12h-13V9z" />
  <path d="M16 20.5 8 9.5" />
  <path d="M15.5 11h3v4h-3z" />
</>)

/** COMMAND CENTRE -- an HQ under its comms mast. */
export const CommandCenter = createIcon('CommandCenter', <>
  <path d="M3 20.5h18" />
  <path d="M5 20.5V11h14v9.5" />
  <path d="M12 11V6" />
  <path d="M10 5.5a2.5 2.5 0 0 1 4 0" />
  <path d="M8 3.5a5.5 5.5 0 0 1 8 0" />
</>)

// ── Task kinds ────────────────────────────────────────────────────────────────

/** CAP -- a fighter holding an orbit. */
export const Cap = createIcon('Cap', <>
  <path d="M12 3 15 9H9z" />
  <path d="M3.5 16.5a8.5 4 0 1 0 17 0 8.5 4 0 1 0-17 0" />
</>)

/** CAS -- ordnance off the rails onto a ground contact. */
export const Cas = createIcon('Cas', <>
  <path d="M12 2 16.5 10h-9z" />
  <path d="M3 20.5h18" />
  <path d="M7 20.5 9.5 14M12 20.5v-7M17 20.5 14.5 14" />
</>)

/** SEAD -- a radar dish struck out: the emitter is the target, not the airframe. */
export const Sead = createIcon('Sead', <>
  <path d="M4.5 13.5a7.5 7.5 0 0 1 15 0z" />
  <path d="M12 13.5v6" />
  <path d="M8 19.5h8" />
  <path d="m4 4 16 16" />
</>)

/** STRIKE -- a weapon run onto a fixed target. */
export const Strike = createIcon('Strike', <>
  <path d="M12 2v8.5" />
  <path d="M9 7.5 12 10.5 15 7.5" />
  <path d="M12 12.5a4.5 4.5 0 1 0 0 9 4.5 4.5 0 1 0 0-9" />
  <path d="M11 16h2v2h-2z" />
</>)

/** INTERCEPT -- a vector run out to a contact. */
export const Intercept = createIcon('Intercept', <>
  <path d="M2.5 21.5 12.5 11.5" />
  <path d="M8.5 11.5h4v4" />
  <path d="M18.5 2.5 21.5 5.5l-3 3-3-3z" />
</>)

/** LOGISTICS -- a supply truck. */
export const Logistics = createIcon('Logistics', <>
  <path d="M2.5 6.5h11v10h-11z" />
  <path d="M13.5 10.5h3.5l3 3.5v2.5h-6.5z" />
  <path d="M8 18.5a1.75 1.75 0 1 1-3.5 0 1.75 1.75 0 1 1 3.5 0" />
  <path d="M19.5 18.5a1.75 1.75 0 1 1-3.5 0 1.75 1.75 0 1 1 3.5 0" />
</>)

/** RECON -- an eye inside a viewfinder. */
export const Recon = createIcon('Recon', <>
  <path d="M3 8V4h4M17 4h4v4M21 16v4h-4M7 20H3v-4" />
  <path d="M5.5 12s2.9-3.75 6.5-3.75S18.5 12 18.5 12 15.1 15.75 12 15.75 5.5 12 5.5 12z" />
  <path d="M11.25 11.25h1.5v1.5h-1.5z" />
</>)

/** CSAR -- a helicopter working a hoist. */
export const Csar = createIcon('Csar', <>
  <path d="M3 4.5h18" />
  <path d="M12 4.5v3" />
  <path d="M6 7.5h9v5.5H6z" />
  <path d="M15 9.5h6" />
  <path d="M9.5 13v4" />
  <path d="M8 17h3" />
</>)

/** CAPTURE -- a flag planted on the objective. */
export const Capture = createIcon('Capture', <>
  <path d="M6.5 3.5v17" />
  <path d="M6.5 4.5h12l-3 4 3 4h-12z" />
  <path d="M3 20.5h18" />
</>)

/** DEFEND -- hold the line. */
export const Defend = createIcon('Defend', <>
  <path d="M12 2.5 20 5.5v6.5c0 4.5-3.5 7.5-8 9-4.5-1.5-8-4.5-8-9V5.5z" />
  <path d="M8 11.5h8" />
</>)

// ── Unit and threat classes ───────────────────────────────────────────────────

/** AIRCRAFT -- fixed wing, plan view. */
export const Aircraft = createIcon('Aircraft', <>
  <path d="M12 2.5v19" />
  <path d="M3 14.5 12 10l9 4.5" />
  <path d="M8 21 12 19l4 2" />
</>)

/** HELICOPTER -- rotor disc, plan view. */
export const Helicopter = createIcon('Helicopter', <>
  <path d="m3 3 4.5 4.5M21 3 16.5 7.5M3 21l4.5-4.5M21 21l-4.5-4.5" />
  <path d="M15 12a3 3 0 1 1-6 0 3 3 0 1 1 6 0" />
  <path d="M12 15v6M10 20.5h4" />
</>)

/** SHIP -- a surface combatant, side on. Plan view is the carrier's job. */
export const Ship = createIcon('Ship', <>
  <path d="M2.5 15.5h19l-3 5H5.5z" />
  <path d="M9 15.5V11h6v4.5" />
  <path d="M12 11V6" />
</>)

/** SAM -- a launcher with its rails erected. */
export const Sam = createIcon('Sam', <>
  <path d="M2.5 16h19v4.5h-19z" />
  <path d="M6.5 16 12 4.5M10.5 16 16 4.5" />
  <path d="M12 4.5h4" />
</>)

/** AAA -- twin barrels on a low mount. */
export const Aaa = createIcon('Aaa', <>
  <path d="M5.5 20.5h13" />
  <path d="M8.5 17.5h7v3h-7z" />
  <path d="M11 17.5 14 7M13.5 17.5 16.5 7" />
</>)

/** ARTILLERY -- a gun and the arc it throws. */
export const Arty = createIcon('Arty', <>
  <path d="M6.5 13.5 17 5l2.5 3-10.5 8.5z" />
  <path d="M9.5 19a2.5 2.5 0 1 1-5 0 2.5 2.5 0 1 1 5 0" />
  <path d="M6.5 17.5 2.5 21" />
</>)

/** ARMOR -- tank, side on. */
export const Armor = createIcon('Armor', <>
  <path d="M2.5 13.5h15v4h-15z" />
  <path d="M7 13.5v-3h6v3" />
  <path d="M13 11.5h7.5" />
  <path d="M3.5 20.5h13" />
</>)

/** INFANTRY -- the NATO infantry box. Real symbology beats an invented glyph. */
export const Infantry = createIcon('Infantry', <>
  <path d="M3 6.5h18v11H3z" />
  <path d="M3 6.5 21 17.5M21 6.5 3 17.5" />
</>)

/** RADAR -- a scope with its sweep and a blip. */
export const Radar = createIcon('Radar', <>
  <path d="M12 3.5a8.5 8.5 0 1 1 0 17 8.5 8.5 0 1 1 0-17" />
  <path d="M12 12 19.4 7.75A8.5 8.5 0 0 1 20.5 12z" />
  <path d="M14.5 15.5h1.75v1.75H14.5z" />
</>)

/** SUPPLY -- a strapped crate. */
export const Supply = createIcon('Supply', <>
  <path d="M3.5 6.5h17v14h-17z" />
  <path d="M3.5 10.5h17" />
  <path d="M9 10.5v10M15 10.5v10" />
</>)

/** STRUCTURE -- a hardened building. */
export const Structure = createIcon('Structure', <>
  <path d="M4 8.5h16v12H4z" />
  <path d="M2.5 8.5 12 3.5l9.5 5" />
  <path d="M7.5 12h3v3h-3zM13.5 12h3v3h-3z" />
</>)

// ── Interface chrome ──────────────────────────────────────────────────────────
// Named exactly like their lucide counterparts so a call site swaps by changing
// the import source and nothing else.

export const ChevronUp    = createIcon('ChevronUp',    <path d="m5 15 7-7 7 7" />)
export const ChevronDown  = createIcon('ChevronDown',  <path d="m5 9 7 7 7-7" />)
export const ChevronLeft  = createIcon('ChevronLeft',  <path d="m15 5-7 7 7 7" />)
export const ChevronRight = createIcon('ChevronRight', <path d="m9 5 7 7-7 7" />)
export const ChevronsLeft  = createIcon('ChevronsLeft',  <path d="m11 5-7 7 7 7M18 5l-7 7 7 7" />)
export const ChevronsRight = createIcon('ChevronsRight', <path d="m13 5 7 7-7 7M6 5l7 7-7 7" />)
export const ChevronsDown  = createIcon('ChevronsDown',  <path d="m5 6 7 7 7-7M5 13l7 7 7-7" />)
export const ArrowUp   = createIcon('ArrowUp',   <><path d="M12 20.5V4" /><path d="m5 11 7-7 7 7" /></>)
export const ArrowDown = createIcon('ArrowDown', <><path d="M12 3.5V20" /><path d="m5 13 7 7 7-7" /></>)

export const X     = createIcon('X',     <path d="m5 5 14 14M19 5 5 19" />)
export const Plus  = createIcon('Plus',  <path d="M12 4v16M4 12h16" />)
export const Minus = createIcon('Minus', <path d="M4 12h16" />)
export const Menu  = createIcon('Menu',  <path d="M3 6h18M3 12h18M3 18h18" />)

export const Search = createIcon('Search', <>
  <path d="M16.5 10.5a6 6 0 1 1-12 0 6 6 0 1 1 12 0" />
  <path d="m15 15 5.5 5.5" />
</>)

export const Save = createIcon('Save', <>
  <path d="M4 4h12l4 4v12H4z" />
  <path d="M8.5 4h6v5h-6z" />
  <path d="M7 13h10v7H7z" />
</>)

export const Trash2 = createIcon('Trash2', <>
  <path d="M4 6.5h16" />
  <path d="M9.5 6.5v-3h5v3" />
  <path d="M6.5 6.5 7.5 20.5h9l1-14" />
  <path d="M10 10.5v6M14 10.5v6" />
</>)

export const Pencil = createIcon('Pencil', <>
  <path d="M4 20h4.5L20.5 8 16 3.5 4 15.5z" />
  <path d="m14.5 5.5 4.5 4.5" />
</>)

export const Eraser = createIcon('Eraser', <>
  <path d="M6.5 20.5h13" />
  <path d="M3.5 14.5 9.5 20.5h3.5l7.5-7.5-6-6z" />
</>)

export const Eye = createIcon('Eye', <>
  <path d="M2.5 12S6.7 6 12 6s9.5 6 9.5 6-4.2 6-9.5 6-9.5-6-9.5-6z" />
  <path d="M10.5 10.5h3v3h-3z" />
</>)
export const EyeOff = createIcon('EyeOff', <>
  <path d="M2.5 12S6.7 6 12 6s9.5 6 9.5 6-4.2 6-9.5 6-9.5-6-9.5-6z" />
  <path d="m3 3 18 18" />
</>)

export const Play   = createIcon('Play',   <path d="M7 4.5 19.5 12 7 19.5z" />)
export const Pause  = createIcon('Pause',  <path d="M7.5 4.5h3.5v15H7.5zM13 4.5h3.5v15H13z" />)
export const Square = createIcon('Square', <path d="M5.5 5.5h13v13h-13z" />)
export const Circle = createIcon('Circle', <path d="M12 3.5a8.5 8.5 0 1 1 0 17 8.5 8.5 0 1 1 0-17" />)
export const PlayCircle = createIcon('PlayCircle', <>
  <path d="M12 3.5a8.5 8.5 0 1 1 0 17 8.5 8.5 0 1 1 0-17" />
  <path d="M10 8.5 16 12l-6 3.5z" />
</>)

export const RotateCw = createIcon('RotateCw', <>
  <path d="M20.5 12a8.5 8.5 0 1 1-2.6-6.1" />
  <path d="M20.5 3.5V9.5H14.5" />
</>)
export const RotateCcw = createIcon('RotateCcw', <>
  <path d="M3.5 12a8.5 8.5 0 1 0 2.6-6.1" />
  <path d="M3.5 3.5V9.5H9.5" />
</>)
export const RefreshCw = createIcon('RefreshCw', <>
  <path d="M20.5 11a8.5 8.5 0 0 0-14.4-4.4L3.5 9" />
  <path d="M3.5 13a8.5 8.5 0 0 0 14.4 4.4L20.5 15" />
  <path d="M3.5 4v5h5M20.5 20v-5h-5" />
</>)

export const Download = createIcon('Download', <>
  <path d="M12 3v11.5" />
  <path d="m7 10 5 5 5-5" />
  <path d="M4 20.5h16" />
</>)
export const Upload = createIcon('Upload', <>
  <path d="M12 20.5V9" />
  <path d="m7 13 5-5 5 5" />
  <path d="M4 3.5h16" />
</>)

export const ExternalLink = createIcon('ExternalLink', <>
  <path d="M13.5 3.5h7v7" />
  <path d="M20.5 3.5 11 13" />
  <path d="M17.5 13.5v7h-14v-14h7" />
</>)

export const Link = createIcon('Link', <>
  <path d="m9.5 14.5 5-5" />
  <path d="M11 6.5 13.5 4a4.6 4.6 0 0 1 6.5 6.5L17.5 13" />
  <path d="M13 17.5 10.5 20a4.6 4.6 0 0 1-6.5-6.5L6.5 11" />
</>)

export const Clock = createIcon('Clock', <>
  <path d="M12 3.5a8.5 8.5 0 1 1 0 17 8.5 8.5 0 1 1 0-17" />
  <path d="M12 6.5V12l4 2.5" />
</>)

export const LogOut = createIcon('LogOut', <>
  <path d="M9.5 4.5H4v15h5.5" />
  <path d="m15 8 4 4-4 4" />
  <path d="M19 12H8.5" />
</>)

export const Moon = createIcon('Moon', <path d="M20.5 14.8A8.6 8.6 0 0 1 9.2 3.5a8.6 8.6 0 1 0 11.3 11.3z" />)
export const Sun = createIcon('Sun', <>
  <path d="M16 12a4 4 0 1 1-8 0 4 4 0 1 1 8 0" />
  <path d="M12 2v2.5M12 19.5V22M2 12h2.5M19.5 12H22" />
  <path d="m5 5 1.8 1.8M17.2 17.2 19 19M19 5l-1.8 1.8M6.8 17.2 5 19" />
</>)

export const Ban = createIcon('Ban', <>
  <path d="M12 3.5a8.5 8.5 0 1 1 0 17 8.5 8.5 0 1 1 0-17" />
  <path d="m6 6 12 12" />
</>)

export const UserX = createIcon('UserX', <>
  <path d="M13 8a3.5 3.5 0 1 1-7 0 3.5 3.5 0 1 1 7 0" />
  <path d="M2.5 20.5v-1.8c0-2.6 2.1-4.7 4.7-4.7h1.6c2.6 0 4.7 2.1 4.7 4.7v1.8" />
  <path d="m16.5 8.5 5 5M21.5 8.5l-5 5" />
</>)

export const Terminal = createIcon('Terminal', <>
  <path d="M3 4.5h18v15H3z" />
  <path d="m7 9 3.5 3.5L7 16" />
  <path d="M12.5 16h5" />
</>)

export const CheckCircle2 = createIcon('CheckCircle2', <>
  <path d="M12 3.5a8.5 8.5 0 1 1 0 17 8.5 8.5 0 1 1 0-17" />
  <path d="m7.5 12 3 3 6-6" />
</>)

export const FolderCog = createIcon('FolderCog', <>
  <path d="M3 5.5h6l2 2.5h10v11H3z" />
  <path d="M15 13.5a2 2 0 1 1-4 0 2 2 0 1 1 4 0" />
  <path d="M13 10v1.5M13 16v1.5M9.8 13.5h1.5M14.7 13.5h1.5" />
</>)

export const TrendingUp = createIcon('TrendingUp', <>
  <path d="m3 17 6.5-6.5 4 4L21 7" />
  <path d="M15 7h6v6" />
</>)

export const Crosshair = createIcon('Crosshair', <>
  <path d="M12 4.5a7.5 7.5 0 1 1 0 15 7.5 7.5 0 1 1 0-15" />
  <path d="M12 1.5v4M12 18.5v4M1.5 12h4M18.5 12h4" />
</>)

export const Grid3x3 = createIcon('Grid3x3', <>
  <path d="M3 3h18v18H3z" />
  <path d="M9 3v18M15 3v18M3 9h18M3 15h18" />
</>)

export const Maximize2 = createIcon('Maximize2', <>
  <path d="M14 3.5h6.5V10M20.5 3.5 14 10" />
  <path d="M10 20.5H3.5V14M3.5 20.5 10 14" />
</>)

export const MousePointer2 = createIcon('MousePointer2', <>
  <path d="m5 3 13.5 7.5-6 1.5-2 6z" />
</>)

export const Move3d = createIcon('Move3d', <>
  <path d="M12 2.5v19M2.5 12h19" />
  <path d="m8.5 6 3.5-3.5L15.5 6M8.5 18l3.5 3.5L15.5 18" />
  <path d="m6 8.5-3.5 3.5L6 15.5M18 8.5l3.5 3.5L18 15.5" />
</>)

export const SendToBack = createIcon('SendToBack', <>
  <path d="M3.5 3.5h10v10h-10z" />
  <path d="M10.5 10.5h10v10h-10z" />
</>)

export const Headphones = createIcon('Headphones', <>
  <path d="M4 15.5v-3.5a8 8 0 0 1 16 0v3.5" />
  <path d="M2.5 14.5h4v6h-4zM17.5 14.5h4v6h-4z" />
</>)

/** COINS -- the campaign economy. */
export const Coins = createIcon('Coins', <>
  <path d="M14 8a5.5 3 0 1 1-11 0 5.5 3 0 1 1 11 0" />
  <path d="M3 8v4c0 1.7 2.5 3 5.5 3s5.5-1.3 5.5-3V8" />
  <path d="M10 16.2c.6 1.6 2.8 2.8 5.5 2.8 3 0 5.5-1.3 5.5-3v-4" />
  <path d="M21 12c0 1.7-2.5 3-5.5 3-1.4 0-2.7-.3-3.7-.8" />
  <path d="M15.5 9c3 0 5.5 1.3 5.5 3" />
</>)

/** HEALTH -- lives and combat rules. */
export const Health = createIcon('Health', <>
  <path d="M2.5 12h4l2-4 3.5 8 2.5-5 2 3h5" />
</>)

/** CLOUD -- weather. */
export const Cloud = createIcon('Cloud', <>
  <path d="M6.5 19h11a4.5 4.5 0 0 0 .4-9 6.5 6.5 0 0 0-12.4 1.6A4 4 0 0 0 6.5 19z" />
</>)

// ── Rank badges ───────────────────────────────────────────────────────────────
// Service stripes: three for first place down to one for third. The count
// carries the ordering, so the podium still reads correctly to anyone who
// can't separate the gold/silver/bronze tints.

export const Rank1 = createIcon('Rank1', <>
  <path d="M4 9.5 12 4.5l8 5" />
  <path d="M4 14.5 12 9.5l8 5" />
  <path d="M4 19.5 12 14.5l8 5" />
</>)

export const Rank2 = createIcon('Rank2', <>
  <path d="M4 12 12 7l8 5" />
  <path d="M4 17 12 12l8 5" />
</>)

export const Rank3 = createIcon('Rank3', <path d="M4 14.5 12 9.5l8 5" />)

/** Badge for a podium position, 0-indexed. */
export const RANK_ICON = [Rank1, Rank2, Rank3]

// ── Rank insignia ─────────────────────────────────────────────────────────────
// Two ladders, because this is a NATO-vs-Russia campaign and a pilot should not
// wear the other side's rank. Eight tiers each, drawn on a shared shoulder board
// so the two families read as one system.
//
// The NATO side abstracts above Captain on purpose: a major's oak leaf and a
// colonel's eagle are unreadable at the 16px table size, so company grade is
// bars, field grade is stars, and general officers are stars over a bar. The
// VKS side is literal -- star-and-stripe boards scale down cleanly as drawn.
//
// Star paths are written out rather than generated by a helper: scripts/
// icon-proof.mjs reads this file statically, and an icon built from an
// expression renders blank on the contact sheet instead of failing loudly.

/** NATO O-1 — one bar. */
export const NatoO1 = createIcon('NatoO1', <>
  <path d="M4 3.5h16v17H4z" />
  <path d="M7.5 11h9" />
</>)

/** NATO O-2 — two bars. */
export const NatoO2 = createIcon('NatoO2', <>
  <path d="M4 3.5h16v17H4z" />
  <path d="M7.5 9h9M7.5 13h9" />
</>)

/** NATO O-3 — three bars. */
export const NatoO3 = createIcon('NatoO3', <>
  <path d="M4 3.5h16v17H4z" />
  <path d="M7.5 7.5h9M7.5 11.5h9M7.5 15.5h9" />
</>)

/** NATO O-4 — one star. */
export const NatoO4 = createIcon('NatoO4', <>
  <path d="M4 3.5h16v17H4z" />
  <path d="M12.0 7.6L13.2 10.4L16.2 10.6L13.9 12.6L14.6 15.6L12.0 14.0L9.4 15.6L10.1 12.6L7.8 10.6L10.8 10.4z" />
</>)

/** NATO O-5 — two stars. */
export const NatoO5 = createIcon('NatoO5', <>
  <path d="M4 3.5h16v17H4z" />
  <path d="M9.0 8.8L9.8 10.8L12.0 11.0L10.4 12.4L10.9 14.6L9.0 13.4L7.1 14.6L7.6 12.4L6.0 11.0L8.2 10.8zM15.0 8.8L15.8 10.8L18.0 11.0L16.4 12.4L16.9 14.6L15.0 13.4L13.1 14.6L13.6 12.4L12.0 11.0L14.2 10.8z" />
</>)

/** NATO O-6 — three stars. */
export const NatoO6 = createIcon('NatoO6', <>
  <path d="M4 3.5h16v17H4z" />
  <path d="M7.8 9.4L8.5 11.1L10.3 11.2L8.9 12.4L9.3 14.1L7.8 13.2L6.3 14.1L6.7 12.4L5.3 11.2L7.1 11.1zM12.0 9.4L12.7 11.1L14.5 11.2L13.1 12.4L13.5 14.1L12.0 13.2L10.5 14.1L10.9 12.4L9.5 11.2L11.3 11.1zM16.2 9.4L16.9 11.1L18.7 11.2L17.3 12.4L17.7 14.1L16.2 13.2L14.7 14.1L15.1 12.4L13.7 11.2L15.5 11.1z" />
</>)

/** NATO O-7 — one star over a bar. */
export const NatoO7 = createIcon('NatoO7', <>
  <path d="M4 3.5h16v17H4z" />
  <path d="M12.0 6.3L12.9 8.5L15.3 8.7L13.5 10.3L14.1 12.6L12.0 11.4L9.9 12.6L10.5 10.3L8.7 8.7L11.1 8.5z" />
  <path d="M7.5 17.5h9" />
</>)

/** NATO O-8 — two stars over a bar. */
export const NatoO8 = createIcon('NatoO8', <>
  <path d="M4 3.5h16v17H4z" />
  <path d="M9.2 7.1L9.9 8.8L11.8 9.0L10.4 10.2L10.8 12.0L9.2 11.0L7.6 12.0L8.0 10.2L6.6 9.0L8.5 8.8zM14.8 7.1L15.5 8.8L17.4 9.0L16.0 10.2L16.4 12.0L14.8 11.0L13.2 12.0L13.6 10.2L12.2 9.0L14.1 8.8z" />
  <path d="M7.5 17.5h9" />
</>)

/** VKS O-1 — two stars. */
export const VksO1 = createIcon('VksO1', <>
  <path d="M4 8 12 3.5 20 8v12.5H4z" />
  <path d="M9.4 11.8L10.1 13.5L11.9 13.6L10.5 14.8L10.9 16.5L9.4 15.6L7.9 16.5L8.3 14.8L6.9 13.6L8.7 13.5zM14.6 11.8L15.3 13.5L17.1 13.6L15.7 14.8L16.1 16.5L14.6 15.6L13.1 16.5L13.5 14.8L12.1 13.6L13.9 13.5z" />
</>)

/** VKS O-2 — three stars. */
export const VksO2 = createIcon('VksO2', <>
  <path d="M4 8 12 3.5 20 8v12.5H4z" />
  <path d="M7.9 12.1L8.5 13.6L10.1 13.7L8.9 14.7L9.3 16.3L7.9 15.4L6.5 16.3L6.9 14.7L5.7 13.7L7.3 13.6zM12.0 12.1L12.6 13.6L14.2 13.7L13.0 14.7L13.4 16.3L12.0 15.4L10.6 16.3L11.0 14.7L9.8 13.7L11.4 13.6zM16.1 12.1L16.7 13.6L18.3 13.7L17.1 14.7L17.5 16.3L16.1 15.4L14.7 16.3L15.1 14.7L13.9 13.7L15.5 13.6z" />
</>)

/** VKS O-3 — four stars. */
export const VksO3 = createIcon('VksO3', <>
  <path d="M4 8 12 3.5 20 8v12.5H4z" />
  <path d="M9.3 9.8L9.9 11.1L11.3 11.3L10.2 12.2L10.5 13.6L9.3 12.8L8.1 13.6L8.4 12.2L7.3 11.3L8.7 11.1zM14.7 9.8L15.3 11.1L16.7 11.3L15.6 12.2L15.9 13.6L14.7 12.8L13.5 13.6L13.8 12.2L12.7 11.3L14.1 11.1zM9.3 15.0L9.9 16.3L11.3 16.5L10.2 17.4L10.5 18.8L9.3 18.0L8.1 18.8L8.4 17.4L7.3 16.5L8.7 16.3zM14.7 15.0L15.3 16.3L16.7 16.5L15.6 17.4L15.9 18.8L14.7 18.0L13.5 18.8L13.8 17.4L12.7 16.5L14.1 16.3z" />
</>)

/** VKS O-4 — stripes and one large star. */
export const VksO4 = createIcon('VksO4', <>
  <path d="M4 8 12 3.5 20 8v12.5H4z" />
  <path d="M7.2 9.4v11.1M16.8 9.4v11.1" />
  <path d="M12.0 11.2L12.9 13.4L15.2 13.5L13.5 15.1L14.0 17.4L12.0 16.1L10.0 17.4L10.5 15.1L8.8 13.5L11.1 13.4z" />
</>)

/** VKS O-5 — stripes and two large stars. */
export const VksO5 = createIcon('VksO5', <>
  <path d="M4 8 12 3.5 20 8v12.5H4z" />
  <path d="M7.2 9.4v11.1M16.8 9.4v11.1" />
  <path d="M12.0 9.1L12.7 10.8L14.5 10.9L13.1 12.1L13.5 13.8L12.0 12.9L10.5 13.8L10.9 12.1L9.5 10.9L11.3 10.8zM12.0 14.8L12.7 16.5L14.5 16.6L13.1 17.8L13.5 19.5L12.0 18.6L10.5 19.5L10.9 17.8L9.5 16.6L11.3 16.5z" />
</>)

/** VKS O-6 — stripes and three large stars. */
export const VksO6 = createIcon('VksO6', <>
  <path d="M4 8 12 3.5 20 8v12.5H4z" />
  <path d="M7.2 9.4v11.1M16.8 9.4v11.1" />
  <path d="M12.0 8.4L12.6 9.8L14.1 9.9L12.9 10.9L13.3 12.4L12.0 11.6L10.7 12.4L11.1 10.9L9.9 9.9L11.4 9.8zM12.0 12.4L12.6 13.8L14.1 13.9L12.9 14.9L13.3 16.4L12.0 15.6L10.7 16.4L11.1 14.9L9.9 13.9L11.4 13.8zM12.0 16.4L12.6 17.8L14.1 17.9L12.9 18.9L13.3 20.4L12.0 19.6L10.7 20.4L11.1 18.9L9.9 17.9L11.4 17.8z" />
</>)

/** VKS general — one large star on a plain board. */
export const VksO7 = createIcon('VksO7', <>
  <path d="M4 8 12 3.5 20 8v12.5H4z" />
  <path d="M12.0 10.0L13.2 12.8L16.2 13.0L13.9 15.0L14.6 18.0L12.0 16.4L9.4 18.0L10.1 15.0L7.8 13.0L10.8 12.8z" />
</>)

/** VKS general — two large stars on a plain board. */
export const VksO8 = createIcon('VksO8', <>
  <path d="M4 8 12 3.5 20 8v12.5H4z" />
  <path d="M12.0 8.4L12.8 10.4L14.9 10.5L13.3 11.9L13.8 14.0L12.0 12.9L10.2 14.0L10.7 11.9L9.1 10.5L11.2 10.4zM12.0 14.5L12.8 16.5L14.9 16.6L13.3 18.0L13.8 20.1L12.0 19.0L10.2 20.1L10.7 18.0L9.1 16.6L11.2 16.5z" />
</>)
