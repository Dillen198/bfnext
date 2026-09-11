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
