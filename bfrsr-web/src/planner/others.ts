// Cartridges outside DCS's Mission Editor DTC system: each module reads its
// own user file from Saved Games. Formats from the modules' shipped samples
// and manuals (the parsers are inside the module DLLs).

import { terrainAt } from './nav'
import { ExportError, safeName } from './exports'
import type { Plan } from './types'

const FT = 0.3048

export interface OtherExport {
  filename: string
  text: string
  /** Where the pilot puts it. */
  folder: string
  warnings: string[]
}

/** Lua string literal, ASCII only. */
function luaStr(s: string): string {
  return `"${s.replace(/[^\x20-\x7e]/g, '').replace(/["\\]/g, '')}"`
}

/** "N41:55.646" -- hemisphere, unpadded degrees, decimal minutes. */
export function m2kCoord(deg: number, isLat: boolean): string {
  const h = isLat ? (deg >= 0 ? 'N' : 'S') : (deg >= 0 ? 'E' : 'W')
  const a = Math.abs(deg)
  let d = Math.floor(a)
  let m = Math.round((a - d) * 60 * 1000) / 1000
  if (m >= 60) { d += 1; m -= 60 }
  return `${h}${d}:${m.toFixed(3).padStart(6, '0')}`
}

function mapOf(plan: Plan) {
  const w = plan.waypoints
  if (!w.length) throw new ExportError('The plan has no waypoints.')
  const map = terrainAt(w[0].lat, w[0].lon)
  if (!map) throw new ExportError('The route is not on a DCS map this planner knows.')
  if (w.some(p => terrainAt(p.lat, p.lon)?.id !== map.id)) throw new ExportError(`The route leaves the ${map.terrain.name} map.`)
  return map
}

/**
 * M-2000C MIP data cartridge: Lua in Saved Games\DCS\Datacartridges. Up to
 * 20 BUTs; 10 or fewer replace BUT 11-20 and keep the mission's 01-10.
 */
export function mirageDtc(plan: Plan, today = new Date()): OtherExport {
  const map = mapOf(plan)
  const warnings: string[] = []
  let w = plan.waypoints
  if (w.length > 20) {
    warnings.push('The Mirage cartridge holds 20 BUTs; the rest were left out.')
    w = w.slice(0, 20)
  }
  warnings.push(w.length <= 10
    ? `Your route loads as BUT 11-${10 + w.length}; BUT 01-10 stay the mission's.`
    : `Your route replaces BUT 01-${w.length}.`)
  const name = safeName(plan.name).toUpperCase().slice(0, 16)
  const dd = (n: number) => String(n).padStart(2, '0')
  const lines = [
    `-- ${name}: Southern Flank 1985 planner. Copy to Saved Games\\DCS\\Datacartridges\\ (create the folder).`,
    `terrain = ${luaStr(map.id)}`,
    'aircraft = "M-2000C"',
    `date = "${dd(today.getDate())}/${dd(today.getMonth() + 1)}/${today.getFullYear()}"`,
    `name = ${luaStr(name)}`,
    '',
    'waypoints = {}',
    ...w.map((p, i) => {
      const nm = (p.name || `WP${i + 1}`).toUpperCase().slice(0, 12)
      return `waypoints[${i + 1}] = { name = ${luaStr(nm)}, lat = "${m2kCoord(p.lat, true)}", lon = "${m2kCoord(p.lon, false)}", alt = ${Math.round(p.alt_ft * FT)} }`
    }),
    '',
  ]
  return { filename: `${name}.dtc`, text: lines.join('\n'), folder: 'Saved Games\\DCS\\Datacartridges', warnings }
}

/**
 * AJS37 Viggen custom cartridge: Saved Games\DCS_AJS37\CustomCartridge.ini
 * (beside the DCS folder, not in it). LS = departure, B1-B9 = the route,
 * L1 = arrival. A target B point becomes a target point (Målpunkt).
 */
export function viggenIni(plan: Plan, elapsedAt: (i: number) => number | null): OtherExport {
  mapOf(plan)
  const warnings: string[] = []
  const w = plan.waypoints
  const ls = w[0]
  const l1 = w.length > 1 ? w[w.length - 1] : undefined
  let mid = w.slice(1, w.length > 1 ? -1 : undefined)
  if (mid.length > 9) {
    warnings.push('The Viggen holds nine route points (B1-B9); the rest were left out.')
    mid = mid.slice(0, 9)
  }
  if (w.some(p => p.lat < 0 || p.lon < 0)) warnings.push('South/west positions are written as negative degrees; check once in game.')
  const deg = (v: number) => v.toFixed(6)
  const pt = (sec: string, p: { lat: number; lon: number }, extra: string[] = []) => [
    `[${sec}]`, `latitude = ${deg(p.lat)}`, 'latitudeminutes = 0', 'latitudeseconds = 0',
    `longitude = ${deg(p.lon)}`, 'longitudeminutes = 0', 'longitudeseconds = 0', ...extra, '',
  ]
  const info = [plan.callsign, plan.notes.split('\n')[0]].filter(Boolean).join(' | ').replace(/[;=\r\n]/g, ' ').slice(0, 120)
  const lines = [
    `; ${safeName(plan.name)}: Southern Flank 1985 planner. Place in %USERPROFILE%\\Saved Games\\DCS_AJS37\\`,
    `cartridgename = ${safeName(plan.name).replace(/[;=]/g, '').slice(0, 30)}`,
    `cartridgeinfo = ${info}`,
    '',
    ...pt('LS', ls),
    ...mid.flatMap((p, k) => {
      const i = k + 1
      // A target with a planned time: lock the ETA (seconds from mission start).
      const t = p.target ? elapsedAt(i) : null
      return pt(`B${k + 1}`, p, [
        `missiontime = ${t != null ? Math.round(t) : 0}`,
        'velocity = 0',
        `etalocked = ${t != null}`,
        'velocitylocked = false',
        `istargetpoint = ${p.target}`,
      ])
    }),
    ...(l1 ? pt('L1', l1) : []),
  ]
  if (mid.some(p => p.target) && w.length && elapsedAt(1) == null) warnings.push('Set a start time to lock the target ETA (TOT).')
  return { filename: 'CustomCartridge.ini', text: lines.join('\r\n'), folder: 'Saved Games\\DCS_AJS37', warnings }
}

/**
 * JF-17: no user file carries a route (it comes from the mission). What a
 * pilot can load is the route drawn on the HSD as an air-space line, and the
 * plan's radio presets as user presets 22+ (1-21 come from the mission).
 */
export function thunderAirspace(plan: Plan, side: 'Blue' | 'Red'): OtherExport {
  const map = mapOf(plan)
  const key = side === 'Red' ? 'RKYA' : 'BKYA'
  const pts = plan.waypoints.slice(0, 30)
  const lines = [
    `-- ${safeName(plan.name)}: Southern Flank 1985 planner. DTC Config > Air Space = Preset, then Update DTC.`,
    'airspace = {}',
    '',
    `airspace['${key}'] = {`,
    "    ['closed'] = false,",
    "    ['points'] = {",
    ...pts.map((p, i) => `        [${i + 1}] = { lat = ${p.lat.toFixed(6)}, lon = ${p.lon.toFixed(6)} },`),
    '    },',
    '}',
    '',
  ]
  const warnings = ['The JF-17 route itself comes from the mission; this draws your planned route on the HSD.']
  if (plan.waypoints.length < 2) throw new ExportError('A route line needs two waypoints.')
  if (plan.waypoints.length > 30) warnings.push('Only the first 30 points were drawn.')
  return {
    filename: `Custom_AirSpace_${map.id}.lua`,
    text: lines.join('\n'),
    folder: 'Saved Games\\DCS\\Mods\\aircraft\\JF-17\\Customization\\airspace',
    warnings,
  }
}

export function thunderRadio(plan: Plan): OtherExport {
  // COMM1 preset n -> 21+n, COMM2 preset n -> 41+n: clear of the ME's 1-21.
  const rows = [
    ...plan.comm1.map(c => [21 + c.preset, c] as const),
    ...plan.comm2.map(c => [41 + c.preset, c] as const),
  ].sort((a, b) => a[0] - b[0])
  if (!rows.length) throw new ExportError('Add radio presets to the plan first.')
  const lines = [
    `-- ${safeName(plan.name)}: Southern Flank 1985 planner. Presets 1-21 are the mission's.`,
    `-- COMM1 preset n is ${22}+, COMM2 preset n is ${42}+ (n-1).`,
    ...rows.map(([n, c]) => `presets[${n}] = ${Math.round(c.freq_mhz * 1e6)}${c.label ? ` -- ${c.label.replace(/[^\x20-\x7e]/g, '')}` : ''}`),
    'return presets',
    '',
  ]
  return {
    filename: 'Custom_Radio.lua',
    text: lines.join('\n'),
    folder: 'Saved Games\\DCS\\Mods\\aircraft\\JF-17\\Customization\\radio',
    warnings: [],
  }
}
