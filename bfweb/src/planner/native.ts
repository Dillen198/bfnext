// Native DCS Data Transfer Cartridges, every section. A cartridge is
// {name, type, data}; `data` is the aircraft descriptor's returnData() -- we
// ship it as the target's defaults -- with the plan's overrides applied and
// the nav pages filled from the route.
//
// Rules every DCS descriptor shares (checked by running them under DCS's own
// Lua, see the research notes):
// * `data.type` must be the unit type or the import silently does nothing;
// * nav data only loads when `data.terrain` is the mission's map;
// * each tab has a `mirror_*` / "do not upload" flag that defaults to on, so
//   a tab with content must have it switched off or it never reaches the jet;
// * point lists are JSON arrays, never objects with "1" keys.

import { applyOverrides, clone, getAt, fromPointer, pruneOverrides, setAt, type Json, type JsonObject, type Overrides } from './model'
import { fromDcsXY, hornetSequences, offsetOnGrid, terrainAt, toDcsXY, type DcsTerrain } from './nav'
import { aircraftDef, type NativeType, type Plan } from './types'
import { ExportError, safeName } from './exports'
import { lar, larCentre } from './f14lar'

const FT = 0.3048
const KTS_TO_KMH = 1.852

/** A tab and the flag that keeps it out of the jet. */
export interface Tab {
  path: string
  flag: string
  /** The flag value that loads the tab: false for mirror_* ("do not
   *  upload"), true for the Apache's isEnabled. */
  load?: boolean
}

/** Per aircraft: tabs with their "do not upload" flags. */
export const TABS: Partial<Record<NativeType, Tab[]>> = {
  'F-16C_50': [
    { path: '/MPD/NAV_PTS', flag: '/MPD/mirror_NAV_PTS' },
    { path: '/MPD/VIPVRP', flag: '/MPD/mirror_NAV_PTS' },
    { path: '/MPD/DEST', flag: '/MPD/mirror_DEST' },
    { path: '/MPD/GEO_LINES', flag: '/MPD/mirror_GEO_LINES' },
    { path: '/MPD/THREAT_PTS', flag: '/MPD/mirror_THREAT_PTS' },
    { path: '/COMM/COMM1', flag: '/COMM/mirror_COMM1' },
    { path: '/COMM/COMM2', flag: '/COMM/mirror_COMM2' },
  ],
  'FA-18C_hornet': [
    { path: '/WYPT/NAV_PTS', flag: '/WYPT/mirror_NAV_PTS' },
    { path: '/WYPT/NAV_ROUTE', flag: '/WYPT/mirror_NAV_PTS' },
    { path: '/COMM/COMM1', flag: '/COMM/mirror_COMM1' },
    { path: '/COMM/COMM2', flag: '/COMM/mirror_COMM2' },
    { path: '/SA/MEZ_THRTS', flag: '/SA/mirror_MEZ_THRTS' },
  ],
  'AH-64D_BLK_II': [
    ...(['Mission_1', 'Mission_2'] as const).flatMap(m => [
      { path: `/NAV/${m}/Points/WPTHZ`, flag: `/NAV/${m}/Points/WPTHZ/isEnabled`, load: true },
      { path: `/NAV/${m}/Points/CTRLM`, flag: `/NAV/${m}/Points/CTRLM/isEnabled`, load: true },
      { path: `/NAV/${m}/Points/TGT`, flag: `/NAV/${m}/Points/TGT/isEnabled`, load: true },
      { path: `/NAV/${m}/ADF`, flag: `/NAV/${m}/ADF/isEnabled`, load: true },
    ]),
    { path: '/MISC/ASE', flag: '/MISC/ASE/isEnabled', load: true },
  ],
  // Program 1 only: the exporter fills it; programs 2-3 load with it.
  'MiG-29 Fulcrum': [
    { path: '/Program_1/Points', flag: '/mirror_NAV' },
    { path: '/Program_2', flag: '/mirror_NAV' },
    { path: '/Program_3', flag: '/mirror_NAV' },
    ...(['Program_1', 'Program_2', 'Program_3'] as const).flatMap(p => [
      { path: `/${p}/Radio`, flag: '/mirror_Radio' },
      { path: `/${p}/ADF`, flag: '/mirror_ADF' },
    ]),
  ],
}

export interface NativeExport {
  filename: string
  text: string
  terrain: string
  warnings: string[]
}

export interface RouteFill {
  data: JsonObject
  plan: Plan
  terrain: string
  xy: { x: number; y: number }[]
  /** The map the route is on, for converting positions back to lat/lon. */
  grid: DcsTerrain
  warnings: string[]
  /** Paths (JSON Pointers) the route filled; their tabs get switched on. */
  filled: string[]
  /** Mission seconds from Zulu midnight at each waypoint, if timed. */
  tos: (i: number) => number | null
}

const name = (s: string, i: number) => (s.trim() || `WP${i + 1}`).toUpperCase()

/** F-16C: steerpoints 1..25 (25 is the bullseye by convention), route 1. */
function f16Route(r: RouteFill) {
  const w = r.plan.waypoints
  const speed = r.plan.speed_kts * KTS_TO_KMH
  if (w.length > 24) r.warnings.push('F-16 steerpoint 25 is the bullseye by convention and AUTO sequencing only runs 1-20.')
  setAt(r.data, ['MPD', 'NAV_PTS'], w.map((p, i) => {
    const n = i + 1
    const t = r.tos(i)
    return {
      number: n, id: `STPT${n}`,
      idOA1: `OA1${n}`, idOA2: `OA2${n}`, idOA1_Line: `OA1${n}Line`, idOA2_Line: `OA2${n}Line`,
      x: r.xy[i].x, y: r.xy[i].y,
      // The DED shows `alt` as the steerpoint ELEV; routeAltitude is planning only.
      alt: p.elev_ft * FT, routeAltitude: p.alt_ft * FT, speed,
      note: name(p.name, i).slice(0, 30),
      R1: true, R2: false, R3: false,
      // TOS is seconds from mission-day 00:00 (Zulu); unfixed points are
      // recomputed by DCS from the mission start and the leg speeds.
      TOS: t ?? -1, isTOSEnabled: true, FIX_Time: t != null,
      altitudeType: 1, velocityType: 3,
      type: p.target ? 'TGT' : 'STPT',
      isOAP_1: false, isOAP_2: false,
      OAP_1_Range: 0, OAP_2_Range: 0, OAP_1_Bearing: 0, OAP_2_Bearing: 0,
      OAP_1_X: 0, OAP_2_X: 0, OAP_1_Y: 0, OAP_2_Y: 0, OAP_1_Alt: 0, OAP_2_Alt: 0,
      OAP_1_DeltaX: 0, OAP_2_DeltaX: 0, OAP_1_DeltaY: 0, OAP_2_DeltaY: 0,
    } as JsonObject
  }))
  setAt(r.data, ['MPD', 'terrain'], r.terrain)
  r.filled.push('/MPD/NAV_PTS')

  // Offset aimpoints: absolute position, delta = steerpoint - OAP, grid
  // bearing in radians, metres (the descriptor's own conventions).
  const pts = getAt(r.data, ['MPD', 'NAV_PTS']) as JsonObject[]
  w.forEach((p, i) => {
    ;(p.oa ?? []).slice(0, 2).forEach((o0, k) => {
      const o = capRange(o0, 99999, `OA${k + 1} of steerpoint ${i + 1}`, r.warnings)
      const g = offsetOnGrid(p.lat, p.lon, o, r.grid.terrain)
      const n = k + 1
      Object.assign(pts[i], {
        [`isOAP_${n}`]: true,
        [`OAP_${n}_X`]: g.x, [`OAP_${n}_Y`]: g.y,
        [`OAP_${n}_DeltaX`]: g.deltaX, [`OAP_${n}_DeltaY`]: g.deltaY,
        [`OAP_${n}_Range`]: g.range, [`OAP_${n}_Bearing`]: g.bearingRad,
        [`OAP_${n}_Alt`]: o.elev_ft * FT,
      })
    })
  })
  // VIP / VRP: the first steerpoint marked as each.
  const vipvrp = { ...((getAt(r.data, ['MPD', 'VIPVRP']) as JsonObject) ?? {}) }
  const group = (anchor: typeof w[number], o0: { range_ft: number; bearing_deg: number; elev_ft: number }, g: string) => {
    const o = capRange(o0, 486090, g, r.warnings)
    const q = offsetOnGrid(anchor.lat, anchor.lon, o, r.grid.terrain)
    Object.assign(vipvrp, {
      [`${g}_id`]: g, [`${g}_LineName`]: `${g}_Line`,
      [`${g}_X`]: q.x, [`${g}_Y`]: q.y, [`${g}_DeltaX`]: q.deltaX, [`${g}_DeltaY`]: q.deltaY,
      [`${g}_Bearing`]: q.bearingRad, [`${g}_Range`]: q.range, [`${g}_Elevation`]: o.elev_ft * FT,
    })
  }
  const vi = w.findIndex(p => p.vip)
  if (vi >= 0) {
    vipvrp.VIP_Number = vi + 1
    group(w[vi], w[vi].vip!.point, 'VIPTOTGT')
    group(w[vi], w[vi].vip!.pup, 'VIPTOPUP')
    vipvrp.VIP_Enabled = true
    vipvrp.VIPPUP_Enabled = true
  }
  const ri = w.findIndex(p => p.vrp)
  if (ri >= 0) {
    vipvrp.VRP_Number = ri + 1
    group(w[ri], w[ri].vrp!.point, 'TGTTOVRP')
    group(w[ri], w[ri].vrp!.pup, 'TGTTOPUP')
    vipvrp.VRP_Enabled = true
    vipvrp.VRPPUP_Enabled = true
  }
  if (vi >= 0 || ri >= 0) {
    setAt(r.data, ['MPD', 'VIPVRP'], vipvrp)
    if (vi >= 0 && vi === ri) r.warnings.push('The F-16 guide recommends different steerpoints for the VIP and the VRP target.')
  }
  // Plan radios: COMM is replaced wholesale on import, so these overlay the
  // full 20-channel tables already in `data`. Modulation 1 = AM, 2 = FM.
  if (r.plan.include_comms) {
    for (const [radio, list] of [['COMM1', r.plan.comm1], ['COMM2', r.plan.comm2]] as const) {
      for (const c of list) {
        setAt(r.data, ['COMM', radio, `Channel_${c.preset}`], { freq: c.freq_mhz, modulation: c.modulation === 'FM' ? 2 : 1 })
      }
      if (list.length) r.filled.push(`/COMM/${radio}`)
    }
    if (r.plan.comm1.some(c => c.freq_mhz < 225 || c.freq_mhz >= 400)) {
      r.warnings.push('COMM1 on the F-16 is UHF (225-399.975 MHz); presets outside that band will not tune.')
    }
  }
}

/** Hornet preset names: five letters or digits, upper case. */
const hornetName = (label: string) => label.toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 5)

/** F/A-18C: waypoints 1..59, all in sequence 1 with its route plan. */
function hornetRoute(r: RouteFill) {
  const w = r.plan.waypoints
  const speed = r.plan.speed_kts * KTS_TO_KMH
  // A Hornet sequence takes 15 points; a longer route runs on through SEQ 2
  // and 3, each starting at the previous sequence's last point.
  const seqs = hornetSequences(w.length)
  const covered = seqs.length ? seqs[seqs.length - 1][seqs[seqs.length - 1].length - 1] + 1 : 0
  if (seqs.length > 1) r.warnings.push(`A Hornet sequence holds 15 points; the route runs across SEQ 1-${seqs.length}.`)
  if (covered < w.length) r.warnings.push(`Waypoints past ${covered} are loaded but in no sequence.`)
  setAt(r.data, ['WYPT', 'NAV_PTS'], w.map((p, i) => {
    const n = i + 1
    const pt: JsonObject = {
      id: `STPT${n}`, idOA: `OA${n}`, idOA_Line: `OA${n}Line`, wypt_num: n,
      x: r.xy[i].x, y: r.xy[i].y, alt: p.elev_ft * FT,
      note: p.note.slice(0, 200), text_note: name(p.name, i).slice(0, 5),
      R1: false, R2: false, R3: false,
      altitudeType: 1, velocityType: 3,
      isOA: false, OA_Range: 0, OA_Bearing: 0, OA_X: 0, OA_Y: 0, OA_Alt: 0, OA_DeltaX: 0, OA_DeltaY: 0,
      OA_Bearing_Units: 1, OA_Range_Units: 1, OA_Elevation_Units: 1,
    }
    const o = p.oa?.[0] && capRange(p.oa[0], 400000, `the OA of waypoint ${i + 1}`, r.warnings)
    if (o) {
      const g = offsetOnGrid(p.lat, p.lon, o, r.grid.terrain)
      Object.assign(pt, {
        isOA: true, OA_X: g.x, OA_Y: g.y, OA_Alt: o.elev_ft * FT,
        OA_Range: g.range, OA_Bearing: (g.bearingRad * 180) / Math.PI, OA_DeltaX: g.deltaX, OA_DeltaY: g.deltaY,
      })
    }
    seqs.forEach((seq, si) => {
      const k = seq.indexOf(i)
      if (k >= 0) {
        pt[`R${si + 1}`] = true
        pt[`R${si + 1}_order`] = k + 1
      }
    })
    return pt
  }))
  // One route plan per sequence; a sequence has at most one TGT point.
  const routes: JsonObject[] = [{}, {}, {}]
  let extraTargets = 0
  seqs.forEach((seq, si) => {
    let tgtUsed = false
    for (const i of seq) {
      const p = w[i]
      const t = r.tos(i)
      const tgt = p.target && !tgtUsed
      if (tgt) tgtUsed = true
      else if (p.target) extraTargets++
      routes[si][`STPT${i + 1}`] = {
        route_num: si + 1, wypt_num: i + 1, alt: p.alt_ft * FT, altitudeType: 1,
        speed, ETA: t ?? 0, FIX_Time: t != null, TGT: tgt,
      }
    }
  })
  if (extraTargets) r.warnings.push('A Hornet sequence takes one TGT point; extra targets are plain waypoints in it.')
  setAt(r.data, ['WYPT', 'NAV_ROUTE'], routes.map(o => (Object.keys(o).length ? o : [])))
  setAt(r.data, ['WYPT', 'terrain'], r.terrain)
  r.filled.push('/WYPT/NAV_PTS', '/WYPT/NAV_ROUTE')
  // Plan radios merge channel by channel. Modulation 0 = AM, 1 = FM.
  if (r.plan.include_comms) {
    for (const [radio, list] of [['COMM1', r.plan.comm1], ['COMM2', r.plan.comm2]] as const) {
      for (const c of list) {
        const ch = (getAt(r.data, ['COMM', radio, `Channel_${c.preset}`]) ?? {}) as JsonObject
        const n = hornetName(c.label)
        setAt(r.data, ['COMM', radio, `Channel_${c.preset}`], { ...ch, frequency: c.freq_mhz, modulation: c.modulation === 'FM' ? 1 : 0, ...(n ? { name: n } : {}) })
      }
      if (list.length) r.filled.push(`/COMM/${radio}`)
    }
  }
}

/** Upper-case A-Z/0-9 only, as the Apache's keyboard fields take. */
const apacheText = (s: string, max: number) => s.toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, max)

/**
 * AH-64D: Mission 1 (what MASTER LOAD loads). Every route point is a WPTHZ
 * waypoint, target-flagged ones also a TGT point; route 1 flies the
 * waypoints in order. Speeds in knots, distances in metres.
 */
function apacheRoute(r: RouteFill) {
  const w = r.plan.waypoints
  if (w.length > 50) throw new ExportError('The Apache holds 50 waypoints/hazards; this plan has more.')
  const m1 = ['NAV', 'Mission_1']
  setAt(r.data, [...m1, 'Points', 'WPTHZ', 'POINTS'], w.map((p, i) => ({
    num: i + 1,
    id: 6, // WP
    text: apacheText(p.name, 3) || `W${String(i + 1).padStart(2, '0')}`,
    note: '',
    x: r.xy[i].x, y: r.xy[i].y,
    alt: p.elev_ft * FT,
  })))
  setAt(r.data, [...m1, 'Points', 'WPTHZ', 'isEnabled'], true)
  const tg = w.map((p, i) => ({ p, i })).filter(({ p }) => p.target)
  if (tg.length) {
    setAt(r.data, [...m1, 'Points', 'TGT', 'POINTS'], tg.map(({ p, i }, k) => ({
      num: k + 1,
      id: 1, // TG
      text: `T${String(k + 1).padStart(2, '0')}`,
      note: apacheText(p.name, 24),
      x: r.xy[i].x, y: r.xy[i].y,
      alt: p.elev_ft * FT,
    })))
    setAt(r.data, [...m1, 'Points', 'TGT', 'isEnabled'], true)
  }
  const speed = Math.round(Math.min(9999, Math.max(1, r.plan.speed_kts)))
  const legs = w.map((p, i) => {
    const d = i === 0 ? 0 : Math.hypot(r.xy[i].x - r.xy[i - 1].x, r.xy[i].y - r.xy[i - 1].y)
    // Never below the ground: a field waypoint planned at 0 ft MSL still sits at its elevation.
    return { num: i + 1, alt: Math.max(p.alt_ft, p.elev_ft) * FT, speed, dist: d, eta: i === 0 ? 0 : d / (speed * 0.514), fix: false }
  })
  const routes = clone((getAt(r.data, [...m1, 'Routes']) as Json[] | undefined) ?? [])
  const first = (routes[0] ?? {}) as JsonObject
  routes[0] = {
    ...first,
    isEnabled: true,
    Name: apacheText(r.plan.callsign || r.plan.name, 5) || 'ALPHA',
    POINTS: legs,
  }
  setAt(r.data, [...m1, 'Routes'], routes)
  r.filled.push('/NAV/Mission_1')

  // Plan radios: COMM preset n (1-10) carries a frequency per radio. UHF and
  // VHF are AM (0), FM is FM (1); DoNotUploadTabData already defaults to
  // upload. A radio whose initial setting is that preset follows it.
  if (r.plan.include_comms) {
    let skipped = 0
    const band = (mhz: number): [string, number] | null =>
      mhz >= 225 && mhz < 400 ? ['UHF', 0] : mhz >= 108 && mhz < 152 ? ['VHF', 0] : mhz >= 30 && mhz < 88 ? ['FM1', 1] : null
    const taken = new Set<string>()
    let clashes = 0
    for (const c of [...r.plan.comm1, ...r.plan.comm2]) {
      const b = band(c.freq_mhz)
      if (c.preset > 10 || !b) { skipped++; continue }
      const [radio, modulation] = b
      if (taken.has(`${c.preset}/${radio}`)) { clashes++; continue }
      taken.add(`${c.preset}/${radio}`)
      const at = ['Presets', 'COMM', c.preset - 1, 'COMMS', radio]
      setAt(r.data, at, { ...((getAt(r.data, at) as JsonObject) ?? { Encryption: 0 }), Frequency: c.freq_mhz, Modulation: modulation })
      const init = getAt(r.data, ['Radios', 'InitialRadioSettings', radio]) as JsonObject | undefined
      if (init && init.Mode === 0 && init.Preset === c.preset) Object.assign(init, { Frequency: c.freq_mhz, Modulation: modulation })
      r.filled.push('/Presets/COMM')
    }
    if (skipped) r.warnings.push(`${skipped} radio preset${skipped > 1 ? 's' : ''} outside the Apache's presets 1-10 or its UHF/VHF/FM bands were left out.`)
    if (clashes) r.warnings.push(`${clashes} preset${clashes > 1 ? 's' : ''} put a second frequency on the same Apache radio and preset; the first was kept.`)
  }
}

/**
 * F-14B(U): the route is flight plan 2 (NAV[1]); plan 1 is by definition the
 * mission's own ME route and the DTC refuses edits to it. No terrain key and
 * no upload flags: everything in the file loads. Elevations in feet here,
 * but JDAM elevations in metres.
 */
function tomcatRoute(r: RouteFill) {
  const w = r.plan.waypoints
  const nav = clone((getAt(r.data, ['NAV']) as Json[] | undefined) ?? [])
  if (nav.length !== 12) throw new ExportError('The F-14 cartridge must hold 12 flight plans.')
  const plan2 = (nav[1] ?? {}) as JsonObject
  const used = ((plan2.lines as JsonObject[] | undefined) ?? []).reduce((s, l) => s + ((l.points as Json[] | undefined)?.length ?? 0), 0)
    + ((plan2.additional_points as Json[] | undefined)?.length ?? 0)
  if (w.length + used > 50) throw new ExportError(`An F-14 flight plan holds 50 points; this one would have ${w.length + used}.`)
  const spd = Math.min(990, Math.round(r.plan.speed_kts / 10) * 10)
  nav[1] = {
    ...plan2,
    name: ((plan2.name as string) || r.plan.name || 'ROUTE').slice(0, 16),
    waypoints: w.map((p, i) => ({
      name: (p.name || `WP${i + 1}`).toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 5) || `WP${i + 1}`,
      x: r.xy[i].x, y: r.xy[i].y, lat: p.lat, lon: p.lon,
      elev: Math.min(60000, Math.max(0, Math.floor(p.elev_ft + 0.5))),
      spd, tot: '',
    })),
  }
  nav[0] = { ...(nav[0] as JsonObject), waypoints: [] }
  setAt(r.data, ['NAV'], nav)
  delete r.data.terrain
  r.data.cartridge_name = (r.data.cartridge_name as string) || 'DEFAULT'
  if (r.plan.include_comms && (r.plan.comm1.length || r.plan.comm2.length)) {
    r.warnings.push('The F-14 cartridge has no radio section; set presets in the jet.')
  }

  // JDAM: an active target the pilot left without a position takes the plan's
  // next target waypoint; every active target gets its release zone and a
  // launch point at the zone's centre.
  const targets = w.map((p, i) => ({ p, i })).filter(({ p }) => p.target)
  let next = 0
  const stations = (getAt(r.data, ['JDAM', 'stations']) as JsonObject[] | undefined) ?? []
  let zeroElev = 0
  for (const st of stations) {
    for (const t of (st.targets as JsonObject[] | undefined) ?? []) {
      if (!t.active) continue
      if (typeof t.x !== 'number' || typeof t.y !== 'number') {
        const tg = targets[next++]
        if (!tg) throw new ExportError('An active F-14 JDAM target has no position; mark a target waypoint on the route for it.')
        Object.assign(t, { x: r.xy[tg.i].x, y: r.xy[tg.i].y, lat: tg.p.lat, lon: tg.p.lon, elev: tg.p.elev_ft * FT })
      }
      // Placed on the map: the cartridge also stores lat/lon.
      if (typeof t.lat !== 'number' || typeof t.lon !== 'number') Object.assign(t, fromDcsXY(t.x as number, t.y as number, r.grid.terrain))
      if (t.elev === 0) zeroElev++
      const l = lar(t.drop_alt as number, t.drop_spd as number)
      const c = larCentre(t.x as number, t.y as number, (t.attack_heading as number) ?? 0, l)
      Object.assign(t, l, { launch_x: c.x, launch_y: c.y })
    }
  }
  if (zeroElev) r.warnings.push(`${zeroElev} JDAM target${zeroElev > 1 ? 's are' : ' is'} at 0 m elevation; set the ground elevation for an accurate hit.`)
  r.filled.push('/NAV')
}

/**
 * MiG-29: Program 1. Every route point goes in the point database and the
 * route; the first three en-route points are the SN-29's three steer points.
 * Metres, km/h, DCS map metres; nothing stored as lat/lon.
 */
function fulcrumRoute(r: RouteFill) {
  const w = r.plan.waypoints
  const kmh = r.plan.speed_kts * KTS_TO_KMH
  const p1 = ['Program_1']
  const note = (s: string) => s.toUpperCase().replace(/[^A-Z0-9 ]/g, '').slice(0, 7)
  // Planned altitude in metres MSL, never below the point's ground.
  const above = (p: { alt_ft: number; elev_ft: number }) => Math.max(p.alt_ft, p.elev_ft) * FT
  setAt(r.data, [...p1, 'Points'], w.map((p, i) => ({
    id: `PNT${i + 1}`, num: `PNT${i + 1}`, name: `PNT${i + 1}`, note: note(p.name),
    x: r.xy[i].x, y: r.xy[i].y, alt: above(p), speed: kmh, terrainAltitude: p.elev_ft * FT,
  })))
  // Steer points: en-route points, skipping the departure and arrival fields.
  const enRoute = w.length > 2 ? w.map((_, i) => i).slice(1, -1) : w.map((_, i) => i)
  const steer = enRoute.slice(0, 3)
  if (enRoute.length > 3) r.warnings.push('The MiG-29 holds three steer points; the first three en-route points were used.')
  setAt(r.data, [...p1, 'Waypoints'], steer.map((i, k) => ({
    num: `WPT${k + 1}`, id: `PNT${i + 1}`, geo: true, name: `PNT${i + 1}`, note: '',
    x: r.xy[i].x, y: r.xy[i].y, alt: above(w[i]), speed: kmh,
  })))
  setAt(r.data, [...p1, 'Route'], w.map((p, i) => {
    const k = steer.indexOf(i)
    const t = r.tos(i)
    return {
      num: k >= 0 ? `WPT${k + 1}` : `PNT${i + 1}`, id: `PNT${i + 1}`, name: `PNT${i + 1}`, note: '',
      x: r.xy[i].x, y: r.xy[i].y, alt: above(p), alt_type: 1, speed: kmh, speed_type: 1,
      ETA: t ?? 0, FIX_Time: t != null && p.target,
    }
  }))
  const ti = Math.max(0, w.findIndex(p => p.target))
  setAt(r.data, [...p1, 'TargetPoint'], { id: `PNT${ti + 1}`, name: `PNT${ti + 1}`, x: r.xy[ti].x, y: r.xy[ti].y, altitude: w[ti].elev_ft * FT })
  const ws = getAt(r.data, [...p1, 'WeaponSettings'])
  if (Array.isArray(ws) || ws == null) setAt(r.data, [...p1, 'WeaponSettings'], { trainingMode: false })
  r.data.SelectedProgram = (r.data.SelectedProgram as string) || 'Program_1'
  r.filled.push('/Program_1/Points')

  // Plan radios: the R-862 has channels 0-19 (VHF 100-149.975 and UHF
  // 220-399.975 MHz). Preset n is channel n, preset 20 channel 0, so the
  // cockpit numbers match the plan's. Modulation 1 = AM, 2 = FM.
  if (r.plan.include_comms) {
    let skipped = 0
    // COMM1 and COMM2 share the one radio: a clash moves to the next free channel.
    const used = new Set<number>()
    let moved = 0
    for (const c of [...r.plan.comm1, ...r.plan.comm2]) {
      const inBand = (c.freq_mhz >= 100 && c.freq_mhz < 150) || (c.freq_mhz >= 220 && c.freq_mhz < 400)
      if (!inBand || c.preset > 20) { skipped++; continue }
      let ch = c.preset % 20
      if (used.has(ch)) {
        const free = Array.from({ length: 20 }, (_, k) => (c.preset + k) % 20).find(k => !used.has(k))
        if (free == null) { skipped++; continue }
        ch = free
        moved++
      }
      used.add(ch)
      setAt(r.data, [...p1, 'Radio', `Channel_${ch}`], { freq: c.freq_mhz, modulation: c.modulation === 'FM' ? 2 : 1 })
      r.filled.push('/Program_1/Radio')
    }
    if (moved) r.warnings.push(`COMM1 and COMM2 share the MiG-29's one radio: ${moved} preset${moved > 1 ? 's' : ''} moved to free channels.`)
    if (skipped) r.warnings.push(`${skipped} radio preset${skipped > 1 ? 's' : ''} outside the R-862's bands (100-149.975, 220-399.975 MHz) were left out.`)
  }
}

/** An offset no longer than the cockpit takes, with a warning if cut. */
function capRange<T extends { range_ft: number }>(o: T, maxFt: number, what: string, warnings: string[]): T {
  if (o.range_ft <= maxFt) return o
  warnings.push(`${what} is longer than the jet takes; cut to ${Math.round(maxFt)} ft.`)
  return { ...o, range_ft: maxFt }
}

/** Route fillers, per aircraft. */
const ROUTE: Partial<Record<NativeType, (r: RouteFill) => void>> = {
  'F-16C_50': f16Route,
  'FA-18C_hornet': hornetRoute,
  'AH-64D_BLK_II': apacheRoute,
  'F-14BU': tomcatRoute,
  'MiG-29 Fulcrum': fulcrumRoute,
}

export function nativeReady(t: NativeType): boolean {
  return t in ROUTE
}

/**
 * The full cartridge for `plan`: every section, the pilot's changes, the
 * route on the right map, and every tab with content switched on.
 */
export function buildNative(plan: Plan, type: NativeType, defaults: Json, ov: Overrides, tos: (i: number) => number | null): NativeExport {
  const def = aircraftDef(plan.aircraft)
  const fill = ROUTE[type]
  if (!fill) throw new ExportError(`The ${def.label} cartridge exporter is not in this build yet.`)
  const w = plan.waypoints
  if (w.length === 0) throw new ExportError('The plan has no waypoints.')
  const max = def.nativeMax ?? 25
  if (w.length > max) throw new ExportError(`The ${def.label} cartridge takes ${max} nav points; this plan has ${w.length}.`)
  const map = terrainAt(w[0].lat, w[0].lon)
  if (!map) throw new ExportError('The route is not on a DCS map this planner knows.')
  if (w.some(p => terrainAt(p.lat, p.lon)?.id !== map.id)) {
    throw new ExportError(`The route leaves the ${map.terrain.name} map; a cartridge holds one map's points.`)
  }

  const nm = safeName(plan.name)
  const kept = pruneOverrides(defaults, ov)
  const data = applyOverrides(clone(defaults), kept) as JsonObject
  data.type = type
  data.name = nm
  data.terrain = map.id
  const r: RouteFill = {
    data, plan, terrain: map.id, grid: map,
    xy: w.map(p => toDcsXY(p.lat, p.lon, map.terrain)),
    warnings: [], filled: [], tos,
  }
  const targets = w.filter(p => p.target && p.elev_ft === 0).length
  if (targets) r.warnings.push(`${targets} target point${targets > 1 ? 's have' : ' has'} elevation 0 ft; set the ground elevation for accurate targeting.`)
  fill(r)

  // Switch on every tab the route filled or the pilot changed, unless the
  // pilot set that tab's flag themselves.
  const touched = [...r.filled, ...Object.keys(kept)]
  const dropped = Object.keys(ov).length - Object.keys(kept).length
  if (dropped) r.warnings.push(`${dropped} saved setting${dropped > 1 ? 's' : ''} no longer match this cartridge's layout and were left out.`)
  for (const tab of TABS[type] ?? []) {
    if (tab.flag in kept) continue
    // The flag can sit inside the tab it guards (isEnabled); setting it is
    // not a change to the tab's content.
    const inTab = touched.some(p => (p === tab.path || p.startsWith(tab.path + '/')) && p !== tab.flag)
    if (inTab) setAt(data, fromPointer(tab.flag), tab.load ?? false)
  }

  return {
    filename: `${nm}.dtc`,
    // UTF-8 without a BOM, as DCS's JSON reader expects.
    text: JSON.stringify({ name: nm, type, data }, null, 2),
    terrain: map.id,
    warnings: r.warnings,
  }
}
