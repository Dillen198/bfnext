// DCS-DTC presets (github.com/the-paid-actor/dcs-dtc, 10.x). Newtonsoft JSON
// with PascalCase names and enums as integers; every file carries
// "Aircraft" and "Version": 2, or the app runs its V1 converter and throws.
//
// Two shapes, because the app reads a file two ways:
// * a whole preset, dropped into Documents\DCS-DTC\Presets\<code>\ -- every
//   system present (a null system crashes its editor on several aircraft);
// * a "Load from file" import -- only the route and the systems the plan
//   changed, every other system an explicit null. A missing or default
//   system would come in ticked and wipe the pilot's own settings.

import { applyOverrides, clone, pruneOverrides, type Json, type JsonObject, type Overrides } from './model'
import type { Plan, PlanWaypoint } from './types'

/** U+2019: DCS-DTC types coordinates character by character and the minute
 *  mark must be this, not an ASCII apostrophe. */
const MIN = '’'
/** U+201D for the seconds mark of DMS strings (Hornet pre-planned targets). */
const SEC = '”'

/** "N 42°10.630’" / "E 042°28.840’", zero-padded to fixed width. */
export function ddm(v: number, isLat: boolean, decimals = 3): string {
  const hemi = isLat ? (v >= 0 ? 'N' : 'S') : (v >= 0 ? 'E' : 'W')
  const a = Math.abs(v)
  let d = Math.floor(a)
  let m = Number(((a - d) * 60).toFixed(decimals))
  if (m >= 60) { d += 1; m = 0 }
  const deg = String(d).padStart(isLat ? 2 : 3, '0')
  const min = m.toFixed(decimals).padStart(decimals + 3, '0')
  return `${hemi} ${deg}°${min}${MIN}`
}

/** "N 42°10’37.80”" */
export function dms(v: number, isLat: boolean): string {
  const hemi = isLat ? (v >= 0 ? 'N' : 'S') : (v >= 0 ? 'E' : 'W')
  let total = Math.round(Math.abs(v) * 360000) // hundredths of a second
  const d = Math.floor(total / 360000)
  total -= d * 360000
  const m = Math.floor(total / 6000)
  const s = (total - m * 6000) / 100
  return `${hemi} ${String(d).padStart(isLat ? 2 : 3, '0')}°${String(m).padStart(2, '0')}${MIN}${s.toFixed(2).padStart(5, '0')}${SEC}`
}

export const freqStr = (mhz: number) => mhz.toFixed(3)

export interface DcsDtcTarget {
  /** DCS-DTC's aircraft code: the JSON "Aircraft" and the preset folder. */
  code: string
  label: string
  /** DCS unit types this preset is for. */
  units: string[]
  /** What the app types into the jet; empty for a store-only aircraft. */
  uploads: string
  /** Stores presets but the app has no uploader yet. */
  storeOnly?: boolean
  maxWaypoints: number
  firstSequence: number
  /** Name length the cockpit takes; longer names are cut. */
  nameMax: number
  /** Systems that must never be null in a whole preset. */
  neverNull?: string[]
  /** Steerpoint elevation the cockpit accepts, feet; several uploaders drop a minus sign. */
  elev: [number, number]
  /** Radio preset shape: two radios of {Number, Name, Frequency}, or one
   *  "Radio6" system with a frequency per radio per preset. */
  radios?: 'two' | 'six' | 'four'
}

export const DCSDTC_TARGETS: DcsDtcTarget[] = [
  // Limits are DCS-DTC's own editor ranges (GetFirst/LastAllowedSequence) and
  // what its uploaders actually type.
  { code: 'F16C', label: 'F-16C Viper', units: ['F-16C_50'], uploads: 'steerpoints, CMS, radios, MFD, HARM, HTS, datalink, misc', maxWaypoints: 699, firstSequence: 1, nameMax: 24, elev: [-1500, 80000], radios: 'two' },
  { code: 'FA18C', label: 'F/A-18C Hornet', units: ['FA-18C_hornet'], uploads: 'waypoints, sequences, CMS, radios, radar, pre-planned weapons, HMD, misc', maxWaypoints: 60, firstSequence: 0, nameMax: 24, neverNull: ['Waypoints', 'Sequences', 'Radios'], elev: [0, 25000], radios: 'two' },
  { code: 'F15E', label: 'F-15E Strike Eagle', units: ['F-15ESE'], uploads: 'routes A/B/C, radios, displays, smart weapons, misc', maxWaypoints: 99, firstSequence: 1, nameMax: 24, neverNull: ['*'], elev: [0, 59999], radios: 'two' },
  { code: 'A10', label: 'A-10C II', units: ['A-10C_2', 'A-10C'], uploads: 'waypoints, ARC-210 presets', maxWaypoints: 200, firstSequence: 1, nameMax: 12, elev: [0, 20000], radios: 'two' },
  { code: 'AV8B', label: 'AV-8B Harrier', units: ['AV8BNA'], uploads: 'waypoints', maxWaypoints: 60, firstSequence: 1, nameMax: 24, elev: [0, 35000] },
  { code: 'AH64D', label: 'AH-64D Apache', units: ['AH-64D_BLK_II'], uploads: 'waypoints, control measures, targets, routes, TSD, laser codes, radios, kneeboard', maxWaypoints: 50, firstSequence: 1, nameMax: 24, neverNull: ['*'], elev: [0, 59999], radios: 'six' },
  { code: 'OH58D', label: 'OH-58D Kiowa', units: ['OH58D'], uploads: 'waypoints, radios', maxWaypoints: 60, firstSequence: 1, nameMax: 24, elev: [0, 35000], radios: 'four' },
  { code: 'CH47F', label: 'CH-47F Chinook', units: ['CH-47Fbl1'], uploads: 'waypoints or flight plan, radios', maxWaypoints: 200, firstSequence: 1, nameMax: 8, elev: [0, 30000], radios: 'two' },
  { code: 'C130', label: 'C-130J', units: ['C-130J-30'], uploads: 'waypoints, route, radios', maxWaypoints: 99, firstSequence: 1, nameMax: 6, elev: [0, 30000], radios: 'two' },
  { code: 'F14BU', label: 'F-14B', units: ['F-14BU', 'F-14B', 'F-14A-135-GR'], uploads: '', storeOnly: true, maxWaypoints: 60, firstSequence: 1, nameMax: 24, elev: [0, 35000] },
]

export function dcsdtcFor(unit: string): DcsDtcTarget | undefined {
  return DCSDTC_TARGETS.find(t => t.units.includes(unit))
}

function wpt(w: PlanWaypoint, seq: number, t: DcsDtcTarget, extra: JsonObject = {}): JsonObject {
  let name = (w.name || `WP${seq}`).toUpperCase()
  // The C-130 takes 1-6 of A-Z/0-9 and strips the rest after truncating, so
  // clean first.
  if (t.code === 'C130') name = name.replace(/[^A-Z0-9]/g, '') || `WP${seq}`
  return {
    Sequence: seq,
    Name: name.slice(0, t.nameMax),
    Latitude: ddm(w.lat, true),
    Longitude: ddm(w.lon, false),
    Elevation: Math.round(Math.min(t.elev[1], Math.max(t.elev[0], w.elev_ft))),
    TimeOverSteerpoint: null,
    Target: w.target,
    ...extra,
  }
}

/** What each two-radio aircraft's radios tune in DCS-DTC, MHz, and how many presets they hold. */
const RADIO_BANDS: Record<string, [{ ranges: [number, number][]; maxPreset: number }, { ranges: [number, number][]; maxPreset: number } | null]> = {
  F16C: [{ ranges: [[225, 399.975]], maxPreset: 20 }, { ranges: [[30, 87.975], [108, 151.975]], maxPreset: 20 }],
  F15E: [{ ranges: [[225, 399.975]], maxPreset: 20 }, { ranges: [[30, 399.975]], maxPreset: 40 }],
  C130: [{ ranges: [[225, 399.975]], maxPreset: 20 }, { ranges: [[108, 173.975]], maxPreset: 20 }],
  CH47F: [{ ranges: [[225, 399.975]], maxPreset: 20 }, { ranges: [[108, 173.975]], maxPreset: 20 }],
  FA18C: [{ ranges: [[30, 399.975]], maxPreset: 20 }, { ranges: [[30, 399.975]], maxPreset: 20 }],
  A10: [{ ranges: [[30, 399.975]], maxPreset: 30 }, null],
}

/** Which of the Apache/Kiowa radios a frequency belongs on: UHF, VHF, FM1. */
function radioIndex(mhz: number): number | null {
  if (mhz >= 225 && mhz < 400) return 0
  if (mhz >= 118 && mhz < 152) return 1
  if (mhz >= 30 && mhz < 88) return 2
  return null
}

/**
 * The plan's radio presets ("write these presets") into the preset's radio
 * system, unless the pilot edited that system in the avionics editor -- their
 * own table wins.
 */
function fillRadios(cfg: JsonObject, plan: Plan, t: DcsDtcTarget, ov: Overrides, warnings: string[]): boolean {
  if (!plan.include_comms || !t.radios || !(plan.comm1.length || plan.comm2.length)) return false
  if (Object.keys(ov).some(p => p.startsWith('/Radios'))) return false
  const radios = (cfg.Radios ?? {}) as JsonObject
  if (t.radios === 'two') {
    const bands = RADIO_BANDS[t.code]
    let skipped = 0
    const list = (chs: Plan['comm1'], radio: 0 | 1, offset = 0) => chs.flatMap(c => {
      const n = c.preset + offset
      const b = bands?.[radio]
      if (!b || n > b.maxPreset || !b.ranges.some(([lo, hi]) => c.freq_mhz >= lo && c.freq_mhz <= hi)) { skipped++; return [] }
      return [{ Number: n, Name: c.label ? c.label.slice(0, t.code === 'C130' ? 5 : 8) : null, Frequency: freqStr(c.freq_mhz) }]
    })
    if (t.code === 'A10') {
      // DCS-DTC uploads only the A-10's ARC-210 (Radio1), which covers every
      // band with 30 presets: COMM2 presets go there as 20 + n.
      const r1 = [...list(plan.comm1, 0), ...list(plan.comm2, 0, 20)]
      const seen = new Set<number>()
      const uniq = r1.filter(p => !seen.has(p.Number) && seen.add(p.Number))
      if (uniq.length < r1.length) warnings.push('Two presets landed on the same ARC-210 channel; the first was kept.')
      radios.Radio1 = { ...((radios.Radio1 as JsonObject) ?? {}), Presets: uniq }
      if (plan.comm2.length) warnings.push('The A-10 has one programmable radio in DCS-DTC: COMM2 presets are ARC-210 presets 21+.')
    } else {
      radios.Radio1 = { ...((radios.Radio1 as JsonObject) ?? {}), Presets: list(plan.comm1, 0) }
      radios.Radio2 = { ...((radios.Radio2 as JsonObject) ?? {}), Presets: list(plan.comm2, 1) }
    }
    if (skipped) warnings.push(`${skipped} radio preset${skipped > 1 ? 's' : ''} outside the ${t.label}'s radio bands or preset range were left out.`)
  } else {
    const width = t.radios === 'six' ? 6 : 4
    const byNum = new Map<number, JsonObject>()
    let skipped = 0
    for (const c of [...plan.comm1, ...plan.comm2]) {
      const ix = radioIndex(c.freq_mhz)
      if (c.preset > 10 || ix == null) { skipped++; continue }
      const p = byNum.get(c.preset) ?? { Number: c.preset, Name: c.label ? c.label.toUpperCase().slice(0, 8) : null, Frequencies: Array(width).fill('') }
      ;(p.Frequencies as string[])[ix] = freqStr(c.freq_mhz)
      byNum.set(c.preset, p)
    }
    if (skipped) warnings.push(`${skipped} preset${skipped > 1 ? 's' : ''} outside 1-10 or the UHF/VHF/FM bands were left out.`)
    const r = (radios.Radio ?? {}) as JsonObject
    r.Presets = [...byNum.values()].sort((a, b) => (a.Number as number) - (b.Number as number))
    // The Kiowa's uploader reads four selected modes by position (UHF, VHF,
    // FM1, FM2); "preset mode, no preset" leaves a radio as it is.
    if (t.radios === 'four') {
      r.SelectedModes = [1, 2, 3, 4].map(n => ({ Number: n, SelectedFrequency: '', SelectedMode: 2, SelectedPreset: '' }))
    } else if (r.SelectedModes == null) {
      r.SelectedModes = []
    }
    radios.Radio = r
  }
  cfg.Radios = radios
  ;(cfg.Upload as JsonObject).Radios = true
  return true
}

/** Mission seconds at each waypoint, if the plan has a start time. */
type Tos = (i: number) => string | null

/** Fill the route-managed systems of `cfg` from the plan, in place. */
function fillRoute(cfg: JsonObject, plan: Plan, t: DcsDtcTarget, tos: Tos): string[] {
  const warnings: string[] = []
  const w = plan.waypoints.slice(0, t.maxWaypoints)
  if (plan.waypoints.length > t.maxWaypoints) warnings.push(`${t.label} takes ${t.maxWaypoints} waypoints; the rest were left out.`)
  const upload = (cfg.Upload ?? {}) as JsonObject

  if (t.code === 'F15E') {
    const route = (cfg.RouteA ?? { Waypoints: [] }) as JsonObject
    route.Waypoints = w.map((p, i) => wpt(p, i + 1, t, { TimeOverSteerpoint: tos(i), MEA: Math.max(0, Math.round(p.alt_ft - 1000)) }))
    cfg.RouteA = route
    upload.RouteA = true
  } else if (t.code === 'AH64D') {
    const nav = w.filter(p => !p.target)
    const tgt = w.filter(p => p.target).slice(0, 50)
    cfg.Waypoints = { ...(cfg.Waypoints as JsonObject ?? {}), Waypoints: nav.slice(0, 50).map((p, i) => wpt(p, i + 1, t, { PointType: 'WP', Identifier: 'WP', Free: (p.name || '').toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 3) })) }
    cfg.Targets = { ...(cfg.Targets as JsonObject ?? {}), Waypoints: tgt.map((p, i) => wpt(p, i + 1, t, { PointType: 'TG', Identifier: 'TG', Free: (p.name || '').toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 3) })) }
    upload.Waypoints = nav.length > 0
    upload.Targets = tgt.length > 0
    if (nav.length > 50) warnings.push('The Apache takes 50 waypoints/hazards; the rest were left out.')
  } else {
    const sys = (cfg.Waypoints ?? { Waypoints: [] }) as JsonObject
    // The C-130's route refers to waypoints by their (6-character) names, so
    // they must be unique once cut.
    const seen = new Set<string>()
    sys.Waypoints = w.map((p, i) => {
      const seq = t.firstSequence + i
      if (t.code === 'F16C') {
        return wpt(p, seq, t, {
          TimeOverSteerpoint: tos(i),
          UseOA: false, OffsetAimpoint1: null, OffsetAimpoint2: null,
          UseVIP: false, VIPtoTGT: null, VIPtoPUP: null,
          UseVRP: false, TGTtoVRP: null, TGTtoPUP: null,
        })
      }
      const out = wpt(p, seq, t, { TimeOverSteerpoint: t.code === 'FA18C' ? tos(i) : null })
      if (t.code === 'C130') {
        let n = out.Name as string
        for (let k = 0; seen.has(n); k++) {
          const tag = k === 0 ? String(seq) : `${seq}${k}`
          n = n.slice(0, Math.max(1, 6 - tag.length)) + tag
        }
        seen.add(n)
        out.Name = n
      }
      return out
    })
    cfg.Waypoints = sys
    upload.Waypoints = true
    if (t.code === 'FA18C') {
      // Put the route in sequence 1 (Mode 1 = all waypoints) and upload it.
      const seqs = (cfg.Sequences ?? {}) as JsonObject
      seqs.Seq1 = { ...((seqs.Seq1 as JsonObject) ?? {}), Mode: 1, Waypoints: null }
      seqs.EnableUpload1 = true
      cfg.Sequences = seqs
      upload.Sequences = true
    }
  }
  if (t.storeOnly) warnings.push(`DCS-DTC ${t.code === 'F14BU' ? '10.x stores F-14 presets but cannot upload them yet; use the native DCS cartridge for the Tomcat.' : 'cannot upload this aircraft yet.'}`)
  cfg.Upload = upload
  return warnings
}

export interface DcsDtcExport {
  filename: string
  text: string
  warnings: string[]
}

/**
 * The preset for `t`: its defaults, the plan's overrides for "dcsdtc:<code>",
 * and the route. `partial` makes a "Load from file" import: systems the plan
 * neither routes nor changes become null.
 */
export function buildDcsDtc(plan: Plan, t: DcsDtcTarget, defaults: Json, ov: Overrides, partial: boolean, tos: Tos): DcsDtcExport {
  const kept = pruneOverrides(defaults, ov)
  const cfg = applyOverrides(clone(defaults), kept) as JsonObject
  cfg.Aircraft = t.code
  cfg.Version = 2
  const warnings = fillRoute(cfg, plan, t, tos)
  const radios = fillRadios(cfg, plan, t, kept, warnings)
  if (plan.notes) cfg.KneeboardNotes = plan.notes
  const dropped = Object.keys(ov).length - Object.keys(kept).length
  if (dropped) warnings.push(`${dropped} saved setting${dropped > 1 ? 's' : ''} no longer match this preset's layout and were left out.`)

  if (partial) {
    // WaypointsCapture is the pilot's own app setting: leave it alone.
    const keep = new Set(['Aircraft', 'Version', 'Upload', 'KneeboardNotes'])
    const w = plan.waypoints.slice(0, t.maxWaypoints)
    const routed = t.code === 'F15E' ? ['RouteA']
      : t.code === 'AH64D' ? [...(w.some(p => !p.target) ? ['Waypoints'] : []), ...(w.some(p => p.target) ? ['Targets'] : [])]
        : t.code === 'FA18C' ? ['Waypoints', 'Sequences'] : ['Waypoints']
    routed.forEach(k => keep.add(k))
    if (radios) keep.add('Radios')
    for (const p of Object.keys(kept)) keep.add(p.split('/')[1])
    for (const k of Object.keys(cfg)) if (!keep.has(k)) cfg[k] = null
  }
  const name = (plan.name || 'Flight plan').replace(/[*/?<>|\\:"]/g, '').trim() || 'Flight plan'
  return { filename: `${name}.json`, text: JSON.stringify(cfg, null, 2), warnings }
}

/** DCS-DTC's clipboard share string: base64 of a 4-byte little-endian
 *  length and the gzipped UTF-8 JSON. */
export async function shareString(json: string): Promise<string> {
  const bytes = new TextEncoder().encode(json)
  const gz = new Uint8Array(await new Response(
    new Blob([bytes]).stream().pipeThrough(new CompressionStream('gzip')),
  ).arrayBuffer())
  const out = new Uint8Array(gz.length + 4)
  new DataView(out.buffer).setInt32(0, bytes.length, true)
  out.set(gz, 4)
  let bin = ''
  for (const b of out) bin += String.fromCharCode(b)
  return btoa(bin)
}
