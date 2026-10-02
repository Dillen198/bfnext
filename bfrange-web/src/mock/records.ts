/**
 * Fixture generators: one realistic `RangeRecord` (with its track) per
 * result kind. Tracks are built to agree with the grade -- a "VERY high all
 * the way" pass flies high on the trap sheet, a bomb's weapon track comes out
 * of the same ballistics model the calculator uses, a missile outcome falls
 * out of a small pursuit simulation.
 */
import { bodyFromWeapon, groundSpeedFromTas, trajectory, windComponents } from '../lib/ballistics'
import { FT, KT, NM, destination, fromLocal, norm180, norm360, rad, type LatLon } from '../lib/geo'
import { aarGrade, bombQuality, clock as clockOf, goodRadius, precisionQuality, strafeQuality } from '../lib/grading'
import { describe as describeCalls, gradePoints, parseCall } from '../lib/lso'
import type {
  GeoPt,
  GrooveSample,
  PassOutcome,
  PilotRef,
  RangeRecord,
  RangeResult,
  RelSample,
  ResultKind,
  TrackPt,
  WeaponClass,
} from '../types'
import { Rng, hash } from './rng'
import {
  ARENAS,
  CARRIERS,
  LZS,
  MISSION_DATE,
  PADS,
  PILOTS,
  SLING_COURSES,
  TANKERS,
  THEATRE,
  pt,
  stationById,
  type MockPilot,
} from './world'

const clamp = (v: number, lo: number, hi: number) => Math.max(lo, Math.min(hi, v))
const r1 = (v: number) => Math.round(v * 10) / 10
const r2 = (v: number) => Math.round(v * 100) / 100

type Base = Omit<RangeRecord, 'result' | 'score' | 'track'>

function base(p: MockPilot, unit_type: string, ts: number): Base {
  const d = new Date(ts)
  const sortie = `range-${d.toISOString().slice(0, 10).replace(/-/g, '')}`
  // ids depend only on the event, never on when the page loaded, so a copied
  // debrief link still opens after a reload
  const seq = hash(`${p.name}|${unit_type}`) % 1000
  // the mission clock runs 3 h ahead of UTC (Caucasus local)
  const mt = new Date(ts + 3 * 3600_000).toISOString().slice(11, 19)
  return {
    id: `${sortie}-${ts}-${seq}`,
    v: 1,
    ts: d.toISOString(),
    mission_time: mt,
    mission_date: MISSION_DATE,
    theatre: THEATRE,
    pilot: { ucid: p.ucid, name: p.name },
    unit_type,
    side: 'blue',
    callsign: p.callsign,
  }
}

const geo = (p: LatLon, alt_m = 0): GeoPt => ({ lat: p.lat, lon: p.lon, alt_m })

/** Local (north, east, up) metres around an origin -> TrackPt. */
function tp(origin: LatLon, t: number, n: number, e: number, alt_m: number, speed_kts: number): TrackPt {
  const g = fromLocal(origin, n, e)
  return { t: r2(t), lat: g.lat, lon: g.lon, alt_m: Math.round(alt_m), speed_kts: Math.round(speed_kts) }
}

// ─── trap ──────────────────────────────────────────────────────────────────

interface TrapScenario {
  grade: string
  comment: string
  outcome: PassOutcome
  wire: number | null
  w: number
}

const TRAP_SCENARIOS: TrapScenario[] = [
  { grade: '_OK_', comment: '', outcome: 'trap', wire: 3, w: 1.5 },
  { grade: 'OK', comment: '(LUL)X (H)IC', outcome: 'trap', wire: 3, w: 5 },
  { grade: 'OK', comment: '(F)X (LO)AR', outcome: 'trap', wire: 2, w: 4 },
  { grade: 'OK', comment: '(DR)IM', outcome: 'trap', wire: 3, w: 3 },
  { grade: '(OK)', comment: '(DL)X LOIC (F)AR', outcome: 'trap', wire: 2, w: 4 },
  { grade: '(OK)', comment: 'HX (SLO)IC', outcome: 'trap', wire: 4, w: 3 },
  { grade: '(OK)', comment: '(H)IM HIC', outcome: 'trap', wire: 4, w: 3 },
  { grade: '--', comment: '_LOIC_ _LOAR_ SLOIW', outcome: 'trap', wire: 1, w: 2 },
  { grade: 'B', comment: '(H)IM _HIC_ LIG', outcome: 'bolter', wire: null, w: 2 },
  { grade: 'WO', comment: 'AAX FIM (SLO)AR _HAW_', outcome: 'waveoff', wire: null, w: 1.5 },
  { grade: 'WO', comment: '_LULIC_ LOAR', outcome: 'waveoff', wire: null, w: 1 },
  { grade: 'C', comment: '_SLOX_ _LURX_ 3PTSIW LNFIW', outcome: 'trap', wire: 1, w: 0.4 },
  { grade: 'OWO', comment: '(H)X _LULIM_', outcome: 'own_waveoff', wire: null, w: 1 },
  { grade: 'WOFD', comment: '', outcome: 'waveoff', wire: null, w: 0.4 },
]

/** Windows of the groove each LSO position covers (0 = groove start, 1 = ramp). */
const WINDOW: Record<string, [number, number]> = {
  X: [0, 0.33], IM: [0.33, 0.66], IC: [0.66, 0.9], AR: [0.88, 1], AW: [0, 1],
  IW: [0.97, 1], TL: [0.4, 1], BC: [0, 0.12],
}

function bump(f: number, [a, b]: [number, number]): number {
  const e = 0.08
  const rise = clamp((f - (a - e)) / e, 0, 1)
  const fall = clamp((b + e - f) / e, 0, 1)
  const x = Math.min(rise, fall)
  return x * x * (3 - 2 * x)
}

const MAG = { little: 0, normal: 1, lot: 2 } as const

/** The groove deviations a comment implies, as functions of groove fraction. */
function grooveProfile(comment: string, rng: Rng) {
  const calls = comment.split(/\s+/).filter(Boolean).map(parseCall)
  const ph = rng.range(0, 6)
  return (f: number) => {
    let gse = 0.07 * Math.sin(f * 11 + ph) + 0.05 * Math.sin(f * 23 + ph * 2)
    let lue = 0.1 * Math.sin(f * 7 + ph)
    let aoa = 0.12 * Math.sin(f * 9 + ph)
    for (const c of calls) {
      const w = WINDOW[c.position ?? 'AW'] ?? WINDOW.AW
      const m = MAG[c.magnitude]
      const k = bump(f, w)
      switch (c.error) {
        case 'H': gse += [0.6, 1.1, 1.85][m] * k; break
        case 'LO': gse -= [0.42, 0.75, 1.05][m] * k; break
        case 'LUL': lue -= [0.8, 1.6, 3.3][m] * k; break
        case 'LUR': lue += [0.8, 1.6, 3.3][m] * k; break
        case 'DL': lue -= [0.7, 1.4, 2.6][m] * k * clamp((f - w[0]) / (w[1] - w[0] + 1e-9), 0, 1); break
        case 'DR': lue += [0.7, 1.4, 2.6][m] * k * clamp((f - w[0]) / (w[1] - w[0] + 1e-9), 0, 1); break
        case 'AA': lue += [1.8, 3.2, 4.5][m] * k * (1 - clamp((f - w[0]) / (w[1] - w[0] + 1e-9), 0, 1)); break
        case 'F': aoa -= [0.45, 0.85, 1.3][m] * k; break
        case 'SLO': aoa += [0.45, 0.85, 1.3][m] * k; break
      }
    }
    return { gse, lue, aoa }
  }
}

const ONSPEED: Record<string, number> = { 'FA-18C_hornet': 8.1, 'F-14B': 10.4, 'F-14A-135-GR': 10.4, 'T-45': 8.6 }

export function trapRecord(p: MockPilot, rng: Rng, ts: number, forced?: Partial<TrapScenario> & { unit_type?: string; night?: boolean }): RangeRecord {
  const unit_type = forced?.unit_type ?? rng.pick(p.airframes.filter(a => a in ONSPEED).length ? p.airframes.filter(a => a in ONSPEED) : ['FA-18C_hornet'])
  const sc: TrapScenario = forced?.grade
    ? { ...TRAP_SCENARIOS.find(s => s.grade === forced.grade)!, ...forced } as TrapScenario
    : rng.weighted(TRAP_SCENARIOS.map(s => {
        const pts = gradePoints(s.grade) ?? 2
        const skillW = pts >= 4 ? 0.4 + p.skill * 1.6 : pts <= 2 ? 1.6 - p.skill * 1.2 : 1
        return [s, s.w * skillW] as const
      }))
  const cv = CARRIERS[0]
  const night = forced?.night ?? rng.chance(0.22)
  const kase = night ? 3 : rng.chance(0.12) ? 2 : 1
  const fb = norm360(cv.brc_deg + cv.deck_angle_deg)
  const landing = destination(destination(cv.pos, cv.brc_deg + 180, 70), cv.brc_deg - 90, 6)
  const toLL = (x: number, y: number) => destination(destination(landing, fb + 180, x), fb + 90, y)
  const prof = grooveProfile(sc.comment, rng)
  const onspeed = ONSPEED[unit_type] ?? 8.1
  const GS = 3.5
  const x0 = 1100
  const samples: GrooveSample[] = []
  let t = 0
  let lastAlt: number | null = null
  const push = (x: number, y: number, altFt: number, aoa: number | null, closure: number, gse?: number, lue?: number) => {
    // climb positive, as the engine writes it (v.y * 60 * M_TO_FT)
    const vs = lastAlt === null ? 0 : ((altFt - lastAlt) / 0.5) * 60
    lastAlt = altFt
    const ll = toLL(x, y)
    const g = gse ?? (x > 60 ? Math.atan2(altFt * FT, x) * 57.2958 - GS : 0)
    const l = lue ?? (x > 60 ? Math.atan2(y, x) * 57.2958 : 0)
    samples.push({
      t: r2(t), x_m: r1(x), y_m: r1(y), alt_ft: Math.round(altFt), gse_deg: r2(g), lue_deg: r2(l),
      aoa_deg: aoa === null ? null : r2(aoa), closure_kts: r1(closure), vs_fpm: Math.round(vs), lat: ll.lat, lon: ll.lon,
    })
    t += 0.5
  }

  // ── groove start state (the end of the 180) ──
  const g0 = prof(0)
  const yG = x0 * Math.tan(rad(g0.lue))
  const altG = (x0 * Math.tan(rad(GS + g0.gse))) / FT
  const abeamY = -rng.range(2000, 2500)
  const abeamAlt = rng.range(570, 640)
  const wakeAlt = altG + rng.range(90, 150)
  // downwind: abeam (x=-250) to the 180 (x = x0)
  const dwSpeed = 160 * KT
  for (let x = -250; x < x0; x += dwSpeed * 0.5) {
    push(x, abeamY + rng.gauss(0, 3), abeamAlt - ((x + 250) / (x0 + 250)) * 40, null, 160)
  }
  // the 180: half-ellipse to the groove start
  const cy = (abeamY + yG) / 2
  const b = (yG - abeamY) / 2
  const a = rng.range(560, 720)
  const turnLen = Math.PI * Math.sqrt((a * a + b * b) / 2)
  const turnSteps = Math.max(8, Math.round(turnLen / (135 * KT * 0.5)))
  for (let i = 0; i < turnSteps; i++) {
    const th = -Math.PI / 2 + (Math.PI * i) / turnSteps
    const f = i / turnSteps
    // descend through the 90 to the wake, then settle onto the ball
    const alt = f < 0.8 ? abeamAlt - 40 - (abeamAlt - 40 - wakeAlt) * (f / 0.8) : wakeAlt + (altG - wakeAlt) * ((f - 0.8) / 0.2)
    push(x0 + a * Math.cos(th), cy + b * Math.sin(th), alt, onspeed + rng.gauss(0, 0.3), 135)
  }

  // ── the groove ──
  const stopF = sc.outcome === 'waveoff' ? rng.range(0.8, 0.9) : sc.outcome === 'own_waveoff' ? rng.range(0.5, 0.65) : 1
  const closureBase = rng.range(108, 118)
  const grooveLen = x0 * stopF
  const n = Math.round(grooveLen / (closureBase * KT * 0.5))
  const grooveStartT = t
  for (let i = 0; i <= n; i++) {
    const f = (i / n) * stopF
    const x = x0 * (1 - f)
    const d = prof(f)
    const y = x * Math.tan(rad(d.lue))
    const alt = Math.max(0, (x * Math.tan(rad(GS + d.gse))) / FT)
    push(x, y, alt, onspeed + d.aoa, closureBase - d.aoa * 4, d.gse, d.lue)
  }
  const grooveTime = t - grooveStartT
  // ── after the groove ──
  const lastS = samples[samples.length - 1]
  if (sc.outcome === 'trap') {
    for (let x = -10, v = closureBase; v > 0; x -= v * KT * 0.5, v -= 38) push(x, lastS.y_m * 0.3, 0, null, Math.max(0, v), 0, 0)
  } else {
    const climb = sc.outcome === 'bolter' ? 0 : lastS.alt_ft
    for (let i = 1; i < 24; i++) {
      const x = lastS.x_m - i * closureBase * KT * 0.5
      push(x, lastS.y_m - i * 2.5, climb + (i * 1500) / 120, null, closureBase + i)
    }
  }

  const wodKts = r1(rng.range(24, 31))
  const b0 = base(p, unit_type, ts)
  const dcsComment = `LSO: GRADE:${sc.grade}${sc.comment ? ' ' + sc.comment : ' :'}${sc.wire ? ` WIRE# ${sc.wire}` : ''}`
  const engineDiffers = rng.chance(0.25)
  const result: RangeResult = {
    kind: 'trap',
    carrier: cv.name,
    carrier_type: cv.unit_type,
    case: kase,
    night,
    outcome: sc.outcome,
    grade: sc.grade,
    points: gradePoints(sc.grade),
    lso_comment: sc.comment,
    lso_description: describeCalls(sc.comment),
    wire: sc.wire,
    wire_from_dcs: sc.wire !== null,
    groove_time_s: r1(grooveTime),
    wind_over_deck_kts: wodKts,
    final_bearing_deg: r1(fb),
    source: 'dcs',
    dcs_comment: dcsComment,
    engine_grade: {
      grade: engineDiffers ? (sc.grade === 'OK' ? '(OK)' : sc.grade === '(OK)' ? 'OK' : sc.grade) : sc.grade,
      points: gradePoints(engineDiffers ? (sc.grade === 'OK' ? '(OK)' : sc.grade === '(OK)' ? 'OK' : sc.grade) : sc.grade),
      comment: sc.comment,
    },
    pattern: {
      break_alt_ft: Math.round(rng.range(760, 860)),
      abeam_distance_nm: r2(-abeamY / NM),
      abeam_alt_ft: Math.round(abeamAlt),
      ninety_alt_ft: Math.round(abeamAlt - 40 - (abeamAlt - 40 - wakeAlt) * 0.55),
      wake_alt_ft: Math.round(wakeAlt),
      pattern_time_s: Math.round(rng.range(52, 72)),
      notes: [
        -abeamY / NM > 1.3 ? 'Wide abeam (TWA)' : -abeamY / NM < 1.1 ? 'Close abeam (TCA)' : 'Abeam distance on the numbers',
        ...(wakeAlt > 420 ? ['High at the wake: start down earlier in the 180'] : []),
      ],
    },
    hook_down: true,
  }
  return {
    ...b0,
    score: gradePoints(sc.grade),
    result,
    track: { kind: 'groove', samples },
  }
}

// ─── bomb ──────────────────────────────────────────────────────────────────

interface WeaponSpec {
  name: string
  display: string
  cls: WeaponClass
  guidance: string
  mass: number
  caliber: number
}

export const BOMBS: WeaponSpec[] = [
  { name: 'Mk_82', display: 'Mk-82', cls: 'unguided', guidance: 'none', mass: 241, caliber: 0.273 },
  { name: 'Mk_83', display: 'Mk-83', cls: 'unguided', guidance: 'none', mass: 447, caliber: 0.357 },
  { name: 'Mk_84', display: 'Mk-84', cls: 'unguided', guidance: 'none', mass: 894, caliber: 0.458 },
  { name: 'BDU_33', display: 'BDU-33', cls: 'unguided', guidance: 'none', mass: 11.3, caliber: 0.102 },
  { name: 'BDU_50LD', display: 'BDU-50LD', cls: 'unguided', guidance: 'none', mass: 241, caliber: 0.273 },
  { name: 'GBU_12', display: 'GBU-12', cls: 'guided', guidance: 'laser', mass: 277, caliber: 0.273 },
  { name: 'GBU_16', display: 'GBU-16', cls: 'guided', guidance: 'laser', mass: 454, caliber: 0.356 },
  { name: 'GBU_38', display: 'GBU-38', cls: 'guided', guidance: 'ins', mass: 253, caliber: 0.273 },
  { name: 'CBU_99', display: 'CBU-99', cls: 'cluster', guidance: 'none', mass: 222, caliber: 0.335 },
  { name: 'HYDRA_70_M151', display: 'Hydra 70 M151', cls: 'rocket', guidance: 'none', mass: 10.4, caliber: 0.07 },
]

/** Per-pilot aim bias (metres long) so the insights have something to find. */
const LONG_BIAS: Record<string, number> = { Mongo: 19, Rook: -16, Viper: 3, Dutch: -4 }

export function bombRecord(
  p: MockPilot,
  rng: Rng,
  ts: number,
  f?: { station?: string; weapon?: string; miss?: number; radial?: number; alt_ft?: number; tas?: number; heading?: number; dive?: number; unit_type?: string; cls?: WeaponClass; guidance?: string },
): RangeRecord {
  const unit_type = f?.unit_type ?? rng.pick(p.airframes)
  const st = stationById(f?.station ?? rng.weighted([['range_a_circle', 6], ['range_b_array', 3], ['range_c_laser', 2], ['range_c_coord', 1]] as const))
  const w0 = BOMBS.find(w => w.name === f?.weapon) ?? rng.weighted(
    (st.kind === 'laser_target'
      ? [['GBU_12', 4], ['GBU_16', 2]]
      : [['Mk_82', 5], ['BDU_33', 4], ['BDU_50LD', 3], ['Mk_83', 1], ['GBU_38', 2], ['GBU_12', 2], ['CBU_99', 1], ['HYDRA_70_M151', 2]]
    ).map(([n, wt]) => [BOMBS.find(b => b.name === n)!, wt as number] as const),
  )
  const w = { ...w0, cls: f?.cls ?? w0.cls, guidance: f?.guidance ?? w0.guidance }
  const heading = f?.heading ?? norm360(rng.pick([45, 75, 225, 255, 300]) + rng.gauss(0, 6))
  const guided = w.cls === 'guided'
  let along: number, cross: number
  if (f?.miss !== undefined && f?.radial !== undefined) {
    const n0 = f.miss * Math.cos(rad(f.radial))
    const e0 = f.miss * Math.sin(rad(f.radial))
    along = n0 * Math.cos(rad(heading)) + e0 * Math.sin(rad(heading))
    cross = -n0 * Math.sin(rad(heading)) + e0 * Math.cos(rad(heading))
  } else if (guided) {
    const sd = 5 * (1.5 - p.skill)
    const blunder = rng.chance(0.12) ? rng.range(18, 40) : 0
    const dir = rng.range(0, Math.PI * 2)
    along = rng.gauss(0, sd) + blunder * Math.cos(dir)
    cross = rng.gauss(0, sd) + blunder * Math.sin(dir)
  } else {
    const sd = (w.cls === 'rocket' ? 22 : 18) * (1.45 - p.skill)
    along = (LONG_BIAS[p.name] ?? 0) + rng.gauss(0, sd * 1.2)
    cross = rng.gauss(0, sd * 0.7)
  }
  const north = along * Math.cos(rad(heading)) - cross * Math.sin(rad(heading))
  const east = along * Math.sin(rad(heading)) + cross * Math.cos(rad(heading))
  const miss = Math.hypot(north, east)
  const radial = norm360((Math.atan2(east, north) * 180) / Math.PI)

  const lowLevel = w.cls === 'rocket' || (w.cls === 'unguided' && rng.chance(0.5))
  const altAgl = (f?.alt_ft ?? (lowLevel ? rng.range(2500, 6000) : rng.range(9000, 22000))) * FT
  const dive = f?.dive ?? (w.cls === 'rocket' ? rng.range(15, 30) : lowLevel ? rng.range(10, 35) : guided ? rng.range(0, 5) : rng.range(0, 15))
  const tasKts = f?.tas ?? rng.range(380, 500)
  const windFrom = norm360(rng.gauss(290, 30))
  const windKts = rng.range(5, 22)
  // the same model the calculator (and bfdb's calibration) uses
  const [wa, wc] = windComponents(heading, windFrom, windKts)
  const gs = groundSpeedFromTas(tasKts * KT, dive, wa, wc)
  const drop = trajectory(
    bodyFromWeapon({ mass_kg: w.mass, caliber_m: w.caliber })!,
    { z0_m: altAgl, ground_msl_m: st.pos.alt_m, gs_mps: gs, dive_deg: dive, wind_along_mps: wa, wind_cross_mps: wc },
    w.cls === 'rocket' ? 0.25 : 1.12,
  )
  // the release point sits a drop's throw before the impact
  const impact = fromLocal(st.pos, north, east)
  const hx = Math.cos(rad(heading)), hy = Math.sin(rad(heading))
  const relN = north - (drop.impact.along_m * hx - drop.impact.cross_m * hy)
  const relE = east - (drop.impact.along_m * hy + drop.impact.cross_m * hx)
  const release = fromLocal(st.pos, relN, relE)
  const points: TrackPt[] = drop.points.map(q => {
    const n = relN + q.x * hx - q.y * hy
    const e = relE + q.x * hy + q.y * hx
    return tp(st.pos, q.t, n, e, st.pos.alt_m + q.z, q.v / KT)
  })
  const ground = Math.hypot(relN, relE)
  const quality = bombQuality(w.cls, miss)
  const b0 = base(p, unit_type, ts)
  const result: RangeResult = {
    kind: 'bomb',
    station_id: st.id,
    range: st.name,
    target: st.target,
    weapon: w.name,
    weapon_display: w.display,
    weapon_class: w.cls,
    guidance: w.guidance,
    release: {
      pos: geo(release, st.pos.alt_m + altAgl),
      alt_agl_m: r1(altAgl),
      tas_kts: r1(tasKts),
      gs_kts: r1(gs / KT),
      heading_deg: r1(heading),
      dive_deg: r1(dive),
      slant_range_m: Math.round(Math.hypot(ground, altAgl)),
      ground_range_m: Math.round(ground),
      wind_from_deg: Math.round(windFrom),
      wind_kts: r1(windKts),
      mach: r2(tasKts / 640),
    },
    target_pos: st.pos,
    impact: geo(impact, st.pos.alt_m),
    impact_north_m: r1(north),
    impact_east_m: r1(east),
    miss_m: r1(miss),
    radial_deg: r1(radial),
    clock: clockOf(radial - heading),
    long_m: r1(along),
    cross_m: r1(cross),
    time_of_flight_s: r1(drop.impact.tof_s),
    quality,
    target_hit: miss < 3,
    ...(w.guidance === 'laser' ? { laser_code: 1688 } : {}),
    rings_m: st.rings_m.length ? st.rings_m : [10, 25, 50, 100],
    good_radius_m: goodRadius(w.cls),
  }
  return { ...b0, score: { SHACK: 5, EXCELLENT: 4, GOOD: 3, INEFFECTIVE: 2, POOR: 1 }[quality], result, track: { kind: 'weapon', points } }
}

// ─── strafe ────────────────────────────────────────────────────────────────

const GUNS: Record<string, string> = {
  'F-16C_50': 'M61A1 20 mm', 'FA-18C_hornet': 'M61A2 20 mm', 'A-10C_2': 'GAU-8/A 30 mm', AV8BNA: 'GAU-12 25 mm',
  'F-15ESE': 'M61A1 20 mm', 'F-14B': 'M61A1 20 mm',
}

export function strafeRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const unit_type = rng.pick(p.airframes.filter(a => a in GUNS).length ? p.airframes.filter(a => a in GUNS) : ['F-16C_50'])
  const st = stationById(rng.pick(['range_a_strafe_1', 'range_a_strafe_2']))
  const rounds = unit_type === 'A-10C_2' ? rng.int(120, 320) : rng.int(90, 260)
  const fouled = rng.chance(0.14 * (1.3 - p.skill))
  const acc = clamp(rng.gauss(30 + p.skill * 50, 11), 3, 97)
  const hits = Math.round((rounds * acc) / 100)
  const quality = strafeQuality(acc, !fouled)
  const minRange = fouled ? rng.range(470, 600) : rng.range(640, 900)
  const heading = norm360(70 + rng.gauss(0, 4))
  const entry = rng.range(300, 750)
  const pts: TrackPt[] = []
  for (let i = 0; i <= 30; i++) {
    const d = 5200 - i * ((5200 - minRange) / 24)
    const along = i <= 24 ? -d : -minRange + (i - 24) * 180
    const off = i <= 24 ? 0 : (i - 24) * 140
    const alt = i <= 24 ? st.pos.alt_m + entry * (d / 5200) + 60 : st.pos.alt_m + 90 + (i - 24) * 55
    const n = along * Math.cos(rad(heading)) - off * Math.sin(rad(heading))
    const e = along * Math.sin(rad(heading)) + off * Math.cos(rad(heading))
    pts.push(tp(st.pos, i * 1.5, n, e, alt, 420))
  }
  const result: RangeResult = {
    kind: 'strafe',
    station_id: st.id,
    range: st.name.split(' — ')[0],
    pit: st.target,
    gun: GUNS[unit_type] ?? 'M61A1 20 mm',
    rounds_fired: rounds,
    hits,
    accuracy_pct: r1(acc),
    quality,
    foul_line_crossed: fouled,
    ...(fouled ? { invalid_reason: `Crossed the foul line (closest ${Math.round(minRange)} m)` } : {}),
    run_in_heading_deg: r1(heading),
    min_range_m: Math.round(minRange),
    entry_alt_agl_m: Math.round(entry),
    target_pos: st.pos,
    foul_line_m: 610,
  }
  const sc = { DEADEYE: 5, EXCELLENT: 4, GOOD: 3, INEFFECTIVE: 2, POOR: 1, INVALID: null }[quality]
  return { ...base(p, unit_type, ts), score: sc, result, track: { kind: 'path', paths: { [p.name]: pts } } }
}

// ─── AAR ───────────────────────────────────────────────────────────────────

const BOOM_TYPES = ['F-16C_50', 'F-15ESE', 'A-10C_2']
const NO_PROBE = ['T-45', 'M-1 Abrams', 'UH-1H', 'CH-47Fbl1', 'AH-64D_BLK_II']

export function aarRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const receivers = p.airframes.filter(a => !NO_PROBE.includes(a))
  const unit_type = rng.pick(receivers.length ? receivers : ['FA-18C_hornet'])
  const boom = BOOM_TYPES.includes(unit_type)
  const tk = boom ? TANKERS[0] : rng.pick([TANKERS[1], TANKERS[2], TANKERS[3], TANKERS[4]])
  const method = tk.method
  const contacts = rng.weighted([[1, 5], [2, 3], [3, 1.5], [4, 0.5]] as const)
  const disconnects = contacts - 1 + (rng.chance(0.25 * (1.2 - p.skill)) ? 1 : 0)
  const join = clamp(rng.gauss(260 - p.skill * 150, 50), 70, 480)
  const k = 1.45 - p.skill
  const stab = {
    fore_aft_sd_m: r2(Math.abs(rng.gauss(1.4 * k, 0.35))),
    lateral_sd_m: r2(Math.abs(rng.gauss(1.0 * k, 0.3))),
    vertical_sd_m: r2(Math.abs(rng.gauss(1.1 * k, 0.3))),
    mean_fwd_m: 0, mean_right_m: 0, mean_up_m: 0,
  }
  const contact = boom ? { fwd: -24, right: 0, up: -8 } : tk.unit_type === 'KC130' ? { fwd: -30, right: -17, up: -5 } : tk.unit_type === 'KC135MPRS' ? { fwd: -33, right: 19, up: -6 } : { fwd: -26, right: 0, up: -4 }
  stab.mean_fwd_m = r2(contact.fwd + rng.gauss(0, 0.4))
  stab.mean_right_m = r2(contact.right + rng.gauss(0, 0.3))
  stab.mean_up_m = r2(contact.up + rng.gauss(0, 0.3))
  const closure = r1(clamp(rng.gauss(4.5 - p.skill * 2, 2), 0.8, 11))
  const overshoot = rng.chance(0.08 * (1.3 - p.skill))
  const rate = boom ? rng.range(1350, 1700) : rng.range(850, 1100)
  const connected = rng.range(110, 300)
  const fuelLbs = (rate * connected) / 60
  const g = aarGrade({
    contacts, disconnects, join_time_s: join, fore_aft_sd: stab.fore_aft_sd_m, lateral_sd: stab.lateral_sd_m,
    vertical_sd: stab.vertical_sd_m, precontact_closure_kts: closure, overshoot,
  })
  // samples at 1 Hz: join from 1 nm trail, then contact / disconnect cycles
  const samples: RelSample[] = []
  let t = 0
  let fuel = 0
  const pre = { fwd: contact.fwd - 14, right: contact.right, up: contact.up - 2 }
  let prevFwd = -NM
  const add = (fwd: number, right: number, up: number, conn: boolean) => {
    if (conn) fuel += (rate * 0.4536) / 60 // lb/min -> kg per 1 s sample
    samples.push({
      t, fwd_m: r2(fwd), right_m: r2(right), up_m: r2(up), connected: conn, fuel_kg: Math.round(fuel),
      closure_kts: r1((fwd - prevFwd) / KT),
    })
    prevFwd = fwd
    t += 1
  }
  for (let i = 0; i < join; i++) {
    const f = i / join
    const e = 1 - Math.pow(1 - f, 2.2)
    const osc = overshoot && f > 0.9 ? Math.sin((f - 0.9) * 30) * 12 : 0
    add(-NM + (pre.fwd + NM) * e + osc, -120 * (1 - e) + pre.right * e, -60 * (1 - e) + pre.up * e, false)
  }
  const perContact = connected / contacts
  for (let c = 0; c < contacts; c++) {
    for (let i = 0; i < 6; i++) add(pre.fwd + (contact.fwd - pre.fwd) * (i / 6), contact.right, pre.up + (contact.up - pre.up) * (i / 6), false)
    const phase = rng.range(0, 6)
    for (let i = 0; i < perContact; i++) {
      add(
        contact.fwd + stab.fore_aft_sd_m * (Math.sin(i * 0.21 + phase) * 1.1 + rng.gauss(0, 0.35)),
        contact.right + stab.lateral_sd_m * (Math.sin(i * 0.17 + phase * 2) * 1.1 + rng.gauss(0, 0.35)),
        contact.up + stab.vertical_sd_m * (Math.sin(i * 0.13 + phase * 3) * 1.1 + rng.gauss(0, 0.35)),
        true,
      )
    }
    if (c < contacts - 1) {
      const gap = rng.int(15, 35)
      for (let i = 0; i < gap; i++) add(contact.fwd - 6 * Math.sin((Math.PI * i) / gap) - 2, contact.right + rng.gauss(0, 0.8), contact.up - 2 * Math.sin((Math.PI * i) / gap), false)
    }
  }
  for (let i = 0; i < 40; i++) add(contact.fwd - i * 4, contact.right - i * 2.5, contact.up - i * 1.2, false)

  const result: RangeResult = {
    kind: 'aar',
    tanker: tk.callsign,
    tanker_type: tk.unit_type,
    method,
    join_time_s: Math.round(join),
    contacts,
    disconnects,
    time_connected_s: Math.round(connected),
    fuel_kg: Math.round(fuelLbs * 0.4536),
    fuel_lbs: Math.round(fuelLbs),
    onload_rate_lbs_min: Math.round(rate),
    stability: stab,
    precontact_closure_kts: closure,
    overshoot,
    alt_ft: tk.alt_ft,
    speed_kts: tk.speed_kts,
    grade: g.letter,
    calls: g.calls,
    session_s: Math.round(t),
  }
  return { ...base(p, unit_type, ts), score: r2(g.score), result, track: { kind: 'aar', samples } }
}

// ─── missile trainer ───────────────────────────────────────────────────────

const AAMS = ['AIM_120C', 'AIM_9X', 'AIM_7M', 'P_77', 'P_27PE']

export function missileRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const unit_type = rng.pick(p.airframes)
  const asTarget = rng.chance(0.6)
  const sam = asTarget && rng.chance(0.3)
  const arena = sam ? { pos: stationById('sam_pit').pos } : ARENAS[1]
  const origin: LatLon = arena.pos
  const other = rng.pick(PILOTS.filter(x => x.name !== p.name && x.airframes.some(a => a.startsWith('F') || a.startsWith('FA'))))
  const vsPlayer = !sam && rng.chance(0.35)
  const shooter: PilotRef = asTarget
    ? sam ? { name: 'SA-8 Gecko (SAM Pit)' } : vsPlayer ? { ucid: other.ucid, name: other.name } : { name: 'MiG-29A (AI adversary)' }
    : { ucid: p.ucid, name: p.name }
  const target: PilotRef = asTarget ? { ucid: p.ucid, name: p.name } : vsPlayer ? { ucid: other.ucid, name: other.name } : { name: 'Su-27 (AI, trainer on)' }
  const weapon = sam ? '9M33' : asTarget ? (vsPlayer ? 'AIM_120C' : 'P_77') : rng.pick(AAMS.slice(0, 3))
  const range = sam ? rng.range(6000, 11000) : rng.range(14000, 42000)
  const brg = rng.range(0, 360)
  const shooterAlt = sam ? stationById('sam_pit').pos.alt_m : rng.range(6500, 9500)
  let tgtAlt = rng.range(5000, 9000)
  // target starts hot-ish toward the shooter
  let tx = Math.sin(rad(brg)) * range, ty = Math.cos(rad(brg)) * range
  let th = norm360(brg + 180 + rng.gauss(0, 15))
  const aspect = Math.abs(norm180(th - (brg + 180)))
  const tv = 250
  const skill = asTarget ? p.skill : 0.5
  const reaction = clamp(rng.gauss(9 - skill * 6, 2), 1.5, 16)
  const plan = rng.weighted([['beam', 5], ['drag', 3], ['none', 0.6]] as const)
  const goLow = plan === 'beam' && rng.chance(0.35)
  let mx = 0, my = 0, mAlt = shooterAlt
  let mh = brg
  let mv = sam ? 300 : 280
  const vmax = sam ? 800 : weapon.startsWith('AIM_9') ? 900 : 1150
  const tau = sam ? 12 : weapon.startsWith('AIM_9') ? 10 : 24
  const killR = 150
  const missile: TrackPt[] = []
  const tgt: TrackPt[] = []
  let minD = Infinity
  let outcome: 'kill' | 'defeated' | 'timeout' = 'timeout'
  let beam = 0, drag = 0, hot = 0
  let firstTurn: number | null = null
  const th0 = th
  const dt = 0.5
  let tt = 0
  let lastD = Infinity
  for (; tt < 90; tt += dt) {
    // missile speed: boost, then bleed
    mv = tt < 4 ? mv + ((vmax - mv) * dt) / 2 : Math.max(180, vmax * Math.exp(-(tt - 4) / tau))
    const dx = tx - mx, dy = ty - my
    const d = Math.hypot(dx, dy, tgtAlt - mAlt)
    minD = Math.min(minD, d)
    missile.push(tp(origin, tt, my, mx, mAlt, mv / KT))
    tgt.push(tp(origin, tt, ty, tx, tgtAlt, tv / KT))
    if (d < killR) { outcome = 'kill'; break }
    if (tt > 6 && d > lastD && mv < 320) { outcome = 'defeated'; break }
    lastD = d
    // PN-ish: turn toward the target with a lead
    const los = Math.atan2(dx, dy)
    const lead = Math.atan2(tv * Math.sin(rad(th) - los), mv) * 0.9
    const want = los + lead
    const maxTurn = (30 * 9.81 / Math.max(mv, 100)) * dt
    const diff = Math.atan2(Math.sin(want - rad(mh)), Math.cos(want - rad(mh)))
    mh = norm360(mh + (clamp(diff, -maxTurn, maxTurn) * 180) / Math.PI)
    mx += Math.sin(rad(mh)) * mv * dt
    my += Math.cos(rad(mh)) * mv * dt
    mAlt += clamp(tgtAlt - mAlt, -40, 40) * dt
    // the target defends
    const toMissile = norm360((Math.atan2(mx - tx, my - ty) * 180) / Math.PI)
    const off = Math.abs(norm180(toMissile - th))
    if (off < 45) hot += dt
    else if (off >= 70 && off <= 110) beam += dt
    else if (off > 135) drag += dt
    if (tt >= reaction && plan !== 'none') {
      const goal = plan === 'beam' ? toMissile + (norm180(toMissile - th) > 0 ? -90 : 90) : toMissile + 180
      const turn = clamp(norm180(goal - th), -12 * dt, 12 * dt)
      th = norm360(th + turn)
      if (goLow) tgtAlt = Math.max(600, tgtAlt - 120 * dt)
    }
    if (firstTurn === null && Math.abs(norm180(th - th0)) > 30) firstTurn = tt
    tx += Math.sin(rad(th)) * tv * dt
    ty += Math.cos(rad(th)) * tv * dt
  }
  const b0 = base(p, unit_type, ts)
  const result: RangeResult = {
    kind: 'missile',
    weapon,
    weapon_category: sam ? 'sam' : 'aam',
    shooter,
    shooter_type: sam ? 'Osa 9A33 ln' : asTarget ? (vsPlayer ? 'FA-18C_hornet' : 'MiG-29A') : unit_type,
    target,
    target_type: asTarget ? unit_type : vsPlayer ? other.airframes[0] : 'Su-27',
    outcome,
    launch: {
      range_m: Math.round(range),
      aspect_deg: Math.round(aspect),
      shooter_alt_m: Math.round(shooterAlt),
      target_alt_m: Math.round(missile.length ? tgt[0].alt_m : tgtAlt),
      shooter_speed_kts: sam ? 0 : 520,
      target_speed_kts: Math.round(tv / KT),
      closure_kts: Math.round((tv * Math.cos(rad(aspect)) + (sam ? 0 : 270)) / KT),
      shooter_pos: geo(origin, shooterAlt),
      target_pos: geo(fromLocal(origin, Math.cos(rad(brg)) * range, Math.sin(rad(brg)) * range), tgt[0]?.alt_m ?? tgtAlt),
    },
    min_distance_m: Math.round(minD),
    time_of_flight_s: r1(tt),
    kill_radius_m: killR,
    defense: {
      reaction_s: firstTurn === null ? null : r1(firstTurn),
      beam_s: r1(beam),
      drag_s: r1(drag),
      hot_s: r1(hot),
      alt_change_m: Math.round(tgtAlt - (tgt[0]?.alt_m ?? tgtAlt)),
      went_low: goLow && tgtAlt < 1500,
    },
    perspective: asTarget ? 'target' : 'shooter',
  }
  const score = asTarget ? (outcome === 'kill' ? 1 : 5) : outcome === 'kill' ? 5 : 2
  return { ...b0, score, result, track: { kind: 'intercept', missile, target: tgt } }
}

// ─── engagement ────────────────────────────────────────────────────────────

export function engagementRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const unit_type = rng.pick(p.airframes)
  const duel = rng.chance(0.4)
  const opp = rng.pick(PILOTS.filter(x => x.name !== p.name && x.kinds.some(([k]) => k === 'engagement' || k === 'missile')))
  const setup = duel ? 'Duel' : rng.pick(['BFM offensive', 'BFM neutral', 'BFM defensive', 'BVR 30 nm hot'])
  const adversary = duel ? opp.name : rng.pick(['MiG-29A', 'Su-27', 'F-5E-3', 'MiG-21Bis'])
  const winP = duel ? 0.5 + (p.skill - opp.skill) : setup.includes('offensive') ? 0.75 : setup.includes('defensive') ? 0.35 : 0.55
  const outcome = rng.chance(0.06) ? 'abort' : rng.chance(0.1) ? 'draw' : rng.chance(clamp(winP, 0.1, 0.9)) ? 'win' : 'loss'
  const dur = rng.range(70, 260)
  const origin = ARENAS[0].pos
  const me: TrackPt[] = []
  const them: TrackPt[] = []
  const r0 = rng.range(900, 1400)
  const ph = rng.range(0, 6)
  for (let t = 0; t <= dur; t += 1) {
    const a = ph + t * 0.075
    const r = r0 * (1 - 0.35 * (t / dur))
    const lag = outcome === 'win' ? 0.5 : outcome === 'loss' ? -0.5 : 0.1
    me.push(tp(origin, t, Math.cos(a) * r + t * 6, Math.sin(a) * r, 5500 - t * 7 + Math.sin(t / 9) * 300, 330 - t * 0.3))
    them.push(tp(origin, t, Math.cos(a + Math.PI + lag) * r * 0.95 + t * 6, Math.sin(a + Math.PI + lag) * r * 0.95, 5400 - t * 8 + Math.cos(t / 8) * 300, 320 - t * 0.3))
  }
  const shots = rng.int(0, 4)
  const kills = outcome === 'win' ? 1 : 0
  const result: RangeResult = {
    kind: 'engagement',
    setup,
    adversary,
    adversary_skill: duel ? '' : rng.pick(['Average', 'Good', 'High', 'Excellent']),
    ...(duel ? { opponent: { ucid: opp.ucid, name: opp.name } } : {}),
    outcome,
    duration_s: Math.round(dur),
    shots_fired: shots,
    trainer_kills: Math.min(kills, shots) || (outcome === 'win' ? 1 : 0),
    gun_hits: outcome === 'win' && rng.chance(0.5) ? rng.int(5, 20) : 0,
    notes: [
      outcome === 'win' ? 'Converted to a rear-quarter shot' : outcome === 'loss' ? 'Bled below corner speed in the second circle' : 'Neutral merge held to the floor',
      ...(rng.chance(0.4) ? ['Hard deck (5,000 ft) touched once'] : []),
    ],
  }
  const score = outcome === 'win' ? 5 : outcome === 'draw' ? 3 : outcome === 'loss' ? 1 : null
  return { ...base(p, unit_type, ts), score, result, track: { kind: 'path', paths: { [p.name]: me, [adversary]: them } } }
}

// ─── anti-ship ─────────────────────────────────────────────────────────────

export function antiShipRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const unit_type = rng.pick(p.airframes)
  const st = stationById('ship_box')
  const weapon = rng.weighted([['AGM_84D', 4], ['AGM_65F', 3], ['Mk_82', 1]] as const)
  const maxR = weapon === 'AGM_84D' ? 124_000 : weapon === 'AGM_65F' ? 22_000 : null
  const launch = weapon === 'AGM_84D' ? rng.range(35_000, 95_000) : weapon === 'AGM_65F' ? rng.range(6_000, 18_000) : rng.range(1200, 2500)
  const hit = rng.chance(weapon === 'Mk_82' ? 0.35 : 0.72)
  const intercepted = !hit && rng.chance(0.3)
  const brg = rng.range(200, 320)
  const lp = destination(st.pos, brg, launch)
  const speed = weapon === 'AGM_84D' ? 240 : weapon === 'AGM_65F' ? 300 : 200
  const tof = launch / speed
  const pts: TrackPt[] = []
  const n = 40
  for (let i = 0; i <= n; i++) {
    const f = i / n
    const q = destination(lp, norm360(brg + 180), launch * f)
    pts.push({ t: r2(tof * f), lat: q.lat, lon: q.lon, alt_m: weapon === 'AGM_84D' ? (f < 0.1 ? 900 - f * 8500 : 15) : 600 * (1 - f), speed_kts: Math.round(speed / KT) })
  }
  const ship = rng.pick(['Molniya 1', 'Dry Cargo 2', 'Dry Cargo 3'])
  const result: RangeResult = {
    kind: 'anti_ship',
    ship,
    ship_type: ship.startsWith('Molniya') ? 'molniya' : 'Dry-cargo ship-1',
    weapon,
    launch_range_m: Math.round(launch),
    weapon_max_range_m: maxR,
    hit,
    damage: hit ? r2(rng.range(0.25, 0.75)) : 0,
    ship_sunk: hit && rng.chance(0.25),
    time_of_flight_s: r1(tof),
    intercepted,
    launch_pos: geo(lp, 3000),
    ship_pos: st.pos,
  }
  return { ...base(p, unit_type, ts), score: hit ? 4 + (result.ship_sunk ? 1 : 0) : 1, result, track: { kind: 'weapon', points: pts } }
}

// ─── rotary ────────────────────────────────────────────────────────────────

const PQ_SCORE = { PERFECT: 5, EXCELLENT: 4, GOOD: 3, FAIR: 2, POOR: 1 } as const

function approachPath(to: GeoPt, rng: Rng, secs: number, fromDist: number, cruiseAgl = 150): TrackPt[] {
  const brg = rng.range(0, 360)
  const pts: TrackPt[] = []
  for (let t = 0; t <= secs; t += 2) {
    const f = t / secs
    const d = fromDist * (1 - f)
    const q = destination(to, brg, d)
    const alt = to.alt_m + (f < 0.7 ? cruiseAgl : cruiseAgl * (1 - (f - 0.7) / 0.3))
    pts.push({ t, lat: q.lat, lon: q.lon, alt_m: Math.round(alt), speed_kts: Math.round(f < 0.7 ? 90 : 90 * (1 - (f - 0.7) / 0.3)) })
  }
  return pts
}

export function slingRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const unit_type = rng.pick(p.airframes.filter(a => a.startsWith('UH') || a.startsWith('CH') || a.startsWith('Mi')).length ? p.airframes.filter(a => a.startsWith('UH') || a.startsWith('CH')) : ['UH-1H'])
  const course = rng.pick(SLING_COURSES)
  const [cargo, mass] = unit_type === 'CH-47Fbl1' ? rng.pick([['container_cargo', 4200], ['ammo_cargo', 2800]] as const) : rng.pick([['uh1h_cargo', 600], ['ammo_cargo', 900]] as const)
  const dist = Math.abs(rng.gauss(0, 9 * (1.4 - p.skill))) + 0.4
  const off = destination(course.dz, rng.range(0, 360), dist)
  const quality = precisionQuality(5, dist)
  const result: RangeResult = {
    kind: 'sling',
    course: course.name,
    cargo,
    mass_kg: mass,
    time_s: Math.round(rng.range(170, 420)),
    distance_m: r1(dist),
    damage: r2(rng.chance(0.2) ? rng.range(0.05, 0.4) : 0),
    quality,
    dz_pos: course.dz,
    set_down_pos: geo(off, course.dz.alt_m),
  }
  return { ...base(p, unit_type, ts), score: PQ_SCORE[quality], result, track: { kind: 'path', paths: { [p.name]: approachPath(course.dz, rng, 200, 4200, 60) } } }
}

export function landingRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const unit_type = rng.pick(p.airframes.filter(a => a !== 'M-1 Abrams'))
  const pad = unit_type === 'AV8BNA' ? PADS[3] : rng.pick(PADS.slice(0, 3))
  const dist = Math.abs(rng.gauss(0, 2.6 * (1.45 - p.skill))) + 0.15
  const quality = precisionQuality(3, dist)
  const td = geo(destination(pad.pos, rng.range(0, 360), dist), pad.pos.alt_m)
  const result: RangeResult = {
    kind: 'landing',
    drill: unit_type === 'AV8BNA' ? 'ship' : pad.drill,
    pad: pad.name,
    distance_m: r1(dist),
    touchdown_fpm: Math.round(clamp(rng.gauss(260 - p.skill * 150, 80), 40, 650)),
    heading_error_deg: r1(rng.gauss(0, 7 * (1.3 - p.skill))),
    hover_s: r1(rng.range(3, 28)),
    quality,
    pad_pos: pad.pos,
    touchdown_pos: td,
  }
  return { ...base(p, unit_type, ts), score: PQ_SCORE[quality], result, track: { kind: 'path', paths: { [p.name]: approachPath(pad.pos, rng, 120, 3000) } } }
}

export function troopsRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const unit_type = rng.pick(p.airframes)
  const dist = Math.abs(rng.gauss(0, 18 * (1.4 - p.skill))) + 2
  const quality = precisionQuality(10, dist)
  const result: RangeResult = {
    kind: 'troops',
    lz: rng.pick(LZS),
    troops: unit_type === 'CH-47Fbl1' ? rng.pick([24, 30, 33]) : rng.pick([6, 8, 10]),
    load_time_s: Math.round(rng.range(35, 110)),
    total_time_s: Math.round(rng.range(320, 900)),
    landing_distance_m: r1(dist),
    quality,
  }
  return { ...base(p, unit_type, ts), score: PQ_SCORE[quality], result, track: { kind: 'path', paths: { [p.name]: approachPath(pt(41.96, 41.99, 40), rng, 240, 6000) } } }
}

export function gunneryRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const unit_type = rng.pick(p.airframes)
  const total = 8
  const killed = clamp(Math.round(rng.gauss(4 + p.skill * 4, 1.2)), 1, total)
  const hits = killed + rng.int(0, 3)
  const shots = hits + rng.int(1, 8)
  const result: RangeResult = {
    kind: 'gunnery',
    lane: 'Gunnery Lane 1',
    targets_total: total,
    targets_killed: killed,
    shots,
    hits,
    time_s: Math.round(rng.range(70, 240)),
    first_round_hits: Math.min(killed, rng.int(1, killed)),
  }
  return { ...base(p, unit_type, ts), score: r2((killed / total) * 5), result }
}

export function casRecord(p: MockPilot, rng: Rng, ts: number): RangeRecord {
  const unit_type = rng.pick(p.airframes.filter(a => a !== 'M-1 Abrams'))
  const weapon = unit_type === 'AH-64D_BLK_II' ? 'AGM_114K' : rng.pick(['GBU_12', 'Mk_82', 'AGM_65D'])
  const correct = rng.chance(0.88)
  const miss = correct ? Math.abs(rng.gauss(0, 9 * (1.4 - p.skill))) : rng.range(150, 600)
  const result: RangeResult = {
    kind: 'cas',
    jtac: 'Axeman (JTAC)',
    target: rng.pick(['T-72B in the treeline', 'BTR-80 at the crossroads', 'ZSU-23-4 on the ridge', 'Infantry in the compound']),
    weapon,
    time_to_impact_s: Math.round(rng.range(140, 560)),
    miss_m: r1(miss),
    correct_target: correct,
    danger_close: rng.chance(0.15),
    nearest_friendly_m: Math.round(rng.range(280, 2200)),
    laser_code: weapon === 'GBU_12' || weapon === 'AGM_114K' ? 1688 : null,
  }
  const score = !correct ? 0.5 : miss < 5 ? 5 : miss < 15 ? 4 : miss < 30 ? 3 : 2
  const tgt = stationById('range_b_array').pos
  return { ...base(p, unit_type, ts), score, result, track: { kind: 'path', paths: { [p.name]: approachPath(tgt, rng, 180, 14000, 3500) } } }
}

// ─── dispatcher ────────────────────────────────────────────────────────────

const BUILDERS: Record<ResultKind, (p: MockPilot, rng: Rng, ts: number) => RangeRecord> = {
  trap: trapRecord,
  bomb: bombRecord,
  strafe: strafeRecord,
  aar: aarRecord,
  missile: missileRecord,
  engagement: engagementRecord,
  anti_ship: antiShipRecord,
  sling: slingRecord,
  landing: landingRecord,
  troops: troopsRecord,
  gunnery: gunneryRecord,
  cas: casRecord,
}

export function randomRecord(rng: Rng, ts: number, pilot?: MockPilot): RangeRecord {
  const p = pilot ?? rng.pick(PILOTS)
  const kind = rng.weighted(p.kinds)
  return BUILDERS[kind](p, rng, ts)
}

export function recordOfKind(kind: ResultKind, p: MockPilot, rng: Rng, ts: number): RangeRecord {
  return BUILDERS[kind](p, rng, ts)
}

