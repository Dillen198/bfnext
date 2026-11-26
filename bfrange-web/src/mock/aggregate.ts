/**
 * What bfdb derives from the record store, re-derived here from the fixtures
 * so every aggregate page has consistent data: pilot profiles (with insights
 * and qualifications), the greenie board, leaderboards and impact groups.
 */
import { parseCall } from '../lib/lso'
import type {
  GreenieQuery,
  GreenieResponse,
  Insight,
  KindStats,
  Leaderboards,
  PilotProfile,
  Qualification,
  RangeRecord,
  ResultKind,
  ResultOf,
  StationImpacts,
  Summary,
  TrendPoint,
} from '../types'

type Rec<K extends ResultKind> = RangeRecord & { result: ResultOf<K> }
const isKind = <K extends ResultKind>(k: K) => (r: RangeRecord): r is Rec<K> => r.result.kind === k

const DAY = 86_400_000
export const within = (days: number | undefined, now: number) => (r: { ts: string }) =>
  !days || now - new Date(r.ts).getTime() <= days * DAY

const avg = (xs: number[]) => (xs.length ? xs.reduce((a, b) => a + b, 0) / xs.length : null)
const median = (xs: number[]) => {
  if (!xs.length) return null
  const s = [...xs].sort((a, b) => a - b)
  const m = Math.floor(s.length / 2)
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2
}

/** ISO-8601 week label, e.g. "2026-W38". */
export function isoWeek(iso: string): string {
  const d = new Date(iso)
  const t = new Date(Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), d.getUTCDate()))
  const day = t.getUTCDay() || 7
  t.setUTCDate(t.getUTCDate() + 4 - day)
  const y0 = new Date(Date.UTC(t.getUTCFullYear(), 0, 1))
  const wk = Math.ceil(((t.getTime() - y0.getTime()) / DAY + 1) / 7)
  return `${t.getUTCFullYear()}-W${String(wk).padStart(2, '0')}`
}

// ─── pilot profile ─────────────────────────────────────────────────────────

function perKind(recs: RangeRecord[]): Partial<Record<ResultKind, KindStats>> {
  const out: Partial<Record<ResultKind, KindStats>> = {}
  for (const r of recs) {
    const k = r.result.kind
    const cur = out[k] ?? { count: 0, avg_score: null, best_score: null, last_ts: null }
    cur.count++
    if (!cur.last_ts || r.ts > cur.last_ts) cur.last_ts = r.ts
    out[k] = cur
  }
  for (const k of Object.keys(out) as ResultKind[]) {
    const scores = recs.filter(r => r.result.kind === k && r.score !== null).map(r => r.score as number)
    out[k]!.avg_score = avg(scores)
    out[k]!.best_score = scores.length ? Math.max(...scores) : null
  }
  return out
}

function trend(recs: RangeRecord[]): Partial<Record<ResultKind, TrendPoint[]>> {
  const out: Partial<Record<ResultKind, TrendPoint[]>> = {}
  const by = new Map<string, number[]>()
  for (const r of recs) {
    if (r.score === null) continue
    const key = `${r.result.kind}|${isoWeek(r.ts)}`
    by.set(key, [...(by.get(key) ?? []), r.score])
  }
  for (const [key, scores] of by) {
    const [k, week] = key.split('|') as [ResultKind, string]
    ;(out[k] ??= []).push({ week, avg_score: avg(scores)!, count: scores.length })
  }
  for (const k of Object.keys(out) as ResultKind[]) out[k]!.sort((a, b) => a.week.localeCompare(b.week))
  return out
}

function insights(recs: RangeRecord[]): Insight[] {
  const out: Insight[] = []
  const newest = [...recs].sort((a, b) => b.ts.localeCompare(a.ts))

  // carrier: the call the LSO writes most often
  const traps = newest.filter(isKind('trap'))
  if (traps.length >= 3) {
    const counts = new Map<string, { n: number; text: string; ids: string[] }>()
    for (const r of traps) {
      for (const tok of r.result.lso_comment.split(/\s+/).filter(Boolean)) {
        const c = parseCall(tok)
        const key = `${c.error}|${c.position ?? ''}`
        const text = parseCall(c.position ? `${c.error}${c.position}` : c.error).text
        const e = counts.get(key) ?? { n: 0, text, ids: [] }
        e.n++
        e.ids.push(r.id)
        counts.set(key, e)
      }
    }
    const top = [...counts.values()].sort((a, b) => b.n - a.n)[0]
    if (top && top.n >= 2) {
      out.push({
        id: 'trap-common-call',
        kind: 'trap',
        severity: 'warn',
        title: `The LSO keeps calling you ${top.text}`,
        detail: `${top.n} of your last ${traps.length} passes carry this call. ${
          /high/.test(top.text) ? 'Carry a touch less power as you roll into the groove and fly the ball, not the deck.'
          : /low/.test(top.text) ? 'Add a little power earlier: a low ball in close is the most dangerous place to be.'
          : /lined up|drift|angling/.test(top.text) ? 'Cross the wake a little earlier and make small lineup corrections with rudder, not bank.'
          : /slow|fast/.test(top.text) ? 'Trim for on-speed AoA before the 90 and hold it with power, not stick.'
          : 'Watch this segment on the trap sheets below.'
        }`,
        evidence: top.ids.slice(0, 5),
      })
    }
    const graded = traps.filter(r => r.result.points !== null)
    const ok = graded.slice(0, 5).filter(r => (r.result.points ?? 0) >= 4)
    if (ok.length >= 3) {
      out.push({
        id: 'trap-streak', kind: 'trap', severity: 'good',
        title: `${ok.length} of your last 5 graded passes were OK or better`,
        detail: 'Consistency is what the greenie board rewards. Keep flying the same pattern.',
        evidence: ok.map(r => r.id),
      })
    }
    const bolters = traps.filter(r => r.result.outcome === 'bolter')
    if (bolters.length >= 2) {
      out.push({
        id: 'trap-bolters', kind: 'trap', severity: 'info',
        title: `${bolters.length} bolters on record`,
        detail: 'Most bolters come from a high, flat finish. Check the side view: the ball rising in close shows up as a line above the 3.5° slope.',
        evidence: bolters.slice(0, 4).map(r => r.id),
      })
    }
  }

  // bombing: along-track bias
  const bombs = newest.filter(isKind('bomb')).filter(r => r.result.weapon_class !== 'guided')
  if (bombs.length >= 4) {
    const longs = bombs.map(r => r.result.long_m)
    const m = avg(longs)!
    if (Math.abs(m) >= 8) {
      out.push({
        id: 'bomb-bias', kind: 'bomb', severity: 'warn',
        title: `Your unguided bombs land ${Math.abs(m).toFixed(0)} m ${m > 0 ? 'long' : 'short'} on average`,
        detail: m > 0
          ? 'A consistent long miss means releasing late or fast for the sight picture you used. Pickle a beat earlier, or check your release airspeed matches the one you planned for.'
          : 'A consistent short miss means releasing early or slow. Hold the pipper on for one more beat, or check you are not dropping below your planned release altitude.',
        evidence: bombs.slice(0, 5).map(r => r.id),
      })
    } else {
      out.push({
        id: 'bomb-bias', kind: 'bomb', severity: 'good',
        title: 'No long/short bias in your bombing',
        detail: `Your mean along-track error is ${m.toFixed(1)} m. The scatter is random, so work on repeatable release parameters rather than a correction.`,
        evidence: bombs.slice(0, 3).map(r => r.id),
      })
    }
  }

  // strafe: foul line
  const strafe = newest.filter(isKind('strafe'))
  const fouls = strafe.filter(r => r.result.foul_line_crossed)
  if (fouls.length) {
    out.push({
      id: 'strafe-foul', kind: 'strafe', severity: 'warn',
      title: `${fouls.length} strafe pass${fouls.length > 1 ? 'es' : ''} scored invalid for the foul line`,
      detail: 'Cease fire and pull off by 2,000 ft slant range. A pass inside the foul line does not count, however many rounds hit.',
      evidence: fouls.slice(0, 4).map(r => r.id),
    })
  }

  // AAR: the worst axis
  const aar = newest.filter(isKind('aar'))
  if (aar.length >= 2) {
    const lat = avg(aar.map(r => r.result.stability.lateral_sd_m))!
    const ver = avg(aar.map(r => r.result.stability.vertical_sd_m))!
    const fa = avg(aar.map(r => r.result.stability.fore_aft_sd_m))!
    const worst = [['fore-aft', fa / 2], ['lateral', lat / 1.5], ['vertical', ver / 1.5]].sort((a, b) => (b[1] as number) - (a[1] as number))[0]
    const bad = (worst[1] as number) > 1
    out.push({
      id: 'aar-stability', kind: 'aar', severity: bad ? 'warn' : 'good',
      title: bad ? `Most of your movement in contact is ${worst[0]}` : 'Stable in contact',
      detail: bad
        ? `Average spread in contact: fore-aft ${fa.toFixed(1)} m, lateral ${lat.toFixed(1)} m, vertical ${ver.toFixed(1)} m. Pick a fixed reference on the tanker and make smaller, earlier corrections.`
        : `Your spread in contact stays inside the grading limits (fore-aft ${fa.toFixed(1)} m, lateral ${lat.toFixed(1)} m, vertical ${ver.toFixed(1)} m).`,
      evidence: aar.slice(0, 4).map(r => r.id),
    })
  }

  // missile defence: reaction time
  const defended = newest.filter(isKind('missile')).filter(r => r.result.perspective === 'target')
  if (defended.length >= 2) {
    const reacts = defended.map(r => r.result.defense.reaction_s).filter((x): x is number => x !== null)
    const kills = defended.filter(r => r.result.outcome === 'kill')
    const ra = avg(reacts)
    if (ra !== null) {
      out.push({
        id: 'missile-reaction', kind: 'missile', severity: ra > 6 ? 'warn' : 'good',
        title: ra > 6 ? `You take ${ra.toFixed(1)} s to react to a launch` : `Quick reactions: ${ra.toFixed(1)} s to your first turn`,
        detail: `${kills.length} of ${defended.length} shots at you would have killed you. ${ra > 6 ? 'Turning to the beam inside 4 s of the launch call defeats most shots from outside 15 nm.' : 'Keep taking it to the beam and going low once it is inside 10 nm.'}`,
        evidence: (kills.length ? kills : defended).slice(0, 4).map(r => r.id),
      })
    }
  }

  // rotary: touchdown rate
  const landings = newest.filter(isKind('landing'))
  if (landings.length >= 3) {
    const fpm = avg(landings.map(r => r.result.touchdown_fpm))!
    if (fpm > 300) {
      out.push({
        id: 'landing-firm', kind: 'landing', severity: 'warn',
        title: `Firm touchdowns: ${fpm.toFixed(0)} fpm on average`,
        detail: 'Establish a stable hover over the spot first, then lower with collective only. Aim for under 200 fpm.',
        evidence: landings.slice(0, 4).map(r => r.id),
      })
    }
  }

  // SEAD: anti-radiation shots at dark radars; deaths to the network
  const sead = newest.filter(isKind('sead'))
  const arm = sead.filter(r => r.result.guidance === 'arm')
  const dark = arm.filter(r => !r.result.site_was_emitting)
  if (arm.length >= 3 && dark.length * 10 >= arm.length * 4) {
    out.push({
      id: 'sead_arm_dark', kind: 'sead', severity: 'warn',
      title: `${dark.length} of ${arm.length} anti-radiation kills fired at a dark radar`,
      detail: 'Those missiles flew to where the radar had been. Fire when the site is emitting (a fresh RWR spike), or use position mode deliberately and expect a lower hit rate.',
      evidence: dark.slice(0, 6).map(r => r.id),
    })
  }
  const seadDeaths = sead.reduce((a, r) => a + r.result.trainer_deaths, 0)
  if (seadDeaths >= 2) {
    out.push({
      id: 'sead_trainer_deaths', kind: 'sead', severity: 'warn',
      title: `SAMs would have killed you ${seadDeaths} times on SEAD runs`,
      detail: 'Launch from nearer the missile\'s maximum range, stay outside the engagement ring until the site is suppressed, and mask your ingress with the terrain.',
      evidence: sead.filter(r => r.result.trainer_deaths > 0).slice(0, 6).map(r => r.id),
    })
  }

  // hot zone: dying in the zone
  const hz = newest.filter(isKind('hot_zone'))
  const died = hz.filter(r => r.result.trainer_deaths > 0 || r.result.outcome === 'shot_down')
  if (hz.length >= 3 && died.length * 3 >= hz.length) {
    out.push({
      id: 'hz_dying', kind: 'hot_zone', severity: 'warn',
      title: `Killed on ${died.length} of your last ${hz.length} hot-zone sorties`,
      detail: 'Check six before committing to a ground target, ask AWACS for the picture before you push, and keep the energy to defend when the CAP comes in.',
      evidence: died.slice(0, 6).map(r => r.id),
    })
  }

  // low level: ceiling busts and time on target
  const ll = newest.filter(isKind('low_level'))
  if (ll.length >= 3) {
    const below = avg(ll.map(r => r.result.pct_below_ceiling))!
    if (below < 90) {
      out.push({
        id: 'll_ceiling', kind: 'low_level', severity: 'warn',
        title: `Above the ceiling for ${(100 - below).toFixed(0)}% of your routes`,
        detail: 'Most busts come climbing over ridgelines: cross at the saddle, unload over the top and get back down on the far side.',
        evidence: ll.slice(0, 6).map(r => r.id),
      })
    }
    const tot = avg(ll.map(r => r.result.tot_error_s))!
    if (Math.abs(tot) > 15) {
      out.push({
        id: `ll_tot_${tot > 0 ? 'late' : 'early'}`, kind: 'low_level', severity: 'warn',
        title: `Consistently ${tot > 0 ? 'late' : 'early'} on target (${tot > 0 ? '+' : ''}${tot.toFixed(0)} s average)`,
        detail: 'Hold the planned ground speed and check the gate times on each run to see where the error builds up.',
        evidence: ll.slice(0, 6).map(r => r.id),
      })
    }
  }

  // field landings: long/short and centreline
  const fl = newest.filter(isKind('field_landing')).filter(r => r.result.outcome !== 'undershoot')
  if (fl.length >= 4) {
    const aim = avg(fl.map(r => r.result.aim_error_m))!
    if (Math.abs(aim) > 100) {
      out.push({
        id: `fl_${aim > 0 ? 'long' : 'short'}`, kind: 'field_landing', severity: 'warn',
        title: `Landing ${aim > 0 ? 'long' : 'short'} of the aim point (${Math.abs(aim).toFixed(0)} m average)`,
        detail: aim > 0
          ? 'You are floating: cross the threshold at the right speed and height, then let it land rather than holding it off.'
          : 'You are low on the approach: fly the glideslope down to the aim point and do not duck under it in the last half mile.',
        evidence: fl.slice(0, 6).map(r => r.id),
      })
    }
    const cl = avg(fl.map(r => r.result.centreline_m))!
    if (Math.abs(cl) > 3) {
      out.push({
        id: `fl_centreline_${cl > 0 ? 'right' : 'left'}`, kind: 'field_landing', severity: 'warn',
        title: `Touching down ${cl > 0 ? 'right' : 'left'} of the centreline (${Math.abs(cl).toFixed(1)} m average)`,
        detail: 'Usually an uncorrected crosswind drift. Hold the centreline with rudder and into-wind aileron all the way to touchdown.',
        evidence: fl.slice(0, 6).map(r => r.id),
      })
    }
    const unstable = fl.filter(r => !r.result.stable)
    if (unstable.length * 10 >= fl.length * 4) {
      out.push({
        id: 'fl_unstable', kind: 'field_landing', severity: 'warn',
        title: `Unstable approach on ${unstable.length} of your last ${fl.length} landings`,
        detail: 'Be on speed, on glideslope and lined up by 1 nm; if you are not, go around rather than fixing it in the flare.',
        evidence: unstable.slice(0, 6).map(r => r.id),
      })
    }
  }

  // CSAR: survivors not brought home
  const cs = newest.filter(isKind('csar'))
  const notHome = cs.filter(r => r.result.outcome !== 'rescued')
  if (cs.length >= 2 && notHome.length >= 2) {
    out.push({
      id: 'csar_not_home', kind: 'csar', severity: 'warn',
      title: `${notHome.length} survivors never made it home`,
      detail: 'Tune the beacon on the ADF as soon as the MAYDAY comes in, and plan the fuel for the trip home before you go looking.',
      evidence: notHome.slice(0, 6).map(r => r.id),
    })
  }

  if (!out.length && recs.length) {
    out.push({
      id: 'keep-flying', kind: newest[0].result.kind, severity: 'info',
      title: 'Not enough graded events yet for a pattern',
      detail: 'Insights appear after three or more results in a discipline.',
      evidence: newest.slice(0, 2).map(r => r.id),
    })
  }
  return out
}

function quals(recs: RangeRecord[]): Qualification[] {
  const traps = recs.filter(isKind('trap'))
  const dayTraps = traps.filter(r => !r.result.night && r.result.outcome === 'trap')
  const nightTraps = traps.filter(r => r.result.night && r.result.outcome === 'trap')
  const dayAvg = avg(dayTraps.map(r => r.result.points ?? 0))
  const aarGood = recs.filter(isKind('aar')).filter(r => r.result.grade <= 'B')
  const boom = aarGood.filter(r => r.result.method === 'boom')
  const drogue = aarGood.filter(r => r.result.method === 'drogue')
  const bombs = recs.filter(isKind('bomb')).sort((a, b) => b.ts.localeCompare(a.ts)).slice(0, 6)
  const bombsGood = bombs.filter(r => ['GOOD', 'EXCELLENT', 'SHACK'].includes(r.result.quality))
  const strafeGood = recs.filter(isKind('strafe')).filter(r => r.result.accuracy_pct >= 75 && !r.result.foul_line_crossed)
  const defeated = recs.filter(isKind('missile')).filter(r => r.result.perspective === 'target' && r.result.outcome === 'defeated')
  const landGood = recs.filter(isKind('landing')).filter(r => ['GOOD', 'EXCELLENT', 'PERFECT'].includes(r.result.quality))
  const good = (q: string) => ['GOOD', 'EXCELLENT', 'PERFECT'].includes(q)
  const deckGood = landGood.filter(r => r.result.drill === 'ship')
  const seadUp = recs.filter(isKind('sead')).filter(r => r.result.site_was_emitting)
  const hzClean = recs.filter(isKind('hot_zone')).filter(r => r.result.outcome === 'egressed' && r.result.trainer_deaths === 0 && r.result.air_kills + r.result.ground_kills > 0)
  const llGood = recs.filter(isKind('low_level')).filter(r => good(r.result.quality) && r.result.gates_hit === r.result.gates_total)
  const flGood = recs.filter(isKind('field_landing')).filter(r => r.result.stable && r.result.outcome !== 'undershoot' && good(r.result.quality))
  const rescued = recs.filter(isKind('csar')).filter(r => r.result.outcome === 'rescued')
  const q = (id: string, name: string, description: string, have: number, need: number, extraOk = true, detail?: string): Qualification => ({
    id, name, description,
    earned: have >= need && extraOk,
    progress: Math.min(1, have / need) * (extraOk || have < need ? 1 : 0.95),
    detail: detail ?? `${Math.min(have, need)} / ${need}`,
  })
  return [
    q('cq-day', 'Day carrier qual', 'Six day traps averaging 3.0 points or better.', dayTraps.length, 6, (dayAvg ?? 0) >= 3,
      `${Math.min(dayTraps.length, 6)} / 6 traps · avg ${dayAvg === null ? '—' : dayAvg.toFixed(2)}`),
    q('cq-night', 'Night carrier qual', 'Four night traps after the day qual.', nightTraps.length, 4),
    q('aar-boom', 'Tanker qual — boom', 'Three boom sessions graded B or better.', boom.length, 3),
    q('aar-drogue', 'Tanker qual — probe & drogue', 'Three drogue sessions graded B or better.', drogue.length, 3),
    q('bomb-qual', 'Bombing qual', 'Four of your last six bombs GOOD or better.', bombsGood.length, 4),
    q('strafe-expert', 'Strafe expert', 'Three valid strafe passes at 75% or better.', strafeGood.length, 3),
    q('missile-def', 'Missile defence', 'Defeat five trainer shots without being killed.', defeated.length, 5),
    q('rotary-precision', 'Rotary precision', 'Five precision landings GOOD or better.', landGood.length, 5),
    q('deck_landing', 'Deck Qual', 'Three helicopter deck landings on a ship under way, GOOD or better.', deckGood.length, 3),
    q('sead', 'SEAD', 'Three IADS kills made while the site\'s radar was up.', seadUp.length, 3),
    q('hot_zone', 'Hot Zone', 'Three hot-zone sorties with a kill, no trainer death and a clean egress.', hzClean.length, 3),
    q('low_level', 'Low Level', 'Two low-level routes with every gate, graded GOOD or better.', llGood.length, 2),
    q('pattern', 'Pattern', 'Five stable runway landings graded GOOD or better.', flGood.length, 5),
    q('csar', 'CSAR', 'Two downed pilots found, picked up and brought home.', rescued.length, 2),
  ].filter(x => x.progress > 0 || ['cq-day', 'bomb-qual', 'aar-drogue'].includes(x.id))
}

export function pilotProfile(all: RangeRecord[], ucid: string, toSummary: (r: RangeRecord) => Summary): PilotProfile | null {
  const recs = all.filter(r => r.pilot.ucid === ucid)
  if (!recs.length) return null
  const af = new Map<string, number>()
  for (const r of recs) af.set(r.unit_type, (af.get(r.unit_type) ?? 0) + 1)
  return {
    ucid,
    name: recs[0].pilot.name,
    per_kind: perKind(recs),
    trend: trend(recs),
    insights: insights(recs),
    quals: quals(recs),
    airframes: [...af].map(([unit_type, count]) => ({ unit_type, count })).sort((a, b) => b.count - a.count),
    recent: [...recs].sort((a, b) => b.ts.localeCompare(a.ts)).slice(0, 12).map(toSummary),
  }
}

// ─── greenie board ─────────────────────────────────────────────────────────

export function greenie(all: RangeRecord[], q: GreenieQuery, now: number): GreenieResponse {
  const traps = all
    .filter(isKind('trap'))
    .filter(within(q.days, now))
    .filter(r => !q.carrier || r.result.carrier === q.carrier)
    .filter(r => !q.unit_type || r.unit_type === q.unit_type)
    .sort((a, b) => a.ts.localeCompare(b.ts))
  const rows = new Map<string, GreenieResponse['rows'][number]>()
  for (const r of traps) {
    const u = r.pilot.ucid ?? r.pilot.name
    const row = rows.get(u) ?? { ucid: u, name: r.pilot.name, avg_points: null, count: 0, passes: [] }
    row.passes.push({
      id: r.id, ts: r.ts, grade: r.result.grade, points: r.result.points, wire: r.result.wire,
      case: r.result.case, night: r.result.night, outcome: r.result.outcome, unit_type: r.unit_type,
    })
    rows.set(u, row)
  }
  for (const row of rows.values()) {
    const pts = row.passes.map(p => p.points).filter((x): x is number => x !== null)
    row.count = pts.length
    row.avg_points = avg(pts)
  }
  return { rows: [...rows.values()].sort((a, b) => (b.avg_points ?? -1) - (a.avg_points ?? -1)) }
}

// ─── leaderboards ──────────────────────────────────────────────────────────

function groupBy<T extends RangeRecord>(recs: T[]) {
  const m = new Map<string, T[]>()
  for (const r of recs) {
    const k = r.pilot.ucid ?? r.pilot.name
    m.set(k, [...(m.get(k) ?? []), r])
  }
  return m
}

export function leaderboards(all: RangeRecord[], days: number | undefined, now: number): Leaderboards {
  const recs = all.filter(within(days, now))
  const bombing = [...groupBy(recs.filter(isKind('bomb')))].map(([ucid, rs]) => ({
    ucid, name: rs[0].pilot.name, count: rs.length,
    cep_m: median(rs.map(r => r.result.miss_m))!,
    avg_score: avg(rs.map(r => r.score ?? 0))!,
  })).filter(x => x.count >= 2).sort((a, b) => a.cep_m - b.cep_m)
  const strafe = [...groupBy(recs.filter(isKind('strafe')))].map(([ucid, rs]) => ({
    ucid, name: rs[0].pilot.name, count: rs.length,
    avg_accuracy: avg(rs.filter(r => !r.result.foul_line_crossed).map(r => r.result.accuracy_pct)) ?? 0,
  })).sort((a, b) => b.avg_accuracy - a.avg_accuracy)
  const lso = [...groupBy(recs.filter(isKind('trap')))].map(([ucid, rs]) => {
    const pts = rs.map(r => r.result.points).filter((x): x is number => x !== null)
    return { ucid, name: rs[0].pilot.name, avg_points: avg(pts) ?? 0, traps: rs.filter(r => r.result.outcome === 'trap').length }
  }).sort((a, b) => b.avg_points - a.avg_points)
  const aar = [...groupBy(recs.filter(isKind('aar')))].map(([ucid, rs]) => ({
    ucid, name: rs[0].pilot.name, count: rs.length, avg_score: avg(rs.map(r => r.score ?? 0))!,
  })).sort((a, b) => b.avg_score - a.avg_score)
  // duel ELO, replayed in time order from 1500
  const elo = new Map<string, { name: string; elo: number; wins: number; losses: number }>()
  const duels = recs.filter(isKind('engagement')).filter(r => r.result.opponent).sort((a, b) => a.ts.localeCompare(b.ts))
  for (const r of duels) {
    const a = r.pilot.ucid ?? r.pilot.name
    const b = r.result.opponent!.ucid ?? r.result.opponent!.name
    const ea = elo.get(a) ?? { name: r.pilot.name, elo: 1500, wins: 0, losses: 0 }
    const eb = elo.get(b) ?? { name: r.result.opponent!.name, elo: 1500, wins: 0, losses: 0 }
    const s = r.result.outcome === 'win' ? 1 : r.result.outcome === 'loss' ? 0 : 0.5
    if (r.result.outcome === 'abort') continue
    const exp = 1 / (1 + Math.pow(10, (eb.elo - ea.elo) / 400))
    ea.elo += 32 * (s - exp)
    eb.elo -= 32 * (s - exp)
    if (s === 1) { ea.wins++; eb.losses++ } else if (s === 0) { ea.losses++; eb.wins++ }
    elo.set(a, ea); elo.set(b, eb)
  }
  const md = [...groupBy(recs.filter(isKind('missile')).filter(r => r.result.perspective === 'target'))].map(([ucid, rs]) => ({
    ucid, name: rs[0].pilot.name,
    defeated: rs.filter(r => r.result.outcome === 'defeated').length,
    killed: rs.filter(r => r.result.outcome === 'kill').length,
  })).sort((a, b) => b.defeated - b.killed - (a.defeated - a.killed))
  const sead = [...groupBy(recs.filter(isKind('sead')))].map(([ucid, rs]) => ({
    ucid, name: rs[0].pilot.name, count: rs.length,
    radar_up: rs.filter(r => r.result.site_was_emitting).length,
    sites_destroyed: rs.filter(r => r.result.site_destroyed).length,
  })).sort((a, b) => b.radar_up - a.radar_up || b.count - a.count)
  const hotZone = [...groupBy(recs.filter(isKind('hot_zone')))].map(([ucid, rs]) => ({
    ucid, name: rs[0].pilot.name, count: rs.length,
    air_kills: rs.reduce((a, r) => a + r.result.air_kills, 0),
    ground_kills: rs.reduce((a, r) => a + r.result.ground_kills, 0),
    deaths: rs.reduce((a, r) => a + r.result.trainer_deaths + (r.result.outcome === 'shot_down' ? 1 : 0), 0),
  })).sort((a, b) => b.air_kills + b.ground_kills - (a.air_kills + a.ground_kills) || a.deaths - b.deaths)
  const lowLevel = [...groupBy(recs.filter(isKind('low_level')))].map(([ucid, rs]) => {
    const scores = rs.map(r => r.score ?? 0)
    const tots = rs.filter(r => r.result.gates_hit === r.result.gates_total).map(r => Math.abs(r.result.tot_error_s))
    return {
      ucid, name: rs[0].pilot.name, count: rs.length,
      best_score: Math.max(...scores), avg_score: avg(scores), best_tot_s: tots.length ? Math.min(...tots) : null,
    }
  }).sort((a, b) => (b.best_score ?? 0) - (a.best_score ?? 0) || (a.best_tot_s ?? 1e9) - (b.best_tot_s ?? 1e9))
  const fieldLanding = [...groupBy(recs.filter(isKind('field_landing')))].filter(([, rs]) => rs.length >= 3).map(([ucid, rs]) => ({
    ucid, name: rs[0].pilot.name, count: rs.length,
    avg_score: avg(rs.map(r => r.score ?? 0)),
    avg_aim_error_m: avg(rs.filter(r => r.result.outcome !== 'undershoot').map(r => Math.abs(r.result.aim_error_m))),
    stable_pct: Math.round((rs.filter(r => r.result.stable).length / rs.length) * 100),
  })).sort((a, b) => (b.avg_score ?? 0) - (a.avg_score ?? 0))
  const deckLanding = [...groupBy(recs.filter(isKind('landing')).filter(r => r.result.drill === 'ship'))].filter(([, rs]) => rs.length >= 2).map(([ucid, rs]) => ({
    ucid, name: rs[0].pilot.name, count: rs.length,
    avg_score: avg(rs.map(r => r.score ?? 0)), avg_distance_m: avg(rs.map(r => r.result.distance_m)),
  })).sort((a, b) => (b.avg_score ?? 0) - (a.avg_score ?? 0))
  const csar = [...groupBy(recs.filter(isKind('csar')))].map(([ucid, rs]) => {
    const times = rs.filter(r => r.result.outcome === 'rescued' && r.result.time_total_s !== null).map(r => r.result.time_total_s as number)
    return { ucid, name: rs[0].pilot.name, count: rs.length, rescues: times.length, fastest_s: times.length ? Math.min(...times) : null }
  }).filter(x => x.rescues > 0).sort((a, b) => (a.fastest_s ?? 1e9) - (b.fastest_s ?? 1e9))
  return {
    bombing,
    strafe,
    lso,
    aar,
    duels: [...elo].map(([ucid, e]) => ({ ucid, name: e.name, wins: e.wins, losses: e.losses, elo: Math.round(e.elo) })).sort((a, b) => b.elo - a.elo),
    missile_defense: md,
    sead,
    hot_zone: hotZone,
    low_level: lowLevel,
    field_landing: fieldLanding,
    deck_landing: deckLanding,
    csar,
  }
}

// ─── impact group ──────────────────────────────────────────────────────────

export function stationImpacts(all: RangeRecord[], station: string, q: { days?: number; pilot?: string }, now: number): StationImpacts {
  const bombs = all
    .filter(isKind('bomb'))
    .filter(r => r.result.station_id === station)
    .filter(within(q.days, now))
    .filter(r => !q.pilot || r.pilot.ucid === q.pilot)
  const first = all.filter(isKind('bomb')).find(r => r.result.station_id === station)
  return {
    target: first?.result.target_pos ?? null,
    rings_m: first?.result.rings_m ?? [],
    impacts: bombs.map(r => ({
      id: r.id, north_m: r.result.impact_north_m, east_m: r.result.impact_east_m, miss_m: r.result.miss_m,
      weapon: r.result.weapon_display || r.result.weapon, weapon_class: r.result.weapon_class,
      quality: r.result.quality, ucid: r.pilot.ucid ?? null, name: r.pilot.name, ts: r.ts,
    })),
    cep_m: median(bombs.map(r => r.result.miss_m)),
  }
}

