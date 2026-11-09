// Career overview for the Pilots page: how a pilot's sorties ended, when they
// fly, how much, what they fly, and how their kills add up -- all from the
// per-sortie rows bfdb serves at /pilot/:ucid/sorties, under one time-range
// filter.
//
// Colours were run through the dataviz palette validator against the dark and
// light card surfaces. A/A and A/G are one step darker than the page's text
// blue and orange, which sit too light for chart marks on the dark theme.

import React, { useCallback, useMemo, useRef, useState } from 'react'
import {
  ResponsiveContainer, BarChart, Bar, LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip,
} from 'recharts'
import type { PilotSortie, SortieOutcome } from '../api'
import { fmtHours } from '../lib/format'
import { OUTCOME, outcomeOf } from '../lib/sortie'
import { Activity } from '@icons'
import { Card as LvCard } from '../components/Levant'
import { useDashLook } from '../lib/look'
import './levant/pilots.css'

const AIR = '#3b82f6'
const GROUND = '#ea580c'

/** Chart marks in each look. The desert look draws from the campaign palette
 *  (A/A in the side blue, A/G in rust, flight time in coyote); its tick
 *  type and tooltip come from levant/pilots.css. */
const PLAIN_C = {
  air: AIR, ground: GROUND, bar: 'var(--accent)', grid: 'var(--border)', gridDash: undefined as string | undefined,
  axis: 'var(--border-light)', cursorFill: 'rgba(148,163,184,0.08)', cursorLine: 'var(--text-dim)',
  dotRing: 'var(--bg-card-solid)', endLabel: 'var(--text-muted)', barRadius: [4, 4, 0, 0] as [number, number, number, number],
}
const DESERT_C: typeof PLAIN_C = {
  air: 'var(--blue)', ground: 'var(--rust-bright)', bar: 'var(--coyote)', grid: 'var(--line)', gridDash: '3 3',
  axis: 'var(--border-light)', cursorFill: 'rgba(227,210,173,0.06)', cursorLine: 'var(--sand-dim)',
  dotRing: 'var(--char-2)', endLabel: 'var(--sand-2)', barRadius: [0, 0, 0, 0],
}
const DESERT_TICK = { fill: 'var(--sand-dim)', fontSize: 11 }
const DAY_MS = 86_400_000

const RANGES = [
  { key: '12w', label: '12 weeks', weeks: 12 },
  { key: '26w', label: '26 weeks', weeks: 26 },
  { key: '1y',  label: '1 year',   weeks: 52 },
  { key: 'all', label: 'All',      weeks: null },
] as const
type RangeKey = typeof RANGES[number]['key']

// The heatmap stays legible up to a year of columns.
const HEATMAP_MAX_WEEKS = 53

const OUTCOME_ORDER: SortieOutcome[] = ['landed', 'no_landing', 'lost', 'killed']

/** Local midnight of the Monday starting `t`'s week. */
function weekStart(t: number): number {
  const d = new Date(t)
  d.setHours(0, 0, 0, 0)
  d.setDate(d.getDate() - ((d.getDay() + 6) % 7))
  return d.getTime()
}

function dayStart(t: number): number {
  const d = new Date(t)
  d.setHours(0, 0, 0, 0)
  return d.getTime()
}

/** Steps a week forward in calendar days, so DST shifts don't drift it. */
function nextWeek(t: number): number {
  const d = new Date(t)
  d.setDate(d.getDate() + 7)
  return d.getTime()
}

const fmtDay = (t: number) => new Date(t).toLocaleDateString([], { weekday: 'short', day: 'numeric', month: 'short' })
const fmtWeek = (t: number) => new Date(t).toLocaleDateString([], { day: 'numeric', month: 'short' })

interface Week { t: number; label: string; sorties: number; hours: number; air: number; ground: number; airCum: number; groundCum: number }

// ── Shared chrome ─────────────────────────────────────────────────────────────

const sectionLabel: React.CSSProperties = {
  fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase',
}
const axisTick = { fill: 'var(--text-dim)', fontSize: 10 }

function Section({ title, note, desert, children }: { title: string; note?: React.ReactNode; desert?: boolean; children: React.ReactNode }) {
  if (desert) {
    return (
      <section className="pl-ch-sec">
        <div className="pl-ch-sec-head">
          <span className="ds-k">{title}</span>
          {note && <span className="lv-sub">{note}</span>}
        </div>
        {children}
      </section>
    )
  }
  return (
    <div>
      <div className="flex items-baseline gap-2" style={{ marginBottom: 8 }}>
        <span style={sectionLabel}>{title}</span>
        {note && <span className="ml-auto" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>{note}</span>}
      </div>
      {children}
    </div>
  )
}

/** Tooltip: values lead, series names follow, each keyed with a short line. */
// Recharts clones the `content` element with these filled in.
interface TipProps {
  active?: boolean
  label?: string | number
  payload?: ReadonlyArray<{ dataKey?: unknown; name?: unknown; value?: unknown; color?: string }>
  unit?: (key: string, v: number) => string
  desert?: boolean
}

function ChartTip({ active, payload, label, unit, desert }: TipProps) {
  if (!active || !payload?.length) return null
  if (desert) {
    return (
      <div className="pl-tip">
        <span className="ds-k">Week of {label}</span>
        {payload.map(p => (
          <div key={String(p.dataKey)} className="pl-tip-row">
            <i style={{ background: p.color }} />
            <b>{unit ? unit(String(p.dataKey), Number(p.value)) : String(p.value)}</b>
            <span>{String(p.name)}</span>
          </div>
        ))}
      </div>
    )
  }
  return (
    <div style={{
      background: 'var(--bg-elevated-solid)', border: '1px solid var(--border-light)', borderRadius: 3,
      padding: '6px 8px', fontSize: 11, color: 'var(--text-muted)', minWidth: 120,
    }}>
      <div style={{ marginBottom: 4 }}>Week of {label}</div>
      {payload.map(p => (
        <div key={String(p.dataKey)} className="flex items-center gap-2">
          <span style={{ width: 10, height: 2, background: p.color, display: 'inline-block' }} />
          <span style={{ color: 'var(--text)', fontWeight: 600 }}>
            {unit ? unit(String(p.dataKey), Number(p.value)) : String(p.value)}
          </span>
          <span>{String(p.name)}</span>
        </div>
      ))}
    </div>
  )
}

// ── Outcome mix ───────────────────────────────────────────────────────────────

function OutcomeMix({ sorties, desert }: { sorties: PilotSortie[]; desert?: boolean }) {
  const counts = useMemo(() => {
    const c: Record<SortieOutcome, number> = { landed: 0, no_landing: 0, lost: 0, killed: 0 }
    for (const s of sorties) c[outcomeOf(s)]++
    return c
  }, [sorties])
  const total = sorties.length
  const shown = OUTCOME_ORDER.filter(o => counts[o] > 0)
  const name = (o: SortieOutcome) => OUTCOME[o].label.replace(/^\S+\s/, '')
  const share = (o: SortieOutcome) => (total ? `${Math.round((counts[o] / total) * 100)}%` : '')

  if (desert) {
    return (
      <Section desert title="How sorties ended" note={`${total} sorties`}>
        <div className="pl-mix" role="img" aria-label={shown.map(o => `${name(o)} ${counts[o]}`).join(', ')}>
          {shown.map(o => (
            <i key={o} className={`pl-o-${o}`} style={{ flex: counts[o] }} title={`${name(o)}: ${counts[o]} (${share(o)})`} />
          ))}
        </div>
        <dl className="pl-mix-key">
          {OUTCOME_ORDER.map(o => (
            <div key={o} className={`pl-o-${o}`}>
              <dt>{name(o)}</dt>
              <dd><b>{counts[o]}</b><span>{share(o)}</span></dd>
            </div>
          ))}
        </dl>
      </Section>
    )
  }

  return (
    <Section title="How sorties ended" note={`${total} sorties`}>
      {/* 100% bar, 2px surface gaps between segments. */}
      <div className="flex" style={{ height: 14, gap: 2 }} role="img"
        aria-label={shown.map(o => `${OUTCOME[o].label.replace(/^\S+\s/, '')} ${counts[o]}`).join(', ')}>
        {shown.map((o, i) => (
          <div key={o} title={`${OUTCOME[o].label.replace(/^\S+\s/, '')}: ${counts[o]} (${Math.round((counts[o] / total) * 100)}%)`}
            style={{
              flex: counts[o], minWidth: 3, background: OUTCOME[o].color,
              borderRadius: `${i === 0 ? 4 : 0}px ${i === shown.length - 1 ? 4 : 0}px ${i === shown.length - 1 ? 4 : 0}px ${i === 0 ? 4 : 0}px`,
            }} />
        ))}
      </div>
      <div className="flex flex-wrap" style={{ gap: '4px 16px', marginTop: 8 }}>
        {OUTCOME_ORDER.map(o => (
          <span key={o} className="flex items-center gap-1.5" style={{ fontSize: '0.66rem', color: 'var(--text-muted)' }}>
            <span style={{ width: 8, height: 8, borderRadius: 2, background: OUTCOME[o].color, display: 'inline-block' }} />
            {OUTCOME[o].label.replace(/^\S+\s/, '')}
            <span className="font-mono" style={{ color: 'var(--text)' }}>{counts[o]}</span>
            <span style={{ color: 'var(--text-dim)' }}>{total ? `${Math.round((counts[o] / total) * 100)}%` : ''}</span>
          </span>
        ))}
      </div>
    </Section>
  )
}

// ── Activity heatmap ──────────────────────────────────────────────────────────

// Sequential, one hue: the accent at rising strength. Index 0 is "no flying".
const HEAT = ['var(--border)', 'rgba(106,171,31,0.28)', 'rgba(106,171,31,0.5)', 'rgba(106,171,31,0.75)', 'rgba(106,171,31,1)']

// Below this a cell stops reading as a day; on a narrow card the heatmap shows
// fewer, more recent weeks instead of shrinking further.
const HEAT_MIN_CELL = 10
const HEAT_MAX_CELL = 16
const HEAT_GAP = 2
const HEAT_LABEL_W = 26

// The desert look's shades: rust at rising strength on the charred cell.
const HEAT_DESERT = ['var(--char-3)', 'rgba(200,100,47,0.3)', 'rgba(200,100,47,0.52)', 'rgba(200,100,47,0.76)', 'var(--rust-bright)']

function Heatmap({ sorties, from, to, desert }: { sorties: PilotSortie[]; from: number; to: number; desert?: boolean }) {
  const heat = desert ? HEAT_DESERT : HEAT
  const [hover, setHover] = useState<number | null>(null)
  const [width, setWidth] = useState(0)
  const observer = useRef<ResizeObserver | null>(null)
  const measure = useCallback((el: HTMLDivElement | null) => {
    observer.current?.disconnect()
    observer.current = null
    if (!el) return
    observer.current = new ResizeObserver(() => setWidth(el.clientWidth))
    observer.current.observe(el)
  }, [])
  const fit = width > 0
    ? Math.max(4, Math.floor((width - HEAT_LABEL_W + HEAT_GAP) / (HEAT_MIN_CELL + HEAT_GAP)))
    : HEATMAP_MAX_WEEKS

  const { days, weeks, max, clipped } = useMemo(() => {
    const days = new Map<number, { sorties: number; secs: number }>()
    for (const s of sorties) {
      const k = dayStart(Date.parse(s.takeoff))
      const e = days.get(k) ?? { sorties: 0, secs: 0 }
      e.sorties++
      e.secs += Math.max(0, s.duration_secs)
      days.set(k, e)
    }
    const all: number[] = []
    for (let w = weekStart(from); w <= to; w = nextWeek(w)) all.push(w)
    const weeks = all.slice(-Math.min(HEATMAP_MAX_WEEKS, fit))
    let max = 0
    for (const [k, v] of days) if (k >= weeks[0]) max = Math.max(max, v.sorties)
    return { days, weeks, max, clipped: weeks.length < all.length }
  }, [sorties, from, to, fit])

  // Shaded by sorties, not hours: most sorties end without a recorded
  // landing, so their flight time is unknown. Four steps of the busiest day.
  const level = (n: number) => (n === 0 ? 0 : Math.min(4, Math.max(1, Math.ceil((n / max) * 4))))
  const today = dayStart(to)
  const activeDays = [...days.keys()].filter(k => k >= weeks[0]).length
  const hovered = hover !== null ? { t: hover, ...(days.get(hover) ?? { sorties: 0, secs: 0 }) } : null
  const small: React.CSSProperties = { fontSize: '0.52rem', color: 'var(--text-dim)', lineHeight: 1, whiteSpace: 'nowrap' }

  return (
    <Section title="Activity" desert={desert}
      note={`${activeDays} day${activeDays === 1 ? '' : 's'} flown${clipped ? ` · last ${weeks.length} weeks` : ''}`}>
      <div ref={measure} className={desert ? 'pl-heat' : undefined}>
        {/* One grid for labels and cells, so the weekday labels sit on their rows. */}
        <div role="img" aria-label={`Sorties per day, ${activeDays} days flown`}
          onMouseLeave={() => setHover(null)}
          style={{
            display: 'grid', gap: HEAT_GAP, alignItems: 'center',
            gridTemplateColumns: `${HEAT_LABEL_W - HEAT_GAP}px repeat(${weeks.length}, minmax(0, ${HEAT_MAX_CELL}px))`,
            gridTemplateRows: `12px repeat(7, auto)`,
          }}>
          {['Mon', '', 'Wed', '', 'Fri', '', 'Sun'].map((d, i) => (
            <span key={`d${i}`} style={{ ...small, gridColumn: 1, gridRow: i + 2 }}>{d}</span>
          ))}
          {weeks.map((w, i) => {
            const m = new Date(w).getMonth()
            const show = i === 0 ? new Date(w).getDate() <= 7 : new Date(weeks[i - 1]).getMonth() !== m
            return show
              ? <span key={`m${w}`} style={{ ...small, gridColumn: i + 2, gridRow: 1, overflow: 'visible' }}>
                  {new Date(w).toLocaleDateString([], { month: 'short' })}
                </span>
              : null
          })}
          {weeks.flatMap((w, wi) => Array.from({ length: 7 }, (_, d) => {
            const t = new Date(w).setDate(new Date(w).getDate() + d)
            const place = { gridColumn: wi + 2, gridRow: d + 2, aspectRatio: '1' }
            if (t > today) return <span key={t} style={place} />
            const v = days.get(t)
            return (
              <span key={t}
                onMouseEnter={() => setHover(t)}
                title={`${fmtDay(t)}: ${v ? `${v.sorties} sortie${v.sorties === 1 ? '' : 's'}${v.secs > 0 ? `, ${fmtHours(v.secs / 3600)}` : ''}` : 'no flying'}`}
                style={{
                  ...place, borderRadius: desert ? 0 : 2, background: heat[level(v?.sorties ?? 0)],
                  outline: hover === t ? `1px solid ${desert ? 'var(--sand)' : 'var(--text-muted)'}` : undefined,
                }} />
            )
          }))}
        </div>
      </div>
      <div className={`flex flex-wrap items-center gap-2${desert ? ' pl-heat-foot' : ''}`} style={{ marginTop: 6, fontSize: '0.6rem', color: 'var(--text-dim)', minHeight: 14 }}>
        <span style={{ color: 'var(--text-muted)' }}>
          {hovered
            ? `${fmtDay(hovered.t)} — ${hovered.sorties ? `${hovered.sorties} sortie${hovered.sorties === 1 ? '' : 's'}${hovered.secs > 0 ? `, ${fmtHours(hovered.secs / 3600)}` : ''}` : 'no flying'}`
            : 'Shade = sorties that day'}
        </span>
        <span className="ml-auto flex items-center gap-1">
          Less
          {heat.map((c, i) => <span key={i} style={{ width: 9, height: 9, borderRadius: desert ? 0 : 2, background: c, display: 'inline-block' }} />)}
          More
        </span>
      </div>
    </Section>
  )
}

// ── Weekly charts ─────────────────────────────────────────────────────────────

function HoursPerWeek({ weeks, desert }: { weeks: Week[]; desert?: boolean }) {
  const C = desert ? DESERT_C : PLAIN_C
  const tick = desert ? DESERT_TICK : axisTick
  return (
    <Section title="Hours per week" note="known flight time only" desert={desert}>
      <ResponsiveContainer width="100%" height={150}>
        <BarChart data={weeks} margin={{ left: -18, right: 4, top: 6, bottom: 0 }} barCategoryGap={2}>
          <CartesianGrid vertical={false} stroke={C.grid} strokeDasharray={C.gridDash} />
          <XAxis dataKey="label" tick={tick} axisLine={{ stroke: C.axis }} tickLine={false} minTickGap={16} />
          <YAxis tick={tick} axisLine={false} tickLine={false} allowDecimals={false} width={40} />
          <Tooltip cursor={{ fill: C.cursorFill }}
            content={<ChartTip desert={desert} unit={(k, v) => k === 'hours' ? fmtHours(v) : String(v)} />} />
          <Bar dataKey="hours" name="flown" fill={C.bar} maxBarSize={24} radius={C.barRadius} />
        </BarChart>
      </ResponsiveContainer>
    </Section>
  )
}

function KillsOverTime({ weeks, desert }: { weeks: Week[]; desert?: boolean }) {
  const C = desert ? DESERT_C : PLAIN_C
  const tick = desert ? DESERT_TICK : axisTick
  const last = weeks.length - 1
  const end = weeks[last]
  // A direct label at each line's end, unless the two would sit on top of each other.
  const apart = end ? Math.abs(end.airCum - end.groundCum) >= Math.max(1, Math.max(end.airCum, end.groundCum) * 0.12) : false
  const endLabel = (key: 'airCum' | 'groundCum') =>
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (p: any) => (apart && p.index === last
      ? <text x={p.x + 6} y={p.y} dy={3} fontSize={10} fill={C.endLabel} className={desert ? 'pl-ch-end' : undefined}>{weeks[last][key]}</text>
      : <g />)

  return (
    <Section title="Kills over time" note={end ? `${end.airCum} A/A · ${end.groundCum} A/G` : undefined} desert={desert}>
      <ResponsiveContainer width="100%" height={150}>
        <LineChart data={weeks} margin={{ left: -18, right: 24, top: 6, bottom: 0 }}>
          <CartesianGrid vertical={false} stroke={C.grid} strokeDasharray={C.gridDash} />
          <XAxis dataKey="label" tick={tick} axisLine={{ stroke: C.axis }} tickLine={false} minTickGap={16} />
          <YAxis tick={tick} axisLine={false} tickLine={false} allowDecimals={false} width={40} />
          <Tooltip cursor={{ stroke: C.cursorLine, strokeWidth: 1 }} content={<ChartTip desert={desert} />} />
          <Line type="stepAfter" dataKey="airCum" name="air-to-air to date" stroke={C.air} strokeWidth={2} dot={false}
            activeDot={{ r: 4, stroke: C.dotRing, strokeWidth: 2 }} label={endLabel('airCum')} isAnimationActive={false} />
          <Line type="stepAfter" dataKey="groundCum" name="air-to-ground to date" stroke={C.ground} strokeWidth={2} dot={false}
            activeDot={{ r: 4, stroke: C.dotRing, strokeWidth: 2 }} label={endLabel('groundCum')} isAnimationActive={false} />
        </LineChart>
      </ResponsiveContainer>
      {desert ? (
        <div className="pl-ch-key">
          <span><i style={{ background: C.air }} />Air-to-air</span>
          <span><i style={{ background: C.ground }} />Air-to-ground</span>
        </div>
      ) : (
        <div className="flex gap-4" style={{ marginTop: 4, fontSize: '0.62rem', color: 'var(--text-muted)' }}>
          <span className="flex items-center gap-1.5"><span style={{ width: 12, height: 2, background: AIR, display: 'inline-block' }} />Air-to-air</span>
          <span className="flex items-center gap-1.5"><span style={{ width: 12, height: 2, background: GROUND, display: 'inline-block' }} />Air-to-ground</span>
        </div>
      )}
    </Section>
  )
}

function WeekTable({ weeks, desert }: { weeks: Week[]; desert?: boolean }) {
  const th: React.CSSProperties = { ...sectionLabel, padding: '4px 8px', textAlign: 'right', borderBottom: '1px solid var(--border)', fontWeight: 600 }
  const td: React.CSSProperties = { padding: '3px 8px', fontSize: '0.66rem', color: 'var(--text-muted)', textAlign: 'right', fontVariantNumeric: 'tabular-nums', borderBottom: '1px solid var(--border)' }
  const rows = [...weeks].reverse()
  if (desert) {
    return (
      <div className="pl-wtab">
        <table>
          <thead><tr>
            <th scope="col" className="l">Week of</th><th scope="col">Sorties</th><th scope="col">Hours</th><th scope="col">A/A</th><th scope="col">A/G</th>
          </tr></thead>
          <tbody>
            {rows.map(w => (
              <tr key={w.t} className={w.sorties ? undefined : 'idle'}>
                <td className="l">{w.label}</td>
                <td>{w.sorties}</td>
                <td>{w.hours > 0 ? fmtHours(w.hours) : '—'}</td>
                <td className={w.air ? 'a' : undefined}>{w.air}</td>
                <td className={w.ground ? 'g' : undefined}>{w.ground}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    )
  }
  return (
    <div style={{ maxHeight: 260, overflowY: 'auto' }}>
      <table style={{ width: '100%', borderCollapse: 'collapse' }}>
        <thead><tr>
          <th style={{ ...th, textAlign: 'left' }}>Week of</th><th style={th}>Sorties</th><th style={th}>Hours</th><th style={th}>A/A</th><th style={th}>A/G</th>
        </tr></thead>
        <tbody>
          {rows.map(w => (
            <tr key={w.t}>
              <td style={{ ...td, textAlign: 'left' }}>{w.label}</td>
              <td style={td}>{w.sorties}</td>
              <td style={td}>{w.hours > 0 ? fmtHours(w.hours) : '—'}</td>
              <td style={td}>{w.air}</td>
              <td style={td}>{w.ground}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

// ── Airframes ─────────────────────────────────────────────────────────────────

const AIRFRAME_ROWS = 8

function Airframes({ sorties, desert }: { sorties: PilotSortie[]; desert?: boolean }) {
  const rows = useMemo(() => {
    const m = new Map<string, { sorties: number; secs: number; kills: number; survived: number }>()
    for (const s of sorties) {
      const e = m.get(s.aircraft) ?? { sorties: 0, secs: 0, kills: 0, survived: 0 }
      e.sorties++
      e.secs += Math.max(0, s.duration_secs)
      e.kills += (s.air_kills ?? 0) + (s.ground_kills ?? 0)
      const o = outcomeOf(s)
      if (o !== 'killed' && o !== 'lost') e.survived++
      m.set(s.aircraft, e)
    }
    const all = [...m.entries()].map(([name, v]) => ({ name, ...v })).sort((a, b) => b.sorties - a.sorties || b.secs - a.secs)
    if (all.length <= AIRFRAME_ROWS) return all
    // Fold the tail into one row rather than a long list of one-sortie types.
    const tail = all.slice(AIRFRAME_ROWS - 1)
    return [...all.slice(0, AIRFRAME_ROWS - 1), tail.reduce((acc, r) => ({
      name: `Other (${tail.length})`, sorties: acc.sorties + r.sorties, secs: acc.secs + r.secs,
      kills: acc.kills + r.kills, survived: acc.survived + r.survived,
    }), { name: '', sorties: 0, secs: 0, kills: 0, survived: 0 })]
  }, [sorties])
  const max = Math.max(...rows.map(r => r.sorties), 1)
  // Both need the per-sortie stats server; an older one can't tell a
  // shoot-down from a despawn, so "survived" would always read 100%.
  const hasKills = sorties.some(s => s.air_kills !== undefined)
  const hasOutcome = sorties.some(s => s.outcome !== undefined)
  const th: React.CSSProperties = { ...sectionLabel, padding: '4px 6px', textAlign: 'right', fontWeight: 600, whiteSpace: 'nowrap' }
  const td: React.CSSProperties = { padding: '4px 6px', fontSize: '0.66rem', color: 'var(--text-muted)', textAlign: 'right', fontVariantNumeric: 'tabular-nums', whiteSpace: 'nowrap' }

  if (desert) {
    return (
      <Section desert title="Airframes" note="by sorties flown">
        <div className="pl-wtab pl-af">
          <table>
            <thead><tr>
              <th scope="col" className="l">Aircraft</th>
              <th scope="col" className="l bar">Sorties</th>
              <th scope="col" title="Known flight time only">Hours</th>
              {hasKills && <th scope="col">Kills</th>}
              {hasOutcome && <th scope="col" title="Sorties not ending in a shoot-down or crash">Survived</th>}
            </tr></thead>
            <tbody>
              {rows.map((r, i) => (
                <tr key={r.name} className={i === 0 ? 'first' : undefined}>
                  <td className="l nm" title={r.name}>{r.name}</td>
                  <td className="l bar">
                    <div><span><i style={{ width: `${Math.max(1, (r.sorties / max) * 100)}%` }} /></span><b>{r.sorties}</b></div>
                  </td>
                  <td>{r.secs > 0 ? fmtHours(r.secs / 3600) : '—'}</td>
                  {hasKills && <td>{r.kills}</td>}
                  {hasOutcome && <td>{Math.round((r.survived / r.sorties) * 100)}%</td>}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </Section>
    )
  }

  return (
    <Section title="Airframes" note="by sorties flown">
      <div style={{ overflowX: 'auto' }}>
        <table style={{ width: '100%', borderCollapse: 'collapse' }}>
          <thead><tr>
            <th style={{ ...th, textAlign: 'left' }}>Aircraft</th>
            <th style={{ ...th, textAlign: 'left', width: '40%' }}>Sorties</th>
            <th style={th} title="Known flight time only">Hours</th>
            {hasKills && <th style={th}>Kills</th>}
            {hasOutcome && <th style={th} title="Sorties not ending in a shoot-down or crash">Survived</th>}
          </tr></thead>
          <tbody>
            {rows.map(r => (
              <tr key={r.name}>
                <td style={{ ...td, textAlign: 'left', color: 'var(--text)', maxWidth: 140, overflow: 'hidden', textOverflow: 'ellipsis' }} title={r.name}>{r.name}</td>
                <td style={{ ...td, textAlign: 'left' }}>
                  <div className="flex items-center gap-2">
                    <div style={{ flex: 1, minWidth: 40 }}>
                      <div style={{ width: `${Math.max(1, (r.sorties / max) * 100)}%`, height: 8, background: 'var(--accent)', borderRadius: '0 4px 4px 0' }} />
                    </div>
                    <span style={{ minWidth: 24, textAlign: 'right' }}>{r.sorties}</span>
                  </div>
                </td>
                <td style={td}>{r.secs > 0 ? fmtHours(r.secs / 3600) : '—'}</td>
                {hasKills && <td style={td}>{r.kills}</td>}
                {hasOutcome && <td style={td}>{Math.round((r.survived / r.sorties) * 100)}%</td>}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </Section>
  )
}

// ── Card ──────────────────────────────────────────────────────────────────────

export default function PilotCharts({ sorties }: { sorties: PilotSortie[] }) {
  const look = useDashLook()
  const [range, setRange] = useState<RangeKey>('26w')
  const [asTable, setAsTable] = useState(false)
  // Fixed at mount: the weekly buckets hang off it, and a pilot's page is not
  // left open across a week boundary often enough to matter.
  const [now] = useState(() => Date.now())
  const r = RANGES.find(x => x.key === range)!
  const oldest = sorties.reduce((m, s) => Math.min(m, Date.parse(s.takeoff)), now)
  const from = r.weeks === null ? weekStart(oldest) : weekStart(now - (r.weeks - 1) * 7 * DAY_MS)

  const inRange = useMemo(() => sorties.filter(s => Date.parse(s.takeoff) >= from), [sorties, from])
  const hasKills = sorties.some(s => s.air_kills !== undefined)

  const weeks = useMemo<Week[]>(() => {
    const out: Week[] = []
    const idx = new Map<number, number>()
    for (let w = from; w <= now; w = nextWeek(w)) {
      idx.set(w, out.length)
      out.push({ t: w, label: fmtWeek(w), sorties: 0, hours: 0, air: 0, ground: 0, airCum: 0, groundCum: 0 })
    }
    for (const s of inRange) {
      const i = idx.get(weekStart(Date.parse(s.takeoff)))
      if (i === undefined) continue
      out[i].sorties++
      out[i].hours += Math.max(0, s.duration_secs) / 3600
      out[i].air += s.air_kills ?? 0
      out[i].ground += s.ground_kills ?? 0
    }
    let a = 0, g = 0
    for (const w of out) { a += w.air; g += w.ground; w.airCum = a; w.groundCum = g }
    return out
  }, [inRange, from, now])

  if (sorties.length === 0) return null

  const btn = (active: boolean): React.CSSProperties => ({
    background: active ? 'var(--bg-hover)' : 'none', border: '1px solid', borderColor: active ? 'var(--border-light)' : 'transparent',
    color: active ? 'var(--text)' : 'var(--text-dim)', borderRadius: 2, padding: '2px 8px', fontSize: '0.6rem',
    letterSpacing: '0.08em', cursor: 'pointer', whiteSpace: 'nowrap',
  })

  if (look.profile) {
    return (
      <LvCard tape="Career overview">
        {/* One filter row; it scopes every chart below. */}
        <div className="pl-ch-filter">
          <div className="pl-seg" role="group" aria-label="Time range">
            {RANGES.map(x => (
              <button key={x.key} type="button" aria-pressed={range === x.key} onClick={() => setRange(x.key)}>{x.label}</button>
            ))}
          </div>
          <button type="button" className="pl-seg-one" aria-pressed={asTable} onClick={() => setAsTable(v => !v)}>
            {asTable ? 'Charts' : 'Table'}
          </button>
        </div>
        <div className="pl-ch-body">
          {inRange.length === 0 ? (
            <p className="lv-empty">No sorties in the last {r.label}.</p>
          ) : (
            <>
              <OutcomeMix sorties={inRange} desert />
              {asTable ? (
                <Section desert title="By week"><WeekTable weeks={weeks} desert /></Section>
              ) : (
                <>
                  <Heatmap sorties={inRange} from={from} to={now} desert />
                  <div className="pl-ch-pair">
                    <HoursPerWeek weeks={weeks} desert />
                    {hasKills
                      ? <KillsOverTime weeks={weeks} desert />
                      : (
                        <Section desert title="Kills over time">
                          <p className="pl-none">Per-sortie kills need the updated stats server.</p>
                        </Section>
                      )}
                  </div>
                </>
              )}
              <Airframes sorties={inRange} desert />
            </>
          )}
        </div>
      </LvCard>
    )
  }

  return (
    <div className="vs-card p-0">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Activity size={12} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
          Career Overview
        </span>
      </div>

      {/* One filter row; it scopes every chart below. */}
      <div className="flex flex-wrap items-center gap-1 px-4 pt-3" role="group" aria-label="Time range">
        {RANGES.map(x => (
          <button key={x.key} type="button" aria-pressed={range === x.key} style={btn(range === x.key)} onClick={() => setRange(x.key)}>
            {x.label}
          </button>
        ))}
        <button type="button" aria-pressed={asTable} className="ml-auto" style={btn(asTable)} onClick={() => setAsTable(v => !v)}>
          {asTable ? 'Charts' : 'Table'}
        </button>
      </div>

      <div className="p-4" style={{ display: 'flex', flexDirection: 'column', gap: 20 }}>
        {inRange.length === 0 ? (
          <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)', textAlign: 'center', padding: '1rem 0' }}>
            No sorties in the last {r.label}.
          </div>
        ) : (
          <>
            <OutcomeMix sorties={inRange} />
            {asTable ? (
              <Section title="By week"><WeekTable weeks={weeks} /></Section>
            ) : (
              <>
                <Heatmap sorties={inRange} from={from} to={now} />
                <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(260px, 1fr))', gap: 20 }}>
                  <HoursPerWeek weeks={weeks} />
                  {hasKills
                    ? <KillsOverTime weeks={weeks} />
                    : (
                      <Section title="Kills over time">
                        <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>
                          Per-sortie kills need the updated stats server.
                        </div>
                      </Section>
                    )}
                </div>
              </>
            )}
            <Airframes sorties={inRange} />
          </>
        )}
      </div>
    </div>
  )
}
