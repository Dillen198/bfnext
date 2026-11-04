import { useMemo } from 'react'
import { AreaChart, Area, XAxis, YAxis, Tooltip, ResponsiveContainer, CartesianGrid } from 'recharts'
import type { CaptureEvent } from '../api'
import { campaign } from '../config/campaign'
import { ago } from '../lib/format'
import { useDashLook } from '../lib/look'
import '../pages/levant/objectives.css'

/**
 * The round's capture history: who took what, when.
 *
 * Deliberately plots CUMULATIVE CAPTURES PER SIDE rather than a territory
 * count over time. A capture event records who took an objective, not who
 * held it beforehand, so any "objectives owned at time T" line would have to
 * guess at the previous owner of every flip -- and a guessed history drawn as
 * a solid line is worse than no history at all. Cumulative captures is
 * exactly what the data says.
 */

const fmtClock = (d: Date) =>
  d.toLocaleTimeString('en-GB', { hour: '2-digit', minute: '2-digit', hour12: false, timeZone: 'UTC' }) + 'Z'

const fmtDay = (d: Date) =>
  d.toLocaleDateString('en-GB', { day: '2-digit', month: 'short', timeZone: 'UTC' })

/** Who took it. bfdb credits a capture no pilot made (ground forces, AI)
 *  to an all-zero UCID, and falls back to the raw UCID when it has no name,
 *  so a run of zeros means "no pilot", not a callsign. */
function capturers(by: string[]): string {
  const names = by.filter(n => !/^0+$/.test(n))
  if (names.length > 0) return names.join(', ')
  return by.length > 0 ? 'no pilot credited' : 'unattributed'
}

const sideTone = (s: string) => (s === 'Blue' ? 'b' : s === 'Red' ? 'r' : 'n')

/** Levant Ablaze: the totals as readouts, the chart on charred ground, the
 *  contested bases as chips and the flips as a log with a side edge. */
function LevantTimeline({ events, series, contested, now }: {
  events: CaptureEvent[]
  series: { t: number; label: string; blue: number; red: number; objective: string }[]
  contested: [string, number][]
  now: number
}) {
  if (events.length === 0) {
    return <p className="lv-empty ob-tl-empty">No objectives have changed hands this round</p>
  }
  const blueTotal = series.length ? series[series.length - 1].blue : 0
  const redTotal = series.length ? series[series.length - 1].red : 0
  const tick = { fill: 'var(--sand-dim)', fontSize: 11, fontFamily: 'var(--ds-label)' }
  return (
    <div className="ob-tl">
      <div className="ob-tl-totals">
        <div className="ob-tl-total b"><span className="ds-k">{campaign.blueLabel} took</span><b>{blueTotal}</b></div>
        <div className="ob-tl-total r"><span className="ds-k">{campaign.redLabel} took</span><b>{redTotal}</b></div>
        <div className="ob-tl-total"><span className="ds-k">Captures · objectives</span><b>{events.length}<small> · {new Set(events.map(e => e.objective)).size}</small></b></div>
      </div>

      <div className="capture-timeline-body">
        <div style={{ minWidth: 0 }}>
          <ResponsiveContainer width="100%" height={160}>
            <AreaChart data={series} margin={{ left: -18, right: 6, top: 4 }}>
              <CartesianGrid stroke="var(--line)" strokeDasharray="2 4" vertical={false} />
              <XAxis dataKey="label" tick={tick} axisLine={false} tickLine={false} minTickGap={40} />
              <YAxis tick={tick} axisLine={false} tickLine={false} width={28} allowDecimals={false} />
              <Tooltip
                contentStyle={{
                  background: 'var(--char-2)', border: '1px solid var(--line)', borderTop: '3px solid var(--rust)',
                  borderRadius: 0, color: 'var(--sand)', fontSize: 12, fontFamily: 'var(--ds-label)',
                }}
                labelStyle={{ color: 'var(--sand-2)', letterSpacing: '0.06em' }}
                labelFormatter={(l, payload) => {
                  const o = payload?.[0]?.payload?.objective
                  return o ? `${l} — ${o}` : String(l)
                }}
              />
              <Area type="stepAfter" dataKey="blue" name={campaign.blueLabel} stroke={campaign.blueColor}
                fill={campaign.blueColor} fillOpacity={0.16} strokeWidth={1.8} isAnimationActive={false} />
              <Area type="stepAfter" dataKey="red" name={campaign.redLabel} stroke={campaign.redColor}
                fill={campaign.redColor} fillOpacity={0.16} strokeWidth={1.8} isAnimationActive={false} />
            </AreaChart>
          </ResponsiveContainer>

          {contested.length > 0 && (
            <div className="ob-tl-contested">
              <span className="ds-k">Most contested</span>
              <div>
                {contested.map(([name, count]) => (
                  <span key={name} className="ob-tl-chip">{name}<b>×{count}</b></span>
                ))}
              </div>
            </div>
          )}
        </div>

        <ol className="capture-timeline-log ob-tl-log">
          {events.slice(0, 20).map((e, i) => {
            const d = new Date(e.time)
            return (
              <li key={`${e.time}-${e.objective}-${i}`} className={sideTone(e.side)}>
                <time>{fmtClock(d)}</time>
                <strong>{e.objective}</strong>
                <span className="by">{capturers(e.by)}</span>
                <span className="ago">{ago(d, now)}</span>
              </li>
            )
          })}
        </ol>
      </div>
    </div>
  )
}

export default function CaptureTimeline({ events }: { events: CaptureEvent[] }) {
  const look = useDashLook()
  const now = Date.now()

  // bfdb hands these back newest-first; the chart needs the other order.
  const chrono = useMemo(
    () => [...events].sort((a, b) => +new Date(a.time) - +new Date(b.time)),
    [events],
  )

  const series = useMemo(() => {
    let blue = 0
    let red = 0
    return chrono.map((e) => {
      if (e.side === 'Blue') blue++
      else if (e.side === 'Red') red++
      const d = new Date(e.time)
      return { t: +d, label: `${fmtDay(d)} ${fmtClock(d)}`, blue, red, objective: e.objective }
    })
  }, [chrono])

  // Objectives that changed hands more than once are where the round was
  // actually fought -- far more interesting than a flat count of captures.
  const contested = useMemo(() => {
    const n: Record<string, number> = {}
    for (const e of events) n[e.objective] = (n[e.objective] ?? 0) + 1
    return Object.entries(n)
      .filter(([, c]) => c > 1)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 6)
  }, [events])

  if (look.profile) return <LevantTimeline events={events} series={series} contested={contested} now={now} />

  if (events.length === 0) {
    return (
      <div style={{ padding: '2rem 1rem', textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.75rem' }}>
        No objectives have changed hands this round
      </div>
    )
  }

  const blueTotal = series.length ? series[series.length - 1].blue : 0
  const redTotal = series.length ? series[series.length - 1].red : 0

  return (
    <div style={{ padding: '14px 16px 16px' }}>
      {/* Totals */}
      <div style={{ display: 'flex', gap: 18, marginBottom: 10, fontFamily: 'var(--font-mono)', fontSize: '0.65rem' }}>
        <span style={{ color: campaign.blueColor, fontWeight: 700 }}>
          {campaign.blueLabel} {blueTotal}
        </span>
        <span style={{ color: campaign.redColor, fontWeight: 700 }}>
          {campaign.redLabel} {redTotal}
        </span>
        <span style={{ color: 'var(--text-dim)' }}>
          {events.length} captures · {new Set(events.map(e => e.objective)).size} objectives
        </span>
      </div>

      <div className="capture-timeline-body">
      <div style={{ minWidth: 0 }}>
      <ResponsiveContainer width="100%" height={150}>
        <AreaChart data={series} margin={{ left: -18, right: 6, top: 4 }}>
          <CartesianGrid stroke="var(--border)" strokeDasharray="2 4" vertical={false} />
          <XAxis
            dataKey="label" tick={{ fill: 'var(--text-dim)', fontSize: 9 }}
            axisLine={false} tickLine={false} minTickGap={40}
          />
          <YAxis tick={{ fill: 'var(--text-dim)', fontSize: 10 }} axisLine={false} tickLine={false} width={28} allowDecimals={false} />
          <Tooltip
            contentStyle={{
              background: 'var(--bg-elevated)', border: '1px solid var(--border-light)',
              borderRadius: 4, color: 'var(--text)', fontSize: 12,
            }}
            labelFormatter={(l, payload) => {
              const o = payload?.[0]?.payload?.objective
              return o ? `${l} — ${o}` : String(l)
            }}
          />
          <Area type="stepAfter" dataKey="blue" name={campaign.blueLabel} stroke={campaign.blueColor}
            fill={campaign.blueColor} fillOpacity={0.13} strokeWidth={1.6} isAnimationActive={false} />
          <Area type="stepAfter" dataKey="red" name={campaign.redLabel} stroke={campaign.redColor}
            fill={campaign.redColor} fillOpacity={0.13} strokeWidth={1.6} isAnimationActive={false} />
        </AreaChart>
      </ResponsiveContainer>

      {contested.length > 0 && (
        <div style={{ marginTop: 10 }}>
          <div style={{
            fontFamily: 'var(--font-mono)', fontSize: '0.58rem', letterSpacing: '0.16em',
            textTransform: 'uppercase', color: 'var(--text-dim)', marginBottom: 5,
          }}>
            Most contested
          </div>
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: '4px 6px' }}>
            {contested.map(([name, count]) => (
              <span key={name} style={{
                fontFamily: 'var(--font-mono)', fontSize: '0.62rem', padding: '1px 6px',
                border: '1px solid var(--border)', color: 'var(--text-muted)',
              }}>
                {name} <span style={{ color: 'var(--yellow)' }}>×{count}</span>
              </span>
            ))}
          </div>
        </div>
      )}

      </div>

      {/* Recent flips, newest first */}
      <div className="capture-timeline-log">
        {events.slice(0, 20).map((e, i) => {
          const d = new Date(e.time)
          const col = e.side === 'Blue' ? campaign.blueColor : campaign.redColor
          return (
            <div
              key={`${e.time}-${e.objective}-${i}`}
              style={{
                display: 'flex', alignItems: 'baseline', gap: 8, padding: '3px 0',
                borderBottom: '1px solid var(--border)', fontSize: '0.66rem',
              }}
            >
              <span style={{ fontFamily: 'var(--font-mono)', color: 'var(--text-dim)', fontSize: '0.6rem', flexShrink: 0 }}>
                {fmtClock(d)}
              </span>
              <span style={{ color: col, fontWeight: 700, flexShrink: 0 }}>{e.objective}</span>
              <span style={{ color: 'var(--text-muted)', flex: 1, minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                {capturers(e.by)}
              </span>
              <span style={{ fontFamily: 'var(--font-mono)', color: 'var(--text-dim)', fontSize: '0.58rem', flexShrink: 0 }}>
                {ago(d, now)}
              </span>
            </div>
          )
        })}
      </div>
      </div>
    </div>
  )
}
