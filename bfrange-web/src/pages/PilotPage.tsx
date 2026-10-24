/**
 * `/pilot/:ucid` — one pilot's range record: a scorecard per discipline,
 * where they can improve (with the result cards that show it), progress
 * toward each qualification, weekly trends, airframes and recent results.
 */
import { Link, useParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { Bar, ComposedChart, CartesianGrid, Line, ResponsiveContainer, Tooltip, XAxis, YAxis } from 'recharts'
import { Award, CheckCircle2 } from '@icons'
import { api } from '../api'
import { Panel } from '../components/Controls'
import { KindIcon } from '../components/KindIcon'
import { ResultCard } from '../components/ResultCard'
import { Empty, ErrorState, Loading } from '../components/States'
import { KIND_LABEL, TONE_VAR, airframe, fmt, fmtAgo, scoreTone, shortUcid } from '../lib/format'
import { useAuth } from '../context/AuthContext'
import type { Insight, KindStats, PilotProfile, Qualification, ResultKind, Summary, TrendPoint } from '../types'

const SEVERITY = {
  warn: { c: 'var(--ball)', label: 'WORK ON' },
  good: { c: 'var(--datum)', label: 'STRENGTH' },
  info: { c: 'var(--sky)', label: 'NOTE' },
} as const

function Scorecard({ ucid, kind, s }: { ucid: string; kind: ResultKind; s: KindStats }) {
  const tone = TONE_VAR[scoreTone(s.avg_score)]
  return (
    <Link to={`/results?pilot=${encodeURIComponent(ucid)}&kind=${kind}&days=0`} className="panel p-3 flex flex-col gap-2 hover:border-[var(--line-2)]">
      <div className="flex items-center gap-2">
        <KindIcon kind={kind} size={16} className="muted" />
        <span className="caps" style={{ color: 'var(--chalk)' }}>{KIND_LABEL[kind]}</span>
      </div>
      <div className="flex items-baseline gap-1">
        <span className="num text-[30px] font-medium leading-none" style={{ color: tone }}>{s.avg_score === null ? '—' : fmt(s.avg_score, 2)}</span>
        <span className="mono dim text-[12px]">/ 5 avg</span>
      </div>
      <div className="flex gap-3 mono text-[11.5px] muted">
        <span>{s.count} flown</span>
        <span>best {s.best_score === null ? '—' : fmt(s.best_score, 1)}</span>
        {s.last_ts && <span className="ml-auto">{fmtAgo(s.last_ts)}</span>}
      </div>
    </Link>
  )
}

function InsightItem({ i, recent }: { i: Insight; recent: Map<string, Summary> }) {
  const sev = SEVERITY[i.severity] ?? SEVERITY.info
  return (
    <li className="grid grid-cols-[3px_1fr] gap-3 py-3 border-b border-[var(--line)] last:border-b-0">
      <span style={{ background: sev.c, borderRadius: 1 }} />
      <div className="min-w-0">
        <div className="flex items-center gap-2 flex-wrap">
          <span className="chip" style={{ color: sev.c, background: `color-mix(in srgb, ${sev.c} 14%, transparent)` }}>{sev.label}</span>
          {i.kind in KIND_LABEL && <span className="caps">{KIND_LABEL[i.kind as ResultKind]}</span>}
        </div>
        <div className="font-semibold mt-1.5 text-[14.5px]">{i.title}</div>
        <p className="muted text-[13px] mt-1 mb-2">{i.detail}</p>
        {i.evidence.length > 0 && (
          <div className="flex flex-wrap gap-1.5 items-center">
            <span className="text-[11.5px] dim">Evidence:</span>
            {i.evidence.map((id, n) => {
              const s = recent.get(id)
              return (
                <Link key={id} to={`/result/${encodeURIComponent(id)}`} className="chip outline hover:text-[var(--chalk)] hover:border-[var(--haze)]">
                  {s ? new Date(s.ts).toLocaleDateString('en-GB', { day: '2-digit', month: 'short' }) : `#${n + 1}`}
                </Link>
              )
            })}
          </div>
        )}
      </div>
    </li>
  )
}

function QualItem({ q }: { q: Qualification }) {
  return (
    <li className="py-2.5 border-b border-[var(--line)] last:border-b-0">
      <div className="flex items-center gap-2">
        {q.earned ? <Award size={16} style={{ color: 'var(--ball)' }} /> : <span className="w-4" />}
        <span className="font-semibold text-[13.5px]">{q.name}</span>
        {q.earned ? (
          <span className="chip warn ml-auto"><CheckCircle2 size={11} /> QUALIFIED</span>
        ) : (
          <span className="mono text-[12px] muted ml-auto">{Math.round(q.progress * 100)}%</span>
        )}
      </div>
      <div className="text-[12px] muted mt-0.5 ml-6">{q.description}</div>
      <div className="ml-6 mt-1.5 flex items-center gap-2">
        <div className="h-1.5 flex-1 rounded-[1px]" style={{ background: 'var(--plot-grid)' }}>
          <div className="h-full rounded-[1px]" style={{ width: `${Math.max(2, q.progress * 100)}%`, background: q.earned ? 'var(--ball)' : 'var(--haze)' }} />
        </div>
        <span className="mono text-[11px] dim shrink-0">{q.detail}</span>
      </div>
    </li>
  )
}

function Trend({ kind, pts }: { kind: ResultKind; pts: TrendPoint[] }) {
  return (
    <div className="panel p-3">
      <div className="flex items-center gap-2 mb-1">
        <KindIcon kind={kind} size={14} className="muted" />
        <span className="caps" style={{ color: 'var(--chalk)' }}>{KIND_LABEL[kind]}</span>
        <span className="mono text-[11px] dim ml-auto whitespace-nowrap">weekly avg · bars = flown</span>
      </div>
      <div style={{ height: 150 }}>
        <ResponsiveContainer width="100%" height="100%">
          <ComposedChart data={pts} margin={{ top: 6, right: 4, bottom: 0, left: -18 }}>
            <CartesianGrid stroke="var(--plot-grid)" vertical={false} />
            <XAxis dataKey="week" tickFormatter={w => String(w).split('-')[1]} stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} />
            <YAxis yAxisId="s" domain={[0, 5]} ticks={[0, 1, 2, 3, 4, 5]} stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} />
            <YAxis yAxisId="c" orientation="right" hide domain={[0, (max: number) => max * 3]} />
            <Tooltip
              contentStyle={{ background: 'var(--panel)', border: '1px solid var(--line-2)', fontFamily: 'var(--font-mono)', fontSize: 12 }}
              formatter={(v, n) => (n === 'avg_score' ? [v === null ? '—' : fmt(Number(v), 2), 'avg'] : [String(v), 'flown'])}
            />
            <Bar yAxisId="c" dataKey="count" fill="var(--line-2)" isAnimationActive={false} />
            <Line yAxisId="s" type="monotone" connectNulls dataKey="avg_score" stroke="var(--ball)" strokeWidth={2} dot={{ r: 2.5, fill: 'var(--ball)' }} isAnimationActive={false} />
          </ComposedChart>
        </ResponsiveContainer>
      </div>
    </div>
  )
}

export function PilotView({ p }: { p: PilotProfile }) {
  const { me } = useAuth()
  const kinds = (Object.keys(p.per_kind) as ResultKind[]).sort((a, b) => (p.per_kind[b]!.count - p.per_kind[a]!.count))
  const total = kinds.reduce((a, k) => a + p.per_kind[k]!.count, 0)
  const recent = new Map(p.recent.map(s => [s.id, s]))
  const trends = (Object.keys(p.trend) as ResultKind[]).filter(k => (p.trend[k]?.length ?? 0) >= 2)
  const maxAf = Math.max(1, ...p.airframes.map(a => a.count))
  const isMe = me?.ucid === p.ucid

  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <div className="caps">{isMe ? 'Your range record' : 'Pilot record'}</div>
          <h1 className="display mt-1">{p.name}</h1>
          <p className="sub m-0 mt-1 mono text-[12px]">UCID {shortUcid(p.ucid)} · {total} graded events · {p.quals.filter(q => q.earned).length} qualifications</p>
        </div>
        <div className="actions">
          <Link className="btn-range" to={`/results?pilot=${encodeURIComponent(p.ucid)}&pilot_name=${encodeURIComponent(p.name)}&days=0`}>All results</Link>
        </div>
      </div>

      {kinds.length === 0 ? (
        <div className="panel"><Empty title="No graded events yet">Fly a range pass, a carrier approach or a tanker session and it will appear here.</Empty></div>
      ) : (
        <div className="grid gap-2.5 mb-5" style={{ gridTemplateColumns: 'repeat(auto-fill, minmax(190px, 1fr))' }}>
          {kinds.map(k => <Scorecard key={k} ucid={p.ucid} kind={k} s={p.per_kind[k]!} />)}
        </div>
      )}

      <div className="grid gap-4 lg:grid-cols-[minmax(0,1.3fr)_minmax(0,1fr)] mb-5">
        <Panel title="Where to improve" bodyClass="px-3">
          {p.insights.length ? (
            <ul className="m-0 p-0 list-none">{p.insights.map(i => <InsightItem key={i.id} i={i} recent={recent} />)}</ul>
          ) : (
            <Empty title="No patterns yet">Insights appear once there are three or more results in a discipline.</Empty>
          )}
        </Panel>
        <Panel title="Qualifications" bodyClass="px-3">
          {p.quals.length ? (
            <ul className="m-0 p-0 list-none">{[...p.quals].sort((a, b) => Number(b.earned) - Number(a.earned) || b.progress - a.progress).map(q => <QualItem key={q.id} q={q} />)}</ul>
          ) : (
            <Empty title="No qualifications tracked" />
          )}
        </Panel>
      </div>

      {trends.length > 0 && (
        <section className="mb-5">
          <h2 className="caps mb-2" style={{ color: 'var(--chalk)' }}>Trends</h2>
          <div className="grid gap-2.5" style={{ gridTemplateColumns: 'repeat(auto-fill, minmax(300px, 1fr))' }}>
            {trends.map(k => <Trend key={k} kind={k} pts={p.trend[k]!} />)}
          </div>
        </section>
      )}

      <div className="grid gap-4 lg:grid-cols-[280px_minmax(0,1fr)]">
        <Panel title="Airframes">
          <ul className="m-0 p-0 list-none flex flex-col gap-2">
            {p.airframes.map(a => (
              <li key={a.unit_type} className="grid grid-cols-[1fr_auto] gap-x-2 text-[13px]">
                <span>{airframe(a.unit_type)}</span>
                <span className="mono muted">{a.count}</span>
                <div className="col-span-2 h-1 rounded-[1px]" style={{ background: 'var(--plot-grid)' }}>
                  <div className="h-full" style={{ width: `${(a.count / maxAf) * 100}%`, background: 'var(--haze)' }} />
                </div>
              </li>
            ))}
          </ul>
        </Panel>
        <Panel title="Recent results">
          {p.recent.length ? (
            <div className="grid gap-1.5 md:grid-cols-2">{p.recent.map(s => <ResultCard key={s.id} s={s} showPilot={false} />)}</div>
          ) : (
            <Empty title="Nothing recent" />
          )}
        </Panel>
      </div>
    </div>
  )
}

export default function PilotPage() {
  const { ucid = '' } = useParams()
  const q = useQuery({ queryKey: ['pilot', ucid], queryFn: () => api.pilot(ucid) })
  if (q.isLoading) return <Loading label="Pulling the record" />
  if (q.error) return <div className="wrap page"><ErrorState error={q.error} retry={() => q.refetch()} /></div>
  if (!q.data) return null
  return <PilotView p={q.data} />
}
