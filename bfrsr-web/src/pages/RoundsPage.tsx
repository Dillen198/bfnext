// Round history: every campaign round this server has played, and for the
// one selected, a report of how it went -- territory over time, the kill
// exchange, the most fought-over objectives, and who carried it.
//
// ?round=<id> selects a round, so a round report can be linked.

import React, { useMemo, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { Link, useSearchParams } from 'react-router-dom'
import {
  ResponsiveContainer, AreaChart, Area, BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ReferenceLine,
} from 'recharts'
import { api, roundNo, roundTitle, type Round, type RoundReport, type RoundReportBucket, type RoundSummary } from '../api'
import { campaign } from '../config/campaign'
import QueryState from '../components/QueryState'
import PageHeader from '../components/PageHeader'
import { fmtHours } from '../lib/format'
import { Award } from '@icons'
import { useAuth } from '../context/AuthContext'

const BLUE = 'var(--blue)'
const RED = 'var(--red)'
const NEUTRAL = 'var(--text-dim)'

const label: React.CSSProperties = {
  fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase',
}
const axisTick = { fill: 'var(--text-dim)', fontSize: 10 }

/** How long a round ran, from its start to its end (or now, if it is still on). */
function duration(r: Pick<Round, 'start' | 'end'>): string {
  const end = r.end ? new Date(r.end).getTime() : Date.now()
  const hours = Math.max(0, (end - new Date(r.start).getTime()) / 3_600_000)
  if (hours < 48) return `${hours.toFixed(1)} h`
  return `${(hours / 24).toFixed(1)} days`
}

const sideColor = (w: string | null | undefined) => (w === 'Blue' ? BLUE : w === 'Red' ? RED : 'var(--text-dim)')
/** "BLUFOR WON", in the campaign's own names for the sides. */
const result = (r: Pick<Round, 'active' | 'winner'>, live = 'IN PROGRESS') =>
  r.active ? live
    : r.winner === 'Blue' ? `${campaign.blueLabel.toUpperCase()} WON`
    : r.winner === 'Red' ? `${campaign.redLabel.toUpperCase()} WON`
    : 'NO WINNER'
/** The result as the ink stamp on the report: NATO in blue ink, Soviet in red. */
function ResultStamp({ r, size }: { r: Pick<Round, 'active' | 'winner'>; size?: 'lg' | 'sm' }) {
  const ink = r.active ? ' stamp--green' : r.winner === 'Blue' ? ' stamp--blue' : r.winner === 'Red' ? '' : ' stamp--dim'
  return <span className={`stamp${ink}${size ? ` stamp--${size}` : ''}`}>{result(r, size === 'sm' ? 'Live' : 'In progress')}</span>
}
const fmtDate = (iso: string) => new Date(iso).toLocaleDateString([], { day: 'numeric', month: 'short', year: 'numeric' })

function bucketLabel(iso: string, secs: number): string {
  const d = new Date(iso)
  const day = d.toLocaleDateString([], { day: 'numeric', month: 'short' })
  return secs >= 86400 ? day : `${day} ${String(d.getHours()).padStart(2, '0')}h`
}

function Card({ title, note, children, pad = true }: { title: string; note?: React.ReactNode; children: React.ReactNode; pad?: boolean }) {
  return (
    <div className="vs-card p-0" style={{ minWidth: 0 }}>
      <div className="rsr-panel-head flex items-center gap-2 px-4 py-2.5">
        <span className="rsr-panel-title" style={{ fontSize: '0.66rem' }}>{title}</span>
        {note && <span className="ml-auto" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>{note}</span>}
      </div>
      <div className={pad ? 'p-4' : undefined}>{children}</div>
    </div>
  )
}

/** Tooltip shell: values lead, names follow, each keyed with a short line. */
function Tip({ title, rows }: { title: string; rows: { color: string; value: string; name: string }[] }) {
  return (
    <div style={{
      background: 'var(--bg-elevated-solid)', border: '1px solid var(--border-light)', borderRadius: 3,
      padding: '6px 8px', fontSize: 11, color: 'var(--text-muted)', minWidth: 130,
    }}>
      <div style={{ marginBottom: 4 }}>{title}</div>
      {rows.map(r => (
        <div key={r.name} className="flex items-center gap-2">
          <span style={{ width: 10, height: 2, background: r.color, display: 'inline-block' }} />
          <span style={{ color: 'var(--text)', fontWeight: 600 }}>{r.value}</span>
          <span>{r.name}</span>
        </div>
      ))}
    </div>
  )
}

function Legend({ items }: { items: { color: string; name: string; line?: boolean }[] }) {
  return (
    <div className="flex flex-wrap" style={{ gap: '2px 14px', marginTop: 6, fontSize: '0.62rem', color: 'var(--text-muted)' }}>
      {items.map(i => (
        <span key={i.name} className="flex items-center gap-1.5">
          <span style={{ width: i.line ? 12 : 8, height: i.line ? 2 : 8, borderRadius: i.line ? 0 : 2, background: i.color, display: 'inline-block' }} />
          {i.name}
        </span>
      ))}
    </div>
  )
}

// ── Charts ────────────────────────────────────────────────────────────────────

interface TipProps<T> { active?: boolean; payload?: ReadonlyArray<{ payload?: T }> }
type Row = RoundReportBucket & { label: string }

function TerritoryTip({ active, payload }: TipProps<Row>) {
  const p = payload?.[0]?.payload
  if (!active || !p) return null
  return <Tip title={p.label} rows={[
    { color: BLUE, value: String(p.blue_held), name: 'Blue held' },
    { color: RED, value: String(p.red_held), name: 'Red held' },
    ...(p.neutral_held ? [{ color: NEUTRAL, value: String(p.neutral_held), name: 'neutral' }] : []),
  ]} />
}

function ExchangeTip({ active, payload }: TipProps<Row>) {
  const p = payload?.[0]?.payload
  if (!active || !p) return null
  return <Tip title={p.label} rows={[
    { color: BLUE, value: `${p.blue_air} A/A · ${p.blue_ground} A/G`, name: 'by Blue' },
    { color: RED, value: `${p.red_air} A/A · ${p.red_ground} A/G`, name: 'by Red' },
  ]} />
}

function Territory({ rep }: { rep: RoundReport }) {
  const data: Row[] = rep.buckets.map(b => ({ ...b, label: bucketLabel(b.t, rep.bucket_secs) }))
  const last = rep.buckets[rep.buckets.length - 1]
  return (
    <Card title="Territory over time" note={last ? `${last.blue_held} Blue · ${last.red_held} Red of ${rep.objectives}` : undefined}>
      <ResponsiveContainer width="100%" height={190}>
        <AreaChart data={data} margin={{ left: -14, right: 6, top: 6, bottom: 0 }}>
          <CartesianGrid vertical={false} stroke="var(--border)" />
          <XAxis dataKey="label" tick={axisTick} axisLine={{ stroke: 'var(--border-light)' }} tickLine={false} minTickGap={28} />
          <YAxis tick={axisTick} axisLine={false} tickLine={false} allowDecimals={false} width={40} domain={[0, rep.objectives || 'auto']} />
          <Tooltip cursor={{ stroke: 'var(--text-dim)', strokeWidth: 1 }} content={<TerritoryTip />} />
          <Area type="stepAfter" dataKey="blue_held" stackId="t" stroke={BLUE} strokeWidth={2} fill={BLUE} fillOpacity={0.18} isAnimationActive={false} />
          <Area type="stepAfter" dataKey="red_held" stackId="t" stroke={RED} strokeWidth={2} fill={RED} fillOpacity={0.18} isAnimationActive={false} />
          <Area type="stepAfter" dataKey="neutral_held" stackId="t" stroke={NEUTRAL} strokeWidth={1} fill={NEUTRAL} fillOpacity={0.1} isAnimationActive={false} />
        </AreaChart>
      </ResponsiveContainer>
      <Legend items={[{ color: BLUE, name: 'Blue' }, { color: RED, name: 'Red' }, { color: NEUTRAL, name: 'Neutral' }]} />
      <div style={{ fontSize: '0.58rem', color: 'var(--text-dim)', marginTop: 4 }}>
        Rebuilt from the capture log; before a base's first recorded capture it is counted as the losing side's.
      </div>
    </Card>
  )
}

function Exchange({ rep }: { rep: RoundReport }) {
  // Blue's kills above the line, Red's below: the exchange reads as which
  // side of zero is bigger.
  const data = rep.buckets.map(b => ({
    ...b,
    label: bucketLabel(b.t, rep.bucket_secs),
    blue: b.blue_air + b.blue_ground,
    red: -(b.red_air + b.red_ground),
  }))
  const tot = rep.buckets.reduce((a, b) => ({
    blue: a.blue + b.blue_air + b.blue_ground, red: a.red + b.red_air + b.red_ground,
  }), { blue: 0, red: 0 })
  return (
    <Card title="Kill exchange" note={`${tot.blue} by Blue · ${tot.red} by Red`}>
      <ResponsiveContainer width="100%" height={190}>
        <BarChart data={data} stackOffset="sign" margin={{ left: -14, right: 6, top: 6, bottom: 0 }} barCategoryGap={2}>
          <CartesianGrid vertical={false} stroke="var(--border)" />
          <XAxis dataKey="label" tick={axisTick} axisLine={false} tickLine={false} minTickGap={28} />
          <YAxis tick={axisTick} axisLine={false} tickLine={false} allowDecimals={false} width={40} tickFormatter={(v: number) => String(Math.abs(v))} />
          <ReferenceLine y={0} stroke="var(--border-light)" />
          <Tooltip cursor={{ fill: 'var(--accent-glow)' }} content={<ExchangeTip />} />
          <Bar dataKey="blue" stackId="k" fill={BLUE} maxBarSize={24} radius={[4, 4, 0, 0]} isAnimationActive={false} />
          <Bar dataKey="red" stackId="k" fill={RED} maxBarSize={24} radius={[0, 0, 4, 4]} isAnimationActive={false} />
        </BarChart>
      </ResponsiveContainer>
      <Legend items={[{ color: BLUE, name: 'Kills by Blue (up)' }, { color: RED, name: 'Kills by Red (down)' }]} />
    </Card>
  )
}

function Contested({ rep }: { rep: RoundReport }) {
  const max = Math.max(1, ...rep.contested.map(c => c.captures))
  return (
    <Card title="Most contested" note="captures">
      {rep.contested.length === 0 ? (
        <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>No objective changed hands.</div>
      ) : (
        <div style={{ display: 'grid', gridTemplateColumns: 'minmax(90px, 40%) 1fr auto', gap: '6px 10px', alignItems: 'center' }}>
          {rep.contested.map(c => (
            <React.Fragment key={c.name}>
              <span style={{ fontSize: '0.68rem', color: 'var(--text)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }} title={c.name}>{c.name}</span>
              <div style={{ height: 8, background: 'var(--bg-elevated)', borderRadius: 2 }}>
                <div style={{ width: `${(c.captures / max) * 100}%`, height: '100%', background: 'var(--accent)', borderRadius: '0 4px 4px 0' }} />
              </div>
              <span className="font-mono" style={{ fontSize: '0.66rem', color: 'var(--text-muted)' }}>{c.captures}</span>
            </React.Fragment>
          ))}
        </div>
      )}
    </Card>
  )
}

function Pilots({ s }: { s: RoundSummary }) {
  const mvp = s.top[0]
  const th: React.CSSProperties = { ...label, padding: '5px 8px', textAlign: 'right', fontWeight: 600, borderBottom: '1px solid var(--border)', whiteSpace: 'nowrap' }
  const td: React.CSSProperties = { padding: '5px 8px', fontSize: '0.68rem', color: 'var(--text-muted)', textAlign: 'right', fontVariantNumeric: 'tabular-nums', borderBottom: '1px solid var(--border)' }
  return (
    <Card title="Who carried it" note="kills, captures and logistics all count" pad={false}>
      {mvp && (
        <div className="flex flex-wrap items-center gap-4 px-4 py-3" style={{ borderBottom: '1px solid var(--border)', background: 'var(--accent-glow)' }}>
          <div>
            <div style={{ ...label, color: 'var(--accent-bright)' }}>MVP</div>
            <Link to={`/pilots?ucid=${mvp.ucid}`} style={{ fontFamily: 'var(--font-display)', fontSize: '1.5rem', letterSpacing: '0.06em', color: 'var(--text)', textDecoration: 'none' }}>
              {mvp.name || mvp.ucid}
            </Link>
          </div>
          <div className="flex flex-wrap gap-4 ml-auto">
            {([['A/A', mvp.air_kills], ['A/G', mvp.ground_kills], ['Captures', mvp.captures], ['Logistics', mvp.logistics], ['Flight time', fmtHours(mvp.hours)]] as const).map(([n, v]) => (
              <div key={n}>
                <div className="rsr-fig" style={{ fontSize: '1.5rem' }}>{v}</div>
                <div style={label}>{n}</div>
              </div>
            ))}
          </div>
        </div>
      )}
      <div style={{ overflowX: 'auto' }}>
        <table style={{ width: '100%', minWidth: 460, borderCollapse: 'collapse' }}>
          <thead><tr>
            <th style={{ ...th, textAlign: 'left' }}>#</th>
            <th style={{ ...th, textAlign: 'left' }}>Pilot</th>
            <th style={th}>A/A</th><th style={th}>A/G</th><th style={th}>Cap</th><th style={th}>Logi</th><th style={th}>Deaths</th><th style={th}>Hours</th>
          </tr></thead>
          <tbody>
            {s.top.map((p, i) => (
              <tr key={p.ucid}>
                <td style={{ ...td, textAlign: 'left', color: 'var(--text-dim)' }}>{i + 1}</td>
                <td style={{ ...td, textAlign: 'left' }}>
                  <Link to={`/pilots?ucid=${p.ucid}`} style={{ color: 'var(--text)', textDecoration: 'none' }}>{p.name || p.ucid}</Link>
                </td>
                <td style={td}>{p.air_kills}</td>
                <td style={td}>{p.ground_kills}</td>
                <td style={td}>{p.captures}</td>
                <td style={td}>{p.logistics}</td>
                <td style={td}>{p.deaths}</td>
                <td style={td}>{fmtHours(p.hours)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </Card>
  )
}

// ── Report ────────────────────────────────────────────────────────────────────

function Report({ round }: { round: Round }) {
  const summary = useQuery({ queryKey: ['round-summary', round.id], queryFn: () => api.roundSummary(round.id) })
  const report = useQuery({ queryKey: ['round-report', round.id], queryFn: () => api.roundReport(round.id) })
  const s = summary.data
  const rep = report.data

  const tiles: [string, string | number][] = s ? [
    ['Pilots', s.pilots],
    ['Flight time', fmtHours(s.hours)],
    ['Air kills', s.air_kills],
    ['Ground kills', s.ground_kills],
    ['Captures', s.captures],
    ['Supply runs', s.supply_runs],
  ] : []
  // Who led each line of work, from a bfdb new enough to say.
  const leaders = s?.leaders
    ? ([
        ['Top ace', s.leaders.air, 'air kills'],
        ['Top striker', s.leaders.ground, 'ground kills'],
        ['Top capturer', s.leaders.captures, 'captures'],
        ['Top logistician', s.leaders.logistics, 'logistics jobs'],
      ] as const).flatMap(([n, l, unit]) => (l ? [[n, l, unit] as const] : []))
    : []

  return (
    <div className="space-y-4" style={{ minWidth: 0 }}>
      <div className="vs-card p-0" style={{ borderLeft: `3px solid ${round.active ? 'var(--accent)' : sideColor(round.winner)}` }}>
        <div className="flex flex-wrap items-center gap-3 px-4 py-3">
          <div style={{ minWidth: 0 }}>
            <div style={label}>Round {roundNo(round)}{round.instance ? ` · ${round.instance}` : ''}</div>
            <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.6rem', letterSpacing: '0.06em', color: 'var(--text)', lineHeight: 1.1 }}>
              {roundTitle(round)}
            </div>
            <div className="font-mono" style={{ fontSize: '0.62rem', color: 'var(--text-muted)', marginTop: 2 }}>
              {fmtDate(round.start)} → {round.end ? fmtDate(round.end) : 'now'} · {duration(round)}
            </div>
          </div>
          <div className="ml-auto" style={{ textAlign: 'right' }}>
            <div style={{ ...label, marginBottom: 6 }}>Result</div>
            <ResultStamp r={round} size="lg" />
          </div>
        </div>
        {tiles.length > 0 && (
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(100px, 1fr))', borderTop: '1px solid var(--border)' }}>
            {tiles.map(([n, v]) => (
              <div key={n} style={{ padding: '10px 16px', borderRight: '1px solid var(--border)' }}>
                <div style={label}>{n}</div>
                <div className="rsr-fig" style={{ fontSize: '1.7rem', marginTop: 4 }}>{v}</div>
              </div>
            ))}
          </div>
        )}
        {leaders.length > 0 && (
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(150px, 1fr))', borderTop: '1px solid var(--border)' }}>
            {leaders.map(([n, l, unit]) => (
              <div key={n} style={{ padding: '10px 16px', borderRight: '1px solid var(--border)', minWidth: 0 }}>
                <div style={label}>{n}</div>
                <div style={{ fontSize: '0.85rem', fontWeight: 700, color: 'var(--text)', marginTop: 2, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{l.name}</div>
                <div className="font-mono" style={{ fontSize: '0.62rem', color: 'var(--text-muted)' }}><span className="rsr-fig" style={{ fontSize: '1rem' }}>{l.value}</span> {unit}</div>
              </div>
            ))}
          </div>
        )}
      </div>

      <QueryState what="round report" isLoading={report.isLoading} isError={report.isError} error={report.error} onRetry={report.refetch} />
      {rep && (
        <>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(320px, 1fr))', gap: 16 }}>
            <Territory rep={rep} />
            <Exchange rep={rep} />
          </div>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(320px, 1fr))', gap: 16, alignItems: 'start' }}>
            {s && s.top.length > 0 ? <Pilots s={s} /> : <div />}
            <Contested rep={rep} />
          </div>
        </>
      )}
      {!report.isLoading && !rep && !report.isError && (
        <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)' }}>
          This stats server doesn't serve round reports yet; it needs the updated bfdb.
        </div>
      )}
    </div>
  )
}

/** Admins only: rename the selected round, merge it into another of its
 *  server's rounds, or delete it. bfdb refuses deleting the live round, and
 *  a merge with the live round keeps the live round's id. */
function EditRound({ round, rounds, onPick, onClose }: {
  round: Round
  rounds: Round[]
  onPick: (id: number | null) => void
  onClose: () => void
}) {
  const qc = useQueryClient()
  const [name, setName] = useState(round.name ?? '')
  const [into, setInto] = useState<number | ''>('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [done, setDone] = useState<string | null>(null)
  const others = rounds.filter(r => r.id !== round.id && (r.instance ?? '') === (round.instance ?? ''))
  const what = `Round ${roundNo(round)}${round.name || round.scenario ? ` (${round.name || round.scenario})` : ''}`

  const run = async (f: () => Promise<string>) => {
    setBusy(true)
    setError(null)
    setDone(null)
    try {
      setDone(await f())
      // Round numbers, names, leaderboards and pilot totals can all move.
      await qc.invalidateQueries()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }
  const rename = (n: string | null) => run(async () => {
    const r = await api.admin.renameRound(round.id, n)
    setName(r.name ?? '')
    return r.name ? `Renamed to "${r.name}".` : 'Name cleared; the scenario shows again.'
  })
  const merge = () => {
    const target = others.find(r => r.id === into)
    if (!target) return
    if (!window.confirm(
      `Merge ${what} into Round ${roundNo(target)}?\n\n` +
      'Their kills, captures, sorties and reports become one round. This cannot be undone.' +
      (round.active ? `\n\n${what} is live, so the merged round keeps its id.` : ''),
    )) return
    void run(async () => {
      const r = await api.admin.mergeRound(round.id, target.id)
      onPick(r.id)
      return 'Merged.'
    })
  }
  const remove = () => {
    if (!window.confirm(
      `Delete ${what}?\n\nEvery kill, capture, sortie and report in it goes, and pilots' lifetime totals lose ` +
      'what they earned in it. Later rounds are renumbered. This cannot be undone.',
    )) return
    void run(async () => {
      await api.admin.deleteRound(round.id)
      onPick(null)
      return `${what} deleted.`
    })
  }

  const small: React.CSSProperties = { fontSize: '0.66rem', padding: '4px 12px' }
  const field: React.CSSProperties = {
    background: 'var(--bg)', color: 'var(--text)', border: '1px solid var(--border)', borderRadius: 4,
    fontSize: '0.75rem', padding: '5px 8px', minWidth: 0,
  }
  const row: React.CSSProperties = {
    display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: 8, padding: '10px 16px', borderBottom: '1px solid var(--border)',
  }
  const rowLabel: React.CSSProperties = { ...label, width: 70, flexShrink: 0 }
  return (
    <div className="vs-card p-0" style={{ marginBottom: 16 }}>
      <div className="rsr-panel-head flex items-center gap-2 px-4 py-2.5">
        <span className="rsr-panel-title" style={{ fontSize: '0.66rem' }}>Edit round · Round {roundNo(round)}</span>
        {busy && <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>working…</span>}
        {error && <span role="alert" style={{ fontSize: '0.65rem', color: 'var(--red)' }}>{error}</span>}
        {done && !error && <span style={{ fontSize: '0.65rem', color: 'var(--accent)' }}>{done}</span>}
        <span className="ml-auto">
          <button type="button" className="vs-btn-ghost" style={{ ...small, padding: '4px 10px' }} onClick={onClose}>Close</button>
        </span>
      </div>
      <form style={row} onSubmit={e => { e.preventDefault(); void rename(name) }}>
        <span style={rowLabel}>Name</span>
        <input value={name} onChange={e => setName(e.target.value)} maxLength={80} aria-label="Round name"
          placeholder={round.scenario || `Round ${roundNo(round)}`} style={{ ...field, flex: '1 1 220px' }} />
        <button type="submit" className="vs-btn" style={small} disabled={busy || name.trim() === (round.name ?? '')}>SAVE</button>
        {round.name && (
          <button type="button" className="vs-btn-ghost" style={small} disabled={busy} onClick={() => void rename(null)}
            title="Show the scenario name again">CLEAR</button>
        )}
      </form>
      <div style={row}>
        <span style={rowLabel}>Merge</span>
        <select value={into} onChange={e => setInto(e.target.value ? Number(e.target.value) : '')} aria-label="Round to merge into"
          style={{ ...field, flex: '1 1 220px' }} disabled={others.length === 0}>
          <option value="">{others.length ? 'Merge this round into…' : 'No other rounds on this server'}</option>
          {others.map(r => (
            <option key={r.id} value={r.id}>
              Round {roundNo(r)} · {r.name || r.scenario} · {fmtDate(r.start)}{r.active ? ' · live' : ''}
            </option>
          ))}
        </select>
        <button type="button" className="vs-btn-outline" style={small} disabled={busy || into === ''} onClick={merge}>MERGE</button>
      </div>
      <div style={{ ...row, borderBottom: 'none' }}>
        <span style={rowLabel}>Delete</span>
        <span style={{ fontSize: '0.66rem', color: 'var(--text-dim)', flex: '1 1 220px' }}>
          {round.active
            ? 'The live round cannot be deleted; end it first, or merge an old round into it.'
            : 'Removes this round and everything recorded in it.'}
        </span>
        <button type="button" className="vs-btn-outline" style={{ ...small, color: 'var(--red)', borderColor: 'var(--red)' }}
          disabled={busy || round.active} onClick={remove}>DELETE</button>
      </div>
    </div>
  )
}

// ── Page ──────────────────────────────────────────────────────────────────────

export default function RoundsPage() {
  const q = useQuery({ queryKey: ['rounds'], queryFn: () => api.rounds() })
  const [params, setParams] = useSearchParams()
  const rounds = useMemo(() => [...(q.data ?? [])].sort((a, b) => b.start.localeCompare(a.start)), [q.data])
  const wanted = Number(params.get('round'))
  const selected = rounds.find(r => r.id === wanted) ?? rounds.find(r => r.active) ?? rounds[0]
  const pick = (id: number) => setParams(p => { p.set('round', String(id)); return p }, { replace: true })
  // After a delete there is no round to show: fall back to the live (or latest) one.
  const pickOrLatest = (id: number | null) => (id == null
    ? setParams(p => { p.delete('round'); return p }, { replace: true })
    : pick(id))
  const { user } = useAuth()
  const [editing, setEditing] = useState(false)
  const editBtn = user?.is_admin && selected
    ? (
      <button type="button" className="vs-btn-outline" onClick={() => setEditing(p => !p)} aria-pressed={editing}
        title="Rename, merge or delete this round" style={{ fontSize: '0.68rem', padding: '5px 12px' }}>
        EDIT ROUND
      </button>
    )
    : undefined

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader art="flanker" title="AFTER-ACTION REPORTS" file="After-action reports"
        stamp={<span className="stamp stamp--dim">Filed</span>}
        sub={`${rounds.length} round${rounds.length === 1 ? '' : 's'} of the campaign on file`} icon={Award} right={editBtn} />
      <div className="flex-1 overflow-auto p-4">
        <QueryState isLoading={q.isLoading} isError={q.isError} error={q.error} isEmpty={rounds.length === 0} what="rounds" onRetry={q.refetch} />
        {editing && user?.is_admin && selected && (
          <EditRound key={selected.id} round={selected} rounds={rounds} onPick={pickOrLatest} onClose={() => setEditing(false)} />
        )}
        {selected && (
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: 16, alignItems: 'flex-start' }}>
            {/* Round list: a column beside the report on wide screens, above it on narrow ones. */}
            <div className="vs-card p-0" style={{ flex: '1 1 240px', maxWidth: 300 }}>
              <div className="rsr-panel-head px-4 py-2.5"><span className="rsr-panel-title" style={{ fontSize: '0.66rem' }}>All rounds</span></div>
              <div style={{ maxHeight: 520, overflowY: 'auto' }}>
                {rounds.map(r => {
                  const on = r.id === selected.id
                  return (
                    <button key={r.id} type="button" onClick={() => pick(r.id)} aria-pressed={on}
                      style={{
                        all: 'unset', boxSizing: 'border-box', width: '100%', cursor: 'pointer', display: 'block',
                        padding: '9px 14px', borderBottom: '1px solid var(--border)',
                        borderLeft: `3px solid ${on ? 'var(--accent)' : 'transparent'}`,
                        background: on ? 'var(--bg-hover)' : undefined,
                      }}>
                      <div className="flex items-center gap-2">
                        <span style={{ fontSize: '0.72rem', color: 'var(--text)', fontWeight: 600 }}>Round {roundNo(r)}</span>
                        <span className="ml-auto" style={{ fontSize: '0.58rem', fontWeight: 700, letterSpacing: '0.1em', color: r.active ? 'var(--accent)' : sideColor(r.winner) }}>
                          {result(r, 'LIVE')}
                        </span>
                      </div>
                      <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 2, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                        {r.name || r.scenario} · {fmtDate(r.start)} · {duration(r)}
                      </div>
                    </button>
                  )
                })}
              </div>
            </div>
            <div style={{ flex: '999 1 560px', minWidth: 0 }}>
              <Report key={selected.id} round={selected} />
            </div>
          </div>
        )}
      </div>
    </div>
  )
}
