// Round history: every campaign round this server has played, and for the
// one selected, a report of how it went -- territory over time, the kill
// exchange, the most fought-over objectives, and who carried it.
//
// ?round=<id> selects a round, so a round report can be linked. Export PDF
// saves the page header and the selected round's report, as drawn, on one
// PDF page (the round list and buttons left out); admins also
// get the text of a Discord post to go with it (lib/roundPost.ts).

import React, { useMemo, useRef, useState } from 'react'
import { useIsFetching, useQuery, type UseQueryResult } from '@tanstack/react-query'
import { Link, useSearchParams } from 'react-router-dom'
import {
  ResponsiveContainer, AreaChart, Area, BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ReferenceLine,
} from 'recharts'
import { api, roundNo, type Round, type RoundReport, type RoundReportBucket, type RoundSummary } from '../api'
import { campaign } from '../config/campaign'
import QueryState from '../components/QueryState'
import PageHeader from '../components/PageHeader'
import { fmtHours } from '../lib/format'
import { useDashLook } from '../lib/look'
import { Card as LvCard, Tape } from '../components/Levant'
import { Award, Download } from '@icons'
import { NO_PDF, saveElementPdf } from '../lib/pdfSnapshot'
import { roundPost } from '../lib/roundPost'
import { useAuth } from '../context/AuthContext'
import './levant/rounds.css'

const BLUE = 'var(--blue)'
const RED = 'var(--red)'
const NEUTRAL = '#64748b'

/** The colours and names a chart keys its sides with, in each look. */
interface Sides { blue: string; red: string; neutral: string; blueName: string; redName: string }
const PLAIN: Sides = { blue: BLUE, red: RED, neutral: NEUTRAL, blueName: 'Blue', redName: 'Red' }
const DESERT: Sides = {
  blue: campaign.blueColor, red: campaign.redColor, neutral: 'var(--sand-dim)',
  blueName: campaign.blueLabel, redName: campaign.redLabel,
}
/** The neutral band's fill in the desert look: the tape's dark coyote. */
const DESERT_NEUTRAL_FILL = '#4a3c2b'

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
const fmtDate = (iso: string) => new Date(iso).toLocaleDateString([], { day: 'numeric', month: 'short', year: 'numeric' })

function bucketLabel(iso: string, secs: number): string {
  const d = new Date(iso)
  const day = d.toLocaleDateString([], { day: 'numeric', month: 'short' })
  return secs >= 86400 ? day : `${day} ${String(d.getHours()).padStart(2, '0')}h`
}

function Card({ title, note, children, pad = true }: { title: string; note?: React.ReactNode; children: React.ReactNode; pad?: boolean }) {
  return (
    <div className="vs-card p-0" style={{ minWidth: 0 }}>
      <div className="flex items-center gap-2 px-4 pt-3.5 pb-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>{title}</span>
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

type DesertTip<T> = TipProps<T> & { desert?: boolean }

function TerritoryTip({ active, payload, desert }: DesertTip<Row>) {
  const p = payload?.[0]?.payload
  if (!active || !p) return null
  const k = desert ? DESERT : PLAIN
  const rows = [
    { color: k.blue, value: String(p.blue_held), name: `${k.blueName} held` },
    { color: k.red, value: String(p.red_held), name: `${k.redName} held` },
    ...(p.neutral_held ? [{ color: k.neutral, value: String(p.neutral_held), name: 'neutral' }] : []),
  ]
  return desert ? <LvTip title={p.label} rows={rows} /> : <Tip title={p.label} rows={rows} />
}

function ExchangeTip({ active, payload, desert }: DesertTip<Row>) {
  const p = payload?.[0]?.payload
  if (!active || !p) return null
  const k = desert ? DESERT : PLAIN
  const rows = [
    { color: k.blue, value: `${p.blue_air} A/A · ${p.blue_ground} A/G`, name: `by ${k.blueName}` },
    { color: k.red, value: `${p.red_air} A/A · ${p.red_ground} A/G`, name: `by ${k.redName}` },
  ]
  return desert ? <LvTip title={p.label} rows={rows} /> : <Tip title={p.label} rows={rows} />
}

/** The report's buckets, labelled for the x axis. */
const territoryData = (rep: RoundReport): Row[] => rep.buckets.map(b => ({ ...b, label: bucketLabel(b.t, rep.bucket_secs) }))

/** Blue's kills above the line, Red's below: the exchange reads as which side of zero is bigger. */
const exchangeData = (rep: RoundReport) => rep.buckets.map(b => ({
  ...b,
  label: bucketLabel(b.t, rep.bucket_secs),
  blue: b.blue_air + b.blue_ground,
  red: -(b.red_air + b.red_ground),
}))

const exchangeTotals = (rep: RoundReport) => rep.buckets.reduce((a, b) => ({
  blue: a.blue + b.blue_air + b.blue_ground, red: a.red + b.red_air + b.red_ground,
}), { blue: 0, red: 0 })

function Territory({ rep }: { rep: RoundReport }) {
  const data: Row[] = territoryData(rep)
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
  const data = exchangeData(rep)
  const tot = exchangeTotals(rep)
  return (
    <Card title="Kill exchange" note={`${tot.blue} by Blue · ${tot.red} by Red`}>
      <ResponsiveContainer width="100%" height={190}>
        <BarChart data={data} stackOffset="sign" margin={{ left: -14, right: 6, top: 6, bottom: 0 }} barCategoryGap={2}>
          <CartesianGrid vertical={false} stroke="var(--border)" />
          <XAxis dataKey="label" tick={axisTick} axisLine={false} tickLine={false} minTickGap={28} />
          <YAxis tick={axisTick} axisLine={false} tickLine={false} allowDecimals={false} width={40} tickFormatter={(v: number) => String(Math.abs(v))} />
          <ReferenceLine y={0} stroke="var(--border-light)" />
          <Tooltip cursor={{ fill: 'rgba(148,163,184,0.08)' }} content={<ExchangeTip />} />
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
              <div style={{ height: 8, background: 'rgba(148,163,184,0.12)', borderRadius: 2 }}>
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
  const td: React.CSSProperties = { padding: '5px 8px', fontSize: '0.68rem', color: 'var(--text-muted)', textAlign: 'right', fontVariantNumeric: 'tabular-nums', borderBottom: '1px solid rgba(42,42,42,0.5)' }
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
                <div style={{ fontSize: '1rem', fontWeight: 700, color: 'var(--text)' }}>{v}</div>
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

/** The round's headline numbers. */
function summaryTiles(s: RoundSummary | null | undefined): [string, string | number][] {
  return s ? [
    ['Pilots', s.pilots],
    ['Flight time', fmtHours(s.hours)],
    ['Air kills', s.air_kills],
    ['Ground kills', s.ground_kills],
    ['Captures', s.captures],
    ['Supply runs', s.supply_runs],
  ] : []
}

/** Who led each line of work, from a bfdb new enough to say. */
function summaryLeaders(s: RoundSummary | null | undefined) {
  return s?.leaders
    ? ([
        ['Top ace', s.leaders.air, 'air kills'],
        ['Top striker', s.leaders.ground, 'ground kills'],
        ['Top capturer', s.leaders.captures, 'captures'],
        ['Top logistician', s.leaders.logistics, 'logistics jobs'],
      ] as const).flatMap(([n, l, unit]) => (l ? [[n, l, unit] as const] : []))
    : []
}

function Report({ round, ref }: { round: Round; ref?: React.Ref<HTMLDivElement> }) {
  const look = useDashLook()
  const summary = useQuery({ queryKey: ['round-summary', round.id], queryFn: () => api.roundSummary(round.id) })
  const report = useQuery({ queryKey: ['round-report', round.id], queryFn: () => api.roundReport(round.id) })
  const s = summary.data
  const rep = report.data

  const tiles = summaryTiles(s)
  const leaders = summaryLeaders(s)

  if (look.profile) return <LvReport ref={ref} round={round} s={s} report={report} tiles={tiles} leaders={leaders} />

  return (
    <div ref={ref} className="space-y-4" style={{ minWidth: 0 }}>
      <div className="vs-card p-0" style={{ borderLeft: `3px solid ${round.active ? 'var(--accent)' : sideColor(round.winner)}` }}>
        <div className="flex flex-wrap items-center gap-3 px-4 py-3">
          <div style={{ minWidth: 0 }}>
            <div style={label}>Round {roundNo(round)}{round.instance ? ` · ${round.instance}` : ''}</div>
            <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.6rem', letterSpacing: '0.06em', color: 'var(--text)', lineHeight: 1.1 }}>
              {round.scenario || `Round ${roundNo(round)}`}
            </div>
            <div className="font-mono" style={{ fontSize: '0.62rem', color: 'var(--text-muted)', marginTop: 2 }}>
              {fmtDate(round.start)} → {round.end ? fmtDate(round.end) : 'now'} · {duration(round)}
            </div>
          </div>
          <div className="ml-auto" style={{ textAlign: 'right' }}>
            <div style={label}>Result</div>
            <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.4rem', letterSpacing: '0.08em', color: round.active ? 'var(--accent-bright)' : sideColor(round.winner) }}>
              {result(round)}
            </div>
          </div>
        </div>
        {tiles.length > 0 && (
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(100px, 1fr))', borderTop: '1px solid var(--border)' }}>
            {tiles.map(([n, v]) => (
              <div key={n} style={{ padding: '10px 16px', borderRight: '1px solid var(--border)' }}>
                <div style={label}>{n}</div>
                <div style={{ fontSize: '1.05rem', fontWeight: 700, color: 'var(--text)', marginTop: 2 }}>{v}</div>
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
                <div className="font-mono" style={{ fontSize: '0.62rem', color: 'var(--text-muted)' }}>{l.value} {unit}</div>
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

// ── Levant Ablaze ─────────────────────────────────────────────────────────────
// The same report drawn in the campaign's desert look (levant/rounds.css):
// charred cards with their names on tape, numbers in heavy Oswald, the
// winner in its side's colour. The plain (olive) look above is unchanged.

const tone = (w: string | null | undefined): 'b' | 'r' | 'n' => (w === 'Blue' ? 'b' : w === 'Red' ? 'r' : 'n')
const sideName = (w: string | null | undefined) => (w === 'Blue' ? campaign.blueLabel : w === 'Red' ? campaign.redLabel : 'No winner')
const lvTick = { fill: 'var(--sand-dim)', fontSize: 11 }

function Live() {
  return <span className="lv-live"><i />Live</span>
}

function LvTip({ title, rows }: { title: string; rows: { color: string; value: string; name: string }[] }) {
  return (
    <div className="rd-tip">
      <span className="ds-k">{title}</span>
      {rows.map(r => (
        <div key={r.name} className="rd-tip-row">
          <i style={{ background: r.color }} />
          <b>{r.value}</b>
          <span>{r.name}</span>
        </div>
      ))}
    </div>
  )
}

function LvLegend({ items }: { items: { color: string; name: string }[] }) {
  return (
    <div className="rd-legend">
      {items.map(i => <span key={i.name}><i style={{ background: i.color }} />{i.name}</span>)}
    </div>
  )
}

/** One side's number above a chart, in its colour. */
function SideSum({ side, v, unit }: { side: 'b' | 'r'; v: number; unit: string }) {
  return (
    <span className={`rd-sum-i ${side}`}>
      <b>{v}</b>
      <span>{side === 'b' ? campaign.blueLabel : campaign.redLabel}{unit ? ` ${unit}` : ''}</span>
    </span>
  )
}

function LvRoundRow({ r, on, onPick }: { r: Round; on: boolean; onPick: () => void }) {
  const t = tone(r.winner)
  return (
    <button type="button" className={`rd-row ${r.active ? 'live' : t}${on ? ' on' : ''}`} onClick={onPick} aria-pressed={on}>
      <span className="rd-no">{roundNo(r)}</span>
      <span className="rd-row-main">
        <span className="rd-row-top">
          <strong>{r.scenario || `Round ${roundNo(r)}`}</strong>
          {r.active ? <Live /> : <Tape tone={t} className="sm">{sideName(r.winner)}</Tape>}
        </span>
        <span className="rd-dates">{fmtDate(r.start)} → {r.end ? fmtDate(r.end) : 'now'} · {duration(r)}</span>
      </span>
    </button>
  )
}

function LvTerritory({ rep }: { rep: RoundReport }) {
  const data = territoryData(rep)
  const last = rep.buckets[rep.buckets.length - 1]
  return (
    <LvCard tape="Territory over time" bodyClass="rd-chart">
      {last && (
        <div className="rd-sum">
          <SideSum side="b" v={last.blue_held} unit="" />
          <SideSum side="r" v={last.red_held} unit="" />
          <span className="rd-sum-of">of {rep.objectives} objectives</span>
        </div>
      )}
      <ResponsiveContainer width="100%" height={200}>
        <AreaChart data={data} margin={{ left: -14, right: 6, top: 6, bottom: 0 }}>
          <CartesianGrid vertical={false} stroke="var(--line)" strokeDasharray="3 4" />
          <XAxis dataKey="label" tick={lvTick} axisLine={{ stroke: 'var(--border-light)' }} tickLine={false} minTickGap={28} />
          <YAxis tick={lvTick} axisLine={false} tickLine={false} allowDecimals={false} width={40} domain={[0, rep.objectives || 'auto']} />
          <Tooltip cursor={{ stroke: 'var(--sand-dim)', strokeWidth: 1, strokeDasharray: '3 3' }} content={<TerritoryTip desert />} />
          <Area type="stepAfter" dataKey="blue_held" stackId="t" stroke={DESERT.blue} strokeWidth={2} fill={DESERT.blue} fillOpacity={0.2} isAnimationActive={false} />
          <Area type="stepAfter" dataKey="red_held" stackId="t" stroke={DESERT.red} strokeWidth={2} fill={DESERT.red} fillOpacity={0.2} isAnimationActive={false} />
          <Area type="stepAfter" dataKey="neutral_held" stackId="t" stroke={DESERT.neutral} strokeWidth={1} fill={DESERT_NEUTRAL_FILL} fillOpacity={0.5} isAnimationActive={false} />
        </AreaChart>
      </ResponsiveContainer>
      <LvLegend items={[{ color: DESERT.blue, name: campaign.blueLabel }, { color: DESERT.red, name: campaign.redLabel }, { color: DESERT_NEUTRAL_FILL, name: 'Neutral' }]} />
      <p className="rd-fine">Rebuilt from the capture log; before a base&apos;s first recorded capture it is counted as the losing side&apos;s.</p>
    </LvCard>
  )
}

function LvExchange({ rep }: { rep: RoundReport }) {
  const data = exchangeData(rep)
  const tot = exchangeTotals(rep)
  return (
    <LvCard tape="Kill exchange" bodyClass="rd-chart">
      <div className="rd-sum">
        <SideSum side="b" v={tot.blue} unit="kills" />
        <SideSum side="r" v={tot.red} unit="kills" />
      </div>
      <ResponsiveContainer width="100%" height={200}>
        <BarChart data={data} stackOffset="sign" margin={{ left: -14, right: 6, top: 6, bottom: 0 }} barCategoryGap={2}>
          <CartesianGrid vertical={false} stroke="var(--line)" strokeDasharray="3 4" />
          <XAxis dataKey="label" tick={lvTick} axisLine={false} tickLine={false} minTickGap={28} />
          <YAxis tick={lvTick} axisLine={false} tickLine={false} allowDecimals={false} width={40} tickFormatter={(v: number) => String(Math.abs(v))} />
          <ReferenceLine y={0} stroke="var(--sand-dim)" />
          <Tooltip cursor={{ fill: 'rgba(216,199,160,0.06)' }} content={<ExchangeTip desert />} />
          <Bar dataKey="blue" stackId="k" fill={DESERT.blue} maxBarSize={24} isAnimationActive={false} />
          <Bar dataKey="red" stackId="k" fill={DESERT.red} maxBarSize={24} isAnimationActive={false} />
        </BarChart>
      </ResponsiveContainer>
      <LvLegend items={[{ color: DESERT.blue, name: `Kills by ${campaign.blueLabel} (up)` }, { color: DESERT.red, name: `Kills by ${campaign.redLabel} (down)` }]} />
    </LvCard>
  )
}

function LvContested({ rep }: { rep: RoundReport }) {
  const max = Math.max(1, ...rep.contested.map(c => c.captures))
  return (
    <LvCard tape="Most contested" bodyClass="rd-cont-body">
      {rep.contested.length === 0 ? (
        <p className="lv-empty">No objective changed hands.</p>
      ) : (
        <>
          <span className="ds-k rd-cont-k">Captures</span>
          <ol className="rd-cont">
            {rep.contested.map((c, i) => (
              <li key={c.name} className={i === 0 ? 'first' : undefined}>
                <span className="nm" title={c.name}>{c.name}</span>
                <span className="bar"><i style={{ width: `${(c.captures / max) * 100}%` }} /></span>
                <b>{c.captures}</b>
              </li>
            ))}
          </ol>
        </>
      )}
    </LvCard>
  )
}

function LvPilots({ s }: { s: RoundSummary }) {
  const mvp = s.top[0]
  return (
    <LvCard tape="Who carried it" tone="rust" bodyClass="rd-pilots">
      {mvp && (
        <div className="rd-mvp">
          <div className="rd-mvp-id">
            <Tape tone="rust" className="sm">MVP</Tape>
            <Link className="rd-mvp-name" to={`/pilots?ucid=${mvp.ucid}`}>{mvp.name || mvp.ucid}</Link>
          </div>
          <dl className="rd-mvp-stats">
            {([['A/A', mvp.air_kills], ['A/G', mvp.ground_kills], ['Captures', mvp.captures], ['Logistics', mvp.logistics], ['Flight time', fmtHours(mvp.hours)]] as const).map(([n, v]) => (
              <div key={n}><dt>{n}</dt><dd>{v}</dd></div>
            ))}
          </dl>
        </div>
      )}
      <div className="rd-table-wrap">
        <table className="rd-table">
          <thead><tr>
            <th className="l">#</th>
            <th className="l">Pilot</th>
            <th>A/A</th><th>A/G</th><th>Cap</th><th>Logi</th><th>Deaths</th><th>Hours</th>
          </tr></thead>
          <tbody>
            {s.top.map((p, i) => (
              <tr key={p.ucid} className={i === 0 ? 'first' : undefined}>
                <td className="l rk">{i + 1}</td>
                <td className="l who"><Link to={`/pilots?ucid=${p.ucid}`}>{p.name || p.ucid}</Link></td>
                <td>{p.air_kills}</td>
                <td>{p.ground_kills}</td>
                <td>{p.captures}</td>
                <td>{p.logistics}</td>
                <td>{p.deaths}</td>
                <td>{fmtHours(p.hours)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <p className="rd-fine">Kills, captures and logistics all count.</p>
    </LvCard>
  )
}

function LvReport({ ref, round, s, report, tiles, leaders }: {
  ref?: React.Ref<HTMLDivElement>
  round: Round
  s?: RoundSummary | null
  report: UseQueryResult<RoundReport | null>
  tiles: ReturnType<typeof summaryTiles>
  leaders: ReturnType<typeof summaryLeaders>
}) {
  const rep = report.data
  const t = tone(round.winner)
  return (
    <div ref={ref} className="rd-report">
      <LvCard
        tape={<>Round {roundNo(round)}{round.instance ? ` · ${round.instance}` : ''}</>}
        tone={round.active ? 'rust' : t === 'n' ? undefined : t}
        right={round.active ? <Live /> : undefined}
        className={`rd-hero${round.active ? ' live' : ''}`}>
        <div className="rd-hero-top">
          <div className="rd-hero-id">
            <span className="ds-k">Scenario</span>
            <h2 className="rd-hero-name">{round.scenario || `Round ${roundNo(round)}`}</h2>
            <div className="rd-hero-dates">
              <b>{fmtDate(round.start)}</b> → <b>{round.end ? fmtDate(round.end) : 'now'}</b>
              <span> · {duration(round)}</span>
            </div>
          </div>
          <div className={`rd-hero-result ${round.active ? 'live' : t}`}>
            <span className="ds-k">Result</span>
            <strong>{round.active ? 'In progress' : sideName(round.winner)}</strong>
            {!round.active && round.winner && <span className="rd-won">won the round</span>}
          </div>
        </div>
        {tiles.length > 0 && (
          <dl className="rd-readouts">
            {tiles.map(([n, v]) => (
              <div key={n} className="lv-readout"><dt className="ds-k">{n}</dt><dd className="lv-v">{v}</dd></div>
            ))}
          </dl>
        )}
        {leaders.length > 0 && (
          <dl className="rd-leaders">
            {leaders.map(([n, l, unit]) => (
              <div key={n}>
                <dt className="ds-k">{n}</dt>
                <dd><strong title={l.name}>{l.name}</strong><span className="lv-sub">{l.value} {unit}</span></dd>
              </div>
            ))}
          </dl>
        )}
      </LvCard>

      <QueryState what="round report" isLoading={report.isLoading} isError={report.isError} error={report.error} onRetry={report.refetch} />
      {rep && (
        <>
          <div className="rd-grid">
            <LvTerritory rep={rep} />
            <LvExchange rep={rep} />
          </div>
          <div className="rd-grid start">
            {s && s.top.length > 0 ? <LvPilots s={s} /> : <div />}
            <LvContested rep={rep} />
          </div>
        </>
      )}
      {!report.isLoading && !rep && !report.isError && (
        <p className="lv-empty">This stats server does not serve round reports yet; it needs the updated bfdb.</p>
      )}
    </div>
  )
}

// ── Page ──────────────────────────────────────────────────────────────────────

/** Admins only: the Discord post for the selected round, editable, with Copy. */
function DiscordPost({ round, onClose }: { round: Round; onClose: () => void }) {
  // The report's own queries, so this shares their cache.
  const summary = useQuery({ queryKey: ['round-summary', round.id], queryFn: () => api.roundSummary(round.id) })
  const report = useQuery({ queryKey: ['round-report', round.id], queryFn: () => api.roundReport(round.id) })
  const url = `${window.location.origin}/rounds?round=${round.id}${round.instance ? `&instance=${encodeURIComponent(round.instance)}` : ''}`
  const draft = roundPost({
    round, summary: summary.data, report: report.data, blue: campaign.blueLabel, red: campaign.redLabel, url,
  })
  const [edited, setEdited] = useState<string | null>(null)
  const [copied, setCopied] = useState(false)
  const text = edited ?? draft
  const copy = async () => {
    await navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 1500)
  }
  return (
    <div className="vs-card p-0" style={{ marginBottom: 16 }}>
      <div className="flex items-center gap-2 px-4 pt-3.5 pb-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
        <span style={{ ...label, fontSize: '0.65rem' }}>Discord post · Round {roundNo(round)}</span>
        <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>edit if you like, then paste it with the PDF</span>
        <span className="ml-auto flex gap-2">
          {edited !== null && <button type="button" className="vs-btn-ghost" style={{ fontSize: '0.66rem', padding: '4px 10px' }} onClick={() => setEdited(null)}>Reset</button>}
          <button type="button" className="vs-btn" style={{ fontSize: '0.66rem', padding: '4px 12px' }} onClick={() => void copy()}>{copied ? 'COPIED' : 'COPY'}</button>
          <button type="button" className="vs-btn-ghost" style={{ fontSize: '0.66rem', padding: '4px 10px' }} onClick={onClose}>Close</button>
        </span>
      </div>
      <textarea value={text} onChange={e => setEdited(e.target.value)} spellCheck aria-label="Discord post text"
        rows={Math.min(22, text.split('\n').length + 1)}
        style={{
          display: 'block', width: '100%', boxSizing: 'border-box', resize: 'vertical', padding: '12px 16px',
          background: 'transparent', color: 'var(--text)', border: 'none', outline: 'none',
          fontFamily: 'var(--font-mono, monospace)', fontSize: '0.75rem', lineHeight: 1.5,
        }} />
    </div>
  )
}

/** "round-12-southern-flank.pdf" */
function pdfName(r: Round): string {
  const slug = (r.scenario || '').toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '')
  return `round-${roundNo(r)}${r.instance ? `-${r.instance}` : ''}${slug ? `-${slug}` : ''}.pdf`
}

/** Saves the selected round's report (not the round list) as a one-page PDF. */
function ExportPdf({ round, target, header }: {
  round: Round | undefined
  target: React.RefObject<HTMLDivElement | null>
  header: React.RefObject<HTMLDivElement | null>
}) {
  const [busy, setBusy] = useState(false)
  const [failed, setFailed] = useState(false)
  // Wait for the report to be drawn before snapshotting it.
  const loading = useIsFetching({ queryKey: ['round-report', round?.id] }) + useIsFetching({ queryKey: ['round-summary', round?.id] }) > 0
  const save = async () => {
    if (!round || !target.current) return
    setBusy(true)
    setFailed(false)
    try {
      await saveElementPdf(target.current, pdfName(round), { header: header.current })
    } catch (e) {
      console.error('round PDF export failed', e)
      setFailed(true)
    } finally {
      setBusy(false)
    }
  }
  return (
    <button type="button" className="vs-btn" {...{ [NO_PDF]: '' }} onClick={() => void save()} disabled={!round || busy || loading}
      title={failed ? 'The export failed; see the browser console' : "This round's report on one PDF page"}
      style={{ display: 'flex', alignItems: 'center', gap: 6, fontSize: '0.68rem', padding: '5px 12px' }}>
      <Download size={12} /> {busy ? 'EXPORTING…' : failed ? 'EXPORT FAILED' : 'EXPORT PDF'}
    </button>
  )
}

export default function RoundsPage() {
  const q = useQuery({ queryKey: ['rounds'], queryFn: () => api.rounds() })
  const [params, setParams] = useSearchParams()
  const rounds = useMemo(() => [...(q.data ?? [])].sort((a, b) => b.start.localeCompare(a.start)), [q.data])
  const wanted = Number(params.get('round'))
  const selected = rounds.find(r => r.id === wanted) ?? rounds.find(r => r.active) ?? rounds[0]
  const pick = (id: number) => setParams(p => { p.set('round', String(id)); return p }, { replace: true })
  const look = useDashLook()
  const reportRef = useRef<HTMLDivElement>(null)
  const headerRef = useRef<HTMLDivElement>(null)
  const { user } = useAuth()
  const [posting, setPosting] = useState(false)
  const exportBtn = (
    <>
      {user?.is_admin && selected && (
        <button type="button" className="vs-btn-outline" {...{ [NO_PDF]: '' }} onClick={() => setPosting(p => !p)} aria-pressed={posting}
          title="Text for a Discord announcement of this round"
          style={{ fontSize: '0.68rem', padding: '5px 12px' }}>
          DISCORD POST
        </button>
      )}
      <ExportPdf round={selected} target={reportRef} header={headerRef} />
    </>
  )
  const post = posting && user?.is_admin && selected
    ? <DiscordPost key={selected.id} round={selected} onClose={() => setPosting(false)} />
    : null

  if (look.profile) {
    return (
      <div className="flex flex-col flex-1 overflow-hidden">
        <PageHeader art="flanker" title="ROUNDS" sub={`${rounds.length} round${rounds.length === 1 ? '' : 's'} on this server`} icon={Award} right={exportBtn} ref={headerRef} />
        <div className="flex-1 overflow-auto rd-page">
          <QueryState isLoading={q.isLoading} isError={q.isError} error={q.error} isEmpty={rounds.length === 0} what="rounds" onRetry={q.refetch} />
          {post}
          {selected && (
            <div className="rd-layout">
              {/* Round list: a column beside the report on wide screens, above it on narrow ones. */}
              <LvCard tape={<>All rounds · {rounds.length}</>} className="rd-list-card" bodyClass="rd-list">
                {rounds.map(r => <LvRoundRow key={r.id} r={r} on={r.id === selected.id} onPick={() => pick(r.id)} />)}
              </LvCard>
              <div className="rd-main">
                <Report key={selected.id} ref={reportRef} round={selected} />
              </div>
            </div>
          )}
        </div>
      </div>
    )
  }

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader art="flanker" title="ROUNDS" sub={`${rounds.length} round${rounds.length === 1 ? '' : 's'} on this server`} icon={Award} right={exportBtn} ref={headerRef} />
      <div className="flex-1 overflow-auto p-4">
        <QueryState isLoading={q.isLoading} isError={q.isError} error={q.error} isEmpty={rounds.length === 0} what="rounds" onRetry={q.refetch} />
        {post}
        {selected && (
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: 16, alignItems: 'flex-start' }}>
            {/* Round list: a column beside the report on wide screens, above it on narrow ones. */}
            <div className="vs-card p-0" style={{ flex: '1 1 240px', maxWidth: 300 }}>
              <div className="px-4 pt-3.5 pb-2.5" style={{ borderBottom: '1px solid var(--border)', ...label, fontSize: '0.65rem' }}>All rounds</div>
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
                        <span className="ml-auto" style={{ fontSize: '0.58rem', letterSpacing: '0.1em', color: r.active ? 'var(--accent-bright)' : sideColor(r.winner) }}>
                          {result(r, 'LIVE')}
                        </span>
                      </div>
                      <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 2, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                        {r.scenario} · {fmtDate(r.start)} · {duration(r)}
                      </div>
                    </button>
                  )
                })}
              </div>
            </div>
            <div style={{ flex: '999 1 560px', minWidth: 0 }}>
              <Report key={selected.id} ref={reportRef} round={selected} />
            </div>
          </div>
        )}
      </div>
    </div>
  )
}
