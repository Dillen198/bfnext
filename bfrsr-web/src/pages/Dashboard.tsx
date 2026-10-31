// F1 SITUATION: the situation board.
//
// The room's main wall. The plot is in the middle under its acetate; on the
// left the CRT bank (forces on station), the status board of holdings, the
// will to fight and the front line; on the right the latest dispatch as a
// clipping, the teletype roll of engagements and the war so far.
//
// Will to fight and story events are drawn now, but the server does not send
// them yet (ENGINE.md E5, E5b): those panels say "awaiting traffic" until
// `stats.will` / `stats.story` arrive. Nothing on this page is invented.

import { useEffect, useMemo, useState, type ReactNode } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Link, useNavigate } from 'react-router-dom'
import { Alert, RefreshCw } from '@icons'
import {
  API_ROOT, api,
  type Frontlines, type Kill, type NewsDay, type Objective, type OnlinePilot, type Pilot, type PilotName,
  type Stats, type StoryEvent, type WarWill,
} from '../api'
import { campaign } from '../config/campaign'
import { BRAND } from '../config/redstorm'
import TacMap from '../components/TacMap'
import ObjectiveDrawer from '../components/ObjectiveDrawer'
import { attentionList, TONE, type Attention } from '../lib/objectives'
import { ResponsiveContainer, AreaChart, Area, YAxis } from 'recharts'
import { classifyTargetClass, fmtTimeZ, windDir, visStr, cloudStr } from '../lib/format'
import { useRound } from '../context/RoundContext'
import { pressable } from '@shared/web/a11y'

// ── Helpers ───────────────────────────────────────────────────────────────────

function useDelta(v: number) {
  const [prev, setPrev] = useState(v)
  const [delta, setDelta] = useState<number | null>(null)
  // Adjusted while rendering, so the delta shows on the same render as the value.
  // The first figures to arrive (from nothing) are not a change.
  if (prev !== v) {
    setPrev(v)
    setDelta(prev === 0 ? null : v - prev)
  }
  return delta
}

const sideColor = (side: string | undefined | null) =>
  side === 'Blue' ? 'var(--blue)' : side === 'Red' ? 'var(--red)' : 'var(--text-dim)'

const sideName = (side: string | undefined | null) =>
  side === 'Blue' ? campaign.blueLabel : side === 'Red' ? campaign.redLabel : 'Unknown'

/** A heading over a group on the board: stencil, a rule running off it. */
function BoardHead({ children, right }: { children: ReactNode; right?: ReactNode }) {
  return (
    <h2 className="sit-head" style={{ margin: 0 }}>
      <span>{children}</span>
      {right}
    </h2>
  )
}

/** The panel for data the server does not send yet. */
function AwaitingTraffic({ what }: { what: string }) {
  return (
    <div className="awaiting">
      <span className="awaiting-cursor" aria-hidden="true" />
      <div>
        Awaiting traffic
        <small>{what}</small>
      </div>
    </div>
  )
}

// ── Forces on station: the CRT bank ──────────────────────────────────────────

function Counter({ label, value, sub, tone, delta }: {
  label: string; value: ReactNode; sub?: string; tone?: 'blue' | 'red'; delta?: number | null
}) {
  return (
    <div className={`sit-counter${tone ? ` sit-counter--${tone}` : ''}`}>
      <div className="sit-counter-label">{label}</div>
      <div className="sit-counter-value">
        {value}
        {delta != null && delta !== 0 && (
          <span style={{ fontSize: '1.1rem', marginLeft: 6 }}>{delta > 0 ? `+${delta}` : delta}</span>
        )}
      </div>
      {sub && <div className="sit-counter-sub">{sub}</div>}
    </div>
  )
}

function ForcesBank({ online, objectives, atRisk, lastHour, stats }: {
  online: OnlinePilot[]
  objectives: Objective[]
  atRisk: number
  lastHour: { all: number; blue: number; red: number }
  stats: Stats | undefined
}) {
  const blue = online.filter(p => p.side === 'Blue').length
  const red = online.filter(p => p.side === 'Red').length
  const up = (side: string) => online.filter(p => p.side === side && p.aircraft != null).length
  const blueObj = objectives.filter(o => o.owner === 'Blue').length
  const redObj = objectives.filter(o => o.owner === 'Red').length
  const total = objectives.length
  const blueDelta = useDelta(blueObj)
  const redDelta = useDelta(redObj)
  const pct = (n: number) => `${total > 0 ? Math.round(n / total * 100) : 0}% of ground`
  return (
    <div className="crt sit-forces">
      <Counter label={`${campaign.blueLabel} on net`} value={blue} tone="blue" sub={`${up('Blue')} airborne`} />
      <Counter label={`${campaign.redLabel} on net`} value={red} tone="red" sub={`${up('Red')} airborne`} />
      <Counter label={`${campaign.blueLabel} holds`} value={blueObj} tone="blue" delta={blueDelta} sub={pct(blueObj)} />
      <Counter label={`${campaign.redLabel} holds`} value={redObj} tone="red" delta={redDelta} sub={pct(redObj)} />
      <Counter label="At risk" value={atRisk} sub="falling or under attack" />
      <Counter label="Kills · last hour" value={lastHour.all} sub={`${lastHour.blue} by ${campaign.blueLabel} · ${lastHour.red} by ${campaign.redLabel}`} />
      <Counter label="Aircrew on file" value={stats?.total_pilots ?? '—'} sub={`${stats?.blue_registered ?? 0} ${campaign.blueLabel} · ${stats?.red_registered ?? 0} ${campaign.redLabel}`} />
      <Counter label="Objectives" value={total} sub={`${total - blueObj - redObj} neutral`} />
    </div>
  )
}

// ── Holdings: the status board ───────────────────────────────────────────────

const KIND_ORDER: [string, string][] = [
  ['Airbase', 'Airfields'],
  ['Naval Base', 'Ports'],
  ['Carrier Group', 'Carrier groups'],
  ['Logistics Hub', 'Rail and depots'],
  ['Factory', 'Refineries and plant'],
  ['FOB', 'Forward bases'],
  ['FARP', 'Forward strips'],
  ['Command Center', 'Command centres'],
]

function Holdings({ objectives, roundId }: { objectives: Objective[]; roundId: number | null }) {
  const total = objectives.length || 1
  const blue = objectives.filter(o => o.owner === 'Blue').length
  const red = objectives.filter(o => o.owner === 'Red').length
  const neu = objectives.length - blue - red
  const rows = KIND_ORDER
    .map(([kind, label]) => {
      const of = objectives.filter(o => o.kind === kind)
      return { label, n: of.length, blue: of.filter(o => o.owner === 'Blue').length, red: of.filter(o => o.owner === 'Red').length }
    })
    .filter(r => r.n > 0)
  const known = new Set(KIND_ORDER.map(([k]) => k))
  const other = objectives.filter(o => !known.has(o.kind))
  if (other.length) rows.push({ label: 'Other', n: other.length, blue: other.filter(o => o.owner === 'Blue').length, red: other.filter(o => o.owner === 'Red').length })
  return (
    <div className="vs-card">
      <div style={{ padding: '10px 12px 8px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '0.7rem', fontWeight: 700, letterSpacing: '0.1em', marginBottom: 5 }}>
          <span style={{ color: 'var(--blue)' }}>{campaign.blueLabel.toUpperCase()} {Math.round(blue / total * 100)}%</span>
          {neu > 0 && <span style={{ color: 'var(--text-dim)' }}>NEUTRAL {neu}</span>}
          <span style={{ color: 'var(--red)' }}>{Math.round(red / total * 100)}% {campaign.redLabel.toUpperCase()}</span>
        </div>
        <div style={{ display: 'flex', height: 12, background: 'var(--bg-input)', border: '1px solid var(--border)' }} aria-hidden="true">
          <div className="health-fill" style={{ width: `${blue / total * 100}%`, background: 'var(--blue)' }} />
          <div className="health-fill" style={{ width: `${neu / total * 100}%`, background: 'repeating-linear-gradient(45deg, var(--border-light) 0 3px, transparent 3px 6px)' }} />
          <div className="health-fill" style={{ width: `${red / total * 100}%`, background: 'var(--red)' }} />
        </div>
      </div>
      <TerritoryTrend roundId={roundId} />
      <div className="sit-board-row sit-board-row--head">
        <span>Holding</span><span>{campaign.blueLabel.slice(0, 6).toUpperCase()}</span><span>{campaign.redLabel.slice(0, 6).toUpperCase()}</span>
      </div>
      {rows.map(r => (
        <div key={r.label} className="sit-board-row">
          <span style={{ color: 'var(--text-muted)' }}>{r.label}</span>
          <span style={{ color: 'var(--blue)' }}>{r.blue}</span>
          <span style={{ color: 'var(--red)' }}>{r.red}</span>
        </div>
      ))}
    </div>
  )
}

/** Objectives held per side over the round, from the round report. */
function TerritoryTrend({ roundId }: { roundId: number | null }) {
  const { data: rep } = useQuery({
    queryKey: ['round-report', roundId], queryFn: () => api.roundReport(roundId!),
    enabled: roundId != null, refetchInterval: 5 * 60_000, retry: false,
  })
  if (!rep || rep.buckets.length < 2) return null
  return (
    <div style={{ padding: '0 12px 8px' }}>
      <div style={{ fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.16em', marginBottom: 2 }}>THIS ROUND</div>
      <ResponsiveContainer width="100%" height={42}>
        <AreaChart data={rep.buckets} margin={{ left: 0, right: 0, top: 2, bottom: 0 }}>
          <YAxis hide domain={[0, rep.objectives || 'auto']} />
          <Area type="stepAfter" dataKey="blue_held" stackId="t" stroke={campaign.blueColor} strokeWidth={1.5} fill={campaign.blueColor} fillOpacity={0.15} isAnimationActive={false} />
          <Area type="stepAfter" dataKey="red_held" stackId="t" stroke={campaign.redColor} strokeWidth={1.5} fill={campaign.redColor} fillOpacity={0.15} isAnimationActive={false} />
        </AreaChart>
      </ResponsiveContainer>
    </div>
  )
}

// ── Will to fight ────────────────────────────────────────────────────────────

/** Both sides' will, against the line where a side asks for terms. Only when
 *  the server sends it (`stats.will`, ENGINE.md E5). */
export function WillToFight({ will }: { will: WarWill | null | undefined }) {
  if (!will) {
    return (
      <div className="vs-card">
        <AwaitingTraffic what="The will-to-fight returns start once the war model reports them." />
      </div>
    )
  }
  const meter = (label: string, v: { value: number; trend?: number | null }, color: string) => (
    <div className="will-meter">
      <span style={{ color, fontWeight: 700, letterSpacing: '0.1em', textTransform: 'uppercase' }}>{label}</span>
      <div className="will-track" role="meter" aria-valuemin={0} aria-valuemax={100} aria-valuenow={Math.round(v.value)} aria-label={`${label} will to fight`}>
        <div className="will-fill" style={{ width: `${Math.max(0, Math.min(100, v.value))}%`, background: color }} />
        {will.threshold != null && <div className="will-terms" style={{ left: `${will.threshold}%` }} title="Asks for terms below this line" />}
      </div>
      <span className="will-value" style={{ color }}>
        {Math.round(v.value)}
        {v.trend != null && v.trend !== 0 && <small style={{ fontSize: '0.8rem', marginLeft: 2 }}>{v.trend > 0 ? '▲' : '▼'}</small>}
      </span>
    </div>
  )
  return (
    <div className="vs-card" style={{ paddingTop: 4, paddingBottom: 4 }}>
      {meter(campaign.blueLabel, will.blue, 'var(--blue)')}
      {meter(campaign.redLabel, will.red, 'var(--red)')}
      {will.causes && will.causes.length > 0 && (
        <ul style={{ listStyle: 'none', margin: 0, padding: '4px 12px 8px', fontSize: '0.7rem', color: 'var(--text-muted)' }}>
          {will.causes.slice(0, 4).map((c, i) => (
            <li key={i} style={{ display: 'flex', gap: 8, padding: '2px 0' }}>
              <span style={{ color: sideColor(c.side), minWidth: 30, textAlign: 'right' }}>{c.delta > 0 ? `+${c.delta}` : c.delta}</span>
              <span>{c.text}</span>
            </li>
          ))}
        </ul>
      )}
    </div>
  )
}

// ── The war so far ───────────────────────────────────────────────────────────

/** Story events that have fired, newest first (`stats.story`, ENGINE.md E5b). */
export function WarSoFar({ story }: { story: StoryEvent[] | null | undefined }) {
  if (!story || story.length === 0) {
    return (
      <div className="vs-card">
        <AwaitingTraffic what={story ? 'No story events have fired this round.' : 'Story events are logged here once the war model sends them.'} />
      </div>
    )
  }
  return (
    <div className="vs-card">
      {[...story].reverse().slice(0, 8).map(e => (
        <div key={e.id} className="story-item">
          <span className="story-time">{fmtTimeZ(e.time)}</span>
          <div style={{ minWidth: 0 }}>
            <div className="story-title" style={{ color: sideColor(e.side) === 'var(--text-dim)' ? 'var(--text)' : sideColor(e.side) }}>{e.title}</div>
            {e.text && <div style={{ fontSize: '0.74rem', lineHeight: 1.45, margin: '2px 0' }}>{e.text}</div>}
            {e.caused_by && e.caused_by.length > 0 && <div className="story-meta">Credited to {e.caused_by.join(', ')}</div>}
            {e.effects && e.effects.length > 0 && <div className="story-meta">{e.effects.join(' · ')}</div>}
          </div>
        </div>
      ))}
    </div>
  )
}

// ── Front line ───────────────────────────────────────────────────────────────

function FrontLine({ items, onOpen }: { items: Attention[]; onOpen: (name: string) => void }) {
  if (items.length === 0) {
    return <div className="vs-card" style={{ padding: '10px 12px', fontSize: '0.7rem', color: 'var(--text-dim)' }}>QUIET FRONT. NOTHING UNDER ATTACK OR FAILING.</div>
  }
  return (
    <div className="vs-card">
      {items.slice(0, 12).map(({ obj, reasons }) => (
        <button key={obj.id} type="button" onClick={() => onOpen(obj.name)} className="kill-row"
          style={{ all: 'unset', boxSizing: 'border-box', width: '100%', cursor: 'pointer', display: 'flex', alignItems: 'center', gap: 8, padding: '6px 12px', borderBottom: '1px solid var(--border)' }}>
          <span style={{ width: 7, height: 7, flexShrink: 0, background: sideColor(obj.owner), transform: 'rotate(45deg)' }} />
          <span style={{ fontSize: '0.74rem', fontWeight: 700, color: 'var(--text)', whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis', minWidth: 0, flex: '0 1 auto' }}>{obj.name}</span>
          <span style={{ display: 'flex', flexWrap: 'wrap', justifyContent: 'flex-end', gap: 4, marginLeft: 'auto' }}>
            {reasons.slice(0, 2).map(r => (
              <span key={r.text} style={{ fontSize: '0.6rem', padding: '1px 5px', whiteSpace: 'nowrap', color: TONE[r.tone].fg, background: TONE[r.tone].bg, border: '1px solid currentColor' }}>{r.text}</span>
            ))}
          </span>
        </button>
      ))}
    </div>
  )
}

// ── Air picture and leading aircrew ──────────────────────────────────────────

function AirPicture({ online }: { online: OnlinePilot[] }) {
  const navigate = useNavigate()
  const airborne = online.filter(p => p.aircraft != null)
  const grounded = online.filter(p => p.aircraft == null)
  if (!online.length) return <div className="awaiting">No aircrew on the net</div>
  const row = (p: OnlinePilot) => (
    <div key={p.ucid} {...pressable(() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`))} className="kill-row"
      style={{ display: 'grid', gridTemplateColumns: '10px minmax(0,1fr) auto', gap: 8, alignItems: 'center', padding: '4px 12px', borderBottom: '1px solid var(--border)', cursor: 'pointer' }}>
      <span style={{ width: 7, height: 7, background: sideColor(p.side) }} />
      <span style={{ fontSize: '0.74rem', fontWeight: 700, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{p.name}</span>
      <span style={{ fontSize: '0.66rem', color: sideColor(p.side) }}>{p.aircraft ?? 'ON GROUND'}</span>
    </div>
  )
  return (
    <>
      {airborne.map(row)}
      {grounded.slice(0, 8).map(row)}
    </>
  )
}

function LeadingAircrew({ pilots }: { pilots: Pilot[] }) {
  const navigate = useNavigate()
  const top = useMemo(() =>
    [...pilots].sort((a, b) => (b.air_kills + b.ground_kills) - (a.air_kills + a.ground_kills)).slice(0, 5),
  [pilots])
  if (!top.length) return <div className="awaiting">No returns yet</div>
  return (
    <>
      {top.map((p, i) => (
        <div key={p.ucid} {...pressable(() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`))} className="kill-row"
          style={{ display: 'grid', gridTemplateColumns: '18px minmax(0,1fr) auto', gap: 8, alignItems: 'center', padding: '4px 12px', borderBottom: '1px solid var(--border)', cursor: 'pointer' }}>
          <span className="font-crt" style={{ fontSize: '1.2rem', color: i === 0 ? 'var(--brass)' : 'var(--text-dim)' }}>{i + 1}</span>
          <span style={{ fontSize: '0.74rem', fontWeight: 700, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{p.name}</span>
          <span style={{ fontSize: '0.66rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>
            AIR <b style={{ color: 'var(--text)' }}>{p.air_kills}</b> · GND <b style={{ color: 'var(--text)' }}>{p.ground_kills}</b> · {p.deaths}D
          </span>
        </div>
      ))}
    </>
  )
}

// ── Teletype roll ────────────────────────────────────────────────────────────

const CLASS_WORD: Record<string, string> = {
  AIR: 'SPLASH', HELO: 'SPLASH', NAVAL: 'SUNK', AD: 'SAM SITE DOWN', ARMOR: 'ARMOUR KILLED',
  VEH: 'VEHICLE KILLED', INF: 'INFANTRY', GND: 'GROUND KILL',
}

function TeletypeRoll({ kills, nameMap }: { kills: Kill[]; nameMap: Map<string, string> }) {
  const shown = kills.slice(0, campaign.dashboardKillFeedCount)
  return (
    <div className="paper teletype" style={{ borderRadius: 1 }}>
      <div className="teletype-head">
        ZCZC SOUTHCOM OPS · ENGAGEMENTS · {shown.length ? `${shown.length} REPORTS` : 'NO TRAFFIC'}
      </div>
      {shown.length === 0 && <div className="teletype-line">NIL REPORTS. NNNN</div>}
      {shown.map((k, i) => {
        const cls = classifyTargetClass(k.target_type)
        const killerName = k.killer?.ucid ? nameMap.get(k.killer.ucid) : null
        const killer = killerName
          ?? (k.killer ? (k.killer.side === 'Blue' || k.killer.side === 'Red' ? `${sideName(k.killer.side)} ${k.killer.airframe ?? 'AI'}` : k.killer.airframe ?? 'AI') : 'ENVIRONMENT')
        return (
          <div key={i} className="teletype-line">
            <span style={{ fontWeight: 700 }}>{fmtTimeZ(k.time).replace(':', '')}</span>{' '}
            <span style={{ color: sideColor(k.killer?.side), fontWeight: 700 }}>{killer.toUpperCase()}</span>
            {killerName && k.killer?.airframe && <span> ({k.killer.airframe})</span>}
            {' '}<b>{CLASS_WORD[cls] ?? 'KILL'}</b>{' '}
            <span style={{ color: sideColor(k.victim.side) }}>{sideName(k.victim.side).toUpperCase()}</span>
            {k.target_type && <span> {k.target_type}</span>}
            {k.killer?.weapon && <span style={{ color: 'var(--text-dim)' }}> / {k.killer.weapon}</span>}
          </div>
        )
      })}
      <div style={{ padding: '8px 0 10px', textAlign: 'center' }}>
        <Link to="/kills" style={{ fontSize: '0.66rem', letterSpacing: '0.14em', color: 'var(--accent)', fontWeight: 700 }}>F7 · ALL LOSS REPORTS</Link>
      </div>
    </div>
  )
}

// ── The latest dispatch, as a clipping ───────────────────────────────────────

function Clipping({ day, error }: { day: NewsDay | undefined; error: unknown }) {
  const [imgFailed, setImgFailed] = useState(false)
  if (!day) {
    return (
      <div className="vs-card">
        <AwaitingTraffic what={error ? `${BRAND.papers.wire} could not be fetched.` : `No dispatch filed yet.`} />
      </div>
    )
  }
  const paras = day.body && day.body.length > 0 ? day.body.slice(0, 2) : day.items.slice(0, 3).map(i => i.text)
  return (
    <div className="clipping-wrap">
      <article className="paper clipping">
        <div className="clipping-paper">{BRAND.papers.wire}</div>
        <div style={{ fontSize: '0.62rem', letterSpacing: '0.1em', textAlign: 'center', marginBottom: 6, color: 'var(--text-muted)', textTransform: 'uppercase' }}>
          {day.facts.campaign_day ? `Day ${day.facts.campaign_day} · ` : ''}{day.day}{day.final_ ? '' : ' · late edition'}
        </div>
        <h3 className="clipping-head">{day.headline}</h3>
        {day.image && !imgFailed && (
          <img src={`${API_ROOT}${day.image}`} alt="" loading="lazy" decoding="async" onError={() => setImgFailed(true)} />
        )}
        <div className="clipping-body">
          {paras.map((p, i) => <p key={i}>{p}</p>)}
        </div>
        {day.alt && (
          <p style={{ margin: '6px 0 0', paddingTop: 6, borderTop: '1px solid var(--text)', fontSize: '0.74rem', lineHeight: 1.35 }}>
            <span style={{ fontFamily: 'var(--font-display)', color: 'var(--red)', letterSpacing: '0.12em', textTransform: 'uppercase' }}>{BRAND.papers.alt}:</span>{' '}
            {day.alt.headline}
          </p>
        )}
        <div style={{ marginTop: 8, textAlign: 'right' }}>
          <Link to="/news" style={{ fontFamily: 'var(--font-mono)', fontSize: '0.66rem', letterSpacing: '0.12em', color: 'var(--accent)', fontWeight: 700 }}>F3 · THE WIRE →</Link>
        </div>
      </article>
    </div>
  )
}

// ── Met ──────────────────────────────────────────────────────────────────────

function MetLine({ stats }: { stats: Stats | undefined }) {
  const w = stats?.weather
  if (!w) return null
  return (
    <div className="crt crt--green sit-metar">
      MET · WIND {String(Math.round(w.wind_from_deg)).padStart(3, '0')}/{Math.round(w.wind_speed_kts)}KT ({windDir(w.wind_from_deg)})
      {' '}· VIS {visStr(w.visibility_m)} · CLOUD {cloudStr(w.cloud_base_m)} · {Math.round(w.temp_c)}°C · QNH {Math.round(w.qnh_hpa)}
    </div>
  )
}

// ── Page ─────────────────────────────────────────────────────────────────────

export default function Dashboard() {
  const navigate = useNavigate()
  const { selectedRound } = useRound()

  const onlineQ = useQuery({ queryKey: ['online'], queryFn: api.online, refetchInterval: 10_000 })
  const { data: online = [] } = onlineQ
  const { data: pilots = [] } = useQuery({ queryKey: ['leaderboard'], queryFn: api.leaderboard, refetchInterval: 60_000 })
  const { data: allPilots = [] } = useQuery({ queryKey: ['all-pilots'], queryFn: api.allPilots, refetchInterval: 120_000 })
  const objectivesQ = useQuery({ queryKey: ['objectives', selectedRound], queryFn: () => api.objectives(selectedRound), refetchInterval: 30_000 })
  const { data: objectives = [] } = objectivesQ
  const { data: fronts = { mid: [], blue: [], red: [] } } = useQuery<Frontlines>({ queryKey: ['frontline', selectedRound], queryFn: () => api.frontline(selectedRound), refetchInterval: 30_000 })
  const { data: kills = [] } = useQuery({ queryKey: ['kills-dash', selectedRound], queryFn: () => api.kills(selectedRound, campaign.dashboardKillFeedCount + 20), refetchInterval: 15_000 })
  const { data: stats } = useQuery({ queryKey: ['stats'], queryFn: api.stats, refetchInterval: 60_000 })
  // Shares the loss reports' cache; the counter takes the last hour from it.
  const { data: feed = [] } = useQuery({ queryKey: ['kills-feed', selectedRound], queryFn: () => api.kills(selectedRound, 500), refetchInterval: 30_000 })
  const { data: captures = [] } = useQuery({ queryKey: ['capture-events', selectedRound], queryFn: () => api.captureEvents(selectedRound, 200), refetchInterval: 60_000 })
  const { data: rounds = [] } = useQuery({ queryKey: ['rounds'], queryFn: () => api.rounds(), refetchInterval: 5 * 60_000 })
  const { data: news, error: newsErr } = useQuery({ queryKey: ['news'], queryFn: () => api.news(5), refetchInterval: 5 * 60_000 })
  const [drawer, setDrawer] = useState<string | null>(null)

  const nameMap = useMemo(() => {
    const m = new Map<string, string>()
    ;(allPilots as PilotName[]).forEach(p => m.set(p.ucid, p.name))
    return m
  }, [allPilots])

  const [now, setNow] = useState(() => Date.now())
  useEffect(() => { const t = setInterval(() => setNow(Date.now()), 60_000); return () => clearInterval(t) }, [])
  const attention = useMemo(() => attentionList(objectives, captures, now), [objectives, captures, now])
  const atRisk = attention.filter(a => a.rank <= 2).length
  const lastHour = useMemo(() => {
    const recent = feed.filter(k => now - Date.parse(k.time) < 3_600_000)
    return { all: recent.length, blue: recent.filter(k => k.killer?.side === 'Blue').length, red: recent.filter(k => k.killer?.side === 'Red').length }
  }, [feed, now])
  const roundId = selectedRound ?? rounds.find(r => r.active)?.id ?? null
  const asOf = fmtTimeZ(new Date(now).toISOString()).replace(':', '')
  const inAir = online.filter(p => p.aircraft != null).length

  return (
    <div style={{ display: 'flex', flexDirection: 'column', flex: 1, minHeight: 0 }}>
      {/* A failed poll used to be invisible: every query defaults to an empty
          array, so "bfdb is down" and "quiet server" drew the same zeros. */}
      {(objectivesQ.isError || onlineQ.isError) && (
        <div role="alert" style={{
          display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0,
          padding: '6px 14px', background: 'var(--red-bg)',
          borderBottom: '1px solid var(--red)', color: 'var(--red)',
          fontSize: '0.7rem', letterSpacing: '0.12em', fontWeight: 700,
        }}>
          <Alert size={11} />
          <span style={{ flex: 1, textTransform: 'uppercase' }}>Live data unavailable. Server off net.</span>
          <button type="button" onClick={() => { objectivesQ.refetch(); onlineQ.refetch() }}
            style={{ display: 'inline-flex', alignItems: 'center', gap: 4, cursor: 'pointer', background: 'transparent', border: '1px solid currentColor', color: 'var(--red)', font: 'inherit', padding: '2px 8px', textTransform: 'uppercase' }}>
            <RefreshCw size={10} /> Retry
          </button>
        </div>
      )}

      <div className="sit">
        {/* ══ The plot ══ */}
        <section className="sit-plot" aria-label="The plot">
          <BoardHead right={<span className="stamp stamp--dim" style={{ fontSize: '0.56rem' }}>Overlay 1</span>}>The plot</BoardHead>
          <div className="sit-plot-map">
            <div className="acetate">
              <TacMap objectives={objectives} fronts={fronts} onOpenTacmap={() => navigate('/map')} acetate />
              <div className="acetate-sheet" aria-hidden="true" />
              <span className="acetate-tape acetate-tape--tl" aria-hidden="true" />
              <span className="acetate-tape acetate-tape--tr" aria-hidden="true" />
              <span className="acetate-tape acetate-tape--bl" aria-hidden="true" />
              <span className="acetate-tape acetate-tape--br" aria-hidden="true" />
              <span className="acetate-label">SITUATION AS OF {asOf} · FRONT IN GREASE PENCIL</span>
            </div>
          </div>
          <div className="sit-plot-foot">
            <div className="vs-card" style={{ display: 'flex', flexDirection: 'column', minHeight: 0 }}>
              <div className="rsr-panel-head" style={{ padding: '6px 12px', display: 'flex', gap: 8 }}>
                <span className="rsr-panel-title" style={{ fontSize: '0.64rem', flex: 1 }}>Aircrew on the net</span>
                <span style={{ fontSize: '0.64rem', color: 'var(--text-dim)' }}>{inAir} UP · {online.length - inAir} DOWN</span>
              </div>
              <div style={{ overflowY: 'auto', minHeight: 0, flex: 1 }}><AirPicture online={online} /></div>
            </div>
            <div className="vs-card" style={{ display: 'flex', flexDirection: 'column', minHeight: 0 }}>
              <div className="rsr-panel-head" style={{ padding: '6px 12px' }}>
                <span className="rsr-panel-title" style={{ fontSize: '0.64rem' }}>Leading aircrew</span>
              </div>
              <div style={{ overflowY: 'auto', minHeight: 0, flex: 1 }}><LeadingAircrew pilots={pilots} /></div>
            </div>
          </div>
        </section>

        {/* ══ Left: the CRT bank and the status board ══ */}
        <div className="sit-col sit-left">
          <BoardHead>Forces</BoardHead>
          <ForcesBank online={online} objectives={objectives} atRisk={atRisk} lastHour={lastHour} stats={stats} />
          <BoardHead>Holdings</BoardHead>
          <Holdings objectives={objectives} roundId={roundId} />
          <BoardHead>Will to fight</BoardHead>
          <WillToFight will={stats?.will} />
          <BoardHead right={<span style={{ fontSize: '0.66rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>{attention.length}</span>}>Front line</BoardHead>
          <FrontLine items={attention} onOpen={setDrawer} />
        </div>

        {/* ══ Right: the wire, the teletype and the war so far ══ */}
        <div className="sit-col sit-right">
          <BoardHead>Latest dispatch</BoardHead>
          <Clipping day={news?.days?.[0]} error={newsErr} />
          <BoardHead>Engagements</BoardHead>
          <TeletypeRoll kills={kills} nameMap={nameMap} />
          <BoardHead>The war so far</BoardHead>
          <WarSoFar story={stats?.story} />
          <MetLine stats={stats} />
        </div>
      </div>

      <ObjectiveDrawer objective={drawer} onClose={() => setDrawer(null)} onOpen={setDrawer} />
    </div>
  )
}
