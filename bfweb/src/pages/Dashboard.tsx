import { useEffect, useMemo, useRef, useState } from 'react'
import NewsPanel from '../components/NewsPanel'
import { useQuery } from '@tanstack/react-query'
import { useNavigate } from 'react-router-dom'
import {
  Aircraft,
  Pilot as PilotIcon,
  Shield,
  Alert,
  KillFeed as KillIcon,
  Pin,
  Comms,
  Wind,
  Temp,
  Baro,
  Visibility,
  Activity,
  TrendingUp,
  RefreshCw,
} from '@icons'
import { api, type OnlinePilot, type Objective, type Pilot, type Kill, type PilotName, type Stats, type Frontlines } from '../api'
import { campaign } from '../config/campaign'
import PanelBase from '../components/Panel'
import TacMap from '../components/TacMap'
import ObjectiveDrawer from '../components/ObjectiveDrawer'
import { attentionList, TONE, type Attention } from '../lib/objectives'
import { ResponsiveContainer, AreaChart, Area, YAxis } from 'recharts'
import { classifyTargetClass, TARGET_CLASS_COLOR, fmtTimeZ, windDir, visStr, cloudStr } from '../lib/format'
import { useRound } from '../context/RoundContext'
import { useFlagBackground } from '../lib/flags'
import { useWireframe } from '../lib/wireframe'

// ── Helpers ───────────────────────────────────────────────────────────────────

/** Thin adapter over the shared classifier -- see lib/format. */
function classifyKill(t: string | null): { label: string; col: string } {
  const label = classifyTargetClass(t)
  return { label, col: TARGET_CLASS_COLOR[label] }
}

function useDelta(v: number) {
  const prev  = useRef<number | null>(null)
  const delta = useRef<number | null>(null)
  useEffect(() => {
    if (prev.current !== null && prev.current !== v) delta.current = v - prev.current
    prev.current = v
  }, [v])
  return delta.current
}

// ── KPI cell ──────────────────────────────────────────────────────────────────

function KpiCell({ label, value, sub, color = 'var(--text)', delta, icon: Icon }: {
  label: string; value: React.ReactNode; sub?: string; color?: string
  delta?: number | null; icon?: React.ElementType
}) {
  return (
    <div className="kpi-cell">
      <div style={{ display: 'flex', alignItems: 'center', gap: 5 }}>
        {Icon && <Icon size={9} style={{ color, opacity: 0.55 }} />}
        <div className="kpi-label">{label}</div>
      </div>
      <div className="kpi-value" style={{ color, display: 'flex', alignItems: 'baseline', gap: 5, marginTop: 1 }}>
        {value}
        {delta != null && delta !== 0 && (
          <span style={{ fontSize: '0.65rem', fontWeight: 700, color: delta > 0 ? 'var(--accent)' : 'var(--red)', letterSpacing: 0 }}>
            {delta > 0 ? `+${delta}` : delta}
          </span>
        )}
      </div>
      {sub && <div className="kpi-sub">{sub}</div>}
    </div>
  )
}

// ── Territory bar ─────────────────────────────────────────────────────────────

function TerritoryBar({ objectives }: { objectives: Objective[] }) {
  const total   = objectives.length || 1
  const blue    = objectives.filter(o => o.owner === 'Blue').length
  const red     = objectives.filter(o => o.owner === 'Red').length
  const neu     = objectives.filter(o => o.owner === 'Neutral').length
  const bluePct = blue / total * 100
  const redPct  = red  / total * 100
  const neuPct  = neu  / total * 100

  return (
    <div style={{ padding: '10px 14px 12px' }}>
      {/* Labels */}
      <div style={{ display: 'flex', justifyContent: 'space-between', marginBottom: 6, fontSize: '0.72rem', fontFamily: 'var(--font-mono)' }}>
        <span style={{ color: campaign.blueColor, fontWeight: 700 }}>
          {campaign.blueLabel.toUpperCase()} <span style={{ color: 'var(--text-muted)' }}>{Math.round(bluePct)}%</span>
          <span style={{ color: 'var(--text-dim)', fontWeight: 400 }}> ({blue})</span>
        </span>
        {neu > 0 && <span style={{ color: 'var(--text-dim)' }}>NEUT {neu}</span>}
        <span style={{ color: campaign.redColor, fontWeight: 700 }}>
          <span style={{ color: 'var(--text-dim)', fontWeight: 400 }}>({red}) </span>
          <span style={{ color: 'var(--text-muted)' }}>{Math.round(redPct)}%</span> {campaign.redLabel.toUpperCase()}
        </span>
      </div>
      {/* Bar */}
      <div style={{ display: 'flex', height: 10, background: 'var(--bg-elevated)', overflow: 'hidden' }}>
        <div className="health-fill" style={{ width: `${bluePct}%`, height: '100%', background: `linear-gradient(90deg, ${campaign.blueColor}cc, ${campaign.blueColor})` }} />
        <div className="health-fill" style={{ width: `${neuPct}%`,  height: '100%', background: 'var(--border-light)' }} />
        <div className="health-fill" style={{ width: `${redPct}%`,  height: '100%', background: `linear-gradient(90deg, ${campaign.redColor}, ${campaign.redColor}cc)` }} />
      </div>
      {/* Mini tick marks */}
      <div style={{ display: 'flex', justifyContent: 'space-between', marginTop: 3 }}>
        {[0,25,50,75,100].map(p => (
          <span key={p} style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>{p}%</span>
        ))}
      </div>
    </div>
  )
}

// ── Weather brief ─────────────────────────────────────────────────────────────

function WeatherBrief({ stats }: { stats: Stats | undefined }) {
  const w = stats?.weather
  if (!w) return (
    <div style={{ padding: '10px 14px', fontSize: '0.62rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>NO METAR</div>
  )
  const items = [
    { icon: Wind,        label: 'WIND',  value: `${Math.round(w.wind_speed_kts)}KT ${windDir(w.wind_from_deg)} (${Math.round(w.wind_from_deg)}°)` },
    { icon: Temp,        label: 'TEMP',  value: `${Math.round(w.temp_c)}°C` },
    { icon: Baro,        label: 'QNH',   value: `${Math.round(w.qnh_hpa)} hPa` },
    { icon: Visibility,  label: 'VIS',   value: visStr(w.visibility_m) },
    { icon: Activity,    label: 'CLOUD', value: cloudStr(w.cloud_base_m) },
  ]
  return (
    <div style={{ display: 'grid', gridTemplateColumns: 'repeat(5, 1fr)', gap: 0 }}>
      {items.map(({ icon: Icon, label, value }) => (
        <div key={label} style={{ padding: '8px 10px', borderRight: '1px solid var(--border)', display: 'flex', flexDirection: 'column', gap: 3, alignItems: 'center' }}>
          <Icon size={10} style={{ color: 'var(--accent)', opacity: 0.7 }} />
          <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', letterSpacing: '0.12em' }}>{label}</span>
          <span className="font-mono-vs" style={{ fontSize: '0.7rem', color: 'var(--text)', fontWeight: 600, textAlign: 'center', lineHeight: 1.2 }}>{value}</span>
        </div>
      ))}
    </div>
  )
}

// ── Air picture ───────────────────────────────────────────────────────────────

function AirPicture({ online }: { online: OnlinePilot[] }) {
  const airborne = online.filter(p => p.aircraft != null)
  const blue = airborne.filter(p => p.side === 'Blue')
  const red  = airborne.filter(p => p.side === 'Red')

  function PilotRow({ p }: { p: OnlinePilot }) {
    const col = p.side === 'Blue' ? campaign.blueColor : campaign.redColor
    return (
      <div style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '5px 12px', borderBottom: '1px solid var(--border)' }}>
        <Aircraft size={9} style={{ color: col, flexShrink: 0 }} />
        <span style={{ fontSize: '0.76rem', fontWeight: 600, color: 'var(--text)', flex: 1, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{p.name}</span>
        <span className="font-mono-vs" style={{ fontSize: '0.65rem', color: col, flexShrink: 0 }}>{p.aircraft}</span>
      </div>
    )
  }

  if (!airborne.length) return (
    <div style={{ padding: '12px', fontSize: '0.62rem', color: 'var(--text-dim)', textAlign: 'center', fontFamily: 'var(--font-mono)', letterSpacing: '0.1em' }}>
      NO AIRCRAFT AIRBORNE
    </div>
  )

  return (
    <>
      {blue.length > 0 && (
        <>
          <div style={{ padding: '2px 12px', background: `${campaign.blueColor}0a`, borderBottom: `1px solid ${campaign.blueColor}18` }}>
            <span style={{ fontSize: '0.6rem', color: campaign.blueColor, letterSpacing: '0.18em', fontFamily: 'var(--font-mono)', fontWeight: 700 }}>
              {campaign.blueLabel.toUpperCase()} · {blue.length}
            </span>
          </div>
          {blue.map(p => <PilotRow key={p.ucid} p={p} />)}
        </>
      )}
      {red.length > 0 && (
        <>
          <div style={{ padding: '2px 12px', background: `${campaign.redColor}0a`, borderBottom: `1px solid ${campaign.redColor}18` }}>
            <span style={{ fontSize: '0.6rem', color: campaign.redColor, letterSpacing: '0.18em', fontFamily: 'var(--font-mono)', fontWeight: 700 }}>
              {campaign.redLabel.toUpperCase()} · {red.length}
            </span>
          </div>
          {red.map(p => <PilotRow key={p.ucid} p={p} />)}
        </>
      )}
    </>
  )
}

// ── Online roster (ground) ────────────────────────────────────────────────────

function GroundRoster({ online }: { online: OnlinePilot[] }) {
  const navigate = useNavigate()
  const grounded = online.filter(p => p.aircraft == null)
  if (!grounded.length) return null
  return (
    <>
      <div style={{ padding: '2px 12px', background: 'rgba(0,0,0,0.1)', borderBottom: '1px solid var(--border)', borderTop: '1px solid var(--border)' }}>
        <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.18em', fontFamily: 'var(--font-mono)', fontWeight: 700 }}>ON GROUND · {grounded.length}</span>
      </div>
      {grounded.slice(0, 6).map(p => {
        const col = p.side === 'Blue' ? campaign.blueColor : p.side === 'Red' ? campaign.redColor : 'var(--text-dim)'
        return (
          <div key={p.ucid} onClick={() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`)} className="kill-row"
            style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '5px 12px', borderBottom: '1px solid var(--border)', cursor: 'pointer' }}>
            <span style={{ width: 5, height: 5, background: col, flexShrink: 0 }} />
            <span style={{ fontSize: '0.74rem', color: 'var(--text)', fontWeight: 500, flex: 1 }}>{p.name}</span>
            <span style={{ fontSize: '0.62rem', color: col, fontFamily: 'var(--font-mono)' }}>{p.side.slice(0,3).toUpperCase()}</span>
          </div>
        )
      })}
    </>
  )
}

// ── Engagement log ────────────────────────────────────────────────────────────

function EngagementLog({ kills, nameMap }: { kills: Kill[]; nameMap: Map<string, string> }) {
  const shown = kills.slice(0, campaign.dashboardKillFeedCount)
  if (!shown.length) return (
    <div style={{ padding: '12px', fontSize: '0.62rem', color: 'var(--text-dim)', textAlign: 'center', fontFamily: 'var(--font-mono)', letterSpacing: '0.1em' }}>
      NO ENGAGEMENTS
    </div>
  )
  return (
    <>
      {shown.map((k, i) => {
        const { label, col } = classifyKill(k.target_type)
        const killerSide = k.killer?.side
        const kc = killerSide === 'Blue' ? campaign.blueColor : killerSide === 'Red' ? campaign.redColor : 'var(--text-dim)'
        const vc = k.victim.side === 'Blue' ? campaign.blueColor : k.victim.side === 'Red' ? campaign.redColor : 'var(--text-muted)'
        const killerName = k.killer?.ucid ? nameMap.get(k.killer.ucid) : null
        // Determine display name: player name > airframe+side label > AI > ENV (no killer = crash/env)
        const killerLabel = killerName
          ?? (k.killer
            ? (killerSide === 'Blue' || killerSide === 'Red' ? killerSide : null) ?? k.killer.airframe ?? 'AI'
            : 'ENV')
        return (
          <div key={i} style={{ padding: '6px 12px', borderBottom: '1px solid var(--border)', display: 'grid', gridTemplateColumns: 'auto 1fr auto', gap: 7, alignItems: 'start' }}>
            <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: 2, paddingTop: 1 }}>
              <span style={{
                fontSize: '0.58rem', fontWeight: 700, color: col, border: `1px solid ${col}`,
                padding: '1px 4px', lineHeight: 1.4, fontFamily: 'var(--font-mono)', background: `${col}12`, letterSpacing: '0.08em',
              }}>{label}</span>
            </div>
            <div style={{ minWidth: 0 }}>
              <div style={{ fontSize: '0.74rem', fontWeight: 600, color: kc, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                {killerLabel}
                {killerName && k.killer?.airframe && <span style={{ fontWeight: 400, color: 'var(--text-dim)', fontSize: '0.65rem' }}> · {k.killer.airframe}</span>}
                {!killerName && k.killer?.airframe && killerLabel !== k.killer.airframe && <span style={{ fontWeight: 400, color: 'var(--text-dim)', fontSize: '0.65rem' }}> · {k.killer.airframe}</span>}
              </div>
              <div style={{ fontSize: '0.65rem', color: 'var(--text-dim)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginTop: 1 }}>
                <span style={{ color: 'var(--text-dim)' }}>↳ </span>
                <span style={{ color: vc }}>{k.victim.side}</span>
                {k.target_type && <span> {k.target_type}</span>}
                {k.killer?.weapon && <span style={{ color: 'var(--text-dim)' }}> [{k.killer.weapon}]</span>}
              </div>
            </div>
            <span className="font-mono-vs" style={{ fontSize: '0.63rem', color: 'var(--text-dim)', whiteSpace: 'nowrap', paddingTop: 1 }}>
              {fmtTimeZ(k.time)}
            </span>
          </div>
        )
      })}
    </>
  )
}

// ── Top shooters ──────────────────────────────────────────────────────────────

function TopShooters({ pilots }: { pilots: Pilot[] }) {
  const top = useMemo(() =>
    [...pilots].sort((a, b) => (b.air_kills + b.ground_kills) - (a.air_kills + a.ground_kills)).slice(0, 5),
    [pilots]
  )
  const navigate = useNavigate()
  if (!top.length) return (
    <div style={{ padding: '10px 12px', fontSize: '0.68rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', letterSpacing: '0.1em' }}>NO DATA</div>
  )
  const maxKills = top[0] ? (top[0].air_kills + top[0].ground_kills) || 1 : 1
  return (
    <>
      {top.map((p, i) => {
        const kills = p.air_kills + p.ground_kills
        const pct   = kills / maxKills * 100
        return (
          <div key={p.ucid} onClick={() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`)} className="kill-row"
            style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '5px 12px', borderBottom: '1px solid var(--border)', cursor: 'pointer' }}>
            <span style={{ fontSize: '0.68rem', fontWeight: 700, color: i === 0 ? 'var(--yellow)' : 'var(--text-dim)', fontFamily: 'var(--font-mono)', width: 12, flexShrink: 0 }}>{i + 1}</span>
            <div style={{ flex: 1, minWidth: 0 }}>
              <div style={{ fontSize: '0.75rem', fontWeight: 600, color: 'var(--text)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 2 }}>
                {p.name}
              </div>
              <div style={{ height: 3, background: 'var(--bg-elevated)' }}>
                <div style={{ width: `${pct}%`, height: '100%', background: i === 0 ? 'var(--yellow)' : 'var(--accent)' }} />
              </div>
            </div>
            <div style={{ flexShrink: 0, textAlign: 'right' }}>
              <div style={{ display: 'flex', gap: 6, alignItems: 'baseline', justifyContent: 'flex-end' }}>
                <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>A2A</span>
                <span className="font-mono-vs" style={{ fontSize: '0.74rem', fontWeight: 700, color: i === 0 ? 'var(--yellow)' : 'var(--blue)' }}>{p.air_kills}</span>
                <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>GND</span>
                <span className="font-mono-vs" style={{ fontSize: '0.74rem', fontWeight: 700, color: i === 0 ? 'var(--yellow)' : 'var(--orange)' }}>{p.ground_kills}</span>
              </div>
              <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', textAlign: 'right', marginTop: 1 }}>{p.deaths}D</div>
            </div>
          </div>
        )
      })}
    </>
  )
}

// The SRS radio-net panel lived here until the war news replaced it in the
// SITREP column. If it is wanted back it is in git history: a self-contained
// SrsPanel plus fmtFreq/fmtMod/isLiveRadio helpers, fed by api.srs(). The
// /api/srs endpoint is untouched and still serves it.

const PANEL_BODY: React.CSSProperties = { flex: 1, overflow: 'hidden', display: 'flex', flexDirection: 'column' }

// ── Live badge ─────────────────────────────────────────────────────────────────

function LiveBadge() {
  return (
    <span className="vs-pulse" style={{
      display: 'inline-flex', alignItems: 'center', gap: 4,
      fontSize: '0.6rem', color: 'var(--accent-bright)', fontFamily: 'var(--font-mono)',
      letterSpacing: '0.14em', border: '1px solid var(--accent-border)', padding: '1px 6px',
    }}>
      <span style={{ width: 5, height: 5, borderRadius: '50%', background: 'var(--accent)', display: 'inline-block' }} />
      LIVE
    </span>
  )
}

// ── Main ──────────────────────────────────────────────────────────────────────

/** Objectives that need a look, most urgent first; opens the drawer. */
function FrontLine({ items, onOpen }: { items: Attention[]; onOpen: (name: string) => void }) {
  if (items.length === 0) {
    return <div style={{ padding: '12px 14px', fontSize: '0.66rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>QUIET FRONT — NOTHING UNDER ATTACK OR FAILING</div>
  }
  return (
    <>
      {items.map(({ obj, reasons }) => (
        <button key={obj.id} type="button" onClick={() => onOpen(obj.name)}
          style={{ all: 'unset', boxSizing: 'border-box', width: '100%', cursor: 'pointer', display: 'flex', alignItems: 'center', gap: 8, padding: '6px 12px', borderBottom: '1px solid var(--border)' }}>
          <span style={{ width: 6, height: 6, borderRadius: '50%', flexShrink: 0, background: obj.owner === 'Blue' ? campaign.blueColor : obj.owner === 'Red' ? campaign.redColor : 'var(--text-dim)' }} />
          <span style={{ fontSize: '0.72rem', fontWeight: 600, color: 'var(--text)', whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis', minWidth: 0, flex: '0 1 auto' }}>{obj.name}</span>
          <span className="flex flex-wrap justify-end" style={{ gap: 4, marginLeft: 'auto' }}>
            {reasons.slice(0, 2).map(r => (
              <span key={r.text} style={{ fontSize: '0.56rem', padding: '1px 5px', borderRadius: 2, whiteSpace: 'nowrap', color: TONE[r.tone].fg, background: TONE[r.tone].bg }}>{r.text}</span>
            ))}
          </span>
        </button>
      ))}
    </>
  )
}

/** Objectives held per side over the round, from the round report. Absent
 *  (renders nothing) on a bfdb without /rounds/:id/report. */
function TerritoryTrend({ roundId }: { roundId: number | null }) {
  const { data: rep } = useQuery({
    queryKey: ['round-report', roundId], queryFn: () => api.roundReport(roundId!),
    enabled: roundId != null, refetchInterval: 5 * 60_000, retry: false,
  })
  if (!rep || rep.buckets.length < 2) return null
  return (
    <div style={{ padding: '0 14px 8px' }}>
      <div style={{ fontSize: '0.56rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', letterSpacing: '0.12em', marginBottom: 2 }}>THIS ROUND</div>
      <ResponsiveContainer width="100%" height={46}>
        <AreaChart data={rep.buckets} margin={{ left: 0, right: 0, top: 2, bottom: 0 }}>
          <YAxis hide domain={[0, rep.objectives || 'auto']} />
          <Area type="stepAfter" dataKey="blue_held" stackId="t" stroke={campaign.blueColor} strokeWidth={1.5} fill={campaign.blueColor} fillOpacity={0.15} isAnimationActive={false} />
          <Area type="stepAfter" dataKey="red_held" stackId="t" stroke={campaign.redColor} strokeWidth={1.5} fill={campaign.redColor} fillOpacity={0.15} isAnimationActive={false} />
        </AreaChart>
      </ResponsiveContainer>
    </div>
  )
}

export default function Dashboard() {
  const navigate = useNavigate()
  const { selectedRound } = useRound()
  // A Hornet in wireframe over the stat row. The territory panel keeps the
  // selected server's flags: it is the one place they say whose ground is whose.
  const kpiArt = useWireframe('fa18', { base: 'var(--bg-chrome)', strength: 'soft' })
  const territoryFlags = useFlagBackground('var(--bg-card)', 'var(--bg-card-solid)', 'soft')

  const onlineQ = useQuery({ queryKey: ['online'], queryFn: api.online, refetchInterval: 10_000 })
  const { data: online     = [] } = onlineQ
  const { data: pilots     = [] } = useQuery({ queryKey: ['leaderboard'],               queryFn: api.leaderboard,                                                      refetchInterval: 60_000 })
  const { data: allPilots  = [] } = useQuery({ queryKey: ['all-pilots'],                queryFn: api.allPilots,                                                        refetchInterval: 120_000 })
  const objectivesQ = useQuery({ queryKey: ['objectives', selectedRound], queryFn: () => api.objectives(selectedRound), refetchInterval: 30_000 })
  const { data: objectives = [] } = objectivesQ
  const { data: fronts = { mid: [], blue: [], red: [] } } = useQuery<Frontlines>({ queryKey: ['frontline', selectedRound], queryFn: () => api.frontline(selectedRound), refetchInterval: 30_000 })
  const { data: kills      = [] } = useQuery({ queryKey: ['kills-dash', selectedRound], queryFn: () => api.kills(selectedRound, campaign.dashboardKillFeedCount + 20), refetchInterval: 15_000 })
  const { data: stats            } = useQuery({ queryKey: ['stats'],                    queryFn: api.stats,                                                            refetchInterval: 60_000 })
  // Shares the Kill Feed's cache; the KPI counts the last hour from it.
  const { data: feed = [] } = useQuery({ queryKey: ['kills-feed', selectedRound], queryFn: () => api.kills(selectedRound, 500), refetchInterval: 30_000 })
  const { data: captures = [] } = useQuery({ queryKey: ['capture-events', selectedRound], queryFn: () => api.captureEvents(selectedRound, 200), refetchInterval: 60_000 })
  const { data: rounds = [] } = useQuery({ queryKey: ['rounds'], queryFn: () => api.rounds(), refetchInterval: 5 * 60_000 })
  const [drawer, setDrawer] = useState<string | null>(null)
  const { data: news, error: newsErr } = useQuery({ queryKey: ['news'],                   queryFn: () => api.news(5),                                                    refetchInterval: 5 * 60_000 })

  const nameMap = useMemo(() => {
    const m = new Map<string, string>()
    ;(allPilots as PilotName[]).forEach(p => m.set(p.ucid, p.name))
    return m
  }, [allPilots])

  const blue    = online.filter(p => p.side === 'Blue')
  const red     = online.filter(p => p.side === 'Red')
  const inAir   = online.filter(p => p.aircraft != null).length
  const blueObj = objectives.filter(o => o.owner === 'Blue').length
  const redObj  = objectives.filter(o => o.owner === 'Red').length
  const total   = objectives.length
  const [now, setNow] = useState(() => Date.now())
  useEffect(() => { const t = setInterval(() => setNow(Date.now()), 60_000); return () => clearInterval(t) }, [])
  const attention = useMemo(() => attentionList(objectives, captures, now), [objectives, captures, now])
  const atRisk = attention.filter(a => a.rank <= 2).length
  const lastHour = useMemo(() => {
    const recent = feed.filter(k => now - Date.parse(k.time) < 3_600_000)
    return { all: recent.length, blue: recent.filter(k => k.killer?.side === 'Blue').length, red: recent.filter(k => k.killer?.side === 'Red').length }
  }, [feed, now])
  const roundId = selectedRound ?? rounds.find(r => r.active)?.id ?? null

  const blueDelta = useDelta(blueObj)
  const redDelta  = useDelta(redObj)

  // Below this width the 3-column body (40/35/25%) stacks vertically --
  // at phone widths the fixed percentages squeezed the map down to a
  // near-unusable sliver with its overlay badges crammed on top of it.
  const [isMobile, setIsMobile] = useState(() => window.innerWidth <= 860)
  useEffect(() => {
    const onResize = () => setIsMobile(window.innerWidth <= 860)
    window.addEventListener('resize', onResize)
    return () => window.removeEventListener('resize', onResize)
  }, [])

  return (
    <div style={{ display: 'flex', flexDirection: 'column', flex: 1, overflow: 'hidden' }}>

      {/* A failed poll used to be invisible here: every query defaults to an
          empty array, so "bfdb is down" and "quiet server" drew the same
          zeros. Call it out once, above the strip. */}
      {(objectivesQ.isError || onlineQ.isError) && (
        <div style={{
          display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0,
          padding: '6px 14px', background: 'rgba(204,68,68,0.10)',
          borderBottom: '1px solid rgba(204,68,68,0.35)', color: 'var(--red)',
          fontFamily: 'var(--font-mono)', fontSize: '0.66rem', letterSpacing: '0.1em',
        }}>
          <Alert size={11} />
          <span style={{ flex: 1, textTransform: 'uppercase' }}>
            Live data unavailable — Server Offline
          </span>
          <button
            type="button"
            onClick={() => { objectivesQ.refetch(); onlineQ.refetch() }}
            style={{
              display: 'inline-flex', alignItems: 'center', gap: 4, cursor: 'pointer',
              background: 'transparent', border: '1px solid rgba(204,68,68,0.4)',
              color: 'var(--red)', font: 'inherit', padding: '2px 8px', textTransform: 'uppercase',
            }}
          >
            <RefreshCw size={10} />
            Retry
          </button>
        </div>
      )}

      {/* ── KPI strip ── */}
      <div className="kpi-strip" style={kpiArt}>
        <KpiCell label="ONLINE" value={online.length} icon={PilotIcon}
          sub={`${blue.length} blu · ${red.length} red`} />
        <KpiCell label="AIRBORNE" value={inAir} icon={Aircraft} color="var(--accent)"
          sub={online.length > 0 ? `${Math.round(inAir / online.length * 100)}% of online` : '—'} />
        <KpiCell label={`${campaign.blueLabel} OBJ`} value={blueObj}
          icon={Shield} color={campaign.blueColor} delta={blueDelta}
          sub={`${total > 0 ? Math.round(blueObj / total * 100) : 0}% territory`} />
        <KpiCell label={`${campaign.redLabel} OBJ`} value={redObj}
          icon={Shield} color={campaign.redColor} delta={redDelta}
          sub={`${total > 0 ? Math.round(redObj / total * 100) : 0}% territory`} />
        <KpiCell label="AT RISK" value={atRisk} icon={Alert}
          color={atRisk > 0 ? 'var(--red)' : 'var(--text-dim)'}
          sub="falling or under attack" />
        <KpiCell label="KILLS · 1H" value={lastHour.all} icon={KillIcon} color="var(--red)"
          sub={`${lastHour.blue} blu · ${lastHour.red} red`} />
        <KpiCell label="TOTAL PILOTS" value={stats?.total_pilots ?? '—'}
          sub={`${stats?.blue_registered ?? 0} blu · ${stats?.red_registered ?? 0} red`} />
      </div>

      {/* ── Main body ── */}
      <div style={{ display: 'flex', flexDirection: isMobile ? 'column' : 'row', flex: 1, overflow: isMobile ? 'auto' : 'hidden' }}>

        {/* ══ LEFT COLUMN — TacMap (40%) ══ */}
        <div style={{
          width: isMobile ? '100%' : '40%', flexShrink: 0, display: 'flex', flexDirection: 'column',
          borderRight: isMobile ? 'none' : '1px solid var(--border)',
          borderBottom: isMobile ? '1px solid var(--border)' : 'none',
        }}>
          {/* Map header */}
          <div style={{ padding: '5px 12px', background: 'rgba(0,0,0,0.3)', borderBottom: '1px solid var(--border)', flexShrink: 0, display: 'flex', alignItems: 'center', gap: 8 }}>
            <Pin size={9} style={{ color: 'var(--accent)' }} />
            <span style={{ fontSize: '0.65rem', fontWeight: 700, letterSpacing: '0.22em', textTransform: 'uppercase', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', flex: 1 }}>
              TACTICAL OVERVIEW
            </span>
            <LiveBadge />
          </div>
          {/* Map */}
          <div style={isMobile ? { flex: '0 0 260px', overflow: 'hidden' } : { flex: '0 0 55%', overflow: 'hidden', minHeight: 0 }}>
            <TacMap objectives={objectives} fronts={fronts} onOpenTacmap={() => navigate('/map')} />
          </div>

          {/* War news, in place of the SRS radio net: the daily dispatch is
              what brings people back, and SRS client state reads better
              in-game than on a dashboard. /api/srs is untouched. */}
          <div style={isMobile
            ? { flex: '0 0 auto', maxHeight: 260, display: 'flex', flexDirection: 'column', borderTop: '1px solid var(--border)' }
            : { flex: 1, minHeight: 0, display: 'flex', flexDirection: 'column', borderTop: '1px solid var(--border)' }}>
            <div style={{ padding: '5px 12px', background: 'rgba(0,0,0,0.25)', borderBottom: '1px solid var(--border)', flexShrink: 0, display: 'flex', alignItems: 'center', gap: 8 }}>
              <Comms size={9} style={{ color: 'var(--accent)' }} />
              <span style={{ fontSize: '0.65rem', fontWeight: 700, letterSpacing: '0.22em', textTransform: 'uppercase', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', flex: 1 }}>
                WAR NEWS
              </span>
              {news?.days?.[0] && !news.days[0].final_ && (
                <span className="vs-pulse font-mono-vs" style={{ fontSize: '0.63rem', color: 'var(--accent)', border: '1px solid var(--accent-border)', padding: '1px 5px' }}>
                  TODAY
                </span>
              )}
            </div>
            <div style={{ overflowY: 'auto', flex: 1 }}>
              <NewsPanel days={news?.days} error={newsErr} />
            </div>
          </div>
        </div>

        {/* ══ CENTER COLUMN (35%) ══ */}
        <div style={{
          width: isMobile ? '100%' : '35%', flexShrink: 0, display: 'flex', flexDirection: 'column',
          borderRight: isMobile ? 'none' : '1px solid var(--border)',
          borderBottom: isMobile ? '1px solid var(--border)' : 'none',
          overflow: isMobile ? 'visible' : 'hidden',
        }}>

          {/* Weather -- only when the server reported a METAR. */}
          {stats?.weather && (
            <PanelBase size="sm" bodyStyle={PANEL_BODY} title="WEATHER BRIEF" icon={Wind} iconColor="var(--cyan)" style={{ flexShrink: 0 }}>
              <WeatherBrief stats={stats} />
            </PanelBase>
          )}

          {/* Territory */}
          <PanelBase size="sm" bodyStyle={PANEL_BODY} title="TERRITORY CONTROL" icon={Activity} iconColor="var(--yellow)" style={{ flexShrink: 0, ...territoryFlags }}>
            <TerritoryBar objectives={objectives} />
            <TerritoryTrend roundId={roundId} />
          </PanelBase>

          {/* Front line: what is under attack, falling, freshly taken or
              running dry. Replaces the full roster -- every base at 100%
              told nobody anything; the Objectives page has the table. */}
          <PanelBase size="sm" bodyStyle={PANEL_BODY} title="FRONT LINE" icon={Alert} iconColor="var(--red)" count={attention.length}
            style={isMobile ? { flex: '0 0 auto', maxHeight: 360 } : { flex: 1 }}>
            <div style={{ overflowY: 'auto', flex: 1 }}>
              <FrontLine items={attention} onOpen={setDrawer} />
            </div>
          </PanelBase>
        </div>

        {/* ══ RIGHT COLUMN (25%) ══ */}
        <div style={{
          flex: isMobile ? undefined : 1, width: isMobile ? '100%' : undefined,
          display: 'flex', flexDirection: 'column', overflow: isMobile ? 'visible' : 'hidden', minWidth: 0,
        }}>

          {/* Air picture */}
          <PanelBase size="sm" bodyStyle={PANEL_BODY} title="AIR PICTURE" icon={Aircraft} iconColor="var(--accent)"
            right={<LiveBadge />}
            count={inAir}
            style={isMobile ? { flex: '0 0 auto', maxHeight: 220 } : { flex: '0 0 auto', maxHeight: '30%' }}>
            <div style={{ overflowY: 'auto', flex: 1 }}>
              <AirPicture online={online} />
              <GroundRoster online={online} />
            </div>
          </PanelBase>

          {/* Top shooters */}
          <PanelBase size="sm" bodyStyle={PANEL_BODY} title="TOP SHOOTERS" icon={TrendingUp} iconColor="var(--yellow)" style={{ flexShrink: 0 }}>
            <TopShooters pilots={pilots} />
          </PanelBase>

          {/* Engagement log */}
          <PanelBase size="sm" bodyStyle={PANEL_BODY} title="ENGAGEMENT LOG" icon={KillIcon} iconColor="var(--red)"
            right={<LiveBadge />}
            count={kills.length}
            style={isMobile ? { flex: '0 0 auto', maxHeight: 320 } : { flex: 1 }}>
            <div style={{ overflowY: 'auto', flex: 1 }}>
              <EngagementLog kills={kills} nameMap={nameMap} />
            </div>
          </PanelBase>
        </div>
      </div>

      <ObjectiveDrawer objective={drawer} onClose={() => setDrawer(null)} onOpen={setDrawer} />
    </div>
  )
}
