import { useEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { ResponsiveContainer, AreaChart, Area, YAxis } from 'recharts'
import { Alert, ArrowRight, RefreshCw } from '@icons'
import {
  api, API_ROOT, roundNo,
  type Frontlines, type Kill, type NewsDay, type Objective, type OnlinePilot, type Pilot, type Round, type Stats,
} from '../api'
import type { ServerProfile } from '@shared/servers/profiles'
import { campaign } from '../config/campaign'
import TacMap, { type MapInset } from '../components/TacMap'
import type { Attention, Tone } from '../lib/objectives'
import { classifyTargetClass, cloudStr, fmtTimeZ, visStr, windDir } from '../lib/format'
import { pressable } from '@shared/web/a11y'
import { useFlagPair } from '../lib/flags'
import { useCountUp } from '../lib/countUp'
import { Card, Tape } from '../components/Levant'

// ─────────────────────────────────────────────────────────────────────────────
// SITREP in Levant Ablaze's look, laid out like the campaign's page on the
// site (bfsite/src/servers/SyriaPage.tsx): a band of battle art carrying the
// scoreboard, a strip of readouts, then three columns of cards with their
// names on tape. It fits the window, as the plain SITREP always has: nothing
// is below the fold, and a long list scrolls inside its own card. The look is the site's; everything on it is the dashboard's
// own live picture (Dashboard fetches it and hands it over). The Red Storm
// look, an admin's pick, keeps the dense three columns.
//
// The panels here are drawn for this page rather than borrowed from the
// three-column layout, whose inline styles were sized for a 25%-wide column.
// ─────────────────────────────────────────────────────────────────────────────

type SideKey = 'Blue' | 'Red'
const tone = (s: string | undefined | null): 'b' | 'r' | 'n' => (s === 'Blue' ? 'b' : s === 'Red' ? 'r' : 'n')
const sideName = (s: string | undefined | null) => (s === 'Blue' ? campaign.blueLabel : s === 'Red' ? campaign.redLabel : 'Neutral')

/** "Day 3" of a round, counted from its start in UTC (as the site counts it). */
function roundDay(start: string, end: string | null): number | undefined {
  const t = Date.parse(start)
  if (Number.isNaN(t)) return undefined
  const to = end ? Date.parse(end) : Date.now()
  return Math.max(1, Math.floor((to - t) / 86_400_000) + 1)
}

/** A figure that counts up to its value (lib/countUp); anything not a number passes through. */
function Num({ v }: { v: ReactNode }) {
  const n = typeof v === 'number' ? v : 0
  const shown = useCountUp(n, 900)
  return <>{typeof v === 'number' ? Math.round(shown).toLocaleString() : v}</>
}

function Live() {
  return <span className="lv-live"><i />Live</span>
}

function Empty({ children }: { children: ReactNode }) {
  return <p className="lv-empty">{children}</p>
}

// ── hero: the scoreboard ─────────────────────────────────────────────────────

interface SideScore { held: number; delta: number | null; online: number; airborne: number; kills: number }

/** A coalition's card: its flag is the card's ground, fading into it from the outer edge. */
function SideCard({ side, s, share, flag }: { side: SideKey; s: SideScore; share: number; flag: string }) {
  const b = side === 'Blue'
  return (
    <div className={`lv-score ${b ? 'b' : 'r'}`} style={{ '--flag': `url(${flag})` } as React.CSSProperties}>
      <div className="lv-score-id">
        <strong>{sideName(side)}</strong>
      </div>
      <div className="lv-score-big">
        <span className="v"><Num v={s.held} /></span>
        <span className="k">objectives{s.delta ? <em className={s.delta > 0 ? 'up' : 'down'}>{s.delta > 0 ? `+${s.delta}` : s.delta}</em> : null}</span>
      </div>
      <dl className="lv-score-row">
        <div><dt>Ground</dt><dd>{share}%</dd></div>
        <div><dt>Online</dt><dd>{s.online}</dd></div>
        <div><dt>Airborne</dt><dd>{s.airborne}</dd></div>
        <div><dt>Kills 1h</dt><dd>{s.kills}</dd></div>
      </dl>
    </div>
  )
}

/** Objectives held per side over the round, from the round report. */
function Trend({ roundId, total }: { roundId: number | null; total: number }) {
  const { data: rep } = useQuery({
    queryKey: ['round-report', roundId], queryFn: () => api.roundReport(roundId!),
    enabled: roundId != null, refetchInterval: 5 * 60_000, retry: false,
  })
  if (!rep || rep.buckets.length < 2) return null
  return (
    <div className="lv-trend">
      <span className="ds-k">This round</span>
      <ResponsiveContainer width="100%" height={54}>
        <AreaChart data={rep.buckets} margin={{ left: 0, right: 0, top: 2, bottom: 0 }}>
          <YAxis hide domain={[0, rep.objectives || total || 'auto']} />
          <Area type="stepAfter" dataKey="blue_held" stackId="t" stroke={campaign.blueColor} strokeWidth={1.5} fill={campaign.blueColor} fillOpacity={0.2} isAnimationActive={false} />
          <Area type="stepAfter" dataKey="red_held" stackId="t" stroke={campaign.redColor} strokeWidth={1.5} fill={campaign.redColor} fillOpacity={0.2} isAnimationActive={false} />
        </AreaChart>
      </ResponsiveContainer>
    </div>
  )
}

function Tug({ objectives, roundId }: { objectives: Objective[]; roundId: number | null }) {
  const total = objectives.length
  const n = (o: Objective['owner']) => objectives.filter(x => x.owner === o).length
  const [blue, red, neu] = [n('Blue'), n('Red'), n('Neutral')]
  const pct = (v: number) => (total ? (v / total) * 100 : 0)
  // Each side's fill runs out from its own edge and the figures count up with
  // it (lib/countUp); after that they glide to each new split.
  const b = useCountUp(pct(blue))
  const r = useCountUp(pct(red))
  const nn = useCountUp(pct(neu))
  return (
    <div className="lv-tug">
      <div className="lv-tug-head">
        <span className="b">{Math.round(b)}%</span>
        <span className="ds-k">The front line{neu ? ` · ${neu} neutral` : ''}</span>
        <span className="r">{Math.round(r)}%</span>
      </div>
      <div className="lv-tug-bar fx" role="img" aria-label={`${campaign.blueLabel} ${blue}, ${campaign.redLabel} ${red}, neutral ${neu}`}>
        <i className="b" style={{ width: `${b}%` }} />
        <i className="n" style={{ width: `${nn}%` }} />
        <i className="r" style={{ width: `${r}%` }} />
        <span className="mid" />
        <span className="front" style={{ left: `${b}%` }} />
      </div>
      <Trend roundId={roundId} total={total} />
    </div>
  )
}

// ── readouts ─────────────────────────────────────────────────────────────────

function Readout({ k, v, sub, alarm }: { k: string; v: ReactNode; sub?: ReactNode; alarm?: boolean }) {
  return (
    <div className={`lv-readout${alarm ? ' alarm' : ''}`}>
      <span className="ds-k">{k}</span>
      <span className="lv-v"><Num v={v} /></span>
      {sub && <span className="lv-sub">{sub}</span>}
    </div>
  )
}

function Conditions({ stats }: { stats?: Stats }) {
  const w = stats?.weather
  if (!w) return <div className="lv-readout wide"><span className="ds-k">Conditions</span><span className="lv-sub">No weather report from the server</span></div>
  return (
    <div className="lv-readout wide">
      <span className="ds-k">Conditions</span>
      <dl className="lv-wx">
        <div><dt>Wind</dt><dd>{windDir(w.wind_from_deg)} {Math.round(w.wind_speed_kts)}<small>kt</small></dd></div>
        <div><dt>Temp</dt><dd>{Math.round(w.temp_c)}<small>°C</small></dd></div>
        <div><dt>QNH</dt><dd>{Math.round(w.qnh_hpa)}</dd></div>
        <div><dt>Vis</dt><dd>{visStr(w.visibility_m)}</dd></div>
        <div><dt>Cloud</dt><dd>{cloudStr(w.cloud_base_m)}</dd></div>
      </dl>
    </div>
  )
}

// ── front line ───────────────────────────────────────────────────────────────

const REASON_TONE: Record<Tone, 'r' | 'warn' | 'n'> = { crit: 'r', warn: 'warn', info: 'n' }

function Meter({ k, v }: { k: string; v: number }) {
  const t = v < 25 ? 'low' : v < 60 ? 'mid' : 'ok'
  return (
    <div className={`lv-meter ${t}`}>
      <span>{k}</span>
      <div><i style={{ width: `${Math.max(0, Math.min(100, v))}%` }} /></div>
      <b>{v}%</b>
    </div>
  )
}

function Attentions({ items, onOpen }: { items: Attention[]; onOpen: (name: string) => void }) {
  if (!items.length) return <Empty>Quiet front. Nothing is under attack or running dry.</Empty>
  return (
    <ul className="lv-att">
      {items.map(({ obj, reasons }) => (
        <li key={obj.id}>
          <button type="button" className={`lv-att-row ${tone(obj.owner)}`} onClick={() => onOpen(obj.name)}>
            <div className="lv-att-top">
              <strong>{obj.name}</strong>
              <span className="lv-att-kind">{sideName(obj.owner)} · {obj.kind}</span>
            </div>
            <div className="lv-att-tags">
              {reasons.map(r => <Tape key={r.text} tone={REASON_TONE[r.tone]} className="sm">{r.text}</Tape>)}
            </div>
            <div className="lv-att-meters">
              <Meter k="Def" v={obj.health} />
              <Meter k="Sup" v={obj.supply} />
              <Meter k="Fuel" v={obj.fuel} />
            </div>
          </button>
        </li>
      ))}
    </ul>
  )
}

// ── air war ──────────────────────────────────────────────────────────────────

function InTheAir({ online }: { online: OnlinePilot[] }) {
  const navigate = useNavigate()
  const up = online.filter(p => p.aircraft != null)
  const down = online.filter(p => p.aircraft == null)
  const open = (u: string) => navigate(`/pilots?ucid=${encodeURIComponent(u)}`)
  return (
    <>
      <div className="lv-air-cols">
        {(['Blue', 'Red'] as const).map(side => {
          const list = up.filter(p => p.side === side)
          return (
            <div key={side} className={`lv-air-col ${tone(side)}`}>
              <div className="lv-air-col-head"><strong>{sideName(side)}</strong><span>{list.length} up</span></div>
              {list.length ? (
                <ul>
                  {list.map(p => (
                    <li key={p.ucid} {...pressable(() => open(p.ucid))}>
                      <span className="ac">{p.aircraft}</span>
                      <span className="nm">{p.name}</span>
                    </li>
                  ))}
                </ul>
              ) : <Empty>Nobody airborne</Empty>}
            </div>
          )
        })}
      </div>
      {down.length > 0 && (
        <div className="lv-ground">
          <span className="ds-k">On the ground · {down.length}</span>
          <div>
            {down.slice(0, 18).map(p => (
              <span key={p.ucid} className={`lv-chip ${tone(p.side)}`} {...pressable(() => open(p.ucid))}>{p.name}</span>
            ))}
            {down.length > 18 && <span className="lv-chip n">+{down.length - 18}</span>}
          </div>
        </div>
      )}
    </>
  )
}

function Shooters({ pilots }: { pilots: Pilot[] }) {
  const navigate = useNavigate()
  const top = useMemo(
    () => [...pilots].sort((a, b) => (b.air_kills + b.ground_kills) - (a.air_kills + a.ground_kills)).slice(0, 5),
    [pilots],
  )
  if (!top.length) return <Empty>No kills on record yet</Empty>
  const max = (top[0].air_kills + top[0].ground_kills) || 1
  return (
    <ol className="lv-shooters">
      {top.map((p, i) => {
        return (
          <li key={p.ucid} className={`${tone(p.side)}${i === 0 ? ' first' : ''}`} {...pressable(() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`))}>
            <span className="rk">{i + 1}</span>
            <div className="who">
              <strong>{p.name}</strong>
              <div className="bar"><i className="a" style={{ width: `${(p.air_kills / max) * 100}%` }} /><i className="g" style={{ width: `${(p.ground_kills / max) * 100}%` }} /></div>
            </div>
            <dl>
              <div><dt>Air</dt><dd>{p.air_kills}</dd></div>
              <div><dt>Gnd</dt><dd>{p.ground_kills}</dd></div>
              <div><dt>Lost</dt><dd>{p.deaths}</dd></div>
            </dl>
          </li>
        )
      })}
    </ol>
  )
}

function Engagements({ kills, nameMap }: { kills: Kill[]; nameMap: Map<string, string> }) {
  const shown = kills.slice(0, campaign.dashboardKillFeedCount)
  // Keyed by the kill, not its row, so a new one drops in at the top while
  // the rest keep their places (levant-motion.css). Two identical kills in
  // one second get a running suffix.
  const seen = new Map<string, number>()
  const keyOf = (k: Kill) => {
    const base = `${k.time}|${k.killer?.ucid ?? k.killer?.airframe ?? ''}|${k.victim.ucid ?? ''}|${k.target_type ?? ''}`
    const n = seen.get(base) ?? 0
    seen.set(base, n + 1)
    return n ? `${base}#${n}` : base
  }
  if (!shown.length) return <Empty>No engagements yet this round</Empty>
  return (
    <ol className="lv-eng">
      {shown.map(k => {
        const cls = classifyTargetClass(k.target_type)
        const killerName = k.killer?.ucid ? nameMap.get(k.killer.ucid) : null
        const killer = killerName ?? (k.killer ? (k.killer.airframe ?? 'AI') : 'Crash / environment')
        return (
          <li key={keyOf(k)} className={tone(k.killer?.side)}>
            <time>{fmtTimeZ(k.time)}</time>
            <span className="cls">{cls}</span>
            <div className="what">
              <div className="by">
                <b>{killer}</b>
                {killerName && k.killer?.airframe && <span> · {k.killer.airframe}</span>}
              </div>
              <div className="on">
                <span className={`v ${tone(k.victim.side)}`}>{sideName(k.victim.side)}</span>
                {k.target_type && <> {k.target_type}</>}
                {k.killer?.weapon && <em> · {k.killer.weapon}</em>}
              </div>
            </div>
          </li>
        )
      })}
    </ol>
  )
}

// ── war news ─────────────────────────────────────────────────────────────────

function DayImage({ day }: { day: NewsDay }) {
  const [failed, setFailed] = useState(false)
  if (!day.image || failed) return null
  return <img className="lv-news-img" src={`${API_ROOT}${day.image}`} alt="" loading="lazy" decoding="async" onError={() => setFailed(true)} />
}

function dayLine(d: NewsDay) {
  return `${d.facts.campaign_day ? `Day ${d.facts.campaign_day} · ` : ''}${d.day}${d.final_ ? '' : ' · developing'}`
}

function paragraphs(d: NewsDay, n?: number): string[] {
  const all = d.body?.length ? d.body : d.items.map(i => i.text)
  return n ? all.slice(0, n) : all
}

function News({ days, error }: { days?: NewsDay[]; error?: unknown }) {
  if (error != null && !days) return <Empty>The war diary is not available on this server</Empty>
  if (!days) return <Empty>Loading the wire…</Empty>
  if (!days.length) return <Empty>No dispatches yet. The first is filed once the campaign has a day behind it.</Empty>
  const [lead, ...older] = days
  return (
    <div className="lv-news">
      <article className="lv-news-lead">
        <DayImage day={lead} />
        <span className="ds-k">{dayLine(lead)}</span>
        <h3>{lead.headline}</h3>
        {paragraphs(lead, 2).map((t, i) => <p key={i}>{t}</p>)}
      </article>
      {older.slice(0, 2).map(d => (
        <article key={d.day} className="lv-news-small">
          <span className="ds-k">{dayLine(d)}</span>
          <h4>{d.headline}</h4>
        </article>
      ))}
      <Link className="ds-link lv-news-more" to="/news">The war diary <ArrowRight size={12} /></Link>
    </div>
  )
}

// ── the stage: the map under everything ──────────────────────────────────────

/** Below this the columns stack and the map is a card again (levant.css). */
const STACKED = '(max-width: 1100px), (max-height: 620px)'

function useStacked() {
  const [stacked, setStacked] = useState(() => window.matchMedia(STACKED).matches)
  useEffect(() => {
    const mq = window.matchMedia(STACKED)
    const on = () => setStacked(mq.matches)
    mq.addEventListener('change', on)
    return () => mq.removeEventListener('change', on)
  }, [])
  return stacked
}

/** How much of the map the two columns cover, so the plot fits the open middle. */
function useInset(stage: React.RefObject<HTMLDivElement | null>, left: React.RefObject<HTMLDivElement | null>, right: React.RefObject<HTMLDivElement | null>, on: boolean): MapInset {
  const [inset, setInset] = useState<MapInset>({ left: 0, right: 0, top: 0, bottom: 0 })
  useEffect(() => {
    const st = stage.current, l = left.current, r = right.current
    if (!on || !st || !l || !r) return
    const measure = () => {
      const sb = st.getBoundingClientRect(), lb = l.getBoundingClientRect(), rb = r.getBoundingClientRect()
      const next = { left: Math.round(lb.right - sb.left), right: Math.round(sb.right - rb.left), top: 30, bottom: 0 }
      setInset(cur => (cur.left === next.left && cur.right === next.right ? cur : next))
    }
    measure()
    const ro = new ResizeObserver(measure)
    ro.observe(st)
    return () => ro.disconnect()
  }, [stage, left, right, on])
  return inset
}

// ── the page ─────────────────────────────────────────────────────────────────

export interface LevantSitrepProps {
  profile: ServerProfile
  round?: Round
  roundId: number | null
  online: OnlinePilot[]
  objectives: Objective[]
  fronts: Frontlines
  attention: Attention[]
  kills: Kill[]
  feed: Kill[]
  now: number
  pilots: Pilot[]
  nameMap: Map<string, string>
  stats?: Stats
  news?: NewsDay[]
  newsErr?: unknown
  blueDelta: number | null
  redDelta: number | null
  offline: boolean
  onRetry: () => void
  onOpen: (name: string) => void
}

export default function LevantSitrep(p: LevantSitrepProps) {
  const navigate = useNavigate()
  const stacked = useStacked()
  const stageRef = useRef<HTMLDivElement>(null)
  const leftRef = useRef<HTMLDivElement>(null)
  const rightRef = useRef<HTMLDivElement>(null)
  const inset = useInset(stageRef, leftRef, rightRef, !stacked)
  const flags = useFlagPair()
  const day = p.round ? roundDay(p.round.start, p.round.end) : undefined
  const [first, ...rest] = p.profile.title.split(' ')
  const total = p.objectives.length
  const recent = useMemo(() => p.feed.filter(k => p.now - Date.parse(k.time) < 3_600_000), [p.feed, p.now])
  const score = (side: SideKey, delta: number | null): SideScore => ({
    held: p.objectives.filter(o => o.owner === side).length,
    delta,
    online: p.online.filter(x => x.side === side).length,
    airborne: p.online.filter(x => x.side === side && x.aircraft != null).length,
    kills: recent.filter(k => k.killer?.side === side).length,
  })
  const blue = score('Blue', p.blueDelta)
  const red = score('Red', p.redDelta)
  const share = (v: number) => (total ? Math.round((v / total) * 100) : 0)
  const atRisk = p.attention.filter(a => a.rank <= 2).length
  const airborne = p.online.filter(x => x.aircraft != null).length

  return (
    <div className="lv-fit">
      <header className="lv-top">
        <div className="lv-top-art" role="img" aria-label="Two jets fighting low over a burning battlefield at dusk" />
        <div className="lv-top-scrim" />
        <div className="lv-top-title">
          <Tape tone="rust">{p.round?.active === false ? 'Round ended' : 'Situation report'}</Tape>
          <h1 className="lv-title">{first} <span>{rest.join(' ')}</span></h1>
          <div className="lv-round">
            {p.round ? <>Round <b>{roundNo(p.round)}</b>{day != null && <> · Day <b>{day}</b></>}</> : 'Between rounds'}
          </div>
        </div>
        <div className="lv-board">
          <SideCard side="Blue" s={blue} share={share(blue.held)} flag={flags.left} />
          <Tug objectives={p.objectives} roundId={p.roundId} />
          <SideCard side="Red" s={red} share={share(red.held)} flag={flags.right} />
        </div>
      </header>

      {p.offline && (
        <div className="lv-offline">
          <Alert size={12} />
          <span>Live data unavailable: the server is offline</span>
          <button type="button" onClick={p.onRetry}><RefreshCw size={11} /> Retry</button>
        </div>
      )}

      <div className="lv-strip" aria-label="Readouts">
        <Readout k="Pilots online" v={p.online.length} sub={`${airborne} airborne`} />
        <Readout k="At risk" v={atRisk} sub="falling or under attack" alarm={atRisk > 0} />
        <Readout k="Kills · 1h" v={recent.length} sub={`${blue.kills} ${campaign.blueLabel} · ${red.kills} ${campaign.redLabel}`} />
        <Readout k="On record" v={p.stats?.total_pilots ?? '—'} sub={`${p.stats?.blue_registered ?? 0} ${campaign.blueLabel} · ${p.stats?.red_registered ?? 0} ${campaign.redLabel}`} />
        <Conditions stats={p.stats} />
      </div>

      {/* The map is the ground the panels stand on: it fills the whole stage
          and the two columns float over its edges, the plot fitted to the
          open middle. Stacked (phone, short window), it is a card again. */}
      <div className={stacked ? 'lv-grid' : 'lv-stage'} ref={stageRef}>
        {stacked ? (
          <div className="lv-gcol lv-gcol-map">
            <Card tape="Tactical overview" tone="rust" right={<Live />} className="lv-map grow">
              <TacMap objectives={p.objectives} fronts={p.fronts} onOpenTacmap={() => navigate('/map')} />
            </Card>
          </div>
        ) : (
          <>
            <div className="lv-stage-map">
              <TacMap objectives={p.objectives} fronts={p.fronts} onOpenTacmap={() => navigate('/map')} inset={inset} />
            </div>
            <div className="lv-stage-fade" aria-hidden="true" />
            <div className="lv-stage-label"><Tape tone="rust">Tactical overview</Tape><Live /></div>
          </>
        )}
        <div className="lv-gcol lv-left" ref={leftRef}>
          <Card tape={<>Needs attention · {p.attention.length}</>} tone="r" bodyClass="lv-scroll" className="grow"
            right={<Link className="lv-more" to="/objectives">All</Link>}>
            <Attentions items={p.attention} onOpen={p.onOpen} />
          </Card>
          <Card tape={p.news?.[0] && !p.news[0].final_ ? 'War news · today' : 'War news'} tone="rust" bodyClass="lv-scroll" className="grow">
            <News days={p.news} error={p.newsErr} />
          </Card>
        </div>
        <div className="lv-gcol lv-right" ref={rightRef}>
          <Card tape={<>In the air · {airborne}</>} tone="b" right={<Live />} bodyClass="lv-scroll" className="lv-air-card">
            <InTheAir online={p.online} />
          </Card>
          <Card tape="Top shooters" tone="rust" right={<Link className="lv-more" to="/leaderboard">Rankings</Link>}>
            <Shooters pilots={p.pilots} />
          </Card>
          <Card tape={<>Engagement log · {p.kills.length}</>} tone="r" right={<Live />} bodyClass="lv-scroll" className="grow">
            <Engagements kills={p.kills} nameMap={p.nameMap} />
          </Card>
        </div>
      </div>
    </div>
  )
}
