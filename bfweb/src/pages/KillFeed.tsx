import React, { useCallback, useMemo, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Link as RouterLink } from 'react-router-dom'
import QueryState from '../components/QueryState'
import { api, type Kill } from '../api'
import SideBadge from '../components/SideBadge'
import PageHeader from '../components/PageHeader'
import {
  BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer, CartesianGrid, ReferenceLine,
} from 'recharts'

import {
  Aircraft, Helicopter, Ship, Armor, Sam, Arty, Infantry, Logistics, Radar,
  Structure, KillFeed as KillFeedIcon, Award, Activity, Strike,
  type IconComponent,
} from '@icons'
import { useRound } from '../context/RoundContext'
import { useTableSort, SortTh } from '../components/SortableTh'

type KillCol = 'time' | 'category' | 'killer' | 'airframe' | 'weapon' | 'target' | 'victim'
const KILL_COLS: { label: string; key: KillCol }[] = [
  { label: 'Time (Z)', key: 'time' },
  { label: 'Category', key: 'category' },
  { label: 'Killer', key: 'killer' },
  { label: 'Airframe', key: 'airframe' },
  { label: 'Weapon', key: 'weapon' },
  { label: 'Target Unit', key: 'target' },
  { label: 'Victim', key: 'victim' },
]

// Classify from DCS unit type name (target_type field contains the DCS type string)
function classifyTarget(targetType: string | null): { label: string; color: string; bg: string; icon?: IconComponent } {
  const t = (targetType ?? '').toLowerCase()
  // Aircraft patterns
  if (
    /\bf-\d|\bf\/a-|\bf-16|f-18|f-15|mig-|su-\d|a-10|av-8|eurofighter|typhoon|tornado|rafale|gripen|mirage|hawk|hornet|viper|eagle|flanker|fullback|frogfoot|fencer|foxbat|foxhound|warthog|harrier|phantom|lightning|hercules|globemaster|strategic/.test(t) ||
    /aircraft|plane|jet|fighter|bomber|transport|tanker|awacs|recon/.test(t)
  ) return { label: 'AIRCRAFT', color: '#60a5fa', bg: '#60a5fa18', icon: Aircraft }
  // Helicopter patterns
  if (/mi-|uh-|ah-|ch-|ka-|sa-\d|heli|huey|apache|blackhawk|chinook|hind|hip|havoc|hokum|seahawk|lynx|gazelle/.test(t) ||
    /helicopter|helo|rotary/.test(t)
  ) return { label: 'HELO', color: '#38bdf8', bg: '#38bdf818', icon: Helicopter }
  // Naval
  if (/ship|boat|frigate|destroyer|carrier|corvette|patrol|naval|lha|cvn|ddg|ffg|slava|kuznetsov|perry|oliver|oliver hazard/.test(t))
    return { label: 'NAVAL', color: '#06b6d4', bg: '#06b6d418', icon: Ship }
  // Armor
  if (/t-\d{2,3}|m1|m60|leopard|challenger|abrams|merkava|k1|leclerc|tank|apc|ifv|bmp-|btr-|bradley|marder|warrior|cv90|stryker|armored/.test(t) ||
    /armor|tank/.test(t)
  ) return { label: 'ARMOR', color: '#f97316', bg: '#f9731618', icon: Armor }
  // Air defence
  if (/sa-\d|s-300|s-400|patriot|hawk|roland|gepard|tunguska|shilka|flak|aaa|vulcan|linebacker|tor|buk|kub|sa-6|sa-8|sa-10|sa-11|sa-13|sa-15|sa-19|radar|tre|str|fan song|search|track|acquisition|crow bar|straight flush|flap lid|clam shell|dog ear|fire dome|low blow/.test(t) ||
    /sam |manpad|stinger|igla|strela|air.def/.test(t)
  ) return { label: 'AIR DEF', color: '#a78bfa', bg: '#a78bfa18', icon: Sam }
  // Artillery / MLRS
  if (/2s\d|m109|paladin|pzh|as-90|caesar|howitzer|mlrs|bm-21|bm-30|smerch|uragan|grad|artillery|mortar/.test(t))
    return { label: 'ARTY', color: '#fb923c', bg: '#fb923c18', icon: Arty }
  // Infantry
  if (/infantry|soldier|troop|manpad|squad|sniper|atgm crew|stinger crew/.test(t))
    return { label: 'INF', color: '#22c55e', bg: '#22c55e18', icon: Infantry }
  // Logistics / vehicles
  if (/truck|supply|ural|kamaz|zil|hemtt|fmtv|mhz|logistics|cargo|transport|car|suv|jeep|humvee|hmmwv|land rover/.test(t))
    return { label: 'VEHICLE', color: '#fbbf24', bg: '#fbbf2418', icon: Logistics }
  // Radar / EW
  if (/radar|ew |elint|jammer/.test(t))
    return { label: 'RADAR', color: '#e879f9', bg: '#e879f918', icon: Radar }
  // Structure
  if (/building|structure|depot|hangar|bunker|farp|warehouse|fuel|ammo|command|cp |hq /.test(t))
    return { label: 'STRUCT', color: '#94a3b8', bg: '#94a3b818', icon: Structure }
  return { label: 'GND', color: '#64748b', bg: '#64748b18' }
}

// ── Filters ───────────────────────────────────────────────────────────────────

type SideF = 'All' | 'Blue' | 'Red'
type WhoF = 'All' | 'Players' | 'AI'

const BLUE = 'var(--blue)'
const RED = 'var(--red)'

const sideColor = (s: string | null | undefined) => (s === 'Blue' ? BLUE : s === 'Red' ? RED : 'var(--text-dim)')

/** "14:03:22Z", with the date in front when it isn't today. */
function fmtZ(iso: string, today: string): string {
  const s = new Date(iso).toISOString()
  const day = s.slice(0, 10)
  return `${day === today ? '' : `${day.slice(5)} `}${s.slice(11, 19)}Z`
}

/** A bucket size that gives the activity chart roughly 24-48 bars. */
function bucketFor(spanMs: number): number {
  const steps = [5, 10, 15, 30, 60, 120, 180, 360, 720, 1440].map(m => m * 60_000)
  return steps.find(s => spanMs / s <= 48) ?? steps[steps.length - 1]
}

const label: React.CSSProperties = { fontSize: '0.58rem', color: 'var(--text-dim)', textTransform: 'uppercase', letterSpacing: '0.14em' }

function Card({ title, icon: Icon, right, children }: { title: string; icon: IconComponent; right?: React.ReactNode; children: React.ReactNode }) {
  return (
    <div className="vs-card" style={{ minWidth: 0 }}>
      <div className="flex items-center gap-2 px-4 pt-3.5 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Icon size={12} style={{ color: 'var(--accent)' }} />
        <span style={{ ...label, fontSize: '0.62rem' }}>{title}</span>
        {right && <span className="ml-auto" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>{right}</span>}
      </div>
      {children}
    </div>
  )
}

function Seg<T extends string>({ value, options, onChange, name }: { value: T; options: readonly T[]; onChange: (v: T) => void; name: string }) {
  return (
    <div role="group" aria-label={name} className="flex overflow-hidden" style={{ border: '1px solid var(--border)', borderRadius: 2 }}>
      {options.map(o => (
        <button key={o} type="button" aria-pressed={value === o} onClick={() => onChange(o)}
          style={{
            fontFamily: 'var(--font-display)', fontSize: '0.75rem', letterSpacing: '0.1em', padding: '0.25rem 0.7rem',
            background: value === o ? 'var(--accent-glow)' : 'transparent', color: value === o ? 'var(--accent-bright)' : 'var(--text-muted)',
            border: 'none', cursor: 'pointer',
          }}>
          {o.toUpperCase()}
        </button>
      ))}
    </div>
  )
}

interface ActRow { t: number; label: string; blue: number; red: number }

function ActivityTip({ active, payload }: { active?: boolean; payload?: ReadonlyArray<{ payload?: ActRow }> }) {
  const p = payload?.[0]?.payload
  if (!active || !p) return null
  return (
    <div style={{ background: 'var(--bg-elevated-solid)', border: '1px solid var(--border-light)', borderRadius: 3, padding: '6px 8px', fontSize: 11, color: 'var(--text-muted)' }}>
      <div style={{ marginBottom: 4 }}>{p.label}</div>
      <div className="flex items-center gap-2"><span style={{ width: 10, height: 2, background: BLUE, display: 'inline-block' }} /><b style={{ color: 'var(--text)' }}>{p.blue}</b> by Blue</div>
      <div className="flex items-center gap-2"><span style={{ width: 10, height: 2, background: RED, display: 'inline-block' }} /><b style={{ color: 'var(--text)' }}>{-p.red}</b> by Red</div>
    </div>
  )
}

/** Horizontal bars as HTML, so every label shows (recharts drops alternate
 *  category ticks when they crowd). */
function BarList({ rows, empty }: { rows: { key: string; label: React.ReactNode; value: number; title?: string }[]; empty: string }) {
  if (rows.length === 0) return <div style={{ padding: 16, fontSize: '0.7rem', color: 'var(--text-dim)' }}>{empty}</div>
  const max = Math.max(1, ...rows.map(r => r.value))
  return (
    <div style={{ padding: '12px 16px', display: 'grid', gridTemplateColumns: 'minmax(80px, 42%) 1fr auto', gap: '6px 10px', alignItems: 'center' }}>
      {rows.map(r => (
        <React.Fragment key={r.key}>
          <span style={{ fontSize: '0.68rem', color: 'var(--text)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }} title={r.title}>{r.label}</span>
          <div style={{ height: 8, background: 'rgba(148,163,184,0.1)', borderRadius: 2 }}>
            <div style={{ width: `${(r.value / max) * 100}%`, height: '100%', background: 'var(--accent)', borderRadius: '0 4px 4px 0' }} />
          </div>
          <span className="font-mono" style={{ fontSize: '0.66rem', color: 'var(--text-muted)' }}>{r.value}</span>
        </React.Fragment>
      ))}
    </div>
  )
}

// ── Page ──────────────────────────────────────────────────────────────────────

const realUcid = (u: string | null): string | null => (u && !/^0+$/.test(u) ? u : null)

/** The server returns at most this many kills per request. */
const FEED_LIMIT = 500

export default function KillFeed() {
  const { selectedRound } = useRound()
  const { data: raw, isLoading, isError, error, refetch, dataUpdatedAt } = useQuery({
    queryKey: ['kills-feed', selectedRound],
    queryFn: () => api.kills(selectedRound, FEED_LIMIT),
    refetchInterval: 15_000,
  })
  // bfdb writes an all-zero UCID where no pilot gets the credit; that is
  // "not a player", not a pilot called 00000000.
  const kills = useMemo(() => (raw ?? []).map(k => ({
    ...k,
    victim: { ...k.victim, ucid: realUcid(k.victim.ucid) },
    killer: k.killer && { ...k.killer, ucid: realUcid(k.killer.ucid) },
  })), [raw])
  const { data: allPilots = [] } = useQuery({
    queryKey: ['all-pilots'],
    queryFn: api.allPilots,
    refetchInterval: 120_000,
  })

  const [side, setSide] = useState<SideF>('All')
  const [who, setWho] = useState<WhoF>('All')
  const [cat, setCat] = useState<string | null>(null)
  const [q, setQ] = useState('')

  const nameMap = useMemo(() => {
    const m = new Map<string, string>()
    allPilots.forEach(p => m.set(p.ucid, p.name))
    return m
  }, [allPilots])
  const nameOf = useCallback((ucid: string | null | undefined) => (ucid ? nameMap.get(ucid) ?? null : null), [nameMap])

  // Every chart and the table read the same filtered slice.
  const shown = useMemo(() => {
    const needle = q.trim().toLowerCase()
    return kills.filter(k => {
      if (side !== 'All' && k.killer?.side !== side) return false
      if (who === 'Players' && !k.killer?.ucid) return false
      if (who === 'AI' && (!k.killer || k.killer.ucid)) return false
      if (cat && classifyTarget(k.target_type).label !== cat) return false
      if (needle) {
        const hay = [nameOf(k.killer?.ucid), nameOf(k.victim.ucid), k.killer?.airframe, k.killer?.weapon, k.target_type]
          .filter(Boolean).join(' ').toLowerCase()
        if (!hay.includes(needle)) return false
      }
      return true
    })
  }, [kills, side, who, cat, q, nameOf])

  const stats = useMemo(() => {
    const blue = shown.filter(k => k.killer?.side === 'Blue').length
    const red = shown.filter(k => k.killer?.side === 'Red').length
    const players = shown.filter(k => k.killer?.ucid).length
    const pvp = shown.filter(k => k.killer?.ucid && k.victim.ucid).length
    return { blue, red, players, pvp }
  }, [shown])

  // Activity over the actual time span (not folded onto hour-of-day): Blue's
  // kills above the line, Red's below.
  const activity = useMemo<ActRow[]>(() => {
    if (shown.length === 0) return []
    const ts = shown.map(k => Date.parse(k.time))
    const lo = Math.min(...ts), hi = Math.max(...ts)
    const step = bucketFor(Math.max(hi - lo, 60_000))
    const start = Math.floor(lo / step) * step
    const n = Math.floor((hi - start) / step) + 1
    const rows: ActRow[] = Array.from({ length: n }, (_, i) => {
      const t = start + i * step
      const iso = new Date(t).toISOString()
      return { t, label: `${iso.slice(5, 10)} ${iso.slice(11, 16)}Z`, blue: 0, red: 0 }
    })
    shown.forEach((k, i) => {
      const r = rows[Math.floor((ts[i] - start) / step)]
      if (k.killer?.side === 'Blue') r.blue++
      else if (k.killer?.side === 'Red') r.red--
    })
    return rows
  }, [shown])

  const topWeapons = useMemo(() => {
    const m = new Map<string, number>()
    shown.forEach(k => { const w = k.killer?.weapon ?? 'Gun / unknown'; m.set(w, (m.get(w) ?? 0) + 1) })
    return [...m.entries()].sort((a, b) => b[1] - a[1]).slice(0, 10)
      .map(([w, n]) => ({ key: w, label: w, title: w, value: n }))
  }, [shown])

  const topKillers = useMemo(() => {
    const m = new Map<string, { n: number; side: string }>()
    shown.forEach(k => {
      if (!k.killer?.ucid) return
      const e = m.get(k.killer.ucid) ?? { n: 0, side: k.killer.side }
      e.n++
      m.set(k.killer.ucid, e)
    })
    return [...m.entries()].sort((a, b) => b[1].n - a[1].n).slice(0, 10).map(([ucid, e]) => ({
      key: ucid, value: e.n, title: nameOf(ucid) ?? ucid,
      label: (
        <RouterLink to={`/pilots?ucid=${ucid}`} style={{ color: 'var(--text)', textDecoration: 'none' }}>
          <span style={{ color: sideColor(e.side), fontSize: '0.6em', marginRight: 6 }}>●</span>{nameOf(ucid) ?? ucid.slice(0, 8)}
        </RouterLink>
      ),
    }))
  }, [shown, nameOf])

  // What each side destroyed, by category.
  const exchange = useMemo(() => {
    const m = new Map<string, { blue: number; red: number; color: string; icon?: IconComponent }>()
    shown.forEach(k => {
      const c = classifyTarget(k.target_type)
      const e = m.get(c.label) ?? { blue: 0, red: 0, color: c.color, icon: c.icon }
      if (k.killer?.side === 'Blue') e.blue++
      else if (k.killer?.side === 'Red') e.red++
      m.set(c.label, e)
    })
    return [...m.entries()].map(([name, e]) => ({ name, ...e })).sort((a, b) => (b.blue + b.red) - (a.blue + a.red))
  }, [shown])
  const exMax = Math.max(1, ...exchange.map(e => Math.max(e.blue, e.red)))
  const cats = useMemo(() => [...new Set(kills.map(k => classifyTarget(k.target_type).label))].sort(), [kills])

  const killCols = useMemo(() => ({
    time:     (k: Kill) => k.time,
    category: (k: Kill) => classifyTarget(k.target_type).label,
    killer:   (k: Kill) => nameOf(k.killer?.ucid) ?? k.killer?.airframe ?? (k.killer ? 'AI' : 'Environment'),
    airframe: (k: Kill) => k.killer?.airframe ?? null,
    weapon:   (k: Kill) => k.killer?.weapon ?? null,
    target:   (k: Kill) => k.target_type ?? null,
    victim:   (k: Kill) => nameOf(k.victim.ucid),
  }), [nameOf])
  const { sorted: sortedKills, sortKey, sortDir, onSort } =
    useTableSort(shown, killCols, { key: 'time', dir: 'desc' })

  const today = new Date().toISOString().slice(0, 10)
  const filtered = shown.length !== kills.length
  const ratio = stats.red > 0 ? (stats.blue / stats.red).toFixed(2) : stats.blue > 0 ? '∞' : '—'

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader art="su27-rear"
        icon={KillFeedIcon}
        title="KILL FEED"
        sub={`${kills.length >= FEED_LIMIT ? `latest ${FEED_LIMIT}` : kills.length} events · refreshes every 15s`}
        right={
          <span className="font-mono" style={{ fontSize: '0.62rem', color: 'var(--text-dim)' }}>
            {dataUpdatedAt ? `UPD ${new Date(dataUpdatedAt).toISOString().slice(11, 19)}Z` : ''}
          </span>
        }
      />

      <div className="flex-1 overflow-auto p-4 space-y-4">
        {/* ── One filter row; it scopes everything below ── */}
        <div className="flex flex-wrap items-center gap-2">
          <Seg name="Killer side" value={side} options={['All', 'Blue', 'Red'] as const} onChange={setSide} />
          <Seg name="Killer" value={who} options={['All', 'Players', 'AI'] as const} onChange={setWho} />
          <select value={cat ?? ''} onChange={e => setCat(e.target.value || null)} className="vs-input" aria-label="Target category"
            style={{ fontSize: '0.7rem', padding: '0.3rem 0.5rem' }}>
            <option value="">All targets</option>
            {cats.map(c => <option key={c} value={c}>{c}</option>)}
          </select>
          <input value={q} onChange={e => setQ(e.target.value)} placeholder="Pilot, weapon, unit…" className="vs-input" aria-label="Search kills"
            style={{ fontSize: '0.7rem', width: 200 }} />
          {filtered && (
            <button type="button" onClick={() => { setSide('All'); setWho('All'); setCat(null); setQ('') }}
              style={{ background: 'none', border: 'none', color: 'var(--accent-bright)', fontSize: '0.62rem', letterSpacing: '0.08em', cursor: 'pointer' }}>
              CLEAR
            </button>
          )}
          <span className="ml-auto font-mono" style={{ fontSize: '0.62rem', color: 'var(--text-dim)' }}>
            {filtered ? `${shown.length} of ${kills.length}` : `${kills.length}`} kills
          </span>
        </div>

        {/* ── Stat row ── */}
        <div className="grid grid-cols-2 sm:grid-cols-4 gap-3">
          {([
            { name: 'Kills by Blue', value: stats.blue, color: BLUE },
            { name: 'Kills by Red', value: stats.red, color: RED },
            { name: 'Exchange (Blue:Red)', value: ratio, color: 'var(--accent)' },
            { name: 'By players', value: `${shown.length ? Math.round((stats.players / shown.length) * 100) : 0}%`, color: 'var(--accent)', note: `${stats.pvp} player-vs-player` },
          ] as const).map(s => (
            <div key={s.name} className="vs-card p-4" style={{ borderTop: `2px solid ${s.color}` }}>
              <div style={label}>{s.name}</div>
              <div style={{ fontSize: '1.6rem', fontWeight: 700, color: 'var(--text)', lineHeight: 1.1, marginTop: 4 }}>{s.value}</div>
              {'note' in s && <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', marginTop: 2 }}>{s.note}</div>}
            </div>
          ))}
        </div>

        {/* ── Activity + exchange ── */}
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(340px, 1fr))', gap: 12 }}>
          <Card title="Activity" icon={Activity} right="Blue up · Red down · UTC">
            <div className="p-4">
              {activity.length === 0 ? (
                <div style={{ height: 160, display: 'flex', alignItems: 'center', justifyContent: 'center', color: 'var(--text-dim)', fontSize: '0.75rem' }}>No kills</div>
              ) : (
                <ResponsiveContainer width="100%" height={160}>
                  <BarChart data={activity} stackOffset="sign" margin={{ left: -16, right: 4, top: 4, bottom: 0 }} barCategoryGap={2}>
                    <CartesianGrid vertical={false} stroke="var(--border)" />
                    <XAxis dataKey="label" tick={{ fill: 'var(--text-dim)', fontSize: 9 }} axisLine={false} tickLine={false} minTickGap={24} />
                    <YAxis tick={{ fill: 'var(--text-dim)', fontSize: 10 }} axisLine={false} tickLine={false} allowDecimals={false} width={36} tickFormatter={(v: number) => String(Math.abs(v))} />
                    <ReferenceLine y={0} stroke="var(--border-light)" />
                    <Tooltip cursor={{ fill: 'rgba(148,163,184,0.08)' }} content={<ActivityTip />} />
                    <Bar dataKey="blue" stackId="k" fill={BLUE} maxBarSize={24} radius={[4, 4, 0, 0]} isAnimationActive={false} />
                    <Bar dataKey="red" stackId="k" fill={RED} maxBarSize={24} radius={[0, 0, 4, 4]} isAnimationActive={false} />
                  </BarChart>
                </ResponsiveContainer>
              )}
            </div>
          </Card>

          <Card title="What each side destroyed" icon={Strike} right="Blue · Red">
            <div style={{ padding: '12px 16px', display: 'grid', gridTemplateColumns: '1fr auto minmax(70px, 90px) 1fr', gap: '6px 8px', alignItems: 'center' }}>
              {exchange.length === 0 && <span style={{ gridColumn: '1 / -1', fontSize: '0.7rem', color: 'var(--text-dim)' }}>No kills</span>}
              {exchange.map(e => (
                <React.Fragment key={e.name}>
                  <div style={{ display: 'flex', justifyContent: 'flex-end' }} title={`${e.blue} ${e.name} by Blue`}>
                    <div style={{ width: `${(e.blue / exMax) * 100}%`, height: 8, background: BLUE, borderRadius: '4px 0 0 4px', minWidth: e.blue ? 2 : 0 }} />
                  </div>
                  <span className="font-mono" style={{ fontSize: '0.64rem', color: 'var(--text-muted)', textAlign: 'right', minWidth: 22 }}>{e.blue}</span>
                  <button type="button" onClick={() => setCat(cat === e.name ? null : e.name)} title={cat === e.name ? 'Show every category' : `Only ${e.name}`}
                    style={{ all: 'unset', cursor: 'pointer', display: 'inline-flex', alignItems: 'center', justifyContent: 'center', gap: 4, fontSize: '0.58rem', fontWeight: 700, letterSpacing: '0.06em', color: cat === e.name ? 'var(--accent-bright)' : 'var(--text-muted)' }}>
                    {e.icon && <e.icon size={10} strokeWidth={1.75} />}{e.name}
                  </button>
                  <div style={{ display: 'flex', alignItems: 'center', gap: 8 }} title={`${e.red} ${e.name} by Red`}>
                    <div style={{ width: `${(e.red / exMax) * 100}%`, height: 8, background: RED, borderRadius: '0 4px 4px 0', minWidth: e.red ? 2 : 0 }} />
                    <span className="font-mono" style={{ fontSize: '0.64rem', color: 'var(--text-muted)' }}>{e.red}</span>
                  </div>
                </React.Fragment>
              ))}
            </div>
          </Card>
        </div>

        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: 12 }}>
          <Card title="Top killers" icon={Award} right="players">
            <BarList rows={topKillers} empty="No player kills in this view." />
          </Card>
          <Card title="Top weapons" icon={Strike}>
            <BarList rows={topWeapons} empty="No kills in this view." />
          </Card>
        </div>

        {/* ── Kill event table ── */}
        <div className="vs-card overflow-hidden">
          <div className="flex items-center gap-2 px-4 pt-3.5 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
            <KillFeedIcon size={12} style={{ color: 'var(--accent)' }} />
            <span style={{ ...label, fontSize: '0.62rem' }}>Event Log</span>
            <span className="ml-auto font-mono" style={{ fontSize: '0.58rem', color: 'var(--text-dim)' }}>{shown.length} events</span>
          </div>
          <div style={{ maxHeight: 560, overflow: 'auto' }}>
            <table style={{ width: '100%', minWidth: 760, borderCollapse: 'collapse' }}>
              <thead>
                <tr style={{ background: 'var(--bg-card-solid)', borderBottom: '1px solid var(--border)', position: 'sticky', top: 0, zIndex: 1 }}>
                  {KILL_COLS.map(c => (
                    <SortTh
                      key={c.key}
                      label={c.label}
                      colKey={c.key}
                      sortKey={sortKey}
                      sortDir={sortDir}
                      onSort={onSort}
                      style={{ padding: '7px 12px', textAlign: 'left', fontSize: '0.58rem', textTransform: 'uppercase', letterSpacing: '0.14em', color: 'var(--text-dim)', fontWeight: 600 }}
                    />
                  ))}
                </tr>
              </thead>
              <tbody>
                <QueryState
                  colSpan={7} what="kills" isLoading={isLoading}
                  isError={isError} error={error} onRetry={refetch}
                  isEmpty={shown.length === 0}
                  emptyText={kills.length === 0 ? 'No kills recorded yet' : 'No kills match the filters'}
                />
                {sortedKills.map((k, i) => {
                  const killerName = nameOf(k.killer?.ucid)
                  const victimName = nameOf(k.victim.ucid)
                  const c = classifyTarget(k.target_type)
                  const cell: React.CSSProperties = { padding: '6px 12px', fontSize: '0.68rem', borderBottom: '1px solid var(--border)', whiteSpace: 'nowrap' }
                  // No killer = environment/crash; Neutral AI = unknown coalition
                  const hasKiller = k.killer != null
                  const isEnv = !hasKiller || k.killer!.side === 'Neutral'
                  return (
                    <tr key={i} style={{ background: i % 2 === 0 ? 'transparent' : 'rgba(0,0,0,0.1)' }}>
                      <td style={{ ...cell, fontFamily: 'monospace', color: 'var(--text-dim)' }}>{fmtZ(k.time, today)}</td>
                      <td style={cell}>
                        <span style={{ display: 'inline-flex', alignItems: 'center', gap: 4, fontSize: '0.6rem', fontWeight: 700, padding: '2px 6px', borderRadius: 2, color: c.color, background: c.bg, letterSpacing: '0.06em' }}>
                          {c.icon && <c.icon size={10} strokeWidth={1.75} />}
                          {c.label}
                        </span>
                      </td>
                      <td style={cell}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                          {isEnv
                            ? <span style={{ fontSize: '0.58rem', color: 'var(--text-dim)', fontStyle: 'italic' }}>ENV</span>
                            : <SideBadge side={k.killer!.side} size="xs" />}
                          {killerName
                            ? <RouterLink to={`/pilots?ucid=${k.killer!.ucid}`} style={{ color: 'var(--text)', fontWeight: 600, textDecoration: 'none' }}>{killerName}</RouterLink>
                            : <span style={{ color: 'var(--text-dim)' }}>{hasKiller ? k.killer!.airframe ?? 'AI' : 'Environment'}</span>}
                        </div>
                      </td>
                      <td style={{ ...cell, color: 'var(--text-muted)', fontFamily: 'monospace' }}>
                        {k.killer?.airframe ?? <span style={{ color: 'var(--text-dim)' }}>—</span>}
                      </td>
                      <td style={{ ...cell, color: 'var(--text-muted)', fontFamily: 'monospace' }}>
                        {k.killer?.weapon ?? <span style={{ color: 'var(--text-dim)' }}>—</span>}
                      </td>
                      <td style={{ ...cell, color: 'var(--text-muted)' }}>{k.target_type ?? '—'}</td>
                      <td style={cell}>
                        {victimName ? (
                          <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                            <SideBadge side={k.victim.side} size="xs" />
                            <RouterLink to={`/pilots?ucid=${k.victim.ucid}`} style={{ color: 'var(--text)', fontWeight: 600, textDecoration: 'none' }}>{victimName}</RouterLink>
                          </div>
                        ) : (
                          <span style={{ color: 'var(--text-dim)' }}>AI</span>
                        )}
                      </td>
                    </tr>
                  )
                })}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
  )
}
