import React, { useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import SideBadge from '../components/SideBadge'
import PageHeader from '../components/PageHeader'
import {
  BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer,
  AreaChart, Area,
} from 'recharts'
import { Crosshair, Zap, Activity, Users, Plane } from 'lucide-react'
import { useRound } from '../context/RoundContext'

const TT = {
  contentStyle: { background: '#0f0f0f', border: '1px solid #222', borderRadius: 3, color: '#e2e8f0', fontSize: 11 },
  cursor: { fill: 'rgba(77,124,15,0.04)' },
}

// Classify from DCS unit type name (target_type field contains the DCS type string)
function classifyTarget(targetType: string | null): { label: string; color: string; bg: string } {
  const t = (targetType ?? '').toLowerCase()
  // Aircraft patterns
  if (
    /\bf-\d|\bf\/a-|\bf-16|f-18|f-15|mig-|su-\d|a-10|av-8|eurofighter|typhoon|tornado|rafale|gripen|mirage|hawk|hornet|viper|eagle|flanker|fullback|frogfoot|fencer|foxbat|foxhound|warthog|harrier|phantom|lightning|hercules|globemaster|strategic/.test(t) ||
    /aircraft|plane|jet|fighter|bomber|transport|tanker|awacs|recon/.test(t)
  ) return { label: 'AIRCRAFT', color: '#60a5fa', bg: '#60a5fa18' }
  // Helicopter patterns
  if (/mi-|uh-|ah-|ch-|ka-|sa-\d|heli|huey|apache|blackhawk|chinook|hind|hip|havoc|hokum|seahawk|lynx|gazelle/.test(t) ||
    /helicopter|helo|rotary/.test(t)
  ) return { label: 'HELO', color: '#38bdf8', bg: '#38bdf818' }
  // Naval
  if (/ship|boat|frigate|destroyer|carrier|corvette|patrol|naval|lha|cvn|ddg|ffg|slava|kuznetsov|perry|oliver|oliver hazard/.test(t))
    return { label: 'NAVAL', color: '#06b6d4', bg: '#06b6d418' }
  // Armor
  if (/t-\d{2,3}|m1|m60|leopard|challenger|abrams|merkava|k1|leclerc|tank|apc|ifv|bmp-|btr-|bradley|marder|warrior|cv90|stryker|armored/.test(t) ||
    /armor|tank/.test(t)
  ) return { label: 'ARMOR', color: '#f97316', bg: '#f9731618' }
  // Air defence
  if (/sa-\d|s-300|s-400|patriot|hawk|roland|gepard|tunguska|shilka|flak|aaa|vulcan|linebacker|tor|buk|kub|sa-6|sa-8|sa-10|sa-11|sa-13|sa-15|sa-19|radar|tre|str|fan song|search|track|acquisition|crow bar|straight flush|flap lid|clam shell|dog ear|fire dome|low blow/.test(t) ||
    /sam |manpad|stinger|igla|strela|air.def/.test(t)
  ) return { label: 'AIR DEF', color: '#a78bfa', bg: '#a78bfa18' }
  // Artillery / MLRS
  if (/2s\d|m109|paladin|pzh|as-90|caesar|howitzer|mlrs|bm-21|bm-30|smerch|uragan|grad|artillery|mortar/.test(t))
    return { label: 'ARTY', color: '#fb923c', bg: '#fb923c18' }
  // Infantry
  if (/infantry|soldier|troop|manpad|squad|sniper|atgm crew|stinger crew/.test(t))
    return { label: 'INF', color: '#22c55e', bg: '#22c55e18' }
  // Logistics / vehicles
  if (/truck|supply|ural|kamaz|zil|hemtt|fmtv|mhz|logistics|cargo|transport|car|suv|jeep|humvee|hmmwv|land rover/.test(t))
    return { label: 'VEHICLE', color: '#fbbf24', bg: '#fbbf2418' }
  // Radar / EW
  if (/radar|ew |elint|jammer/.test(t))
    return { label: 'RADAR', color: '#e879f9', bg: '#e879f918' }
  // Structure
  if (/building|structure|depot|hangar|bunker|farp|warehouse|fuel|ammo|command|cp |hq /.test(t))
    return { label: 'STRUCT', color: '#94a3b8', bg: '#94a3b818' }
  return { label: 'GND', color: '#64748b', bg: '#64748b18' }
}

export default function KillFeed() {
  const { selectedRound } = useRound()
  const { data: kills = [], isLoading, dataUpdatedAt } = useQuery({
    queryKey: ['kills-feed', selectedRound],
    queryFn: () => api.kills(selectedRound, 200),
    refetchInterval: 15_000,
  })
  const { data: allPilots = [] } = useQuery({
    queryKey: ['all-pilots'],
    queryFn: api.allPilots,
    refetchInterval: 120_000,
  })

  const nameMap = useMemo(() => {
    const m = new Map<string, string>()
    allPilots.forEach(p => m.set(p.ucid, p.name))
    return m
  }, [allPilots])

  // Weapon kill counts
  const weaponCounts: Record<string, number> = {}
  kills.forEach(k => {
    const w = k.killer?.weapon ?? 'Gun / Cannon'
    weaponCounts[w] = (weaponCounts[w] ?? 0) + 1
  })
  const topWeapons = Object.entries(weaponCounts)
    .sort((a, b) => b[1] - a[1]).slice(0, 10)
    .map(([name, count]) => ({ name: name.length > 24 ? name.slice(0, 23) + '…' : name, count }))

  // Kill activity by hour
  const hourBuckets: Record<number, { blue: number; red: number }> = {}
  kills.forEach(k => {
    const h = new Date(k.time).getHours()
    if (!hourBuckets[h]) hourBuckets[h] = { blue: 0, red: 0 }
    if (k.killer?.side === 'Blue') hourBuckets[h].blue++
    else if (k.killer?.side === 'Red') hourBuckets[h].red++
  })
  const activityData = Array.from({ length: 24 }, (_, i) => ({
    hour: `${i}h`,
    blue: hourBuckets[i]?.blue ?? 0,
    red:  hourBuckets[i]?.red  ?? 0,
  }))

  const blueKills   = kills.filter(k => k.killer?.side === 'Blue').length
  const redKills    = kills.filter(k => k.killer?.side === 'Red').length
  const playerKills = kills.filter(k => k.killer?.ucid).length
  const aiKills     = kills.filter(k => k.killer && !k.killer.ucid).length

  const fmtTime = (iso: string) =>
    new Date(iso).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false })

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader
        title="KILL FEED"
        sub={`${kills.length} events · auto-refreshes every 15s`}
        right={
          <span className="text-[11px] text-slate-600 font-mono">
            {dataUpdatedAt ? `UPD ${new Date(dataUpdatedAt).toLocaleTimeString()}` : ''}
          </span>
        }
      />

      <div className="flex-1 overflow-auto p-4 space-y-4" style={{ background: 'var(--bg)' }}>
        {/* ── Stat row ── */}
        <div className="grid grid-cols-2 sm:grid-cols-4 gap-3">
          {([
            { label: 'Blue Kills',    value: blueKills,   color: '#3b82f6', icon: Crosshair },
            { label: 'Red Kills',     value: redKills,    color: '#ef4444', icon: Crosshair },
            { label: 'Player Kills',  value: playerKills, color: '#22c55e', icon: Users },
            { label: 'AI Kills',      value: aiKills,     color: '#a78bfa', icon: Zap },
          ] as const).map(s => (
            <div key={s.label} className="vs-card p-4" style={{ borderTop: `2px solid ${s.color}` }}>
              <div className="flex items-center justify-between mb-1.5">
                <span style={{ fontSize: '0.58rem', color: 'var(--text-dim)', textTransform: 'uppercase', letterSpacing: '0.14em' }}>{s.label}</span>
                <s.icon size={11} style={{ color: s.color }} />
              </div>
              <div style={{ fontSize: '1.6rem', fontWeight: 800, color: s.color, fontFamily: 'monospace', lineHeight: 1 }}>{s.value}</div>
            </div>
          ))}
        </div>

        {/* ── Charts ── */}
        <div className="grid grid-cols-1 lg:grid-cols-5 gap-3">
          <div className="vs-card lg:col-span-3">
            <div className="flex items-center gap-2 px-4 pt-3.5 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
              <Activity size={12} style={{ color: '#22d3ee' }} />
              <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>Kill Activity (UTC hour)</span>
            </div>
            <div className="p-4">
              <ResponsiveContainer width="100%" height={120}>
                <AreaChart data={activityData} margin={{ left: -10, right: 4, top: 4, bottom: 0 }}>
                  <defs>
                    <linearGradient id="kbg" x1="0" y1="0" x2="0" y2="1">
                      <stop offset="5%"  stopColor="#3b82f6" stopOpacity={0.35} />
                      <stop offset="95%" stopColor="#3b82f6" stopOpacity={0} />
                    </linearGradient>
                    <linearGradient id="krg" x1="0" y1="0" x2="0" y2="1">
                      <stop offset="5%"  stopColor="#ef4444" stopOpacity={0.35} />
                      <stop offset="95%" stopColor="#ef4444" stopOpacity={0} />
                    </linearGradient>
                  </defs>
                  <XAxis dataKey="hour" tick={{ fill: '#374151', fontSize: 9 }} axisLine={false} tickLine={false} interval={3} />
                  <YAxis tick={{ fill: '#374151', fontSize: 10 }} axisLine={false} tickLine={false} />
                  <Tooltip {...TT} />
                  <Area type="monotone" dataKey="blue" name="Blue" stroke="#3b82f6" strokeWidth={1.5} fill="url(#kbg)" dot={false} />
                  <Area type="monotone" dataKey="red"  name="Red"  stroke="#ef4444" strokeWidth={1.5} fill="url(#krg)"  dot={false} />
                </AreaChart>
              </ResponsiveContainer>
            </div>
          </div>

          <div className="vs-card lg:col-span-2">
            <div className="flex items-center gap-2 px-4 pt-3.5 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
              <Zap size={12} style={{ color: '#fb923c' }} />
              <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>Top Weapons</span>
            </div>
            <div className="p-4">
              {topWeapons.length > 0 ? (
                <ResponsiveContainer width="100%" height={120}>
                  <BarChart data={topWeapons} layout="vertical" margin={{ left: 0, right: 8 }}>
                    <XAxis type="number" tick={{ fill: '#374151', fontSize: 10 }} axisLine={false} tickLine={false} />
                    <YAxis type="category" dataKey="name" tick={{ fill: '#94a3b8', fontSize: 9 }} width={150} axisLine={false} tickLine={false} />
                    <Tooltip {...TT} />
                    <Bar dataKey="count" fill="#f97316" radius={[0, 3, 3, 0]} />
                  </BarChart>
                </ResponsiveContainer>
              ) : (
                <div className="h-[120px] flex items-center justify-center" style={{ color: '#374151', fontSize: '0.75rem' }}>No data</div>
              )}
            </div>
          </div>
        </div>

        {/* ── Kill event table ── */}
        <div className="vs-card overflow-hidden">
          <div className="flex items-center gap-2 px-4 pt-3.5 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
            <Crosshair size={12} style={{ color: '#f87171' }} />
            <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>Event Log</span>
            <span className="ml-auto font-mono" style={{ fontSize: '0.58rem', color: '#374151' }}>{kills.length} events</span>
          </div>
          <div style={{ maxHeight: 480, overflow: 'auto' }}>
            <table style={{ width: '100%', borderCollapse: 'collapse' }}>
              <thead>
                <tr style={{ background: 'var(--bg-card)', borderBottom: '1px solid var(--border)', position: 'sticky', top: 0, zIndex: 1 }}>
                  {['Time (Z)', 'Category', 'Killer', 'Airframe', 'Weapon', 'Target Unit', 'Victim'].map(h => (
                    <th key={h} style={{ padding: '7px 12px', textAlign: 'left', fontSize: '0.58rem', textTransform: 'uppercase', letterSpacing: '0.14em', color: '#374151', fontWeight: 600, whiteSpace: 'nowrap' }}>{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {isLoading && (
                  <tr><td colSpan={7} style={{ textAlign: 'center', padding: '2.5rem', color: '#374151', fontSize: '0.75rem' }}>Loading…</td></tr>
                )}
                {kills.length === 0 && !isLoading && (
                  <tr><td colSpan={7} style={{ textAlign: 'center', padding: '2.5rem', color: '#374151', fontSize: '0.75rem' }}>No kills recorded yet</td></tr>
                )}
                {kills.map((k, i) => {
                  const killerName = k.killer?.ucid ? (nameMap.get(k.killer.ucid) ?? null) : null
                  const victimName = k.victim.ucid ? (nameMap.get(k.victim.ucid) ?? null) : null
                  const cat = classifyTarget(k.target_type)
                  const rowBg = i % 2 === 0 ? 'transparent' : 'rgba(0,0,0,0.1)'
                  const cell: React.CSSProperties = { padding: '6px 12px', fontSize: '0.68rem', borderBottom: '1px solid rgba(34,34,34,0.5)' }
                  // No killer = environment/crash; Neutral AI = unknown coalition
                  const hasKiller = k.killer !== null && k.killer !== undefined
                  const killerSide = hasKiller ? k.killer!.side : null
                  const isEnv = !hasKiller || killerSide === 'Neutral'
                  const killerDisplay = killerName ?? (hasKiller ? k.killer!.airframe ?? 'AI' : 'Environment')
                  return (
                    <tr key={i} style={{ background: rowBg }}>
                      <td style={{ ...cell, fontFamily: 'monospace', color: '#475569', whiteSpace: 'nowrap' }}>
                        {fmtTime(k.time)}
                      </td>
                      <td style={cell}>
                        <span style={{ fontSize: '0.6rem', fontWeight: 700, padding: '2px 6px', borderRadius: 2, color: cat.color, background: cat.bg, letterSpacing: '0.06em', whiteSpace: 'nowrap' }}>
                          {cat.label}
                        </span>
                      </td>
                      <td style={{ ...cell, whiteSpace: 'nowrap' }}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                          {isEnv
                            ? <span style={{ fontSize: '0.58rem', color: '#475569', fontStyle: 'italic' }}>ENV</span>
                            : <SideBadge side={killerSide!} size="xs" />
                          }
                          <span style={{ color: killerName ? '#e2e8f0' : '#475569', fontWeight: killerName ? 600 : 400 }}>
                            {killerDisplay}
                          </span>
                        </div>
                      </td>
                      <td style={{ ...cell, color: '#60a5fa', fontFamily: 'monospace', whiteSpace: 'nowrap' }}>
                        {k.killer?.airframe ? (
                          <span style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
                            <Plane size={10} style={{ opacity: 0.6, flexShrink: 0 }} />
                            {k.killer.airframe}
                          </span>
                        ) : <span style={{ color: '#374151' }}>—</span>}
                      </td>
                      <td style={{ ...cell, color: '#fbbf24', fontFamily: 'monospace', whiteSpace: 'nowrap' }}>
                        {k.killer?.weapon ?? <span style={{ color: '#374151' }}>—</span>}
                      </td>
                      <td style={{ ...cell, color: '#94a3b8', whiteSpace: 'nowrap' }}>
                        {k.target_type ?? '—'}
                      </td>
                      <td style={{ ...cell, whiteSpace: 'nowrap' }}>
                        {victimName ? (
                          <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                            <SideBadge side={k.victim.side} size="xs" />
                            <span style={{ color: '#f59e0b', fontWeight: 600 }}>{victimName}</span>
                          </div>
                        ) : (
                          <span style={{ color: '#374151' }}>AI / env</span>
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
