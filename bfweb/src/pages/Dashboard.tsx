import { useEffect, useMemo, useRef, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { MapContainer, TileLayer, CircleMarker, useMap } from 'react-leaflet'
import type { LatLngBoundsExpression } from 'leaflet'
import 'leaflet/dist/leaflet.css'
import { useNavigate } from 'react-router-dom'
import { ExternalLink, Crosshair, ChevronDown, ChevronUp } from 'lucide-react'
import { api, type OnlinePilot, type Objective, type Pilot, type Kill, type PilotName } from '../api'
import { campaign } from '../config/campaign'
import { useRound } from '../context/RoundContext'

// ── Kill classifier ───────────────────────────────────────────────────────────

function classifyKill(targetType: string | null): { label: string; color: string } {
  const t = (targetType ?? '').toLowerCase()
  if (t.includes('air') || t.includes('plane') || t.includes('heli') || t.includes('fighter'))
    return { label: 'AIR',   color: '#3b82f6' }
  if (t.includes('ship') || t.includes('naval') || t.includes('carrier'))
    return { label: 'NAVAL', color: '#06b6d4' }
  if (t.includes('armor') || t.includes('tank') || t.includes('apc') || t.includes('ifv'))
    return { label: 'ARMOR', color: '#f97316' }
  if (t.includes('sam') || t.includes('radar') || t.includes('aaa') || t.includes('air defence'))
    return { label: 'AD',    color: '#a78bfa' }
  if (t.includes('truck') || t.includes('supply') || t.includes('vehicle') || t.includes('car'))
    return { label: 'VEH',   color: '#fbbf24' }
  if (t.includes('infantry') || t.includes('troop') || t.includes('soldier'))
    return { label: 'INF',   color: '#22c55e' }
  return { label: 'GND', color: '#94a3b8' }
}

function fmtTime(iso: string) {
  return new Date(iso).toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit', hour12: false })
}

// ── Territory delta hook ──────────────────────────────────────────────────────

function useDelta(value: number): number | null {
  const prev = useRef<number | null>(null)
  const delta = useRef<number | null>(null)
  useEffect(() => {
    if (prev.current !== null && prev.current !== value) {
      delta.current = value - prev.current
    }
    prev.current = value
  }, [value])
  return delta.current
}

// ── KPI strip ─────────────────────────────────────────────────────────────────

function KpiCell({
  label, value, sub, color = 'var(--text)', delta,
}: {
  label: string; value: React.ReactNode; sub?: string; color?: string; delta?: number | null
}) {
  return (
    <div className="kpi-cell">
      <div className="kpi-label">{label}</div>
      <div className="kpi-value" style={{ color, display: 'flex', alignItems: 'baseline', gap: 5 }}>
        {value}
        {delta != null && delta !== 0 && (
          <span style={{
            fontSize: '0.7rem',
            fontFamily: "'Inter', sans-serif",
            fontWeight: 700,
            color: delta > 0 ? '#4ade80' : '#f87171',
            letterSpacing: 0,
          }}>
            {delta > 0 ? `+${delta}` : delta}
          </span>
        )}
      </div>
      {sub && <div className="kpi-sub">{sub}</div>}
    </div>
  )
}

// ── Kill sparkline ────────────────────────────────────────────────────────────

function KillSparkline({ kills }: { kills: Kill[] }) {
  const BUCKETS = 8
  const WINDOW_MS = 2 * 60 * 60 * 1000  // 2 hours
  const now = Date.now()

  const counts = useMemo(() => {
    const buckets = Array(BUCKETS).fill(0) as number[]
    const bucketMs = WINDOW_MS / BUCKETS
    for (const k of kills) {
      const age = now - new Date(k.time).getTime()
      if (age > WINDOW_MS) continue
      const idx = Math.min(BUCKETS - 1, Math.floor(age / bucketMs))
      buckets[BUCKETS - 1 - idx]++
    }
    return buckets
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [kills])

  const max = Math.max(...counts, 1)
  const W = 48, H = 18
  const pts = counts.map((v, i) => {
    const x = (i / (BUCKETS - 1)) * W
    const y = H - (v / max) * H
    return `${x.toFixed(1)},${y.toFixed(1)}`
  })

  return (
    <svg width={W} height={H} style={{ display: 'block', overflow: 'visible' }}>
      <polyline
        points={pts.join(' ')}
        fill="none"
        stroke="var(--accent)"
        strokeWidth={1.5}
        strokeLinejoin="round"
        strokeLinecap="round"
        opacity={0.7}
      />
      {counts.map((v, i) => v > 0 && (
        <circle
          key={i}
          cx={(i / (BUCKETS - 1)) * W}
          cy={H - (v / max) * H}
          r={2}
          fill="var(--accent)"
          opacity={0.9}
        />
      ))}
    </svg>
  )
}

// ── Objectives strip ──────────────────────────────────────────────────────────

const OBJ_ICON: Record<string, string> = {
  Airbase: '✈', FARP: '⬡', FOB: '⬡', 'Logistics Hub': '⬡',
  'Naval Base': '⚓', 'Carrier Group': '⚓', Factory: '⚙',
}

function ObjectivesStrip({ objectives }: { objectives: Objective[] }) {
  const sorted = useMemo(() =>
    [...objectives]
      .filter(o => o.lat !== 0 || o.lon !== 0)
      .sort((a, b) => {
        const order = ['Airbase', 'Naval Base', 'Carrier Group', 'Factory', 'Logistics Hub', 'FARP', 'FOB']
        return order.indexOf(a.kind) - order.indexOf(b.kind)
      }),
    [objectives]
  )

  if (!sorted.length) return null

  return (
    <div style={{
      display: 'flex',
      overflowX: 'auto',
      gap: 6,
      padding: '7px 10px',
      borderBottom: '1px solid var(--border)',
      background: '#080808',
      flexShrink: 0,
      scrollbarWidth: 'none',
    }}>
      {sorted.map(obj => {
        const ownerColor =
          obj.owner === 'Blue' ? campaign.blueColor :
          obj.owner === 'Red'  ? campaign.redColor  : '#4b5563'
        const health = obj.health
        const healthColor = health >= 75 ? '#4ade80' : health >= 40 ? '#fbbf24' : '#f87171'

        return (
          <div key={obj.id} style={{
            flexShrink: 0,
            background: 'var(--bg-card)',
            border: `1px solid ${ownerColor}28`,
            borderTop: `2px solid ${ownerColor}`,
            borderRadius: 2,
            padding: '5px 8px',
            minWidth: 90,
            display: 'flex',
            flexDirection: 'column',
            gap: 3,
          }}>
            <div style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
              <span style={{ fontSize: '0.6rem', color: ownerColor }}>{OBJ_ICON[obj.kind] ?? '●'}</span>
              <span style={{
                fontSize: '0.55rem', fontWeight: 700, letterSpacing: '0.06em',
                color: 'var(--text-muted)', whiteSpace: 'nowrap', overflow: 'hidden',
                textOverflow: 'ellipsis', maxWidth: 70,
              }}>
                {obj.name}
              </span>
            </div>
            <div style={{ height: 3, background: 'var(--border)', borderRadius: 1, overflow: 'hidden' }}>
              <div style={{ width: `${health}%`, height: '100%', background: healthColor, transition: 'width 0.4s' }} />
            </div>
            <div style={{ display: 'flex', justifyContent: 'space-between' }}>
              <span style={{ fontSize: '0.48rem', color: ownerColor, letterSpacing: '0.1em', textTransform: 'uppercase', fontWeight: 700 }}>
                {obj.owner === 'Neutral' ? 'NEUT' : obj.owner.slice(0, 3).toUpperCase()}
              </span>
              <span style={{ fontSize: '0.48rem', color: healthColor, fontFamily: "'JetBrains Mono', monospace" }}>
                {health}%
              </span>
            </div>
          </div>
        )
      })}
    </div>
  )
}

// ── Map ───────────────────────────────────────────────────────────────────────

function FitBounds({ objectives }: { objectives: Objective[] }) {
  const map = useMap()
  useEffect(() => {
    if (!objectives.length) return
    const bounds: LatLngBoundsExpression = objectives.map(o => [o.lat, o.lon] as [number, number])
    map.fitBounds(bounds, { padding: [40, 40], maxZoom: 9, animate: false })
  }, [map, objectives])
  return null
}

function TacMap({ objectives, onOpenTacmap }: { objectives: Objective[]; onOpenTacmap: () => void }) {
  const valid = objectives.filter(o => o.lat !== 0 || o.lon !== 0)
  const dot = (owner: string) =>
    owner === 'Blue' ? { color: campaign.blueColor, fillColor: campaign.blueColor } :
    owner === 'Red'  ? { color: campaign.redColor,  fillColor: campaign.redColor  } :
                       { color: '#6b7280',           fillColor: '#4b5563'          }

  return (
    <div style={{ flex: 1, position: 'relative', overflow: 'hidden' }}>
      <MapContainer
        center={campaign.mapCenter}
        zoom={campaign.mapZoom}
        style={{ position: 'absolute', inset: 0, width: '100%', height: '100%' }}
        zoomControl={false}
        attributionControl={false}
      >
        <TileLayer
          url="https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png"
          maxZoom={19}
          opacity={0.7}
        />
        {valid.length > 0 && <FitBounds objectives={valid} />}
        {valid.map(obj => (
          <CircleMarker
            key={obj.id}
            center={[obj.lat, obj.lon]}
            radius={obj.kind === 'Airbase' ? 7 : obj.kind === 'Carrier Group' ? 6 : 5}
            pathOptions={{
              ...dot(obj.owner),
              fillOpacity: obj.health > 0 ? 0.85 : 0.25,
              weight: 1.5,
            }}
          />
        ))}
      </MapContainer>

      {/* Open TACMAP */}
      <button
        onClick={onOpenTacmap}
        style={{
          position: 'absolute', top: 10, right: 10, zIndex: 1000,
          display: 'flex', alignItems: 'center', gap: 4,
          background: 'rgba(5,5,5,0.82)',
          border: '1px solid var(--border)',
          backdropFilter: 'blur(4px)',
          color: 'var(--accent)', borderRadius: 2,
          padding: '4px 9px',
          fontSize: '0.58rem', letterSpacing: '0.12em', cursor: 'pointer',
          fontFamily: "'Bebas Neue', sans-serif",
          transition: 'background 0.15s',
        }}
        onMouseEnter={e => (e.currentTarget.style.background = 'rgba(20,20,20,0.92)')}
        onMouseLeave={e => (e.currentTarget.style.background = 'rgba(5,5,5,0.82)')}
      >
        <ExternalLink size={9} /> TACMAP
      </button>

      {/* Legend */}
      <div style={{
        position: 'absolute', bottom: 10, left: 10, zIndex: 1000,
        display: 'flex', gap: 8,
        background: 'rgba(5,5,5,0.78)',
        backdropFilter: 'blur(4px)',
        padding: '4px 9px', borderRadius: 2,
        border: '1px solid rgba(30,30,30,0.8)',
        fontSize: '0.56rem', color: '#64748b',
      }}>
        {[
          { label: campaign.blueLabel, color: campaign.blueColor },
          { label: campaign.redLabel,  color: campaign.redColor  },
          { label: 'Neutral',          color: '#4b5563'          },
        ].map(({ label, color }) => (
          <span key={label} style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
            <span style={{ width: 6, height: 6, borderRadius: '50%', background: color, display: 'inline-block', flexShrink: 0 }} />
            {label}
          </span>
        ))}
      </div>
    </div>
  )
}

// ── Online Pilots ─────────────────────────────────────────────────────────────

function OnlinePilotsSection({ online, leaderboard }: { online: OnlinePilot[]; leaderboard: Pilot[] }) {
  const navigate = useNavigate()

  const killMap = useMemo(() => {
    const m = new Map<string, number>()
    leaderboard.forEach(p => m.set(p.ucid, p.air_kills + p.ground_kills))
    return m
  }, [leaderboard])

  const blue = online.filter(p => p.side === 'Blue')
  const red  = online.filter(p => p.side === 'Red')
  const spec = online.filter(p => p.side === 'Neutral')
  const inAir = online.filter(p => p.aircraft != null).length

  function PilotRow({ p, color }: { p: OnlinePilot; color: string }) {
    const kills = killMap.get(p.ucid) ?? 0
    return (
      <div
        onClick={() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`)}
        style={{
          display: 'flex', alignItems: 'center', gap: 6,
          padding: '4px 10px', cursor: 'pointer',
          borderBottom: '1px solid rgba(30,30,30,0.5)',
          transition: 'background 0.1s',
        }}
        onMouseEnter={e => (e.currentTarget.style.background = 'rgba(255,255,255,0.025)')}
        onMouseLeave={e => (e.currentTarget.style.background = 'transparent')}
      >
        <span style={{ width: 4, height: 4, borderRadius: '50%', background: color, flexShrink: 0 }} />
        <div style={{ flex: 1, minWidth: 0 }}>
          <div style={{ fontSize: '0.65rem', color: 'var(--text-muted)', fontWeight: 600, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
            {p.name}
          </div>
          {p.aircraft && (
            <div style={{ fontSize: '0.52rem', color: 'var(--text-dim)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
              {p.aircraft}
            </div>
          )}
        </div>
        {kills > 0 && (
          <span className="font-mono-vs" style={{ fontSize: '0.6rem', fontWeight: 700, color, flexShrink: 0 }}>
            {kills}
          </span>
        )}
        {!p.aircraft && kills === 0 && (
          <span style={{ fontSize: '0.5rem', color: 'var(--text-dim)', flexShrink: 0 }}>spec</span>
        )}
      </div>
    )
  }

  function SideGroup({ list, color, label }: { list: OnlinePilot[]; color: string; label: string }) {
    if (!list.length) return null
    return (
      <>
        <div style={{
          padding: '2px 10px',
          background: `${color}08`,
          borderBottom: `1px solid ${color}18`,
        }}>
          <span style={{ fontSize: '0.5rem', color, letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 700 }}>
            {label} · {list.length}
          </span>
        </div>
        {list.map(p => <PilotRow key={p.ucid} p={p} color={color} />)}
      </>
    )
  }

  return (
    <div style={{ borderBottom: '1px solid var(--border)', flexShrink: 0 }}>
      <div style={{
        display: 'flex', alignItems: 'center', justifyContent: 'space-between',
        padding: '5px 10px',
        background: '#070707',
        borderBottom: '1px solid var(--border)',
      }}>
        <span style={{ fontSize: '0.55rem', letterSpacing: '0.18em', color: 'var(--text-dim)', textTransform: 'uppercase', fontWeight: 700 }}>
          Online Pilots
        </span>
        <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
          {inAir > 0 && (
            <span style={{ fontSize: '0.5rem', color: 'var(--accent)', letterSpacing: '0.1em', fontWeight: 700 }}>
              ✈ {inAir}
            </span>
          )}
          <span className="font-mono-vs" style={{ fontSize: '0.55rem', color: 'var(--text-dim)' }}>
            <span style={{ color: campaign.redColor }}>{red.length}</span>
            <span style={{ margin: '0 3px', color: 'var(--text-dim)' }}>·</span>
            <span style={{ color: campaign.blueColor }}>{blue.length}</span>
          </span>
        </div>
      </div>
      {online.length === 0 ? (
        <div style={{ padding: '10px', fontSize: '0.6rem', color: 'var(--text-dim)' }}>No pilots online</div>
      ) : (
        <>
          <SideGroup list={red}  color={campaign.redColor}  label={campaign.redLabel}  />
          <SideGroup list={blue} color={campaign.blueColor} label={campaign.blueLabel} />
          <SideGroup list={spec} color="#4b5563"            label="Spectators"         />
        </>
      )}
    </div>
  )
}

// ── Recent Kills ──────────────────────────────────────────────────────────────

function RecentKillsSection({ kills, nameMap }: { kills: Kill[]; nameMap: Map<string, string> }) {
  const shown = kills.slice(0, campaign.dashboardKillFeedCount)

  return (
    <div style={{ display: 'flex', flexDirection: 'column', flex: 1, overflow: 'hidden', minHeight: 0 }}>
      <div style={{
        display: 'flex', alignItems: 'center', justifyContent: 'space-between',
        padding: '5px 10px',
        borderBottom: '1px solid var(--border)',
        background: '#070707',
        flexShrink: 0,
      }}>
        <span style={{ fontSize: '0.55rem', letterSpacing: '0.18em', color: 'var(--text-dim)', textTransform: 'uppercase', fontWeight: 700 }}>
          Recent Kills
        </span>
        <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
          <KillSparkline kills={kills} />
          <Crosshair size={9} style={{ color: 'var(--text-dim)' }} />
        </div>
      </div>

      <div style={{ overflowY: 'auto', flex: 1 }}>
        {shown.length === 0 ? (
          <div style={{ padding: '10px', fontSize: '0.6rem', color: 'var(--text-dim)' }}>No kills recorded</div>
        ) : shown.map((k, i) => {
          const { label, color } = classifyKill(k.target_type)
          const killerColor = k.killer?.side === 'Blue' ? campaign.blueColor : k.killer?.side === 'Red' ? campaign.redColor : '#4b5563'
          const victimColor = k.victim.side  === 'Blue' ? campaign.blueColor : k.victim.side  === 'Red' ? campaign.redColor : '#4b5563'
          const killerName  = k.killer?.ucid ? nameMap.get(k.killer.ucid) : null
          return (
            <div
              key={i}
              style={{
                padding: '4px 10px',
                borderBottom: '1px solid rgba(30,30,30,0.5)',
                display: 'flex', gap: 6, alignItems: 'flex-start',
              }}
            >
              <span style={{
                fontSize: '0.46rem', fontWeight: 700, letterSpacing: '0.1em',
                color, border: `1px solid ${color}`, borderRadius: 1,
                padding: '1px 3px', flexShrink: 0, marginTop: 2, lineHeight: 1.4,
              }}>
                {label}
              </span>
              <div style={{ flex: 1, minWidth: 0 }}>
                <div style={{ fontSize: '0.6rem', color: killerColor, fontWeight: 600, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                  {killerName ?? k.killer?.side ?? '?'}
                  {k.killer?.weapon && (
                    <span style={{ fontWeight: 400, color: 'var(--text-dim)' }}> · {k.killer.weapon}</span>
                  )}
                </div>
                <div style={{ fontSize: '0.53rem', color: 'var(--text-dim)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                  <span style={{ color: victimColor }}>{k.victim.side}</span>
                  {k.target_type && <span> · {k.target_type}</span>}
                </div>
              </div>
              <span className="font-mono-vs" style={{ fontSize: '0.5rem', color: 'var(--text-dim)', flexShrink: 0, marginTop: 1 }}>
                {fmtTime(k.time)}
              </span>
            </div>
          )
        })}
      </div>
    </div>
  )
}

// ── Main ──────────────────────────────────────────────────────────────────────

export default function Dashboard() {
  const navigate = useNavigate()
  const { selectedRound } = useRound()
  const [showObjStrip, setShowObjStrip] = useState(true)

  const { data: online     = [] } = useQuery({ queryKey: ['online'],                  queryFn: api.online,                                                            refetchInterval: 10_000 })
  const { data: pilots     = [] } = useQuery({ queryKey: ['leaderboard'],             queryFn: api.leaderboard,                                                       refetchInterval: 60_000 })
  const { data: allPilots  = [] } = useQuery({ queryKey: ['all-pilots'],              queryFn: api.allPilots,                                                         refetchInterval: 120_000 })
  const { data: objectives = [] } = useQuery({ queryKey: ['objectives', selectedRound], queryFn: () => api.objectives(selectedRound),                                 refetchInterval: 30_000 })
  const { data: kills      = [] } = useQuery({ queryKey: ['kills-dash', selectedRound], queryFn: () => api.kills(selectedRound, campaign.dashboardKillFeedCount + 10), refetchInterval: 15_000 })

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
  const bluePct = total > 0 ? Math.round(blueObj / total * 100) : 0
  const redPct  = total > 0 ? Math.round(redObj  / total * 100) : 0

  const blueDelta = useDelta(blueObj)
  const redDelta  = useDelta(redObj)

  const rightPanelWidth = campaign.dashboardRightPanelWidth

  return (
    <div style={{ display: 'flex', flexDirection: 'column', flex: 1, overflow: 'hidden', background: 'var(--bg)' }}>

      {/* ── KPI strip ── */}
      <div className="kpi-strip" style={{ gridTemplateColumns: 'repeat(6, 1fr)' }}>
        <KpiCell
          label="Online"
          value={online.length}
          sub={`${blue.length} ${campaign.blueLabel} · ${red.length} ${campaign.redLabel}`}
          color="var(--text)"
        />
        <KpiCell
          label="In Air"
          value={inAir}
          sub={`${online.length > 0 ? Math.round(inAir / online.length * 100) : 0}% of online`}
          color="var(--accent)"
        />
        <KpiCell
          label={`${campaign.blueLabel} Holds`}
          value={blueObj}
          sub={`${bluePct}% of objectives`}
          color={campaign.blueColor}
          delta={blueDelta}
        />
        <KpiCell
          label={`${campaign.redLabel} Holds`}
          value={redObj}
          sub={`${redPct}% of objectives`}
          color={campaign.redColor}
          delta={redDelta}
        />
        <KpiCell
          label="Total Objectives"
          value={total}
          sub={`${objectives.filter(o => o.owner === 'Neutral').length} neutral`}
          color="var(--text-muted)"
        />
        <KpiCell
          label="Recent Kills"
          value={kills.length}
          sub="this session"
          color="var(--accent)"
        />
      </div>

      {/* ── Objectives strip toggle ── */}
      <div style={{
        display: 'flex', alignItems: 'center', justifyContent: 'space-between',
        padding: '3px 10px',
        background: '#060606',
        borderBottom: showObjStrip ? 'none' : '1px solid var(--border)',
        flexShrink: 0,
      }}>
        <span style={{ fontSize: '0.5rem', letterSpacing: '0.18em', color: 'var(--text-dim)', textTransform: 'uppercase', fontWeight: 700 }}>
          Objectives · {total}
        </span>
        <button
          onClick={() => setShowObjStrip(v => !v)}
          style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', display: 'flex', padding: 2 }}
        >
          {showObjStrip ? <ChevronUp size={11} /> : <ChevronDown size={11} />}
        </button>
      </div>
      {showObjStrip && <ObjectivesStrip objectives={objectives} />}

      {/* ── Map + right panel ── */}
      <div style={{ display: 'flex', flex: 1, overflow: 'hidden' }}>

        {/* Map */}
        <TacMap
          objectives={objectives}
          onOpenTacmap={() => navigate('/map')}
        />

        {/* Right panel */}
        <div style={{
          width: rightPanelWidth,
          flexShrink: 0,
          display: 'flex',
          flexDirection: 'column',
          background: '#080808',
          borderLeft: '1px solid var(--border)',
          overflow: 'hidden',
        }}>
          <OnlinePilotsSection online={online} leaderboard={pilots} />
          <RecentKillsSection kills={kills} nameMap={nameMap} />
        </div>

      </div>
    </div>
  )
}
