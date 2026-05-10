import { useEffect, useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { MapContainer, TileLayer, CircleMarker, useMap } from 'react-leaflet'
import type { LatLngBoundsExpression } from 'leaflet'
import 'leaflet/dist/leaflet.css'
import { useNavigate } from 'react-router-dom'
import { ExternalLink, Crosshair } from 'lucide-react'
import { api, type OnlinePilot, type Objective, type Pilot, type Kill } from '../api'
import { campaign } from '../config/campaign'
import { useRound } from '../context/RoundContext'

// ── Kill classifier (shared with KillFeed) ────────────────────────────────────

function classifyKill(targetType: string | null): { label: string; color: string } {
  const t = (targetType ?? '').toLowerCase()
  if (t.includes('air') || t.includes('plane') || t.includes('heli') || t.includes('fighter'))
    return { label: 'AIR', color: '#3b82f6' }
  if (t.includes('ship') || t.includes('naval') || t.includes('carrier'))
    return { label: 'NAVAL', color: '#06b6d4' }
  if (t.includes('armor') || t.includes('tank') || t.includes('apc') || t.includes('ifv'))
    return { label: 'ARMOR', color: '#f97316' }
  if (t.includes('sam') || t.includes('radar') || t.includes('aaa') || t.includes('air defence'))
    return { label: 'AD', color: '#a78bfa' }
  if (t.includes('truck') || t.includes('supply') || t.includes('vehicle') || t.includes('car'))
    return { label: 'VEH', color: '#fbbf24' }
  if (t.includes('infantry') || t.includes('troop') || t.includes('soldier'))
    return { label: 'INF', color: '#22c55e' }
  return { label: 'GND', color: '#94a3b8' }
}

function fmtTime(iso: string) {
  return new Date(iso).toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit', hour12: false })
}

// ── Map ───────────────────────────────────────────────────────────────────────

function FitBounds({ objectives }: { objectives: Objective[] }) {
  const map = useMap()
  useEffect(() => {
    if (!objectives.length) return
    const bounds: LatLngBoundsExpression = objectives.map(o => [o.lat, o.lon] as [number, number])
    map.fitBounds(bounds, { padding: [32, 32], maxZoom: 9, animate: false })
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
          opacity={0.75}
        />
        {valid.length > 0 && <FitBounds objectives={valid} />}
        {valid.map(obj => (
          <CircleMarker
            key={obj.id}
            center={[obj.lat, obj.lon]}
            radius={obj.kind === 'Airbase' ? 7 : obj.kind === 'Carrier Group' ? 6 : 5}
            pathOptions={{ ...dot(obj.owner), fillOpacity: obj.health > 0 ? 0.85 : 0.25, weight: 1.5 }}
          />
        ))}
      </MapContainer>

      {/* TACMAP button */}
      <button
        onClick={onOpenTacmap}
        style={{
          position: 'absolute', top: 8, right: 8, zIndex: 1000,
          display: 'flex', alignItems: 'center', gap: 4,
          background: 'rgba(0,0,0,0.75)', border: '1px solid var(--border)',
          color: 'var(--accent)', borderRadius: 2, padding: '4px 8px',
          fontSize: '0.6rem', letterSpacing: '0.1em', cursor: 'pointer',
          fontFamily: "'Bebas Neue', sans-serif",
        }}
      >
        <ExternalLink size={9} /> TACMAP
      </button>

      {/* Legend */}
      <div style={{
        position: 'absolute', bottom: 8, left: 8, zIndex: 1000,
        display: 'flex', gap: 8, background: 'rgba(0,0,0,0.65)',
        padding: '4px 8px', borderRadius: 2,
        fontSize: '0.58rem', color: '#94a3b8',
      }}>
        {[
          { label: campaign.blueLabel, color: campaign.blueColor },
          { label: campaign.redLabel,  color: campaign.redColor  },
          { label: 'Neutral',          color: '#6b7280'          },
        ].map(({ label, color }) => (
          <span key={label} style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
            <span style={{ width: 7, height: 7, borderRadius: '50%', background: color, display: 'inline-block' }} />
            {label}
          </span>
        ))}
      </div>
    </div>
  )
}

// ── Objectives strip ──────────────────────────────────────────────────────────

const OBJ_CATEGORIES: { label: string; kinds: string[]; icon: string }[] = [
  { label: 'Airfields',        kinds: ['Airbase'],                          icon: '✈' },
  { label: 'FARPs & FOBs',     kinds: ['FARP', 'FOB'],                      icon: '⬡' },
  { label: 'Infrastructure',   kinds: ['Factory', 'Logistics Hub'],         icon: '⚙' },
  { label: 'Naval & SAM',      kinds: ['Naval Base', 'Carrier Group', 'SAM Site', 'Port'], icon: '⚓' },
]

function ObjectivesStrip({ objectives }: { objectives: Objective[] }) {
  const cols = OBJ_CATEGORIES.map(cat => ({
    ...cat,
    items: objectives.filter(o => cat.kinds.includes(o.kind)).sort((a, b) => a.name.localeCompare(b.name)),
  })).filter(c => c.items.length > 0)

  if (!cols.length) return null

  return (
    <div style={{
      display: 'grid',
      gridTemplateColumns: `repeat(${Math.min(cols.length, 4)}, 1fr)`,
      borderTop: '1px solid var(--border)',
      maxHeight: campaign.dashboardObjectivesHeight,
      overflow: 'hidden',
      flexShrink: 0,
    }}>
      {cols.map((col, ci) => (
        <div key={col.label} style={{
          borderRight: ci < cols.length - 1 ? '1px solid var(--border)' : 'none',
          display: 'flex', flexDirection: 'column', overflow: 'hidden',
        }}>
          <div style={{
            display: 'flex', alignItems: 'center', justifyContent: 'space-between',
            padding: '4px 10px',
            background: '#0d0d0d',
            borderBottom: '1px solid var(--border)',
            flexShrink: 0,
          }}>
            <span style={{ fontSize: '0.58rem', letterSpacing: '0.14em', color: 'var(--text-dim)', textTransform: 'uppercase', fontWeight: 700 }}>
              {col.icon} {col.label}
            </span>
            <span style={{ fontSize: '0.58rem', color: 'var(--text-dim)', fontFamily: 'monospace' }}>{col.items.length}</span>
          </div>
          <div style={{ overflowY: 'auto', flex: 1 }}>
            {col.items.map(obj => {
              const ownerColor = obj.owner === 'Blue' ? campaign.blueColor : obj.owner === 'Red' ? campaign.redColor : '#4b5563'
              return (
                <div key={obj.id} style={{
                  display: 'flex', alignItems: 'center', gap: 6,
                  padding: '3px 10px',
                  borderBottom: '1px solid rgba(42,42,42,0.4)',
                }}>
                  <span style={{ width: 5, height: 5, borderRadius: '50%', background: ownerColor, flexShrink: 0 }} />
                  <span style={{ fontSize: '0.62rem', color: 'var(--text-muted)', flex: 1, minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                    {obj.name}
                  </span>
                </div>
              )
            })}
          </div>
        </div>
      ))}
    </div>
  )
}

// ── Right panel — Online Pilots ───────────────────────────────────────────────

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

  const group = (list: OnlinePilot[], color: string) =>
    list.map(p => {
      const kills = killMap.get(p.ucid) ?? 0
      return (
        <div
          key={p.ucid}
          onClick={() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`)}
          style={{
            display: 'flex', alignItems: 'center', gap: 6,
            padding: '4px 10px', cursor: 'pointer',
            borderBottom: '1px solid rgba(42,42,42,0.3)',
          }}
          onMouseEnter={e => (e.currentTarget.style.background = 'rgba(255,255,255,0.03)')}
          onMouseLeave={e => (e.currentTarget.style.background = 'transparent')}
        >
          <span style={{ width: 5, height: 5, borderRadius: '50%', background: color, flexShrink: 0 }} />
          <div style={{ flex: 1, minWidth: 0 }}>
            <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', fontWeight: 600, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
              {p.name}
            </div>
            {p.aircraft && (
              <div style={{ fontSize: '0.54rem', color: 'var(--text-dim)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                {p.aircraft}
              </div>
            )}
          </div>
          {kills > 0 && (
            <span style={{ fontSize: '0.62rem', fontFamily: 'monospace', fontWeight: 700, color, flexShrink: 0 }}>
              {kills}
            </span>
          )}
          {!p.aircraft && kills === 0 && (
            <span style={{ fontSize: '0.52rem', color: 'var(--text-dim)', flexShrink: 0 }}>spec</span>
          )}
        </div>
      )
    })

  return (
    <div style={{ borderBottom: '1px solid var(--border)', flexShrink: 0 }}>
      {/* header */}
      <div style={{
        display: 'flex', alignItems: 'center', justifyContent: 'space-between',
        padding: '5px 10px 4px',
        borderBottom: '1px solid var(--border)',
        background: '#0d0d0d',
      }}>
        <span style={{ fontSize: '0.58rem', letterSpacing: '0.16em', color: 'var(--text-dim)', textTransform: 'uppercase', fontWeight: 700 }}>
          Online Pilots
        </span>
        <span style={{ fontSize: '0.58rem', fontFamily: 'monospace', color: 'var(--text-dim)' }}>
          <span style={{ color: campaign.redColor }}>{red.length}</span>
          <span style={{ margin: '0 3px', color: 'var(--border)' }}>·</span>
          <span style={{ color: campaign.blueColor }}>{blue.length}</span>
        </span>
      </div>

      {online.length === 0 ? (
        <div style={{ padding: '8px 10px', fontSize: '0.62rem', color: 'var(--text-dim)' }}>No pilots online</div>
      ) : (
        <>
          {red.length > 0 && (
            <>
              <div style={{ padding: '2px 10px', background: 'rgba(239,68,68,0.05)', borderBottom: '1px solid rgba(239,68,68,0.1)' }}>
                <span style={{ fontSize: '0.52rem', color: campaign.redColor, letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 700 }}>
                  {campaign.redLabel} · {red.length}
                </span>
              </div>
              {group(red, campaign.redColor)}
            </>
          )}
          {blue.length > 0 && (
            <>
              <div style={{ padding: '2px 10px', background: 'rgba(59,130,246,0.05)', borderBottom: '1px solid rgba(59,130,246,0.1)' }}>
                <span style={{ fontSize: '0.52rem', color: campaign.blueColor, letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 700 }}>
                  {campaign.blueLabel} · {blue.length}
                </span>
              </div>
              {group(blue, campaign.blueColor)}
            </>
          )}
          {spec.length > 0 && (
            <>
              <div style={{ padding: '2px 10px', background: 'rgba(107,114,128,0.05)', borderBottom: '1px solid rgba(107,114,128,0.1)' }}>
                <span style={{ fontSize: '0.52rem', color: '#6b7280', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 700 }}>
                  Spectators · {spec.length}
                </span>
              </div>
              {group(spec, '#6b7280')}
            </>
          )}
        </>
      )}
    </div>
  )
}

// ── Right panel — Recent Kills ────────────────────────────────────────────────

function RecentKillsSection({ kills }: { kills: Kill[] }) {
  const navigate = useNavigate()
  const shown = kills.slice(0, campaign.dashboardKillFeedCount)

  return (
    <div style={{ display: 'flex', flexDirection: 'column', flex: 1, overflow: 'hidden', minHeight: 0 }}>
      {/* header */}
      <div style={{
        display: 'flex', alignItems: 'center', justifyContent: 'space-between',
        padding: '5px 10px 4px',
        borderBottom: '1px solid var(--border)',
        background: '#0d0d0d',
        flexShrink: 0,
      }}>
        <span style={{ fontSize: '0.58rem', letterSpacing: '0.16em', color: 'var(--text-dim)', textTransform: 'uppercase', fontWeight: 700 }}>
          Recent Kills
        </span>
        <Crosshair size={9} style={{ color: 'var(--text-dim)' }} />
      </div>

      <div style={{ overflowY: 'auto', flex: 1 }}>
        {shown.length === 0 ? (
          <div style={{ padding: '8px 10px', fontSize: '0.62rem', color: 'var(--text-dim)' }}>No kills recorded</div>
        ) : shown.map((k, i) => {
          const { label, color } = classifyKill(k.target_type)
          const killerColor = k.killer?.side === 'Blue' ? campaign.blueColor : k.killer?.side === 'Red' ? campaign.redColor : '#6b7280'
          const victimColor = k.victim.side === 'Blue' ? campaign.blueColor : k.victim.side === 'Red' ? campaign.redColor : '#6b7280'
          return (
            <div
              key={i}
              style={{
                padding: '4px 10px',
                borderBottom: '1px solid rgba(42,42,42,0.4)',
                display: 'flex', gap: 6, alignItems: 'flex-start',
              }}
            >
              {/* kill type badge */}
              <span style={{
                fontSize: '0.48rem', fontWeight: 700, letterSpacing: '0.1em',
                color, border: `1px solid ${color}`, borderRadius: 1,
                padding: '1px 3px', flexShrink: 0, marginTop: 2, lineHeight: 1.4,
              }}>
                {label}
              </span>

              {/* content */}
              <div style={{ flex: 1, minWidth: 0 }}>
                {k.killer?.ucid ? (
                  <span
                    style={{ fontSize: '0.62rem', color: killerColor, fontWeight: 600, cursor: 'pointer' }}
                    onClick={() => k.killer?.ucid && navigate(`/pilots?ucid=${encodeURIComponent(k.killer.ucid)}`)}
                  >
                    {/* name shown via kills feed — ucid available but no name here, use side */}
                    {k.killer.side}
                  </span>
                ) : (
                  <span style={{ fontSize: '0.62rem', color: killerColor, fontWeight: 600 }}>
                    {k.killer?.side ?? '?'}
                  </span>
                )}
                {k.killer?.weapon && (
                  <span style={{ fontSize: '0.55rem', color: 'var(--text-dim)' }}> · {k.killer.weapon}</span>
                )}
                <div style={{ fontSize: '0.55rem', color: 'var(--text-dim)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                  <span style={{ color: victimColor }}>{k.victim.side}</span>
                  {k.target_type && <span> · {k.target_type}</span>}
                </div>
              </div>

              {/* time */}
              <span style={{ fontSize: '0.52rem', color: 'var(--text-dim)', fontFamily: 'monospace', flexShrink: 0, marginTop: 1 }}>
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

  const { data: online = []     } = useQuery({ queryKey: ['online'],      queryFn: api.online,                                   refetchInterval: 10_000 })
  const { data: pilots = []     } = useQuery({ queryKey: ['leaderboard'], queryFn: api.leaderboard,                              refetchInterval: 60_000 })
  const { data: objectives = [] } = useQuery({ queryKey: ['objectives', selectedRound],  queryFn: () => api.objectives(selectedRound),  refetchInterval: 30_000 })
  const { data: kills = []      } = useQuery({ queryKey: ['kills-dash', selectedRound],  queryFn: () => api.kills(selectedRound, campaign.dashboardKillFeedCount + 10), refetchInterval: 15_000 })

  return (
    <div style={{ display: 'flex', flex: 1, overflow: 'hidden', background: 'var(--bg)' }}>

      {/* ── LEFT — map + objectives strip ── */}
      <div style={{ flex: 1, display: 'flex', flexDirection: 'column', overflow: 'hidden', minWidth: 0 }}>
        <TacMap
          objectives={objectives}
          onOpenTacmap={() => navigate('/map')}
        />
        <ObjectivesStrip objectives={objectives} />
      </div>

      {/* ── RIGHT — live panel ── */}
      <div style={{
        width: campaign.dashboardRightPanelWidth,
        flexShrink: 0,
        display: 'flex',
        flexDirection: 'column',
        background: '#0a0a0a',
        borderLeft: '1px solid var(--border)',
        overflow: 'hidden',
      }}>
        <OnlinePilotsSection online={online} leaderboard={pilots} />
        <RecentKillsSection kills={kills} />
      </div>

    </div>
  )
}
