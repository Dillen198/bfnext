import { useEffect, useMemo, useRef, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { MapContainer, TileLayer, Marker, useMap } from 'react-leaflet'
import L from 'leaflet'
import type { LatLngBoundsExpression } from 'leaflet'
import 'leaflet/dist/leaflet.css'
import { useNavigate } from 'react-router-dom'
import {
  Crosshair, Plane, Users, Shield, AlertTriangle,
  Wind, Thermometer, Eye, Gauge, ExternalLink, Activity,
  TrendingUp, MapPin, Radio,
} from 'lucide-react'
import { api, type OnlinePilot, type Objective, type Pilot, type Kill, type PilotName, type Stats, type SrsClient, type SrsStatus } from '../api'
import { campaign } from '../config/campaign'
import { useTheme } from '../context/ThemeContext'
import { useRound } from '../context/RoundContext'

// ── Helpers ───────────────────────────────────────────────────────────────────

function classifyKill(t: string | null): { label: string; col: string } {
  const s = (t ?? '').toLowerCase()
  if (/\bf-\d|\bf\/a|mig-|su-\d|a-10|av-8|aircraft|heli|ka-|mi-|uh-|ah-/.test(s)) return { label: 'AIR',   col: 'var(--blue)'   }
  if (/ship|naval|carrier|frigate/.test(s))                                         return { label: 'NAVAL', col: 'var(--cyan)'   }
  if (/t-\d{2}|tank|armor|apc|ifv|bmp|btr/.test(s))                                return { label: 'ARMOR', col: 'var(--orange)' }
  if (/sa-\d|sam |patriot|radar|aaa|s-300|s-400|buk|tor/.test(s))                  return { label: 'AD',    col: 'var(--purple)' }
  if (/truck|supply|ural|kamaz|vehicle|car|logistic/.test(s))                       return { label: 'VEH',   col: 'var(--yellow)' }
  if (/infantry|soldier|troop/.test(s))                                             return { label: 'INF',   col: 'var(--accent)' }
  return { label: 'GND', col: 'var(--text-dim)' }
}

function fmtTimeZ(iso: string) {
  return new Date(iso).toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit', hour12: false }) + 'Z'
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

function windDir(deg: number) {
  const dirs = ['N','NE','E','SE','S','SW','W','NW']
  return dirs[Math.round(deg / 45) % 8]
}

function visStr(m: number | null | undefined) {
  if (!m) return '—'
  return m >= 9999 ? '10KM+' : `${(m / 1000).toFixed(1)}KM`
}

function cloudStr(m: number) {
  if (!m || m === 0) return 'CLEAR'
  const ft = Math.round(m * 3.281 / 100) * 100
  return `${ft.toLocaleString()}FT`
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

// ── TacMap ────────────────────────────────────────────────────────────────────

function FitBounds({ objectives }: { objectives: Objective[] }) {
  const map = useMap()
  useEffect(() => {
    if (!objectives.length) return
    map.fitBounds(objectives.map(o => [o.lat, o.lon] as [number, number]) as LatLngBoundsExpression,
      { padding: [28, 28], maxZoom: 9, animate: false })
  }, [map, objectives])
  return null
}

function TacMap({ objectives, onOpenTacmap }: { objectives: Objective[]; onOpenTacmap: () => void }) {
  const valid = objectives.filter(o => o.lat !== 0 || o.lon !== 0)
  const ownerColor = (owner: string) =>
    owner === 'Blue' ? campaign.blueColor :
    owner === 'Red'  ? campaign.redColor  :
                       '#4a5240'
  const blue = valid.filter(o => o.owner === 'Blue').length
  const red  = valid.filter(o => o.owner === 'Red').length
  const neu  = valid.filter(o => o.owner === 'Neutral').length

  // Generic shape-language glyphs (airbase/naval/factory/etc), not NATO or
  // Warsaw Pact military symbology -- same OBJ_ICON set used by the
  // Critical Objectives list below, so an objective reads the same way in
  // both places on this page.
  const markerIcon = (obj: Objective) => {
    const c = ownerColor(obj.owner)
    const alive = obj.health > 0
    const size = obj.kind === 'Airbase' ? 20 : (obj.kind === 'Carrier Group' || obj.kind === 'Naval Base') ? 18 : 15
    const symbol = OBJ_ICON[obj.kind] ?? '●'
    return L.divIcon({
      html: `<div style="width:${size}px;height:${size}px;border-radius:50%;
               background:${c}${alive ? '33' : '11'};border:1.5px solid ${c}${alive ? 'ff' : '66'};
               display:flex;align-items:center;justify-content:center;
               font-size:${Math.round(size * 0.55)}px;line-height:1;color:${c}${alive ? 'ff' : '77'};
               box-shadow:0 0 ${alive ? 6 : 0}px ${c}77;">${symbol}</div>`,
      className: '',
      iconSize: [size, size],
      iconAnchor: [size / 2, size / 2],
    })
  }

  const { theme } = useTheme()
  const canvasBase = theme === 'light'
    ? 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
    : 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}'

  return (
    <div style={{ position: 'relative', height: '100%', background: theme === 'light' ? '#dfe0d8' : '#050806' }}>
      <MapContainer center={campaign.mapCenter} zoom={campaign.mapZoom}
        style={{ position: 'absolute', inset: 0 }} zoomControl={false} attributionControl={false}>
        <TileLayer key={theme} url={canvasBase} maxZoom={19} opacity={0.5} />
        {valid.length > 0 && <FitBounds objectives={valid} />}
        {valid.map(obj => (
          <Marker key={obj.id} position={[obj.lat, obj.lon]} icon={markerIcon(obj)} />
        ))}
      </MapContainer>

      {/* Top-left stat chip */}
      <div style={{
        position: 'absolute', top: 8, left: 8, zIndex: 1000,
        background: 'rgba(8,11,6,0.90)', border: '1px solid var(--border)',
        padding: '5px 10px', display: 'flex', gap: 10,
        fontSize: '0.58rem', fontFamily: 'var(--font-mono)', letterSpacing: '0.1em',
      }}>
        <span style={{ color: campaign.blueColor }}>{blue} BLU</span>
        <span style={{ color: 'var(--border-light)' }}>·</span>
        <span style={{ color: campaign.redColor }}>{red} RED</span>
        <span style={{ color: 'var(--border-light)' }}>·</span>
        <span style={{ color: 'var(--text-dim)' }}>{neu} NEU</span>
      </div>

      {/* TACMAP button */}
      <button onClick={onOpenTacmap} style={{
        position: 'absolute', top: 8, right: 8, zIndex: 1000,
        display: 'flex', alignItems: 'center', gap: 5,
        background: 'rgba(8,11,6,0.90)', border: '1px solid var(--accent-border)',
        color: 'var(--accent-bright)', padding: '5px 10px',
        fontSize: '0.58rem', fontWeight: 700, letterSpacing: '0.14em',
        cursor: 'pointer', textTransform: 'uppercase', fontFamily: 'var(--font-mono)',
      }}>
        <ExternalLink size={9} /> TACMAP
      </button>

      {/* Bottom legend */}
      <div style={{
        position: 'absolute', bottom: 8, left: '50%', transform: 'translateX(-50%)', zIndex: 1000,
        display: 'flex', gap: 14, background: 'rgba(8,11,6,0.88)',
        padding: '4px 12px', border: '1px solid var(--border)',
        fontSize: '0.56rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', letterSpacing: '0.1em',
      }}>
        {[
          { label: campaign.blueLabel, color: campaign.blueColor },
          { label: campaign.redLabel,  color: campaign.redColor  },
          { label: 'NEUTRAL',          color: '#4a5240'          },
        ].map(({ label, color }) => (
          <span key={label} style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
            <span style={{ width: 6, height: 6, background: color, display: 'inline-block', flexShrink: 0 }} />
            {label.toUpperCase()}
          </span>
        ))}
      </div>
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

// ── Critical objectives ───────────────────────────────────────────────────────

const OBJ_ICON: Record<string, string> = {
  Airbase: '✈', FARP: '⬡', FOB: '▲', Factory: '⚙',
  'Logistics Hub': '◈', 'Naval Base': '⚓', 'Carrier Group': '⚓', 'Command Center': '◆',
}
const OBJ_KIND_SHORT: Record<string, string> = {
  Airbase: 'AB', FARP: 'FARP', FOB: 'FOB', Factory: 'FAC',
  'Logistics Hub': 'LOG', 'Naval Base': 'NB', 'Carrier Group': 'CVN', 'Command Center': 'CC',
}

function CriticalObjectives({ objectives }: { objectives: Objective[] }) {
  const crit = useMemo(() =>
    [...objectives].filter(o => o.health < 60).sort((a, b) => a.health - b.health).slice(0, 7),
    [objectives]
  )
  if (!crit.length) return (
    <div style={{ padding: '10px 14px', fontSize: '0.62rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', letterSpacing: '0.1em', textAlign: 'center' }}>
      NO CRITICAL ASSETS
    </div>
  )
  return (
    <div style={{ display: 'flex', flexDirection: 'column' }}>
      {crit.map(obj => {
        const oc  = obj.owner === 'Blue' ? campaign.blueColor : obj.owner === 'Red' ? campaign.redColor : '#4a5240'
        const hc  = obj.health < 30 ? 'var(--red)' : obj.health < 55 ? 'var(--yellow)' : 'var(--accent)'
        return (
          <div key={obj.id} style={{ padding: '6px 14px', borderBottom: '1px solid var(--border)', display: 'flex', alignItems: 'center', gap: 8 }}>
            <span style={{ fontSize: '0.85rem', color: oc, width: 16, textAlign: 'center', flexShrink: 0 }}>{OBJ_ICON[obj.kind] ?? '■'}</span>
            <div style={{ flex: 1, minWidth: 0 }}>
              <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', marginBottom: 3 }}>
                <span style={{ fontSize: '0.74rem', fontWeight: 600, color: 'var(--text)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', maxWidth: 130 }}>
                  {obj.name}
                </span>
                <div style={{ display: 'flex', alignItems: 'center', gap: 5, flexShrink: 0 }}>
                  <span style={{ fontSize: '0.6rem', color: oc, fontFamily: 'var(--font-mono)', fontWeight: 700, letterSpacing: '0.1em' }}>
                    {obj.owner === 'Neutral' ? 'NEUT' : obj.owner.toUpperCase()}
                  </span>
                  <span className="font-mono-vs" style={{ fontSize: '0.72rem', color: hc, fontWeight: 700 }}>{obj.health}%</span>
                </div>
              </div>
              <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                <div style={{ flex: 1, height: 4, background: 'var(--bg-elevated)', overflow: 'hidden' }}>
                  <div style={{ width: `${obj.health}%`, height: '100%', background: hc, transition: 'width 0.4s' }} />
                </div>
                <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', flexShrink: 0 }}>
                  L{obj.logi}·S{obj.supply}·F{obj.fuel}
                </span>
              </div>
            </div>
          </div>
        )
      })}
    </div>
  )
}

// ── Objective roster (all, scrollable) ───────────────────────────────────────

function ObjectiveRoster({ objectives }: { objectives: Objective[] }) {
  const sorted = useMemo(() =>
    [...objectives].filter(o => o.lat !== 0 || o.lon !== 0)
      .sort((a, b) => {
        const order: Record<string, number> = { Airbase: 0, 'Naval Base': 1, 'Carrier Group': 2, Factory: 3, 'Logistics Hub': 4, FARP: 5, FOB: 6, 'Command Center': 7 }
        return (order[a.kind] ?? 9) - (order[b.kind] ?? 9) || a.name.localeCompare(b.name)
      }),
    [objectives]
  )
  if (!sorted.length) return null

  const blue = sorted.filter(o => o.owner === 'Blue')
  const red  = sorted.filter(o => o.owner === 'Red')
  const neu  = sorted.filter(o => o.owner === 'Neutral')

  function ObjRow({ obj }: { obj: Objective }) {
    const oc = obj.owner === 'Blue' ? campaign.blueColor : obj.owner === 'Red' ? campaign.redColor : '#4a5240'
    const hc = obj.health < 30 ? 'var(--red)' : obj.health < 60 ? 'var(--yellow)' : 'var(--accent)'
    return (
      <div style={{ display: 'flex', alignItems: 'center', gap: 7, padding: '5px 14px', borderBottom: '1px solid var(--border)' }}>
        <span style={{ fontSize: '0.7rem', color: oc, width: 14, textAlign: 'center', flexShrink: 0 }}>{OBJ_ICON[obj.kind] ?? '■'}</span>
        <span style={{ fontSize: '0.68rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', width: 34, flexShrink: 0 }}>{OBJ_KIND_SHORT[obj.kind] ?? '—'}</span>
        <span style={{ fontSize: '0.75rem', color: 'var(--text)', fontWeight: 500, flex: 1, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{obj.name}</span>
        <div style={{ width: 50, height: 3, background: 'var(--bg-elevated)', flexShrink: 0, overflow: 'hidden' }}>
          <div style={{ width: `${obj.health}%`, height: '100%', background: hc }} />
        </div>
        <span className="font-mono-vs" style={{ fontSize: '0.68rem', color: hc, width: 30, textAlign: 'right', flexShrink: 0 }}>{obj.health}%</span>
        {obj.health < 40 && <AlertTriangle size={8} style={{ color: 'var(--yellow)', flexShrink: 0 }} />}
      </div>
    )
  }

  function Section({ label, items, color }: { label: string; items: Objective[]; color: string }) {
    if (!items.length) return null
    return (
      <>
        <div style={{ padding: '3px 14px', background: `${color}08`, borderBottom: `1px solid ${color}18`, display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
          <span style={{ fontSize: '0.62rem', color, letterSpacing: '0.18em', textTransform: 'uppercase', fontFamily: 'var(--font-mono)', fontWeight: 700 }}>{label}</span>
          <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>{items.length}</span>
        </div>
        {items.map(o => <ObjRow key={o.id} obj={o} />)}
      </>
    )
  }

  return (
    <>
      <Section label={campaign.blueLabel} items={blue} color={campaign.blueColor} />
      <Section label={campaign.redLabel}  items={red}  color={campaign.redColor}  />
      <Section label="NEUTRAL"            items={neu}  color="#4a5240"            />
    </>
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
    { icon: Thermometer, label: 'TEMP',  value: `${Math.round(w.temp_c)}°C` },
    { icon: Gauge,       label: 'QNH',   value: `${Math.round(w.qnh_hpa)} hPa` },
    { icon: Eye,         label: 'VIS',   value: visStr(w.visibility_m) },
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
        <Plane size={9} style={{ color: col, flexShrink: 0 }} />
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
        const medals = ['▲', '△', '○', '○', '○']
        return (
          <div key={p.ucid} onClick={() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`)} className="kill-row"
            style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '5px 12px', borderBottom: '1px solid var(--border)', cursor: 'pointer' }}>
            <span style={{ fontSize: '0.68rem', color: i === 0 ? 'var(--yellow)' : 'var(--text-dim)', fontFamily: 'var(--font-mono)', width: 12, flexShrink: 0 }}>{medals[i]}</span>
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

// ── SRS radio panel ───────────────────────────────────────────────────────────

function fmtFreq(hz: number): string {
  const mhz = hz / 1_000_000
  return mhz.toFixed(3) + ' MHz'
}

function fmtMod(mod: number): string {
  return mod === 1 ? 'FM' : mod === 2 ? 'IC' : 'AM'
}

function SrsPanel({ srs }: { srs: SrsStatus | undefined }) {
  const clients = srs?.clients ?? []
  const blue    = clients.filter(c => c.Coalition === 2)
  const red     = clients.filter(c => c.Coalition === 1)
  const spec    = clients.filter(c => c.Coalition === 0)

  function getActive(c: SrsClient): { freq: number; mod: number } | null {
    const radios = c.RadioInfo?.radios ?? []
    const r = radios.find(r => r.enabled && r.freq > 1) ?? radios[0]
    return r ? { freq: r.freq, mod: r.modulation } : null
  }

  function ClientRow({ c, sideColor }: { c: SrsClient; sideColor: string }) {
    const active = getActive(c)
    const inAir  = c.RadioInfo?.inAircraft ?? false
    return (
      <div style={{ display: 'flex', alignItems: 'center', gap: 7, padding: '5px 12px', borderBottom: '1px solid var(--border)' }}>
        <span style={{ width: 5, height: 5, background: sideColor, flexShrink: 0, marginTop: 1 }} />
        <div style={{ flex: 1, minWidth: 0 }}>
          <div style={{ display: 'flex', alignItems: 'center', gap: 5 }}>
            {inAir && <Plane size={8} style={{ color: sideColor, flexShrink: 0, opacity: 0.8 }} />}
            <span style={{ fontSize: '0.76rem', fontWeight: 600, color: 'var(--text)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
              {c.Name}
            </span>
          </div>
          {active && (
            <div className="font-mono-vs" style={{ fontSize: '0.65rem', color: 'var(--text-dim)', marginTop: 1 }}>
              <span style={{ color: 'var(--accent)', fontWeight: 700 }}>{fmtMod(active.mod)}</span>
              {' '}{fmtFreq(active.freq)}
            </div>
          )}
        </div>
        {(c.RadioInfo?.radios ?? []).filter(r => r.enabled && r.freq > 1).length > 1 && (
          <span className="font-mono-vs" style={{ fontSize: '0.63rem', color: 'var(--text-dim)', flexShrink: 0 }}>
            +{(c.RadioInfo?.radios ?? []).filter(r => r.enabled && r.freq > 1).length - 1}
          </span>
        )}
      </div>
    )
  }

  if (!srs) return (
    <div style={{ padding: '10px 12px', fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', letterSpacing: '0.12em', textAlign: 'center' }}>
      SRS OFFLINE — start bfdb with --srs-url
    </div>
  )

  if (!clients.length) return (
    <div style={{ padding: '10px 12px', fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', letterSpacing: '0.12em', textAlign: 'center' }}>
      NO CLIENTS CONNECTED
    </div>
  )

  function Section({ label, items, color }: { label: string; items: SrsClient[]; color: string }) {
    if (!items.length) return null
    return (
      <>
        <div style={{ padding: '2px 12px', background: `${color}0a`, borderBottom: `1px solid ${color}18` }}>
          <span style={{ fontSize: '0.6rem', color, letterSpacing: '0.18em', fontFamily: 'var(--font-mono)', fontWeight: 700 }}>
            {label} · {items.length}
          </span>
        </div>
        {items.map(c => <ClientRow key={c.ClientGuid} c={c} sideColor={color} />)}
      </>
    )
  }

  return (
    <>
      <Section label={campaign.blueLabel.toUpperCase()} items={blue} color={campaign.blueColor} />
      <Section label={campaign.redLabel.toUpperCase()}  items={red}  color={campaign.redColor}  />
      {spec.length > 0 && <Section label="SPECTATOR" items={spec} color="var(--text-dim)" />}
    </>
  )
}

// ── Panel wrapper ─────────────────────────────────────────────────────────────

function Panel({ title, icon: Icon, iconColor = 'var(--accent)', count, badge, children, style }: {
  title: string; icon: React.ElementType; iconColor?: string
  count?: number; badge?: React.ReactNode; children: React.ReactNode; style?: React.CSSProperties
}) {
  return (
    <div style={{ display: 'flex', flexDirection: 'column', background: 'var(--bg-card)', border: '1px solid var(--border)', overflow: 'hidden', ...style }}>
      <div style={{
        display: 'flex', alignItems: 'center', gap: 7, padding: '6px 12px',
        borderBottom: '1px solid var(--border)', background: 'rgba(0,0,0,0.25)', flexShrink: 0,
      }}>
        <Icon size={10} style={{ color: iconColor, flexShrink: 0 }} />
        <span style={{ fontSize: '0.66rem', fontWeight: 700, letterSpacing: '0.2em', textTransform: 'uppercase', color: 'var(--text-muted)', fontFamily: 'var(--font-mono)', flex: 1 }}>
          {title}
        </span>
        {badge}
        {count != null && (
          <span className="font-mono-vs" style={{ fontSize: '0.65rem', color: 'var(--text-dim)', background: 'var(--bg-elevated)', padding: '1px 5px' }}>{count}</span>
        )}
      </div>
      <div style={{ flex: 1, overflow: 'hidden', display: 'flex', flexDirection: 'column' }}>
        {children}
      </div>
    </div>
  )
}

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

export default function Dashboard() {
  const navigate = useNavigate()
  const { selectedRound } = useRound()

  const { data: online     = [] } = useQuery({ queryKey: ['online'],                    queryFn: api.online,                                                           refetchInterval: 10_000 })
  const { data: pilots     = [] } = useQuery({ queryKey: ['leaderboard'],               queryFn: api.leaderboard,                                                      refetchInterval: 60_000 })
  const { data: allPilots  = [] } = useQuery({ queryKey: ['all-pilots'],                queryFn: api.allPilots,                                                        refetchInterval: 120_000 })
  const { data: objectives = [] } = useQuery({ queryKey: ['objectives', selectedRound], queryFn: () => api.objectives(selectedRound),                                  refetchInterval: 30_000 })
  const { data: kills      = [] } = useQuery({ queryKey: ['kills-dash', selectedRound], queryFn: () => api.kills(selectedRound, campaign.dashboardKillFeedCount + 20), refetchInterval: 15_000 })
  const { data: stats            } = useQuery({ queryKey: ['stats'],                    queryFn: api.stats,                                                            refetchInterval: 60_000 })
  const { data: srs              } = useQuery({ queryKey: ['srs'],                      queryFn: api.srs,                                                              refetchInterval: 5_000  })

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
  const critCount = objectives.filter(o => o.health < 40).length

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

      {/* ── KPI strip ── */}
      <div className="kpi-strip">
        <KpiCell label="ONLINE" value={online.length} icon={Users}
          sub={`${blue.length} blu · ${red.length} red`} />
        <KpiCell label="AIRBORNE" value={inAir} icon={Plane} color="var(--accent)"
          sub={online.length > 0 ? `${Math.round(inAir / online.length * 100)}% of online` : '—'} />
        <KpiCell label={`${campaign.blueLabel} OBJ`} value={blueObj}
          icon={Shield} color={campaign.blueColor} delta={blueDelta}
          sub={`${total > 0 ? Math.round(blueObj / total * 100) : 0}% territory`} />
        <KpiCell label={`${campaign.redLabel} OBJ`} value={redObj}
          icon={Shield} color={campaign.redColor} delta={redDelta}
          sub={`${total > 0 ? Math.round(redObj / total * 100) : 0}% territory`} />
        <KpiCell label="CRITICAL" value={critCount} icon={AlertTriangle}
          color={critCount > 0 ? 'var(--red)' : 'var(--text-dim)'}
          sub="obj below 40% HP" />
        <KpiCell label="KILLS" value={kills.length} icon={Crosshair} color="var(--red)"
          sub="this session" />
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
            <MapPin size={9} style={{ color: 'var(--accent)' }} />
            <span style={{ fontSize: '0.65rem', fontWeight: 700, letterSpacing: '0.22em', textTransform: 'uppercase', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', flex: 1 }}>
              TACTICAL OVERVIEW
            </span>
            <LiveBadge />
          </div>
          {/* Map */}
          <div style={isMobile ? { flex: '0 0 260px', overflow: 'hidden' } : { flex: '0 0 55%', overflow: 'hidden', minHeight: 0 }}>
            <TacMap objectives={objectives} onOpenTacmap={() => navigate('/map')} />
          </div>

          {/* SRS radio panel */}
          <div style={isMobile
            ? { flex: '0 0 auto', maxHeight: 220, display: 'flex', flexDirection: 'column', borderTop: '1px solid var(--border)' }
            : { flex: 1, minHeight: 0, display: 'flex', flexDirection: 'column', borderTop: '1px solid var(--border)' }}>
            <div style={{ padding: '5px 12px', background: 'rgba(0,0,0,0.25)', borderBottom: '1px solid var(--border)', flexShrink: 0, display: 'flex', alignItems: 'center', gap: 8 }}>
              <Radio size={9} style={{ color: 'var(--accent)' }} />
              <span style={{ fontSize: '0.65rem', fontWeight: 700, letterSpacing: '0.22em', textTransform: 'uppercase', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', flex: 1 }}>
                SRS RADIO NET
              </span>
              {srs && srs.clients.length > 0 && (
                <span className="vs-pulse font-mono-vs" style={{ fontSize: '0.63rem', color: 'var(--accent)', border: '1px solid var(--accent-border)', padding: '1px 5px' }}>
                  {srs.clients.length} ONLINE
                </span>
              )}
              {srs?.version && (
                <span className="font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>v{srs.version}</span>
              )}
            </div>
            <div style={{ overflowY: 'auto', flex: 1 }}>
              <SrsPanel srs={srs} />
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

          {/* Weather */}
          <Panel title="WEATHER BRIEF" icon={Wind} iconColor="var(--cyan)" style={{ flexShrink: 0 }}>
            <WeatherBrief stats={stats} />
          </Panel>

          {/* Territory */}
          <Panel title="TERRITORY CONTROL" icon={Activity} iconColor="var(--yellow)" style={{ flexShrink: 0 }}>
            <TerritoryBar objectives={objectives} />
          </Panel>

          {/* Critical assets */}
          <Panel title="CRITICAL ASSETS" icon={AlertTriangle} iconColor="var(--red)"
            badge={critCount > 0 ? <span className="font-mono-vs" style={{ fontSize: '0.58rem', color: 'var(--red)', background: 'rgba(204,68,68,0.12)', border: '1px solid rgba(204,68,68,0.3)', padding: '1px 6px' }}>{critCount} CRIT</span> : undefined}
            style={{ flexShrink: 0 }}>
            <CriticalObjectives objectives={objectives} />
          </Panel>

          {/* Full objective roster — scrollable */}
          <Panel title="OBJECTIVE ROSTER" icon={MapPin} iconColor="var(--accent)" count={total}
            style={isMobile ? { flex: '0 0 auto', maxHeight: 320 } : { flex: 1 }}>
            <div style={{ overflowY: 'auto', flex: 1 }}>
              <ObjectiveRoster objectives={objectives} />
            </div>
          </Panel>
        </div>

        {/* ══ RIGHT COLUMN (25%) ══ */}
        <div style={{
          flex: isMobile ? undefined : 1, width: isMobile ? '100%' : undefined,
          display: 'flex', flexDirection: 'column', overflow: isMobile ? 'visible' : 'hidden', minWidth: 0,
        }}>

          {/* Air picture */}
          <Panel title="AIR PICTURE" icon={Plane} iconColor="var(--accent)"
            badge={<LiveBadge />}
            count={inAir}
            style={isMobile ? { flex: '0 0 auto', maxHeight: 220 } : { flex: '0 0 auto', maxHeight: '30%' }}>
            <div style={{ overflowY: 'auto', flex: 1 }}>
              <AirPicture online={online} />
              <GroundRoster online={online} />
            </div>
          </Panel>

          {/* Top shooters */}
          <Panel title="TOP SHOOTERS" icon={TrendingUp} iconColor="var(--yellow)" style={{ flexShrink: 0 }}>
            <TopShooters pilots={pilots} />
          </Panel>

          {/* Engagement log */}
          <Panel title="ENGAGEMENT LOG" icon={Crosshair} iconColor="var(--red)"
            badge={<LiveBadge />}
            count={kills.length}
            style={isMobile ? { flex: '0 0 auto', maxHeight: 320 } : { flex: 1 }}>
            <div style={{ overflowY: 'auto', flex: 1 }}>
              <EngagementLog kills={kills} nameMap={nameMap} />
            </div>
          </Panel>
        </div>
      </div>
    </div>
  )
}
