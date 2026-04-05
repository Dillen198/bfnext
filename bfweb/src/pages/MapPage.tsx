import { useEffect, useRef, useState, useCallback } from 'react'
import { useQuery } from '@tanstack/react-query'
import {
  MapContainer, TileLayer, CircleMarker, Marker, Popup, Tooltip,
  useMap, ScaleControl, Polyline, Circle, useMapEvents,
} from 'react-leaflet'
import type { LatLngBoundsExpression, LatLngExpression } from 'leaflet'
import L from 'leaflet'
import 'leaflet.heat'
import { api, type Objective, type MapUnit } from '../api'
import PageHeader from '../components/PageHeader'
import { createMapIcon, type IconStyle } from '../lib/mapIcons'
import {
  Navigation, Ruler, Shield, Trash2,
  ChevronRight, ChevronLeft, X, MapPin, Radio, Flame,
} from 'lucide-react'

// ── localStorage persistence helpers ──────────────────────────────────
const STORAGE_PREFIX = 'bfmap_'

function loadState<T>(key: string, fallback: T): T {
  try {
    const raw = localStorage.getItem(STORAGE_PREFIX + key)
    if (raw === null) return fallback
    return JSON.parse(raw) as T
  } catch { return fallback }
}

function saveState<T>(key: string, value: T): void {
  try { localStorage.setItem(STORAGE_PREFIX + key, JSON.stringify(value)) } catch { /* quota */ }
}

function usePersistedState<T>(key: string, fallback: T): [T, React.Dispatch<React.SetStateAction<T>>] {
  const [state, setState] = useState<T>(() => loadState(key, fallback))
  const ref = useRef(state)
  ref.current = state
  useEffect(() => { saveState(key, ref.current) }, [key, state])
  return [state, setState]
}

// ── Heatmap component ─────────────────────────────────────────────────
function HeatmapLayer({ points }: { points: [number, number, number][] }) {
  const map = useMap()
  const layerRef = useRef<L.HeatLayer | null>(null)

  useEffect(() => {
    if (points.length === 0) {
      if (layerRef.current) {
        map.removeLayer(layerRef.current)
        layerRef.current = null
      }
      return
    }
    if (!layerRef.current) {
      layerRef.current = (L as any).heatLayer(points, {
        radius: 25,
        blur: 20,
        maxZoom: 12,
        max: 1.0,
        gradient: { 0.2: '#0000ff', 0.4: '#00ffff', 0.6: '#00ff00', 0.8: '#ffff00', 1.0: '#ff0000' },
      }).addTo(map)
    } else {
      layerRef.current.setLatLngs(points as any)
    }
    return () => {
      if (layerRef.current) {
        map.removeLayer(layerRef.current)
        layerRef.current = null
      }
    }
  }, [map, points])

  return null
}

// ── Tile layers ────────────────────────────────────────────────────────
const TILE_LAYERS = {
  satellite: {
    label: 'SAT',
    url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
    attr: 'Tiles &copy; Esri',
  },
  hybrid: {
    label: 'HYBRID',
    url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
    labelUrl: 'https://{s}.basemaps.cartocdn.com/dark_only_labels/{z}/{x}/{y}{r}.png',
    attr: 'Tiles &copy; Esri',
  },
  tactical: {
    label: 'TACMAP',
    url: 'https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png',
    attr: '&copy; CARTO',
  },
  topo: {
    label: 'TOPO',
    url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
    attr: 'Tiles &copy; Esri',
  },
} as const
type TileKey = keyof typeof TILE_LAYERS

// ── DCS region presets ─────────────────────────────────────────────────
const DCS_REGIONS = [
  { label: 'Caucasus',     center: [42.35, 43.50] as [number, number], zoom: 8 },
  { label: 'Persian Gulf', center: [26.50, 55.50] as [number, number], zoom: 7 },
  { label: 'Syria',        center: [35.00, 38.50] as [number, number], zoom: 8 },
  { label: 'NTTR',         center: [37.00, -116.00] as [number, number], zoom: 8 },
  { label: 'Marianas',     center: [14.50, 145.50] as [number, number], zoom: 8 },
  { label: 'Normandy',     center: [49.00, -0.50] as [number, number], zoom: 8 },
  { label: 'Kola',         center: [69.00, 28.00] as [number, number], zoom: 7 },
  { label: 'Sinai',        center: [29.00, 34.50] as [number, number], zoom: 8 },
]

// ── Planning types ─────────────────────────────────────────────────────
type PlanMode = 'none' | 'waypoint' | 'marker' | 'measure'
type MarkerType = 'IP' | 'TGT' | 'CP' | 'BULL' | 'EGR'
interface Waypoint { id: string; lat: number; lon: number }
interface PlanMarker { id: string; lat: number; lon: number; type: MarkerType }

// ── Geo helpers ────────────────────────────────────────────────────────
function haversineNm(lat1: number, lon1: number, lat2: number, lon2: number): number {
  const R = 3440.065
  const φ1 = lat1 * Math.PI / 180, φ2 = lat2 * Math.PI / 180
  const Δφ = (lat2 - lat1) * Math.PI / 180
  const Δλ = (lon2 - lon1) * Math.PI / 180
  const a = Math.sin(Δφ/2)**2 + Math.cos(φ1)*Math.cos(φ2)*Math.sin(Δλ/2)**2
  return R * 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a))
}

function trueBearing(lat1: number, lon1: number, lat2: number, lon2: number): number {
  const φ1 = lat1 * Math.PI / 180, φ2 = lat2 * Math.PI / 180
  const Δλ = (lon2 - lon1) * Math.PI / 180
  const y = Math.sin(Δλ)*Math.cos(φ2)
  const x = Math.cos(φ1)*Math.sin(φ2) - Math.sin(φ1)*Math.cos(φ2)*Math.cos(Δλ)
  return ((Math.atan2(y, x) * 180 / Math.PI) + 360) % 360
}

function nmToMeters(nm: number): number { return nm * 1852 }

function fmtCoord(lat: number, lon: number): string {
  return `${Math.abs(lat).toFixed(4)}°${lat>=0?'N':'S'} ${Math.abs(lon).toFixed(4)}°${lon>=0?'E':'W'}`
}

function fmtTime(minutes: number): string {
  const h = Math.floor(minutes / 60)
  const m = Math.round(minutes % 60)
  return h > 0 ? `${h}h ${m}m` : `${m}m`
}

// ── Leaflet DivIcon factories ──────────────────────────────────────────
function waypointIcon(num: number, isFinal: boolean): L.DivIcon {
  const color = isFinal ? '#ef4444' : '#00b4f5'
  return L.divIcon({
    html: `<div style="width:22px;height:22px;border-radius:50%;background:${color}30;border:2px solid ${color};color:#fff;font-size:10px;font-weight:700;display:flex;align-items:center;justify-content:center;font-family:monospace;box-shadow:0 0 10px ${color}88;">${num}</div>`,
    className: '',
    iconSize: [22, 22],
    iconAnchor: [11, 11],
  })
}

const MARKER_STYLES: Record<MarkerType, { bg: string; border: string }> = {
  IP:   { bg: '#22c55e30', border: '#22c55e' },
  TGT:  { bg: '#ef444430', border: '#ef4444' },
  CP:   { bg: '#3b82f630', border: '#3b82f6' },
  BULL: { bg: '#ffffff15', border: '#e2e8f0' },
  EGR:  { bg: '#eab30830', border: '#eab308' },
}

const MARKER_ICONS: Record<MarkerType, string> = {
  IP:   `<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linejoin="round"><polygon points="12,2 22,22 2,22"/></svg>`,
  TGT:  `<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor"><circle cx="12" cy="12" r="8" stroke-width="2"/><line x1="12" y1="2" x2="12" y2="22" stroke-width="1.5"/><line x1="2" y1="12" x2="22" y2="12" stroke-width="1.5"/></svg>`,
  CP:   `<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linejoin="round"><polygon points="12,2 22,12 12,22 2,12"/></svg>`,
  BULL: `<svg width="14" height="14" viewBox="0 0 24 24"><circle cx="12" cy="12" r="10" fill="none" stroke="currentColor" stroke-width="1.5"/><circle cx="12" cy="12" r="5" fill="none" stroke="currentColor" stroke-width="1.5"/><circle cx="12" cy="12" r="1.5" fill="currentColor"/></svg>`,
  EGR:  `<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="2" y1="12" x2="20" y2="12"/><polyline points="14,6 20,12 14,18"/></svg>`,
}

function planMarkerIcon(type: MarkerType): L.DivIcon {
  const s = MARKER_STYLES[type]
  const icon = MARKER_ICONS[type]
  return L.divIcon({
    html: `<div style="padding:4px 6px;border-radius:3px;background:${s.bg};color:#fff;font-family:monospace;white-space:nowrap;box-shadow:0 0 8px ${s.border}77;display:flex;flex-direction:column;align-items:center;gap:1px;line-height:1.1;">${icon}<span style="font-size:8px;font-weight:700;letter-spacing:0.08em;opacity:0.9;">${type}</span></div>`,
    className: '',
    popupAnchor: [5, 0] as unknown as L.PointExpression,
  })
}

// ── Radar unit icon helpers ────────────────────────────────────────────
function isAirUnit(tags: string[]): boolean {
  return tags.some(t => t === 'Aircraft' || t === 'Helicopter' || t === 'AWACS')
}

function unitCategory(tags: string[]): string {
  if (tags.includes('Aircraft') || tags.includes('AWACS')) return 'AIR'
  if (tags.includes('Helicopter')) return 'HELO'
  if (tags.includes('SAM')) return 'SAM'
  if (tags.includes('EWR')) return 'EWR'
  if (tags.includes('Armor')) return 'ARMOR'
  if (tags.includes('AAA')) return 'AAA'
  if (tags.includes('Boat')) return 'NAVAL'
  if (tags.includes('Artillery')) return 'ARTY'
  return 'GND'
}

function radarUnitIcon(unit: MapUnit): L.DivIcon {
  const color = unit.owner === 'Red' ? '#ef4444' : unit.owner === 'Blue' ? '#3b82f6' : '#6b7280'
  const isAir = isAirUnit(unit.tags)
  const cat = unitCategory(unit.tags)
  const hdg = unit.heading

  if (isAir) {
    // Directional chevron for aircraft
    return L.divIcon({
      html: `<div style="position:relative;width:20px;height:20px;">
        <svg width="20" height="20" viewBox="0 0 20 20" style="transform:rotate(${hdg}deg);filter:drop-shadow(0 0 4px ${color}88);">
          <polygon points="10,2 16,16 10,12 4,16" fill="${color}" fill-opacity="0.85" stroke="${color}" stroke-width="1"/>
        </svg>
        <div style="position:absolute;top:20px;left:50%;transform:translateX(-50%);white-space:nowrap;font-size:7px;font-family:monospace;font-weight:700;color:${color};text-shadow:0 0 3px #000,0 0 6px #000;letter-spacing:0.05em;">${cat}</div>
      </div>`,
      className: '',
      iconSize: [20, 30],
      iconAnchor: [10, 10],
    })
  }

  // Diamond for ground units
  return L.divIcon({
    html: `<div style="position:relative;width:14px;height:14px;">
      <svg width="14" height="14" viewBox="0 0 14 14" style="filter:drop-shadow(0 0 3px ${color}66);">
        <polygon points="7,1 13,7 7,13 1,7" fill="${color}" fill-opacity="0.7" stroke="${color}" stroke-width="1"/>
      </svg>
      <div style="position:absolute;top:14px;left:50%;transform:translateX(-50%);white-space:nowrap;font-size:7px;font-family:monospace;font-weight:700;color:${color};text-shadow:0 0 3px #000,0 0 6px #000;letter-spacing:0.05em;">${cat}</div>
    </div>`,
    className: '',
    iconSize: [14, 24],
    iconAnchor: [7, 7],
  })
}

// ── Threat ring radii (nautical miles) ────────────────────────────────
const THREAT_NM: Record<string, number> = {
  Airbase: 25, 'Naval Base': 40, 'Carrier Group': 50,
  Factory: 15, 'Logistics Hub': 10, FARP: 5, FOB: 5,
}

// ── Map sub-components ─────────────────────────────────────────────────

function FlyTo({ center, zoom }: { center: [number, number]; zoom: number }) {
  const map = useMap()
  useEffect(() => { map.flyTo(center, zoom, { duration: 1.2 }) }, [center, zoom, map])
  return null
}

function FitBounds({ objectives }: { objectives: Objective[] }) {
  const map = useMap()
  const done = useRef(false)
  useEffect(() => {
    if (done.current || objectives.length === 0) return
    done.current = true
    const lats = objectives.map(o => o.lat)
    const lons = objectives.map(o => o.lon)
    const bounds: LatLngBoundsExpression = [
      [Math.min(...lats) - 0.3, Math.min(...lons) - 0.3],
      [Math.max(...lats) + 0.3, Math.max(...lons) + 0.3],
    ]
    map.fitBounds(bounds, { padding: [30, 30], maxZoom: 10 })
  }, [objectives, map])
  return null
}

function MapInteractionLayer({
  mode, onMapClick,
}: { mode: PlanMode; onMapClick: (lat: number, lon: number) => void }) {
  const map = useMap()
  useEffect(() => {
    const c = map.getContainer()
    c.style.cursor = mode !== 'none' ? 'crosshair' : ''
    return () => { c.style.cursor = '' }
  }, [mode, map])
  useMapEvents({ click: e => { if (mode !== 'none') onMapClick(e.latlng.lat, e.latlng.lng) } })
  return null
}

// ── Side colours ──────────────────────────────────────────────────────
const SIDE_COLOR: Record<string, string> = {
  Red: '#ef4444', Blue: '#3b82f6', Neutral: '#6b7280',
}
const KIND_RADIUS: Record<string, number> = {
  Airbase: 13, 'Naval Base': 12, 'Carrier Group': 11, Factory: 9, default: 7,
}
const KIND_SYMBOL: Record<string, string> = {
  Airbase: '✈', FARP: '⬡', FOB: '⬡', 'Logistics Hub': '⬡',
  'Naval Base': '⚓', 'Carrier Group': '⚓', Factory: '⚙',
}

// ── Planning panel ─────────────────────────────────────────────────────
function PlanningPanel({
  waypoints, planMarkers, measurePts,
  speed, onSpeedChange,
  onRemoveWaypoint, onClearWaypoints,
  onRemoveMarker, onClearMarkers,
  showThreats, onToggleThreats,
}: {
  waypoints: Waypoint[]
  planMarkers: PlanMarker[]
  measurePts: { lat: number; lon: number }[]
  speed: number
  onSpeedChange: (v: number) => void
  onRemoveWaypoint: (id: string) => void
  onClearWaypoints: () => void
  onRemoveMarker: (id: string) => void
  onClearMarkers: () => void
  showThreats: boolean
  onToggleThreats: () => void
}) {
  const [tab, setTab] = useState<'route' | 'markers' | 'measure'>('route')

  // Build legs
  let totalNm = 0
  const legs = waypoints.slice(0, -1).map((wp, i) => {
    const next = waypoints[i + 1]!
    const dist = haversineNm(wp.lat, wp.lon, next.lat, next.lon)
    const brg  = trueBearing(wp.lat, wp.lon, next.lat, next.lon)
    const ete  = speed > 0 ? dist / speed * 60 : 0
    totalNm += dist
    return { from: i + 1, to: i + 2, dist, brg, ete }
  })
  const totalEte = speed > 0 ? totalNm / speed * 60 : 0

  // Measure info
  const measDist = measurePts.length === 2
    ? haversineNm(measurePts[0]!.lat, measurePts[0]!.lon, measurePts[1]!.lat, measurePts[1]!.lon)
    : null
  const measBrg = measurePts.length === 2
    ? trueBearing(measurePts[0]!.lat, measurePts[0]!.lon, measurePts[1]!.lat, measurePts[1]!.lon)
    : null

  return (
    <div className="w-72 flex-shrink-0 flex flex-col border-l border-[#1e3a5f]/50 overflow-hidden" style={{ background: '#040b16' }}>
      {/* Panel header */}
      <div className="px-3 py-2.5 border-b border-[#1e3a5f]/40 flex items-center justify-between">
        <span className="text-[10px] font-bold text-slate-400 uppercase tracking-widest">Mission Planning</span>
        <button
          onClick={onToggleThreats}
          className={`flex items-center gap-1 text-[9px] font-bold px-2 py-1 rounded transition-colors ${
            showThreats ? 'text-red-400 bg-red-500/15 border border-red-500/30' : 'text-slate-600 border border-[#1e3a5f]/40 hover:text-slate-400'
          }`}
        >
          <Shield size={9} />
          THREATS
        </button>
      </div>

      {/* Tabs */}
      <div className="flex border-b border-[#1e3a5f]/40">
        {([['route', 'ROUTE'], ['markers', 'MARKERS'], ['measure', 'MEASURE']] as const).map(([t, label]) => (
          <button
            key={t}
            onClick={() => setTab(t)}
            className={`flex-1 py-2 text-[9px] font-bold tracking-widest transition-colors ${
              tab === t ? 'text-blue-300 border-b-2 border-blue-400 -mb-px' : 'text-slate-700 hover:text-slate-500'
            }`}
          >
            {label}
          </button>
        ))}
      </div>

      <div className="flex-1 overflow-y-auto">

        {/* ── Route tab ── */}
        {tab === 'route' && (
          <div className="p-3 space-y-3">
            {/* Speed input */}
            <div className="flex items-center gap-2">
              <span className="text-[9px] text-slate-600 uppercase tracking-widest w-10">KTAS</span>
              <input
                type="number"
                min={0}
                max={2000}
                value={speed}
                onChange={e => onSpeedChange(Number(e.target.value))}
                className="flex-1 bg-[#080f1a] border border-[#1e3a5f]/60 rounded px-2 py-1 text-[11px] font-mono text-slate-200 focus:outline-none focus:border-blue-500/50"
              />
              <span className="text-[9px] text-slate-700">kts</span>
            </div>

            {/* Totals */}
            {waypoints.length >= 2 && (
              <div className="tac-card p-3 grid grid-cols-2 gap-2 text-center">
                <div>
                  <div className="text-[18px] font-bold font-mono text-cyan-400">{totalNm.toFixed(1)}</div>
                  <div className="text-[8px] text-slate-700 uppercase tracking-widest">NM Total</div>
                </div>
                <div>
                  <div className="text-[18px] font-bold font-mono text-purple-400">{speed > 0 ? fmtTime(totalEte) : '—'}</div>
                  <div className="text-[8px] text-slate-700 uppercase tracking-widest">ETE</div>
                </div>
              </div>
            )}

            {/* Waypoints list */}
            {waypoints.length === 0 ? (
              <div className="text-center py-6 text-slate-700 text-[10px]">
                <Navigation size={20} className="mx-auto mb-2 opacity-30" />
                Click the map in ROUTE mode<br/>to add waypoints
              </div>
            ) : (
              <div className="space-y-1">
                <div className="flex justify-between items-center mb-2">
                  <span className="text-[9px] text-slate-600 uppercase tracking-widest">{waypoints.length} waypoints</span>
                  <button onClick={onClearWaypoints} className="text-[9px] text-red-500/60 hover:text-red-400 flex items-center gap-1">
                    <Trash2 size={9} /> Clear
                  </button>
                </div>
                {waypoints.map((wp, i) => {
                  const leg = legs[i - 1]
                  return (
                    <div key={wp.id} className="tac-card text-[10px]">
                      {/* Leg info (above waypoint, for all except first) */}
                      {leg && (
                        <div className="px-2 py-1 border-b border-[#1e3a5f]/30 flex justify-between bg-[#03080f]">
                          <span className="text-slate-600 font-mono">{leg.dist.toFixed(1)} NM</span>
                          <span className="text-slate-500 font-mono">{Math.round(leg.brg).toString().padStart(3,'0')}°T</span>
                          {speed > 0 && <span className="text-purple-500 font-mono">{fmtTime(leg.ete)}</span>}
                        </div>
                      )}
                      <div className="flex items-center gap-2 px-2 py-2">
                        <span className={`w-5 h-5 rounded-full flex items-center justify-center text-[9px] font-bold flex-shrink-0 ${
                          i === waypoints.length - 1 ? 'bg-red-500/20 text-red-400 border border-red-500/40' : 'bg-cyan-500/15 text-cyan-400 border border-cyan-500/30'
                        }`}>
                          {i + 1}
                        </span>
                        <div className="flex-1 min-w-0">
                          <div className="font-mono text-[9px] text-slate-400 truncate">{fmtCoord(wp.lat, wp.lon)}</div>
                        </div>
                        <button onClick={() => onRemoveWaypoint(wp.id)} className="text-slate-700 hover:text-red-400 ml-1 flex-shrink-0">
                          <X size={10} />
                        </button>
                      </div>
                    </div>
                  )
                })}
              </div>
            )}
          </div>
        )}

        {/* ── Markers tab ── */}
        {tab === 'markers' && (
          <div className="p-3 space-y-3">
            {planMarkers.length === 0 ? (
              <div className="text-center py-6 text-slate-700 text-[10px]">
                <MapPin size={20} className="mx-auto mb-2 opacity-30" />
                Click the map in MARK mode<br/>to add point labels
              </div>
            ) : (
              <>
                <div className="flex justify-between items-center">
                  <span className="text-[9px] text-slate-600 uppercase tracking-widest">{planMarkers.length} markers</span>
                  <button onClick={onClearMarkers} className="text-[9px] text-red-500/60 hover:text-red-400 flex items-center gap-1">
                    <Trash2 size={9} /> Clear all
                  </button>
                </div>
                <div className="space-y-1">
                  {planMarkers.map(m => {
                    const s = MARKER_STYLES[m.type]
                    return (
                      <div key={m.id} className="tac-card flex items-center gap-2 px-3 py-2">
                        <span
                          className="text-[9px] font-bold px-1.5 py-0.5 rounded font-mono"
                          style={{ color: s.border, background: s.bg, border: `1px solid ${s.border}55` }}
                        >
                          {m.type}
                        </span>
                        <div className="flex-1 min-w-0">
                          <div className="text-[9px] font-mono text-slate-500 truncate">{fmtCoord(m.lat, m.lon)}</div>
                        </div>
                        <button onClick={() => onRemoveMarker(m.id)} className="text-slate-700 hover:text-red-400">
                          <X size={10} />
                        </button>
                      </div>
                    )
                  })}
                </div>
              </>
            )}
          </div>
        )}

        {/* ── Measure tab ── */}
        {tab === 'measure' && (
          <div className="p-3 space-y-3">
            {measurePts.length === 0 ? (
              <div className="text-center py-6 text-slate-700 text-[10px]">
                <Ruler size={20} className="mx-auto mb-2 opacity-30" />
                Click the map in MEASURE mode<br/>to place two points
              </div>
            ) : (
              <div className="space-y-2">
                {measurePts.map((p, i) => (
                  <div key={i} className="tac-card px-3 py-2">
                    <div className="text-[9px] text-slate-600 uppercase tracking-widest mb-1">Point {i + 1}</div>
                    <div className="text-[10px] font-mono text-slate-300">{fmtCoord(p.lat, p.lon)}</div>
                  </div>
                ))}

                {measDist !== null && measBrg !== null && (
                  <div className="tac-card p-3 space-y-2">
                    <div className="text-[9px] text-slate-600 uppercase tracking-widest border-b border-[#1e3a5f]/40 pb-2 mb-2">Result</div>
                    <div className="grid grid-cols-2 gap-2 text-center">
                      <div>
                        <div className="text-[22px] font-bold font-mono text-cyan-400">{measDist.toFixed(1)}</div>
                        <div className="text-[8px] text-slate-700 uppercase tracking-widest">Nautical Miles</div>
                      </div>
                      <div>
                        <div className="text-[22px] font-bold font-mono text-orange-400">{Math.round(measBrg).toString().padStart(3,'0')}°</div>
                        <div className="text-[8px] text-slate-700 uppercase tracking-widest">True Bearing</div>
                      </div>
                    </div>
                    <div className="grid grid-cols-2 gap-2 text-center text-[10px]">
                      <div>
                        <div className="font-mono text-slate-400">{(measDist * 1.852).toFixed(1)} km</div>
                        <div className="text-slate-700 text-[8px]">Kilometres</div>
                      </div>
                      <div>
                        <div className="font-mono text-slate-400">{((measBrg + 180) % 360).toString().padStart(3,'0')}°</div>
                        <div className="text-slate-700 text-[8px]">Reciprocal</div>
                      </div>
                    </div>
                  </div>
                )}
              </div>
            )}
          </div>
        )}

      </div>

      {/* Threat ring legend */}
      {showThreats && (
        <div className="px-3 py-2 border-t border-[#1e3a5f]/40 space-y-1">
          <div className="text-[9px] text-slate-700 uppercase tracking-widest mb-1.5">Threat Rings</div>
          {Object.entries(THREAT_NM).map(([kind, nm]) => (
            <div key={kind} className="flex justify-between text-[9px]">
              <span className="text-slate-600">{kind}</span>
              <span className="font-mono text-slate-500">{nm} NM</span>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

// ═══════════════════════════════════════════════════════════════════════
// Main MapPage
// ═══════════════════════════════════════════════════════════════════════
export default function MapPage() {
  // ── Persisted UI settings ───────────────────────────────────────────
  const [tileKey,     setTileKey]     = usePersistedState<TileKey>('tileKey', 'satellite')
  const [sideFilter,  setSideFilter]  = usePersistedState<'All' | 'Red' | 'Blue' | 'Neutral'>('sideFilter', 'All')
  const [iconStyle,   setIconStyle]   = usePersistedState<IconStyle>('iconStyle', 'dot')
  const [showThreats, setShowThreats] = usePersistedState('showThreats', false)
  const [showRadar,   setShowRadar]   = usePersistedState('showRadar', true)
  const [showHeatmap, setShowHeatmap] = usePersistedState('showHeatmap', false)
  const [showPanel,   setShowPanel]   = usePersistedState('showPanel', true)
  const [speed,       setSpeed]       = usePersistedState('speed', 400)

  // ── Persisted planning data ─────────────────────────────────────────
  const [waypoints,   setWaypoints]   = usePersistedState<Waypoint[]>('waypoints', [])
  const [planMarkers, setPlanMarkers] = usePersistedState<PlanMarker[]>('planMarkers', [])
  const [measurePts,  setMeasurePts]  = usePersistedState<{lat:number;lon:number}[]>('measurePts', [])

  // ── Ephemeral state ─────────────────────────────────────────────────
  const [flyTarget,   setFlyTarget]   = useState<{ center: [number, number]; zoom: number } | null>(null)
  const [selectedId,  setSelectedId]  = useState<string | null>(null)
  const [planMode,    setPlanMode]    = useState<PlanMode>('none')
  const [markerType,  setMarkerType]  = useState<MarkerType>('IP')

  const tileLayer = TILE_LAYERS[tileKey]

  const { data: objectives = [], isLoading } = useQuery({
    queryKey: ['objectives'],
    queryFn: () => api.objectives(),
    refetchInterval: 30_000,
  })
  const { data: radarUnits = [] } = useQuery({
    queryKey: ['units'],
    queryFn: () => api.units(),
    refetchInterval: 5_000,
  })
  const { data: rounds = [] } = useQuery({
    queryKey: ['rounds'],
    queryFn: api.rounds,
    refetchInterval: 60_000,
  })

  const activeRound  = rounds.find(r => r.active)
  const validObjs    = objectives.filter(o => o.lat !== 0 || o.lon !== 0)
  const shown        = sideFilter === 'All' ? validObjs : validObjs.filter(o => o.owner === sideFilter)

  const counts = {
    All:     validObjs.length,
    Red:     validObjs.filter(o => o.owner === 'Red').length,
    Blue:    validObjs.filter(o => o.owner === 'Blue').length,
    Neutral: validObjs.filter(o => o.owner === 'Neutral').length,
  }
  const redPct  = counts.All > 0 ? Math.round(counts.Red  / counts.All * 100) : 0
  const bluePct = counts.All > 0 ? Math.round(counts.Blue / counts.All * 100) : 0

  // ── Heatmap data: combine unit contacts + contested objectives ──────
  const heatmapPoints: [number, number, number][] = (() => {
    if (!showHeatmap) return []
    const pts: [number, number, number][] = []
    // Radar contacts are activity hotspots
    for (const u of radarUnits) {
      pts.push([u.lat, u.lon, 0.6])
    }
    // Contested objectives (low health = active combat area)
    for (const o of validObjs) {
      if (o.health < 90) {
        const intensity = Math.max(0.3, 1.0 - o.health / 100)
        pts.push([o.lat, o.lon, intensity])
      }
    }
    return pts
  })()

  // ── Map click handler ───────────────────────────────────────────────
  const handleMapClick = useCallback((lat: number, lon: number) => {
    if (planMode === 'waypoint') {
      setWaypoints(prev => [...prev, { id: crypto.randomUUID(), lat, lon }])
    } else if (planMode === 'marker') {
      setPlanMarkers(prev => [...prev, { id: crypto.randomUUID(), lat, lon, type: markerType }])
    } else if (planMode === 'measure') {
      setMeasurePts(prev => prev.length >= 2 ? [{ lat, lon }] : [...prev, { lat, lon }])
    }
  }, [planMode, markerType])

  // ── Mode toggle (click active mode to deactivate) ───────────────────
  function toggleMode(m: PlanMode) {
    setPlanMode(prev => prev === m ? 'none' : m)
  }

  // Build route polyline positions
  const routePositions = waypoints.map(w => [w.lat, w.lon] as LatLngExpression)

  // Build measure line positions
  const measurePositions = measurePts.map(p => [p.lat, p.lon] as LatLngExpression)

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader
        title="TACTICAL MAP"
        sub={activeRound ? activeRound.scenario : 'No active campaign'}
        right={
          <div className="flex items-center gap-3">
            <div className="flex items-center gap-2 text-[11px] font-semibold tabular-nums">
              <span className="text-red-400">{redPct}%</span>
              <div className="w-20 h-1.5 bg-[#0d1117] rounded-full overflow-hidden flex">
                <div className="h-full bg-red-500/70"  style={{ width: `${redPct}%` }} />
                <div className="h-full bg-blue-500/70" style={{ width: `${bluePct}%` }} />
              </div>
              <span className="text-blue-400">{bluePct}%</span>
            </div>
            <span className={`flex items-center gap-1.5 text-[10px] font-semibold tracking-wider px-2 py-1 rounded border ${
              activeRound
                ? 'text-green-400 bg-green-500/10 border-green-500/25'
                : 'text-slate-600 bg-slate-800/30 border-slate-700/30'
            }`}>
              <span className={`w-1.5 h-1.5 rounded-full ${activeRound ? 'bg-green-400 glow-pulse' : 'bg-slate-600'}`} />
              {activeRound ? 'LIVE' : 'OFFLINE'}
            </span>
          </div>
        }
      />

      {/* ── Toolbar row 1: map controls ──────────────────────────────── */}
      <div
        className="flex items-center gap-2 px-4 py-2 border-b border-[#1e3a5f]/40 flex-shrink-0 flex-wrap gap-y-1.5"
        style={{ background: '#040c18' }}
      >
        {/* Tile switcher */}
        <div className="flex rounded border border-[#1e3a5f]/50 overflow-hidden">
          {(Object.keys(TILE_LAYERS) as TileKey[]).map(k => (
            <button key={k} onClick={() => setTileKey(k)}
              className={`px-2.5 py-1.5 text-[9px] font-bold tracking-wider transition-colors ${
                tileKey === k ? 'bg-blue-500/15 text-blue-300' : 'text-slate-600 hover:text-slate-400 hover:bg-white/[0.02]'
              }`}
            >
              {TILE_LAYERS[k].label}
            </button>
          ))}
        </div>

        <div className="w-px h-4 bg-[#1e3a5f]/50" />

        {/* Side filter */}
        <div className="flex rounded border border-[#1e3a5f]/50 overflow-hidden">
          {(['All', 'Red', 'Blue', 'Neutral'] as const).map(f => (
            <button key={f} onClick={() => setSideFilter(f)}
              className={`px-2.5 py-1.5 text-[9px] font-bold tracking-wider transition-colors ${
                sideFilter === f
                  ? f === 'Red'     ? 'bg-red-500/15 text-red-400'
                  : f === 'Blue'    ? 'bg-blue-500/15 text-blue-400'
                  : f === 'Neutral' ? 'bg-slate-500/15 text-slate-400'
                  : 'bg-blue-500/10 text-blue-300'
                  : 'text-slate-600 hover:text-slate-400 hover:bg-white/[0.02]'
              }`}
            >
              {f === 'All' ? `ALL (${counts.All})` : `${f.toUpperCase()} (${counts[f]})`}
            </button>
          ))}
        </div>

        <div className="w-px h-4 bg-[#1e3a5f]/50" />

        {/* Icon style */}
        <div className="flex rounded border border-[#1e3a5f]/50 overflow-hidden">
          {([
            { key: 'dot',     label: '● DOT' },
            { key: 'nato',    label: '⬛ NATO' },
            { key: 'russian', label: '◆ RU' },
          ] as { key: IconStyle; label: string }[]).map(opt => (
            <button key={opt.key} onClick={() => setIconStyle(opt.key)}
              className={`px-2.5 py-1.5 text-[9px] font-bold tracking-wider transition-colors ${
                iconStyle === opt.key ? 'bg-blue-500/15 text-blue-300' : 'text-slate-600 hover:text-slate-400 hover:bg-white/[0.02]'
              }`}
            >
              {opt.label}
            </button>
          ))}
        </div>

        <div className="w-px h-4 bg-[#1e3a5f]/50" />

        {/* Region jump */}
        <div className="flex items-center gap-1 flex-wrap">
          {DCS_REGIONS.map(r => (
            <button key={r.label} onClick={() => setFlyTarget({ center: r.center, zoom: r.zoom })}
              className="px-2 py-1.5 text-[9px] font-bold tracking-wider text-slate-600 hover:text-blue-400 hover:bg-blue-500/[0.05] rounded transition-colors border border-transparent hover:border-blue-500/20"
            >
              {r.label}
            </button>
          ))}
        </div>

        <span className="ml-auto text-[9px] text-slate-700 tabular-nums">{shown.length} obj</span>
      </div>

      {/* ── Toolbar row 2: planning tools ─────────────────────────────── */}
      <div
        className="flex items-center gap-2 px-4 py-1.5 border-b border-[#1e3a5f]/40 flex-shrink-0"
        style={{ background: '#03090f' }}
      >
        <span className="text-[9px] text-slate-700 uppercase tracking-widest mr-1">Plan</span>

        {/* ROUTE mode */}
        <button
          onClick={() => toggleMode('waypoint')}
          className={`flex items-center gap-1.5 px-3 py-1.5 rounded text-[9px] font-bold tracking-wider border transition-colors ${
            planMode === 'waypoint'
              ? 'bg-cyan-500/15 text-cyan-300 border-cyan-500/40'
              : 'text-slate-600 border-[#1e3a5f]/50 hover:text-cyan-400 hover:border-cyan-500/30'
          }`}
        >
          <Navigation size={11} />
          ROUTE {waypoints.length > 0 ? `(${waypoints.length})` : ''}
        </button>

        {/* MARK mode */}
        <button
          onClick={() => toggleMode('marker')}
          className={`flex items-center gap-1.5 px-3 py-1.5 rounded text-[9px] font-bold tracking-wider border transition-colors ${
            planMode === 'marker'
              ? 'bg-yellow-500/15 text-yellow-300 border-yellow-500/40'
              : 'text-slate-600 border-[#1e3a5f]/50 hover:text-yellow-400 hover:border-yellow-500/30'
          }`}
        >
          <MapPin size={11} />
          MARK
        </button>

        {/* Marker type selector (shown when in mark mode) */}
        {planMode === 'marker' && (
          <div className="flex rounded border border-[#1e3a5f]/50 overflow-hidden">
            {(['IP', 'TGT', 'CP', 'BULL', 'EGR'] as MarkerType[]).map(t => (
              <button key={t} onClick={() => setMarkerType(t)}
                className={`px-2.5 py-1.5 text-[9px] font-bold transition-colors ${
                  markerType === t
                    ? 'text-white'
                    : 'text-slate-700 hover:text-slate-400'
                }`}
                style={markerType === t ? {
                  background: MARKER_STYLES[t].bg,
                  color: MARKER_STYLES[t].border,
                  borderRight: `1px solid ${MARKER_STYLES[t].border}33`,
                } : {}}
              >
                {t}
              </button>
            ))}
          </div>
        )}

        {/* MEASURE mode */}
        <button
          onClick={() => toggleMode('measure')}
          className={`flex items-center gap-1.5 px-3 py-1.5 rounded text-[9px] font-bold tracking-wider border transition-colors ${
            planMode === 'measure'
              ? 'bg-purple-500/15 text-purple-300 border-purple-500/40'
              : 'text-slate-600 border-[#1e3a5f]/50 hover:text-purple-400 hover:border-purple-500/30'
          }`}
        >
          <Ruler size={11} />
          MEASURE
        </button>

        {/* RADAR toggle */}
        <button
          onClick={() => setShowRadar(t => !t)}
          className={`flex items-center gap-1.5 px-3 py-1.5 rounded text-[9px] font-bold tracking-wider border transition-colors ${
            showRadar
              ? 'bg-green-500/15 text-green-300 border-green-500/40'
              : 'text-slate-600 border-[#1e3a5f]/50 hover:text-green-400 hover:border-green-500/30'
          }`}
        >
          <Radio size={11} />
          RADAR {radarUnits.length > 0 ? `(${radarUnits.length})` : ''}
        </button>

        {/* THREATS toggle */}
        <button
          onClick={() => setShowThreats(t => !t)}
          className={`flex items-center gap-1.5 px-3 py-1.5 rounded text-[9px] font-bold tracking-wider border transition-colors ${
            showThreats
              ? 'bg-red-500/15 text-red-300 border-red-500/40'
              : 'text-slate-600 border-[#1e3a5f]/50 hover:text-red-400 hover:border-red-500/30'
          }`}
        >
          <Shield size={11} />
          THREATS
        </button>

        {/* HEATMAP toggle */}
        <button
          onClick={() => setShowHeatmap(t => !t)}
          className={`flex items-center gap-1.5 px-3 py-1.5 rounded text-[9px] font-bold tracking-wider border transition-colors ${
            showHeatmap
              ? 'bg-orange-500/15 text-orange-300 border-orange-500/40'
              : 'text-slate-600 border-[#1e3a5f]/50 hover:text-orange-400 hover:border-orange-500/30'
          }`}
        >
          <Flame size={11} />
          HEAT
        </button>

        {/* Clear all */}
        {(waypoints.length > 0 || planMarkers.length > 0 || measurePts.length > 0) && (
          <button
            onClick={() => {
              setWaypoints([])
              setPlanMarkers([])
              setMeasurePts([])
              setPlanMode('none')
            }}
            className="flex items-center gap-1.5 px-3 py-1.5 rounded text-[9px] font-bold tracking-wider border border-[#1e3a5f]/50 text-slate-600 hover:text-red-400 hover:border-red-500/30 transition-colors"
          >
            <Trash2 size={11} />
            CLEAR
          </button>
        )}

        {/* Active mode indicator */}
        {planMode !== 'none' && (
          <span className="text-[9px] text-slate-600 italic ml-1">
            click map to {planMode === 'waypoint' ? 'add waypoint' : planMode === 'marker' ? `place ${markerType}` : 'measure'}
          </span>
        )}

        {/* Panel toggle */}
        <button
          onClick={() => setShowPanel(v => !v)}
          className="ml-auto flex items-center gap-1 text-[9px] text-slate-600 hover:text-slate-400 transition-colors"
        >
          {showPanel ? <ChevronRight size={12} /> : <ChevronLeft size={12} />}
          {showPanel ? 'Hide' : 'Plan'}
        </button>
      </div>

      {/* ── Map + Panel ─────────────────────────────────────────────── */}
      <div className="flex-1 flex overflow-hidden">
        {/* Map */}
        <div className="flex-1 relative overflow-hidden">
          {isLoading && (
            <div className="absolute inset-0 flex items-center justify-center bg-[#020810] z-20 pointer-events-none">
              <span className="text-[11px] text-slate-600 tracking-widest animate-pulse">LOADING MAP…</span>
            </div>
          )}

          <MapContainer
            center={[42.35, 43.50]}
            zoom={7}
            style={{ position: 'absolute', inset: 0, width: '100%', height: '100%' }}
            zoomControl={false}
            attributionControl={true}
          >
            <TileLayer key={tileKey} url={tileLayer.url} attribution={tileLayer.attr} maxZoom={19} />

            {tileKey === 'hybrid' && (
              <TileLayer
                url="https://{s}.basemaps.cartocdn.com/dark_only_labels/{z}/{x}/{y}{r}.png"
                attribution=""
                maxZoom={19}
                pane="overlayPane"
              />
            )}

            <ScaleControl position="bottomleft" />

            {validObjs.length > 0 && <FitBounds objectives={validObjs} />}
            {flyTarget && <FlyTo center={flyTarget.center} zoom={flyTarget.zoom} />}

            {/* Map interaction handler */}
            <MapInteractionLayer mode={planMode} onMapClick={handleMapClick} />

            {/* ── Heatmap layer ────────────────────────────────────── */}
            {showHeatmap && <HeatmapLayer points={heatmapPoints} />}

            {/* ── Threat rings ─────────────────────────────────────── */}
            {showThreats && shown.map(obj => {
              const nm = THREAT_NM[obj.kind]
              if (!nm) return null
              const color = SIDE_COLOR[obj.owner] ?? '#6b7280'
              return (
                <Circle
                  key={`threat-${obj.id}`}
                  center={[obj.lat, obj.lon]}
                  radius={nmToMeters(nm)}
                  pathOptions={{
                    color,
                    fillColor: color,
                    fillOpacity: 0.04,
                    weight: 1,
                    dashArray: '4 4',
                  }}
                />
              )
            })}

            {/* ── Objectives ───────────────────────────────────────── */}
            {shown.map(obj => {
              const color     = SIDE_COLOR[obj.owner] ?? '#6b7280'
              const r         = KIND_RADIUS[obj.kind] ?? KIND_RADIUS.default
              const isSelected = selectedId === obj.id
              const icon      = iconStyle !== 'dot'
                ? createMapIcon(obj.kind, obj.owner as 'Red' | 'Blue' | 'Neutral', iconStyle, isSelected)
                : null

              const popup = (
                <Popup minWidth={210} maxWidth={250}>
                  <div style={{ fontFamily: 'Segoe UI, system-ui, sans-serif' }}>
                    <div style={{ borderBottom: '1px solid #1e3a5f', paddingBottom: 8, marginBottom: 10 }}>
                      <div style={{ fontSize: 13, fontWeight: 700, color: '#e2e8f0' }}>
                        {KIND_SYMBOL[obj.kind] ?? '●'} {obj.name}
                      </div>
                      <div style={{ display: 'flex', alignItems: 'center', gap: 6, marginTop: 4 }}>
                        <span style={{ fontSize: 9, fontWeight: 700, letterSpacing: '0.1em', padding: '2px 6px', borderRadius: 3, border: '1px solid', color, borderColor: `${color}55`, background: `${color}18` }}>
                          {obj.owner.toUpperCase()}
                        </span>
                        <span style={{ fontSize: 10, color: '#64748b' }}>{obj.kind}</span>
                      </div>
                    </div>
                    <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '8px 12px', marginBottom: 10 }}>
                      {[
                        { label: 'Health', value: obj.health },
                        { label: 'Logistics', value: obj.logi },
                        { label: 'Supply', value: obj.supply },
                        { label: 'Fuel', value: obj.fuel },
                      ].map(s => {
                        const c = s.value >= 75 ? '#22c55e' : s.value >= 40 ? '#eab308' : '#ef4444'
                        return (
                          <div key={s.label}>
                            <div style={{ fontSize: 9, color: '#475569', textTransform: 'uppercase', letterSpacing: '0.08em', marginBottom: 3 }}>{s.label}</div>
                            <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                              <div style={{ flex: 1, height: 4, background: '#0d1117', borderRadius: 2, overflow: 'hidden' }}>
                                <div style={{ width: `${s.value}%`, height: '100%', background: c, borderRadius: 2 }} />
                              </div>
                              <span style={{ fontSize: 11, color: c, fontFamily: 'monospace', minWidth: 30, textAlign: 'right' }}>{s.value}%</span>
                            </div>
                          </div>
                        )
                      })}
                    </div>
                    <div style={{ fontSize: 9, color: '#374151', borderTop: '1px solid #1e3a5f', paddingTop: 6, display: 'flex', justifyContent: 'space-between' }}>
                      <span>{obj.lat.toFixed(4)}°N {obj.lon.toFixed(4)}°E</span>
                      <span>{new Date(obj.last_change).toLocaleTimeString()}</span>
                    </div>
                  </div>
                </Popup>
              )

              const nameLabel = (
                <Tooltip permanent direction="bottom" offset={[0, icon ? 14 : r + 4]} className="obj-label">
                  <span style={{
                    fontFamily: 'monospace', fontSize: 8, fontWeight: 700,
                    color, textShadow: '0 0 4px #000, 0 0 8px #000',
                    letterSpacing: '0.06em', textTransform: 'uppercase',
                  }}>
                    {obj.name}
                  </span>
                </Tooltip>
              )

              if (icon) {
                return (
                  <Marker key={obj.id} position={[obj.lat, obj.lon]} icon={icon}
                    eventHandlers={{ click: () => setSelectedId(isSelected ? null : obj.id) }}
                  >
                    {popup}
                    {nameLabel}
                  </Marker>
                )
              }

              return (
                <CircleMarker
                  key={obj.id}
                  center={[obj.lat, obj.lon]}
                  radius={isSelected ? r + 3 : r}
                  pathOptions={{ color, fillColor: color, fillOpacity: isSelected ? 0.9 : 0.65, weight: isSelected ? 3 : 2 }}
                  eventHandlers={{ click: () => setSelectedId(isSelected ? null : obj.id) }}
                >
                  {popup}
                  {nameLabel}
                </CircleMarker>
              )
            })}

            {/* ── Radar units (detected) ────────────────────────────── */}
            {showRadar && radarUnits.map(unit => {
              const color = unit.owner === 'Red' ? '#ef4444' : unit.owner === 'Blue' ? '#3b82f6' : '#6b7280'
              const isAir = isAirUnit(unit.tags)
              const cat = unitCategory(unit.tags)
              return (
                <Marker
                  key={`unit-${unit.id}`}
                  position={[unit.lat, unit.lon]}
                  icon={radarUnitIcon(unit)}
                >
                  <Popup minWidth={180} maxWidth={220}>
                    <div style={{ fontFamily: 'Segoe UI, system-ui, sans-serif' }}>
                      <div style={{ borderBottom: '1px solid #1e3a5f', paddingBottom: 6, marginBottom: 8 }}>
                        <div style={{ fontSize: 12, fontWeight: 700, color: '#e2e8f0' }}>{unit.typ}</div>
                        <div style={{ display: 'flex', alignItems: 'center', gap: 6, marginTop: 3 }}>
                          <span style={{ fontSize: 9, fontWeight: 700, letterSpacing: '0.1em', padding: '2px 6px', borderRadius: 3, border: '1px solid', color, borderColor: `${color}55`, background: `${color}18` }}>
                            {unit.owner.toUpperCase()}
                          </span>
                          <span style={{ fontSize: 9, color: '#64748b', fontFamily: 'monospace' }}>{cat}</span>
                        </div>
                      </div>
                      <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '4px 12px', fontSize: 10 }}>
                        {isAir && <>
                          <div style={{ color: '#475569' }}>ALT</div>
                          <div style={{ color: '#e2e8f0', fontFamily: 'monospace', textAlign: 'right' }}>{Math.round(unit.alt * 3.28084).toLocaleString()} ft</div>
                        </>}
                        <div style={{ color: '#475569' }}>HDG</div>
                        <div style={{ color: '#e2e8f0', fontFamily: 'monospace', textAlign: 'right' }}>{Math.round(unit.heading).toString().padStart(3, '0')}°</div>
                        <div style={{ color: '#475569' }}>SPD</div>
                        <div style={{ color: '#e2e8f0', fontFamily: 'monospace', textAlign: 'right' }}>{Math.round(unit.speed)} kts</div>
                        <div style={{ color: '#475569' }}>DET</div>
                        <div style={{ color: '#22c55e', fontFamily: 'monospace', textAlign: 'right', fontSize: 9 }}>{unit.detected_by.join(', ')}</div>
                      </div>
                      <div style={{ fontSize: 8, color: '#374151', borderTop: '1px solid #1e3a5f', paddingTop: 4, marginTop: 6, fontFamily: 'monospace' }}>
                        {unit.lat.toFixed(4)}°N {unit.lon.toFixed(4)}°E
                      </div>
                    </div>
                  </Popup>
                  <Tooltip direction="top" offset={[0, -12]} opacity={0.9}>
                    <span style={{ fontFamily: 'monospace', fontSize: 9, color }}>
                      {unit.typ}{isAir ? ` · ${Math.round(unit.alt * 3.28084).toLocaleString()}ft` : ''}
                    </span>
                  </Tooltip>
                </Marker>
              )
            })}

            {/* ── Route polyline ───────────────────────────────────── */}
            {routePositions.length >= 2 && (
              <>
                {/* Shadow line */}
                <Polyline
                  positions={routePositions}
                  pathOptions={{ color: '#000000', weight: 5, opacity: 0.4, dashArray: undefined }}
                />
                {/* Main route line */}
                <Polyline
                  positions={routePositions}
                  pathOptions={{ color: '#00b4f5', weight: 2, opacity: 0.9, dashArray: '8 4' }}
                />
                {/* Per-leg labels */}
                {waypoints.slice(0, -1).map((wp, i) => {
                  const next = waypoints[i + 1]!
                  const midLat = (wp.lat + next.lat) / 2
                  const midLon = (wp.lon + next.lon) / 2
                  const dist = haversineNm(wp.lat, wp.lon, next.lat, next.lon)
                  const brg  = trueBearing(wp.lat, wp.lon, next.lat, next.lon)
                  return (
                    <CircleMarker key={`leg-${i}`} center={[midLat, midLon]} radius={0} pathOptions={{ opacity: 0, fillOpacity: 0 }}>
                      <Tooltip permanent direction="top" offset={[0, -2]}>
                        <div style={{ fontFamily: 'monospace', fontSize: 9, color: '#00b4f5', background: 'transparent', border: 'none', textShadow: '0 0 4px #000' }}>
                          {dist.toFixed(1)}NM · {Math.round(brg).toString().padStart(3,'0')}°
                        </div>
                      </Tooltip>
                    </CircleMarker>
                  )
                })}
              </>
            )}

            {/* ── Waypoint markers ─────────────────────────────────── */}
            {waypoints.map((wp, i) => (
              <Marker
                key={wp.id}
                position={[wp.lat, wp.lon]}
                icon={waypointIcon(i + 1, i === waypoints.length - 1)}
              >
                <Tooltip direction="top" offset={[0, -14]} opacity={0.95}>
                  <span style={{ fontFamily: 'monospace', fontSize: 10 }}>
                    WP{i + 1} · {fmtCoord(wp.lat, wp.lon)}
                  </span>
                </Tooltip>
              </Marker>
            ))}

            {/* ── Planning markers ─────────────────────────────────── */}
            {planMarkers.map(m => (
              <Marker
                key={m.id}
                position={[m.lat, m.lon]}
                icon={planMarkerIcon(m.type)}
              >
                <Tooltip direction="top" offset={[0, -6]} opacity={0.95}>
                  <span style={{ fontFamily: 'monospace', fontSize: 10 }}>
                    {m.type} · {fmtCoord(m.lat, m.lon)}
                  </span>
                </Tooltip>
              </Marker>
            ))}

            {/* ── Measurement layer ────────────────────────────────── */}
            {measurePositions.length >= 2 && (
              <>
                <Polyline
                  positions={measurePositions}
                  pathOptions={{ color: '#a78bfa', weight: 2, opacity: 0.9, dashArray: '5 5' }}
                />
              </>
            )}
            {measurePts.map((p, i) => (
              <CircleMarker
                key={`meas-${i}`}
                center={[p.lat, p.lon]}
                radius={5}
                pathOptions={{ color: '#a78bfa', fillColor: '#a78bfa', fillOpacity: 0.7, weight: 2 }}
              />
            ))}

          </MapContainer>

          {/* ── Map HUD: legend ────────────────────────────────────── */}
          <div className="absolute bottom-8 right-3 z-[1000] text-[10px] space-y-1.5 backdrop-blur-sm"
            style={{ background: 'rgba(2,8,16,0.85)', border: '1px solid rgba(30,58,95,0.6)', borderRadius: 8, padding: '10px 12px' }}>
            <div className="text-slate-700 uppercase tracking-widest mb-2 text-[8px]">
              Legend · {iconStyle === 'nato' ? 'NATO APP-6' : iconStyle === 'russian' ? 'Russian' : 'Dot'}
            </div>
            {[
              { color: '#ef4444', label: 'Red Force' },
              { color: '#3b82f6', label: 'Blue Force' },
              { color: '#6b7280', label: 'Neutral' },
            ].map(l => (
              <div key={l.label} className="flex items-center gap-2">
                <span className="w-3 h-3 rounded-full border-2 flex-shrink-0" style={{ borderColor: l.color, background: `${l.color}40` }} />
                <span className="text-slate-500">{l.label}</span>
              </div>
            ))}
            {waypoints.length > 0 && (
              <div className="flex items-center gap-2 border-t border-[#1e3a5f]/40 pt-1.5 mt-1">
                <span className="w-3 h-1.5 rounded bg-[#00b4f5]" />
                <span className="text-slate-500">Route ({waypoints.length} WP)</span>
              </div>
            )}
            {showRadar && (
              <div className="flex items-center gap-2 border-t border-[#1e3a5f]/40 pt-1.5 mt-1">
                <svg width="12" height="12" viewBox="0 0 14 14"><polygon points="7,1 13,7 7,13 1,7" fill="#22c55e" fillOpacity="0.7" stroke="#22c55e" strokeWidth="1"/></svg>
                <span className="text-slate-500">Radar contact ({radarUnits.length})</span>
              </div>
            )}
            {showThreats && (
              <div className="flex items-center gap-2">
                <span className="w-3 h-3 rounded-full border border-dashed border-red-500/60 flex-shrink-0" />
                <span className="text-slate-500">Threat ring</span>
              </div>
            )}
            {showHeatmap && (
              <div className="flex items-center gap-2 border-t border-[#1e3a5f]/40 pt-1.5 mt-1">
                <span className="w-3 h-3 rounded" style={{ background: 'linear-gradient(135deg, #00f, #0ff, #0f0, #ff0, #f00)' }} />
                <span className="text-slate-500">Activity heatmap</span>
              </div>
            )}
          </div>

          {/* ── No data overlay ──────────────────────────────────── */}
          {!isLoading && validObjs.length === 0 && (
            <div className="absolute inset-0 flex flex-col items-center justify-center z-[500] pointer-events-none">
              <div className="text-center backdrop-blur-sm" style={{ background: 'rgba(2,8,16,0.8)', border: '1px solid rgba(30,58,95,0.6)', borderRadius: 12, padding: 32 }}>
                <div className="text-3xl mb-3 opacity-20">⬡</div>
                <div className="text-[11px] text-slate-500 tracking-wider uppercase">No objective coordinates</div>
                <div className="text-[10px] text-slate-700 mt-1">Connect to a live server to populate the map</div>
              </div>
            </div>
          )}
        </div>

        {/* ── Planning Panel ────────────────────────────────────────── */}
        {showPanel && (
          <PlanningPanel
            waypoints={waypoints}
            planMarkers={planMarkers}
            measurePts={measurePts}
            speed={speed}
            onSpeedChange={setSpeed}
            onRemoveWaypoint={id => setWaypoints(ws => ws.filter(w => w.id !== id))}
            onClearWaypoints={() => setWaypoints([])}
            onRemoveMarker={id => setPlanMarkers(ms => ms.filter(m => m.id !== id))}
            onClearMarkers={() => setPlanMarkers([])}
            showThreats={showThreats}
            onToggleThreats={() => setShowThreats(t => !t)}
          />
        )}
      </div>
    </div>
  )
}
