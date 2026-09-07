import { Fragment, useEffect, useMemo, useRef, useState, useCallback } from 'react'
import { useQuery } from '@tanstack/react-query'
import {
  MapContainer, TileLayer, Marker, Circle, Polyline, Popup,
  ScaleControl, useMap, useMapEvents,
} from 'react-leaflet'
import type { LatLngExpression, LatLngBoundsExpression } from 'leaflet'
import L from 'leaflet'
import geomagnetism from 'geomagnetism'
import {
  api,
  type Frontlines,
  type AirTrack, type GroundContact, type TacBullseye, type TacPicture,
} from '../api'
import { useTacFeed } from '../lib/useTacFeed'
import { createMapIcon, type Side } from '../lib/mapIcons'
import {
  tacSymbol, airSidc, groundSidc, bullSidc,
  airDisp, dispColor, fmtBearing, getCardinal,
  formatDMS, formatDDM, formatMGRS,
  COL_FRIEND, COL_HOSTILE, COL_ALT, COL_GS, COL_CLIMB,
} from '../lib/tacSymbols'
import { useRound } from '../context/RoundContext'
import { useAuth } from '../context/AuthContext'
import { useTheme } from '../context/ThemeContext'

// ── Constants ──────────────────────────────────────────────────────────
const OBJ_INT = 30_000
const DR_HZ = 8
const TRAIL_MAX = 40
const TRAIL_MS = 90_000
const SLOW_AIR_KT = 25          // peace-eye's slow-air cutoff

const GREEN = '#6aab1f'
const HUD_BG = 'rgba(8,11,6,0.92)'
const HUD_BORDER = 'rgba(106,171,31,0.25)'
const HUD_TEXT = '#8ec83f'
const HUD_DIM = 'rgba(142,200,63,0.45)'
const PANEL_BG = 'rgba(8,11,6,0.96)'
const FONT_MONO = "'JetBrains Mono','Share Tech Mono','Courier New',monospace"
const FONT_HEAD = "'Bebas Neue',sans-serif"
const COL_RED = '#cc4444'
const COL_BLUE = '#4a8fd4'
const COL_OBJ_NEUTRAL = '#6a7a5a'

const TACTICAL_TILE_DARK = 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}'
const TACTICAL_TILE_LIGHT = 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
const TILE_LAYERS = {
  tactical:  { label: 'TACMAP', url: TACTICAL_TILE_DARK, attr: 'Esri' },
  satellite: { label: 'SAT', url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', attr: 'Esri' },
  topo:      { label: 'TOPO', url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}', attr: 'Esri' },
} as const
type TileKey = keyof typeof TILE_LAYERS

const DCS_REGIONS: { label: string; center: [number, number]; zoom: number }[] = [
  { label: 'CAUCASUS', center: [42.35, 43.50], zoom: 8 },
  { label: 'PERSIA', center: [26.50, 55.50], zoom: 7 },
  { label: 'SYRIA', center: [35.00, 38.50], zoom: 8 },
  { label: 'NTTR', center: [37.00, -116.00], zoom: 8 },
  { label: 'MARIANAS', center: [14.50, 145.50], zoom: 8 },
  { label: 'NORMANDY', center: [49.00, -0.50], zoom: 8 },
  { label: 'KOLA', center: [69.00, 28.00], zoom: 7 },
  { label: 'SINAI', center: [29.00, 34.50], zoom: 8 },
]

// ── Geo helpers ────────────────────────────────────────────────────────
function haversineNm(a: { lat: number; lon: number }, b: { lat: number; lon: number }) {
  const R = 3440.065, φ1 = a.lat * Math.PI / 180, φ2 = b.lat * Math.PI / 180
  const Δφ = (b.lat - a.lat) * Math.PI / 180, Δλ = (b.lon - a.lon) * Math.PI / 180
  const x = Math.sin(Δφ / 2) ** 2 + Math.cos(φ1) * Math.cos(φ2) * Math.sin(Δλ / 2) ** 2
  return R * 2 * Math.atan2(Math.sqrt(x), Math.sqrt(1 - x))
}
function bearingTrue(a: { lat: number; lon: number }, b: { lat: number; lon: number }) {
  const φ1 = a.lat * Math.PI / 180, φ2 = b.lat * Math.PI / 180, Δλ = (b.lon - a.lon) * Math.PI / 180
  const y = Math.sin(Δλ) * Math.cos(φ2)
  const x = Math.cos(φ1) * Math.sin(φ2) - Math.sin(φ1) * Math.cos(φ2) * Math.cos(Δλ)
  return ((Math.atan2(y, x) * 180 / Math.PI) + 360) % 360
}
function projectPos(lat: number, lon: number, hdgDeg: number, distNm: number): [number, number] {
  const R = 3440.065, d = distNm / R, brg = hdgDeg * Math.PI / 180
  const φ1 = lat * Math.PI / 180, λ1 = lon * Math.PI / 180
  const φ2 = Math.asin(Math.sin(φ1) * Math.cos(d) + Math.cos(φ1) * Math.sin(d) * Math.cos(brg))
  const λ2 = λ1 + Math.atan2(Math.sin(brg) * Math.sin(d) * Math.cos(φ1), Math.cos(d) - Math.sin(φ1) * Math.sin(φ2))
  return [φ2 * 180 / Math.PI, λ2 * 180 / Math.PI]
}
const nmToM = (nm: number) => nm * 1852

// ── localStorage ───────────────────────────────────────────────────────
const PFX = 'bftac_'
function usePersisted<T>(k: string, fb: T): [T, React.Dispatch<React.SetStateAction<T>>] {
  const [s, set] = useState<T>(() => {
    try { const r = localStorage.getItem(PFX + k); return r ? JSON.parse(r) as T : fb } catch { return fb }
  })
  const r = useRef(s); r.current = s
  useEffect(() => { try { localStorage.setItem(PFX + k, JSON.stringify(r.current)) } catch { /* quota */ } }, [k, s])
  return [s, set]
}

// ── Per-contact settings (peace-eye objectSettings) ────────────────────
// Keyed by composite id ("A<n>" air / "G<n>" ground). wr/tr of 0 mean "use
// the automatic range for this unit", any other value is a manual override.
interface ObjSettings { wr: number; tr: number; watch: boolean }
type ObjInventory = Record<string, ObjSettings>
const NO_SETTINGS: ObjSettings = { wr: 0, tr: 0, watch: false }

/** Automatic warning / threat range (NM) for a ground contact — from the
 *  engine-identified emitter range, else a class default so SAM/AAA sites
 *  always get a ring. */
function groundAutoRange(c: GroundContact): { wr: number; tr: number } {
  const trM = c.threat_range_m ?? (c.class === 'airdefense' ? 12000 : 0)
  if (trM <= 0) return { wr: 0, tr: 0 }
  const tr = trM / 1852
  return { tr, wr: tr * 1.3 }
}

// ── Symbology helpers ──────────────────────────────────────────────────
const GROUND_TAG: Record<GroundContact['class'], string> = {
  armor: 'ARMOR', airdefense: 'ADS', artillery: 'ARTY', infantry: 'INF',
  airbase: 'AIRBASE', naval: 'NAVAL', unknown: 'UNK',
}
const AIR_SRC_TAG: Record<AirTrack['source'], string> = {
  groundradar: 'G', awacs: 'A', fused: 'GA', datalink: 'L16',
}

function airName(t: AirTrack): string {
  if (t.label) return t.label
  const disp = airDisp(t)
  return disp === 'hostile' ? 'HOSTILE' : disp === 'unknown' ? 'BOGEY' : 'FRIENDLY'
}

// Air marker: hollow framed symbol + heading tick + peace-eye data block
function airIcon(t: AirTrack, selected: boolean, showLabel: boolean): L.DivIcon {
  const disp = airDisp(t)
  const col = dispColor(disp)
  const { svg, w, h } = tacSymbol(airSidc(t), 18, col)
  const dim = t.stale ? 'opacity:0.45;' : ''
  const lead = 18
  const rad = (t.heading - 90) * Math.PI / 180
  const lx = w / 2 + Math.cos(rad) * lead, ly = h / 2 + Math.sin(rad) * lead
  const altKft = (t.alt_m * 3.28084) / 1000
  const climbKft = (t.vspd_ms * 196.85) / 1000   // m/s → kft/min
  const block = showLabel ? `
    <div style="position:absolute;left:${w + 5}px;top:-3px;white-space:nowrap;font-family:${FONT_MONO};line-height:1.35;pointer-events:none;${dim}">
      <div style="display:inline-block;border:1px solid ${col};background:rgba(71,85,105,0.5);color:#fff;font-size:10px;padding:0 2px;border-radius:2px">${airName(t)}</div>
      <div style="font-size:10px;text-shadow:0 0 3px #000">
        <span style="color:${COL_ALT}">${altKft.toFixed(1)}</span>
        <span style="color:${COL_GS};margin-left:5px">${Math.round(t.speed_kts)}</span>
        <span style="color:${COL_CLIMB};margin-left:5px">${climbKft.toFixed(1)}</span>
        <span style="color:${HUD_DIM};margin-left:5px">${AIR_SRC_TAG[t.source]}${t.jammed ? ' ✳' : ''}</span>
      </div>
    </div>` : ''
  return L.divIcon({
    html: `<div style="position:relative;display:inline-block;${dim}">
      <svg width="${w}" height="${h}" style="position:absolute;left:0;top:0;overflow:visible;pointer-events:none">
        <line x1="${w / 2}" y1="${h / 2}" x2="${lx}" y2="${ly}" stroke="${col}" stroke-width="1.5"/>
      </svg>
      ${selected ? `<div style="position:absolute;left:${w / 2 - h / 2 - 6}px;top:-6px;width:${h + 12}px;height:${h + 12}px;border:2px solid #fff;border-radius:50%;pointer-events:none"></div>` : ''}
      ${svg}
      ${block}
    </div>`,
    className: '', iconSize: [0, 0], iconAnchor: [Math.round(w / 2), Math.round(h / 2)],
  })
}

function groundIcon(c: GroundContact, selected: boolean): L.DivIcon {
  const { svg, w, h } = tacSymbol(groundSidc(c), 16, COL_HOSTILE)
  const faint = 0.4 + 0.6 * Math.max(0, Math.min(1, c.confidence))
  return L.divIcon({
    html: `<div style="position:relative;display:inline-block;opacity:${faint.toFixed(2)}">
      ${selected ? `<div style="position:absolute;left:${w / 2 - h / 2 - 6}px;top:-6px;width:${h + 12}px;height:${h + 12}px;border:2px solid #fff;border-radius:50%"></div>` : ''}
      ${svg}
      <div style="position:absolute;left:${w + 3}px;top:0;white-space:nowrap;font-family:${FONT_MONO};font-size:9px;color:${COL_HOSTILE};text-shadow:0 0 3px #000">
        ${c.count > 1 ? c.count + '×' : ''}${GROUND_TAG[c.class]}
      </div>
    </div>`,
    className: '', iconSize: [0, 0], iconAnchor: [Math.round(w / 2), Math.round(h / 2)],
  })
}

function bullMarkerIcon(b: TacBullseye): L.DivIcon {
  const { svg, w, h } = tacSymbol(bullSidc(b.side), 22, b.side === 'Red' ? COL_HOSTILE : COL_FRIEND)
  return L.divIcon({ html: svg, className: '', iconSize: [w, h], iconAnchor: [w / 2, h / 2] })
}

// ── Map sub-components ─────────────────────────────────────────────────
function FlyTo({ center, zoom }: { center: [number, number]; zoom: number }) {
  const map = useMap()
  useEffect(() => { map.flyTo(center, zoom, { duration: 1.0 }) }, [center, zoom, map])
  return null
}
function FitBounds({ pts }: { pts: [number, number][] }) {
  const map = useMap(), done = useRef(false)
  useEffect(() => {
    if (done.current || pts.length === 0) return
    done.current = true
    const lats = pts.map(p => p[0]), lons = pts.map(p => p[1])
    map.fitBounds([[Math.min(...lats) - .3, Math.min(...lons) - .3], [Math.max(...lats) + .3, Math.max(...lons) + .3]] as LatLngBoundsExpression, { padding: [40, 40], maxZoom: 9 })
  }, [pts, map])
  return null
}
function MapEvents({
  braaStart, onContext, onFinish, onMove,
}: {
  braaStart: { lat: number; lon: number } | null
  onContext: (lat: number, lon: number) => void
  onFinish: (lat: number, lon: number) => void
  onMove: (lat: number, lon: number) => void
}) {
  const map = useMap()
  useEffect(() => {
    const c = map.getContainer()
    c.style.cursor = braaStart ? 'crosshair' : ''
    return () => { c.style.cursor = '' }
  }, [braaStart, map])
  useMapEvents({
    click: e => { if (braaStart) onFinish(e.latlng.lat, e.latlng.lng) },
    contextmenu: e => { e.originalEvent.preventDefault(); onContext(e.latlng.lat, e.latlng.lng) },
    mousemove: e => onMove(e.latlng.lat, e.latlng.lng),
  })
  return null
}

function HudBtn({ active, onClick, children, title }: {
  active?: boolean; onClick: () => void; children: React.ReactNode; title?: string
}) {
  return (
    <button title={title} onClick={onClick} style={{
      fontFamily: FONT_HEAD, fontSize: '0.72rem', letterSpacing: '0.12em',
      padding: '0.28rem 0.6rem', borderRadius: '2px',
      border: active ? `1px solid ${GREEN}66` : `1px solid ${HUD_BORDER}`,
      background: active ? `${GREEN}18` : 'transparent',
      color: active ? GREEN : HUD_DIM, cursor: 'pointer', whiteSpace: 'nowrap',
    }}>{children}</button>
  )
}

// ── Trails ─────────────────────────────────────────────────────────────
interface TrailPt { lat: number; lon: number; ts: number }
type Trails = Map<number, TrailPt[]>

// ═══════════════════════════════════════════════════════════════════════
export default function MapPage() {
  const { theme } = useTheme()
  const { selectedRound } = useRound()
  const { user } = useAuth()

  const [tileKey, setTileKey] = usePersisted<TileKey>('tile', 'tactical')
  const [showAir, setShowAir] = usePersisted('air', true)
  const [showGround, setShowGround] = usePersisted('ground', true)
  const [showRings, setShowRings] = usePersisted('rings', true)
  const [showThreat, setShowThreat] = usePersisted('threat', true)
  const [showTrails, setShowTrails] = usePersisted('trails', true)
  const [showLabels, setShowLabels] = usePersisted('labels', true)
  const [showObjectives, setShowObjectives] = usePersisted('objs', true)
  const [showSlowAir, setShowSlowAir] = usePersisted('slowair', false)
  const [showCursor, setShowCursor] = usePersisted('cursor', false)
  const [magHeading, setMagHeading] = usePersisted('mag', true)
  const [iffFilter, setIffFilter] = usePersisted<'all' | 'friendly' | 'hostile'>('iff', 'all')
  const [panelOpen, setPanelOpen] = usePersisted('panel', true)
  const [panelTab, setPanelTab] = usePersisted<'search' | 'watches' | 'settings' | null>('ptab', null)
  const [objInv, setObjInv] = usePersisted<ObjInventory>('objinv', {})

  const [flyTarget, setFlyTarget] = useState<{ center: [number, number]; zoom: number } | null>(null)
  const [selId, setSelId] = useState<string | null>(null)
  const [search, setSearch] = useState('')
  const [cursor, setCursor] = useState<{ lat: number; lon: number } | null>(null)

  const [braaStart, setBraaStart] = useState<{ lat: number; lon: number } | null>(null)
  const [braaCursor, setBraaCursor] = useState<{ lat: number; lon: number } | null>(null)
  const [braaLines, setBraaLines] = useState<{ id: string; from: { lat: number; lon: number }; to: { lat: number; lon: number } }[]>([])

  // ── Feed ──────────────────────────────────────────────────────────
  const { picture, reason, status } = useTacFeed()
  const denied = !picture

  // ── Magnetic model (pinned — the published WMM epoch peace-eye ships
  //    expires; declination drifts <0.2°/yr so an old epoch is fine) ──
  const geomag = useMemo(() => {
    try { return geomagnetism.model(new Date(2024, 5, 1)) }
    catch { try { return geomagnetism.model() } catch { return null } }
  }, [])
  const declAt = useCallback((lat: number, lon: number): number => {
    if (!geomag) return 0
    try { return geomag.point([lat, lon]).decl } catch { return 0 }
  }, [geomag])
  /** Convert a true bearing at a point to the displayed (mag or true) bearing. */
  const dispBrg = useCallback((deg: number, at: { lat: number; lon: number }): number => {
    return magHeading ? (deg - declAt(at.lat, at.lon) + 360) % 360 : deg
  }, [magHeading, declAt])

  // Trails
  const trailsRef = useRef<Trails>(new Map())
  const [drTick, setDrTick] = useState(0)
  const picRef = useRef<TacPicture | null>(null)
  const picAtRef = useRef<number>(Date.now())

  useEffect(() => {
    if (!picture) return
    picRef.current = picture
    picAtRef.current = Date.now()
    const now = Date.now()
    const trails = trailsRef.current
    const live = new Set<number>()
    for (const t of picture.air) {
      live.add(t.id)
      const pts = trails.get(t.id) ?? []
      pts.push({ lat: t.lat, lon: t.lon, ts: now })
      const kept = pts.filter(p => p.ts >= now - TRAIL_MS)
      if (kept.length > TRAIL_MAX) kept.splice(0, kept.length - TRAIL_MAX)
      trails.set(t.id, kept)
    }
    for (const id of Array.from(trails.keys())) if (!live.has(id)) trails.delete(id)
  }, [picture])

  useEffect(() => {
    const iv = setInterval(() => setDrTick(t => (t + 1) % 100000), 1000 / DR_HZ)
    return () => clearInterval(iv)
  }, [])

  // Air tracks dead-reckoned forward, then filtered
  const airTracks: AirTrack[] = useMemo(() => {
    void drTick
    const pic = picRef.current
    if (!pic) return []
    const dtH = Math.min(6, (Date.now() - picAtRef.current) / 1000) / 3600
    let out = pic.air.map(t => {
      if (t.stale || t.speed_kts < 5) return t
      const [lat, lon] = projectPos(t.lat, t.lon, t.heading, t.speed_kts * dtH)
      return { ...t, lat, lon }
    })
    if (!showSlowAir) out = out.filter(t => t.speed_kts >= SLOW_AIR_KT || t.iff === 'friendly')
    if (iffFilter === 'friendly') out = out.filter(t => t.iff === 'friendly')
    if (iffFilter === 'hostile') out = out.filter(t => t.iff !== 'friendly')
    if (search.trim()) {
      const q = search.toLowerCase()
      out = out.filter(t => airName(t).toLowerCase().includes(q))
    }
    return out
  }, [drTick, iffFilter, search, showSlowAir])

  const groundContacts: GroundContact[] = useMemo(() => {
    if (!picture || iffFilter === 'friendly') return []
    let g = picture.ground
    if (search.trim()) {
      const q = search.toLowerCase()
      g = g.filter(c => GROUND_TAG[c.class].toLowerCase().includes(q))
    }
    return g
  }, [picture, iffFilter, search])

  // ── REST: objectives + frontline (public) ─────────────────────────
  const { data: objectives = [] } = useQuery({
    queryKey: ['objectives', selectedRound],
    queryFn: () => api.objectives(selectedRound),
    refetchInterval: OBJ_INT,
  })
  const { data: fronts = { mid: [], blue: [], red: [] } } = useQuery<Frontlines>({
    queryKey: ['frontline', selectedRound],
    queryFn: () => api.frontline(selectedRound),
    refetchInterval: OBJ_INT,
  })
  const validObjs = objectives.filter(o => o.lat !== 0 || o.lon !== 0)

  // ── Bullseye anchor for BRG/RNG ──────────────────────────────────
  const ownBull = useMemo(() => {
    const b = picture?.bullseye ?? []
    if (b.length === 1) return b[0]
    return b.find(x => picture?.side && x.side === picture.side) ?? b[0] ?? null
  }, [picture])

  const oobRows = useMemo(() => {
    const anchor = ownBull ? { lat: ownBull.lat, lon: ownBull.lon } : null
    return airTracks
      .map(t => ({
        t,
        brg: anchor ? dispBrg(bearingTrue(anchor, t), anchor) : 0,
        rng: anchor ? haversineNm(anchor, t) : 0,
      }))
      .sort((a, b) => a.rng - b.rng)
  }, [airTracks, ownBull, dispBrg])

  // ── Per-contact settings helpers (composite key: "A<n>" / "G<n>") ──
  const settingsFor = useCallback((key: string): ObjSettings => objInv[key] ?? NO_SETTINGS, [objInv])
  const setSettingsFor = useCallback((key: string, patch: Partial<ObjSettings>) => {
    setObjInv(prev => {
      const cur = prev[key] ?? NO_SETTINGS
      const next = { ...cur, ...patch }
      if (next.wr === 0 && next.tr === 0 && !next.watch) {
        const { [key]: _, ...rest } = prev
        return rest
      }
      return { ...prev, [key]: next }
    })
  }, [setObjInv])

  const watched = airTracks.filter(t => settingsFor(`A${t.id}`).watch)

  // ── Handlers ─────────────────────────────────────────────────────
  const rid = () => (crypto.randomUUID?.() ?? String(Math.random()))
  const handleContext = useCallback((lat: number, lon: number) => {
    if (braaStart) {
      setBraaLines(p => [...p, { id: rid(), from: braaStart, to: { lat, lon } }])
      setBraaStart(null); setBraaCursor(null)
    } else {
      setBraaStart({ lat, lon }); setBraaCursor({ lat, lon })
    }
  }, [braaStart])
  const handleFinish = useCallback((lat: number, lon: number) => {
    if (braaStart) {
      setBraaLines(p => [...p, { id: rid(), from: braaStart, to: { lat, lon } }])
      setBraaStart(null); setBraaCursor(null)
    }
  }, [braaStart])
  const handleMove = useCallback((lat: number, lon: number) => {
    setCursor({ lat, lon })
    if (braaStart) setBraaCursor({ lat, lon })
  }, [braaStart])

  const braaLive: [number, number][] = braaStart && braaCursor
    ? [[braaStart.lat, braaStart.lon], [braaCursor.lat, braaCursor.lon]] : []
  const braaLiveRng = braaStart && braaCursor ? haversineNm(braaStart, braaCursor) : null
  const braaLiveBrg = braaStart && braaCursor ? dispBrg(bearingTrue(braaStart, braaCursor), braaStart) : null

  const selectedTrack = airTracks.find(t => `A${t.id}` === selId) ?? null
  const selectedGround = groundContacts.find(c => `G${c.id}` === selId) ?? null

  const hudPanel: React.CSSProperties = {
    background: HUD_BG, border: `1px solid ${HUD_BORDER}`, borderRadius: '3px',
    backdropFilter: 'blur(8px)', padding: '6px 9px', color: HUD_TEXT, fontFamily: FONT_MONO,
  }

  // Cursor→bullseye readout (peace-eye CursorInfo)
  const cursorBull = useMemo(() => {
    if (!cursor || !ownBull) return null
    const a = { lat: ownBull.lat, lon: ownBull.lon }
    return `${fmtBearing(dispBrg(bearingTrue(a, cursor), a))} / ${Math.round(haversineNm(a, cursor))}`
  }, [cursor, ownBull, dispBrg])

  return (
    <div style={{ position: 'relative', flex: 1, overflow: 'hidden', display: 'flex' }}>
      {/* ── MAP ───────────────────────────────────────────────────── */}
      <div style={{ position: 'relative', flex: 1, overflow: 'hidden' }}>
        <MapContainer
          center={[42.35, 43.50]} zoom={7}
          style={{ position: 'absolute', inset: 0, background: theme === 'light' ? '#dfe0d8' : '#060a06' }}
          zoomControl={false} attributionControl={false}
        >
          <TileLayer
            key={`${tileKey}-${theme}`}
            url={tileKey === 'tactical' && theme === 'light' ? TACTICAL_TILE_LIGHT : TILE_LAYERS[tileKey].url}
            attribution={TILE_LAYERS[tileKey].attr} maxZoom={19}
            opacity={tileKey === 'tactical' ? 0.85 : 0.7} />
          <ScaleControl position="bottomleft" />
          <MapEvents braaStart={braaStart} onContext={handleContext} onFinish={handleFinish} onMove={handleMove} />
          {validObjs.length > 0 && <FitBounds pts={validObjs.map(o => [o.lat, o.lon] as [number, number])} />}
          {flyTarget && <FlyTo center={flyTarget.center} zoom={flyTarget.zoom} />}

          {/* Frontline */}
          {fronts.blue.map((l, i) => l.length > 1 && (
            <Polyline key={`fb${i}`} positions={l as LatLngExpression[]} pathOptions={{ color: '#2f7dff', weight: 2, opacity: 0.8, dashArray: '8 6' }} />
          ))}
          {fronts.red.map((l, i) => l.length > 1 && (
            <Polyline key={`fr${i}`} positions={l as LatLngExpression[]} pathOptions={{ color: '#ff3b3b', weight: 2, opacity: 0.8, dashArray: '8 6' }} />
          ))}
          {fronts.mid.map((l, i) => l.length > 1 && (
            <Polyline key={`fm${i}`} positions={l as LatLngExpression[]} pathOptions={{ color: '#fff', weight: 1.5, opacity: 0.9, dashArray: '2 5' }} />
          ))}

          {/* Objectives */}
          {showObjectives && validObjs.map(obj => {
            const side = (obj.owner === 'Red' || obj.owner === 'Blue' ? obj.owner : 'Neutral') as Side
            const icon = createMapIcon(obj.kind, side, 'nato', obj.health <= 0)
            const c = obj.owner === 'Red' ? COL_RED : obj.owner === 'Blue' ? COL_BLUE : COL_OBJ_NEUTRAL
            return (
              <Marker key={obj.id} position={[obj.lat, obj.lon]} icon={icon ?? L.divIcon({ html: `<div style="width:10px;height:10px;border-radius:50%;background:${c}"></div>`, className: '', iconSize: [10, 10] })}>
                <Popup minWidth={160}>
                  <div style={{ background: '#060a06', color: HUD_TEXT, fontFamily: FONT_MONO, fontSize: '0.7rem', padding: '2px 0' }}>
                    <div style={{ fontFamily: FONT_HEAD, letterSpacing: '0.1em', fontSize: '0.85rem' }}>{obj.name}</div>
                    <div style={{ color: HUD_DIM, marginTop: 2 }}>{obj.kind} · <span style={{ color: c }}>{obj.owner.toUpperCase()}</span></div>
                    {obj.owner !== 'Neutral' && <div style={{ color: HUD_DIM, marginTop: 2 }}>HP {obj.health}% · LOGI {obj.logi}%</div>}
                  </div>
                </Popup>
              </Marker>
            )
          })}

          {/* Friendly radar coverage rings */}
          {showRings && !denied && picture!.radar_rings.map((r, i) => (
            <Circle key={`ring${i}`} center={[r.lat, r.lon]} radius={r.range_m}
              pathOptions={{ color: r.airborne ? '#38bdf8' : GREEN, weight: 0.8, opacity: 0.45, fillColor: GREEN, fillOpacity: 0.02, dashArray: r.airborne ? '2 6' : '6 8' }}
              interactive={false} />
          ))}

          {/* Bullseye */}
          {!denied && picture!.bullseye.map(b => (
            <Fragment key={`bl${b.side}`}>
              <Marker position={[b.lat, b.lon]} icon={bullMarkerIcon(b)}>
                <Popup minWidth={130}>
                  <div style={{ background: '#060a06', color: HUD_TEXT, fontFamily: FONT_MONO, fontSize: '0.68rem' }}>
                    {b.side} bullseye<br />{formatDMS(b.lat, b.lon)}
                  </div>
                </Popup>
              </Marker>
            </Fragment>
          ))}

          {/* Air trails */}
          {showTrails && airTracks.map(t => {
            const pts = trailsRef.current.get(t.id)
            if (!pts || pts.length < 2) return null
            return (
              <Polyline key={`tr${t.id}`} positions={pts.map(p => [p.lat, p.lon] as LatLngExpression)}
                pathOptions={{ color: dispColor(airDisp(t)), weight: 1, opacity: 0.35 }} interactive={false} />
            )
          })}

          {/* Ground contacts: uncertainty + auto/override threat & warn rings */}
          {showGround && groundContacts.map(c => {
            const auto = groundAutoRange(c)
            const s = settingsFor(`G${c.id}`)
            const wr = s.wr || auto.wr
            const tr = s.tr || auto.tr
            return (
              <Fragment key={`g${c.id}`}>
                {c.uncertainty_m > 200 && (
                  <Circle center={[c.lat, c.lon]} radius={c.uncertainty_m}
                    pathOptions={{ color: COL_HOSTILE, weight: 0.6, opacity: 0.3, fillColor: COL_HOSTILE, fillOpacity: 0.03 }} interactive={false} />
                )}
                {showThreat && wr > 0 && (
                  <Circle center={[c.lat, c.lon]} radius={nmToM(wr)} interactive={false}
                    pathOptions={{ color: '#eab308', weight: 1, opacity: 0.55, fill: false, dashArray: '4 4' }} />
                )}
                {showThreat && tr > 0 && (
                  <Circle center={[c.lat, c.lon]} radius={nmToM(tr)} interactive={false}
                    pathOptions={{ color: '#ef4444', weight: 1.2, opacity: 0.8, fillColor: '#ef4444', fillOpacity: 0.05 }} />
                )}
                <Marker position={[c.lat, c.lon]} icon={groundIcon(c, selId === `G${c.id}`)}
                  eventHandlers={{ click: () => setSelId(`G${c.id}`) }} />
              </Fragment>
            )
          })}

          {/* Per-contact WR / TR rings on AIR tracks (manual only) */}
          {showThreat && airTracks.map(t => {
            const s = settingsFor(`A${t.id}`)
            return (
              <Fragment key={`rings${t.id}`}>
                {s.wr > 0 && <Circle center={[t.lat, t.lon]} radius={nmToM(s.wr)} interactive={false}
                  pathOptions={{ color: '#eab308', weight: 1, opacity: 0.7, fill: false, dashArray: '4 4' }} />}
                {s.tr > 0 && <Circle center={[t.lat, t.lon]} radius={nmToM(s.tr)} interactive={false}
                  pathOptions={{ color: '#ef4444', weight: 1, opacity: 0.8, fill: false }} />}
              </Fragment>
            )
          })}

          {/* Air tracks */}
          {showAir && airTracks.map(t => (
            <Marker key={`a${t.id}`} position={[t.lat, t.lon]} icon={airIcon(t, selId === `A${t.id}`, showLabels)}
              eventHandlers={{ click: () => setSelId(`A${t.id}`) }} />
          ))}

          {/* BRAA lines */}
          {braaLines.map(b => {
            const rng = haversineNm(b.from, b.to)
            const brg = dispBrg(bearingTrue(b.from, b.to), b.from)
            return (
              <Fragment key={b.id}>
                <Polyline positions={[[b.from.lat, b.from.lon], [b.to.lat, b.to.lon]]}
                  pathOptions={{ color: GREEN, weight: 1.5, dashArray: '5 5' }}
                  eventHandlers={{ click: () => setBraaLines(p => p.filter(x => x.id !== b.id)) }} />
                <Marker position={[b.to.lat, b.to.lon]} icon={L.divIcon({
                  html: `<div style="transform:translateX(6px);background:#94a3b8;color:#0b0f14;font-family:${FONT_MONO};font-size:12px;padding:1px 4px;white-space:nowrap">${fmtBearing(brg)} / ${rng.toFixed(0)}</div>`,
                  className: '', iconSize: [0, 0], iconAnchor: [0, 8],
                })} />
              </Fragment>
            )
          })}
          {braaLive.length === 2 && (
            <Polyline positions={braaLive} pathOptions={{ color: GREEN, weight: 1.5, dashArray: '3 4', opacity: 0.8 }} />
          )}
        </MapContainer>

        {/* ── TOP-LEFT HUD ──────────────────────────────────────────── */}
        <div style={{ position: 'absolute', top: 10, left: 10, zIndex: 500, display: 'flex', flexDirection: 'column', gap: 6, maxWidth: 'calc(100% - 20px)' }}>
          <div style={{ ...hudPanel, display: 'flex', gap: 5, alignItems: 'center', flexWrap: 'wrap' }}>
            <span style={{ fontFamily: FONT_HEAD, letterSpacing: '0.14em', color: HUD_TEXT, fontSize: '0.9rem', marginRight: 4 }}>TACMAP</span>
            <span style={{
              fontSize: '0.6rem', letterSpacing: '0.1em', padding: '2px 6px', borderRadius: 2,
              border: `1px solid ${status === 'open' && !denied ? GREEN : COL_HOSTILE}55`,
              color: status === 'open' && !denied ? GREEN : COL_HOSTILE,
            }}>
              {denied ? (reason === 'login' ? 'NOT LOGGED IN' : reason === 'nocoalition' ? 'NO COALITION' : 'NO PICTURE')
                : status === 'open' ? `${picture!.side ?? 'GOD'} · LIVE` : 'CONNECTING'}
            </span>
          </div>
          <div style={{ ...hudPanel, display: 'flex', gap: 4, flexWrap: 'wrap' }}>
            {(Object.keys(TILE_LAYERS) as TileKey[]).map(k => (
              <HudBtn key={k} active={tileKey === k} onClick={() => setTileKey(k)}>{TILE_LAYERS[k].label}</HudBtn>
            ))}
          </div>
          <div style={{ ...hudPanel, display: 'flex', gap: 4, flexWrap: 'wrap' }}>
            <HudBtn active={showAir} onClick={() => setShowAir(v => !v)}>AIR</HudBtn>
            <HudBtn active={showGround} onClick={() => setShowGround(v => !v)}>GND</HudBtn>
            <HudBtn active={showRings} onClick={() => setShowRings(v => !v)} title="Friendly radar coverage">RINGS</HudBtn>
            <HudBtn active={showThreat} onClick={() => setShowThreat(v => !v)} title="SAM threat / warning rings">THREAT</HudBtn>
            <HudBtn active={showTrails} onClick={() => setShowTrails(v => !v)}>TRAILS</HudBtn>
            <HudBtn active={showLabels} onClick={() => setShowLabels(v => !v)}>LABELS</HudBtn>
            <HudBtn active={showObjectives} onClick={() => setShowObjectives(v => !v)}>OBJ</HudBtn>
          </div>
          <div style={{ ...hudPanel, display: 'flex', gap: 4, flexWrap: 'wrap' }}>
            {(['all', 'friendly', 'hostile'] as const).map(f => (
              <HudBtn key={f} active={iffFilter === f} onClick={() => setIffFilter(f)}>{f.toUpperCase()}</HudBtn>
            ))}
            <HudBtn active={magHeading} onClick={() => setMagHeading(v => !v)} title="Magnetic vs true heading">{magHeading ? 'MAG' : 'TRUE'}</HudBtn>
          </div>
          <div style={{ ...hudPanel, display: 'flex', gap: 4, flexWrap: 'wrap' }}>
            {DCS_REGIONS.map(r => (
              <HudBtn key={r.label} onClick={() => setFlyTarget({ center: r.center, zoom: r.zoom })}>{r.label}</HudBtn>
            ))}
          </div>
          {(braaLiveRng != null || braaLines.length > 0) && (
            <div style={{ ...hudPanel, fontSize: '0.7rem' }}>
              {braaLiveRng != null
                ? <span>BRAA {fmtBearing(braaLiveBrg!)} / {braaLiveRng.toFixed(1)} — click to place</span>
                : <span>{braaLines.length} BRAA line{braaLines.length > 1 ? 's' : ''} · click to remove</span>}
            </div>
          )}
          <div style={{ ...hudPanel, fontSize: '0.62rem', color: HUD_DIM }}>right-drag = BRAA · bearings {magHeading ? 'magnetic' : 'true'}</div>
        </div>

        {/* ── CURSOR / BULLSEYE READOUT (bottom-right) ──────────────── */}
        {!denied && ownBull && cursor && (
          <div style={{
            position: 'absolute', right: 0, bottom: 0, zIndex: 500,
            background: 'rgba(64,64,64,0.25)', color: '#ca8a04', padding: '4px 8px',
            fontFamily: FONT_MONO,
          }}>
            {showCursor ? (
              <div style={{ display: 'flex', flexDirection: 'column', fontSize: '0.85rem', minWidth: 340 }}>
                <div style={{ display: 'flex', justifyContent: 'space-between' }}><span>DMS</span><span>{formatDMS(cursor.lat, cursor.lon)}</span></div>
                <div style={{ display: 'flex', justifyContent: 'space-between' }}><span>DDM</span><span>{formatDDM(cursor.lat, cursor.lon)}</span></div>
                <div style={{ display: 'flex', justifyContent: 'space-between' }}><span>MGRS</span><span>{formatMGRS(cursor.lat, cursor.lon)}</span></div>
                <div style={{ display: 'flex', justifyContent: 'space-between' }}><span>BULL</span><span>{cursorBull}</span></div>
              </div>
            ) : (
              <span style={{ fontSize: '1.05rem' }}>{cursorBull}</span>
            )}
          </div>
        )}

        {/* ── DENIED OVERLAY ────────────────────────────────────────── */}
        {denied && (
          <div style={{
            position: 'absolute', bottom: 16, left: '50%', transform: 'translateX(-50%)', zIndex: 500,
            ...hudPanel, maxWidth: 420, textAlign: 'center', fontSize: '0.72rem', color: HUD_TEXT,
          }}>
            {reason === 'nocoalition'
              ? 'You are signed in but the dashboard can’t resolve your coalition. Link Discord (-linkme in DCS chat) and register a side this campaign to see the tactical picture.'
              : 'Sign in and register a coalition on the server to see the live air & ground picture. Fog of war is enforced server-side — you only ever see what your side’s sensors detect.'}
          </div>
        )}
      </div>

      {/* ── SIDE PANEL (peace-eye ControlPanel + ObjectInfo) ───────── */}
      {!denied && (
        <>
          <button onClick={() => setPanelOpen(v => !v)} style={{
            position: 'absolute', top: 10, right: panelOpen ? 282 : 10, zIndex: 600,
            ...hudPanel, cursor: 'pointer', fontFamily: FONT_HEAD, letterSpacing: '0.1em', fontSize: '0.7rem',
          }}>{panelOpen ? 'PANEL ›' : '‹ PANEL'}</button>
          {panelOpen && (
            <div style={{
              width: 280, flexShrink: 0, background: PANEL_BG, borderLeft: `1px solid ${HUD_BORDER}`,
              display: 'flex', flexDirection: 'column', color: HUD_TEXT, fontFamily: FONT_MONO, overflow: 'hidden',
            }}>
              {/* tab bar */}
              <div style={{ display: 'flex', gap: 6, padding: '8px', borderBottom: `1px solid ${HUD_BORDER}` }}>
                {(['search', 'watches', 'settings'] as const).map(t => (
                  <HudBtn key={t} active={panelTab === t} onClick={() => setPanelTab(p => p === t ? null : t)}>
                    {t === 'search' ? 'SEARCH' : t === 'watches' ? `WATCH ${watched.length || ''}` : 'CFG'}
                  </HudBtn>
                ))}
              </div>

              {panelTab === 'search' && (
                <div style={{ padding: 8, borderBottom: `1px solid ${HUD_BORDER}` }}>
                  <input value={search} onChange={e => setSearch(e.target.value)} placeholder="filter contacts…"
                    style={{ width: '100%', boxSizing: 'border-box', background: 'rgba(0,20,0,0.5)', border: `1px solid ${HUD_BORDER}`, color: GREEN, fontFamily: FONT_MONO, fontSize: '0.66rem', padding: '4px 7px', outline: 'none' }} />
                </div>
              )}

              {panelTab === 'settings' && (
                <div style={{ padding: 10, borderBottom: `1px solid ${HUD_BORDER}`, display: 'flex', flexDirection: 'column', gap: 8, fontSize: '0.66rem' }}>
                  {([
                    ['Magnetic heading', magHeading, () => setMagHeading(v => !v)],
                    ['Show slow / parked air', showSlowAir, () => setShowSlowAir(v => !v)],
                    ['Cursor coord readout', showCursor, () => setShowCursor(v => !v)],
                    ['Ground picture', showGround, () => setShowGround(v => !v)],
                    ['Friendly radar coverage rings', showRings, () => setShowRings(v => !v)],
                    ['SAM threat / warning rings', showThreat, () => setShowThreat(v => !v)],
                  ] as const).map(([label, val, fn]) => (
                    <label key={label} style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', cursor: 'pointer' }}>
                      <span style={{ color: HUD_DIM }}>{label}</span>
                      <input type="checkbox" checked={val} onChange={fn} />
                    </label>
                  ))}
                </div>
              )}

              {/* OOB table (always visible) */}
              <div style={{ padding: '6px 8px', borderBottom: `1px solid ${HUD_BORDER}`, fontSize: '0.58rem', color: HUD_DIM, letterSpacing: '0.08em' }}>
                {airTracks.length} AIR · {groundContacts.length} GND{ownBull ? ` · BRG/RNG from BULL (${magHeading ? 'M' : 'T'})` : ''}
              </div>
              <div style={{ flex: 1, overflowY: 'auto' }}>
                {panelTab === 'watches' ? (
                  watched.length === 0
                    ? <div style={{ padding: 16, color: HUD_DIM, fontSize: '0.62rem', textAlign: 'center' }}>No watched contacts</div>
                    : watched.map(t => (
                      <div key={t.id} onClick={() => { setSelId(`A${t.id}`); setFlyTarget({ center: [t.lat, t.lon], zoom: 9 }) }}
                        style={{ display: 'flex', alignItems: 'center', gap: 6, padding: '5px 8px', cursor: 'pointer', borderBottom: '1px solid rgba(255,255,255,0.04)', color: dispColor(airDisp(t)) }}>
                        <span dangerouslySetInnerHTML={{ __html: tacSymbol(airSidc(t), 14, dispColor(airDisp(t))).svg }} />
                        <span style={{ fontSize: '0.66rem' }}>{airName(t)}</span>
                      </div>
                    ))
                ) : (
                  <table style={{ width: '100%', borderCollapse: 'collapse', fontSize: '0.62rem' }}>
                    <thead>
                      <tr style={{ color: HUD_DIM, textAlign: 'left' }}>
                        <th style={{ padding: '4px 6px' }}>ID</th><th>BRG</th><th>NM</th><th>ALT</th><th>GS</th><th>AGE</th>
                      </tr>
                    </thead>
                    <tbody>
                      {oobRows.length === 0 && (
                        <tr><td colSpan={6} style={{ padding: '14px 6px', color: HUD_DIM, textAlign: 'center' }}>No contacts in picture</td></tr>
                      )}
                      {oobRows.map(({ t, brg, rng }) => (
                        <tr key={t.id} onClick={() => { setSelId(`A${t.id}`); setFlyTarget({ center: [t.lat, t.lon], zoom: 9 }) }}
                          style={{
                            cursor: 'pointer', borderBottom: '1px solid rgba(255,255,255,0.04)',
                            background: selId === `A${t.id}` ? 'rgba(106,171,31,0.12)' : 'transparent',
                            opacity: t.stale ? 0.5 : 1,
                          }}>
                          <td style={{ padding: '4px 6px', color: dispColor(airDisp(t)), whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis', maxWidth: 78 }}>
                            {settingsFor(`A${t.id}`).watch ? '★ ' : ''}{airName(t)}
                          </td>
                          <td>{ownBull ? fmtBearing(brg).replace(/[A-Z]+$/, '') : '—'}</td>
                          <td>{ownBull ? rng.toFixed(0) : '—'}</td>
                          <td>{Math.round(t.alt_m * 3.28084 / 100)}</td>
                          <td>{Math.round(t.speed_kts)}</td>
                          <td>{t.age_s}{t.jammed ? 'J' : ''}</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                )}
              </div>

              {/* ── ObjectInfo card (peace-eye) ──────────────────────── */}
              {(selectedTrack || selectedGround) && (
                <div style={{ borderTop: `1px solid ${HUD_BORDER}`, padding: '9px 10px', fontSize: '0.64rem', maxHeight: '46%', overflowY: 'auto' }}>
                  {selectedTrack && (() => {
                    const t = selectedTrack
                    const s = settingsFor(`A${t.id}`)
                    const at = { lat: t.lat, lon: t.lon }
                    const hdg = Math.round(dispBrg(t.heading, at))
                    return (
                      <>
                        <div style={{ display: 'flex', alignItems: 'baseline', gap: 6 }}>
                          <span style={{ color: dispColor(airDisp(t)), fontFamily: FONT_HEAD, letterSpacing: '0.1em', fontSize: '0.85rem' }}>{airName(t)}</span>
                          <span style={{ color: HUD_DIM }}>{t.class.toUpperCase()}</span>
                        </div>
                        <div style={{ display: 'grid', gridTemplateColumns: 'auto 1fr', gap: '2px 10px', color: HUD_DIM, margin: '5px 0' }}>
                          <span>Heading</span><span>{hdg.toString().padStart(3, '0')}{getCardinal(hdg)}</span>
                          <span>Altitude</span><span>{Math.round(t.alt_m * 3.28084)} ft</span>
                          <span>GS</span><span>{Math.round(t.speed_kts)} kt</span>
                          <span>Source</span><span>{t.source.toUpperCase()} · {t.age_s}s{t.stale ? ' (stale)' : ''}{t.jammed ? ' · JAMMED' : ''}</span>
                        </div>
                        <button onClick={() => setFlyTarget({ center: [t.lat, t.lon], zoom: 10 })} style={{
                          width: '100%', marginBottom: 6, padding: '4px', background: `${GREEN}18`, border: `1px solid ${GREEN}55`,
                          color: GREEN, fontFamily: FONT_HEAD, letterSpacing: '0.1em', fontSize: '0.66rem', cursor: 'pointer',
                        }}>CENTER</button>
                        <div style={{ display: 'flex', gap: 6, marginBottom: 6 }}>
                          <label style={{ flex: 1, display: 'flex', alignItems: 'center', gap: 4 }}>
                            <span style={{ background: '#eab308', color: '#000', padding: '1px 4px', fontSize: '0.58rem' }}>WR</span>
                            <input type="number" min={0} value={s.wr || ''} placeholder="0" onChange={e => setSettingsFor(`A${t.id}`, { wr: Math.max(0, +e.target.value) })}
                              style={{ width: '100%', boxSizing: 'border-box', background: 'rgba(0,20,0,0.5)', border: `1px solid ${HUD_BORDER}`, color: GREEN, fontFamily: FONT_MONO, fontSize: '0.62rem', padding: '2px 4px' }} />
                          </label>
                          <label style={{ flex: 1, display: 'flex', alignItems: 'center', gap: 4 }}>
                            <span style={{ background: '#ef4444', color: '#000', padding: '1px 4px', fontSize: '0.58rem' }}>TR</span>
                            <input type="number" min={0} value={s.tr || ''} placeholder="0" onChange={e => setSettingsFor(`A${t.id}`, { tr: Math.max(0, +e.target.value) })}
                              style={{ width: '100%', boxSizing: 'border-box', background: 'rgba(0,20,0,0.5)', border: `1px solid ${HUD_BORDER}`, color: GREEN, fontFamily: FONT_MONO, fontSize: '0.62rem', padding: '2px 4px' }} />
                          </label>
                        </div>
                        <label style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 6, cursor: 'pointer' }}>
                          <span style={{ color: HUD_DIM }}>Watch</span>
                          <input type="checkbox" checked={s.watch} onChange={e => setSettingsFor(`A${t.id}`, { watch: e.target.checked })} />
                        </label>
                        <div style={{ display: 'grid', gridTemplateColumns: 'auto 1fr', gap: '1px 10px' }}>
                          <span style={{ color: HUD_DIM }}>DMS</span><span>{formatDMS(t.lat, t.lon)}</span>
                          <span style={{ color: HUD_DIM }}>DDM</span><span>{formatDDM(t.lat, t.lon)}</span>
                          <span style={{ color: HUD_DIM }}>MGRS</span><span>{formatMGRS(t.lat, t.lon)}</span>
                          {ownBull && <>
                            <span style={{ color: HUD_DIM }}>Bullseye</span>
                            <span>{fmtBearing(dispBrg(bearingTrue({ lat: ownBull.lat, lon: ownBull.lon }, at), { lat: ownBull.lat, lon: ownBull.lon }))} / {Math.round(haversineNm({ lat: ownBull.lat, lon: ownBull.lon }, at))}</span>
                          </>}
                        </div>
                      </>
                    )
                  })()}
                  {selectedGround && (() => {
                    const c = selectedGround
                    const auto = groundAutoRange(c)
                    const s = settingsFor(`G${c.id}`)
                    return (
                      <>
                        <div style={{ color: COL_HOSTILE, fontFamily: FONT_HEAD, letterSpacing: '0.1em', fontSize: '0.85rem' }}>
                          {c.count}× {GROUND_TAG[c.class]}
                        </div>
                        <div style={{ display: 'grid', gridTemplateColumns: 'auto 1fr', gap: '2px 10px', color: HUD_DIM, margin: '5px 0' }}>
                          <span>Confidence</span><span>{(c.confidence * 100).toFixed(0)}%</span>
                          <span>Source</span><span>{c.source.toUpperCase()} · {c.age_s}s</span>
                          <span>Uncertainty</span><span>±{Math.round(c.uncertainty_m)} m</span>
                          {auto.tr > 0 && <><span>Threat</span><span>~{auto.tr.toFixed(0)} NM {c.threat_range_m ? '(identified)' : '(est.)'}</span></>}
                        </div>
                        <button onClick={() => setFlyTarget({ center: [c.lat, c.lon], zoom: 10 })} style={{
                          width: '100%', marginBottom: 6, padding: '4px', background: `${GREEN}18`, border: `1px solid ${GREEN}55`,
                          color: GREEN, fontFamily: FONT_HEAD, letterSpacing: '0.1em', fontSize: '0.66rem', cursor: 'pointer',
                        }}>CENTER</button>
                        {(auto.tr > 0 || c.class === 'airdefense') && (
                          <div style={{ display: 'flex', gap: 6, marginBottom: 6 }}>
                            <label style={{ flex: 1, display: 'flex', alignItems: 'center', gap: 4 }}>
                              <span style={{ background: '#eab308', color: '#000', padding: '1px 4px', fontSize: '0.58rem' }}>WR</span>
                              <input type="number" min={0} value={s.wr || ''} placeholder={auto.wr ? auto.wr.toFixed(0) : '0'}
                                onChange={e => setSettingsFor(`G${c.id}`, { wr: Math.max(0, +e.target.value) })}
                                style={{ width: '100%', boxSizing: 'border-box', background: 'rgba(0,20,0,0.5)', border: `1px solid ${HUD_BORDER}`, color: GREEN, fontFamily: FONT_MONO, fontSize: '0.62rem', padding: '2px 4px' }} />
                            </label>
                            <label style={{ flex: 1, display: 'flex', alignItems: 'center', gap: 4 }}>
                              <span style={{ background: '#ef4444', color: '#000', padding: '1px 4px', fontSize: '0.58rem' }}>TR</span>
                              <input type="number" min={0} value={s.tr || ''} placeholder={auto.tr ? auto.tr.toFixed(0) : '0'}
                                onChange={e => setSettingsFor(`G${c.id}`, { tr: Math.max(0, +e.target.value) })}
                                style={{ width: '100%', boxSizing: 'border-box', background: 'rgba(0,20,0,0.5)', border: `1px solid ${HUD_BORDER}`, color: GREEN, fontFamily: FONT_MONO, fontSize: '0.62rem', padding: '2px 4px' }} />
                            </label>
                          </div>
                        )}
                        <div style={{ display: 'grid', gridTemplateColumns: 'auto 1fr', gap: '1px 10px' }}>
                          <span style={{ color: HUD_DIM }}>DMS</span><span>{formatDMS(c.lat, c.lon)}</span>
                          <span style={{ color: HUD_DIM }}>MGRS</span><span>{formatMGRS(c.lat, c.lon)}</span>
                        </div>
                      </>
                    )
                  })()}
                </div>
              )}

              <div style={{ padding: '6px 10px', borderTop: `1px solid ${HUD_BORDER}`, fontSize: '0.55rem', color: HUD_DIM, lineHeight: 1.5 }}>
                {picture!.side ?? 'god'} picture — EWR / AWACS / JTAC / recon only.
                {user?.is_admin ? ' Admin: ?side=blue|red locks a coalition.' : ''}
              </div>
            </div>
          )}
        </>
      )}
    </div>
  )
}
