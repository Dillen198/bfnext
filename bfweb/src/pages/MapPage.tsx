import { useEffect, useRef, useState, useCallback, useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import {
  MapContainer, TileLayer, CircleMarker, Marker, Popup,
  useMap, ScaleControl, Polyline, Circle, useMapEvents,
} from 'react-leaflet'
import type { LatLngBoundsExpression, LatLngExpression } from 'leaflet'
import L from 'leaflet'
import 'leaflet.heat'
// @ts-ignore – milsymbol v3 ships its own types but declaration may be missing
import ms from 'milsymbol'
import {
  api, connectLiveUnits,
  type Objective, type MapUnit, type LiveUnit, type WsUnitsMsg, type Bullseye,
} from '../api'
import { type IconStyle } from '../lib/mapIcons'
import { useRound } from '../context/RoundContext'

// ── Constants ──────────────────────────────────────────────────────────
const TRAIL_MAX  = 12
const TRAIL_SEC  = 30
const VEL_SECS   = 60      // project position this many seconds ahead
const OBJ_INT    = 30_000
const REST_INT   = 5_000

// ── Colors ─────────────────────────────────────────────────────────────
// Sneaker palette: enemies=cyan, allies=red/pink, watches=yellow
const COL_ENEMY   = '#17c2f6'   // red coalition  (Sneaker: cyan)
const COL_ALLY    = '#ff8080'   // blue coalition  (Sneaker: red/pink)
const COL_NEUTRAL = '#888888'
const COL_WATCH   = '#ffd600'   // watched tracks
const GREEN  = '#39ff14'
const HUD_BG     = 'rgba(4,10,4,0.82)'
const HUD_BORDER = 'rgba(57,255,20,0.22)'
const HUD_TEXT   = '#39ff14'
const HUD_DIM    = 'rgba(57,255,20,0.45)'
const PANEL_BG   = 'rgba(6,12,6,0.90)'
const FONT_MONO  = "'Share Tech Mono','Courier New',monospace"
const FONT_HEAD  = "'Bebas Neue',sans-serif"

function unitColor(coa: number, watched: boolean) {
  if (watched) return COL_WATCH
  return coa === 1 ? COL_ENEMY : coa === 2 ? COL_ALLY : COL_NEUTRAL
}
function restUnitColor(owner: string, watched: boolean) {
  if (watched) return COL_WATCH
  return owner === 'Red' ? COL_ENEMY : owner === 'Blue' ? COL_ALLY : COL_NEUTRAL
}

// ── Tile layers ────────────────────────────────────────────────────────
const TILE_LAYERS = {
  tactical:  { label: 'TACMAP',  url: 'https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png',                                                                attr: '© CARTO' },
  satellite: { label: 'SAT',     url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',                                 attr: 'Esri' },
  topo:      { label: 'TOPO',    url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',                                 attr: 'Esri' },
} as const
type TileKey = keyof typeof TILE_LAYERS

// ── DCS region presets ─────────────────────────────────────────────────
const DCS_REGIONS: { label: string; center: [number, number]; zoom: number }[] = [
  { label: 'CAUCASUS',  center: [42.35, 43.50],   zoom: 8 },
  { label: 'PERSIA',    center: [26.50, 55.50],   zoom: 7 },
  { label: 'SYRIA',     center: [35.00, 38.50],   zoom: 8 },
  { label: 'NTTR',      center: [37.00,-116.00],  zoom: 8 },
  { label: 'MARIANAS',  center: [14.50, 145.50],  zoom: 8 },
  { label: 'NORMANDY',  center: [49.00,  -0.50],  zoom: 8 },
  { label: 'KOLA',      center: [69.00,  28.00],  zoom: 7 },
  { label: 'SINAI',     center: [29.00,  34.50],  zoom: 8 },
]

// ── Planning types ─────────────────────────────────────────────────────
type PlanMode   = 'none' | 'waypoint' | 'marker' | 'measure'
type MarkerType = 'IP' | 'TGT' | 'CP' | 'BULL' | 'EGR'
interface Waypoint   { id: string; lat: number; lon: number }
interface PlanMarker { id: string; lat: number; lon: number; type: MarkerType }

// ── BRAA lines ─────────────────────────────────────────────────────────
interface BraaLine { id: string; from: { lat: number; lon: number }; to: { lat: number; lon: number } }

// ── Hack timer ─────────────────────────────────────────────────────────
interface HackTimer { id: string; label: string; startedAt: number }  // startedAt = Date.now() ms

// ── Trail storage ──────────────────────────────────────────────────────
interface TrailPt { lat: number; lon: number; ts: number }
type Trails = Map<string, TrailPt[]>

// ── Geo helpers ────────────────────────────────────────────────────────
function haversineNm(a: {lat:number;lon:number}, b: {lat:number;lon:number}) {
  const R=3440.065, φ1=a.lat*Math.PI/180, φ2=b.lat*Math.PI/180
  const Δφ=(b.lat-a.lat)*Math.PI/180, Δλ=(b.lon-a.lon)*Math.PI/180
  const x=Math.sin(Δφ/2)**2+Math.cos(φ1)*Math.cos(φ2)*Math.sin(Δλ/2)**2
  return R*2*Math.atan2(Math.sqrt(x),Math.sqrt(1-x))
}
function bearingDeg(a: {lat:number;lon:number}, b: {lat:number;lon:number}) {
  const φ1=a.lat*Math.PI/180, φ2=b.lat*Math.PI/180, Δλ=(b.lon-a.lon)*Math.PI/180
  const y=Math.sin(Δλ)*Math.cos(φ2)
  const x=Math.cos(φ1)*Math.sin(φ2)-Math.sin(φ1)*Math.cos(φ2)*Math.cos(Δλ)
  return ((Math.atan2(y,x)*180/Math.PI)+360)%360
}
function projectPos(lat: number, lon: number, hdgDeg: number, distNm: number): [number, number] {
  const R=3440.065, d=distNm/R, brg=hdgDeg*Math.PI/180
  const φ1=lat*Math.PI/180, λ1=lon*Math.PI/180
  const φ2=Math.asin(Math.sin(φ1)*Math.cos(d)+Math.cos(φ1)*Math.sin(d)*Math.cos(brg))
  const λ2=λ1+Math.atan2(Math.sin(brg)*Math.sin(d)*Math.cos(φ1),Math.cos(d)-Math.sin(φ1)*Math.sin(φ2))
  return [φ2*180/Math.PI, λ2*180/Math.PI]
}
function nmToM(nm: number) { return nm*1852 }
function fmtCoord(lat: number, lon: number) {
  return `${Math.abs(lat).toFixed(4)}°${lat>=0?'N':'S'} ${Math.abs(lon).toFixed(4)}°${lon>=0?'E':'W'}`
}
function fmtBrg(deg: number) { return Math.round(deg).toString().padStart(3,'0') + '°' }
function fmtAlt(m: number)  { const ft=Math.round(m*3.28084/100)*100; return ft>=1000?`${(ft/1000).toFixed(1)}K`:`${ft}` }
function fmtSpd(kts: number){ return Math.round(kts).toString() }
function fmtTime(min: number){ const h=Math.floor(min/60),m=Math.round(min%60); return h?`${h}h${m}m`:`${m}m` }
function fmtDcsTime(secs: number) {
  const h=Math.floor(secs/3600),m=Math.floor((secs%3600)/60),s=Math.floor(secs%60)
  return `${h.toString().padStart(2,'0')}:${m.toString().padStart(2,'0')}:${s.toString().padStart(2,'0')}`
}
function fmtElapsed(ms: number) {
  const s=Math.floor(ms/1000), m=Math.floor(s/60), sec=s%60
  return `${m.toString().padStart(2,'0')}:${sec.toString().padStart(2,'0')}`
}

// ── localStorage helpers ───────────────────────────────────────────────
const PFX = 'bfmap_'
function loadS<T>(k: string, fb: T): T { try { const r=localStorage.getItem(PFX+k); return r?JSON.parse(r) as T:fb } catch { return fb } }
function saveS<T>(k: string, v: T) { try { localStorage.setItem(PFX+k,JSON.stringify(v)) } catch { /* quota */ } }
function usePersisted<T>(k: string, fb: T): [T, React.Dispatch<React.SetStateAction<T>>] {
  const [s,set] = useState<T>(()=>loadS(k,fb))
  const r = useRef(s); r.current=s
  useEffect(()=>saveS(k,r.current),[k,s])
  return [s,set]
}

// ── milsymbol SIDC factory ─────────────────────────────────────────────
// Standard identity: F=Friendly, H=Hostile
// cat 1=Plane, 2=Helo, 3=Ground, 4=Ship
function getSIDC(coa: number, cat: number): string {
  const si = coa === 2 ? 'F' : 'H'
  if (cat === 1) return `S${si}AP----------`
  if (cat === 2) return `S${si}AH----------`
  if (cat === 4) return `S${si}SP----------`
  return         `S${si}GP----------`
}

// ── Sneaker-style unit icon with persistent labels ─────────────────────
// Layout (matches Sneaker):
//   [NATO symbol]  CALLSIGN          ← coalition color
//                  ↑ALT              ← pink  #FFC0CB
//                  SPDkts            ← orange
interface UnitIconOpts {
  coa:     number    // 1=Red/hostile 2=Blue/friendly
  cat:     number    // 1=plane 2=helo 3=ground 4=ship
  name:    string    // callsign / unit name
  alt:     number    // metres
  spd:     number    // knots
  vspd?:   number    // m/s vertical speed (optional)
  watched: boolean
  symSize?: number
}

function sneakerUnitIcon(opts: UnitIconOpts): L.DivIcon {
  const { coa, cat, name, alt, spd, vspd, watched, symSize = 26 } = opts
  const sidc = getSIDC(coa, cat)
  const namCol  = watched ? COL_WATCH : (coa === 1 ? COL_ENEMY : COL_ALLY)
  const fillCol = namCol
  const altCol  = '#FFC0CB'                        // pink – Sneaker
  const spdCol  = '#f97316'                        // orange – Sneaker
  const vCol    = '#6EE7B7'                        // mint – Sneaker
  const shadow  = `text-shadow:0 0 4px #000,0 0 8px #000`

  // vertical speed arrow + value
  const vspdStr = vspd !== undefined && Math.abs(vspd) > 0.5
    ? `<div style="color:${vCol};font-size:9px;line-height:1.3;${shadow}">${vspd>0?'↑':'↓'}${Math.round(Math.abs(vspd)*196.85)}fpm</div>`
    : ''

  // altitude – show as FL if high enough
  const altFt   = alt * 3.28084
  const altStr  = altFt >= 18000
    ? `FL${Math.round(altFt/100).toString().padStart(3,'0')}`
    : `${Math.round(altFt/100)*100}ft`

  let symSvg = ''
  let symW = symSize, symH = symSize
  try {
    const sym = new ms.Symbol(sidc, {
      size:        symSize,
      fillColor:   fillCol,
      strokeWidth: 0.4,
      iconColor:   '#ffffff',
    })
    symSvg = sym.asSVG() as string
    const sz = sym.getSize() as { width:number; height:number }
    symW = sz.width; symH = sz.height
  } catch {
    symSvg = `<svg width="${symSize}" height="${symSize}" viewBox="0 0 20 20">
      <polygon points="10,2 17,17 10,12 3,17" fill="${fillCol}" stroke="#000" stroke-width="0.5"/>
    </svg>`
    symW = symSize; symH = symSize
  }

  const labelHtml = cat <= 2
    ? `<div style="position:absolute;left:${symW+4}px;top:0;white-space:nowrap;font-family:'Share Tech Mono','Courier New',monospace;line-height:1.35;pointer-events:none;">
        <div style="color:${namCol};font-size:10px;font-weight:700;${shadow}">${name||'UNKNOWN'}</div>
        <div style="color:${altCol};font-size:9px;${shadow}">${altStr}</div>
        <div style="color:${spdCol};font-size:9px;${shadow}">${Math.round(spd)}kts</div>
        ${vspdStr}
      </div>`
    : `<div style="position:absolute;left:${symW+4}px;top:2px;white-space:nowrap;font-family:'Share Tech Mono','Courier New',monospace;line-height:1.35;pointer-events:none;">
        <div style="color:${namCol};font-size:9px;${shadow}">${name||'UNKNOWN'}</div>
      </div>`

  return L.divIcon({
    html: `<div style="position:relative;display:inline-block;filter:drop-shadow(0 0 3px ${fillCol}66)">
             ${symSvg}
             ${labelHtml}
           </div>`,
    className:  '',
    iconSize:   [0, 0],
    iconAnchor: [Math.round(symW / 2), Math.round(symH / 2)],
  })
}

function restMilSymIcon(owner: string, tags: string[], typ: string, alt: number, spd: number, _heading: number, watched = false): L.DivIcon {
  const coa = owner === 'Red' ? 1 : 2
  const cat = tags.includes('Aircraft')||tags.includes('AWACS') ? 1
            : tags.includes('Helicopter') ? 2
            : tags.includes('Boat')       ? 4
            : 3
  return sneakerUnitIcon({ coa, cat, name: typ, alt, spd, watched, symSize: 24 })
}

// ── Threat / objective icons ───────────────────────────────────────────
const OBJ_SYMBOL: Record<string,string> = {
  Airbase:'✈', FARP:'⬡', FOB:'⬡', 'Logistics Hub':'⬡',
  'Naval Base':'⚓', 'Carrier Group':'⚓', Factory:'⚙',
}
const OBJ_RADIUS: Record<string,number> = {
  Airbase:12, 'Naval Base':11, 'Carrier Group':11, Factory:9, default:7,
}
const THREAT_NM: Record<string,number> = {
  Airbase:25, 'Naval Base':40, 'Carrier Group':50, Factory:15, 'Logistics Hub':10, FARP:5, FOB:5,
}
const MK_STYLE: Record<MarkerType,{bg:string;bd:string}> = {
  IP:{bg:'#22c55e30',bd:'#22c55e'}, TGT:{bg:'#ef444430',bd:'#ef4444'},
  CP:{bg:'#3b82f630',bd:'#3b82f6'}, BULL:{bg:'#ffffff10',bd:'#e2e8f0'},
  EGR:{bg:'#eab30830',bd:'#eab308'},
}

function waypointIcon(n: number, last: boolean): L.DivIcon {
  const c = last ? '#ef4444' : '#00b4f5'
  return L.divIcon({
    html:`<div style="width:22px;height:22px;border-radius:50%;background:${c}22;border:2px solid ${c};
               color:#fff;font-size:9px;font-weight:700;display:flex;align-items:center;
               justify-content:center;font-family:${FONT_MONO};box-shadow:0 0 6px ${c}88;">${n}</div>`,
    className:'', iconSize:[22,22], iconAnchor:[11,11],
  })
}
function planMarkerIcon(type: MarkerType): L.DivIcon {
  const s = MK_STYLE[type]
  const ICONS: Record<MarkerType, string> = {
    IP:   `<svg width="10" height="10" viewBox="0 0 10 10"><polygon points="5,0 10,10 0,10" fill="none" stroke="currentColor" stroke-width="1.5"/></svg>`,
    TGT:  `<svg width="10" height="10" viewBox="0 0 10 10"><circle cx="5" cy="5" r="4" fill="none" stroke="currentColor" stroke-width="1.2"/><line x1="5" y1="1" x2="5" y2="9" stroke="currentColor" stroke-width="1"/><line x1="1" y1="5" x2="9" y2="5" stroke="currentColor" stroke-width="1"/></svg>`,
    CP:   `<svg width="10" height="10" viewBox="0 0 10 10"><polygon points="5,0 10,5 5,10 0,5" fill="none" stroke="currentColor" stroke-width="1.5"/></svg>`,
    BULL: `<svg width="10" height="10" viewBox="0 0 10 10"><circle cx="5" cy="5" r="4" fill="none" stroke="currentColor" stroke-width="1"/><circle cx="5" cy="5" r="2" fill="none" stroke="currentColor" stroke-width="1"/><circle cx="5" cy="5" r="0.8" fill="currentColor"/></svg>`,
    EGR:  `<svg width="10" height="10" viewBox="0 0 10 10"><line x1="1" y1="5" x2="9" y2="5" stroke="currentColor" stroke-width="1.5"/><polyline points="6,2 9,5 6,8" fill="none" stroke="currentColor" stroke-width="1.5"/></svg>`,
  }
  return L.divIcon({
    html: `<div style="display:flex;flex-direction:column;align-items:center;gap:2px;pointer-events:none;">
      <div style="padding:3px 7px;border-radius:2px;background:${s.bg};color:${s.bd};
                  font-family:${FONT_MONO};font-size:9px;font-weight:700;letter-spacing:0.1em;
                  border:1px solid ${s.bd};box-shadow:0 0 6px ${s.bd}66;
                  display:flex;align-items:center;gap:4px;white-space:nowrap;">
        <span style="color:${s.bd}">${ICONS[type]}</span>${type}
      </div>
    </div>`,
    className: '',
    iconSize:   [0, 0],
    iconAnchor: [0, 0],
  })
}

// ── Heatmap layer ──────────────────────────────────────────────────────
function HeatmapLayer({ points }: { points: [number,number,number][] }) {
  const map = useMap()
  const ref = useRef<L.HeatLayer|null>(null)
  useEffect(() => {
    if (!points.length) { if(ref.current){map.removeLayer(ref.current);ref.current=null}; return }
    if (!ref.current) ref.current=(L as any).heatLayer(points,{radius:25,blur:20,maxZoom:12,max:1,
      gradient:{0.2:'#002200',0.5:'#006600',0.8:'#00cc00',1.0:'#39ff14'}}).addTo(map)
    else ref.current.setLatLngs(points as any)
    return ()=>{ if(ref.current){map.removeLayer(ref.current);ref.current=null} }
  },[map,points])
  return null
}

// ── Map sub-components ─────────────────────────────────────────────────
function FlyTo({ center, zoom }: { center:[number,number]; zoom:number }) {
  const map=useMap()
  useEffect(()=>{ map.flyTo(center,zoom,{duration:1.2}) },[center,zoom,map])
  return null
}
function FitBounds({ objectives }: { objectives: Objective[] }) {
  const map=useMap(), done=useRef(false)
  useEffect(()=>{
    if(done.current||!objectives.length) return; done.current=true
    const lats=objectives.map(o=>o.lat), lons=objectives.map(o=>o.lon)
    map.fitBounds([[Math.min(...lats)-.3,Math.min(...lons)-.3],[Math.max(...lats)+.3,Math.max(...lons)+.3]] as LatLngBoundsExpression,{padding:[40,40],maxZoom:10})
  },[objectives,map])
  return null
}

// Combined map event handler: planning clicks, cursor coords, BRAA
function MapEvents({
  planMode, onPlanClick,
  braaStart, onBraaContextMenu, onBraaFinish, onMouseMove,
}: {
  planMode: PlanMode
  onPlanClick: (lat:number,lon:number) => void
  braaStart: {lat:number;lon:number}|null
  onBraaContextMenu: (lat:number,lon:number) => void
  onBraaFinish: (lat:number,lon:number) => void
  onMouseMove: (lat:number,lon:number) => void
}) {
  const map = useMap()
  useEffect(()=>{
    const c=map.getContainer()
    c.style.cursor = planMode!=='none' ? 'crosshair' : braaStart ? 'crosshair' : ''
    return ()=>{ c.style.cursor='' }
  },[planMode,braaStart,map])
  useMapEvents({
    click: e => {
      if (planMode!=='none') onPlanClick(e.latlng.lat, e.latlng.lng)
      else if (braaStart)   onBraaFinish(e.latlng.lat, e.latlng.lng)
    },
    contextmenu: e => {
      e.originalEvent.preventDefault()
      onBraaContextMenu(e.latlng.lat, e.latlng.lng)
    },
    mousemove: e => onMouseMove(e.latlng.lat, e.latlng.lng),
  })
  return null
}

// ── HUD button ─────────────────────────────────────────────────────────
function HudBtn({ active, onClick, children, title, color=GREEN }: {
  active?: boolean; onClick: ()=>void; children: React.ReactNode; title?: string; color?: string
}) {
  return (
    <button title={title} onClick={onClick} style={{
      fontFamily:FONT_HEAD, fontSize:'0.72rem', letterSpacing:'0.14em',
      padding:'0.3rem 0.7rem', borderRadius:'2px',
      border: active ? `1px solid ${color}66` : `1px solid ${HUD_BORDER}`,
      background: active ? `${color}15` : 'transparent',
      color: active ? color : HUD_DIM,
      cursor:'pointer', transition:'all 0.12s', whiteSpace:'nowrap',
    }}
    onMouseEnter={e=>{ if(!active) e.currentTarget.style.color=color }}
    onMouseLeave={e=>{ if(!active) e.currentTarget.style.color=HUD_DIM }}
    >{children}</button>
  )
}

// ── Mission timer display ──────────────────────────────────────────────
function MissionTimer({ liveTime, hacks, onAddHack, onRemoveHack }: {
  liveTime: number
  hacks: HackTimer[]
  onAddHack: () => void
  onRemoveHack: (id: string) => void
}) {
  const [, setTick] = useState(0)
  useEffect(()=>{ const id=setInterval(()=>setTick(t=>t+1),1000); return ()=>clearInterval(id) },[])
  const now = Date.now()
  const base: React.CSSProperties = {
    background:'rgba(64,64,64,0.2)', border:'1px solid rgba(255,128,128,0.25)',
    backdropFilter:'blur(6px)', borderRadius:'2px', padding:'6px 10px',
    fontFamily:FONT_MONO, color:'#ff8080', fontSize:'0.65rem',
  }
  return (
    <div style={{ display:'flex', flexDirection:'column', gap:'4px' }}>
      {/* Hack timers above mission timer */}
      {hacks.map(h => (
        <div key={h.id} style={{ ...base, display:'flex', justifyContent:'space-between', gap:'10px', cursor:'pointer' }}
          onClick={() => onRemoveHack(h.id)}>
          <span style={{ color:'#ffd600', letterSpacing:'0.08em' }}>{h.label}</span>
          <span style={{ color:'#ffd600' }}>{fmtElapsed(now - h.startedAt)}</span>
        </div>
      ))}
      {/* Mission timer */}
      <div style={{ ...base, display:'flex', gap:'12px', alignItems:'center' }}>
        <span style={{ color:'rgba(255,128,128,0.5)', fontSize:'0.58rem', letterSpacing:'0.12em' }}>T+</span>
        <span style={{ fontSize:'0.82rem', color:'#ff3333', letterSpacing:'0.08em' }}>
          {liveTime > 0 ? fmtDcsTime(liveTime) : '--:--:--'}
        </span>
        <button onClick={onAddHack} title="Start hack timer"
          style={{ marginLeft:'4px', background:'none', border:'1px solid rgba(255,214,0,0.35)',
                   borderRadius:'2px', color:'#ffd600', cursor:'pointer',
                   fontSize:'0.6rem', padding:'1px 6px', fontFamily:FONT_MONO }}>
          HACK
        </button>
      </div>
    </div>
  )
}

// ═══════════════════════════════════════════════════════════════════════
// Main MapPage
// ═══════════════════════════════════════════════════════════════════════
export default function MapPage() {
  // ── Persisted settings ──────────────────────────────────────────────
  const [tileKey,     setTileKey]     = usePersisted<TileKey>('tileKey','tactical')
  const [_iconStyle,  _setIconStyle]  = usePersisted<IconStyle>('iconStyle','dot')
  const [sideFilter,  setSideFilter]  = usePersisted<'All'|'Red'|'Blue'|'Neutral'>('sideFilter','All')
  const [showThreats, setShowThreats] = usePersisted('showThreats',false)
  const [showRadar,   setShowRadar]   = usePersisted('showRadar',true)
  const [showLive,    setShowLive]    = usePersisted('showLive',true)
  const [showHeat,    setShowHeat]    = usePersisted('showHeat',false)
  const [showVectors, setShowVectors] = usePersisted('showVectors',true)
  const [showTrails,  setShowTrails]  = usePersisted('showTrails',true)
  const [showPlan,    setShowPlan]    = usePersisted('showPlan',false)
  const [speed,       setSpeed]       = usePersisted('speed',400)
  const [scratchText, setScratchText] = usePersisted('scratch','')

  // ── Persisted planning data ─────────────────────────────────────────
  const [waypoints,   setWaypoints]   = usePersisted<Waypoint[]>('waypoints',[])
  const [planMarkers, setPlanMarkers] = usePersisted<PlanMarker[]>('planMarkers',[])
  const [measurePts,  setMeasurePts]  = usePersisted<{lat:number;lon:number}[]>('measurePts',[])

  // ── Ephemeral state ─────────────────────────────────────────────────
  const [flyTarget,    setFlyTarget]    = useState<{center:[number,number];zoom:number}|null>(null)
  const [planMode,     setPlanMode]     = useState<PlanMode>('none')
  const [markerType,   setMarkerType]   = useState<MarkerType>('IP')
  const [wsStatus,     setWsStatus]     = useState<'open'|'closed'|'error'>('closed')

  // BRAA
  const [braaStart,   setBraaStart]   = useState<{lat:number;lon:number}|null>(null)
  const [braaCursor,  setBraaCursor]  = useState<{lat:number;lon:number}|null>(null)
  const [drawnBraas,  setDrawnBraas]  = useState<BraaLine[]>([])

  // Watches (set of unit IDs)
  const [watches,     setWatches]     = useState<Set<string>>(new Set())

  // Console panel
  const [showConsole,  setShowConsole]  = useState(true)
  const [consoleTab,   setConsoleTab]   = useState<'search'|'watches'|'draw'>('search')
  const [searchQuery,  setSearchQuery]  = useState('')

  // Scratchpad
  const [showScratch, setShowScratch] = useState(false)

  // Hack timers
  const [hacks, setHacks] = useState<HackTimer[]>([])
  let hackCounter = useRef(1)

  // Live units + bullseye
  const [liveUnits,  setLiveUnits]  = useState<LiveUnit[]>([])
  const [liveTime,   setLiveTime]   = useState<number>(0)
  const [bullseyes,  setBullseyes]  = useState<Bullseye[]>([])
  const trailsRef = useRef<Trails>(new Map())

  // Cursor coords (ref + 5Hz display to avoid re-render on every mouse move)
  const cursorLatLon = useRef<{lat:number;lon:number}|null>(null)
  const [cursorCoord, setCursorCoord] = useState('')
  useEffect(()=>{ const id=setInterval(()=>{
    if (cursorLatLon.current) setCursorCoord(fmtCoord(cursorLatLon.current.lat, cursorLatLon.current.lon))
  },200); return ()=>clearInterval(id) },[])

  const { selectedRound } = useRound()

  // ── REST queries ────────────────────────────────────────────────────
  const { data: objectives=[], isLoading } = useQuery({
    queryKey:['objectives',selectedRound],
    queryFn:()=>api.objectives(selectedRound),
    refetchInterval: OBJ_INT,
  })
  const { data: restUnits=[] } = useQuery({
    queryKey:['units'], queryFn:api.units, refetchInterval: REST_INT,
    enabled: wsStatus!=='open',
  })
  const { data: rounds=[] } = useQuery({
    queryKey:['rounds'], queryFn:api.rounds, refetchInterval:60_000,
  })

  // ── Load historical trail points on mount ───────────────────────────
  useEffect(()=>{
    api.trails().then(points => {
      const trails = trailsRef.current
      for (const p of points) {
        const pts = trails.get(p.id) ?? []
        pts.push({ lat: p.lat, lon: p.lon, ts: p.ts })
        trails.set(p.id, pts)
      }
    }).catch(() => {/* trails are optional */})
  }, [])

  // ── WebSocket ────────────────────────────────────────────────────────
  useEffect(()=>{
    return connectLiveUnits((msg: WsUnitsMsg)=>{
      setLiveTime(msg.t)
      setLiveUnits(msg.units)
      if (msg.bull && msg.bull.length > 0) setBullseyes(msg.bull)
      const now = Date.now()/1000
      const trails = trailsRef.current
      for (const u of msg.units) {
        const pts = trails.get(u.id) ?? []
        pts.push({ lat:u.lat, lon:u.lon, ts:now })
        const trimmed = pts.filter(p=>p.ts>=now-TRAIL_SEC)
        if (trimmed.length>TRAIL_MAX) trimmed.splice(0,trimmed.length-TRAIL_MAX)
        trails.set(u.id, trimmed)
      }
      const liveIds = new Set(msg.units.map(u=>u.id))
      for (const id of Array.from(trails.keys())) if (!liveIds.has(id)) trails.delete(id)
    }, setWsStatus)
  },[])

  // ── Derived data ────────────────────────────────────────────────────
  rounds.find(r=>r.active)
  const validObjs   = objectives.filter(o=>o.lat!==0||o.lon!==0)
  const shownObjs   = sideFilter==='All' ? validObjs : validObjs.filter(o=>o.owner===sideFilter)
  const counts = {
    All:     validObjs.length,
    Red:     validObjs.filter(o=>o.owner==='Red').length,
    Blue:    validObjs.filter(o=>o.owner==='Blue').length,
    Neutral: validObjs.filter(o=>o.owner==='Neutral').length,
  }
  const redPct  = counts.All>0 ? Math.round(counts.Red /counts.All*100) : 0
  const bluePct = counts.All>0 ? Math.round(counts.Blue/counts.All*100) : 0

  // Active unit list
  const useLive = wsStatus==='open' && showLive
  const activeUnits: LiveUnit[] = useMemo(()=>{
    if (!useLive) return []
    if (!searchQuery) return liveUnits
    const q = searchQuery.toLowerCase()
    return liveUnits.filter(u=>u.nm.toLowerCase().includes(q)||u.typ.toLowerCase().includes(q))
  },[useLive,liveUnits,searchQuery])

  const activeRestUnits: MapUnit[] = useMemo(()=>{
    if (useLive || !showRadar) return []
    if (!searchQuery) return restUnits
    const q = searchQuery.toLowerCase()
    return restUnits.filter(u=>u.typ.toLowerCase().includes(q))
  },[useLive,showRadar,restUnits,searchQuery])

  const airCount = useLive
    ? activeUnits.filter(u=>u.cat<=2).length
    : activeRestUnits.filter(u=>u.tags.some(t=>t==='Aircraft'||t==='Helicopter')).length

  // Heatmap
  const heatPoints: [number,number,number][] = useMemo(()=>{
    if (!showHeat) return []
    const pts: [number,number,number][] = []
    for (const u of activeUnits) pts.push([u.lat,u.lon,0.6])
    for (const u of activeRestUnits) pts.push([u.lat,u.lon,0.6])
    for (const o of validObjs) if (o.health<90) pts.push([o.lat,o.lon,Math.max(0.3,1-o.health/100)])
    return pts
  },[showHeat,activeUnits,activeRestUnits,validObjs])

  // Route / measure
  const routePos   = waypoints.map(w=>[w.lat,w.lon] as LatLngExpression)
  const measurePos = measurePts.map(p=>[p.lat,p.lon] as LatLngExpression)
  const measDist   = measurePts.length===2 ? haversineNm(measurePts[0]!,measurePts[1]!) : null
  const measBrg    = measurePts.length===2 ? bearingDeg(measurePts[0]!,measurePts[1]!) : null

  let totalNm = 0
  const legs = waypoints.slice(0,-1).map((wp,i)=>{
    const next=waypoints[i+1]!
    const dist=haversineNm(wp,next), brg=bearingDeg(wp,next)
    totalNm+=dist
    return { dist, brg, ete: speed>0?dist/speed*60:0 }
  })

  // BRAA live line
  const braaActiveLine: [number,number][] = braaStart && braaCursor
    ? [[braaStart.lat,braaStart.lon],[braaCursor.lat,braaCursor.lon]]
    : []
  const braaActiveDist  = braaStart && braaCursor ? haversineNm(braaStart,braaCursor) : null
  const braaActiveBrg   = braaStart && braaCursor ? bearingDeg(braaStart,braaCursor) : null

  // ── Handlers ────────────────────────────────────────────────────────
  const handlePlanClick = useCallback((lat:number,lon:number)=>{
    if (planMode==='waypoint') setWaypoints(p=>[...p,{id:crypto.randomUUID(),lat,lon}])
    else if (planMode==='marker') setPlanMarkers(p=>[...p,{id:crypto.randomUUID(),lat,lon,type:markerType}])
    else if (planMode==='measure') setMeasurePts(p=>p.length>=2?[{lat,lon}]:[...p,{lat,lon}])
  },[planMode,markerType])

  const handleContextMenu = useCallback((lat:number,lon:number)=>{
    if (braaStart) {
      setDrawnBraas(p=>[...p,{id:crypto.randomUUID(),from:braaStart,to:{lat,lon}}])
      setBraaStart(null); setBraaCursor(null)
    } else {
      setBraaStart({lat,lon}); setBraaCursor({lat,lon})
    }
  },[braaStart])

  const handleBraaFinish = useCallback((lat:number,lon:number)=>{
    if (braaStart) {
      setDrawnBraas(p=>[...p,{id:crypto.randomUUID(),from:braaStart,to:{lat,lon}}])
      setBraaStart(null); setBraaCursor(null)
    }
  },[braaStart])

  const handleMouseMove = useCallback((lat:number,lon:number)=>{
    cursorLatLon.current = {lat,lon}
    if (braaStart) setBraaCursor({lat,lon})
  },[braaStart])

  function toggleMode(m: PlanMode) { setPlanMode(p=>p===m?'none':m) }
  function toggleWatch(id: string) {
    setWatches(prev=>{ const s=new Set(prev); s.has(id)?s.delete(id):s.add(id); return s })
  }
  function addHack() {
    setHacks(p=>[...p,{id:crypto.randomUUID(),label:`HACK ${hackCounter.current++}`,startedAt:Date.now()}])
  }
  function clearAll() { setWaypoints([]); setPlanMarkers([]); setMeasurePts([]); setPlanMode('none') }

  // ── Shared panel style ───────────────────────────────────────────────
  const hudPanel: React.CSSProperties = {
    background:HUD_BG, border:`1px solid ${HUD_BORDER}`,
    borderRadius:'3px', backdropFilter:'blur(8px)',
    padding:'7px 11px', color:HUD_TEXT, fontFamily:FONT_MONO,
  }

  // ── Render ───────────────────────────────────────────────────────────
  return (
    <div style={{ position:'relative', flex:1, overflow:'hidden', display:'flex' }}>

      {/* ── LEFT CONSOLE PANEL (Sneaker-style) ────────────────────────── */}
      {showConsole && (
        <div style={{
          width:'240px', flexShrink:0, display:'flex', flexDirection:'column',
          background:PANEL_BG, borderRight:`1px solid ${HUD_BORDER}`,
          fontFamily:FONT_MONO, color:HUD_TEXT, zIndex:10,
        }}>
          {/* Tabs */}
          <div style={{ display:'flex', borderBottom:`1px solid ${HUD_BORDER}` }}>
            {(['search','watches','draw'] as const).map(t=>(
              <button key={t} onClick={()=>setConsoleTab(t)} style={{
                flex:1, padding:'8px 0', fontFamily:FONT_HEAD, fontSize:'0.68rem',
                letterSpacing:'0.14em', cursor:'pointer', border:'none',
                borderBottom: consoleTab===t ? `2px solid ${GREEN}` : '2px solid transparent',
                marginBottom:'-1px', background:'transparent',
                color: consoleTab===t ? GREEN : HUD_DIM, transition:'all 0.12s',
              }}>
                {t.toUpperCase()}
              </button>
            ))}
          </div>

          <div style={{ flex:1, overflowY:'auto', padding:'10px' }}>

            {/* ── SEARCH TAB ── */}
            {consoleTab==='search' && (
              <div style={{ display:'flex', flexDirection:'column', gap:'8px' }}>
                <input
                  type="text" placeholder="filter by name or type…"
                  value={searchQuery} onChange={e=>setSearchQuery(e.target.value)}
                  style={{
                    background:'rgba(0,20,0,0.5)', border:`1px solid ${HUD_BORDER}`,
                    borderRadius:'2px', color:GREEN, fontFamily:FONT_MONO,
                    fontSize:'0.68rem', padding:'5px 8px', outline:'none', width:'100%',
                    boxSizing:'border-box',
                  }}
                />
                <div style={{ fontSize:'0.6rem', color:HUD_DIM, letterSpacing:'0.1em' }}>
                  {useLive
                    ? `${activeUnits.length} LIVE CONTACTS`
                    : `${activeRestUnits.length} EWR CONTACTS`}
                </div>
                {/* unit list */}
                <div style={{ display:'flex', flexDirection:'column', gap:'3px' }}>
                  {(useLive ? activeUnits : activeRestUnits).slice(0,40).map(u=>{
                    const isLive = 'cat' in u
                    const id  = u.id
                    const coa = isLive ? (u as LiveUnit).coa : ((u as MapUnit).owner==='Red'?1:2)
                    const col = unitColor(coa, watches.has(id))
                    const name = isLive ? ((u as LiveUnit).nm||(u as LiveUnit).typ) : (u as MapUnit).typ
                    return (
                      <div key={id} onClick={()=>toggleWatch(id)}
                        style={{
                          padding:'4px 7px', borderRadius:'2px', cursor:'pointer',
                          background: watches.has(id) ? 'rgba(255,214,0,0.08)' : 'rgba(255,255,255,0.03)',
                          border:`1px solid ${watches.has(id)?'rgba(255,214,0,0.3)':'transparent'}`,
                          display:'flex', justifyContent:'space-between', alignItems:'center',
                        }}>
                        <span style={{ fontSize:'0.65rem', color:col, flex:1, overflow:'hidden', textOverflow:'ellipsis', whiteSpace:'nowrap' }}>
                          {name}
                        </span>
                        {isLive && (
                          <span style={{ fontSize:'0.58rem', color:HUD_DIM, marginLeft:'6px', flexShrink:0 }}>
                            {fmtAlt((u as LiveUnit).alt)}
                          </span>
                        )}
                      </div>
                    )
                  })}
                </div>
              </div>
            )}

            {/* ── WATCHES TAB ── */}
            {consoleTab==='watches' && (
              <div style={{ display:'flex', flexDirection:'column', gap:'6px' }}>
                {watches.size===0 ? (
                  <div style={{ fontSize:'0.62rem', color:HUD_DIM, textAlign:'center', paddingTop:'20px' }}>
                    Click any contact to watch it
                  </div>
                ) : (
                  Array.from(watches).map(id=>{
                    const u = useLive
                      ? liveUnits.find(u=>u.id===id)
                      : restUnits.find(u=>u.id===id)
                    if (!u) return (
                      <div key={id} style={{ fontSize:'0.62rem', color:HUD_DIM }}>
                        {id.slice(0,8)}… (lost)
                        <button onClick={()=>toggleWatch(id)} style={{ marginLeft:'6px', background:'none', border:'none', color:HUD_DIM, cursor:'pointer' }}>✕</button>
                      </div>
                    )
                    const isLive = 'cat' in u
                    const coa = isLive ? (u as LiveUnit).coa : ((u as MapUnit).owner==='Red'?1:2)
                    const col = unitColor(coa, true)
                    const name = isLive ? ((u as LiveUnit).nm||(u as LiveUnit).typ) : (u as MapUnit).typ
                    return (
                      <div key={id} style={{
                        padding:'6px 8px', borderRadius:'2px',
                        background:'rgba(255,214,0,0.06)', border:'1px solid rgba(255,214,0,0.25)',
                      }}>
                        <div style={{ display:'flex', justifyContent:'space-between', marginBottom:'3px' }}>
                          <span style={{ fontSize:'0.65rem', color:col, fontWeight:700 }}>{name}</span>
                          <button onClick={()=>toggleWatch(id)}
                            style={{ background:'none', border:'none', color:HUD_DIM, cursor:'pointer', fontSize:'0.65rem' }}>✕</button>
                        </div>
                        {isLive && (
                          <div style={{ display:'grid', gridTemplateColumns:'1fr 1fr', gap:'2px 6px', fontSize:'0.6rem', color:HUD_DIM }}>
                            <span>ALT {fmtAlt((u as LiveUnit).alt)}</span>
                            <span>SPD {fmtSpd((u as LiveUnit).spd)}kts</span>
                            <span>HDG {fmtBrg((u as LiveUnit).hdg)}</span>
                            <span>{(u as LiveUnit).coa===1?'RED':'BLUE'}</span>
                          </div>
                        )}
                      </div>
                    )
                  })
                )}
                {watches.size>0 && (
                  <button onClick={()=>setWatches(new Set())}
                    style={{ marginTop:'4px', background:'none', border:`1px solid ${HUD_BORDER}`,
                             borderRadius:'2px', color:HUD_DIM, cursor:'pointer', padding:'4px',
                             fontFamily:FONT_HEAD, fontSize:'0.65rem', letterSpacing:'0.12em' }}>
                    CLEAR WATCHES
                  </button>
                )}
              </div>
            )}

            {/* ── DRAW TAB ── */}
            {consoleTab==='draw' && (
              <div style={{ display:'flex', flexDirection:'column', gap:'6px' }}>
                <div style={{ fontSize:'0.6rem', color:HUD_DIM, letterSpacing:'0.1em', marginBottom:'2px' }}>
                  RIGHT-CLICK MAP TO DRAW BRAA
                </div>
                {braaStart && (
                  <div style={{ padding:'5px 7px', borderRadius:'2px', background:'rgba(255,200,0,0.08)',
                                border:'1px solid rgba(255,200,0,0.3)', fontSize:'0.62rem', color:'#ffd600' }}>
                    DRAWING… click or right-click to finish
                  </div>
                )}
                {drawnBraas.length===0 ? (
                  <div style={{ fontSize:'0.62rem', color:HUD_DIM, textAlign:'center', paddingTop:'10px' }}>
                    No BRAA lines drawn
                  </div>
                ) : drawnBraas.map((b,i)=>{
                  const dist = haversineNm(b.from,b.to)
                  const brg  = bearingDeg(b.from,b.to)
                  return (
                    <div key={b.id} style={{
                      padding:'5px 8px', borderRadius:'2px',
                      background:'rgba(57,255,20,0.04)', border:`1px solid ${HUD_BORDER}`,
                      fontSize:'0.62rem',
                    }}>
                      <div style={{ display:'flex', justifyContent:'space-between', marginBottom:'2px' }}>
                        <span style={{ color:GREEN }}>BRAA {i+1}</span>
                        <button onClick={()=>setDrawnBraas(p=>p.filter(x=>x.id!==b.id))}
                          style={{ background:'none', border:'none', color:HUD_DIM, cursor:'pointer' }}>✕</button>
                      </div>
                      <div style={{ color:HUD_DIM }}>
                        {fmtBrg(brg)} · {dist.toFixed(1)} NM
                      </div>
                    </div>
                  )
                })}
                {drawnBraas.length>0 && (
                  <button onClick={()=>setDrawnBraas([])}
                    style={{ background:'none', border:`1px solid ${HUD_BORDER}`,
                             borderRadius:'2px', color:HUD_DIM, cursor:'pointer', padding:'4px',
                             fontFamily:FONT_HEAD, fontSize:'0.65rem', letterSpacing:'0.12em' }}>
                    CLEAR ALL
                  </button>
                )}
              </div>
            )}
          </div>

          {/* Console footer: scratchpad toggle */}
          <div style={{ padding:'7px 10px', borderTop:`1px solid ${HUD_BORDER}`, display:'flex', gap:'6px' }}>
            <button onClick={()=>setShowScratch(v=>!v)} style={{
              flex:1, background: showScratch?`${GREEN}12`:'transparent',
              border:`1px solid ${showScratch?`${GREEN}44`:HUD_BORDER}`,
              borderRadius:'2px', color: showScratch?GREEN:HUD_DIM,
              cursor:'pointer', fontFamily:FONT_HEAD, fontSize:'0.65rem',
              letterSpacing:'0.12em', padding:'4px',
            }}>SCRATCHPAD</button>
          </div>
        </div>
      )}

      {/* ── MAP AREA ─────────────────────────────────────────────────── */}
      <div style={{ position:'relative', flex:1, overflow:'hidden' }}>
        <MapContainer
          center={[42.35,43.50]} zoom={7}
          style={{ position:'absolute', inset:0, width:'100%', height:'100%', background:'#060a06' }}
          zoomControl={false} attributionControl={false}
        >
          <TileLayer key={tileKey} url={TILE_LAYERS[tileKey].url}
            attribution={TILE_LAYERS[tileKey].attr} maxZoom={19}
            opacity={tileKey==='tactical'?0.85:0.70} />

          <ScaleControl position="bottomleft" />
          <MapEvents
            planMode={planMode}
            onPlanClick={handlePlanClick}
            braaStart={braaStart}
            onBraaContextMenu={handleContextMenu}
            onBraaFinish={handleBraaFinish}
            onMouseMove={handleMouseMove}
          />
          {validObjs.length>0 && <FitBounds objectives={validObjs} />}
          {flyTarget && <FlyTo center={flyTarget.center} zoom={flyTarget.zoom} />}

          {/* Heatmap */}
          {showHeat && <HeatmapLayer points={heatPoints} />}

          {/* ── Threat rings ──────────────────────────────────────── */}
          {showThreats && shownObjs.map(obj=>{
            const nm=THREAT_NM[obj.kind]; if(!nm) return null
            const c=obj.owner==='Red'?COL_ENEMY:obj.owner==='Blue'?COL_ALLY:COL_NEUTRAL
            return <Circle key={`thr-${obj.id}`} center={[obj.lat,obj.lon]} radius={nmToM(nm)}
              pathOptions={{color:c,fillColor:c,fillOpacity:0.03,weight:0.8,dashArray:'4 6'}}
              interactive={false} />
          })}

          {/* ── Bullseye markers (from mission) ──────────────────── */}
          {bullseyes.map(b=>{
            const col = b.side===1 ? COL_ENEMY : COL_ALLY
            const label = b.side===1 ? 'RED BULL' : 'BLU BULL'
            const icon = L.divIcon({
              html: `<div style="position:relative;display:flex;flex-direction:column;align-items:center;pointer-events:none;">
                <svg width="32" height="32" viewBox="0 0 32 32" style="filter:drop-shadow(0 0 4px ${col});">
                  <circle cx="16" cy="16" r="13" fill="none" stroke="${col}" stroke-width="1.2"/>
                  <circle cx="16" cy="16" r="7"  fill="none" stroke="${col}" stroke-width="1"/>
                  <circle cx="16" cy="16" r="2"  fill="${col}"/>
                  <line x1="16" y1="2"  x2="16" y2="8"  stroke="${col}" stroke-width="1"/>
                  <line x1="16" y1="24" x2="16" y2="30" stroke="${col}" stroke-width="1"/>
                  <line x1="2"  y1="16" x2="8"  y2="16" stroke="${col}" stroke-width="1"/>
                  <line x1="24" y1="16" x2="30" y2="16" stroke="${col}" stroke-width="1"/>
                </svg>
                <div style="color:${col};font-family:'Share Tech Mono','Courier New',monospace;
                            font-size:8px;font-weight:700;letter-spacing:0.1em;margin-top:1px;
                            text-shadow:0 0 4px #000,0 0 8px #000;">${label}</div>
              </div>`,
              className: '', iconSize: [0,0], iconAnchor: [16,16],
            })
            return (
              <Marker key={`bull-${b.side}`} position={[b.lat,b.lon]} icon={icon}>
                <Popup minWidth={140}>
                  <div style={{background:'#060a06',color:HUD_TEXT,fontFamily:FONT_MONO,fontSize:'0.7rem',padding:'2px 0'}}>
                    <div style={{color:col,fontFamily:FONT_HEAD,letterSpacing:'0.12em',marginBottom:'4px'}}>{label}</div>
                    <div style={{fontSize:'0.62rem',color:HUD_DIM}}>{fmtCoord(b.lat,b.lon)}</div>
                  </div>
                </Popup>
              </Marker>
            )
          })}

          {/* ── Objectives ────────────────────────────────────────── */}
          {shownObjs.map(obj=>{
            const c=obj.owner==='Red'?COL_ENEMY:obj.owner==='Blue'?COL_ALLY:COL_NEUTRAL
            const r=OBJ_RADIUS[obj.kind]??OBJ_RADIUS.default
            return (
              <CircleMarker key={obj.id} center={[obj.lat,obj.lon]} radius={r}
                pathOptions={{color:c,fillColor:c,fillOpacity:0.15,weight:1.5}}>
                <Popup minWidth={200} maxWidth={240}>
                  <div style={{background:'#060a06',color:HUD_TEXT,fontFamily:FONT_MONO,fontSize:'0.72rem',padding:'2px 0'}}>
                    <div style={{fontFamily:FONT_HEAD,letterSpacing:'0.12em',marginBottom:'6px',fontSize:'0.85rem'}}>
                      {OBJ_SYMBOL[obj.kind]??'●'} {obj.name}
                    </div>
                    <div style={{display:'flex',gap:'6px',marginBottom:'8px',fontSize:'0.65rem'}}>
                      <span style={{color:c,border:`1px solid ${c}55`,padding:'1px 6px',borderRadius:'2px'}}>{obj.owner.toUpperCase()}</span>
                      <span style={{color:HUD_DIM}}>{obj.kind}</span>
                    </div>
                    {obj.owner !== 'Neutral' && (['health','logi','supply','fuel'] as const).map(k=>{
                      const v=obj[k as keyof Objective] as number
                      const bc=v>=75?GREEN:v>=40?'#ffaa00':COL_ENEMY
                      return (
                        <div key={k} style={{marginBottom:'4px'}}>
                          <div style={{display:'flex',justifyContent:'space-between',marginBottom:'2px',fontSize:'0.6rem',color:HUD_DIM}}>
                            <span>{k.toUpperCase()}</span><span style={{color:bc}}>{v}%</span>
                          </div>
                          <div style={{height:'3px',background:'#0a150a',borderRadius:'1px',overflow:'hidden'}}>
                            <div style={{width:`${v}%`,height:'100%',background:bc}} />
                          </div>
                        </div>
                      )
                    })}
                  </div>
                </Popup>
              </CircleMarker>
            )
          })}

          {/* ── Unit trails – fading dots (Sneaker style) ─────────── */}
          {showTrails && activeUnits.filter(u=>u.cat<=2).flatMap(u=>{
            const pts=trailsRef.current.get(u.id)
            if (!pts||pts.length<2) return []
            const col=unitColor(u.coa,watches.has(u.id))
            const now=Date.now()/1000
            // Render each historical point as a small circle, older = more transparent
            return pts.slice(0,-1).map((p,i)=>{
              const age=(now-p.ts)/TRAIL_SEC          // 0=fresh 1=oldest
              const opacity=Math.max(0.08, 0.55*(1-age))
              return (
                <CircleMarker key={`trail-${u.id}-${i}`}
                  center={[p.lat,p.lon]} radius={2}
                  pathOptions={{color:col,fillColor:col,fillOpacity:opacity,weight:0,stroke:false}} />
              )
            })
          })}

          {/* ── Velocity vectors ──────────────────────────────────── */}
          {showVectors && activeUnits.filter(u=>u.cat<=2&&u.spd>20).map(u=>{
            const distNm = u.spd/3600*VEL_SECS  // knots → NM in VEL_SECS seconds
            const [lat2,lon2] = projectPos(u.lat,u.lon,u.hdg,distNm)
            return (
              <Polyline key={`vel-${u.id}`}
                positions={[[u.lat,u.lon],[lat2,lon2]]}
                pathOptions={{color:unitColor(u.coa,watches.has(u.id)),weight:1,opacity:0.55}} />
            )
          })}
          {showVectors && activeRestUnits.filter(u=>u.speed>20&&u.tags.some(t=>t==='Aircraft'||t==='Helicopter')).map(u=>{
            const distNm = u.speed/3600*VEL_SECS
            const [lat2,lon2] = projectPos(u.lat,u.lon,u.heading,distNm)
            return (
              <Polyline key={`velr-${u.id}`}
                positions={[[u.lat,u.lon],[lat2,lon2]]}
                pathOptions={{color:restUnitColor(u.owner,watches.has(u.id)),weight:1,opacity:0.55}} />
            )
          })}

          {/* ── Live unit markers (milsymbol + Sneaker labels) ─────── */}
          {activeUnits.map(u=>(
            <Marker key={`live-${u.id}`} position={[u.lat,u.lon]}
              icon={sneakerUnitIcon({coa:u.coa,cat:u.cat,name:u.nm||u.typ,alt:u.alt,spd:u.spd,watched:watches.has(u.id)})}
              eventHandlers={{click:()=>toggleWatch(u.id)}}>
              <Popup minWidth={170}>
                <div style={{background:'#060a06',color:HUD_TEXT,fontFamily:FONT_MONO,fontSize:'0.7rem',padding:'2px 0'}}>
                  <div style={{fontFamily:FONT_HEAD,letterSpacing:'0.12em',marginBottom:'4px',fontSize:'0.82rem'}}>
                    {u.nm||u.typ||'UNKNOWN'}
                  </div>
                  <div style={{color:unitColor(u.coa,false),fontSize:'0.65rem',marginBottom:'6px'}}>
                    {u.coa===1?'RED':'BLUE'} · {u.cat===1?'AIRPLANE':u.cat===2?'HELICOPTER':u.cat===4?'NAVAL':'GROUND'}
                  </div>
                  <div style={{display:'grid',gridTemplateColumns:'auto 1fr',gap:'2px 8px',fontSize:'0.65rem'}}>
                    <span style={{color:HUD_DIM}}>ALT</span><span>{fmtAlt(u.alt)} ft</span>
                    <span style={{color:HUD_DIM}}>SPD</span><span>{fmtSpd(u.spd)} kts</span>
                    <span style={{color:HUD_DIM}}>HDG</span><span>{fmtBrg(u.hdg)}</span>
                    <span style={{color:HUD_DIM}}>TYP</span><span style={{fontSize:'0.58rem'}}>{u.typ}</span>
                  </div>
                  <div style={{marginTop:'5px',fontSize:'0.6rem',color:HUD_DIM}}>{fmtCoord(u.lat,u.lon)}</div>
                  <div style={{marginTop:'4px',fontSize:'0.6rem',color:COL_WATCH,cursor:'pointer'}}
                    onClick={()=>toggleWatch(u.id)}>
                    {watches.has(u.id)?'★ WATCHING':'☆ WATCH'}
                  </div>
                </div>
              </Popup>
            </Marker>
          ))}

          {/* ── REST unit markers (milsymbol + Sneaker labels) ─────── */}
          {activeRestUnits.map(u=>(
            <Marker key={`rest-${u.id}`} position={[u.lat,u.lon]}
              icon={restMilSymIcon(u.owner,u.tags,u.typ,u.alt,u.speed,u.heading,watches.has(u.id))}
              eventHandlers={{click:()=>toggleWatch(u.id)}}>
              <Popup minWidth={160}>
                <div style={{background:'#060a06',color:HUD_TEXT,fontFamily:FONT_MONO,fontSize:'0.7rem',padding:'2px 0'}}>
                  <div style={{fontFamily:FONT_HEAD,letterSpacing:'0.12em',marginBottom:'4px'}}>{u.typ||'UNKNOWN'}</div>
                  <div style={{color:restUnitColor(u.owner,false),fontSize:'0.65rem',marginBottom:'6px'}}>
                    {u.owner.toUpperCase()} · EWR
                  </div>
                  <div style={{display:'grid',gridTemplateColumns:'auto 1fr',gap:'2px 8px',fontSize:'0.65rem'}}>
                    <span style={{color:HUD_DIM}}>ALT</span><span>{fmtAlt(u.alt)} ft</span>
                    <span style={{color:HUD_DIM}}>SPD</span><span>{fmtSpd(u.speed)} kts</span>
                    <span style={{color:HUD_DIM}}>HDG</span><span>{fmtBrg(u.heading)}</span>
                  </div>
                </div>
              </Popup>
            </Marker>
          ))}

          {/* ── BRAA lines (drawn) ────────────────────────────────── */}
          {drawnBraas.map((b,i)=>{
            const dist=haversineNm(b.from,b.to), brg=bearingDeg(b.from,b.to)
            const mid: [number,number] = [(b.from.lat+b.to.lat)/2,(b.from.lon+b.to.lon)/2]
            return (
              <div key={b.id}>
                <Polyline positions={[[b.from.lat,b.from.lon],[b.to.lat,b.to.lon]]}
                  pathOptions={{color:GREEN,weight:1.5,opacity:0.7,dashArray:'6 4'}} />
                <Marker position={mid} icon={L.divIcon({
                  html:`<div style="background:rgba(4,10,4,0.85);border:1px solid ${HUD_BORDER};padding:2px 6px;
                              border-radius:2px;font-family:${FONT_MONO};font-size:8px;color:${GREEN};
                              white-space:nowrap;display:inline-block;">
                          BRAA${i+1} ${fmtBrg(brg)} ${dist.toFixed(1)}NM
                        </div>`,
                  className:'', iconSize:[0,0], iconAnchor:[0,8],
                })} />
              </div>
            )
          })}

          {/* ── BRAA active drawing line ──────────────────────────── */}
          {braaActiveLine.length===2 && (
            <Polyline positions={braaActiveLine as LatLngExpression[]}
              pathOptions={{color:COL_WATCH,weight:1.5,opacity:0.9,dashArray:'5 3'}} />
          )}

          {/* ── Planning overlays ─────────────────────────────────── */}
          {routePos.length>=2 && (
            <Polyline positions={routePos}
              pathOptions={{color:'#00b4f5',weight:1.5,opacity:0.8,dashArray:'6 4'}} />
          )}
          {waypoints.map((wp,i)=>(
            <Marker key={wp.id} position={[wp.lat,wp.lon]} icon={waypointIcon(i+1,i===waypoints.length-1)} />
          ))}
          {measurePos.length===2 && (
            <Polyline positions={measurePos}
              pathOptions={{color:'#a78bfa',weight:1.5,opacity:0.8,dashArray:'4 3'}} />
          )}
          {measurePts.map((p,i)=>(
            <CircleMarker key={i} center={[p.lat,p.lon]} radius={4}
              pathOptions={{color:'#a78bfa',fillColor:'#a78bfa',fillOpacity:0.8,weight:1}} />
          ))}
          {planMarkers.map(m=>(
            <Marker key={m.id} position={[m.lat,m.lon]} icon={planMarkerIcon(m.type)} />
          ))}
        </MapContainer>

        {/* ═══════════════ HUD OVERLAYS ═══════════════ */}

        {/* ── TOP-RIGHT: controls ─────────────────────────────────── */}
        <div style={{
          position:'absolute', top:10, right:10, zIndex:1000,
          display:'flex', flexDirection:'column', gap:'5px', alignItems:'flex-end',
        }}>
          {/* Tile + view toggles */}
          <div style={{...hudPanel, display:'flex', gap:'3px', flexWrap:'wrap', justifyContent:'flex-end', padding:'5px 9px'}}>
            {(Object.keys(TILE_LAYERS) as TileKey[]).map(k=>(
              <HudBtn key={k} active={tileKey===k} onClick={()=>setTileKey(k)}>{TILE_LAYERS[k].label}</HudBtn>
            ))}
            <div style={{width:'1px',background:HUD_BORDER,margin:'0 2px'}} />
            <HudBtn active={useLive} onClick={()=>setShowLive(v=>!v)}
              color={wsStatus==='open'?GREEN:COL_NEUTRAL}
              title={wsStatus==='open'?'Live export connected':'No live export – using EWR poll'}>
              {wsStatus==='open'?'⬤ LIVE':'◯ LIVE'}
            </HudBtn>
            <HudBtn active={showRadar}   onClick={()=>setShowRadar(v=>!v)}>RADAR</HudBtn>
            <HudBtn active={showVectors} onClick={()=>setShowVectors(v=>!v)}>VECTORS</HudBtn>
            <HudBtn active={showTrails}  onClick={()=>setShowTrails(v=>!v)}>TRAILS</HudBtn>
            <HudBtn active={showThreats} onClick={()=>setShowThreats(v=>!v)}>THREATS</HudBtn>
            <HudBtn active={showHeat}    onClick={()=>setShowHeat(v=>!v)}>HEAT</HudBtn>
          </div>

          {/* Side filter */}
          <div style={{...hudPanel, display:'flex', gap:'3px', padding:'5px 9px'}}>
            {(['All','Red','Blue','Neutral'] as const).map(f=>(
              <HudBtn key={f} active={sideFilter===f}
                color={f==='Red'?COL_ENEMY:f==='Blue'?COL_ALLY:GREEN}
                onClick={()=>setSideFilter(f)}>
                {f==='All'?`ALL ${counts.All}`:`${f.slice(0,3).toUpperCase()} ${counts[f]}`}
              </HudBtn>
            ))}
          </div>

          {/* Planning */}
          <div style={{...hudPanel, display:'flex', gap:'3px', flexWrap:'wrap', justifyContent:'flex-end', padding:'5px 9px'}}>
            <span style={{fontFamily:FONT_HEAD,fontSize:'0.64rem',letterSpacing:'0.18em',color:HUD_DIM,alignSelf:'center',marginRight:'2px'}}>PLAN</span>
            <HudBtn active={planMode==='waypoint'} onClick={()=>toggleMode('waypoint')} color='#00b4f5'>
              ROUTE{waypoints.length>0?` (${waypoints.length})`:''}
            </HudBtn>
            <HudBtn active={planMode==='marker'} onClick={()=>toggleMode('marker')} color='#eab308'>MARK</HudBtn>
            {planMode==='marker' && (['IP','TGT','CP','BULL','EGR'] as MarkerType[]).map(t=>(
              <HudBtn key={t} active={markerType===t} onClick={()=>setMarkerType(t)} color={MK_STYLE[t].bd}>{t}</HudBtn>
            ))}
            <HudBtn active={planMode==='measure'} onClick={()=>toggleMode('measure')} color='#a78bfa'>MEAS</HudBtn>
            {(waypoints.length>0||planMarkers.length>0||measurePts.length>0) && (
              <HudBtn active={false} onClick={clearAll} color={COL_ENEMY}>CLR</HudBtn>
            )}
            <HudBtn active={showPlan} onClick={()=>setShowPlan(v=>!v)}>
              {showPlan?'▸ HIDE':'◂ PANEL'}
            </HudBtn>
          </div>

          {/* Region jump */}
          <div style={{...hudPanel, display:'flex', gap:'2px', flexWrap:'wrap', justifyContent:'flex-end', padding:'4px 9px'}}>
            {DCS_REGIONS.map(r=>(
              <button key={r.label} onClick={()=>setFlyTarget({center:r.center,zoom:r.zoom})}
                style={{fontFamily:FONT_HEAD,fontSize:'0.62rem',letterSpacing:'0.1em',
                        padding:'2px 7px',background:'transparent',color:HUD_DIM,
                        border:'1px solid transparent',borderRadius:'2px',cursor:'pointer'}}
                onMouseEnter={e=>{e.currentTarget.style.color=GREEN;e.currentTarget.style.borderColor=HUD_BORDER}}
                onMouseLeave={e=>{e.currentTarget.style.color=HUD_DIM;e.currentTarget.style.borderColor='transparent'}}>
                {r.label}
              </button>
            ))}
          </div>
        </div>

        {/* ── BOTTOM-LEFT: mission timer + cursor ─────────────────── */}
        <div style={{
          position:'absolute', bottom:28, left:10, zIndex:1000,
          display:'flex', flexDirection:'column', gap:'5px',
          alignItems:'flex-start',
        }}>
          <MissionTimer
            liveTime={liveTime} hacks={hacks}
            onAddHack={addHack}
            onRemoveHack={id=>setHacks(p=>p.filter(h=>h.id!==id))} />
          <div style={{...hudPanel, fontSize:'0.63rem', letterSpacing:'0.07em'}}>
            <div style={{color:HUD_DIM,marginBottom:'2px',fontSize:'0.57rem',letterSpacing:'0.14em'}}>CURSOR</div>
            <div>{cursorCoord||'—'}</div>
          </div>
        </div>

        {/* ── BOTTOM-RIGHT: territory + contacts ──────────────────── */}
        <div style={{position:'absolute',bottom:28,right:10,zIndex:1000}}>
          <div style={{...hudPanel, minWidth:'150px'}}>
            <div style={{display:'flex',alignItems:'center',gap:'6px',marginBottom:'7px'}}>
              <span style={{fontFamily:FONT_MONO,fontSize:'0.65rem',color:COL_ENEMY,fontWeight:700}}>{redPct}%</span>
              <div style={{flex:1,height:'3px',background:'#0a150a',borderRadius:'1px',overflow:'hidden',display:'flex'}}>
                <div style={{width:`${redPct}%`,background:`${COL_ENEMY}99`}} />
                <div style={{width:`${bluePct}%`,background:`${COL_ALLY}99`}} />
              </div>
              <span style={{fontFamily:FONT_MONO,fontSize:'0.65rem',color:COL_ALLY,fontWeight:700}}>{bluePct}%</span>
            </div>
            <div style={{display:'grid',gridTemplateColumns:'auto 1fr',gap:'2px 8px',fontSize:'0.62rem'}}>
              <span style={{color:HUD_DIM}}>AIR</span><span>{airCount}</span>
              <span style={{color:HUD_DIM}}>GND</span><span>{(useLive?activeUnits:activeRestUnits).length - airCount}</span>
              <span style={{color:HUD_DIM}}>OBJ</span><span>{shownObjs.length}</span>
              <span style={{color:HUD_DIM}}>WATCH</span><span style={{color:COL_WATCH}}>{watches.size}</span>
            </div>
          </div>
        </div>

        {/* ── BRAA active info overlay ──────────────────────────────── */}
        {braaStart && braaActiveDist!==null && braaActiveBrg!==null && (
          <div style={{position:'absolute',top:10,left:'50%',transform:'translateX(-50%)',zIndex:1000,pointerEvents:'none'}}>
            <div style={{...hudPanel,display:'flex',gap:'18px',fontSize:'0.7rem',borderColor:`rgba(255,214,0,0.4)`,background:'rgba(4,10,4,0.9)'}}>
              <div style={{textAlign:'center'}}>
                <div style={{color:HUD_DIM,fontSize:'0.55rem',letterSpacing:'0.14em',marginBottom:'2px'}}>BRG</div>
                <div style={{color:COL_WATCH,fontWeight:700}}>{fmtBrg(braaActiveBrg)}</div>
              </div>
              <div style={{textAlign:'center'}}>
                <div style={{color:HUD_DIM,fontSize:'0.55rem',letterSpacing:'0.14em',marginBottom:'2px'}}>RANGE</div>
                <div style={{color:COL_WATCH,fontWeight:700}}>{braaActiveDist.toFixed(1)} NM</div>
              </div>
              <div style={{textAlign:'center',alignSelf:'center'}}>
                <div style={{color:'rgba(255,214,0,0.5)',fontSize:'0.58rem'}}>click or R-click to set</div>
              </div>
            </div>
          </div>
        )}

        {/* ── Measure overlay ───────────────────────────────────────── */}
        {measDist!==null && measBrg!==null && !braaStart && (
          <div style={{position:'absolute',top:10,left:'50%',transform:'translateX(-50%)',zIndex:1000,pointerEvents:'none'}}>
            <div style={{...hudPanel,display:'flex',gap:'18px',fontSize:'0.7rem'}}>
              <div style={{textAlign:'center'}}>
                <div style={{color:HUD_DIM,fontSize:'0.55rem',letterSpacing:'0.14em',marginBottom:'2px'}}>DIST</div>
                <div style={{color:'#00b4f5',fontWeight:700}}>{measDist.toFixed(1)} NM</div>
              </div>
              <div style={{textAlign:'center'}}>
                <div style={{color:HUD_DIM,fontSize:'0.55rem',letterSpacing:'0.14em',marginBottom:'2px'}}>BRG</div>
                <div style={{color:'#f97316',fontWeight:700}}>{fmtBrg(measBrg)}</div>
              </div>
              {speed>0 && (
                <div style={{textAlign:'center'}}>
                  <div style={{color:HUD_DIM,fontSize:'0.55rem',letterSpacing:'0.14em',marginBottom:'2px'}}>ETE</div>
                  <div style={{color:'#a78bfa',fontWeight:700}}>{fmtTime(measDist/speed*60)}</div>
                </div>
              )}
            </div>
          </div>
        )}

        {/* ── Console toggle button (top-left) ─────────────────────── */}
        <button onClick={()=>setShowConsole(v=>!v)} style={{
          position:'absolute', top:10, left:10, zIndex:1000,
          fontFamily:FONT_HEAD, fontSize:'0.7rem', letterSpacing:'0.15em',
          padding:'0.35rem 0.8rem', borderRadius:'2px',
          border:`1px solid ${HUD_BORDER}`,
          background: showConsole?`${GREEN}15`:HUD_BG,
          color: showConsole?GREEN:HUD_DIM,
          cursor:'pointer', backdropFilter:'blur(6px)',
        }}>
          {showConsole ? '◂ GCI' : '▸ GCI'}
        </button>

        {/* ── Scratchpad floating panel ─────────────────────────────── */}
        {showScratch && (
          <div style={{
            position:'absolute', bottom:100, left: showConsole?250:10, zIndex:1000,
            width:'260px', background:PANEL_BG,
            border:`1px solid ${HUD_BORDER}`, borderRadius:'3px',
            backdropFilter:'blur(10px)', display:'flex', flexDirection:'column',
          }}>
            <div style={{
              padding:'6px 10px', borderBottom:`1px solid ${HUD_BORDER}`,
              display:'flex', justifyContent:'space-between', alignItems:'center',
            }}>
              <span style={{fontFamily:FONT_HEAD,fontSize:'0.72rem',letterSpacing:'0.18em',color:GREEN}}>SCRATCHPAD</span>
              <button onClick={()=>setShowScratch(false)}
                style={{background:'none',border:'none',color:HUD_DIM,cursor:'pointer',fontSize:'0.75rem'}}>✕</button>
            </div>
            <textarea
              value={scratchText}
              onChange={e=>setScratchText(e.target.value)}
              placeholder="notes…"
              style={{
                background:'transparent', border:'none', outline:'none',
                color:GREEN, fontFamily:FONT_MONO, fontSize:'0.68rem',
                padding:'8px 10px', resize:'none', minHeight:'140px',
                lineHeight:1.5,
              }}
            />
          </div>
        )}

        {/* Loading overlay */}
        {isLoading && (
          <div style={{position:'absolute',inset:0,zIndex:2000,display:'flex',alignItems:'center',
                       justifyContent:'center',background:'rgba(6,10,6,0.8)',pointerEvents:'none'}}>
            <span style={{fontFamily:FONT_HEAD,fontSize:'1.1rem',letterSpacing:'0.3em',color:GREEN,
                          animation:'pulse 1.2s ease-in-out infinite'}}>
              LOADING MAP…
            </span>
          </div>
        )}
      </div>

      {/* ── Plan panel – flex sibling so it pushes the map ─────────── */}
      {showPlan && (
        <div style={{
          width:'250px', flexShrink:0, display:'flex', flexDirection:'column',
          background:PANEL_BG, borderLeft:`1px solid ${HUD_BORDER}`,
          fontFamily:FONT_MONO, color:HUD_TEXT, overflowY:'auto', zIndex:10,
        }}>
          <div style={{padding:'11px 13px 8px',borderBottom:`1px solid ${HUD_BORDER}`}}>
            <div style={{fontFamily:FONT_HEAD,fontSize:'0.85rem',letterSpacing:'0.2em',color:GREEN}}>MISSION PLAN</div>
          </div>
          <div style={{padding:'9px 13px',borderBottom:`1px solid ${HUD_BORDER}`}}>
            <div style={{fontSize:'0.59rem',color:HUD_DIM,letterSpacing:'0.12em',marginBottom:'4px'}}>SPEED (KTAS)</div>
            <input type="number" min={0} max={2000} value={speed} onChange={e=>setSpeed(Number(e.target.value))}
              style={{width:'100%',background:'rgba(0,20,0,0.5)',border:`1px solid ${HUD_BORDER}`,
                      borderRadius:'2px',color:GREEN,fontFamily:FONT_MONO,fontSize:'0.75rem',
                      padding:'4px 8px',outline:'none',boxSizing:'border-box'}} />
          </div>
          {waypoints.length>0 && (
            <div style={{padding:'9px 13px',borderBottom:`1px solid ${HUD_BORDER}`}}>
              <div style={{fontSize:'0.59rem',color:HUD_DIM,letterSpacing:'0.12em',marginBottom:'5px'}}>
                ROUTE — {totalNm.toFixed(1)} NM{speed>0?` / ${fmtTime(totalNm/speed*60)}`:''}
              </div>
              {waypoints.map((wp,i)=>{
                const leg=legs[i-1]
                return (
                  <div key={wp.id} style={{marginBottom:'4px',fontSize:'0.62rem'}}>
                    {leg && <div style={{color:HUD_DIM,fontSize:'0.58rem',marginBottom:'1px'}}>
                      {leg.dist.toFixed(1)} NM  {fmtBrg(leg.brg)}{speed>0?`  ${fmtTime(leg.ete)}`:''}
                    </div>}
                    <div style={{display:'flex',gap:'6px',alignItems:'center'}}>
                      <span style={{color:'#00b4f5',minWidth:'16px',fontWeight:700}}>{i+1}</span>
                      <span style={{flex:1}}>{fmtCoord(wp.lat,wp.lon)}</span>
                      <button onClick={()=>setWaypoints(p=>p.filter(w=>w.id!==wp.id))}
                        style={{background:'none',border:'none',color:HUD_DIM,cursor:'pointer',padding:'0 2px'}}>✕</button>
                    </div>
                  </div>
                )
              })}
            </div>
          )}
          {planMarkers.length>0 && (
            <div style={{padding:'9px 13px',borderBottom:`1px solid ${HUD_BORDER}`}}>
              <div style={{fontSize:'0.59rem',color:HUD_DIM,letterSpacing:'0.12em',marginBottom:'5px'}}>MARKERS</div>
              {planMarkers.map(m=>(
                <div key={m.id} style={{display:'flex',gap:'6px',alignItems:'center',marginBottom:'3px',fontSize:'0.62rem'}}>
                  <span style={{color:MK_STYLE[m.type].bd,minWidth:'28px',fontWeight:700}}>{m.type}</span>
                  <span style={{flex:1}}>{fmtCoord(m.lat,m.lon)}</span>
                  <button onClick={()=>setPlanMarkers(p=>p.filter(mk=>mk.id!==m.id))}
                    style={{background:'none',border:'none',color:HUD_DIM,cursor:'pointer',padding:'0 2px'}}>✕</button>
                </div>
              ))}
            </div>
          )}
          {showThreats && (
            <div style={{padding:'9px 13px',marginTop:'auto',borderTop:`1px solid ${HUD_BORDER}`}}>
              <div style={{fontSize:'0.59rem',color:HUD_DIM,letterSpacing:'0.12em',marginBottom:'5px'}}>THREAT RINGS</div>
              {Object.entries(THREAT_NM).map(([kind,nm])=>(
                <div key={kind} style={{display:'flex',justifyContent:'space-between',fontSize:'0.62rem',marginBottom:'2px'}}>
                  <span>{kind}</span><span style={{color:HUD_DIM}}>{nm} NM</span>
                </div>
              ))}
            </div>
          )}
        </div>
      )}
    </div>
  )
}
