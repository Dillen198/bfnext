import { Fragment, useCallback, useEffect, useMemo, useRef, useState, type CSSProperties } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import {
  MapContainer, TileLayer, Marker, Polygon, Polyline, CircleMarker,
  Tooltip, useMap, useMapEvents, ScaleControl,
} from 'react-leaflet'
import L from 'leaflet'
import {
  Camera, Trash2, Crosshair, X, Upload, MapPin, Move3d, Eye, EyeOff,
  Grid3x3, Navigation, Play, Pause, SendToBack, ChevronRight,
  MousePointer2, Pencil, Minus, Square, Circle as CircleIcon, Download, Maximize2, Eraser,
} from 'lucide-react'
import html2canvas from 'html2canvas'
import { api, type IntelCapture, type IntelMarkupKind, type Objective, type Frontlines } from '../api'
import { useAuth } from '../context/AuthContext'
import {
  fmtLatLon, fmtGridLabel, niceGridStep, groundFootprint,
  INTEL_TILE_LAYERS, type IntelTileKey, type LatLon,
} from '../lib/geo'
import { warpedGroundQuad } from '../lib/warp'
import IntelWarpOverlay from './IntelWarpOverlay'
import IntelMarkupLayer, { type MarkupTool } from './IntelMarkupLayer'

const MARKUP_COLORS = ['#ef4444', '#f59e0b', '#eab308', '#22c55e', '#3b82f6', '#a855f7', '#ffffff']
const RUN_COLORS = ['#22c55e', '#f59e0b', '#3b82f6', '#a855f7', '#ec4899', '#14b8a6', '#f43f5e', '#84cc16']
function runColor(name: string, override?: string): string {
  if (override) return override
  let h = 0
  for (let i = 0; i < name.length; i++) h = (h * 31 + name.charCodeAt(i)) | 0
  return RUN_COLORS[Math.abs(h) % RUN_COLORS.length]
}

const REFRESH_MS = 20_000
const COL_BLUE = '#4a8fd4'
const COL_RED = '#cc4444'
const COL_NEUTRAL = '#6a7a5a'
const FONT_HEAD = "'Bebas Neue',sans-serif"
const FONT_MONO = "'Share Tech Mono','Courier New',monospace"
const DEFAULT_OPACITY = 0.9

function sideColor(s: string) {
  return s === 'Blue' ? COL_BLUE : s === 'Red' ? COL_RED : COL_NEUTRAL
}

type Dims = { w: number; h: number }

/** Ground quad [TL,TR,BR,BL] a capture's photo should be pinned to. */
function quadFor(c: IntelCapture, dims: Dims | undefined, seed = false): LatLon[] | null {
  if (c.adjust?.corners) return c.adjust.corners as LatLon[]
  if (!c.placed) return null
  const aspect = dims ? dims.w / dims.h : 1
  if (c.alt_ft != null && c.heading_deg != null) {
    const w = warpedGroundQuad({
      lat: c.lat, lon: c.lon, altFt: c.alt_ft, headingDeg: c.heading_deg,
      pitchDeg: c.pitch_deg ?? 0, rollDeg: c.roll_deg ?? 0, aspect,
    })
    if (w) return w
    return groundFootprint(c.lat, c.lon, c.alt_ft, c.heading_deg, aspect)
  }
  return seed ? groundFootprint(c.lat, c.lon, 6000, c.heading_deg ?? 0, aspect) : null
}

// ── Camera marker (rotates to capture heading) ─────────────────────────
function cameraIcon(headingDeg: number | null, color: string): L.DivIcon {
  const rot = headingDeg ?? 0
  return L.divIcon({
    className: '',
    iconSize: [24, 24],
    iconAnchor: [12, 12],
    html: `<div style="width:24px;height:24px;transform:rotate(${rot}deg);filter:drop-shadow(0 1px 2px #000a)">
      <svg viewBox="0 0 24 24" width="24" height="24" fill="none"
           stroke="${color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
        <path d="M12 2 L12 9" stroke="${color}"/>
        <path d="M8 5 L12 1 L16 5" fill="${color}" stroke="${color}"/>
        <rect x="4" y="9" width="16" height="11" rx="2" fill="rgba(0,0,0,0.6)"/>
        <circle cx="12" cy="14.5" r="3.4"/>
      </svg>
    </div>`,
  })
}

const HANDLE_ICON = L.divIcon({
  className: '',
  iconSize: [14, 14],
  iconAnchor: [7, 7],
  html: `<div style="width:14px;height:14px;border:2px solid #fff;background:#000a;border-radius:3px;
           box-shadow:0 0 0 1px #000,0 1px 3px #000"></div>`,
})

// ── Coordinate grid (graticule) ───────────────────────────────────────
function CoordGrid() {
  const map = useMap()
  const [, force] = useState(0)
  useMapEvents({ moveend: () => force(n => n + 1), zoomend: () => force(n => n + 1) })
  const b = map.getBounds()
  const stepLat = niceGridStep(b.getNorth() - b.getSouth())
  const stepLon = niceGridStep(b.getEast() - b.getWest())
  const lats: number[] = []
  const lons: number[] = []
  for (let y = Math.ceil(b.getSouth() / stepLat) * stepLat; y <= b.getNorth(); y += stepLat) lats.push(y)
  for (let x = Math.ceil(b.getWest() / stepLon) * stepLon; x <= b.getEast(); x += stepLon) lons.push(x)
  const line = { color: '#fff', weight: 0.5, opacity: 0.35, interactive: false } as const
  return (
    <>
      {lats.map(y => (
        <Polyline key={`la${y}`} positions={[[y, b.getWest()], [y, b.getEast()]]} pathOptions={line}>
          <Tooltip permanent direction="right" offset={[0, 0]} className="grid-lbl">{fmtGridLabel(y, 'lat')}</Tooltip>
        </Polyline>
      ))}
      {lons.map(x => (
        <Polyline key={`lo${x}`} positions={[[b.getSouth(), x], [b.getNorth(), x]]} pathOptions={line}>
          <Tooltip permanent direction="top" offset={[0, 0]} className="grid-lbl">{fmtGridLabel(x, 'lon')}</Tooltip>
        </Polyline>
      ))}
    </>
  )
}

function FlyToFirst({ captures, objectives }: { captures: IntelCapture[]; objectives: Objective[] }) {
  const map = useMap()
  const done = useRef(false)
  useEffect(() => {
    if (done.current) return
    const placed = captures.filter(c => c.placed)
    if (placed.length) {
      done.current = true
      map.fitBounds(L.latLngBounds(placed.map(c => [c.lat, c.lon] as LatLon)).pad(0.6), { maxZoom: 13 })
    } else if (objectives.length) {
      const valid = objectives.filter(o => o.lat || o.lon)
      if (valid.length) {
        done.current = true
        map.fitBounds(L.latLngBounds(valid.map(o => [o.lat, o.lon] as LatLon)).pad(0.15), { maxZoom: 9 })
      }
    }
  }, [captures, objectives, map])
  return null
}

function PlacementClicks({ onPick }: { onPick: (lat: number, lon: number) => void }) {
  useMapEvents({ click: e => onPick(e.latlng.lat, e.latlng.lng) })
  const map = useMap()
  useEffect(() => {
    const c = map.getContainer()
    c.style.cursor = 'crosshair'
    return () => { c.style.cursor = '' }
  }, [map])
  return null
}

export default function IntelPage() {
  const { user } = useAuth()
  const qc = useQueryClient()
  const ownSide = user?.side ?? null
  const canSwitch = !!user?.is_admin && !ownSide

  const [adminSide, setAdminSide] = useState<'blue' | 'red' | 'all'>('blue')
  const [tileKey, setTileKey] = useState<IntelTileKey | 'grid'>('satellite')
  const [showImagery, setShowImagery] = useState(true)
  const [keyBlack, setKeyBlack] = useState(true)   // drop the black letterbox/matte around TARPS frames
  const [showGrid, setShowGrid] = useState(false)
  const [showPath, setShowPath] = useState(true)
  const [lightbox, setLightbox] = useState<IntelCapture | null>(null)
  const [placing, setPlacing] = useState<IntelCapture | null>(null)
  const [dims, setDims] = useState<Record<string, Dims>>({})
  const [uploadMsg, setUploadMsg] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)

  // Per-capture display state (client-local): stacking order (back → front) and opacity override.
  const [backList, setBackList] = useState<string[]>([])   // ids pushed to the back, oldest-first
  const [opa, setOpa] = useState<Record<string, number>>({})
  // Capture list is grouped by uploader; these track which groups are
  // collapsed and which uploaders are hidden from the map.
  const [collapsed, setCollapsed] = useState<Set<string>>(new Set())
  const [hiddenBy, setHiddenBy] = useState<Set<string>>(new Set())
  const [runColors, setRunColors] = useState<Record<string, string>>(() => {
    try { return JSON.parse(localStorage.getItem('intel_run_colors') || '{}') } catch { return {} }
  })
  useEffect(() => {
    try { localStorage.setItem('intel_run_colors', JSON.stringify(runColors)) } catch { /* quota */ }
  }, [runColors])
  const [globalOpacity, setGlobalOpacity] = useState(1)
  const [selCap, setSelCap] = useState<string | null>(null)
  const [exporting, setExporting] = useState(false)
  const mapWrapRef = useRef<HTMLDivElement | null>(null)

  // Timeline
  const [cutoff, setCutoff] = useState(1)                   // 0..1, show captures up to this point in the run
  const [playing, setPlaying] = useState(false)
  const [speed, setSpeed] = useState(10)

  // Markup
  const [showMarkup, setShowMarkup] = useState(true)
  const [markupTool, setMarkupTool] = useState<MarkupTool>('select')
  const [markupColor, setMarkupColor] = useState(MARKUP_COLORS[0])
  const [markupWidth, setMarkupWidth] = useState(3)
  const [selMarkup, setSelMarkup] = useState<string | null>(null)

  // Warp-align editor
  const [aligning, setAligning] = useState<IntelCapture | null>(null)
  const [alignCorners, setAlignCorners] = useState<LatLon[] | null>(null)
  const [alignOpacity, setAlignOpacity] = useState(DEFAULT_OPACITY)

  const sideParam = canSwitch ? adminSide : undefined
  const bannerSide = canSwitch ? adminSide.toUpperCase() : (ownSide?.toUpperCase() ?? '—')

  const { data: captures = [], isError, error } = useQuery({
    queryKey: ['intel', 'captures', sideParam ?? 'me'],
    queryFn: () => api.intel.captures(sideParam),
    refetchInterval: REFRESH_MS,
  })
  const { data: markup = [] } = useQuery({
    queryKey: ['intel', 'markup', sideParam ?? 'me'],
    queryFn: () => api.intel.markup.list(sideParam),
    refetchInterval: REFRESH_MS,
  })
  const { data: objectives = [] } = useQuery({
    queryKey: ['objectives'], queryFn: () => api.objectives(), refetchInterval: 60_000,
  })
  const { data: frontsRaw } = useQuery({
    queryKey: ['frontline'], queryFn: () => api.frontline(), refetchInterval: 60_000,
  })
  const midLines: LatLon[][] = Array.isArray((frontsRaw as Frontlines | undefined)?.mid)
    ? (frontsRaw as Frontlines).mid as LatLon[][]
    : []

  const refresh = useCallback(() => {
    qc.invalidateQueries({ queryKey: ['intel', 'captures'] })
  }, [qc])
  const refreshMarkup = useCallback(() => {
    qc.invalidateQueries({ queryKey: ['intel', 'markup'] })
  }, [qc])

  const addMarkup = useCallback(async (kind: IntelMarkupKind, points: [number, number][]) => {
    try {
      await api.intel.markup.add({ kind, points, color: markupColor, width: markupWidth })
      refreshMarkup()
    } catch (e) { window.alert(String(e)) }
  }, [markupColor, markupWidth, refreshMarkup])

  const deleteMarkup = useCallback(async (id: string) => {
    try { await api.intel.markup.del(id); setSelMarkup(null); refreshMarkup() }
    catch (e) { window.alert(String(e)) }
  }, [refreshMarkup])

  const exportPng = useCallback(async () => {
    const el = mapWrapRef.current
    if (!el) return
    setExporting(true)
    try {
      const canvas = await html2canvas(el, { useCORS: true, backgroundColor: '#0b0e14', logging: false })
      const a = document.createElement('a')
      a.href = canvas.toDataURL('image/png')
      a.download = `recon-intel-${new Date().toISOString().slice(0, 19).replace(/[:T]/g, '')}.png`
      a.click()
    } catch (e) {
      window.alert(`Export failed: ${e}`)
    } finally {
      setExporting(false)
    }
  }, [])

  // Natural image dimensions → correct footprint / warp aspect ratio.
  useEffect(() => {
    for (const c of captures) {
      if (dims[c.id]) continue
      const img = new Image()
      img.crossOrigin = 'use-credentials'
      img.onload = () => setDims(d => ({ ...d, [c.id]: { w: img.naturalWidth || 1, h: img.naturalHeight || 1 } }))
      img.onerror = () => setDims(d => ({ ...d, [c.id]: { w: 1, h: 1 } }))
      img.src = api.intel.imageUrl(c.id)
    }
  }, [captures, dims])

  const placed = useMemo(() => captures.filter(c => c.placed), [captures])
  const unplaced = useMemo(() => captures.filter(c => !c.placed), [captures])

  // ── Timeline range from capture times ───────────────────────────────
  const times = useMemo(
    () => placed.map(c => (c.captured_at ? Date.parse(c.captured_at) : NaN)).filter(t => !Number.isNaN(t)),
    [placed],
  )
  const [tMin, tMax] = times.length ? [Math.min(...times), Math.max(...times)] : [0, 0]
  const hasTimeline = times.length >= 2 && tMax > tMin
  const cutoffMs = tMin + (tMax - tMin) * cutoff

  useEffect(() => {
    if (!playing || !hasTimeline) return
    const id = setInterval(() => {
      setCutoff(p => {
        const next = p + 0.004 * speed
        if (next >= 1) { setPlaying(false); return 1 }
        return next
      })
    }, 120)
    return () => clearInterval(id)
  }, [playing, hasTimeline, speed])

  const visible = useMemo(() => placed.filter(c => {
    if (hiddenBy.has(c.uploaded_by_name)) return false
    if (!hasTimeline || !c.captured_at) return true
    return Date.parse(c.captured_at) <= cutoffMs + 1
  }), [placed, hasTimeline, cutoffMs, hiddenBy])

  // Group the on-map list by uploader, newest contributor first.
  const groups = useMemo(() => {
    const by = new Map<string, IntelCapture[]>()
    for (const c of placed) {
      const arr = by.get(c.uploaded_by_name) ?? []
      arr.push(c)
      by.set(c.uploaded_by_name, arr)
    }
    const keyTime = (c: IntelCapture) => (c.captured_at ? Date.parse(c.captured_at) : Date.parse(c.uploaded_at))
    return [...by.entries()]
      .map(([name, caps]) => ({
        name,
        caps: [...caps].sort((a, b) => keyTime(a) - keyTime(b)),
        latest: Math.max(...caps.map(c => Date.parse(c.uploaded_at))),
      }))
      .sort((a, b) => b.latest - a.latest)
  }, [placed])

  // Flight path: one polyline per run through its visible captures, in time order.
  const runPaths = useMemo(() => groups
    .map(g => ({
      name: g.name,
      color: runColor(g.name, runColors[g.name]),
      pts: g.caps.filter(c => visible.includes(c)).map(c => [c.lat, c.lon] as LatLon),
    }))
    .filter(rp => rp.pts.length >= 2), [groups, visible, runColors])

  // Stacking: a capture in backList renders behind; earlier in the list = further back.
  const zOf = useCallback((id: string) => {
    const i = backList.indexOf(id)
    return i === -1 ? 500 : i        // pushed-back ids get low z (0..n), everything else 500
  }, [backList])
  const sendBack = useCallback((id: string) => {
    setBackList(l => [...l.filter(x => x !== id), id])
  }, [])

  async function handleFiles(files: FileList | null) {
    if (!files || !files.length) return
    setBusy(true)
    setUploadMsg(null)
    let ok = 0, placedCount = 0, failed = 0
    for (const f of Array.from(files)) {
      try {
        const cap = await api.intel.upload(f, canSwitch && adminSide !== 'all' ? adminSide : undefined)
        ok++
        if (cap.placed) placedCount++
      } catch { failed++ }
    }
    setBusy(false)
    setUploadMsg(
      `${ok} uploaded` +
      (placedCount < ok ? `, ${ok - placedCount} need manual placement` : '') +
      (failed ? `, ${failed} failed` : ''),
    )
    refresh()
  }

  async function doDelete(c: IntelCapture) {
    if (!window.confirm(`Delete recon capture "${c.filename || c.id}"?`)) return
    if (aligning?.id === c.id) cancelAlign()
    try { await api.intel.del(c.id); refresh() } catch (e) { window.alert(String(e)) }
  }

  async function placeAt(lat: number, lon: number) {
    if (!placing) return
    try {
      await api.intel.adjust({ id: placing.id, lat, lon, placed: true })
      setPlacing(null)
      refresh()
    } catch (e) { window.alert(String(e)) }
  }

  function startAlign(c: IntelCapture) {
    setPlacing(null)
    const q = quadFor(c, dims[c.id], true)
    setAligning(c)
    setAlignCorners(q ?? null)
    setAlignOpacity(c.adjust?.opacity ?? opa[c.id] ?? DEFAULT_OPACITY)
  }
  function cancelAlign() { setAligning(null); setAlignCorners(null) }
  async function saveAlign(corners: LatLon[] | null) {
    if (!aligning) return
    const tuple = corners && corners.length === 4
      ? [
          [corners[0][0], corners[0][1]], [corners[1][0], corners[1][1]],
          [corners[2][0], corners[2][1]], [corners[3][0], corners[3][1]],
        ] as [[number, number], [number, number], [number, number], [number, number]]
      : null
    try {
      await api.intel.adjust({ id: aligning.id, adjust: { corners: tuple, opacity: alignOpacity } })
      cancelAlign()
      refresh()
    } catch (e) { window.alert(String(e)) }
  }

  const tile = tileKey === 'grid' ? null : INTEL_TILE_LAYERS[tileKey]
  const gridShown = showGrid || tileKey === 'grid'
  const toggleBtn = (on: boolean): CSSProperties => ({
    padding: '4px 8px', fontSize: '0.6rem', letterSpacing: '0.08em', cursor: 'pointer', borderRadius: 3,
    border: `1px solid ${on ? 'var(--accent)' : 'var(--border)'}`,
    background: on ? 'var(--accent)' : 'rgba(0,0,0,0.55)',
    color: on ? '#fff' : 'var(--text-dim)', display: 'flex', alignItems: 'center', gap: 4,
  })

  return (
    <div style={{ position: 'relative', flex: 1, display: 'flex', overflow: 'hidden' }}>
      {/* ── Side panel ─────────────────────────────────────────────── */}
      <div style={{
        width: 300, flexShrink: 0, background: 'var(--bg-card)', borderRight: '1px solid var(--border)',
        display: 'flex', flexDirection: 'column', overflow: 'hidden',
      }}>
        <div style={{ padding: '12px 14px', borderBottom: '1px solid var(--border)' }}>
          <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
            <Camera size={15} style={{ color: 'var(--accent)' }} />
            <span style={{ fontFamily: FONT_HEAD, letterSpacing: '0.12em', fontSize: '1rem' }}>RECON INTEL</span>
          </div>
          <div style={{ marginTop: 6, fontSize: '0.68rem', letterSpacing: '0.1em', color: 'var(--text-muted)' }}>
            {canSwitch ? (
              <div style={{ display: 'flex', gap: 4 }}>
                {(['blue', 'red', 'all'] as const).map(s => (
                  <button key={s} onClick={() => setAdminSide(s)} style={{
                    flex: 1, padding: '4px 0', fontSize: '0.6rem', letterSpacing: '0.1em', cursor: 'pointer',
                    border: `1px solid ${adminSide === s ? 'var(--accent)' : 'var(--border)'}`,
                    background: adminSide === s ? 'var(--accent)' : 'transparent',
                    color: adminSide === s ? '#fff' : 'var(--text-dim)', borderRadius: 3,
                  }}>{s.toUpperCase()}</button>
                ))}
              </div>
            ) : (
              <span>COALITION: <strong style={{ color: sideColor(ownSide ?? '') }}>{bannerSide}</strong></span>
            )}
          </div>
        </div>

        {/* Upload */}
        <div style={{ padding: '12px 14px', borderBottom: '1px solid var(--border)' }}>
          <label style={{
            display: 'flex', alignItems: 'center', justifyContent: 'center', gap: 8,
            padding: '9px 0', border: '1px dashed var(--border-light)', borderRadius: 4,
            cursor: busy ? 'wait' : 'pointer', fontSize: '0.72rem', letterSpacing: '0.08em',
            color: 'var(--text-muted)', background: 'var(--bg-elevated)',
          }}>
            <Upload size={13} />
            {busy ? 'UPLOADING…' : 'UPLOAD TARPS PHOTOS'}
            <input type="file" accept="image/*" multiple hidden disabled={busy}
              onChange={e => { handleFiles(e.target.files); e.target.value = '' }} />
          </label>
          <div style={{ marginTop: 6, fontSize: '0.6rem', color: 'var(--text-dim)', lineHeight: 1.5 }}>
            F-14 TARPS screenshots — position &amp; attitude are read from the filename.
          </div>
          {uploadMsg && <div style={{ marginTop: 6, fontSize: '0.66rem', color: 'var(--accent)' }}>{uploadMsg}</div>}
        </div>

        {/* Needs placement */}
        {unplaced.length > 0 && (
          <div style={{ padding: '10px 14px', borderBottom: '1px solid var(--border)' }}>
            <div style={{ fontSize: '0.6rem', letterSpacing: '0.12em', color: '#eab308', marginBottom: 6 }}>
              NEEDS PLACEMENT ({unplaced.length})
            </div>
            {unplaced.map(c => (
              <div key={c.id} style={{ display: 'flex', alignItems: 'center', gap: 6, padding: '4px 0', fontSize: '0.66rem' }}>
                <button onClick={() => setPlacing(c)} title="Place on map" style={{
                  display: 'flex', alignItems: 'center', gap: 4, padding: '3px 6px', cursor: 'pointer',
                  border: `1px solid ${placing?.id === c.id ? 'var(--accent)' : 'var(--border)'}`,
                  background: placing?.id === c.id ? 'var(--accent)' : 'transparent',
                  color: placing?.id === c.id ? '#fff' : 'var(--text-dim)', borderRadius: 3,
                }}>
                  <MapPin size={11} /> DROP
                </button>
                <span style={{
                  flex: 1, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
                  color: 'var(--text-muted)', cursor: 'pointer',
                }} onClick={() => setLightbox(c)}>{c.filename || c.id}</span>
                {c.mine && (
                  <button onClick={() => doDelete(c)} style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)' }}>
                    <Trash2 size={12} />
                  </button>
                )}
              </div>
            ))}
          </div>
        )}

        {/* Markup toolbar */}
        {showMarkup && (
          <div style={{ padding: '10px 14px', borderBottom: '1px solid var(--border)' }}>
            <div style={{ fontSize: '0.6rem', letterSpacing: '0.12em', color: 'var(--text-dim)', marginBottom: 6 }}>MARKUP</div>
            <div style={{ display: 'flex', gap: 4, flexWrap: 'wrap' }}>
              {([
                ['select', MousePointer2], ['pencil', Pencil], ['line', Minus],
                ['rect', Square], ['circle', CircleIcon], ['x', X],
              ] as [MarkupTool, typeof X][]).map(([t, Ico]) => (
                <button key={t} onClick={() => { setMarkupTool(t); setSelMarkup(null) }} title={t}
                  style={{
                    width: 30, height: 26, display: 'flex', alignItems: 'center', justifyContent: 'center',
                    cursor: 'pointer', borderRadius: 3,
                    border: `1px solid ${markupTool === t ? 'var(--accent)' : 'var(--border)'}`,
                    background: markupTool === t ? 'var(--accent)' : 'transparent',
                    color: markupTool === t ? '#fff' : 'var(--text-dim)',
                  }}>
                  <Ico size={13} />
                </button>
              ))}
            </div>
            <div style={{ display: 'flex', gap: 4, marginTop: 8, alignItems: 'center' }}>
              {MARKUP_COLORS.map(c => (
                <button key={c} onClick={() => setMarkupColor(c)} style={{
                  width: 18, height: 18, borderRadius: 3, cursor: 'pointer', background: c,
                  border: markupColor === c ? '2px solid var(--text)' : '1px solid var(--border)',
                }} />
              ))}
            </div>
            <div style={{ display: 'flex', gap: 8, marginTop: 8, alignItems: 'center' }}>
              <span style={{ fontSize: '0.58rem', color: 'var(--text-dim)' }}>WIDTH</span>
              <input type="range" min={1} max={10} step={1} value={markupWidth}
                onChange={e => setMarkupWidth(Number(e.target.value))} style={{ flex: 1 }} />
              {selMarkup && (markup.find(m => m.id === selMarkup)?.mine || user?.is_admin) && (
                <button onClick={() => deleteMarkup(selMarkup)} title="Delete selected"
                  style={{ background: '#7f1d1d', border: 'none', color: '#fff', borderRadius: 3, padding: '3px 6px', cursor: 'pointer', fontSize: '0.6rem' }}>
                  <Trash2 size={11} />
                </button>
              )}
            </div>
          </div>
        )}

        {/* Capture list — grouped by uploader */}
        <div style={{ flex: 1, overflowY: 'auto' }}>
          <div style={{ fontSize: '0.6rem', letterSpacing: '0.12em', color: 'var(--text-dim)', padding: '10px 14px 6px' }}>
            ON MAP ({visible.length}{visible.length !== placed.length ? ` / ${placed.length}` : ''})
          </div>
          {placed.length === 0 && (
            <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', lineHeight: 1.6, padding: '0 14px' }}>
              No recon captures yet. Upload TARPS photos to build the picture.
            </div>
          )}
          {groups.map(g => {
            const isOpen = !collapsed.has(g.name)
            const hidden = hiddenBy.has(g.name)
            const last = new Date(g.latest)
            return (
              <div key={g.name}>
                {/* group header */}
                <div style={{
                  display: 'flex', alignItems: 'center', gap: 6, padding: '7px 14px',
                  background: 'var(--bg-elevated)', borderTop: '1px solid var(--border)',
                  borderBottom: isOpen ? '1px solid var(--border)' : 'none', cursor: 'pointer', fontSize: '0.68rem',
                }} onClick={() => setCollapsed(s => { const n = new Set(s); if (n.has(g.name)) n.delete(g.name); else n.add(g.name); return n })}>
                  <span style={{ color: 'var(--text-dim)', transform: isOpen ? 'rotate(90deg)' : 'none', transition: 'transform 0.12s' }}>
                    <ChevronRight size={12} />
                  </span>
                  <label onClick={e => e.stopPropagation()} title="Run colour"
                    style={{ width: 12, height: 12, borderRadius: 3, flexShrink: 0, cursor: 'pointer', position: 'relative', background: runColor(g.name, runColors[g.name]) }}>
                    <input type="color" value={runColor(g.name, runColors[g.name])}
                      onChange={e => setRunColors(rc => ({ ...rc, [g.name]: e.target.value }))}
                      style={{ position: 'absolute', inset: 0, opacity: 0, cursor: 'pointer' }} />
                  </label>
                  <span style={{ flex: 1, color: 'var(--text)', fontWeight: 600, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                    {g.name}
                  </span>
                  <span style={{ color: 'var(--text-dim)', fontFamily: FONT_MONO, fontSize: '0.6rem' }}>
                    {g.caps.length} · {last.toLocaleDateString([], { month: 'short', day: 'numeric' })} {last.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                  </span>
                  <button
                    onClick={e => { e.stopPropagation(); setHiddenBy(s => { const n = new Set(s); if (n.has(g.name)) n.delete(g.name); else n.add(g.name); return n }) }}
                    title={hidden ? 'Show on map' : 'Hide from map'}
                    style={{ background: 'none', border: 'none', cursor: 'pointer', color: hidden ? 'var(--text-dim)' : 'var(--accent)' }}>
                    {hidden ? <EyeOff size={12} /> : <Eye size={12} />}
                  </button>
                </div>

                {/* group rows */}
                {isOpen && g.caps.map(c => {
                  const dim = !visible.includes(c)
                  return (
                    <div key={c.id} style={{
                      padding: '6px 14px', borderBottom: '1px solid var(--border)', fontSize: '0.66rem',
                      display: 'flex', flexDirection: 'column', gap: 3, opacity: dim ? 0.4 : 1,
                      background: aligning?.id === c.id ? 'var(--bg-elevated)' : undefined,
                    }}>
                      <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                        <span style={{
                          flex: 1, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
                          color: 'var(--text)', cursor: 'pointer',
                        }} onClick={() => setLightbox(c)}>{c.filename || 'capture'}</span>
                        <button onClick={() => sendBack(c.id)} title="Send behind other photos"
                          style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)' }}>
                          <SendToBack size={12} />
                        </button>
                        {c.mine && (
                          <button onClick={() => (aligning?.id === c.id ? cancelAlign() : startAlign(c))}
                            title="Align photo to the map"
                            style={{ background: 'none', border: 'none', cursor: 'pointer', color: aligning?.id === c.id ? 'var(--accent)' : 'var(--text-dim)' }}>
                            <Move3d size={13} />
                          </button>
                        )}
                        {c.mine && (
                          <button onClick={() => doDelete(c)} title="Delete" style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)' }}>
                            <Trash2 size={12} />
                          </button>
                        )}
                      </div>
                      <div style={{ color: 'var(--text-dim)', fontFamily: FONT_MONO }}>
                        {fmtLatLon(c.lat, c.lon)}
                        {c.captured_at && ` · shot ${new Date(c.captured_at).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit' })}`}
                        {c.adjust?.corners && ' · aligned'}
                      </div>
                      <input type="range" min={0.15} max={1} step={0.05}
                        value={opa[c.id] ?? c.adjust?.opacity ?? DEFAULT_OPACITY}
                        onChange={e => setOpa(o => ({ ...o, [c.id]: Number(e.target.value) }))}
                        style={{ width: '100%', height: 3 }} />
                    </div>
                  )
                })}
              </div>
            )
          })}
        </div>

        {/* SELECTED capture details */}
        {selCap && (() => {
          const c = placed.find(x => x.id === selCap)
          if (!c) return null
          return (
            <div style={{ flexShrink: 0, borderTop: '1px solid var(--border)', background: 'var(--bg-elevated)', padding: '10px 14px', fontSize: '0.66rem' }}>
              <div style={{ display: 'flex', alignItems: 'center', gap: 6, marginBottom: 6 }}>
                <span style={{ fontSize: '0.6rem', letterSpacing: '0.12em', color: 'var(--text-dim)', flex: 1 }}>SELECTED</span>
                <button onClick={() => setLightbox(c)} title="View full photo"
                  style={{ background: 'var(--accent)', border: 'none', color: '#fff', borderRadius: 3, padding: '2px 6px', cursor: 'pointer', display: 'flex', alignItems: 'center', gap: 3, fontSize: '0.58rem' }}>
                  <Maximize2 size={10} /> VIEW
                </button>
                <button onClick={() => setSelCap(null)} style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)' }}><X size={12} /></button>
              </div>
              <div style={{ display: 'grid', gridTemplateColumns: 'auto 1fr', gap: '2px 8px', fontFamily: FONT_MONO, color: 'var(--text-muted)' }}>
                <span style={{ color: 'var(--text-dim)' }}>File</span><span style={{ overflow: 'hidden', textOverflow: 'ellipsis' }}>{c.filename || '—'}</span>
                <span style={{ color: 'var(--text-dim)' }}>Run</span><span>{c.uploaded_by_name}</span>
                <span style={{ color: 'var(--text-dim)' }}>Pos</span><span>{fmtLatLon(c.lat, c.lon)}</span>
                <span style={{ color: 'var(--text-dim)' }}>Alt</span><span>{c.alt_ft != null ? `${Math.round(c.alt_ft)} ft` : '—'}</span>
                <span style={{ color: 'var(--text-dim)' }}>Hdg</span><span>{c.heading_deg != null ? `${Math.round(c.heading_deg)}°` : '—'}</span>
                <span style={{ color: 'var(--text-dim)' }}>Att</span><span>{c.pitch_deg != null ? `pitch ${c.pitch_deg}°  roll ${c.roll_deg ?? 0}°` : '—'}</span>
                <span style={{ color: 'var(--text-dim)' }}>Shot</span><span>{c.captured_at ? new Date(c.captured_at).toLocaleString() : '—'}</span>
              </div>
            </div>
          )
        })()}
      </div>

      {/* ── Map ───────────────────────────────────────────────────── */}
      <div style={{ position: 'relative', flex: 1, display: 'flex', flexDirection: 'column' }}>
        {isError && (
          <div style={{
            position: 'absolute', zIndex: 1000, top: 10, left: '50%', transform: 'translateX(-50%)',
            background: 'rgba(120,20,20,0.9)', color: '#fff', padding: '6px 12px', borderRadius: 4, fontSize: '0.7rem',
          }}>{String((error as Error)?.message ?? 'failed to load intel')}</div>
        )}

        {/* Top-right controls */}
        <div style={{ position: 'absolute', zIndex: 1000, top: 10, right: 10, display: 'flex', gap: 4, flexWrap: 'wrap', justifyContent: 'flex-end', maxWidth: 380 }}>
          <button onClick={() => setShowImagery(v => !v)} title="Toggle photo overlays" style={toggleBtn(showImagery)}>
            {showImagery ? <Eye size={11} /> : <EyeOff size={11} />} PHOTOS
          </button>
          <button onClick={() => setKeyBlack(v => !v)} title="Drop the black matte around each TARPS frame" style={toggleBtn(keyBlack)}>
            <Eraser size={11} /> DE-MATTE
          </button>
          <button onClick={() => setShowPath(v => !v)} title="Toggle flight-path markers" style={toggleBtn(showPath)}>
            <Navigation size={11} /> PATH
          </button>
          <button onClick={() => setShowGrid(v => !v)} title="Toggle coordinate grid" style={toggleBtn(showGrid)}>
            <Grid3x3 size={11} /> GRID
          </button>
          <button onClick={() => { setShowMarkup(v => !v); setMarkupTool('select') }} title="Toggle markup layer / tools" style={toggleBtn(showMarkup)}>
            <Pencil size={11} /> MARKUP
          </button>
          {([...(Object.keys(INTEL_TILE_LAYERS) as IntelTileKey[]), 'grid'] as const).map(k => (
            <button key={k} onClick={() => setTileKey(k)} style={{
              ...toggleBtn(tileKey === k), gap: 0,
            }}>{k === 'grid' ? 'GRID MAP' : INTEL_TILE_LAYERS[k].label}</button>
          ))}
          <button onClick={exportPng} disabled={exporting} title="Export the current view as PNG"
            style={{ ...toggleBtn(false), opacity: exporting ? 0.5 : 1 }}>
            <Download size={11} /> {exporting ? '…' : 'PNG'}
          </button>
          <label style={{ ...toggleBtn(false), gap: 5, cursor: 'default' }} title="Overall photo opacity">
            OPACITY
            <input type="range" min={0.1} max={1} step={0.05} value={globalOpacity}
              onChange={e => setGlobalOpacity(Number(e.target.value))} style={{ width: 64 }} />
          </label>
        </div>

        {placing && (
          <div style={{
            position: 'absolute', zIndex: 1000, top: 10, left: '50%', transform: 'translateX(-50%)',
            background: 'rgba(0,0,0,0.8)', color: '#fff', padding: '6px 12px', borderRadius: 4, fontSize: '0.7rem',
            display: 'flex', alignItems: 'center', gap: 10,
          }}>
            <Crosshair size={13} /> Click the map to place “{placing.filename || placing.id}”
            <button onClick={() => setPlacing(null)} style={{ background: 'none', border: 'none', color: '#fff', cursor: 'pointer' }}>
              <X size={13} />
            </button>
          </div>
        )}

        {aligning && (
          <div style={{
            position: 'absolute', zIndex: 1000, top: 10, left: '50%', transform: 'translateX(-50%)',
            background: 'rgba(0,0,0,0.85)', color: '#fff', padding: '8px 14px', borderRadius: 4, fontSize: '0.7rem',
            display: 'flex', alignItems: 'center', gap: 14, fontFamily: FONT_MONO,
          }}>
            <span style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
              <Move3d size={13} /> Drag the corners onto features
            </span>
            <label style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
              opacity
              <input type="range" min={0.15} max={1} step={0.05} value={alignOpacity}
                onChange={e => setAlignOpacity(Number(e.target.value))} style={{ width: 90 }} />
            </label>
            <button onClick={() => saveAlign(alignCorners)} style={btn('#16a34a')}>SAVE</button>
            <button onClick={() => saveAlign(null)} title="Back to auto projection" style={btn('#555')}>RESET</button>
            <button onClick={cancelAlign} style={btn('#555')}>CANCEL</button>
          </div>
        )}

        <div ref={mapWrapRef} style={{ flex: 1, position: 'relative' }}>
          <MapContainer center={[35, 40]} zoom={6} style={{ height: '100%', width: '100%' }}>
            {tile && <TileLayer url={tile.url} attribution={tile.attr} maxZoom={18} crossOrigin="" />}
            <ScaleControl position="bottomleft" />
            <FlyToFirst captures={captures} objectives={objectives} />
            {placing && <PlacementClicks onPick={placeAt} />}
            {gridShown && <CoordGrid />}
            <IntelMarkupLayer
              items={markup}
              tool={showMarkup ? markupTool : 'select'}
              color={markupColor}
              width={markupWidth}
              selectedId={selMarkup}
              onAdd={addMarkup}
              onSelect={setSelMarkup}
              visible={showMarkup}
            />

            {midLines.map((line, i) => (
              <Polyline key={`m${i}`} positions={line} pathOptions={{ color: '#ddd', weight: 1, opacity: 0.5, dashArray: '4 4' }} />
            ))}
            {objectives.filter(o => o.lat || o.lon).map(o => (
              <CircleMarker key={o.id} center={[o.lat, o.lon]} radius={4}
                pathOptions={{ color: sideColor(o.owner), fillColor: sideColor(o.owner), fillOpacity: 0.5, weight: 1 }}>
                <Tooltip>{o.name} · {o.kind}</Tooltip>
              </CircleMarker>
            ))}

            {/* Flight path per run */}
            {showPath && runPaths.map(rp => (
              <Polyline key={`rp-${rp.name}`} positions={rp.pts}
                pathOptions={{ color: rp.color, weight: 2, opacity: 0.6 }} />
            ))}

            {/* Recon captures */}
            {placed.map(c => {
              const col = runColor(c.uploaded_by_name, runColors[c.uploaded_by_name])
              const isAligning = aligning?.id === c.id
              const isSel = selCap === c.id
              const shown = isAligning || visible.includes(c)
              const quad: LatLon[] | null = isAligning ? alignCorners : quadFor(c, dims[c.id])
              const d = dims[c.id]
              const hasQuad = !!quad && quad.length === 4
              const showWarp = shown && showImagery && !!d && hasQuad
              const baseOpacity = isAligning ? alignOpacity : (opa[c.id] ?? c.adjust?.opacity ?? DEFAULT_OPACITY)
              const opacity = baseOpacity * globalOpacity
              return (
                <Fragment key={c.id}>
                  {showWarp && d && quad && (
                    <IntelWarpOverlay
                      url={api.intel.imageUrl(c.id)}
                      corners={quad}
                      naturalW={d.w}
                      naturalH={d.h}
                      opacity={opacity}
                      interactive={!isAligning}
                      keyBlack={keyBlack}
                      zIndex={zOf(c.id)}
                      onClick={() => setSelCap(c.id)}
                      onContextMenu={() => sendBack(c.id)}
                    />
                  )}
                  {shown && hasQuad && quad && (
                    <Polygon positions={quad}
                      pathOptions={{
                        color: isAligning ? '#fff' : col,
                        weight: isAligning || isSel ? 2.5 : 1,
                        opacity: isAligning ? 0.9 : (isSel ? 0.95 : (showWarp ? 0.4 : 0.75)),
                        fillOpacity: showWarp ? 0 : 0.08,
                        dashArray: isAligning ? '6 4' : undefined,
                      }}
                      eventHandlers={{
                        click: () => !isAligning && setSelCap(c.id),
                        contextmenu: () => sendBack(c.id),
                      }} />
                  )}
                  {shown && showPath && !isAligning && (() => {
                    const tip = (
                      <Tooltip direction="top" offset={[0, -10]}>
                        <div style={{ fontFamily: FONT_MONO, fontSize: '0.7rem' }}>
                          <div>{c.filename || 'capture'}</div>
                          <div>{fmtLatLon(c.lat, c.lon)}</div>
                          {c.alt_ft != null && <div>ALT {Math.round(c.alt_ft)}ft · HDG {Math.round(c.heading_deg ?? 0)}</div>}
                          {c.pitch_deg != null && <div>PITCH {c.pitch_deg}° · ROLL {c.roll_deg ?? 0}°</div>}
                        </div>
                      </Tooltip>
                    )
                    const handlers = { click: () => setSelCap(c.id), contextmenu: () => sendBack(c.id) }
                    // Full heading-camera glyph only for the selected shot; every
                    // other capture is a small dot so a dense run doesn't smear
                    // into a bar of camera icons.
                    return isSel
                      ? <Marker position={[c.lat, c.lon]} icon={cameraIcon(c.heading_deg, col)} eventHandlers={handlers}>{tip}</Marker>
                      : <CircleMarker center={[c.lat, c.lon]} radius={3}
                          pathOptions={{ color: col, fillColor: col, fillOpacity: 0.9, weight: 1 }}
                          eventHandlers={handlers}>{tip}</CircleMarker>
                  })()}
                  {isAligning && hasQuad && quad && quad.map((corner, ci) => (
                    <Marker key={ci} position={corner} icon={HANDLE_ICON} draggable
                      eventHandlers={{
                        drag: e => {
                          const ll = (e.target as L.Marker).getLatLng()
                          setAlignCorners(prev => prev ? prev.map((p, j) => (j === ci ? [ll.lat, ll.lng] : p)) : prev)
                        },
                      }} />
                  ))}
                </Fragment>
              )
            })}
          </MapContainer>
        </div>

        {/* ── Timeline ─────────────────────────────────────────────── */}
        {hasTimeline && (
          <div style={{
            flexShrink: 0, background: 'var(--bg-card)', borderTop: '1px solid var(--border)',
            padding: '8px 14px', display: 'flex', alignItems: 'center', gap: 12,
          }}>
            <button onClick={() => { if (cutoff >= 1) setCutoff(0); setPlaying(p => !p) }} style={{
              background: 'var(--accent)', border: 'none', color: '#fff', borderRadius: 4,
              width: 26, height: 26, display: 'flex', alignItems: 'center', justifyContent: 'center', cursor: 'pointer',
            }}>
              {playing ? <Pause size={13} /> : <Play size={13} />}
            </button>
            <div style={{ flex: 1, position: 'relative' }}>
              {/* per-capture ticks */}
              <div style={{ position: 'absolute', inset: '0 0 auto 0', height: 6, pointerEvents: 'none' }}>
                {times.map((t, i) => (
                  <span key={i} style={{
                    position: 'absolute', left: `${((t - tMin) / (tMax - tMin)) * 100}%`,
                    width: 2, height: 6, background: 'var(--text-dim)', transform: 'translateX(-1px)',
                  }} />
                ))}
              </div>
              <input type="range" min={0} max={1} step={0.002} value={cutoff}
                onChange={e => { setPlaying(false); setCutoff(Number(e.target.value)) }}
                style={{ width: '100%' }} />
            </div>
            <select value={speed} onChange={e => setSpeed(Number(e.target.value))} className="vs-input"
              style={{ fontSize: '0.62rem', padding: '2px 4px', height: 22 }}>
              {[1, 5, 10, 30, 60].map(s => <option key={s} value={s}>{s}×</option>)}
            </select>
            <span style={{ fontFamily: FONT_MONO, fontSize: '0.66rem', color: 'var(--text-dim)', minWidth: 74, textAlign: 'right' }}>
              {new Date(cutoffMs).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit' })}
            </span>
          </div>
        )}
      </div>

      {lightbox && <Lightbox capture={lightbox} onClose={() => setLightbox(null)} />}
    </div>
  )
}

function btn(bg: string): CSSProperties {
  return {
    padding: '4px 10px', background: bg, color: '#fff', border: 'none', borderRadius: 3,
    cursor: 'pointer', fontSize: '0.62rem', letterSpacing: '0.1em', fontFamily: FONT_HEAD,
  }
}

// ── Full-image viewer with wheel-zoom / drag-pan ──────────────────────
function Lightbox({ capture, onClose }: { capture: IntelCapture; onClose: () => void }) {
  const [zoom, setZoom] = useState(1)
  const [pan, setPan] = useState({ x: 0, y: 0 })
  const [dragging, setDragging] = useState(false)
  const drag = useRef<{ x: number; y: number } | null>(null)

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') onClose() }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [onClose])

  return (
    <div onClick={onClose} style={{
      position: 'fixed', inset: 0, zIndex: 4000, background: 'rgba(0,0,0,0.88)',
      display: 'flex', alignItems: 'center', justifyContent: 'center', overflow: 'hidden',
    }}>
      <button onClick={onClose} style={{
        position: 'absolute', top: 16, right: 16, zIndex: 4100, background: 'rgba(0,0,0,0.5)',
        border: '1px solid #555', color: '#fff', borderRadius: 4, padding: 6, cursor: 'pointer',
      }}><X size={16} /></button>

      <div style={{
        position: 'absolute', top: 16, left: 16, zIndex: 4100, color: '#ddd', fontFamily: FONT_MONO,
        fontSize: '0.72rem', maxWidth: '60%', lineHeight: 1.5, pointerEvents: 'none',
      }}>
        <div>{capture.filename || 'capture'}</div>
        {capture.placed && <div>{fmtLatLon(capture.lat, capture.lon)}</div>}
        <div>{capture.uploaded_by_name}{capture.captured_at && ` · ${new Date(capture.captured_at).toLocaleString()}`}</div>
      </div>

      <img
        src={api.intel.imageUrl(capture.id)}
        alt={capture.filename}
        crossOrigin="use-credentials"
        draggable={false}
        onClick={e => e.stopPropagation()}
        onWheel={e => setZoom(z => Math.min(8, Math.max(0.25, z * (e.deltaY < 0 ? 1.15 : 1 / 1.15))))}
        onMouseDown={e => { drag.current = { x: e.clientX - pan.x, y: e.clientY - pan.y }; setDragging(true) }}
        onMouseMove={e => { if (drag.current) setPan({ x: e.clientX - drag.current.x, y: e.clientY - drag.current.y }) }}
        onMouseUp={() => { drag.current = null; setDragging(false) }}
        onMouseLeave={() => { drag.current = null; setDragging(false) }}
        style={{
          maxWidth: '92vw', maxHeight: '92vh', userSelect: 'none',
          transform: `translate(${pan.x}px, ${pan.y}px) scale(${zoom})`,
          cursor: dragging ? 'grabbing' : 'grab', transition: dragging ? 'none' : 'transform 0.08s',
        }}
      />
    </div>
  )
}
