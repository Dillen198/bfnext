import { Fragment, useCallback, useEffect, useMemo, useRef, useState, type CSSProperties } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import {
  MapContainer, TileLayer, Marker, Polygon, Polyline, CircleMarker,
  Tooltip, useMap, useMapEvents, ScaleControl,
} from 'react-leaflet'
import L from 'leaflet'
import { Camera, Trash2, Crosshair, X, Upload, MapPin, Move3d, Eye, EyeOff } from 'lucide-react'
import { api, type IntelCapture, type Objective, type Frontlines } from '../api'
import { useAuth } from '../context/AuthContext'
import {
  fmtLatLon, groundFootprint, INTEL_TILE_LAYERS, type IntelTileKey, type LatLon,
} from '../lib/geo'
import { warpedGroundQuad } from '../lib/warp'
import IntelWarpOverlay from './IntelWarpOverlay'

const REFRESH_MS = 20_000
const COL_BLUE = '#4a8fd4'
const COL_RED = '#cc4444'
const COL_NEUTRAL = '#6a7a5a'
const FONT_HEAD = "'Bebas Neue',sans-serif"
const FONT_MONO = "'Share Tech Mono','Courier New',monospace"
const DEFAULT_OPACITY = 0.85

function sideColor(s: string) {
  return s === 'Blue' ? COL_BLUE : s === 'Red' ? COL_RED : COL_NEUTRAL
}

type Dims = { w: number; h: number }

/** Ground quad [TL,TR,BR,BL] a capture's photo should be pinned to: an
 *  explicit manual override, else the pinhole projection, else the flat
 *  footprint. `null` when there's not enough metadata to place imagery. */
function quadFor(c: IntelCapture, dims: Dims | undefined, seed = false): LatLon[] | null {
  if (c.adjust?.corners) return c.adjust.corners as LatLon[]
  if (!c.placed) return null
  const aspect = dims ? dims.w / dims.h : 1.5
  if (c.alt_ft != null && c.heading_deg != null) {
    const w = warpedGroundQuad({
      lat: c.lat, lon: c.lon, altFt: c.alt_ft, headingDeg: c.heading_deg,
      pitchDeg: c.pitch_deg ?? 0, rollDeg: c.roll_deg ?? 0, aspect,
    })
    if (w) return w
    return groundFootprint(c.lat, c.lon, c.alt_ft, c.heading_deg, aspect)
  }
  // No attitude/altitude — only seed a box when the align editor asks for one.
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

function FlyToFirst({ captures, objectives }: { captures: IntelCapture[]; objectives: Objective[] }) {
  const map = useMap()
  const done = useRef(false)
  useEffect(() => {
    if (done.current) return
    const placed = captures.filter(c => c.placed)
    if (placed.length) {
      done.current = true
      map.fitBounds(L.latLngBounds(placed.map(c => [c.lat, c.lon] as LatLon)).pad(0.4), { maxZoom: 12 })
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
  // A registered player is locked to their own coalition. Only a dashboard
  // admin with no in-game side of their own gets the Blue/Red/All switcher.
  const ownSide = user?.side ?? null
  const canSwitch = !!user?.is_admin && !ownSide

  const [adminSide, setAdminSide] = useState<'blue' | 'red' | 'all'>('blue')
  const [tileKey, setTileKey] = useState<IntelTileKey>('satellite')
  const [showImagery, setShowImagery] = useState(true)
  const [lightbox, setLightbox] = useState<IntelCapture | null>(null)
  const [placing, setPlacing] = useState<IntelCapture | null>(null)
  const [dims, setDims] = useState<Record<string, Dims>>({})
  const [uploadMsg, setUploadMsg] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)

  // Warp-align editor
  const [aligning, setAligning] = useState<IntelCapture | null>(null)
  const [alignCorners, setAlignCorners] = useState<LatLon[] | null>(null)
  const [alignOpacity, setAlignOpacity] = useState(DEFAULT_OPACITY)

  // When not switching, omit ?side= entirely — the server forces our own side.
  const sideParam = canSwitch ? adminSide : undefined
  const bannerSide = canSwitch ? adminSide.toUpperCase() : (ownSide?.toUpperCase() ?? '—')

  const { data: captures = [], isError, error } = useQuery({
    queryKey: ['intel', 'captures', sideParam ?? 'me'],
    queryFn: () => api.intel.captures(sideParam),
    refetchInterval: REFRESH_MS,
  })
  const { data: objectives = [] } = useQuery({
    queryKey: ['objectives'], queryFn: () => api.objectives(), refetchInterval: 60_000,
  })
  const { data: frontsRaw } = useQuery({
    queryKey: ['frontline'], queryFn: () => api.frontline(), refetchInterval: 60_000,
  })
  // The endpoint returns `[]` (not an object) when there's no round yet — be defensive.
  const midLines: LatLon[][] = Array.isArray((frontsRaw as Frontlines | undefined)?.mid)
    ? (frontsRaw as Frontlines).mid as LatLon[][]
    : []

  const refresh = useCallback(() => {
    qc.invalidateQueries({ queryKey: ['intel', 'captures'] })
  }, [qc])

  // Natural image dimensions → correct footprint / warp aspect ratio.
  useEffect(() => {
    for (const c of captures) {
      if (dims[c.id]) continue
      const img = new Image()
      img.onload = () => setDims(d => ({ ...d, [c.id]: { w: img.naturalWidth || 3, h: img.naturalHeight || 2 } }))
      img.onerror = () => setDims(d => ({ ...d, [c.id]: { w: 3, h: 2 } }))
      img.src = api.intel.imageUrl(c.id)
    }
  }, [captures, dims])

  const placed = useMemo(() => captures.filter(c => c.placed), [captures])
  const unplaced = useMemo(() => captures.filter(c => !c.placed), [captures])

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
      } catch {
        failed++
      }
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
    setAlignOpacity(c.adjust?.opacity ?? DEFAULT_OPACITY)
  }
  function cancelAlign() {
    setAligning(null)
    setAlignCorners(null)
  }
  async function saveAlign(corners: LatLon[] | null) {
    if (!aligning) return
    const tuple = corners && corners.length === 4
      ? [
          [corners[0][0], corners[0][1]],
          [corners[1][0], corners[1][1]],
          [corners[2][0], corners[2][1]],
          [corners[3][0], corners[3][1]],
        ] as [[number, number], [number, number], [number, number], [number, number]]
      : null
    try {
      await api.intel.adjust({ id: aligning.id, adjust: { corners: tuple, opacity: alignOpacity } })
      cancelAlign()
      refresh()
    } catch (e) { window.alert(String(e)) }
  }

  const tile = INTEL_TILE_LAYERS[tileKey]

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
            <input
              type="file" accept="image/*" multiple hidden disabled={busy}
              onChange={e => { handleFiles(e.target.files); e.target.value = '' }}
            />
          </label>
          <div style={{ marginTop: 6, fontSize: '0.6rem', color: 'var(--text-dim)', lineHeight: 1.5 }}>
            F-14 TARPS screenshots — position &amp; attitude are read from the filename.
          </div>
          {uploadMsg && (
            <div style={{ marginTop: 6, fontSize: '0.66rem', color: 'var(--accent)' }}>{uploadMsg}</div>
          )}
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

        {/* Capture list */}
        <div style={{ flex: 1, overflowY: 'auto', padding: '10px 14px' }}>
          <div style={{ fontSize: '0.6rem', letterSpacing: '0.12em', color: 'var(--text-dim)', marginBottom: 6 }}>
            ON MAP ({placed.length})
          </div>
          {placed.length === 0 && (
            <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
              No recon captures yet. Upload TARPS photos to build the picture.
            </div>
          )}
          {placed.map(c => (
            <div key={c.id} style={{
              padding: '6px 0', borderBottom: '1px solid var(--border)', fontSize: '0.66rem',
              display: 'flex', flexDirection: 'column', gap: 2,
              background: aligning?.id === c.id ? 'var(--bg-elevated)' : undefined,
            }}>
              <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                <span style={{
                  flex: 1, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
                  color: 'var(--text)', cursor: 'pointer',
                }} onClick={() => setLightbox(c)}>{c.filename || 'capture'}</span>
                {c.mine && (
                  <button onClick={() => (aligning?.id === c.id ? cancelAlign() : startAlign(c))}
                    title="Align photo to the map" style={{
                      background: 'none', border: 'none', cursor: 'pointer',
                      color: aligning?.id === c.id ? 'var(--accent)' : 'var(--text-dim)',
                    }}>
                    <Move3d size={13} />
                  </button>
                )}
                {c.mine && (
                  <button onClick={() => doDelete(c)} title="Delete" style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)' }}>
                    <Trash2 size={12} />
                  </button>
                )}
              </div>
              <div style={{ color: 'var(--text-dim)', fontFamily: FONT_MONO }}>{fmtLatLon(c.lat, c.lon)}</div>
              <div style={{ color: 'var(--text-dim)' }}>
                {c.uploaded_by_name}
                {c.captured_at && ` · ${new Date(c.captured_at).toLocaleString()}`}
                {c.adjust?.corners && ' · aligned'}
              </div>
            </div>
          ))}
        </div>
      </div>

      {/* ── Map ───────────────────────────────────────────────────── */}
      <div style={{ position: 'relative', flex: 1 }}>
        {isError && (
          <div style={{
            position: 'absolute', zIndex: 1000, top: 10, left: '50%', transform: 'translateX(-50%)',
            background: 'rgba(120,20,20,0.9)', color: '#fff', padding: '6px 12px', borderRadius: 4, fontSize: '0.7rem',
          }}>{String((error as Error)?.message ?? 'failed to load intel')}</div>
        )}

        {/* Top-right controls */}
        <div style={{ position: 'absolute', zIndex: 1000, top: 10, right: 10, display: 'flex', gap: 4 }}>
          <button onClick={() => setShowImagery(v => !v)} title="Toggle photo overlays" style={{
            padding: '4px 8px', fontSize: '0.6rem', letterSpacing: '0.08em', cursor: 'pointer',
            border: `1px solid ${showImagery ? 'var(--accent)' : 'var(--border)'}`,
            background: showImagery ? 'var(--accent)' : 'rgba(0,0,0,0.55)',
            color: showImagery ? '#fff' : 'var(--text-dim)', borderRadius: 3,
            display: 'flex', alignItems: 'center', gap: 4,
          }}>
            {showImagery ? <Eye size={11} /> : <EyeOff size={11} />} PHOTOS
          </button>
          {(Object.keys(INTEL_TILE_LAYERS) as IntelTileKey[]).map(k => (
            <button key={k} onClick={() => setTileKey(k)} style={{
              padding: '4px 8px', fontSize: '0.6rem', letterSpacing: '0.08em', cursor: 'pointer',
              border: `1px solid ${tileKey === k ? 'var(--accent)' : 'var(--border)'}`,
              background: tileKey === k ? 'var(--accent)' : 'rgba(0,0,0,0.55)',
              color: tileKey === k ? '#fff' : 'var(--text-dim)', borderRadius: 3,
            }}>{INTEL_TILE_LAYERS[k].label}</button>
          ))}
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

        <MapContainer center={[35, 40]} zoom={6} style={{ height: '100%', width: '100%' }} preferCanvas>
          <TileLayer url={tile.url} attribution={tile.attr} maxZoom={18} />
          <ScaleControl position="bottomleft" />
          <FlyToFirst captures={captures} objectives={objectives} />
          {placing && <PlacementClicks onPick={placeAt} />}

          {/* Frontline + objectives context */}
          {midLines.map((line, i) => (
            <Polyline key={`m${i}`} positions={line} pathOptions={{ color: '#ddd', weight: 1, opacity: 0.5, dashArray: '4 4' }} />
          ))}
          {objectives.filter(o => o.lat || o.lon).map(o => (
            <CircleMarker key={o.id} center={[o.lat, o.lon]} radius={4}
              pathOptions={{ color: sideColor(o.owner), fillColor: sideColor(o.owner), fillOpacity: 0.5, weight: 1 }}>
              <Tooltip>{o.name} · {o.kind}</Tooltip>
            </CircleMarker>
          ))}

          {/* Recon captures */}
          {placed.map(c => {
            const col = sideColor(c.side)
            const isAligning = aligning?.id === c.id
            const quad: LatLon[] | null = isAligning ? alignCorners : quadFor(c, dims[c.id])
            const d = dims[c.id]
            const hasQuad = !!quad && quad.length === 4
            const showWarp = showImagery && !!d && hasQuad
            const opacity = isAligning ? alignOpacity : (c.adjust?.opacity ?? DEFAULT_OPACITY)
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
                    onClick={() => setLightbox(c)}
                  />
                )}
                {hasQuad && quad && (
                  <Polygon positions={quad}
                    pathOptions={{
                      color: isAligning ? '#fff' : col,
                      weight: isAligning ? 2 : 1,
                      opacity: isAligning ? 0.9 : (showWarp ? 0.5 : 0.75),
                      fillOpacity: showWarp ? 0 : 0.08,
                      dashArray: isAligning ? '6 4' : undefined,
                    }}
                    eventHandlers={{ click: () => !isAligning && setLightbox(c) }} />
                )}
                {!isAligning && (
                  <Marker position={[c.lat, c.lon]} icon={cameraIcon(c.heading_deg, col)}
                    eventHandlers={{ click: () => setLightbox(c) }}>
                    <Tooltip direction="top" offset={[0, -12]}>
                      <div style={{ fontFamily: FONT_MONO, fontSize: '0.7rem' }}>
                        <div>{c.filename || 'capture'}</div>
                        <div>{fmtLatLon(c.lat, c.lon)}</div>
                        {c.alt_ft != null && <div>ALT {Math.round(c.alt_ft)}ft · HDG {Math.round(c.heading_deg ?? 0)}</div>}
                      </div>
                    </Tooltip>
                  </Marker>
                )}
                {isAligning && hasQuad && quad && quad.map((corner, ci) => (
                  <Marker
                    key={ci}
                    position={corner}
                    icon={HANDLE_ICON}
                    draggable
                    eventHandlers={{
                      drag: e => {
                        const ll = (e.target as L.Marker).getLatLng()
                        setAlignCorners(prev => prev
                          ? prev.map((p, j) => (j === ci ? [ll.lat, ll.lng] : p))
                          : prev)
                      },
                    }}
                  />
                ))}
              </Fragment>
            )
          })}
        </MapContainer>
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
    <div
      onClick={onClose}
      style={{
        position: 'fixed', inset: 0, zIndex: 4000, background: 'rgba(0,0,0,0.88)',
        display: 'flex', alignItems: 'center', justifyContent: 'center', overflow: 'hidden',
      }}
    >
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
