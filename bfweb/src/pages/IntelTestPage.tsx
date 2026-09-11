// Dev harness for the recon warp overlay in isolation — real captures, no
// sidebar / markup / timeline. Route: /inteltest  (remove once happy).
import { Fragment, useMemo, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { MapContainer, TileLayer, Polygon, ScaleControl } from 'react-leaflet'
import { api } from '../api'
import { INTEL_TILE_LAYERS, groundFootprint, type LatLon } from '../lib/geo'
import { warpedGroundQuad } from '../lib/warp'
import IntelWarpOverlay from './IntelWarpOverlay'

export default function IntelTestPage() {
  const [blend, setBlend] = useState(true)
  const [opacity, setOpacity] = useState(0.95)
  const [only, setOnly] = useState<number | null>(null)   // show just capture #n

  const { data: caps = [], isLoading, error } = useQuery({
    queryKey: ['inteltest', 'captures'],
    queryFn: () => api.intel.captures(),
  })

  const placed = useMemo(() => caps.filter(c => c.placed), [caps])
  const center: LatLon = placed[0] ? [placed[0].lat, placed[0].lon] : [35, 40]

  return (
    <div style={{ position: 'fixed', inset: 0 }}>
      <div style={{ position: 'absolute', zIndex: 1000, top: 8, left: 8, display: 'flex', gap: 8, alignItems: 'center', background: '#000b', padding: 8, color: '#fff', fontSize: 12, fontFamily: 'monospace' }}>
        <button onClick={() => setBlend(v => !v)}>BLEND {blend ? 'ON' : 'OFF'}</button>
        <label>op <input type="range" min={0.2} max={1} step={0.05} value={opacity} onChange={e => setOpacity(+e.target.value)} /></label>
        <button onClick={() => setOnly(o => (o == null ? 0 : null))}>{only == null ? 'solo first' : 'show all'}</button>
        {only != null && <button onClick={() => setOnly(o => ((o ?? 0) + 1) % Math.max(1, placed.length))}>next #{only}</button>}
        <span>{isLoading ? 'loading…' : error ? `err: ${(error as Error).message}` : `${placed.length} placed / ${caps.length}`}</span>
      </div>
      <MapContainer center={center} zoom={14} style={{ height: '100%', width: '100%' }}>
        <TileLayer url={INTEL_TILE_LAYERS.satellite.url} attribution="Esri" maxZoom={18} crossOrigin="" />
        <ScaleControl position="bottomleft" />
        {placed.map((c, idx) => {
          if (only != null && idx !== only) return null
          const quad: LatLon[] | null =
            (c.alt_ft != null && c.heading_deg != null
              ? warpedGroundQuad({
                  lat: c.lat, lon: c.lon, altFt: c.alt_ft, headingDeg: c.heading_deg,
                  pitchDeg: c.pitch_deg ?? 0, rollDeg: c.roll_deg ?? 0, aspect: 1.5,
                }) ?? groundFootprint(c.lat, c.lon, c.alt_ft, c.heading_deg, 1.5)
              : null)
          if (!quad) return null
          return (
            <Fragment key={c.id}>
              <IntelWarpOverlay
                url={api.intel.imageUrl(c.id)}
                corners={quad}
                naturalW={1920}
                naturalH={1080}
                opacity={opacity}
                interactive
                keyBlack={blend}
                zIndex={500}
              />
              <Polygon positions={quad} pathOptions={{ color: '#3b82f6', weight: 1, opacity: 0.25, fillOpacity: 0 }} />
            </Fragment>
          )
        })}
      </MapContainer>
    </div>
  )
}
