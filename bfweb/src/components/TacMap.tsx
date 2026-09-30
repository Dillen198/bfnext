import { useEffect } from 'react'
import { MapContainer, TileLayer, Marker, Polyline, CircleMarker, useMap } from 'react-leaflet'
import L from 'leaflet'
import type { LatLngBoundsExpression } from 'leaflet'
import 'leaflet/dist/leaflet.css'
import { renderToStaticMarkup } from 'react-dom/server'
import { Pin, ExternalLink } from '@icons'
import type { Objective, Frontlines } from '../api'
import { campaign } from '../config/campaign'
import { useTheme } from '../context/ThemeContext'
import { OBJ_ICON } from '../lib/objIcon'

function FitBounds({ objectives, padding }: { objectives: Objective[]; padding: number }) {
  const map = useMap()
  useEffect(() => {
    if (!objectives.length) return
    map.fitBounds(objectives.map(o => [o.lat, o.lon] as [number, number]) as LatLngBoundsExpression,
      { padding: [padding, padding], maxZoom: 9, animate: false })
  }, [map, objectives, padding])
  return null
}

/** The public campaign map: objectives and the front line on a plain basemap.
 *  Everything drawn here comes from /api/objectives and /api/frontline, the
 *  public routes -- no contacts, so it is safe to show to anyone (the SITREP
 *  page, and the Discord status snapshot via /snapshot).
 *
 *  `snapshot` is the Discord render: bigger glyphs, a stronger basemap, and a
 *  ring around every objective under attack or ready to capture, so the
 *  picture carries the "where is the fight" answer on its own. */
export default function TacMap({ objectives, fronts, onOpenTacmap, snapshot = false, onTilesLoaded }: {
  objectives: Objective[]
  fronts: Frontlines
  /** Shows the TACMAP button when given. */
  onOpenTacmap?: () => void
  snapshot?: boolean
  /** Snapshot only: every visible basemap tile has loaded. */
  onTilesLoaded?: () => void
}) {
  const valid = objectives.filter(o => o.lat !== 0 || o.lon !== 0)
  const ownerColor = (owner: string) =>
    owner === 'Blue' ? campaign.blueColor :
    owner === 'Red'  ? campaign.redColor  :
                       '#4a5240'
  const blue = valid.filter(o => o.owner === 'Blue').length
  const red  = valid.filter(o => o.owner === 'Red').length
  const neu  = valid.filter(o => o.owner === 'Neutral').length
  const contested = snapshot ? valid.filter(o => o.threatened || o.captureable) : []

  // The objective-type glyphs rendered straight onto the
  // map -- no circle, no fill -- same OBJ_ICON set used by the Critical
  // Objectives list on the SITREP page, so an objective reads the same way in
  // both places. Owner colour + a flat dark outline keeps them legible over
  // tiles (no coloured glow).
  const markerIcon = (obj: Objective) => {
    const c = ownerColor(obj.owner)
    const alive = obj.health > 0
    const base = obj.kind === 'Airbase' ? 22 : (obj.kind === 'Carrier Group' || obj.kind === 'Naval Base') ? 20 : 17
    const size = snapshot ? Math.round(base * 1.5) : base
    const Icon = OBJ_ICON[obj.kind] ?? Pin
    const svg = renderToStaticMarkup(
      <Icon size={size} color={c} strokeWidth={2.25}
        style={{ opacity: alive ? 1 : 0.5, filter: 'drop-shadow(0 1px 1.5px rgba(0,0,0,0.9))' }} />
    )
    return L.divIcon({
      html: svg,
      className: '',
      iconSize: [size, size],
      iconAnchor: [size / 2, size / 2],
    })
  }

  const { theme } = useTheme()
  const canvasBase = theme === 'light'
    ? 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
    : 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}'
  // Discord shows the snapshot at ~40% of its 1200px width, so its overlay
  // text is sized for that, not for a browser.
  const chipFont = snapshot ? '1.6rem' : '0.58rem'
  const legendFont = snapshot ? '1.4rem' : '0.56rem'
  const swatch = snapshot ? 14 : 6

  return (
    <div style={{ position: 'relative', height: '100%', background: theme === 'light' ? '#dfe0d8' : '#050806' }}>
      {/* A snapshot starts at its final bounds, with no fade: starting at the
          default view and then fitting loads a second zoom level of tiles, and
          a one-shot screenshot catches both stacked. The fractional zoom it
          fits at scales the tiles, which leaves hairline seams between them;
          .tacmap-snapshot overlaps each tile by a pixel to close them. */}
      <MapContainer
        {...(snapshot && valid.length > 0
          ? { bounds: valid.map(o => [o.lat, o.lon] as [number, number]) as LatLngBoundsExpression,
              boundsOptions: { padding: [48, 48] as [number, number], maxZoom: 9 } }
          : { center: campaign.mapCenter, zoom: campaign.mapZoom })}
        style={{ position: 'absolute', inset: 0 }} zoomControl={false} attributionControl={false}
        className={snapshot ? 'tacmap-snapshot' : undefined}
        zoomSnap={snapshot ? 0.25 : 1} fadeAnimation={!snapshot} zoomAnimation={!snapshot}>
        <TileLayer key={theme} url={canvasBase} maxZoom={19} opacity={snapshot ? 0.8 : 0.5}
          eventHandlers={snapshot ? { load: onTilesLoaded } : undefined} />
        {valid.length > 0 && !snapshot && <FitBounds objectives={valid} padding={28} />}
        {fronts.blue.map((l, i) => l.length > 1 && (
          <Polyline key={`fb-${i}`} positions={l}
            pathOptions={{ color: '#2f7dff', weight: snapshot ? 2 : 1.5, opacity: 0.8, dashArray: '6 5' }} />
        ))}
        {fronts.red.map((l, i) => l.length > 1 && (
          <Polyline key={`fr-${i}`} positions={l}
            pathOptions={{ color: '#ff3b3b', weight: snapshot ? 2 : 1.5, opacity: 0.8, dashArray: '6 5' }} />
        ))}
        {fronts.mid.map((l, i) => l.length > 1 && (
          <Polyline key={`fm-${i}`} positions={l}
            pathOptions={{ color: '#ffffff', weight: snapshot ? 1.75 : 1.25, opacity: 0.9, dashArray: '2 4' }} />
        ))}
        {contested.map(obj => (
          <CircleMarker key={`c-${obj.id}`} center={[obj.lat, obj.lon]} radius={28}
            pathOptions={{ color: '#fee75c', weight: 4, opacity: 0.95, fill: true, fillColor: '#fee75c', fillOpacity: 0.15 }} />
        ))}
        {valid.map(obj => (
          <Marker key={obj.id} position={[obj.lat, obj.lon]} icon={markerIcon(obj)} />
        ))}
      </MapContainer>

      {/* Top-left stat chip */}
      <div style={{
        position: 'absolute', top: 8, left: 8, zIndex: 1000,
        background: 'rgba(8,11,6,0.90)', border: '1px solid var(--border)',
        padding: snapshot ? '10px 18px' : '5px 10px', display: 'flex', gap: snapshot ? 16 : 10,
        fontSize: chipFont, fontFamily: 'var(--font-mono)', letterSpacing: '0.1em',
      }}>
        <span style={{ color: campaign.blueColor }}>{blue} BLU</span>
        <span style={{ color: 'var(--border-light)' }}>·</span>
        <span style={{ color: campaign.redColor }}>{red} RED</span>
        <span style={{ color: 'var(--border-light)' }}>·</span>
        <span style={{ color: 'var(--text-dim)' }}>{neu} NEU</span>
      </div>

      {/* TACMAP button */}
      {onOpenTacmap && (
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
      )}

      {/* Bottom legend */}
      <div style={{
        position: 'absolute', bottom: 8, left: '50%', transform: 'translateX(-50%)', zIndex: 1000,
        display: 'flex', gap: snapshot ? 24 : 14, background: 'rgba(8,11,6,0.88)',
        padding: snapshot ? '9px 18px' : '4px 12px', border: '1px solid var(--border)',
        fontSize: legendFont, color: 'var(--text-dim)', fontFamily: 'var(--font-mono)', letterSpacing: '0.1em',
      }}>
        {[
          { label: campaign.blueLabel, color: campaign.blueColor },
          { label: campaign.redLabel,  color: campaign.redColor  },
          { label: 'NEUTRAL',          color: '#4a5240'          },
        ].map(({ label, color }) => (
          <span key={label} style={{ display: 'flex', alignItems: 'center', gap: snapshot ? 8 : 4 }}>
            <span style={{ width: swatch, height: swatch, background: color, display: 'inline-block', flexShrink: 0 }} />
            {label.toUpperCase()}
          </span>
        ))}
        {snapshot && (
          <span style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
            <span style={{ width: 14, height: 14, border: '3px solid #fee75c', borderRadius: '50%', display: 'inline-block', flexShrink: 0 }} />
            CONTESTED
          </span>
        )}
      </div>
    </div>
  )
}
