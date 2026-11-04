import { useEffect, useMemo, useState } from 'react'
import { MapContainer, TileLayer, Marker, Polyline, Polygon, CircleMarker, Pane, useMap } from 'react-leaflet'
import L from 'leaflet'
import type { LatLngBoundsExpression } from 'leaflet'
import 'leaflet/dist/leaflet.css'
import { ExternalLink, Layers } from '@icons'
import type { Objective, Frontlines, LatLon } from '../api'
import { campaign } from '../config/campaign'
import { useTheme } from '@shared/web/theme'
import { objectiveSymbol } from '../lib/objSymbol'
import { placeLabels } from '../lib/plotLabels'
import { territoryMap } from '../lib/territory'

/** Room the plot leaves on each side for whatever sits over it, in px. */
export interface MapInset { left: number; right: number; top: number; bottom: number }
const NO_INSET: MapInset = { left: 0, right: 0, top: 0, bottom: 0 }

function FitBounds({ objectives, inset }: { objectives: Objective[]; inset: MapInset }) {
  const map = useMap()
  const { left, right, top, bottom } = inset
  useEffect(() => {
    if (!objectives.length) return
    // The plot can't be panned or zoomed, so it fits again whenever its box
    // changes size too. An inset keeps the objectives clear of panels laid
    // over the map (Levant SITREP), so the plot sits in the open part.
    const fit = () => map.fitBounds(objectives.map(o => [o.lat, o.lon] as [number, number]) as LatLngBoundsExpression,
      { paddingTopLeft: [left + 28, top + 46], paddingBottomRight: [right + 28, bottom + 46], maxZoom: 9, animate: false })
    fit()
    map.on('resize', fit)
    return () => { map.off('resize', fit) }
  }, [map, objectives, left, right, top, bottom])
  return null
}

const TERRITORY_PREF = 'dash.territory'

function loadTerritoryPref(): boolean {
  try {
    return localStorage.getItem(TERRITORY_PREF) !== '0'
  } catch {
    return true
  }
}

/** How far apart two bases facing each other across the front can be and
 *  still get a connector, km: past that they are not each other's fight. */
const FACING_KM = 60

function km(a: Objective, b: Objective): number {
  const r = Math.PI / 180
  const h = Math.sin(((b.lat - a.lat) * r) / 2) ** 2 +
    Math.cos(a.lat * r) * Math.cos(b.lat * r) * Math.sin(((b.lon - a.lon) * r) / 2) ** 2
  return 2 * 6371 * Math.asin(Math.sqrt(h))
}

/** The ground each side holds (lib/territory: every base's Voronoi cell, cut
 *  to the coast where we have one) in its owner's colour, the front where
 *  the sides' ground meets, and a dotted connector between bases that face
 *  each other across it. Its own pane sits over the tiles and under the
 *  engine's front lines and the markers. It is worked out again only when a
 *  base changes hands or moves, not on every refetch. */
function Territory({ objectives, color, ink }: { objectives: Objective[]; color: (owner: string) => string; ink: string }) {
  const key = objectives.map(o => `${o.id}:${o.owner}:${o.lat}:${o.lon}`).join(',')
  const terr = useMemo(() => {
    const t = territoryMap(objectives)
    const byOwner = new Map<string, LatLon[][][]>()
    for (const c of t.cells) byOwner.set(c.owner, [...(byOwner.get(c.owner) ?? []), [c.ring]])
    const facing = t.facing
      .filter(([i, j]) => km(objectives[i], objectives[j]) <= FACING_KM)
      .map(([i, j]) => ({ a: objectives[i], b: objectives[j] }))
    return { sides: [...byOwner], front: t.front, facing }
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key])
  return (
    <Pane name="territory" style={{ zIndex: 350 }}>
      {terr.sides.map(([owner, polys]) => (
        <Polygon key={owner} positions={polys} interactive={false}
          pathOptions={{ stroke: false, fillColor: color(owner), fillOpacity: 0.14 }} />
      ))}
      <Polyline positions={terr.front} interactive={false}
        pathOptions={{ color: ink, weight: 2, opacity: 0.55, lineCap: 'round', lineJoin: 'round' }} />
      {terr.facing.map(({ a, b }) => (
        <Polyline key={`${a.id}-${b.id}`} positions={[[a.lat, a.lon], [b.lat, b.lon]]} interactive={false}
          pathOptions={{ color: ink, weight: 1, opacity: 0.6, dashArray: '2 3' }} />
      ))}
    </Pane>
  )
}

/** Which names win a crowded spot: the command centre and fleets, then
 *  airbases, then the hubs, factories and ports, then the forward bases. */
const NAME_PRI: Record<string, number> = {
  'Command Center': 6, 'Carrier Group': 6, Airbase: 5,
  'Logistics Hub': 4, Factory: 4, 'Naval Base': 4, FOB: 3, FARP: 3,
}
const NAME_FONT = '600 10px'

/** Every objective's name beside its symbol (lib/plotLabels: right, else
 *  left, else left off where it would land on another name or symbol). Its
 *  own pane sits over the markers and lets clicks through to them. */
function ObjectiveNames({ objectives, ink, halo }: { objectives: Objective[]; ink: string; halo: string }) {
  const map = useMap()
  // Where the plot sits, which only a refit changes.
  const [view, setView] = useState(() => `${map.getZoom()}:${map.getCenter().toString()}`)
  useEffect(() => {
    const onView = () => setView(`${map.getZoom()}:${map.getCenter().toString()}`)
    map.on('zoomend moveend', onView)
    return () => { map.off('zoomend moveend', onView) }
  }, [map])
  const key = objectives.map(o => `${o.id}:${o.name}:${o.kind}:${o.lat}:${o.lon}`).join(',')
  const sides = useMemo(() => {
    const ctx = document.createElement('canvas').getContext('2d')
    const family = getComputedStyle(document.documentElement).getPropertyValue('--font-body').trim() || 'sans-serif'
    if (ctx) ctx.font = `${NAME_FONT} ${family}`
    const zoom = map.getZoom()
    const px = map.getPixelBounds()
    return placeLabels(objectives.map(o => {
      const p = map.project([o.lat, o.lon], zoom)
      return { x: p.x, y: p.y, w: (ctx?.measureText(o.name).width ?? o.name.length * 6) + 2, h: 12, pri: NAME_PRI[o.kind] ?? 2 }
    }), 10, 7, { x0: px.min!.x + 6, x1: px.max!.x - 6, y0: px.min!.y + 6, y1: px.max!.y - 6 })
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [map, key, view])
  return (
    <Pane name="names" style={{ zIndex: 650, pointerEvents: 'none' }}>
      {objectives.map((o, i) => sides[i] && (
        <Marker key={o.id} position={[o.lat, o.lon]} interactive={false} pane="names"
          icon={L.divIcon({
            className: '',
            iconSize: [0, 0],
            html: `<span style="position:absolute;top:-7px;${sides[i] === 'right' ? 'left:10px' : 'left:-10px;transform:translateX(-100%)'};white-space:nowrap;font:${NAME_FONT} var(--font-body);line-height:14px;color:${ink};text-shadow:0 0 2px ${halo},0 0 2px ${halo},0 0 3px ${halo}">${o.name.replace(/[&<>"]/g, c => `&#${c.charCodeAt(0)};`)}</span>`,
          })} />
      ))}
    </Pane>
  )
}

/** The public campaign map: objectives and the front line on a plain basemap.
 *  Everything drawn here comes from /api/objectives and /api/frontline, the
 *  public routes -- no contacts, so it is safe to show to anyone (the SITREP
 *  page, and the Discord status snapshot via /snapshot).
 *
 *  `snapshot` is the Discord render: bigger glyphs, a stronger basemap, and a
 *  ring around every objective under attack or ready to capture, so the
 *  picture carries the "where is the fight" answer on its own.
 *
 *  Every objective is named beside its symbol, where there is room.
 *
 *  The plot is fixed to its objectives: no panning or zooming (TACMAP has
 *  the map to move about). The TERRITORY toggle shades the ground each side
 *  holds and links the bases facing each other; the choice sticks
 *  per browser. The snapshot leaves it out. */
export default function TacMap({ objectives, fronts, onOpenTacmap, snapshot = false, onTilesLoaded, onSelect, inset = NO_INSET }: {
  objectives: Objective[]
  fronts: Frontlines
  /** Shows the TACMAP button when given. */
  onOpenTacmap?: () => void
  snapshot?: boolean
  /** Snapshot only: every visible basemap tile has loaded. */
  onTilesLoaded?: () => void
  /** Clicking an objective's glyph; the Objectives page opens its drawer. */
  onSelect?: (name: string) => void
  /** Panels laid over the map: the plot and its chips keep clear of them. */
  inset?: MapInset
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
  const [territory, setTerritoryState] = useState(loadTerritoryPref)
  const setTerritory = (on: boolean) => {
    setTerritoryState(on)
    try {
      localStorage.setItem(TERRITORY_PREF, on ? '1' : '0')
    } catch {
      /* private window: the choice just doesn't stick */
    }
  }

  // The plot's own symbols (lib/objSymbol): a solid shape per kind in the
  // owner's colour, edged in the board's dark (or paper) so it reads over the
  // territory and the tiles. A destroyed objective is drawn faded.
  const markerIcon = (obj: Objective) => {
    const s = (obj.kind === 'Airbase' || obj.kind === 'Command Center' ? 6.5 : 6) * (snapshot ? 1.5 : 1)
    const { svg, size } = objectiveSymbol(obj.kind, ownerColor(obj.owner), halo, s)
    return L.divIcon({
      html: obj.health > 0 ? svg : `<div style="opacity:0.5">${svg}</div>`,
      className: '',
      iconSize: [size, size],
      iconAnchor: [size / 2, size / 2],
    })
  }

  const { theme } = useTheme()
  // What the symbols and their names are edged in: the board's dark,
  // or the paper.
  const halo = theme === 'light' ? '#fbfbf8' : '#0b0f0d'
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
          a one-shot screenshot catches both stacked. Every plot fits at a
          fractional zoom, so the objectives fill it; that scales the tiles,
          which leaves hairline seams between them, and .tacmap-snapshot
          overlaps each tile by a pixel to close them. */}
      <MapContainer
        {...(snapshot && valid.length > 0
          ? { bounds: valid.map(o => [o.lat, o.lon] as [number, number]) as LatLngBoundsExpression,
              boundsOptions: { padding: [48, 48] as [number, number], maxZoom: 9 } }
          : { center: campaign.mapCenter, zoom: campaign.mapZoom })}
        style={{ position: 'absolute', inset: 0 }} zoomControl={false} attributionControl={false}
        dragging={false} scrollWheelZoom={false} doubleClickZoom={false} touchZoom={false} boxZoom={false} keyboard={false}
        className="tacmap-snapshot"
        zoomSnap={snapshot ? 0.25 : 0.1} fadeAnimation={!snapshot} zoomAnimation={!snapshot}>
        <TileLayer key={theme} url={canvasBase} maxZoom={19} opacity={snapshot ? 0.8 : 0.5}
          eventHandlers={snapshot ? { load: onTilesLoaded } : undefined} />
        {valid.length > 0 && !snapshot && <FitBounds objectives={valid} inset={inset} />}
        {territory && !snapshot && <Territory objectives={valid} color={ownerColor} ink={theme === 'light' ? '#1c2326' : '#e4e7e1'} />}
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
          <Marker key={obj.id} position={[obj.lat, obj.lon]} icon={markerIcon(obj)} title={obj.name}
            eventHandlers={onSelect ? { click: () => onSelect(obj.name) } : undefined} />
        ))}
        {!snapshot && <ObjectiveNames objectives={valid} ink={theme === 'light' ? '#1c2326' : '#e4e7e1'} halo={halo} />}
      </MapContainer>

      {/* Top-left stat chip */}
      <div style={{
        position: 'absolute', top: inset.top + 8, left: inset.left + 8, zIndex: 1000,
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

      {/* Layer toggle and TACMAP button */}
      {(!snapshot || onOpenTacmap) && (
        <div style={{ position: 'absolute', top: inset.top + 8, right: inset.right + 8, zIndex: 1000, display: 'flex', gap: 6 }}>
          {!snapshot && (
            <button type="button" aria-pressed={territory} onClick={() => setTerritory(!territory)}
              title="Shade the ground each side holds (every base's zone of control, in its owner's colour) and link the bases facing each other across the front" style={{
                display: 'flex', alignItems: 'center', gap: 5,
                background: 'rgba(8,11,6,0.90)', border: `1px solid ${territory ? 'var(--accent-border)' : 'var(--border)'}`,
                color: territory ? 'var(--accent-bright)' : 'var(--text-dim)', padding: '5px 10px',
                fontSize: '0.58rem', fontWeight: 700, letterSpacing: '0.14em',
                cursor: 'pointer', textTransform: 'uppercase', fontFamily: 'var(--font-mono)',
              }}>
              <Layers size={9} /> TERRITORY
            </button>
          )}
          {onOpenTacmap && (
            <button type="button" onClick={onOpenTacmap} style={{
              display: 'flex', alignItems: 'center', gap: 5,
              background: 'rgba(8,11,6,0.90)', border: '1px solid var(--accent-border)',
              color: 'var(--accent-bright)', padding: '5px 10px',
              fontSize: '0.58rem', fontWeight: 700, letterSpacing: '0.14em',
              cursor: 'pointer', textTransform: 'uppercase', fontFamily: 'var(--font-mono)',
            }}>
              <ExternalLink size={9} /> TACMAP
            </button>
          )}
        </div>
      )}

      {/* Bottom legend */}
      <div style={{
        position: 'absolute', bottom: inset.bottom + 8, left: `calc(${inset.left}px + (100% - ${inset.left + inset.right}px) / 2)`, transform: 'translateX(-50%)', zIndex: 1000,
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
