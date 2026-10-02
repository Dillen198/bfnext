/**
 * The range's sectors on the live map: each part of the theatre with one job
 * (bombing range, fight area, tanker track, carrier op area...), drawn in the
 * same colour as the F10 map, with a key and a card saying what each is for.
 */
import { useMemo, type CSSProperties, type Ref } from 'react'
import { Layer, Marker, Source } from 'react-map-gl/maplibre'
import type { FeatureCollection } from 'geojson'
import { X } from '@icons'
import { SECTOR_KIND_ORDER, kindInfo, metresPerPixel, type DrawnSector } from '../../lib/sectors'

/** Map layers the pointer reads a sector from. */
export const SECTOR_HIT_LAYER = 'sector-fill'

/** The kind colour, darkened on the light map so yellow, lime and white still read. */
const ink = (color: string, light: boolean) => (light ? `color-mix(in srgb, ${color} 55%, #000)` : color)

function sideText(d: DrawnSector): string {
  return d.side === 'blue' ? 'Blue sector' : d.side === 'red' ? 'Red sector' : 'Shared by both sides'
}

function Swatch({ color, dashed, light, size = 12 }: { color: string; dashed?: boolean; light: boolean; size?: number }) {
  return (
    <span
      aria-hidden
      style={{
        width: size, height: size, flex: 'none', display: 'inline-block', borderRadius: 2,
        background: `${color}40`, border: `2px ${dashed ? 'dashed' : 'solid'} ${color}`,
        boxShadow: light ? '0 0 0 1px rgba(15, 24, 34, 0.35)' : undefined,
      }}
    />
  )
}

/** The fills and outlines. Mount before the live shapes so these sit underneath. */
export function SectorShapes({ drawn, visible, highlight, light }: {
  drawn: DrawnSector[]
  visible: boolean
  highlight: string | null
  light: boolean
}) {
  const data = useMemo<FeatureCollection>(() => ({
    type: 'FeatureCollection',
    features: drawn.map(d => ({
      type: 'Feature',
      properties: { id: d.sector.id, color: d.info.color, dashed: d.info.dashed ? 1 : 0 },
      geometry: { type: 'Polygon', coordinates: [d.ring] },
    })),
  }), [drawn])
  // Always mounted, hidden with `visibility`, so toggling never re-adds a
  // layer on top of the live shapes.
  const vis = visible ? 'visible' : 'none'
  const hi = highlight ?? ''
  return (
    <Source id="sectors" type="geojson" data={data}>
      <Layer id="sector-casing" type="line" layout={{ visibility: light && visible ? 'visible' : 'none' }}
        paint={{ 'line-color': '#0f1822', 'line-width': 3.5, 'line-opacity': 0.4 }} />
      <Layer id={SECTOR_HIT_LAYER} type="fill" layout={{ visibility: vis }}
        paint={{ 'fill-color': ['get', 'color'], 'fill-opacity': ['case', ['==', ['get', 'id'], hi], 0.3, 0.15] }} />
      <Layer id="sector-line" type="line" filter={['==', ['get', 'dashed'], 0]} layout={{ visibility: vis }}
        paint={{ 'line-color': ['get', 'color'], 'line-width': 2, 'line-opacity': 0.9 }} />
      <Layer id="sector-line-dashed" type="line" filter={['==', ['get', 'dashed'], 1]} layout={{ visibility: vis }}
        paint={{ 'line-color': ['get', 'color'], 'line-width': 2, 'line-opacity': 0.9, 'line-dasharray': [3, 2] }} />
      <Layer id="sector-hi" type="line" filter={['==', ['get', 'id'], hi]} layout={{ visibility: vis }}
        paint={{ 'line-color': ['get', 'color'], 'line-width': 3.5 }} />
    </Source>
  )
}

/**
 * Sector names. One marker per sector, always mounted and hidden with CSS,
 * so they stay underneath the station / aircraft markers (markers stack in
 * the order they were added). A name shows once its sector is wide enough on
 * screen to hold it; the highlighted sector always shows its name.
 */
export function SectorLabels({ drawn, visible, zoom, detail, highlight, light }: {
  drawn: DrawnSector[]
  /** ids of the sectors the side filter keeps; empty when the layer is off */
  visible: Set<string>
  zoom: number
  detail: number
  highlight: string | null
  light: boolean
}) {
  return (
    <>
      {drawn.map(d => {
        const room = d.width_m / metresPerPixel(zoom, d.label.lat)
        const hi = highlight === d.sector.id
        const on = visible.has(d.sector.id) && (hi || (detail >= 0 && room >= 64))
        const style: CSSProperties = { pointerEvents: 'none', visibility: on ? 'visible' : 'hidden', zIndex: hi ? 1 : undefined }
        return (
          <Marker key={d.sector.id} latitude={d.label.lat} longitude={d.label.lon} anchor="center" style={style}>
            <div className="map-label text-center" style={{ color: ink(d.info.color, light), fontWeight: 600, letterSpacing: '0.04em', opacity: hi ? 1 : 0.9 }}>
              {d.sector.name}
              {(hi || (detail >= 1 && room >= 90)) && (
                <div style={{ fontSize: 9, fontWeight: 500, opacity: 0.8 }}>{d.info.label}</div>
              )}
            </div>
          </Marker>
        )
      })}
    </>
  )
}

/** Kind → colour, for the kinds actually on the map. */
export function SectorLegend({ drawn, light, canHover }: { drawn: DrawnSector[]; light: boolean; canHover: boolean }) {
  const present = new Set(drawn.map(d => d.sector.kind as string))
  const kinds = [...SECTOR_KIND_ORDER.filter(k => present.has(k)), ...[...present].filter(k => !(SECTOR_KIND_ORDER as string[]).includes(k))]
  if (!kinds.length) return null
  return (
    <div className="map-key" role="list" aria-label="Sector colours">
      <div className="grid grid-cols-2 gap-x-3 gap-y-1">
        {kinds.map(k => {
          const info = kindInfo(k)
          return (
            <div key={k} role="listitem" className="flex items-center gap-1.5 min-w-0">
              <Swatch color={info.color} dashed={info.dashed} light={light} />
              <span className="mono truncate" style={{ fontSize: 10.5 }}>{info.label}</span>
            </div>
          )
        })}
      </div>
      <div className="muted mt-1.5" style={{ fontSize: 11 }}>{canHover ? 'Point at' : 'Tap'} an area to see what it is for.</div>
    </div>
  )
}

function SectorInfo({ d, light }: { d: DrawnSector; light: boolean }) {
  return (
    <div className="flex items-start gap-2 min-w-0">
      <span className="mt-[3px]"><Swatch color={d.info.color} dashed={d.info.dashed} light={light} /></span>
      <div className="min-w-0">
        <div className="mono font-semibold text-[12.5px]" style={{ color: 'var(--chalk)' }}>{d.sector.name}</div>
        <div className="flex flex-wrap items-center gap-x-1.5 text-[11.5px]">
          <span className="mono" style={{ color: ink(d.info.color, light) }}>{d.info.label}</span>
          <span className="dim">·</span>
          <span className="flex items-center gap-1 muted">
            {d.side !== 'all' && <span className="dot" style={{ background: d.side === 'blue' ? 'var(--sky)' : 'var(--wave)' }} />}
            {sideText(d)}
          </span>
        </div>
        {d.sector.purpose && <div className="text-[12.5px] mt-1" style={{ color: 'var(--chalk)' }}>{d.sector.purpose}</div>}
      </div>
    </div>
  )
}

/** Follows the pointer; the parent positions it (no re-render per mouse move). */
export function SectorTip({ d, light, ref }: { d: DrawnSector | null; light: boolean; ref: Ref<HTMLDivElement> }) {
  return (
    <div ref={ref} className="map-tip" style={{ visibility: d ? 'visible' : 'hidden' }} aria-hidden>
      {d && <SectorInfo d={d} light={light} />}
    </div>
  )
}

/** The tapped / clicked sector, docked at the bottom of the map. */
export function SectorCard({ d, light, onClose }: { d: DrawnSector; light: boolean; onClose: () => void }) {
  return (
    <div className="map-card" role="dialog" aria-label={d.sector.name}>
      <SectorInfo d={d} light={light} />
      <button className="btn-range ghost sm" style={{ padding: 4, height: 'auto' }} onClick={onClose} aria-label="Close">
        <X size={14} />
      </button>
    </div>
  )
}
