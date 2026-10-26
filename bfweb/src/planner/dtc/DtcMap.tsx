import { useMemo, useState } from 'react'
import Map, { Layer, Marker, Source, type MapLayerMouseEvent } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'
import { useTheme } from '@shared/web/theme'
import { mapStyleFor } from '../../lib/mapStyle'
import { getAt, type Json, type JsonObject, type Seg } from '../model'
import { addableList, numbered, positions, slotsOn } from './places'
import { fromDcsXY, toDcsXY, type DcsTerrain } from '../nav'
import { lar, larCentre, larPolygon } from '../f14lar'
import type { Plan } from '../types'
import type { Ctx } from './core'

interface Props {
  ctx: Ctx
  plan: Plan
  map: DcsTerrain
  pageSegs: Seg[]
}

/**
 * The cartridge on the map: your route for reference, the points of the page
 * you are on (drag to move, click the map to add), and every other cartridge
 * point faintly.
 */
export default function DtcMap({ ctx, plan, map, pageSegs }: Props) {
  const { theme } = useTheme()
  const mapStyle = useMemo(() => mapStyleFor(theme), [theme])
  const pts = useMemo(() => positions(ctx, map, pageSegs), [ctx, map, pageSegs])
  const add = addableList(ctx, pageSegs)
  const slots = add ? [] : slotsOn(ctx, pageSegs)
  const [slotKey, setSlotKey] = useState<string | null>(null)
  const slot = slots.find(x => x.segs.join('/') === slotKey) ?? slots.find(x => !x.placed) ?? slots[0]

  // Release zones of active JDAM targets on this page, as the exporter
  // computes them, and their launch points.
  const lars = useMemo(() => {
    const feats: { type: 'Feature'; properties: object; geometry: { type: 'Polygon'; coordinates: number[][][] } }[] = []
    const launches: { lat: number; lon: number }[] = []
    for (const p of pts) {
      if (!p.here) continue
      const t = getAt(ctx.value, p.segs) as JsonObject
      if (typeof t.drop_alt !== 'number' || typeof t.drop_spd !== 'number' || typeof t.x !== 'number' || typeof t.y !== 'number') continue
      const l = lar(t.drop_alt, t.drop_spd)
      const head = typeof t.attack_heading === 'number' ? t.attack_heading : 0
      const ring = larPolygon(t.x, t.y, head, l).map(q => { const ll = fromDcsXY(q.x, q.y, map.terrain); return [ll.lon, ll.lat] })
      feats.push({ type: 'Feature', properties: {}, geometry: { type: 'Polygon', coordinates: [[...ring, ring[0]]] } })
      const c = larCentre(t.x, t.y, head, l)
      launches.push(fromDcsXY(c.x, c.y, map.terrain))
    }
    return { geo: { type: 'FeatureCollection' as const, features: feats }, launches }
  }, [pts, ctx.value, map])

  const route = useMemo(() => ({
    type: 'FeatureCollection' as const,
    features: plan.waypoints.length < 2 ? [] : [{
      type: 'Feature' as const, properties: {},
      geometry: { type: 'LineString' as const, coordinates: plan.waypoints.map(w => [w.lon, w.lat]) },
    }],
  }), [plan.waypoints])

  const view = useMemo(() => {
    const all: [number, number][] = [...plan.waypoints.map(w => [w.lat, w.lon] as [number, number]), ...pts.filter(p => p.here).map(p => [p.lat, p.lon] as [number, number])]
    if (!all.length) return { latitude: map.terrain.center[0], longitude: map.terrain.center[1], zoom: 6 }
    const lat = all.reduce((s, p) => s + p[0], 0) / all.length
    const lon = all.reduce((s, p) => s + p[1], 0) / all.length
    return { latitude: lat, longitude: lon, zoom: 7 }
    // Only on mount and theme: editing never yanks the view.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [theme])

  const place = (segs: Seg[], lat: number, lon: number, base: JsonObject) => {
    const { x, y } = toDcsXY(lat, lon, map.terrain)
    const next: JsonObject = { ...base, x, y }
    if ('lat' in base) next.lat = lat
    if ('lon' in base) next.lon = lon
    ctx.set(segs, next)
  }

  const click = (e: MapLayerMouseEvent) => {
    if (!add && slot && !ctx.readOnly) {
      // Put the chosen slot here and switch it on, then move to the next empty one.
      const cur = getAt(ctx.value, slot.segs) as JsonObject
      const { x, y } = toDcsXY(e.lngLat.lat, e.lngLat.lng, map.terrain)
      ctx.set(slot.segs, { ...cur, active: true, x, y, lat: e.lngLat.lat, lon: e.lngLat.lng })
      const next = slots.find(q => !q.placed && q.segs.join('/') !== slot.segs.join('/'))
      setSlotKey(next ? next.segs.join('/') : null)
      return
    }
    if (!add) return
    const list = (getAt(ctx.value, add.segs) as Json[]) ?? []
    const item = numbered(list, add.template)
    const { x, y } = toDcsXY(e.lngLat.lat, e.lngLat.lng, map.terrain)
    item.x = x
    item.y = y
    if ('lat' in item) item.lat = e.lngLat.lat
    if ('lon' in item) item.lon = e.lngLat.lng
    ctx.set(add.segs, [...list, item])
  }

  return (
    <div style={{ position: 'relative', height: '100%', minHeight: 220, borderRadius: 6, overflow: 'hidden', border: '1px solid var(--border)' }}>
      <Map key={theme} mapStyle={mapStyle} style={{ width: '100%', height: '100%' }} initialViewState={view}
        dragRotate={false} attributionControl={false} cursor={add ? 'crosshair' : 'grab'} onClick={click}>
        <Source id="dtc-lar" type="geojson" data={lars.geo}>
          <Layer id="dtc-lar-fill" type="fill" paint={{ 'fill-color': '#40a6ff', 'fill-opacity': 0.12 }} />
          <Layer id="dtc-lar-line" type="line" paint={{ 'line-color': '#40a6ff', 'line-width': 1.2, 'line-dasharray': [3, 2] }} />
        </Source>
        {lars.launches.map((p, i) => (
          <Marker key={`launch-${i}`} latitude={p.lat} longitude={p.lon}>
            <div title="Launch point (centre of the release zone)" style={{ width: 8, height: 8, transform: 'rotate(45deg)', background: '#40a6ff', border: '1px solid #04121f' }} />
          </Marker>
        ))}
        <Source id="dtc-route" type="geojson" data={route}>
          <Layer id="dtc-route-line" type="line" paint={{ 'line-color': '#facc15', 'line-width': 2, 'line-opacity': 0.7, 'line-dasharray': [2, 1] }} />
        </Source>
        {plan.waypoints.map((w, i) => (
          <Marker key={`wp-${i}`} latitude={w.lat} longitude={w.lon}>
            <div title={`${i + 1} ${w.name}`} style={{ width: 16, height: 16, borderRadius: 8, background: '#facc15', color: '#0b0f08', fontSize: 9, fontWeight: 800, display: 'flex', alignItems: 'center', justifyContent: 'center', opacity: 0.8, border: '1px solid #0b0f08' }}>{i + 1}</div>
          </Marker>
        ))}
        {pts.filter(p => !p.here).map(p => (
          <Marker key={p.segs.join('/')} latitude={p.lat} longitude={p.lon}>
            <div title={p.label} style={{ width: 7, height: 7, borderRadius: 4, background: '#9aa3ad', opacity: 0.7 }} />
          </Marker>
        ))}
        {pts.filter(p => p.here).map(p => (
          <Marker key={p.segs.join('/')} latitude={p.lat} longitude={p.lon} draggable={p.editable}
            onDragEnd={e => place(p.segs, e.lngLat.lat, e.lngLat.lng, getAt(ctx.value, p.segs) as JsonObject)}
            onClick={e => e.originalEvent.stopPropagation()}>
            <div title={`${p.label}${p.editable ? ' (drag to move)' : ''}`} style={{
              padding: '1px 5px', borderRadius: 3, background: '#40a6ff', color: '#04121f', fontSize: 10, fontWeight: 800,
              border: '1px solid #04121f', cursor: p.editable ? 'move' : 'default', whiteSpace: 'nowrap',
            }}>{p.label}</div>
          </Marker>
        ))}
      </Map>
      {slots.length > 0 && !add && !ctx.readOnly && (
        <label style={{ position: 'absolute', left: 8, top: 8, display: 'flex', gap: 6, alignItems: 'center', padding: '4px 8px', borderRadius: 4, background: 'var(--bg-card)', border: '1px solid var(--border)', fontSize: '0.66rem' }}>
          Place
          <select className="vs-input" value={slot?.segs.join('/') ?? ''} onChange={e => setSlotKey(e.target.value)} style={{ fontSize: '0.66rem', padding: '2px 4px' }}>
            {slots.map(q => <option key={q.segs.join('/')} value={q.segs.join('/')}>{q.label}{q.placed ? ' ✓' : ''}</option>)}
          </select>
        </label>
      )}
      <div style={{ position: 'absolute', left: 8, bottom: 8, padding: '4px 8px', borderRadius: 4, background: 'var(--bg-card)', border: '1px solid var(--border)', fontSize: '0.62rem', color: 'var(--text-dim)', pointerEvents: 'none' }}>
        {slot && !add ? <>Click the map to place <b style={{ color: 'var(--text)' }}>{slot.label}</b>. Shaded: release zone, ◆ launch point.</> : add ? <>Click the map to add a point to <b style={{ color: 'var(--text)' }}>{add.label}</b>. Drag a blue marker to move it.</> : <><span style={{ color: '#facc15' }}>●</span> your route · <span style={{ color: '#40a6ff' }}>■</span> this page · <span style={{ color: '#9aa3ad' }}>●</span> other cartridge points</>}
      </div>
    </div>
  )
}
