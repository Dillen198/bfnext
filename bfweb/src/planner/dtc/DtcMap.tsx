import { useMemo, useState } from 'react'
import Map, { Layer, Marker, Source, type MapLayerMouseEvent } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'
import { useTheme } from '@shared/web/theme'
import { mapStyleFor } from '../../lib/mapStyle'
import { getAt, toPointer, type Json, type JsonObject, type Seg } from '../model'
import { addableLists, numbered, positions, slotsOn, type AddableList } from './places'
import { addPoint, addRect, removePoint, shapeLists, shapesOf, type ShapeList } from './shapes'
import { fromDcsXY, toDcsXY, type DcsTerrain } from '../nav'
import { lar, larCentre, larPolygon } from '../f14lar'
import type { Plan } from '../types'
import type { Ctx } from './core'

interface Props {
  /** Points derived from the route (offset aimpoints, VIP/VRP), drawn from their steerpoint. */
  extras?: { lat: number; lon: number; label: string; from: { lat: number; lon: number } }[]
  ctx: Ctx
  plan: Plan
  map: DcsTerrain
  pageSegs: Seg[]
  /** Drawing the route: a click adds a waypoint, a drag moves one, a
   *  right-click deletes it. Null leaves the route as it is. */
  route?: { add: (lat: number, lon: number) => void; move: (i: number, lat: number, lon: number) => void; remove: (i: number) => void } | null
}

/**
 * The cartridge on the map: your route for reference, the points of the page
 * you are on (drag to move, click the map to add), and every other cartridge
 * point faintly.
 */
export default function DtcMap({ ctx, plan, map, pageSegs, extras = [], route = null }: Props) {
  const { theme } = useTheme()
  const mapStyle = useMemo(() => mapStyleFor(theme), [theme])
  const pts = useMemo(() => positions(ctx, map, pageSegs), [ctx, map, pageSegs])
  const lists = useMemo(() => shapeLists(ctx, pageSegs), [ctx, pageSegs])
  const shapes = useMemo(() => lists.flatMap(l => shapesOf(ctx, map, l)), [lists, ctx, map])

  // What a click on the map adds. While the route is drawn, clicks are the route's.
  const tools: Tool[] = route ? [] : [
    ...lists.filter(l => l.editable).map(l => ({ kind: 'shape' as const, list: l })),
    ...addableLists(ctx, pageSegs).map(l => ({ kind: 'point' as const, list: l })),
  ]
  const [toolKey, setToolKey] = useState<string | null>(null)
  const tool = tools.find(t => toPointer(t.list.segs) === toolKey) ?? tools[0] ?? null
  // The line being drawn (null: the next click starts a new one), the F-16
  // geo line a click goes on, and the first corner of an area.
  const [shapeSel, setShapeSel] = useState<number | null>(null)
  const [flag, setFlag] = useState(0)
  const [corner, setCorner] = useState<{ lat: number; lon: number } | null>(null)
  const [full, setFull] = useState<string | null>(null)
  const pickTool = (k: string) => { setToolKey(k); setShapeSel(null); setCorner(null); setFull(null) }
  const add = tool?.kind === 'point' ? tool.list : null
  const drawing = tool?.kind === 'shape' ? tool.list : null
  const drawn = drawing ? shapes.filter(sh => sh.list.segs.join('/') === drawing.segs.join('/')) : []
  const sel = shapeSel != null && drawn.some(sh => sh.index === shapeSel) ? shapeSel : null
  const isDrawing = (l: ShapeList) => !!drawing && l.segs.join('/') === drawing.segs.join('/')

  const slots = tool || route ? [] : slotsOn(ctx, pageSegs)
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

  const shapeGeo = useMemo(() => ({
    type: 'FeatureCollection' as const,
    features: shapes.filter(sh => sh.pts.length >= 2).map(sh => {
      const ring = sh.pts.map(p => [p.lon, p.lat])
      return {
        type: 'Feature' as const,
        properties: { color: sh.list.here ? lineColour(sh) : '#9aa3ad', width: sh.list.here ? 2.5 : 1.2, opacity: sh.list.here ? 0.95 : 0.5 },
        geometry: { type: 'LineString' as const, coordinates: sh.closed ? [...ring, ring[0]] : ring },
      }
    }),
  }), [shapes])

  const routeGeo = useMemo(() => ({
    type: 'FeatureCollection' as const,
    features: plan.waypoints.length < 2 ? [] : [{
      type: 'Feature' as const, properties: {},
      geometry: { type: 'LineString' as const, coordinates: plan.waypoints.map(w => [w.lon, w.lat]) },
    }],
  }), [plan.waypoints])

  const view = useMemo(() => {
    const all: [number, number][] = [...plan.waypoints.map(w => [w.lat, w.lon] as [number, number]), ...pts.filter(p => p.here).map(p => [p.lat, p.lon] as [number, number])]
    if (!all.length) return { latitude: map.terrain.center[0], longitude: map.terrain.center[1], zoom: 6 }
    if (all.length === 1) return { latitude: all[0][0], longitude: all[0][1], zoom: 8 }
    // The whole route and this page's points in view.
    const lats = all.map(p => p[0])
    const lons = all.map(p => p[1])
    return {
      bounds: [[Math.min(...lons), Math.min(...lats)], [Math.max(...lons), Math.max(...lats)]] as [[number, number], [number, number]],
      fitBoundsOptions: { padding: 40, maxZoom: 10 },
    }
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
    const ll = { lat: e.lngLat.lat, lon: e.lngLat.lng }
    if (route) { route.add(ll.lat, ll.lon); return }
    if (drawing) {
      if (drawing.rect) {
        if (!corner) { setCorner(ll); return }
        const v = addRect(ctx, drawing, corner, ll, map)
        setCorner(null)
        if (v) ctx.set(drawing.segs, v)
        else setFull(`${drawing.label} is full.`)
        return
      }
      const v = addPoint(ctx, drawing, { shape: sel, flag }, ll.lat, ll.lon, map)
      if (!v) { setFull(sel == null ? `${drawing.label} is full.` : 'This line is full: start a new one.'); return }
      setFull(null)
      ctx.set(drawing.segs, v)
      // A new line keeps taking the clicks that follow.
      if (sel == null && drawing.mode === 'shapes') setShapeSel(((getAt(ctx.value, drawing.segs) as Json[] | undefined) ?? []).length)
      return
    }
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
        dragRotate={false} attributionControl={false} cursor={tool || slot || route ? 'crosshair' : 'grab'} onClick={click}>
        <Source id="dtc-lar" type="geojson" data={lars.geo}>
          <Layer id="dtc-lar-fill" type="fill" paint={{ 'fill-color': '#40a6ff', 'fill-opacity': 0.12 }} />
          <Layer id="dtc-lar-line" type="line" paint={{ 'line-color': '#40a6ff', 'line-width': 1.2, 'line-dasharray': [3, 2] }} />
        </Source>
        {lars.launches.map((p, i) => (
          <Marker key={`launch-${i}`} latitude={p.lat} longitude={p.lon}>
            <div title="Launch point (centre of the release zone)" style={{ width: 8, height: 8, transform: 'rotate(45deg)', background: '#40a6ff', border: '1px solid #04121f' }} />
          </Marker>
        ))}
        <Source id="dtc-offsets" type="geojson" data={{
          type: 'FeatureCollection',
          features: extras.map(e => ({ type: 'Feature', properties: {}, geometry: { type: 'LineString', coordinates: [[e.from.lon, e.from.lat], [e.lon, e.lat]] } })),
        }}>
          <Layer id="dtc-offsets-line" type="line" paint={{ 'line-color': '#f0a030', 'line-width': 1.2, 'line-dasharray': [2, 2] }} />
        </Source>
        {extras.map((e, i) => (
          <Marker key={`x-${i}`} latitude={e.lat} longitude={e.lon}>
            <div title={e.label} style={{ padding: '0 4px', borderRadius: 3, background: '#f0a030', color: '#1a1004', fontSize: 9, fontWeight: 800, border: '1px solid #1a1004', whiteSpace: 'nowrap' }}>{e.label}</div>
          </Marker>
        ))}
        <Source id="dtc-shapes" type="geojson" data={shapeGeo}>
          <Layer id="dtc-shapes-line" type="line" layout={{ 'line-join': 'round', 'line-cap': 'round' }}
            paint={{ 'line-color': ['get', 'color'], 'line-width': ['get', 'width'], 'line-opacity': ['get', 'opacity'] }} />
        </Source>
        {shapes.filter(sh => sh.list.here).flatMap(sh => sh.pts.map((p, j) => (
          <Marker key={`v-${toPointer(p.segs)}`} latitude={p.lat} longitude={p.lon} draggable={sh.list.editable}
            onDragEnd={e => place(p.segs, e.lngLat.lat, e.lngLat.lng, getAt(ctx.value, p.segs) as JsonObject)}
            onClick={e => { e.originalEvent.stopPropagation(); if (isDrawing(sh.list) && sh.list.mode === 'shapes') setShapeSel(sh.index) }}>
            <div title={`${sh.name}, point ${j + 1}${sh.list.editable ? ' (drag to move, right-click to delete)' : ''}`}
              onContextMenu={e => { if (sh.list.editable) { e.preventDefault(); ctx.set(sh.list.segs, removePoint(ctx, sh.list, p.segs)) } }}
              style={{
                width: 10, height: 10, borderRadius: 5, background: lineColour(sh), border: '1.5px solid #0b0f08', cursor: sh.list.editable ? 'move' : 'default',
                boxShadow: isDrawing(sh.list) && (sh.list.mode === 'flags' ? sh.index === flag : sh.index === sel) ? '0 0 0 3px rgba(255,255,255,0.5)' : undefined,
              }} />
          </Marker>
        )))}
        {shapes.filter(sh => sh.list.here && sh.pts.length).map(sh => (
          <Marker key={`n-${toPointer(sh.list.segs)}-${sh.index}`} latitude={sh.pts[0].lat} longitude={sh.pts[0].lon} anchor="bottom-left" offset={[6, -4]}>
            <div style={{ fontSize: 10, fontWeight: 800, color: lineColour(sh), textShadow: '0 0 3px #000, 0 0 3px #000', whiteSpace: 'nowrap', pointerEvents: 'none' }}>{sh.name}</div>
          </Marker>
        ))}
        {corner && (
          <Marker latitude={corner.lat} longitude={corner.lon}>
            <div title="First corner: click the opposite one" style={{ width: 10, height: 10, border: '2px solid #fff', background: 'transparent' }} />
          </Marker>
        )}
        <Source id="dtc-route" type="geojson" data={routeGeo}>
          <Layer id="dtc-route-line" type="line" paint={{ 'line-color': '#facc15', 'line-width': 2, 'line-opacity': 0.7, 'line-dasharray': [2, 1] }} />
        </Source>
        {plan.waypoints.map((w, i) => (
          <Marker key={`wp-${i}`} latitude={w.lat} longitude={w.lon} draggable={!!route}
            onDragEnd={e => route?.move(i, e.lngLat.lat, e.lngLat.lng)}
            onClick={e => { if (route) e.originalEvent.stopPropagation() }}>
            <div title={`${i + 1} ${w.name}${route ? ' (drag to move, right-click to delete)' : ''}`}
              onContextMenu={e => { if (route) { e.preventDefault(); route.remove(i) } }}
              style={{
                width: route ? 20 : 16, height: route ? 20 : 16, borderRadius: 10, background: '#facc15', color: '#0b0f08', fontSize: 9, fontWeight: 800,
                display: 'flex', alignItems: 'center', justifyContent: 'center', opacity: route ? 1 : 0.8, border: '1px solid #0b0f08', cursor: route ? 'move' : 'default',
              }}>{i + 1}</div>
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
      {tool && (
        <div style={{ position: 'absolute', left: 8, top: 8, right: 8, display: 'flex', gap: 6, alignItems: 'center', flexWrap: 'wrap', pointerEvents: 'none' }}>
          <label style={{ ...panel, pointerEvents: 'auto' }}>
            Click adds
            <select className="vs-input" value={toPointer(tool.list.segs)} onChange={e => pickTool(e.target.value)} style={{ fontSize: '0.66rem', padding: '2px 4px', maxWidth: 220 }}>
              {tools.map(t => <option key={toPointer(t.list.segs)} value={toPointer(t.list.segs)}>{t.kind === 'shape' ? t.list.label : `${t.list.label} (point)`}</option>)}
            </select>
          </label>
          {drawing?.mode === 'flags' && (
            <span style={{ ...panel, pointerEvents: 'auto' }}>
              {drawing.flags.map((f, i) => (
                <button key={f} type="button" aria-pressed={flag === i} onClick={() => setFlag(i)}
                  style={{ padding: '2px 8px', fontSize: '0.64rem', fontWeight: 700, borderRadius: 4, cursor: 'pointer', color: '#0b0f08', background: PALETTE[i % PALETTE.length], border: flag === i ? '2px solid #fff' : '2px solid transparent' }}>
                  Line {i + 1}
                </button>
              ))}
            </span>
          )}
          {drawing?.mode === 'shapes' && !drawing.rect && (
            <span style={{ ...panel, pointerEvents: 'auto' }}>
              <select className="vs-input" value={sel ?? 'new'} onChange={e => setShapeSel(e.target.value === 'new' ? null : Number(e.target.value))} aria-label="Line to add to" style={{ fontSize: '0.66rem', padding: '2px 4px' }}>
                <option value="new">Start a new one</option>
                {drawn.map(sh => <option key={sh.index} value={sh.index}>Add to {sh.name} ({sh.pts.length}{drawing.maxPts ? `/${drawing.maxPts}` : ''})</option>)}
              </select>
              {sel != null && <button type="button" className="vs-btn-ghost" style={{ padding: '2px 6px', fontSize: '0.64rem' }} onClick={() => setShapeSel(null)}>Finish</button>}
            </span>
          )}
        </div>
      )}
      {slots.length > 0 && !add && !ctx.readOnly && (
        <label style={{ position: 'absolute', left: 8, top: 8, display: 'flex', gap: 6, alignItems: 'center', padding: '4px 8px', borderRadius: 4, background: 'var(--bg-card)', border: '1px solid var(--border)', fontSize: '0.66rem' }}>
          Place
          <select className="vs-input" value={slot?.segs.join('/') ?? ''} onChange={e => setSlotKey(e.target.value)} style={{ fontSize: '0.66rem', padding: '2px 4px' }}>
            {slots.map(q => <option key={q.segs.join('/')} value={q.segs.join('/')}>{q.label}{q.placed ? ' ✓' : ''}</option>)}
          </select>
        </label>
      )}
      <div style={{ position: 'absolute', left: 8, bottom: 8, padding: '4px 8px', borderRadius: 4, background: 'var(--bg-card)', border: '1px solid var(--border)', fontSize: '0.62rem', color: 'var(--text-dim)', pointerEvents: 'none' }}>
        {full ? <span style={{ color: '#f0a030' }}>{full}</span> : drawing ? hint(drawing, sel, flag, !!corner, drawn) : route ? <>Click the map to add waypoint <b style={{ color: 'var(--text)' }}>{plan.waypoints.length + 1}</b>. Drag a waypoint to move it, right-click to delete it.</> : slot && !add ? <>Click the map to place <b style={{ color: 'var(--text)' }}>{slot.label}</b>. Shaded: release zone, ◆ launch point.</> : add ? <>Click the map to add a point to <b style={{ color: 'var(--text)' }}>{add.label}</b>. Drag a blue marker to move it.</> : <><span style={{ color: '#facc15' }}>●</span> your route · <span style={{ color: '#40a6ff' }}>■</span> this page · <span style={{ color: '#9aa3ad' }}>●</span> other cartridge points and lines</>}
      </div>
    </div>
  )
}

type Tool = { kind: 'shape'; list: ShapeList } | { kind: 'point'; list: AddableList }

const panel = {
  display: 'flex', gap: 6, alignItems: 'center', padding: '4px 8px', borderRadius: 4,
  background: 'var(--bg-card)', border: '1px solid var(--border)', fontSize: '0.66rem',
} as const

/** One colour per line, the same in the toolbar and on the map. */
const PALETTE = ['#f472b6', '#34d399', '#fbbf24', '#a78bfa', '#f87171', '#60a5fa', '#fb923c', '#2dd4bf']
const lineColour = (sh: { index: number }) => PALETTE[sh.index % PALETTE.length]

function hint(list: ShapeList, sel: number | null, flag: number, cornerSet: boolean, drawn: { index: number; name: string; pts: unknown[] }[]) {
  const b = (t: string) => <b style={{ color: 'var(--text)' }}>{t}</b>
  if (list.rect) {
    return cornerSet
      ? <>Click the opposite corner.</>
      : <>Click one corner of a new {b(list.label)} area, then the opposite one. Drag a corner to move it; right-click one to delete the area.</>
  }
  if (list.mode === 'flags') return <>Click to add a point to {b(`line ${flag + 1}`)}. Drag to move, right-click to delete.</>
  const cur = drawn.find(d => d.index === sel)
  const few = cur && cur.pts.length < list.minPts ? ` It needs at least ${list.minPts} points.` : ''
  return cur
    ? <>Click to add to {b(cur.name)}.{few} Right-click a point to delete it.</>
    : <>Click to start a new line in {b(list.label)}; keep clicking to add its points.</>
}
