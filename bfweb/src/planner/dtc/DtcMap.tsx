import { useMemo, useState } from 'react'
import Map, { Layer, Marker, Source, type MapLayerMouseEvent } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'
import { useTheme } from '@shared/web/theme'
import { MAP_LOOKS, styleFor, type MapLook } from '../../pages/groundwar/mapStyles'
import { getAt, toPointer, type Json, type JsonObject, type Seg } from '../model'
import { addableLists, numbered, positions, slotsOn, type AddableList } from './places'
import { addPoint, addRect, removePoint, shapeLists, shapesOf, type ShapeList } from './shapes'
import { toDcsXY, type DcsTerrain } from '../nav'
import { drawings, moveHandle } from './drawings'
import type { Plan, PlanWaypoint } from '../types'
import type { Ctx } from './core'
import IntelLayers from './IntelLayers'
import PickCard from './PickCard'
import { allPicks, LAYERS, sendTargets, sendTo, type Intel, type LayerKey, type Pick, type SendTarget } from './picks'

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
  /** The side's picture (briefing report and live JTAC/AWACS/recon contacts). */
  intel?: Intel | null
  side?: string
  /** Change the plan's route (picked targets sent to it). */
  onWaypoints?: (w: PlanWaypoint[]) => void
}

/**
 * The cartridge on the map: your route for reference, the points of the page
 * you are on (drag to move, click the map to add), and every other cartridge
 * point faintly.
 */
export default function DtcMap({ ctx, plan, map, pageSegs, extras = [], route = null, intel = null, side = 'Blue', onWaypoints }: Props) {
  const { theme } = useTheme()
  const [look, setLook] = useState<MapLook>(() => saved('dtc-map-look', 'sat') as MapLook)
  const mapStyle = useMemo(() => styleFor(look, theme), [look, theme])
  const [on, setOn] = useState<Record<LayerKey, boolean>>(() => {
    try { return { ...DEFAULT_LAYERS, ...JSON.parse(saved('dtc-map-layers', '{}')) } } catch { return DEFAULT_LAYERS }
  })
  const [layersOpen, setLayersOpen] = useState(false)
  const toggle = (k: LayerKey) => setOn(o => { const n = { ...o, [k]: !o[k] }; keep('dtc-map-layers', JSON.stringify(n)); return n })
  const pickLook = (l: MapLook) => { setLook(l); keep('dtc-map-look', l) }

  // What is picked: one thing clicked, or everything in a box.
  const [picked, setPicked] = useState<Pick[]>([])
  const [selecting, setSelecting] = useState(false)
  const [boxCorner, setBoxCorner] = useState<{ lat: number; lon: number } | null>(null)
  const pickedKeys = useMemo(() => new Set(picked.map(p => p.key)), [picked])
  const onPick = (p: Pick) => {
    if (selecting) setPicked(ps => (ps.some(x => x.key === p.key) ? ps.filter(x => x.key !== p.key) : [...ps, p]))
    else setPicked(ps => (ps.length === 1 && ps[0].key === p.key ? [] : [p]))
  }
  const send = (t: SendTarget): string => {
    const r = sendTo(ctx, plan, t, picked, map)
    if (r.waypoints) onWaypoints?.(r.waypoints)
    if (r.writes?.length) (ctx.setMany ?? ((pairs: [Seg[], Json][]) => pairs.forEach(([s, v]) => ctx.set(s, v))))(r.writes)
    return r.added
      ? `Sent ${r.added} to ${t.label.toLowerCase()}.${r.skipped ? ` ${r.skipped} did not fit.` : ''}`
      : 'Nothing sent: it is full.'
  }
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

  // What DCS's own editor draws around items: threat rings, CAP orbits,
  // corridor edges, JDAM launch zones with launch point and heading.
  const art = useMemo(() => drawings(ctx, map, pageSegs), [ctx, map, pageSegs])
  const artGeo = useMemo(() => ({
    type: 'FeatureCollection' as const,
    features: art.lines.map(l => ({
      type: 'Feature' as const,
      properties: { color: l.color, dashed: l.dashed ? 1 : 0, opacity: l.here ? 0.95 : 0.4, width: l.here ? 1.8 : 1 },
      geometry: { type: 'LineString' as const, coordinates: l.coords },
    })),
  }), [art])

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
    let all: [number, number][] = [...plan.waypoints.map(w => [w.lat, w.lon] as [number, number]), ...pts.filter(p => p.here).map(p => [p.lat, p.lon] as [number, number])]
    // Nothing drawn yet: frame the side's picture (its objectives and contacts).
    if (!all.length && intel) all = allPicks(intel, side, k => k === 'objective' || k === 'contact').map(p => [p.lat, p.lon])
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
    if (selecting && intel) {
      // Two clicks, opposite corners: everything shown inside is picked.
      if (!boxCorner) { setBoxCorner(ll); return }
      const [s, n] = [Math.min(boxCorner.lat, ll.lat), Math.max(boxCorner.lat, ll.lat)]
      const [w, ea] = [Math.min(boxCorner.lon, ll.lon), Math.max(boxCorner.lon, ll.lon)]
      const inside = allPicks(intel, side, k => on[k as LayerKey] ?? false).filter(p => p.lat >= s && p.lat <= n && p.lon >= w && p.lon <= ea)
      setPicked(inside)
      setBoxCorner(null)
      return
    }
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
      <Map key={theme} mapStyle={mapStyle as never} style={{ width: '100%', height: '100%' }} initialViewState={view}
        dragRotate={false} attributionControl={false} cursor={selecting || tool || slot || route ? 'crosshair' : 'grab'} onClick={click}>
        {intel && <IntelLayers intel={intel} side={side} on={on} picked={pickedKeys} onPick={onPick} />}
        {boxCorner && (
          <Marker latitude={boxCorner.lat} longitude={boxCorner.lon}>
            <div title="First corner of the area" style={{ width: 12, height: 12, border: '2px dashed #fff', pointerEvents: 'none' }} />
          </Marker>
        )}
        <Source id="dtc-art" type="geojson" data={artGeo}>
          <Layer id="dtc-art-solid" type="line" filter={['==', ['get', 'dashed'], 0]} layout={{ 'line-join': 'round' }}
            paint={{ 'line-color': ['get', 'color'], 'line-width': ['get', 'width'], 'line-opacity': ['get', 'opacity'] }} />
          <Layer id="dtc-art-dashed" type="line" filter={['==', ['get', 'dashed'], 1]}
            paint={{ 'line-color': ['get', 'color'], 'line-width': ['get', 'width'], 'line-opacity': ['get', 'opacity'], 'line-dasharray': [3, 2] }} />
        </Source>
        {art.labels.filter(l => l.text).map((l, i) => (
          <Marker key={`al-${i}`} latitude={l.lat} longitude={l.lon} anchor="top-left" offset={[8, 4]}>
            <div style={{ fontSize: 9, fontWeight: 800, color: l.color, opacity: l.here ? 1 : 0.6, textShadow: '0 0 3px #000, 0 0 3px #000', whiteSpace: 'nowrap', pointerEvents: 'none' }}>{l.text}</div>
          </Marker>
        ))}
        {art.handles.map(h => {
          const live = h.here && !ctx.readOnly
          return (
            <Marker key={h.key} latitude={h.lat} longitude={h.lon} draggable={live}
              onDragEnd={e => ctx.set(h.segs, moveHandle(ctx, h, e.lngLat.lat, e.lngLat.lng, map))}
              onClick={e => e.originalEvent.stopPropagation()}>
              <div title={h.kind === 'launch' ? `${h.label}: launch point${live ? ' (drag within the zone)' : ''}` : `Attack heading ${h.label}${live ? ' (drag to turn the attack)' : ''}`}
                style={{ position: 'relative', cursor: live ? 'move' : 'default', opacity: h.here ? 1 : 0.5 }}>
                {h.kind === 'launch'
                  ? <div style={{ width: 10, height: 10, transform: 'rotate(45deg)', background: h.color, border: '1.5px solid #200' }} />
                  : <div style={{ width: 12, height: 12, borderRadius: 6, background: '#200', border: `2px solid ${h.color}` }} />}
                <span style={{ position: 'absolute', left: 14, top: -2, fontSize: 9, fontWeight: 800, color: h.color, textShadow: '0 0 3px #000, 0 0 3px #000', whiteSpace: 'nowrap', pointerEvents: 'none' }}>{h.label}</span>
              </div>
            </Marker>
          )
        })}
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
      <div style={{ position: 'absolute', right: 8, top: 8, display: 'flex', flexDirection: 'column', alignItems: 'flex-end', gap: 6, zIndex: 2 }}>
        <span style={{ ...panel, padding: 2, gap: 2 }}>
          {MAP_LOOKS.map(l => (
            <button key={l.key} type="button" title={l.hint} aria-pressed={look === l.key} onClick={() => pickLook(l.key)}
              style={{ padding: '2px 7px', fontSize: '0.6rem', fontWeight: 800, borderRadius: 3, cursor: 'pointer', border: 'none', background: look === l.key ? 'var(--accent)' : 'transparent', color: look === l.key ? '#0b0f08' : 'var(--text)' }}>
              {l.label}
            </button>
          ))}
          {intel && (
            <button type="button" aria-expanded={layersOpen} onClick={() => setLayersOpen(v => !v)}
              style={{ padding: '2px 7px', fontSize: '0.6rem', fontWeight: 800, borderRadius: 3, cursor: 'pointer', border: 'none', background: layersOpen ? 'var(--bg-elevated)' : 'transparent', color: 'var(--text)' }}>
              LAYERS
            </button>
          )}
          {intel && (
            <button type="button" aria-pressed={selecting} title="Pick everything in an area: click two opposite corners"
              onClick={() => { setSelecting(v => !v); setBoxCorner(null); setPicked([]) }}
              style={{ padding: '2px 7px', fontSize: '0.6rem', fontWeight: 800, borderRadius: 3, cursor: 'pointer', border: 'none', background: selecting ? '#f0a030' : 'transparent', color: selecting ? '#0b0f08' : 'var(--text)' }}>
              SELECT AREA
            </button>
          )}
        </span>
        {layersOpen && intel && (
          <div style={{ ...panel, flexDirection: 'column', alignItems: 'stretch', gap: 3 }}>
            {LAYERS.map(l => (
              <label key={l.key} style={{ display: 'flex', gap: 6, alignItems: 'center', cursor: 'pointer' }}>
                <input type="checkbox" checked={on[l.key]} onChange={() => toggle(l.key)} /> {l.label}
                {l.key === 'contact' && <span style={{ color: 'var(--text-dim)' }}>({(intel.tac?.ground ?? []).filter(c => c.side !== side).length})</span>}
              </label>
            ))}
            {!intel.tac && <span style={{ color: 'var(--text-dim)', fontSize: '0.6rem' }}>No live contacts: log in with a coalition to see them.</span>}
          </div>
        )}
      </div>
      {picked.length > 0 && (
        <PickCard key={picked.map(p => p.key).join('|')} picks={picked} map={map} readOnly={ctx.readOnly}
          targets={sendTargets(ctx, plan, pageSegs)} onSend={send}
          onRemove={k => setPicked(ps => ps.filter(p => p.key !== k))} onClose={() => setPicked([])} />
      )}
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
        {selecting ? <>{boxCorner ? 'Click the opposite corner of the area.' : 'Click one corner of the area to pick from, then the opposite one. Or click things one by one.'}</> : full ? <span style={{ color: '#f0a030' }}>{full}</span> : drawing ? hint(drawing, sel, flag, !!corner, drawn) : route ? <>Click the map to add waypoint <b style={{ color: 'var(--text)' }}>{plan.waypoints.length + 1}</b>. Drag a waypoint to move it, right-click to delete it.</> : slot && !add ? <>Click the map to place <b style={{ color: 'var(--text)' }}>{slot.label}</b>. Red outline: release zone; drag ◆ the launch point or ○ the heading handle.</> : add ? <>Click the map to add a point to <b style={{ color: 'var(--text)' }}>{add.label}</b>. Drag a blue marker to move it.</> : <><span style={{ color: '#facc15' }}>●</span> your route · <span style={{ color: '#40a6ff' }}>■</span> this page · <span style={{ color: '#9aa3ad' }}>●</span> other cartridge points and lines</>}
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

const DEFAULT_LAYERS: Record<LayerKey, boolean> = { front: true, objective: true, threat: true, task: true, contact: true, air: false, bullseye: true }

/** Per-viewer map choices; the map works the same without storage. */
function saved(key: string, fallback: string): string {
  try { return localStorage.getItem(key) ?? fallback } catch { return fallback }
}
function keep(key: string, v: string) {
  try { localStorage.setItem(key, v) } catch { /* private window */ }
}
