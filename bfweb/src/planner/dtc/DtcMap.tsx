import { useMemo } from 'react'
import Map, { Layer, Marker, Source, type MapLayerMouseEvent } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'
import { useTheme } from '@shared/web/theme'
import { mapStyleFor } from '../../lib/mapStyle'
import { clone, getAt, patternSegs, type Json, type JsonObject, type Seg } from '../model'
import { fromDcsXY, toDcsXY, type DcsTerrain } from '../nav'
import type { Plan } from '../types'
import { isObj, rowLabel, type Ctx } from './editor'

/** A cartridge item with a map position. */
interface Pos {
  segs: Seg[]
  lat: number
  lon: number
  label: string
  /** On the page being edited (bright, draggable) or elsewhere (faint). */
  here: boolean
  editable: boolean
}

const under = (segs: Seg[], root: Seg[]) => root.every((s, i) => String(segs[i]) === String(s))

/** Every object with a DCS map position (x north, y east) in the cartridge. */
function positions(ctx: Ctx, map: DcsTerrain, pageSegs: Seg[]): Pos[] {
  const out: Pos[] = []
  const walk = (v: Json | undefined, segs: Seg[], managed: boolean, depth: number) => {
    if (out.length > 600 || depth > 9 || v === null || typeof v !== 'object') return
    const a = segs.length ? ctx.idx.get(segs) : undefined
    const m = managed || !!a?.managed
    if (isObj(v) && typeof v.x === 'number' && typeof v.y === 'number' && (v.x !== 0 || v.y !== 0)) {
      const ll = fromDcsXY(v.x, v.y, map.terrain)
      const parent = segs.slice(0, -1)
      out.push({
        segs, lat: ll.lat, lon: ll.lon,
        label: rowLabel(ctx, parent, segs[segs.length - 1], v),
        here: under(segs, pageSegs), editable: !m && !ctx.readOnly,
      })
    }
    const entries: [Seg, Json][] = Array.isArray(v) ? v.map((x, i) => [i, x]) : Object.entries(v)
    for (const [k, x] of entries) walk(x, [...segs, k], m, depth + 1)
  }
  walk(ctx.value, [], false, 0)
  return out
}

/** The first editable list of positioned items on the page: where a map click adds one. */
export function addableList(ctx: Ctx, pageSegs: Seg[]): { segs: Seg[]; template: JsonObject; label: string } | null {
  const queue: Seg[][] = [pageSegs]
  while (queue.length) {
    const segs = queue.shift()!
    const v = getAt(ctx.value, segs)
    const a = ctx.idx.get(segs)
    if (a?.managed || a?.hidden) continue
    if (Array.isArray(v)) {
      const tmpl = (a?.item ?? v[v.length - 1] ?? null) as Json
      const full = a?.maxItems != null && v.length >= a.maxItems
      if (isObj(tmpl) && typeof tmpl.x === 'number' && typeof tmpl.y === 'number' && !full && a?.fixedLength == null && !ctx.readOnly) {
        return { segs, template: tmpl, label: a?.label ?? 'point' }
      }
      continue
    }
    if (isObj(v)) for (const k of Object.keys(v)) if (v[k] !== null && typeof v[k] === 'object') queue.push([...segs, k])
  }
  return null
}

/** Whether a page has anything to show on a map: route-filled nav, a list
 *  of points to add to, or items that carry a map position. Countermeasures,
 *  radios, RWR tables and the like do not. */
export function pageHasMap(ctx: Ctx, segs: Seg[], routeFilled: boolean): boolean {
  if (routeFilled || addableList(ctx, segs)) return true
  // The route fills something deeper in the page (the F-14's flight plan 2).
  const page = segs.map(s => (typeof s === 'number' ? '[]' : String(s)))
  if (ctx.idx.ann.fields.some(f => f.managed === 'route' && patternSegs(f.path).length > page.length && page.every((p, i) => {
    const q = patternSegs(f.path)[i]
    return q === p || q === '[]' || q === '{}'
  }))) return true
  const walk = (v: Json | undefined, depth: number): boolean => {
    if (depth > 8 || v === null || typeof v !== 'object') return false
    if (isObj(v) && typeof v.x === 'number' && typeof v.y === 'number') return true
    return (Array.isArray(v) ? v : Object.values(v)).some(x => walk(x, depth + 1))
  }
  return walk(getAt(ctx.value, segs), 0)
}

/** Number a new list item on from the last: num/number fields, and an id like
 *  "DEST81" whose numeric part keeps the template's offset from its number. */
function numbered(list: Json[], tmpl: JsonObject): JsonObject {
  const item = clone(tmpl)
  const last = (list[list.length - 1] ?? null) as JsonObject | null
  for (const f of ['num', 'number', 'Number', 'wypt_num']) {
    if (typeof item[f] === 'number') {
      const prev = last && typeof last[f] === 'number' ? (last[f] as number) : (tmpl[f] as number) - 1
      const n = prev + 1
      if (typeof item.id === 'string') {
        const m = /^(.*?)(\d+)$/.exec(item.id)
        if (m) item.id = `${m[1]}${Number(m[2]) - (tmpl[f] as number) + n}`
      }
      item[f] = n
    }
  }
  return item
}

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
      <div style={{ position: 'absolute', left: 8, bottom: 8, padding: '4px 8px', borderRadius: 4, background: 'var(--bg-card)', border: '1px solid var(--border)', fontSize: '0.62rem', color: 'var(--text-dim)', pointerEvents: 'none' }}>
        {add ? <>Click the map to add a point to <b style={{ color: 'var(--text)' }}>{add.label}</b>. Drag a blue marker to move it.</> : <><span style={{ color: '#facc15' }}>●</span> your route · <span style={{ color: '#40a6ff' }}>■</span> this page · <span style={{ color: '#9aa3ad' }}>●</span> other cartridge points</>}
      </div>
    </div>
  )
}
