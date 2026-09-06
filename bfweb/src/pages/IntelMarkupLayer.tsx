import { useEffect, useRef, useState } from 'react'
import { Polyline, Rectangle, Circle, Marker, useMap, useMapEvents } from 'react-leaflet'
import L from 'leaflet'
import type { IntelMarkup, IntelMarkupKind } from '../api'

export type MarkupTool = 'select' | 'pencil' | 'line' | 'rect' | 'circle' | 'x'

type LL = [number, number]

function xIcon(color: string, size: number): L.DivIcon {
  return L.divIcon({
    className: '',
    iconSize: [size, size],
    iconAnchor: [size / 2, size / 2],
    html: `<svg width="${size}" height="${size}" viewBox="0 0 24 24" stroke="${color}"
      stroke-width="3.5" stroke-linecap="round" style="filter:drop-shadow(0 1px 2px #000a)">
      <line x1="5" y1="5" x2="19" y2="19"/><line x1="19" y1="5" x2="5" y2="19"/></svg>`,
  })
}

/** Renders coalition markup and, when a drawing tool is active, captures the
 *  strokes/shapes the user draws and hands finished ones to `onAdd`. */
export default function IntelMarkupLayer({
  items, tool, color, width, selectedId, onAdd, onSelect, visible,
}: {
  items: IntelMarkup[]
  tool: MarkupTool
  color: string
  width: number
  selectedId: string | null
  onAdd: (kind: IntelMarkupKind, points: LL[]) => void
  onSelect: (id: string | null) => void
  visible: boolean
}) {
  const map = useMap()
  const [draft, setDraft] = useState<LL[] | null>(null)      // points committed so far
  const [cursor, setCursor] = useState<LL | null>(null)      // live pointer for previews
  const drawingPencil = useRef(false)

  // Toggle map drag / cursor while a tool is active.
  useEffect(() => {
    const c = map.getContainer()
    if (tool === 'select') {
      map.dragging.enable()
      c.style.cursor = ''
    } else {
      c.style.cursor = 'crosshair'
    }
    return () => { map.dragging.enable(); c.style.cursor = '' }
  }, [tool, map])

  useMapEvents({
    mousedown: e => {
      if (tool === 'pencil') {
        drawingPencil.current = true
        map.dragging.disable()
        setDraft([[e.latlng.lat, e.latlng.lng]])
      }
    },
    mousemove: e => {
      const p: LL = [e.latlng.lat, e.latlng.lng]
      if (tool === 'pencil' && drawingPencil.current) {
        setDraft(d => (d ? [...d, p] : [p]))
      } else if ((tool === 'line' || tool === 'rect' || tool === 'circle') && draft) {
        setCursor(p)
      }
    },
    mouseup: () => {
      if (tool === 'pencil' && drawingPencil.current) {
        drawingPencil.current = false
        map.dragging.enable()
        setDraft(d => {
          if (d && d.length >= 2) onAdd('pencil', d)
          return null
        })
      }
    },
    click: e => {
      const p: LL = [e.latlng.lat, e.latlng.lng]
      if (tool === 'select') { onSelect(null); return }
      if (tool === 'x') { onAdd('x', [p]); return }
      if (tool === 'line' || tool === 'rect' || tool === 'circle') {
        if (!draft) { setDraft([p]); setCursor(p) }
        else {
          onAdd(tool, [draft[0], p])
          setDraft(null); setCursor(null)
        }
      }
    },
  })

  // Esc cancels an in-progress shape.
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') { setDraft(null); setCursor(null); drawingPencil.current = false; map.dragging.enable() } }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [map])

  if (!visible) return null

  const renderShape = (m: { kind: string; points: LL[]; color: string; width: number }, key: string, id?: string) => {
    const sel = id != null && id === selectedId
    const opts = {
      color: m.color, weight: m.width + (sel ? 2 : 0), opacity: 0.95,
      fillOpacity: m.kind === 'rect' || m.kind === 'circle' ? 0.08 : 0,
      dashArray: sel ? '6 4' : undefined,
    }
    const handlers = id != null ? { click: (e: L.LeafletMouseEvent) => { L.DomEvent.stop(e); onSelect(id) } } : undefined
    switch (m.kind) {
      case 'pencil':
      case 'line':
        return <Polyline key={key} positions={m.points} pathOptions={opts} eventHandlers={handlers} />
      case 'rect':
        return <Rectangle key={key} bounds={L.latLngBounds(m.points[0], m.points[1] ?? m.points[0])} pathOptions={opts} eventHandlers={handlers} />
      case 'circle': {
        const r = m.points[1] ? L.latLng(m.points[0]).distanceTo(L.latLng(m.points[1])) : 1
        return <Circle key={key} center={m.points[0]} radius={r} pathOptions={opts} eventHandlers={handlers} />
      }
      case 'x':
        return <Marker key={key} position={m.points[0]} icon={xIcon(m.color, 22 + m.width)}
          eventHandlers={id != null ? { click: () => onSelect(id) } : undefined} />
      default:
        return null
    }
  }

  return (
    <>
      {items.map(m => renderShape(m as unknown as { kind: string; points: LL[]; color: string; width: number }, m.id, m.id))}
      {draft && draft.length > 0 && renderShape(
        {
          kind: tool === 'select' ? 'pencil' : tool,
          points: cursor && tool !== 'pencil' ? [draft[0], cursor] : draft,
          color, width,
        },
        'draft',
      )}
    </>
  )
}
