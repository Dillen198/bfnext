// Mouse handling on the map, RTS style: left-click selects (markers handle
// their own clicks), Shift+drag rubber-bands a box, right-click gives the
// selection an order on the base nearest the cursor, and hovering previews
// that order. Listeners sit on the map container itself so they see events
// from the canvas and the markers alike.
import type { Map as MlMap } from 'maplibre-gl'
import type { GroundPicture } from '../../api'

/** How far, in screen pixels, the cursor snaps to a base. */
const SNAP_PX = 72
const DRAG_PX = 5

export interface InputHooks {
  pic: () => GroundPicture | null
  /** The cursor moved; `obj` is the base it snapped to, if any. */
  hover: (x: number, y: number, obj: number | null) => void
  leave: () => void
  /** A left click on bare map (not a marker, not a drag), and where. */
  click: (obj: number | null, at: [number, number]) => void
  /** A right click anywhere on the map, and where. */
  context: (obj: number | null, at: [number, number]) => void
  box: (ids: number[], add: boolean) => void
}

export interface InputHandle {
  detach: () => void
  /** True if the last mouse press turned into a drag (so it wasn't a click). */
  wasDrag: () => boolean
}

export function snapObjective(map: MlMap, pic: GroundPicture | null, x: number, y: number): number | null {
  if (!pic) return null
  let best: number | null = null
  let bestD = SNAP_PX
  for (const o of pic.objectives) {
    const q = map.project([o.pos[1], o.pos[0]])
    const d = Math.hypot(q.x - x, q.y - y)
    if (d < bestD) {
      bestD = d
      best = o.id
    }
  }
  return best
}

export function attachInput(map: MlMap, boxEl: HTMLDivElement, hooks: InputHooks): InputHandle {
  const el = map.getContainer()
  let down: { x: number; y: number } | null = null
  let dragged = false
  let box: { x0: number; y0: number; x1: number; y1: number; add: boolean } | null = null
  /** Last cursor position over the map, so a moving map re-snaps the preview. */
  let last: { x: number; y: number } | null = null

  const local = (e: MouseEvent) => {
    const r = el.getBoundingClientRect()
    return { x: e.clientX - r.left, y: e.clientY - r.top }
  }
  const onMarker = (e: Event) => !!(e.target as HTMLElement | null)?.closest?.('.gw-mk')

  const drawBox = () => {
    if (!box) {
      boxEl.style.display = 'none'
      return
    }
    const x = Math.min(box.x0, box.x1)
    const y = Math.min(box.y0, box.y1)
    boxEl.style.display = 'block'
    boxEl.style.left = `${x}px`
    boxEl.style.top = `${y}px`
    boxEl.style.width = `${Math.abs(box.x1 - box.x0)}px`
    boxEl.style.height = `${Math.abs(box.y1 - box.y0)}px`
  }

  const onDown = (e: MouseEvent) => {
    if (e.button !== 0) return
    const p = local(e)
    down = p
    dragged = false
    if (e.shiftKey && !onMarker(e)) {
      // Rubber band: keep the map still and stop maplibre seeing the press.
      e.stopPropagation()
      e.preventDefault()
      map.dragPan.disable()
      box = { x0: p.x, y0: p.y, x1: p.x, y1: p.y, add: e.ctrlKey || e.metaKey }
      drawBox()
      window.addEventListener('mousemove', onBoxMove)
      window.addEventListener('mouseup', onBoxUp)
    }
  }
  const onBoxMove = (e: MouseEvent) => {
    if (!box) return
    const p = local(e)
    box.x1 = p.x
    box.y1 = p.y
    drawBox()
  }
  const onBoxUp = () => {
    window.removeEventListener('mousemove', onBoxMove)
    window.removeEventListener('mouseup', onBoxUp)
    map.dragPan.enable()
    if (!box) return
    const b = box
    box = null
    drawBox()
    dragged = true
    const pic = hooks.pic()
    const x0 = Math.min(b.x0, b.x1)
    const x1 = Math.max(b.x0, b.x1)
    const y0 = Math.min(b.y0, b.y1)
    const y1 = Math.max(b.y0, b.y1)
    if (x1 - x0 < 4 && y1 - y0 < 4) {
      // A shift-click on bare map: nothing to box.
      dragged = false
      return
    }
    const ids = (pic?.formations ?? [])
      .filter((f) => {
        const q = map.project([f.pos[1], f.pos[0]])
        return q.x >= x0 && q.x <= x1 && q.y >= y0 && q.y <= y1
      })
      .map((f) => f.id)
    hooks.box(ids, b.add)
  }
  const onMove = (e: MouseEvent) => {
    const p = local(e)
    if (down && Math.hypot(p.x - down.x, p.y - down.y) > DRAG_PX) dragged = true
    last = p
    if (box) return
    hooks.hover(p.x, p.y, snapObjective(map, hooks.pic(), p.x, p.y))
  }
  const onMapMove = () => {
    if (last && !box) hooks.hover(last.x, last.y, snapObjective(map, hooks.pic(), last.x, last.y))
  }
  const onUp = (e: MouseEvent) => {
    if (e.button !== 0 || box) return
    const wasDown = down
    down = null
    if (!wasDown || dragged || onMarker(e)) return
    const p = local(e)
    hooks.click(snapObjective(map, hooks.pic(), p.x, p.y), latLonAt(p.x, p.y))
  }
  const onContext = (e: MouseEvent) => {
    e.preventDefault()
    const p = local(e)
    hooks.context(snapObjective(map, hooks.pic(), p.x, p.y), latLonAt(p.x, p.y))
  }
  const latLonAt = (x: number, y: number): [number, number] => {
    const ll = map.unproject([x, y])
    return [ll.lat, ll.lng]
  }
  const onLeave = () => {
    last = null
    hooks.leave()
  }

  el.addEventListener('mousedown', onDown, true)
  el.addEventListener('mousemove', onMove)
  el.addEventListener('mouseup', onUp)
  el.addEventListener('contextmenu', onContext)
  el.addEventListener('mouseleave', onLeave)
  map.on('moveend', onMapMove)
  return {
    detach: () => {
      el.removeEventListener('mousedown', onDown, true)
      el.removeEventListener('mousemove', onMove)
      el.removeEventListener('mouseup', onUp)
      el.removeEventListener('contextmenu', onContext)
      el.removeEventListener('mouseleave', onLeave)
      map.off('moveend', onMapMove)
      window.removeEventListener('mousemove', onBoxMove)
      window.removeEventListener('mouseup', onBoxUp)
    },
    wasDrag: () => dragged,
  }
}
