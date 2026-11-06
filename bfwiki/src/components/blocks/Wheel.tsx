import { useEffect, useId, useReducer, useRef } from 'react'
import { WHEEL_THEMES, type WheelSpec } from './parse'
import {
  CENTRE as C, G, arcEntries, arcSpan, clip, hit, hubText, initialWheel, polar, ringEntries, ringSpan,
  sectorPath, short, slotMid, wheelKey, wheelReducer, type Entry, type WheelState, type Zone,
} from './wheelModel'

/**
 * The ```wheel block: the Vector Strike radial wheel, drawn from the
 * plugin's own geometry and colours (bfcockpit.lua `render_wheel`,
 * Theme/wheel/layout.lua) and its icon set (copied to public/cockpit).
 *
 * Point at a slot to highlight it (a group shows its children as an arc
 * outside the ring); click to fire it or, for a group whose arc is showing,
 * to make its children the ring. The hub goes back. With the keyboard: Tab
 * to the wheel, then the plugin's arrow-key set.
 */

const ICON_URL = `${import.meta.env.BASE_URL}cockpit/wheel-icons/`
const EMBLEM = `${import.meta.env.BASE_URL}cockpit/emblem-64.png`
const INK = [0x0d, 0x10, 0x09] as const
const TEXT = [0xcd, 0xd4, 0xbe] as const
const DIM = [0x8a, 0x96, 0x78] as const
const BAD = [0xe0, 0x6c, 0x6c] as const
/** settings.wheel_opacity's default. */
const OP = 0.9
const HEIGHT = G.canvas + 46

type RGB = readonly [number, number, number]
const rgba = (c: RGB, a = 255) => `rgba(${c[0]},${c[1]},${c[2]},${(Math.max(0, Math.min(255, a)) / 255).toFixed(3)})`

function sameZone(s: WheelState, z: Zone): boolean {
  switch (z.zone) {
    case 'hub': return s.onHub && s.hlMain === undefined
    case 'main': return s.hlMain === z.i && s.hlArc === undefined && !s.onHub
    case 'arc': return s.hlArc === z.j
    case 'none': return s.hlMain === undefined && s.hlArc === undefined && !s.onHub
  }
}

/** A slot's element, by its data-seg id, for moving focus to it. */
function seg(svg: SVGSVGElement | null, id: string): SVGGElement | null {
  return svg?.querySelector<SVGGElement>(`[data-seg="${id}"]`) ?? null
}

function Icon({ name, x, y, size, color, uid }: { name: string; x: number; y: number; size: number; color: string; uid: string }) {
  return <rect x={x} y={y} width={size} height={size} fill={color} mask={`url(#${uid}-i-${name})`} />
}

export default function Wheel({ spec }: { spec: WheelSpec }) {
  const uid = useId().replace(/[^a-zA-Z0-9_-]/g, '')
  const [s, dispatch] = useReducer(wheelReducer, undefined, () => initialWheel(spec.items, spec.title ?? 'VECTOR STRIKE'))
  const acc = WHEEL_THEMES[spec.theme]
  const svgRef = useRef<SVGSVGElement>(null)
  const openRef = useRef<HTMLButtonElement>(null)
  const wasOpen = useRef(true)

  const ring = ringEntries(s)
  const arc = arcEntries(s)
  const n = ring.length
  const hub = hubText(s, spec.subtitle ?? '')

  // A destructive entry waits three seconds for its second click.
  useEffect(() => {
    if (!s.armed) return
    const t = setTimeout(() => dispatch({ type: 'disarm' }), 3000)
    return () => clearTimeout(t)
  }, [s.armed])

  // Keys drive the highlight: keep focus on what is highlighted.
  useEffect(() => {
    if (s.source !== 'keys') return
    const id = s.hlArc !== undefined ? `a${s.hlArc}` : s.hlMain !== undefined ? `m${s.hlMain}` : 'hub'
    const el = seg(svgRef.current, id)
    if (el && document.activeElement !== el && svgRef.current?.contains(document.activeElement)) el.focus()
  }, [s.source, s.hlArc, s.hlMain, s.stack.length])

  // Closing from the keyboard leaves focus on the button that reopens it,
  // and reopening puts it back on the wheel.
  useEffect(() => {
    if (wasOpen.current && !s.open) openRef.current?.focus()
    if (!wasOpen.current && s.open) seg(svgRef.current, 'm0')?.focus()
    wasOpen.current = s.open
  }, [s.open])

  function toCanvas(e: React.PointerEvent): [number, number] | null {
    const r = svgRef.current?.getBoundingClientRect()
    if (!r || r.width === 0) return null
    const k = G.canvas / r.width
    return [(e.clientX - r.left) * k - C, (e.clientY - r.top) * k - C]
  }

  function onPointerMove(e: React.PointerEvent) {
    // Touch has no hover: a tap is a click, and a group's first tap opens its arc.
    if (e.pointerType !== 'mouse') return
    const p = toCanvas(e)
    if (!p) return
    const z = hit(s, p[0], p[1])
    if (!sameZone(s, z) || s.source !== 'mouse') dispatch({ type: 'point', zone: z, source: 'mouse' })
  }

  function segProps(id: string, zone: Zone, label: string) {
    return {
      'data-seg': id,
      role: 'button' as const,
      tabIndex: 0,
      'aria-label': label,
      className: 'wh-seg',
      onClick: () => dispatch({ type: 'click', zone }),
      onFocus: (e: React.FocusEvent<SVGGElement>) => {
        if (e.currentTarget.matches(':focus-visible') && !sameZone(s, zone)) {
          dispatch({ type: 'point', zone, source: 'keys' })
        }
      },
      onKeyDown: (e: React.KeyboardEvent) => {
        const a = wheelKey(e.key)
        if (!a) return
        e.preventDefault()
        if (a === 'fire') dispatch({ type: 'click', zone })
        else dispatch(a)
      },
    }
  }

  function describe(e: Entry): string {
    return `${e.label}${e.group ? ', opens a menu' : ''}${e.danger ? ', asks you to confirm' : ''}`
  }

  const usedIcons = new Set<string>([...ring.map(e => e.icon), ...arc.map(e => e.icon)])
  const big = n > 8

  if (!s.open) {
    return (
      <div className="mk-block">
        <div className="wh-closed">
          <button ref={openRef} type="button" className="wh-reopen" onClick={() => dispatch({ type: 'open' })}>
            Wheel closed. Open it again
          </button>
        </div>
      </div>
    )
  }

  return (
    <div className="mk-block">
      <figure className="wh-stage" aria-label="Vector Strike wheel replica">
        <svg
          ref={svgRef}
          className="wh"
          viewBox={`0 0 ${G.canvas} ${HEIGHT}`}
          onPointerMove={onPointerMove}
          onPointerLeave={() => s.source === 'mouse' && dispatch({ type: 'leave' })}
        >
          <defs>
            <filter id={`${uid}-glow`} x="-40%" y="-40%" width="180%" height="180%">
              <feGaussianBlur stdDeviation="9" />
            </filter>
            {[...usedIcons].map(name => (
              <mask key={name} id={`${uid}-i-${name}`} maskContentUnits="objectBoundingBox">
                <image href={`${ICON_URL}${name}.png`} x="0" y="0" width="1" height="1" preserveAspectRatio="none" />
              </mask>
            ))}
          </defs>

          {/* glows under everything */}
          {n >= 2 && s.hlMain !== undefined && s.hlArc === undefined && (
            <path d={sectorPath(G.rIn, G.rOut, ...ringSpan(s.hlMain, n))} fill={rgba(acc, 150)} filter={`url(#${uid}-glow)`} />
          )}
          {s.arc && s.hlArc !== undefined && (
            <path d={sectorPath(G.subIn, G.subOut, ...arcSpan(s.arc.start, s.hlArc))} fill={rgba(acc, 140)} filter={`url(#${uid}-glow)`} />
          )}

          {/* the ring */}
          {ring.map((e, i) => {
            const hot = s.hlMain === i && s.hlArc === undefined
            const owner = s.arc?.owner === i
            const fill = hot ? rgba(acc, 240) : owner ? rgba(acc, 110 * OP) : rgba(INK, 214 * OP)
            let line = hot || owner ? rgba(acc) : rgba(acc, 80)
            if (e.danger && !hot) line = rgba(BAD, 170)
            const fg = hot ? INK : e.danger ? BAD : TEXT
            const rm = (G.rIn + G.rOut) / 2
            const mid = slotMid(i, n)
            const [ix, iy] = polar(rm - 6, mid)
            const isz = big ? 34 : 40
            const [gx, gy] = polar(G.rOut - 10, mid)
            // One slot is a full circle, which an arc path cannot draw.
            const d = n >= 2 ? sectorPath(G.rIn, G.rOut, ...ringSpan(i, n)) : sectorPath(G.rIn, G.rOut, -89.9, 269.9)
            return (
              <g key={`m${i}`} {...segProps(`m${i}`, { zone: 'main', i }, describe(e))}>
                <path d={d} fill={fill} />
                <path d={d} fill="none" stroke={line} strokeWidth={1.6} />
                <Icon name={e.icon} x={ix - isz / 2} y={iy - isz / 2 - 9} size={isz} color={rgba(fg)} uid={uid} />
                <text x={ix} y={iy + isz / 2 + 2} className={big ? 'wh-label wh-label--small' : 'wh-label'} fill={rgba(fg)}>
                  {short(e.label, big ? 10 : 13)}
                </text>
                {e.group && (
                  <text x={gx} y={gy} className="wh-mark" fill={hot ? rgba(INK) : rgba(acc)}>
                    •
                  </text>
                )}
              </g>
            )
          })}

          {/* the arc */}
          {s.arc &&
            arc.map((e, j) => {
              const hot = s.hlArc === j
              const [a0, a1] = arcSpan(s.arc!.start, j)
              const d = sectorPath(G.subIn, G.subOut, a0, a1)
              const am = (G.subIn + G.subOut) / 2
              const [ix, iy] = polar(am - 4, (a0 + a1) / 2)
              const fg = hot ? INK : e.danger ? BAD : TEXT
              return (
                <g key={`a${s.arc!.owner}-${j}`} {...segProps(`a${j}`, { zone: 'arc', j }, describe(e))}>
                  <path d={d} fill={hot ? rgba(acc, 245) : rgba(INK, 226 * OP)} />
                  <path d={d} fill="none" stroke={e.danger ? rgba(BAD, 180) : rgba(acc, hot ? 255 : 100)} strokeWidth={1.6} />
                  <Icon name={e.icon} x={ix - 15} y={iy - 22} size={30} color={rgba(fg)} uid={uid} />
                  <text x={ix} y={iy + 14} className="wh-label wh-label--arc" fill={rgba(fg)}>
                    {short(e.label, 11)}
                  </text>
                </g>
              )
            })}

          {/* the hub */}
          <g
            {...segProps('hub', { zone: 'hub' }, s.stack.length > 1 ? 'Back' : 'Close the wheel')}
            className="wh-seg wh-hub"
          >
            <circle cx={C} cy={C} r={G.hubR} fill={rgba(INK, 236)} />
            <circle cx={C} cy={C} r={G.hubR} fill="none" stroke={s.onHub ? rgba(acc) : rgba(acc, 190)} strokeWidth={2} />
            <image href={EMBLEM} x={C - 28} y={C - 74} width={56} height={56} />
            <text x={C} y={C} className="wh-hub-title" fill={hub.bad ? rgba(BAD) : rgba(acc)}>
              {hub.title}
            </text>
            <text x={C} y={C + 22} className="wh-hub-sub" fill={rgba(DIM)}>
              {hub.sub}
            </text>
            <text x={C} y={C + 42} className="wh-hub-hint" fill={rgba(DIM)}>
              {hub.hint}
            </text>
          </g>

          {/* the server's latest answer, under the wheel */}
          {s.answer && (
            <g>
              <rect
                x={C - G.strip.w / 2} y={G.canvas - 8} width={G.strip.w} height={G.strip.h} rx={12}
                fill={rgba(INK, 232)} stroke={rgba(acc, 160)} strokeWidth={1.4}
              />
              <text x={C - G.strip.w / 2 + 50} y={G.canvas - 8 + G.strip.h / 2} className="wh-strip-tag" fill={rgba(acc)}>
                SERVER
              </text>
              <text x={C - G.strip.w / 2 + 96} y={G.canvas - 8 + G.strip.h / 2} className="wh-strip-text" fill={rgba(TEXT)}>
                {clip(s.answer, 70)}
              </text>
            </g>
          )}
        </svg>
        <figcaption className="sr-only" aria-live="polite">
          {s.answer ? `The server says: ${s.answer}` : ''}
        </figcaption>
      </figure>
      <p className="mk-hint">
        Point at a slot; a group opens its arc. Click to run it, or to go into a group. The middle goes back. Keys: Tab
        to the wheel, then <kbd>←</kbd> <kbd>→</kbd> move, <kbd>↑</kbd> arc, <kbd>↓</kbd> ring, <kbd>Enter</kbd> fire,{' '}
        <kbd>Backspace</kbd> back.
      </p>
    </div>
  )
}
