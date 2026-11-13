/**
 * The four status hexes, drawn for the wiki.
 *
 * The page used to show them as an ASCII diagram in a code block, which the
 * monospace font renders as four identical grey dots -- no shape, no colour,
 * and therefore no explanation of the one thing the hexes exist to convey.
 *
 * Geometry and palette are copied from the two places that already draw them,
 * so all three agree: the engine's F10 overlay and `BriefingMap.tsx` on the
 * dashboard. If the engine palette moves, move it here too.
 */

/** > 66% fine, 33-66% thin, below that critical. Same buckets as the engine. */
export const BUCKET = (v: number) => (v > 66 ? '#33d94f' : v > 33 ? '#ffb300' : '#f22a2a')
/** Unlimited stock: gold outline on the hex. */
export const GOLD = '#ffd24a'
/** The same 50% fill the F10 map uses, so the two read alike. */
const FILL_OPACITY = 0.5
/** The engine's hex, in a 20x22 box. */
const POINTS = '10,0 20,5.5 20,16.5 10,22 0,16.5 0,5.5'

const ORDER = ['health', 'logi', 'supply', 'fuel'] as const
type Key = (typeof ORDER)[number]

const LABEL: Record<Key, string> = {
  health: 'Health',
  logi: 'Logi',
  supply: 'Supply',
  fuel: 'Fuel',
}

export interface HexSpec {
  health?: number
  logi?: number
  supply?: number
  fuel?: number
  /** Which resources are unlimited — drawn with the gold outline. */
  unlimited?: string[]
  /** Dim every hex except these, for explaining one at a time. */
  focus?: string[]
  caption?: string
}

function Hex({
  value,
  unlimited,
  dim,
  size = 34,
}: {
  value: number
  unlimited: boolean
  dim: boolean
  size?: number
}) {
  return (
    <svg
      width={size}
      height={(size * 22) / 20}
      viewBox="0 0 20 22"
      aria-hidden
      style={{ opacity: dim ? 0.22 : 1, transition: 'opacity .15s' }}
    >
      <polygon
        points={POINTS}
        fill={BUCKET(value)}
        fillOpacity={FILL_OPACITY}
        stroke={unlimited ? GOLD : 'rgba(255,255,255,0.45)'}
        strokeWidth={unlimited ? 3 : 1.5}
      />
    </svg>
  )
}

export default function StatusHexes(spec: HexSpec) {
  const unlimited = new Set((spec.unlimited ?? []).map(s => s.toLowerCase()))
  const focus = spec.focus?.length ? new Set(spec.focus.map(s => s.toLowerCase())) : null
  const shown = ORDER.filter(k => typeof spec[k] === 'number')
  if (shown.length === 0) return null

  return (
    <figure className="wiki-hexrow">
      <div className="wiki-hexrow-row">
        {shown.map(k => {
          const v = spec[k] as number
          const dim = focus ? !focus.has(k) : false
          return (
            <div key={k} className="wiki-hexrow-cell" style={{ opacity: dim ? 0.35 : 1 }}>
              <span className="wiki-hexrow-label">{LABEL[k]}</span>
              <Hex value={v} unlimited={unlimited.has(k)} dim={dim} />
              <span className="wiki-hexrow-value" style={{ color: dim ? undefined : BUCKET(v) }}>
                {v}%{unlimited.has(k) ? ' ∞' : ''}
              </span>
            </div>
          )
        })}
      </div>
      {spec.caption && <figcaption>{spec.caption}</figcaption>}
    </figure>
  )
}

/** The colour key: what the three buckets and the gold outline mean. */
export function HexLegend() {
  const rows: [number, string, string][] = [
    [90, 'Green', 'Above 66% — fine.'],
    [50, 'Amber', '33–66% — getting thin.'],
    [15, 'Red', 'Below 33% — critical.'],
  ]
  return (
    <figure className="wiki-hexrow">
      <div className="wiki-hexlegend">
        {rows.map(([v, name, meaning]) => (
          <div key={name} className="wiki-hexlegend-row">
            <Hex value={v} unlimited={false} dim={false} size={26} />
            <strong style={{ color: BUCKET(v) }}>{name}</strong>
            <span>{meaning}</span>
          </div>
        ))}
        <div className="wiki-hexlegend-row">
          <Hex value={90} unlimited dim={false} size={26} />
          <strong style={{ color: GOLD }}>Gold outline</strong>
          <span>That resource is unlimited — it never runs dry.</span>
        </div>
      </div>
    </figure>
  )
}
