// Buying a formation from the treasury: N copies of one deployable, built at
// our nearest unthreatened base and sent along one route as a formation
// (`CommandOrder::buy_formation`). The catalogue lists what can be bought as
// "formation:<deployable>" entries priced per copy; this panel picks one and
// how many, then the map takes the route the way a formation Move does --
// click waypoints, right-click or Enter on the last.
import { useMemo, useState, type ReactElement } from 'react'
import type { FormationPurchase, GroundObjective, LatLon, OrderOption } from '../../api'
import { km } from './geo'
import type { Side } from './theme'

export const FORMATION_PREFIX = 'formation:'

// eslint-disable-next-line react-refresh/only-export-components
export const isFormationKey = (key: string): boolean => key.startsWith(FORMATION_PREFIX)

/** The base the engine will build it at: ours, not under threat, nearest the
 *  first point of the route and within reach (as `deploy_base` picks it). */
// eslint-disable-next-line react-refresh/only-export-components
export function formationBase(objectives: GroundObjective[], side: Side, first: LatLon | undefined, rangeM: number): GroundObjective | null {
  if (!first) return null
  let best: GroundObjective | null = null
  let bestKm = rangeM / 1000
  for (const o of objectives) {
    if (o.owner !== side || o.threatened || o.kind === 'carrier' || o.kind === 'naval' || o.kind === 'sam') continue
    const d = km(o.pos, first)
    if (d <= bestKm) {
      best = o
      bestKm = d
    }
  }
  return best
}

export function FormationShop({ opts, limits, treasury, canCommand, busy, armed, onArm }: {
  opts: OrderOption[]
  limits: FormationPurchase
  treasury: number
  canCommand: boolean
  busy: boolean
  /** The armed order's key, if any. */
  armed: string | null
  onArm: (o: OrderOption, count: number) => void
}): ReactElement {
  const [key, setKey] = useState(opts[0]?.key ?? '')
  const [count, setCount] = useState(() => Math.min(limits.max_count, Math.max(limits.min_count, 6)))
  const opt = useMemo(() => opts.find((o) => o.key === key) ?? opts[0], [opts, key])
  const full = limits.in_field >= limits.max_in_field
  if (!opt) return <p className="gw-drawer-note">Nothing this side can buy as a formation.</p>
  const n = Math.min(limits.max_count, Math.max(limits.min_count, count))
  const total = opt.cost * n
  const why = full
    ? `${limits.in_field} of ${limits.max_in_field} formations already in the field`
    : total > treasury ? `the treasury has ${treasury.toLocaleString()} of ${total.toLocaleString()}` : ''
  const off = !canCommand || busy || !!why
  return (
    <div className="cm-buy">
      <div className="cm-buy-row">
        <select value={opt.key} onChange={(e) => setKey(e.target.value)} disabled={!canCommand} aria-label="What to buy">
          {opts.map((o) => <option key={o.key} value={o.key}>{o.label} · {o.cost.toLocaleString()} each</option>)}
        </select>
        <div className="cm-buy-n" role="group" aria-label="How many">
          <button disabled={!canCommand || n <= limits.min_count} onClick={() => setCount(n - 1)} aria-label="One fewer">−</button>
          <b>{n}</b>
          <button disabled={!canCommand || n >= limits.max_count} onClick={() => setCount(n + 1)} aria-label="One more">+</button>
        </div>
      </div>
      <div className={`cm-op${why ? ' off' : ''}${armed === opt.key ? ' armed' : ''}`}>
        <button className="cm-op-where" disabled={off} onClick={() => onArm(opt, n)}
          title={why || `${n} × ${opt.label}. Click the route on the map; right-click or Enter on the last point.`}>
          <b>ROUTE {n} × {opt.label.toUpperCase()}</b>
          <span>{why || `from our nearest base within ${Math.round(limits.range_m / 1000)} km of the first point · ${limits.in_field}/${limits.max_in_field} in the field`}</span>
        </button>
        <button className="cm-op-go" disabled={off} onClick={() => onArm(opt, n)} title={why || `${total.toLocaleString()} from the treasury`}>
          {total.toLocaleString()}
        </button>
      </div>
    </div>
  )
}
