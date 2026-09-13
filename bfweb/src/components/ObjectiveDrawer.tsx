import { useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api, type Warehouse, type WarehouseItem } from '../api'
import { campaign } from '../config/campaign'
import { ago } from '../lib/format'
import QueryState from './QueryState'
import { X, Alert, Supply, Logistics } from '@icons'

/**
 * Everything known about one objective, including the supply topology the
 * engine already maintains but nothing displayed.
 *
 * Fog of war is enforced in bflib, not here -- the response simply arrives
 * with `visibility` set, and enemy objectives carry no stock lines and no
 * edges at all. That ordering matters: gating in the UI would still put the
 * data on the wire, where anyone can read it.
 */

const sideColor = (owner: string) =>
  owner === 'Blue' ? campaign.blueColor : owner === 'Red' ? campaign.redColor : 'var(--text-dim)'

/** Split the flat equipment map into the four families DCS names imply, so
 *  the panel is not one 300-row wall of `weapons.bombs.Mk_82`. */
function groupItems(equipment: Record<string, WarehouseItem>) {
  const groups: Record<string, [string, WarehouseItem][]> = {
    Airframes: [], Vehicles: [], Weapons: [], Fortifications: [],
  }
  for (const [k, v] of Object.entries(equipment)) {
    if (k.startsWith('weapons.')) groups.Weapons.push([k.replace(/^weapons\./, ''), v])
    else if (k.startsWith('vehicles.')) groups.Vehicles.push([k.replace(/^vehicles\./, ''), v])
    else if (k.startsWith('Fortifications.')) groups.Fortifications.push([k.replace(/^Fortifications\./, ''), v])
    else groups.Airframes.push([k, v])
  }
  // Sorted by how DEPLETED a line is, not by how many are stored. Sorting by
  // count buries the thing you are about to run out of under a pile of
  // chaff cartridges -- the useful question is "what is nearly gone?".
  // Uncapped lines (capacity 0) have no meaningful fill, so they sink.
  for (const g of Object.values(groups)) {
    g.sort((a, b) => fillOf(a[1]) - fillOf(b[1]))
  }
  return groups
}

/** Fraction of capacity in stock, 0-1. Uncapped lines report as full. */
function fillOf(i: WarehouseItem): number {
  if (i.capacity <= 0) return 1
  return Math.min(1, i.stored / i.capacity)
}

/** How many lines in a group are at or below `frac` of capacity. */
function countBelow(items: [string, WarehouseItem][], frac: number): number {
  return items.filter(([, v]) => v.capacity > 0 && fillOf(v) <= frac).length
}

function StockBar({ item }: { item: WarehouseItem }) {
  const pct = item.capacity > 0 ? Math.min(100, (item.stored / item.capacity) * 100) : 0
  const col = pct < 20 ? 'var(--red)' : pct < 50 ? 'var(--yellow)' : 'var(--accent)'
  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 7, flex: 1, minWidth: 0 }}>
      <div style={{ flex: 1, height: 4, background: 'var(--bg-elevated)', minWidth: 30 }}>
        <div style={{ width: `${pct}%`, height: '100%', background: col }} />
      </div>
      <span className="font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--text-dim)', width: 66, textAlign: 'right', flexShrink: 0 }}>
        {item.stored}
        {item.capacity > 0 && <span style={{ opacity: 0.55 }}>/{item.capacity}</span>}
      </span>
      {item.capacity > 0 && (
        <span
          className="font-mono-vs"
          style={{ fontSize: '0.6rem', color: col, width: 34, textAlign: 'right', flexShrink: 0 }}
        >
          {Math.round(pct)}%
        </span>
      )}
    </div>
  )
}

/** The objective's own two-hop neighbourhood in the supply forest: the hub
 *  above it, and everything it feeds below. Two hops rather than the whole
 *  network on purpose -- it answers "what dies if I lose this?" without
 *  needing a graph layout engine or turning into a hairball at 120 nodes. */
function SupplyGraph({ w, onOpen }: { w: Warehouse; onOpen: (name: string) => void }) {
  const hasAny = w.supplier || w.destinations.length > 0
  if (!hasAny) {
    return (
      <div style={{ padding: '10px 14px', fontSize: '0.68rem', color: 'var(--text-dim)' }}>
        Not on a supply line — this objective is a hub, or nothing routes to it.
      </div>
    )
  }
  const node = (name: string, owner: string, interdicted: boolean, role: string) => (
    <button
      type="button"
      onClick={() => onOpen(name)}
      style={{
        display: 'flex', alignItems: 'center', gap: 7, width: '100%', textAlign: 'left',
        background: 'none', border: '1px solid var(--border)', padding: '5px 9px',
        font: 'inherit', color: 'var(--text)', cursor: 'pointer', fontSize: '0.68rem',
      }}
    >
      <span style={{ width: 6, height: 6, background: sideColor(owner), flexShrink: 0 }} />
      <span style={{ flex: 1, minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
        {name}
      </span>
      {interdicted && (
        <span className="font-mono-vs" style={{ fontSize: '0.56rem', color: 'var(--red)', flexShrink: 0 }}>
          CUT
        </span>
      )}
      <span className="font-mono-vs" style={{ fontSize: '0.55rem', color: 'var(--text-dim)', flexShrink: 0 }}>
        {role}
      </span>
    </button>
  )

  return (
    <div style={{ padding: '10px 14px', display: 'flex', flexDirection: 'column', gap: 5 }}>
      {w.supplier && (
        <>
          {node(w.supplier.name, w.supplier.owner, w.supplier.interdicted, 'SUPPLIES THIS')}
          <div style={{ textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.7rem', lineHeight: 1 }}>↓</div>
        </>
      )}
      <div style={{
        display: 'flex', alignItems: 'center', gap: 7, padding: '6px 9px',
        border: `1px solid ${sideColor(w.owner)}`, background: 'var(--bg-elevated)', fontSize: '0.7rem', fontWeight: 700,
      }}>
        <span style={{ width: 6, height: 6, background: sideColor(w.owner), flexShrink: 0 }} />
        {w.objective_name}
      </div>
      {w.destinations.length > 0 && (
        <>
          <div style={{ textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.7rem', lineHeight: 1 }}>↓</div>
          {w.destinations.map(d => node(d.name, d.owner, d.interdicted, 'FED BY THIS'))}
        </>
      )}
    </div>
  )
}

export default function ObjectiveDrawer({
  objective, onClose, onOpen,
}: {
  objective: string | null
  onClose: () => void
  onOpen: (name: string) => void
}) {
  const q = useQuery({
    queryKey: ['warehouse', objective],
    queryFn: () => api.warehouse(objective!),
    enabled: !!objective,
    refetchInterval: 60_000,
    retry: false,
  })
  const w = q.data
  const groups = useMemo(() => (w ? groupItems(w.equipment) : null), [w])
  // Stock lines at or under 20% of capacity, across every family and the
  // liquids -- the "is this base about to stop working?" number.
  const lowCount = useMemo(() => {
    if (!w) return 0
    const all = [...Object.values(w.equipment), ...Object.values(w.liquids)]
    return all.filter(i => i.capacity > 0 && i.stored / i.capacity <= 0.2).length
  }, [w])

  if (!objective) return null

  return (
    <>
      {/* Scrim: dims the page and gives "click away to close" somewhere to
          land. Without it the drawer read as part of the table behind it. */}
      <div
        onClick={onClose}
        style={{ position: 'fixed', inset: 0, background: 'rgba(0,0,0,0.45)', zIndex: 59 }}
      />
    <div
      role="dialog"
      aria-label={`${objective} detail`}
      style={{
        position: 'fixed', top: 0, right: 0, bottom: 0, width: 'min(460px, 100vw)',
        // --bg-card is only 72% opaque -- it is meant for a card sitting ON
        // the page, and as an overlay the table showed straight through it.
        // Compositing it over the opaque page colour keeps the intended tint
        // in both themes without hardcoding one.
        backgroundColor: 'var(--bg)',
        backgroundImage: 'linear-gradient(var(--bg-card), var(--bg-card))',
        borderLeft: '1px solid var(--border)',
        display: 'flex', flexDirection: 'column', zIndex: 60, boxShadow: '-8px 0 24px rgba(0,0,0,0.55)',
      }}
    >
      <div style={{
        display: 'flex', alignItems: 'center', gap: 8, padding: '11px 14px',
        borderBottom: '1px solid var(--border)', flexShrink: 0,
      }}>
        <span style={{ width: 8, height: 8, background: sideColor(w?.owner ?? 'Neutral'), flexShrink: 0 }} />
        <span style={{ fontSize: '0.8rem', fontWeight: 700, flex: 1, minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
          {w?.objective_name ?? objective}
        </span>
        {w && (
          <span className="font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
            {w.kind}
          </span>
        )}
        <button type="button" onClick={onClose} aria-label="Close"
          style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', padding: 2, display: 'flex' }}>
          <X size={15} />
        </button>
      </div>

      <div style={{ overflowY: 'auto', flex: 1 }}>
        <QueryState
          what="this objective" isLoading={q.isLoading} isError={q.isError}
          error={q.error} onRetry={q.refetch}
        />

        {w && (
          <>
            {/* Condition, always visible -- it is on the F10 map anyway */}
            <div style={{ display: 'grid', gridTemplateColumns: 'repeat(4,1fr)', borderBottom: '1px solid var(--border)' }}>
              {([['HP', w.health], ['LOGI', w.logi], ['SUP', w.supply], ['FUEL', w.fuel]] as const).map(([l, v]) => (
                <div key={l} style={{ padding: '9px 10px', borderRight: '1px solid var(--border)' }}>
                  <div className="font-mono-vs" style={{ fontSize: '0.54rem', letterSpacing: '0.14em', color: 'var(--text-dim)' }}>{l}</div>
                  {/* Unknown is not zero: an em dash, not a misleading 0%. */}
                  <div
                    title={v == null ? 'Not known — recon has not reported this' : undefined}
                    style={{ fontSize: '0.95rem', fontWeight: 700, color: v == null ? 'var(--text-dim)' : v < 40 ? 'var(--red)' : 'var(--text)' }}
                  >
                    {v == null ? '—' : `${v}%`}
                  </div>
                </div>
              ))}
            </div>

            {w.damaged && (
              <div style={{ display: 'flex', gap: 7, alignItems: 'center', padding: '8px 14px', color: 'var(--red)', fontSize: '0.68rem', borderBottom: '1px solid var(--border)' }}>
                <Alert size={12} /> Warehouse knocked out — this base cannot store or issue materiel.
              </div>
            )}

            {/* What the viewer is allowed to know, stated rather than implied */}
            {w.visibility !== 'full' && (
              <div style={{ padding: '10px 14px', fontSize: '0.68rem', color: 'var(--text-dim)', lineHeight: 1.55, borderBottom: '1px solid var(--border)' }}>
                {w.visibility === 'intel' ? (
                  <>
                    Enemy objective — showing what recon has seen. Last observed{' '}
                    <strong style={{ color: 'var(--yellow)' }}>
                      {w.intel_as_of ? ago(w.intel_as_of) : 'unknown'}
                    </strong>
                    . Stock and supply routing need a closer look.
                  </>
                ) : (
                  <>Enemy objective — no recon on file. Fly a reconnaissance sortie to reveal its state.</>
                )}
              </div>
            )}

            {w.visibility === 'full' && (
              <>
                <div className="lb-card-head" style={{ display: 'flex', alignItems: 'center', gap: 7 }}>
                  <Logistics size={11} /> Supply line
                </div>
                <SupplyGraph w={w} onOpen={onOpen} />

                <div className="lb-card-head" style={{ display: 'flex', alignItems: 'center', gap: 7 }}>
                  <Supply size={11} /> Stock
                  {lowCount > 0 && (
                    <span style={{ marginLeft: 'auto', color: 'var(--red)', letterSpacing: 0, textTransform: 'none' }}>
                      {lowCount} line{lowCount === 1 ? '' : 's'} low
                    </span>
                  )}
                </div>
                {groups && Object.entries(groups).map(([name, items]) =>
                  items.length === 0 ? null : (
                    <div key={name}>
                      <div style={{
                        display: 'flex', alignItems: 'baseline', gap: 6,
                        padding: '9px 14px 3px', fontFamily: 'var(--font-mono)', fontSize: '0.56rem',
                        letterSpacing: '0.16em', textTransform: 'uppercase', color: 'var(--text-dim)',
                      }}>
                        <span>{name} <span style={{ opacity: 0.6 }}>({items.length})</span></span>
                        {/* Lead with the shortage -- it is the only number in
                            this panel anyone acts on. */}
                        {countBelow(items, 0.2) > 0 && (
                          <span style={{ marginLeft: 'auto', color: 'var(--red)' }}>
                            {countBelow(items, 0.2)} under 20%
                          </span>
                        )}
                      </div>
                      {items.slice(0, 25).map(([k, v]) => (
                        <div key={k} style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '2px 14px', fontSize: '0.66rem' }}>
                          <span style={{ width: 150, flexShrink: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{k}</span>
                          <StockBar item={v} />
                        </div>
                      ))}
                      {items.length > 25 && (
                        <div style={{ padding: '2px 14px 6px', fontSize: '0.6rem', color: 'var(--text-dim)' }}>
                          + {items.length - 25} more
                        </div>
                      )}
                    </div>
                  ),
                )}
                {Object.keys(w.liquids).length > 0 && (
                  <>
                    <div style={{
                      padding: '7px 14px 3px', fontFamily: 'var(--font-mono)', fontSize: '0.56rem',
                      letterSpacing: '0.16em', textTransform: 'uppercase', color: 'var(--text-dim)',
                    }}>
                      Liquids
                    </div>
                    {Object.entries(w.liquids).map(([k, v]) => (
                      <div key={k} style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '2px 14px 6px', fontSize: '0.66rem' }}>
                        <span style={{ width: 150, flexShrink: 0 }}>{k}</span>
                        <StockBar item={v} />
                      </div>
                    ))}
                  </>
                )}
              </>
            )}
          </>
        )}
      </div>
    </div>
    </>
  )
}
