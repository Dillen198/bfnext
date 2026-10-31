// What we know about one base, for the TacMap: the drawer a click on a base
// opens. It only shows; nothing here gives an order.
//
// Three sources, each already scoped to the viewer's side by the server:
// the ground picture (garrison and assault state, ours only), the public
// objective list (condition, flags, when it last changed hands) and the
// engine's warehouse read (stock, airframes and the supply line for our
// bases; for the enemy's only what recon has seen, and nothing at all
// without it). Where a value isn't known it says so instead of showing 0.
import { useMemo, type ReactElement } from 'react'
import { Objective as ObjIcon, X } from '@icons'
import type {
  Asset, CaptureEvent, GroundEnemyContact, GroundObjective, GroundPicture, Objective, SupplyLink, Warehouse, WarehouseItem,
} from '../../api'
import { ago } from '../../lib/format'
import CaptureBlockers from '../../components/CaptureBlockers'
import { fmtAge, km } from './geo'
import { Gauge } from './hud'
import { OBJ_ICON, natoSymbol, shortName } from './sprites'
import { SIDE_COLOR, other, type Side } from './theme'

/** The warehouse read for the selected base, and how it is going. */
export interface BaseIntel {
  w: Warehouse | null
  loading: boolean
  error: string | null
}

/** How close an enemy contact has to be to count as at the base. */
const NEAR_KM = 6

const fill = (i: WarehouseItem) => (i.capacity > 0 ? Math.min(1, i.stored / i.capacity) : 1)

function Level({ label, pct, why }: { label: string; pct: number | null | undefined; why: string }) {
  if (pct == null) {
    return (
      <div className="gw-gauge tm-unknown" title={why}>
        <span>{label}</span>
        <div />
        <b>—</b>
      </div>
    )
  }
  return <Gauge label={label} pct={pct} />
}

/** Fuel is kept in litres (a full tank reads 100000): shorten the big ones,
 *  per line, so a fraction never mixes units. Counts stay exact. */
function amount(n: number, big: boolean): string {
  if (!big) return String(n)
  return n < 1_000_000 ? `${Math.round(n / 100) / 10}k` : `${Math.round(n / 100_000) / 10}M`
}

function Stock({ name, item }: { name: string; item: WarehouseItem }) {
  const p = fill(item) * 100
  const col = p < 20 ? 'var(--gw-bad)' : p < 50 ? 'var(--gw-warn)' : 'var(--gw-good)'
  const big = Math.max(item.stored, item.capacity) >= 10_000
  return (
    <div className="tm-stock" title={`${name}: ${item.stored.toLocaleString()} of ${item.capacity ? item.capacity.toLocaleString() : 'no limit'}`}>
      <span>{name}</span>
      <div><i style={{ width: `${item.capacity > 0 ? p : 100}%`, background: item.capacity > 0 ? col : 'var(--gw-dim)' }} /></div>
      <b>{amount(item.stored, big)}{item.capacity > 0 && <small>/{amount(item.capacity, big)}</small>}</b>
    </div>
  )
}

type Family = [string, [string, WarehouseItem][]]

/** The warehouse split into the families DCS names imply, each sorted by how
 *  depleted a line is (what is about to run out comes first; lines with no
 *  capacity have no fill and sink). */
function families(w: Warehouse): Family[] {
  const by: Record<string, [string, WarehouseItem][]> = { AIRFRAMES: [], VEHICLES: [], WEAPONS: [], FORTIFICATIONS: [], LIQUIDS: [] }
  for (const [k, v] of Object.entries(w.equipment)) {
    if (k.startsWith('weapons.')) by.WEAPONS.push([k.slice(8), v])
    else if (k.startsWith('vehicles.')) by.VEHICLES.push([k.slice(9), v])
    else if (k.startsWith('Fortifications.')) by.FORTIFICATIONS.push([k.slice(15), v])
    else by.AIRFRAMES.push([k, v])
  }
  for (const [k, v] of Object.entries(w.liquids)) by.LIQUIDS.push([k, v])
  for (const l of Object.values(by)) l.sort((a, b) => fill(a[1]) - fill(b[1]) || a[0].localeCompare(b[0]))
  return Object.entries(by).filter(([, l]) => l.length > 0)
}

const lowIn = (l: [string, WarehouseItem][]) => l.filter(([, i]) => i.capacity > 0 && fill(i) <= 0.2).length

function WarehouseStock({ w }: { w: Warehouse }) {
  const fams = useMemo(() => families(w), [w])
  const low = fams.reduce((n, [, l]) => n + lowIn(l), 0)
  return (
    <>
      <h3>WAREHOUSE{low > 0 && <span className="tm-h3-note bad">{low} LINE{low === 1 ? '' : 'S'} UNDER 20%</span>}</h3>
      {fams.length === 0 && <p className="gw-drawer-note">Nothing in store.</p>}
      {fams.map(([name, items]) => {
        const n = lowIn(items)
        return (
          <details key={name} className="tm-fam" open={name === 'AIRFRAMES' || name === 'LIQUIDS'}>
            <summary>
              <span>{name} <small>{items.length}</small></span>
              {n > 0 && <span className="bad">{n} LOW</span>}
            </summary>
            {items.map(([k, v]) => <Stock key={k} name={k} item={v} />)}
          </details>
        )
      })}
    </>
  )
}

function LinkRow({ l, relation, onPick }: { l: SupplyLink; relation: string; onPick: (name: string) => void }) {
  return (
    <button className="tm-row" onClick={() => onPick(l.name)} title="Show this base">
      <i style={{ background: SIDE_COLOR[l.owner] }} />
      <b>{l.name.toUpperCase()}</b>
      {l.interdicted && <span className="bad">CUT</span>}
      <span>{relation}</span>
    </button>
  )
}

export function BaseDrawer({ o, pub, side, pic, assets, captures, intel, onClose, onPickBase, onPickFormation }: {
  o: GroundObjective
  /** Its line in the public objective list, when that has loaded. */
  pub: Objective | null
  side: Side
  pic: GroundPicture
  /** Our assets (from the command picture), to list the ones based here. */
  assets: Asset[]
  captures: CaptureEvent[]
  intel: BaseIntel
  onClose: () => void
  onPickBase: (name: string) => void
  onPickFormation: (id: number) => void
}): ReactElement {
  const Icon = OBJ_ICON[o.kind] ?? ObjIcon
  const ours = o.owner === side
  const neutral = o.owner === 'Neutral'
  const w = intel.w && intel.w.objective_name === o.name ? intel.w : null
  const full = w?.visibility === 'full'

  // Condition: the engine's scoped read first. The public list carries health
  // and logistics for every base (they are on the F10 map anyway); stock
  // levels only count for our own bases unless recon has earned them.
  const hp = w?.health ?? pub?.health ?? o.health
  const logi = w?.logi ?? pub?.logi ?? null
  const sup = w ? w.supply : ours ? pub?.supply ?? o.supply : null
  const fuel = w ? w.fuel : ours ? pub?.fuel ?? null : null
  const hidden = ours ? 'Not reported yet' : 'Needs recon: fly a reconnaissance sortie over it'

  const status = o.being_captured
    ? ours ? ['UNDER ASSAULT', 'bad'] : ['OUR TROOPS ARE ASSAULTING IT', 'hot']
    : pub?.captureable
      ? ['CAPTURABLE: DEFENCES DOWN', ours ? 'bad' : 'hot']
      : (o.threatened ?? pub?.threatened)
        ? ['THREATENED', 'warn']
        : neutral ? ['UNCLAIMED', ''] : ours ? ['SECURE', 'ok'] : ['ENEMY HELD', '']

  const based = pic.formations.filter((f) => f.home === o.id)
  const defending = pic.formations.filter((f) => f.order === 'defend' && f.target === o.id && f.home !== o.id)
  const attacking = pic.formations.filter((f) => (f.order === 'attack' && f.target === o.id) || (f.order === 'withdraw' && f.target === o.id))
  const enemyNear = pic.enemy.filter((e) => km(e.pos, o.pos) <= NEAR_KM)
  const battles = pic.battles.filter((b) => b.objective === o.id || km(b.pos, o.pos) <= NEAR_KM)
  const ourAssets = assets.filter((a) => a.base === o.name)
  const history = captures.filter((c) => c.objective === o.name).slice(0, 4)
  const forces = based.length + defending.length + attacking.length + enemyNear.length + battles.length + ourAssets.length

  const fRow = (f: GroundObjective['id'], name: string, alive: number, total: number, what: string) => (
    <button key={`${what}${f}`} className="tm-row" onClick={() => onPickFormation(f)} title="Select this formation">
      <i style={{ background: SIDE_COLOR[side] }} />
      <b>{shortName(name)}</b>
      <span>{alive}/{total}</span>
      <span>{what}</span>
    </button>
  )

  return (
    <aside className="gw-drawer tm-base">
      <button className="gw-drawer-x" onClick={onClose} title="Deselect (Esc)"><X size={14} /></button>
      <div className="tm-base-head" style={{ ['--oc' as string]: SIDE_COLOR[o.owner] }}>
        <Icon size={26} strokeWidth={1.6} />
        <div>
          <h2>{o.name}</h2>
          <div className="gw-drawer-sub">
            {o.kind.toUpperCase()} · <span style={{ color: SIDE_COLOR[o.owner] }}>{ours ? 'OURS' : neutral ? 'NEUTRAL' : 'ENEMY'}</span>
            {pub?.priority ? ' · PRIORITY' : ''}
          </div>
        </div>
      </div>
      <div className={`gw-drawer-order tm-status ${status[1]}`}>{status[0]}</div>

      {neutral ? (
        <p className="gw-drawer-note">Nobody holds it. Troops landed here take it for whoever brought them.</p>
      ) : (
        <>
          <h3>CONDITION</h3>
          <Level label="HP" pct={hp} why={hidden} />
          <Level label="LOG" pct={logi} why={hidden} />
          <Level label="SUP" pct={sup} why={hidden} />
          <Level label="FUEL" pct={fuel} why={hidden} />
          {(pub?.unlimited_supply || pub?.unlimited_aircraft) && (
            <div className="gw-chips tm-flags">
              {pub.unlimited_supply && <span className="gw-chip">UNLIMITED SUPPLY</span>}
              {pub.unlimited_aircraft && <span className="gw-chip">UNLIMITED AIRCRAFT</span>}
            </div>
          )}
          {w?.damaged && <p className="gw-drawer-note bad">Warehouse knocked out: it can't store or issue anything.</p>}
          {intel.loading && !w && <p className="gw-drawer-note">Asking the engine for its stock…</p>}
          {intel.error && !w && (
            <p className="gw-drawer-note" title={intel.error}>
              {/busy|did not answer|not answering|not reachable|breaker/i.test(intel.error)
                ? "The game server didn't answer for this base's stock: it may be busy. Asking again every minute."
                : `Stock and supply line unavailable: ${intel.error}`}
            </p>
          )}
          {w && w.visibility === 'intel' && !ours && (
            <p className="gw-drawer-note">From recon, last seen {w.intel_as_of ? ago(w.intel_as_of) : 'at an unknown time'}. Stock and supply routing need a closer look.</p>
          )}
          {w && w.visibility === 'hidden' && (
            <p className="gw-drawer-note">No recon on file. A reconnaissance sortie reveals its state.</p>
          )}
        </>
      )}

      {/* Neutral bases too: "why won't it take?" is asked of those as well. */}
      {w?.capture && <CaptureBlockers report={w.capture} objectiveName={w.objective_name} />}

      {ours && (
        <>
          <h3>GROUND</h3>
          <dl className="gw-dl tm-dl">
            <dt>GARRISON</dt>
            <dd className={(o.garrison ?? 0) === 0 ? 'bad' : ''}>{o.garrison != null ? `${o.garrison} UNIT${o.garrison === 1 ? '' : 'S'}` : '—'}</dd>
            <dt>SPARE TROOPS</dt>
            <dd>{o.can_raise == null ? '—' : o.can_raise > 0 ? `${o.can_raise} COMPAN${o.can_raise === 1 ? 'Y' : 'IES'}` : 'NONE'}</dd>
          </dl>
        </>
      )}

      {full && w && <WarehouseStock w={w} />}

      {full && w && (
        <>
          <h3>SUPPLY LINE</h3>
          {!w.supplier && w.destinations.length === 0 && <p className="gw-drawer-note">Not on a supply line: a hub, or nothing routes to it.</p>}
          {w.supplier && <LinkRow l={w.supplier} relation="FEEDS IT" onPick={onPickBase} />}
          {w.destinations.map((d) => <LinkRow key={d.id} l={d} relation="FED FROM HERE" onPick={onPickBase} />)}
        </>
      )}

      {forces > 0 && (
        <>
          <h3>FORCES</h3>
          {battles.map((b) => (
            <div key={`b${b.id}`} className="tm-row static">
              <i style={{ background: 'var(--gw-fire)' }} />
              <b>{b.kind === 'assault' ? 'ASSAULT' : 'BATTLE'}{b.live ? ' · LIVE' : ''}</b>
              <span>{b.our_losses} LOST / {b.enemy_losses} KILLED</span>
            </div>
          ))}
          {based.map((f) => fRow(f.id, f.name, f.alive, f.total, 'BASED HERE'))}
          {defending.map((f) => fRow(f.id, f.name, f.alive, f.total, 'DEFENDING'))}
          {attacking.map((f) => fRow(f.id, f.name, f.alive, f.total, f.order === 'withdraw' ? 'FALLING BACK' : 'ATTACKING'))}
          {ourAssets.map((a) => (
            <div key={`a${a.id}`} className="tm-row static">
              <i style={{ background: SIDE_COLOR[side] }} />
              <b>{a.role.toUpperCase()}</b>
              <span>{a.alive}/{a.total}</span>
              <span>{a.kind.toUpperCase()}</span>
            </div>
          ))}
          {enemyNear.map((e) => (
            <div key={`e${e.id}`} className="tm-row static">
              <i style={{ background: SIDE_COLOR[o.owner === 'Red' ? 'Blue' : 'Red'] }} />
              <b>HOSTILE {e.kind.toUpperCase()}</b>
              <span>~{e.approx_vehicles} VEH</span>
              <span>{e.last_seen_secs > 0 ? `${fmtAge(e.last_seen_secs)} AGO` : 'IN SIGHT'}</span>
            </div>
          ))}
        </>
      )}

      {(pub?.last_change || history.length > 0) && (
        <>
          <h3>HISTORY</h3>
          {history.map((c) => (
            <div key={c.time + c.side} className="tm-row static">
              <i style={{ background: SIDE_COLOR[c.side] }} />
              <b>{c.side === side ? 'WE TOOK IT' : 'THEY TOOK IT'}</b>
              <span>{ago(c.time).toUpperCase()}</span>
              {c.by.length > 0 && <span className="tm-by" title={c.by.join(', ')}>{c.by.slice(0, 2).join(', ')}{c.by.length > 2 ? ` +${c.by.length - 2}` : ''}</span>}
            </div>
          ))}
          {history.length === 0 && pub?.last_change && (
            <p className="gw-drawer-note">Last changed hands {ago(pub.last_change)}.</p>
          )}
        </>
      )}
    </aside>
  )
}

/** An enemy ground contact: what our forces have seen of it. */
export function ContactDrawer({ e, side, onClose }: { e: GroundEnemyContact; side: Side; onClose: () => void }): ReactElement {
  const ghost = e.last_seen_secs > 0
  const s = natoSymbol({ kind: e.kind, hostile: true, ghost, size: 30, fill: SIDE_COLOR[other(side)] })
  return (
    <aside className="gw-drawer">
      <button className="gw-drawer-x" onClick={onClose} title="Deselect (Esc)"><X size={14} /></button>
      <div className="gw-drawer-head"><img src={s.url} width={s.w} height={s.h} alt="" /></div>
      <h2>Hostile {e.kind}</h2>
      <div className="gw-drawer-sub">ENEMY CONTACT</div>
      <div className={`gw-drawer-order tm-status ${ghost ? 'warn' : 'bad'}`}>{ghost ? `LAST SEEN ${fmtAge(e.last_seen_secs)} AGO` : 'IN SIGHT'}</div>
      <dl className="gw-dl">
        <dt>SIZE</dt><dd>~{e.approx_vehicles} VEHICLES</dd>
        <dt>MOVING</dt><dd>{e.moving ? `YES · HDG ${String(Math.round(e.heading)).padStart(3, '0')}` : 'NO'}</dd>
      </dl>
      {e.units.length > 0 && (
        <>
          <h3>SEEN UP CLOSE</h3>
          <div className="gw-chips">
            {Object.entries(e.units.reduce<Record<string, number>>((m, u) => ({ ...m, [u.typ]: (m[u.typ] ?? 0) + 1 }), {}))
              .map(([t, n]) => <span key={t} className="gw-chip tm-chip">{n} × {t}</span>)}
          </div>
        </>
      )}
      <p className="gw-drawer-note">{ghost ? 'Where it was last seen: it may have moved since.' : 'In sight of our forces now.'}</p>
    </aside>
  )
}
