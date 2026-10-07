// The command layer over the ground war: every one of our assets -- AI
// flights, convoys, deployed units, troops, batteries, carrier groups -- live
// on the map with its own units, the enemy aircraft our side can see, and the
// panels a commander orders them from. The ground war's own formations,
// bases and battles stay where they are (`markers`, `engine`).
//
// What is ours comes from /api/command (the engine reads it out of DCS); the
// enemy only from the fog-of-war feeds. Orders go to /api/command/order and
// the engine checks every one: the asset is ours, the treasury can pay, the
// battery is in range and reloaded.
import { memo, useMemo, type MouseEvent, type ReactElement } from 'react'
import { Layer, Marker, Source } from 'react-map-gl/maplibre'
import type { Feature, FeatureCollection } from 'geojson'
import { X } from '@icons'
import {
  type AirTrack, type Asset, type AssetVerb, type CommandOrder, type CommandPicture,
  type GroundObjective, type HqOpKind, type LatLon,
} from '../../api'
import type { AssetMode, Layers } from './commandFeed'
import { offset } from './geo'
import { playerSvg } from './sprites'
import { PENCIL, SIDE_BRIGHT, SIDE_COLOR, other, type Side } from './theme'

// ── Layers ───────────────────────────────────────────────────────────────

const LAYER_LABELS: [keyof Layers, string, string][] = [
  ['air', 'AIR', 'Our AI flights'],
  ['ground', 'GND', 'Deployed units, troops, batteries'],
  ['naval', 'SEA', 'Carrier groups'],
  ['logi', 'LOG', 'Supply convoys'],
  ['enemyAir', 'HOS', 'Enemy aircraft our side can see'],
]

/** Why our own assets may be missing or behind: two short lines and the long story. */
export interface AssetNote { top: string; bottom: string; title: string }

export function LayerToggles({ layers, set, note }: { layers: Layers; set: (l: Layers) => void; note: AssetNote | null }): ReactElement {
  return (
    <div className="cm-layers" role="group" aria-label="Map layers">
      {note && (
        <div className="cm-offline" title={note.title}>
          {note.top}<br />{note.bottom}
        </div>
      )}
      {LAYER_LABELS.map(([k, label, title]) => (
        <button key={k} className={layers[k] ? 'on' : ''} title={title} onClick={() => set({ ...layers, [k]: !layers[k] })}>
          {label}
        </button>
      ))}
    </div>
  )
}

// ── Asset glyphs ─────────────────────────────────────────────────────────

const HELO = /^(Mi-|Ka-|AH-|UH-|CH-|SA342|OH-58|Tiger|SH-60|MH-60|OH58)/i
const AIR_DEF = /hawk|patriot|nasams|roland|sa-|s-300|tor|osa|buk|kub|strela|stinger|avenger|chaparral|linebacker|igla|shilka|tunguska|gepard|zsu|vulcan|rapier|hq-7|aaa|sam/i

/** A small friendly frame with what's inside saying what it is. */
function groundGlyph(a: Asset, fill: string): string {
  const inner = (() => {
    switch (a.kind) {
      case 'troops':
        return '<path d="M3 3 L21 15 M21 3 L3 15" stroke="#000" stroke-width="1.6"/>'
      case 'artillery':
        return '<circle cx="12" cy="9" r="3.2" fill="#000"/>'
      case 'convoy':
        return '<line x1="3" y1="9" x2="21" y2="9" stroke="#000" stroke-width="1.6"/><circle cx="8" cy="15.5" r="1.6" fill="#000"/><circle cx="16" cy="15.5" r="1.6" fill="#000"/>'
      default:
        return AIR_DEF.test(`${a.role} ${a.typ}`)
          ? '<path d="M5 15 Q12 2 19 15" fill="none" stroke="#000" stroke-width="1.6"/>'
          : '<ellipse cx="12" cy="9" rx="6.5" ry="3.6" fill="none" stroke="#000" stroke-width="1.6"/>'
    }
  })()
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="-1 -1 26 20" width="24" height="18"><rect x="0" y="0" width="24" height="18" fill="${fill}" stroke="#000" stroke-width="1.4"/>${inner}</svg>`
}

function assetSvg(a: Asset, fill: string): string {
  if (a.kind === 'air') return playerSvg(HELO.test(a.typ) ? 'helicopter' : 'plane', fill)
  if (a.kind === 'naval') return playerSvg('ship', fill)
  return groundGlyph(a, fill)
}

const fl = (m: number) => `FL${String(Math.round((m * 3.281) / 100)).padStart(3, '0')}`

interface AProps {
  a: Asset
  side: Side
  selected: boolean
  near: boolean
  onPick: (e: MouseEvent, id: number) => void
}

function AssetMarkerImpl({ a, side, selected, near, onPick }: AProps): ReactElement {
  const fill = a.live ? SIDE_BRIGHT[side] : SIDE_COLOR[side]
  const rotate = a.kind === 'air' || a.kind === 'naval'
  const sub = a.kind === 'air'
    ? `${fl(a.alt_m)} · ${Math.round(a.speed_kts)} KT`
    : a.kind === 'convoy' || a.kind === 'naval'
      ? `${Math.round(a.speed_kts)} KT`
      : `${a.alive}/${a.total}`
  return (
    <>
      {near && a.kind !== 'air' && a.units.slice(1).map((u, i) => (
        <Marker key={i} longitude={u.pos[1]} latitude={u.pos[0]} anchor="center">
          <div className="cm-unit" style={{ background: fill, transform: `rotate(${u.heading}deg)` }} title={u.typ} />
        </Marker>
      ))}
      <Marker longitude={a.pos[1]} latitude={a.pos[0]} anchor="center">
        <div
          className={`gw-mk cm-asset cm-${a.kind}${selected ? ' sel' : ''}${a.live ? '' : ' sim'}`}
          onClick={(e) => onPick(e, a.id)}
          title={`${a.name} · ${a.role}${a.typ ? ` (${a.typ})` : ''}`}
        >
          <div className="cm-ico" style={rotate ? { transform: `rotate(${a.heading}deg)` } : undefined}
            dangerouslySetInnerHTML={{ __html: assetSvg(a, fill) }} />
          <div className="cm-tag">
            <b>{a.role.toUpperCase()}</b>
            <span>{sub}</span>
          </div>
        </div>
      </Marker>
    </>
  )
}
const aKey = (p: AProps) =>
  `${p.a.id}|${p.a.pos[0].toFixed(4)}|${p.a.pos[1].toFixed(4)}|${Math.round(p.a.heading / 5)}|${Math.round(p.a.alt_m / 30)}|${Math.round(p.a.speed_kts / 5)}|${p.a.alive}|${p.a.live}|${p.selected}|${p.near}|${p.side}|${p.near ? p.a.units.map((u) => u.pos[0].toFixed(4) + u.pos[1].toFixed(4)).join() : ''}`
export const AssetMarker = memo(AssetMarkerImpl, (x, y) => aKey(x) === aKey(y) && x.onPick === y.onPick)

/** An enemy aircraft our side holds a track on. */
export const EnemyAirMarker = memo(function EnemyAirMarker({ t, side }: { t: AirTrack; side: Side }): ReactElement {
  const col = t.iff === 'hostile' ? SIDE_BRIGHT[other(side)] : '#c9c4a8'
  return (
    <Marker longitude={t.lon} latitude={t.lat} anchor="center">
      <div className="gw-mk cm-hostile" title={`${t.iff} ${t.unit_type ?? t.class} · ${t.age_s}s old`}>
        <div className="cm-ico" style={{ transform: `rotate(${t.heading}deg)` }}
          dangerouslySetInnerHTML={{ __html: playerSvg(t.class === 'helo' ? 'helicopter' : 'plane', col) }} />
        <div className="cm-tag hostile">
          <b>{(t.unit_type ?? t.class).toUpperCase()}</b>
          <span>{fl(t.alt_m)} · {Math.round(t.speed_kts)} KT</span>
        </div>
      </div>
    </Marker>
  )
}, (a, b) => a.t.id === b.t.id && a.t.lat === b.t.lat && a.t.lon === b.t.lon && a.t.heading === b.t.heading && a.t.alt_m === b.t.alt_m && a.side === b.side)

function ring(c: LatLon, m: number, n = 72): [number, number][] {
  const pts: [number, number][] = []
  for (let i = 0; i <= n; i++) {
    const b = (i / n) * 360
    const p = offset(c, b, m)
    pts.push([p[1], p[0]])
  }
  return pts
}

/** Where our assets are headed, and the reach of the selected battery. */
export function AssetLines({ assets, sel, side }: { assets: Asset[]; sel: Asset | null; side: Side }): ReactElement {
  const routes: FeatureCollection = useMemo(() => ({
    type: 'FeatureCollection',
    features: assets.filter((a) => a.dest).map((a): Feature => ({
      type: 'Feature',
      properties: { sel: sel?.id === a.id ? 1 : 0 },
      geometry: { type: 'LineString', coordinates: [[a.pos[1], a.pos[0]], [a.dest![1], a.dest![0]]] },
    })),
  }), [assets, sel])
  const reach: FeatureCollection = useMemo(() => ({
    type: 'FeatureCollection',
    features: sel?.range_m ? [{ type: 'Feature', properties: {}, geometry: { type: 'LineString', coordinates: ring(sel.pos, sel.range_m) } }] : [],
  }), [sel])
  return (
    <>
      <Source id="cm-routes" type="geojson" data={routes}>
        <Layer id="cm-routes" type="line" paint={{
          'line-color': ['match', ['get', 'sel'], 1, PENCIL, SIDE_BRIGHT[side]],
          'line-width': ['match', ['get', 'sel'], 1, 2, 1.2],
          'line-opacity': 0.75,
          'line-dasharray': [2, 2],
        }} />
      </Source>
      <Source id="cm-reach" type="geojson" data={reach}>
        <Layer id="cm-reach" type="line" paint={{ 'line-color': '#ff8c1a', 'line-width': 1.4, 'line-opacity': 0.8, 'line-dasharray': [4, 3] }} />
      </Source>
    </>
  )
}

// ── Asset panel ──────────────────────────────────────────────────────────

const VERB: Record<AssetVerb, { label: string; key: string; mode: AssetMode | null; hint: string }> = {
  move: { label: 'MOVE', key: 'M', mode: 'move', hint: 'Click where it should go. Costs treasury by distance.' },
  fire: { label: 'FIRE', key: 'G', mode: 'fire', hint: 'Click the target inside the orange ring.' },
  station: { label: 'STATION', key: 'S', mode: 'station', hint: 'Click where it should work.' },
  sail: { label: 'SAIL', key: 'M', mode: 'sail', hint: 'Click open water.' },
  rtb: { label: 'RTB', key: 'B', mode: null, hint: '' },
}

export function AssetPanel({ a, canCommand, mode, setMode, onRtb, onClose }: {
  a: Asset
  canCommand: boolean
  mode: AssetMode
  setMode: (m: AssetMode) => void
  onRtb: () => void
  onClose: () => void
}): ReactElement {
  return (
    <aside className="gw-drawer cm-panel">
      <button className="gw-drawer-x" onClick={onClose} title="Deselect (Esc)"><X size={14} /></button>
      <div className="gw-drawer-sub">{a.kind.toUpperCase()}{a.base ? ` · ${a.base.toUpperCase()}` : ''}</div>
      <h2>{a.name}</h2>
      <div className="gw-drawer-order">{(a.task ?? (a.dest ? 'UNDER WAY' : 'NO ORDERS')).toUpperCase()}</div>
      <dl className="gw-dl">
        <dt>WHAT</dt><dd>{a.role.toUpperCase()}{a.typ ? ` · ${a.typ}` : ''}</dd>
        <dt>STRENGTH</dt><dd className={a.alive < a.total ? 'bad' : 'ok'}>{a.alive} / {a.total}</dd>
        {a.kind === 'air' && (<><dt>ALT / SPEED</dt><dd>{fl(a.alt_m)} · {Math.round(a.speed_kts)} KT</dd></>)}
        {(a.kind === 'convoy' || a.kind === 'naval' || a.kind === 'troops' || a.kind === 'deployed') && a.speed_kts > 0.5 && (
          <><dt>SPEED</dt><dd>{Math.round(a.speed_kts)} KT · HDG {String(Math.round(a.heading)).padStart(3, '0')}</dd></>
        )}
        {a.range_m != null && (<><dt>REACH</dt><dd>{(a.range_m / 1000).toFixed(1)} KM</dd></>)}
        <dt>STATUS</dt><dd>{a.live ? 'LIVE IN DCS' : 'NOT SPAWNED'}</dd>
      </dl>
      {a.orders.length > 0 ? (
        <div className="cm-orders">
          {a.orders.map((v) => {
            const d = VERB[v]
            const active = d.mode != null && mode === d.mode
            return (
              <button key={v} className={active ? 'on' : ''} disabled={!canCommand}
                title={canCommand ? `${d.label} (${d.key})` : 'Commanders give orders'}
                onClick={() => (d.mode ? setMode(active ? 'none' : d.mode) : onRtb())}>
                <kbd>{d.key}</kbd> {d.label}
              </button>
            )
          })}
        </div>
      ) : (
        <p className="gw-drawer-note">Runs itself: nothing to order.</p>
      )}
      {mode !== 'none' && mode !== 'barrage' && mode !== 'fmove' && <p className="gw-drawer-note">{a.orders.map((v) => VERB[v]).find((d) => d.mode === mode)?.hint}</p>}
    </aside>
  )
}

// ── Launch and logistics ─────────────────────────────────────────────────

const OP_LABEL: Partial<Record<HqOpKind, string>> = {
  cap: 'CAP', strike: 'STRIKE', sead: 'SEAD', recon: 'RECON', artillery: 'ARTILLERY', missile_strike: 'MISSILES',
  ambush: 'AMBUSH', convoy: 'CONVOY', helo_supply: 'HELO SUPPLY', helo_troops: 'HELO TROOPS', reinforce: 'REINFORCE',
  bomber: 'BOMBER', awacs: 'AWACS', tanker: 'TANKER', naval_strike: 'NAVAL STRIKE', air_repair: 'AIR REPAIR',
}

export function LaunchPanel({ cp, selObj, side, canCommand, busy, onOrder, onFocus, onBarrage, onClose }: {
  cp: CommandPicture
  selObj: GroundObjective | null
  side: Side
  canCommand: boolean
  busy: boolean
  onOrder: (o: CommandOrder, confirm?: string) => void
  onFocus: (p: LatLon) => void
  onBarrage: () => void
  onClose: () => void
}): ReactElement {
  const ours = selObj && selObj.owner === side
  return (
    <aside className="cm-launch">
      <button className="gw-drawer-x" onClick={onClose} title="Close (L)"><X size={14} /></button>
      <h2>COMMAND</h2>
      <div className="cm-treasury">TREASURY <b>{cp.treasury.toLocaleString()}</b></div>
      {!canCommand && <p className="gw-drawer-note">Commanders give orders. You can watch.</p>}

      <h3>FIRES</h3>
      <div className="cm-orders">
        <button disabled={!canCommand || busy} onClick={onBarrage} title="Every battery of ours in range fires on a point (V)"><kbd>V</kbd> BARRAGE</button>
      </div>

      <h3>LOGISTICS{selObj ? ` · ${selObj.name.toUpperCase()}` : ''}</h3>
      {!selObj ? (
        <p className="gw-drawer-note">Select a base to send supplies or troops to it.</p>
      ) : (
        <div className="cm-orders">
          <button disabled={!canCommand || busy || !ours} title={ours ? 'A supply convoy by road' : 'Supplies go to our own bases'}
            onClick={() => onOrder({ convoy: { to: selObj.id } }, `Send a supply convoy to ${selObj.name}?`)}>CONVOY</button>
          <button disabled={!canCommand || busy || !ours} title={ours ? 'A helicopter supply run' : 'Supplies go to our own bases'}
            onClick={() => onOrder({ helo_supply: { to: selObj.id } }, `Send a helicopter supply run to ${selObj.name}?`)}>HELO SUPPLY</button>
          <button disabled={!canCommand || busy} title="Helicopters put troops down there"
            onClick={() => onOrder({ helo_troops: { to: selObj.id } }, `Send helicopter troops to ${selObj.name}?`)}>HELO TROOPS</button>
        </div>
      )}

      <h3>HQ OPERATIONS</h3>
      {!cp.hq ? (
        <p className="gw-drawer-note">This server has no theatre HQ.</p>
      ) : cp.launch.length === 0 ? (
        <p className="gw-drawer-note">The HQ has nothing it can run right now.</p>
      ) : (
        <div className="cm-ops">
          {cp.launch.map((l) => (
            <div key={`${l.kind}-${l.objective}`} className={`cm-op${l.ready ? '' : ' off'}`}>
              <button className="cm-op-where" onClick={() => onFocus(l.pos)} title="Show on the map">
                <b>{OP_LABEL[l.kind] ?? l.kind.toUpperCase()}</b> {l.objective_name.toUpperCase()}
                <span>{l.why}</span>
              </button>
              <button className="cm-op-go" disabled={!canCommand || busy || !l.ready}
                title={l.ready ? `Launch, ${l.cost} from the treasury` : 'Not enough in the treasury, or the HQ is at its limit'}
                onClick={() => onOrder({ launch: { kind: l.kind, objective: l.objective } },
                  `Launch ${OP_LABEL[l.kind] ?? l.kind} on ${l.objective_name} for ${l.cost.toLocaleString()} treasury points?`)}>
                {l.cost.toLocaleString()}
              </button>
            </div>
          ))}
        </div>
      )}
    </aside>
  )
}

// ── Orders to a base ─────────────────────────────────────────────────────

/** Beside a selected base in the command bar: what can be sent to it. */
export function BaseOrders({ obj, side, cp, canCommand, busy, onOrder }: {
  obj: GroundObjective
  side: Side
  cp: CommandPicture | null
  canCommand: boolean
  busy: boolean
  onOrder: (o: CommandOrder, confirm?: string) => void
}): ReactElement | null {
  // Nothing to order until the server has the command map (the layer
  // buttons say so).
  if (!cp) return null
  const ours = obj.owner === side
  const ops = cp.launch.filter((l) => l.objective === obj.id)
  const off = !canCommand || busy
  return (
    <div className="cm-bord">
      <div className="cm-bord-title">ORDERS · TREASURY {cp.treasury.toLocaleString()}</div>
      <div className="cm-orders">
        {ours && (
          <>
            <button disabled={off} title="A supply convoy by road"
              onClick={() => onOrder({ convoy: { to: obj.id } }, `Send a supply convoy to ${obj.name}?`)}>CONVOY</button>
            <button disabled={off} title="A helicopter supply run"
              onClick={() => onOrder({ helo_supply: { to: obj.id } }, `Send a helicopter supply run to ${obj.name}?`)}>HELO SUPPLY</button>
          </>
        )}
        <button disabled={off} title="Helicopters put troops down there"
          onClick={() => onOrder({ helo_troops: { to: obj.id } }, `Send helicopter troops to ${obj.name}?`)}>HELO TROOPS</button>
        {ops.map((l) => (
          <button key={l.kind} disabled={off || !l.ready} className="cm-op-btn"
            title={`${l.why}${l.ready ? '' : ' (not enough in the treasury, or the HQ is at its limit)'}`}
            onClick={() => onOrder({ launch: { kind: l.kind, objective: l.objective } },
              `Launch ${OP_LABEL[l.kind] ?? l.kind} on ${l.objective_name} for ${l.cost.toLocaleString()} treasury points?`)}>
            {OP_LABEL[l.kind] ?? l.kind.toUpperCase()} <span>{l.cost.toLocaleString()}</span>
          </button>
        ))}
      </div>
      {!canCommand && <div className="cm-bord-note">Commanders give orders.</div>}
    </div>
  )
}
