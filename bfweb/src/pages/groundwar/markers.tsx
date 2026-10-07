// The map's DOM markers: few enough to be React (formations, contacts, bases,
// battles, destinations), each memoised on what it actually shows so a new
// picture every 2 s only touches the ones that changed.
import { memo, type MouseEvent, type ReactElement } from 'react'
import { Marker } from 'react-map-gl/maplibre'
import { Defend, Objective as ObjIcon } from '@icons'
import type { GroundBattle, GroundEnemyContact, GroundFormation, GroundObjective } from '../../api'
import { fmtAge, offset } from './geo'
import { OBJ_ICON, natoSymbol, shortName } from './sprites'
import { ATTACK, ICON, PENCIL, SIDE_COLOR, WITHDRAW, other, type Side } from './theme'

const SYM = ICON
const SYM_NEAR = 15

export interface PickHandlers {
  onPick: (e: MouseEvent, id: number) => void
  onDouble?: (e: MouseEvent, id: number) => void
}

// ── Our formations ───────────────────────────────────────────────────────

interface FProps extends PickHandlers {
  f: GroundFormation
  side: Side
  selected: boolean
  near: boolean
  group: number | null
}

function FormationMarkerImpl({ f, side, selected, near, group, onPick, onDouble }: FProps): ReactElement {
  const pct = f.total ? f.alive / f.total : 0
  const barCol = pct >= 0.7 ? '#8ec83f' : pct >= 0.4 ? '#e8c547' : '#ff5b45'
  const flags: [string, string][] = []
  if (f.broken) flags.push(['BROKEN', '#ff5b45'])
  if (!f.in_supply) flags.push(['NO SUPPLY', '#e8c547'])
  if (f.halted) flags.push(['HALTED', '#e8c547'])
  if (!f.live) flags.push(['SIM', '#9a9a90'])
  const cls = `gw-mk gw-fm${selected ? ' sel' : ''}${f.broken ? ' broken' : ''}${f.engaged ? ' engaged' : ''}${!f.live ? ' sim' : ''}`
  const handlers = {
    onClick: (e: MouseEvent) => onPick(e, f.id),
    onDoubleClick: (e: MouseEvent) => onDouble?.(e, f.id),
  }
  if (near) {
    const s = natoSymbol({ kind: f.kind, hostile: false, company: false, size: SYM_NEAR, fill: SIDE_COLOR[side] })
    return (
      <Marker longitude={f.pos[1]} latitude={f.pos[0]} anchor="bottom" offset={[0, -14]}>
        <div className={`${cls} near`} {...handlers} title={f.name}>
          <img src={s.url} width={s.w} height={s.h} alt="" draggable={false} />
          <span className="gw-fm-name">{shortName(f.name)}</span>
          <span className="gw-fm-count" style={{ color: barCol }}>{f.alive}/{f.total}</span>
          {flags.map(([t, c]) => <span key={t} className="gw-flag" style={{ color: c, borderColor: c }}>{t}</span>)}
          {group != null && <span className="gw-grp">{group}</span>}
        </div>
      </Marker>
    )
  }
  const s = natoSymbol({
    kind: f.kind,
    hostile: false,
    company: true,
    size: SYM,
    fill: f.broken ? '#8a8a80' : SIDE_COLOR[side],
    designation: shortName(f.name),
    direction: f.posture === 'moving' ? f.heading : null,
    reduced: f.alive < f.total,
  })
  const fw = SYM * 0.75
  const fh = SYM * 0.5
  return (
    <Marker longitude={f.pos[1]} latitude={f.pos[0]} anchor="top-left" offset={[-s.ax, -s.ay]}>
      <div className={cls} {...handlers} style={{ width: s.w, height: s.h }} title={f.name}>
        <img src={s.url} width={s.w} height={s.h} alt="" draggable={false} />
        <div className="gw-fm-hp" style={{ left: s.ax - fw - 7, top: s.ay - fh, height: fh * 2 }}>
          <i style={{ height: `${Math.round(pct * 100)}%`, background: barCol }} />
        </div>
        {selected && (
          <div className="gw-brackets" style={{ left: s.ax - fw - 5, top: s.ay - fh - 5, width: fw * 2 + 10, height: fh * 2 + 10 }}>
            <i /><i /><i /><i />
          </div>
        )}
        {(flags.length > 0 || group != null) && (
          <div className="gw-fm-flags" style={{ left: s.ax + fw + 4, top: s.ay + 7 }}>
            {group != null && <span className="gw-grp">{group}</span>}
            {flags.map(([t, c]) => <span key={t} className="gw-flag" style={{ color: c, borderColor: c }}>{t}</span>)}
          </div>
        )}
      </div>
    </Marker>
  )
}

const fKey = (p: FProps) => {
  const f = p.f
  return `${f.id}|${f.pos[0].toFixed(5)}|${f.pos[1].toFixed(5)}|${Math.round(f.heading / 15)}|${f.alive}|${f.total}|${f.broken}|${f.in_supply}|${f.halted}|${f.live}|${f.engaged}|${f.posture}|${f.name}|${f.kind}|${p.selected}|${p.near}|${p.group}|${p.side}`
}
export const FormationMarker = memo(FormationMarkerImpl, (a, b) => fKey(a) === fKey(b) && a.onPick === b.onPick)

// ── Enemy contacts ───────────────────────────────────────────────────────

interface EProps {
  e: GroundEnemyContact
  side: Side
  near: boolean
  onPick: (e: MouseEvent, id: number) => void
}

function EnemyMarkerImpl({ e, side, near, onPick }: EProps): ReactElement {
  const ghost = e.last_seen_secs > 0
  // A last-seen position fades over a quarter of an hour.
  const op = ghost ? Math.max(0.28, 1 - e.last_seen_secs / 1200) : 1
  const s = natoSymbol({
    kind: e.kind,
    hostile: true,
    ghost,
    size: near && e.units.length ? SYM_NEAR : ICON,
    fill: SIDE_COLOR[other(side)],
    direction: e.moving && !ghost && !(near && e.units.length) ? e.heading : null,
  })
  return (
    <Marker longitude={e.pos[1]} latitude={e.pos[0]} anchor="top-left" offset={[-s.ax, -s.ay]}>
      <div
        className={`gw-mk gw-en${ghost ? ' ghost' : ''}`}
        style={{ width: s.w, height: s.h, opacity: op }}
        onClick={(ev) => onPick(ev, e.id)}
        title={ghost ? `Enemy ${e.kind}, last seen ${fmtAge(e.last_seen_secs).toLowerCase()} ago` : `Enemy ${e.kind}, about ${e.approx_vehicles} vehicles`}
      >
        <img src={s.url} width={s.w} height={s.h} alt="" draggable={false} />
        <div className="gw-en-tag" style={{ left: s.ax, top: s.ay + (near && e.units.length ? 10 : 16) }}>
          {ghost ? `LAST SEEN ${fmtAge(e.last_seen_secs)}` : `~${e.approx_vehicles} VEH`}
        </div>
      </div>
    </Marker>
  )
}
const eKey = (p: EProps) =>
  `${p.e.id}|${p.e.pos[0].toFixed(5)}|${p.e.pos[1].toFixed(5)}|${Math.round(p.e.heading / 15)}|${Math.floor(p.e.last_seen_secs / 30)}|${p.e.approx_vehicles}|${p.e.moving}|${p.e.units.length > 0}|${p.near}|${p.side}`
export const EnemyMarker = memo(EnemyMarkerImpl, (a, b) => eKey(a) === eKey(b) && a.onPick === b.onPick)

// ── Bases ────────────────────────────────────────────────────────────────


interface OProps {
  o: GroundObjective
  side: Side
  selected: boolean
  onPick: (e: MouseEvent, id: number) => void
}

function ObjectiveMarkerImpl({ o, side, selected, onPick }: OProps): ReactElement {
  const Icon = OBJ_ICON[o.kind] ?? ObjIcon
  const ours = o.owner === side
  const col = SIDE_COLOR[o.owner]
  return (
    <Marker longitude={o.pos[1]} latitude={o.pos[0]} anchor="top" offset={[0, -13]}>
      <div
        className={`gw-mk gw-obj${ours ? ' ours' : ''}${o.being_captured ? ' capturing' : ''}${selected ? ' sel' : ''}`}
        style={{ ['--oc' as string]: col }}
        onClick={(e) => onPick(e, o.id)}
        title={`${o.name} (${o.kind}, ${o.owner})`}
      >
        <div className="gw-obj-plate">
          <Icon size={16} strokeWidth={1.7} />
          {o.threatened && <span className="gw-obj-threat">!</span>}
          {ours && (o.can_raise ?? 0) > 0 && <span className="gw-obj-raise">+{o.can_raise}</span>}
        </div>
        <div className="gw-obj-name">{o.name.toUpperCase()}</div>
        {ours && (o.supply != null || o.garrison != null) && (
          <div className="gw-obj-bars">
            {o.supply != null && (
              <div className="gw-obj-sup" title={`Supply ${o.supply}%`}>
                <i style={{ width: `${Math.max(0, Math.min(100, o.supply))}%`, background: o.supply < 30 ? '#ff5b45' : o.supply < 60 ? '#e8c547' : '#8ec83f' }} />
              </div>
            )}
            {o.garrison != null && (
              <div className="gw-obj-gar" title={`Garrison: ${o.garrison} groups`}>
                {Array.from({ length: Math.min(8, o.garrison) }, (_, i) => <i key={i} />)}
              </div>
            )}
          </div>
        )}
      </div>
    </Marker>
  )
}
const oKey = (p: OProps) =>
  `${p.o.id}|${p.o.owner}|${p.o.being_captured}|${p.o.threatened}|${p.o.can_raise}|${p.o.supply}|${p.o.garrison}|${p.o.name}|${p.o.kind}|${p.selected}|${p.side}`
export const ObjectiveMarker = memo(ObjectiveMarkerImpl, (a, b) => oKey(a) === oKey(b) && a.onPick === b.onPick)

// ── Battles ──────────────────────────────────────────────────────────────

export function BattleLabel({ b, onPick }: { b: GroundBattle; onPick: (b: GroundBattle) => void }): ReactElement {
  // On the ring's northern edge, clear of the formations fighting inside it.
  const top = offset(b.pos, 0, b.radius_m)
  return (
    <Marker longitude={top[1]} latitude={top[0]} anchor="bottom" offset={[0, -4]}>
      <div className={`gw-mk gw-battle${b.live ? ' live' : ''}`} onClick={() => onPick(b)}>
        <b>⚔ {(b.near ?? 'BATTLE').toUpperCase()}</b>
        <span>OURS {b.our_losses} LOST · THEIRS {b.enemy_losses}</span>
      </div>
    </Marker>
  )
}

// ── Where a formation is going ───────────────────────────────────────────

function Chevrons() {
  return (
    <svg width="18" height="14" viewBox="0 0 18 14" aria-hidden="true">
      <path d="M2 2 7 7 2 12M9 2l5 5-5 5" fill="none" stroke="currentColor" strokeWidth="2.2" strokeLinecap="square" />
    </svg>
  )
}
function BackArrow() {
  return (
    <svg width="16" height="14" viewBox="0 0 16 14" aria-hidden="true">
      <path d="M14 7H3M7 2 2 7l5 5" fill="none" stroke="currentColor" strokeWidth="2.2" strokeLinecap="square" />
    </svg>
  )
}

export function DestMarker({ f, selected, side }: { f: GroundFormation; selected: boolean; side: Side }): ReactElement | null {
  const end = f.path[f.path.length - 1]
  if (!end || f.order === 'hold') return null
  const col = f.order === 'attack' ? ATTACK : f.order === 'withdraw' ? WITHDRAW : SIDE_COLOR[side]
  return (
    <Marker longitude={end[1]} latitude={end[0]} anchor="center">
      <div className={`gw-dest${selected ? ' sel' : ''}`} style={{ color: col, borderColor: selected ? PENCIL : col }}>
        {f.order === 'attack' ? <Chevrons /> : f.order === 'withdraw' ? <BackArrow /> : <Defend size={14} strokeWidth={2} />}
        {selected && (
          <span className="gw-dest-eta">
            {shortName(f.name)} · {f.km_to_go.toFixed(1)} KM{f.eta_mins != null ? ` · ETA ${f.eta_mins} MIN` : ''}
          </span>
        )}
      </div>
    </Marker>
  )
}
