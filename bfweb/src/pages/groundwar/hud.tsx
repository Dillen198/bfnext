// The game chrome laid over the map: top status bar, combat log, the RTS
// command bar (selection cards + command card), the single-formation detail
// drawer, toasts and the controls overlay. Presentational only -- the page
// owns state and passes actions down.
import { useEffect, useState, type ReactElement, type ReactNode } from 'react'
import {
  Activity, Alert, Armor, Capture, Defend, Eye, Helicopter, Plus, Shield, Strike, Supply, X, type IconComponent,
} from '@icons'
import type {
  GroundEnemyContact, GroundEvent, GroundFormation, GroundObjective, GroundPicture, GroundRole,
} from '../../api'
import { fmtAge, fmtZulu } from './geo'
import { MAP_LOOKS, type MapLook } from './mapStyles'
import { OBJ_ICON, natoSymbol, shortName, vehicleSpriteUrl } from './sprites'
import { ROLES, ROLE_LABEL, SIDE_COLOR, other, type Side } from './theme'
import type { Link } from './useGroundFeed'

// ── Bits ─────────────────────────────────────────────────────────────────

function Gauge({ label, pct, warn = 50, bad = 25, title }: { label: string; pct: number; warn?: number; bad?: number; title?: string }) {
  const v = Math.max(0, Math.min(100, pct))
  const col = v < bad ? '#ff5b45' : v < warn ? '#e8c547' : '#8ec83f'
  return (
    <div className="gw-gauge" title={title ?? `${label} ${Math.round(v)}%`}>
      <span>{label}</span>
      <div><i style={{ width: `${v}%`, background: col }} /></div>
      <b style={{ color: col }}>{Math.round(v)}</b>
    </div>
  )
}

function Strength({ alive, total }: { alive: number; total: number }) {
  const cells = Math.min(total, 16)
  const per = total / cells
  return (
    <div className="gw-str" title={`${alive} of ${total} vehicles`}>
      {Array.from({ length: cells }, (_, i) => <i key={i} className={i * per < alive ? 'on' : ''} />)}
      <b>{alive}/{total}</b>
    </div>
  )
}

function orderLine(f: GroundFormation): string {
  const to = f.target_name?.toUpperCase() ?? '?'
  const where = f.posture === 'moving' && f.km_to_go > 0
    ? ` · ${f.km_to_go.toFixed(1)} KM${f.eta_mins != null ? ` · ETA ${f.eta_mins}M` : ''}`
    : ''
  if (f.broken) return `FALLING BACK${f.target_name ? ` ON ${to}` : ''}${where}`
  switch (f.order) {
    case 'hold': return f.posture === 'moving' ? `MOVING${where}` : 'HOLDING'
    case 'attack': return `${f.posture === 'assaulting' ? 'ASSAULTING' : 'ATTACK'} ${to}${where}`
    case 'defend': return `DEFEND ${to}${where}`
    case 'withdraw': return `WITHDRAW → ${to}${where}`
  }
}

const DEPLOY: Record<GroundFormation['deployment'], string> = {
  column: 'COLUMN',
  deploying: 'DEPLOYING',
  deployed: 'DEPLOYED',
  dug_in: 'DUG IN',
}

function Clock({ base, at }: { base: number; at: number }) {
  const [txt, setTxt] = useState(() => fmtZulu(base))
  useEffect(() => {
    const tick = () => setTxt(fmtZulu(base + Math.max(0, (performance.now() - at) / 1000)))
    tick()
    const iv = window.setInterval(tick, 1000)
    return () => window.clearInterval(iv)
  }, [base, at])
  return <>{txt}</>
}

// ── Top bar ──────────────────────────────────────────────────────────────

export function TopHud({
  pic, link, frameAt, look, setLook, fog, setFog, territory, setTerritory, onHelp, adminPick, viewSide, setViewSide,
}: {
  pic: GroundPicture
  link: Link
  frameAt: number
  look: MapLook
  setLook: (l: MapLook) => void
  fog: boolean
  setFog: (b: boolean) => void
  territory: boolean
  setTerritory: (b: boolean) => void
  onHelp: () => void
  adminPick: boolean
  viewSide: Side
  setViewSide: (s: Side) => void
}): ReactElement {
  const side = pic.side as Side
  const ourPower = pic.formations.reduce((a, f) => a + f.power, 0)
  const perVeh = (() => {
    const full = pic.formations.reduce((a, f) => a + f.power_full, 0)
    const tot = pic.formations.reduce((a, f) => a + f.total, 0)
    return tot ? full / tot : 10
  })()
  const spottedVeh = pic.enemy.filter((e) => e.last_seen_secs === 0).reduce((a, e) => a + e.approx_vehicles, 0)
  const theirPower = spottedVeh * perVeh
  const share = ourPower + theirPower > 0 ? ourPower / (ourPower + theirPower) : 0.5
  const linkTxt: Record<Link, [string, string]> = {
    live: ['LIVE', '#8ec83f'],
    polling: ['POLL 5S', '#e8c547'],
    offline: ['OFFLINE', '#ff5b45'],
    connecting: ['LINKING', '#9a9a90'],
    mock: ['MOCK', '#b48cff'],
  }
  const [lt, lc] = linkTxt[link]
  return (
    <header className="gw-hud" style={{ ['--own' as string]: SIDE_COLOR[side] }}>
      <div className="gw-hud-id">
        <span className="gw-hud-side">{side === 'Blue' ? 'BLUE FORCES' : 'RED FORCES'}</span>
        <span className="gw-hud-title">GROUND COMMAND</span>
      </div>
      <div className="gw-hud-stats">
        <div className="gw-stat"><span>ZULU</span><b><Clock base={pic.time} at={frameAt} /></b></div>
        <div className="gw-stat"><span>FORMATIONS</span><b>{pic.formations.length}<small>/{pic.max_formations}</small></b></div>
        <div className="gw-stat" title="Formations spawned in DCS right now; the rest are moved by the engine off-map">
          <span>IN DCS</span><b>{pic.live}<small>/{pic.max_live}</small></b>
        </div>
        <div className="gw-stat"><span>BATTLES</span><b style={{ color: pic.battles.some((b) => b.live) ? '#ff8c1a' : undefined }}>{pic.battles.length}</b></div>
        <div className="gw-stat"><span>PILOTS</span><b>{pic.players.length}</b></div>
        <div className="gw-power" title={`Our combat power ${Math.round(ourPower)}; spotted enemy about ${Math.round(theirPower)} (estimated from ${spottedVeh} vehicles in sight)`}>
          <span>STRENGTH · OURS {Math.round(ourPower)} / SPOTTED ~{Math.round(theirPower)}</span>
          <div>
            <i style={{ width: `${share * 100}%`, background: SIDE_COLOR[side] }} />
            <i style={{ width: `${(1 - share) * 100}%`, background: SIDE_COLOR[other(side)] }} />
          </div>
        </div>
      </div>
      <div className="gw-hud-tools">
        {adminPick && (
          <div className="gw-seg" title="Admin: which side to watch">
            {(['Blue', 'Red'] as const).map((s) => (
              <button key={s} className={viewSide === s ? 'on' : ''} style={{ ['--c' as string]: SIDE_COLOR[s] }} onClick={() => setViewSide(s)}>
                {s.toUpperCase()}
              </button>
            ))}
          </div>
        )}
        <div className="gw-seg">
          {MAP_LOOKS.map((l) => (
            <button key={l.key} className={look === l.key ? 'on' : ''} title={l.hint} onClick={() => setLook(l.key)}>{l.label}</button>
          ))}
        </div>
        <div className="gw-seg">
          <button className={fog ? 'on' : ''} title="Shade what our forces can't see" onClick={() => setFog(!fog)}>FOG</button>
          <button className={territory ? 'on' : ''} title="Frontline and territory wash" onClick={() => setTerritory(!territory)}>TERR</button>
        </div>
        <span className="gw-link" style={{ color: lc }} title={link === 'polling' ? 'Live feed unavailable; polling every 5 s' : undefined}>
          <i style={{ background: lc }} />{lt}
        </span>
        <button className="gw-help-btn" onClick={onHelp} title="Controls (?)">?</button>
      </div>
    </header>
  )
}

// ── Combat log ───────────────────────────────────────────────────────────

const EVENT_ICON: Record<GroundEvent['kind'], [IconComponent, string]> = {
  contact: [Eye, '#e8c547'],
  battle: [Strike, '#ff8c1a'],
  loss: [X, '#ff5b45'],
  kill: [Strike, '#8ec83f'],
  assault: [Capture, '#ff8c1a'],
  capture: [Capture, '#8ec83f'],
  broken: [Alert, '#ff5b45'],
  supply: [Supply, '#e8c547'],
  arrived: [Defend, '#b9bca9'],
  raised: [Plus, '#8ec83f'],
  order: [Activity, '#ffd23f'],
  destroyed: [X, '#ff5b45'],
}

export function EventFeed({ events, time, onPick }: { events: GroundEvent[]; time: number; onPick: (e: GroundEvent) => void }): ReactElement {
  const [open, setOpen] = useState(true)
  const list = [...events].sort((a, b) => b.at - a.at).slice(0, 14)
  return (
    <section className={`gw-log${open ? '' : ' shut'}`}>
      <button className="gw-log-head" onClick={() => setOpen(!open)}>
        <span>COMBAT LOG</span><span>{open ? '–' : '+'}</span>
      </button>
      {open && (
        <ol>
          {list.length === 0 && <li className="gw-log-empty">Quiet front. Nothing reported yet.</li>}
          {list.map((e) => {
            const [Icon, col] = EVENT_ICON[e.kind] ?? [Activity, '#b9bca9']
            return (
              <li key={`${e.at}|${e.text}`} className={e.pos ? 'go' : ''} onClick={() => onPick(e)} title={e.pos ? 'Show on the map' : undefined}>
                <Icon size={13} strokeWidth={1.8} style={{ color: col, flexShrink: 0 }} />
                <span className="gw-log-txt">{e.text}</span>
                <time>{fmtAge(Math.max(0, time - e.at))}</time>
              </li>
            )
          })}
        </ol>
      )}
    </section>
  )
}

// ── Command bar ──────────────────────────────────────────────────────────

export interface CmdButton {
  key: string
  label: string
  icon: IconComponent | (() => ReactElement)
  active?: boolean
  /** Why it can't be used right now; null when it can. */
  disabled: string | null
  tone?: 'attack' | 'withdraw' | 'plain'
  run: () => void
}

function CommandCard({ buttons, locked }: { buttons: CmdButton[]; locked: string | null }) {
  return (
    <div className="gw-card-grid">
      {buttons.map((b) => {
        const Icon = b.icon
        return (
          <button
            key={b.key}
            className={`gw-cmd${b.active ? ' on' : ''}${b.tone ? ` ${b.tone}` : ''}`}
            disabled={!!b.disabled}
            title={b.disabled ?? `${b.label} (${b.key})`}
            onClick={(e) => {
              // Hand the keyboard back to the map: Space centres, it doesn't re-press.
              e.currentTarget.blur()
              b.run()
            }}
          >
            <kbd>{b.key}</kbd>
            <Icon size={20} strokeWidth={1.6} />
            <span>{b.label}</span>
          </button>
        )
      })}
      {locked && <div className="gw-card-lock">{locked}</div>}
    </div>
  )
}

function FormationCard({ f, side, group, onPick, compact }: { f: GroundFormation; side: Side; group: number | null; onPick: () => void; compact?: boolean }) {
  const s = natoSymbol({ kind: f.kind, hostile: false, company: true, size: compact ? 16 : 20, fill: f.broken ? '#8a8a80' : SIDE_COLOR[side] })
  return (
    <button className={`gw-fcard${compact ? ' compact' : ''}${f.broken ? ' broken' : ''}`} onClick={onPick} title={f.name}>
      <div className="gw-fcard-head">
        <img src={s.url} width={s.w} height={s.h} alt="" />
        <b>{shortName(f.name)}</b>
        {group != null && <span className="gw-grp">{group}</span>}
        <span className="gw-fcard-cmd" style={{ color: f.commander ? SIDE_COLOR[side] : undefined }}>{f.commander ?? 'AI'}</span>
      </div>
      <Strength alive={f.alive} total={f.total} />
      {!compact && (
        <>
          <div className="gw-fcard-gauges">
            <Gauge label="SUP" pct={f.supply_pct} title={`Fuel and ammunition ${f.supply_pct}%${f.in_supply ? '' : ' (cut off)'}`} />
            <Gauge label="MOR" pct={f.morale_pct} warn={45} bad={25} />
          </div>
          <div className="gw-fcard-order">{orderLine(f)}</div>
          <div className="gw-fcard-tags">
            <span>{DEPLOY[f.deployment]}</span>
            {f.engaged && <span className="hot">ENGAGED</span>}
            {f.broken && <span className="bad">BROKEN</span>}
            {!f.in_supply && <span className="warn">CUT OFF</span>}
            {f.halted && <span className="warn">HALTED</span>}
            <span className={f.live ? 'ok' : ''}>{f.live ? 'IN DCS' : 'SIM'}</span>
          </div>
        </>
      )}
    </button>
  )
}

export function CommandBar({
  pic, side, selected, selObj, selEnemy, groups, buttons, locked, onSelect, onFocusObj,
}: {
  pic: GroundPicture
  side: Side
  selected: GroundFormation[]
  selObj: GroundObjective | null
  selEnemy: GroundEnemyContact | null
  groups: Record<number, number[]>
  buttons: CmdButton[]
  locked: string | null
  onSelect: (id: number, add: boolean) => void
  onFocusObj: (o: GroundObjective) => void
}): ReactElement {
  const groupOf = (id: number): number | null => {
    for (const [k, ids] of Object.entries(groups)) if (ids.includes(id)) return Number(k)
    return null
  }
  let body: ReactNode
  if (selected.length > 0) {
    body = (
      <>
        <div className="gw-bar-title">{selected.length === 1 ? 'SELECTED' : `${selected.length} SELECTED`}</div>
        <div className={`gw-cards${selected.length > 4 ? ' many' : ''}`}>
          {selected.map((f) => (
            <FormationCard key={f.id} f={f} side={side} group={groupOf(f.id)} compact={selected.length > 4} onPick={() => onSelect(f.id, false)} />
          ))}
        </div>
      </>
    )
  } else if (selObj) {
    const Icon = OBJ_ICON[selObj.kind] ?? Shield
    const ours = selObj.owner === side
    body = (
      <>
        <div className="gw-bar-title">BASE</div>
        <div className="gw-ocard" style={{ ['--oc' as string]: SIDE_COLOR[selObj.owner] }}>
          <div className="gw-ocard-head" onClick={() => onFocusObj(selObj)}>
            <Icon size={22} strokeWidth={1.6} />
            <div>
              <b>{selObj.name.toUpperCase()}</b>
              <span>{selObj.kind.toUpperCase()} · {selObj.owner === side ? 'OURS' : selObj.owner === 'Neutral' ? 'NEUTRAL' : 'ENEMY'}</span>
            </div>
          </div>
          {ours ? (
            <div className="gw-ocard-body">
              {selObj.health != null && <Gauge label="HP" pct={selObj.health} />}
              {selObj.supply != null && <Gauge label="SUP" pct={selObj.supply} />}
              <div className="gw-ocard-line">
                GARRISON {selObj.garrison ?? '?'} · {(selObj.can_raise ?? 0) > 0 ? `CAN RAISE ${selObj.can_raise}` : 'CAN\'T RAISE'}
                {selObj.threatened ? ' · THREATENED' : ''}{selObj.being_captured ? ' · UNDER ASSAULT' : ''}
              </div>
            </div>
          ) : (
            <div className="gw-ocard-body">
              <div className="gw-ocard-line">
                {selObj.being_captured ? 'OUR TROOPS ARE ASSAULTING IT. ' : ''}Select formations, then right-click it to attack.
              </div>
            </div>
          )}
        </div>
      </>
    )
  } else if (selEnemy) {
    const ghost = selEnemy.last_seen_secs > 0
    const s = natoSymbol({ kind: selEnemy.kind, hostile: true, ghost, size: 20, fill: SIDE_COLOR[other(side)] })
    body = (
      <>
        <div className="gw-bar-title">ENEMY CONTACT</div>
        <div className="gw-ocard">
          <div className="gw-ocard-head">
            <img src={s.url} width={s.w} height={s.h} alt="" />
            <div>
              <b>HOSTILE {selEnemy.kind.toUpperCase()}</b>
              <span>~{selEnemy.approx_vehicles} VEHICLES · {ghost ? `LAST SEEN ${fmtAge(selEnemy.last_seen_secs)} AGO` : 'IN SIGHT'}{selEnemy.moving ? ' · MOVING' : ''}</span>
            </div>
          </div>
          <div className="gw-ocard-body">
            <div className="gw-ocard-line">Orders go to bases: right-click the nearest one with your formations selected.</div>
          </div>
        </div>
      </>
    )
  } else {
    body = (
      <>
        <div className="gw-bar-title">FORMATIONS · CLICK, TAB OR 1-9 TO SELECT</div>
        <div className="gw-cards roster">
          {pic.formations.length === 0 && (
            <div className="gw-empty">No formations in the field. Select one of our bases and press R to raise one.</div>
          )}
          {pic.formations.map((f) => (
            <FormationCard key={f.id} f={f} side={side} group={groupOf(f.id)} compact onPick={() => onSelect(f.id, false)} />
          ))}
        </div>
      </>
    )
  }
  return (
    <footer className="gw-bar">
      <div className="gw-bar-sel">{body}</div>
      <CommandCard buttons={buttons} locked={locked} />
    </footer>
  )
}

// ── Detail drawer ────────────────────────────────────────────────────────

export function Drawer({ f, side, pic, lockMins, onClose }: { f: GroundFormation; side: Side; pic: GroundPicture; lockMins: number; onClose: () => void }): ReactElement {
  const s = natoSymbol({
    kind: f.kind, hostile: false, company: true, size: 30, fill: f.broken ? '#8a8a80' : SIDE_COLOR[side],
    designation: shortName(f.name), reduced: f.alive < f.total,
  })
  const powerPct = f.power_full > 0 ? (f.power / f.power_full) * 100 : 0
  const home = pic.objectives.find((o) => o.id === f.home)
  const roles = ROLES.filter((r) => (f.make_up[r] ?? 0) > 0)
  return (
    <aside className="gw-drawer">
      <button className="gw-drawer-x" onClick={onClose} title="Deselect (Esc)"><X size={14} /></button>
      <div className="gw-drawer-head">
        <img src={s.url} width={s.w} height={s.h} alt="" />
      </div>
      <h2>{f.name}</h2>
      <div className="gw-drawer-sub">
        {f.kind.toUpperCase()} COMPANY · {f.commander ? `COMMANDED BY ${f.commander.toUpperCase()}${f.locked_mins != null ? ` · ${f.locked_mins} MIN LEFT` : ''}` : 'AI COMMAND'}
      </div>
      <div className="gw-drawer-order">{orderLine(f)}</div>

      <h3>COMPOSITION</h3>
      <div className="gw-chips">
        {roles.map((r: GroundRole) => (
          <span key={r} className="gw-chip" title={ROLE_LABEL[r]}>
            <img src={vehicleSpriteUrl(r, side)} width={18} height={18} alt="" />
            {f.make_up[r]} {ROLE_LABEL[r].toUpperCase()}
          </span>
        ))}
        {roles.length === 0 && <span className="gw-chip">NO VEHICLES LEFT</span>}
      </div>
      <Strength alive={f.alive} total={f.total} />

      <h3>CONDITION</h3>
      <Gauge label="PWR" pct={powerPct} warn={55} bad={30} title={`Combat power ${Math.round(f.power)} of ${Math.round(f.power_full)} at full strength`} />
      <Gauge label="SUP" pct={f.supply_pct} title={`Fuel and ammunition ${f.supply_pct}%`} />
      <Gauge label="MOR" pct={f.morale_pct} warn={45} bad={25} />
      <dl className="gw-dl">
        <dt>SUPPLY LINE</dt><dd className={f.in_supply ? 'ok' : 'bad'}>{f.in_supply ? 'IN SUPPLY' : 'CUT OFF'}</dd>
        <dt>DEPLOYMENT</dt><dd>{DEPLOY[f.deployment]}{f.speed_kph > 0 ? ` · ${Math.round(f.speed_kph)} KM/H` : ''}</dd>
        <dt>LOSSES / KILLS</dt><dd><span className="bad">{f.losses}</span> / <span className="ok">{f.kills}</span></dd>
        <dt>HOME</dt><dd>{(home?.name ?? f.home_name).toUpperCase()}{home && home.owner !== side ? ' (LOST)' : ''}</dd>
        {f.path.length > 1 && (<><dt>ROUTE</dt><dd>{f.km_to_go.toFixed(1)} KM{f.eta_mins != null ? ` · ETA ${f.eta_mins} MIN` : ''}</dd></>)}
        <dt>STATUS</dt><dd>{f.live ? 'SPAWNED IN DCS' : 'OFF-MAP (SIMULATED)'}</dd>
      </dl>
      {!f.has_infantry && <p className="gw-drawer-note">No infantry or troop carriers left: it can break a base but not take it.</p>}
      {f.broken && <p className="gw-drawer-note bad">Morale has collapsed. It is falling back whatever its orders until it rallies.</p>}
      <p className="gw-drawer-note">Your orders keep the AI off it for {lockMins} min.</p>
    </aside>
  )
}

// ── Toasts ───────────────────────────────────────────────────────────────

export interface Toast { id: number; ok: boolean; text: string }

export function Toasts({ toasts, onDismiss }: { toasts: Toast[]; onDismiss: (id: number) => void }): ReactElement {
  return (
    <div className="gw-toasts" aria-live="polite">
      {toasts.map((t) => (
        <div key={t.id} className={`gw-toast ${t.ok ? 'ok' : 'bad'}`} onClick={() => onDismiss(t.id)}>
          <span>{t.ok ? '✓' : '✕'}</span>{t.text}
        </div>
      ))}
    </div>
  )
}

// ── Controls overlay ─────────────────────────────────────────────────────

const KEYS: [string, string][] = [
  ['Click', 'Select a formation, base or contact'],
  ['Shift + click', 'Add or remove a formation'],
  ['Shift + drag', 'Box-select formations (Ctrl + Shift adds)'],
  ['Double-click', 'Every formation of that kind on screen'],
  ['Right-click base', 'Order the selection there: enemy base = attack, ours = move and defend'],
  ['A', 'Attack: next click on an enemy base'],
  ['D', 'Defend: next click on one of our bases'],
  ['H', 'Hold where they are'],
  ['W', 'Withdraw to home, or the nearest base we hold'],
  ['X', 'Hand back to the AI'],
  ['R', 'Raise a formation at the base under the cursor or selected'],
  ['Ctrl / Alt + 1-9', 'Make the selection a group'],
  ['1-9', 'Select a group; twice to centre on it'],
  ['Space', 'Centre on the selection'],
  ['Tab', 'Next formation (Shift + Tab: previous)'],
  ['F', 'Follow your own aircraft'],
  ['Esc', 'Cancel the order, then deselect'],
  ['?', 'This overlay'],
]

export function HelpOverlay({ onClose, canCommand }: { onClose: () => void; canCommand: boolean }): ReactElement {
  return (
    <div className="gw-help" onClick={onClose}>
      <div className="gw-help-card" onClick={(e) => e.stopPropagation()} role="dialog" aria-label="Controls">
        <div className="gw-help-head">
          <Armor size={20} />
          <h2>CONTROLS</h2>
          <button onClick={onClose} title="Close (Esc)"><X size={14} /></button>
        </div>
        <dl>
          {KEYS.map(([k, v]) => (<div key={k}><dt><kbd>{k}</kbd></dt><dd>{v}</dd></div>))}
        </dl>
        <p>
          Orders go to bases: attack, defend or withdraw to an objective, hold, release, or raise a new company at
          one of ours. {canCommand ? '' : 'You are in view-only mode, so the orders are switched off.'} Every order
          is checked again by the engine against the side your pilot is on.
        </p>
        <p className="gw-help-foot"><Helicopter size={14} /> Live pilots on our side are drawn in real time; yours is marked YOU.</p>
      </div>
    </div>
  )
}
