/*
 * The order of battle: the side's formations (battalions) grouped into the
 * regiments the engine keeps (`groundwar::oob`), each battalion's companies,
 * and what each is doing. Click a regiment to select all its battalions, a
 * battalion to select it; both fly the map there.
 */
import { useState, type ReactElement } from 'react'
import { X } from '@icons'
import type { GroundEvent, GroundFormation, GroundPicture, GroundRegiment, LatLon } from '../../api'

const pct = (a: number, t: number) => (t > 0 ? Math.round((a * 100) / t) : 0)

function strengthTone(p: number): string {
  return p >= 70 ? 'ok' : p >= 40 ? 'warn' : 'bad'
}

/** What a battalion is doing, in one line. */
function battalionDoing(f: GroundFormation): string {
  if (f.broken) return 'broken, falling back'
  const at = f.target_name ?? 'its objective'
  const verb = f.order === 'attack' ? `attacking ${at}`
    : f.order === 'defend' ? `defending ${at}`
      : f.order === 'withdraw' ? `withdrawing to ${at}`
        : f.posture === 'moving' ? 'moving' : 'holding'
  if (f.posture === 'moving' && f.km_to_go > 0.5) {
    return `${verb} · ${f.km_to_go.toFixed(0)} km${f.eta_mins != null ? `, ${f.eta_mins} min` : ''}`
  }
  if (f.posture === 'assaulting') return `${verb} · assaulting`
  return f.engaged ? `${verb} · in contact` : verb
}

/** The regiments of a picture; an engine too old to send them groups
 *  nothing, and every formation shows under "Unassigned". */
function regimentsOf(pic: GroundPicture): { regs: GroundRegiment[]; loose: GroundFormation[] } {
  const regs = pic.regiments ?? []
  const inReg = new Set(regs.flatMap((r) => r.battalions))
  return { regs, loose: pic.formations.filter((f) => !inReg.has(f.id)) }
}

function Battalion({ f, open, onToggle, onPick }: {
  f: GroundFormation
  open: boolean
  onToggle: () => void
  onPick: (ids: number[], pos: LatLon) => void
}): ReactElement {
  const p = pct(f.alive, f.total)
  return (
    <li className={`ob-bn${f.engaged ? ' engaged' : ''}${f.broken ? ' broken' : ''}`}>
      <div className="ob-row">
        <button className="ob-fold" onClick={onToggle} aria-expanded={open} title={open ? 'Hide companies' : 'Show companies'}>
          {open ? '−' : '+'}
        </button>
        <button className="ob-name" onClick={() => onPick([f.id], f.pos)} title="Select and fly there">
          <b>{f.battalion || 'Bn'}</b> {f.name}
        </button>
        <span className={`ob-str ${strengthTone(p)}`} title={`${f.alive} of ${f.total} vehicles`}>{p}%</span>
      </div>
      <div className="ob-doing">
        {battalionDoing(f)}
        {f.commander ? ` · ${f.commander}` : ''}
      </div>
      <div className="ob-meta">
        {f.kind} · morale {f.morale_pct}% · supply {f.supply_pct}%{f.in_supply ? '' : ' (cut off)'}
        {f.kills || f.losses ? ` · ${f.kills} kills, ${f.losses} lost` : ''}
      </div>
      {open && (
        <ul className="ob-coys">
          {(f.companies ?? []).map((c) => (
            <li key={c.name} className={c.alive === 0 ? 'dead' : undefined}>
              <span>{c.name}</span>
              <span className="ob-coy-typ">{c.role}{c.typ ? ` · ${c.typ}` : ''}</span>
              <span className={`ob-str ${strengthTone(pct(c.alive, c.total))}`}>{c.alive}/{c.total}</span>
            </li>
          ))}
          {!(f.companies ?? []).length && <li className="dead">no company detail from this engine</li>}
        </ul>
      )}
    </li>
  )
}

export function OrbatPanel({ pic, onPick, onEvent, onClose }: {
  pic: GroundPicture
  /** Select these formations and fly to `pos`. */
  onPick: (ids: number[], pos: LatLon) => void
  onEvent: (e: GroundEvent) => void
  onClose: () => void
}): ReactElement {
  const [open, setOpen] = useState<Set<number>>(new Set())
  const toggle = (id: number) => setOpen((s) => {
    const n = new Set(s)
    if (n.has(id)) n.delete(id)
    else n.add(id)
    return n
  })
  const byId = new Map(pic.formations.map((f) => [f.id, f]))
  const { regs, loose } = regimentsOf(pic)
  const alive = pic.formations.reduce((a, f) => a + f.alive, 0)
  return (
    <aside className="cm-launch ob-panel">
      <button className="gw-drawer-x" onClick={onClose} title="Close (O)"><X size={14} /></button>
      <h2>ORDER OF BATTLE</h2>
      <div className="cm-treasury">
        {regs.length} REGIMENT{regs.length === 1 ? '' : 'S'} · {pic.formations.length} BATTALION{pic.formations.length === 1 ? '' : 'S'} · <b>{alive}</b> VEHICLES
      </div>
      {!pic.formations.length && <p className="gw-drawer-note">No formations in the field. The AI raises them from base garrisons as the war needs them.</p>}
      {regs.map((r) => {
        const bns = r.battalions.map((id) => byId.get(id)).filter((f): f is GroundFormation => !!f)
        const ids = new Set(r.battalions)
        const news = pic.events.filter((e) => e.formation != null && ids.has(e.formation)).slice(0, 3)
        const p = pct(r.alive, r.total)
        return (
          <section key={r.id} className={`ob-reg${r.main_effort ? ' main' : ''}`}>
            <button className="ob-reg-head" onClick={() => onPick(r.battalions, r.pos)} title="Select its battalions and fly there">
              <span className="ob-reg-name">{r.name}</span>
              {r.main_effort && <span className="ob-badge" title="The HQ's main effort">MAIN EFFORT</span>}
              <span className={`ob-str ${strengthTone(p)}`} title={`${r.alive} of ${r.total} vehicles`}>{p}%</span>
            </button>
            <div className="ob-task">
              {r.task}{r.engaged ? ` · ${r.engaged} in contact` : ''}
            </div>
            <ul className="ob-bns">
              {bns.map((f) => (
                <Battalion key={f.id} f={f} open={open.has(f.id)} onToggle={() => toggle(f.id)} onPick={onPick} />
              ))}
            </ul>
            {news.length > 0 && (
              <ul className="ob-news">
                {news.map((e) => (
                  <li key={`${e.at}-${e.text}`}>
                    <button onClick={() => onEvent(e)}>{e.text}</button>
                  </li>
                ))}
              </ul>
            )}
          </section>
        )
      })}
      {loose.length > 0 && (
        <section className="ob-reg">
          <div className="ob-reg-head static"><span className="ob-reg-name">Unassigned</span></div>
          <div className="ob-task">joins a regiment on the next ground-war tick</div>
          <ul className="ob-bns">
            {loose.map((f) => (
              <Battalion key={f.id} f={f} open={open.has(f.id)} onToggle={() => toggle(f.id)} onPick={onPick} />
            ))}
          </ul>
        </section>
      )}
    </aside>
  )
}
