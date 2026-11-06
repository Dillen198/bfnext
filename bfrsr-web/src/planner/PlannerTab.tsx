import { useMemo, useState, type CSSProperties, type ReactNode } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { ArrowDown, ArrowUp, Download, Plus, Save, Trash2 as Trash, X } from '@icons'
import { api, type Briefing, type SituationReport } from '../api'
import Panel from '../components/Panel'
import PlannerMap from './PlannerMap'
import { fmtClock, fmtDuration, legs, parseClock, terrainAt } from './nav'
import { combatFliteXml, download, safeName } from './exports'
import { routeCardPng } from './card'
import DtcCard from './dtc/DtcCard'
import { commsFromCard, type MissionData } from './dtc/mission'
import { mockTacPicture } from '../pages/briefingMocks'
import { targetsFor } from './targets'
import {
  AIRCRAFT, aircraftDef, MAX_CHANNELS, MAX_WAYPOINTS, newPlan,
  type AircraftDef, type Plan, type PlanChannel, type PlanWaypoint, type StoredPlan,
} from './types'

/** The aircraft picker's groups, in order. */
const GROUPS = [...new Set(AIRCRAFT.map(a => a.group))]

/** What an aircraft can export, for the picker. */
function exportsOf(a: AircraftDef): string {
  const t: string[] = targetsFor(a).map(x => (x.tool === 'native' ? 'DCS cartridge' : 'DCS-DTC app'))
  if (a.other) t.push('cartridge file')
  return t.length ? ` · ${t.join(' + ')}` : ' · no DTC'
}

/** Has some way to load a cartridge: DCS's, DCS-DTC, or its own file. */
const hasDtc = (a: AircraftDef) => targetsFor(a).length > 0 || !!a.other

type Side = 'Blue' | 'Red'

/** The dashboard's dev mock (?mock): no engine, canned data. */
const MOCK = import.meta.env.DEV && new URLSearchParams(location.search).has('mock')

interface Props {
  side: Side
  /** Pass `side` to the API: an admin with no in-game side picking one. */
  adminSide: boolean
  /** For the map context and the comms card. Optional: the planner works
   *  without the engine. */
  report?: SituationReport
  /** Navaids for the cartridges' "from the mission" fills. */
  briefing?: Briefing
}

interface Editing {
  /** Absent until first saved. */
  id?: string
  plan: Plan
  /** Ours to save over; false for a teammate's plan (copy it to change it). */
  mine: boolean
  by?: string
  dirty: boolean
}

const fresh = (): Editing => ({ plan: newPlan(), mine: true, dirty: false })

const input: CSSProperties = { padding: '4px 6px', fontSize: '0.72rem', minWidth: 0 }
const label: CSSProperties = {
  display: 'flex', flexDirection: 'column', gap: 3, fontSize: '0.6rem',
  color: 'var(--text-dim)', letterSpacing: '0.06em', textTransform: 'uppercase',
}
const th: CSSProperties = { textAlign: 'left', fontWeight: 700, padding: '3px 4px', color: 'var(--text-dim)', fontSize: '0.58rem', letterSpacing: '0.06em' }
const td: CSSProperties = { padding: '2px 4px', verticalAlign: 'middle' }
const mono: CSSProperties = { fontFamily: 'var(--font-mono)', fontSize: '0.68rem' }

function num(v: string, fallback = 0): number {
  const n = Number(v)
  return Number.isFinite(n) ? n : fallback
}

function Field({ name, children, grow = 1 }: { name: string; children: ReactNode; grow?: number }) {
  return <label style={{ ...label, flex: `${grow} 1 0`, minWidth: 0 }}>{name}{children}</label>
}

/**
 * Advance planning: build a route on the coalition's map, share it with your
 * side, and take it into the jet -- a CombatFlite route for DCS-DTC (any of
 * its aircraft), a native DCS cartridge for the F-16C and F/A-18C, and a
 * kneeboard route card.
 */
export default function PlannerTab({ side, adminSide, report, briefing }: Props) {
  const qc = useQueryClient()
  const apiSide = adminSide ? side : undefined
  const [ed, setEd] = useState<Editing>(fresh)
  const [sel, setSel] = useState<number | null>(null)
  const [msg, setMsg] = useState<{ kind: 'ok' | 'warn' | 'err'; text: string } | null>(null)

  const { data: plans = [], error: listError } = useQuery<StoredPlan[]>({
    queryKey: ['plans', side],
    queryFn: () => api.plans.list(apiSide),
    refetchInterval: 60_000,
  })

  const plan = ed.plan
  const readOnly = !ed.mine
  const def = aircraftDef(plan.aircraft)
  const legList = useMemo(() => legs(plan), [plan])
  const start = parseClock(plan.start_time)
  const totals = legList.reduce((s, l) => ({ d: s.d + l.dist_nm, t: s.t + l.ete_s }), { d: 0, t: 0 })
  const map = plan.waypoints.length ? terrainAt(plan.waypoints[0].lat, plan.waypoints[0].lon) : null
  // The campaign's map, for drawing a route from nothing in the cartridge window.
  const homeMap = useMemo(() => {
    const o = (report?.map ?? []).filter(x => x.lat !== 0 || x.lon !== 0)
    if (!o.length) return null
    return terrainAt(o.reduce((s, x) => s + x.lat, 0) / o.length, o.reduce((s, x) => s + x.lon, 0) / o.length)?.terrain.name ?? null
  }, [report])

  const edit = (f: (p: Plan) => Plan) => setEd(e => ({ ...e, plan: f(e.plan), dirty: true }))
  const editWpt = (i: number, patch: Partial<PlanWaypoint>) =>
    edit(p => ({ ...p, waypoints: p.waypoints.map((w, j) => (j === i ? { ...w, ...patch } : w)) }))

  const addWpt = (lat: number, lon: number, name?: string) => {
    if (plan.waypoints.length >= MAX_WAYPOINTS) return
    // A new point goes after the selected one, so a route can be extended in
    // the middle; with nothing selected it goes on the end.
    const at = sel == null ? plan.waypoints.length : sel + 1
    const prev = plan.waypoints[at - 1]
    const w: PlanWaypoint = {
      name: (name ?? '').toUpperCase().slice(0, 24),
      lat, lon,
      alt_ft: prev?.alt_ft ?? 20000,
      elev_ft: 0,
      target: false,
      note: '',
    }
    edit(p => ({ ...p, waypoints: [...p.waypoints.slice(0, at), w, ...p.waypoints.slice(at)] }))
    setSel(at)
  }

  const moveWpt = (i: number, d: -1 | 1) => {
    const j = i + d
    if (j < 0 || j >= plan.waypoints.length) return
    edit(p => {
      const w = [...p.waypoints]
      ;[w[i], w[j]] = [w[j], w[i]]
      return { ...p, waypoints: w }
    })
    setSel(j)
  }

  const delWpt = (i: number) => {
    edit(p => ({ ...p, waypoints: p.waypoints.filter((_, j) => j !== i) }))
    setSel(null)
  }

  const load = (s: StoredPlan) => {
    if (ed.dirty && !confirm('Discard unsaved changes to this plan?')) return
    setEd({ id: s.id, plan: s.plan, mine: s.mine, by: s.by_name, dirty: false })
    setSel(null)
    setMsg(null)
  }

  const startNew = () => {
    if (ed.dirty && !confirm('Discard unsaved changes to this plan?')) return
    setEd(fresh())
    setSel(null)
    setMsg(null)
  }

  const copy = () => {
    setEd(e => ({ plan: { ...e.plan, name: `${e.plan.name} (copy)`.slice(0, 60) }, mine: true, dirty: true }))
    setMsg({ kind: 'ok', text: 'Copied. Save to keep your own version.' })
  }

  const save = useMutation({
    mutationFn: () => api.plans.save({ id: ed.id, plan }, apiSide),
    onSuccess: s => {
      setEd({ id: s.id, plan: s.plan, mine: s.mine, by: s.by_name, dirty: false })
      setMsg({ kind: 'ok', text: `Saved and shared with ${side}.` })
      qc.invalidateQueries({ queryKey: ['plans', side] })
    },
    onError: (e: Error) => setMsg({ kind: 'err', text: e.message }),
  })

  const remove = useMutation({
    mutationFn: (id: string) => api.plans.del(id, apiSide),
    onSuccess: (_, id) => {
      if (ed.id === id) setEd(fresh())
      setMsg({ kind: 'ok', text: 'Plan deleted.' })
      qc.invalidateQueries({ queryKey: ['plans', side] })
    },
    onError: (e: Error) => setMsg({ kind: 'err', text: e.message }),
  })

  const canSave = !readOnly && plan.name.trim() !== '' && plan.waypoints.length > 0 && !save.isPending

  // ── Exports ────────────────────────────────────────────────────────────
  const exportDcsDtc = () => {
    download(`${safeName(plan.name)}.xml`, combatFliteXml(plan), 'application/xml')
    setMsg({ kind: 'ok', text: 'In DCS-DTC: open the aircraft, Waypoints, Import, "From CombatFlite XML Export...", pick this file.' })
  }

  /** Mission clock at waypoint i, for timed cartridges and presets. */
  const tosSeconds = (i: number) => (start == null ? null : start + (i > 0 ? legList[i - 1]?.elapsed_s ?? 0 : 0))
  const tosText = (i: number) => {
    const s = tosSeconds(i)
    return s == null ? null : fmtClock(s)
  }

  const exportCard = async () => {
    try {
      download(`${safeName(plan.name)}.png`, await routeCardPng(plan), 'image/png')
      setMsg({ kind: 'ok', text: `Route card saved. For the in-game kneeboard put it in Saved Games\\DCS\\Kneeboard\\${plan.aircraft}.` })
    } catch (e) {
      setMsg({ kind: 'err', text: String(e) })
    }
  }

  // ── Radios ─────────────────────────────────────────────────────────────
  const setChannels = (radio: 'comm1' | 'comm2', list: PlanChannel[]) =>
    edit(p => ({ ...p, [radio]: [...list].sort((a, b) => a.preset - b.preset) }))

  /** UHF presets from the comms card on COMM1, VHF on COMM2 -- the F-16's
   *  split, and harmless on the Hornet's two ARC-210s. */
  const fillFromComms = () => {
    const { comm1, comm2 } = commsFromCard(report?.comms ?? [])
    if (!comm1.length && !comm2.length) {
      setMsg({ kind: 'warn', text: 'The comms card has no numbered presets to copy.' })
      return
    }
    edit(p => ({ ...p, comm1, comm2, include_comms: true }))
  }

  // What the campaign knows that a cartridge can take.
  const mission: MissionData | null = useMemo(() => (report || briefing ? {
    side,
    comms: report?.comms ?? [],
    threats: report?.threats ?? [],
    objectives: report?.map ?? [],
    navaids: briefing?.navaids ?? [],
    report: report ?? null,
    tac: MOCK ? mockTacPicture(side) : undefined,
  } : null), [report, briefing, side])

  const radioEditor = (radio: 'comm1' | 'comm2', title: string) => {
    const list = plan[radio]
    const free = Array.from({ length: MAX_CHANNELS }, (_, i) => i + 1).find(n => !list.some(c => c.preset === n))
    const upd = (i: number, patch: Partial<PlanChannel>) => setChannels(radio, list.map((c, j) => (j === i ? { ...c, ...patch } : c)))
    return (
      <div style={{ flex: '1 1 300px', minWidth: 0 }}>
        <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', marginBottom: 4 }}>
          <span style={{ ...label, flexDirection: 'row' }}>{title}</span>
          {!readOnly && free && (
            <button type="button" className="vs-btn-ghost" style={{ ...input, display: 'flex', gap: 4, alignItems: 'center' }}
              onClick={() => setChannels(radio, [...list, { preset: free, freq_mhz: radio === 'comm1' ? 251 : 124, label: '', modulation: 'AM' }])}>
              <Plus size={11} /> preset
            </button>
          )}
        </div>
        {list.length === 0 && <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>Mission presets</div>}
        {list.map((c, i) => (
          <div key={c.preset} style={{ display: 'flex', gap: 4, marginBottom: 3, alignItems: 'center' }}>
            <select className="vs-input" style={{ ...input, width: 48 }} disabled={readOnly} value={c.preset}
              aria-label={`${title} preset number`}
              onChange={e => {
                const p = num(e.target.value, c.preset)
                if (!list.some(x => x.preset === p)) upd(i, { preset: p })
              }}>
              {Array.from({ length: MAX_CHANNELS }, (_, n) => n + 1).map(n => <option key={n} value={n}>{n}</option>)}
            </select>
            <input className="vs-input" style={{ ...input, ...mono, width: 72 }} disabled={readOnly} type="number" step="0.025"
              aria-label={`${title} preset ${c.preset} MHz`} value={c.freq_mhz}
              onChange={e => upd(i, { freq_mhz: num(e.target.value, c.freq_mhz) })} />
            <select className="vs-input" style={{ ...input, width: 52 }} disabled={readOnly} value={c.modulation}
              aria-label={`${title} preset ${c.preset} modulation`}
              onChange={e => upd(i, { modulation: e.target.value === 'FM' ? 'FM' : 'AM' })}>
              <option>AM</option><option>FM</option>
            </select>
            <input className="vs-input" style={{ ...input, flex: 1 }} disabled={readOnly} maxLength={24} placeholder="label"
              aria-label={`${title} preset ${c.preset} label`} value={c.label}
              onChange={e => upd(i, { label: e.target.value })} />
            {!readOnly && (
              <button type="button" className="vs-btn-ghost" style={{ ...input, padding: 3 }} title="Remove preset"
                onClick={() => setChannels(radio, list.filter((_, j) => j !== i))}><X size={11} /></button>
            )}
          </div>
        ))}
      </div>
    )
  }

  const msgColor = msg?.kind === 'err' ? '#f04747' : msg?.kind === 'warn' ? '#f0a030' : 'var(--accent)'

  return (
    <div style={{ display: 'flex', flexWrap: 'wrap', gap: 12, alignItems: 'stretch', minHeight: 0 }}>
      {/* ── Map ─────────────────────────────────────────────────────── */}
      <div style={{ flex: '2 1 480px', minHeight: 460, height: 'calc(100vh - 210px)', maxHeight: 900, position: 'relative', border: '1px solid var(--border)', borderRadius: 6, overflow: 'hidden' }}>
        <PlannerMap
          waypoints={plan.waypoints}
          selected={sel}
          report={report}
          readOnly={readOnly}
          onAdd={addWpt}
          onMove={(i, lat, lon) => editWpt(i, { lat, lon })}
          onSelect={setSel}
        />
        <div style={{
          position: 'absolute', bottom: 8, left: 8, padding: '6px 9px', borderRadius: 4,
          background: 'var(--bg-card)', border: '1px solid var(--border)',
          fontSize: '0.6rem', color: 'var(--text-dim)', lineHeight: 1.7, pointerEvents: 'none',
        }}>
          {readOnly
            ? <div>{ed.by ? `${ed.by}'s plan` : 'Read only'} · copy it to make changes</div>
            : <div>click map: add waypoint{sel != null ? ` after ${sel + 1}` : ''} · click objective: add on it · drag to move</div>}
          <div><span style={{ color: '#facc15' }}>●</span> route · <span style={{ color: '#f04747' }}>■</span> target · <span style={{ color: '#f04747' }}>◌</span> known SAM ring</div>
        </div>
      </div>

      {/* ── Editor ──────────────────────────────────────────────────── */}
      <div style={{ flex: '1 1 380px', maxWidth: 620, minWidth: 0, display: 'flex', flexDirection: 'column', gap: 12 }}>
        <Panel
          title={ed.id ? (readOnly ? 'Plan (read only)' : 'Plan') : 'New plan'}
          size="sm"
          right={
            <div style={{ display: 'flex', gap: 6 }}>
              <button type="button" className="vs-btn-ghost" style={{ ...input, display: 'flex', gap: 4, alignItems: 'center' }} onClick={startNew}>
                <Plus size={11} /> New
              </button>
              {ed.id && (
                <button type="button" className="vs-btn-ghost" style={input} onClick={copy}>Copy</button>
              )}
              {!readOnly && (
                <button type="button" className="vs-btn" disabled={!canSave} onClick={() => save.mutate()}
                  title={canSave ? `Save and share with ${side}` : 'Name the plan and add a waypoint first'}
                  style={{ ...input, display: 'flex', gap: 4, alignItems: 'center' }}>
                  <Save size={11} /> {save.isPending ? 'Saving…' : ed.dirty || !ed.id ? 'Save' : 'Saved'}
                </button>
              )}
            </div>
          }
        >
          <div style={{ display: 'flex', flexDirection: 'column', gap: 8, padding: 10 }}>
            <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap' }}>
              <Field name="Name" grow={2}>
                <input className="vs-input" style={input} disabled={readOnly} maxLength={60} placeholder="e.g. SEAD Kutaisi"
                  value={plan.name} onChange={e => edit(p => ({ ...p, name: e.target.value }))} />
              </Field>
              <Field name="Callsign">
                <input className="vs-input" style={input} disabled={readOnly} maxLength={24} placeholder="Enfield 1"
                  value={plan.callsign} onChange={e => edit(p => ({ ...p, callsign: e.target.value }))} />
              </Field>
            </div>
            <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap' }}>
              <Field name="Aircraft" grow={2}>
                <select className="vs-input" style={input} disabled={readOnly} value={plan.aircraft}
                  onChange={e => edit(p => ({ ...p, aircraft: e.target.value }))}>
                  {GROUPS.map(g => (
                    <optgroup key={g} label={g}>
                      {AIRCRAFT.filter(a => a.group === g).map(a => (
                        // Kept in the list so pilots can see why, but not pickable.
                        <option key={a.id} value={a.id} disabled={!hasDtc(a)}>{a.label}{exportsOf(a)}</option>
                      ))}
                    </optgroup>
                  ))}
                </select>
              </Field>
              <Field name="Speed kt GS">
                <input className="vs-input" style={{ ...input, ...mono }} disabled={readOnly} type="number" min={40} max={1500}
                  value={plan.speed_kts} onChange={e => edit(p => ({ ...p, speed_kts: num(e.target.value, p.speed_kts) }))} />
              </Field>
              <Field name="Start (mission)">
                <input className="vs-input" style={{ ...input, ...mono }} disabled={readOnly} placeholder="HH:MM"
                  value={plan.start_time ?? ''} onChange={e => edit(p => ({ ...p, start_time: e.target.value || null }))} />
              </Field>
            </div>
            {plan.start_time && start == null && (
              <div style={{ fontSize: '0.64rem', color: '#f0a030' }}>Start time is HH:MM or HH:MM:SS.</div>
            )}
          </div>
        </Panel>

        <Panel
          title="Waypoints"
          size="sm"
          count={`${plan.waypoints.length} · ${totals.d.toFixed(0)} nm · ${fmtDuration(totals.t)}${map ? ` · ${map.terrain.name}` : ''}`}
          bodyStyle={{ overflowX: 'auto' }}
        >
          {plan.waypoints.length === 0 ? (
            <div style={{ padding: 12, fontSize: '0.7rem', color: 'var(--text-dim)' }}>
              {readOnly ? 'This plan has no waypoints.' : 'Click the map to lay down the route, starting from your field.'}
            </div>
          ) : (
            <table style={{ width: '100%', borderCollapse: 'collapse', minWidth: 440 }}>
              <thead>
                <tr>
                  <th style={th}>#</th><th style={th}>NAME</th><th style={th} title="Planned altitude, feet MSL">ALT</th>
                  <th style={th} title="Steerpoint elevation for the jet, feet MSL. Use the ground elevation for a target.">ELEV</th>
                  <th style={th} title="Target point">TGT</th><th style={th}>CRS°M</th><th style={th}>NM</th>
                  <th style={th}>{start != null ? 'ETA' : 'ETE'}</th><th style={th} />
                </tr>
              </thead>
              <tbody>
                {plan.waypoints.map((w, i) => {
                  const leg = i > 0 ? legList[i - 1] : null
                  return (
                    <tr key={i} onClick={() => setSel(i)}
                      style={{ background: sel === i ? 'var(--accent-glow, rgba(106,171,31,0.12))' : undefined, cursor: 'pointer' }}>
                      <td style={{ ...td, ...mono, fontWeight: 800 }}>{i + 1}</td>
                      <td style={td}>
                        <input className="vs-input" style={{ ...input, width: '100%' }} disabled={readOnly} maxLength={24}
                          aria-label={`Waypoint ${i + 1} name`} placeholder={`WP${i + 1}`} value={w.name}
                          onChange={e => editWpt(i, { name: e.target.value.toUpperCase() })} />
                      </td>
                      <td style={td}>
                        <input className="vs-input" style={{ ...input, ...mono, width: 60 }} disabled={readOnly} type="number" step={500}
                          aria-label={`Waypoint ${i + 1} altitude`} value={w.alt_ft}
                          onChange={e => editWpt(i, { alt_ft: num(e.target.value, w.alt_ft) })} />
                      </td>
                      <td style={td}>
                        <input className="vs-input" style={{ ...input, ...mono, width: 58, borderColor: w.target && w.elev_ft === 0 ? '#f0a030' : undefined }}
                          disabled={readOnly} type="number" aria-label={`Waypoint ${i + 1} elevation`} value={w.elev_ft}
                          onChange={e => editWpt(i, { elev_ft: num(e.target.value, w.elev_ft) })} />
                      </td>
                      <td style={td}>
                        <input type="checkbox" disabled={readOnly} checked={w.target} aria-label={`Waypoint ${i + 1} is a target`}
                          onChange={e => editWpt(i, { target: e.target.checked })} />
                      </td>
                      <td style={{ ...td, ...mono }}>{leg ? String(Math.round(leg.course_mag)).padStart(3, '0') : ''}</td>
                      <td style={{ ...td, ...mono }}>{leg ? leg.dist_nm.toFixed(1) : ''}</td>
                      <td style={{ ...td, ...mono }}>
                        {start != null ? fmtClock(start + (leg?.elapsed_s ?? 0)) : leg ? fmtDuration(leg.ete_s) : ''}
                      </td>
                      <td style={{ ...td, whiteSpace: 'nowrap' }}>
                        {!readOnly && (
                          <>
                            <button type="button" className="vs-btn-ghost" style={{ padding: 2 }} title="Move up" disabled={i === 0}
                              onClick={e => { e.stopPropagation(); moveWpt(i, -1) }}><ArrowUp size={11} /></button>
                            <button type="button" className="vs-btn-ghost" style={{ padding: 2 }} title="Move down" disabled={i === plan.waypoints.length - 1}
                              onClick={e => { e.stopPropagation(); moveWpt(i, 1) }}><ArrowDown size={11} /></button>
                            <button type="button" className="vs-btn-ghost" style={{ padding: 2 }} title="Delete waypoint"
                              onClick={e => { e.stopPropagation(); delWpt(i) }}><Trash size={11} /></button>
                          </>
                        )}
                      </td>
                    </tr>
                  )
                })}
              </tbody>
            </table>
          )}
        </Panel>

        <Panel
          title="Radios"
          size="sm"
          right={!readOnly && report?.comms.some(c => c.preset) ? (
            <button type="button" className="vs-btn-ghost" style={input} onClick={fillFromComms}>From comms card</button>
          ) : undefined}
        >
          <div style={{ padding: 10, display: 'flex', flexDirection: 'column', gap: 8 }}>
            <label style={{ display: 'flex', gap: 6, alignItems: 'center', fontSize: '0.68rem' }}>
              <input type="checkbox" disabled={readOnly} checked={plan.include_comms}
                onChange={e => edit(p => ({ ...p, include_comms: e.target.checked }))} />
              Write these presets into the native cartridge (otherwise the jet keeps the mission's)
            </label>
            <div style={{ display: 'flex', gap: 12, flexWrap: 'wrap' }}>
              {radioEditor('comm1', def.native === 'F-16C_50' ? 'COMM1 UHF' : 'COMM1')}
              {radioEditor('comm2', def.native === 'F-16C_50' ? 'COMM2 VHF' : 'COMM2')}
            </div>
            <Field name="Notes (route card)">
              <textarea className="vs-input" style={{ ...input, minHeight: 50, resize: 'vertical' }} disabled={readOnly} maxLength={2000}
                value={plan.notes} onChange={e => edit(p => ({ ...p, notes: e.target.value }))} />
            </Field>
          </div>
        </Panel>

        <DtcCard
          plan={plan}
          readOnly={readOnly}
          onSystems={(key, next) => edit(p => {
            const systems = { ...(p.systems ?? {}) }
            if (Object.keys(next).length) systems[key] = next
            else delete systems[key]
            return { ...p, systems }
          })}
          tosSeconds={tosSeconds}
          tosText={tosText}
          side={side}
          onWaypoints={w => edit(p => ({ ...p, waypoints: w }))}
          onComms={(comm1, comm2) => edit(p => ({ ...p, comm1, comm2, include_comms: true }))}
          homeMap={homeMap}
          mission={mission}
        />

        <Panel title="Route & kneeboard" size="sm">
          <div style={{ padding: 10, display: 'flex', flexDirection: 'column', gap: 8 }}>
            <div style={{ display: 'flex', gap: 6, flexWrap: 'wrap' }}>
              <button type="button" className="vs-btn-outline" disabled={!plan.waypoints.length} onClick={exportCard}
                style={{ ...input, display: 'flex', gap: 5, alignItems: 'center' }}>
                <Download size={11} /> Kneeboard card
              </button>
              <button type="button" className="vs-btn-outline" disabled={!plan.waypoints.length} onClick={exportDcsDtc}
                title="CombatFlite XML route: DCS-DTC's waypoint page imports it for any aircraft, and other planners read it"
                style={{ ...input, display: 'flex', gap: 5, alignItems: 'center' }}>
                <Download size={11} /> Route XML (CombatFlite)
              </button>
            </div>
            <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
              The card and route work for every aircraft. Cartridges load nav points only on the map they were planned on
              {map ? ` (${map.terrain.name})` : ''}.
            </div>
            {msg && <div role="status" style={{ fontSize: '0.68rem', color: msgColor, lineHeight: 1.5 }}>{msg.text}</div>}
          </div>
        </Panel>

        <Panel title={`${side} plans`} size="sm" count={plans.length} bodyStyle={{ maxHeight: 260, overflowY: 'auto' }}>
          {listError && <div style={{ padding: 10, fontSize: '0.68rem', color: '#f04747' }}>{(listError as Error).message}</div>}
          {!listError && plans.length === 0 && (
            <div style={{ padding: 10, fontSize: '0.68rem', color: 'var(--text-dim)' }}>No shared plans yet. Saved plans show here for your whole coalition.</div>
          )}
          {plans.map(s => (
            <div key={s.id} style={{
              display: 'flex', alignItems: 'center', gap: 8, padding: '6px 10px', borderTop: '1px solid var(--border)',
              background: s.id === ed.id ? 'var(--bg-elevated)' : undefined,
            }}>
              <button type="button" onClick={() => load(s)} style={{ flex: 1, minWidth: 0, textAlign: 'left', background: 'none', border: 'none', color: 'inherit', cursor: 'pointer', padding: 0 }}>
                <div style={{ fontSize: '0.74rem', fontWeight: 700, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{s.plan.name}</div>
                <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
                  {aircraftDef(s.plan.aircraft).label} · {s.plan.waypoints.length} wpts · {s.by_name} · {new Date(s.updated).toLocaleString()}
                </div>
              </button>
              {s.mine && (
                <button type="button" className="vs-btn-ghost" style={{ padding: 3 }} title="Delete plan"
                  onClick={() => { if (confirm(`Delete "${s.plan.name}"?`)) remove.mutate(s.id) }}><Trash size={12} /></button>
              )}
            </div>
          ))}
        </Panel>
      </div>
    </div>
  )
}
