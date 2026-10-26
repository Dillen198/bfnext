import type { CSSProperties } from 'react'
import type { PlanOffset, PlanOffsetPair, PlanWaypoint } from '../types'

/** What a target can carry beyond the route itself. */
export interface OffsetSupport {
  /** Offset aimpoints per steerpoint. */
  oa: number
  /** VIP and VRP (F-16). */
  vipvrp: boolean
}

/** Which cartridges take offsets: the F-16 (native and DCS-DTC) and the
 *  native Hornet. */
export function offsetSupport(code: string): OffsetSupport | null {
  if (code === 'F-16C_50' || code === 'F16C') return { oa: 2, vipvrp: true }
  if (code === 'FA-18C_hornet') return { oa: 1, vipvrp: false }
  return null
}

const FT_PER_NM = 6076.12
const inp: CSSProperties = { padding: '3px 5px', fontSize: '0.7rem', width: 66, fontFamily: 'var(--font-mono)' }
const th: CSSProperties = { textAlign: 'left', fontSize: '0.6rem', fontWeight: 600, color: 'var(--text-dim)', padding: '4px 6px', borderBottom: '1px solid var(--border)', whiteSpace: 'nowrap' }
const td: CSSProperties = { padding: '3px 6px', borderBottom: '1px solid var(--border)', verticalAlign: 'middle', fontSize: '0.7rem' }

const num = (v: string, d: number) => {
  const n = Number(v)
  return v !== '' && Number.isFinite(n) ? n : d
}

/** Range (in the unit shown), bearing and elevation of one offset. */
function OffsetInputs({ o, nm, ro, onChange, label }: { o: PlanOffset; nm?: boolean; ro: boolean; onChange: (o: PlanOffset) => void; label: string }) {
  const range = nm ? Math.round((o.range_ft / FT_PER_NM) * 10) / 10 : o.range_ft
  return (
    <span style={{ display: 'inline-flex', gap: 4, alignItems: 'center', flexWrap: 'wrap' }}>
      <input className="vs-input" style={inp} type="number" min={0} step={nm ? 0.1 : 100} value={range} disabled={ro}
        aria-label={`${label} range ${nm ? 'NM' : 'ft'}`} title={`Range, ${nm ? 'NM' : 'feet'}`}
        onChange={e => onChange({ ...o, range_ft: nm ? num(e.target.value, range) * FT_PER_NM : num(e.target.value, range) })} />
      <span style={{ color: 'var(--text-dim)', fontSize: '0.6rem' }}>{nm ? 'NM' : 'ft'}</span>
      <input className="vs-input" style={{ ...inp, width: 56 }} type="number" min={0} max={359.9} step={1} value={o.bearing_deg} disabled={ro}
        aria-label={`${label} bearing`} title="Bearing, degrees true"
        onChange={e => onChange({ ...o, bearing_deg: ((num(e.target.value, o.bearing_deg) % 360) + 360) % 360 })} />
      <span style={{ color: 'var(--text-dim)', fontSize: '0.6rem' }}>°T</span>
      <input className="vs-input" style={inp} type="number" step={10} value={o.elev_ft} disabled={ro}
        aria-label={`${label} elevation`} title="Elevation, feet MSL (the ground at that point)"
        onChange={e => onChange({ ...o, elev_ft: num(e.target.value, o.elev_ft) })} />
      <span style={{ color: 'var(--text-dim)', fontSize: '0.6rem' }}>ft</span>
    </span>
  )
}

/**
 * Offset aimpoints and VIP/VRP on the route's steerpoints. They belong to the
 * plan's waypoints, so the native cartridge and the DCS-DTC preset both get
 * them.
 */
export default function OffsetsEditor({ waypoints, support, readOnly, onChange }: {
  waypoints: PlanWaypoint[]; support: OffsetSupport; readOnly: boolean; onChange: (w: PlanWaypoint[]) => void
}) {
  const set = (i: number, patch: Partial<PlanWaypoint>) => onChange(waypoints.map((w, j) => (j === i ? { ...w, ...patch } : w)))
  const setOa = (i: number, k: number, o: PlanOffset | null) => {
    const oa = [...(waypoints[i].oa ?? [])]
    if (o) oa[k] = o
    else oa.splice(k, 1)
    set(i, { oa: oa.filter(Boolean) })
  }
  const fresh = (w: PlanWaypoint, range_ft: number): PlanOffset => ({ range_ft, bearing_deg: 0, elev_ft: w.elev_ft })
  const vipAt = waypoints.findIndex(w => w.vip)
  const vrpAt = waypoints.findIndex(w => w.vrp)
  /** Make `i` the only VIP (or VRP), or none. */
  const pick = (kind: 'vip' | 'vrp', i: number | null) => onChange(waypoints.map((w, j) => {
    if (j === i) {
      const pair: PlanOffsetPair = w[kind] ?? { point: fresh(w, kind === 'vip' ? 5 * FT_PER_NM : 3000), pup: fresh(w, kind === 'vip' ? 3 * FT_PER_NM : 6000) }
      return { ...w, [kind]: pair }
    }
    const { [kind]: _drop, ...rest } = w
    void _drop
    return rest
  }))

  if (!waypoints.length) return null
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 12 }}>
      <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
        Offset aimpoints (OA) mark a point at a range and bearing from a steerpoint, for radar and targeting pod cues.
        {support.vipvrp && ' VIP and VRP set up the F-16\'s visual pop-up attack: a VIP steerpoint before the target, or the target steerpoint with a reference point beside it.'}
      </div>
      <div style={{ overflowX: 'auto', border: '1px solid var(--border)', borderRadius: 4 }}>
        <table style={{ borderCollapse: 'collapse', width: '100%' }}>
          <thead>
            <tr>
              <th style={th}>Steerpoint</th>
              {Array.from({ length: support.oa }, (_, k) => <th key={k} style={th}>{support.oa > 1 ? `OA${k + 1}` : 'Offset aimpoint'}</th>)}
              {support.vipvrp && <th style={th}>VIP</th>}
              {support.vipvrp && <th style={th}>VRP target</th>}
            </tr>
          </thead>
          <tbody>
            {waypoints.map((w, i) => (
              <tr key={i}>
                <td style={{ ...td, fontWeight: 600, whiteSpace: 'nowrap' }}>{i + 1} {w.name || `WP${i + 1}`}{w.target ? ' ■' : ''}</td>
                {Array.from({ length: support.oa }, (_, k) => {
                  const o = w.oa?.[k]
                  // OA2 only once OA1 is set.
                  const can = k === 0 || !!w.oa?.[0]
                  return (
                    <td key={k} style={td}>
                      <span style={{ display: 'inline-flex', gap: 6, alignItems: 'center' }}>
                        <input type="checkbox" checked={!!o} disabled={readOnly || !can} aria-label={`OA${k + 1} for steerpoint ${i + 1}`}
                          onChange={e => setOa(i, k, e.target.checked ? fresh(w, 3000) : null)} />
                        {o && <OffsetInputs o={o} ro={readOnly} label={`OA${k + 1}`} onChange={v => setOa(i, k, v)} />}
                      </span>
                    </td>
                  )
                })}
                {support.vipvrp && (
                  <td style={{ ...td, textAlign: 'center' }}>
                    <input type="radio" name="vip" checked={vipAt === i} disabled={readOnly} aria-label={`Steerpoint ${i + 1} is the VIP`} onChange={() => pick('vip', i)} />
                  </td>
                )}
                {support.vipvrp && (
                  <td style={{ ...td, textAlign: 'center' }}>
                    <input type="radio" name="vrp" checked={vrpAt === i} disabled={readOnly} aria-label={`Steerpoint ${i + 1} is the VRP target`} onChange={() => pick('vrp', i)} />
                  </td>
                )}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      {support.vipvrp && (vipAt >= 0 || vrpAt >= 0) && (
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(320px, 1fr))', gap: 10 }}>
          {vipAt >= 0 && (
            <Pair title={`VIP: steerpoint ${vipAt + 1}`} nm pair={waypoints[vipAt].vip!} ro={readOnly}
              labels={['To the target', 'To the pull-up point']} onChange={p => set(vipAt, { vip: p })} onClear={() => pick('vip', null)} />
          )}
          {vrpAt >= 0 && (
            <Pair title={`VRP: target steerpoint ${vrpAt + 1}`} pair={waypoints[vrpAt].vrp!} ro={readOnly}
              labels={['To the reference point', 'To the pull-up point']} onChange={p => set(vrpAt, { vrp: p })} onClear={() => pick('vrp', null)} />
          )}
        </div>
      )}
    </div>
  )
}

function Pair({ title, pair, nm, ro, labels, onChange, onClear }: {
  title: string; pair: PlanOffsetPair; nm?: boolean; ro: boolean; labels: [string, string]
  onChange: (p: PlanOffsetPair) => void; onClear: () => void
}) {
  return (
    <div style={{ border: '1px solid var(--border)', borderRadius: 6, padding: 10, background: 'var(--bg-elevated)' }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 6 }}>
        <b style={{ fontSize: '0.74rem' }}>{title}</b>
        {!ro && <button type="button" className="vs-btn-ghost" style={{ fontSize: '0.62rem', padding: '1px 6px' }} onClick={onClear}>Remove</button>}
      </div>
      {(['point', 'pup'] as const).map((k, j) => (
        <div key={k} style={{ display: 'flex', gap: 8, alignItems: 'center', flexWrap: 'wrap', padding: '3px 0' }}>
          <span style={{ fontSize: '0.66rem', color: 'var(--text-dim)', minWidth: 120 }}>{labels[j]}</span>
          <OffsetInputs o={pair[k]} nm={nm} ro={ro} label={labels[j]} onChange={o => onChange({ ...pair, [k]: o })} />
        </div>
      ))}
    </div>
  )
}
