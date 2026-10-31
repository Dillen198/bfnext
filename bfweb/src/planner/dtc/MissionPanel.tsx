// "From the mission": what the campaign already knows, offered per
// cartridge with what it writes and what it replaces.

import type { CSSProperties } from 'react'
import { Check, Download } from '@icons'
import type { MissionFill } from './mission'

const card: CSSProperties = {
  border: '1px solid var(--border)', borderRadius: 6, padding: '10px 12px', display: 'flex', gap: 12,
  alignItems: 'flex-start', background: 'var(--bg-elevated)',
}

export default function MissionPanel({ fills, loaded, readOnly, onFill, onFillAll }: {
  fills: MissionFill[]
  /** The mission data has arrived (the briefing and situation). */
  loaded: boolean
  readOnly: boolean
  onFill: (f: MissionFill) => void
  onFillAll: () => void
}) {
  const todo = fills.filter(f => !f.done)
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 10, maxWidth: 760 }}>
      <div>
        <h3 style={{ margin: 0, fontSize: '0.95rem', fontWeight: 700 }}>From the mission</h3>
        <p style={{ fontSize: '0.7rem', color: 'var(--text-dim)', lineHeight: 1.6, margin: '4px 0 0' }}>
          What the campaign already knows, put straight into this cartridge: no typing frequencies or coordinates.
          Each fill replaces that page&apos;s list; everything else stays as you set it.
        </p>
      </div>
      {!loaded && <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)' }}>Loading the mission's comms card and intel…</div>}
      {loaded && fills.length === 0 && (
        <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
          Nothing from the mission fits this cartridge. The comms card fills radio presets; known threats fill the F-16&apos;s
          and Hornet&apos;s threat rings; your airfields fill F-16 destination points; NDBs fill Apache ADF presets.
        </div>
      )}
      {fills.length > 1 && todo.length > 1 && !readOnly && (
        <button type="button" className="vs-btn" onClick={onFillAll}
          style={{ alignSelf: 'flex-start', display: 'inline-flex', gap: 6, alignItems: 'center', padding: '6px 12px', fontSize: '0.72rem' }}>
          <Download size={12} /> Fill all {todo.length}
        </button>
      )}
      {fills.map(f => (
        <div key={f.id} style={card}>
          <div style={{ flex: 1, minWidth: 0 }}>
            <div style={{ fontSize: '0.76rem', fontWeight: 700, display: 'flex', gap: 8, alignItems: 'center' }}>
              {f.title}
              {f.done && <span style={{ color: 'var(--accent)', fontSize: '0.64rem', display: 'inline-flex', gap: 3, alignItems: 'center' }}><Check size={11} /> in the cartridge</span>}
            </div>
            <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', lineHeight: 1.55, marginTop: 2 }}>{f.detail}</div>
            {!f.done && f.replaces > 0 && (
              <div style={{ fontSize: '0.64rem', color: '#f0a030', marginTop: 3 }}>Replaces the {f.replaces} entr{f.replaces > 1 ? 'ies' : 'y'} there now.</div>
            )}
          </div>
          {!readOnly && (
            <button type="button" className={f.done ? 'vs-btn-ghost' : 'vs-btn-outline'} onClick={() => onFill(f)}
              style={{ flex: '0 0 auto', padding: '5px 12px', fontSize: '0.7rem' }}>
              {f.done ? 'Fill again' : 'Fill'}
            </button>
          )}
        </div>
      ))}
    </div>
  )
}
