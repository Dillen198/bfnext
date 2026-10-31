// What was picked on the cartridge map: its coordinates in every format a
// pilot types, and where it can go in the plan or the cartridge.

import { useState, type CSSProperties } from 'react'
import { X } from '@icons'
import type { DcsTerrain } from '../nav'
import { coordFormats, type Pick, type SendTarget } from './picks'

const panel: CSSProperties = {
  position: 'absolute', right: 8, bottom: 8, width: 'min(340px, calc(100% - 16px))', maxHeight: 'calc(100% - 70px)', overflowY: 'auto',
  background: 'linear-gradient(var(--bg-card), var(--bg-card)), var(--bg)', border: '1px solid var(--border)', borderRadius: 6,
  padding: 10, fontSize: '0.68rem', boxShadow: '0 6px 20px rgba(0,0,0,0.45)', zIndex: 3,
}
const btn: CSSProperties = { padding: '2px 6px', fontSize: '0.62rem' }

export default function PickCard({ picks, map, targets, readOnly, onSend, onRemove, onClose }: {
  picks: Pick[]
  map: DcsTerrain | null
  targets: SendTarget[]
  readOnly: boolean
  onSend: (t: SendTarget) => string
  onRemove: (key: string) => void
  onClose: () => void
}) {
  const [to, setTo] = useState(targets[0]?.key ?? 'route')
  const [msg, setMsg] = useState<string | null>(null)
  const [copied, setCopied] = useState<string | null>(null)
  const target = targets.find(t => t.key === to) ?? targets[0]
  const one = picks.length === 1 ? picks[0] : null
  const copy = (v: string) => {
    navigator.clipboard?.writeText(v).then(() => { setCopied(v); setTimeout(() => setCopied(null), 1200) }).catch(() => undefined)
  }

  return (
    <div style={panel} role="dialog" aria-label={one ? one.name : `${picks.length} picked`}>
      <div style={{ display: 'flex', alignItems: 'flex-start', gap: 6, marginBottom: 6 }}>
        <div style={{ flex: 1, minWidth: 0 }}>
          <div style={{ fontWeight: 800, fontSize: '0.76rem' }}>{one ? one.name : `${picks.length} picked in the area`}</div>
          {one && <div style={{ color: 'var(--text-dim)', lineHeight: 1.45 }}>{one.detail}</div>}
        </div>
        <button type="button" className="vs-btn-ghost" style={{ padding: 3 }} onClick={onClose} aria-label="Close"><X size={12} /></button>
      </div>

      {one ? (
        <table style={{ width: '100%', borderCollapse: 'collapse', marginBottom: 8 }}>
          <tbody>
            {coordFormats(one.lat, one.lon, map).map(c => (
              <tr key={c.label}>
                <td style={{ color: 'var(--text-dim)', padding: '2px 6px 2px 0', whiteSpace: 'nowrap' }}>{c.label}</td>
                <td style={{ fontFamily: 'var(--font-mono)', padding: '2px 0', wordBreak: 'break-all' }}>{c.value}</td>
                <td style={{ textAlign: 'right' }}>
                  <button type="button" className="vs-btn-ghost" style={btn} onClick={() => copy(c.value)}>{copied === c.value ? 'Copied' : 'Copy'}</button>
                </td>
              </tr>
            ))}
            {one.uncertainty_m != null && one.uncertainty_m > 300 && (
              <tr><td colSpan={3} style={{ color: '#f0a030', paddingTop: 3 }}>Position may be off by {Math.round(one.uncertainty_m)} m: check it before you drop on it.</td></tr>
            )}
          </tbody>
        </table>
      ) : (
        <div style={{ maxHeight: 160, overflowY: 'auto', marginBottom: 8, border: '1px solid var(--border)', borderRadius: 4 }}>
          {picks.map(p => (
            <div key={p.key} style={{ display: 'flex', gap: 6, alignItems: 'center', padding: '3px 6px', borderBottom: '1px solid var(--border)' }}>
              <span style={{ flex: 1, minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }} title={p.detail}>{p.name}</span>
              <button type="button" className="vs-btn-ghost" style={btn} onClick={() => onRemove(p.key)} aria-label={`Leave out ${p.name}`}><X size={10} /></button>
            </div>
          ))}
        </div>
      )}

      {!readOnly && target && (
        <div style={{ display: 'flex', flexDirection: 'column', gap: 5 }}>
          <label style={{ display: 'flex', flexDirection: 'column', gap: 3, color: 'var(--text-dim)' }}>
            Send {one ? 'it' : `all ${picks.length}`} to
            <select className="vs-input" value={target.key} onChange={e => { setTo(e.target.value); setMsg(null) }} style={{ fontSize: '0.68rem', padding: '3px 4px' }}>
              {targets.map(t => <option key={t.key} value={t.key} disabled={t.room <= 0}>{t.label}{t.room <= 0 ? ' (full)' : t.room < 99 ? ` (${t.room} free)` : ''}</option>)}
            </select>
          </label>
          <button type="button" className="vs-btn" disabled={target.room <= 0} onClick={() => setMsg(onSend(target))}
            style={{ padding: '5px 10px', fontSize: '0.7rem', alignSelf: 'flex-start' }}>
            Send
          </button>
          {msg && <div role="status" style={{ color: 'var(--accent)', lineHeight: 1.45 }}>{msg}</div>}
        </div>
      )}
    </div>
  )
}
