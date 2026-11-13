import { useState, type ReactElement } from 'react'
import { api } from '../api'
import { useAuth } from '../context/AuthContext'

/** The human GCI seat: an admin or one of the side's commanders types a call
 *  and the server's GCI voice speaks it on that side's frequency over SRS.
 *  bfdb decides who may transmit; this only hides the box from signed-out
 *  viewers. */
export default function GciSeat(): ReactElement | null {
  const { user } = useAuth()
  const [open, setOpen] = useState(false)
  const [text, setText] = useState('')
  const [status, setStatus] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)
  if (!user) return null
  const send = () => {
    const t = text.trim()
    if (!t) return
    setBusy(true)
    setStatus(null)
    api
      .gciTransmit(t)
      .then(() => {
        setStatus('Transmitted.')
        setText('')
      })
      .catch(e => setStatus(e instanceof Error ? e.message : String(e)))
      .finally(() => setBusy(false))
  }
  return (
    <div
      style={{
        position: 'absolute', left: 12, bottom: 12, zIndex: 5, maxWidth: 'calc(100% - 24px)',
        background: 'var(--bg-elevated, rgba(11,18,32,0.92))', border: '1px solid var(--border, #334155)',
        borderRadius: 6, padding: open ? 10 : 6, fontSize: 12,
      }}
    >
      {!open ? (
        <button type="button" className="vs-btn" onClick={() => setOpen(true)}>GCI seat</button>
      ) : (
        <div style={{ display: 'grid', gap: 6, width: 360, maxWidth: '100%' }}>
          <div style={{ display: 'flex', alignItems: 'center' }}>
            <strong>GCI SEAT</strong>
            <button type="button" className="vs-btn" style={{ marginLeft: 'auto' }} onClick={() => setOpen(false)}>Close</button>
          </div>
          <div style={{ color: 'var(--text-dim, #9ca3af)' }}>
            Your call is spoken on your side's GCI frequency. Commanders and admins only.
          </div>
          <textarea
            className="vs-input"
            rows={3}
            maxLength={200}
            placeholder="Viper 1-1, Overlord, group bullseye 090 for 40, angels 20, hostile"
            value={text}
            onChange={e => setText(e.target.value)}
            onKeyDown={e => { if (e.key === 'Enter' && !e.shiftKey) { e.preventDefault(); send() } }}
          />
          <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
            <button type="button" className="vs-btn" disabled={busy || !text.trim()} onClick={send}>Transmit</button>
            <span style={{ color: 'var(--text-dim, #9ca3af)' }}>{text.length}/200</span>
            {status && <span style={{ marginLeft: 'auto' }}>{status}</span>}
          </div>
        </div>
      )}
    </div>
  )
}
