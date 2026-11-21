/** Small building blocks for the Fowl Engine Manager screens -- same visual
 *  vocabulary as the dashboard (vs-card, the CSS tokens in index.css). */
import { useEffect, useRef } from 'react'
import { DIM, MONO, RED, AMBER, OK } from './style'
import { pressable } from '@shared/web/a11y'

/** The server box an action goes to, big enough not to be missed. */
export function ServerTag({ name, local }: { name: string; local?: boolean }) {
  return (
    <span style={{
      display: 'inline-flex', alignItems: 'center', gap: 6, padding: '2px 8px', borderRadius: 2,
      border: `1px solid ${local ? 'var(--border)' : AMBER}`, color: local ? 'var(--text)' : AMBER,
      fontFamily: 'var(--font-display)', letterSpacing: '0.1em', fontSize: '0.8rem', whiteSpace: 'nowrap',
    }}>{name}{!local && <span style={{ ...MONO, fontSize: '0.55rem', opacity: 0.8 }}>REMOTE</span>}</span>
  )
}

/** An in-page confirm for a destructive action, naming the server box it
 *  goes to (never window.confirm: the box's name has to be on it). Escape
 *  or the backdrop cancels; Cancel has the focus. */
export function ConfirmDialog({ server, local, title, children, confirmLabel = 'Confirm', danger = true, onConfirm, onCancel }: {
  server: string; local?: boolean; title: string; children?: React.ReactNode; confirmLabel?: string; danger?: boolean
  onConfirm: () => void; onCancel: () => void
}) {
  const cancelRef = useRef<HTMLButtonElement>(null)
  // the latest onCancel, without re-running the effect (and stealing the
  // focus back) every time the page behind re-renders
  const cancel = useRef(onCancel)
  useEffect(() => { cancel.current = onCancel })
  useEffect(() => {
    cancelRef.current?.focus()
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') cancel.current() }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [])
  return (
    <div style={{ position: 'fixed', inset: 0, zIndex: 1000, display: 'flex', alignItems: 'center', justifyContent: 'center' }}>
      <div {...pressable(onCancel, 'Cancel')} style={{ position: 'absolute', inset: 0, background: 'rgba(0,0,0,0.6)' }} />
      <div role="dialog" aria-modal="true" aria-label={`${title} on ${server}`} className="vs-card" style={{
        position: 'relative', width: 'min(520px, calc(100vw - 32px))', padding: 0,
        borderColor: danger ? 'rgba(239,68,68,0.55)' : undefined, background: 'var(--bg-elevated)',
      }}>
        <div style={{ padding: '14px 16px 10px', borderBottom: '1px solid var(--border)' }}>
          <div style={{ ...DIM, marginBottom: 8 }}>on server box</div>
          <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.5rem', letterSpacing: '0.08em', lineHeight: 1.1,
                        color: danger ? RED : 'var(--text)', wordBreak: 'break-word' }}>{server}</div>
          {!local && <div style={{ fontSize: '0.62rem', color: AMBER, marginTop: 4 }}>a remote box, over Tailscale -- not this PC</div>}
        </div>
        <div style={{ padding: '12px 16px', fontSize: '0.76rem', lineHeight: 1.6 }}>
          <div style={{ fontWeight: 600, marginBottom: 6 }}>{title}</div>
          {children}
        </div>
        <div className="flex gap-2" style={{ padding: '10px 16px 14px', justifyContent: 'flex-end' }}>
          <button ref={cancelRef} onClick={onCancel} style={{
            fontSize: '0.72rem', padding: '0.45rem 1rem', borderRadius: 3, cursor: 'pointer', background: 'none',
            color: 'var(--text)', border: '1px solid var(--border)',
          }}>Cancel</button>
          <button onClick={onConfirm} style={{
            fontSize: '0.72rem', padding: '0.45rem 1rem', borderRadius: 3, cursor: 'pointer', fontWeight: 600,
            background: danger ? RED : 'var(--accent)', color: danger ? '#fff' : '#000', border: 'none',
          }}>{confirmLabel} on {server}</button>
        </div>
      </div>
    </div>
  )
}

export function Card({ icon, title, badge, children }: {
  icon?: React.ReactNode; title: string; badge?: React.ReactNode; children: React.ReactNode
}) {
  return (
    <div className="vs-card">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)', flexWrap: 'wrap' }}>
        {icon}
        <span style={{ ...DIM, fontSize: '0.65rem' }}>{title}</span>
        {badge && <span className="ml-auto">{badge}</span>}
      </div>
      <div className="p-4">{children}</div>
    </div>
  )
}

export function Pill({ color, children }: { color: string; children: React.ReactNode }) {
  return (
    <span style={{
      fontSize: '0.58rem', color, border: `1px solid ${color}`, padding: '1px 7px', borderRadius: 2,
      letterSpacing: '0.08em', textTransform: 'uppercase', whiteSpace: 'nowrap', ...MONO,
    }}>{children}</span>
  )
}

export function Dot({ state }: { state: 'ok' | 'warn' | 'bad' | 'off' }) {
  const c = state === 'ok' ? OK : state === 'warn' ? AMBER : state === 'bad' ? RED : 'var(--text-dim)'
  return <span style={{ display: 'inline-block', width: 8, height: 8, borderRadius: '50%', background: c, flexShrink: 0 }} />
}

export function Btn({ onClick, children, danger, primary, disabled, title }: {
  onClick: () => void; children: React.ReactNode; danger?: boolean; primary?: boolean; disabled?: boolean; title?: string
}) {
  return (
    <button onClick={onClick} disabled={disabled} title={title} style={{
      display: 'inline-flex', alignItems: 'center', gap: 6, fontSize: '0.7rem', letterSpacing: '0.04em',
      color: primary ? '#000' : danger ? RED : 'var(--text)',
      background: primary ? 'var(--accent)' : 'none',
      border: `1px solid ${primary ? 'var(--accent)' : danger ? 'rgba(239,68,68,0.4)' : 'var(--border)'}`,
      padding: '0.4rem 0.85rem', borderRadius: 3, cursor: disabled ? 'not-allowed' : 'pointer',
      opacity: disabled ? 0.5 : 1, whiteSpace: 'nowrap', fontWeight: primary ? 600 : 400,
    }}>{children}</button>
  )
}

export function Row({ k, children }: { k: string; children: React.ReactNode }) {
  return (
    <div className="flex gap-3" style={{ padding: '5px 0', borderBottom: '1px solid var(--border)', alignItems: 'baseline' }}>
      <span style={{ ...DIM, width: 150, flexShrink: 0 }}>{k}</span>
      <span style={{ fontSize: '0.72rem', color: 'var(--text)', minWidth: 0, wordBreak: 'break-word' }}>{children}</span>
    </div>
  )
}

export function Note({ tone = 'info', children }: { tone?: 'info' | 'warn' | 'bad' | 'ok'; children: React.ReactNode }) {
  const c = tone === 'warn' ? AMBER : tone === 'bad' ? RED : tone === 'ok' ? OK : 'var(--text-muted)'
  return (
    <div style={{ fontSize: '0.7rem', color: c, lineHeight: 1.55, padding: '0.5rem 0.7rem', margin: '0 0 10px',
                  border: `1px solid ${tone === 'info' ? 'var(--border)' : c}`, borderRadius: 3, background: 'var(--bg-elevated)' }}>
      {children}
    </div>
  )
}

export function Field({ label, hint, children }: { label: string; hint?: React.ReactNode; children: React.ReactNode }) {
  return (
    <label style={{ display: 'flex', flexDirection: 'column', gap: 5, minWidth: 0 }}>
      <span style={DIM}>{label}</span>
      {children}
      {hint && <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', lineHeight: 1.5 }}>{hint}</span>}
    </label>
  )
}

export function Result({ r, onClose }: { r: { ok: boolean; message: string } | null; onClose: () => void }) {
  if (!r) return null
  return (
    <div {...pressable(onClose, 'Dismiss')} style={{
      cursor: 'pointer', fontSize: '0.7rem', color: r.ok ? OK : RED, padding: '0.5rem 0.7rem', margin: '0 0 12px',
      border: `1px solid ${r.ok ? 'var(--border)' : 'rgba(239,68,68,0.4)'}`, borderRadius: 3,
      background: 'var(--bg-elevated)', whiteSpace: 'pre-wrap',
    }}>{r.ok ? '✓ ' : '✗ '}{r.message}</div>
  )
}
