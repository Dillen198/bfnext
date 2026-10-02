/** Small building blocks for the Fowl Engine Manager screens -- same visual
 *  vocabulary as the dashboard (vs-card, the CSS tokens in index.css). */
import { DIM, MONO, RED, AMBER, OK } from './style'

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
    <div onClick={onClose} style={{
      cursor: 'pointer', fontSize: '0.7rem', color: r.ok ? OK : RED, padding: '0.5rem 0.7rem', margin: '0 0 12px',
      border: `1px solid ${r.ok ? 'var(--border)' : 'rgba(239,68,68,0.4)'}`, borderRadius: 3,
      background: 'var(--bg-elevated)', whiteSpace: 'pre-wrap',
    }}>{r.ok ? '✓ ' : '✗ '}{r.message}</div>
  )
}
