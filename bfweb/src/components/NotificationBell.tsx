import { useEffect, useRef, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { Bell, ChevronRight, ExternalLink } from '@icons'
import { useInstance } from '../context/InstanceContext'
import { ago } from '../lib/format'
import { useNotifications, useReadState, type AppNotification } from '../lib/notifications'

// ─────────────────────────────────────────────────────────────────────────────
// The notification bell in the top bar: round results, server notices and
// maintenance, in one list instead of a stack of banners over every page.
// The badge counts what this viewer has not opened the list since; opening
// it marks everything read, and what was new stays highlighted until it is
// closed. Only an outage still gets a banner (StatusBanner.tsx).
// ─────────────────────────────────────────────────────────────────────────────

const PANEL_W = 360

export default function NotificationBell() {
  const items = useNotifications()
  const { read, unread, markAllRead } = useReadState(items)
  const { current, select } = useInstance()
  const navigate = useNavigate()
  const [open, setOpen] = useState(false)
  // What was unread when the list was opened: still shown as new while it is.
  const [fresh, setFresh] = useState<Set<string>>(new Set())
  const [pos, setPos] = useState({ top: 0, right: 0 })
  const button = useRef<HTMLButtonElement>(null)
  const panel = useRef<HTMLDivElement>(null)

  const toggle = () => {
    if (open) { setOpen(false); return }
    const r = button.current?.getBoundingClientRect()
    if (r) setPos({ top: r.bottom + 6, right: Math.max(8, window.innerWidth - r.right) })
    setFresh(new Set(items.filter(n => !read.has(n.id)).map(n => n.id)))
    markAllRead()
    setOpen(true)
  }

  useEffect(() => {
    if (!open) return
    const onDown = (e: MouseEvent) => {
      const t = e.target as Node
      if (!panel.current?.contains(t) && !button.current?.contains(t)) setOpen(false)
    }
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') setOpen(false) }
    document.addEventListener('mousedown', onDown)
    document.addEventListener('keydown', onKey)
    return () => {
      document.removeEventListener('mousedown', onDown)
      document.removeEventListener('keydown', onKey)
    }
  }, [open])

  const follow = (n: AppNotification) => {
    if (!n.link) return
    setOpen(false)
    if (n.link.external) {
      window.open(n.link.href, '_blank', 'noopener,noreferrer')
      return
    }
    // A result from the other server: switch to it first, or its round id
    // would be looked up on the server being shown.
    if (n.instance && n.instance !== current?.id) select(n.instance)
    navigate(n.link.href)
  }

  return (
    <>
      <button
        ref={button}
        type="button"
        onClick={toggle}
        aria-label={unread ? `Notifications, ${unread} new` : 'Notifications'}
        aria-expanded={open}
        aria-haspopup="dialog"
        className="vs-touch-target"
        style={{
          position: 'relative', display: 'inline-flex', alignItems: 'center', justifyContent: 'center',
          width: 30, height: 30, flexShrink: 0, cursor: 'pointer',
          background: open ? 'var(--bg-hover)' : 'none', border: '1px solid var(--border-light)',
          color: unread ? 'var(--text)' : 'var(--text-dim)',
        }}
      >
        <Bell size={15} />
        {unread > 0 && (
          <span
            aria-hidden="true"
            style={{
              position: 'absolute', top: -5, right: -5, minWidth: 16, height: 16, padding: '0 4px',
              boxSizing: 'border-box', borderRadius: 8, background: 'var(--accent)', color: '#000',
              fontSize: '0.58rem', fontWeight: 700, lineHeight: '16px', textAlign: 'center',
              fontFamily: 'var(--font-mono)',
            }}
          >
            {unread > 9 ? '9+' : unread}
          </span>
        )}
      </button>

      {open && (
        <div
          ref={panel}
          role="dialog"
          aria-label="Notifications"
          style={{
            position: 'fixed', top: pos.top, right: pos.right, zIndex: 80,
            width: `min(${PANEL_W}px, calc(100vw - 16px))`, maxHeight: '70vh', overflowY: 'auto',
            background: 'var(--bg-chrome-solid)', border: '1px solid var(--border-light)',
            boxShadow: '0 12px 32px rgba(0,0,0,0.5)',
          }}
        >
          <div style={{
            padding: '9px 14px', borderBottom: '1px solid var(--border)', fontFamily: 'var(--font-mono)',
            fontSize: '0.62rem', letterSpacing: '0.16em', color: 'var(--text-dim)',
          }}>
            NOTIFICATIONS
          </div>
          {items.length === 0 ? (
            <div style={{ padding: '18px 14px', fontSize: '0.75rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
              Nothing new. Round results, maintenance and server notices show up here.
            </div>
          ) : items.map(n => {
            const isNew = fresh.has(n.id)
            return (
              <div
                key={n.id}
                style={{
                  display: 'flex', gap: 10, padding: '11px 14px', borderBottom: '1px solid var(--border)',
                  borderLeft: `3px solid ${n.color}`,
                  background: isNew ? `color-mix(in srgb, ${n.color} 8%, transparent)` : undefined,
                }}
              >
                <div style={{ flex: 1, minWidth: 0 }}>
                  <div style={{ display: 'flex', alignItems: 'baseline', gap: 8 }}>
                    <span style={{ flex: 1, minWidth: 0, fontSize: '0.78rem', fontWeight: 700, color: 'var(--text)' }}>
                      {n.title}
                    </span>
                    <span className="font-mono-vs" style={{ fontSize: '0.58rem', color: isNew ? n.color : 'var(--text-dim)', flexShrink: 0 }}>
                      {isNew ? 'NEW' : ago(new Date(n.time))}
                    </span>
                  </div>
                  {n.message && (
                    <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginTop: 3, lineHeight: 1.5 }}>
                      {n.message}
                    </div>
                  )}
                  {n.link && (
                    <button
                      type="button"
                      onClick={() => follow(n)}
                      style={{
                        all: 'unset', cursor: 'pointer', display: 'inline-flex', alignItems: 'center', gap: 3,
                        marginTop: 6, color: n.color, fontFamily: 'var(--font-mono)', fontSize: '0.6rem',
                        fontWeight: 700, letterSpacing: '0.14em', textTransform: 'uppercase',
                      }}
                    >
                      {n.link.label} {n.link.external ? <ExternalLink size={10} /> : <ChevronRight size={11} />}
                    </button>
                  )}
                </div>
              </div>
            )
          })}
        </div>
      )}
    </>
  )
}
