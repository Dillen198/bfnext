import { useEffect, useRef, useState } from 'react'
import { Link } from 'react-router-dom'
import { ArrowRight, Bell, ExternalLink } from '@icons'
import { dashboard } from '../config/campaign'
import { useNotifications, useReadState } from '../notifications'

// ─────────────────────────────────────────────────────────────────────────────
// The site's notification bell: round results, server notices and
// maintenance, in one list instead of banners. Opening it marks everything
// read; what was new stays highlighted until it closes. Only an outage still
// gets a banner (StatusBanner.tsx).
// ─────────────────────────────────────────────────────────────────────────────

function ago(t: number): string {
  const mins = Math.max(0, Math.round((Date.now() - t) / 60_000))
  if (mins < 60) return `${mins}m ago`
  const hrs = Math.floor(mins / 60)
  return hrs < 24 ? `${hrs}h ago` : `${Math.floor(hrs / 24)}d ago`
}

export default function NotificationBell() {
  const items = useNotifications(dashboard)
  const { read, unread, markAllRead } = useReadState(items)
  const [open, setOpen] = useState(false)
  const [fresh, setFresh] = useState<Set<string>>(new Set())
  const box = useRef<HTMLDivElement>(null)

  const toggle = () => {
    if (open) { setOpen(false); return }
    setFresh(new Set(items.filter(n => !read.has(n.id)).map(n => n.id)))
    markAllRead()
    setOpen(true)
  }

  useEffect(() => {
    if (!open) return
    const onDown = (e: MouseEvent) => { if (!box.current?.contains(e.target as Node)) setOpen(false) }
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') setOpen(false) }
    document.addEventListener('mousedown', onDown)
    document.addEventListener('keydown', onKey)
    return () => {
      document.removeEventListener('mousedown', onDown)
      document.removeEventListener('keydown', onKey)
    }
  }, [open])

  return (
    <div className="notif" ref={box}>
      <button
        type="button"
        className="icon-btn notif-btn"
        onClick={toggle}
        aria-label={unread ? `Notifications, ${unread} new` : 'Notifications'}
        aria-expanded={open}
        aria-haspopup="dialog"
      >
        <Bell size={17} />
        {unread > 0 && <span className="notif-count" aria-hidden="true">{unread > 9 ? '9+' : unread}</span>}
      </button>
      {open && (
        <div className="notif-panel" role="dialog" aria-label="Notifications">
          <div className="notif-head">Notifications</div>
          {items.length === 0 ? (
            <div className="notif-empty">Nothing new. Round results, maintenance and server notices show up here.</div>
          ) : items.map(n => {
            const isNew = fresh.has(n.id)
            return (
              <div key={n.id} className={`notif-item tone-${n.tone}${isNew ? ' is-new' : ''}`}>
                <div className="notif-top">
                  <span className="notif-title">{n.title}</span>
                  <span className="notif-when">{isNew ? 'New' : ago(n.time)}</span>
                </div>
                {n.message && <div className="notif-msg">{n.message}</div>}
                {n.link && (n.link.external ? (
                  <a className="notif-link" href={n.link.href} target="_blank" rel="noopener noreferrer" onClick={() => setOpen(false)}>
                    {n.link.label} <ExternalLink size={10} />
                  </a>
                ) : (
                  <Link className="notif-link" to={n.link.href} onClick={() => setOpen(false)}>
                    {n.link.label} <ArrowRight size={11} />
                  </Link>
                ))}
              </div>
            )
          })}
        </div>
      )}
    </div>
  )
}
