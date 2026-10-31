import React from 'react'
import { NavLink, Outlet, useLocation, useNavigate } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import {
  Alert,
  LogOut,
  Menu,
  Plus,
  Search,
  Shield,
  X,
} from '@icons'
import { api, type WikiPageMeta } from '../api'
import { useAuth } from '../context/AuthContext'
import { ThemeToggle } from '@shared/web/theme'
import InstanceSelect from './InstanceSelect'
import { ErrorBoundary } from '@shared/web/ErrorBoundary'
import { LogoMark } from '@shared/web/LogoMark'
import { useOffline, useOfflineText } from '@shared/web/snapshot'

function groupBySection(pages: WikiPageMeta[]) {
  const groups = new Map<string, WikiPageMeta[]>()
  for (const p of pages) {
    if (!groups.has(p.section)) groups.set(p.section, [])
    groups.get(p.section)!.push(p)
  }
  for (const list of groups.values()) list.sort((a, b) => a.order - b.order)
  return groups
}

export default function Layout() {
  const { user, logout } = useAuth()
  // bfdb is down: the pages come from the snapshot, read-only.
  const { offline } = useOffline()
  const offlineText = useOfflineText()
  const canEdit = !!user?.is_admin && !offline
  const navigate = useNavigate()
  const { pathname } = useLocation()
  const [query, setQuery] = React.useState('')
  const [sidebarOpen, setSidebarOpen] = React.useState(false)

  const { data: pages = [] } = useQuery({
    queryKey: ['wiki', 'pages'],
    queryFn: api.wiki.list,
    staleTime: 30_000,
  })

  const filtered = query.trim()
    ? pages.filter(p => p.title.toLowerCase().includes(query.toLowerCase()))
    : pages
  const groups = groupBySection(filtered)

  return (
    <div style={{ display: 'flex', flexDirection: 'column', height: '100vh', overflow: 'hidden', background: 'var(--bg)' }}>
      {/* TOPBAR */}
      <header className="wiki-topbar">
        <button
          className="wiki-mobile-menu-btn"
          onClick={() => setSidebarOpen(v => !v)}
          aria-label={sidebarOpen ? 'Close navigation' : 'Open navigation'}
        >
          {sidebarOpen ? <X size={16} /> : <Menu size={16} />}
        </button>

        <NavLink to="/" style={{ display: 'flex', alignItems: 'center', gap: 10, textDecoration: 'none' }}>
          <LogoMark size={46} alt="Vector Strike Wiki" />
          <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.05rem', letterSpacing: '0.18em', color: 'var(--text)' }}>
            VECTOR STRIKE WIKI
          </div>
        </NavLink>

        <div style={{ flex: 1 }} />

        <InstanceSelect style={{ marginRight: 12 }} />

        <ThemeToggle style={{ marginRight: 12 }} />

        {canEdit && (
          <button
            onClick={() => navigate('/new')}
            className="vs-btn vs-btn-outline"
            style={{ marginRight: 12 }}
          >
            <Plus size={13} /> NEW PAGE
          </button>
        )}

        {user ? (
          <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
            <div style={{ lineHeight: 1.2, textAlign: 'right' }}>
              <div style={{ fontSize: '0.75rem', color: 'var(--text)', fontWeight: 600 }}>{user.username}</div>
              {user.is_admin && (
                <div style={{ display: 'flex', alignItems: 'center', gap: 3, justifyContent: 'flex-end' }}>
                  <Shield size={9} style={{ color: '#fbbf24' }} />
                  <span style={{ fontSize: '0.58rem', color: '#fbbf24', letterSpacing: '0.1em' }}>ADMIN</span>
                </div>
              )}
            </div>
            <button onClick={logout} title="Logout" style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', display: 'flex' }}>
              <LogOut size={15} />
            </button>
          </div>
        ) : offline ? null : (
          <button onClick={() => navigate('/login')} className="vs-btn-ghost" style={{ padding: '6px 14px', borderRadius: 3, cursor: 'pointer', fontSize: '0.72rem', letterSpacing: '0.08em' }}>
            ADMIN LOGIN
          </button>
        )}
      </header>

      {offlineText && (
        <div
          role="status"
          style={{
            display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0, padding: '7px 16px',
            background: 'rgba(239,68,68,0.10)', borderBottom: '1px solid #ef4444',
            color: 'var(--text)', fontSize: '0.78rem',
          }}
        >
          <Alert size={13} style={{ color: '#ef4444', flexShrink: 0 }} />
          <span>{offlineText}. The wiki is read-only until it is back.</span>
        </div>
      )}

      <div style={{ display: 'flex', flex: 1, overflow: 'hidden' }}>
        {/* Backdrop -- closes the drawer on mobile when tapped outside it */}
        <div
          className={`wiki-sidebar-backdrop${sidebarOpen ? ' open' : ''}`}
          role="presentation"
          onClick={() => setSidebarOpen(false)}
        />

        {/* SIDEBAR */}
        <aside className={`wiki-sidebar${sidebarOpen ? ' open' : ''}`}>
          <div style={{ position: 'relative', margin: '10px 14px 14px' }}>
            <Search size={13} style={{ position: 'absolute', left: 8, top: '50%', transform: 'translateY(-50%)', color: 'var(--text-dim)', pointerEvents: 'none' }} />
            <input
              className="wiki-search"
              style={{ margin: 0, width: '100%', paddingLeft: 26 }}
              placeholder="Search pages…"
              value={query}
              onChange={e => setQuery(e.target.value)}
            />
          </div>

          {[...groups.entries()].map(([section, items]) => (
            <div key={section}>
              <div className="wiki-nav-group-label">{section}</div>
              {items.map(p => (
                <NavLink
                  key={p.slug}
                  to={`/${p.slug}`}
                  onClick={() => setSidebarOpen(false)}
                  className={({ isActive }) => `wiki-nav-item${isActive ? ' active' : ''}`}
                >
                  {p.title}
                </NavLink>
              ))}
            </div>
          ))}

          {pages.length === 0 && (
            <div style={{ padding: '0 18px', fontSize: '0.72rem', color: 'var(--text-dim)' }}>No pages yet.</div>
          )}
        </aside>

        {/* MAIN */}
        <main className="wiki-main">
          {/* A crashed page keeps the navigation; moving to another page resets it. */}
          <ErrorBoundary key={pathname}>
            <Outlet />
          </ErrorBoundary>
        </main>
      </div>
    </div>
  )
}
