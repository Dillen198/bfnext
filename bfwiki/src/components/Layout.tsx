import React from 'react'
import { NavLink, Outlet, useNavigate } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { LogOut, Shield, Plus, Search, Menu, X } from 'lucide-react'
import { api, type WikiPageMeta } from '../api'
import { useAuth } from '../context/AuthContext'
import ThemeToggle from './ThemeToggle'

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
  const navigate = useNavigate()
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
          <div style={{
            width: 28, height: 28, borderRadius: 4, flexShrink: 0,
            background: 'linear-gradient(135deg, var(--accent) 0%, var(--accent-dim) 100%)',
            display: 'flex', alignItems: 'center', justifyContent: 'center',
            fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.7rem', color: 'var(--accent-fg)',
          }}>FE</div>
          <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.05rem', letterSpacing: '0.18em', color: 'var(--text)' }}>
            FOWL ENGINE WIKI
          </div>
        </NavLink>

        <div style={{ flex: 1 }} />

        <ThemeToggle style={{ marginRight: 12 }} />

        {user?.is_admin && (
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
        ) : (
          <button onClick={() => navigate('/login')} className="vs-btn-ghost" style={{ padding: '6px 14px', borderRadius: 3, cursor: 'pointer', fontSize: '0.72rem', letterSpacing: '0.08em' }}>
            ADMIN LOGIN
          </button>
        )}
      </header>

      <div style={{ display: 'flex', flex: 1, overflow: 'hidden' }}>
        {/* Backdrop -- closes the drawer on mobile when tapped outside it */}
        <div
          className={`wiki-sidebar-backdrop${sidebarOpen ? ' open' : ''}`}
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
          <Outlet />
        </main>
      </div>
    </div>
  )
}
