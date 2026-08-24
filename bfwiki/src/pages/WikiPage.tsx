import { useParams, useNavigate, Link } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { Pencil } from 'lucide-react'
import { api } from '../api'
import { useAuth } from '../context/AuthContext'
import WikiMarkdown from '../components/WikiMarkdown'

export default function WikiPage() {
  const params = useParams()
  const slug = params['*'] ?? ''
  const navigate = useNavigate()
  const { user } = useAuth()

  const { data: page, isLoading, error } = useQuery({
    queryKey: ['wiki', 'page', slug],
    queryFn: () => api.wiki.get(slug),
    enabled: !!slug,
  })

  if (!slug) return null

  if (isLoading) {
    return <div style={{ color: 'var(--text-dim)', fontSize: '0.85rem' }}>Loading…</div>
  }

  if (error || !page) {
    return (
      <div>
        <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.6rem', color: 'var(--text)', marginBottom: '0.5rem' }}>
          PAGE NOT FOUND
        </div>
        <p style={{ color: 'var(--text-muted)', fontSize: '0.85rem', marginBottom: '1.25rem' }}>
          No wiki page exists at <code style={{ fontFamily: 'var(--font-mono)', color: 'var(--accent-bright)' }}>{slug}</code>.
        </p>
        {user?.is_admin && (
          <button className="vs-btn" onClick={() => navigate(`/new?slug=${encodeURIComponent(slug)}`)}>
            CREATE THIS PAGE
          </button>
        )}
        <div style={{ marginTop: '1rem' }}>
          <Link to="/" style={{ color: 'var(--accent-bright)', fontSize: '0.8rem' }}>← Back to home</Link>
        </div>
      </div>
    )
  }

  return (
    <div>
      <div style={{ display: 'flex', alignItems: 'flex-start', justifyContent: 'space-between', gap: 16, marginBottom: '0.5rem' }}>
        <div style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
          {page.section}
        </div>
        {user?.is_admin && (
          <button
            onClick={() => navigate(`/edit?slug=${encodeURIComponent(slug)}`)}
            className="vs-btn-outline"
            style={{ display: 'inline-flex', alignItems: 'center', gap: 6, padding: '5px 12px', borderRadius: 2, fontSize: '0.68rem', letterSpacing: '0.1em', fontFamily: "'Bebas Neue', sans-serif", cursor: 'pointer', flexShrink: 0 }}
          >
            <Pencil size={12} /> EDIT
          </button>
        )}
      </div>

      <div className="wiki-prose">
        <WikiMarkdown>{page.content}</WikiMarkdown>
      </div>

      <div style={{ marginTop: '2.5rem', paddingTop: '1rem', borderTop: '1px solid var(--border)', fontSize: '0.65rem', color: 'var(--text-dim)' }}>
        Last updated {new Date(page.updated_at).toLocaleString()}
      </div>
    </div>
  )
}
