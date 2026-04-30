import { useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api } from '../api'
import { useAuth } from '../context/AuthContext'
import PageHeader from '../components/PageHeader'
import { Shield, Link, Users, Trash2 } from 'lucide-react'
import { useNavigate } from 'react-router-dom'

export default function AdminPage() {
  const { user } = useAuth()
  const navigate = useNavigate()
  const queryClient = useQueryClient()
  const [unlinkTarget, setUnlinkTarget] = useState<string | null>(null)

  // Redirect non-admins
  if (!user) { navigate('/login'); return null }
  if (!user.is_admin) { navigate('/'); return null }

  const { data: links = [], isLoading: linksLoading } = useQuery({
    queryKey: ['admin', 'links'],
    queryFn: api.admin.links,
    refetchInterval: 30_000,
  })

  const { data: sessions = [], isLoading: sessionsLoading } = useQuery({
    queryKey: ['admin', 'sessions'],
    queryFn: api.admin.sessions,
    refetchInterval: 15_000,
  })

  async function handleUnlink(discord_id: string) {
    await api.admin.unlink(discord_id)
    queryClient.invalidateQueries({ queryKey: ['admin', 'links'] })
    setUnlinkTarget(null)
  }

  const dim: React.CSSProperties = { fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }
  const cell: React.CSSProperties = { padding: '6px 10px', fontSize: '0.7rem', color: 'var(--text-muted)', borderBottom: '1px solid var(--border)' }

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader title="ADMIN PANEL" sub="Server management — restricted access" />

      <div className="flex-1 overflow-auto p-4 space-y-4" style={{ background: 'var(--bg)' }}>

        {/* ── Active sessions ── */}
        <div className="vs-card">
          <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
            <Users size={13} style={{ color: 'var(--accent)' }} />
            <span style={{ ...dim, fontSize: '0.65rem' }}>Active Sessions</span>
            <span className="ml-auto font-mono" style={{ fontSize: '0.65rem', color: 'var(--text-muted)' }}>{sessions.length}</span>
          </div>
          {sessionsLoading && <div style={{ padding: '1rem', ...dim }}>Loading…</div>}
          {!sessionsLoading && sessions.length === 0 && (
            <div style={{ padding: '1rem', ...dim }}>No active sessions</div>
          )}
          {sessions.length > 0 && (
            <table style={{ width: '100%', borderCollapse: 'collapse' }}>
              <thead>
                <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
                  {['User', 'Discord ID', 'Admin', 'Expires'].map(h => (
                    <th key={h} style={{ ...cell, color: 'var(--text-dim)', textAlign: 'left', fontWeight: 600 }}>{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {sessions.map(s => (
                  <tr key={s.discord_id} style={{ background: s.discord_id === user.discord_id ? 'rgba(77,124,15,0.04)' : 'transparent' }}>
                    <td style={cell}>
                      <div className="flex items-center gap-2">
                        {s.avatar ? (
                          <img src={`https://cdn.discordapp.com/avatars/${s.discord_id}/${s.avatar}.webp?size=24`} alt="" style={{ width: 20, height: 20, borderRadius: '50%' }} />
                        ) : (
                          <div style={{ width: 20, height: 20, borderRadius: '50%', background: 'var(--accent)', flexShrink: 0 }} />
                        )}
                        <span style={{ color: 'var(--text)' }}>{s.username}</span>
                        {s.discord_id === user.discord_id && <span style={{ fontSize: '0.5rem', color: 'var(--accent)', border: '1px solid var(--accent)', padding: '0 3px', borderRadius: 2 }}>YOU</span>}
                      </div>
                    </td>
                    <td style={{ ...cell, fontFamily: 'monospace' }}>{s.discord_id}</td>
                    <td style={cell}>
                      {s.is_admin && <Shield size={11} style={{ color: '#f59e0b' }} />}
                    </td>
                    <td style={{ ...cell, fontFamily: 'monospace', fontSize: '0.62rem' }}>
                      {new Date(s.expires).toLocaleString()}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          )}
        </div>

        {/* ── Linked accounts ── */}
        <div className="vs-card">
          <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
            <Link size={13} style={{ color: 'var(--accent)' }} />
            <span style={{ ...dim, fontSize: '0.65rem' }}>Discord ↔ DCS Links</span>
            <span className="ml-auto font-mono" style={{ fontSize: '0.65rem', color: 'var(--text-muted)' }}>{links.length}</span>
          </div>
          {linksLoading && <div style={{ padding: '1rem', ...dim }}>Loading…</div>}
          {!linksLoading && links.length === 0 && (
            <div style={{ padding: '1rem', ...dim }}>No linked accounts</div>
          )}
          {links.length > 0 && (
            <table style={{ width: '100%', borderCollapse: 'collapse' }}>
              <thead>
                <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
                  {['Discord ID', 'DCS UCID', ''].map(h => (
                    <th key={h} style={{ ...cell, color: 'var(--text-dim)', textAlign: 'left', fontWeight: 600 }}>{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {links.map(l => (
                  <tr key={l.discord_id}>
                    <td style={{ ...cell, fontFamily: 'monospace' }}>{l.discord_id}</td>
                    <td style={{ ...cell, fontFamily: 'monospace' }}>{l.ucid}</td>
                    <td style={{ ...cell, textAlign: 'right' }}>
                      {unlinkTarget === l.discord_id ? (
                        <span className="flex items-center gap-2 justify-end">
                          <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)' }}>Confirm?</span>
                          <button onClick={() => handleUnlink(l.discord_id)}
                            style={{ fontSize: '0.62rem', color: 'var(--accent)', background: 'none', border: '1px solid var(--accent)', padding: '1px 8px', borderRadius: 2, cursor: 'pointer' }}>
                            Yes
                          </button>
                          <button onClick={() => setUnlinkTarget(null)}
                            style={{ fontSize: '0.62rem', color: 'var(--text-dim)', background: 'none', border: '1px solid var(--border)', padding: '1px 8px', borderRadius: 2, cursor: 'pointer' }}>
                            No
                          </button>
                        </span>
                      ) : (
                        <button onClick={() => setUnlinkTarget(l.discord_id)}
                          style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', padding: 0 }}
                          title="Unlink account">
                          <Trash2 size={12} />
                        </button>
                      )}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          )}
        </div>

      </div>
    </div>
  )
}
