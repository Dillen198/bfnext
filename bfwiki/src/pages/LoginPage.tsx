import React, { useEffect, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { Eye, EyeOff, KeyRound, User } from 'lucide-react'
import { api } from '../api'
import { useAuth } from '../context/AuthContext'

export default function LoginPage() {
  const { refresh } = useAuth()
  const navigate    = useNavigate()

  const [localEnabled, setLocalEnabled] = useState(false)
  const [mode, setMode]                 = useState<'discord' | 'local'>('discord')

  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [showPw,   setShowPw]   = useState(false)
  const [loading,  setLoading]  = useState(false)
  const [error,    setError]    = useState<string | null>(null)

  useEffect(() => {
    api.auth.localEnabled().then(e => {
      setLocalEnabled(e)
      if (e) setMode('local')
    }).catch(() => {})
  }, [])

  async function handleLocalLogin(e: React.FormEvent) {
    e.preventDefault()
    if (!username || !password) return
    setLoading(true)
    setError(null)
    try {
      await api.auth.localLogin(username, password)
      await refresh()
      navigate('/')
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Login failed')
    } finally {
      setLoading(false)
    }
  }

  const inputStyle: React.CSSProperties = {
    width: '100%', padding: '0.55rem 0.75rem',
    background: 'var(--bg-elevated)', border: '1px solid var(--border)',
    borderRadius: '3px', color: 'var(--text)', fontSize: '0.82rem',
    outline: 'none', fontFamily: 'inherit', letterSpacing: '0.04em',
  }

  return (
    <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'center', height: '100%', background: 'var(--bg)' }}>
      <div style={{ padding: '2rem', width: '100%', maxWidth: '340px', border: '1px solid var(--border)', borderRadius: '4px', background: 'var(--bg-card)' }}>
        <div style={{ textAlign: 'center', marginBottom: '1.75rem' }}>
          <div style={{
            width: 48, height: 48, borderRadius: 6, margin: '0 auto 0.75rem',
            background: 'linear-gradient(135deg, var(--accent) 0%, var(--accent-dim) 100%)',
            display: 'flex', alignItems: 'center', justifyContent: 'center',
            fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.1rem', color: '#0a0d07',
          }}>
            FE
          </div>
          <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.8rem', letterSpacing: '0.15em', color: 'var(--text)' }}>
            FOWL ENGINE
          </div>
          <div style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', marginTop: '0.25rem' }}>
            WIKI ADMIN LOGIN
          </div>
        </div>

        {localEnabled && (
          <div className="flex mb-5" style={{ borderBottom: '1px solid var(--border)' }}>
            {(['discord', 'local'] as const).map(m => (
              <button
                key={m}
                onClick={() => { setMode(m); setError(null) }}
                style={{
                  flex: 1, padding: '0.5rem', fontSize: '0.65rem',
                  letterSpacing: '0.12em', fontFamily: "'Bebas Neue', sans-serif",
                  background: 'none', border: 'none', cursor: 'pointer',
                  color: mode === m ? 'var(--accent)' : 'var(--text-dim)',
                  borderBottom: mode === m ? '2px solid var(--accent)' : '2px solid transparent',
                  marginBottom: '-1px', transition: 'color 0.15s',
                }}
              >
                {m === 'discord' ? 'Discord' : 'Admin Login'}
              </button>
            ))}
          </div>
        )}

        {mode === 'discord' && (
          <div style={{ textAlign: 'center' }}>
            <p style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginBottom: '1.25rem', lineHeight: 1.6 }}>
              Sign in with Discord. Only admins can edit wiki pages — everyone can read them without logging in.
            </p>
            <a
              href={api.auth.loginUrl()}
              style={{
                display: 'inline-flex', alignItems: 'center', gap: '0.6rem',
                background: '#5865F2', color: '#fff', padding: '0.65rem 1.5rem', borderRadius: '3px',
                textDecoration: 'none', fontSize: '0.78rem', letterSpacing: '0.1em',
                fontFamily: "'Bebas Neue', sans-serif",
              }}
            >
              <DiscordIcon />
              LOGIN WITH DISCORD
            </a>
          </div>
        )}

        {mode === 'local' && (
          <form onSubmit={handleLocalLogin} style={{ display: 'flex', flexDirection: 'column', gap: '0.85rem' }}>
            <p style={{ fontSize: '0.68rem', color: 'var(--text-dim)', letterSpacing: '0.08em', textAlign: 'center', marginBottom: '0.25rem' }}>
              Admin credentials
            </p>

            <div>
              <label style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', display: 'block', marginBottom: '0.3rem' }}>
                Username
              </label>
              <div style={{ position: 'relative' }}>
                <User size={13} style={{ position: 'absolute', left: '0.6rem', top: '50%', transform: 'translateY(-50%)', color: 'var(--text-dim)', pointerEvents: 'none' }} />
                <input
                  type="text" value={username} onChange={e => setUsername(e.target.value)}
                  autoComplete="username" placeholder="admin"
                  style={{ ...inputStyle, paddingLeft: '2rem' }}
                />
              </div>
            </div>

            <div>
              <label style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', display: 'block', marginBottom: '0.3rem' }}>
                Password
              </label>
              <div style={{ position: 'relative' }}>
                <KeyRound size={13} style={{ position: 'absolute', left: '0.6rem', top: '50%', transform: 'translateY(-50%)', color: 'var(--text-dim)', pointerEvents: 'none' }} />
                <input
                  type={showPw ? 'text' : 'password'} value={password} onChange={e => setPassword(e.target.value)}
                  autoComplete="current-password" placeholder="••••••••"
                  style={{ ...inputStyle, paddingLeft: '2rem', paddingRight: '2.2rem' }}
                />
                <button
                  type="button" onClick={() => setShowPw(v => !v)}
                  style={{ position: 'absolute', right: '0.6rem', top: '50%', transform: 'translateY(-50%)', background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', padding: 0 }}
                >
                  {showPw ? <EyeOff size={13} /> : <Eye size={13} />}
                </button>
              </div>
            </div>

            {error && (
              <div style={{ fontSize: '0.68rem', color: '#ef4444', textAlign: 'center', letterSpacing: '0.06em' }}>
                {error}
              </div>
            )}

            <button
              type="submit" disabled={loading || !username || !password}
              style={{
                padding: '0.65rem', background: 'var(--accent)', color: '#fff', border: 'none',
                borderRadius: '3px', cursor: loading ? 'not-allowed' : 'pointer', fontSize: '0.78rem',
                letterSpacing: '0.12em', fontFamily: "'Bebas Neue', sans-serif",
                opacity: loading || (!username || !password) ? 0.6 : 1, transition: 'opacity 0.15s',
              }}
            >
              {loading ? 'SIGNING IN…' : 'SIGN IN'}
            </button>

            <button
              type="button" onClick={() => { setMode('discord'); setError(null) }}
              style={{ background: 'none', border: 'none', cursor: 'pointer', fontSize: '0.62rem', color: '#5865F2', letterSpacing: '0.08em', padding: 0 }}
            >
              ← Sign in with Discord instead
            </button>
          </form>
        )}
      </div>
    </div>
  )
}

function DiscordIcon() {
  return (
    <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
      <path d="M20.317 4.37a19.791 19.791 0 0 0-4.885-1.515.074.074 0 0 0-.079.037c-.21.375-.444.864-.608 1.25a18.27 18.27 0 0 0-5.487 0 12.64 12.64 0 0 0-.617-1.25.077.077 0 0 0-.079-.037A19.736 19.736 0 0 0 3.677 4.37a.07.07 0 0 0-.032.027C.533 9.046-.32 13.58.099 18.057a.082.082 0 0 0 .031.057 19.9 19.9 0 0 0 5.993 3.03.078.078 0 0 0 .084-.028 14.09 14.09 0 0 0 1.226-1.994.076.076 0 0 0-.041-.106 13.107 13.107 0 0 1-1.872-.892.077.077 0 0 1-.008-.128 10.2 10.2 0 0 0 .372-.292.074.074 0 0 1 .077-.01c3.928 1.793 8.18 1.793 12.062 0a.074.074 0 0 1 .078.01c.12.098.246.198.373.292a.077.077 0 0 1-.006.127 12.299 12.299 0 0 1-1.873.892.077.077 0 0 0-.041.107c.36.698.772 1.362 1.225 1.993a.076.076 0 0 0 .084.028 19.839 19.839 0 0 0 6.002-3.03.077.077 0 0 0 .032-.054c.5-5.177-.838-9.674-3.549-13.66a.061.061 0 0 0-.031-.03z"/>
    </svg>
  )
}
