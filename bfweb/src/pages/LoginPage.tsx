import { api } from '../api'

export default function LoginPage() {
  return (
    <div style={{
      display: 'flex', alignItems: 'center', justifyContent: 'center',
      height: '100%', background: 'var(--bg)',
      fontFamily: "'Bebas Neue', sans-serif",
    }}>
      <div style={{
        textAlign: 'center', padding: '2rem',
        border: '1px solid var(--border)', borderRadius: '4px',
        background: '#0d0d0d', maxWidth: '320px',
      }}>
        <div style={{ fontSize: '2rem', letterSpacing: '0.15em', color: 'var(--text)', marginBottom: '0.5rem' }}>
          FOWL ENGINE
        </div>
        <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)', letterSpacing: '0.14em', marginBottom: '2rem' }}>
          Sign in to access your pilot profile
        </div>
        <a
          href={api.auth.loginUrl()}
          style={{
            display: 'inline-flex', alignItems: 'center', gap: '0.6rem',
            background: '#5865F2', color: '#fff',
            padding: '0.6rem 1.4rem', borderRadius: '3px',
            textDecoration: 'none', fontSize: '0.8rem', letterSpacing: '0.1em',
          }}
        >
          <DiscordIcon />
          LOGIN WITH DISCORD
        </a>
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
