import { api } from '../api'
import ThemeToggle from '../components/ThemeToggle'
import LogoMark from '../components/LogoMark'

/**
 * Discord is the only way a person signs in. bfdb's username/password login
 * (/api/auth/local-login) is a machine credential for DCSServerBot on the
 * server itself -- it only answers from that machine -- so offering it here
 * just put an admin password prompt on the public internet.
 */
export default function LoginPage() {
  return (
    <div style={{ position: 'relative', display: 'flex', alignItems: 'center', justifyContent: 'center', height: '100%', background: 'var(--bg)' }}>
      <ThemeToggle style={{ position: 'absolute', top: '1.25rem', right: '1.25rem' }} />
      <div style={{ padding: '2rem', width: '100%', maxWidth: '340px', border: '1px solid var(--border)', borderRadius: '4px', background: 'var(--bg-card)' }}>
        <div style={{ textAlign: 'center', marginBottom: '1.75rem' }}>
          <LogoMark size={96} alt="Vector Strike Wiki" style={{ margin: '0 auto 0.75rem', display: 'block' }} />
          <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.8rem', letterSpacing: '0.15em', color: 'var(--text)' }}>
            VECTOR STRIKE
          </div>
          <div style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', marginTop: '0.25rem' }}>
            WIKI ADMIN LOGIN
          </div>
        </div>

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
