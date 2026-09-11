import { api } from '../api'
import { campaign } from '../config/campaign'
import ThemeToggle from '../components/ThemeToggle'
import LogoMark from '../components/LogoMark'
import Backdrop from '../components/Backdrop'

/**
 * Sign-in. Discord is the only way a person gets into the dashboard: a pilot's
 * identity here *is* their Discord account (that is what links them to a UCID,
 * a coalition and their stats), so a separate username/password would create a
 * second class of user with no pilot behind it.
 *
 * bfdb still exposes POST /api/auth/local-login, but only as a machine
 * credential -- DCSServerBot uses it to reach the admin API for /feops, the log
 * relay and binary swaps. It is deliberately not offered in this UI.
 */
export default function LoginPage() {
  return (
    <div style={{
      display: 'flex', alignItems: 'center', justifyContent: 'center',
      height: '100%', position: 'relative',
    }}>
      <Backdrop />
      <ThemeToggle style={{ position: 'absolute', top: 16, right: 16 }} />
      <div style={{
        padding: '2rem', width: '100%', maxWidth: '340px',
        border: '1px solid var(--border)', borderRadius: '4px',
        background: 'var(--bg-card)',
      }}>
        {/* Header */}
        <div style={{ textAlign: 'center', marginBottom: '1.75rem' }}>
          {campaign.logoUrl ? (
            <img
              src={campaign.logoUrl}
              alt={campaign.name}
              style={{ width: 64, height: 64, objectFit: 'contain', margin: '0 auto 0.75rem', display: 'block' }}
            />
          ) : (
            <LogoMark size={110} alt={campaign.name} style={{ margin: '0 auto 0.75rem', display: 'block' }} />
          )}
          <div style={{
            fontFamily: "'Bebas Neue', sans-serif",
            fontSize: '1.8rem', letterSpacing: '0.15em', color: 'var(--text)',
          }}>
            {campaign.name}
          </div>
          <div style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', marginTop: '0.25rem' }}>
            OPERATIONS DASHBOARD
          </div>
        </div>

        {/* Discord login */}
        <div style={{ textAlign: 'center' }}>
          <p style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginBottom: '1.25rem', lineHeight: 1.6 }}>
            Sign in with Discord to access your pilot profile and stats.
          </p>
          <a
            href={api.auth.loginUrl()}
            style={{
              display: 'inline-flex', alignItems: 'center', gap: '0.6rem',
              background: '#5865F2', color: '#fff',
              padding: '0.65rem 1.5rem', borderRadius: '3px',
              textDecoration: 'none', fontSize: '0.78rem',
              letterSpacing: '0.1em', fontFamily: "'Bebas Neue', sans-serif",
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
