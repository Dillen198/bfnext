import { Link } from 'react-router-dom'
import { ExternalLink } from '@icons'
import { campaign, dashboard, links, wiki } from '../config/campaign'
import LogoMark from './LogoMark'

export default function Footer() {
  const year = new Date().getFullYear()
  return (
    <footer className="footer">
      <div className="wrap">
        <div className="footer-top">
          <div>
            <div style={{ display: 'flex', alignItems: 'center', gap: 12, marginBottom: 12 }}>
              <LogoMark size={52} alt="" />
              <div>
                <div className="nav-brand-name">{campaign.name}</div>
                <div className="nav-brand-sub">Persistent DCS campaign</div>
              </div>
            </div>
            <p style={{ margin: 0, maxWidth: 340, fontSize: '0.88rem', color: 'var(--text-muted)' }}>{campaign.tagline}</p>
          </div>
          <div>
            <h4>Fly</h4>
            <div className="footer-links">
              <Link to="/join">How to join</Link>
              <Link to="/servers">Servers</Link>
              <Link to="/status">Server status</Link>
              <Link to="/community">Community</Link>
              <a href={links.discord} target="_blank" rel="noopener noreferrer">Discord <ExternalLink size={10} /></a>
              <a href={links.dcs} target="_blank" rel="noopener noreferrer">DCS World <ExternalLink size={10} /></a>
            </div>
          </div>
          <div>
            <h4>Intel</h4>
            <div className="footer-links">
              <a href={links.dashboard}>Ops dashboard</a>
              <a href={dashboard('/news')}>War Diary</a>
              <a href={dashboard('/leaderboard')}>Leaderboard</a>
            </div>
          </div>
          <div>
            <h4>Wiki</h4>
            <div className="footer-links">
              <a href={wiki('playbooks/first-sortie')}>Your first sortie</a>
              <a href={wiki('reference/aircraft-roster')}>Aircraft roster</a>
              <a href={wiki('reference/faq')}>FAQ</a>
            </div>
          </div>
        </div>
        <div className="footer-bottom">
          <span>© {year} {campaign.name} · Powered by Fowl Engine</span>
          <span>Not affiliated with Eagle Dynamics</span>
        </div>
      </div>
    </footer>
  )
}
