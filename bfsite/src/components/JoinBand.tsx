import { Link } from 'react-router-dom'
import { ArrowRight, Support } from '@icons'
import { links } from '../config/campaign'
import Reveal from './Reveal'

/** The closing call to action on every page that isn't the join page itself. */
export default function JoinBand() {
  return (
    <section className="section section-tight">
      <div className="wrap">
        <Reveal className="panel cta-band">
          <div>
            <h3>Ready to fly?</h3>
            <p>Four steps from your desktop to the flight line. The squadron is on Discord to help.</p>
          </div>
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: 10 }}>
            <Link to="/join" className="vs-btn vs-btn-outline">How to join <ArrowRight size={14} /></Link>
            <a href={links.discord} target="_blank" rel="noopener noreferrer" className="vs-btn">
              <Support size={15} />
              Join the Discord
            </a>
          </div>
        </Reveal>
      </div>
    </section>
  )
}
