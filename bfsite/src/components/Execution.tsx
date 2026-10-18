import { ArrowRight, ExternalLink, Support } from '@icons'
import { links, wiki } from '../config/campaign'
import { SERVERS } from '../config/servers'
import { BrowserName, SectionHead } from './Bits'
import Reveal from './Reveal'

export default function Execution() {
  return (
    <section id="join" className="section">
      <div className="wrap">
        <SectionHead
          num={3}
          para="Execution"
          title={<>Get in <em>the fight.</em></>}
          lede="Four steps from the desktop to the flight line. All skill levels are welcome, and nobody minds a question on Discord."
        />

        <div className="exec-grid">
          <Reveal>
            <ol className="steps">
              <li className="step">
                <span className="step-num">01</span>
                <div>
                  <h3>Get DCS World</h3>
                  <p>
                    DCS World is free. Any module on either server’s roster gets you in, and the free
                    Su-25T flies for the Red side on both.
                  </p>
                  <div className="step-extra" style={{ flexDirection: 'row', flexWrap: 'wrap', gap: 16 }}>
                    <a className="text-link" href={links.dcs} target="_blank" rel="noopener noreferrer">
                      digitalcombatsimulator.com <ExternalLink size={11} />
                    </a>
                    <a className="text-link" href={wiki('reference/aircraft-roster')}>
                      Aircraft roster <ArrowRight size={12} />
                    </a>
                  </div>
                </div>
              </li>
              <li className="step">
                <span className="step-num">02</span>
                <div>
                  <h3>Join the Discord</h3>
                  <p>Server status, the current briefing, squadrons looking for wingmen, and the people who run it.</p>
                  <div className="step-extra" style={{ flexDirection: 'row' }}>
                    <a className="text-link" href={links.discord} target="_blank" rel="noopener noreferrer">
                      Open the invite <ExternalLink size={11} />
                    </a>
                  </div>
                </div>
              </li>
              <li className="step">
                <span className="step-num">03</span>
                <div>
                  <h3>Find the server</h3>
                  <p>Search <strong style={{ color: 'var(--text)' }}>[VS] Vector Strike</strong> in the DCS multiplayer browser. Pick a theatre:</p>
                  <div className="step-extra">
                    {SERVERS.map(s => (
                      <div key={s.instanceId}>
                        <div className="label" style={{ marginBottom: 4 }}>{s.label}</div>
                        <BrowserName name={s.browserName} />
                      </div>
                    ))}
                  </div>
                </div>
              </li>
              <li className="step">
                <span className="step-num">04</span>
                <div>
                  <h3>Pick a side and fly</h3>
                  <p>Choose a coalition, take a slot, and check the briefing for what your side needs most right now.</p>
                  <div className="step-extra" style={{ flexDirection: 'row' }}>
                    <a className="text-link" href={wiki('getting-started/joining-team')}>
                      Joining a team <ArrowRight size={12} />
                    </a>
                  </div>
                </div>
              </li>
            </ol>
          </Reveal>

          <Reveal delay={120}>
            <a className="panel panel-bracket exec-card" href={wiki('playbooks/first-sortie')} style={{ display: 'block', textDecoration: 'none' }}>
              <img src="/media/join.jpg" alt="A pilot at a desk with a campaign map and a laptop showing the server list" loading="lazy" width={1100} height={614} />
              <div className="exec-card-body">
                <span className="chip">Wiki playbook</span>
                <h3>Your first sortie</h3>
                <p>
                  It isn’t “join and fly”. The twenty-minute version of what to do, in order, the first
                  time you connect: the briefing, the slot list, the F10 menus and your first task.
                </p>
                <span className="text-link">Read it before you spawn <ArrowRight size={12} /></span>
              </div>
            </a>
          </Reveal>
        </div>

        <Reveal className="panel cta-band">
          <div>
            <h3>Ready to fly?</h3>
            <p>The squadron is on Discord. Say hello and we’ll get you on the flight line.</p>
          </div>
          <a href={links.discord} target="_blank" rel="noopener noreferrer" className="vs-btn">
            <Support size={15} />
            Join the Discord
          </a>
        </Reveal>
      </div>
    </section>
  )
}
