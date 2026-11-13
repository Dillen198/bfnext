import { ExternalLink, Server, Support } from '@icons'
import { credits, links } from '../config/campaign'
import { SectionHead } from './Bits'
import { LogoMark } from '@shared/web/LogoMark'
import Reveal from './Reveal'

export default function Community() {
  return (
    <section id="community" className="section">
      <div className="wrap">
        <SectionHead
          num={5}
          para="Command & signal"
          title={<>The <em>squadron.</em></>}
        />

        <div className="comm-grid">
          <Reveal className="panel panel-bracket discord-panel">
            <span className="chip">Primary net</span>
            <h3>Discord is where the war is planned</h3>
            <ul>
              <li>Server status and restarts, posted as they happen</li>
              <li>The War Diary and capture alerts from the front</li>
              <li>Squadrons recruiting, and pilots looking for a wingman</li>
              <li>Questions answered by the people who build the campaign</li>
            </ul>
            <div>
              <a href={links.discord} target="_blank" rel="noopener noreferrer" className="vs-btn">
                <Support size={15} />
                Join the Discord
              </a>
            </div>
          </Reveal>

          <Reveal delay={100} className="credits">
            <div className="credit">
              <span className="credit-mark"><LogoMark size={34} alt="" /></span>
              <div>
                <div className="credit-role">Campaign engine</div>
                <h3>Fowl Engine</h3>
                <p>
                  The persistent campaign engine under Vector Strike: territory, logistics, cargo, troops
                  and stats, running live inside DCS. Originally created by <strong>Eric Stokes</strong>.
                </p>
              </div>
            </div>
            <div className="credit">
              <span className="credit-mark"><img src="/hacker.jpg" alt="" /></span>
              <div>
                <div className="credit-role">Server host</div>
                <h3>61st ATP</h3>
                <p>
                  A 24/7 campaign needs a machine that never goes offline. <strong>Hacker</strong>, founder
                  of <strong>61st ATP (Arctic Terns &amp; Penguins)</strong>, donated the one it runs on.
                  Every sortie on the board exists because of it. Thank you.
                </p>
              </div>
            </div>
            <div className="credit">
              <span className="credit-mark"><img src="/special-k.png" alt="" style={{ objectFit: 'contain', padding: 4 }} /></span>
              <div>
                <div className="credit-role">Server bot</div>
                <h3>DCSServerBot</h3>
                <p>
                  <strong>Special K</strong>’s Discord framework runs our server bot: mission control, player
                  management and the link between Discord and the fight.{' '}
                  <a href="https://github.com/Special-K-s-Flightsim-Bots/DCSServerBot" target="_blank" rel="noopener noreferrer">
                    GitHub <ExternalLink size={10} />
                  </a>
                </p>
              </div>
            </div>
            <div className="credit">
              <span className="credit-mark"><Server size={20} /></span>
              <div>
                <div className="credit-role">Simulator</div>
                <h3>Eagle Dynamics</h3>
                <p>
                  Makers of DCS World, the simulator all of this is built on.{' '}
                  <a href={links.dcs} target="_blank" rel="noopener noreferrer">
                    digitalcombatsimulator.com <ExternalLink size={10} />
                  </a>
                </p>
              </div>
            </div>
            <div className="credit">
              <span className="credit-mark"><Support size={20} /></span>
              <div>
                <div className="credit-role">Developed &amp; customised by</div>
                <h3>{credits.developers.join(' & ')}</h3>
              </div>
            </div>
          </Reveal>
        </div>
      </div>
    </section>
  )
}
