import { ArrowRight, Comms, Sam, Supply, Health, CommandCenter, Rankings } from '@icons'
import { capabilities, wiki } from '../config/campaign'
import { Link } from 'react-router-dom'
import { SectionHead } from './Bits'
import Reveal from './Reveal'

const ICONS = { Comms, Sam, Supply, Health, CommandCenter, Rankings }

export default function Situation() {
  return (
    <section id="situation" className="section">
      <div className="wrap">
        <SectionHead
          num={1}
          para="Situation"
          title={<>The war <em>never stops.</em></>}
        />

        <div className="sit-grid">
          <Reveal className="sit-copy">
            <p>
              Vector Strike is a <strong>dynamic campaign</strong> run on Fowl Engine. Two coalitions
              of real pilots fight over airbases, FARPs, factories and SAM sites, and every objective
              taken stays taken until someone takes it back.
            </p>
            <p>
              There is no mission reset after you land. Supply has to be flown in, damage has to be
              repaired, and the front line you log off from is the one you come back to, or a worse one.
            </p>
            <p>
              Fly whatever you own: fast jets, attack helicopters, transports or trainers. Every role
              moves the war, and every sortie is on the record.
            </p>
            <Link to="/servers" className="text-link">Two servers, two wars <ArrowRight size={12} /></Link>
          </Reveal>

          <Reveal delay={100} className="cap-grid">
            {capabilities.map(c => {
              const Icon = ICONS[c.icon]
              return (
                <a key={c.title} className="cap" href={wiki(c.wiki)}>
                  <span className="cap-icon"><Icon size={17} /></span>
                  <div>
                    <h3>{c.title}</h3>
                    <p>{c.body}</p>
                  </div>
                </a>
              )
            })}
          </Reveal>
        </div>

      </div>
    </section>
  )
}
