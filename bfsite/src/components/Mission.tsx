import { ArrowRight, Intercept, Cas, Logistics, Csar, Capture, CommandCenter } from '@icons'
import { roles, wiki } from '../config/campaign'
import { SectionHead } from './Bits'
import Reveal from './Reveal'

const ICONS = { Intercept, Cas, Logistics, Csar, Capture, CommandCenter }

export default function Mission() {
  return (
    <section id="mission" className="section alt">
      <div className="wrap">
        <SectionHead
          num={2}
          para="Mission"
          title={<>Pick <em>your war.</em></>}
          lede="Six ways into the same fight. Each one has a field manual in the wiki for when you want the detail."
        />
        <div className="roles">
          {roles.map((r, i) => {
            const Icon = ICONS[r.icon]
            return (
              <Reveal key={r.title} delay={(i % 3) * 90}>
                <a className="panel role" href={wiki(r.wiki)}>
                  <div className="role-img">
                    <img src={r.image} alt={r.alt} loading="lazy" width={1100} height={614} />
                    <span className="role-tag"><Icon size={13} />{r.tag}</span>
                  </div>
                  <div className="role-body">
                    <h3>{r.title}</h3>
                    <p>{r.body}</p>
                    <span className="text-link">Field manual <ArrowRight size={12} /></span>
                  </div>
                </a>
              </Reveal>
            )
          })}
        </div>
      </div>
    </section>
  )
}
