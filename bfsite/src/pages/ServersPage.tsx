import { useServerStates } from '../live'
import { SectionHead } from '../components/Bits'
import JoinBand from '../components/JoinBand'
import Reveal from '../components/Reveal'
import TheatreCard from '../components/TheatreCard'

export default function ServersPage() {
  const { rows } = useServerStates()
  return (
    <>
      <section className="section page-first">
        <div className="wrap">
          <SectionHead
            num={1}
            para="Situation · Theatres"
            title={<>Two servers, <em>two wars.</em></>}
            lede="Same engine, different eras and different rules. Each server runs its own campaign with its own front line, so pick the war you want to fight."
          />
          <div className="theatres">
            {rows.map((r, i) => (
              <Reveal key={r.theatre.key} delay={i * 120}>
                <TheatreCard row={r} />
              </Reveal>
            ))}
          </div>
        </div>
      </section>
      <JoinBand />
    </>
  )
}
