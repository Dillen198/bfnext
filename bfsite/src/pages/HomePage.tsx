import Hero from '../components/Hero'
import Situation from '../components/Situation'
import Mission from '../components/Mission'
import Intel from '../components/Intel'
import JoinBand from '../components/JoinBand'
import ServerTiles from '../servers/ServerTiles'

/** Pick a server: each tile is a piece of that server's own page. */
function Theatres() {
  return (
    <section id="theatres" className="section">
      <div className="wrap">
        <header className="section-head">
          <div className="para"><span>Theatres</span></div>
          <h2 className="section-title">Pick your <em>theatre.</em></h2>
          <p className="section-lede">
            A modern coalition war over Syria, a 1985 war on the Soviet Union’s southern flank, and a range to
            practise for both. Every server has its own briefing, and its own look.
          </p>
        </header>
        <ServerTiles />
      </div>
    </section>
  )
}

export default function HomePage() {
  return (
    <>
      <Hero />
      <Theatres />
      <Situation />
      <Mission />
      <Intel />
      <JoinBand />
    </>
  )
}
