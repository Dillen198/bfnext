import { SectionHead } from '../components/Bits'
import JoinBand from '../components/JoinBand'
import ServerTiles from '../servers/ServerTiles'

export default function ServersPage() {
  return (
    <>
      <section className="section page-first">
        <div className="wrap">
          <SectionHead
            num={1}
            para="Situation · Theatres"
            title={<>Two wars <em>and a range.</em></>}
            lede="Same engine, different eras and different rules. Each campaign runs its own front line; the range is where you practise for both. Open one for its briefing and story."
          />
          <ServerTiles />
        </div>
      </section>
      <JoinBand />
    </>
  )
}
