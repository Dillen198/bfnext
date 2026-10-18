import { Comms, Sam, Supply, Health, CommandCenter, Rankings } from '@icons'
import { capabilities, wiki } from '../config/campaign'
import { roundDay, useTheatreLive, useTheatres, type Theatre } from '../live'
import { BrowserName, SectionHead, StatusTag, TerritoryBar } from './Bits'
import Reveal from './Reveal'

const ICONS = { Comms, Sam, Supply, Health, CommandCenter, Rankings }

function TheatreCard({ t, offline }: { t: Theatre; offline: boolean }) {
  const { front, frontLoading, airborne } = useTheatreLive(t.instance)
  const round = t.instance?.active_round
  const day = round ? roundDay(round.start) : undefined
  const p = t.profile
  return (
    <article className="panel panel-bracket theatre">
      <div className="theatre-head">
        <div>
          <h3 className="theatre-name">{t.label}</h3>
          {p && <div className="theatre-era">{p.era}</div>}
        </div>
        <StatusTag live={!!round} known={!!t.instance && !offline} />
      </div>

      {p && <p>{p.summary}</p>}

      <TerritoryBar front={front} loading={frontLoading} blueLabel={t.blueLabel} redLabel={t.redLabel} />

      <div className="theatre-stats">
        <div>
          <div className="label">Pilots on</div>
          <div className="v">{round && airborne.known ? airborne.total : '—'}</div>
        </div>
        <div>
          <div className="label">Round</div>
          <div className="v">{round ? <>#{round.id} <small>{day ? `day ${day}` : ''}</small></> : '—'}</div>
        </div>
        <div>
          <div className="label">Lives</div>
          <div className="v" style={{ color: p?.livesEnforced ? 'var(--yellow)' : undefined }}>
            {p ? (p.livesEnforced ? 'Enforced' : 'Unlimited') : '—'}
          </div>
        </div>
      </div>

      {p && (
        <div>
          <div className="label" style={{ marginBottom: 6 }}>Find it in the DCS server browser</div>
          <BrowserName name={p.browserName} />
        </div>
      )}
    </article>
  )
}

export default function Situation() {
  const { theatres, offline } = useTheatres()
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

        <div className="theatres">
          {theatres.map((t, i) => (
            <Reveal key={t.key} delay={i * 120}>
              <TheatreCard t={t} offline={offline} />
            </Reveal>
          ))}
        </div>
      </div>
    </section>
  )
}
