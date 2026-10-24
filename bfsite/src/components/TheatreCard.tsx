import { ArrowRight } from '@icons'
import { dashboard, wiki } from '../config/campaign'
import { roundNo } from '../api'
import { lastResult, roundDay, useTheatreLive, type ServerRow } from '../live'
import { BrowserName, NoticeBox, StatusTag, TerritoryBar } from './Bits'
import FlagBackdrop from './Flags'

/** One server, full width: the pitch on the left, its live state on the right, its flags behind. */
export default function TheatreCard({ row }: { row: ServerRow }) {
  const t = row.theatre
  const { front, frontLoading, airborne } = useTheatreLive(row.state === 'offline' ? undefined : t.instance)
  const round = t.instance?.active_round
  const up = row.state === 'live' || row.state === 'standby'
  const day = round ? roundDay(round.start) : undefined
  const p = t.profile
  const won = lastResult(t)
  return (
    <article className="panel panel-bracket theatre">
      {p && <FlagBackdrop flags={p.flags} />}
      <div className="theatre-inner">
        <div className="theatre-main">
          <div className="theatre-head">
            <div>
              <h3 className="theatre-name">{t.label}</h3>
              {p && <div className="theatre-era">{p.era}</div>}
            </div>
            <StatusTag state={row.state} />
          </div>
          {p && <p>{p.summary}</p>}
          <div className="theatre-sides">
            <span className="b">{t.blueLabel}</span>
            <span className="vs">vs</span>
            <span className="r">{t.redLabel}</span>
          </div>
          {won && (
            <div className={`theatre-result ${won.side === 'Blue' ? 'b' : 'r'}${won.recent ? ' is-recent' : ''}`}>
              <span className="label">{won.recent ? 'Round over' : 'Last round'}</span>
              <span className="theatre-result-line">
                Round {won.n}: <strong>{won.winner} won</strong> after {won.lasted},{' '}
                {new Date(won.end).toLocaleDateString([], { day: 'numeric', month: 'short', timeZone: 'UTC' })}
              </span>
              <a className="text-link" href={dashboard(`/rounds?round=${won.id}&instance=${encodeURIComponent(t.key)}`)}>
                Full rundown <ArrowRight size={12} />
              </a>
            </div>
          )}
          {/* "Back online" and other news live in the bell; a card keeps
              the notices that say what state the server is in. */}
          {row.notices.filter(n => n.level !== 'info').map(n => <NoticeBox key={n.id} notice={n} />)}
          <div className="linkcard-links">
            <a className="text-link" href={wiki('reference/aircraft-roster')}>Aircraft roster <ArrowRight size={12} /></a>
            <a className="text-link" href={wiki('gameplay/points-and-lives')}>Points &amp; lives <ArrowRight size={12} /></a>
          </div>
        </div>

        <div className="theatre-side">
          <div>
            <div className="label" style={{ marginBottom: 8 }}>Front line</div>
            <TerritoryBar front={up ? front : undefined} loading={up && frontLoading} blueLabel={t.blueLabel} redLabel={t.redLabel} />
          </div>
          <div className="theatre-stats">
            <div>
              <div className="label">Pilots on</div>
              <div className="v">{up && airborne.known ? airborne.total : '—'}</div>
            </div>
            <div>
              <div className="label">Round</div>
              <div className="v">{up && round ? <>#{roundNo(round)} <small>{day ? `day ${day}` : ''}</small></> : '—'}</div>
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
        </div>
      </div>
    </article>
  )
}
