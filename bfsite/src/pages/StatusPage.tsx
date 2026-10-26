import { Alert, CheckCircle2, Clock, ExternalLink } from '@icons'
import { links } from '../config/campaign'
import { ago } from '@shared/web/snapshot'
import { roundDay, useServerStates, type ServerRow } from '../live'
import { roundNo } from '../api'
import { fmtUtc, stateTone } from '../status'
import { NoticeBox, SectionHead, StatusTag } from '../components/Bits'

function Overall({ tone, title, sub }: { tone: 'ok' | 'warn' | 'bad' | 'idle'; title: string; sub: string }) {
  const Icon = tone === 'ok' ? CheckCircle2 : tone === 'idle' ? Clock : Alert
  return (
    <div className={`panel overall tone-${tone}`}>
      <span className="overall-icon"><Icon size={22} /></span>
      <div>
        <h3>{title}</h3>
        <p>{sub}</p>
      </div>
    </div>
  )
}

/** "Offline since Sun 19 Oct, 18:00 UTC · last mission ODFv2, round 4, 12 pilots on":
 *  a server's last known state while bfdb is down, from the snapshot. */
function lastSeen(r: ServerRow): string | undefined {
  const l = r.last
  if (!l) return undefined
  const since = l.last_seen ? `Offline since ${fmtUtc(l.last_seen)}` : 'Offline'
  const parts = [
    l.mission && `last mission ${l.mission}`,
    l.active_round && `round ${l.active_round.number ?? l.active_round.id}`,
    // Written while the engine still answered, so it is the count at the end.
    l.players && `${l.players.total} pilot${l.players.total === 1 ? '' : 's'} on`,
  ].filter(Boolean)
  return parts.length ? `${since} · ${parts.join(', ')}` : since
}

export default function StatusPage() {
  const { rows, offline, loading, notices, upcoming, checkedAt, snapshotAt } = useServerStates()

  const tones = rows.map(r => stateTone(r.state))
  const overall =
    loading ? { tone: 'idle' as const, title: 'Checking the servers…', sub: 'Asking each server how it is.' }
    : offline || tones.every(t => t === 'bad') ? {
      tone: 'bad' as const,
      title: 'Servers offline',
      sub: snapshotAt
        ? `The game servers are down. The site, dashboard and wiki stay up, showing the last known state from ${fmtUtc(snapshotAt)} (${ago(snapshotAt)}); live data returns with the servers.`
        : 'The game servers are down. The site, dashboard and wiki stay up; live data returns with the servers.',
    }
    : tones.some(t => t === 'bad' || t === 'warn') ? { tone: 'warn' as const, title: 'Partly available', sub: 'At least one server is down or in maintenance. Details below.' }
    : { tone: 'ok' as const, title: 'All servers up', sub: 'Everything is answering. See you on the flight line.' }

  const checked = checkedAt ? new Date(checkedAt).toLocaleTimeString('en-GB', { timeZone: 'UTC' }) + ' UTC' : undefined

  return (
    <section className="section page-first">
      <div className="wrap">
        <SectionHead
          num={5}
          para="Signal · Server status"
          title={<>Server <em>status.</em></>}
          lede="Live checks of each game server, and notices from the people who run them. This page works even when the game host is down."
        />

        <Overall {...overall} />

        {notices.length > 0 && (
          <div className="status-block">
            <div className="label">Notices</div>
            <div className="status-notices">
              {notices.map(n => <NoticeBox key={n.id} notice={n} />)}
            </div>
          </div>
        )}

        <div className="status-block">
          <div className="label">Systems</div>
          <div className="systems">
            <div className="sys-row">
              <div>
                <div className="sys-name">Website, dashboard &amp; wiki</div>
                <div className="sys-sub">Hosted apart from the game servers</div>
              </div>
              <span className="status tone-ok"><span className="dot" />Up</span>
            </div>
            <div className="sys-row">
              <div>
                <div className="sys-name">Live data</div>
                <div className="sys-sub">Rounds, front lines, pilots on, the War Diary</div>
              </div>
              <span className={`status tone-${loading ? 'idle' : offline ? 'bad' : 'ok'}`}>
                <span className="dot" />{loading ? 'Checking…' : offline ? (snapshotAt ? `Last known, ${fmtUtc(snapshotAt)}` : 'Unreachable') : 'Up'}
              </span>
            </div>
            {rows.map(r => {
              const round = r.theatre.instance?.active_round
              const day = round ? roundDay(round.start) : undefined
              const latency = r.health?.engine.latency_ms
              return (
                <div className="sys-row" key={r.theatre.key}>
                  <div>
                    <div className="sys-name">{r.theatre.label}</div>
                    <div className="sys-sub">
                      {r.state === 'live' && round ? `Round ${roundNo(round)}${day ? `, day ${day}` : ''}` : r.state === 'standby' ? 'Up, between rounds' : r.notices[0]?.title ?? (r.state === 'engine-down' ? 'The DCS server isn’t answering' : r.state === 'offline' ? lastSeen(r) ?? 'No answer from the host' : 'DCS server')}
                      {latency != null && (r.state === 'live' || r.state === 'standby') ? ` · ${latency} ms` : ''}
                    </div>
                  </div>
                  <StatusTag state={r.state} />
                </div>
              )
            })}
          </div>
          {checked && <div className="sys-checked">Last checked {checked} · refreshes every minute</div>}
        </div>

        {upcoming.length > 0 && (
          <div className="status-block">
            <div className="label">Scheduled maintenance</div>
            <div className="status-notices">
              {upcoming.map(m => (
                <div key={m.start} className="notice lvl-maintenance">
                  <div className="notice-head">
                    <span className="notice-level">Planned</span>
                    <strong>{m.title}</strong>
                  </div>
                  {m.message && <p>{m.message}</p>}
                  <div className="notice-meta"><span>{fmtUtc(m.start)} to {fmtUtc(m.end)}</span></div>
                </div>
              ))}
            </div>
          </div>
        )}

        <p className="status-foot">
          Restarts and outages are also posted on{' '}
          <a href={links.discord} target="_blank" rel="noopener noreferrer">Discord <ExternalLink size={10} /></a>.
        </p>
      </div>
    </section>
  )
}
