import { Link } from 'react-router-dom'
import { Alert, ArrowRight } from '@icons'
import { useOfflineText } from '@shared/web/snapshot'
import { useServerStates } from '../live'
import { fmtUtc, type Notice } from '../status'

const RANK: Record<Notice['level'], number> = { outage: 0, maintenance: 1, degraded: 2, info: 3 }

/**
 * One line under the nav on every page, only while a server is down: an
 * outage notice or a maintenance window running now, or -- when nobody has
 * written one -- what the probes see. Everything else ("back online", a
 * window coming up, a round won) is in the notification bell instead, so
 * the pages stay clear.
 */
export default function StatusBanner() {
  const { notices, offline, rows } = useServerStates()
  const fromSnapshot = useOfflineText()
  const top = notices
    .filter(n => n.level === 'outage' || n.level === 'maintenance')
    .sort((a, b) => RANK[a.level] - RANK[b.level])[0]
  const engineDown = rows.filter(r => r.state === 'engine-down').map(r => r.theatre.label)

  let level: Notice['level']
  let text: string
  if (top) {
    level = top.level
    const eta = fmtUtc(top.eta)
    text = top.title + (eta ? ` · ${top.level === 'maintenance' ? 'until' : 'expected back'} ${eta}` : '')
  } else if (offline) {
    level = 'outage'
    text = fromSnapshot
      ? `${fromSnapshot}.`
      : 'The game servers aren’t answering. Live data is unavailable until they’re back.'
  } else if (engineDown.length) {
    level = 'degraded'
    text = `${engineDown.join(' and ')} ${engineDown.length > 1 ? 'are' : 'is'} down right now.`
  } else {
    return null
  }

  return (
    <div className={`banner lvl-${level}`}>
      <div className="wrap banner-row">
        <Alert size={14} />
        <span className="banner-text">{text}</span>
        <Link to="/status" className="banner-link">Status <ArrowRight size={11} /></Link>
      </div>
    </div>
  )
}
