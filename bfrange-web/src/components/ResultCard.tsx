import { Link } from 'react-router-dom'
import { KIND_CODE, TONE_VAR, airframe, fmtAgo } from '../lib/format'
import type { Summary } from '../types'
import { describeResult } from './describeResult'
import { KindIcon } from './KindIcon'

export function ResultCard({ s, fresh = false, showPilot = true }: { s: Summary; fresh?: boolean; showPilot?: boolean }) {
  const h = describeResult(s.result, s.score)
  const kind = s.result.kind
  return (
    <Link to={`/result/${encodeURIComponent(s.id)}`} className={`rcard${fresh ? ' fresh' : ''}`} title={s.headline}>
      <div className="strip" style={{ background: TONE_VAR[h.tone] }} />
      <div className="body">
        <div className="top">
          <KindIcon kind={kind} size={15} className="shrink-0 muted" />
          <span className="caps" style={{ fontSize: 10, minWidth: 34 }}>{KIND_CODE[kind]}</span>
          <span className="title">
            {showPilot ? s.pilot.name : null}
            <span className="muted font-normal">{showPilot ? ' · ' : ''}{airframe(s.unit_type)}</span>
          </span>
          <span className="big" style={{ color: typeof h.big === 'string' ? TONE_VAR[h.tone] : undefined }}>{h.big}</span>
        </div>
        <div className="meta">
          <span className="truncate min-w-0 flex-1">{h.detail}</span>
          <span className="mono dim shrink-0">{fmtAgo(s.ts)}</span>
        </div>
      </div>
    </Link>
  )
}
