/**
 * Where a calculator's conditions come from, shared by the bomb and
 * wind-over-deck tools:
 *  - LIVE · DCS    read from the running mission (refreshed every 30 s)
 *  - EDITED        the user typed their own numbers over them
 *  - NO LIVE DATA  the range server is not up (or sent nothing usable), so
 *                  the fields hold plain defaults
 */
import type { ReactNode } from 'react'
import { fmtAgo } from '../../lib/format'
import { useNow } from '../../lib/useNow'

export type Source = 'live' | 'edited' | 'none' | 'loading'

export function SourceChip({ source }: { source: Source }) {
  switch (source) {
    case 'live':
      return <span className="chip live" title="Read from the running DCS mission"><span className="dot live" />LIVE · DCS</span>
    case 'edited':
      return <span className="chip warn" title="Your own numbers, not the mission's">EDITED</span>
    case 'loading':
      return <span className="chip outline">READING DCS…</span>
    default:
      return <span className="chip bad" title="The range server is not sending live conditions">NO LIVE DATA</span>
  }
}

/**
 * One line under the source chip saying what the numbers are, plus the
 * one-click way back to the mission's values after an edit.
 */
export function SourceNote({
  source,
  updatedAt,
  canUseLive,
  onUseLive,
  live,
  edited,
  none,
}: {
  source: Source
  /** ms timestamp of the live read */
  updatedAt?: number
  canUseLive: boolean
  onUseLive: () => void
  live: ReactNode
  edited: ReactNode
  none: ReactNode
}) {
  const now = useNow(10_000)
  return (
    <div className="flex flex-col gap-2 text-[12px]">
      <p className="m-0 muted">
        {source === 'live' ? (
          <>
            {live}
            {updatedAt ? <span className="dim"> Updated {fmtAgo(new Date(updatedAt).toISOString(), now)}.</span> : null}
          </>
        ) : source === 'edited' ? edited : source === 'loading' ? 'Asking the range server for the mission’s conditions…' : none}
      </p>
      {source === 'edited' && canUseLive && (
        <button className="btn-range sm self-start" onClick={onUseLive}>
          Use live DCS values
        </button>
      )}
    </div>
  )
}
