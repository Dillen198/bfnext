// /replay -- the selected server's Tacview recordings, newest first.

import { Link } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { PlayCircle } from '@icons'
import { api } from '../api'
import { useInstance } from '../context/InstanceContext'
import PageHeader from '../components/PageHeader'
import QueryState from '../components/QueryState'
import { fmtDuration } from './data'
import './replay.css'

export default function ReplayListPage() {
  const { selected, current } = useInstance()
  const instance = selected ?? current?.id
  const recs = useQuery({ queryKey: ['replay-recs', instance], queryFn: api.replayRecordings, refetchInterval: 60_000 })
  const status = useQuery({ queryKey: ['replay-status'], queryFn: api.replayStatus, staleTime: 60_000 })
  const st = status.data?.instances.find(i => i.instance === instance) ?? status.data?.instances[0]

  let note: string | null = null
  if (st && !st.tacview) note = 'This server does not publish its Tacview recordings yet, so there is nothing to replay.'
  else if (st && st.pending > 0) note = `${st.pending} recording${st.pending === 1 ? ' is' : 's are'} waiting to be processed. A mission's recording appears here a few minutes after the mission ends.`

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader
        icon={PlayCircle}
        title="FLIGHT REPLAY"
        sub="Every sortie, replayed in 2D or 3D in the browser. No Tacview needed."
      />
      <div className="flex-1 overflow-auto p-4" style={{ display: 'flex', flexDirection: 'column', gap: 12 }}>
        <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', lineHeight: 1.6, maxWidth: 760 }}>
          Open a recording and pick a flight, or open your own flights from the Flight Log on your
          pilot page. Recordings are kept for {status.data?.retention_days ?? 30} days.
          {note && <div style={{ color: 'var(--yellow)', marginTop: 6 }}>{note}</div>}
        </div>
        {recs.data && recs.data.length > 0 ? (
          <div className="rp-recs">
            {recs.data.map(r => (
              <Link key={r.id} to={`/replay/${encodeURIComponent(r.id)}`} className="rp-rec">
                <span className="rp-rec-date">{new Date(r.start_ms).toISOString().slice(0, 16).replace('T', ' ')}Z</span>
                <span className="rp-rec-title">{r.title || r.file}</span>
                <span className="rp-rec-stats">
                  <span>{fmtDuration(r.duration_ms)}</span>
                  <span>{r.flights} flights</span>
                  <span>{r.pilots} pilots</span>
                </span>
              </Link>
            ))}
          </div>
        ) : (
          <QueryState
            isLoading={recs.isLoading}
            isError={recs.isError}
            error={recs.error}
            isEmpty={!recs.data?.length}
            what="recordings"
            emptyText="No recordings yet"
            onRetry={recs.refetch}
          />
        )}
      </div>
    </div>
  )
}
