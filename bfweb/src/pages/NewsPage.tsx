/**
 * The war archive — every daily dispatch, newest first.
 *
 * The SITREP panel shows the last few days; this is where you read the
 * campaign as a story. Same source, same non-fog-of-war rule: one wire report
 * both coalitions see.
 */
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { NewsDayBlock } from '../components/NewsPanel'

export default function NewsPage() {
  const { data, isLoading, error } = useQuery({
    queryKey: ['news', 'archive'],
    queryFn: () => api.news(200),
    refetchInterval: 5 * 60_000,
  })

  const days = data?.days ?? []

  return (
    <div style={{ height: '100%', overflowY: 'auto' }}>
      <div style={{ maxWidth: 820, margin: '0 auto', padding: '0 0 40px' }}>
        <header style={{ padding: '22px 18px 14px', borderBottom: '1px solid var(--border)' }}>
          <h1
            style={{
              margin: 0,
              fontFamily: 'var(--font-display, var(--font-mono))',
              fontSize: '1.5rem',
              letterSpacing: '0.1em',
              textTransform: 'uppercase',
            }}
          >
            War Diary
          </h1>
          <p style={{ margin: '6px 0 0', fontSize: '0.8rem', color: 'var(--text-muted)', lineHeight: 1.55 }}>
            A dispatch for every day of the campaign, written from what actually
            happened on the server — captures, losses, who took what and how
            long a line has held. Both coalitions read the same report.
          </p>
        </header>

        {isLoading && (
          <p className="font-mono-vs" style={{ padding: 18, fontSize: '0.65rem', color: 'var(--text-dim)' }}>
            LOADING ARCHIVE…
          </p>
        )}

        {error != null && (
          <p style={{ padding: 18, fontSize: '0.8rem', color: 'var(--bad, #f22a2a)' }}>
            The archive could not be loaded. If this persists the news generator
            may not be running — it starts a minute after bfdb does.
          </p>
        )}

        {!isLoading && error == null && days.length === 0 && (
          <p style={{ padding: 18, fontSize: '0.85rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
            Nothing has been filed yet. The first dispatch is written once the
            campaign has a day of events behind it.
          </p>
        )}

        {days.map(d => (
          <NewsDayBlock key={d.day} day={d} />
        ))}
      </div>
    </div>
  )
}
