import { useEffect, useRef, useState } from 'react'
import { Link } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api } from '../../api'
import { ResultCard } from '../../components/ResultCard'
import { Empty, ErrorState, Loading } from '../../components/States'
import { useAuth } from '../../context/AuthContext'
import { usePref } from '../../lib/usePref'

const isWho = (v: unknown): v is 'all' | 'mine' => v === 'all' || v === 'mine'

/**
 * Newest results first, refreshed every 10 s. Anything that was not in the
 * previous fetch slides in highlighted, so a pass you just flew is obvious.
 * A linked pilot can narrow it to their own results.
 */
export function LiveFeed({ limit = 20 }: { limit?: number }) {
  const { me } = useAuth()
  const [who, setWho] = usePref<'all' | 'mine'>('range.live.feed', 'all', isWho)
  const q = useQuery({
    queryKey: ['feed', limit],
    queryFn: () => api.feed({ limit }),
    refetchInterval: 10_000,
  })
  const seen = useRef<Set<string> | null>(null)
  const [fresh, setFresh] = useState<Set<string>>(new Set())
  const all = q.data?.items
  const mineOnly = who === 'mine' && !!me?.ucid
  const items = mineOnly ? all?.filter(i => i.pilot.ucid === me?.ucid) : all

  useEffect(() => {
    if (!all) return
    const ids = new Set(all.map(i => i.id))
    if (seen.current) {
      const prev = seen.current
      const added = all.filter(i => !prev.has(i.id)).map(i => i.id)
      if (added.length) {
        setFresh(new Set(added))
        const t = setTimeout(() => setFresh(new Set()), 4000)
        seen.current = ids
        return () => clearTimeout(t)
      }
    }
    seen.current = ids
  }, [all])

  return (
    <section className="rdock-sec">
      <header className="rdock-sec-h">
        <span className="dot live" />
        <h3 className="caps m-0">Results as they land</h3>
        <span className="ml-auto flex items-center gap-2">
          {me?.ucid && (
            <span className="seg" role="group" aria-label="Whose results">
              <button aria-pressed={!mineOnly} onClick={() => setWho('all')} style={{ height: 22, fontSize: 11.5, padding: '0 8px' }}>All</button>
              <button aria-pressed={mineOnly} onClick={() => setWho('mine')} style={{ height: 22, fontSize: 11.5, padding: '0 8px' }}>Mine</button>
            </span>
          )}
          <Link to={mineOnly && me?.ucid ? `/pilot/${encodeURIComponent(me.ucid)}` : '/results'} className="text-[11.5px] muted hover:text-[var(--chalk)]">
            {mineOnly ? 'My record' : 'All'} →
          </Link>
        </span>
      </header>
      <div className="px-2 pb-2">
        {q.isLoading ? (
          <Loading label="Loading results" />
        ) : q.error ? (
          <ErrorState error={q.error} retry={() => q.refetch()} />
        ) : !items?.length ? (
          mineOnly
            ? <Empty title="Nothing of yours yet">Your results appear here the moment they are graded.</Empty>
            : <Empty title="Nothing graded yet">Results appear here the moment a bomb lands, a pass is graded or a session ends.</Empty>
        ) : (
          <div className="flex flex-col gap-1.5">
            {items.map(s => <ResultCard key={s.id} s={s} fresh={fresh.has(s.id)} />)}
          </div>
        )}
      </div>
    </section>
  )
}
