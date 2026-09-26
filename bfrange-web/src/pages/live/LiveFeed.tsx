import { useEffect, useRef, useState } from 'react'
import { Link } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api } from '../../api'
import { Panel } from '../../components/Controls'
import { ResultCard } from '../../components/ResultCard'
import { Empty, ErrorState, Loading } from '../../components/States'

/**
 * Newest results first, refreshed every 10 s. Anything that was not in the
 * previous fetch slides in highlighted, so a pass you just flew is obvious.
 */
export function LiveFeed({ limit = 20 }: { limit?: number }) {
  const q = useQuery({
    queryKey: ['feed', limit],
    queryFn: () => api.feed({ limit }),
    refetchInterval: 10_000,
  })
  const seen = useRef<Set<string> | null>(null)
  const [fresh, setFresh] = useState<Set<string>>(new Set())
  const items = q.data?.items

  useEffect(() => {
    if (!items) return
    const ids = new Set(items.map(i => i.id))
    if (seen.current) {
      const prev = seen.current
      const added = items.filter(i => !prev.has(i.id)).map(i => i.id)
      if (added.length) {
        setFresh(new Set(added))
        const t = setTimeout(() => setFresh(new Set()), 4000)
        seen.current = ids
        return () => clearTimeout(t)
      }
    }
    seen.current = ids
  }, [items])

  return (
    <Panel
      title={<span className="inline-flex items-center gap-2"><span className="dot live" /> Live results</span>}
      right={<Link to="/results" className="text-[12px] muted hover:text-[var(--chalk)]">All results →</Link>}
    >
      {q.isLoading ? (
        <Loading label="Loading results" />
      ) : q.error ? (
        <ErrorState error={q.error} retry={() => q.refetch()} />
      ) : !items?.length ? (
        <Empty title="Nothing graded yet">Results appear here the moment a bomb lands, a pass is graded or a session ends.</Empty>
      ) : (
        <div className="flex flex-col gap-1.5">
          {items.map(s => <ResultCard key={s.id} s={s} fresh={fresh.has(s.id)} />)}
        </div>
      )}
    </Panel>
  )
}
