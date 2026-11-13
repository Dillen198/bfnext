import { useEffect, useRef } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { describeResult } from '../components/describeResult'
import { useAuth } from '../context/AuthContext'
import { useToast } from '../context/ToastContext'
import { KIND_LABEL, TONE_VAR } from './format'

/**
 * Tell a linked pilot the moment one of their own results is graded, on any
 * page, with a link to the debrief. Shares the live feed's query, so on the
 * control page it costs nothing extra. Results already on the board when the
 * page opened are not announced.
 */
export function useMyResultAlerts() {
  const { me } = useAuth()
  const toast = useToast()
  const ucid = me?.ucid ?? null
  const q = useQuery({
    queryKey: ['feed', 20],
    queryFn: () => api.feed({ limit: 20 }),
    refetchInterval: 15_000,
    enabled: !!ucid,
  })
  const seen = useRef<Set<string> | null>(null)
  const items = q.data?.items

  useEffect(() => {
    if (!items || !ucid) return
    const prev = seen.current
    seen.current = new Set(items.map(i => i.id))
    if (!prev) return
    for (const s of items) {
      if (prev.has(s.id) || s.pilot.ucid !== ucid) continue
      const h = describeResult(s.result, s.score)
      toast(
        <span className="flex flex-col gap-1">
          <span className="caps" style={{ color: 'var(--chalk)' }}>Your {KIND_LABEL[s.result.kind] ?? s.result.kind} is graded</span>
          <span className="flex items-center gap-2">
            <span className="mono font-semibold" style={{ color: typeof h.big === 'string' ? TONE_VAR[h.tone] : undefined }}>{h.big}</span>
            <span className="muted text-[12px] truncate">{h.detail}</span>
          </span>
        </span>,
        true,
        { label: 'Open debrief', to: `/result/${encodeURIComponent(s.id)}` },
      )
    }
  }, [items, ucid, toast])
}
