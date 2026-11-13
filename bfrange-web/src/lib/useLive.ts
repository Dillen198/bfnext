import { useQuery } from '@tanstack/react-query'
import { api } from '../api'

/**
 * The live range picture. Every page shares the one `['live']` query, so the
 * top bar's status, the control page and the spawner never disagree; each
 * caller only says how fresh it needs it (the shortest interval wins).
 */
export function useLive(refetchMs = 10_000) {
  return useQuery({
    queryKey: ['live'],
    queryFn: () => api.live(),
    refetchInterval: refetchMs,
    staleTime: 1000,
  })
}
