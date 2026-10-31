import { useCallback, useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api, roundNo } from '../api'

/**
 * A lookup from a round's id to the number to show for it, for pages that
 * only carry the id (a pilot's sorties, kills, theatres). The ids are
 * database keys that jump in blocks of a million; the number is the round's
 * place on its own server. Falls back to the id until the list has loaded.
 */
export function useRoundNo(): (id: number) => number {
  const { data } = useQuery({
    queryKey: ['rounds', 'all'],
    queryFn: () => api.rounds(true),
    staleTime: 5 * 60_000,
  })
  const byId = useMemo(() => new Map((data ?? []).map(r => [r.id, roundNo(r)])), [data])
  return useCallback((id: number) => byId.get(id) ?? id, [byId])
}
