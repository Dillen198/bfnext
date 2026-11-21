import { useQuery } from '@tanstack/react-query'
import { api } from '../api'

/** How often the calculators re-read the live picture. */
export const LIVE_REFRESH_MS = 30_000

/**
 * The engine's live picture (`/api/range/live`), re-read every 30 s while a
 * calculator is open. Shares the `['live']` cache with the other pages.
 */
export function useLivePicture() {
  return useQuery({
    queryKey: ['live'],
    queryFn: () => api.live(),
    refetchInterval: LIVE_REFRESH_MS,
    staleTime: 10_000,
    retry: 1,
  })
}
