import { useQuery } from '@tanstack/react-query'
import { fetchStatusFile } from '@shared/web/status'
import { STATUS_FILE_URL } from '@shared/web/urls'

// ─────────────────────────────────────────────────────────────────────────────
// The hand-written server notices (shared/web/status.ts has the format). The
// dashboard reads them cross-origin from the public site, so they are readable
// when the game host, and bfdb with it, is down -- the one time a dashboard
// most needs to explain itself. The outage banner and the notification bell
// both read them, through this one query.
// ─────────────────────────────────────────────────────────────────────────────

export const STATUS_URL: string = import.meta.env.VITE_STATUS_URL ?? STATUS_FILE_URL
export const STATUS_PAGE = new URL('/status', STATUS_URL).href

export function useStatusFile() {
  return useQuery({
    queryKey: ['status-file'],
    queryFn: () => fetchStatusFile(STATUS_URL),
    refetchInterval: 60_000,
    retry: 1,
  })
}
