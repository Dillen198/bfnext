// The notification bell's bookkeeping, shared by the dashboard and the public
// site. What each bell lists is the app's own business; how long a round is
// news and which items this browser has seen are not.

import { useCallback, useState } from 'react'

/** How long a finished round stays in the bell. */
export const ROUND_NEWS_MS = 7 * 86_400_000

/** How long a round lasted, as the bell says it: "5.2 hours", "3.1 days". */
export function lasted(start: string, end: string): string {
  const hours = Math.max(0, (Date.parse(end) - Date.parse(start)) / 3_600_000)
  return hours < 48 ? `${hours.toFixed(1)} hours` : `${(hours / 24).toFixed(1)} days`
}

const READ_KEEP = 200

function readIds(key: string): string[] {
  try {
    const v = JSON.parse(localStorage.getItem(key) ?? '[]')
    return Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string') : []
  } catch {
    return []
  }
}

/** Which notifications this browser has seen, and a way to mark them seen.
 *  Read state lives in localStorage under `storageKey`; it only drives the
 *  unread count, so nothing is lost when storage is unavailable. */
export function useReadState(items: { id: string }[], storageKey: string) {
  const [read, setRead] = useState<Set<string>>(() => new Set(readIds(storageKey)))
  const unread = items.filter(n => !read.has(n.id)).length
  const markAllRead = useCallback(() => {
    const next = new Set(read)
    for (const n of items) next.add(n.id)
    // Newest last, trimmed, so the list cannot grow without end.
    const kept = [...next].slice(-READ_KEEP)
    try { localStorage.setItem(storageKey, JSON.stringify(kept)) } catch { /* private window */ }
    setRead(new Set(kept))
  }, [items, read, storageKey])
  return { read, unread, markAllRead }
}
