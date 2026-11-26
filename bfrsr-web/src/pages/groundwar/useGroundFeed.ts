// The ground picture, live. /ws/groundwar pushes a frame about every 2 s; a
// bfdb that predates the socket (or a proxy that drops it) gets polled on
// /api/groundwar every 5 s instead, until the socket comes back. `?mock` runs
// the dev fixture's simulation on the same 2 s beat.
import { useCallback, useEffect, useRef, useState } from 'react'
import { api, connectGroundwar, type GroundFrame, type GroundPicture } from '../../api'
import type { GroundMock } from '../groundwarMock'
import { normalizePicture } from './normalize'

export type Link = 'connecting' | 'live' | 'polling' | 'offline' | 'mock'

export interface Feed {
  pic: GroundPicture | null
  /** Why there is no picture, from the socket: login | nocoalition | unavailable | disabled. */
  reason: string | null
  error: string | null
  link: Link
  /** performance.now() of the latest picture, and the gap before it (ms). */
  at: number
  gap: number
  /** Ask for a fresh picture now (after an order). */
  refresh: () => void
}

interface State {
  key: string
  pic: GroundPicture | null
  reason: string | null
  error: string | null
  link: Link
  at: number
  gap: number
}

const POLL_MS = 5_000
const POLL_BACKOFF_MS = 60_000
/** How long to give the socket before polling starts. */
const WS_GRACE_MS = 3_500

export function useGroundFeed(side: 'Blue' | 'Red' | undefined, instance: string | undefined, mock: GroundMock | null): Feed {
  const key = `${side ?? ''}|${instance ?? ''}|${mock ? 'mock' : ''}`
  const [st, setSt] = useState<State>({ key, pic: null, reason: null, error: null, link: 'connecting', at: 0, gap: 2000 })
  const pollNow = useRef<() => void>(() => {})

  useEffect(() => {
    let stopped = false
    const put = (patch: Partial<State> & { pic?: GroundPicture | null }) => {
      if (stopped) return
      setSt((prev) => {
        const base = prev.key === key ? prev : { key, pic: null, reason: null, error: null, link: 'connecting' as Link, at: 0, gap: 2000 }
        const next = { ...base, ...patch }
        if (patch.pic) {
          const t = performance.now()
          next.gap = base.at ? Math.max(500, Math.min(6000, t - base.at)) : 2000
          next.at = t
        }
        return next
      })
    }

    if (mock) {
      const emit = () => put({ pic: mock.picture(), reason: null, error: null, link: 'mock' })
      const t0 = window.setTimeout(emit, 0)
      let last = performance.now()
      const iv = window.setInterval(() => {
        const t = performance.now()
        mock.step((t - last) / 1000)
        last = t
        emit()
      }, 2000)
      pollNow.current = () => window.setTimeout(emit, 150)
      return () => {
        stopped = true
        window.clearTimeout(t0)
        window.clearInterval(iv)
      }
    }

    let wsLive = false
    let pollTimer: number | null = null
    const schedule = (ms: number) => {
      if (pollTimer !== null) window.clearTimeout(pollTimer)
      pollTimer = window.setTimeout(poll, ms)
    }
    const poll = async () => {
      pollTimer = null
      if (stopped || wsLive) return
      try {
        const pic = await api.groundwar.picture(side)
        if (stopped || wsLive) return
        put({ pic: normalizePicture(pic), reason: null, error: null, link: 'polling' })
        schedule(POLL_MS)
      } catch (e) {
        if (stopped || wsLive) return
        put({ error: (e as Error).message, link: 'offline' })
        schedule(POLL_BACKOFF_MS)
      }
    }
    pollNow.current = () => {
      if (!wsLive) schedule(0)
    }

    const close = connectGroundwar(
      (f: GroundFrame) => {
        wsLive = true
        if (pollTimer !== null) {
          window.clearTimeout(pollTimer)
          pollTimer = null
        }
        if (f.picture) put({ pic: normalizePicture(f.picture), reason: null, error: null, link: 'live' })
        else put({ reason: f.reason ?? 'unavailable', link: 'live', ...(f.reason === 'unavailable' ? {} : { pic: null }) })
      },
      (s) => {
        if (s === 'open') return
        if (wsLive) {
          wsLive = false
          schedule(WS_GRACE_MS)
        }
      },
      side,
    )
    schedule(WS_GRACE_MS)
    return () => {
      stopped = true
      close()
      if (pollTimer !== null) window.clearTimeout(pollTimer)
    }
  }, [key, side, mock])

  const refresh = useCallback(() => pollNow.current(), [])
  const cur = st.key === key ? st : { pic: null, reason: null, error: null, link: 'connecting' as Link, at: 0, gap: 2000 }
  return { pic: cur.pic, reason: cur.reason, error: cur.error, link: cur.link, at: cur.at, gap: cur.gap, refresh }
}
