import { useEffect, useRef, useState } from 'react'
import { connectTacmap, type TacFrame, type TacPicture } from '../api'

export interface TacFeed {
  /** The latest coalition picture, or null when denied / not yet received. */
  picture: TacPicture | null
  /** Set when the server withheld the picture: 'login' or 'nocoalition'. */
  reason: 'login' | 'nocoalition' | null
  status: 'open' | 'closed' | 'error'
}

/** Subscribe to `/ws/tacmap`. The server resolves the viewer's coalition from
 *  the session cookie and streams only that side's fogged picture (or a
 *  `reason` when it withholds one). Reconnects on drop. */
export function useTacFeed(): TacFeed {
  const [picture, setPicture] = useState<TacPicture | null>(null)
  const [reason, setReason] = useState<'login' | 'nocoalition' | null>(null)
  const [status, setStatus] = useState<'open' | 'closed' | 'error'>('closed')
  const retry = useRef<number | null>(null)

  useEffect(() => {
    let closed = false
    let cleanup: (() => void) | null = null

    const open = () => {
      if (closed) return
      cleanup = connectTacmap(
        (frame: TacFrame) => {
          setPicture(frame.picture)
          setReason(frame.picture ? null : frame.reason ?? null)
        },
        (s) => {
          setStatus(s)
          if ((s === 'closed' || s === 'error') && !closed) {
            if (retry.current) window.clearTimeout(retry.current)
            retry.current = window.setTimeout(open, 3000)
          }
        },
      )
    }
    open()

    return () => {
      closed = true
      if (retry.current) window.clearTimeout(retry.current)
      cleanup?.()
    }
  }, [])

  return { picture, reason, status }
}
