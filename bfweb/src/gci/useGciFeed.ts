import { useEffect, useRef, useState } from 'react'
import type { GciDesk, WsGciMsg } from './types'

export function connectGci(
  desk: GciDesk,
  onMsg: (msg: WsGciMsg) => void,
  onStatus: (s: 'open' | 'closed' | 'error') => void,
): () => void {
  const proto = window.location.protocol === 'https:' ? 'wss' : 'ws'
  const ws = new WebSocket(`${proto}://${window.location.host}/ws/gci?desk=${desk}`)
  ws.onopen = () => onStatus('open')
  ws.onclose = () => onStatus('closed')
  ws.onerror = () => onStatus('error')
  ws.onmessage = (e) => {
    try {
      onMsg(JSON.parse(e.data as string) as WsGciMsg)
    } catch {
      /* ignore */
    }
  }
  return () => ws.close()
}

export function useGciFeed(desk: GciDesk) {
  const [msg, setMsg] = useState<WsGciMsg | null>(null)
  const [status, setStatus] = useState<'open' | 'closed' | 'error'>('closed')
  const deskRef = useRef(desk)
  deskRef.current = desk

  useEffect(() => {
    setMsg(null)
    return connectGci(deskRef.current, setMsg, setStatus)
  }, [desk])

  return { msg, status }
}
