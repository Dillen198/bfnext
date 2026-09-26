import { createContext, useCallback, useContext, useState, type ReactNode } from 'react'
import { X } from '@icons'

interface Toast {
  id: number
  message: string
  ok: boolean
}

const Ctx = createContext<(message: string, ok?: boolean) => void>(() => {})

let next = 1

export function ToastProvider({ children }: { children: ReactNode }) {
  const [toasts, setToasts] = useState<Toast[]>([])
  const dismiss = useCallback((id: number) => setToasts(ts => ts.filter(t => t.id !== id)), [])
  const push = useCallback(
    (message: string, ok = true) => {
      const id = next++
      setToasts(ts => [...ts.slice(-3), { id, message, ok }])
      setTimeout(() => dismiss(id), ok ? 5000 : 9000)
    },
    [dismiss],
  )
  return (
    <Ctx.Provider value={push}>
      {children}
      <div className="toast-stack" role="status" aria-live="polite">
        {toasts.map(t => (
          <div key={t.id} className={`toast-item${t.ok ? '' : ' err'}`}>
            <div className="strip" />
            <div className="msg">{t.message}</div>
            <button className="btn-range ghost sm" style={{ margin: 6 }} onClick={() => dismiss(t.id)} aria-label="Dismiss">
              <X size={14} />
            </button>
          </div>
        ))}
      </div>
    </Ctx.Provider>
  )
}

// eslint-disable-next-line react-refresh/only-export-components
export const useToast = () => useContext(Ctx)
