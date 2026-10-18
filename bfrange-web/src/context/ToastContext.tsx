import { createContext, useCallback, useContext, useState, type ReactNode } from 'react'
import { Link } from 'react-router-dom'
import { X } from '@icons'

/** A link the toast offers, e.g. "Open debrief". */
export interface ToastAction {
  label: string
  to: string
}

interface Toast {
  id: number
  message: ReactNode
  ok: boolean
  action?: ToastAction
}

type Push = (message: ReactNode, ok?: boolean, action?: ToastAction) => void

const Ctx = createContext<Push>(() => {})

let next = 1

/** Needs to sit inside the router: a toast can carry a link. */
export function ToastProvider({ children }: { children: ReactNode }) {
  const [toasts, setToasts] = useState<Toast[]>([])
  const dismiss = useCallback((id: number) => setToasts(ts => ts.filter(t => t.id !== id)), [])
  const push = useCallback<Push>(
    (message, ok = true, action) => {
      const id = next++
      setToasts(ts => [...ts.slice(-3), { id, message, ok, action }])
      setTimeout(() => dismiss(id), action ? 12000 : ok ? 5000 : 9000)
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
            <div className="msg">
              {t.message}
              {t.action && (
                <Link to={t.action.to} className="toast-action" onClick={() => dismiss(t.id)}>
                  {t.action.label} →
                </Link>
              )}
            </div>
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
