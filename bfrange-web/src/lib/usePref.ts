import { useCallback, useEffect, useRef, useState, type SetStateAction } from 'react'

/**
 * A small per-viewer preference, remembered in this browser (like the theme,
 * src/context/ThemeContext.tsx). Storage can be blocked or empty (private
 * windows, cleared site data): then it quietly falls back to `initial`.
 * `valid` guards against a stale or hand-edited value. Only a change the
 * viewer makes is stored, so a default that depends on the screen (say) is
 * worked out afresh until they pick something.
 */
export function usePref<T>(key: string, initial: T | (() => T), valid: (v: unknown) => v is T) {
  const [value, setValue] = useState<T>(() => {
    try {
      const raw = localStorage.getItem(key)
      if (raw != null) {
        const v: unknown = JSON.parse(raw)
        if (valid(v)) return v
      }
    } catch { /* storage blocked or not JSON */ }
    return typeof initial === 'function' ? (initial as () => T)() : initial
  })
  const chosen = useRef(false)
  useEffect(() => {
    if (!chosen.current) return
    try { localStorage.setItem(key, JSON.stringify(value)) } catch { /* storage blocked */ }
  }, [key, value])
  const set = useCallback((next: SetStateAction<T>) => {
    chosen.current = true
    setValue(next)
  }, [])
  return [value, set] as const
}
