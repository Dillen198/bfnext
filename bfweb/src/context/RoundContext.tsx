import { createContext, useCallback, useContext, useMemo } from 'react'
import { useSearchParams } from 'react-router-dom'

interface RoundContextValue {
  /** The round being viewed, or undefined for the live/active one. */
  selectedRound: number | undefined
  setSelectedRound: (id: number | undefined) => void
}

const RoundContext = createContext<RoundContextValue>({
  selectedRound: undefined,
  setSelectedRound: () => {},
})

/**
 * Which round the dashboard is looking at.
 *
 * This lives in the URL (`?round=76000000`) rather than in component state.
 * It used to be a plain useState, which meant the selection could not be
 * linked to or bookmarked and was silently thrown away on every refresh --
 * you could not send someone "last round's leaderboard" at all.
 *
 * Undefined means the active round, and is represented by the parameter being
 * absent rather than by an explicit value, so the common case stays a clean
 * URL. The provider must sit INSIDE the router for useSearchParams to work.
 */
export function RoundProvider({ children }: { children: React.ReactNode }) {
  const [params, setParams] = useSearchParams()

  const selectedRound = useMemo(() => {
    const raw = params.get('round')
    if (!raw) return undefined
    const n = Number(raw)
    return Number.isFinite(n) && n > 0 ? n : undefined
  }, [params])

  const setSelectedRound = useCallback((id: number | undefined) => {
    setParams(prev => {
      const next = new URLSearchParams(prev)
      if (id == null) next.delete('round')
      else next.set('round', String(id))
      return next
      // replace: switching rounds is a filter, not navigation -- it should
      // not stack up entries the back button has to walk through.
    }, { replace: true })
  }, [setParams])

  const value = useMemo(
    () => ({ selectedRound, setSelectedRound }),
    [selectedRound, setSelectedRound],
  )

  return <RoundContext.Provider value={value}>{children}</RoundContext.Provider>
}

export function useRound() {
  return useContext(RoundContext)
}
