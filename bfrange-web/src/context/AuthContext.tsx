/**
 * Who is looking at the range site. Same session as bfweb/bfwiki (a Discord
 * OAuth cookie on the API host), read through the range's own `/api/range/me`
 * because that one also answers the question the range cares about: is this
 * Discord account linked to a DCS UCID?
 */
import { createContext, useContext, type ReactNode } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api } from '../api'
import type { Me } from '../types'

interface AuthCtx {
  me: Me | null
  loading: boolean
  login: () => void
  logout: () => void
}

export const AFTER_LOGIN_KEY = 'range.afterLogin'

const Ctx = createContext<AuthCtx>({ me: null, loading: true, login: () => {}, logout: () => {} })

export function AuthProvider({ children }: { children: ReactNode }) {
  const qc = useQueryClient()
  const { data, isLoading } = useQuery({
    queryKey: ['me'],
    queryFn: () => api.me(),
    staleTime: 5 * 60_000,
    retry: false,
  })

  async function login() {
    if (__RANGE_MOCK__) {
      const m = await import('../mock/server')
      m.setMockUser('casper')
      await qc.invalidateQueries()
      return
    }
    // bfdb only accepts return_to = <origin>/, so remember where we were and
    // come back to it after the round trip (see ReturnAfterLogin in App.tsx)
    try { sessionStorage.setItem(AFTER_LOGIN_KEY, window.location.pathname + window.location.search) } catch { /* storage blocked */ }
    window.location.href = api.loginUrl()
  }

  async function logout() {
    await api.logout().catch(() => {})
    qc.setQueryData(['me'], { logged_in: false, ucid: null, name: null, admin: false } satisfies Me)
    await qc.invalidateQueries()
  }

  return (
    <Ctx.Provider value={{ me: data ?? null, loading: isLoading, login, logout }}>
      {children}
    </Ctx.Provider>
  )
}

// eslint-disable-next-line react-refresh/only-export-components
export const useAuth = () => useContext(Ctx)
