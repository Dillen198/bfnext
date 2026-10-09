import React, { createContext, useContext } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api } from '../api'
import type { AuthUser } from '../api'
import { getDevUser, setDevUser } from '../lib/devAuth'

interface AuthCtx {
  user:    AuthUser | null
  loading: boolean
  logout:  () => void
  refresh: () => Promise<void>
}

const AuthContext = createContext<AuthCtx>({ user: null, loading: true, logout: () => {}, refresh: async () => {} })

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const queryClient = useQueryClient()

  // A dev persona short-circuits the real identity check entirely -- see
  // lib/devAuth. `enabled: false` keeps /auth/me from firing at all, so the
  // fake user cannot be raced and replaced by a real (null) answer.
  const devUser = getDevUser()

  const { data: fetched = null, isLoading } = useQuery<AuthUser | null>({
    queryKey: ['auth', 'me'],
    queryFn: () => api.auth.me(),
    staleTime: 5 * 60_000,
    retry: false,
    enabled: !devUser,
  })

  const user = devUser ?? fetched

  function logout() {
    // Signing out of a dev persona is local-only; there is no server session
    // to end, and reloading is the simplest way to drop every cached query
    // that was fetched as that identity.
    if (getDevUser()) {
      setDevUser(null)
      window.location.reload()
      return
    }
    api.auth.logout().then(() => {
      queryClient.setQueryData(['auth', 'me'], null)
      queryClient.invalidateQueries({ queryKey: ['auth'] })
    })
  }

  async function refresh() {
    await queryClient.invalidateQueries({ queryKey: ['auth', 'me'] })
  }

  return (
    <AuthContext.Provider value={{ user, loading: isLoading, logout, refresh }}>
      {children}
    </AuthContext.Provider>
  )
}

export function useAuth() {
  return useContext(AuthContext)
}
