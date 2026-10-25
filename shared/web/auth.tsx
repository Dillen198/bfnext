// Who is signed in, for the dashboard and the wiki: the Discord session cookie
// on the API host, read through `/api/auth/me`. (The range site asks its own
// `/api/range/me`, which also says whether the account is linked to a UCID,
// so it keeps its own provider.)

import { createContext, useContext, type ReactNode } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import type { AuthUser } from './api'

interface AuthCtx {
  user:    AuthUser | null
  loading: boolean
  logout:  () => void
  refresh: () => Promise<void>
}

const AuthContext = createContext<AuthCtx>({ user: null, loading: true, logout: () => {}, refresh: async () => {} })

export interface AuthSource {
  me: () => Promise<AuthUser | null>
  logout: () => Promise<unknown>
}

export function AuthProvider({ source, devUser = null, onDevLogout, children }: {
  source: AuthSource
  /** A dev persona (dashboard dev builds only) short-circuits the identity
   *  check: /auth/me is never fired, so the fake user cannot be raced and
   *  replaced by a real (null) answer. */
  devUser?: AuthUser | null
  /** Signing out of a dev persona; there is no server session to end. */
  onDevLogout?: () => void
  children: ReactNode
}) {
  const queryClient = useQueryClient()

  const { data: fetched = null, isLoading } = useQuery<AuthUser | null>({
    queryKey: ['auth', 'me'],
    queryFn: () => source.me(),
    staleTime: 5 * 60_000,
    retry: false,
    enabled: !devUser,
  })

  const user = devUser ?? fetched

  function logout() {
    if (devUser && onDevLogout) {
      onDevLogout()
      return
    }
    source.logout().then(() => {
      queryClient.setQueryData(['auth', 'me'], null)
      queryClient.invalidateQueries({ queryKey: ['auth'] })
    })
  }

  async function refresh() {
    await queryClient.invalidateQueries({ queryKey: ['auth', 'me'] })
  }

  return (
    <AuthContext.Provider value={{ user, loading: isLoading && !devUser, logout, refresh }}>
      {children}
    </AuthContext.Provider>
  )
}

// eslint-disable-next-line react-refresh/only-export-components
export function useAuth(): AuthCtx {
  return useContext(AuthContext)
}
