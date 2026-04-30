import React, { createContext, useContext } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api } from '../api'
import type { AuthUser } from '../api'

interface AuthCtx {
  user:    AuthUser | null
  loading: boolean
  logout:  () => void
}

const AuthContext = createContext<AuthCtx>({ user: null, loading: true, logout: () => {} })

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const queryClient = useQueryClient()

  const { data: user = null, isLoading } = useQuery<AuthUser | null>({
    queryKey: ['auth', 'me'],
    queryFn: () => api.auth.me(),
    staleTime: 5 * 60_000,
    retry: false,
  })

  function logout() {
    api.auth.logout().then(() => {
      queryClient.setQueryData(['auth', 'me'], null)
      queryClient.invalidateQueries({ queryKey: ['auth'] })
    })
  }

  return (
    <AuthContext.Provider value={{ user, loading: isLoading, logout }}>
      {children}
    </AuthContext.Provider>
  )
}

export function useAuth() {
  return useContext(AuthContext)
}
