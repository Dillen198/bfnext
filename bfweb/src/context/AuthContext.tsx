import type { ReactNode } from 'react'
import { AuthProvider as SharedAuthProvider } from '@shared/web/auth'
import { api } from '../api'
import { getDevUser, setDevUser } from '../lib/devAuth'

// eslint-disable-next-line react-refresh/only-export-components
export { useAuth } from '@shared/web/auth'

/** The shared provider over bfdb's /auth/me, plus the dashboard's dev
 *  personas (lib/devAuth, dev builds only). */
export function AuthProvider({ children }: { children: ReactNode }) {
  return (
    <SharedAuthProvider
      source={api.auth}
      devUser={getDevUser()}
      // Signing out of a dev persona is local-only; reloading is the simplest
      // way to drop every cached query that was fetched as that identity.
      onDevLogout={() => {
        setDevUser(null)
        window.location.reload()
      }}
    >
      {children}
    </SharedAuthProvider>
  )
}
