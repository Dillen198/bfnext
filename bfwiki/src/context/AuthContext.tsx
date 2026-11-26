import type { ReactNode } from 'react'
import { AuthProvider as SharedAuthProvider } from '@shared/web/auth'
import { api } from '../api'

// eslint-disable-next-line react-refresh/only-export-components
export { useAuth } from '@shared/web/auth'

export function AuthProvider({ children }: { children: ReactNode }) {
  return <SharedAuthProvider source={api.auth}>{children}</SharedAuthProvider>
}
