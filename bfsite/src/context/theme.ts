import { createContext, useContext } from 'react'

export type Theme = 'dark' | 'light'

export interface ThemeCtx {
  theme: Theme
  toggle: () => void
}

/** Also read by the pre-paint script in index.html; keep the two in step. */
export const STORAGE_KEY = 'bfsite-theme'

export const ThemeContext = createContext<ThemeCtx>({ theme: 'dark', toggle: () => {} })

export function useTheme() {
  return useContext(ThemeContext)
}
