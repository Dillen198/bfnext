// Light/dark theme for every front end: the provider, the hook and the toggle
// button. Each app passes its own options (storage key, default, how the
// theme reaches the document) once, as a module-level constant.

import { createContext, useCallback, useContext, useEffect, useState, type CSSProperties, type ReactNode } from 'react'
import { Moon, Sun } from '../icons'

export type Theme = 'dark' | 'light'

export interface ThemeOptions {
  /** localStorage key. bfsite's pre-paint script in index.html reads its key
   *  too; keep the two in step. */
  storageKey: string
  /** The theme when nothing is stored. Omitted: follow the OS setting. */
  fallback?: Theme
  /** Only remember a theme the user picked with the toggle; until then the
   *  page follows the OS. Otherwise the current theme is always stored. */
  rememberChoiceOnly?: boolean
  /** Honour `?theme=dark|light` over everything else. The dashboard needs it:
   *  the Discord snapshot is rendered by a headless browser with a fresh
   *  profile (which reports a light OS theme) and must come out dark. */
  urlParam?: boolean
  /** Put the theme on the document. Default: `<html data-theme="dark|light">`. */
  apply?: (theme: Theme) => void
}

/** `?theme=dark|light`, if the URL names one. */
// eslint-disable-next-line react-refresh/only-export-components
export function urlTheme(): Theme | null {
  try {
    const t = new URLSearchParams(window.location.search).get('theme')
    return t === 'dark' || t === 'light' ? t : null
  } catch {
    return null
  }
}

/** The theme a page should open in. Exported for pre-render code that sets
 *  `data-theme` before React mounts. */
// eslint-disable-next-line react-refresh/only-export-components
export function initialTheme(opts: ThemeOptions): Theme {
  const fromUrl = opts.urlParam ? urlTheme() : null
  if (fromUrl) return fromUrl
  try {
    const stored = localStorage.getItem(opts.storageKey)
    if (stored === 'dark' || stored === 'light') return stored
  } catch { /* private mode / storage blocked */ }
  if (opts.fallback) return opts.fallback
  return window.matchMedia?.('(prefers-color-scheme: light)').matches ? 'light' : 'dark'
}

function store(key: string, theme: Theme) {
  try { localStorage.setItem(key, theme) } catch { /* not persisted; the toggle still works */ }
}

interface ThemeCtx {
  theme: Theme
  toggle: () => void
}

const ThemeContext = createContext<ThemeCtx>({ theme: 'dark', toggle: () => {} })

export function ThemeProvider({ options, children }: { options: ThemeOptions; children: ReactNode }) {
  const [theme, setTheme] = useState<Theme>(() => initialTheme(options))

  useEffect(() => {
    if (options.apply) options.apply(theme)
    else document.documentElement.setAttribute('data-theme', theme)
    if (!options.rememberChoiceOnly) store(options.storageKey, theme)
  }, [theme, options])

  const toggle = useCallback(() => {
    setTheme(t => {
      const next = t === 'dark' ? 'light' : 'dark'
      if (options.rememberChoiceOnly) store(options.storageKey, next)
      return next
    })
  }, [options])

  return <ThemeContext.Provider value={{ theme, toggle }}>{children}</ThemeContext.Provider>
}

// eslint-disable-next-line react-refresh/only-export-components
export function useTheme(): ThemeCtx {
  return useContext(ThemeContext)
}

/**
 * The sun/moon button. Styled as a small bordered square by default; pass
 * `bare` to style it entirely from `className` (bfsite's .icon-btn).
 */
export function ThemeToggle({ className, style, bare = false }: {
  className?: string
  style?: CSSProperties
  bare?: boolean
}) {
  const { theme, toggle } = useTheme()
  const label = theme === 'dark' ? 'Switch to light theme' : 'Switch to dark theme'
  return (
    <button
      type="button"
      onClick={toggle}
      className={className}
      title={label}
      aria-label={label}
      style={bare ? style : {
        display: 'flex', alignItems: 'center', justifyContent: 'center',
        width: 30, height: 30, flexShrink: 0,
        background: 'none', border: '1px solid var(--border-light)', borderRadius: 4,
        color: 'var(--text-muted)', cursor: 'pointer',
        ...style,
      }}
    >
      {theme === 'dark' ? <Sun size={14} /> : <Moon size={14} />}
    </button>
  )
}
