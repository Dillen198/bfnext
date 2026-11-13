// A crash screen instead of a blank page, for every front end.
//
// The common crash after a deploy is not a bug at all: a tab opened before the
// deploy asks for a route chunk (`lazy(() => import(...))`) whose hashed file
// name no longer exists, the import rejects, and React unmounts the whole tree.
// That case reloads the page once, which fetches the new index.html and its
// new chunk names. Anything else shows a short message and a Reload button.

import { Component, type ErrorInfo, type ReactNode } from 'react'
import { isChunkLoadError, reloadOnce } from './chunkReload'

interface Props {
  children: ReactNode
  /** Replace the default crash screen. */
  fallback?: (error: Error, reset: () => void) => ReactNode
}

interface State {
  error: Error | null
  reloading: boolean
}

export class ErrorBoundary extends Component<Props, State> {
  state: State = { error: null, reloading: false }

  static getDerivedStateFromError(error: Error): Partial<State> {
    return { error }
  }

  componentDidCatch(error: Error, info: ErrorInfo) {
    if (isChunkLoadError(error) && reloadOnce()) {
      this.setState({ reloading: true })
      return
    }
    console.error('Uncaught render error', error, info.componentStack)
  }

  reset = () => this.setState({ error: null, reloading: false })

  render() {
    const { error, reloading } = this.state
    if (!error) return this.props.children
    if (this.props.fallback && !reloading) return this.props.fallback(error, this.reset)
    const chunk = isChunkLoadError(error)
    return (
      <div role="alert" style={{
        display: 'flex', flexDirection: 'column', alignItems: 'center', justifyContent: 'center',
        gap: 12, minHeight: 240, padding: 24, textAlign: 'center',
        color: 'var(--text-muted, inherit)', fontSize: '0.9rem',
      }}>
        {reloading ? (
          <p>A new version is out. Reloading&#8230;</p>
        ) : (
          <>
            <p style={{ fontWeight: 600, color: 'var(--text, inherit)' }}>
              {chunk ? 'This page could not load part of itself.' : 'Something went wrong on this page.'}
            </p>
            <p style={{ maxWidth: 480, opacity: 0.8 }}>
              {chunk
                ? 'The site was probably updated while you had it open. Reloading fetches the new version.'
                : error.message || String(error)}
            </p>
            <button
              type="button"
              onClick={() => window.location.reload()}
              style={{
                padding: '6px 14px', borderRadius: 4, cursor: 'pointer',
                border: '1px solid var(--border-light, currentColor)',
                background: 'none', color: 'inherit', font: 'inherit',
              }}
            >
              Reload
            </button>
          </>
        )}
      </div>
    )
  }
}
