import type { ReactNode } from 'react'
import { Alert, RefreshCw } from '@icons'

/**
 * The loading / failed / empty states for a react-query-backed view.
 *
 * Every page in here defaults its data (`const { data: kills = [] } =
 * useQuery(...)`), which means a failed request and a genuinely quiet server
 * render identically: an empty table. That is the wrong answer often enough
 * to matter -- bfdb restarting, the engine RPC timing out and the campaign
 * simply having no kills yet are three different situations, and the viewer
 * can only act on one of them.
 *
 * So: render this above/inside the list and let it decide. It returns `null`
 * when there is data to show, so the normal path is unaffected.
 */
export interface QueryStateProps {
  /** Spread straight from useQuery -- only these three fields are read. */
  isLoading: boolean
  isError?: boolean
  error?: unknown
  /** True when the request succeeded but returned nothing. */
  isEmpty?: boolean
  /** What is being loaded, lower case: "kills", "objectives", "pilots". */
  what: string
  /** Shown instead of the stock wording when there is simply no data yet. */
  emptyText?: string
  /** useQuery's refetch, if the failure should offer a retry. */
  onRetry?: () => void
  /** Render as a full-width table row with this many columns. */
  colSpan?: number
}

/** Network failures surface as a bare TypeError, which is useless to read.
 *  Everything else is either bfdb's own `{error}` body or `HTTP <status>`. */
function describe(error: unknown): { headline: string; detail: string | null } {
  const msg = error instanceof Error ? error.message : String(error ?? '')
  if (/failed to fetch|networkerror|load failed/i.test(msg)) {
    return { headline: 'Cannot reach the server', detail: 'bfdb is not responding.' }
  }
  if (/^HTTP 5\d\d/.test(msg)) {
    return { headline: 'Server error', detail: msg }
  }
  if (/^HTTP 40[13]/.test(msg)) {
    return { headline: 'Not authorised', detail: 'Sign in, or this data is locked to another coalition.' }
  }
  // An unknown /api path falls through to the SPA's index.html, so the fetch
  // succeeds and JSON.parse chokes on "<!doctype". That is not a parse bug --
  // it means this dashboard is newer than the bfdb serving it.
  if (/Unexpected token '<'|not valid JSON|JSON\.parse|Unexpected end of JSON/i.test(msg)) {
    return {
      headline: 'Not available on this server',
      detail: 'The running bfdb does not have this endpoint yet — it likely predates the feature.',
    }
  }
  // A timed-out engine RPC comes back as bfdb's own message, which is
  // already written for a human -- show it as-is.
  return { headline: 'Could not load', detail: msg || null }
}

export default function QueryState({
  isLoading, isError, error, isEmpty, what, emptyText, onRetry, colSpan,
}: QueryStateProps) {
  let body: ReactNode = null

  if (isLoading) {
    body = <span style={{ color: 'var(--text-dim)' }}>Loading {what}…</span>
  } else if (isError) {
    const { headline, detail } = describe(error)
    body = (
      <span style={{ display: 'inline-flex', flexDirection: 'column', alignItems: 'center', gap: 6 }}>
        <span style={{ display: 'inline-flex', alignItems: 'center', gap: 6, color: 'var(--red)', fontWeight: 700 }}>
          <Alert size={12} />
          {headline}
        </span>
        {detail && (
          <span style={{ color: 'var(--text-dim)', fontSize: '0.68rem', maxWidth: 380, lineHeight: 1.5 }}>
            {detail}
          </span>
        )}
        {onRetry && (
          <button
            type="button"
            onClick={onRetry}
            style={{
              display: 'inline-flex', alignItems: 'center', gap: 5, marginTop: 2,
              background: 'transparent', border: '1px solid var(--border)', cursor: 'pointer',
              color: 'var(--text-muted)', font: 'inherit', fontSize: '0.66rem',
              letterSpacing: '0.1em', textTransform: 'uppercase', padding: '3px 9px',
            }}
          >
            <RefreshCw size={11} />
            Retry
          </button>
        )}
      </span>
    )
  } else if (isEmpty) {
    body = <span style={{ color: 'var(--text-dim)' }}>{emptyText ?? `No ${what} yet`}</span>
  } else {
    return null
  }

  const pad = { textAlign: 'center' as const, padding: '2.5rem 1rem', fontSize: '0.75rem' }
  if (colSpan != null) {
    return <tr><td colSpan={colSpan} style={pad}>{body}</td></tr>
  }
  return <div style={pad}>{body}</div>
}
