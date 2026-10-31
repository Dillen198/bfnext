import { useOfflineText } from '@shared/web/snapshot'

// ─────────────────────────────────────────────────────────────────────────────
// What a live-only view (TACMAP, the GCI scope, the log streams) shows while
// bfdb is down. Those views come over a websocket and have no snapshot to fall
// back on, so rather than spin on "connecting" forever they say so. Whether
// bfdb is down is `useOffline().offline` (any request to it just failed).
// ─────────────────────────────────────────────────────────────────────────────

export default function OfflineView({ what }: { what: string }) {
  const text = useOfflineText()
  return (
    <div
      role="status"
      className="flex h-full flex-col items-center justify-center gap-2 p-6 text-center text-sm"
      style={{ background: 'var(--bg)', color: 'var(--text-dim, #94a3b8)' }}
    >
      <div style={{ fontFamily: 'var(--font-display)', letterSpacing: '0.14em', color: 'var(--text)', fontSize: '1.1rem' }}>
        SERVER OFFLINE
      </div>
      <div className="max-w-md leading-relaxed">
        {what} is live from the game server, which is not answering. This page reconnects by itself when it is back.
        {text && <><br />The rest of the dashboard is {text.replace(/^Server offline: s/, 's')}.</>}
      </div>
    </div>
  )
}
