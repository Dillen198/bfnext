import { useQuery } from '@tanstack/react-query'
import { Alert, ExternalLink } from '@icons'
import { api } from '../api'
import { useInstance } from '../context/InstanceContext'
import { appliesTo, fmtUtc, LEVEL_RANK as RANK, type NoticeLevel as Level, type Notice } from '@shared/web/status'
import { STATUS_PAGE, useStatusFile } from '../lib/statusFile'

// ─────────────────────────────────────────────────────────────────────────────
// Outage banner: one line across every page, only while the server is down.
//
// Everything else a server has to say -- "back online", a degraded service, a
// maintenance window coming up, a round won -- goes to the notification bell
// (NotificationBell.tsx), so the pages stay clear. A banner is kept for the
// one case where the page itself is wrong: the data on it is stale, because
// the server behind it is down. That is:
//   * an `outage` notice in status.json, or a maintenance window running now
//   * bfdb not answering at all, or the selected server's DCS engine down.
// A notice wins over a probe: a probe can only say "offline", a notice says why.
// ─────────────────────────────────────────────────────────────────────────────

const TONE: Record<Level, { fg: string; bg: string }> = {
  outage:      { fg: 'var(--red)',    bg: 'var(--red-bg)' },
  maintenance: { fg: 'var(--yellow)', bg: 'rgba(201,162,39,0.10)' },
  degraded:    { fg: 'var(--yellow)', bg: 'rgba(201,162,39,0.10)' },
  info:        { fg: 'var(--accent)', bg: 'var(--accent-glow)' },
}

export default function StatusBanner() {
  const { current } = useInstance()
  const file = useStatusFile()
  // Same key and options as the topbar's live badge, so this adds no request.
  const health = useQuery({
    queryKey: ['health'],
    queryFn: api.health,
    refetchInterval: 30_000,
    retry: false,
  })

  const id = current?.id
  // The file is refetched every minute; its fetch time is "now" to within that.
  const now = file.dataUpdatedAt
  const running: Notice[] = (file.data?.maintenance ?? [])
    .filter(m => Date.parse(m.start) <= now && Date.parse(m.end) > now)
    .map(m => ({ id: m.start, level: 'maintenance', title: m.title, servers: m.servers, eta: m.end }))
  const top = [...(file.data?.notices ?? []).filter(n => n.level === 'outage'), ...running]
    .filter(n => appliesTo(n.servers, id))
    .sort((a, b) => RANK[a.level] - RANK[b.level])[0]

  let level: Level
  let text: string
  if (top) {
    level = top.level
    const eta = fmtUtc(top.eta)
    text = top.title + (eta ? ` · ${top.level === 'maintenance' ? 'until' : 'expected back'} ${eta}` : '')
  } else if (health.isError) {
    level = 'outage'
    text = 'The game servers aren’t answering. Live data on this dashboard is paused until they’re back.'
  } else if (health.data && !health.data.engine.reachable && health.data.active_round) {
    level = 'degraded'
    text = `${current?.label ?? 'This server'}: the DCS server isn’t answering. What you see is the last known picture.`
  } else {
    return null
  }

  const tone = TONE[level]
  return (
    <div
      role={level === 'outage' ? 'alert' : 'status'}
      style={{
        display: 'flex', alignItems: 'center', gap: 10, flexShrink: 0,
        padding: '7px 16px', background: tone.bg, color: tone.fg,
        borderBottom: `1px solid ${tone.fg}`, fontSize: '0.78rem',
      }}
    >
      <Alert size={13} style={{ flexShrink: 0 }} />
      <span style={{ flex: 1, minWidth: 0, color: 'var(--text)' }}>{text}</span>
      <a
        href={STATUS_PAGE}
        target="_blank"
        rel="noopener noreferrer"
        style={{ display: 'inline-flex', alignItems: 'center', gap: 4, flexShrink: 0, color: tone.fg, fontFamily: 'var(--font-mono)', fontSize: '0.62rem', fontWeight: 700, letterSpacing: '0.14em', textDecoration: 'none' }}
      >
        STATUS <ExternalLink size={10} />
      </a>
    </div>
  )
}
