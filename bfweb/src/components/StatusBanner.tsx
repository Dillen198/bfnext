import { useQuery } from '@tanstack/react-query'
import { Alert, ExternalLink } from '@icons'
import { api } from '../api'
import { useInstance } from '../context/InstanceContext'

// ─────────────────────────────────────────────────────────────────────────────
// Server status banner, fed from outside bfdb.
//
// The notices ("down for a RAM upgrade, back Sunday") live in bfsite's
// public/status.json, served by the public site's static host -- so they
// are readable when the game host, and bfdb with it, is down. That is the
// one time a dashboard most needs to explain itself. The public site's
// /status page reads the same file. With no notice written, the banner
// still says what the probe sees: bfdb not answering, or the engine down.
// ─────────────────────────────────────────────────────────────────────────────

const STATUS_URL: string = import.meta.env.VITE_STATUS_URL ?? 'https://www.vectorstrike.org/status.json'
const STATUS_PAGE = new URL('/status', STATUS_URL).href

type Level = 'outage' | 'degraded' | 'maintenance' | 'info'

interface Notice {
  id: string
  level: Level
  title: string
  message?: string
  servers?: string[]
  eta?: string
}

interface Maintenance {
  title: string
  servers?: string[]
  start: string
  end: string
}

interface StatusFile {
  notices?: Notice[]
  maintenance?: Maintenance[]
}

const RANK: Record<Level, number> = { outage: 0, maintenance: 1, degraded: 2, info: 3 }

const TONE: Record<Level, { fg: string; bg: string }> = {
  outage:      { fg: 'var(--red)',    bg: 'var(--red-bg)' },
  maintenance: { fg: 'var(--yellow)', bg: 'rgba(201,162,39,0.10)' },
  degraded:    { fg: 'var(--yellow)', bg: 'rgba(201,162,39,0.10)' },
  info:        { fg: 'var(--accent)', bg: 'var(--accent-glow)' },
}

function fmtUtc(iso?: string): string | undefined {
  if (!iso) return undefined
  const d = new Date(iso)
  if (Number.isNaN(d.getTime())) return undefined
  return d.toLocaleString('en-GB', { weekday: 'short', day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit', timeZone: 'UTC' }) + ' UTC'
}

export default function StatusBanner() {
  const { current } = useInstance()
  const file = useQuery({
    queryKey: ['status-file'],
    queryFn: async (): Promise<StatusFile> => {
      const res = await fetch(STATUS_URL, { cache: 'no-store' })
      if (!res.ok) throw new Error(`${res.status}`)
      return res.json()
    },
    refetchInterval: 60_000,
    retry: 1,
  })
  // Same key and options as the topbar's live badge, so this adds no request.
  const health = useQuery({
    queryKey: ['health'],
    queryFn: api.health,
    refetchInterval: 30_000,
    retry: false,
  })

  const id = current?.id
  const applies = (servers?: string[]) => !servers?.length || !id || servers.includes(id)
  // The file is refetched every minute; its fetch time is "now" to within that.
  const now = file.dataUpdatedAt
  const running: Notice[] = (file.data?.maintenance ?? [])
    .filter(m => Date.parse(m.start) <= now && Date.parse(m.end) > now)
    .map(m => ({ id: m.start, level: 'maintenance', title: m.title, servers: m.servers, eta: m.end }))
  const top = [...(file.data?.notices ?? []), ...running]
    .filter(n => applies(n.servers))
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
