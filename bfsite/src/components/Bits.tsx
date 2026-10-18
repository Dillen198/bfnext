import { useState, type ReactNode } from 'react'
import { CheckCircle2 as CheckCircle, Link as LinkIcon } from '@icons'
import type { FrontLine } from '../live'
import { STATE_LABEL, fmtUtc, stateTone, type Notice, type ServerState } from '../status'

/** A section heading in the shape of an operations-order paragraph. */
export function SectionHead({
  num,
  para,
  title,
  lede,
}: {
  num: number
  para: string
  title: ReactNode
  lede?: ReactNode
}) {
  return (
    <header className="section-head">
      <div className="para">
        <span className="num">{num}.</span>
        <span>{para}</span>
      </div>
      <h2 className="section-title">{title}</h2>
      {lede && <p className="section-lede">{lede}</p>}
    </header>
  )
}

export function StatusTag({ state }: { state: ServerState }) {
  return (
    <span className={`status tone-${stateTone(state)}`}>
      <span className="dot" />
      {STATE_LABEL[state]}
    </span>
  )
}

const LEVEL_LABEL: Record<Notice['level'], string> = {
  outage: 'Outage',
  degraded: 'Degraded',
  maintenance: 'Maintenance',
  info: 'Notice',
}

/** A hand-written status notice, as status.json carries it. */
export function NoticeBox({ notice, compact = false }: { notice: Notice; compact?: boolean }) {
  const eta = fmtUtc(notice.eta)
  const since = fmtUtc(notice.since)
  return (
    <div className={`notice lvl-${notice.level}`} role={notice.level === 'outage' ? 'alert' : undefined}>
      <div className="notice-head">
        <span className="notice-level">{LEVEL_LABEL[notice.level]}</span>
        <strong>{notice.title}</strong>
      </div>
      {!compact && notice.message && <p>{notice.message}</p>}
      {(eta || (since && !compact)) && (
        <div className="notice-meta">
          {since && !compact && <span>Since {since}</span>}
          {eta && <span>{notice.level === 'maintenance' ? 'Until' : 'Expected back'} {eta}</span>}
        </div>
      )}
    </div>
  )
}

/** Objectives by owner as one bar, the dashboard's territory split. */
export function TerritoryBar({
  front,
  loading,
  blueLabel,
  redLabel,
  legend = true,
}: {
  front: FrontLine | undefined
  loading: boolean
  blueLabel: string
  redLabel: string
  legend?: boolean
}) {
  if (!front || front.total === 0) {
    return (
      <div>
        <div className={`terr${loading ? ' is-loading' : ''}`} />
        {legend && (
          <div className="terr-legend">
            <span>{loading ? 'Syncing front line…' : 'Front line unavailable'}</span>
          </div>
        )}
      </div>
    )
  }
  const pct = (n: number) => (n / front.total) * 100
  return (
    <div>
      <div className="terr" role="img" aria-label={`${blueLabel} ${front.blue}, neutral ${front.neutral}, ${redLabel} ${front.red} objectives`}>
        <div className="b" style={{ width: `${pct(front.blue)}%` }} />
        <div className="n" style={{ width: `${pct(front.neutral)}%` }} />
        <div className="r" style={{ width: `${pct(front.red)}%` }} />
      </div>
      {legend && (
        <div className="terr-legend">
          <span className="b">{blueLabel} {front.blue}</span>
          {front.neutral > 0 && <span>Neutral {front.neutral}</span>}
          <span className="r">{redLabel} {front.red}</span>
        </div>
      )}
    </div>
  )
}

/** The exact server-browser name, with a button to copy it. */
export function BrowserName({ name }: { name: string }) {
  const [copied, setCopied] = useState(false)
  const copy = async () => {
    try {
      await navigator.clipboard.writeText(name)
      setCopied(true)
      window.setTimeout(() => setCopied(false), 1600)
    } catch {
      // No clipboard (insecure origin, old browser): the name is on screen to type.
    }
  }
  return (
    <div className="browser-name">
      <code title={name}>{name}</code>
      <button type="button" className="copy-btn" onClick={copy} aria-label={`Copy server name ${name}`}>
        {copied ? <CheckCircle size={11} /> : <LinkIcon size={11} />}
        {copied ? 'Copied' : 'Copy'}
      </button>
    </div>
  )
}
