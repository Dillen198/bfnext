import { useState, type ReactNode } from 'react'
import { CheckCircle2 as CheckCircle, Link as LinkIcon } from '@icons'
import type { FrontLine } from '../live'

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

export function StatusTag({ live, known }: { live: boolean; known: boolean }) {
  return (
    <span className={`status${live ? ' is-live' : ''}`}>
      <span className="dot" />
      {!known ? 'No signal' : live ? 'Round live' : 'Standby'}
    </span>
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
