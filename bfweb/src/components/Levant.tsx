import type { ReactNode } from 'react'

// The pieces Levant Ablaze's pages are built from (SITREP, About), named and
// drawn as on the campaign's page on the site. Styles: levant.css.

/** A strip of tape with a word on it. */
export function Tape({ children, tone: t, className = '' }: { children: ReactNode; tone?: 'b' | 'r' | 'rust' | 'warn' | 'n'; className?: string }) {
  return <span className={`ds-tape${t ? ` ${t}` : ''} ${className}`}>{children}</span>
}

/** A section heading: its number in rust, its name in heavy capitals. */
export function Head({ n, children, right }: { n: string; children: ReactNode; right?: ReactNode }) {
  return (
    <div className="lv-head">
      <h2 className="ds-h"><span>{n}</span>{children}</h2>
      {right}
    </div>
  )
}

/** A card with its name on a strip of tape across the top edge. */
export function Card({ tape, tone: t, right, className = '', bodyClass = '', children }: {
  tape: ReactNode; tone?: 'b' | 'r' | 'rust'; right?: ReactNode; className?: string; bodyClass?: string; children: ReactNode
}) {
  return (
    <article className={`lv-card${t === 'b' || t === 'r' ? ` ${t}` : ''} ${className}`}>
      <Tape tone={t}>{tape}</Tape>
      {right && <div className="lv-card-right">{right}</div>}
      <div className={`lv-card-body ${bodyClass}`}>{children}</div>
    </article>
  )
}
