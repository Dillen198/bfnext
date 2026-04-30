import type { ReactNode } from 'react'

interface Props {
  title: string
  sub?: string
  right?: ReactNode
}

export default function PageHeader({ title, sub, right }: Props) {
  return (
    <div
      className="flex items-center justify-between px-5 py-3.5 flex-shrink-0"
      style={{ background: '#0d0d0d', borderBottom: '1px solid var(--border)' }}
    >
      <div>
        <h1
          className="uppercase"
          style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.15rem', letterSpacing: '0.2em', color: 'var(--text)', lineHeight: 1 }}
        >
          {title}
        </h1>
        {sub && <p className="mt-0.5" style={{ fontSize: '0.65rem', color: 'var(--text-muted)', letterSpacing: '0.06em' }}>{sub}</p>}
      </div>
      {right && <div className="flex items-center gap-3">{right}</div>}
    </div>
  )
}
