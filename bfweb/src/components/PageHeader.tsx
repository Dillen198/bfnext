import type { ReactNode } from 'react'

interface Props {
  title: string
  sub?: string
  right?: ReactNode
}

export default function PageHeader({ title, sub, right }: Props) {
  return (
    <div className="page-header">
      <div>
        <h1 className="page-header-title">{title}</h1>
        {sub && <p className="page-header-sub">{sub}</p>}
      </div>
      {right && <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>{right}</div>}
    </div>
  )
}
