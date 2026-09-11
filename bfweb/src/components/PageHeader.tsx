import type { ReactNode } from 'react'
import type { IconComponent } from '../icons'

interface Props {
  title: string
  sub?: string
  right?: ReactNode
  /** Section glyph, shown to the left of the title. */
  icon?: IconComponent
}

export default function PageHeader({ title, sub, right, icon: Icon }: Props) {
  return (
    <div className="page-header">
      <div style={{ display: 'flex', alignItems: 'center', gap: 12, minWidth: 0 }}>
        {Icon && (
          <span className="page-header-icon">
            <Icon size={20} strokeWidth={1.5} />
          </span>
        )}
        <div style={{ minWidth: 0 }}>
          <h1 className="page-header-title">{title}</h1>
          {sub && <p className="page-header-sub">{sub}</p>}
        </div>
      </div>
      {right && <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>{right}</div>}
    </div>
  )
}
