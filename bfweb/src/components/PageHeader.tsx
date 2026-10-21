import type { ReactNode } from 'react'
import type { IconComponent } from '@icons'
import { useWireframe, type WireframeName } from '../lib/wireframe'

interface Props {
  title: string
  sub?: string
  right?: ReactNode
  /** Section glyph, shown to the left of the title. */
  icon?: IconComponent
  /** Wireframe art along the header's right end. */
  art?: WireframeName
}

function Art({ name, children }: { name: WireframeName; children: ReactNode }) {
  const style = useWireframe(name, { base: 'var(--bg-chrome)', strength: 'soft', position: 'right 16px center', size: 'auto 92%' })
  return <div className="page-header" style={style}>{children}</div>
}

export default function PageHeader({ title, sub, right, icon: Icon, art }: Props) {
  const body = (
    <>
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
    </>
  )
  return art ? <Art name={art}>{body}</Art> : <div className="page-header">{body}</div>
}
