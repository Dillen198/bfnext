import type { ReactNode } from 'react'
import { useLocation } from 'react-router-dom'
import type { IconComponent } from '@icons'
import type { WireframeName } from '../lib/wireframe'
import { keyFor } from './shell/keys'

interface Props {
  title: string
  sub?: string
  right?: ReactNode
  /** Section glyph, shown to the left of the title. */
  icon?: IconComponent
  /** Kept for the pages that pass it: the 1985 headers carry no aircraft
   *  wireframes, they are file folders. */
  art?: WireframeName
  /** A rubber stamp beside the title ("RESTRICTED", "CONFIRMED"). */
  stamp?: ReactNode
}

/**
 * A page's header as the folder it is filed in: the function key that opens
 * it and the file's name above a stencilled title, a typed line under it.
 * Folder-coloured on paper, a label plate on the consoles.
 */
export default function PageHeader({ title, sub, right, icon: Icon, stamp }: Props) {
  const { pathname } = useLocation()
  const key = keyFor(pathname)
  return (
    <div className="page-header">
      <div style={{ display: 'flex', alignItems: 'center', gap: 12, minWidth: 0 }}>
        {Icon && (
          <span className="page-header-icon">
            <Icon size={20} strokeWidth={1.5} />
          </span>
        )}
        <div style={{ minWidth: 0 }}>
          <div className="page-header-file">
            {key && <span className="rsr-fkey-code">{key.code}</span>}
            <span>File · {key ? key.file : 'Southern Flank 1985'}</span>
          </div>
          <div style={{ display: 'flex', alignItems: 'center', flexWrap: 'wrap', rowGap: 4 }}>
            <h1 className="page-header-title">{title}</h1>
            {stamp && <span className="page-header-stamp">{stamp}</span>}
          </div>
          {sub && <p className="page-header-sub">{sub}</p>}
        </div>
      </div>
      {right && <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>{right}</div>}
    </div>
  )
}
