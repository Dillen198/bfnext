import type { ReactNode, Ref } from 'react'
import { useLocation } from 'react-router-dom'
import type { IconComponent } from '@icons'
import { useWireframe, type WireframeName } from '../lib/wireframe'
import { useDashLook } from '../lib/look'
import { Tape } from './Levant'

interface Props {
  title: string
  sub?: string
  right?: ReactNode
  /** Section glyph, shown to the left of the title. */
  icon?: IconComponent
  /** Wireframe art along the header's right end. */
  art?: WireframeName
  /** The header band's element (the rounds page puts it on its PDF). */
  ref?: Ref<HTMLDivElement>
}

/** The sidebar group a page sits in, for the tape over its title. */
function kicker(path: string): string {
  if (/^\/(objectives|map|news)/.test(path)) return 'Operations'
  if (/^\/(briefing|intel|hq|command|ground)/.test(path)) return 'Coalition'
  if (/^\/(leaderboard|pilots|kills|replay|rounds|squadrons|pilot)/.test(path)) return 'Records'
  if (/^\/admin/.test(path)) return 'Admin'
  return 'Vector Strike'
}

function Art({ name, children, ref }: { name: WireframeName; children: ReactNode; ref?: Ref<HTMLDivElement> }) {
  const style = useWireframe(name, { base: 'var(--bg-chrome)', strength: 'soft', position: 'right 16px center', size: 'auto 92%' })
  return <div ref={ref} className="page-header" style={style}>{children}</div>
}

/** In Levant Ablaze's look: a band of the battle art, as SITREP's (levant.css). */
function LevantHeader({ title, sub, right, ref }: Props) {
  const { pathname } = useLocation()
  const words = title.split(' ')
  const last = words.length > 1 ? words.pop() : undefined
  return (
    <div ref={ref} className="page-header lv-pagehead">
      <div className="lv-pagehead-main">
        <Tape tone="rust">{kicker(pathname)}</Tape>
        <h1 className="lv-pagehead-title">{words.join(' ')}{last && <> <span>{last}</span></>}</h1>
        {sub && <p className="lv-pagehead-sub">{sub}</p>}
      </div>
      {right && <div className="lv-pagehead-right">{right}</div>}
    </div>
  )
}

export default function PageHeader(props: Props) {
  const look = useDashLook()
  if (look.profile) return <LevantHeader {...props} />
  const { title, sub, right, icon: Icon, art, ref } = props
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
  return art ? <Art name={art} ref={ref}>{body}</Art> : <div ref={ref} className="page-header">{body}</div>
}
