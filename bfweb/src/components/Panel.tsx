import type { CSSProperties, ReactNode } from 'react'
import type { IconComponent } from '@icons'

/**
 * The one card chrome for the whole dashboard.
 *
 * Six pages had grown their own near-identical `Card` / `CardHeader` /
 * `Section` (Objectives, SituationTab, KneeboardTab, AdminPage, Dashboard,
 * AboutPage), each with slightly different padding, title size and icon
 * treatment -- which is most of why the dashboard read as several products
 * stapled together. They all render `.vs-card` with a bordered header, so
 * this replaces them.
 *
 * Two header sizes only: `md` (the default, for a page-level card) and `sm`
 * (for a dense panel inside a column, the Dashboard's old `Panel`).
 */
export interface PanelProps {
  title: string
  icon?: IconComponent
  /** Icon tint. Defaults to the muted body colour, not the accent, so a
   *  page full of panels does not turn into a wall of green. */
  iconColor?: string
  /** Right-aligned count, rendered monospace. */
  count?: number | string
  /** Right-aligned arbitrary content (a badge, a control). */
  right?: ReactNode
  size?: 'sm' | 'md'
  /** Applied to the card, not the body. */
  style?: CSSProperties
  className?: string
  /** Set when the body scrolls and the card owns a fixed height. */
  bodyStyle?: CSSProperties
  children?: ReactNode
}

export default function Panel({
  title, icon: Icon, iconColor, count, right,
  size = 'md', style, className = '', bodyStyle, children,
}: PanelProps) {
  const sm = size === 'sm'
  return (
    // flexShrink: 0 is load-bearing. `overflow: hidden` makes a flex item's
    // automatic minimum size resolve to 0 instead of its content height, so
    // inside the page's flex column the card collapsed to its borders. The
    // caller's `style` still wins -- Dashboard's panels pass their own `flex`,
    // which re-enables shrinking where the layout depends on it.
    <div className={`vs-card ${className}`} style={{ overflow: 'hidden', flexShrink: 0, ...style }}>
      <div
        style={{
          display: 'flex', alignItems: 'center', gap: 8,
          padding: sm ? '7px 13px' : '11px 18px',
          borderBottom: '1px solid var(--border)',
          flexShrink: 0,
        }}
      >
        {Icon && <Icon size={sm ? 11 : 13} style={{ color: iconColor ?? 'var(--text-dim)', flexShrink: 0 }} />}
        <span
          style={{
            fontSize: sm ? '0.63rem' : '0.66rem',
            fontWeight: 700,
            letterSpacing: sm ? '0.2em' : '0.12em',
            textTransform: 'uppercase',
            color: 'var(--text-muted)',
            fontFamily: sm ? 'var(--font-mono)' : undefined,
            flex: 1,
            minWidth: 0,
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
          }}
        >
          {title}
        </span>
        {right}
        {count != null && (
          <span
            className="font-mono-vs"
            style={{ fontSize: '0.63rem', color: 'var(--text-dim)', flexShrink: 0 }}
          >
            {count}
          </span>
        )}
      </div>
      {children != null && <div style={bodyStyle}>{children}</div>}
    </div>
  )
}
