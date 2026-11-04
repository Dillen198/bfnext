import type { CSSProperties, ReactNode } from 'react'
import type { IconComponent } from '@icons'
import { useDashLook } from '../lib/look'

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
  // Levant Ablaze: the name on a strip of tape, as the campaign's cards are
  // (levant.css). Inline rather than over the edge, so it never overlaps a
  // card above it in a tight grid.
  const look = useDashLook()
  const head = look.profile ? (
    <div className={`vs-panel-head lv-panel-head${sm ? ' sm' : ''}`}>
      <span className={`ds-tape${sm ? ' sm' : ''}`}>{title}</span>
      <span style={{ flex: 1 }} />
      {right}
      {count != null && <span className="vs-panel-count">{count}</span>}
    </div>
  ) : (
    <div className={`vs-panel-head${sm ? ' sm' : ''}`}>
      {Icon && <Icon size={sm ? 11 : 13} className="vs-panel-icon" style={{ color: iconColor ?? 'var(--text-dim)' }} />}
      <span className="vs-panel-title">{title}</span>
      {right}
      {count != null && <span className="vs-panel-count font-mono-vs">{count}</span>}
    </div>
  )
  return (
    // The card is a flex column, and that is load-bearing rather than
    // cosmetic: `.vs-card` is a plain block, so a `bodyStyle` of `flex: 1`
    // resolved to nothing and the body took its full content height. Inside a
    // card with `overflow: hidden` that reads as a panel whose list is cut off
    // and cannot be scrolled -- the body's own `overflow: auto` never has a
    // bounded height to scroll within. Stacking a header above a body is what
    // block flow was already doing, so this changes nothing for the panels
    // that do not scroll.
    //
    // flexShrink: 0 is load-bearing too. `overflow: hidden` makes a flex
    // item's automatic minimum size resolve to 0 instead of its content
    // height, so inside the page's flex column the card collapsed to its
    // borders. The caller's `style` still wins -- Dashboard's panels pass
    // their own `flex`, which re-enables shrinking where the layout depends
    // on it.
    <div
      className={`vs-card ${className}`}
      style={{
        display: 'flex',
        flexDirection: 'column',
        overflow: 'hidden',
        flexShrink: 0,
        ...style,
      }}
    >
      {/* Header styles live in index.css (.vs-panel-head). */}
      {head}
      {children != null && <div style={bodyStyle}>{children}</div>}
    </div>
  )
}
