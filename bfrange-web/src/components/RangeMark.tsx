import { useId } from 'react'

/**
 * The range's brand mark, from the Vector Strike logo: the same ring broken
 * at the top right, the same strike arrow leaving through the gap, but the
 * letters inside are swapped for the range itself — a dashed scoring ring,
 * the range's cross ticks and the amber shack (the meatball colour) where
 * the arrow starts. The arrow is cut out of everything under it with a mask,
 * so the mark sits on any background.
 *
 * `compact` is the favicon drawing (thicker ring, no ticks or scoring ring)
 * for sizes under ~24 px. As a loading indicator the scoring ring turns.
 * The files in public/brand/ are the same drawing.
 */
const ARROW = 'M61 3 54.29 18.91 54.85 11.69 13 51 52.31 9.15 45.09 9.71Z'
const ARROW_COMPACT = 'M61 3 54.29 18.91 55.2 12.1 10.5 53.5 51.9 8.8 45.09 9.71Z'
const RING = 'M55.84 19.32A27 27 0 1 1 44.68 8.16'

export function RangeMark({
  size = 32,
  loading = false,
  compact = size < 24,
  title,
}: {
  size?: number
  loading?: boolean
  compact?: boolean
  title?: string
}) {
  const id = useId().replace(/:/g, '')
  const arrow = compact ? ARROW_COMPACT : ARROW
  return (
    <svg
      className={`rmark${loading ? ' loading' : ''}`}
      width={size}
      height={size}
      viewBox="0 0 64 64"
      role={title ? 'img' : undefined}
      aria-label={title}
      aria-hidden={title ? undefined : true}
    >
      <defs>
        <mask id={`rm-${id}`} maskUnits="userSpaceOnUse" x="0" y="0" width="64" height="64">
          <rect width="64" height="64" fill="#fff" />
          <path d={arrow} fill="#000" stroke="#000" strokeWidth={compact ? 3.4 : 2.8} strokeLinejoin="round" />
        </mask>
      </defs>
      <g mask={`url(#rm-${id})`}>
        <g fill="none" stroke="var(--mark-ring, var(--chalk))" strokeLinecap="round">
          <path d={RING} strokeWidth={compact ? 5 : 3.2} />
          {!compact && (
            <>
              <circle className="rmark-score" cx="32" cy="32" r="15.5" strokeWidth="1.9" strokeDasharray="3.2 2.9" opacity=".7" />
              <path d="M32 2.5V10M32 54v7.5M2.5 32H10" strokeWidth="2.6" />
            </>
          )}
        </g>
        <circle cx="32" cy="32" r={compact ? 7.5 : 5.4} fill="var(--ball)" />
      </g>
      <path d={arrow} fill="var(--ball)" />
    </svg>
  )
}

/** Mark plus the two-line name, as the top bar and the print header use it. */
export function RangeLockup({ size = 34, sub = 'Vector Strike' }: { size?: number; sub?: string }) {
  return (
    <span className="flex items-center gap-2.5">
      <RangeMark size={size} />
      <span className="flex flex-col leading-none">
        <span className="caps" style={{ fontSize: 9, letterSpacing: '0.26em' }}>{sub}</span>
        <span className="display" style={{ fontSize: size * 0.68, color: 'var(--chalk)', letterSpacing: '0.06em' }}>Range</span>
      </span>
    </span>
  )
}
