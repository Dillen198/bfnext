/**
 * The brand mark: the carrier's Fresnel lens seen from the groove. Green
 * datum bars either side, red waveoff lights above, the amber "meatball" in
 * the middle cell. As a loading indicator the ball drifts up and down the
 * lens, which is what it does when you chase it.
 */
export function Lens({ size = 28, loading = false, title }: { size?: number; loading?: boolean; title?: string }) {
  return (
    <svg
      className={`lens${loading ? ' loading' : ''}`}
      width={size}
      height={size}
      viewBox="0 0 32 32"
      role={title ? 'img' : undefined}
      aria-label={title}
      aria-hidden={title ? undefined : true}
    >
      <rect x="12" y="3" width="8" height="26" rx="1" fill="var(--panel-3)" stroke="var(--line-2)" strokeWidth="0.75" />
      <rect x="2" y="14.5" width="8" height="3" fill="var(--datum)" />
      <rect x="22" y="14.5" width="8" height="3" fill="var(--datum)" />
      <rect x="4" y="3.5" width="3.5" height="3.5" fill="var(--wave)" />
      <rect x="24.5" y="3.5" width="3.5" height="3.5" fill="var(--wave)" />
      <circle className="lens-ball" cx="16" cy="16" r="3.4" fill="var(--ball)" />
    </svg>
  )
}
