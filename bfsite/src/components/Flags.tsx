import type { FlagId } from '../config/servers'

// Country flags are drawn as plain SVG at 3:2, so they stay crisp at any card
// size. The coalition flags are cloth pictures (public/media/flag-*.png), the
// red one mirrored so both fly toward the middle. NATO and the four-nation
// block it fights are pictures too (bfweb/scripts/gen-flags.py), the Iraqi
// script and Iranian emblem being more than a hand-drawn SVG can carry. All
// of them are backdrop art, blended in at low opacity: close to the real
// thing, not a reference.

/** Coalitions have no national flag; these fly plain blue and red cloth. */
const CLOTH: Partial<Record<FlagId, string>> = {
  blufor: '/media/flag-blufor.png',
  redfor: '/media/flag-redfor.png',
}

/** Flat flag pictures, masked like the SVG ones. */
const PICTURE: Partial<Record<FlagId, string>> = {
  nato: '/media/flag-nato.png',
  rsii: '/media/flag-rsii.png',
}

function Georgia() {
  // The five-cross flag: a red St George's cross with a smaller cross in each quarter.
  const small = (cx: number, cy: number) => (
    <g key={`${cx}-${cy}`} fill="#ff0000">
      <rect x={cx - 15} y={cy - 4.5} width={30} height={9} />
      <rect x={cx - 4.5} y={cy - 15} width={9} height={30} />
    </g>
  )
  return (
    <>
      <rect width="300" height="200" fill="#ffffff" />
      <rect x="0" y="80" width="300" height="40" fill="#ff0000" />
      <rect x="130" y="0" width="40" height="200" fill="#ff0000" />
      {[small(65, 40), small(235, 40), small(65, 160), small(235, 160)]}
    </>
  )
}

function Russia() {
  return (
    <>
      <rect width="300" height="67" fill="#ffffff" />
      <rect y="66" width="300" height="68" fill="#0039a6" />
      <rect y="133" width="300" height="67" fill="#d52b1e" />
    </>
  )
}

function FlagArt({ id }: { id: FlagId }) {
  switch (id) {
    case 'georgia': return <Georgia />
    case 'russia': return <Russia />
    default: return null
  }
}

function Flag({ id, side }: { id: FlagId; side: 'left' | 'right' }) {
  const cloth = CLOTH[id]
  if (cloth) return <img className={`flag flag-${side} flag-cloth`} src={cloth} alt="" />
  const picture = PICTURE[id]
  if (picture) return <img className={`flag flag-${side} flag-picture flag-${id}`} src={picture} alt="" />
  return (
    <svg className={`flag flag-${side}`} viewBox="0 0 300 200" preserveAspectRatio="xMidYMid slice">
      <FlagArt id={id} />
    </svg>
  )
}

/** Two flags blended into a card's background, one from each edge, with a fabric sheen. */
export default function FlagBackdrop({ flags }: { flags: [FlagId, FlagId] }) {
  return (
    <div className="flag-backdrop" aria-hidden="true">
      <Flag id={flags[0]} side="left" />
      <Flag id={flags[1]} side="right" />
      <div className="flag-sheen" />
    </div>
  )
}
