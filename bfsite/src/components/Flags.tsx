import type { FlagId } from '../config/servers'

// Flags drawn as plain SVG at 3:2, so they need no image files and stay crisp
// at any card size. They are backdrop art, blended in at low opacity: close
// to the real proportions, not a vexillology reference.

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

/** Coalition banner: field colour plus a NATO frame -- rectangle for friendly, diamond for hostile. */
function Coalition({ side }: { side: 'blue' | 'red' }) {
  const field = side === 'blue' ? '#1d4f8c' : '#8c1d1d'
  const mark = side === 'blue' ? '#9cc3ef' : '#f0a0a0'
  return (
    <>
      <rect width="300" height="200" fill={field} />
      <g fill="none" stroke={mark} strokeWidth="7">
        {side === 'blue' ? (
          <>
            <rect x="95" y="62" width="110" height="76" />
            <path d="M95 62 L205 138 M205 62 L95 138" />
          </>
        ) : (
          <>
            <path d="M150 40 L215 100 L150 160 L85 100 Z" />
            <path d="M118 70 L182 130 M182 70 L118 130" />
          </>
        )}
      </g>
    </>
  )
}

function FlagArt({ id }: { id: FlagId }) {
  switch (id) {
    case 'georgia': return <Georgia />
    case 'russia': return <Russia />
    case 'blufor': return <Coalition side="blue" />
    case 'redfor': return <Coalition side="red" />
  }
}

/** Two flags blended into a card's background, one from each edge, with a fabric sheen. */
export default function FlagBackdrop({ flags }: { flags: [FlagId, FlagId] }) {
  return (
    <div className="flag-backdrop" aria-hidden="true">
      <svg className="flag flag-left" viewBox="0 0 300 200" preserveAspectRatio="xMidYMid slice">
        <FlagArt id={flags[0]} />
      </svg>
      <svg className="flag flag-right" viewBox="0 0 300 200" preserveAspectRatio="xMidYMid slice">
        <FlagArt id={flags[1]} />
      </svg>
      <div className="flag-sheen" />
    </div>
  )
}
