/**
 * Every symbol the F10 map draws, drawn again for the wiki.
 *
 * The page used to describe these in Markdown tables — "Chevron on a convoy",
 * "Burst star in a circle" — which asks the reader to imagine the thing and
 * then recognise it at 200 knots. These are the same shapes, in the same
 * colours, so the page shows rather than tells.
 *
 * Palette is the engine's (`dcso3::Color::violet/azure`, `bflib/src/db/mapcolor.rs`)
 * and matches `bfweb`'s BriefingMap, so the game, the dashboard and the wiki
 * all read alike.
 */

export const SIDE = {
  Blue: '#40a6ff',
  Red: '#b84dff',
  Neutral: '#ffffff',
} as const
export const GOLD = '#ffd24a'
export const WARN = '#f0a030'
export const BAD = '#f22a2a'
export const OK = '#33d94f'
const DIM = 'rgba(255,255,255,0.55)'

type Side = keyof typeof SIDE

/** One 48x48 glyph. `side` tints the symbols that are coalition-coloured. */
function Glyph({ icon, side = 'Blue' }: { icon: string; side?: Side }) {
  const c = SIDE[side] ?? SIDE.Blue
  const S = 48
  const common = { width: S, height: S, viewBox: '0 0 48 48' as const }

  switch (icon) {
    // ---- the rings around an objective ---------------------------------
    case 'ring-owner':
      return (
        <svg {...common} aria-hidden>
          <circle cx="24" cy="24" r="17" fill="none" stroke={c} strokeWidth="2.5" strokeDasharray="5 4" />
        </svg>
      )
    case 'ring-unlimited':
      return (
        <svg {...common} aria-hidden>
          <circle cx="24" cy="24" r="17" fill="none" stroke={GOLD} strokeWidth="3.5" strokeDasharray="5 4" />
        </svg>
      )
    case 'ring-capturable':
      return (
        <svg {...common} aria-hidden>
          <circle cx="24" cy="24" r="17" fill="none" stroke={c} strokeWidth="2" strokeDasharray="5 4" opacity="0.5" />
          <circle cx="24" cy="24" r="10" fill="none" stroke="#ffffff" strokeWidth="2.5" />
        </svg>
      )
    case 'ring-threatened':
      return (
        <svg {...common} aria-hidden>
          <circle cx="24" cy="24" r="17" fill="none" stroke={WARN} strokeWidth="4.5" />
        </svg>
      )
    case 'ring-attacked':
      return (
        <svg {...common} aria-hidden>
          <circle cx="24" cy="24" r="17" fill="none" stroke={BAD} strokeWidth="4.5" />
        </svg>
      )

    // ---- lines and routes ----------------------------------------------
    case 'supply-arrow':
      return (
        <svg {...common} aria-hidden>
          <line x1="6" y1="24" x2="34" y2="24" stroke={c} strokeWidth="3" opacity="0.5" />
          <polygon points="42,24 32,18 32,30" fill={c} opacity="0.5" />
        </svg>
      )
    case 'frontline':
      return (
        <svg {...common} aria-hidden>
          <path d="M4 34 L16 22 L30 30 L44 14" fill="none" stroke="#ffffff" strokeWidth="2.5" strokeDasharray="6 5" />
        </svg>
      )
    case 'jtac-line':
      return (
        <svg {...common} aria-hidden>
          <circle cx="7" cy="34" r="3" fill={c} />
          <line x1="10" y1="32" x2="33" y2="16" stroke={c} strokeWidth="2" strokeDasharray="7 3 2 3" />
          <polygon points="38,13 43,18 38,23 33,18" fill="none" stroke={BAD} strokeWidth="2.5" />
        </svg>
      )
    case 'convoy-chevron':
      return (
        <svg {...common} aria-hidden>
          <line x1="5" y1="24" x2="40" y2="24" stroke={c} strokeWidth="2" opacity="0.6" />
          <polyline points="26,15 36,24 26,33" fill="none" stroke={c} strokeWidth="3.5" strokeLinecap="round" />
        </svg>
      )
    case 'task-star':
      return (
        <svg {...common} aria-hidden>
          <circle cx="24" cy="24" r="16" fill="none" stroke={GOLD} strokeWidth="2" strokeDasharray="4 3" />
          <polygon
            points="24,12 27.2,20.4 36,20.8 29.2,26.4 31.4,35 24,30.2 16.6,35 18.8,26.4 12,20.8 20.8,20.4"
            fill={GOLD}
          />
        </svg>
      )

    // ---- warnings and events -------------------------------------------
    case 'arty-burst':
      return (
        <svg {...common} aria-hidden>
          <circle cx="24" cy="24" r="16" fill="none" stroke={BAD} strokeWidth="2" strokeDasharray="4 3" />
          <g stroke={BAD} strokeWidth="2.5" strokeLinecap="round">
            <line x1="24" y1="11" x2="24" y2="19" />
            <line x1="24" y1="29" x2="24" y2="37" />
            <line x1="11" y1="24" x2="19" y2="24" />
            <line x1="29" y1="24" x2="37" y2="24" />
            <line x1="15" y1="15" x2="20" y2="20" />
            <line x1="28" y1="28" x2="33" y2="33" />
            <line x1="33" y1="15" x2="28" y2="20" />
            <line x1="20" y1="28" x2="15" y2="33" />
          </g>
        </svg>
      )
    case 'recon-contacts':
      return (
        <svg {...common} aria-hidden>
          <rect x="5" y="14" width="38" height="20" fill="none" stroke={DIM} strokeWidth="1.5" strokeDasharray="4 3" />
          {[13, 24, 35].map(x => (
            <polygon key={x} points={`${x},18 ${x + 5},24 ${x},30 ${x - 5},24`} fill={SIDE.Red} fillOpacity="0.55" stroke={SIDE.Red} strokeWidth="1.5" />
          ))}
        </svg>
      )
    case 'csar':
      return (
        <svg {...common} aria-hidden>
          <circle cx="20" cy="24" r="12" fill="none" stroke="#ffffff" strokeWidth="2.5" />
          <polygon points="36,15 43,19 43,27 36,31 29,27 29,19" fill={WARN} fillOpacity="0.5" stroke={WARN} strokeWidth="2" />
        </svg>
      )
    case 'cap-threat':
      return (
        <svg {...common} aria-hidden>
          <circle cx="24" cy="24" r="20" fill={c} fillOpacity="0.1" stroke={c} strokeWidth="2.5" strokeDasharray="8 4" />
          <path d="M14 26 L24 18 L34 26" fill="none" stroke={c} strokeWidth="2.5" strokeLinecap="round" />
        </svg>
      )
    case 'ambush':
      return (
        <svg {...common} aria-hidden>
          <line x1="2" y1="36" x2="46" y2="26" stroke={DIM} strokeWidth="2" />
          <circle cx="26" cy="27" r="8" fill={BAD} fillOpacity="0.2" stroke={BAD} strokeWidth="2.5" />
        </svg>
      )
    case 'missile':
      return (
        <svg {...common} aria-hidden>
          <circle cx="24" cy="24" r="13" fill={BAD} fillOpacity="0.12" stroke={BAD} strokeWidth="2.5" strokeDasharray="5 4" />
          <line x1="24" y1="17" x2="24" y2="31" stroke={BAD} strokeWidth="2.5" strokeLinecap="round" />
          <line x1="17" y1="24" x2="31" y2="24" stroke={BAD} strokeWidth="2.5" strokeLinecap="round" />
        </svg>
      )

    // ---- intel contacts -------------------------------------------------
    case 'contact-diamond':
      return <svg {...common} aria-hidden><polygon points="24,9 39,24 24,39 9,24" fill={c} fillOpacity="0.35" stroke={c} strokeWidth="2.5" /></svg>
    case 'contact-square':
      return <svg {...common} aria-hidden><rect x="11" y="11" width="26" height="26" fill={c} fillOpacity="0.35" stroke={c} strokeWidth="2.5" /></svg>
    case 'contact-triangle':
      return <svg {...common} aria-hidden><polygon points="24,9 39,36 9,36" fill={c} fillOpacity="0.35" stroke={c} strokeWidth="2.5" /></svg>
    case 'contact-hex':
      return <svg {...common} aria-hidden><polygon points="24,10 35,17 35,31 24,38 13,31 13,17" fill={c} fillOpacity="0.35" stroke={c} strokeWidth="2.5" /></svg>
    case 'contact-hex-large':
      return <svg {...common} aria-hidden><polygon points="24,5 40,14 40,34 24,43 8,34 8,14" fill={c} fillOpacity="0.35" stroke={c} strokeWidth="3" /></svg>
    case 'contact-octagon':
      return <svg {...common} aria-hidden><polygon points="17,8 31,8 40,17 40,31 31,40 17,40 8,31 8,17" fill={DIM} fillOpacity="0.25" stroke={DIM} strokeWidth="2.5" /></svg>

    default:
      return <svg {...common} aria-hidden><circle cx="24" cy="24" r="4" fill={DIM} /></svg>
  }
}

export interface SymbolRow {
  icon: string
  side?: Side
  name: string
  text: string
}

/** A drawn legend: the symbol, what it is called, and what it means. */
export default function MapSymbolList({ rows }: { rows: SymbolRow[] }) {
  return (
    <figure className="wiki-symbols">
      {rows.map((r, i) => (
        <div className="wiki-symbol-row" key={`${r.icon}-${i}`}>
          <div className="wiki-symbol-art">
            <Glyph icon={r.icon} side={r.side} />
          </div>
          <div className="wiki-symbol-copy">
            <strong>{r.name}</strong>
            <span>{r.text}</span>
          </div>
        </div>
      ))}
    </figure>
  )
}
