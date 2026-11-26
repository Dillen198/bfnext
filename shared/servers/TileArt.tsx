import type { ServerProfile } from './profiles'

// A server tile's picture: a small piece of its server's page, for the
// site's tiles and the wiki's. Styles are in tiles.css beside this.

/** The tile's picture: a small piece of its server's page. */
export function TileArt({ p }: { p: ServerProfile }) {
  if (p.theme === 'desert') {
    return (
      <div className="tile-art" aria-hidden="true">
        <span className="ta-tape">Operation Bronze Helm</span>
      </div>
    )
  }
  if (p.theme === 'staff') {
    return (
      <div className="tile-art" aria-hidden="true">
        <span className="ta-stamp">Secret</span>
        <span className="ta-file">File RSR / SF-85</span>
      </div>
    )
  }
  return (
    <svg className="tile-art" viewBox="0 0 200 120" aria-hidden="true" preserveAspectRatio="xMaxYMid meet">
      <g className="ta-rose">
        <circle cx="156" cy="62" r="40" />
        <circle cx="156" cy="62" r="30" />
        {Array.from({ length: 36 }, (_, i) => {
          const a = (i * 10 * Math.PI) / 180
          const r2 = i % 9 === 0 ? 30 : 36
          return <line key={i} x1={156 + 40 * Math.sin(a)} y1={62 - 40 * Math.cos(a)} x2={156 + r2 * Math.sin(a)} y2={62 - r2 * Math.cos(a)} />
        })}
        <path className="ta-north" d="M156 26 L160 62 L156 58 L152 62 Z" />
      </g>
      <path className="ta-sector" d="M30 20 h44 v30 h-44 z" />
      <circle className="ta-ring" cx="62" cy="88" r="14" />
    </svg>
  )
}
