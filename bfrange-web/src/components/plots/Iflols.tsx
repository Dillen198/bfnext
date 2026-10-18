/**
 * The lens as the pilot sees it: the IFLOLS source's twelve cells with the
 * ball (red in the bottom two), ten green datum lights either side level
 * with the middle of the source, the red wave-off lights beside the source
 * (lit on a wave-off) and the green cut lights over it. Drawn the same as
 * the lens on bfdb's LSO card.
 */
import { LENS_CELLS, ballPos, litCells } from '../../lib/lens'

/** `width` in px, or omit it to fill the container. */
export function Iflols({ gse, waveoff = false, width, title }: { gse: number | null; waveoff?: boolean; width?: number; title?: string }) {
  const cell = 7, srcW = 15, srcH = cell * LENS_CELLS
  const half = srcW / 2 + 22 + 9 * 7
  const W = 2 * (half + 6), H = srcH + 40
  const cx = W / 2, top = 26
  const cy = top + srcH / 2
  const ball = gse === null ? null : ballPos(gse)
  const lit = ball && !ball.off ? litCells(ball.p) : []
  const red = lit.some(i => i >= LENS_CELLS - 2)
  return (
    <svg viewBox={`0 0 ${W} ${H}`} width={width ?? '100%'} height={width ? (width * H) / W : undefined} role="img" aria-label={title ?? 'The lens'} style={{ display: 'block', maxWidth: '100%' }}>
      <rect x={0.5} y={0.5} width={W - 1} height={H - 1} rx={4} fill="#101317" stroke="#2c3e51" />
      {Array.from({ length: LENS_CELLS }, (_, i) => {
        const on = lit.includes(i)
        const low = i >= LENS_CELLS - 2
        const fill = on ? (low ? '#ff3b30' : '#ffb000') : low ? '#3a1d1b' : '#3a3020'
        return <rect key={i} x={cx - srcW / 2} y={top + i * cell + 0.6} width={srcW} height={cell - 1.2} rx={1} fill={fill} />
      })}
      {ball && !ball.off && <circle cx={cx} cy={top + ball.p * cell} r={cell * 1.3} fill={red ? '#ff3b30' : '#ffb000'} opacity={0.35} />}
      {[-1, 1].map(side => (
        <g key={side}>
          {Array.from({ length: 10 }, (_, j) => <circle key={j} cx={cx + side * (srcW / 2 + 22 + j * 7)} cy={cy} r={2.6} fill="#2fd16b" />)}
          {Array.from({ length: 4 }, (_, j) => <circle key={`w${j}`} cx={cx + side * (srcW / 2 + 10)} cy={top + 2 + j * 7} r={2.6} fill={waveoff ? '#ff3b30' : '#4a2422'} />)}
        </g>
      ))}
      {/* off the lens: nothing shows on the real thing; mark which way it went */}
      {ball?.off && (gse ?? 0) > 0 && <path d={`M${cx - 6} ${top - 3}L${cx} ${top - 9}L${cx + 6} ${top - 3}Z`} fill="#ffb000" />}
      {ball?.off && (gse ?? 0) < 0 && <path d={`M${cx - 6} ${top + srcH + 3}L${cx} ${top + srcH + 9}L${cx + 6} ${top + srcH + 3}Z`} fill="#ff3b30" />}
      {Array.from({ length: 4 }, (_, j) => <circle key={`c${j}`} cx={cx + (j - 1.5) * 7} cy={top - 13} r={2.4} fill="#24452f" />)}
    </svg>
  )
}
