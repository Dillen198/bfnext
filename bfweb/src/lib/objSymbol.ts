// The plot's objective symbols, after the Southern Flank briefing map: a solid
// shape in the owner's colour, edged in a halo so it reads over the ground,
// one shape per kind (an airbase is a ringed slash, a FOB a triangle, a
// logistics hub a barred square, a factory a sawtooth roof, a naval base a
// diamond with an anchor, the command centre a star, a fleet a ship).

function star(s: number): string {
  let d = ''
  for (let k = 0; k < 10; k++) {
    const r = k % 2 ? s * 0.48 : s * 1.2
    const a = -Math.PI / 2 + (k * Math.PI) / 5
    d += (k ? 'L' : 'M') + (r * Math.cos(a)).toFixed(2) + ' ' + (r * Math.sin(a)).toFixed(2)
  }
  return d + 'Z'
}

/** The shape for `kind` at scale `s` (about its radius, px), centred on 0 0:
 *  `f` the filled parts, `i` the strokes drawn over them in the halo. */
export function symbolShape(kind: string, s: number): { f: string[]; i: string[] } {
  switch (kind) {
    case 'Airbase':
      return { f: [`M${-s} 0A${s} ${s} 0 1 0 ${s} 0A${s} ${s} 0 1 0 ${-s} 0Z`], i: [`M${-s * 0.6} ${s * 0.6}L${s * 0.6} ${-s * 0.6}`] }
    case 'FOB':
    case 'FARP':
      return { f: [`M0 ${-s * 1.15}L${s * 1.05} ${s * 0.75}L${-s * 1.05} ${s * 0.75}Z`], i: [] }
    case 'Logistics Hub':
      return { f: [`M${-s * 0.88} ${-s * 0.88}h${s * 1.76}v${s * 1.76}h${-s * 1.76}Z`], i: [`M${-s * 0.45} 0H${s * 0.45}`] }
    case 'Factory':
      return {
        f: [`M${-s} ${s * 0.85}V${-s * 0.15}L${-s * 0.33} ${-s * 0.75}V${-s * 0.15}L${s * 0.33} ${-s * 0.75}V${-s * 0.15}L${s} ${-s * 0.75}V${s * 0.85}Z`],
        i: [],
      }
    case 'Naval Base':
      return {
        f: [`M0 ${-s * 1.15}L${s * 1.15} 0L0 ${s * 1.15}L${-s * 1.15} 0Z`],
        i: [`M0 ${-s * 0.5}V${s * 0.5}M${-s * 0.45} ${s * 0.1}Q0 ${s * 0.75} ${s * 0.45} ${s * 0.1}`],
      }
    case 'Command Center':
      return { f: [star(s)], i: [] }
    case 'Carrier Group':
      return {
        f: [`M${-s * 1.6} ${-s * 0.05}H${s * 1.6}L${s * 1.15} ${s * 0.7}H${-s * 1.15}Z`, `M${-s * 0.45} ${-s * 0.05}V${-s * 0.7}H${s * 0.35}V${-s * 0.05}Z`],
        i: [],
      }
    default:
      return { f: [`M${-s * 0.7} 0A${s * 0.7} ${s * 0.7} 0 1 0 ${s * 0.7} 0A${s * 0.7} ${s * 0.7} 0 1 0 ${-s * 0.7} 0Z`], i: [] }
  }
}

/** The symbol as standalone SVG markup, and the square it fills, px. */
export function objectiveSymbol(kind: string, fill: string, halo: string, s: number): { svg: string; size: number } {
  const { f, i } = symbolShape(kind, s)
  const h = Math.ceil(s * 1.75) + 2
  const size = h * 2
  const paths =
    f.map((d) => `<path d="${d}" fill="${fill}" stroke="${halo}" stroke-width="1.4" stroke-linejoin="round"/>`).join('') +
    i.map((d) => `<path d="${d}" fill="none" stroke="${halo}" stroke-width="1.5" stroke-linecap="round"/>`).join('')
  return { svg: `<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}" viewBox="${-h} ${-h} ${size} ${size}">${paths}</svg>`, size }
}
