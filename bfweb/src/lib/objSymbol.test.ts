import { describe, expect, it } from 'vitest'
import { objectiveSymbol, symbolShape } from './objSymbol'

describe('objective symbols', () => {
  it('gives each kind its own shape', () => {
    const kinds = ['Airbase', 'FOB', 'Logistics Hub', 'Factory', 'Naval Base', 'Command Center', 'Carrier Group']
    const shapes = kinds.map((k) => symbolShape(k, 6).f.join())
    expect(new Set(shapes).size).toBe(kinds.length)
    // A FARP is forward ground like a FOB; anything new still gets a dot.
    expect(symbolShape('FARP', 6)).toEqual(symbolShape('FOB', 6))
    expect(symbolShape('Something new', 6).f).toHaveLength(1)
  })

  it('fits the whole shape in its square', () => {
    const { svg, size } = objectiveSymbol('Carrier Group', '#ec6a5c', '#171c1f', 6)
    // The ship is the widest symbol: 1.6 s either side of the centre.
    expect(size / 2).toBeGreaterThan(6 * 1.6 + 0.7)
    expect(svg).toContain('fill="#ec6a5c"')
    expect(svg).toContain(`width="${size}"`)
  })
})
