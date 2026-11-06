import { describe, expect, it } from 'vitest'
import { placeLabels, type LabelItem } from './plotLabels'

const item = (x: number, y: number, pri = 1, w = 40): LabelItem => ({ x, y, w, h: 10, pri })

describe('placeLabels', () => {
  it('puts a lone name to the right', () => {
    expect(placeLabels([item(0, 0)], 8, 6)).toEqual(['right'])
  })

  it('moves a name left when a symbol sits to the right', () => {
    // The second symbol is just right of the first; its own name has room.
    expect(placeLabels([item(0, 0, 2), item(30, 0, 1)], 8, 6)).toEqual(['left', 'right'])
  })

  it('leaves off the less important name when both sides are taken', () => {
    // Three in a row: the middle one is boxed in by its neighbours' symbols.
    expect(placeLabels([item(-30, 0, 2), item(0, 0, 1), item(30, 0, 2)], 8, 6)).toEqual(['left', null, 'right'])
  })

  it('places the more important name first', () => {
    // Two names reaching for the same gap above and below each other: the
    // airbase (higher) wins it.
    const [fob, airbase] = placeLabels([item(0, 0, 1), item(0, 6, 5)], 8, 2)
    expect(airbase).toBe('right')
    expect(fob).toBe('left')
  })

  it('keeps names inside the plot', () => {
    const frame = { x0: 0, x1: 100, y0: 0, y1: 100 }
    // Near the right edge the name goes left; near the top it has no room.
    expect(placeLabels([item(90, 50)], 8, 6, frame)).toEqual(['left'])
    expect(placeLabels([item(50, 2)], 8, 6, frame)).toEqual([null])
  })
})
