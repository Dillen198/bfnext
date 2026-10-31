import { describe, expect, it } from 'vitest'
import type { GroundObjective } from '../../api'
import { territoryCells } from './territory'

const obj = (id: number, pos: [number, number], owner: 'Blue' | 'Red', kind = 'fob'): GroundObjective => ({
  id, name: `O${id}`, pos, owner, kind, health: null, threatened: null, can_raise: null,
  being_captured: false, supply: null, garrison: null,
})

describe('territoryCells', () => {
  it('splits the ground between two bases at the midpoint', () => {
    const cells = territoryCells([obj(1, [42, 43.0], 'Blue'), obj(2, [42, 43.2], 'Red')])
    expect(cells.features).toHaveLength(2)
    const blue = cells.features.find((f) => f.properties?.owner === 'Blue')!
    const maxLon = Math.max(...blue.geometry.coordinates[0].map((c) => c[0]))
    expect(maxLon).toBeCloseTo(43.1, 2)
  })

  it('leaves out carriers and naval bases', () => {
    const cells = territoryCells([obj(1, [42, 43], 'Blue', 'carrier'), obj(2, [42, 44], 'Red', 'naval')])
    expect(cells.features).toHaveLength(0)
  })
})
