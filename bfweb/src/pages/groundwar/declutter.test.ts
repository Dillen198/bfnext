import { describe, expect, it } from 'vitest'
import type { GroundObjective } from '../../api'
import { displayName, visibleNames, type Pt } from './declutter'

const obj = (id: number, name: string, owner: 'Blue' | 'Red', kind = 'fob'): GroundObjective => ({
  id, name, pos: [0, 0], owner, kind, health: null, threatened: null, can_raise: null,
  being_captured: false, supply: null, garrison: null,
})

describe('displayName', () => {
  it('drops the owner suffix', () => {
    expect(displayName('Latakia - SA-2 (RED)')).toBe('LATAKIA - SA-2')
    expect(displayName('Kutaisi')).toBe('KUTAISI')
  })
})

describe('visibleNames', () => {
  const at: Record<number, Pt> = { 1: { x: 100, y: 100 }, 2: { x: 112, y: 104 }, 3: { x: 400, y: 300 } }
  const project = (o: GroundObjective) => at[o.id] ?? null

  it('keeps the more important of two clashing names', () => {
    const objs = [obj(1, 'Latakia - SA-2 (RED)', 'Red', 'sam'), obj(2, 'Latakia', 'Red', 'airbase'), obj(3, 'Hatay', 'Blue')]
    const shown = visibleNames(objs, project, 'Red', null)
    expect(shown.has(2)).toBe(true) // airbase outranks the SAM site
    expect(shown.has(1)).toBe(false)
    expect(shown.has(3)).toBe(true) // far away: no clash
  })

  it('always shows the selected base', () => {
    const objs = [obj(1, 'Latakia - SA-2', 'Red', 'sam'), obj(2, 'Latakia', 'Red', 'airbase')]
    expect(visibleNames(objs, project, 'Red', 1).has(1)).toBe(true)
  })

  it('shows everything when nothing clashes', () => {
    const spread: Record<number, Pt> = { 1: { x: 0, y: 0 }, 2: { x: 300, y: 0 } }
    const objs = [obj(1, 'Alpha', 'Red'), obj(2, 'Bravo', 'Blue')]
    expect(visibleNames(objs, (o) => spread[o.id], 'Red', null).size).toBe(2)
  })
})
