import { describe, expect, it } from 'vitest'
import { ownModelOf, waterlineZ, SHIP_FIT } from '../../../shared/models/catalog'

describe('real models on the range site', () => {
  it("uses a type's own model", () => {
    expect(ownModelOf('air', 'FA-18C_hornet')).toBe('F-18')
    expect(ownModelOf('air', 'F-14B')).toBe('F-14')
    expect(ownModelOf('air', 'AV8BNA')).toBe('AV-8B')
    expect(ownModelOf('helo', 'SH-60B')).toBe('SH-60')
    expect(ownModelOf('ship', 'TICONDEROG')).toBe('CG-47')
  })
  it('keeps our own build where the dashboard would lend a lookalike', () => {
    expect(ownModelOf('air', 'T-45')).toBeNull()
    expect(ownModelOf('air', 'L-39ZA')).toBeNull()
    expect(ownModelOf('helo', 'Ka-50')).toBeNull()
    expect(ownModelOf('ship', 'LHA_Tarawa')).toBeNull()
    expect(ownModelOf('ship', 'La_Combattante_II')).toBeNull()
  })
  it('sits ships on the water', () => {
    // an escort's waterline is its draft above the keel
    expect(waterlineZ('CG-47')! - SHIP_FIT['CG-47']!.bottom_z).toBeCloseTo(10.2)
    // a carrier's is its deck height below the flight deck
    expect(waterlineZ('CVN', 20.1)).toBeCloseTo(SHIP_FIT.CVN!.deck_z - 20.1)
  })
})
