import { describe, expect, it } from 'vitest'
import { ownModelOf, waterlineZ, SHIP_FIT } from '../../../shared/models/catalog'

describe('real models on the range site', () => {
  it("uses the type's own Tacview model", () => {
    expect(ownModelOf('air', 'FA-18C_hornet')).toBe('FixedWing.F-18C')
    expect(ownModelOf('air', 'F-14B')).toBe('FixedWing.F-14')
    expect(ownModelOf('air', 'AV8BNA')).toBe('FixedWing.AV-8B')
    expect(ownModelOf('air', 'T-45')).toBe('FixedWing.Hawk')
    expect(ownModelOf('air', 'L-39ZA')).toBe('FixedWing.L-39')
    expect(ownModelOf('helo', 'Ka-50')).toBe('Rotorcraft.Ka-50')
    expect(ownModelOf('ship', 'TICONDEROG')).toBe('Watercraft.CG-47')
    expect(ownModelOf('carrier', 'CVN_71')).toBe('Watercraft.CVN-72')
  })
  it('keeps our own build where only a lookalike would do', () => {
    // names Tacview does not know get no borrowed model on the range
    expect(ownModelOf('armor', 'CHAP_T90M')).toBeNull()
    expect(ownModelOf('air', 'Totally_Unknown_Jet')).toBeNull()
    expect(ownModelOf('ship', 'LHA_Tarawa')).toBeNull()
  })
  it('sits ships on the water', () => {
    // an escort's waterline is its draft above the keel
    expect(waterlineZ('Watercraft.CG-47')! - SHIP_FIT['Watercraft.CG-47']!.bottom_z).toBeCloseTo(10.2)
    // a carrier's is its deck height below the flight deck
    expect(waterlineZ('Watercraft.CVN-74', 20.1)).toBeCloseTo(SHIP_FIT['Watercraft.CVN-74']!.deck_z - 20.1)
  })
})
