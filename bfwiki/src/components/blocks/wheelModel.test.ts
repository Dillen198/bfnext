import { describe, expect, it } from 'vitest'
import type { WheelItem } from './parse'
import {
  G, arcEntries, arcStart, hit, hubText, iconFor, initialWheel, isDestructive, resolve, ringEntries,
  slotMid, wheelReducer, type WheelAction, type WheelState,
} from './wheelModel'

const ITEMS: WheelItem[] = [
  {
    label: 'Cargo',
    items: [
      { label: 'Unpack', says: 'Unpacked' },
      { label: 'Load', says: 'Loaded' },
      { label: 'Destroy', says: 'Destroyed' },
      { label: 'More Cargo', items: [{ label: 'List Cargo', says: 'Nothing aboard' }] },
    ],
  },
  { label: 'Troops', items: [{ label: 'Unload', says: 'Deployed' }] },
  { label: 'Points', says: '31108 pts' },
  { label: 'F10 Menu', icon: 'panel' },
]

const run = (actions: WheelAction[], s: WheelState = initialWheel(ITEMS)) => actions.reduce(wheelReducer, s)
const main = (i: number) => ({ zone: 'main', i }) as const
const arc = (j: number) => ({ zone: 'arc', j }) as const
const HUB = { zone: 'hub' } as const

describe('wheel slots', () => {
  it('picks icons from the label like the plugin does', () => {
    expect(iconFor('Unpack Nearby Crate(s)')).toBe('crate_open')
    expect(iconFor('Unload Crate')).toBe('unload')
    expect(iconFor('C-130 Cargo')).toBe('transport')
    expect(iconFor('Something')).toBe('generic')
    expect(ringEntries(initialWheel(ITEMS))[3].icon).toBe('panel')
  })

  it('knows destructive entries', () => {
    expect(isDestructive('Destroy Nearby Crate')).toBe(true)
    expect(isDestructive('Load')).toBe(false)
  })

  it('folds slots past the maximum into MORE', () => {
    const many = Array.from({ length: 13 }, (_, i) => ({ label: `S${i}` }))
    const ring = resolve(many, G.maxRing)
    expect(ring).toHaveLength(10)
    expect(ring[9].label).toBe('MORE')
    expect(ring[9].children.map(c => c.label)).toEqual(['S9', 'S10', 'S11', 'S12'])
  })

  it('puts the first slot straight up and goes clockwise', () => {
    expect(slotMid(0, 4)).toBe(-90)
    expect(slotMid(1, 4)).toBe(0)
  })

  it('centres an arc on its owner, on the 5-degree grid', () => {
    // 4 children under the slot at 0 degrees: 80 degrees wide, from -40.
    expect(arcStart(1, 4, 4)).toBe(-40)
    // 3 children under the top slot: 60 degrees wide, centred on -90.
    expect(arcStart(0, 4, 3)).toBe(-120)
    expect(Math.abs(arcStart(2, 3, 4) % G.arcStep)).toBe(0)
  })
})

describe('wheel sub-ring', () => {
  it('pointing at a group opens its arc; pointing at a command closes it', () => {
    let s = run([{ type: 'point', zone: main(0), source: 'mouse' }])
    expect(s.hlMain).toBe(0)
    expect(s.arc?.owner).toBe(0)
    expect(arcEntries(s).map(e => e.label)).toEqual(['Unpack', 'Load', 'Destroy', 'More Cargo'])
    s = run([{ type: 'point', zone: main(2), source: 'mouse' }], s)
    expect(s.arc).toBeUndefined()
  })

  it('the arc stays open while the pointer moves out onto it', () => {
    const s = run([
      { type: 'point', zone: main(0), source: 'mouse' },
      { type: 'point', zone: arc(1), source: 'mouse' },
    ])
    expect(s.arc?.owner).toBe(0)
    expect(s.hlArc).toBe(1)
    expect(hubText(s).title).toBe('CARGO › LOAD')
    expect(hubText(s).sub).toBe('Cargo > Load')
  })

  it('a tap on a group opens its arc first, and a second tap goes in', () => {
    let s = run([{ type: 'click', zone: main(1) }])
    expect(s.arc?.owner).toBe(1)
    expect(s.stack).toHaveLength(1)
    s = run([{ type: 'click', zone: main(1) }], s)
    expect(s.stack).toHaveLength(2)
    expect(ringEntries(s).map(e => e.label)).toEqual(['Unload'])
  })

  it('the hub closes the arc, then goes back a level, then closes the wheel', () => {
    let s = run([
      { type: 'point', zone: main(0), source: 'mouse' },
      { type: 'click', zone: arc(3) }, // More Cargo: a group in the arc goes in
    ])
    expect(s.stack.map(f => f.title)).toEqual(['VECTOR STRIKE', 'More Cargo'])
    s = run([{ type: 'point', zone: HUB, source: 'mouse' }], s)
    expect(hubText(s).title).toBe('‹ BACK')
    s = run([{ type: 'click', zone: HUB }], s)
    expect(s.stack).toHaveLength(1)
    s = run([{ type: 'point', zone: main(0), source: 'mouse' }, { type: 'point', zone: HUB, source: 'mouse' }], s)
    expect(s.arc).toBeUndefined()
    expect(hubText(s).title).toBe('CLOSE')
    s = run([{ type: 'click', zone: HUB }], s)
    expect(s.open).toBe(false)
    s = run([{ type: 'open' }], s)
    expect(s.open).toBe(true)
    expect(s.stack).toHaveLength(1)
  })

  it('a command shows what the server says', () => {
    const s = run([{ type: 'point', zone: main(0), source: 'mouse' }, { type: 'click', zone: arc(0) }])
    expect(s.answer).toBe('Unpacked')
    expect(run([{ type: 'click', zone: main(2) }]).answer).toBe('31108 pts')
  })

  it('a destructive command needs a second click', () => {
    let s = run([{ type: 'point', zone: main(0), source: 'mouse' }, { type: 'click', zone: arc(2) }])
    expect(s.armed).toBeDefined()
    expect(s.answer).toBeUndefined()
    expect(hubText(s)).toMatchObject({ title: 'AGAIN TO CONFIRM', bad: true })
    s = run([{ type: 'click', zone: arc(2) }], s)
    expect(s.answer).toBe('Destroyed')
    expect(s.armed).toBeUndefined()
  })

  it('moving away disarms a pending confirm', () => {
    let s = run([{ type: 'point', zone: main(0), source: 'mouse' }, { type: 'click', zone: arc(2) }])
    s = run([{ type: 'point', zone: arc(1), source: 'mouse' }], s)
    expect(s.armed).toBeUndefined()
  })
})

describe('wheel pointer and keys', () => {
  it('hit-tests the hub, the ring and an open arc', () => {
    const s0 = initialWheel(ITEMS)
    expect(hit(s0, 0, 0)).toEqual({ zone: 'hub' })
    expect(hit(s0, 0, -160)).toEqual({ zone: 'main', i: 0 })
    expect(hit(s0, 160, 0)).toEqual({ zone: 'main', i: 1 })
    expect(hit(s0, 0, -600)).toEqual({ zone: 'none' })
    const s = run([{ type: 'point', zone: main(0), source: 'mouse' }])
    // the arc is 80 degrees centred straight up: its second segment is just left of up
    expect(hit(s, -40, -290)).toEqual({ zone: 'arc', j: 1 })
  })

  it('arrow keys step round the ring, out onto the arc and back in', () => {
    let s = run([{ type: 'nav', dir: 'next' }])
    expect(s.hlMain).toBe(0)
    expect(s.source).toBe('keys')
    expect(hubText(s).hint).toMatch(/move/)
    s = run([{ type: 'nav', dir: 'prev' }], s)
    expect(s.hlMain).toBe(3)
    s = run([{ type: 'nav', dir: 'next' }, { type: 'nav', dir: 'out' }], s)
    expect(s.hlMain).toBe(0)
    expect(s.hlArc).toBe(1)
    s = run([{ type: 'nav', dir: 'next' }], s)
    expect(s.hlArc).toBe(2)
    s = run([{ type: 'nav', dir: 'into' }], s)
    expect(s.hlArc).toBeUndefined()
    expect(s.hlMain).toBe(0)
  })
})
