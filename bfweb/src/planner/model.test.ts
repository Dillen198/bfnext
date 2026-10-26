import { describe, expect, it } from 'vitest'
import {
  AnnotationIndex, applyOverrides, checkValue, fromPointer, patternSegs, toPointer, withOverride,
  type Json,
} from './model'

const defaults: Json = {
  CMS: { Programs: [{ Chaff: { Qty: 1 } }, { Chaff: { Qty: 2 } }] },
  Radios: { 'a/b': { Freq: '251.000' } },
  Name: 'x',
}

describe('overrides', () => {
  it('round-trips JSON Pointers, escapes included', () => {
    const segs = ['Radios', 'a/b', 'Freq~1']
    expect(fromPointer(toPointer(segs))).toEqual(segs)
  })

  it('applies over a clone and records only changes', () => {
    let ov = withOverride(defaults, {}, ['CMS', 'Programs', 1, 'Chaff', 'Qty'], 5)
    expect(ov).toEqual({ '/CMS/Programs/1/Chaff/Qty': 5 })
    const out = applyOverrides(defaults, ov) as { CMS: { Programs: { Chaff: { Qty: number } }[] } }
    expect(out.CMS.Programs[1].Chaff.Qty).toBe(5)
    expect((defaults as { CMS: { Programs: { Chaff: { Qty: number } }[] } }).CMS.Programs[1].Chaff.Qty).toBe(2)
    // Setting it back to the default drops the override.
    ov = withOverride(defaults, ov, ['CMS', 'Programs', 1, 'Chaff', 'Qty'], 2)
    expect(ov).toEqual({})
  })

  it('lets a whole-array override carry edits inside it', () => {
    let ov = withOverride(defaults, {}, ['CMS', 'Programs'], [{ Chaff: { Qty: 1 } }])
    ov = withOverride(defaults, ov, ['CMS', 'Programs', 0, 'Chaff', 'Qty'], 9)
    const out = applyOverrides(defaults, ov) as { CMS: { Programs: unknown[] } }
    expect(out.CMS.Programs).toEqual([{ Chaff: { Qty: 9 } }])
    // Replacing the array again drops edits made inside the old one.
    ov = withOverride(defaults, ov, ['CMS', 'Programs'], [])
    expect(Object.keys(ov)).toEqual(['/CMS/Programs'])
  })
})

describe('annotations', () => {
  it('parses patterns', () => {
    expect(patternSegs('CMS.Programs[].Chaff')).toEqual(['CMS', 'Programs', '[]', 'Chaff'])
    expect(patternSegs('M[][]')).toEqual(['M', '[]', '[]'])
    expect(patternSegs('Radios.{}.Freq')).toEqual(['Radios', '{}', 'Freq'])
    expect(patternSegs('PrePlanned.Stations{}.PP{}')).toEqual(['PrePlanned', 'Stations', '{}', 'PP', '{}'])
  })

  it('picks the most specific match', () => {
    const idx = new AnnotationIndex({
      code: 'T', label: 'T',
      fields: [
        { path: 'CMS.Programs[].Chaff.Qty', label: 'any', max: 10 },
        { path: 'CMS.Programs.0.Chaff.Qty', label: 'first' },
        { path: 'Radios.{}.Freq', kind: 'freq', pattern: '^\\d{3}\\.\\d{3}$' },
      ],
    })
    expect(idx.get(['CMS', 'Programs', 1, 'Chaff', 'Qty'])?.label).toBe('any')
    expect(idx.get(['CMS', 'Programs', 0, 'Chaff', 'Qty'])?.label).toBe('first')
    const f = idx.get(['Radios', 'a/b', 'Freq'])
    expect(checkValue(f, '251.000')).toBeNull()
    expect(checkValue(f, '251')).toBe('format')
    expect(checkValue(idx.get(['CMS', 'Programs', 3, 'Chaff', 'Qty']), 11)).toBe('max 10')
  })
})

describe('item labels', () => {
  it('fills fields, numbers and offsets', async () => {
    const { fillLabel } = await import('./model')
    expect(fillLabel('W{num}', { num: 3 }, 2)).toBe('W3')
    expect(fillLabel('DEST {80+index}', { number: 1 }, 0)).toBe('DEST 81')
    expect(fillLabel('STA {index+2}', {}, 1)).toBe('STA 4')
    expect(fillLabel('TCN {index0}', {}, 4)).toBe('TCN 4')
    expect(fillLabel('Channel {n}', {}, 'Channel_12')).toBe('Channel 12')
    expect(fillLabel('PP{index}', {}, 7)).toBe('PP8')
  })
})
