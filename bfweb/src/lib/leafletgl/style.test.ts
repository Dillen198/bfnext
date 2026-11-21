import { describe, expect, it } from 'vitest'
import type { Feature } from 'geojson'
import { colorOf, dashArray, evaluate, numberOf, parseColor, passes } from './style'

const feat = (properties: Record<string, unknown>, type: 'Point' | 'LineString' | 'MultiPolygon' = 'LineString'): Feature => ({
  type: 'Feature',
  properties,
  geometry: type === 'Point'
    ? { type: 'Point', coordinates: [0, 0] }
    : type === 'LineString'
      ? { type: 'LineString', coordinates: [[0, 0], [1, 1]] }
      : { type: 'MultiPolygon', coordinates: [] },
})

describe('leafletgl style expressions', () => {
  it('reads properties and matches on them, the way the command map colours its layers', () => {
    const ownerColor = ['match', ['get', 'owner'], 'Blue', '#4a8fd4', 'Red', '#cc4444', '#999']
    expect(evaluate(ownerColor, { zoom: 8, feature: feat({ owner: 'Red' }) })).toBe('#cc4444')
    expect(evaluate(ownerColor, { zoom: 8, feature: feat({ owner: 'Neutral' }) })).toBe('#999')
    expect(evaluate(['match', ['get', 'k'], ['a', 'b'], 1, 2], { zoom: 0, feature: feat({ k: 'b' }) })).toBe(1)
  })

  it('does case and arithmetic (the threat rings fade with confidence)', () => {
    const f = feat({ live: 1, a: 0.5 })
    expect(evaluate(['case', ['==', ['get', 'live'], 1], 0.07, 0.03], { zoom: 0, feature: f })).toBe(0.07)
    expect(evaluate(['*', 0.8, ['get', 'a']], { zoom: 0, feature: f })).toBeCloseTo(0.4)
  })

  it('interpolates on zoom, linear and exponential, numbers and colours', () => {
    const w = ['interpolate', ['linear'], ['zoom'], 8, 0.8, 14, 1.6]
    expect(numberOf(w, { zoom: 11 }, 0)).toBeCloseTo(1.2)
    expect(numberOf(w, { zoom: 2 }, 0)).toBe(0.8)
    expect(numberOf(w, { zoom: 20 }, 0)).toBe(1.6)
    const e = ['interpolate', ['exponential', 2], ['zoom'], 0, 0, 2, 3]
    expect(numberOf(e, { zoom: 1 }, 0)).toBeCloseTo(1)
    const grad = ['interpolate', ['linear'], ['line-progress'], 0, 'rgba(0,0,0,0)', 1, 'rgba(100,200,50,0.6)']
    expect(colorOf(grad, { zoom: 0, progress: 0.5 }, '')).toBe('rgba(50,100,25,0.3)')
  })

  it('filters on geometry type and the legacy form', () => {
    const pt = feat({}, 'Point')
    const ln = feat({ cut: 1 })
    expect(passes(['==', ['geometry-type'], 'Point'], pt, 0)).toBe(true)
    expect(passes(['==', ['geometry-type'], 'Point'], ln, 0)).toBe(false)
    expect(passes(['==', ['get', 'cut'], 1], ln, 0)).toBe(true)
    expect(passes(['==', '$type', 'Polygon'], feat({}, 'MultiPolygon'), 0)).toBe(true)
    expect(passes(['==', 'cut', 0], ln, 0)).toBe(false)
    expect(passes(undefined, ln, 0)).toBe(true)
  })

  it('turns line-dasharray (in widths) into px', () => {
    expect(dashArray([2, 1.5], { zoom: 0 }, 2)).toBe('4 3')
    expect(dashArray([0, 4, 3], { zoom: 0 }, 1.6)).toBe('0 6.4 4.8')
    expect(dashArray(undefined, { zoom: 0 }, 2)).toBeUndefined()
  })

  it('parses the colours the pages use', () => {
    expect(parseColor('#4a8fd4')).toEqual([74, 143, 212, 1])
    expect(parseColor('rgba(255,210,63,0.08)')).toEqual([255, 210, 63, 0.08])
    expect(parseColor('#fff')).toEqual([255, 255, 255, 1])
    expect(parseColor('nope')).toBeNull()
  })

  it('treats anything it does not know as no value', () => {
    expect(evaluate(['heatmap-density'], { zoom: 0 })).toBeNull()
    expect(numberOf(['nope'], { zoom: 0 }, 3)).toBe(3)
  })
})
