import { describe, expect, it } from 'vitest'
import type { Asset } from '../../api'
import { assetSvg } from './assetGlyph'

// A hostile string in every server-supplied field: none of it may reach the
// markup command.tsx injects.
const EVIL = '"><img src=x onerror=alert(1)><svg onload=alert(2)>'

function asset(kind: Asset['kind'], over: Partial<Asset> = {}): Asset {
  return { kind, role: EVIL, typ: EVIL, name: EVIL, ...over } as unknown as Asset
}

describe('asset glyph markup', () => {
  it('never contains server-supplied text', () => {
    for (const kind of ['air', 'naval', 'troops', 'artillery', 'convoy', 'ground'] as Asset['kind'][]) {
      const svg = assetSvg(asset(kind), '#7db8ff')
      expect(svg.startsWith('<svg ')).toBe(true)
      expect(svg).not.toContain('onerror')
      expect(svg).not.toContain('onload')
      expect(svg).not.toContain('<img')
      expect(svg.match(/<svg/g)).toHaveLength(1)
    }
  })

  it('still picks the glyph from those fields', () => {
    expect(assetSvg(asset('air', { typ: 'Mi-8MT' }), '#fff')).toContain('stroke-dasharray')
    expect(assetSvg(asset('air', { typ: 'F-16C_50' }), '#fff')).not.toContain('stroke-dasharray')
    expect(assetSvg(asset('ground', { role: 'SAM', typ: 'Hawk ln' }), '#fff')).toContain('Q12 2 19 15')
  })
})
