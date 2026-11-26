import { describe, expect, it } from 'vitest'
import { applyCfgTokens, formatFact, hasCfgTokens, lookupFact } from './cfgTokens'

const facts = {
  points: { air_kill: 350, ratio: 0.35000000000000003 },
  default_lives: { Standard: [3, 6] },
  enabled: true,
}

describe('cfg tokens', () => {
  it('looks up dotted and indexed paths', () => {
    expect(lookupFact(facts, 'points.air_kill')).toBe(350)
    expect(lookupFact(facts, 'default_lives.Standard[1]')).toBe(6)
    expect(lookupFact(facts, 'points.missing.deeper')).toBeUndefined()
    expect(lookupFact(facts, 'default_lives.Standard[x]')).toBeUndefined()
  })

  it('formats values the way a page reads them', () => {
    expect(formatFact(facts.points.ratio)).toBe('0.35')
    expect(formatFact(true)).toBe('yes')
    expect(formatFact([3, 6])).toBe('3, 6')
    expect(formatFact({})).toBeUndefined()
  })

  it('substitutes tokens, falling back to the written default then the path', () => {
    const md = 'Kill {{cfg:points.air_kill|100}}, lives {{ cfg:default_lives.Standard[0] }}, x {{cfg:nope|7}}, y {{cfg:nope.at.all}}'
    expect(hasCfgTokens(md)).toBe(true)
    expect(applyCfgTokens(md, facts)).toBe('Kill 350, lives 3, x 7, y nope.at.all')
    expect(applyCfgTokens(md, undefined)).toBe('Kill 100, lives default_lives.Standard[0], x 7, y nope.at.all')
  })
})
