import { describe, expect, it } from 'vitest'
import fx from '../../../bfprotocols/src/range/fixtures/lso.json'
import { goldenDiff } from './goldenCompare'
import { gradeName, gradePoints, normalizeGrade, parseComment } from './lso'

// The golden cases bfprotocols/src/range/fixtures.rs writes from lso.rs: the
// port must read every comment exactly as the engine and bfdb do.
describe('lso.ts against the Rust fixtures', () => {
  it('parses every comment like lso.rs', () => {
    for (const c of fx.comments) expect(goldenDiff(parseComment(c.raw), c.expect, c.raw)).toBeNull()
  })

  it('names and scores every grade like lso.rs', () => {
    for (const g of fx.grades) {
      expect(gradeName(g.grade), g.grade).toBe(g.name)
      expect(gradePoints(g.grade), g.grade).toBe(g.points)
    }
  })

  it('normalises grades like lso.rs', () => {
    for (const n of fx.normalize) expect(normalizeGrade(n.raw), n.raw).toBe(n.expect)
  })
})
