import { describe, expect, it } from 'vitest'
import { gradeName, gradePoints, parseCall, parseComment } from './lso'

// The same cases as bfprotocols/src/range/lso.rs, so the port cannot drift.
describe('lso parser (mirrors lso.rs tests)', () => {
  it('reads the reference card comment', () => {
    const c = parseComment('LSO: GRADE:WO AAX FIM (SLO)AR _HAW_')!
    expect(c.grade).toBe('WO')
    expect(gradeName(c.grade)).toBe('Waveoff')
    expect(c.calls.map(x => x.text)).toEqual([
      'angling approach at the start (first third)',
      'fast in the middle (second third)',
      'a little slow at the ramp',
      'VERY high all the way',
    ])
  })

  it('pulls the wire out in every spelling', () => {
    let c = parseComment('LSO: GRADE:_OK_ : WIRE# 3')!
    expect(c.grade).toBe('_OK_')
    expect(c.wire).toBe(3)
    expect(c.calls).toHaveLength(0)
    c = parseComment('LSO: GRADE:C _SLOX_ _LURX_ 3PTSIW LNFIW WIRE #1')!
    expect(c.grade).toBe('C')
    expect(c.wire).toBe(1)
    expect(c.calls.map(x => x.text)).toEqual([
      'VERY slow at the start (first third)',
      'VERY lined up right at the start (first third)',
      'three-point landing in the wires',
      'landed nose first in the wires',
    ])
  })

  it('handles no-count and unknown tokens', () => {
    const c = parseComment('GRADE:--- : _LOIC_ _LOAR_')!
    expect(c.grade).toBe('--')
    expect(c.calls[0].text).toBe('VERY low in close (last third)')
    expect(gradePoints('WOFD')).toBeNull()
    expect(parseCall('ZZZ').text).toBe('ZZZ')
  })

  it('scores grades like the Rust', () => {
    expect(gradePoints('_OK_')).toBe(5)
    expect(gradePoints('B')).toBe(2.5)
    expect(gradePoints('OWO')).toBe(2)
    expect(gradePoints('WOP')).toBe(1)
    expect(gradePoints('C')).toBe(0)
    expect(gradePoints('NC')).toBeNull()
  })
})
