import { describe, expect, it } from 'vitest'
import { LENS_CELLS, ballPos, ballSay, litCells } from './lens'

describe('the lens', () => {
  it('straddles the datum on glideslope', () => {
    expect(litCells(ballPos(0).p)).toEqual([5, 6])
    expect(ballSay(0)).toBe('centred')
  })
  it('moves half a cell at a time and turns red at the bottom', () => {
    expect(ballSay(0.2)).toBe('1.5 cells high')
    expect(litCells(ballPos(0.2).p)).toEqual([4])
    expect(ballSay(-0.7)).toMatch(/low, red$/)
    expect(litCells(ballPos(-0.7).p).every(i => i >= LENS_CELLS - 2)).toBe(true)
  })
  it('goes off the lens beyond about 0.8 degrees', () => {
    expect(ballPos(1.9).off).toBe(true)
    expect(ballSay(1.9)).toBe('off the top')
    expect(ballSay(-1.2)).toBe('off the bottom')
  })
})
