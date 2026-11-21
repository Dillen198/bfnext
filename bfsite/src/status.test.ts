import { describe, expect, it } from 'vitest'
import { fmtUtc, stateTone, STATE_LABEL, type ServerState } from './status'

describe('server status helpers', () => {
  it('gives every state a label and a tone', () => {
    const states = Object.keys(STATE_LABEL) as ServerState[]
    expect(states.length).toBeGreaterThan(0)
    for (const s of states) expect(['ok', 'warn', 'bad', 'idle']).toContain(stateTone(s))
    expect(stateTone('live')).toBe('ok')
    expect(stateTone('engine-down')).toBe('bad')
  })

  it('formats a notice time in UTC and rejects junk', () => {
    expect(fmtUtc('2025-10-19T18:00:00Z')).toBe('Sun 19 Oct, 18:00 UTC')
    expect(fmtUtc(undefined)).toBeUndefined()
    expect(fmtUtc('not a date')).toBeUndefined()
  })
})
