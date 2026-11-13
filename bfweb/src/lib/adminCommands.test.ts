import { describe, expect, it } from 'vitest'
import { COMMAND_GROUPS, buildCommand, cleanConsoleLine, refusedHere, type AdminCommandDef } from './adminCommands'

const find = (label: string): AdminCommandDef => {
  const def = COMMAND_GROUPS.flatMap(g => g.commands).find(c => c.label === label)
  if (!def) throw new Error(`no command ${label}`)
  return def
}

describe('admin commands', () => {
  it('builds the text chat takes, in the engine’s argument order', () => {
    expect(buildCommand(find('Repair all'), {})).toEqual({ ok: true, text: 'repair all' })
    expect(buildCommand(find('Repair base'), { objective: 'King Hussein Air College' }))
      .toEqual({ ok: true, text: 'repair King Hussein Air College' })
    expect(buildCommand(find('Switch side'), { side: 'blue', player: 'D4n' }))
      .toEqual({ ok: true, text: 'switch blue D4n' })
    expect(buildCommand(find('Set points'), { n: '500', player: 'D4n' }))
      .toEqual({ ok: true, text: 'set-points 500 D4n' })
    expect(buildCommand(find('Set lives'), { player: 'D4n', type: 'attack', n: '2' }))
      .toEqual({ ok: true, text: 'lives D4n attack 2' })
    expect(buildCommand(find('Explosion at mark'), { key: 'boom' }))
      .toEqual({ ok: true, text: 'tim boom' })
  })

  it('says what is missing or wrong', () => {
    expect(buildCommand(find('Kick'), {})).toEqual({ ok: false, error: 'Player is needed' })
    expect(buildCommand(find('Add points'), { player: 'D4n', n: 'lots' }))
      .toEqual({ ok: false, error: 'Points must be a number' })
    expect(buildCommand(find('Add points'), { player: 'Big D4n', n: '5' }).ok).toBe(false)
  })

  it('never offers what the engine refuses from here', () => {
    const words = COMMAND_GROUPS.flatMap(g => g.commands).map(c => c.word)
    expect(words).not.toContain('reset')
    expect(words).not.toContain('shutdown')
    expect(words).not.toContain('log-desc')
    expect(refusedHere('Reset blue')).toMatch(/own button/)
    expect(refusedHere('log-desc')).toMatch(/in game/)
    expect(refusedHere('reset-lives-all')).toBeNull()
  })

  it('takes a line pasted from chat', () => {
    expect(cleanConsoleLine('  -admin   repair all ')).toBe('repair all')
    expect(cleanConsoleLine('kick D4n')).toBe('kick D4n')
  })
})
