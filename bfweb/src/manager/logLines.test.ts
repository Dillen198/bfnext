import { describe, expect, it } from 'vitest'
import { appendCapped, levelOf, makeFilter, resetNote, type FollowAnswer } from './logLines'
import { isMissingRoute, skewMessage } from './tauri'

describe('levelOf', () => {
  it('reads the level of each log format the box writes', () => {
    expect(levelOf('13:05:25 [ERROR] (20) bflib::db: no such objective')).toBe('error')
    expect(levelOf('13:05:25 [WARN] (3) bfrange: slow frame')).toBe('warn')
    expect(levelOf('2026-10-25 12:00:00,123 WARNING  FowlEngine/ops: DEPRECATED')).toBe('warn')
    expect(levelOf('2026-10-25 12:00:00.000 ERROR   SCRIPTING (Main): x.lua:12')).toBe('error')
    expect(levelOf('2026-10-25 12:00:00.000 INFO    APP: started')).toBe('info')
    expect(levelOf('12:00:00 [DEBUG] tick')).toBe('debug')
    expect(levelOf('Traceback (most recent call last):')).toBe('error')
    expect(levelOf('just a line about an ERRORS table')).toBeNull()
  })
})

describe('makeFilter', () => {
  const lines = ['12:00 [INFO] up', '12:01 [ERROR] login failed', '12:02 [WARN] slow vs1']
  it('matches text, regex and negation', () => {
    expect(lines.filter(makeFilter(''))).toHaveLength(3)
    expect(lines.filter(makeFilter('LOGIN'))).toEqual(['12:01 [ERROR] login failed'])
    expect(lines.filter(makeFilter('/\\[(ERROR|WARN)\\]/'))).toHaveLength(2)
    expect(lines.filter(makeFilter('!info'))).toHaveLength(2)
    // a broken regex is plain text, not a crash
    expect(lines.filter(makeFilter('/[unclosed/'))).toEqual([])
  })
})

describe('appendCapped', () => {
  it('keeps the newest lines only', () => {
    expect(appendCapped(['a', 'b'], ['c', 'd'], 3)).toEqual(['b', 'c', 'd'])
    const buf = ['x']
    expect(appendCapped(buf, [])).toBe(buf)
  })
})

describe('resetNote', () => {
  const base: FollowAnswer = { which: 'bot', path: 'p', lines: [], offset: 0, ident: null, size: 0, reset: true,
                               reason: null, more: false, skipped: 0 }
  it('says why the log started over', () => {
    expect(resetNote({ ...base, reason: 'rotated' })).toMatch(/rotated/)
    expect(resetNote({ ...base, reason: 'truncated' })).toMatch(/truncated/)
    expect(resetNote({ ...base, reason: 'skipped', skipped: 4096 })).toMatch(/4 KB/)
    expect(resetNote({ ...base, reason: 'start', reset: false })).toBeNull()
  })
})

describe('version skew', () => {
  it("tells FastAPI's missing route from the plugin's own 404", () => {
    expect(isMissingRoute(404, { detail: 'Not Found' })).toBe(true)
    expect(isMissingRoute(404, { ok: false, error: 'no DCS server named x' })).toBe(false)
    expect(isMissingRoute(404, null)).toBe(false)
    expect(isMissingRoute(200, { detail: 'Not Found' })).toBe(false)
    expect(skewMessage('RANGE')).toMatch(/update the plugin there/i)
  })
})
