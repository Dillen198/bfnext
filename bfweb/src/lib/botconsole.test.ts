import { describe, expect, it } from 'vitest'
import {
  buildParams, byPlugin, canSeeBotConsole, cell, cellFor, duration, initialValue, inputKind, label, mainTable,
  mayUse, serverControls, statusTone, topLevelChanges, setPath, getPath, scheduleRows,
  presetFieldValue, setPresetField, parseFleetEvent,
} from './botconsole'
import type { BotActionInfo, BotParam } from '../api'

const p = (name: string, extra: Partial<BotParam> = {}): BotParam => ({ name, required: false, ...extra })

describe('who sees the bot console', () => {
  it('lets in full admins, ops and moderators', () => {
    expect(canSeeBotConsole(null)).toBe(false)
    expect(canSeeBotConsole({ is_admin: true })).toBe(true)
    expect(canSeeBotConsole({ is_admin: false, roles: ['ops'] })).toBe(true)
    expect(canSeeBotConsole({ is_admin: false, roles: ['moderation'] })).toBe(true)
    expect(canSeeBotConsole({ is_admin: false, roles: ['content'] })).toBe(false)
    expect(canSeeBotConsole({ is_admin: false })).toBe(false)
  })
  it('mirrors the plugin role check', () => {
    expect(mayUse(['admin'], 'admin')).toBe(true)
    expect(mayUse(['ops', 'moderation'], 'admin')).toBe(false)
    expect(mayUse(['moderation'], 'moderation')).toBe(true)
    expect(mayUse(['moderation'], 'ops')).toBe(false)
  })
})

describe('server controls', () => {
  it('follow the server state', () => {
    expect(serverControls('SHUTDOWN')).toEqual(['startup_server'])
    expect(serverControls('STOPPED')).toContain('start_server')
    expect(serverControls('RUNNING')).toContain('pause_mission')
    expect(serverControls('RUNNING')).not.toContain('unpause_mission')
    expect(serverControls('PAUSED')).toContain('unpause_mission')
    expect(serverControls('LOADING')).toEqual([])
    expect(serverControls(null)).toEqual([])
  })
  it('colour by state', () => {
    expect(statusTone('RUNNING')).toBe('ok')
    expect(statusTone('paused')).toBe('warn')
    expect(statusTone('LOADING')).toBe('busy')
    expect(statusTone('SHUTDOWN')).toBe('down')
  })
})

describe('action parameters', () => {
  it('pick an input from the annotation', () => {
    expect(inputKind(p('force', { type: 'bool', default: false }))).toBe('bool')
    expect(inputKind(p('days', { type: 'int' }))).toBe('number')
    expect(inputKind(p('missions', { type: 'list[int]', required: true }))).toBe('json')
    expect(inputKind(p('values', { type: 'dict' }))).toBe('json')
    expect(inputKind(p('mission', { type: 'int | str' }))).toBe('text')
    expect(inputKind(p('preset', { type: 'str | dict | list | None' }))).toBe('text')
    expect(inputKind(p('x', { default: true }))).toBe('bool')
  })
  it('start from the bot default', () => {
    expect(initialValue(p('force', { type: 'bool', default: true }))).toBe(true)
    expect(initialValue(p('mode', { type: 'str', default: 'popup' }))).toBe('popup')
    expect(initialValue(p('reason', { type: 'str', required: true }))).toBe('')
    expect(initialValue(p('order', { type: 'list', default: [1, 2] }))).toBe('[1,2]')
  })
  it('turn form text into params', () => {
    const params = [
      p('server_name', { type: 'str', required: true }),
      p('mission', { type: 'int | str', required: true }),
      p('modify_mission', { type: 'bool', default: true }),
      p('force', { type: 'bool', default: false }),
      p('days', { type: 'int' }),
      p('reason', { type: 'str', default: '' }),
      p('missions', { type: 'list[int]' }),
    ]
    expect(buildParams(params, {
      server_name: ' vs1 ', mission: '3', modify_mission: true, force: false, days: '', reason: '', missions: '[1,2]',
    })).toEqual({ params: { server_name: 'vs1', mission: 3, missions: [1, 2] }, errors: {} })
    // a name stays a name; a changed bool is sent
    expect(buildParams(params, { server_name: 'vs1', mission: 'Op Fowl', modify_mission: false, days: '7' }).params)
      .toEqual({ server_name: 'vs1', mission: 'Op Fowl', modify_mission: false, days: 7 })
    expect(buildParams(params, { mission: '1', days: 'x', missions: '[1,' }).errors)
      .toEqual({ server_name: 'required', days: 'not a number', missions: 'not valid JSON' })
  })
})

describe('results', () => {
  it('find the table in an answer', () => {
    expect(mainTable({ targets: [], bans: [{ ucid: 'a' }, { ucid: 'b', reason: 'tk' }] }))
      .toEqual({ key: 'bans', columns: ['ucid', 'reason'], rows: [{ ucid: 'a' }, { ucid: 'b', reason: 'tk' }] })
    expect(mainTable({ count: 3 })).toBeNull()
    expect(mainTable(null)).toBeNull()
  })
  it('format cells', () => {
    expect(cell(null)).toBe('—')
    expect(cell(true)).toBe('yes')
    expect(cell(1.234)).toBe('1.23')
    expect(cell('2026-11-05T12:34:56')).toBe('2026-11-05 12:34')
    expect(cell('9999-12-31T00:00:00')).toBe('never')
    expect(cell(['a', 1])).toBe('a, 1')
    expect(cell({ a: 1 })).toBe('{"a":1}')
  })
  it('format durations and labels', () => {
    expect(duration(3725)).toBe('1h 02m')
    expect(duration(65)).toBe('1m 05s')
    expect(duration(null)).toBe('—')
    expect(label('server_name')).toBe('Server name')
    expect(cellFor('playtime', 3725)).toBe('1h 02m')
    expect(cellFor('average_session', 65)).toBe('1m 05s')
    expect(cellFor('kills', 3725)).toBe('3725')
    expect(cellFor('sessions', 311)).toBe('311')
  })
  it('group the catalog by plugin', () => {
    const a = (name: string, plugin: string) => ({ name, plugin }) as BotActionInfo
    expect(byPlugin([a('x', 'music'), a('y', 'mission'), a('z', 'zz'), a('w', 'admin')]).map(([g]) => g))
      .toEqual(['mission', 'admin', 'music', 'zz'])
  })
})

describe('config editing', () => {
  it('diffs top-level keys', () => {
    expect(topLevelChanges({ a: 1, b: { x: 1 }, c: 3 }, { a: 1, b: { x: 2 }, d: 4 }))
      .toEqual({ set: { b: { x: 2 }, d: 4 }, delete: ['c'] })
    expect(topLevelChanges(null, { a: 1 })).toEqual({ set: { a: 1 }, delete: [] })
  })
  it('edits by path without touching the original', () => {
    const o = { a: { b: [1, 2, 3] } }
    const n = setPath(o, ['a', 'b', 1], 9)
    expect(n).toEqual({ a: { b: [1, 9, 3] } })
    expect(o).toEqual({ a: { b: [1, 2, 3] } })
    expect(setPath(o, ['a', 'b', 0], undefined)).toEqual({ a: { b: [2, 3] } })
    expect(setPath({}, ['x', 'y'], 1)).toEqual({ x: { y: 1 } })
    expect(setPath({ x: 1, y: 2 }, ['x'], undefined)).toEqual({ y: 2 })
    expect(getPath(n, ['a', 'b', 1])).toBe(9)
    expect(getPath(n, ['a', 'z', 1])).toBeUndefined()
  })
  it('reads a schedule', () => {
    expect(scheduleRows({ '00-12': 'YYYYYNN', '12-24': 'pyx' })).toEqual([
      { hours: '00-12', days: ['Y', 'Y', 'Y', 'Y', 'Y', 'N', 'N'] },
      { hours: '12-24', days: ['P', 'Y', '?', '?', '?', '?', '?'] },
    ])
    expect(scheduleRows(null)).toEqual([])
  })
  it('edits preset fields', () => {
    const p = { temperature: 20, wind: { atGround: { speed: 3 } }, clouds: { preset: 'Preset3' }, modify: [1] }
    expect(presetFieldValue(p, ['wind', 'atGround', 'speed'])).toBe('3')
    expect(presetFieldValue(p, ['clouds'])).toBe('{"preset":"Preset3"}')
    expect(presetFieldValue(p, ['qnh'])).toBe('')
    expect(setPresetField(p, ['temperature'], 'number', '25')).toMatchObject({ temperature: 25, modify: [1] })
    expect(setPresetField(p, ['temperature'], 'number', '')).not.toHaveProperty('temperature')
    expect(setPresetField(p, ['temperature'], 'number', 'hot')).toBe(p)
    expect(setPresetField(p, ['enable_fog'], 'bool', true)).toMatchObject({ enable_fog: true })
    expect(setPresetField(p, ['clouds'], 'text', 'Preset7')).toMatchObject({ clouds: 'Preset7' })
    expect(setPresetField(p, ['clouds'], 'text', '{"preset":"RainyPreset1"}')).toMatchObject({ clouds: { preset: 'RainyPreset1' } })
  })
  it('reads a fleet event', () => {
    expect(parseFleetEvent('{"servers":[],"nodes":[]}')).toEqual({ servers: [], nodes: [] })
    expect(parseFleetEvent('{"servers":1}')).toBeNull()
    expect(parseFleetEvent('nope')).toBeNull()
  })
})
