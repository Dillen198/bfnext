import { describe, expect, it } from 'vitest'
import type { InstanceStore } from '@shared/web/api'
import type { ServerInstance } from '../api'
import { lockedInstance, lockStore } from './instanceLock'
import { lockSelection } from '../context/InstanceContext'

/** A store that remembers what it was told, like the real one. */
function memoryStore(initial?: string): InstanceStore & { sets: (string | undefined)[] } {
  let current = initial
  const sets: (string | undefined)[] = []
  return {
    sets,
    get: () => current,
    set(id) { sets.push(id); current = id },
    withInstance: path => (current ? `${path}${path.includes('?') ? '&' : '?'}instance=${current}` : path),
  }
}

const inst = (id: string, isDefault = false): ServerInstance => ({
  id, label: id.toUpperCase(), default: isDefault, live: true, sortie: null,
  active_round: null, dcs_server_name: null, public: true,
})

describe('lockedInstance', () => {
  it('reads an id, trimmed', () => {
    expect(lockedInstance('redstorm')).toBe('redstorm')
    expect(lockedInstance('  redstorm \n')).toBe('redstorm')
  })
  it('treats unset or blank as no lock', () => {
    expect(lockedInstance(undefined)).toBeUndefined()
    expect(lockedInstance(null)).toBeUndefined()
    expect(lockedInstance('')).toBeUndefined()
    expect(lockedInstance('   ')).toBeUndefined()
  })
})

describe('lockStore', () => {
  it('returns the store itself when there is no lock', () => {
    const s = memoryStore('vs1')
    expect(lockStore(s, undefined)).toBe(s)
  })

  it('pins every request to the locked instance, whatever was remembered', () => {
    const s = lockStore(memoryStore('vs1'), 'redstorm')
    expect(s.get()).toBe('redstorm')
    expect(s.withInstance('/api/objectives')).toBe('/api/objectives?instance=redstorm')
    expect(s.withInstance('/api/kills?limit=5')).toBe('/api/kills?limit=5&instance=redstorm')
  })

  it('ignores a switch', () => {
    const base = memoryStore()
    const s = lockStore(base, 'redstorm')
    s.set('vs1')
    expect(s.get()).toBe('redstorm')
    expect(base.sets).toEqual([])
  })

  it('leaves a path that names its own scope alone', () => {
    const s = lockStore(memoryStore(), 'redstorm')
    expect(s.withInstance('/api/rounds?instance=all')).toBe('/api/rounds?instance=all')
  })

  it('escapes the id', () => {
    expect(lockStore(memoryStore(), 'red storm').withInstance('/x')).toBe('/x?instance=red%20storm')
  })
})

describe('lockSelection', () => {
  const base = {
    instances: [inst('vs1', true), inst('redstorm')],
    selected: undefined,
    current: inst('vs1', true),
    multi: true,
    select: () => { throw new Error('should not switch') },
  }

  it('passes the selection through, with a selector, when unlocked', () => {
    const v = lockSelection(base, undefined)
    expect(v.locked).toBe(false)
    expect(v.multi).toBe(true)
    expect(v.instances).toHaveLength(2)
  })

  it('shows only the locked instance and no selector', () => {
    const v = lockSelection(base, 'redstorm')
    expect(v.locked).toBe(true)
    expect(v.multi).toBe(false)
    expect(v.selected).toBe('redstorm')
    expect(v.current?.id).toBe('redstorm')
    expect(v.instances.map(i => i.id)).toEqual(['redstorm'])
    expect(() => v.select('vs1')).not.toThrow()
  })

  it('never falls back to the default server while locked', () => {
    const v = lockSelection({ ...base, instances: [inst('vs1', true)] }, 'redstorm')
    expect(v.current).toBeUndefined()
    expect(v.selected).toBe('redstorm')
  })
})
