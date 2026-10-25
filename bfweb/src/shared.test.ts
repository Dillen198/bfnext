// Tests for shared/web, which has no test runner of its own (see its README).
import { afterEach, describe, expect, it, vi } from 'vitest'
import { createInstanceStore, errorMessage, wsUrl } from '@shared/web/api'
import { appliesTo, fmtUtc, maintenanceVisible } from '@shared/web/status'
import { lasted } from '@shared/web/notifications'

function fakeBrowser(search: string, stored: Record<string, string> = {}) {
  const store = new Map(Object.entries(stored))
  vi.stubGlobal('window', { location: { search, protocol: 'https:', host: 'dash.example' } })
  vi.stubGlobal('localStorage', {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => { store.set(k, v) },
    removeItem: (k: string) => { store.delete(k) },
  })
  return store
}

afterEach(() => vi.unstubAllGlobals())

describe('instance store', () => {
  it('prefers ?instance= over the remembered choice and appends it', () => {
    fakeBrowser('?instance=vs2', { k: 'vs1' })
    const s = createInstanceStore({ storageKey: 'k' })
    expect(s.get()).toBe('vs2')
    expect(s.withInstance('/objectives')).toBe('/objectives?instance=vs2')
    expect(s.withInstance('/news?limit=5')).toBe('/news?limit=5&instance=vs2')
    expect(s.withInstance('/rounds?instance=all')).toBe('/rounds?instance=all')
  })

  it('remembers a switch and falls back to the cockpit server name', () => {
    const storage = fakeBrowser('?server=%5BVS%5D%20Two')
    const s = createInstanceStore({ storageKey: 'k', serverParam: true })
    expect(s.withInstance('/units')).toBe('/units?server=%5BVS%5D%20Two')
    s.set('vs3')
    expect(storage.get('k')).toBe('vs3')
    expect(s.withInstance('/units')).toBe('/units?instance=vs3')
    s.set(undefined)
    expect(storage.has('k')).toBe(false)
  })

  it('survives a browser without storage', () => {
    vi.stubGlobal('window', { location: { search: '' } })
    vi.stubGlobal('localStorage', { getItem: () => { throw new Error('blocked') } })
    expect(createInstanceStore({ storageKey: 'k' }).get()).toBeUndefined()
  })
})

describe('api helpers', () => {
  it('reads bfdb error bodies, else the status', async () => {
    expect(await errorMessage(new Response(JSON.stringify({ error: 'nope' }), { status: 400 }))).toBe('nope')
    expect(await errorMessage(new Response(JSON.stringify({ message: 'later' }), { status: 503 }))).toBe('later')
    expect(await errorMessage(new Response('<html>', { status: 502 }))).toBe('HTTP 502')
  })

  it('builds websocket URLs for both deployments', () => {
    fakeBrowser('')
    expect(wsUrl('https://api.example', '/ws/units')).toBe('wss://api.example/ws/units')
    expect(wsUrl('', '/ws/units')).toBe('wss://dash.example/ws/units')
  })
})

describe('status file helpers', () => {
  const now = Date.parse('2025-10-20T12:00:00Z')

  it('scopes notices to servers', () => {
    expect(appliesTo(undefined, 'vs1')).toBe(true)
    expect(appliesTo([], 'vs1')).toBe(true)
    expect(appliesTo(['vs2'], undefined)).toBe(true)
    expect(appliesTo(['vs2'], 'vs1')).toBe(false)
  })

  it('shows maintenance from a week out until it ends', () => {
    const w = (start: string, end: string) => maintenanceVisible({ title: 't', start, end }, now)
    expect(w('2025-10-25T00:00:00Z', '2025-10-25T04:00:00Z')).toBe(true)
    expect(w('2025-11-05T00:00:00Z', '2025-11-05T04:00:00Z')).toBe(false)
    expect(w('2025-10-19T00:00:00Z', '2025-10-19T04:00:00Z')).toBe(false)
    expect(w('junk', '2025-10-25T04:00:00Z')).toBe(false)
    expect(fmtUtc('2025-10-19T18:00:00Z')).toBe('Sun 19 Oct, 18:00 UTC')
  })

  it('says how long a round lasted', () => {
    expect(lasted('2025-10-20T00:00:00Z', '2025-10-20T05:12:00Z')).toBe('5.2 hours')
    expect(lasted('2025-10-17T00:00:00Z', '2025-10-20T02:24:00Z')).toBe('3.1 days')
  })
})
