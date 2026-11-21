import { afterEach, describe, expect, it, vi } from 'vitest'
import { isChunkLoadError, reloadOnce } from '@shared/web/chunkReload'

afterEach(() => vi.unstubAllGlobals())

describe('chunk reload', () => {
  it('recognises a stale route chunk in each browser', () => {
    expect(isChunkLoadError(new TypeError('Failed to fetch dynamically imported module: https://x/assets/Ops-abc.js'))).toBe(true)
    expect(isChunkLoadError(new TypeError('error loading dynamically imported module'))).toBe(true)
    expect(isChunkLoadError(new Error('Importing a module script failed.'))).toBe(true)
    expect(isChunkLoadError(new Error('Unable to preload CSS for /assets/x.css'))).toBe(true)
    expect(isChunkLoadError(new Error("Cannot read properties of undefined (reading 'map')"))).toBe(false)
  })

  it('reloads once, then not again straight away', () => {
    const store = new Map<string, string>()
    const reload = vi.fn()
    vi.stubGlobal('sessionStorage', { getItem: (k: string) => store.get(k) ?? null, setItem: (k: string, v: string) => { store.set(k, v) } })
    vi.stubGlobal('window', { location: { reload } })
    expect(reloadOnce()).toBe(true)
    expect(reloadOnce()).toBe(false)
    expect(reload).toHaveBeenCalledTimes(1)
  })

  it('never reloads without a loop guard', () => {
    const reload = vi.fn()
    vi.stubGlobal('sessionStorage', { getItem: () => { throw new Error('blocked') } })
    vi.stubGlobal('window', { location: { reload } })
    expect(reloadOnce()).toBe(false)
    expect(reload).not.toHaveBeenCalled()
  })
})
