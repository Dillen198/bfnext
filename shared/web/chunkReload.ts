// Recovering from a route chunk that no longer exists after a deploy (see
// ErrorBoundary.tsx).

const RELOAD_KEY = 'vs-chunk-reload-at'
/** A second chunk failure this soon after a reload is not a stale tab, so the
 *  error is shown instead of reloading forever. */
const RELOAD_GUARD_MS = 30_000

/** True for the errors browsers and Vite raise when a JS/CSS chunk fails to load. */
export function isChunkLoadError(error: unknown): boolean {
  const msg = error instanceof Error ? `${error.name} ${error.message}` : String(error)
  return /Failed to fetch dynamically imported module|error loading dynamically imported module|Importing a module script failed|Unable to preload CSS|ChunkLoadError|Loading (CSS )?chunk \S+ failed/i.test(msg)
}

/** Reload the page, unless this tab already did so moments ago. Returns
 *  whether it is reloading. */
export function reloadOnce(): boolean {
  try {
    const last = Number(sessionStorage.getItem(RELOAD_KEY) ?? 0)
    if (Date.now() - last < RELOAD_GUARD_MS) return false
    sessionStorage.setItem(RELOAD_KEY, String(Date.now()))
  } catch {
    // No sessionStorage: no loop guard either, so do not risk a reload loop.
    return false
  }
  window.location.reload()
  return true
}

/** Vite fires `vite:preloadError` when a dynamic import's preload fails;
 *  reload for it too. Call once at startup. */
export function installChunkReload(): void {
  window.addEventListener('vite:preloadError', event => {
    if (reloadOnce()) event.preventDefault()
  })
}
