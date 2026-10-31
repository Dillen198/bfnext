// This dashboard shows one campaign: the Red Storm server. A build names its
// bfdb instance id in VITE_INSTANCE, and every request is then scoped to that
// instance whatever the URL or the remembered choice says, and the server
// selector is hidden. Unset (local dev against the live API), the dashboard
// keeps the ordinary selector.

import type { InstanceStore } from '@shared/web/api'

/** The instance id a build is locked to, or undefined. Blank counts as unset. */
export function lockedInstance(raw: string | undefined | null): string | undefined {
  const id = raw?.trim()
  return id ? id : undefined
}

/**
 * `store`, pinned to `id`. `get` always answers `id`, `set` is ignored (there
 * is nothing to switch to), and every path is scoped to `id`. With no `id`
 * the store is returned as it is.
 */
export function lockStore(store: InstanceStore, id: string | undefined): InstanceStore {
  if (!id) return store
  return {
    get: () => id,
    set: () => {},
    withInstance(path) {
      // A path that names its own scope (`?instance=all`) keeps it.
      if (/[?&](instance|server)=/.test(path)) return path
      const sep = path.includes('?') ? '&' : '?'
      return `${path}${sep}instance=${encodeURIComponent(id)}`
    },
  }
}

/** The id this build is locked to (VITE_INSTANCE), if any. */
export const LOCKED_INSTANCE: string | undefined = lockedInstance(import.meta.env.VITE_INSTANCE)
