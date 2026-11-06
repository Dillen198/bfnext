import { SERVERS } from '@shared/servers/profiles'
import { VS_URLS } from '@shared/web/urls'

/**
 * Where a server's own dashboard lives, when it is not this one: Red Storm
 * has bfrsr-web, its 1985 situation room. Picking such a server hands the
 * viewer over to it (Layout, and a ?instance= link on arrival), except on the
 * admin pages, which work on any server here.
 */
export function elsewhere(instanceId: string | undefined): string | undefined {
  const p = instanceId ? SERVERS.find(s => s.instanceId === instanceId) : undefined
  return p && p.site.url !== VS_URLS.dashboard ? p.site.url : undefined
}

/** The admin pages, where every server is looked at here, in the plain look. */
export const onAdminPage = (path: string) => path === '/admin' || path.startsWith('/admin/')

/**
 * The same page on that dashboard: its routes are this app's, so the viewer
 * lands where they were (an unknown one falls back to its SITREP there).
 * The ?instance= that chose the server is dropped -- the other app knows its
 * own.
 */
export function handOffUrl(base: string, loc: { pathname: string; search: string; hash: string }): string {
  const q = new URLSearchParams(loc.search)
  q.delete('instance')
  const qs = q.toString()
  return `${base}${loc.pathname}${qs ? `?${qs}` : ''}${loc.hash}`
}
