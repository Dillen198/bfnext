import { SERVERS } from '@shared/servers/profiles'
import { VS_URLS } from '@shared/web/urls'

/**
 * The campaigns whose dashboard is not this one: Levant Ablaze lives on the
 * main dashboard (bfweb). The round selector lists them under "Other
 * servers", and picking one hands the viewer over to it, as the main
 * dashboard hands Red Storm over to this one.
 */
export const OTHER_SERVERS = SERVERS.filter(s => s.instanceId && s.site.url !== VS_URLS.redstorm)

/** That campaign's dashboard, for an instance id; undefined for this one's own. */
export function elsewhere(instanceId: string | undefined): string | undefined {
  return OTHER_SERVERS.find(s => s.instanceId === instanceId)?.site.url
}

/**
 * The same page on that dashboard: its routes are this app's, so the viewer
 * lands where they were (one it lacks falls back to its SITREP). The
 * ?instance= is dropped -- the other app knows its own.
 */
export function handOffUrl(base: string, loc: { pathname: string; search: string; hash: string }): string {
  const q = new URLSearchParams(loc.search)
  q.delete('instance')
  const qs = q.toString()
  return `${base}${loc.pathname}${qs ? `?${qs}` : ''}${loc.hash}`
}
