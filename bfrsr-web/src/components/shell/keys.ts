// The console's function keys: which page each key opens, which are locked
// for this viewer, and the keyboard shortcut that presses them.
//
// Kept apart from the components so the rules can be tested without a DOM.

import type { AuthUser } from '@shared/web/api'

/** A key the viewer can see but has not unlocked. It still navigates, to the
 *  login page or to the page's own explainer, so it never swallows a click. */
export interface KeyLock {
  reason: string
  go: string
}

export interface FKey {
  /** "F1".."F8". */
  code: string
  to: string
  label: string
  /** The page as its file is named (page headers carry it). */
  file: string
  locked?: KeyLock
}

export interface MoreItem {
  to: string
  label: string
  /** An external link (wiki, support). */
  external?: boolean
  locked?: KeyLock
}

type Viewer = Pick<AuthUser, 'side' | 'is_admin' | 'ucid'> | null | undefined

/** Coalition pages for a viewer who cannot open them yet: signed out goes to
 *  login; signed in with no resolvable side goes to the page's explainer. */
function coalitionLock(to: string, user: Viewer): KeyLock | undefined {
  if (user?.side || user?.is_admin) return undefined
  return user
    ? { reason: 'Locked to your coalition - link your Discord in game with -linkme', go: to }
    : { reason: 'Sign in with Discord to view', go: '/login' }
}

/** The eight function keys, in order. */
export function functionKeys(user: Viewer): FKey[] {
  return [
    { code: 'F1', to: '/', label: 'Situation', file: 'Situation board' },
    { code: 'F2', to: '/map', label: 'Plot', file: 'Plot' },
    // Public on purpose: one paper both coalitions read.
    { code: 'F3', to: '/news', label: 'Wire', file: 'Wire' },
    { code: 'F4', to: '/hq', label: 'Front HQ', file: 'Front HQ', locked: coalitionLock('/hq', user) },
    { code: 'F5', to: '/objectives', label: 'Targets', file: 'Target folders' },
    { code: 'F6', to: '/leaderboard', label: 'Honours', file: 'Honours board' },
    { code: 'F7', to: '/kills', label: 'Losses', file: 'Loss reports' },
    { code: 'F8', to: '/briefing', label: 'Briefing', file: 'Briefing', locked: coalitionLock('/briefing', user) },
  ]
}

/** Everything else, grouped, for the MORE menu. */
export function moreGroups(user: Viewer, links: { wikiUrl?: string; donationUrl?: string }): { label: string; items: MoreItem[] }[] {
  const groups: { label: string; items: MoreItem[] }[] = [
    {
      label: 'Operations',
      items: [
        { to: '/intel', label: 'Recon intel', locked: coalitionLock('/intel', user) },
        { to: '/command', label: 'Ground command', locked: coalitionLock('/command', user) },
      ],
    },
    {
      label: 'Records',
      items: [
        { to: '/pilots', label: 'Personnel files' },
        user?.ucid
          ? { to: `/pilots?ucid=${user.ucid}`, label: 'My file' }
          : {
              to: '/pilots', label: 'My file',
              locked: user
                ? { reason: 'No DCS account linked - run -linkme in game chat', go: '/pilots' }
                : { reason: 'Sign in with Discord to view your own record', go: '/login' },
            },
        { to: '/squadrons', label: 'Squadrons' },
        { to: '/rounds', label: 'Rounds' },
        { to: '/replay', label: 'Replay' },
      ],
    },
    {
      label: 'Station',
      items: [
        { to: '/about', label: 'About' },
        ...(links.wikiUrl ? [{ to: links.wikiUrl, label: 'Wiki', external: true }] : []),
        ...(links.donationUrl ? [{ to: links.donationUrl, label: 'Support', external: true }] : []),
      ],
    },
  ]
  if (user?.is_admin) {
    groups.push({
      label: 'Admin',
      items: [
        { to: '/admin', label: 'Admin' },
        { to: '/admin/config', label: 'Config' },
        { to: '/admin/ops', label: 'Server ops' },
      ],
    })
  }
  return groups
}

/**
 * Is `to` the page being shown? Matches the query string too, so Personnel
 * files (/pilots) and My file (/pilots?ucid=..) are never lit together.
 */
export function isActive(to: string, pathname: string, search: string): boolean {
  const [toPath, toQuery] = to.split('?')
  const exact = toPath === '/' || toPath === '/admin'
  const pathMatches = exact ? pathname === toPath : pathname === toPath || pathname.startsWith(toPath + '/')
  if (!pathMatches) return false
  return toQuery ? search === `?${toQuery}` : !search.includes('ucid=')
}

/** The key whose page this is, for page headers ("F5 · Target folders"). */
export function keyFor(pathname: string): FKey | undefined {
  return functionKeys({ side: 'Blue', is_admin: false, ucid: null }).find(k => isActive(k.to, pathname, ''))
}

/**
 * The shortcut for a key event: Alt+1..Alt+8 press F1..F8. The real F-keys
 * are left to the browser (F5 reloads, F12 opens the tools, F1, F3 and F6
 * all do something), so the console's keys are on Alt+number. Nothing fires
 * while the viewer is typing in a field.
 */
export function shortcutIndex(e: Pick<KeyboardEvent, 'altKey' | 'ctrlKey' | 'metaKey' | 'shiftKey' | 'code'>, typing: boolean): number | null {
  if (typing || !e.altKey || e.ctrlKey || e.metaKey || e.shiftKey) return null
  const m = /^(?:Digit|Numpad)([1-8])$/.exec(e.code)
  return m ? Number(m[1]) - 1 : null
}

/** Whether focus is somewhere a key press is text. */
export function isTyping(target: EventTarget | null): boolean {
  if (!target || typeof (target as HTMLElement).tagName !== 'string') return false
  const el = target as HTMLElement
  const tag = el.tagName
  return tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || el.isContentEditable === true
}
