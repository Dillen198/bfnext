import { describe, expect, it } from 'vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import { MemoryRouter } from 'react-router-dom'
import { functionKeys, isActive, isTyping, keyFor, moreGroups, shortcutIndex } from './keys'
import FunctionKeyStrip from './FunctionKeys'

const pilot = { side: 'Blue' as const, is_admin: false, ucid: 'abc' }
const linked = { side: null, is_admin: false, ucid: 'abc' }

const key = (over: Partial<KeyboardEvent> = {}) => ({
  altKey: true, ctrlKey: false, metaKey: false, shiftKey: false, code: 'Digit1', ...over,
})

describe('function keys', () => {
  it('are F1 to F8 in the situation room order', () => {
    const keys = functionKeys(pilot)
    expect(keys.map(k => `${k.code} ${k.label}`)).toEqual([
      'F1 Situation', 'F2 Plot', 'F3 Wire', 'F4 Front HQ',
      'F5 Targets', 'F6 Honours', 'F7 Losses', 'F8 Briefing',
    ])
    expect(keys.map(k => k.to)).toEqual(['/', '/map', '/news', '/hq', '/objectives', '/leaderboard', '/kills', '/briefing'])
  })

  it('lock the coalition pages for a visitor, and send them to sign in', () => {
    const keys = functionKeys(null)
    const locked = keys.filter(k => k.locked)
    expect(locked.map(k => k.code)).toEqual(['F4', 'F8'])
    expect(locked.every(k => k.locked!.go === '/login')).toBe(true)
  })

  it('send a signed-in pilot with no side to the page explainer', () => {
    const hq = functionKeys(linked).find(k => k.code === 'F4')!
    expect(hq.locked?.go).toBe('/hq')
    expect(hq.locked?.reason).toMatch(/linkme/)
  })

  it('unlock for a pilot with a side, and for an admin', () => {
    expect(functionKeys(pilot).some(k => k.locked)).toBe(false)
    expect(functionKeys({ side: null, is_admin: true, ucid: null }).some(k => k.locked)).toBe(false)
  })
})

describe('the MORE menu', () => {
  it('holds the rest of the pages, admin only for admins', () => {
    const labels = (u: Parameters<typeof moreGroups>[0]) =>
      moreGroups(u, {}).flatMap(g => g.items.map(i => i.label))
    expect(labels(pilot)).toEqual(expect.arrayContaining(['Recon intel', 'Ground command', 'Personnel files', 'My file', 'Squadrons', 'Rounds', 'Replay', 'About']))
    expect(labels(pilot)).not.toContain('Admin')
    expect(labels({ ...pilot, is_admin: true })).toEqual(expect.arrayContaining(['Admin', 'Config', 'Server ops']))
  })

  it('links a pilot to their own file', () => {
    const mine = moreGroups(pilot, {}).flatMap(g => g.items).find(i => i.label === 'My file')!
    expect(mine.to).toBe('/pilots?ucid=abc')
    expect(mine.locked).toBeUndefined()
  })

  it('lists the wiki and support only when configured', () => {
    const items = moreGroups(null, { wikiUrl: 'https://wiki.example', donationUrl: '' }).flatMap(g => g.items)
    expect(items.find(i => i.label === 'Wiki')).toMatchObject({ external: true, to: 'https://wiki.example' })
    expect(items.find(i => i.label === 'Support')).toBeUndefined()
  })
})

describe('isActive', () => {
  it('lights the situation key on the home page only', () => {
    expect(isActive('/', '/', '')).toBe(true)
    expect(isActive('/', '/map', '')).toBe(false)
  })
  it('lights a key on its sub-pages', () => {
    expect(isActive('/pilots', '/pilots/x/sortie/1', '')).toBe(true)
  })
  it('never lights personnel files and my file together', () => {
    expect(isActive('/pilots', '/pilots', '?ucid=abc')).toBe(false)
    expect(isActive('/pilots?ucid=abc', '/pilots', '?ucid=abc')).toBe(true)
  })
  it('names the key for a page header', () => {
    expect(keyFor('/objectives')?.code).toBe('F5')
    expect(keyFor('/about')).toBeUndefined()
  })
})

describe('shortcuts', () => {
  it('press F1..F8 with Alt+1..Alt+8', () => {
    expect(shortcutIndex(key({ code: 'Digit1' }), false)).toBe(0)
    expect(shortcutIndex(key({ code: 'Digit8' }), false)).toBe(7)
    expect(shortcutIndex(key({ code: 'Numpad5' }), false)).toBe(4)
  })
  it('leave the real F-keys and other chords to the browser', () => {
    expect(shortcutIndex(key({ altKey: false, code: 'F5' }), false)).toBeNull()
    expect(shortcutIndex(key({ code: 'Digit9' }), false)).toBeNull()
    expect(shortcutIndex(key({ ctrlKey: true }), false)).toBeNull()
    expect(shortcutIndex(key({ shiftKey: true }), false)).toBeNull()
    expect(shortcutIndex(key({ altKey: false }), false)).toBeNull()
  })
  it('do nothing while typing', () => {
    expect(shortcutIndex(key(), true)).toBeNull()
    expect(isTyping({ tagName: 'INPUT' } as unknown as EventTarget)).toBe(true)
    expect(isTyping({ tagName: 'DIV', isContentEditable: true } as unknown as EventTarget)).toBe(true)
    expect(isTyping({ tagName: 'BUTTON' } as unknown as EventTarget)).toBe(false)
    expect(isTyping(null)).toBe(false)
  })
})

describe('the key strip', () => {
  const render = (path: string, user: Parameters<typeof functionKeys>[0]) => {
    const [pathname, query] = path.split('?')
    return renderToStaticMarkup(
      <MemoryRouter initialEntries={[path]}>
        <FunctionKeyStrip keys={functionKeys(user)} pathname={pathname} search={query ? `?${query}` : ''} />
      </MemoryRouter>,
    )
  }

  it('renders the eight keys with their codes and shortcuts', () => {
    const html = render('/', pilot)
    for (let i = 1; i <= 8; i++) {
      expect(html).toContain(`>F${i}</span>`)
      expect(html).toContain(`aria-keyshortcuts="Alt+${i}"`)
    }
  })

  it('lights only the key for this page', () => {
    const html = render('/kills', pilot)
    expect(html.match(/aria-current="page"/g)).toHaveLength(1)
    expect(html).toMatch(/class="rsr-fkey active"[^>]*href="\/kills"|href="\/kills"[^>]*class="rsr-fkey active"/)
  })

  it('shows a locked key as locked, pointing at sign-in', () => {
    const html = render('/', null)
    expect(html).toMatch(/href="\/login"[^>]*class="rsr-fkey locked"|class="rsr-fkey locked"[^>]*href="\/login"/)
    expect(html).toContain('aria-label="locked"')
  })
})
