import { describe, expect, it } from 'vitest'
import type { MenuItem } from './parse'
import { NEXT_PAGE, entryText, initialMenu, keyToAction, levelAt, menuReducer, paginate, type MenuAction, type MenuState } from './f10menuModel'

const TREE: MenuItem[] = [
  {
    label: 'Cargo',
    items: [
      { label: 'Unpack', says: 'Unpacked' },
      { label: 'Crates', items: [{ label: 'SA-3 Launcher', says: 'Spawned', note: 'n' }] },
    ],
  },
  { label: 'Troops', items: [] },
  { label: 'Help', says: 'Read the wiki' },
]

function run(actions: MenuAction[], tree = TREE) {
  let state: MenuState = initialMenu
  let fired: MenuItem | undefined
  for (const a of actions) {
    const step = menuReducer(tree, state, a)
    state = step.state
    fired = step.fired ?? fired
  }
  return { state, fired }
}

describe('f10menu navigation', () => {
  it('drills into submenus and back out', () => {
    const { state } = run([{ type: 'pick', index: 0 }, { type: 'pick', index: 1 }])
    expect(state.path).toEqual([0, 1])
    expect(levelAt(TREE, 'F10 Other', state.path).trail).toEqual(['F10 Other', 'Cargo', 'Crates'])
    const back = menuReducer(TREE, state, { type: 'back' }).state
    expect(back.path).toEqual([0])
  })

  it('back at the top stays at the top', () => {
    expect(run([{ type: 'back' }]).state).toEqual(initialMenu)
  })

  it('a command fires and the menu stays where it is', () => {
    const { state, fired } = run([{ type: 'pick', index: 0 }, { type: 'pick', index: 1 }, { type: 'pick', index: 0 }])
    expect(fired?.says).toBe('Spawned')
    expect(state.path).toEqual([0, 1])
  })

  it('an entry past the end does nothing', () => {
    const { state, fired } = run([{ type: 'pick', index: 7 }])
    expect(state).toEqual(initialMenu)
    expect(fired).toBeUndefined()
  })

  it('exit closes the menu and forgets where it was; open starts at the top', () => {
    const closed = run([{ type: 'pick', index: 0 }, { type: 'exit' }]).state
    expect(closed).toEqual({ path: [], open: false })
    // picks do nothing while it is closed
    expect(menuReducer(TREE, closed, { type: 'pick', index: 0 }).state).toEqual(closed)
    expect(menuReducer(TREE, closed, { type: 'open' }).state).toEqual(initialMenu)
  })

  it('an empty submenu still opens', () => {
    const { state } = run([{ type: 'pick', index: 1 }])
    expect(levelAt(TREE, 'F10 Other', state.path).items).toEqual([])
  })
})

describe('f10menu keys', () => {
  it('maps the digit row and the F-keys the way DCS numbers entries', () => {
    expect(keyToAction('1')).toEqual({ type: 'pick', index: 0 })
    expect(keyToAction('9')).toEqual({ type: 'pick', index: 8 })
    expect(keyToAction('0')).toEqual({ type: 'pick', index: 9 })
    expect(keyToAction('F3')).toEqual({ type: 'pick', index: 2 })
    expect(keyToAction('F10')).toEqual({ type: 'pick', index: 9 })
    expect(keyToAction('F11')).toEqual({ type: 'back' })
    expect(keyToAction('Backspace')).toEqual({ type: 'back' })
    expect(keyToAction('F12')).toEqual({ type: 'exit' })
    expect(keyToAction('Escape')).toEqual({ type: 'exit' })
    expect(keyToAction('\\')).toEqual({ type: 'open' })
    expect(keyToAction('a')).toBeNull()
    expect(keyToAction('F13')).toBeNull()
  })

  it('keys drive the same navigation', () => {
    const keys = ['1', '2', '1'].map(k => keyToAction(k)!)
    expect(run(keys).fired?.label).toBe('SA-3 Launcher')
  })
})

describe('f10menu layout', () => {
  it('writes entries as DCS does, submenus ending in ...', () => {
    expect(entryText(TREE[0], 0)).toBe('F1. Cargo...')
    expect(entryText(TREE[2], 2)).toBe('F3. Help')
  })

  it('splits more than ten entries into Next Page submenus', () => {
    const many = Array.from({ length: 23 }, (_, i) => ({ label: `E${i + 1}` }))
    const tree = paginate(many)
    expect(tree).toHaveLength(10)
    expect(tree[9].label).toBe(NEXT_PAGE)
    expect(tree[8].label).toBe('E9')
    const page2 = tree[9].items!
    expect(page2).toHaveLength(10)
    expect(page2[0].label).toBe('E10')
    expect(page2[9].label).toBe(NEXT_PAGE)
    expect(page2[9].items!.map(i => i.label)).toEqual(['E19', 'E20', 'E21', 'E22', 'E23'])
    // ten exactly fits on one page
    expect(paginate(many.slice(0, 10))).toHaveLength(10)
  })
})
