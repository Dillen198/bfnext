/**
 * The F10 radio menu's behaviour, kept apart from the drawing so it can be
 * tested without a browser.
 *
 * DCS numbers the entries of the current menu F1..F10, puts "F11. Back" and
 * "F12. Exit" under them, and moves anything past the tenth entry into a
 * "Next Page..." submenu in the F10 slot (nine entries plus the link).
 */
import type { MenuItem } from './parse'

/** DCS shows ten entries per menu page. */
export const PAGE_SIZE = 10
export const NEXT_PAGE = 'Next Page'

/** The tree as DCS lays it out: long menus split across "Next Page" submenus. */
export function paginate(items: MenuItem[]): MenuItem[] {
  const fixed = items.map(it => (it.items ? { ...it, items: paginate(it.items) } : it))
  if (fixed.length <= PAGE_SIZE) return fixed
  const head = fixed.slice(0, PAGE_SIZE - 1)
  const rest = fixed.slice(PAGE_SIZE - 1)
  return [...head, { label: NEXT_PAGE, items: paginate(rest) }]
}

export interface MenuState {
  /** Indices from the root to the submenu being shown. */
  path: number[]
  /** false after F12. Exit, until the menu is opened again. */
  open: boolean
}

export const initialMenu: MenuState = { path: [], open: true }

export interface Level {
  items: MenuItem[]
  /** The labels from the root down to here, root first. */
  trail: string[]
}

export function levelAt(tree: MenuItem[], root: string, path: number[]): Level {
  let items = tree
  const trail = [root]
  for (const i of path) {
    const it = items[i]
    if (!it?.items) break
    items = it.items
    trail.push(it.label)
  }
  return { items, trail }
}

export type MenuAction =
  | { type: 'pick'; index: number }
  | { type: 'back' }
  | { type: 'exit' }
  | { type: 'open' }

export interface MenuStep {
  state: MenuState
  /** The command that was run, when the action picked one. */
  fired?: MenuItem
}

export function menuReducer(tree: MenuItem[], state: MenuState, action: MenuAction): MenuStep {
  switch (action.type) {
    case 'open':
      return { state: { path: [], open: true } }
    case 'exit':
      return { state: { path: [], open: false } }
    case 'back':
      if (!state.open) return { state }
      return { state: { ...state, path: state.path.slice(0, -1) } }
    case 'pick': {
      if (!state.open) return { state }
      const { items } = levelAt(tree, '', state.path)
      const it = items[action.index]
      if (!it) return { state }
      if (it.items) return { state: { ...state, path: [...state.path, action.index] } }
      return { state, fired: it }
    }
  }
}

/**
 * A key, as KeyboardEvent.key names it, to what DCS would do with it. The
 * digit row stands in for F1..F10 (1 = F1, 0 = F10) because the browser keeps
 * most function keys for itself; the real F-keys work too where it lets them
 * through. Backspace is F11 (Back), Escape is F12 (Exit), and backslash opens
 * the menu, as it does in DCS.
 */
export function keyToAction(key: string): MenuAction | null {
  if (/^[1-9]$/.test(key)) return { type: 'pick', index: Number(key) - 1 }
  if (key === '0') return { type: 'pick', index: 9 }
  const f = /^F(\d{1,2})$/.exec(key)
  if (f) {
    const n = Number(f[1])
    if (n >= 1 && n <= 10) return { type: 'pick', index: n - 1 }
    if (n === 11) return { type: 'back' }
    if (n === 12) return { type: 'exit' }
  }
  if (key === 'Backspace') return { type: 'back' }
  if (key === 'Escape') return { type: 'exit' }
  if (key === '\\') return { type: 'open' }
  return null
}

/** "F1. Logistics..." -- the way DCS writes an entry. Submenus end in "...". */
export function entryText(it: MenuItem, index: number): string {
  return `F${index + 1}. ${it.label}${it.items ? '...' : ''}`
}
