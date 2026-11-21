import { useEffect, useMemo, useRef, useState } from 'react'
import type { F10MenuSpec, MenuItem } from './parse'
import { entryText, initialMenu, keyToAction, levelAt, menuReducer, paginate, type MenuAction } from './f10menuModel'
import { DcsMessageBox, DcsScreen } from './DcsMessage'

/**
 * The ```f10menu block: a working copy of DCS's radio menu. Click an entry or
 * press its number; a submenu opens in place, a command prints what the
 * server says in the message box, top right, as it would in the game.
 */
export default function F10Menu({ spec }: { spec: F10MenuSpec }) {
  const tree = useMemo(() => paginate(spec.items), [spec.items])
  const [state, setState] = useState(initialMenu)
  const [fired, setFired] = useState<MenuItem | null>(null)
  const menuRef = useRef<HTMLDivElement>(null)
  const openRef = useRef<HTMLButtonElement>(null)
  // Set when an action came from inside the menu, so focus follows it to the
  // next level instead of falling to the page when the clicked entry goes.
  const keepFocus = useRef(false)

  const { items, trail } = levelAt(tree, spec.root, state.path)

  function act(action: MenuAction) {
    const el = document.activeElement
    keepFocus.current =
      !!el && (!!menuRef.current?.contains(el) || el === openRef.current)
    const step = menuReducer(tree, state, action)
    setState(step.state)
    if (step.fired) setFired(step.fired)
  }

  useEffect(() => {
    if (!keepFocus.current) return
    keepFocus.current = false
    if (state.open) menuRef.current?.focus()
    else openRef.current?.focus()
  }, [state])

  function onKeyDown(e: React.KeyboardEvent) {
    if (e.altKey || e.ctrlKey || e.metaKey) return
    if (e.key === 'ArrowDown' || e.key === 'ArrowUp' || e.key === 'Home' || e.key === 'End') {
      const entries = Array.from(menuRef.current?.querySelectorAll<HTMLElement>('[role="menuitem"]') ?? [])
      if (entries.length === 0) return
      e.preventDefault()
      const at = entries.indexOf(document.activeElement as HTMLElement)
      const next =
        e.key === 'Home' ? 0
          : e.key === 'End' ? entries.length - 1
            : e.key === 'ArrowDown' ? (at + 1) % entries.length
              : (at <= 0 ? entries.length : at) - 1
      entries[next].focus()
      return
    }
    const action = keyToAction(e.key)
    if (!action) return
    e.preventDefault()
    act(action)
  }

  const where = trail.join(' > ')

  return (
    <div className="mk-block mk-f10-block">
      <DcsScreen label="F10 radio menu replica" className="mk-screen--f10">
        <div className="mk-screen-tl">
          {state.open ? (
            <div className="mk-f10">
              <div className="mk-f10-title">{trail[trail.length - 1]}</div>
              <div
                ref={menuRef}
                role="menu"
                tabIndex={0}
                aria-label={`Radio menu: ${where}. Press a number to pick an entry, Backspace for back, Escape to exit.`}
                className="mk-f10-list"
                onKeyDown={onKeyDown}
              >
                {items.map((it, i) => (
                  <button
                    type="button"
                    role="menuitem"
                    tabIndex={-1}
                    key={`${state.path.join('.')}:${i}`}
                    className={`mk-f10-item${it.items ? ' mk-f10-item--sub' : ''}`}
                    aria-haspopup={it.items ? 'menu' : undefined}
                    onClick={() => act({ type: 'pick', index: i })}
                  >
                    {entryText(it, i)}
                  </button>
                ))}
                {items.length === 0 && <div className="mk-f10-empty">(empty)</div>}
                <div className="mk-f10-sep" role="separator" />
                <button type="button" role="menuitem" tabIndex={-1} className="mk-f10-item" onClick={() => act({ type: 'back' })}>
                  F11. Back
                </button>
                <button type="button" role="menuitem" tabIndex={-1} className="mk-f10-item" onClick={() => act({ type: 'exit' })}>
                  F12. Exit
                </button>
              </div>
            </div>
          ) : (
            <button ref={openRef} type="button" className="mk-f10-reopen" onClick={() => act({ type: 'open' })} onKeyDown={onKeyDown}>
              Radio menu closed. Press <kbd>\</kbd> or click to open it.
            </button>
          )}
        </div>
        <div className="mk-screen-tr" aria-live="polite">
          {fired?.says && <DcsMessageBox key={fired.label + fired.says} text={fired.says} />}
        </div>
      </DcsScreen>
      {fired?.note && (
        <p className="mk-note">
          <strong>{fired.label}:</strong> {fired.note}
        </p>
      )}
      <p className="mk-hint">
        Click an entry, or focus the menu and press <kbd>1</kbd>–<kbd>9</kbd> (<kbd>0</kbd> is F10),{' '}
        <kbd>Backspace</kbd> for F11 and <kbd>Esc</kbd> for F12.
      </p>
    </div>
  )
}
