// Making a clickable element that is not a <button> usable without a mouse.
//
// A real <button> is always better where the layout allows. Where it does not
// (a table row, a map marker, a card), spread `pressable(handler)` onto the
// element: it becomes a focusable button for keyboards and screen readers,
// and Enter or Space press it.

import type { KeyboardEvent, MouseEvent } from 'react'

/** What a pressable's handler gets: the click, or the key that pressed it.
 *  Both carry stopPropagation() and the modifier keys. */
export type PressEvent<E extends Element = Element> = MouseEvent<E> | KeyboardEvent<E>

export function pressable<E extends Element = HTMLElement>(
  onPress: (e: PressEvent<E>) => void,
  label?: string,
) {
  return {
    role: 'button' as const,
    tabIndex: 0,
    onClick: (e: MouseEvent<E>) => onPress(e),
    onKeyDown: (e: KeyboardEvent<E>) => {
      if (e.target !== e.currentTarget) return
      if (e.key === 'Enter' || e.key === ' ') {
        e.preventDefault()
        onPress(e)
      }
    },
    ...(label ? { 'aria-label': label } : {}),
  }
}
