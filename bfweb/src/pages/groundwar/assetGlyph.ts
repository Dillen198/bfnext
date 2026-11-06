// The SVG markup for an asset's map marker (command.tsx renders it with
// dangerouslySetInnerHTML, as the markers are a few hundred tiny SVGs).
//
// Safe to inject because nothing from the server reaches the markup: the
// asset's fields (kind, role, typ) only choose between the fixed fragments
// below, and the colour comes from our own palette (theme.ts). Keep it that
// way -- interpolate a server string here and it becomes an XSS hole.
// assetGlyph.test.ts checks it.
import type { Asset } from '../../api'
import { playerSvg } from './sprites'

const HELO = /^(Mi-|Ka-|AH-|UH-|CH-|SA342|OH-58|Tiger|SH-60|MH-60|OH58)/i
const AIR_DEF = /hawk|patriot|nasams|roland|sa-|s-300|tor|osa|buk|kub|strela|stinger|avenger|chaparral|linebacker|igla|shilka|tunguska|gepard|zsu|vulcan|rapier|hq-7|aaa|sam/i

/** A small friendly frame with what's inside saying what it is. */
function groundGlyph(a: Asset, fill: string): string {
  const inner = (() => {
    switch (a.kind) {
      case 'troops':
        return '<path d="M3 3 L21 15 M21 3 L3 15" stroke="#000" stroke-width="1.6"/>'
      case 'artillery':
        return '<circle cx="12" cy="9" r="3.2" fill="#000"/>'
      case 'convoy':
        return '<line x1="3" y1="9" x2="21" y2="9" stroke="#000" stroke-width="1.6"/><circle cx="8" cy="15.5" r="1.6" fill="#000"/><circle cx="16" cy="15.5" r="1.6" fill="#000"/>'
      default:
        return AIR_DEF.test(`${a.role} ${a.typ}`)
          ? '<path d="M5 15 Q12 2 19 15" fill="none" stroke="#000" stroke-width="1.6"/>'
          : '<ellipse cx="12" cy="9" rx="6.5" ry="3.6" fill="none" stroke="#000" stroke-width="1.6"/>'
    }
  })()
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="-1 -1 26 20" width="24" height="18"><rect x="0" y="0" width="24" height="18" fill="${fill}" stroke="#000" stroke-width="1.4"/>${inner}</svg>`
}

export function assetSvg(a: Asset, fill: string): string {
  if (a.kind === 'air') return playerSvg(HELO.test(a.typ) ? 'helicopter' : 'plane', fill)
  if (a.kind === 'naval') return playerSvg('ship', fill)
  return groundGlyph(a, fill)
}
