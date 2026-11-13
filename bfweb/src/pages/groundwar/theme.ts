// Colours and thresholds shared by every piece of the ground-war screen. The
// HUD is a game screen laid over satellite imagery, so it keeps its own dark
// glass palette whatever the dashboard theme is; only the TAC map style
// follows the theme.
import type { GroundRole } from '../../api'

export type Side = 'Blue' | 'Red'

export const SIDE_COLOR = { Blue: '#4a8fd4', Red: '#cc4444', Neutral: '#8a8f80' } as const
/** Side colours lifted for small things on dark imagery (vehicles, players). */
export const SIDE_BRIGHT = { Blue: '#7db8ff', Red: '#ff6b5e', Neutral: '#b9bca9' } as const

/** Chinagraph yellow: selection, orders, anything the viewer is about to do. */
export const PENCIL = '#ffd23f'
export const FIRE = '#ff8c1a'
export const ATTACK = '#ff5b45'
export const WITHDRAW = '#f2c14e'
export const BONE = '#e6e1cf'

export const other = (s: Side): Side => (s === 'Blue' ? 'Red' : 'Blue')

/** Every map icon -- formations, enemy contacts, our assets, pilots, bases --
 *  is drawn to one scale, so none dominates: a NATO frame is `ICON` px tall
 *  (milsymbol's `size`), a glyph `GLYPH` px square. */
export const ICON = 18
export const GLYPH = 24

/** At or above this zoom the map draws every vehicle instead of one symbol. */
export const NEAR_ZOOM = 10.5

export const ROLE_LABEL: Record<GroundRole, string> = {
  tank: 'Tank',
  ifv: 'IFV',
  apc: 'APC',
  recon: 'Recon',
  aaa: 'AAA',
  sam: 'SAM',
  artillery: 'Artillery',
  infantry: 'Infantry',
  truck: 'Truck',
}
export const ROLES: GroundRole[] = ['tank', 'ifv', 'apc', 'recon', 'aaa', 'sam', 'artillery', 'infantry', 'truck']

export const reducedMotion = (): boolean => {
  try {
    return window.matchMedia('(prefers-reduced-motion: reduce)').matches
  } catch {
    return false
  }
}
