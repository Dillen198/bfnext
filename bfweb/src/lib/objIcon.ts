import {
  Airbase,
  Farp,
  Fob,
  Factory,
  LogiHub,
  NavalBase,
  Carrier,
  CommandCenter,
  type IconComponent,
} from '@icons'

/** Glyph per objective kind -- the TacMap markers and the SITREP lists use
 *  the same set, so an objective reads the same way everywhere. */
export const OBJ_ICON: Record<string, IconComponent> = {
  Airbase, FARP: Farp, FOB: Fob, Factory,
  'Logistics Hub': LogiHub, 'Naval Base': NavalBase,
  'Carrier Group': Carrier, 'Command Center': CommandCenter,
}
