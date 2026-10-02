import {
  Armor,
  Carrier,
  Cas,
  Cap,
  Crosshair,
  Helicopter,
  Infantry,
  Intercept,
  Logistics,
  Refuel,
  Ship,
  Strike,
  type IconComponent,
} from '@icons'
import type { ResultKind } from '../types'

export const KIND_ICON: Record<ResultKind, IconComponent> = {
  bomb: Strike,
  strafe: Crosshair,
  trap: Carrier,
  aar: Refuel,
  missile: Intercept,
  engagement: Cap,
  anti_ship: Ship,
  sling: Logistics,
  landing: Helicopter,
  troops: Infantry,
  gunnery: Armor,
  cas: Cas,
}

export function KindIcon({ kind, size = 16, className }: { kind: ResultKind; size?: number; className?: string }) {
  const I = KIND_ICON[kind]
  return <I size={size} className={className} />
}
