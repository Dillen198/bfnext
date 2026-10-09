import {
  Airbase,
  Armor,
  Carrier,
  Cas,
  Cap,
  Compass,
  Crosshair,
  Csar,
  Explosion,
  Helicopter,
  Infantry,
  Intercept,
  Logistics,
  Refuel,
  Sead,
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
  sead: Sead,
  hot_zone: Explosion,
  low_level: Compass,
  field_landing: Airbase,
  csar: Csar,
}

export function KindIcon({ kind, size = 16, className }: { kind: ResultKind; size?: number; className?: string }) {
  const I = KIND_ICON[kind]
  return <I size={size} className={className} />
}
