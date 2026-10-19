/**
 * One glyph per range discipline: the range pack in the shared set
 * (shared/icons/index.tsx, "Training range"), the same drawings the bot's
 * Discord emoji are rendered from. CAS, SEAD and CSAR keep the campaign's own;
 * CAS as the reticle, which is what its Discord emoji shows too.
 */
import {
  AntiShip,
  Bfm,
  Bombing,
  Crosshair,
  Csar,
  FieldLanding,
  Gunnery,
  Helipad,
  HotZone,
  LowLevel,
  MissileDefence,
  Refuel,
  Sead,
  SlingLoad,
  Strafe,
  Trap,
  TroopDrop,
  type IconComponent,
} from '@icons'
import type { ResultKind } from '../types'

export const KIND_ICON: Record<ResultKind, IconComponent> = {
  bomb: Bombing,
  strafe: Strafe,
  trap: Trap,
  aar: Refuel,
  missile: MissileDefence,
  engagement: Bfm,
  anti_ship: AntiShip,
  sling: SlingLoad,
  landing: Helipad,
  troops: TroopDrop,
  gunnery: Gunnery,
  cas: Crosshair,
  sead: Sead,
  hot_zone: HotZone,
  low_level: LowLevel,
  field_landing: FieldLanding,
  csar: Csar,
}

export function KindIcon({ kind, size = 16, className }: { kind: ResultKind; size?: number; className?: string }) {
  const I = KIND_ICON[kind]
  return <I size={size} className={className} />
}
