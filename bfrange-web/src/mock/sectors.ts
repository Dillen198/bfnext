/**
 * The range's 27 sectors, copied from bfrange/RANGE_CFG.sample.json (`sectors`)
 * as the engine serves them in the live picture (`announce` defaults to true).
 * A copy, not an import: the site builds without anything outside bfrange-web.
 * Regenerate by hand if the sample config changes.
 */
import type { Sector, SectorPoint } from '../types'

const ll = (lat: number, lon: number): SectorPoint => ({ lat, lon })

export const SECTORS: Sector[] = [
  {
    id: 'r-1', name: 'R-1 SAMGORI', kind: 'air_to_ground', side: 'blue', announce: true,
    purpose: 'Bomb circle, strafe pit (run in from the east)',
    shape: { polygon: [ll(41.64075, 45.14635), ll(41.62683, 45.27543), ll(41.56522, 45.26358), ll(41.57912, 45.13462)] },
  },
  {
    id: 'g-1', name: 'G-1 LILO', kind: 'gunnery', side: 'blue', announce: true,
    purpose: 'Combined Arms gunnery lane',
    shape: { polygon: [ll(41.68524, 45.16334), ll(41.67892, 45.22206), ll(41.65075, 45.21665), ll(41.65707, 45.15795)] },
  },
  {
    id: 'r-2', name: 'R-2 IORI', kind: 'tactical', side: 'blue', announce: true,
    purpose: 'Tactical array (laser 1688), moving convoy, JDAM targets. JTAC Axeman 133.0 AM',
    shape: { polygon: [ll(41.64726, 45.2973), ll(41.63188, 45.43804), ll(41.55272, 45.42261), ll(41.56806, 45.28203)] },
  },
  {
    id: 'r-3', name: 'R-3 TETRI TSKARO', kind: 'threat', side: 'blue', announce: true,
    purpose: 'SA-8 site, radar on / weapons hold. Instructors can spawn more SAMs',
    shape: { polygon: [ll(41.53797, 44.3248), ll(41.52144, 44.4893), ll(41.44199, 44.47508), ll(41.45848, 44.31078)] },
  },
  {
    id: 'h-1', name: 'H-1 VAZIANI', kind: 'helo', side: 'blue', announce: true,
    purpose: 'Precision pad, confined LZ, pinnacle, sling-load and troop courses',
    shape: { polygon: [ll(41.66759, 45.0403), ll(41.65819, 45.12838), ll(41.6124, 45.11967), ll(41.62178, 45.03164)] },
  },
  {
    id: 'h-2', name: 'H-2 TSKALTUBO', kind: 'helo', side: 'blue', announce: true,
    purpose: 'Mountain flying: pads, confined LZ, pinnacle, sling and troop courses near Kutaisi',
    shape: { polygon: [ll(42.40478, 42.48637), ll(42.39166, 42.64213), ll(42.2052, 42.61345), ll(42.21824, 42.45814)] },
  },
  {
    id: 'moa-kakheti', name: 'MOA KAKHETI', kind: 'air_to_air', side: 'blue', announce: true,
    purpose: 'BFM over the mountains. Floor 8,000 ft',
    shape: { circle: { lat: 41.93, lon: 45.18, radius_m: 20372 } },
  },
  {
    id: 'ar-4', name: 'AR-4 TEXACO 2', kind: 'aar', side: 'blue', announce: true,
    purpose: 'KC-135 boom, FL240, TACAN 54Y, 254.0',
    shape: { track: { lat: 41.95, lon: 43.55, heading_deg: 90, leg_m: 55560, width_m: 14816 } },
  },
  {
    id: 'r-11', name: 'R-11 STEPNOYE', kind: 'air_to_ground', side: 'red', announce: true,
    purpose: 'Bomb circle, strafe pit (run in from the east)',
    shape: { polygon: [ll(44.46776, 44.39835), ll(44.45397, 44.53386), ll(44.39222, 44.52156), ll(44.40598, 44.38619)] },
  },
  {
    id: 'g-11', name: 'G-11 STARODUB', kind: 'gunnery', side: 'red', announce: true,
    purpose: 'Combined Arms gunnery lane',
    shape: { polygon: [ll(44.49219, 44.15189), ll(44.48607, 44.21357), ll(44.45781, 44.20809), ll(44.46392, 44.14644)] },
  },
  {
    id: 'r-12', name: 'R-12 KARST', kind: 'tactical', side: 'red', announce: true,
    purpose: 'Tactical array (laser 1511), moving convoy, JDAM targets. JTAC Topor 124.5 AM',
    shape: { polygon: [ll(44.42769, 44.21295), ll(44.41289, 44.36077), ll(44.34228, 44.34695), ll(44.35704, 44.1993)] },
  },
  {
    id: 'r-13', name: 'R-13 ACHIKULAK', kind: 'threat', side: 'red', announce: true,
    purpose: 'SA-8 site, radar on / weapons hold. Instructors can spawn more SAMs',
    shape: { polygon: [ll(44.60786, 44.3523), ll(44.59031, 44.52519), ll(44.5021, 44.50756), ll(44.5196, 44.33492)] },
  },
  {
    id: 'r-14', name: 'R-14 KUBAN', kind: 'air_to_ground', side: 'red', announce: true,
    purpose: 'Bomb circle, strafe pit, tactical array (laser 1512) for Maykop and Krymsk',
    shape: { polygon: [ll(44.99078, 40.16546), ll(44.98277, 40.2913), ll(44.9292, 40.28448), ll(44.93719, 40.15875)] },
  },
  {
    id: 'h-11', name: 'H-11 MOZDOK', kind: 'helo', side: 'red', announce: true,
    purpose: 'Precision pad, confined LZ, pinnacle, sling-load and troop courses',
    shape: { polygon: [ll(43.82765, 44.63186), ll(43.81818, 44.72319), ll(43.77234, 44.7141), ll(43.78179, 44.62284)] },
  },
  {
    id: 'h-12', name: 'H-12 NALCHIK', kind: 'helo', side: 'red', announce: true,
    purpose: 'Mountain flying: pads, confined LZ, pinnacle, sling and troop courses',
    shape: { polygon: [ll(43.54997, 43.64278), ll(43.54018, 43.74617), ll(43.43403, 43.72714), ll(43.44377, 43.62393)] },
  },
  {
    id: 'moa-nogai', name: 'MOA NOGAI', kind: 'air_to_air', side: 'red', announce: true,
    purpose: 'BFM over the steppe. Floor 5,000 ft',
    shape: { circle: { lat: 44.25, lon: 45.3, radius_m: 27780 } },
  },
  {
    id: 'ar-5', name: 'AR-5 ILYUSHA', kind: 'aar', side: 'red', announce: true,
    purpose: 'IL-78M basket, FL230, TACAN 59Y, 124.0',
    shape: { track: { lat: 44.65, lon: 43.3, heading_deg: 90, leg_m: 55560, width_m: 14816 } },
  },
  {
    id: 'ar-6', name: 'AR-6 ILYUSHA WEST', kind: 'aar', side: 'red', announce: true,
    purpose: 'IL-78M basket, FL220, TACAN 58Y, 123.0',
    shape: { track: { lat: 44.3, lon: 37.3, heading_deg: 90, leg_m: 55560, width_m: 14816 } },
  },
  {
    id: 'oparea', name: 'CV OPAREA', kind: 'carrier', side: 'all', announce: true,
    purpose: 'CVN-72 TACAN 72X ICLS 11 Link-4 336.0 | LHA-1 TACAN 71X | recovery tanker A-6E 63Y 261.0',
    shape: { circle: { lat: 41.7, lon: 41, radius_m: 37040 } },
  },
  {
    id: 'as-1', name: 'AS-1 SHIPPING', kind: 'anti_ship', side: 'all', announce: true,
    purpose: 'Undefended merchant ships steaming west. Spawn warships from the site or F10',
    shape: { polygon: [ll(42.55796, 40.08125), ll(42.5023, 40.92627), ll(42.10088, 40.87576), ll(42.15576, 40.03602)] },
  },
  {
    id: 'w-1', name: 'W-1 BFM NORTH', kind: 'air_to_air', side: 'all', announce: true,
    purpose: 'BFM set-ups and duels. Floor 5,000 ft',
    shape: { circle: { lat: 42.3, lon: 39.45, radius_m: 27780 } },
  },
  {
    id: 'w-2', name: 'W-2 BFM SOUTH', kind: 'air_to_air', side: 'all', announce: true,
    purpose: 'BFM set-ups and duels. Floor 5,000 ft',
    shape: { circle: { lat: 41.75, lon: 39.45, radius_m: 27780 } },
  },
  {
    id: 'w-3', name: 'W-3 BVR', kind: 'bvr', side: 'all', announce: true,
    purpose: 'BVR presentations, Fox 1 / Fox 3 missile defence',
    shape: { polygon: [ll(43.97926, 37.34252), ll(43.91277, 38.832), ll(43.01694, 38.74624), ll(43.08138, 37.27848)] },
  },
  {
    id: 'w-4', name: 'W-4 DUEL', kind: 'duel', side: 'all', announce: true,
    purpose: 'Blue vs red: meet here. Missile trainer on, guns are real',
    shape: { circle: { lat: 43.2, lon: 39.45, radius_m: 27780 } },
  },
  {
    id: 'ar-1', name: 'AR-1 TEXACO', kind: 'aar', side: 'blue', announce: true,
    purpose: 'KC-135 boom, FL250, TACAN 51Y, 251.0',
    shape: { track: { lat: 42.75, lon: 39.1, heading_deg: 90, leg_m: 55560, width_m: 14816 } },
  },
  {
    id: 'ar-2', name: 'AR-2 ARCO', kind: 'aar', side: 'blue', announce: true,
    purpose: 'KC-135MPRS basket, FL210, TACAN 52Y, 252.0',
    shape: { track: { lat: 42.25, lon: 38.2, heading_deg: 90, leg_m: 46300, width_m: 14816 } },
  },
  {
    id: 'ar-3', name: 'AR-3 SHELL', kind: 'aar', side: 'blue', announce: true,
    purpose: 'KC-130 basket (helos too), 14,000 ft, TACAN 53Y, 253.0',
    shape: { track: { lat: 41.6, lon: 39.95, heading_deg: 90, leg_m: 37040, width_m: 11112 } },
  },
]
