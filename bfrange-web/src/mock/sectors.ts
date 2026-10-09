/**
 * The range's 45 sectors, copied from bfrange/RANGE_CFG.sample.json (`sectors`)
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
  {
    id: 's-1', name: 'S-1 AKHALKALAKI', kind: 'threat', side: 'blue', announce: true,
    purpose: 'LIVE SEAD/IADS: EWR, SA-2, SA-3, SA-6, SA-11, SA-10, Pantsir. Weapons free, networked, trainer protects you',
    shape: { polygon: [ll(41.66129, 43.07785), ll(41.59628, 43.78623), ll(41.19807, 43.71981), ll(41.26218, 43.0156)] },
  },
  {
    id: 'r-4', name: 'R-4 GAREJI', kind: 'air_to_ground', side: 'blue', announce: true,
    purpose: 'Tiered targets: EASY trucks, MEDIUM armour under AAA, HARD armour under SHORAD',
    shape: { polygon: [ll(41.54973, 45.24893), ll(41.53059, 45.42461), ll(41.46022, 45.41095), ll(41.47932, 45.23546)] },
  },
  {
    id: 'ew-1', name: 'EW-1 TSALKA', kind: 'ew', side: 'blue', announce: true,
    purpose: 'GPS jamming: jammer on the Trialeti slope, coordinate targets on the Tsalka plateau',
    shape: { polygon: [ll(41.76783, 44.04357), ll(41.7446, 44.27959), ll(41.61211, 44.25617), ll(41.63522, 44.02061)] },
  },
  {
    id: 'hz-1', name: 'HZ-1 LIAKHVI', kind: 'hot_zone', side: 'blue', announce: true,
    purpose: 'HOT ZONE: red CAP, SHORAD and targets that fight back. AWACS Overlord 1 on 251.5',
    shape: { circle: { lat: 42.38, lon: 44.1, radius_m: 37040 } },
  },
  {
    id: 'll-1', name: 'LL-1 KOLKHETI', kind: 'low_level', side: 'blue', announce: true,
    purpose: 'Low-level route, 8 gates, 69 nm: below 500 ft AGL, 420 kts',
    shape: { polygon: [ll(42.07259, 42.42621), ll(42.13321, 42.20315), ll(42.13132, 41.93969), ll(42.28186, 41.8479), ll(42.41236, 41.87489), ll(42.48729, 42.0558), ll(42.50689, 42.24615), ll(42.43866, 42.39033), ll(42.46134, 42.40967), ll(42.53311, 42.25386), ll(42.51271, 42.0442), ll(42.42764, 41.8451), ll(42.27814, 41.8121), ll(42.10868, 41.92032), ll(42.10679, 42.19685), ll(42.04741, 42.41379)] },
  },
  {
    id: 'cs-1', name: 'CS-1 BAKHMARO', kind: 'csar', side: 'blue', announce: true,
    purpose: 'CSAR: forested Meskheti ridges. Beacon 350 kHz',
    shape: { circle: { lat: 41.83, lon: 42.4, radius_m: 15000 } },
  },
  {
    id: 'pt-1', name: 'PT-1 SENAKI', kind: 'pattern', side: 'blue', announce: true,
    purpose: 'Circuits and landings, RWY 09/27; every landing graded',
    shape: { circle: { lat: 42.24085, lon: 42.04802, radius_m: 9260 } },
  },
  {
    id: 'fd-1', name: 'FD-1 KOBULETI', kind: 'ship_deck', side: 'blue', announce: true,
    purpose: 'Frigate deck landings: FFG-7 Perry and Arleigh Burke under way',
    shape: { circle: { lat: 42, lon: 41.52, radius_m: 14816 } },
  },
  {
    id: 'as-2', name: 'AS-2 SUKHUMI', kind: 'anti_ship', side: 'all', announce: true,
    purpose: 'Escorted convoy, red: Krivak, Neustrashimy and a Tor-armed corvette guarding merchants. Weapons free',
    shape: { polygon: [ll(42.92234, 40.25759), ll(42.87355, 40.98599), ll(42.51681, 40.94011), ll(42.56499, 40.21581)] },
  },
  {
    id: 's-11', name: 'S-11 KURSAVKA', kind: 'threat', side: 'red', announce: true,
    purpose: 'LIVE SEAD/IADS: EWR, Hawk, Patriot, NASAMS, IRIS-T SLM, Roland, Gepard. Weapons free, networked',
    shape: { polygon: [ll(44.7806, 42.13883), ll(44.71775, 42.88636), ll(44.31861, 42.81843), ll(44.38059, 42.07585)] },
  },
  {
    id: 'r-15', name: 'R-15 EDISSEYA', kind: 'air_to_ground', side: 'red', announce: true,
    purpose: 'Tiered targets: EASY trucks, MEDIUM armour under AAA, HARD armour under SHORAD',
    shape: { polygon: [ll(44.01264, 44.45626), ll(43.99386, 44.63956), ll(43.92331, 44.62561), ll(43.94205, 44.44251)] },
  },
  {
    id: 'ew-11', name: 'EW-11 TEREK', kind: 'ew', side: 'red', announce: true,
    purpose: 'GPS jamming: jammer on the Terek ridge, coordinate targets on the plain north of it',
    shape: { polygon: [ll(43.65683, 44.78186), ll(43.6312, 45.02449), ll(43.4991, 44.99785), ll(43.52461, 44.75572)] },
  },
  {
    id: 'hz-11', name: 'HZ-11 ZELENCHUK', kind: 'hot_zone', side: 'red', announce: true,
    purpose: 'HOT ZONE: blue CAP, SHORAD and targets that fight back. AWACS Focus 1 on 124.5',
    shape: { circle: { lat: 43.95, lon: 41.75, radius_m: 37040 } },
  },
  {
    id: 'll-11', name: 'LL-11 PSEKUPS', kind: 'low_level', side: 'red', announce: true,
    purpose: 'Low-level route, 8 gates, 78 nm Krymsk to Maykop: below 500 ft AGL, 420 kts',
    shape: { polygon: [ll(44.84706, 38.0747), ll(44.7968, 38.31623), ll(44.77687, 38.55574), ll(44.7168, 38.79624), ll(44.70686, 39.01584), ll(44.62752, 39.29293), ll(44.5566, 39.53821), ll(44.5968, 39.82366), ll(44.6232, 39.81634), ll(44.5834, 39.54179), ll(44.65248, 39.30707), ll(44.73313, 39.02416), ll(44.7432, 38.80377), ll(44.80313, 38.56426), ll(44.8232, 38.32377), ll(44.87294, 38.0853)] },
  },
  {
    id: 'cs-11', name: 'CS-11 CHEGEM', kind: 'csar', side: 'red', announce: true,
    purpose: 'CSAR: forested ridges between the Baksan and Chegem gorges. Beacon 400 kHz',
    shape: { circle: { lat: 43.44, lon: 43.34, radius_m: 13000 } },
  },
  {
    id: 'pt-11', name: 'PT-11 MINERALNYE VODY', kind: 'pattern', side: 'red', announce: true,
    purpose: 'Circuits and landings, RWY 12/30; every landing graded',
    shape: { circle: { lat: 44.22785, lon: 43.08119, radius_m: 9260 } },
  },
  {
    id: 'fd-11', name: 'FD-11 UTRISH', kind: 'ship_deck', side: 'red', announce: true,
    purpose: 'Frigate deck landings: Neustrashimy and Project 22160 under way',
    shape: { circle: { lat: 44.52, lon: 37.43, radius_m: 14816 } },
  },
  {
    id: 'as-3', name: 'AS-3 TAMAN', kind: 'anti_ship', side: 'all', announce: true,
    purpose: 'Escorted convoy, NATO: Arleigh Burke and Perry guarding merchants. Weapons free',
    shape: { polygon: [ll(44.76167, 36.4335), ll(44.73639, 37.18979), ll(44.37724, 37.16406), ll(44.40221, 36.41239)] },
  },
]
