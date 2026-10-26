// Flight plan types. Mirrors `bfdb::flightplans::Plan` -- the server
// validates and normalises every field, so keep the two in step.

export interface PlanWaypoint {
  name: string
  lat: number
  lon: number
  /** Planned altitude, feet MSL: the route card and leg plan. */
  alt_ft: number
  /** Steerpoint elevation, feet MSL: what the jet's nav system is given.
   *  Matters for a target point, where it should be the ground. */
  elev_ft: number
  target: boolean
  note: string
}

export interface PlanChannel {
  /** 1-20 */
  preset: number
  freq_mhz: number
  label: string
  modulation: 'AM' | 'FM'
}

export interface Plan {
  name: string
  /** DCS unit type, e.g. "F-16C_50" -- see AIRCRAFT. */
  aircraft: string
  callsign: string
  /** Planning ground speed for the leg times. */
  speed_kts: number
  /** Mission time at the first waypoint, "HH:MM" or "HH:MM:SS". */
  start_time?: string | null
  waypoints: PlanWaypoint[]
  comm1: PlanChannel[]
  comm2: PlanChannel[]
  /** Write the presets into the native cartridge; off keeps the mission's. */
  include_comms: boolean
  notes: string
  /** Avionics beyond the route, per export target ("native:F-16C_50",
   *  "dcsdtc:F16C"): JSON Pointer -> value, only what differs from that
   *  target's defaults. See model.ts. */
  systems?: Record<string, Record<string, unknown>>
}

/** A plan as `/api/plans` returns it. */
export interface StoredPlan {
  id: string
  side: 'Blue' | 'Red'
  by_name: string
  created: string
  updated: string
  /** The caller may edit or delete it (author or admin). */
  mine: boolean
  plan: Plan
}

export const MAX_WAYPOINTS = 99
export const MAX_CHANNELS = 20

/** DCS types with a native Mission Editor cartridge this planner writes. */
export type NativeType = 'F-16C_50' | 'FA-18C_hornet' | 'AH-64D_BLK_II' | 'F-14BU' | 'MiG-29 Fulcrum'

/** Aircraft with a cartridge format of their own, outside the ME DTC. */
export type OtherCartridge = 'M-2000C' | 'AJS37' | 'JF-17'

export type AircraftGroup = 'Fighters & strike' | 'Attack' | 'Helicopters' | 'Transport' | 'Trainers & light' | 'Warbirds'

export interface AircraftDef {
  /** DCS unit type. */
  id: string
  label: string
  group: AircraftGroup
  /** Has a native DCS cartridge this planner can write. */
  native?: NativeType
  /** Most nav points the native cartridge takes. */
  nativeMax?: number
  /** DCS-DTC's aircraft code, when DCS-DTC supports it. */
  dcsdtc?: string
  other?: OtherCartridge
}

/**
 * Every player-flyable DCS aircraft (2.9.30), so any pilot can plan a route
 * and print the card. Cartridge exports are offered per aircraft: native DCS
 * DTC for the five with a working descriptor, DCS-DTC for its ten, and the
 * M-2000C / Viggen / JF-17 formats for theirs.
 */
export const AIRCRAFT: AircraftDef[] = [
  { id: 'F-16C_50', label: 'F-16C Viper', group: 'Fighters & strike', native: 'F-16C_50', nativeMax: 25, dcsdtc: 'F16C' },
  { id: 'FA-18C_hornet', label: 'F/A-18C Hornet', group: 'Fighters & strike', native: 'FA-18C_hornet', nativeMax: 59, dcsdtc: 'FA18C' },
  { id: 'F-15ESE', label: 'F-15E Strike Eagle', group: 'Fighters & strike', dcsdtc: 'F15E' },
  { id: 'F-14BU', label: 'F-14B(U) Tomcat', group: 'Fighters & strike', native: 'F-14BU', nativeMax: 50, dcsdtc: 'F14BU' },
  { id: 'F-14B', label: 'F-14B Tomcat', group: 'Fighters & strike', dcsdtc: 'F14BU' },
  { id: 'F-14A-135-GR', label: 'F-14A Tomcat', group: 'Fighters & strike', dcsdtc: 'F14BU' },
  { id: 'F-14A-135-GR-Early', label: 'F-14A Tomcat (early)', group: 'Fighters & strike' },
  { id: 'F-14A-95-GR', label: 'F-14A-95 Tomcat', group: 'Fighters & strike' },
  { id: 'MiG-29 Fulcrum', label: 'MiG-29 Fulcrum', group: 'Fighters & strike', native: 'MiG-29 Fulcrum', nativeMax: 20 },
  { id: 'MiG-29A', label: 'MiG-29A (FC)', group: 'Fighters & strike' },
  { id: 'MiG-29S', label: 'MiG-29S (FC)', group: 'Fighters & strike' },
  { id: 'MiG-29G', label: 'MiG-29G (FC)', group: 'Fighters & strike' },
  { id: 'F-15C', label: 'F-15C Eagle (FC)', group: 'Fighters & strike' },
  { id: 'Su-27', label: 'Su-27 Flanker (FC)', group: 'Fighters & strike' },
  { id: 'Su-33', label: 'Su-33 (FC)', group: 'Fighters & strike' },
  { id: 'J-11A', label: 'J-11A (FC)', group: 'Fighters & strike' },
  { id: 'JF-17', label: 'JF-17 Thunder', group: 'Fighters & strike', other: 'JF-17' },
  { id: 'M-2000C', label: 'Mirage 2000C', group: 'Fighters & strike', other: 'M-2000C' },
  { id: 'Mirage-F1CE', label: 'Mirage F1CE', group: 'Fighters & strike' },
  { id: 'Mirage-F1EE', label: 'Mirage F1EE', group: 'Fighters & strike' },
  { id: 'Mirage-F1BE', label: 'Mirage F1BE', group: 'Fighters & strike' },
  { id: 'AJS37', label: 'AJS37 Viggen', group: 'Fighters & strike', other: 'AJS37' },
  { id: 'F-4E-45MC', label: 'F-4E Phantom II', group: 'Fighters & strike' },
  { id: 'F-5E-3', label: 'F-5E Tiger II', group: 'Fighters & strike' },
  { id: 'MiG-21Bis', label: 'MiG-21bis', group: 'Fighters & strike' },
  { id: 'MiG-19P', label: 'MiG-19P', group: 'Fighters & strike' },
  { id: 'MiG-15bis', label: 'MiG-15bis', group: 'Fighters & strike' },
  { id: 'F-86F Sabre', label: 'F-86F Sabre', group: 'Fighters & strike' },
  { id: 'F-100D', label: 'F-100D Super Sabre', group: 'Fighters & strike' },
  { id: 'A-10C_2', label: 'A-10C II', group: 'Attack', dcsdtc: 'A10' },
  { id: 'A-10C', label: 'A-10C', group: 'Attack', dcsdtc: 'A10' },
  { id: 'A-10A', label: 'A-10A (FC)', group: 'Attack' },
  { id: 'AV8BNA', label: 'AV-8B Harrier', group: 'Attack', dcsdtc: 'AV8B' },
  { id: 'Su-25T', label: 'Su-25T (FC)', group: 'Attack' },
  { id: 'Su-25', label: 'Su-25 (FC)', group: 'Attack' },
  { id: 'AH-64D_BLK_II', label: 'AH-64D Apache', group: 'Helicopters', native: 'AH-64D_BLK_II', nativeMax: 50, dcsdtc: 'AH64D' },
  { id: 'CH-47Fbl1', label: 'CH-47F Chinook', group: 'Helicopters', dcsdtc: 'CH47F' },
  { id: 'OH58D', label: 'OH-58D Kiowa Warrior', group: 'Helicopters', dcsdtc: 'OH58D' },
  { id: 'Ka-50_3', label: 'Ka-50 III', group: 'Helicopters' },
  { id: 'Ka-50', label: 'Ka-50', group: 'Helicopters' },
  { id: 'Mi-24P', label: 'Mi-24P Hind', group: 'Helicopters' },
  { id: 'Mi-8MT', label: 'Mi-8MTV2', group: 'Helicopters' },
  { id: 'UH-1H', label: 'UH-1H Huey', group: 'Helicopters' },
  { id: 'SA342M', label: 'SA342M Gazelle', group: 'Helicopters' },
  { id: 'SA342L', label: 'SA342L Gazelle', group: 'Helicopters' },
  { id: 'SA342Mistral', label: 'SA342 Mistral', group: 'Helicopters' },
  { id: 'SA342Minigun', label: 'SA342 Minigun', group: 'Helicopters' },
  { id: 'C-130J-30', label: 'C-130J Super Hercules', group: 'Transport', dcsdtc: 'C130' },
  { id: 'L-39C', label: 'L-39C Albatros', group: 'Trainers & light' },
  { id: 'L-39ZA', label: 'L-39ZA Albatros', group: 'Trainers & light' },
  { id: 'C-101CC', label: 'C-101CC Aviojet', group: 'Trainers & light' },
  { id: 'C-101EB', label: 'C-101EB Aviojet', group: 'Trainers & light' },
  { id: 'MB-339A', label: 'MB-339A', group: 'Trainers & light' },
  { id: 'MB-339APAN', label: 'MB-339A PAN', group: 'Trainers & light' },
  { id: 'Christen Eagle II', label: 'Christen Eagle II', group: 'Trainers & light' },
  { id: 'Yak-52', label: 'Yak-52', group: 'Trainers & light' },
  { id: 'TF-51D', label: 'TF-51D Mustang', group: 'Trainers & light' },
  { id: 'P-51D-30-NA', label: 'P-51D-30 Mustang', group: 'Warbirds' },
  { id: 'P-51D', label: 'P-51D-25 Mustang', group: 'Warbirds' },
  { id: 'P-47D-30', label: 'P-47D Thunderbolt', group: 'Warbirds' },
  { id: 'SpitfireLFMkIX', label: 'Spitfire LF Mk IX', group: 'Warbirds' },
  { id: 'SpitfireLFMkIXCW', label: 'Spitfire LF Mk IX CW', group: 'Warbirds' },
  { id: 'MosquitoFBMkVI', label: 'Mosquito FB Mk VI', group: 'Warbirds' },
  { id: 'F4U-1D', label: 'F4U-1D Corsair', group: 'Warbirds' },
  { id: 'F4U-1D_CW', label: 'F4U-1D Corsair CW', group: 'Warbirds' },
  { id: 'Bf-109K-4', label: 'Bf 109 K-4', group: 'Warbirds' },
  { id: 'FW-190A8', label: 'Fw 190 A-8', group: 'Warbirds' },
  { id: 'FW-190D9', label: 'Fw 190 D-9', group: 'Warbirds' },
  { id: 'I-16', label: 'I-16', group: 'Warbirds' },
  { id: 'La-7', label: 'La-7', group: 'Warbirds' },
]

export function aircraftDef(id: string): AircraftDef {
  return AIRCRAFT.find(a => a.id === id) ?? { id, label: id, group: "Fighters & strike" }
}

export function newPlan(aircraft = 'F-16C_50'): Plan {
  return {
    name: '',
    aircraft,
    callsign: '',
    speed_kts: 420,
    start_time: null,
    waypoints: [],
    comm1: [],
    comm2: [],
    include_comms: false,
    notes: '',
  }
}
