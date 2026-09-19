// ─────────────────────────────────────────────────────────────────────────────
// Vector Strike — Server Profiles
// Each server the campaign runs on can have its own era, life-role config, and
// aircraft roster. Add a new entry here when a new server goes live — the
// Pilot Field Manual's Lives and Aircraft Roster sections switch to it via the
// server selector automatically once there's more than one entry.
// ─────────────────────────────────────────────────────────────────────────────

export type LifeRole = 'Standard' | 'Intercept' | 'Attack' | 'Recon' | 'Logistics'

export interface RoleLifeConfig {
  role: LifeRole
  lives: number
  resetHrs: number
  blurb: string
}

export interface AircraftGroup {
  role: LifeRole
  lives: number
  aircraft: string[]
}

export interface ServerProfile {
  id: string
  label: string
  era: string
  lifeRoles: RoleLifeConfig[]
  roster: {
    blue: AircraftGroup[]
    red: AircraftGroup[]
  }
  sharedAircraft: string
}

export const SERVERS: ServerProfile[] = [
  {
    id: 'vector-golan',
    label: 'The Coop · Operation Vector Strike',
    era: 'Modern',
    lifeRoles: [
      { role: 'Standard', lives: 3, resetHrs: 6, blurb: 'Front-line multirole fighters — the core combat pool.' },
      { role: 'Intercept', lives: 4, resetHrs: 6, blurb: 'Dedicated air-to-air fighters for defending your airspace.' },
      { role: 'Attack', lives: 4, resetHrs: 6, blurb: 'CAS jets and attack helicopters built for hitting ground targets.' },
      { role: 'Recon', lives: 6, resetHrs: 6, blurb: 'Light and reconnaissance airframes — lowest risk, most lives.' },
      { role: 'Logistics', lives: 6, resetHrs: 6, blurb: 'Transports and utility helicopters that move cargo and troops.' },
    ],
    roster: {
      blue: [
        { role: 'Standard', lives: 3, aircraft: ['F-14A Tomcat', 'F-14B Tomcat', 'F-15C Eagle', 'F-15E Strike Eagle', 'F-16C Viper (Block 50)', 'F/A-18C Hornet'] },
        { role: 'Intercept', lives: 4, aircraft: ['F-4E Phantom II', 'F-100', 'F-5E Tiger II', 'Mirage F1EE', 'Mirage 2000C'] },
        { role: 'Attack', lives: 4, aircraft: ['A-10A Warthog', 'A-10C II Warthog', 'AH-64D Apache', 'AV-8B N/A Harrier II', 'OH-58D Kiowa Warrior'] },
        { role: 'Recon', lives: 6, aircraft: ['MB-339A', 'P-47D Thunderbolt', 'P-51D Mustang'] },
        { role: 'Logistics', lives: 6, aircraft: ['C-130J-30 Super Hercules', 'CH-47F Chinook', 'UH-1H Huey'] },
      ],
      red: [
        { role: 'Standard', lives: 3, aircraft: ['F-14A Tomcat (Early)', 'F-14B(U) Tomcat', 'F-16C Viper (Block 50)', 'J-11A Flanker', 'JF-17 Thunder', 'Su-27 Flanker', 'Su-33 Flanker-D'] },
        { role: 'Intercept', lives: 4, aircraft: ['F-4E Phantom II', 'MiG-21bis', 'MiG-29A Fulcrum', 'MiG-29S Fulcrum', 'Mirage F1BE', 'Mirage F1CE', 'Mirage 2000C'] },
        { role: 'Attack', lives: 4, aircraft: ['AJS37 Viggen', 'Ka-50 Black Shark', 'Ka-50-3 Black Shark', 'Mi-24P Hind', 'SA342 Gazelle (Minigun)', 'Su-25', 'Su-25T Frogfoot'] },
        { role: 'Recon', lives: 6, aircraft: ['FW-190D9 Dora', 'L-39C Albatros', 'Spitfire LF Mk.IX'] },
        { role: 'Logistics', lives: 6, aircraft: ['C-130J-30 Super Hercules', 'CH-47F Chinook', 'Mi-8MT Hip', 'SA342 Gazelle (Mistral)', 'SA342L Gazelle', 'SA342M Gazelle'] },
      ],
    },
    sharedAircraft: 'the F-14 Tomcat family, F-16C (Block 50), F-4E Phantom II, F-5E, the C-130J-30, the CH-47F, and the UH-1H',
  },
]

export const DEFAULT_SERVER_ID = SERVERS[0].id
