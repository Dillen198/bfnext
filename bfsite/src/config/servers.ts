// ─────────────────────────────────────────────────────────────────────────────
// Vector Strike — Server Profiles
// Each server the campaign runs on can have its own era, life-role config, and
// aircraft roster. Add a new entry here when a new server goes live — the
// Pilot Field Manual's Lives and Aircraft Roster sections switch to it via the
// server selector automatically once there's more than one entry.
//
// THESE ARE DERIVED FROM THE ENGINE CONFIGS, not written by hand: `lifeRoles`
// from `default_lives`, `livesEnforced` from `limited_lives`, and each roster
// from that campaign's BINVENTORY / RINVENTORY warehouse grouped by
// `life_types`. Airframes that differ only by DCS variant are collapsed into
// one readable name. Re-derive when a campaign's roster changes rather than
// editing entries here to match what you think is deployed.
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
  /**
   * The bfdb instance this profile describes (`?instance=`). The live
   * stats picker is driven by `/api/instances`, so keeping the ids and
   * labels in step is what stops one page calling the same server two
   * different things.
   */
  instanceId: string
  label: string
  era: string
  /** One line on what this server actually is, shown under the selector. */
  summary: string
  /**
   * The engine's `limited_lives`. When false, the pools below are the
   * campaign's design values but nothing is enforced — you can fly as often as
   * you like. Worth stating plainly, because the two servers differ on it and
   * a lives table with no note reads as a promise.
   */
  livesEnforced: boolean
  lifeRoles: RoleLifeConfig[]
  roster: {
    blue: AircraftGroup[]
    red: AircraftGroup[]
  }
  /** Coalition names for this campaign — they are not BLUFOR/REDFOR everywhere. */
  blueLabel: string
  redLabel: string
  sharedAircraft: string
}

export const SERVERS: ServerProfile[] = [
  {
    id: 'vector-golan',
    instanceId: 'vs1',
    label: 'Modern · Syria',
    era: 'Modern day',
    summary:
      'The long-running modern-era campaign — coalition airpower against a ' +
      'peer air force, fought with current-generation jets.',
    // ODFv2_CFG has limited_lives = false: design values, not enforced.
    livesEnforced: false,
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
    blueLabel: 'BLUFOR',
    redLabel: 'REDFOR',
    sharedAircraft: 'the F-14 Tomcat family, F-16C (Block 50), F-4E Phantom II, F-5E, the C-130J-30, the CH-47F, and the UH-1H',
  },
  {
    id: 'rgw2008-caucasus',
    instanceId: 'vs2',
    label: '2008 · Caucasus',
    era: 'Russo-Georgian War, August 2008',
    summary:
      'August 2008, fought over the ground it actually happened on — 56 ' +
      'objectives across the Caucasus, 44 hidden SAM sites, and a points ' +
      'economy that charges you for the airframe and the loadout.',
    // RGW2008_CFG has limited_lives = true: on this server lives bite.
    livesEnforced: true,
    // Nothing in this campaign's life_types maps to Standard, so that pool is
    // never drawn from. Listing a role with no aircraft behind it would be
    // worse than leaving it out.
    lifeRoles: [
      { role: 'Intercept', lives: 3, resetHrs: 6, blurb: 'Every fast jet on this campaign — the fewest lives, so the fighter war is the one that costs you.' },
      { role: 'Attack', lives: 5, resetHrs: 6, blurb: 'CAS jets and attack helicopters working the front line.' },
      { role: 'Recon', lives: 8, resetHrs: 6, blurb: 'Light trainers and recon types — cheap to lose, and how you find the hidden SAMs.' },
      { role: 'Logistics', lives: 10, resetHrs: 6, blurb: 'Transports and utility helicopters. The campaign is won here, so the pool is deep.' },
    ],
    roster: {
      blue: [
        { role: 'Intercept', lives: 3, aircraft: ['F-15C Eagle', 'F-15E Strike Eagle', 'F-16C Viper (Block 50)', 'F/A-18C Hornet', 'F-14A Tomcat (Early)', 'F-14B Tomcat', 'F-14B(U) Tomcat', 'F-5E Tiger II', 'F-100 Super Sabre', 'MiG-21bis', 'MiG-29A Fulcrum', 'MiG-29G Fulcrum', 'Mirage F1BE', 'Mirage F1CE', 'Mirage F1EE', 'F4U-1D Corsair', 'La-7', 'P-51D Mustang', 'Spitfire LF Mk.IX'] },
        { role: 'Attack', lives: 5, aircraft: ['A-10A Warthog', 'A-10C Warthog', 'A-10C II Warthog', 'AV-8B N/A Harrier II', 'Su-25 Frogfoot', 'AH-64D Apache', 'Mi-24P Hind', 'OH-58D Kiowa Warrior'] },
        { role: 'Recon', lives: 8, aircraft: ['C-101CC Aviojet', 'L-39C Albatros', 'MB-339A'] },
        { role: 'Logistics', lives: 10, aircraft: ['C-130J-30 Super Hercules', 'CH-47F Chinook', 'Mi-8MT Hip', 'UH-1H Huey'] },
      ],
      red: [
        { role: 'Intercept', lives: 3, aircraft: ['Su-27 Flanker', 'Su-33 Flanker-D', 'MiG-29A Fulcrum', 'MiG-29S Fulcrum', 'MiG-21bis', 'MiG-19P Farmer', 'MiG-15bis', 'J-11A Flanker', 'JF-17 Thunder', 'F-14A Tomcat', 'F-16C Viper (Block 50)', 'F-4E Phantom II', 'Mirage 2000C', 'FW-190 A-8', 'FW-190D9 Dora', 'La-7'] },
        { role: 'Attack', lives: 5, aircraft: ['Su-25 Frogfoot', 'Su-25T Frogfoot', 'AJS37 Viggen', 'Ka-50 Black Shark', 'Ka-50-3 Black Shark', 'Mi-24P Hind', 'SA342M Gazelle', 'SA342 Gazelle (Minigun)'] },
        { role: 'Recon', lives: 8, aircraft: ['L-39C Albatros', 'Yak-52'] },
        { role: 'Logistics', lives: 10, aircraft: ['C-130J-30 Super Hercules', 'CH-47F Chinook', 'Mi-8MT Hip', 'SA342L Gazelle'] },
      ],
    },
    blueLabel: 'GEORGIA',
    redLabel: 'RUSSIA',
    sharedAircraft: 'the F-16C (Block 50), MiG-21bis, MiG-29A, Su-25, Mi-24P, L-39C, La-7, the C-130J-30, the CH-47F and the Mi-8MT',
  },
]

export const DEFAULT_SERVER_ID = SERVERS[0].id
