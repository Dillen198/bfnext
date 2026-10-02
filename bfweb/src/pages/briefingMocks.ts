// Fixtures behind `?mock` in dev, so the whole BRIEFING page — including the
// map, the tasking list and the comms card — renders with no engine and no
// netidx. Shapes mirror `bfprotocols::api::Briefing` and
// `bfprotocols::situation::SituationReport`; the Rust-side golden JSON tests
// are what keep the real wire format honest.

import type { Briefing, SituationReport } from '../api'

type Side = 'Blue' | 'Red'

export function mockBriefing(side: Side): Briefing {
  return {
    side, generated: new Date().toISOString(),
    navaids: [
      { objective: 'Incirlik', kind: 'Airbase', deck: null, lat: 37.002, lon: 35.42, tacan: '21X INC', ndb_khz: 350, icls: null, link4_mhz: null, acls: false, brc: null },
      { objective: 'Blue Strike Group', kind: 'Carrier Group', deck: 'CVN74', lat: 35.1, lon: 34.9, tacan: '5Y CVN', ndb_khz: null, icls: 1, link4_mhz: 336, acls: true, brc: 131 },
      { objective: 'Blue Strike Group', kind: 'Carrier Group', deck: 'CV72', lat: 35.1, lon: 34.9, tacan: '6Y CV7', ndb_khz: null, icls: 2, link4_mhz: 337, acls: true, brc: 131 },
      { objective: 'Blue Strike Group', kind: 'Carrier Group', deck: 'LHA-1', lat: 35.1, lon: 34.9, tacan: '7Y LHA', ndb_khz: null, icls: 3, link4_mhz: null, acls: false, brc: 131 },
      { objective: 'Kingsfield Logistics Hub Alpha', kind: 'Logistics Hub', deck: null, lat: 34.98, lon: 33.0, tacan: '2Y KIN', ndb_khz: 375, icls: null, link4_mhz: null, acls: false, brc: null },
      ...Array.from({ length: 10 }, (_, i) => ({
        objective: `Forward Operating Base ${String.fromCharCode(65 + i)}`,
        kind: i % 3 === 0 ? 'FARP' : i % 3 === 1 ? 'FOB' : 'Naval Base',
        deck: null,
        lat: 36.5 - i * 0.12, lon: 34.1 + i * 0.09,
        tacan: i % 3 === 1 ? null : `${3 + i}Y FB${i}`,
        ndb_khz: 200 + i * 5, icls: null, link4_mhz: null, acls: false, brc: null,
      })),
    ],
    radios: [
      { label: 'AWACS Magic', kind: 'AWACS', freq_mhz: 251.0, tacan: '52Y MAG', extra: null },
      { label: 'Tanker Texaco 1-1 heavy drogue basket', kind: 'TANKER', freq_mhz: 274.0, tacan: '38Y TEX', extra: null },
      { label: 'JTAC 1042', kind: 'JTAC', freq_mhz: null, tacan: null, extra: 'laser 1688 near Ahmed al Jaber' },
    ],
    artillery: [
      { group: 'BLUE ARTY 3', typ: 'M109 Paladin 155mm SP Howitzer', lat: 34.5, lon: 33.1, min_range_m: 4000, max_range_m: 22000, alive: 3 },
      { group: 'BLUE MLRS 1', typ: 'M142 HIMARS', lat: 34.6, lon: 33.05, min_range_m: 8000, max_range_m: 70000, alive: 2 },
    ],
    deployables: [
      { name: 'Deployables / SAM / SA-11 Buk Battery', cost: 1200, crates_required: 3, limit: 2, deployed: 1, tags: [] },
      { name: 'Deployables / Radar / Early Warning Radar 55G6', cost: 400, crates_required: 1, limit: 4, deployed: 4, tags: ['EWR'] },
      { name: 'Deployables / JTAC / Ground JTAC Humvee', cost: 200, crates_required: 1, limit: 3, deployed: 0, tags: ['JTAC'] },
    ],
    threats: [
      { typ: 'SA-11 Buk LN 9A310M1', count: 4, band: 'Xband', harm_code: '115', max_range_km: 35 },
      { typ: 'Kub 1S91 str', count: 2, band: 'Cband', harm_code: '108', max_range_km: 24 },
      { typ: 'ZSU-23-4 Shilka', count: 6, band: null, harm_code: '121', max_range_km: 2 },
      { typ: 'p-19 s-125 sr', count: 1, band: null, harm_code: null, max_range_km: null },
    ],
  }
}

export function mockSituation(side: Side): SituationReport {
  const now = Date.now()
  const ago = (m: number) => new Date(now - m * 60_000).toISOString()
  return {
    side,
    generated: new Date().toISOString(),
    mission_time: '14:32',
    headline:
      `${side} is ahead: 11 objectives (4 primary) against 6 (3 primary), 65% of contested ground. ` +
      'A capture timer is running at Gudauta right now — that is the sortie. Air: 3 hostile tracks NE of ' +
      'your territory. 2 of your objectives cannot pay for their own repairs.',
    posture: {
      friendly_objectives: 11, enemy_objectives: 6, neutral_objectives: 1,
      friendly_primary: 4, enemy_primary: 3, territory_pct: 64.7,
      gained_recent: 1, lost_recent: 0, treasury: 2400,
      players_friendly: 7, players_enemy: 5,
      last_stand: undefined,
      victory_condition: 'round ends when one side holds 80% of the map',
    },
    weather: {
      wind_from_deg: 230, wind_kts: 12, temp_c: 19, qnh_inhg: 29.94, qnh_hpa: 1013,
      cloud_base_m: 1800, visibility_m: 20000, precip: false,
      summary: 'VFR — wind 230° at 12kt, 20km vis, cloud base 5906ft',
    },
    tasking: [
      {
        id: 'defend-Gudauta', kind: 'defend', urgency: 'critical', title: 'DEFEND Gudauta',
        detail: 'Red troops are in the zone — 74s of the ~180s they need is already on the clock. eligible — capture troops in the zone, hold position',
        success: 'kill every enemy capture group inside the zone to reset the timer',
        objective: 'Gudauta', lat: 43.10, lon: 40.58, bearing_deg: 312, range_nm: 24, roles: ['CAS', 'Troops'],
      },
      {
        id: 'defend-hub-Kutaisi', kind: 'defend', urgency: 'critical', title: 'DEFEND HUB Kutaisi',
        detail: 'logistics hub feeding 6 of your objectives — supply 22%, fuel 41%, health 68%, enemy in contact',
        success: 'hub secure and stocked — everything downstream of it stops healing without it',
        objective: 'Kutaisi', lat: 42.18, lon: 42.48, bearing_deg: 96, range_nm: 61, roles: ['CAS', 'Heavy lift'],
      },
      {
        id: 'sead-Sukhumi', kind: 'sead', urgency: 'high', title: 'SEAD near Sukhumi',
        detail: '4x air defence held on recon intel, 82% confidence, 19nm engagement ring, ±0.8km position error. Everything inside that ring is in a launch basket.',
        success: 'radar off the air or the launchers dead, then the strike package pushes',
        objective: 'Sukhumi', lat: 42.86, lon: 41.12, bearing_deg: 288, range_nm: 38, roles: ['SEAD', 'HARM'],
      },
      {
        id: 'capture-Senaki', kind: 'capture', urgency: 'high', title: 'CAPTURE Senaki-Kolkhi',
        detail: 'eligible — move your capture troops into the zone. repairs FROZEN while enemy units stay in sight',
        success: 'land capture troops inside the zone and hold them there',
        objective: 'Senaki-Kolkhi', lat: 42.24, lon: 42.05, bearing_deg: 104, range_nm: 47,
        roles: ['Troop lift', 'CAS escort'],
      },
      {
        id: 'intercept-nearest', kind: 'intercept', urgency: 'high', title: 'INTERCEPT inbound near Kobuleti',
        detail: 'fighter track 31nm off Kobuleti on a bearing of 041°, 24000 ft, 460 kt, heading 218°. 3 hostile track(s) on the net.',
        success: 'push the raid off your territory or kill it',
        objective: 'Kobuleti', lat: 42.09, lon: 41.93, bearing_deg: 41, range_nm: 31, roles: ['CAP', 'Intercept'],
      },
      {
        id: 'logistics-Batumi', kind: 'logistics', urgency: 'routine', title: 'RESUPPLY Batumi',
        detail: 'repair stalled — supply 6% under the 10% each pulse costs Health 82%, fuel 55%.',
        success: 'supply back over the repair cost so the garrison can heal itself',
        objective: 'Batumi', lat: 41.61, lon: 41.60, bearing_deg: 168, range_nm: 72,
        roles: ['Heavy lift', 'Convoy escort'],
      },
      {
        id: 'strike-Bombora', kind: 'strike', urgency: 'routine', title: 'STRIKE Bombora',
        detail: 'already softened to 34% — 14 more and it is takeable. self-repairing — next pulse in ~7m',
        success: 'health at or under 20% and no infantry left, then move troops in',
        objective: 'Bombora', lat: 43.14, lon: 40.36, bearing_deg: 301, range_nm: 44, roles: ['Strike', 'CAS'],
      },
    ],
    hotspots: [
      {
        objective: 'Gudauta', kind: 'Airbase', owner: side, lat: 43.10, lon: 40.58,
        health: 12, logi: 40, supply: 55, threatened: true, capture_progress: ['Red', 74, 180],
        captureable: true, in_capture_hold: false,
        status: 'being taken by Red — held 74s (~180s needed)',
        repair_outlook: 'repairs FROZEN while the capture timer runs', risk: 'critical',
      },
      {
        objective: 'Senaki-Kolkhi', kind: 'Airbase', owner: side === 'Blue' ? 'Red' : 'Blue',
        lat: 42.24, lon: 42.05, health: 18, logi: 20, supply: 30, threatened: true,
        captureable: true, in_capture_hold: false,
        status: 'eligible — move your capture troops into the zone',
        repair_outlook: 'repairs FROZEN while enemy units stay in sight', risk: 'high',
      },
      {
        objective: 'Bombora', kind: 'FARP', owner: side === 'Blue' ? 'Red' : 'Blue',
        lat: 43.14, lon: 40.36, health: 34, logi: 70, supply: 62, threatened: false,
        captureable: false, in_capture_hold: false,
        status: 'not eligible — need health <=20% (now 34%)',
        repair_outlook: 'self-repairing — next pulse in ~7m', risk: 'routine',
      },
    ],
    threats: [
      {
        label: '4x air defence', lat: 42.86, lon: 41.12, radius_m: 35000, uncertainty_m: 800,
        confidence: 0.82, source: 'recon', age_s: 140, count: 4, near: 'Sukhumi',
      },
      {
        label: 'air-defence emitter', lat: 42.30, lon: 41.80, radius_m: 24000, uncertainty_m: 3200,
        confidence: 0.44, source: 'ewr', age_s: 610, count: 2, near: 'Senaki-Kolkhi',
      },
    ],
    air: {
      hostile_tracks: 3, friendly_airborne: 6, stale_tracks: 1,
      axis: '3 hostile tracks NE of your territory',
      nearest: {
        lat: 42.45, lon: 42.30, alt_ft: 24000, heading: 218, speed_kts: 460,
        class: 'fighter', near: 'Kobuleti', bearing_deg: 41, range_nm: 31,
      },
      radar_blind: false,
    },
    logistics: {
      hubs: [
        { objective: 'Kutaisi', lat: 42.18, lon: 42.48, supply: 22, fuel: 41, health: 68, logi: 80, feeding: 6, threatened: true },
        { objective: 'Kobuleti', lat: 42.09, lon: 41.93, supply: 78, fuel: 91, health: 100, logi: 100, feeding: 5, threatened: false },
      ],
      gaps: [
        { objective: 'Batumi', lat: 41.61, lon: 41.60, supply: 4, fuel: 55, health: 82, note: 'repair stalled — supply 4% under the 10% each pulse costs' },
        { objective: 'Gudauta', lat: 43.10, lon: 40.58, supply: 8, fuel: 12, health: 12, note: 'repair stalled — supply 8% under the 10% each pulse costs' },
      ],
      convoys_active: 3,
      stage: 'ManageConvoys',
    },
    support: [
      { label: 'AWACS Magic', kind: 'AWACS', freq_mhz: 251.0, tacan: '52Y MAG', lat: 42.6, lon: 41.4 },
      { label: 'TANKER Texaco', kind: 'TANKER', freq_mhz: 253.0, tacan: '38Y TEX', lat: 42.0, lon: 41.2 },
      { label: 'JTAC 1042', kind: 'JTAC', note: 'laser 1688 near Senaki-Kolkhi' },
    ],
    comms: [
      { preset: 1, label: 'AWACS / GCI -- MAGIC', freq_mhz: 251.0, modulation: 'AM', purpose: 'Primary control: picture, bogey dope, commit, declare', live: true, note: 'AWACS Magic -- TACAN 52Y MAG' },
      { preset: 2, label: 'AWACS alternate -- DARKSTAR', freq_mhz: 252.0, modulation: 'AM', purpose: 'Second controller / overflow when MAGIC is saturated', live: false },
      { preset: 3, label: 'Tanker TEXACO (boom)', freq_mhz: 253.0, modulation: 'AM', purpose: 'Pre-strike and post-strike refuel, fixed-wing boom', live: true, note: 'TANKER Texaco -- TACAN 38Y TEX' },
      { preset: 4, label: 'Tanker ARCO (boom)', freq_mhz: 254.0, modulation: 'AM', purpose: 'Second boom track', live: false },
      { preset: 5, label: 'Tanker SHELL (drogue)', freq_mhz: 255.0, modulation: 'AM', purpose: 'Probe-and-drogue: Hornet, Tomcat, Harrier, Viggen', live: false },
      { preset: 6, label: 'JTAC 1', freq_mhz: 256.0, modulation: 'AM', purpose: 'Nine-line, talk-on, laser for the first active JTAC', live: true, note: 'JTAC 1042 -- laser 1688 near Senaki-Kolkhi' },
      { preset: 7, label: 'JTAC 2', freq_mhz: 257.0, modulation: 'AM', purpose: 'Second active JTAC', live: false },
      { preset: 10, label: 'CSAR -- SANDY', freq_mhz: 260.0, modulation: 'AM', purpose: 'Downed-pilot pickup: on-scene commander and the helo', live: false },
      { preset: 11, label: 'Package common -- STRIKE', freq_mhz: 265.0, modulation: 'AM', purpose: 'Strike package internal, all flights', live: false },
      { preset: 12, label: 'Package common -- SEAD', freq_mhz: 266.0, modulation: 'AM', purpose: 'SEAD/DEAD package internal', live: false },
      { preset: 15, label: 'Carrier MARSHAL', freq_mhz: 270.0, modulation: 'AM', purpose: 'Carrier approach control, marshal stack, case II/III', live: false },
      { preset: 16, label: 'Carrier TOWER / LSO', freq_mhz: 127.5, modulation: 'AM', purpose: 'Ball call, paddles, deck ops', live: false },
      { preset: 17, label: 'GUARD (UHF)', freq_mhz: 243.0, modulation: 'AM', purpose: 'Emergency only -- never used for traffic', live: false },
      { preset: 19, label: 'Ground / logistics net', freq_mhz: 30.0, modulation: 'FM', purpose: 'Convoy, crate and warehouse coordination', live: false },
    ],
    flight_channels: [
      ['FLIGHT 1', 305.0], ['FLIGHT 2', 306.0], ['FLIGHT 3', 307.0], ['FLIGHT 4', 308.0],
    ],
    recent: [
      { at: ago(6), text: 'Gudauta (Blue) under attack — health 12%', good: false, lat: 43.10, lon: 40.58 },
      { at: ago(19), text: 'Bombora captured by Blue by Viper 1-1, Hog 2-2', good: true },
      { at: ago(41), text: 'Senaki-Kolkhi (Red) damaged — health 18%', good: true, lat: 42.24, lon: 42.05 },
    ],
    map: [
      { name: 'Gudauta', kind: 'Airbase', owner: 'Blue', lat: 43.10, lon: 40.58, health: 12, logi: 40, supply: 8, fuel: 12, threatened: true, captureable: true, priority: true, primary: true },
      { name: 'Sukhumi', kind: 'Airbase', owner: 'Red', lat: 42.86, lon: 41.12, health: 100, logi: 100, threatened: false, captureable: false, priority: false, primary: true },
      { name: 'Senaki-Kolkhi', kind: 'Airbase', owner: 'Red', lat: 42.24, lon: 42.05, health: 18, logi: 20, threatened: true, captureable: true, priority: false, primary: true },
      { name: 'Kutaisi', kind: 'Airbase', owner: 'Blue', lat: 42.18, lon: 42.48, health: 68, logi: 80, supply: 22, fuel: 41, threatened: true, captureable: false, priority: false, primary: true },
      { name: 'Kobuleti', kind: 'Airbase', owner: 'Blue', lat: 42.09, lon: 41.93, health: 100, logi: 100, supply: 78, fuel: 91, threatened: false, captureable: false, priority: false, primary: true },
      { name: 'Batumi', kind: 'Airbase', owner: 'Blue', lat: 41.61, lon: 41.60, health: 82, logi: 90, supply: 4, fuel: 55, threatened: false, captureable: false, priority: false, primary: true },
      { name: 'Bombora', kind: 'FARP', owner: 'Red', lat: 43.14, lon: 40.36, health: 34, logi: 70, threatened: false, captureable: false, priority: false, primary: true },
      { name: 'Tkvarcheli Depot', kind: 'Logistics Hub', owner: 'Red', lat: 42.90, lon: 41.68, health: 100, logi: 100, threatened: false, captureable: false, priority: false, primary: false },
      { name: 'Zugdidi FOB', kind: 'FOB', owner: 'Neutral', lat: 42.51, lon: 41.87, health: 0, logi: 0, threatened: false, captureable: true, priority: false, primary: false },
      { name: 'Lentekhi Outpost', kind: 'FOB', owner: 'Blue', lat: 42.79, lon: 42.72, health: 95, logi: 100, supply: 61, fuel: 70, threatened: false, captureable: false, priority: false, primary: false },
    ],
  }
}
