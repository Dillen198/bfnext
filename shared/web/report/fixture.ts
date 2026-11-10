// A week of each campaign, as bfdb's /api/report/weekly answers it: for the
// tests, and for the print page's dev-only `?fixture=redstorm|levant`
// preview (drawing the poster without a bfdb).

import type { SideForces, WeeklyReportResponse } from './weekly'

const side = (o: Partial<SideForces>): SideForces => ({
  objectives: 0, airfields: 0, fobs: 0, farps: 0, fleets: [], naval_bases: 0, sam_sites: 0,
  command_centres: 0, factories: 0, deployed: 0, captured: 0, air_kills: 0, ground_kills: 0, ...o,
})

export const REDSTORM_FIXTURE: WeeklyReportResponse = {
  instance: 'redstorm',
  label: 'Red Storm',
  theme: 'redstorm',
  brand_name: 'Southern Flank 1985',
  dcs_server_name: '[VS] Vector Strike | Cold War 1985 PvP',
  blue_label: 'NATO',
  red_label: 'USSR',
  days: 7,
  since: '2026-11-03T18:00:00Z',
  until: '2026-11-10T18:00:00Z',
  round_number: 1,
  round_name: null,
  scenario: 'Southern Flank',
  round_active: true,
  report: {
    round: 41,
    round_start: '2026-10-30T12:00:00Z',
    since: '2026-11-03T18:00:00Z',
    until: '2026-11-10T18:00:00Z',
    forces: {
      blue: side({ objectives: 34, airfields: 7, fobs: 12, farps: 2, fleets: ['Forrestal group', 'Tarawa group'], sam_sites: 19, deployed: 41, captured: 6, air_kills: 58, ground_kills: 212 }),
      red: side({ objectives: 26, airfields: 12, fobs: 5, fleets: ['Moskva group'], sam_sites: 22, deployed: 37, captured: 2, air_kills: 44, ground_kills: 171 }),
      neutral: 0,
    },
    captures: [
      { time: '2026-11-10T14:12:00Z', objective: 'Kutaisi', side: 'Blue', by: ['Shade', 'Mako'] },
      { time: '2026-11-09T21:40:00Z', objective: 'Senaki', side: 'Blue', by: ['Viper 1-1'] },
      { time: '2026-11-08T17:05:00Z', objective: 'Sukhumi', side: 'Red', by: ['Ivanov'] },
      { time: '2026-11-07T10:30:00Z', objective: 'Zugdidi FOB', side: 'Blue', by: ['Hawk'] },
    ],
    capture_count: 8,
    contested: [{ name: 'Senaki', captures: 3 }],
    story: [
      { id: 'tuapse_burns', time: '2026-11-08T20:00:00Z', title: 'Tuapse refinery burns', text: 'Strike aircraft set the Tuapse refinery alight; Soviet fuel output falls.', caused_by: ['Shade'], side: 'Red', effects: ['Red fuel output -40%'] },
      { id: 'front_collapse', time: '2026-11-09T19:00:00Z', title: 'The Transcaucasus Front breaks', text: '', caused_by: [], side: 'Red', effects: ['Red orders a nuclear strike'] },
      { id: 'last_stand_intercepted', time: '2026-11-09T19:40:00Z', title: 'The Last Stand: the Red strike on Kutaisi is shot down', text: '', caused_by: ['Mako', 'Viper 1-1'], side: 'Red', effects: [] },
    ],
    last_stand: {
      side: 'Red', ordered: '2026-11-09T19:00:00Z', outcome: 'intercepted', time: '2026-11-09T19:40:00Z',
      title: 'The Last Stand: the Red strike on Kutaisi is shot down', by: ['Mako', 'Viper 1-1'], this_week: true,
    },
    will: { blue: 61.2, red: 23.8, blue_trend: 1.5, red_trend: -6, threshold: 20 },
    pilots: [
      { name: 'Shade', side: 'Blue', squadron: 'VF84', air_kills: 9, ground_kills: 31, captures: 3, deploys: 2, score: 500 },
      { name: 'Ivanov', side: 'Red', squadron: 'GSV', air_kills: 12, ground_kills: 4, captures: 2, deploys: 0, score: 220 },
      { name: 'Mako', side: 'Blue', squadron: 'VF84', air_kills: 6, ground_kills: 8, captures: 2, deploys: 1, score: 205 },
      { name: 'Viper 1-1', side: 'Blue', squadron: null, air_kills: 3, ground_kills: 10, captures: 1, deploys: 4, score: 180 },
      { name: 'Tamara', side: 'Red', squadron: 'GSV', air_kills: 2, ground_kills: 9, captures: 0, deploys: 6, score: 140 },
    ],
    squadrons: [
      { tag: 'VF84', name: 'Jolly Rogers', pilots: 5, air_kills: 21, ground_kills: 52, captures: 6, score: 1100 },
      { tag: 'GSV', name: 'Guards Aviation', pilots: 4, air_kills: 15, ground_kills: 20, captures: 2, score: 470 },
    ],
    active_pilots: 46,
  },
}

export const LEVANT_FIXTURE: WeeklyReportResponse = {
  instance: 'vs1',
  label: 'Modern · vs1',
  theme: null,
  brand_name: 'Levant Ablaze',
  dcs_server_name: '[VS] Vector Strike | Modern PvP',
  blue_label: 'NATO',
  red_label: 'RSII',
  days: 7,
  since: '2026-11-03T18:00:00Z',
  until: '2026-11-10T18:00:00Z',
  round_number: 3,
  round_name: 'Levant Ablaze',
  scenario: 'levant',
  round_active: true,
  report: {
    round: 40,
    round_start: '2026-11-01T09:00:00Z',
    since: '2026-11-03T18:00:00Z',
    until: '2026-11-10T18:00:00Z',
    forces: {
      blue: side({ objectives: 61, airfields: 40, fobs: 9, fleets: ['Ike CSG'], sam_sites: 51, command_centres: 2, deployed: 63, captured: 9, air_kills: 74, ground_kills: 390 }),
      red: side({ objectives: 43, airfields: 27, fobs: 6, fleets: [], sam_sites: 58, command_centres: 2, deployed: 48, captured: 5, air_kills: 52, ground_kills: 288 }),
      neutral: 0,
    },
    captures: [
      { time: '2026-11-10T12:00:00Z', objective: 'Taftanaz', side: 'Blue', by: ['Rook'] },
      { time: '2026-11-09T22:15:00Z', objective: 'Minakh', side: 'Blue', by: ['Rook', 'Tango'] },
      { time: '2026-11-09T08:30:00Z', objective: 'Kiryat Shmona', side: 'Red', by: ['Saqr'] },
    ],
    capture_count: 14,
    contested: [{ name: 'Minakh', captures: 4 }],
    story: [
      { id: 'aleppo_falls', time: '2026-11-08T16:00:00Z', title: 'Aleppo falls', text: 'NATO armour is in the factory belt north of the city.', caused_by: ['Rook'], side: 'Red', effects: ['Red will -8'] },
    ],
    last_stand: null,
    will: { blue: 66, red: 41, blue_trend: 2, red_trend: -8, threshold: 20 },
    pilots: [
      { name: 'Rook', side: 'Blue', squadron: '494', air_kills: 4, ground_kills: 48, captures: 5, deploys: 3, score: 685 },
      { name: 'Saqr', side: 'Red', squadron: null, air_kills: 14, ground_kills: 12, captures: 2, deploys: 0, score: 320 },
      { name: 'Tango', side: 'Blue', squadron: '494', air_kills: 8, ground_kills: 9, captures: 2, deploys: 2, score: 240 },
    ],
    squadrons: [{ tag: '494', name: 'Panthers', pilots: 6, air_kills: 17, ground_kills: 70, captures: 9, score: 1310 }],
    active_pilots: 71,
  },
}
