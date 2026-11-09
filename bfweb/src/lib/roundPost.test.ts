import { describe, expect, it } from 'vitest'
import type { RoundReport, RoundSummary } from '../api'
import { roundPost } from './roundPost'

const round = {
  id: 124000162, number: 12, scenario: 'Southern Flank',
  start: '2026-11-01T18:00:00Z', end: '2026-11-05T22:00:00Z', active: false, winner: 'Blue',
}

const summary: RoundSummary = {
  id: round.id, pilots: 47, air_kills: 312, ground_kills: 1204, captures: 88, supply_runs: 140, hours: 612.4,
  leaders: { air: { name: 'Viper', value: 23 }, ground: null, captures: { name: 'Mud', value: 12 }, logistics: { name: 'Hauler', value: 55 } },
  top: [{ ucid: 'u1', name: 'Mud', air_kills: 2, ground_kills: 140, captures: 12, logistics: 0, deaths: 4, hours: 30 }],
}

const report: RoundReport = {
  start: round.start, end: round.end, winner: 'Blue', bucket_secs: 86400, objectives: 40,
  buckets: [
    { t: round.start, blue_held: 20, red_held: 20, neutral_held: 0, blue_air: 10, blue_ground: 100, red_air: 8, red_ground: 50 } as RoundReport['buckets'][number],
  ],
  contested: [{ name: 'Kobuleti', captures: 7 }, { name: 'Senaki', captures: 5 }],
}

const post = (over: Partial<Parameters<typeof roundPost>[0]> = {}) =>
  roundPost({ round, summary, report, blue: 'BLUFOR', red: 'REDFOR', url: 'https://x/rounds?round=1', ...over })

describe('roundPost', () => {
  it('names the winner, the numbers and the people', () => {
    const t = post()
    expect(t).toMatch(/BLUFOR/)
    expect(t).toMatch(/Round 12/)
    expect(t).toMatch(/1,204/)
    expect(t).toMatch(/\*\*Mud\*\*/)
    expect(t).toMatch(/Viper/)
    expect(t).toMatch(/Kobuleti/)
    expect(t.trim().endsWith('https://x/rounds?round=1')).toBe(true)
  })

  it('reads like a person wrote it', () => {
    for (let id = 1; id < 200; id++) {
      const t = post({ round: { ...round, id } })
      expect(t).not.toMatch(/[—–]/) // no em/en dashes
      expect(t).not.toMatch(/delve|dive in|testament|tapestry|thrilling|epic|unleash|battle-hardened/i)
      expect(t).not.toMatch(/\p{Extended_Pictographic}/u)
    }
  })

  it('names each pilot once', () => {
    const t = post({
      summary: {
        ...summary,
        leaders: { air: { name: 'Mud', value: 2 }, ground: { name: 'Mud', value: 140 }, captures: { name: 'Hauler', value: 12 }, logistics: { name: 'Hauler', value: 55 } },
      },
    })
    expect(t.match(/Mud/g)).toHaveLength(1)
    expect(t.match(/Hauler/g)).toHaveLength(1)
    expect(t).toMatch(/MVP: \*\*Mud\*\*, top ace and most ground kills/)
    expect(t).toMatch(/\*\*Hauler\*\*: 12 captures and 55 logistics jobs/)
  })

  it('is the same text every time for a round', () => {
    expect(post()).toBe(post())
  })

  it('handles a live round and missing data', () => {
    const t = roundPost({ round: { ...round, active: true, end: null, winner: null }, blue: 'BLUFOR', red: 'REDFOR', now: Date.parse('2026-11-02T18:00:00Z') })
    expect(t).toMatch(/Round 12/)
    expect(t).not.toMatch(/won/)
    expect(t).not.toMatch(/undefined|NaN|null/)
  })

  it('skips the leader whose count is zero and the dull contested line', () => {
    const t = post({ report: { ...report, contested: [{ name: 'Batumi', captures: 1 }] } })
    expect(t).not.toMatch(/Batumi/)
    expect(t).not.toMatch(/most ground kills/)
  })
})
