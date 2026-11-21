// Tests for shared/web/report: the weekly report poster's data assembly and
// its Discord post (shared/web has no test runner of its own; see its README).
import { describe, expect, it } from 'vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import {
  DISCORD_MAX, draftDispatch, paragraphs, posterFilename, posterModel, postText, reportPostBody, weekRange,
  type WeeklyReportResponse,
} from '@shared/web/report/weekly'
import { LEVANT_FIXTURE, REDSTORM_FIXTURE } from '@shared/web/report/fixture'
import { WeeklyPoster } from '@shared/web/report/WeeklyPoster'

const FALLBACK = { blue: 'BLUFOR', red: 'REDFOR', brand: 'Vector Strike' }

describe('posterModel', () => {
  it('builds the Red Storm board, the day of the war and the week', () => {
    const m = posterModel(REDSTORM_FIXTURE, FALLBACK)
    expect(m.theme).toBe('redstorm')
    expect(m.brand).toBe('Southern Flank 1985')
    expect([m.blue, m.red]).toEqual(['NATO', 'USSR'])
    // round from 30 Oct 12:00Z; the window ends 10 Nov 18:00Z
    expect(m.day).toBe(12)
    expect(m.week).toBe('3–10 NOV 2026')
    expect(m.board.map(r => [r.label, r.blue, r.red])).toEqual([
      ['Objectives', 34, 26], ['Airfields', 7, 12], ['Fleets', 2, 1], ['SAM sites', 19, 22], ['Deployed', 41, 37],
    ])
    expect(m.board[1].blueNote).toBe('+14 FOBs')
    expect(m.board[2].blueNote).toBe('Forrestal group, Tarawa group')
    expect(m.board[0].blueNote).toBe('+6 this week')
    expect(m.taken).toEqual({ blue: 6, red: 2 })
    expect(m.totals).toEqual({ captures: 8, airKills: 102, groundKills: 383, activePilots: 46, deployed: 78 })
    expect(m.blueShare).toBeCloseTo(34 / 60)
  })

  it('puts the Last Stand first, then story events, then captures, newest first', () => {
    const m = posterModel(REDSTORM_FIXTURE, FALLBACK)
    expect(m.events.map(e => e.kind)).toEqual(['laststand', 'story', 'story', 'capture', 'capture', 'capture', 'capture'])
    expect(m.events[0].title).toBe('The Last Stand is shot down')
    expect(m.events[0].detail).toBe('NATO pilots Mako, Viper 1-1 brought down the nuclear-armed aircraft.')
    // its outcome's own story event is not listed twice
    expect(m.events.some(e => e.title.startsWith('The Last Stand:'))).toBe(false)
    expect(m.events[1].title).toBe('The Transcaucasus Front breaks')
    expect(m.events[3]).toMatchObject({ kind: 'capture', side: 'Blue', title: 'NATO take Kutaisi', detail: 'Captured by Shade, Mako' })
  })

  it('uses the dashboard look and the campaign names when bfdb has none', () => {
    const m = posterModel({ ...LEVANT_FIXTURE, blue_label: null, red_label: null }, FALLBACK)
    expect(m.theme).toBe('levant')
    expect([m.blue, m.red]).toEqual(['BLUFOR', 'REDFOR'])
    expect(m.lastStand).toBeNull()
    expect(m.events[0]).toMatchObject({ kind: 'story', title: 'Aleppo falls' })
  })

  it('draws an empty board for a server with no round', () => {
    const r: WeeklyReportResponse = { ...LEVANT_FIXTURE, brand_name: null, round_number: null, report: null }
    const m = posterModel(r, FALLBACK)
    expect(m.quiet).toBe(true)
    expect(m.day).toBeNull()
    expect(m.brand).toBe('Vector Strike')
    expect(m.board.every(b => b.blue === 0 && b.red === 0)).toBe(true)
    expect(m.events).toEqual([])
    expect(draftDispatch(m).headline).toBe('A quiet week at the front')
  })

  it('names the week across a month end', () => {
    expect(weekRange(new Date('2026-10-28T18:00:00Z'), new Date('2026-11-04T18:00:00Z'))).toBe('28 OCT – 4 NOV 2026')
    expect(weekRange(new Date('2026-11-10T00:00:00Z'), new Date('2026-11-11T00:00:00Z'))).toBe('10 NOV 2026')
  })
})

describe('draftDispatch', () => {
  it('leads with the Last Stand when it happened this week', () => {
    const d = draftDispatch(posterModel(REDSTORM_FIXTURE, FALLBACK))
    expect(d.headline).toBe('Last Stand shot down')
    expect(d.standfirst).toBe('8 objectives changed hands, 485 kills claimed and 46 pilots in the fight in the week')
    const ps = paragraphs(d.body)
    expect(ps[0]).toBe('NATO hold 34 objectives and USSR 26. This week NATO took 6 and USSR 2.')
    expect(ps[1]).toContain('brought down the nuclear-armed aircraft')
    expect(ps.some(p => p.startsWith('Senaki changed hands 3 times'))).toBe(true)
    expect(ps.some(p => p.startsWith('Shade [VF84] had the best week: 9 air kills, 31 ground kills, 3 captures, 2 deployments.'))).toBe(true)
    expect(ps[ps.length - 1]).toBe('Will to fight stands at NATO 61, USSR 24.')
  })

  it('says who gained ground otherwise', () => {
    expect(draftDispatch(posterModel(LEVANT_FIXTURE, FALLBACK)).headline).toBe('NATO gain ground')
    const red = structuredClone(LEVANT_FIXTURE)
    red.report!.forces.red.captured = 12
    expect(draftDispatch(posterModel(red, FALLBACK)).headline).toBe('RSII push forward')
  })
})

describe('the Discord post', () => {
  const m = posterModel(REDSTORM_FIXTURE, FALLBACK)
  const d = draftDispatch(m)

  it('is short, names the week and links the dashboard', () => {
    const t = postText(m, d, 'https://redstorm.vectorstrike.org')
    expect(t.split('\n')).toEqual([
      '**Southern Flank 1985: the week at the front** (Day 12, Round 1)',
      '**Last Stand shot down.** 8 objectives changed hands, 485 kills claimed and 46 pilots in the fight in the week.',
      'Pilots of the week: **Shade**, **Ivanov**, **Mako**',
      'Squadron of the week: **Jolly Rogers** [VF84]',
      'NATO pilots Mako, Viper 1-1 brought down the nuclear-armed aircraft.',
      'https://redstorm.vectorstrike.org',
    ])
  })

  it('never goes over Discord’s limit', () => {
    const t = postText(m, { ...d, standfirst: 'x'.repeat(3000) })
    expect(t.length).toBe(DISCORD_MAX)
    expect(t.endsWith('…')).toBe(true)
  })

  it('carries the poster, the text and where it goes', () => {
    const png = `data:image/png;base64,${btoa('\x89PNG fake')}`
    expect(reportPostBody(REDSTORM_FIXTURE, m, '  Week 12  ', png)).toEqual({
      instance: 'redstorm',
      server: '[VS] Vector Strike | Cold War 1985 PvP',
      text: 'Week 12',
      image: btoa('\x89PNG fake'),
      filename: 'weekly-report-redstorm-2026-11-10.png',
    })
    expect(reportPostBody(REDSTORM_FIXTURE, m, 'x', 'data:image/jpeg;base64,AAAA').filename)
      .toBe('weekly-report-redstorm-2026-11-10.jpg')
    expect(() => reportPostBody(REDSTORM_FIXTURE, m, '  ', png)).toThrow('no text')
    expect(() => reportPostBody(REDSTORM_FIXTURE, m, 'x'.repeat(2001), png)).toThrow('2001 characters')
    expect(() => reportPostBody(REDSTORM_FIXTURE, m, 'x', 'data:image/gif;base64,AAAA')).toThrow('not a PNG')
    expect(() => reportPostBody(REDSTORM_FIXTURE, m, 'x', `data:image/png;base64,${'A'.repeat(4_000_000)}`)).toThrow('too big')
    expect(posterFilename(posterModel(LEVANT_FIXTURE, FALLBACK))).toBe('weekly-report-vs1-2026-11-10.png')
  })
})

describe('WeeklyPoster', () => {
  it('draws Red Storm in the situation-room look, with the Last Stand flash', () => {
    const m = posterModel(REDSTORM_FIXTURE, FALLBACK)
    const html = renderToStaticMarkup(<WeeklyPoster id="weekly-poster" model={m} dispatch={draftDispatch(m)} site={{ dashboard: 'https://redstorm.vectorstrike.org' }} />)
    expect(html).toContain('id="weekly-poster"')
    expect(html).toContain('data-poster="redstorm"')
    expect(html).toContain('The Allied Dispatch')
    expect(html).toContain('Flash · The Last Stand')
    expect(html).toContain('Last Stand shot down')
    expect(html).toContain('redstorm.vectorstrike.org')
    expect(html).toContain('#ffb000')
  })

  it('draws Levant in the dashboard look, without a flash', () => {
    const m = posterModel(LEVANT_FIXTURE, FALLBACK)
    const html = renderToStaticMarkup(<WeeklyPoster model={m} dispatch={draftDispatch(m)} />)
    expect(html).toContain('data-poster="levant"')
    expect(html).toContain('Levant <span>Ablaze</span>')
    expect(html).toContain('NATO gain ground')
    expect(html).not.toContain('Flash · The Last Stand')
  })
})
