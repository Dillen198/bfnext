// The SITUATION board's panels fed with what bfdb's `/api/stats` and
// `/api/oob` really send (bfdb/src/routes/campaign.rs: will_json,
// story_json, oob_json), not with the dashboard's own idea of them.

import { describe, expect, it, vi } from 'vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import { MemoryRouter } from 'react-router-dom'
import type { ReactElement } from 'react'
import type { Stats } from '../api'
import { LossesStrip, WarSoFar, WillToFight } from './Dashboard'
import { oobMock } from './oobMock'

// The map pulls in Leaflet, which needs a window; the board's panels do not.
vi.mock('../components/TacMap', () => ({ default: () => null }))
vi.mock('../components/ObjectiveDrawer', () => ({ default: () => null }))

const html = (el: ReactElement) => renderToStaticMarkup(<MemoryRouter>{el}</MemoryRouter>)

/** `/api/stats` from an engine with `war_will` and `story` on, as JSON
 *  comes off the wire. */
const STATS = JSON.parse(`{
  "will": {
    "blue": { "value": 71.4, "trend": -2.3 },
    "red": { "value": 38.2, "trend": null },
    "threshold": 25,
    "causes": [
      { "side": "Red", "text": "Tuapse refinery burning", "delta": -6, "time": "2026-10-31T10:12:00+00:00" },
      { "side": "Blue", "text": "Batumi held", "delta": 2.5, "time": "2026-10-31T09:40:00+00:00" },
      { "side": "Neutral", "text": "Ceasefire talks", "delta": 0.4, "time": "2026-10-31T09:00:00+00:00" }
    ]
  },
  "story": [
    { "id": "tuapse_burns", "time": "2026-10-31T10:12:00+00:00", "title": "Tuapse burns",
      "text": "The refinery at Tuapse is burning.", "caused_by": ["Viper 1-1"], "side": "Red",
      "effects": ["Red fuel output -40%"] },
    { "id": "backfire_reserve", "time": "2026-10-31T11:00:00+00:00", "title": "Backfires from the reserve",
      "text": "", "caused_by": [], "side": null, "effects": [] },
    { "id": "backfire_reserve", "time": "2026-10-31T13:00:00+00:00", "title": "Backfires from the reserve",
      "text": "Two more.", "caused_by": [], "side": "Neutral", "effects": ["Red Tu-22M3 +2"] }
  ]
}`) as Pick<Stats, 'will' | 'story'>

describe('will to fight', () => {
  it('draws both meters, the terms line and the causes from the engine\'s shape', () => {
    const out = html(<WillToFight will={STATS.will} />)
    expect(out).toContain('aria-valuenow="71"')
    expect(out).toContain('aria-valuenow="38"')
    expect(out).toContain('left:25%')
    expect(out).toContain('▼')
    expect(out).toContain('Tuapse refinery burning')
    expect(out).toContain('+2.5')
    expect(out).toContain('Ceasefire talks')
    expect(out).not.toContain('Awaiting traffic')
  })

  it('awaits traffic with the feature off', () => {
    expect(html(<WillToFight will={null} />)).toContain('Awaiting traffic')
  })
})

describe('the war so far', () => {
  it('logs every firing newest first, a repeated event included', () => {
    const out = html(<WarSoFar story={STATS.story} />)
    expect(out.split('Backfires from the reserve').length - 1).toBe(2)
    expect(out.indexOf('Two more.')).toBeLessThan(out.indexOf('Tuapse burns'))
    expect(out).toContain('Credited to Viper 1-1')
    expect(out).toContain('Red fuel output -40%')
  })

  it('awaits traffic with nothing fired', () => {
    expect(html(<WarSoFar story={[]} />)).toContain('No story events have fired this round.')
  })
})

describe('the losses line', () => {
  it('prints each side\'s losses this round and links to the order of battle', () => {
    const oob = oobMock(false)
    const out = html(<LossesStrip oob={oob} />)
    const lost = (rows: typeof oob.blue) => String(rows.reduce((n, r) => n + r.lost, 0))
    expect(out).toContain('href="/oob"')
    expect(out).toContain(`data-testid="losses-blue">${lost(oob.blue)}<`)
    expect(out).toContain(`data-testid="losses-red">${lost(oob.red)}<`)
  })

  it('stays off the board while the engine sends no order of battle', () => {
    expect(html(<LossesStrip oob={{ round: null, blue: [], red: [] }} />)).toBe('')
    expect(html(<LossesStrip oob={undefined} />)).toBe('')
  })
})
