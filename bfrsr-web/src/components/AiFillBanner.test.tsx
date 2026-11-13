import { beforeAll, describe, expect, it } from 'vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import type { HqAiFill, HqView } from '../api'
import { campaign } from '../config/campaign'
import AiFillBanner, { aiFillLines } from './AiFillBanner'

const fill = (over: Partial<HqAiFill> = {}): HqAiFill => ({
  pilots: 0, enemy_pilots: 0, enemy_match: 0, pressure: 0, hq_pressure: 0, ai_led: false, ...over,
})
const view = (side: 'Blue' | 'Red', ai_fill?: HqAiFill | null): Pick<HqView, 'side' | 'ai_fill'> => ({ side, ai_fill })

beforeAll(() => {
  campaign.blueLabel = 'NATO'
  campaign.redLabel = 'Soviet'
})

describe('AI share lines', () => {
  it('gives the viewer’s side in full and only the other side’s pilots', () => {
    const lines = aiFillLines([view('Red', fill({ pilots: 0, enemy_pilots: 3, pressure: 0.43, ai_led: true }))])
    expect(lines.map((l) => l.text)).toEqual([
      'NATO — 3 PILOTS',
      'SOVIET — AI-LED · 0 PILOTS · AI AT 43%',
    ])
    expect(lines[0].aiPct).toBeNull()
    expect(lines[0].aiLed).toBeNull()
  })

  it('gives both sides in full from an admin’s two views', () => {
    const lines = aiFillLines([
      view('Blue', fill({ pilots: 3, enemy_pilots: 0, pressure: 0.351 })),
      view('Red', fill({ pilots: 0, enemy_pilots: 3, pressure: 0.43, ai_led: true })),
    ])
    expect(lines.map((l) => l.text)).toEqual([
      'NATO — 3 PILOTS · AI AT 35%',
      'SOVIET — AI-LED · 0 PILOTS · AI AT 43%',
    ])
  })

  it('says one pilot, and keeps the percentage within 0..100', () => {
    expect(aiFillLines([view('Blue', fill({ pilots: 1, pressure: 1.4 }))])[0].text).toBe('NATO — 1 PILOT · AI AT 100%')
    expect(aiFillLines([view('Blue', fill({ pilots: 2, pressure: -0.2 }))])[0].text).toBe('NATO — 2 PILOTS · AI AT 0%')
  })

  it('is empty when ai_fill is absent or null, or there is no view', () => {
    expect(aiFillLines([view('Blue')])).toEqual([])
    expect(aiFillLines([view('Red', null)])).toEqual([])
    expect(aiFillLines([undefined, null])).toEqual([])
    expect(aiFillLines([])).toEqual([])
  })
})

describe('AI share banner', () => {
  it('draws nothing without lines', () => {
    expect(renderToStaticMarkup(<AiFillBanner lines={[]} />)).toBe('')
    expect(renderToStaticMarkup(<AiFillBanner lines={aiFillLines([view('Blue', null)])} compact />)).toBe('')
  })

  it('labels each line with its full text and draws only what is known', () => {
    const html = renderToStaticMarkup(
      <AiFillBanner lines={aiFillLines([view('Red', fill({ pilots: 0, enemy_pilots: 3, pressure: 0.43, ai_led: true }))])} compact />,
    )
    expect(html).toContain('aria-label="SOVIET — AI-LED · 0 PILOTS · AI AT 43%"')
    expect(html).toContain('aria-label="NATO — 3 PILOTS"')
    expect(html).toContain('ai-fill--compact')
    // One AI figure (Soviet's) and one AI-LED mark: nothing invented for NATO.
    expect(html.match(/AI AT <span/g)).toHaveLength(1)
    expect(html.match(/>AI-LED</g)).toHaveLength(1)
  })
})
