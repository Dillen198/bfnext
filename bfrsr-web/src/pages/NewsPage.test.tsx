import { describe, expect, it } from 'vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { MemoryRouter } from 'react-router-dom'
import type { NewsArchive, NewsDay } from '../api'
import NewsPage from './NewsPage'

const day = (over: Partial<NewsDay> = {}): NewsDay => ({
  day: '2026-10-30',
  generated: '2026-10-30T23:00:00Z',
  round: 1,
  headline: 'Armour Pushes Up the Rioni',
  body: ['The line moved east today.', 'Losses were heavy on both sides.'],
  written_by: 'templates',
  items: [],
  facts: {
    blue_held: 6, red_held: 34, blue_captures: 1, red_captures: 0,
    air_kills: 3, ground_kills: 14, changed_hands: ['Senaki-Kolkhi'], campaign_day: 4, theatre: 'Caucasus',
  } as NewsDay['facts'],
  factions: { blue: 'NATO', red: 'Soviet' } as NewsDay['factions'],
  final_: true,
  ...over,
})

/** The page as it renders with the archive already fetched. */
function render(days: NewsDay[], path = '/news'): string {
  const qc = new QueryClient({ defaultOptions: { queries: { retry: false, staleTime: Infinity } } })
  qc.setQueryData<NewsArchive>(['news', 'archive'], { days })
  return renderToStaticMarkup(
    <QueryClientProvider client={qc}>
      <MemoryRouter initialEntries={[path]}>
        <NewsPage />
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

const ALT = {
  voice: 'soviet',
  headline: 'Heroic Defenders Hold the Rioni Line',
  body: ['Units of the Front repelled the aggressor.', 'The bridgehead is contained.'],
  written_by: 'claude-test',
}

describe('the wire', () => {
  it('prints The Allied Dispatch alone when the day has no other voice', () => {
    const html = render([day()])
    expect(html).toContain('The Allied Dispatch')
    expect(html).toContain('Special Dispatch to The Allied Dispatch.')
    expect(html).toContain('Armour Pushes Up the Rioni')
    expect(html).not.toContain('np-dual')
    expect(html).not.toContain('Front Bulletin')
    expect(html).not.toContain('role="tablist"')
  })

  it('never names the other dashboard or its paper', () => {
    const html = render([day({ alt: ALT })])
    expect(html).not.toMatch(/War Diary|Vector Strike/)
  })

  it('treats a null alt as no alt', () => {
    expect(render([day({ alt: null })])).not.toContain('np-dual')
  })

  it('sets the Front Bulletin beside the wire, two columns wide', () => {
    const html = render([day({ alt: ALT })])
    expect(html).toContain('np-dual')
    expect(html).toContain('np-voice--wire')
    expect(html).toContain('np-voice--alt')
    expect(html).toContain('Front Bulletin')
    expect(html).toContain('Heroic Defenders Hold the Rioni Line')
    expect(html).toContain('Units of the Front repelled the aggressor.')
    expect(html).toContain('The bridgehead is contained.')
    expect(html).toContain('written by claude-test')
    // The wire is all there, on the left.
    expect(html).toContain('Armour Pushes Up the Rioni')
    expect(html.indexOf('np-voice--wire')).toBeLessThan(html.indexOf('np-voice--alt'))
  })

  it('gives a phone two tabs, the wire selected', () => {
    const html = render([day({ alt: ALT })])
    expect(html).toContain('role="tablist"')
    expect(html).toMatch(/id="np-tab-wire"[^>]*aria-selected="true"/)
    expect(html).toMatch(/id="np-tab-alt"[^>]*aria-selected="false"/)
    expect(html).toMatch(/id="np-voice-wire"[^>]*data-active="true"/)
    expect(html).toMatch(/id="np-voice-alt"[^>]*data-active="false"/)
  })

  it('prints the communique of the edition being read', () => {
    const older = day({ day: '2026-10-29', headline: 'Quiet on the Coast', alt: { ...ALT, headline: 'Calm Holds at Sukhumi' } })
    const html = render([day(), older], '/news?ed=2026-10-29')
    expect(html).toContain('Calm Holds at Sukhumi')
    expect(html).toContain('A previous edition')
    // The newest day has no communique, so none of it shows here.
    expect(html).not.toContain('Heroic Defenders')
  })
})
