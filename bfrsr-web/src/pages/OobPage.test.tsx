import { describe, expect, it } from 'vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { MemoryRouter } from 'react-router-dom'
import type { OrderOfBattle } from '../api'
import { campaign } from '../config/campaign'
import { oobByRole, oobStrength, oobTotals } from '../lib/oob'
import OobPage from './OobPage'
import { oobMock } from './oobMock'

/** The page as it renders with `/api/oob` already answered. */
function render(oob: OrderOfBattle): string {
  const qc = new QueryClient({ defaultOptions: { queries: { retry: false, staleTime: Infinity } } })
  qc.setQueryData<OrderOfBattle>(['oob', null, ''], oob)
  return renderToStaticMarkup(
    <QueryClientProvider client={qc}>
      <MemoryRouter initialEntries={['/oob']}>
        <OobPage />
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

/** The text of the element carrying `data-testid="<id>"`. */
function fig(html: string, id: string): string | undefined {
  return new RegExp(`data-testid="${id}"[^>]*>([^<]*)<`).exec(html)?.[1]
}

const count = (html: string, s: string) => html.split(s).length - 1

describe('the order of battle', () => {
  it('files both sides, every type a line', () => {
    const oob = oobMock(true)
    const html = render(oob)
    expect(html).toContain('ORDER OF BATTLE')
    expect(html).toContain('data-testid="oob-roll-blue"')
    expect(html).toContain('data-testid="oob-roll-red"')
    expect(html).toContain(`${campaign.blueLabel} · regiment roll`)
    expect(html).toContain(`${campaign.redLabel} · regiment roll`)
    expect(html).toContain('F-14A')
    expect(html).toContain('Tu-22M3')
    expect(count(html, 'data-testid="oob-row"')).toBe(oob.blue.length + oob.red.length)
    expect(html).not.toContain('Classified')
  })

  it('stamps the other side CLASSIFIED when its stock is null, and still prints its losses', () => {
    const oob = oobMock(false)
    const html = render(oob)
    // One stamp per Soviet line, one on the Soviet total; none for NATO.
    expect(count(html, '>Classified<')).toBe(oob.red.length + 1)
    expect(html).toContain('data-testid="oob-stock-blue"')
    expect(html).not.toContain('data-testid="oob-stock-red"')
    expect(fig(html, 'oob-lost-red')).toBe(String(oob.red.reduce((n, r) => n + r.lost, 0)))
    expect(html).toContain('oob-bar--redacted')
  })

  it('adds the totals up from the lines', () => {
    const oob = oobMock(true)
    const html = render(oob)
    const sum = (k: 'stock' | 'lost', rows: OrderOfBattle['blue']) => rows.reduce((n, r) => n + (r[k] ?? 0), 0)
    // NATO and the Soviets both hold a type on an unlimited base: a floor, "+".
    expect(fig(html, 'oob-stock-blue')).toBe(`${sum('stock', oob.blue)}+`)
    expect(fig(html, 'oob-stock-red')).toBe(`${sum('stock', oob.red)}+`)
    expect(fig(html, 'oob-lost-blue')).toBe(String(sum('lost', oob.blue)))
    expect(fig(html, 'oob-lost-red')).toBe(String(sum('lost', oob.red)))
  })

  it('marks the types that never run out', () => {
    const html = render({ round: 1, blue: [{ type: 'F-4E-45MC', stock: 3, lost: 9, unlimited: true }], red: [] })
    expect(html).toContain('∞ Unltd')
    expect(html).toContain('oob-bar--unl')
    expect(html).toContain(`No airframes on file for ${campaign.redLabel}.`)
  })

  it('says it is awaiting traffic while the engine has sent none', () => {
    const html = render({ round: null, blue: [], red: [] })
    expect(html).toContain('Awaiting traffic')
    expect(html).not.toContain('data-testid="oob-roll-blue"')
  })
})

describe('order of battle sums', () => {
  it('reads a side with every stock null as classified, not as zero', () => {
    const t = oobTotals([{ type: 'Su-27', stock: null, lost: 2, unlimited: false }, { type: 'Mi-8MT', stock: null, lost: 1, unlimited: false }])
    expect(t).toEqual({ stock: null, lost: 3, types: 2, unlimited: false })
    expect(oobTotals([]).stock).toBe(0)
  })

  it('files helicopters apart from fixed wing, each by name', () => {
    const g = oobByRole([
      { type: 'Mi-8MT', stock: 1, lost: 0, unlimited: false },
      { type: 'MiG-29A', stock: 1, lost: 0, unlimited: false },
      { type: 'SH-60B', stock: 1, lost: 0, unlimited: false },
      { type: 'MiG-21Bis', stock: 1, lost: 0, unlimited: false },
    ])
    expect(g.map(x => [x.role, x.rows.map(r => r.type)])).toEqual([
      ['Fixed wing', ['MiG-21Bis', 'MiG-29A']],
      ['Rotary wing', ['Mi-8MT', 'SH-60B']],
    ])
  })

  it('measures strength against the round\'s starting number', () => {
    expect(oobStrength({ type: 'x', stock: 3, lost: 1, unlimited: false })).toBe(0.75)
    expect(oobStrength({ type: 'x', stock: 0, lost: 0, unlimited: false })).toBe(0)
    expect(oobStrength({ type: 'x', stock: 0, lost: 4, unlimited: true })).toBe(1)
    expect(oobStrength({ type: 'x', stock: null, lost: 4, unlimited: false })).toBeNull()
  })
})
