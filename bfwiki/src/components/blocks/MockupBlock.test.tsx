import { describe, expect, it } from 'vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import MockupBlock from './MockupBlock'
import { BLOCK_EXAMPLES } from './examples'
import { isMockupLang, type MockupLang } from './langs'

const render = (lang: MockupLang, text: string) =>
  renderToStaticMarkup(<MockupBlock lang={lang} text={text} renderMarkdown={md => <p>{md}</p>} />)

describe('MockupBlock', () => {
  it('knows its languages', () => {
    expect(isMockupLang('wheel')).toBe(true)
    expect(isMockupLang('statushexes')).toBe(false)
    expect(isMockupLang('json')).toBe(false)
  })

  it.each(BLOCK_EXAMPLES.map(ex => [ex.lang, ex] as const))('draws the %s example', (_, ex) => {
    const html = render(ex.lang, ex.body)
    expect(html).not.toContain('mk-error')
    expect(html.length).toBeGreaterThan(200)
  })

  it.each(['f10menu', 'cockpitpanel', 'wheel', 'steps'] as const)('shows an error box with the source for broken %s JSON', lang => {
    const html = render(lang, '{"items": [ {"label": "Cargo", } ]}')
    expect(html).toContain('mk-error')
    expect(html).toContain(`This <code>${lang}</code> block could not be drawn`)
    expect(html).toContain('invalid JSON')
    // the author's text is still on the page
    expect(html).toContain('&quot;Cargo&quot;')
  })

  it('shows an error box for an empty message', () => {
    expect(render('dcsmessage', '  ')).toContain('mk-error')
  })

  it('draws the F10 menu the way DCS numbers it', () => {
    const html = render('f10menu', '{"root":"F10 Other","items":[{"label":"Cargo","items":[]},{"label":"Help","says":"x"}]}')
    expect(html).toContain('F1. Cargo...')
    expect(html).toContain('F2. Help')
    expect(html).toContain('F11. Back')
    expect(html).toContain('F12. Exit')
    expect(html).toContain('role="menu"')
  })

  it('draws every panel tab, the chosen one lit', () => {
    const html = render('cockpitpanel', '{"tab":"WIKI","rows":[{"heading":"H"},{"button":"Go","says":"ok"}],"status":"st"}')
    for (const t of ['MENU', 'CARGO', 'STATUS', 'EWR', 'LOG', 'WHEEL', 'WIKI']) expect(html).toContain(`>${t}</span>`)
    expect(html).toContain('cp-tab cp-on">WIKI')
    expect(html).toContain('<button type="button" class="cp-btn cp-cmd">Go</button>')
    expect(html).toContain('st')
  })

  it('MENU has the back-and-crumb row', () => {
    expect(render('cockpitpanel', '{"tab":"MENU","rows":[]}')).toContain('F10 &gt; Other')
    expect(render('cockpitpanel', '{"tab":"STATUS","rows":[]}')).not.toContain('cp-crumb')
  })

  it('draws a wheel slot per item, as buttons', () => {
    const html = render('wheel', '{"theme":"blue","items":[{"label":"Cargo","items":[{"label":"Load"}]},{"label":"Points"}]}')
    expect(html).toContain('aria-label="Cargo, opens a menu"')
    expect(html).toContain('aria-label="Points"')
    expect(html).toContain('CARGO')
    // the blue theme's accent on the hub title
    expect(html).toContain('rgba(79,163,224,1.000)')
  })

  it('draws the first step with progress', () => {
    const html = render('steps', '[{"title":"One","text":"first"},{"title":"Two","text":"second"}]')
    expect(html).toContain('Step 1 of 2')
    expect(html).toContain('first')
    expect(html).not.toContain('second')
    expect(html).toContain('aria-current="step"')
  })
})
