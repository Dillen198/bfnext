import { describe, expect, it } from 'vitest'
import { BLOCK_EXAMPLES } from './examples'
import { parseCockpitPanel, parseDcsMessage, parseF10Menu, parseSteps, parseWheel } from './parse'

const PARSERS = {
  f10menu: parseF10Menu,
  dcsmessage: parseDcsMessage,
  cockpitpanel: parseCockpitPanel,
  wheel: parseWheel,
  steps: parseSteps,
} as const

describe('block examples', () => {
  it.each(BLOCK_EXAMPLES.map(ex => [ex.lang, ex] as const))('%s example parses', (_, ex) => {
    const p = PARSERS[ex.lang](ex.body)
    expect(p.ok, p.ok ? '' : p.error).toBe(true)
  })

  it('has one example per block', () => {
    expect(BLOCK_EXAMPLES.map(e => e.lang).sort()).toEqual(Object.keys(PARSERS).sort())
  })
})

describe('invalid JSON', () => {
  it.each(['f10menu', 'cockpitpanel', 'wheel', 'steps'] as const)('%s reports the line of a syntax error', lang => {
    const p = PARSERS[lang]('{\n  "items": [\n    {"label": "x",}\n  ]\n}')
    expect(p.ok).toBe(false)
    if (!p.ok) expect(p.error).toMatch(/invalid JSON/)
  })

  it('an empty block is an error, not a blank', () => {
    for (const parse of Object.values(PARSERS)) {
      const p = parse('   \n')
      expect(p.ok).toBe(false)
    }
  })
})

describe('f10menu', () => {
  it('reads nested items and defaults the root', () => {
    const p = parseF10Menu('{"items":[{"label":"Cargo","items":[{"label":"Unpack","says":"done","note":"n"}]}]}')
    expect(p).toEqual({
      ok: true,
      value: { root: 'F10 Other', items: [{ label: 'Cargo', items: [{ label: 'Unpack', says: 'done', note: 'n' }] }] },
    })
  })

  it('names the path of a bad field', () => {
    const p = parseF10Menu('{"items":[{"label":"Cargo","items":[{"label":3}]}]}')
    expect(p).toEqual({ ok: false, error: 'items[0].items[0].label must be a string' })
  })

  it('wants items to be a list', () => {
    const p = parseF10Menu('{"root":"F10 Other"}')
    expect(p.ok).toBe(false)
  })
})

describe('dcsmessage', () => {
  it('keeps the text as written, line breaks included', () => {
    expect(parseDcsMessage('line one\n  line two\n\n')).toEqual({ ok: true, value: 'line one\n  line two' })
  })

  it('is plain text, so JSON-looking bodies are fine', () => {
    expect(parseDcsMessage('{"not": "json"').ok).toBe(true)
  })
})

describe('cockpitpanel', () => {
  it('reads the row kinds and guesses submenu buttons from a trailing ">"', () => {
    const p = parseCockpitPanel(
      '{"tab":"cargo","rows":[{"heading":"H"},{"text":"t","style":"dim"},{"button":"Go","says":"ok"},{"button":"Crates  >"}],"status":"s"}',
    )
    expect(p.ok).toBe(true)
    if (!p.ok) return
    expect(p.value.tab).toBe('CARGO')
    expect(p.value.rows).toEqual([
      { kind: 'heading', text: 'H' },
      { kind: 'text', text: 't', style: 'dim' },
      { kind: 'button', label: 'Go', says: 'ok', sub: false, go: false, enabled: true },
      { kind: 'button', label: 'Crates  >', says: undefined, sub: true, go: false, enabled: true },
    ])
    expect(p.value.status).toBe('s')
  })

  it('accepts the WIKI tab and refuses an unknown one', () => {
    expect(parseCockpitPanel('{"tab":"WIKI","rows":[]}').ok).toBe(true)
    const p = parseCockpitPanel('{"tab":"MAP","rows":[]}')
    expect(p.ok).toBe(false)
    if (!p.ok) expect(p.error).toMatch(/tab must be one of MENU, CARGO/)
  })

  it('refuses a row it cannot draw', () => {
    const p = parseCockpitPanel('{"tab":"MENU","rows":[{"picture":"x"}]}')
    expect(p).toEqual({ ok: false, error: 'rows[0] needs a "heading", "text" or "button" field' })
  })
})

describe('wheel', () => {
  it('defaults the theme and checks it', () => {
    const p = parseWheel('{"items":[{"label":"A"}]}')
    expect(p.ok && p.value.theme).toBe('green')
    expect(parseWheel('{"theme":"pink","items":[{"label":"A"}]}').ok).toBe(false)
  })

  it('needs at least one slot', () => {
    expect(parseWheel('{"theme":"blue","items":[]}').ok).toBe(false)
  })
})

describe('steps', () => {
  it('reads steps with optional images', () => {
    const p = parseSteps('[{"title":"One","text":"**a**"},{"title":"Two","image":"/api/wiki/images/x"}]')
    expect(p).toEqual({
      ok: true,
      value: [
        { title: 'One', text: '**a**', image: undefined, alt: undefined },
        { title: 'Two', text: '', image: '/api/wiki/images/x', alt: undefined },
      ],
    })
  })

  it('wants a list of steps', () => {
    const p = parseSteps('{"title":"One"}')
    expect(p).toEqual({ ok: false, error: 'the block must be a list ([...])' })
  })
})
