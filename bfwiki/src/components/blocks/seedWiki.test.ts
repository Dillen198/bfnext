import { describe, expect, it } from 'vitest'
import { MOCKUP_LANGS, isMockupLang, type MockupLang } from './langs'
import { parseCockpitPanel, parseDcsMessage, parseF10Menu, parseSteps, parseWheel, type Parsed } from './parse'

/**
 * Every mockup block in bfdb/seed_wiki must parse. bfdb seeds the live wiki
 * from those pages, and a block that does not parse draws as an error box on
 * the site, so a typo there should fail CI rather than reach players.
 */

const PARSERS: Record<MockupLang, (text: string) => Parsed<unknown>> = {
  f10menu: parseF10Menu,
  dcsmessage: parseDcsMessage,
  cockpitpanel: parseCockpitPanel,
  wheel: parseWheel,
  steps: parseSteps,
}

const PAGES = import.meta.glob<string>('../../../../bfdb/seed_wiki/**/*.md', {
  query: '?raw',
  import: 'default',
  eager: true,
})

// A fence opens with three or more backticks or tildes and closes with the same run.
const FENCE = /^(`{3,}|~{3,})(\w+)[^\n]*\n([\s\S]*?)^\1[ \t]*$/gm

const BLOCKS = Object.entries(PAGES).flatMap(([path, src]) => {
  const page = path.replace(/^.*seed_wiki\//, '')
  return [...src.replace(/\r\n/g, '\n').matchAll(FENCE)]
    .filter(m => isMockupLang(m[2]))
    .map((m, i) => ({ name: `${page} ${m[2]} #${i + 1}`, lang: m[2] as MockupLang, body: m[3] }))
})

describe('seed wiki', () => {
  it('finds the pages and their blocks', () => {
    expect(Object.keys(PAGES).length).toBeGreaterThan(10)
    for (const lang of MOCKUP_LANGS) expect(BLOCKS.some(b => b.lang === lang), lang).toBe(true)
  })

  it.each(BLOCKS.map(b => [b.name, b] as const))('%s parses', (_, b) => {
    const p = PARSERS[b.lang](b.body)
    expect(p.ok, p.ok ? '' : p.error).toBe(true)
  })
})
