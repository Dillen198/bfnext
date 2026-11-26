import './blocks.css'
import BlockError from './BlockError'
import CockpitPanel from './CockpitPanel'
import DcsMessage from './DcsMessage'
import F10Menu from './F10Menu'
import Steps from './Steps'
import Wheel from './Wheel'
import type { MockupLang } from './langs'
import { parseCockpitPanel, parseDcsMessage, parseF10Menu, parseSteps, parseWheel, type Parsed } from './parse'

/**
 * One mockup block, from its fenced-code language and body. A body that does
 * not parse draws as an error box with the source under it.
 *
 * Each block is keyed on its text, so editing a block in the live preview
 * starts its replica afresh rather than keeping a menu level that may no
 * longer exist.
 */
export default function MockupBlock({
  lang,
  text,
  renderMarkdown,
}: {
  lang: MockupLang
  text: string
  /** How a steps block draws its markdown: the page's own renderer. */
  renderMarkdown: (markdown: string) => React.ReactNode
}) {
  const bad = (p: Parsed<unknown>) => (!p.ok ? <BlockError lang={lang} error={p.error} source={text} /> : null)
  switch (lang) {
    case 'f10menu': {
      const p = parseF10Menu(text)
      return p.ok ? <F10Menu key={text} spec={p.value} /> : bad(p)
    }
    case 'dcsmessage': {
      const p = parseDcsMessage(text)
      return p.ok ? <DcsMessage text={p.value} /> : bad(p)
    }
    case 'cockpitpanel': {
      const p = parseCockpitPanel(text)
      return p.ok ? <CockpitPanel key={text} spec={p.value} /> : bad(p)
    }
    case 'wheel': {
      const p = parseWheel(text)
      return p.ok ? <Wheel key={text} spec={p.value} /> : bad(p)
    }
    case 'steps': {
      const p = parseSteps(text)
      return p.ok ? <Steps key={text} steps={p.value} renderText={renderMarkdown} /> : bad(p)
    }
  }
}
