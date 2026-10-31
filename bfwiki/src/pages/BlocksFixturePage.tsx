import { ThemeToggle } from '@shared/web/theme'
import WikiMarkdown from '../components/WikiMarkdown'
import BlocksReference from '../components/blocks/BlocksReference'
import { BLOCK_EXAMPLES, fenced } from '../components/blocks/examples'

/**
 * Dev-only (/__blocks, registered only under `vite dev`): every mockup block
 * drawn from its reference example, plus a broken one, so they can be looked
 * at and screenshotted without a bfdb behind the dev server
 * (scripts/shoot-blocks.mjs takes the pictures in docs/blocks/).
 */
const BROKEN = '```wheel\n{"theme": "green", "items": [ {"label": "Cargo", } ]}\n```'

// A full ring in another theme: past ten slots the rest fold into MORE.
const WHEEL_AMBER = [
  '```wheel',
  JSON.stringify({
    theme: 'amber',
    title: 'COMBAT',
    subtitle: 'F-16C  ·  Vector 1-1',
    items: ['JTAC', 'GCI/EWR', 'EWR', 'Nearest', 'Objectives', 'Actions', 'Flight', 'Recon', 'Smoke', 'Fuel', 'Repair', 'Info'].map(
      label => ({ label, says: `${label}: done` }),
    ),
  }),
  '```',
].join('\n')

const PAGE = [
  '# Mockup blocks',
  "Every block the wiki draws as a live replica, from the examples in the editor's Blocks reference.",
  ...BLOCK_EXAMPLES.flatMap(ex => [`## ${ex.name} (\`${ex.lang}\`)`, fenced(ex)]),
  '## Wheel, amber theme, more than ten slots',
  WHEEL_AMBER,
  '## A block with a mistake in it',
  BROKEN,
  "## The editor's Blocks reference",
].join('\n\n')

export default function BlocksFixturePage() {
  return (
    <div style={{ minHeight: '100%', background: 'var(--bg)' }}>
      <div style={{ display: 'flex', justifyContent: 'flex-end', padding: '10px 16px' }}>
        <ThemeToggle />
      </div>
      <main className="wiki-main" style={{ paddingTop: 0 }}>
        <div className="wiki-prose">
          <WikiMarkdown slug="__blocks">{PAGE}</WikiMarkdown>
          <BlocksReference onInsert={() => {}} />
        </div>
      </main>
    </div>
  )
}
