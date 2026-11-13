/**
 * Parsers for the wiki's mockup blocks: fenced code blocks whose language tag
 * picks a live replica of something the player sees in DCS.
 *
 *   ```f10menu       the F10 radio menu (JSON)
 *   ```dcsmessage    the top-right message box (plain text)
 *   ```cockpitpanel  the Vector Strike cockpit panel (JSON)
 *   ```wheel         the Vector Strike radial wheel (JSON)
 *   ```steps         a step-by-step walkthrough (JSON)
 *
 * Each parser returns either the checked value or a sentence saying what is
 * wrong and where, which the page shows in place of the block: a typo in a
 * page must show its author the mistake, never blank the section.
 *
 * The shapes are the shared block contract (see bfwiki/README.md, "Mockup
 * blocks"). Fields beyond the contract are optional extras this renderer
 * understands; anything else is ignored, so content written for a newer
 * renderer still draws here.
 */

export type Parsed<T> = { ok: true; value: T } | { ok: false; error: string }

// ── small helpers ──────────────────────────────────────────────────────

class ShapeError extends Error {}

function fail(path: string, what: string): never {
  throw new ShapeError(`${path} ${what}`)
}

function isObj(v: unknown): v is Record<string, unknown> {
  return typeof v === 'object' && v !== null && !Array.isArray(v)
}

function str(v: unknown, path: string): string {
  if (typeof v !== 'string') fail(path, 'must be a string')
  return v
}

function optStr(v: unknown, path: string): string | undefined {
  if (v === undefined || v === null) return undefined
  return str(v, path)
}

function optBool(v: unknown, path: string): boolean | undefined {
  if (v === undefined || v === null) return undefined
  if (typeof v !== 'boolean') fail(path, 'must be true or false')
  return v
}

function arr(v: unknown, path: string): unknown[] {
  if (!Array.isArray(v)) fail(path, 'must be a list ([...])')
  return v
}

/** JSON.parse with the position turned into a line number an author can find. */
function parseJson(text: string): unknown {
  try {
    return JSON.parse(text)
  } catch (e) {
    const msg = e instanceof Error ? e.message : String(e)
    const m = /position (\d+)/.exec(msg)
    // Newer engines already say "(line 3 column 7)".
    if (m && !/\bline \d/.test(msg)) {
      const line = text.slice(0, Number(m[1])).split('\n').length
      throw new ShapeError(`invalid JSON near line ${line}: ${msg}`)
    }
    throw new ShapeError(`invalid JSON: ${msg}`)
  }
}

function run<T>(text: string, check: (v: unknown) => T): Parsed<T> {
  if (!text.trim()) return { ok: false, error: 'the block is empty' }
  try {
    return { ok: true, value: check(parseJson(text)) }
  } catch (e) {
    if (e instanceof ShapeError) return { ok: false, error: e.message }
    throw e
  }
}

// ── f10menu ────────────────────────────────────────────────────────────

export interface MenuItem {
  label: string
  /** Present: a submenu. Absent: a command. */
  items?: MenuItem[]
  /** What the server prints in the message box after the click. */
  says?: string
  /** An explanation shown under the message, for the reader. */
  note?: string
}

export interface F10MenuSpec {
  root: string
  items: MenuItem[]
}

function menuItems(v: unknown, path: string): MenuItem[] {
  return arr(v, path).map((it, i) => {
    const p = `${path}[${i}]`
    if (!isObj(it)) fail(p, 'must be an object like {"label": "..."}')
    const item: MenuItem = { label: str(it.label, `${p}.label`) }
    if (it.items !== undefined) item.items = menuItems(it.items, `${p}.items`)
    item.says = optStr(it.says, `${p}.says`)
    item.note = optStr(it.note, `${p}.note`)
    return item
  })
}

export function parseF10Menu(text: string): Parsed<F10MenuSpec> {
  return run(text, v => {
    if (!isObj(v)) fail('the block', 'must be an object like {"root": "F10 Other", "items": [...]}')
    return {
      root: optStr(v.root, 'root') ?? 'F10 Other',
      items: menuItems(v.items, 'items'),
    }
  })
}

// ── dcsmessage ─────────────────────────────────────────────────────────

/** Plain text, exactly as printed. Only trailing blank lines are dropped. */
export function parseDcsMessage(text: string): Parsed<string> {
  const body = text.replace(/\s+$/, '')
  if (!body.trim()) return { ok: false, error: 'the block is empty: write the message text inside it' }
  return { ok: true, value: body }
}

// ── cockpitpanel ───────────────────────────────────────────────────────

export const PANEL_TABS = ['MENU', 'CARGO', 'STATUS', 'EWR', 'LOG', 'WHEEL', 'WIKI'] as const
export type PanelTab = (typeof PANEL_TABS)[number]

/** Colours a text row can take: the panel's own static skins. */
export const TEXT_STYLES = ['text', 'dim', 'bad', 'warn', 'good', 'blue', 'mono', 'answer'] as const
export type TextStyle = (typeof TEXT_STYLES)[number]

export type PanelRow =
  | { kind: 'heading'; text: string }
  | { kind: 'text'; text: string; style: TextStyle }
  | { kind: 'button'; label: string; says?: string; sub: boolean; go: boolean; enabled: boolean }
  | { kind: 'gap' }

export interface CockpitPanelSpec {
  tab: PanelTab
  rows: PanelRow[]
  status?: string
  /** The second row's breadcrumb ("F10 > Cargo"). MENU always has one. */
  crumb?: string
}

export function parseCockpitPanel(text: string): Parsed<CockpitPanelSpec> {
  return run(text, v => {
    if (!isObj(v)) fail('the block', 'must be an object like {"tab": "MENU", "rows": [...]}')
    const tabRaw = (optStr(v.tab, 'tab') ?? 'MENU').toUpperCase()
    if (!(PANEL_TABS as readonly string[]).includes(tabRaw)) {
      fail('tab', `must be one of ${PANEL_TABS.join(', ')} (got "${tabRaw}")`)
    }
    const rows = arr(v.rows ?? [], 'rows').map((r, i): PanelRow => {
      const p = `rows[${i}]`
      if (!isObj(r)) fail(p, 'must be an object: {"heading"}, {"text"} or {"button"}')
      if (r.heading !== undefined) return { kind: 'heading', text: str(r.heading, `${p}.heading`) }
      if (r.button !== undefined) {
        const label = str(r.button, `${p}.button`)
        return {
          kind: 'button',
          label,
          says: optStr(r.says, `${p}.says`),
          // A submenu row reads "Name  >" in the panel's accent, bold.
          sub: optBool(r.sub, `${p}.sub`) ?? label.trim().endsWith('>'),
          go: optBool(r.go, `${p}.go`) ?? false,
          enabled: optBool(r.enabled, `${p}.enabled`) ?? true,
        }
      }
      if (r.text !== undefined) {
        const style = (optStr(r.style, `${p}.style`) ?? 'text') as TextStyle
        if (!(TEXT_STYLES as readonly string[]).includes(style)) {
          fail(`${p}.style`, `must be one of ${TEXT_STYLES.join(', ')}`)
        }
        return { kind: 'text', text: str(r.text, `${p}.text`), style }
      }
      if (r.gap !== undefined) return { kind: 'gap' }
      fail(p, 'needs a "heading", "text" or "button" field')
    })
    return {
      tab: tabRaw as PanelTab,
      rows,
      status: optStr(v.status, 'status'),
      crumb: optStr(v.crumb, 'crumb'),
    }
  })
}

// ── wheel ──────────────────────────────────────────────────────────────

/** The wheel's colour themes, from THEMES in bfcockpit.lua. */
export const WHEEL_THEMES = {
  green: [0x8e, 0xc8, 0x3f],
  blue: [0x4f, 0xa3, 0xe0],
  amber: [0xe0, 0xa0, 0x50],
  red: [0xe0, 0x6c, 0x6c],
  white: [0xd8, 0xdd, 0xe0],
} as const
export type WheelTheme = keyof typeof WHEEL_THEMES

export interface WheelItem {
  label: string
  items?: WheelItem[]
  /** What the server answers, shown in the strip under the wheel. */
  says?: string
  /** An icon name from the wheel's set; picked from the label otherwise. */
  icon?: string
  /** The hub's second line while this slot is highlighted. */
  hub?: string
  /** Asks for a second click, like DESTROY does. Guessed from the label otherwise. */
  danger?: boolean
}

export interface WheelSpec {
  theme: WheelTheme
  items: WheelItem[]
  /** The hub's title at the top level. */
  title?: string
  /** The hub's second line at the top level (airframe, points). */
  subtitle?: string
}

function wheelItems(v: unknown, path: string): WheelItem[] {
  return arr(v, path).map((it, i) => {
    const p = `${path}[${i}]`
    if (!isObj(it)) fail(p, 'must be an object like {"label": "..."}')
    const item: WheelItem = { label: str(it.label, `${p}.label`) }
    if (it.items !== undefined) item.items = wheelItems(it.items, `${p}.items`)
    item.says = optStr(it.says, `${p}.says`)
    item.icon = optStr(it.icon, `${p}.icon`)
    item.hub = optStr(it.hub, `${p}.hub`)
    item.danger = optBool(it.danger, `${p}.danger`)
    return item
  })
}

export function parseWheel(text: string): Parsed<WheelSpec> {
  return run(text, v => {
    if (!isObj(v)) fail('the block', 'must be an object like {"theme": "green", "items": [...]}')
    const theme = (optStr(v.theme, 'theme') ?? 'green').toLowerCase()
    if (!(theme in WHEEL_THEMES)) fail('theme', `must be one of ${Object.keys(WHEEL_THEMES).join(', ')}`)
    const items = wheelItems(v.items, 'items')
    if (items.length === 0) fail('items', 'needs at least one slot')
    return {
      theme: theme as WheelTheme,
      items,
      title: optStr(v.title, 'title'),
      subtitle: optStr(v.subtitle, 'subtitle'),
    }
  })
}

// ── steps ──────────────────────────────────────────────────────────────

export interface Step {
  title: string
  /** Markdown. */
  text: string
  image?: string
  /** Alt text for the image; the step title otherwise. */
  alt?: string
}

export function parseSteps(text: string): Parsed<Step[]> {
  return run(text, v => {
    const steps = arr(v, 'the block').map((s, i) => {
      const p = `step ${i + 1}`
      if (!isObj(s)) fail(p, 'must be an object like {"title": "...", "text": "..."}')
      return {
        title: str(s.title, `${p}.title`),
        text: optStr(s.text, `${p}.text`) ?? '',
        image: optStr(s.image, `${p}.image`),
        alt: optStr(s.alt, `${p}.alt`),
      }
    })
    if (steps.length === 0) fail('the block', 'needs at least one step')
    return steps
  })
}
