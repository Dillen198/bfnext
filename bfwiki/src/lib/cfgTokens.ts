/**
 * `{{cfg:...}}` placeholders in wiki markdown.
 *
 * Every DCS server instance this bfdb fronts runs its own engine config, so a
 * number written into a page — what an air kill pays, how long consolidation
 * takes, what an AWACS costs — is only correct for one of them. Instead of
 * forking the page per server, a page writes the *path* to the value and the
 * reader's selected instance supplies it:
 *
 * ```
 * An air kill is worth {{cfg:points.air_kill|350}} points.
 * ```
 *
 * `|350` is the fallback, used when the instance has no engine config
 * configured, the key is absent from it, or the facts request failed. Always
 * write one: it is what a reader sees if the live value can't be fetched, and
 * it keeps the raw markdown readable in the editor and in git.
 *
 * Paths index into the fact object with `.` for keys and `[n]` for array
 * indices, e.g. `warehouse.materiel.repair_cost`, `default_lives.Standard[0]`.
 */

const TOKEN = /\{\{\s*cfg:([^|}]+?)\s*(?:\|([^}]*?))?\s*\}\}/g

/** Read `points.air_kill` / `default_lives.Standard[0]` out of the fact set. */
export function lookupFact(facts: unknown, path: string): unknown {
  let cur: unknown = facts
  for (const rawKey of path.split('.')) {
    // Split "Standard[0][1]" into "Standard", "0", "1".
    const parts = rawKey.split('[').map((p, i) => (i === 0 ? p : p.replace(/\]$/, '')))
    for (const key of parts) {
      if (key === '') continue
      if (cur === null || cur === undefined) return undefined
      if (Array.isArray(cur)) {
        const idx = Number(key)
        if (!Number.isInteger(idx)) return undefined
        cur = cur[idx]
      } else if (typeof cur === 'object') {
        cur = (cur as Record<string, unknown>)[key]
      } else {
        return undefined
      }
    }
  }
  return cur
}

/** Render a looked-up value the way a page wants to read it. */
export function formatFact(value: unknown): string | undefined {
  if (value === null || value === undefined) return undefined
  if (typeof value === 'number') {
    // Whole numbers are the overwhelming case; keep at most two decimals
    // otherwise so a float like 0.35 doesn't print as 0.3500000000000001.
    return Number.isInteger(value) ? String(value) : String(Number(value.toFixed(2)))
  }
  if (typeof value === 'boolean') return value ? 'yes' : 'no'
  if (typeof value === 'string') return value
  if (Array.isArray(value)) return value.map(v => formatFact(v) ?? '').join(', ')
  return undefined
}

/**
 * Substitute every `{{cfg:...}}` in `markdown` against `facts`.
 *
 * Unresolvable tokens fall back to the literal written after `|`, and to the
 * path itself if the page didn't write one — never to an empty string, which
 * would silently turn "worth 350 points" into "worth points".
 */
export function applyCfgTokens(markdown: string, facts: unknown): string {
  if (!markdown.includes('{{')) return markdown
  return markdown.replace(TOKEN, (_match, path: string, fallback?: string) => {
    const resolved = formatFact(lookupFact(facts, path.trim()))
    if (resolved !== undefined && resolved !== '') return resolved
    if (fallback !== undefined) return fallback
    return path.trim()
  })
}

/** True when a page quotes any instance-specific number at all. */
export function hasCfgTokens(markdown: string): boolean {
  TOKEN.lastIndex = 0
  return TOKEN.test(markdown)
}

/**
 * `{{list:...}}` placeholders — the table version of `{{cfg:...}}`.
 *
 * Some of what a page needs to say is a *list*, not a number, and the list is
 * different on every server: the live campaign offers 27 Blue deployables and
 * 13 actions, the Caucasus campaign 16 and 10, with almost no names in common.
 * A hand-written table is stale the moment someone edits a config, so a page
 * writes the path and the reader's selected instance supplies the rows:
 *
 * ```
 * {{list:deployables.Blue|_Deployable list unavailable for this server._}}
 * {{list:actions.Red}}
 * {{list:troops.Blue}}
 * ```
 *
 * The columns are chosen from the first path segment, because each of these
 * has its own useful shape — a deployable is interesting for its crates, an
 * action for its cost and type. Output is GitHub-flavoured Markdown, inserted
 * before react-markdown runs, so it renders exactly like a hand-written table.
 */
const LIST_TOKEN = /\{\{\s*list:([^|}]+?)\s*(?:\|([^}]*?))?\s*\}\}/g

type Row = Record<string, unknown>

const num = (v: unknown): string =>
  typeof v === 'number' ? formatFact(v) ?? '' : v === null || v === undefined ? '' : String(v)

/** Escape a cell so a stray `|` can't break the table it sits in. */
const cell = (v: string): string => v.replace(/\|/g, '\\|').replace(/\n+/g, ' ').trim()

function table(headers: string[], rows: string[][]): string {
  if (rows.length === 0) return ''
  const head = `| ${headers.join(' | ')} |`
  const rule = `|${headers.map(() => '---').join('|')}|`
  const body = rows.map(r => `| ${r.map(cell).join(' | ')} |`).join('\n')
  return `${head}\n${rule}\n${body}`
}

/** A deployable's menu path is the name a player actually sees in F10. */
function deployableRows(list: Row[]): string[][] {
  return list.map(d => {
    const path = Array.isArray(d.path) ? (d.path as unknown[]).map(String) : []
    const name = path.length ? path[path.length - 1] : String(d.name ?? '?')
    const group = path.length > 1 ? path.slice(0, -1).join(' / ') : ''
    const crates = Array.isArray(d.crates) ? (d.crates as Row[]) : []
    const need = crates
      .map(c => `${num(c.required)}x ${String(c.name ?? '')} (${num(c.weight)} kg)`)
      .join(', ')
    const tags = [d.jtac ? 'JTAC' : '', d.ewr ? 'EWR' : '', d.gci ? 'GCI' : '']
      .filter(Boolean)
      .join(', ')
    return [
      `**${name}**`,
      group,
      need || '—',
      num(d.limit) || '—',
      tags || '—',
    ]
  })
}

function troopRows(list: Row[]): string[][] {
  return list.map(t => [
    `**${String(t.name ?? '?')}**`,
    num(t.cost) || '0',
    `${num(t.weight)} kg`,
    t.can_capture ? 'yes' : 'no',
    num(t.limit) || '—',
  ])
}

/** Actions arrive pre-summarised by bfdb (`wiki_summarize_actions`). */
function actionRows(list: Row[]): string[][] {
  return list.map(a => [
    `**${String(a.name ?? '?')}**`,
    num(a.cost) || '0',
    String(a.kind ?? '—'),
    num(a.limit) || '—',
  ])
}

const SHAPES: Record<string, { headers: string[]; rows: (l: Row[]) => string[][] }> = {
  deployables: {
    headers: ['Deployable', 'Menu group', 'Crates required', 'Limit', 'Provides'],
    rows: deployableRows,
  },
  troops: { headers: ['Squad', 'Cost', 'Weight', 'Can capture', 'Limit'], rows: troopRows },
  actions: { headers: ['Action', 'Cost', 'Type', 'Limit'], rows: actionRows },
}

/**
 * Substitute every `{{list:...}}` in `markdown` against `facts`.
 *
 * An unresolvable list falls back to whatever the page wrote after `|`, and to
 * a plain italic note otherwise — never to an empty string, which would leave a
 * heading introducing a table that isn't there.
 */
export function applyListTokens(markdown: string, facts: unknown): string {
  if (!markdown.includes('{{')) return markdown
  return markdown.replace(LIST_TOKEN, (_m, rawPath: string, fallback?: string) => {
    const path = rawPath.trim()
    const shape = SHAPES[path.split('.')[0]]
    const value = lookupFact(facts, path)
    if (shape && Array.isArray(value) && value.length > 0) {
      const rendered = table(shape.headers, shape.rows(value as Row[]))
      if (rendered) return rendered
    }
    if (fallback !== undefined && fallback !== '') return fallback
    return `_No ${path.replace('.', ' ')} are configured on this server._`
  })
}

/** True when a page renders any instance-specific list. */
export function hasListTokens(markdown: string): boolean {
  LIST_TOKEN.lastIndex = 0
  return LIST_TOKEN.test(markdown)
}
