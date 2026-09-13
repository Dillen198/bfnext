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
