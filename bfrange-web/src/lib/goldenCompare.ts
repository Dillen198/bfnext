/**
 * Comparison for the golden fixtures in `bfprotocols/src/range/fixtures/`,
 * the same rule as the Rust side (`fixtures.rs` `same`): floats equal within
 * a relative 1e-9, and a missing key, `undefined` and `null` all the same
 * (Rust writes `None` as `null`, the ports leave the key out). Returns the
 * first difference, or null.
 */
export function goldenDiff(a: unknown, b: unknown, at = ''): string | null {
  const absent = (x: unknown) => x === null || x === undefined
  if (absent(a) && absent(b)) return null
  if (typeof a === 'number' && typeof b === 'number') {
    return Math.abs(a - b) <= 1e-9 * Math.max(Math.abs(a), Math.abs(b), 1) ? null : `${at}: ${a} != ${b}`
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return `${at}: ${a.length} items != ${b.length}`
    for (let i = 0; i < a.length; i++) {
      const d = goldenDiff(a[i], b[i], `${at}[${i}]`)
      if (d) return d
    }
    return null
  }
  if (a && b && typeof a === 'object' && typeof b === 'object' && !Array.isArray(a) && !Array.isArray(b)) {
    const ao = a as Record<string, unknown>, bo = b as Record<string, unknown>
    for (const k of new Set([...Object.keys(ao), ...Object.keys(bo)])) {
      const d = goldenDiff(ao[k], bo[k], `${at}.${k}`)
      if (d) return d
    }
    return null
  }
  return a === b ? null : `${at}: ${JSON.stringify(a)} != ${JSON.stringify(b)}`
}
