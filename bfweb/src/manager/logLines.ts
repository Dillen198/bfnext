/** Pure helpers for the live LOGS tab (no React, so vitest can reach them). */

export type Level = 'error' | 'warn' | 'info' | 'debug' | null

// bflib / bfrange / bfdb (simplelog): 13:05:25 [ERROR] (20) bflib::db: msg
// DCSServerBot: 2026-10-25 12:00:00,123 ERROR  ...
// dcs.log:      2026-10-25 12:00:00.000 ERROR   SCRIPTING (Main): ...
const LEVEL = /(?:^|[\s[|])(ERROR|ERR|CRITICAL|FATAL|WARNING|WARN|INFO|DEBUG|TRACE)(?:[\]\s:|]|$)/

export function levelOf(line: string): Level {
  const m = LEVEL.exec(line.slice(0, 80))
  if (!m) return /Traceback \(most recent call last\)|^\s+File ".*", line \d+/.test(line) ? 'error' : null
  switch (m[1]) {
    case 'ERROR': case 'ERR': case 'CRITICAL': case 'FATAL': return 'error'
    case 'WARNING': case 'WARN': return 'warn'
    case 'INFO': return 'info'
    default: return 'debug'
  }
}

/** A filter box: plain text matches case-insensitively; `/re/` is a regex;
 *  a leading `!` keeps the lines that do NOT match. A broken regex matches
 *  as plain text. */
export function makeFilter(q: string): (line: string) => boolean {
  let s = q.trim()
  if (!s) return () => true
  const negate = s.startsWith('!')
  if (negate) s = s.slice(1)
  let test: (l: string) => boolean
  const re = /^\/(.+)\/([a-z]*)$/.exec(s)
  if (re) {
    try {
      const r = new RegExp(re[1], re[2].includes('i') ? 'i' : '')
      test = l => r.test(l)
    } catch {
      const t = s.toLowerCase()
      test = l => l.toLowerCase().includes(t)
    }
  } else {
    const t = s.toLowerCase()
    test = l => l.toLowerCase().includes(t)
  }
  return negate ? l => !test(l) : test
}

export const MAX_LINES = 5000

/** Append `incoming` to `buf`, keeping the newest MAX_LINES. */
export function appendCapped(buf: string[], incoming: string[], max = MAX_LINES): string[] {
  if (!incoming.length) return buf
  const out = buf.concat(incoming)
  return out.length > max ? out.slice(out.length - max) : out
}

/** One logs/follow answer (opsapi.py follow_file). */
export interface FollowAnswer {
  which: string
  path: string
  lines: string[]
  offset: number
  ident: string | null
  size: number
  reset: boolean
  reason: 'start' | 'rotated' | 'truncated' | 'skipped' | 'missing' | 'unreadable' | null
  more: boolean
  skipped: number
  missing?: boolean
}

/** What to say in the log when a follow answer starts over. */
export function resetNote(a: FollowAnswer): string | null {
  switch (a.reason) {
    case 'rotated': return '── the log was rotated (a new file started) -- following the new one ──'
    case 'truncated': return '── the log was truncated -- showing its tail again ──'
    case 'skipped': return `── fell behind: skipped ${Math.round(a.skipped / 1024)} KB, showing the tail ──`
    case 'missing': return a.reset ? '── the log file is gone (waiting for it to come back) ──' : null
    default: return null
  }
}
