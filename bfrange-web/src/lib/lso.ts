/**
 * Landing Signal Officer shorthand, ported from
 * `bfprotocols/src/range/lso.rs`. Keep the tables and the parser in step with
 * the Rust: the engine and bfdb describe a pass with that code, and the site
 * must say exactly the same thing when it re-renders a comment.
 *
 * A call is `ERROR` + `POSITION`, wrapped to say how big it was:
 * `(x)` a little, `x` as written, `_x_` a lot.
 */

export type Magnitude = 'little' | 'normal' | 'lot'

export interface LsoCall {
  raw: string
  error: string
  position?: string
  magnitude: Magnitude
  text: string
}

export interface LsoComment {
  raw: string
  grade: string
  details: string
  calls: LsoCall[]
  wire?: number
}

/** Positions, longest first so `IM` wins over `M`-anything and `AW` over `W`. */
export const POSITIONS: [string, string][] = [
  ['IW', 'in the wires'],
  ['IM', 'in the middle (second third)'],
  ['IC', 'in close (last third)'],
  ['AR', 'at the ramp'],
  ['AW', 'all the way'],
  ['TL', 'to landing'],
  ['BC', 'at the ball call'],
  ['X', 'at the start (first third)'],
]

/** Errors; whole-string matches against what is left after the position. */
export const ERRORS: [string, string][] = [
  ['TMRD', 'too much rate of descent'],
  ['NERD', 'not enough rate of descent'],
  ['NESA', 'not enough straightaway'],
  ['3PTS', 'three-point landing'],
  ['LLWD', 'left wing down a lot'],
  ['LRWD', 'right wing down a lot'],
  ['SLO', 'slow'],
  ['LUL', 'lined up left'],
  ['LUR', 'lined up right'],
  ['LIG', 'long in the groove'],
  ['LNF', 'landed nose first'],
  ['PNU', 'pulled nose up'],
  ['NEP', 'not enough power'],
  ['TMP', 'too much power'],
  ['SRD', 'stopped rate of descent'],
  ['DEC', 'decelerating'],
  ['ACC', 'accelerating'],
  ['LWD', 'left wing down'],
  ['RWD', 'right wing down'],
  ['TWA', 'too wide abeam'],
  ['TCA', 'too close abeam'],
  ['NSU', 'not set up'],
  ['OSCB', 'overshoot coming back'],
  ['AFU', 'all fouled up'],
  ['CB', 'climbing'],
  ['CD', 'come down'],
  ['DL', 'drifted left'],
  ['DR', 'drifted right'],
  ['AA', 'angling approach'],
  ['OS', 'overshoot'],
  ['EG', 'eased gun'],
  ['LO', 'low'],
  ['WU', 'wings up'],
  ['ST', 'settled'],
  ['P', 'power'],
  ['H', 'high'],
  ['F', 'fast'],
]

function lookup(tbl: [string, string][], k: string): string | undefined {
  return tbl.find(([c]) => c === k)?.[1]
}

/** Human name of a grade, e.g. `WO` -> "Waveoff". Mirrors `lso::grade_name`. */
export function gradeName(grade: string): string {
  switch (grade) {
    case '_OK_': return 'Perfect pass'
    case 'OK': return 'Good pass'
    case '(OK)': return 'Fair pass'
    case '--': return 'No grade'
    case 'C': return 'Cut pass'
    case 'B': return 'Bolter'
    case 'WO': return 'Waveoff'
    case 'OWO': return 'Own waveoff'
    case 'WOP': return 'Pattern waveoff'
    case 'WOFD': return 'Waveoff, foul deck'
    case 'NC': return 'No count'
    default: return 'Ungraded'
  }
}

/**
 * Greenie board points (MOOSE/NATOPS). `null` = does not count toward an
 * average. Mirrors `lso::grade_points`.
 */
export function gradePoints(grade: string): number | null {
  switch (grade) {
    case '_OK_': return 5
    case 'OK': return 4
    case '(OK)': return 3
    case 'B': return 2.5
    case '--': return 2
    case 'OWO': return 2
    case 'WO':
    case 'WOP': return 1
    case 'C': return 0
    default: return null
  }
}

/** Mirrors `lso::normalize_grade`. */
export function normalizeGrade(g: string): string {
  const s = g.trim().replace(/:+$/, '').trim()
  switch (s) {
    case '---':
    case '--':
    case '-': return '--'
    case 'CUT': return 'C'
    case 'BOLTER': return 'B'
    case 'WOAF':
    case 'TWO':
    case 'TLU': return 'WO'
    default: return s
  }
}

function splitCall(inner: string): [string, string | undefined] {
  for (const [p] of POSITIONS) {
    if (inner.length > p.length && inner.endsWith(p)) {
      const e = inner.slice(0, inner.length - p.length)
      if (lookup(ERRORS, e) !== undefined) return [e, p]
    }
  }
  return [inner, undefined]
}

function render(error: string, position: string | undefined, mag: Magnitude): string {
  let e = lookup(ERRORS, error) ?? error
  if (mag === 'little') e = `a little ${e}`
  else if (mag === 'lot') e = `VERY ${e}`
  const p = position !== undefined ? lookup(POSITIONS, position) : undefined
  return p !== undefined ? `${e} ${p}` : e
}

/** Parse one call token such as `_HAW_`, `(SLO)AR`, `(DRX)`, `FIM`. */
export function parseCall(tok: string): LsoCall {
  let mag: Magnitude
  let error: string
  let position: string | undefined
  if (tok.length > 2 && tok.startsWith('_') && tok.endsWith('_')) {
    ;[error, position] = splitCall(tok.slice(1, -1))
    mag = 'lot'
  } else if (tok.startsWith('(')) {
    const rest = tok.slice(1)
    const i = rest.indexOf(')')
    mag = 'little'
    if (i >= 0) {
      const inside = rest.slice(0, i)
      const after = rest.slice(i + 1)
      if (after === '') [error, position] = splitCall(inside)
      else { error = inside; position = after }
    } else {
      ;[error, position] = splitCall(rest)
    }
  } else if (tok.startsWith('_')) {
    // `_SLO_AR`: underscores around the error only
    const rest = tok.slice(1)
    const i = rest.indexOf('_')
    mag = 'lot'
    if (i >= 0) {
      error = rest.slice(0, i)
      const after = rest.slice(i + 1)
      position = after === '' ? undefined : after
    } else {
      ;[error, position] = splitCall(rest)
    }
  } else {
    ;[error, position] = splitCall(tok)
    mag = 'normal'
  }
  const call: LsoCall = { raw: tok, error, magnitude: mag, text: render(error, position, mag) }
  if (position !== undefined) call.position = position
  return call
}

function takeWire(s: string): [string, number | undefined] {
  const i = s.toUpperCase().indexOf('WIRE')
  if (i < 0) return [s, undefined]
  const tail = s.slice(i + 4)
  const m = /^[#\s]*(\d*)/.exec(tail)!
  const consumedMatch = /^[#\s\d]*/.exec(tail)!
  const n = m[1] ? Number(m[1]) : NaN
  const wire = n >= 1 && n <= 4 ? n : undefined
  return [s.slice(0, i) + tail.slice(consumedMatch[0].length), wire]
}

/** Parse a DCS LANDING_QUALITY_MARK comment; `null` when there is no `GRADE:`. */
export function parseComment(raw: string): LsoComment | null {
  const gi = raw.toUpperCase().indexOf('GRADE:')
  if (gi < 0) return null
  const after = raw.slice(gi + 6).trimStart()
  let end = after.search(/[\s:]/)
  if (end < 0) end = after.length
  const grade = normalizeGrade(after.slice(0, end))
  const rest = after.slice(end).replace(/^[:\s]+/, '')
  const [noWire, wire] = takeWire(rest)
  const details = noWire.split(/\s+/).filter(t => t !== '' && t !== ':')
  const c: LsoComment = {
    raw,
    grade,
    details: details.join(' '),
    calls: details.map(parseCall),
  }
  if (wire !== undefined) c.wire = wire
  return c
}

/** Plain-English lines for a details string (no grade, no wire). */
export function describe(details: string): string[] {
  return details.split(/\s+/).filter(Boolean).map(t => parseCall(t).text)
}

// ─── greenie board colours ─────────────────────────────────────────────────

export interface GradeStyle {
  /** fill */
  bg: string
  /** text on the fill */
  fg: string
  label: string
}

/**
 * The traditional ready-room greenie board colours. Squares that do not count
 * (foul-deck waveoff, no count, ungraded) are grey.
 */
export function gradeStyle(grade: string): GradeStyle {
  switch (grade) {
    case '_OK_': return { bg: 'var(--g-ok-under)', fg: '#fff', label: 'Perfect (_OK_) · 5' }
    case 'OK': return { bg: 'var(--g-ok)', fg: '#06240f', label: 'OK · 4' }
    case '(OK)': return { bg: 'var(--g-fair)', fg: '#2b2200', label: 'Fair (OK) · 3' }
    case 'B': return { bg: 'var(--g-bolter)', fg: '#fff', label: 'Bolter · 2.5' }
    case '--': return { bg: 'var(--g-nograde)', fg: '#2a1300', label: 'No grade (--) · 2' }
    case 'WO':
    case 'OWO':
    case 'WOP': return { bg: 'var(--g-waveoff)', fg: '#fff', label: 'Waveoff · 1 (OWO 2)' }
    case 'C': return { bg: 'var(--g-cut)', fg: '#fff', label: 'Cut · 0' }
    default: return { bg: 'var(--g-nocount)', fg: '#111', label: 'No count' }
  }
}

/** Legend order for the board. */
export const GRADE_LEGEND: { grade: string; points: string }[] = [
  { grade: '_OK_', points: '5' },
  { grade: 'OK', points: '4' },
  { grade: '(OK)', points: '3' },
  { grade: 'B', points: '2.5' },
  { grade: '--', points: '2' },
  { grade: 'WO', points: '1–2' },
  { grade: 'C', points: '0' },
  { grade: 'NC', points: '–' },
]
