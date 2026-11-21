// A streaming reader for Tacview's ACMI 2.x text format, in the browser.
// The TypeScript twin of bfdb/src/replay/acmi.rs:
//
//   FileType=text/acmi/tacview
//   FileVersion=2.2
//   0,ReferenceTime=2011-06-02T05:00:00Z      global properties on object 0
//   #47.13                                     time frame, seconds
//   3000102,T=1.2|0.5|2000|0|5|90,Type=Air+FixedWing,Name=F-16C_50
//   -3000102                                   object removed
//   // comment
//
// Object ids are hex. A value escapes a literal comma as `\,`, and a line
// ending in an odd number of backslashes continues on the next one (the
// backslash becomes a newline). Recordings are hundreds of megabytes, so
// lines are never sliced out of the chunk they arrived in: the sink gets the
// chunk string and offsets, and props are scanned in place.

/**
 * A Tacview object id. Ids of up to 13 hex digits (52 bits) are numbers;
 * longer ones are kept as their lowercase hex string so nothing is rounded.
 */
export type Id = number | string

/** Receives the logical lines of a recording, in order. */
export interface AcmiSink {
  /** `#<seconds>` */
  frame(t: number): void
  /** `-<id>` */
  remove(id: Id): void
  /** `<id>,<props>`; `p` is only valid during the call. Object 0 is global. */
  object(id: Id, p: Props): void
}

const BS = 92 /* \ */, COMMA = 44, EQ = 61, NL = 10, CR = 13, SP = 32, TAB = 9

const POW10: number[] = []
for (let i = 0, p = 1; i <= 22; i++, p *= 10) POW10.push(p)

const NUM_RE = /^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/

/**
 * Parse `s[a..b)` as a decimal number without slicing it; NaN when it is
 * not one (empty, junk, hex, "Infinity"). Up to 15 significant digits takes
 * the exact fast path (an integer over a power of ten is correctly rounded);
 * anything else falls back to parseFloat.
 */
export function parseNum(s: string, a: number, b: number): number {
  while (a < b && (s.charCodeAt(a) === SP || s.charCodeAt(a) === TAB)) a++
  while (b > a && (s.charCodeAt(b - 1) === SP || s.charCodeAt(b - 1) === TAB)) b--
  if (a >= b) return NaN
  let i = a
  let c = s.charCodeAt(i)
  let neg = false
  if (c === 45) { neg = true; i++ } else if (c === 43) i++
  let m = 0, digits = 0, frac = 0, dot = false
  for (; i < b; i++) {
    c = s.charCodeAt(i)
    if (c >= 48 && c <= 57) {
      m = m * 10 + (c - 48)
      digits++
      if (dot) frac++
    } else if (c === 46 && !dot) dot = true
    else break
  }
  if (i === b && digits > 0 && digits <= 15) {
    const v = frac ? m / POW10[frac] : m
    return neg ? -v : v
  }
  const str = s.slice(a, b)
  return NUM_RE.test(str) ? parseFloat(str) : NaN
}

/**
 * Parse `s[a..b)` as a hex object id (surrounding blanks allowed); null when
 * it is not one.
 */
export function parseId(s: string, a: number, b: number): Id | null {
  while (a < b && (s.charCodeAt(a) === SP || s.charCodeAt(a) === TAB)) a++
  while (b > a && (s.charCodeAt(b - 1) === SP || s.charCodeAt(b - 1) === TAB)) b--
  if (a >= b) return null
  if (b - a > 13) {
    for (let i = a; i < b; i++) if (hexVal(s.charCodeAt(i)) < 0) return null
    // strip leading zeros so "0001" and "1" name the same object
    let z = a
    while (z < b - 1 && s.charCodeAt(z) === 48) z++
    if (b - z <= 13) return parseId(s, z, b)
    return s.slice(z, b).toLowerCase()
  }
  let v = 0
  for (let i = a; i < b; i++) {
    const h = hexVal(s.charCodeAt(i))
    if (h < 0) return null
    v = v * 16 + h
  }
  return v
}

function hexVal(c: number): number {
  if (c >= 48 && c <= 57) return c - 48
  if (c >= 97 && c <= 102) return c - 87
  if (c >= 65 && c <= 70) return c - 55
  return -1
}

/** An id as Tacview writes it: lowercase hex. */
export function idHex(id: Id): string {
  return typeof id === 'number' ? id.toString(16) : id
}

/**
 * The comma-separated `Key=Value` list of an object line, scanned in place.
 * One instance is reused for every line: call `next()` until it returns
 * false; after each true, `key` is the property name and the value lies at
 * `src[vs..ve)` (raw, possibly with escapes when `esc`).
 */
export class Props {
  src = ''
  private pos = 0
  private end = 0
  key = ''
  vs = 0
  ve = 0
  esc = false

  reset(src: string, start: number, end: number): this {
    this.src = src
    this.pos = start
    this.end = end
    return this
  }

  next(): boolean {
    const s = this.src, end = this.end
    while (this.pos < end) {
      const start = this.pos
      let i = start, escaped = false, eq = -1
      while (i < end) {
        const c = s.charCodeAt(i)
        if (c === BS) { escaped = true; i += 2; continue }
        if (c === COMMA) break
        if (c === EQ && eq < 0) eq = i
        i++
      }
      if (i > end) i = end
      this.pos = i + 1
      if (eq < 0) continue // no `=`: not a property
      this.key = s.slice(start, eq)
      this.vs = eq + 1
      this.ve = i
      this.esc = escaped
      return true
    }
    return false
  }

  /** The value, unescaped. */
  value(): string {
    const raw = this.src.slice(this.vs, this.ve)
    return this.esc ? unescape(raw) : raw
  }

  /** The value as a number, NaN when it is not one. */
  num(): number {
    if (this.esc) { const v = this.value(); return parseNum(v, 0, v.length) }
    return parseNum(this.src, this.vs, this.ve)
  }

  /** The value as an object id, null when it is not one. */
  id(): Id | null {
    if (this.esc) { const v = this.value(); return parseId(v, 0, v.length) }
    return parseId(this.src, this.vs, this.ve)
  }

  /** All remaining props, materialised (tests, debugging). */
  toArray(): [string, string][] {
    const out: [string, string][] = []
    while (this.next()) out.push([this.key, this.value()])
    return out
  }
}

/** `\x` -> `x` */
export function unescape(s: string): string {
  let out = ''
  let from = 0
  for (let i = s.indexOf('\\'); i >= 0; i = s.indexOf('\\', i + 2)) {
    out += s.slice(from, i)
    if (i + 1 < s.length) out += s[i + 1]
    from = i + 2
  }
  return from < s.length ? out + s.slice(from) : out
}

/**
 * A `T=` value: `lon|lat|alt`, `lon|lat|alt|u|v`, `lon|lat|alt|roll|pitch|yaw`
 * or `lon|lat|alt|roll|pitch|yaw|u|v|heading`. Writes lon, lat, alt, roll,
 * pitch, yaw into `out[0..6)` (NaN for a component left out, which keeps its
 * previous value; roll/pitch/yaw NaN unless the form carries attitude) and
 * returns whether the form carries attitude (6 or 9 components).
 */
export function parseTransform(s: string, a: number, b: number, out: Float64Array): boolean {
  let n = 0
  let start = a
  for (let i = a; i <= b; i++) {
    if (i === b || s.charCodeAt(i) === 124 /* | */) {
      if (n < 6) out[n] = start < i ? parseNum(s, start, i) : NaN
      n++
      start = i + 1
    }
  }
  for (let k = n; k < 6; k++) out[k] = NaN
  const att = n === 6 || n === 9
  if (!att) out[3] = out[4] = out[5] = NaN
  return att
}

/** A line ending in an odd number of backslashes continues on the next one. */
function continues(s: string, a: number, b: number): boolean {
  let n = 0
  while (b - 1 - n >= a && s.charCodeAt(b - 1 - n) === BS) n++
  return (n & 1) === 1
}

/**
 * Feeds text chunks in, calls the sink for each logical line. Checks the
 * two header lines (`FileType=...acmi...` and `FileVersion=`, either order)
 * and throws when the text is not a recording. Malformed lines are skipped.
 */
export class AcmiParser {
  private sink: AcmiSink
  private carry = ''
  /** a continued logical line being joined */
  private joined: string | null = null
  private header = 0
  private sawType = false
  private props = new Props()
  /** physical lines read so far */
  lineno = 0

  constructor(sink: AcmiSink) {
    this.sink = sink
  }

  push(chunk: string): void {
    const s = this.carry ? this.carry + chunk : chunk
    this.carry = ''
    let pos = 0
    for (;;) {
      const nl = s.indexOf('\n', pos)
      if (nl < 0) break
      this.physical(s, pos, nl)
      pos = nl + 1
    }
    if (pos < s.length) this.carry = s.slice(pos)
  }

  /** End of text: the last line need not end in a newline. */
  end(): void {
    if (this.carry) {
      const s = this.carry
      this.carry = ''
      this.physical(s, 0, s.length)
    }
    if (this.joined !== null) {
      const j = this.joined
      this.joined = null
      this.logical(j, 0, j.length)
    }
    if (this.header < 2) throw new Error(this.header === 0 ? 'empty recording' : 'ACMI header is incomplete')
  }

  private physical(s: string, a: number, b: number) {
    this.lineno++
    while (b > a && (s.charCodeAt(b - 1) === CR || s.charCodeAt(b - 1) === NL)) b--
    if (this.joined !== null || continues(s, a, b)) {
      const part = s.slice(a, b)
      if (continues(s, a, b)) {
        this.joined = (this.joined ?? '') + part.slice(0, -1) + '\n'
        return
      }
      const j = this.joined + part
      this.joined = null
      this.logical(j, 0, j.length)
      return
    }
    this.logical(s, a, b)
  }

  private logical(s: string, a: number, b: number) {
    while (a < b && s.charCodeAt(a) === 0xfeff) a++
    if (a >= b) return
    if (this.header < 2) {
      this.headerLine(s.slice(a, b).trim())
      return
    }
    const c = s.charCodeAt(a)
    if (c === 35 /* # */) {
      const t = parseNum(s, a + 1, b)
      if (!Number.isNaN(t)) this.sink.frame(t)
    } else if (c === 45 /* - */) {
      const id = parseId(s, a + 1, b)
      if (id !== null) this.sink.remove(id)
    } else if (c === 47 /* / */) {
      // comment
    } else {
      let comma = s.indexOf(',', a)
      if (comma < 0 || comma > b) comma = b
      const id = parseId(s, a, comma)
      if (id !== null) this.sink.object(id, this.props.reset(s, Math.min(comma + 1, b), b))
    }
  }

  private headerLine(l: string) {
    if (!l) return
    if (l.startsWith('FileType=')) {
      if (!l.includes('acmi')) throw new Error(`not an ACMI recording: ${l.slice(0, 60)}`)
      this.sawType = true
    } else if (!l.startsWith('FileVersion=')) {
      throw new Error(`not an ACMI recording (line ${this.lineno}: ${l.slice(0, 60)})`)
    }
    this.header++
    if (this.header === 2 && !this.sawType) throw new Error('ACMI header has no FileType')
  }
}

/** A materialised logical line (tests, debugging). */
export type Line =
  | { k: 'frame'; t: number }
  | { k: 'remove'; id: Id }
  | { k: 'object'; id: Id; props: [string, string][] }

/** Parse a whole text into materialised lines (tests, debugging). */
export function parseLines(text: string): Line[] {
  const out: Line[] = []
  const p = new AcmiParser({
    frame: t => out.push({ k: 'frame', t }),
    remove: id => out.push({ k: 'remove', id }),
    object: (id, props) => out.push({ k: 'object', id, props: props.toArray() }),
  })
  p.push(text)
  p.end()
  return out
}
