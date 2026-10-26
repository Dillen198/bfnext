/**
 * `types.ts` is written by hand (it carries the comments the site's code is
 * read with), but it must say what `bfprotocols/src/range/mod.rs` puts on
 * the wire. `bfprotocols/src/range/fixtures/range.schema.json` is the JSON
 * Schema of those types, generated from the Rust by
 * `bfprotocols/src/range/fixtures.rs` (`wire_schema`); this test reads
 * `types.ts` with the TypeScript compiler and checks every type the two
 * share: the same fields, optional where Rust may leave one out, `| null`
 * only where Rust writes null, the same primitive / enum / referenced type.
 *
 * After changing a wire type in Rust:
 *   BLESS_RANGE_FIXTURES=1 cargo test -p bfprotocols range::fixtures
 * then make `types.ts` agree until this passes.
 */
import fs from 'node:fs'
import { fileURLToPath } from 'node:url'
import ts from 'typescript'
import { describe, expect, it } from 'vitest'
import schemaFile from '../../bfprotocols/src/range/fixtures/range.schema.json'

type Schema = {
  type?: string | string[]
  enum?: unknown[]
  $ref?: string
  items?: Schema | Schema[]
  properties?: Record<string, Schema>
  required?: string[]
  additionalProperties?: Schema | boolean
  anyOf?: Schema[]
  oneOf?: Schema[]
  allOf?: Schema[]
  default?: unknown
}

const DEFS = (schemaFile as unknown as { definitions: Record<string, Schema> }).definitions

/** Rust names that types.ts spells differently. */
const TS_NAME: Record<string, string> = { SectorCfg: 'Sector', LatLon: 'SectorPoint' }
const tsName = (rust: string) => TS_NAME[rust] ?? rust

// ─── types.ts ────────────────────────────────────────────────────────────────

const src = fs.readFileSync(fileURLToPath(new URL('./types.ts', import.meta.url)), 'utf8')
const sf = ts.createSourceFile('types.ts', src, ts.ScriptTarget.Latest, true)
const interfaces = new Map<string, ts.InterfaceDeclaration>()
const aliases = new Map<string, ts.TypeNode>()
for (const st of sf.statements) {
  if (ts.isInterfaceDeclaration(st)) interfaces.set(st.name.text, st)
  else if (ts.isTypeAliasDeclaration(st)) aliases.set(st.name.text, st.type)
}

interface Member { optional: boolean; type: ts.TypeNode | undefined }

/** The fields of an object type in types.ts: an interface (with what it extends), a literal, an intersection. */
function tsMembers(node: ts.TypeNode | ts.InterfaceDeclaration, seen = new Set<string>()): Map<string, Member> | null {
  const out = new Map<string, Member>()
  const addMembers = (ms: ts.NodeArray<ts.TypeElement>) => {
    for (const m of ms) {
      if (ts.isPropertySignature(m) && m.name && (ts.isIdentifier(m.name) || ts.isStringLiteral(m.name))) {
        out.set(m.name.text, { optional: !!m.questionToken, type: m.type })
      }
    }
  }
  if (ts.isInterfaceDeclaration(node)) {
    for (const h of node.heritageClauses ?? []) {
      for (const e of h.types) {
        const base = ts.isIdentifier(e.expression) ? e.expression.text : ''
        const bm = base && !seen.has(base) ? named(base, new Set([...seen, base])) : null
        bm?.forEach((v, k) => out.set(k, v))
      }
    }
    addMembers(node.members)
    return out
  }
  if (ts.isParenthesizedTypeNode(node)) return tsMembers(node.type, seen)
  if (ts.isTypeLiteralNode(node)) {
    addMembers(node.members)
    return out
  }
  if (ts.isIntersectionTypeNode(node)) {
    for (const t of node.types) {
      const m = tsMembers(t, seen)
      if (!m) return null
      m.forEach((v, k) => out.set(k, v))
    }
    return out
  }
  if (ts.isTypeReferenceNode(node) && ts.isIdentifier(node.typeName)) return named(node.typeName.text, seen)
  return null
}

function named(name: string, seen: Set<string>): Map<string, Member> | null {
  const i = interfaces.get(name)
  if (i) return tsMembers(i, seen)
  const a = aliases.get(name)
  return a ? tsMembers(a, seen) : null
}

/** A TS type as a comparable shape. */
type Shape =
  | { k: 'number' | 'string' | 'boolean' | 'null' | 'unknown' }
  | { k: 'array'; of: Shape }
  | { k: 'tuple' }
  | { k: 'map'; of: Shape }
  | { k: 'ref'; name: string }
  | { k: 'strings'; values: string[] }
  | { k: 'union'; of: Shape[] }

function tsShape(t: ts.TypeNode | undefined): Shape {
  if (!t) return { k: 'unknown' }
  switch (t.kind) {
    case ts.SyntaxKind.NumberKeyword: return { k: 'number' }
    case ts.SyntaxKind.StringKeyword: return { k: 'string' }
    case ts.SyntaxKind.BooleanKeyword: return { k: 'boolean' }
    case ts.SyntaxKind.UnknownKeyword:
    case ts.SyntaxKind.AnyKeyword: return { k: 'unknown' }
  }
  if (ts.isParenthesizedTypeNode(t)) return tsShape(t.type)
  if (ts.isLiteralTypeNode(t)) {
    if (t.literal.kind === ts.SyntaxKind.NullKeyword) return { k: 'null' }
    if (ts.isStringLiteral(t.literal)) return { k: 'strings', values: [t.literal.text] }
    if (ts.isNumericLiteral(t.literal)) return { k: 'number' }
    return { k: 'boolean' }
  }
  if (ts.isArrayTypeNode(t)) return { k: 'array', of: tsShape(t.elementType) }
  if (ts.isTupleTypeNode(t)) return { k: 'tuple' }
  if (ts.isTypeLiteralNode(t) && t.members.length === 1 && ts.isIndexSignatureDeclaration(t.members[0])) {
    return { k: 'map', of: tsShape(t.members[0].type) }
  }
  if (ts.isUnionTypeNode(t)) {
    const of = t.types.map(tsShape)
    if (of.every(s => s.k === 'strings')) return { k: 'strings', values: of.flatMap(s => (s as { values: string[] }).values) }
    // `'parked' | 'left' | string`: documented values, any string accepted
    if (of.every(s => s.k === 'strings' || s.k === 'string')) return { k: 'string' }
    return { k: 'union', of }
  }
  if (ts.isTypeReferenceNode(t) && ts.isIdentifier(t.typeName)) {
    const n = t.typeName.text
    const args = t.typeArguments ?? []
    if (n === 'Record' && args.length === 2) return { k: 'map', of: tsShape(args[1]) }
    if (n === 'Array' && args.length === 1) return { k: 'array', of: tsShape(args[0]) }
    if (interfaces.has(n)) return { k: 'ref', name: n }
    const a = aliases.get(n)
    if (a) {
      // an alias of a primitive or of string literals reads as that; an
      // alias of an object type is a reference
      const s = tsShape(a)
      return s.k === 'union' || s.k === 'unknown' ? { k: 'ref', name: n } : DEFS[rustName(n)] ? { k: 'ref', name: n } : s
    }
    return { k: 'ref', name: n }
  }
  return { k: 'unknown' }
}

const rustName = (ts: string) => Object.entries(TS_NAME).find(([, v]) => v === ts)?.[0] ?? ts

// ─── the schema ──────────────────────────────────────────────────────────────

const refName = (r: string) => r.replace('#/definitions/', '')

/** The schema with null taken out, and whether it allowed null. */
function denull(s: Schema): [Schema, boolean] {
  if (Array.isArray(s.type) && s.type.includes('null')) {
    const rest = s.type.filter(x => x !== 'null')
    return [{ ...s, type: rest.length === 1 ? rest[0] : rest }, true]
  }
  if (s.anyOf && s.anyOf.some(x => x.type === 'null')) {
    const rest = s.anyOf.filter(x => x.type !== 'null')
    return [rest.length === 1 ? rest[0] : { anyOf: rest }, true]
  }
  return [s, false]
}

/** Does the TS shape agree with the (non-null) schema? `why` explains a no. */
function agrees(s: Schema, t: Shape): string | null {
  if (t.k === 'unknown') return null
  if (s.$ref) {
    const n = refName(s.$ref)
    const d = DEFS[n]
    if (t.k === 'ref') return t.name === tsName(n) ? null : `${t.name} where Rust has ${n}`
    // a string enum written out inline in TS
    if (d?.enum && t.k === 'strings') return sameSet(d.enum as string[], t.values)
    return `${t.k} where Rust has ${n}`
  }
  if (s.enum) {
    if (t.k === 'strings') return sameSet(s.enum as string[], t.values)
    return t.k === 'string' ? null : `${t.k} where Rust has an enum`
  }
  if (s.anyOf || s.oneOf) return null
  switch (s.type) {
    case 'number':
    case 'integer': return t.k === 'number' ? null : `${t.k} where Rust has a number`
    case 'string': return t.k === 'string' || t.k === 'strings' ? null : `${t.k} where Rust has a string`
    case 'boolean': return t.k === 'boolean' ? null : `${t.k} where Rust has a boolean`
    case 'array': {
      if (Array.isArray(s.items)) return t.k === 'tuple' || t.k === 'array' ? null : `${t.k} where Rust has a tuple`
      if (t.k === 'tuple') return null
      if (t.k !== 'array') return `${t.k} where Rust has an array`
      const why = s.items ? agrees(denull(s.items)[0], t.of) : null
      return why ? `array of ${why}` : null
    }
    case 'object': {
      if (t.k === 'map' && s.additionalProperties && typeof s.additionalProperties === 'object') {
        const why = agrees(denull(s.additionalProperties)[0], t.of)
        return why ? `map of ${why}` : null
      }
      return t.k === 'map' || t.k === 'ref' ? null : `${t.k} where Rust has an object`
    }
  }
  return null
}

function sameSet(rust: string[], tsv: string[]): string | null {
  const a = [...new Set(rust)].sort(), b = [...new Set(tsv)].sort()
  return JSON.stringify(a) === JSON.stringify(b) ? null : `'${b.join("' | '")}' where Rust has '${a.join("' | '")}'`
}

/** The fields of an object schema (following $ref / allOf). */
function schemaProps(s: Schema): { props: Record<string, Schema>; required: Set<string> } | null {
  if (s.$ref) return schemaProps(DEFS[refName(s.$ref)])
  const props: Record<string, Schema> = {}
  const required = new Set<string>()
  let any = false
  for (const part of [s, ...(s.allOf ?? [])]) {
    if (part !== s && part.$ref) {
      const p = schemaProps(part)
      if (!p) return null
      Object.assign(props, p.props)
      p.required.forEach(r => required.add(r))
      any = true
    }
    if (part.properties) {
      Object.assign(props, part.properties)
      ;(part.required ?? []).forEach(r => required.add(r))
      any = true
    }
  }
  return any ? { props, required } : null
}

/** Compare one object: every difference, as text. */
function compareObject(what: string, s: Schema, members: Map<string, Member>): string[] {
  const sp = schemaProps(s)
  if (!sp) return [`${what}: not an object in Rust`]
  const out: string[] = []
  for (const k of Object.keys(sp.props)) if (!members.has(k)) out.push(`${what}.${k}: missing in types.ts`)
  for (const k of members.keys()) if (!(k in sp.props)) out.push(`${what}.${k}: not in Rust`)
  for (const [k, ps] of Object.entries(sp.props)) {
    const m = members.get(k)
    if (!m) continue
    const [inner, nullable] = denull(ps)
    let shape = tsShape(m.type)
    let tsNull = false
    if (shape.k === 'union') {
      tsNull = shape.of.some(x => x.k === 'null')
      const rest = shape.of.filter(x => x.k !== 'null')
      shape = rest.length === 1 ? rest[0] : { k: 'union', of: rest }
      if (shape.k === 'union' && shape.of.every(x => x.k === 'strings')) {
        shape = { k: 'strings', values: shape.of.flatMap(x => (x as { values: string[] }).values) }
      }
    } else if (shape.k === 'null') tsNull = true
    // Rust always writes a field that is required in the schema
    if (sp.required.has(k) && m.optional) out.push(`${what}.${k}: optional in types.ts, always written by Rust`)
    // Rust may leave out / null an Option: TS must allow that
    if (nullable && !m.optional && !tsNull) out.push(`${what}.${k}: Rust writes null or leaves it out; types.ts must say \`| null\` or \`?\``)
    if (tsNull && !nullable) out.push(`${what}.${k}: \`| null\` in types.ts, never null in Rust`)
    const why = agrees(inner, shape)
    if (why) out.push(`${what}.${k}: ${why}`)
  }
  return out
}

/** A tagged union: match variants on `kind` and compare each as an object. */
function compareTagged(name: string, s: Schema, t: ts.TypeNode): string[] {
  const out: string[] = []
  const variants = new Map<string, Schema>()
  for (const v of s.oneOf ?? []) {
    const kind = schemaProps(v)?.props.kind?.enum?.[0]
    if (typeof kind === 'string') variants.set(kind, v)
  }
  const members = new Map<string, Map<string, Member>>()
  for (const u of ts.isUnionTypeNode(t) ? t.types : [t]) {
    const m = tsMembers(u)
    const kt = m?.get('kind')?.type
    const kind = kt && ts.isLiteralTypeNode(kt) && ts.isStringLiteral(kt.literal) ? kt.literal.text : undefined
    if (m && kind) members.set(kind, m)
  }
  for (const k of variants.keys()) if (!members.has(k)) out.push(`${name}: kind '${k}' missing in types.ts`)
  for (const k of members.keys()) if (!variants.has(k)) out.push(`${name}: kind '${k}' not in Rust`)
  for (const [k, v] of variants) {
    const m = members.get(k)
    if (m) out.push(...compareObject(`${name}[${k}]`, v, m))
  }
  return out
}

function compare(rust: string): string[] {
  const s = DEFS[rust]
  const name = tsName(rust)
  if (s.enum) {
    const a = aliases.get(name)
    if (!a) return [`${name}: no type in types.ts`]
    const sh = tsShape(a)
    if (sh.k !== 'strings') return [`${name}: not a string union in types.ts`]
    const why = sameSet(s.enum as string[], sh.values)
    return why ? [`${name}: ${why}`] : []
  }
  if (s.oneOf) {
    const a = aliases.get(name)
    return a ? compareTagged(name, s, a) : [`${name}: no type in types.ts`]
  }
  const i = interfaces.get(name)
  const m = i ? tsMembers(i) : aliases.has(name) ? tsMembers(aliases.get(name)!) : null
  return m ? compareObject(name, s, m) : [`${name}: no type in types.ts`]
}

/**
 * Rust types the site does not read, so types.ts has no copy. Listed so a
 * new wire type is a decision, not an oversight.
 */
const NOT_MIRRORED = new Set(['SpawnRequest'])

describe('types.ts against the Rust wire types', () => {
  it('has the schema the Rust writes', () => {
    expect(Object.keys(DEFS).length).toBeGreaterThan(50)
  })

  for (const rust of Object.keys(DEFS).sort()) {
    if (NOT_MIRRORED.has(rust)) continue
    it(`mirrors ${rust}`, () => {
      expect(compare(rust)).toEqual([])
    })
  }
})
