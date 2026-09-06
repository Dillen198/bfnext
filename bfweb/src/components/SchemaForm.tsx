import { useState } from 'react'
import { Plus, Trash2, ChevronDown, ChevronRight } from 'lucide-react'
import type { JsonSchema } from '../api'

// ── shared styles ─────────────────────────────────────────────────────────

const LABEL: React.CSSProperties = {
  fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.04em',
}
const HINT: React.CSSProperties = {
  fontSize: '0.6rem', color: 'var(--text-dim)', opacity: 0.75, marginTop: 2, lineHeight: 1.4,
}
const INPUT: React.CSSProperties = {
  width: '100%', background: 'var(--bg-card)', border: '1px solid var(--border)',
  color: 'var(--text)', padding: '0.35rem 0.55rem', fontSize: '0.72rem', borderRadius: 3,
  boxSizing: 'border-box', fontFamily: 'var(--font-mono)',
}
const FIELD_ROW: React.CSSProperties = {
  marginBottom: '0.6rem',
}
const BTN: React.CSSProperties = {
  fontSize: '0.62rem', color: 'var(--accent)', background: 'none',
  border: '1px solid var(--accent)', padding: '2px 8px', borderRadius: 3,
  cursor: 'pointer', display: 'inline-flex', alignItems: 'center', gap: 4,
}
const BTN_DANGER: React.CSSProperties = {
  ...BTN, color: '#ef4444', border: '1px solid rgba(239,68,68,0.4)',
}

// ── schema resolution ────────────────────────────────────────────────────

export function resolve(schema: JsonSchema | undefined, root: JsonSchema): JsonSchema {
  if (!schema) return {}
  if (schema.$ref) {
    const name = schema.$ref.replace('#/definitions/', '')
    const def = root.definitions?.[name]
    if (!def) return {}
    // allow a $ref sibling description/default to override the definition's
    return { ...def, description: schema.description ?? def.description, default: 'default' in schema ? schema.default : def.default }
  }
  // schemars wraps a $ref in a single-element allOf when the field also
  // carries sibling keywords (description/default) that draft-07 doesn't
  // allow directly alongside $ref — e.g. any struct-typed field with
  // #[serde(default)]. Unwrap it the same way.
  if (schema.allOf && schema.allOf.length === 1) {
    const inner = resolve(schema.allOf[0], root)
    return { ...inner, description: schema.description ?? inner.description, default: 'default' in schema ? schema.default : inner.default }
  }
  return schema
}

/** Detect Option<T> represented as anyOf: [T, {type: "null"}] */
function nullableInner(schema: JsonSchema, root: JsonSchema): JsonSchema | null {
  if (!schema.anyOf || schema.anyOf.length !== 2) return null
  const nullBranch = schema.anyOf.find(b => resolve(b, root).type === 'null')
  const other = schema.anyOf.find(b => resolve(b, root).type !== 'null')
  if (!nullBranch || !other) return null
  return other
}

function defaultForSchema(schema: JsonSchema, root: JsonSchema): unknown {
  const s = resolve(schema, root)
  if ('default' in s) return s.default
  const nullable = nullableInner(s, root)
  if (nullable) return null
  if (s.oneOf) {
    const first = resolve(s.oneOf[0], root)
    if (first.enum) return first.enum[0]
    if (first.type === 'object' && first.required?.length === 1) {
      const key = first.required[0]
      const payload = first.properties?.[key]
      return { [key]: payload ? defaultForSchema(payload, root) : null }
    }
    return null
  }
  if (s.enum) return s.enum[0]
  if (s.type === 'object' && s.properties) {
    const out: Record<string, unknown> = {}
    for (const key of s.required ?? []) {
      out[key] = defaultForSchema(s.properties[key], root)
    }
    return out
  }
  if (s.type === 'object') return {}
  if (s.type === 'array') return []
  if (s.type === 'boolean') return false
  if (s.type === 'integer' || s.type === 'number') return 0
  if (s.type === 'string') return ''
  return null
}

// ── variant helpers (externally-tagged Rust enums) ──────────────────────

interface VariantInfo { name: string; payload: JsonSchema | null }

function variantsOf(schema: JsonSchema, root: JsonSchema): VariantInfo[] {
  const out: VariantInfo[] = []
  for (const branch of schema.oneOf ?? []) {
    const b = resolve(branch, root)
    if (b.enum) {
      for (const v of b.enum) out.push({ name: String(v), payload: null })
    } else if (b.type === 'object' && b.required?.length === 1) {
      const name = b.required[0]
      out.push({ name, payload: b.properties?.[name] ?? null })
    }
  }
  return out
}

function currentVariantName(value: unknown, variants: VariantInfo[]): string | undefined {
  if (typeof value === 'string') return value
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    const keys = Object.keys(value as object)
    if (keys.length === 1 && variants.some(v => v.name === keys[0])) return keys[0]
  }
  return variants[0]?.name
}

// ── field wrapper ─────────────────────────────────────────────────────────

function FieldLabel({ name, schema }: { name?: string; schema: JsonSchema }) {
  if (!name) return null
  return (
    <div style={{ marginBottom: 3 }}>
      <span style={LABEL}>{name}</span>
      {schema.description && <div style={HINT}>{schema.description}</div>}
    </div>
  )
}

// ── main recursive dispatcher ────────────────────────────────────────────

interface FieldProps {
  schema: JsonSchema
  root: JsonSchema
  value: unknown
  onChange: (v: unknown) => void
  name?: string
  depth: number
}

export default function SchemaField({ schema: raw, root, value, onChange, name, depth }: FieldProps) {
  const schema = resolve(raw, root)

  // Option<T> → nullable toggle wrapping the inner type
  const nullable = nullableInner(schema, root)
  if (nullable) {
    const enabled = value !== null && value !== undefined
    return (
      <div style={FIELD_ROW}>
        <div className="flex items-center gap-2" style={{ marginBottom: enabled ? 4 : 0 }}>
          <input
            type="checkbox"
            checked={enabled}
            onChange={e => onChange(e.target.checked ? defaultForSchema(nullable, root) : null)}
          />
          <span style={LABEL}>{name ?? '(value)'}</span>
        </div>
        {schema.description && <div style={{ ...HINT, marginLeft: 20 }}>{schema.description}</div>}
        {enabled && (
          <div style={{ marginLeft: 20, marginTop: 4 }}>
            <SchemaField schema={nullable} root={root} value={value} onChange={onChange} depth={depth + 1} />
          </div>
        )}
      </div>
    )
  }

  // Rust enum with variants (oneOf)
  if (schema.oneOf) {
    const variants = variantsOf(schema, root)
    const current = currentVariantName(value, variants)
    const variant = variants.find(v => v.name === current)
    return (
      <div style={FIELD_ROW}>
        <FieldLabel name={name} schema={schema} />
        <select
          style={INPUT}
          value={current ?? ''}
          onChange={e => {
            const v = variants.find(x => x.name === e.target.value)
            if (!v) return
            onChange(v.payload ? { [v.name]: defaultForSchema(v.payload, root) } : v.name)
          }}
        >
          {variants.map(v => <option key={v.name} value={v.name}>{v.name}</option>)}
        </select>
        {variant?.payload && (
          <div style={{ marginTop: 6, marginLeft: 10, paddingLeft: 10, borderLeft: '2px solid var(--border)' }}>
            <SchemaField
              schema={variant.payload}
              root={root}
              value={(value as Record<string, unknown>)?.[variant.name]}
              onChange={v => onChange({ [variant.name]: v })}
              depth={depth + 1}
            />
          </div>
        )}
      </div>
    )
  }

  // plain string/number enum
  if (schema.enum) {
    return (
      <div style={FIELD_ROW}>
        <FieldLabel name={name} schema={schema} />
        <select style={INPUT} value={String(value ?? schema.enum[0])} onChange={e => onChange(e.target.value)}>
          {schema.enum.map(v => <option key={String(v)} value={String(v)}>{String(v)}</option>)}
        </select>
      </div>
    )
  }

  // object with fixed named properties
  if (schema.type === 'object' && schema.properties) {
    const obj = (value as Record<string, unknown>) ?? {}
    const entries = Object.entries(schema.properties)
    const body = (
      <div style={{ marginLeft: depth === 0 ? 0 : 10, paddingLeft: depth === 0 ? 0 : 10, borderLeft: depth === 0 ? 'none' : '2px solid var(--border)' }}>
        {entries.map(([key, propSchema]) => (
          <SchemaField
            key={key}
            schema={propSchema}
            root={root}
            value={obj[key]}
            onChange={v => onChange({ ...obj, [key]: v })}
            name={key}
            depth={depth + 1}
          />
        ))}
      </div>
    )
    if (!name) return body
    return (
      <div style={FIELD_ROW}>
        <FieldLabel name={name} schema={schema} />
        {body}
      </div>
    )
  }

  // map (additionalProperties, dynamic string keys — Vehicle/Side/etc.)
  if (schema.type === 'object' && schema.additionalProperties && typeof schema.additionalProperties === 'object') {
    const valueSchema = schema.additionalProperties
    const obj = (value as Record<string, unknown>) ?? {}
    return (
      <MapField
        schema={valueSchema} root={root} obj={obj} name={name} description={schema.description}
        onChange={onChange} depth={depth}
      />
    )
  }
  if (schema.type === 'object') {
    // fully open object (additionalProperties: true / unspecified) — raw JSON fallback
    return <RawJsonField value={value} onChange={onChange} name={name} schema={schema} />
  }

  // array
  if (schema.type === 'array') {
    const itemSchema = schema.items ?? {}
    const arr = Array.isArray(value) ? value : []
    return (
      <div style={FIELD_ROW}>
        <FieldLabel name={name} schema={schema} />
        <div style={{ display: 'flex', flexDirection: 'column', gap: 6 }}>
          {arr.map((item, i) => (
            <div key={i} style={{ display: 'flex', alignItems: 'flex-start', gap: 6 }}>
              <div style={{ flex: 1, minWidth: 0 }}>
                <SchemaField
                  schema={itemSchema} root={root} value={item}
                  onChange={v => onChange(arr.map((x, idx) => idx === i ? v : x))}
                  depth={depth + 1}
                />
              </div>
              <button style={BTN_DANGER} onClick={() => onChange(arr.filter((_, idx) => idx !== i))} title="Remove item">
                <Trash2 size={11} />
              </button>
            </div>
          ))}
        </div>
        <button style={{ ...BTN, marginTop: 6 }} onClick={() => onChange([...arr, defaultForSchema(itemSchema, root)])}>
          <Plus size={11} /> Add
        </button>
      </div>
    )
  }

  if (schema.type === 'boolean') {
    return (
      <div style={{ ...FIELD_ROW, display: 'flex', alignItems: 'center', gap: 8 }}>
        <input type="checkbox" checked={Boolean(value)} onChange={e => onChange(e.target.checked)} />
        <span style={LABEL}>{name}</span>
        {schema.description && <span style={{ ...HINT, marginTop: 0 }}>{schema.description}</span>}
      </div>
    )
  }

  if (schema.type === 'integer' || schema.type === 'number') {
    return (
      <div style={FIELD_ROW}>
        <FieldLabel name={name} schema={schema} />
        <input
          type="number"
          style={INPUT}
          value={typeof value === 'number' ? value : ''}
          min={schema.minimum}
          max={schema.maximum}
          step={schema.type === 'integer' ? 1 : 'any'}
          onChange={e => onChange(e.target.value === '' ? 0 : Number(e.target.value))}
        />
      </div>
    )
  }

  if (schema.type === 'string') {
    return (
      <div style={FIELD_ROW}>
        <FieldLabel name={name} schema={schema} />
        <input
          type="text"
          style={INPUT}
          value={typeof value === 'string' ? value : ''}
          onChange={e => onChange(e.target.value)}
        />
      </div>
    )
  }

  // fallback: raw JSON editor for anything not otherwise handled (allOf, etc.)
  return <RawJsonField value={value} onChange={onChange} name={name} schema={schema} />
}

// ── map field (dynamic-key object) ───────────────────────────────────────

function MapField({ schema, root, obj, name, description, onChange, depth }: {
  schema: JsonSchema; root: JsonSchema; obj: Record<string, unknown>; name?: string
  description?: string; onChange: (v: Record<string, unknown>) => void; depth: number
}) {
  const [newKey, setNewKey] = useState('')
  const keys = Object.keys(obj)
  return (
    <div style={FIELD_ROW}>
      <FieldLabel name={name} schema={{ description }} />
      <div style={{ display: 'flex', flexDirection: 'column', gap: 8 }}>
        {keys.map(k => (
          <div key={k} style={{ border: '1px solid var(--border)', borderRadius: 3, padding: '6px 8px' }}>
            <div className="flex items-center justify-between" style={{ marginBottom: 4 }}>
              <span style={{ ...LABEL, ...{ fontFamily: 'var(--font-mono)', color: 'var(--accent)' } }}>{k}</span>
              <button style={BTN_DANGER} onClick={() => {
                const next = { ...obj }; delete next[k]; onChange(next)
              }} title="Remove entry">
                <Trash2 size={11} />
              </button>
            </div>
            <SchemaField
              schema={schema} root={root} value={obj[k]}
              onChange={v => onChange({ ...obj, [k]: v })}
              depth={depth + 1}
            />
          </div>
        ))}
      </div>
      <div className="flex items-center gap-2" style={{ marginTop: 6 }}>
        <input
          type="text" placeholder="new key…" value={newKey}
          onChange={e => setNewKey(e.target.value)}
          style={{ ...INPUT, width: 160 }}
        />
        <button
          style={BTN}
          disabled={!newKey || newKey in obj}
          onClick={() => {
            onChange({ ...obj, [newKey]: defaultForSchema(schema, root) })
            setNewKey('')
          }}
        >
          <Plus size={11} /> Add
        </button>
      </div>
    </div>
  )
}

// ── raw JSON fallback (safety net for unhandled schema shapes) ─────────

function RawJsonField({ value, onChange, name, schema }: {
  value: unknown; onChange: (v: unknown) => void; name?: string; schema: JsonSchema
}) {
  const [text, setText] = useState(() => JSON.stringify(value ?? null, null, 2))
  const [error, setError] = useState<string | null>(null)
  return (
    <div style={FIELD_ROW}>
      <FieldLabel name={name} schema={schema} />
      <textarea
        style={{ ...INPUT, minHeight: 60, minWidth: 240, maxWidth: '100%', resize: 'both' }}
        value={text}
        onChange={e => {
          setText(e.target.value)
          try { onChange(JSON.parse(e.target.value)); setError(null) }
          catch { setError('invalid JSON') }
        }}
      />
      {error && <div style={{ ...HINT, color: '#ef4444' }}>{error}</div>}
    </div>
  )
}

// ── collapsible top-level section ────────────────────────────────────────

export function SchemaSection({ title, schema, root, value, onChange, defaultOpen = false }: {
  title: string; schema: JsonSchema; root: JsonSchema; value: unknown
  onChange: (v: unknown) => void; defaultOpen?: boolean
}) {
  const [open, setOpen] = useState(defaultOpen)
  const resolved = resolve(schema, root)
  return (
    <div className="vs-card" style={{ marginBottom: '0.6rem' }}>
      <button
        onClick={() => setOpen(o => !o)}
        style={{
          width: '100%', display: 'flex', alignItems: 'center', gap: 8, textAlign: 'left',
          background: 'none', border: 'none', cursor: 'pointer', padding: '0.6rem 0.9rem',
        }}
      >
        {open ? <ChevronDown size={13} style={{ color: 'var(--accent)' }} /> : <ChevronRight size={13} style={{ color: 'var(--text-dim)' }} />}
        <span style={{ fontSize: '0.72rem', color: 'var(--text)', fontWeight: 600 }}>{title}</span>
        {resolved.description && (
          <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', marginLeft: 4, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
            — {resolved.description}
          </span>
        )}
      </button>
      {open && (
        <div style={{ padding: '0 0.9rem 0.9rem 0.9rem', borderTop: '1px solid var(--border)', paddingTop: '0.7rem' }}>
          <SchemaField schema={schema} root={root} value={value} onChange={onChange} depth={0} />
        </div>
      )}
    </div>
  )
}
