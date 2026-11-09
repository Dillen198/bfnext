// Settings (the bot's own config files), Presets (presets.yaml's weather and
// time) and Access (which dashboard role each action needs).
//
// The files are read and saved through the bot plugin (botconfig.py): secrets
// arrive masked and stay as they were, each save is checked against the bot's
// schema and refused if the file changed since it was read, and nothing that
// names a program, a path or a host can be changed from here.

import { useEffect, useMemo, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useSearchParams } from 'react-router-dom'
import {
  api, type BotAccessRow, type BotConfigDoc, type BotConfigFile, type BotRole,
} from '../../api'
import {
  PRESET_FIELDS, getPath, label, presetFieldValue, setPath, setPresetField, topLevelChanges, type PathKey,
} from '../../lib/botconsole'
import { Btn, Card, ConfirmBtn, Empty, Pill, useRunner } from './ui'
import { CELL, DIM, INPUT, MONO, RED, TH, errText } from './style'
import { Check, Config, Lock, Plus, RotateCw, Save, Shield, Trash2, X } from '@icons'

const APPLIES: Record<string, string> = {
  reload: 'the plugin is reloaded when you save',
  launch: 'used at the next mission launch',
  restart: 'read when the bot starts: restart the node (Nodes tab) to apply',
}

// ── a tree editor for any YAML document ─────────────────────────────────────

function Scalar({ value, mask, onChange }: { value: unknown; mask?: string; onChange: (v: unknown) => void }) {
  if (typeof value === 'boolean') {
    return <input type="checkbox" checked={value} onChange={e => onChange(e.target.checked)} />
  }
  if (typeof value === 'number') {
    return <input type="number" value={value} onChange={e => onChange(e.target.value === '' ? 0 : Number(e.target.value))} style={{ ...INPUT, width: 140 }} />
  }
  if (mask && value === mask) {
    return (
      <span className="inline-flex items-center gap-2">
        <input type="password" placeholder="saved -- type to replace" onChange={e => onChange(e.target.value || mask)} style={{ ...INPUT, width: 200 }} />
      </span>
    )
  }
  if (value === null) {
    return <input value="" placeholder="(empty)" onChange={e => onChange(e.target.value)} style={{ ...INPUT, width: 200 }} />
  }
  const text = String(value)
  return text.length > 60 || text.includes('\n')
    ? <textarea value={text} rows={Math.min(6, text.split('\n').length + 1)} onChange={e => onChange(e.target.value)} style={{ ...INPUT, width: '100%', ...MONO }} />
    : <input value={text} onChange={e => onChange(e.target.value)} style={{ ...INPUT, width: Math.max(140, Math.min(420, text.length * 7 + 30)) }} />
}

function NewKey({ onAdd }: { onAdd: (key: string, value: unknown) => void }) {
  const [open, setOpen] = useState(false)
  const [key, setKey] = useState('')
  const [kind, setKind] = useState('text')
  if (!open) return <button type="button" onClick={() => setOpen(true)} style={{ background: 'none', border: 0, color: 'var(--text-dim)', cursor: 'pointer', fontSize: '0.6rem' }}><Plus size={9} /> add</button>
  const add = () => {
    if (!key.trim()) return
    onAdd(key.trim(), kind === 'bool' ? false : kind === 'number' ? 0 : kind === 'map' ? {} : kind === 'list' ? [] : '')
    setKey('')
    setOpen(false)
  }
  return (
    <span className="inline-flex items-center gap-1">
      <input value={key} onChange={e => setKey(e.target.value)} onKeyDown={e => { if (e.key === 'Enter') add() }} placeholder="key" style={{ ...INPUT, width: 120 }} />
      <select value={kind} onChange={e => setKind(e.target.value)} style={INPUT}>
        {['text', 'number', 'bool', 'map', 'list'].map(k => <option key={k}>{k}</option>)}
      </select>
      <Btn onClick={add}>Add</Btn>
      <Btn onClick={() => setOpen(false)}><X size={9} /></Btn>
    </span>
  )
}

function Tree({ value, path, mask, onChange, depth = 0 }: {
  value: unknown; path: PathKey[]; mask?: string; depth?: number
  onChange: (path: PathKey[], v: unknown) => void
}) {
  if (Array.isArray(value)) {
    return (
      <div style={{ paddingLeft: depth ? 14 : 0, borderLeft: depth ? '1px solid var(--border)' : undefined }}>
        {value.map((item, i) => (
          <div key={i} className="flex items-start gap-2" style={{ padding: '2px 0' }}>
            <span style={{ ...MONO, fontSize: '0.6rem', color: 'var(--text-dim)', minWidth: 18 }}>-</span>
            <div style={{ flex: 1 }}><Tree value={item} path={[...path, i]} mask={mask} onChange={onChange} depth={depth + 1} /></div>
            <button type="button" title="Remove" onClick={() => onChange([...path, i], undefined)} style={{ background: 'none', border: 0, color: 'var(--text-dim)', cursor: 'pointer' }}><X size={9} /></button>
          </div>
        ))}
        <button type="button" onClick={() => onChange([...path, value.length], value.length && typeof value[0] === 'object' ? {} : '')}
          style={{ background: 'none', border: 0, color: 'var(--text-dim)', cursor: 'pointer', fontSize: '0.6rem' }}><Plus size={9} /> item</button>
      </div>
    )
  }
  if (value !== null && typeof value === 'object') {
    const entries = Object.entries(value as Record<string, unknown>)
    return (
      <div style={{ paddingLeft: depth ? 14 : 0, borderLeft: depth ? '1px solid var(--border)' : undefined }}>
        {entries.map(([k, v]) => {
          const nested = v !== null && typeof v === 'object'
          return (
            <div key={k} style={{ padding: '3px 0' }}>
              <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
                <span style={{ ...MONO, fontSize: '0.64rem', color: nested ? 'var(--accent)' : 'var(--text-muted)', minWidth: 140 }}>{k}</span>
                {!nested && <Scalar value={v} mask={mask} onChange={x => onChange([...path, k], x)} />}
                {nested && <span style={{ fontSize: '0.58rem', color: 'var(--text-dim)' }}>{Array.isArray(v) ? `${v.length} item${v.length === 1 ? '' : 's'}` : `${Object.keys(v as object).length} key${Object.keys(v as object).length === 1 ? '' : 's'}`}</span>}
                <button type="button" title={`Remove ${k}`} onClick={() => onChange([...path, k], undefined)} style={{ background: 'none', border: 0, color: 'var(--text-dim)', cursor: 'pointer', marginLeft: 'auto' }}><Trash2 size={9} /></button>
              </div>
              {nested && <SubTree value={v} path={[...path, k]} mask={mask} onChange={onChange} depth={depth + 1} />}
            </div>
          )
        })}
        <NewKey onAdd={(k, v) => onChange([...path, k], v)} />
      </div>
    )
  }
  return <Scalar value={value} mask={mask} onChange={v => onChange(path, v)} />
}

/** A nested map or list, folded until opened. */
function SubTree(props: { value: unknown; path: PathKey[]; mask?: string; depth: number; onChange: (path: PathKey[], v: unknown) => void }) {
  const [open, setOpen] = useState(props.depth < 4)
  const toggle = (
    <button type="button" onClick={() => setOpen(o => !o)} style={{ background: 'none', border: 0, color: 'var(--text-dim)', cursor: 'pointer', fontSize: '0.6rem', paddingLeft: 14 }}>
      {open ? 'hide' : 'show…'}
    </button>
  )
  return open ? <>{props.depth >= 4 && toggle}<Tree {...props} /></> : toggle
}

// ── Settings ─────────────────────────────────────────────────────────────────

function FileEditor({ file, onSaved }: { file: BotConfigFile; onSaved: () => void }) {
  const runner = useRunner()
  const q = useQuery<BotConfigDoc>({ queryKey: ['bot', 'config', file.path], queryFn: () => api.bot.config.read(file.path), retry: false })
  const [mode, setMode] = useState<'form' | 'yaml'>('form')
  const [doc, setDoc] = useState<unknown>(null)
  const [text, setText] = useState('')
  const [errors, setErrors] = useState<string[] | null>(null)
  const [saving, setSaving] = useState(false)
  useEffect(() => {
    if (q.data) {
      setDoc(q.data.doc)
      setText(q.data.yaml)
      setErrors(null)
      if (q.data.doc === null) setMode('yaml')
    }
  }, [q.data])
  const data = q.data
  const dirty = data ? (mode === 'yaml' ? text !== data.yaml : JSON.stringify(doc) !== JSON.stringify(data.doc)) : false
  const change = (): { yaml: string } | { set?: Record<string, unknown>; delete?: string[] } =>
    mode === 'yaml' ? { yaml: text } : topLevelChanges(data?.doc, doc)
  const check = async () => {
    if (!data) return
    try {
      const r = await api.bot.config.validate(file.path, data.revision, change())
      setErrors(r.errors)
    } catch (e) {
      setErrors([errText(e)])
    }
  }
  const save = async () => {
    if (!data) return
    setSaving(true)
    try {
      const r = await api.bot.config.save(file.path, data.revision, change())
      const how = !r.changed ? 'nothing changed'
        : r.applies === 'reload' ? (r.reloaded ? 'saved, and the plugin reloaded' : r.reloaded === false ? 'saved, but the plugin did not reload -- see the bot log' : 'saved; it applies when the bot restarts')
        : `saved -- ${APPLIES[r.applies]}`
      runner.say(true, `${file.path}: ${how}${r.backup ? ` (the old one is in ${r.backup})` : ''}`)
      setErrors(null)
      await q.refetch()
      onSaved()
    } catch (e) {
      runner.say(false, `${file.path}: ${errText(e)}`)
    } finally {
      setSaving(false)
    }
  }
  return (
    <Card icon={<Config size={13} style={{ color: 'var(--accent)' }} />} title={file.path}
      right={<>
        <Btn active={mode === 'form'} disabled={data?.doc === null} onClick={() => setMode('form')}>Form</Btn>
        <Btn active={mode === 'yaml'} onClick={() => setMode('yaml')}>YAML</Btn>
        <Btn onClick={() => void q.refetch()} title="Read it again (drops your changes)"><RotateCw size={10} /></Btn>
        {file.schema && <Btn disabled={!dirty} onClick={() => void check()}><Check size={10} />Check</Btn>}
        <Btn disabled={!dirty || saving} onClick={() => void save()}><Save size={10} />{saving ? 'Saving…' : 'Save'}</Btn>
      </>}>
      {q.error && <Empty>{errText(q.error)}</Empty>}
      {data && (
        <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
          <div className="flex items-center gap-2" style={{ fontSize: '0.62rem', color: 'var(--text-dim)', flexWrap: 'wrap' }}>
            <Pill>{data.kind}</Pill>
            {data.schema ? <Pill tone="ok">schema-checked</Pill> : <Pill title="The bot has no schema for it">no schema</Pill>}
            {data.masked > 0 && <Pill title="Shown masked; they stay as they are unless you type a new one">{data.masked} secret{data.masked === 1 ? '' : 's'}</Pill>}
            <span>{APPLIES[data.applies]}</span>
          </div>
          {data.parse_error && <div style={{ fontSize: '0.66rem', color: RED }}>The file doesn't parse as it is ({data.parse_error}) -- fix it as YAML.</div>}
          {errors && errors.length === 0 && <div style={{ fontSize: '0.66rem', color: 'var(--accent)' }}>The bot's schema accepts it.</div>}
          {errors && errors.length > 0 && <div style={{ fontSize: '0.66rem', color: RED, whiteSpace: 'pre-wrap' }}>{errors.join('\n')}</div>}
          {mode === 'form' && doc !== null
            ? <Tree value={doc} path={[]} mask={data.secret_mask} onChange={(p, v) => setDoc((d: unknown) => setPath(d, p, v))} />
            : <textarea value={text} onChange={e => setText(e.target.value)} spellCheck={false}
                style={{ ...INPUT, ...MONO, width: '100%', minHeight: 420, fontSize: '0.66rem', lineHeight: 1.5 }} />}
          <div style={{ ...DIM, textTransform: 'none', letterSpacing: 0 }}>
            Programs, paths, installs and hosts can't be changed from here: edit those on the box.
          </div>
        </div>
      )}
    </Card>
  )
}

export function SettingsTab() {
  const [params, setParams] = useSearchParams()
  const qc = useQueryClient()
  const runner = useRunner()
  const files = useQuery({ queryKey: ['bot', 'config', 'files'], queryFn: api.bot.config.files, retry: false })
  const pick = params.get('file')
  const list = (files.data?.files ?? []).filter(f => f.kind !== 'presets')
  const current = list.find(f => f.path === pick) ?? null
  const select = (path: string) => setParams(p => { const n = new URLSearchParams(p); n.set('file', path); return n }, { replace: true })
  const groups = useMemo(() => {
    const order = ['main', 'nodes', 'servers', 'plugin', 'service']
    const g = new Map<string, BotConfigFile[]>()
    for (const f of list) g.set(f.kind, [...(g.get(f.kind) ?? []), f])
    return [...g.entries()].sort(([a], [b]) => (order.indexOf(a) + 99) % 99 - (order.indexOf(b) + 99) % 99)
  }, [list])
  const toggle = async (f: BotConfigFile) => {
    const name = f.path.replace(/^services\//, '').replace(/\.yaml(\.disabled)?$/, '')
    try {
      await api.bot.config.service(name, f.disabled)
      runner.say(true, `${name} service ${f.disabled ? 'enabled' : 'disabled'} -- restart the node to apply`)
      void qc.invalidateQueries({ queryKey: ['bot', 'config'] })
    } catch (e) {
      runner.say(false, errText(e))
    }
  }
  if (files.error) return <Empty>{errText(files.error)}</Empty>
  return (
    <div style={{ display: 'grid', gridTemplateColumns: 'minmax(200px, 260px) 1fr', gap: 16, alignItems: 'start' }}>
      <div className="vs-card" style={{ padding: '0.5rem 0' }}>
        {groups.map(([kind, fs]) => (
          <div key={kind} style={{ marginBottom: 6 }}>
            <div style={{ ...DIM, padding: '4px 12px' }}>{kind === 'plugin' ? 'plugins' : kind === 'service' ? 'services' : kind}</div>
            {fs.map(f => (
              <div key={f.path} className="flex items-center" style={{ padding: '0 8px 0 12px', background: f.path === pick ? 'rgba(255,255,255,0.05)' : undefined }}>
                <button type="button" onClick={() => select(f.path)} style={{
                  flex: 1, textAlign: 'left', background: 'none', border: 0, cursor: 'pointer', padding: '4px 0', ...MONO,
                  fontSize: '0.64rem', color: f.disabled ? 'var(--text-dim)' : f.path === pick ? 'var(--accent)' : 'var(--text)',
                }}>{f.path.replace(/^(plugins|services)\//, '').replace(/\.disabled$/, '')}</button>
                {f.kind === 'service' && (
                  <button type="button" onClick={() => void toggle(f)} title={f.disabled ? 'Turn the service on' : 'Turn the service off'}
                    style={{ background: 'none', border: 0, cursor: 'pointer', fontSize: '0.56rem', color: f.disabled ? 'var(--text-dim)' : 'var(--accent)' }}>
                    {f.disabled ? 'off' : 'on'}
                  </button>
                )}
              </div>
            ))}
          </div>
        ))}
        {files.isLoading && <div style={{ padding: '0 12px' }}><Empty>Loading…</Empty></div>}
      </div>
      <div>
        {current ? <FileEditor key={current.path} file={current} onSaved={() => void qc.invalidateQueries({ queryKey: ['bot', 'config', 'files'] })} />
          : <Empty>Pick a file. The bot's own: main.yaml, nodes.yaml, servers.yaml, every plugin's and every service's. fowlengine.yaml is on the OPS page.</Empty>}
      </div>
    </div>
  )
}

// ── Presets ──────────────────────────────────────────────────────────────────

function PresetEditor({ path, name, preset, revision, onDone }: {
  path: string; name: string; preset: unknown; revision: string; onDone: () => void
}) {
  const runner = useRunner()
  const [value, setValue] = useState<unknown>(preset)
  const [raw, setRaw] = useState(false)
  const [rawText, setRawText] = useState(JSON.stringify(preset, null, 2))
  const isList = Array.isArray(preset)
  const save = async (del = false) => {
    let v = value
    if (raw) {
      try {
        v = JSON.parse(rawText)
      } catch {
        runner.say(false, `${name}: that is not valid JSON`)
        return
      }
    }
    try {
      const r = await api.bot.config.save(path, revision, del ? { delete: [name] } : { set: { [name]: v } })
      runner.say(true, `${name}: ${del ? 'removed' : r.changed ? 'saved -- used at the next mission launch' : 'nothing changed'}`)
      onDone()
    } catch (e) {
      runner.say(false, `${name}: ${errText(e)}`)
    }
  }
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
      {isList ? (
        <div style={{ fontSize: '0.66rem', color: 'var(--text-muted)' }}>A combination of: <span style={MONO}>{(preset as unknown[]).join(', ')}</span></div>
      ) : !raw && (
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(170px, 1fr))', gap: 8 }}>
          {PRESET_FIELDS.map(f => {
            const v = presetFieldValue(value, f.path)
            return (
              <label key={f.path.join('.')} style={{ display: 'flex', flexDirection: 'column', gap: 3 }}>
                <span style={{ ...DIM, letterSpacing: '0.06em', textTransform: 'none' }}>{f.label}</span>
                {f.kind === 'bool'
                  ? <input type="checkbox" checked={v === true} onChange={e => setValue((p: unknown) => setPresetField(p, f.path, 'bool', e.target.checked))} style={{ alignSelf: 'flex-start' }} />
                  : <input value={String(v)} onChange={e => setValue((p: unknown) => setPresetField(p, f.path, f.kind, e.target.value))} style={INPUT} />}
              </label>
            )
          })}
        </div>
      )}
      {raw && <textarea value={rawText} onChange={e => setRawText(e.target.value)} rows={14} style={{ ...INPUT, ...MONO, width: '100%' }} />}
      {!raw && !isList && (
        <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
          Other keys kept as they are: {Object.keys((value as object) ?? {}).filter(k => !PRESET_FIELDS.some(f => f.path[0] === k)).join(', ') || 'none'}
        </div>
      )}
      <div className="flex items-center gap-2">
        <Btn onClick={() => void save()}><Save size={10} />Save</Btn>
        <Btn active={raw} onClick={() => { setRawText(JSON.stringify(value, null, 2)); setRaw(r => !r) }}>{raw ? 'Fields' : 'All keys'}</Btn>
        <ConfirmBtn label="Remove" icon={<Trash2 size={10} />} confirm={`Remove the ${name} preset?`} onConfirm={() => void save(true)} />
      </div>
    </div>
  )
}

function PresetFile({ file }: { file: BotConfigFile }) {
  const q = useQuery<BotConfigDoc>({ queryKey: ['bot', 'config', file.path], queryFn: () => api.bot.config.read(file.path), retry: false })
  const runner = useRunner()
  const [open, setOpen] = useState<string | null>(null)
  const [newName, setNewName] = useState('')
  const presets = (q.data?.doc && typeof q.data.doc === 'object' && !Array.isArray(q.data.doc)) ? q.data.doc as Record<string, unknown> : {}
  const add = async () => {
    const name = newName.trim()
    if (!name || !q.data) return
    if (name in presets) {
      setOpen(name)
      return
    }
    try {
      await api.bot.config.save(file.path, q.data.revision, { set: { [name]: { start_time: '08:00' } } })
      setNewName('')
      await q.refetch()
      setOpen(name)
    } catch (e) {
      runner.say(false, errText(e))
    }
  }
  return (
    <Card icon={<Config size={13} style={{ color: 'var(--accent)' }} />} title={`${file.path} · ${Object.keys(presets).length}`}
      right={<>
        <input value={newName} onChange={e => setNewName(e.target.value)} onKeyDown={e => { if (e.key === 'Enter') void add() }} placeholder="new preset" style={{ ...INPUT, width: 140 }} />
        <Btn disabled={!newName.trim()} onClick={() => void add()}><Plus size={10} />Add</Btn>
      </>} pad={false}>
      {q.error && <div style={{ padding: '0.75rem 1rem' }}><Empty>{errText(q.error)}</Empty></div>}
      {q.data?.parse_error && <div style={{ padding: '0.75rem 1rem' }}><Empty>It doesn't parse: fix it on the Settings tab ({q.data.parse_error}).</Empty></div>}
      {Object.entries(presets).map(([name, p]) => {
        const isOpen = open === name
        const summary = Array.isArray(p) ? `combines ${p.join(', ')}` :
          [getPath(p, ['start_time']), getPath(p, ['temperature']) !== undefined ? `${String(getPath(p, ['temperature']))}°C` : null,
            typeof getPath(p, ['clouds']) === 'string' ? getPath(p, ['clouds']) : getPath(p, ['clouds', 'preset'])].filter(Boolean).join(' · ')
        return (
          <div key={name} style={{ borderBottom: '1px solid var(--border)' }}>
            <button type="button" onClick={() => setOpen(isOpen ? null : name)} className="flex items-center gap-3"
              style={{ width: '100%', padding: '7px 14px', background: 'none', border: 0, cursor: 'pointer', textAlign: 'left' }}>
              <span style={{ fontSize: '0.7rem', color: 'var(--text)' }}>{name}</span>
              <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', ...MONO }}>{String(summary)}</span>
            </button>
            {isOpen && q.data && (
              <div style={{ padding: '4px 14px 14px' }}>
                <PresetEditor key={`${name}-${q.data.revision}`} path={file.path} name={name} preset={p} revision={q.data.revision}
                  onDone={() => void q.refetch()} />
              </div>
            )}
          </div>
        )
      })}
    </Card>
  )
}

export function PresetsTab() {
  const files = useQuery({ queryKey: ['bot', 'config', 'files'], queryFn: api.bot.config.files, retry: false })
  if (files.error) return <Empty>{errText(files.error)}</Empty>
  const presets = (files.data?.files ?? []).filter(f => f.kind === 'presets')
  if (files.data && !presets.length) return <Empty>The bot has no presets file (config/presets.yaml).</Empty>
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>
      <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)' }}>
        Weather and time presets the scheduler and the mission loader apply when a mission starts. Each save is checked against the bot's preset schema.
      </div>
      {presets.map(f => <PresetFile key={f.path} file={f} />)}
    </div>
  )
}

// ── Access ───────────────────────────────────────────────────────────────────

export function AccessTab() {
  const runner = useRunner()
  const qc = useQueryClient()
  const q = useQuery({ queryKey: ['bot', 'access'], queryFn: api.bot.access, retry: false })
  const [filter, setFilter] = useState('')
  const set = async (row: BotAccessRow, role: BotRole) => {
    try {
      await api.bot.setAccess(row.name, role === row.default ? null : role)
      runner.say(true, `${label(row.name)} now needs the ${role} role`)
      void qc.invalidateQueries({ queryKey: ['bot'] })
    } catch (e) {
      runner.say(false, errText(e))
    }
  }
  if (q.error) return <Empty>{errText(q.error)}</Empty>
  if (!q.data) return <Empty>Loading…</Empty>
  const needle = filter.trim().toLowerCase()
  const rows = q.data.actions.filter(a => !needle || a.name.includes(needle) || a.plugin.includes(needle))
  return (
    <Card icon={<Shield size={13} style={{ color: 'var(--accent)' }} />} title="Who may do what" pad={false}
      right={<input value={filter} onChange={e => setFilter(e.target.value)} placeholder="filter" style={{ ...INPUT, width: 160 }} />}>
      <div style={{ padding: '0.5rem 1rem', fontSize: '0.64rem', color: 'var(--text-dim)' }}>
        The dashboard role each bot action needs. Full admins can do everything; ops and moderation are the dashboard's Discord roles.
        {!q.data.editable && ' Only a full admin changes these.'}
      </div>
      <div style={{ overflowX: 'auto' }}>
        <table style={{ width: '100%', borderCollapse: 'collapse' }}>
          <thead><tr>{['Action', 'Plugin', 'Kind', 'Needs', ''].map(h => <th key={h} style={TH}>{h}</th>)}</tr></thead>
          <tbody>
            {rows.map(a => (
              <tr key={a.name}>
                <td style={{ ...CELL, color: 'var(--text)' }}>{label(a.name)}</td>
                <td style={CELL}>{a.plugin}</td>
                <td style={CELL}>{a.kind}</td>
                <td style={CELL}>
                  {a.locked ? <span className="inline-flex items-center gap-1"><Lock size={10} />admin</span>
                    : (['moderation', 'ops', 'admin'] as const).map(r => (
                      <span key={r} style={{ marginRight: 4 }}>
                        <Btn active={a.role === r} disabled={!q.data.editable} onClick={() => void set(a, r)}>{r}</Btn>
                      </span>
                    ))}
                </td>
                <td style={{ ...CELL, fontSize: '0.6rem' }}>{!a.locked && a.role !== a.default && <Pill tone="warn">default {a.default}</Pill>}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </Card>
  )
}
