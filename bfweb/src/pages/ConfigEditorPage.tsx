import { useEffect, useMemo, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useNavigate } from 'react-router-dom'
import { Settings2, Search, Save, AlertTriangle, CheckCircle2, FolderCog } from 'lucide-react'
import { api, type JsonSchema } from '../api'
import { useAuth } from '../context/AuthContext'
import PageHeader from '../components/PageHeader'
import { SchemaSection, resolve } from '../components/SchemaForm'
import { CFG_CATEGORIES } from '../config/cfgCategories'

export default function ConfigEditorPage() {
  const { user } = useAuth()
  const navigate = useNavigate()
  const queryClient = useQueryClient()

  const [search, setSearch] = useState('')
  const [activeCat, setActiveCat] = useState(CFG_CATEGORIES[0].key)
  const [draft, setDraft] = useState<Record<string, unknown> | null>(null)
  const [saveState, setSaveState] = useState<'idle' | 'saving' | 'error' | 'saved'>('idle')
  const [saveError, setSaveError] = useState<string | null>(null)

  const { data: schema, isLoading: schemaLoading, error: schemaError } =
    useQuery({ queryKey: ['admin', 'cfg-schema'], queryFn: api.admin.cfgSchema, retry: false })
  const { data: cfg, isLoading: cfgLoading, error: cfgError } =
    useQuery({ queryKey: ['admin', 'cfg'], queryFn: api.admin.cfg, retry: false })

  useEffect(() => {
    if (cfg && !draft) setDraft(cfg)
  }, [cfg, draft])

  const dirty = draft !== null && cfg !== undefined && JSON.stringify(draft) !== JSON.stringify(cfg)

  // Every schema field, bucketed into categories. Anything not listed in
  // CFG_CATEGORIES falls into "Other" automatically — never silently hidden.
  const { categories, fieldsByCat } = useMemo(() => {
    const allKeys = schema?.properties ? Object.keys(schema.properties) : []
    const categorized = new Set(CFG_CATEGORIES.flatMap(c => c.fields))
    const other = allKeys.filter(k => !categorized.has(k)).sort()
    const byCat = new Map<string, string[]>()
    for (const c of CFG_CATEGORIES) {
      byCat.set(c.key, c.fields.filter(f => allKeys.includes(f)))
    }
    if (other.length > 0) byCat.set('other', other)
    const cats = [
      ...CFG_CATEGORIES.filter(c => (byCat.get(c.key)?.length ?? 0) > 0),
      ...(other.length > 0 ? [{ key: 'other', label: 'Other', icon: FolderCog, fields: other }] : []),
    ]
    return { categories: cats, fieldsByCat: byCat }
  }, [schema])

  // Search overrides category navigation — matches across every field.
  const searchResults = useMemo(() => {
    if (!schema?.properties) return null
    const q = search.trim().toLowerCase()
    if (!q) return null
    return Object.entries(schema.properties)
      .filter(([key, propSchema]) => {
        const desc = resolve(propSchema, schema).description ?? ''
        return key.toLowerCase().includes(q) || desc.toLowerCase().includes(q)
      })
      .sort(([a], [b]) => a.localeCompare(b))
  }, [schema, search])

  const visibleKeys = fieldsByCat.get(activeCat) ?? []

  if (!user) { navigate('/login'); return null }
  if (!user.is_admin) { navigate('/'); return null }

  async function handleSave() {
    if (!draft) return
    setSaveState('saving')
    setSaveError(null)
    try {
      await api.admin.cfgSave(draft)
      setSaveState('saved')
      queryClient.setQueryData(['admin', 'cfg'], draft)
      setTimeout(() => setSaveState('idle'), 2500)
    } catch (e) {
      setSaveState('error')
      setSaveError(e instanceof Error ? e.message : String(e))
    }
  }

  const notConfigured = (cfgError instanceof Error && /engine config not configured/i.test(cfgError.message))
    || (schemaError instanceof Error && /engine config not configured/i.test(schemaError.message))

  function renderSection(key: string, propSchema: JsonSchema, root: JsonSchema, defaultOpen: boolean) {
    return (
      <SchemaSection
        key={key}
        title={key}
        schema={propSchema}
        root={root}
        value={draft?.[key]}
        onChange={v => setDraft(d => d ? { ...d, [key]: v } : d)}
        defaultOpen={defaultOpen}
      />
    )
  }

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader
        title="CONFIG EDITOR"
        sub="Campaign engine configuration — restricted access"
        right={
          dirty ? (
            <button
              onClick={handleSave}
              disabled={saveState === 'saving'}
              className="flex items-center gap-2"
              style={{
                fontSize: '0.7rem', color: '#000', background: 'var(--accent)', border: 'none',
                padding: '0.45rem 1rem', borderRadius: 3, cursor: saveState === 'saving' ? 'not-allowed' : 'pointer',
                opacity: saveState === 'saving' ? 0.7 : 1, fontWeight: 600, letterSpacing: '0.04em',
              }}
            >
              <Save size={13} /> {saveState === 'saving' ? 'Saving…' : 'Save Changes'}
            </button>
          ) : saveState === 'saved' ? (
            <span className="flex items-center gap-1.5" style={{ fontSize: '0.68rem', color: 'var(--accent)' }}>
              <CheckCircle2 size={13} /> Saved
            </span>
          ) : null
        }
      />

      {(schemaLoading || cfgLoading) && (
        <div style={{ padding: '2rem', textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.75rem' }}>
          Loading configuration schema…
        </div>
      )}

      {notConfigured && (
        <div style={{ padding: '1rem' }}>
          <div className="vs-card" style={{ padding: '1.5rem', border: '1px solid rgba(245,158,11,0.4)' }}>
            <div className="flex items-center gap-2" style={{ marginBottom: 8 }}>
              <AlertTriangle size={15} style={{ color: '#f59e0b' }} />
              <span style={{ fontSize: '0.8rem', color: 'var(--text)', fontWeight: 600 }}>Config editor not enabled</span>
            </div>
            <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
              bfdb was started without <code style={{ color: 'var(--accent)' }}>--engine-config &lt;path&gt;</code>.
              Point it at the same JSON file bflib loads (e.g. <code style={{ color: 'var(--accent)' }}>ODFv2_CFG</code>) to enable this page.
            </div>
          </div>
        </div>
      )}

      {!notConfigured && (cfgError || schemaError) && (
        <div style={{ padding: '1rem' }}>
          <div className="vs-card" style={{ padding: '1.5rem', border: '1px solid rgba(239,68,68,0.4)' }}>
            <div className="flex items-center gap-2">
              <AlertTriangle size={15} style={{ color: '#ef4444' }} />
              <span style={{ fontSize: '0.75rem', color: 'var(--text)' }}>
                {String((cfgError ?? schemaError) instanceof Error ? (cfgError ?? schemaError as Error).message : cfgError ?? schemaError)}
              </span>
            </div>
          </div>
        </div>
      )}

      {schema && draft && (
        <div className="flex flex-1 overflow-hidden">

          {/* ── Category sidebar ── */}
          <aside style={{ width: 220, flexShrink: 0, borderRight: '1px solid var(--border)', display: 'flex', flexDirection: 'column', background: 'var(--bg-elevated)' }}>
            <div style={{ padding: '0.7rem 0.8rem 0.5rem' }}>
              <div style={{ display: 'flex', alignItems: 'center', gap: 6, background: 'var(--bg-input)', border: '1px solid var(--border)', padding: '5px 8px', borderRadius: 4 }}>
                <Search size={11} style={{ color: 'var(--text-dim)', flexShrink: 0 }} />
                <input
                  value={search}
                  onChange={e => setSearch(e.target.value)}
                  placeholder="Search all settings…"
                  style={{ background: 'none', border: 'none', outline: 'none', color: 'var(--text)', fontSize: '0.68rem', width: '100%' }}
                />
              </div>
            </div>
            <nav style={{ flex: 1, overflowY: 'auto', padding: '0 0.4rem 0.6rem' }}>
              {categories.map(c => {
                const Icon = c.icon
                const count = fieldsByCat.get(c.key)?.length ?? 0
                const active = activeCat === c.key && !searchResults
                return (
                  <button
                    key={c.key}
                    onClick={() => { setActiveCat(c.key); setSearch('') }}
                    style={{
                      width: '100%', display: 'flex', alignItems: 'center', gap: 8, textAlign: 'left',
                      background: active ? 'var(--bg-card)' : 'none', border: 'none',
                      borderLeft: `2px solid ${active ? 'var(--accent)' : 'transparent'}`,
                      color: active ? 'var(--text)' : 'var(--text-muted)',
                      padding: '0.45rem 0.6rem', borderRadius: 3, cursor: 'pointer', marginBottom: 2, fontSize: '0.68rem',
                    }}
                  >
                    <Icon size={13} style={{ color: active ? 'var(--accent)' : 'var(--text-dim)', flexShrink: 0 }} />
                    <span style={{ flex: 1, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{c.label}</span>
                    <span style={{ fontSize: '0.58rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>{count}</span>
                  </button>
                )
              })}
            </nav>
          </aside>

          {/* ── Content ── */}
          <div className="flex-1 overflow-auto p-4" style={{ background: 'var(--bg)' }}>
            <div className="vs-card" style={{ padding: '0.6rem 0.9rem', marginBottom: '0.8rem' }}>
              <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
                <Settings2 size={13} style={{ color: 'var(--accent)', flexShrink: 0 }} />
                <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', lineHeight: 1.5 }}>
                  Changes take effect on the <strong style={{ color: 'var(--text-muted)' }}>next mission/server restart</strong> — bflib
                  only reads this file once at startup. Every field is generated straight from the engine's config schema, so it
                  always matches what bflib actually accepts. Saving validates against that schema before writing to disk, and the
                  previous file is backed up alongside the new one.
                </div>
              </div>
            </div>

            {saveState === 'error' && (
              <div className="vs-card" style={{ padding: '0.8rem 0.9rem', marginBottom: '0.8rem', border: '1px solid rgba(239,68,68,0.4)' }}>
                <div style={{ fontSize: '0.68rem', color: '#ef4444' }}>Save failed: {saveError}</div>
              </div>
            )}

            {searchResults ? (
              <>
                <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginBottom: '0.6rem', letterSpacing: '0.06em' }}>
                  {searchResults.length} result{searchResults.length === 1 ? '' : 's'} for "{search}"
                </div>
                {searchResults.length === 0 && (
                  <div style={{ padding: '1rem', color: 'var(--text-dim)', fontSize: '0.7rem' }}>No settings match "{search}"</div>
                )}
                {searchResults.map(([key, propSchema]) => renderSection(key, propSchema, schema, true))}
              </>
            ) : (
              visibleKeys.map(key => renderSection(key, schema.properties![key], schema, false))
            )}
          </div>
        </div>
      )}
    </div>
  )
}
