// The bot console's building blocks: the OPS page's visual vocabulary, the
// runner every control goes through (one notice line for the whole page), a
// generic form for any action from the catalog, and a generic view of any
// action's answer.

import { createContext, useContext, useState, type ReactNode } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api, type BotActionInfo, type BotCatalog, type BotResult } from '../../api'
import {
  buildParams, cellFor, initialValue, inputKind, label, mainTable, type FormValue,
} from '../../lib/botconsole'
import { Alert, CheckCircle2, Play, X } from '@icons'
import { AMBER, CELL, DIM, INPUT, MONO, OK, RED, TH, errText } from './style'
import { OpsTargetContext } from '../../context/OpsTarget'

export function Card({ icon, title, right, children, pad = true }: {
  icon?: ReactNode; title: string; right?: ReactNode; children: ReactNode; pad?: boolean
}) {
  return (
    <div className="vs-card" style={{ display: 'flex', flexDirection: 'column' }}>
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)', flexWrap: 'wrap' }}>
        {icon}
        <span style={{ ...DIM, fontSize: '0.65rem' }}>{title}</span>
        {right && <span className="ml-auto flex items-center gap-2" style={{ flexWrap: 'wrap' }}>{right}</span>}
      </div>
      <div style={pad ? { padding: '0.75rem 1rem' } : undefined}>{children}</div>
    </div>
  )
}

export function Btn({ onClick, children, danger, disabled, title, active }: {
  onClick: () => void; children: ReactNode; danger?: boolean; disabled?: boolean; title?: string; active?: boolean
}) {
  return (
    <button type="button" onClick={onClick} disabled={disabled} title={title} style={{
      display: 'inline-flex', alignItems: 'center', gap: 5, fontSize: '0.62rem', letterSpacing: '0.05em',
      color: active ? '#000' : danger ? RED : 'var(--text)', background: active ? 'var(--accent)' : 'none',
      border: `1px solid ${active ? 'var(--accent)' : danger ? 'rgba(239,68,68,0.4)' : 'var(--border)'}`,
      padding: '0.25rem 0.6rem', borderRadius: 3, cursor: disabled ? 'not-allowed' : 'pointer',
      opacity: disabled ? 0.45 : 1, whiteSpace: 'nowrap',
    }}>{children}</button>
  )
}

/** A link that looks like a Btn (a download, say). */
export function LinkBtn({ href, children, title }: { href: string; children: ReactNode; title?: string }) {
  return (
    <a href={href} title={title} style={{
      display: 'inline-flex', alignItems: 'center', gap: 5, fontSize: '0.62rem', color: 'var(--text)',
      border: '1px solid var(--border)', padding: '0.25rem 0.6rem', borderRadius: 3, textDecoration: 'none',
    }}>{children}</a>
  )
}

/** A button that asks once, inline, before doing something disruptive. */
export function ConfirmBtn({ label: text, icon, confirm, onConfirm, danger = true, disabled, title }: {
  label: string; icon?: ReactNode; confirm: string; onConfirm: () => void; danger?: boolean; disabled?: boolean
  title?: string
}) {
  const [asking, setAsking] = useState(false)
  // Fowl Engine Manager driving more than one box: say which one, loudly
  const target = useContext(OpsTargetContext)
  if (!asking) return <Btn danger={danger} disabled={disabled} title={title} onClick={() => setAsking(true)}>{icon}{text}</Btn>
  return (
    <span style={{
      display: 'inline-flex', alignItems: 'center', gap: 6, flexWrap: 'wrap', padding: '0.2rem 0.45rem',
      background: 'rgba(239,68,68,0.06)', border: '1px solid rgba(239,68,68,0.25)', borderRadius: 3,
    }}>
      {target && (
        <span style={{
          fontFamily: 'var(--font-display)', fontSize: '0.74rem', letterSpacing: '0.08em', color: AMBER,
          border: `1px solid ${AMBER}`, padding: '0 6px', borderRadius: 2,
        }}>ON {target}</span>
      )}
      <span style={{ fontSize: '0.62rem', color: 'var(--text)' }}>{confirm}</span>
      <Btn danger onClick={() => { setAsking(false); onConfirm() }}>Yes</Btn>
      <Btn onClick={() => setAsking(false)}>No</Btn>
    </span>
  )
}

export function Pill({ children, tone = 'dim', title }: {
  children: ReactNode; tone?: 'ok' | 'warn' | 'down' | 'busy' | 'dim'; title?: string
}) {
  const color = tone === 'ok' ? OK : tone === 'warn' ? AMBER : tone === 'down' ? RED : tone === 'busy' ? '#60a5fa' : 'var(--text-dim)'
  return (
    <span title={title} style={{
      display: 'inline-block', fontSize: '0.56rem', letterSpacing: '0.1em', padding: '1px 6px', borderRadius: 2,
      border: `1px solid ${color}`, color, whiteSpace: 'nowrap', ...MONO,
    }}>{children}</span>
  )
}

export function Empty({ children }: { children: ReactNode }) {
  return <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)', padding: '0.5rem 0' }}>{children}</div>
}


// ── the runner ───────────────────────────────────────────────────────────────

export interface Notice { ok: boolean; text: string; at: number }

interface Runner {
  /** Call a write action; the page shows its answer. Resolves to it (null on a transport error). */
  act: (name: string, params?: Record<string, unknown>, what?: string) => Promise<BotResult | null>
  /** The action running now, as `name` or `name:target`. */
  busy: string | null
  notice: Notice | null
  setNotice: (n: Notice | null) => void
  /** Show a line in the page's notice bar (the time is stamped here). */
  say: (ok: boolean, text: string) => void
}

const RunnerContext = createContext<Runner | null>(null)

export function RunnerProvider({ children }: { children: ReactNode }) {
  const qc = useQueryClient()
  const [busy, setBusy] = useState<string | null>(null)
  const [notice, setNotice] = useState<Notice | null>(null)
  const act: Runner['act'] = async (name, params = {}, what) => {
    const key = what ? `${name}:${what}` : name
    setBusy(key)
    try {
      const r = await api.bot.act(name, params)
      setNotice({ ok: r.ok, text: r.message || (r.ok ? `${label(name)}: done` : `${label(name)}: refused`), at: Date.now() })
      // whatever changed, the fleet and every read shows it next
      void qc.invalidateQueries({ queryKey: ['bot'] })
      return r
    } catch (e) {
      setNotice({ ok: false, text: `${label(name)}: ${errText(e)}`, at: Date.now() })
      return null
    } finally {
      setBusy(null)
    }
  }
  const say = (ok: boolean, text: string) => setNotice({ ok, text, at: Date.now() })
  return <RunnerContext.Provider value={{ act, busy, notice, setNotice, say }}>{children}</RunnerContext.Provider>
}

// eslint-disable-next-line react-refresh/only-export-components
export function useRunner(): Runner {
  const r = useContext(RunnerContext)
  if (!r) throw new Error('useRunner outside RunnerProvider')
  return r
}

export function NoticeBar() {
  const { notice, setNotice } = useRunner()
  if (!notice) return null
  const color = notice.ok ? OK : RED
  return (
    <div className="vs-card flex items-start gap-2" style={{ padding: '0.6rem 1rem', borderColor: notice.ok ? 'var(--border)' : 'rgba(239,68,68,0.4)' }}>
      {notice.ok ? <CheckCircle2 size={13} style={{ color, flexShrink: 0, marginTop: 1 }} /> : <Alert size={13} style={{ color, flexShrink: 0, marginTop: 1 }} />}
      <div style={{ fontSize: '0.68rem', color: notice.ok ? 'var(--text)' : RED, lineHeight: 1.5, whiteSpace: 'pre-wrap', flex: 1 }}>
        {notice.text}
        <span style={{ color: 'var(--text-dim)', marginLeft: 8, fontSize: '0.6rem' }}>{new Date(notice.at).toLocaleTimeString()}</span>
      </div>
      <button type="button" onClick={() => setNotice(null)} title="Dismiss" style={{ background: 'none', border: 0, color: 'var(--text-dim)', cursor: 'pointer' }}>
        <X size={12} />
      </button>
    </div>
  )
}

// ── catalog ──────────────────────────────────────────────────────────────────

// eslint-disable-next-line react-refresh/only-export-components
export function useCatalog() {
  return useQuery<BotCatalog>({ queryKey: ['bot', 'catalog'], queryFn: api.bot.catalog, staleTime: 60_000, retry: false })
}

/** The catalog entry for an action, if the bot has it and the caller may use it. */
// eslint-disable-next-line react-refresh/only-export-components
export function useAction(name: string): BotActionInfo | undefined {
  const { data } = useCatalog()
  const a = data?.actions.find(x => x.name === name)
  return a && a.available && a.allowed ? a : undefined
}

/** A read action as a query; disabled when the bot doesn't have it. */
// eslint-disable-next-line react-refresh/only-export-components
export function useRead<D = Record<string, unknown>>(name: string, params: Record<string, unknown>, opts: { enabled?: boolean; refetchInterval?: number } = {}) {
  const has = !!useAction(name)
  return useQuery<BotResult<D>>({
    queryKey: ['bot', 'read', name, params],
    queryFn: () => api.bot.read<D>(name, params),
    enabled: has && opts.enabled !== false,
    refetchInterval: opts.refetchInterval,
    retry: false,
  })
}

// ── a generic form for any action ────────────────────────────────────────────

export function ActionForm({ action, fixed = {}, onResult, compact }: {
  action: BotActionInfo
  /** Parameters the context already decides (server_name, ucid, ...): not shown. */
  fixed?: Record<string, unknown>
  onResult?: (r: BotResult) => void
  compact?: boolean
}) {
  const shown = action.params.filter(p => !(p.name in fixed))
  const [values, setValues] = useState<Record<string, FormValue>>(() =>
    Object.fromEntries(shown.map(p => [p.name, initialValue(p)])))
  const [errors, setErrors] = useState<Record<string, string>>({})
  const [result, setResult] = useState<BotResult | null>(null)
  const [running, setRunning] = useState(false)
  const runner = useRunner()
  const qc = useQueryClient()
  const submit = async () => {
    const built = buildParams(shown, values)
    setErrors(built.errors)
    if (Object.keys(built.errors).length) return
    const params = { ...built.params, ...fixed }
    setRunning(true)
    try {
      let r: BotResult | null
      if (action.kind === 'write') {
        r = await runner.act(action.name, params)
      } else {
        try {
          r = await api.bot.read(action.name, params)
        } catch (e) {
          r = { ok: false, message: errText(e), data: {} }
        }
      }
      if (r) {
        setResult(r)
        onResult?.(r)
      }
      if (action.kind === 'write') void qc.invalidateQueries({ queryKey: ['bot'] })
    } finally {
      setRunning(false)
    }
  }
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 8 }}>
      {!action.allowed && <Empty>Needs the {action.role} role.</Empty>}
      {!action.available && <Empty>Not on this bot: its {action.plugin} plugin is not loaded.</Empty>}
      <div style={{ display: 'grid', gridTemplateColumns: compact ? 'repeat(auto-fill, minmax(150px, 1fr))' : 'repeat(auto-fill, minmax(190px, 1fr))', gap: 8 }}>
        {shown.map(p => {
          const kind = inputKind(p)
          const v = values[p.name]
          return (
            <label key={p.name} style={{ display: 'flex', flexDirection: 'column', gap: 3, gridColumn: kind === 'json' ? '1 / -1' : undefined }}>
              <span style={{ ...DIM, letterSpacing: '0.08em' }} title={p.type}>
                {label(p.name)}{p.required ? ' *' : ''}
                {errors[p.name] && <span style={{ color: RED, textTransform: 'none', marginLeft: 6 }}>{errors[p.name]}</span>}
              </span>
              {kind === 'bool' ? (
                <input type="checkbox" checked={v === true} onChange={e => setValues(s => ({ ...s, [p.name]: e.target.checked }))} style={{ alignSelf: 'flex-start' }} />
              ) : kind === 'json' ? (
                <textarea value={String(v ?? '')} rows={3} onChange={e => setValues(s => ({ ...s, [p.name]: e.target.value }))} style={{ ...INPUT, ...MONO }} placeholder="JSON" />
              ) : (
                <input value={String(v ?? '')} type={kind === 'number' ? 'number' : 'text'}
                  onChange={e => setValues(s => ({ ...s, [p.name]: e.target.value }))} style={INPUT}
                  placeholder={p.type ?? ''} />
              )}
            </label>
          )
        })}
      </div>
      <div className="flex items-center gap-2">
        <Btn onClick={() => void submit()} disabled={running || !action.available || !action.allowed} danger={action.kind === 'write'}>
          <Play size={10} />{running ? 'Running…' : action.kind === 'write' ? 'Run' : 'Read'}
        </Btn>
        {shown.length === 0 && <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>no parameters</span>}
      </div>
      {result && action.kind === 'read' && <ResultView result={result} />}
    </div>
  )
}

// ── a generic view of any answer ─────────────────────────────────────────────

export function Table({ columns, rows, render, actions }: {
  columns: string[]
  rows: Record<string, unknown>[]
  render?: (col: string, value: unknown, row: Record<string, unknown>) => ReactNode
  actions?: (row: Record<string, unknown>) => ReactNode
}) {
  return (
    <div style={{ overflowX: 'auto' }}>
      <table style={{ width: '100%', borderCollapse: 'collapse' }}>
        <thead>
          <tr>
            {columns.map(c => <th key={c} style={TH}>{label(c)}</th>)}
            {actions && <th style={TH} />}
          </tr>
        </thead>
        <tbody>
          {rows.map((r, i) => (
            <tr key={i}>
              {columns.map(c => <td key={c} style={CELL}>{render ? render(c, r[c], r) : cellFor(c, r[c])}</td>)}
              {actions && <td style={{ ...CELL, whiteSpace: 'nowrap' }}>{actions(r)}</td>}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

function Scalars({ data }: { data: Record<string, unknown> }) {
  const entries = Object.entries(data).filter(([, v]) => v === null || typeof v !== 'object' || (Array.isArray(v) && v.every(x => typeof x !== 'object')))
  if (!entries.length) return null
  return (
    <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(170px, 1fr))', gap: '6px 14px' }}>
      {entries.map(([k, v]) => (
        <div key={k}>
          <div style={{ ...DIM, letterSpacing: '0.08em' }}>{label(k)}</div>
          <div style={{ fontSize: '0.7rem', color: 'var(--text)', ...MONO, wordBreak: 'break-word' }}>{cellFor(k, v)}</div>
        </div>
      ))}
    </div>
  )
}

export function ResultView({ result }: { result: BotResult }) {
  const [raw, setRaw] = useState(false)
  const t = mainTable(result.data)
  const nested = Object.entries(result.data).filter(([k, v]) =>
    k !== t?.key && v !== null && typeof v === 'object' && !Array.isArray(v))
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 10, borderTop: '1px solid var(--border)', paddingTop: 10 }}>
      {result.message && <div style={{ fontSize: '0.68rem', color: result.ok ? 'var(--text)' : RED }}>{result.message}</div>}
      <Scalars data={result.data} />
      {nested.map(([k, v]) => {
        const sub = mainTable(v)
        return (
          <div key={k}>
            <div style={{ ...DIM, marginBottom: 4 }}>{label(k)}</div>
            {sub ? <Table columns={sub.columns} rows={sub.rows} /> : <Scalars data={v as Record<string, unknown>} />}
          </div>
        )
      })}
      {t && (
        <div>
          <div style={{ ...DIM, marginBottom: 4 }}>{label(t.key)} · {t.rows.length}</div>
          <Table columns={t.columns} rows={t.rows} />
        </div>
      )}
      <div>
        <Btn onClick={() => setRaw(r => !r)}>{raw ? 'Hide' : 'Show'} raw answer</Btn>
        {raw && (
          <pre style={{ margin: '6px 0 0', maxHeight: 360, overflow: 'auto', fontSize: '0.6rem', ...MONO, color: 'var(--text-muted)', whiteSpace: 'pre-wrap' }}>
            {JSON.stringify(result, null, 2)}
          </pre>
        )}
      </div>
    </div>
  )
}
