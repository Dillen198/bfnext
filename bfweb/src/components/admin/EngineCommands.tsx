import { useState } from 'react'
import { api } from '../../api'
import { useInstance } from '../../context/InstanceContext'
import { Terminal, Play } from '@icons'
import {
  COMMAND_GROUPS, buildCommand, cleanConsoleLine, refusedHere,
  type AdminCommandDef, type CommandArg,
} from '../../lib/adminCommands'

/**
 * The engine's `-admin` commands from the dashboard, for when nobody can get
 * into the game: forms for the common ones, a free-text line for anything
 * else, and each reply as chat would have shown it. Goes to the server picked
 * in the top bar (bfdb /api/admin/command, `?server=`).
 */

interface Entry {
  id: number
  server: string
  command: string
  ok: boolean
  lines: string[]
}

const MONO: React.CSSProperties = { fontFamily: 'var(--font-mono)' }
const LABEL: React.CSSProperties = {
  fontSize: '0.58rem', letterSpacing: '0.12em', textTransform: 'uppercase', color: 'var(--text-dim)',
}
const INPUT: React.CSSProperties = {
  ...MONO, fontSize: '0.7rem', background: 'var(--bg-card)', color: 'var(--text)',
  border: '1px solid var(--border)', borderRadius: 2, padding: '0.35rem 0.5rem', minWidth: 0, width: '100%',
}
const BUTTON: React.CSSProperties = {
  fontSize: '0.66rem', letterSpacing: '0.06em', borderRadius: 2, padding: '0.35rem 0.85rem',
  cursor: 'pointer', background: 'none', whiteSpace: 'nowrap',
}

let nextId = 1

function ArgInput({ arg, value, onChange, onEnter }: {
  arg: CommandArg; value: string; onChange: (v: string) => void; onEnter: () => void
}) {
  const id = `ec-${arg.name}`
  const common = { id, value, style: INPUT }
  return (
    <label htmlFor={id} style={{ display: 'flex', flexDirection: 'column', gap: 4, flex: arg.rest ? '2 1 14rem' : '1 1 8rem', minWidth: 0 }}>
      <span style={LABEL}>{arg.label}{arg.optional ? ' (optional)' : ''}</span>
      {arg.kind === 'side' || arg.kind === 'choice' ? (
        <select {...common} onChange={e => onChange(e.target.value)}>
          <option value="">—</option>
          {(arg.kind === 'side' ? ['blue', 'red'] : arg.options ?? []).map(o => <option key={o} value={o}>{o}</option>)}
        </select>
      ) : (
        <input
          {...common}
          inputMode={arg.kind === 'number' ? 'numeric' : undefined}
          placeholder={arg.placeholder}
          onChange={e => onChange(e.target.value)}
          onKeyDown={e => { if (e.key === 'Enter') onEnter() }}
        />
      )}
    </label>
  )
}

export default function EngineCommands() {
  const { current } = useInstance()
  const [group, setGroup] = useState(0)
  const [def, setDef] = useState<AdminCommandDef>(COMMAND_GROUPS[0].commands[0])
  const [values, setValues] = useState<Record<string, string>>({})
  const [armed, setArmed] = useState(false)
  const [line, setLine] = useState('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [log, setLog] = useState<Entry[]>([])
  const server = current?.label ?? current?.id ?? 'this server'
  const offline = current ? !current.sortie : false

  function pick(d: AdminCommandDef) {
    setDef(d); setValues({}); setArmed(false); setError(null)
  }

  async function send(command: string) {
    setBusy(true); setError(null)
    try {
      const r = await api.admin.command(command)
      setLog(l => [{ id: nextId++, server, command, ok: r.ok, lines: r.lines.length ? r.lines : ['done'] }, ...l].slice(0, 40))
    } catch (e) {
      setLog(l => [{ id: nextId++, server, command, ok: false, lines: [e instanceof Error ? e.message : String(e)] }, ...l].slice(0, 40))
    } finally {
      setBusy(false)
    }
  }

  function runForm() {
    const built = buildCommand(def, values)
    if (!built.ok) { setError(built.error); return }
    if (def.danger && !armed) { setArmed(true); return }
    setArmed(false)
    void send(built.text)
  }

  function runLine() {
    const text = cleanConsoleLine(line)
    if (!text) return
    const refused = refusedHere(text)
    if (refused) { setError(refused); return }
    setLine('')
    void send(text)
  }

  const preview = buildCommand(def, values)

  return (
    <div className="vs-card" aria-labelledby="engine-commands-title">
      <div className="flex items-center gap-2" style={{ padding: '0.6rem 0.9rem', borderBottom: '1px solid var(--border)' }}>
        <Terminal size={13} style={{ color: 'var(--accent)' }} />
        <span id="engine-commands-title" style={{ fontSize: '0.68rem', letterSpacing: '0.14em', textTransform: 'uppercase', color: 'var(--text)' }}>
          Engine Commands
        </span>
        <span style={{ ...MONO, marginLeft: 'auto', fontSize: '0.62rem', color: offline ? 'var(--red)' : 'var(--text-muted)' }}>
          {server}{offline ? ' · not running' : ''}
        </span>
      </div>

      <div style={{ padding: '0.75rem 0.9rem', display: 'flex', flexDirection: 'column', gap: '0.75rem' }}>
        <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>
          The in-game <span style={MONO}>-admin</span> commands, run on the server picked in the top bar without
          joining it. Replies come back here as chat would show them.
        </div>

        <div role="tablist" aria-label="Command groups" className="flex flex-wrap gap-1">
          {COMMAND_GROUPS.map((g, i) => (
            <button
              key={g.label}
              role="tab"
              aria-selected={group === i}
              onClick={() => { setGroup(i); pick(g.commands[0]) }}
              style={{
                ...BUTTON, padding: '0.3rem 0.7rem',
                border: `1px solid ${group === i ? 'var(--accent)' : 'var(--border)'}`,
                color: group === i ? 'var(--accent)' : 'var(--text-dim)',
              }}
            >
              {g.label}
            </button>
          ))}
        </div>

        <div className="flex flex-wrap gap-1">
          {COMMAND_GROUPS[group].commands.map(c => (
            <button
              key={c.label}
              onClick={() => pick(c)}
              aria-pressed={def === c}
              style={{
                ...BUTTON, padding: '0.25rem 0.6rem', fontSize: '0.62rem',
                border: `1px solid ${def === c ? 'var(--text-dim)' : 'transparent'}`,
                background: def === c ? 'var(--bg-elevated)' : 'none',
                color: c.danger ? '#f59e0b' : 'var(--text)',
              }}
            >
              {c.label}
            </button>
          ))}
        </div>

        <div style={{ border: '1px solid var(--border)', borderRadius: 2, padding: '0.65rem 0.75rem', display: 'flex', flexDirection: 'column', gap: '0.6rem' }}>
          <div style={{ fontSize: '0.68rem', color: 'var(--text)' }}>{def.help}</div>
          {def.args.length > 0 && (
            <div className="flex flex-wrap gap-2">
              {def.args.map(a => (
                <ArgInput
                  key={`${def.label}-${a.name}`}
                  arg={a}
                  value={values[a.name] ?? ''}
                  onChange={v => { setValues(s => ({ ...s, [a.name]: v })); setArmed(false); setError(null) }}
                  onEnter={runForm}
                />
              ))}
            </div>
          )}
          <div className="flex flex-wrap items-center gap-2">
            <code style={{ ...MONO, fontSize: '0.66rem', color: 'var(--text-muted)', flex: '1 1 12rem', wordBreak: 'break-all' }}>
              -admin {preview.ok ? preview.text : `${def.word}${def.fixed ? ' ' + def.fixed : ''} …`}
            </code>
            <button
              onClick={runForm}
              disabled={busy}
              style={{
                ...BUTTON, display: 'inline-flex', alignItems: 'center', gap: 6,
                border: `1px solid ${armed ? 'var(--red)' : 'var(--accent)'}`,
                color: armed ? 'var(--red)' : 'var(--accent)', opacity: busy ? 0.5 : 1,
              }}
            >
              <Play size={11} /> {armed ? `Confirm ${def.label.toLowerCase()}` : busy ? 'Running…' : 'Run'}
            </button>
            {armed && (
              <button onClick={() => setArmed(false)} style={{ ...BUTTON, border: '1px solid var(--border)', color: 'var(--text-dim)' }}>
                Cancel
              </button>
            )}
          </div>
        </div>

        <label htmlFor="ec-line" className="flex items-center gap-2">
          <span style={{ ...MONO, fontSize: '0.7rem', color: 'var(--accent)', whiteSpace: 'nowrap' }}>-admin</span>
          <input
            id="ec-line"
            value={line}
            onChange={e => { setLine(e.target.value); setError(null) }}
            onKeyDown={e => { if (e.key === 'Enter') runLine() }}
            placeholder="any command, as typed in chat (Enter to run)"
            aria-label="Admin command"
            style={INPUT}
          />
        </label>

        {error && <div role="alert" style={{ fontSize: '0.66rem', color: 'var(--red)' }}>{error}</div>}

        {log.length > 0 && (
          <div aria-live="polite" style={{ display: 'flex', flexDirection: 'column', gap: 6, maxHeight: 320, overflow: 'auto' }}>
            {log.map(e => (
              <div key={e.id} style={{ borderLeft: `2px solid ${e.ok ? 'var(--accent)' : 'var(--red)'}`, padding: '0.3rem 0.6rem', background: 'rgba(0,0,0,0.2)' }}>
                <div style={{ ...MONO, fontSize: '0.62rem', color: 'var(--text-muted)' }}>
                  {e.server} › -admin {e.command}
                </div>
                {e.lines.map((l, i) => (
                  <div key={i} style={{ ...MONO, fontSize: '0.66rem', color: e.ok ? 'var(--text)' : 'var(--red)', whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}>
                    {l}
                  </div>
                ))}
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  )
}
