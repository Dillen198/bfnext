import { useEffect, useMemo, useRef, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { FolderCog, KeyRound, Save, RotateCcw, CheckCircle2, RefreshCw, ChevronDown, ChevronRight } from '@icons'
import {
  mgr, CONFLICT, MANUAL,
  type BotConfigFile, type ConfigCheck, type ConfigCheckAction, type ConfigChange, type ConfigValidation,
} from './tauri'
import { Btn, Dot, Pill } from './ui'
import { input, OK, AMBER, RED, MONO, DIM, fmtWhen } from './style'

/**
 * DCSServerBot's own config files, edited on this box. The dashboard's OPS
 * page can't touch the protected keys (programs, paths, URLs, update source
 * and keys, tokens) because it goes through the bot's web-facing OPS API;
 * this tab edits the files directly, as the box's administrator. The Rust
 * side (botcfg.rs) re-checks everything: conflict, YAML, backup, atomic write.
 */

// both are read by the bot at start-up, so a save wants a bot restart
const RESTART_FILES = ['plugins/fowlengine.yaml', 'services/webservice.yaml']

interface Doc {
  rel: string
  path: string
  sha256: string
  /** the file's own line ending -- a textarea only ever holds '\n' */
  eol: '\r\n' | '\n'
  /** LF-normalised, as last read / saved */
  original: string
}

interface Banner {
  tone: 'ok' | 'warn' | 'bad'
  message: string
  restart?: boolean
  conflict?: boolean
}

interface Confirm {
  title: string
  body?: React.ReactNode
  yes: string
  danger?: boolean
  onYes: () => void | Promise<void>
}

const msg = (e: unknown) => (e instanceof Error ? e.message : String(e))
const toDisk = (text: string, eol: Doc['eol']) => (eol === '\r\n' ? text.replace(/\n/g, '\r\n') : text)
const restartWanted = (rel: string) => RESTART_FILES.some(r => r.toLowerCase() === rel.toLowerCase())

function fmtSize(n: number): string {
  return n < 1024 ? `${n} B` : `${(n / 1024).toFixed(1)} KB`
}

function ChangeView({ change }: { change: ConfigChange }) {
  return (
    <pre style={{ ...MONO, fontSize: '0.66rem', lineHeight: 1.6, margin: '6px 0 0', padding: '6px 8px', overflowX: 'auto',
                  background: 'var(--bg)', border: '1px solid var(--border)', borderRadius: 2 }}>
      <div style={{ color: 'var(--text-dim)' }}>@ line {change.line}</div>
      {change.before.map((l, i) => <div key={`b${i}`} style={{ color: RED }}>- {l}</div>)}
      {change.after.map((l, i) => <div key={`a${i}`} style={{ color: OK }}>+ {l}</div>)}
    </pre>
  )
}

function Checklist({ checks, error, busy, pubFor, onAction, onLoadPub, onCancelPub }: {
  checks?: ConfigCheck[]; error: unknown; busy: boolean; pubFor: string | null
  onAction: (c: ConfigCheck) => void; onLoadPub: (c: ConfigCheck, path: string) => void; onCancelPub: () => void
}) {
  const todo = (checks ?? []).filter(c => c.level === 'warn').length
  const [open, setOpen] = useState<boolean | null>(null)
  const [pubPath, setPubPath] = useState('')
  const shown = open ?? todo > 0
  return (
    <div className="vs-card" style={{ flexShrink: 0 }}>
      <button onClick={() => setOpen(!shown)} className="flex items-center gap-2 px-4" style={{
        width: '100%', padding: '10px 16px', background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text)',
        borderBottom: shown ? '1px solid var(--border)' : 'none',
      }}>
        {shown ? <ChevronDown size={12} /> : <ChevronRight size={12} />}
        <KeyRound size={13} style={{ color: 'var(--accent)' }} />
        <span style={{ ...DIM, fontSize: '0.65rem' }}>Setup checklist</span>
        <span className="ml-auto">
          {error ? <Pill color={RED}>unavailable</Pill>
            : !checks ? <Pill color="var(--text-dim)">checking…</Pill>
            : todo ? <Pill color={AMBER}>{todo} to fix</Pill>
            : <Pill color={OK}>all set</Pill>}
        </span>
      </button>
      {shown && (
        <div style={{ padding: '4px 16px 10px' }}>
          {!!error && <div style={{ fontSize: '0.7rem', color: RED, padding: '6px 0' }}>{msg(error)}</div>}
          {(checks ?? []).map(c => (
            <div key={c.id} style={{ padding: '8px 0', borderBottom: '1px solid var(--border)' }}>
              <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
                <Dot state={c.level === 'ok' ? 'ok' : c.level === 'warn' ? 'warn' : 'off'} />
                <span style={{ fontSize: '0.72rem', fontWeight: 600 }}>{c.title}</span>
                {c.current && <span style={{ ...MONO, fontSize: '0.64rem', color: 'var(--text-dim)' }}>{c.current}</span>}
                {c.action && c.level !== 'ok' && pubFor !== c.id && (
                  <span className="ml-auto">
                    <Btn disabled={busy} onClick={() => onAction(c)}>{c.action.label}</Btn>
                  </span>
                )}
              </div>
              <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', lineHeight: 1.5, marginTop: 3, paddingLeft: 16 }}>
                {c.message}
                {c.fix && c.level !== 'ok' && <div style={{ color: 'var(--text-dim)' }}>Fix: {c.fix}</div>}
              </div>
              {pubFor === c.id && (
                <div className="flex items-center gap-2" style={{ marginTop: 6, paddingLeft: 16, flexWrap: 'wrap' }}>
                  <input style={{ ...input, flex: 1, minWidth: 260, ...MONO, fontSize: '0.68rem' }} value={pubPath}
                         placeholder="%USERPROFILE%\.tauri\fowl-engine.key.pub (leave blank for this)"
                         onChange={e => setPubPath(e.target.value)} />
                  <Btn primary disabled={busy} onClick={() => onLoadPub(c, pubPath)}>Load</Btn>
                  <Btn onClick={onCancelPub}>Cancel</Btn>
                </div>
              )}
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

export default function BotConfig({ onDirtyChange }: { onDirtyChange: (dirty: boolean) => void }) {
  const qc = useQueryClient()
  const list = useQuery({ queryKey: ['mgr', 'botcfg', 'list'], queryFn: mgr.listBotConfigs, retry: false })
  const checks = useQuery({ queryKey: ['mgr', 'botcfg', 'checks'], queryFn: mgr.configChecks, retry: false })

  const [doc, setDoc] = useState<Doc | null>(null)
  const [text, setText] = useState('')
  const [validation, setValidation] = useState<(ConfigValidation & { for: string }) | null>(null)
  const [banner, setBanner] = useState<Banner | null>(null)
  const [confirm, setConfirm] = useState<Confirm | null>(null)
  const [busy, setBusy] = useState<string | null>(null)
  const [pubFor, setPubFor] = useState<string | null>(null)
  const taRef = useRef<HTMLTextAreaElement>(null)
  const gutterRef = useRef<HTMLDivElement>(null)

  const dirty = !!doc && text !== doc.original
  useEffect(() => onDirtyChange(dirty), [dirty, onDirtyChange])

  const refresh = () => qc.invalidateQueries({ queryKey: ['mgr', 'botcfg'] })

  async function load(rel: string, keepBanner = false) {
    setBusy('load')
    try {
      const r = await mgr.readBotConfig(rel)
      const lf = r.text.replace(/\r\n/g, '\n')
      setDoc({ rel: r.rel, path: r.path, sha256: r.sha256, eol: r.text.includes('\r\n') ? '\r\n' : '\n', original: lf })
      setText(lf)
      setValidation(null)
      if (!keepBanner) setBanner(null)
    } catch (e) {
      setBanner({ tone: 'bad', message: msg(e) })
    } finally {
      setBusy(null)
    }
  }

  // open the first file (fowlengine.yaml sorts first) once the list is in
  const firstRel = list.data?.files[0]?.rel
  useEffect(() => {
    if (!doc && firstRel) load(firstRel)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [firstRel])

  // validate as you type (debounced); Save waits for a verdict on this exact text
  useEffect(() => {
    if (!doc) return
    const rel = doc.rel, eol = doc.eol, t = text
    const h = setTimeout(() => {
      mgr.validateBotConfig(rel, toDisk(t, eol))
        .then(v => setValidation({ ...v, for: t }))
        .catch(e => setValidation({ ok: false, errors: [{ line: null, column: null, message: msg(e) }], warnings: [], for: t }))
    }, 350)
    return () => clearTimeout(h)
  }, [text, doc])

  const current = validation && validation.for === text ? validation : null
  const errorLines = useMemo(() => new Set((current?.errors ?? []).map(e => e.line).filter((l): l is number => l != null)), [current])
  const lineCount = useMemo(() => text.split('\n').length, [text])

  function guard(action: () => void, what: string) {
    if (!dirty) return action()
    setConfirm({ title: `Discard your unsaved changes to ${doc?.rel}?`, body: what, yes: 'Discard', danger: true, onYes: action })
  }

  async function save() {
    if (!doc) return
    setBusy('save')
    setBanner(null)
    try {
      const w = await mgr.writeBotConfig(doc.rel, toDisk(text, doc.eol), doc.sha256)
      setDoc({ ...doc, sha256: w.sha256, original: text })
      setBanner({
        tone: 'ok',
        message: `Saved ${doc.rel}.` + (w.backup ? ` The previous version is in ${w.backup}` : ' (no changes on disk)'),
        restart: restartWanted(doc.rel),
      })
      refresh()
    } catch (e) {
      const m = msg(e)
      setBanner({ tone: 'bad', message: m, conflict: m.startsWith(CONFLICT) })
    } finally {
      setBusy(null)
    }
  }

  async function validateNow() {
    if (!doc) return
    setBusy('validate')
    try {
      const v = await mgr.validateBotConfig(doc.rel, toDisk(text, doc.eol))
      setValidation({ ...v, for: text })
      setBanner(v.ok ? { tone: 'ok', message: `${doc.rel} is valid YAML.` } : null)
    } catch (e) {
      setBanner({ tone: 'bad', message: msg(e) })
    } finally {
      setBusy(null)
    }
  }

  async function copyEdits() {
    try {
      await navigator.clipboard.writeText(toDisk(text, doc?.eol ?? '\n'))
    } catch {
      // older WebView / no permission: select it and use the legacy copy
      taRef.current?.focus()
      taRef.current?.select()
      document.execCommand('copy')
    }
    setBanner(b => (b ? { ...b, message: `${b.message}\nYour edits are on the clipboard.` } : b))
  }

  async function restartBot() {
    setBusy('restart')
    try {
      await mgr.agentCommand('restart-bot')
      setBanner({ tone: 'ok', message: 'Asked the FowlEngine service to restart DCSServerBot -- it does within a few seconds.' })
    } catch (e) {
      setBanner({ tone: 'bad', message: msg(e) })
    } finally {
      setBusy(null)
    }
  }

  function gotoLine(line: number, column?: number | null) {
    const ta = taRef.current
    if (!ta) return
    const lines = text.split('\n')
    let start = 0
    for (let i = 0; i < Math.min(line - 1, lines.length); i++) start += lines[i].length + 1
    const end = start + (lines[line - 1]?.length ?? 0)
    ta.focus()
    ta.setSelectionRange(column ? Math.min(start + column - 1, end) : start, end)
    const lh = parseFloat(getComputedStyle(ta).lineHeight) || 18
    ta.scrollTop = Math.max(0, (line - 4) * lh)
  }

  // YAML forbids tabs: Tab indents two spaces instead of leaving the editor
  function onKeyDown(e: React.KeyboardEvent<HTMLTextAreaElement>) {
    if (e.key !== 'Tab' || e.shiftKey || e.ctrlKey || e.altKey) return
    e.preventDefault()
    const ta = e.currentTarget
    const { selectionStart: s, selectionEnd: en } = ta
    setText(text.slice(0, s) + '  ' + text.slice(en))
    requestAnimationFrame(() => ta.setSelectionRange(s + 2, s + 2))
  }

  // ---- checklist actions: always shown as a change, written only on "yes" ----

  async function propose(a: ConfigCheckAction, value: string, note?: string) {
    const p = await mgr.previewConfigValue(a.rel, a.path, value)
    setConfirm({
      title: `${a.label}: ${a.path.join('.')} in ${a.rel}`,
      body: (
        <>
          {note && <div style={{ marginBottom: 4 }}>{note}</div>}
          <div>This line change is written (the old file is backed up first):</div>
          <ChangeView change={p.change} />
        </>
      ),
      yes: 'Write it',
      onYes: async () => {
        setBusy('set')
        try {
          const r = await mgr.setConfigValue(a.rel, a.path, value, p.sha256)
          await load(a.rel)
          setBanner({
            tone: 'ok',
            message: `${a.path.join('.')} set in ${a.rel}.` + (r.written.backup ? ` Previous version: ${r.written.backup}` : ''),
            restart: restartWanted(a.rel),
          })
          refresh()
        } catch (e) {
          setBanner({ tone: 'bad', message: msg(e) })
        } finally {
          setBusy(null)
        }
      },
    })
  }

  async function runAction(c: ConfigCheck, value?: string, note?: string) {
    const a = c.action
    if (!a) return
    if (doc && dirty && doc.rel.toLowerCase() === a.rel.toLowerCase()) {
      setBanner({ tone: 'warn', message: `Save or revert your edits to ${a.rel} first -- this changes the same file.` })
      return
    }
    setBanner(null)
    setBusy('action')
    try {
      if (a.kind === 'public_key' && value === undefined) {
        setPubFor(c.id)
        return
      }
      const v = value ?? (a.kind === 'generate_secret' ? await mgr.generateSecret() : a.value)
      if (v == null) throw new Error('nothing to set')
      await propose(a, v, note ?? (a.kind === 'generate_secret' ? 'A fresh 32-byte random key (shown masked).' : undefined))
    } catch (e) {
      const m = msg(e)
      setBanner({ tone: m.startsWith(MANUAL) ? 'warn' : 'bad', message: m.startsWith(MANUAL) ? `${m}\nOpened ${a.rel} below.` : m })
      if (m.startsWith(MANUAL) && !dirty) load(a.rel, true)
    } finally {
      setBusy(null)
    }
  }

  async function loadPub(c: ConfigCheck, path: string) {
    setBusy('pub')
    try {
      const k = await mgr.readPublicKey(path.trim() || null)
      setPubFor(null)
      await runAction(c, k.key, `Key id ${k.key_id}, read from ${k.path}.`)
    } catch (e) {
      setBanner({ tone: 'bad', message: msg(e) })
    } finally {
      setBusy(null)
    }
  }

  const files = list.data?.files ?? []
  const canSave = dirty && !!current?.ok && !busy

  return (
    <div style={{ flex: 1, minHeight: 0, display: 'flex' }}>
      {/* files */}
      <div style={{ width: 250, flexShrink: 0, borderRight: '1px solid var(--border)', display: 'flex', flexDirection: 'column', minHeight: 0 }}>
        <div className="flex items-center gap-2" style={{ padding: '12px 12px 8px' }}>
          <FolderCog size={13} style={{ color: 'var(--accent)' }} />
          <span style={{ ...DIM, fontSize: '0.62rem' }}>Bot config</span>
          <span className="ml-auto flex gap-1">
            <button title="Refresh" onClick={() => refresh()} style={{ background: 'none', border: 'none', color: 'var(--text-dim)', cursor: 'pointer' }}>
              <RefreshCw size={12} />
            </button>
          </span>
        </div>
        <div style={{ overflow: 'auto', flex: 1, padding: '0 6px 8px' }}>
          {list.error ? (
            <div style={{ fontSize: '0.68rem', color: RED, padding: 8, lineHeight: 1.5 }}>{msg(list.error)}</div>
          ) : !list.data ? (
            <div style={{ ...DIM, padding: 8 }}>Loading…</div>
          ) : files.map((f: BotConfigFile) => {
            const active = doc?.rel === f.rel
            return (
              <button key={f.rel} onClick={() => { if (!active) guard(() => load(f.rel), `Opening ${f.rel} drops them.`) }} style={{
                display: 'block', width: '100%', textAlign: 'left', padding: '6px 8px', marginBottom: 1, borderRadius: 3, cursor: 'pointer',
                background: active ? 'rgba(106,171,31,0.12)' : 'none', border: `1px solid ${active ? 'rgba(106,171,31,0.35)' : 'transparent'}`,
                color: 'var(--text)',
              }}>
                <div className="flex items-center gap-2" style={{ fontSize: '0.7rem' }}>
                  <span style={{ minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{f.label}</span>
                  {active && dirty && <span className="ml-auto"><Dot state="warn" /></span>}
                </div>
                <div style={{ ...MONO, fontSize: '0.6rem', color: 'var(--text-dim)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                  {f.rel}
                </div>
                <div style={{ fontSize: '0.56rem', color: 'var(--text-dim)' }}>{fmtSize(f.size)} · {fmtWhen(f.modified)}</div>
              </button>
            )
          })}
        </div>
        <div style={{ padding: 10, borderTop: '1px solid var(--border)' }}>
          <Btn onClick={() => mgr.openPath('bot-config').catch(e => setBanner({ tone: 'bad', message: msg(e) }))}>Open config folder</Btn>
          {list.data && <div style={{ ...MONO, fontSize: '0.56rem', color: 'var(--text-dim)', marginTop: 6, wordBreak: 'break-all' }}>{list.data.dir}</div>}
        </div>
      </div>

      {/* checklist + editor */}
      <div style={{ flex: 1, minWidth: 0, display: 'flex', flexDirection: 'column', gap: 10, padding: 14, minHeight: 0, overflow: 'auto' }}>
        <Checklist checks={checks.data} error={checks.error} busy={!!busy} pubFor={pubFor}
                   onAction={c => runAction(c)} onLoadPub={loadPub} onCancelPub={() => setPubFor(null)} />

        {confirm && (
          <div style={{ fontSize: '0.7rem', lineHeight: 1.55, padding: '0.6rem 0.8rem', border: `1px solid ${confirm.danger ? RED : AMBER}`,
                        borderRadius: 3, background: 'var(--bg-elevated)', flexShrink: 0 }}>
            <div style={{ fontWeight: 600, marginBottom: 4 }}>{confirm.title}</div>
            {confirm.body && <div style={{ color: 'var(--text-muted)' }}>{confirm.body}</div>}
            <div className="flex gap-2" style={{ marginTop: 8 }}>
              <Btn primary={!confirm.danger} danger={confirm.danger} disabled={!!busy} onClick={() => {
                const c = confirm
                setConfirm(null)
                void c.onYes()
              }}>{confirm.yes}</Btn>
              <Btn onClick={() => setConfirm(null)}>Cancel</Btn>
            </div>
          </div>
        )}

        {banner && (
          <div style={{
            fontSize: '0.7rem', lineHeight: 1.55, padding: '0.5rem 0.7rem', borderRadius: 3, background: 'var(--bg-elevated)', flexShrink: 0,
            whiteSpace: 'pre-wrap', color: banner.tone === 'ok' ? OK : banner.tone === 'warn' ? AMBER : RED,
            border: `1px solid ${banner.tone === 'ok' ? 'var(--border)' : banner.tone === 'warn' ? AMBER : 'rgba(239,68,68,0.4)'}`,
          }}>
            <div className="flex items-start gap-2">
              <span style={{ flex: 1 }}>{banner.message}</span>
              <button onClick={() => setBanner(null)} style={{ background: 'none', border: 'none', color: 'var(--text-dim)', cursor: 'pointer' }}>✕</button>
            </div>
            {banner.restart && (
              <div className="flex items-center gap-2" style={{ marginTop: 6, color: 'var(--text-muted)' }}>
                DCSServerBot reads this file when it starts.
                <Btn disabled={!!busy} onClick={restartBot}><RefreshCw size={11} />Restart DCSServerBot now</Btn>
              </div>
            )}
            {banner.conflict && doc && (
              <div className="flex items-center gap-2" style={{ marginTop: 6 }}>
                <Btn onClick={() => setConfirm({
                  title: `Reload ${doc.rel} from disk?`, body: 'Your edits here are discarded -- copy them first if you need them.',
                  yes: 'Reload', danger: true, onYes: () => load(doc.rel),
                })}><RotateCcw size={11} />Reload</Btn>
                <Btn onClick={copyEdits}>Copy my edits</Btn>
              </div>
            )}
          </div>
        )}

        {doc && (
          <div className="vs-card" style={{ flex: 1, minHeight: 360, display: 'flex', flexDirection: 'column' }}>
            <div className="flex items-center gap-2" style={{ padding: '8px 12px', borderBottom: '1px solid var(--border)', flexWrap: 'wrap' }}>
              <span style={{ ...MONO, fontSize: '0.7rem' }}>{doc.rel}</span>
              {dirty ? <Pill color={AMBER}>unsaved</Pill> : <Pill color="var(--text-dim)">saved</Pill>}
              {doc.eol === '\r\n' && <Pill color="var(--text-dim)">crlf</Pill>}
              {current && (current.ok ? <Pill color={OK}>valid</Pill> : <Pill color={RED}>{current.errors.length} error</Pill>)}
              <span className="ml-auto flex gap-2">
                <Btn disabled={!!busy} onClick={validateNow}><CheckCircle2 size={11} />Validate</Btn>
                <Btn disabled={!dirty || !!busy} onClick={() => setConfirm({
                  title: `Revert ${doc.rel}?`, body: 'Back to the version last read from disk; your edits are dropped.',
                  yes: 'Revert', danger: true, onYes: () => setText(doc.original),
                })}><RotateCcw size={11} />Revert</Btn>
                <Btn primary disabled={!canSave} title={dirty && current && !current.ok ? 'Fix the YAML errors first' : undefined}
                     onClick={save}><Save size={11} />{busy === 'save' ? 'Saving…' : 'Save'}</Btn>
              </span>
            </div>
            <div style={{ flex: 1, minHeight: 0, display: 'flex', position: 'relative' }}>
              {/* the extra bottom padding covers the textarea's horizontal scrollbar at full scroll */}
              <div ref={gutterRef} aria-hidden style={{
                ...MONO, fontSize: '0.7rem', lineHeight: 1.6, padding: '8px 6px 40px 0', textAlign: 'right', width: 48, flexShrink: 0,
                overflow: 'hidden', color: 'var(--text-dim)', borderRight: '1px solid var(--border)', userSelect: 'none', background: 'var(--bg-chrome)',
              }}>
                {Array.from({ length: lineCount }, (_, i) => (
                  <div key={i} style={errorLines.has(i + 1) ? { color: RED, fontWeight: 700 } : undefined}>{i + 1}</div>
                ))}
              </div>
              <textarea
                ref={taRef}
                value={text}
                onChange={e => setText(e.target.value)}
                onKeyDown={onKeyDown}
                onScroll={e => { if (gutterRef.current) gutterRef.current.scrollTop = e.currentTarget.scrollTop }}
                spellCheck={false}
                autoCapitalize="off"
                autoComplete="off"
                wrap="off"
                style={{
                  ...MONO, flex: 1, minWidth: 0, fontSize: '0.7rem', lineHeight: 1.6, padding: '8px 10px', resize: 'none',
                  border: 'none', outline: 'none', background: 'var(--bg-input)', color: 'var(--text)', whiteSpace: 'pre', overflow: 'auto',
                  tabSize: 2,
                }}
              />
            </div>
            {current && (current.errors.length > 0 || current.warnings.length > 0) && (
              <div style={{ borderTop: '1px solid var(--border)', padding: '6px 12px', maxHeight: 130, overflow: 'auto' }}>
                {[...current.errors.map(d => ({ d, bad: true })), ...current.warnings.map(d => ({ d, bad: false }))].map(({ d, bad }, i) => (
                  <div key={i} onClick={() => d.line && gotoLine(d.line, d.column)} style={{
                    fontSize: '0.66rem', lineHeight: 1.6, color: bad ? RED : AMBER, cursor: d.line ? 'pointer' : 'default',
                  }}>
                    <span style={{ ...MONO }}>{d.line ? `line ${d.line}${d.column ? `:${d.column}` : ''}` : bad ? 'error' : 'warning'}</span>
                    {'  '}{d.message}
                  </div>
                ))}
              </div>
            )}
            <div style={{ ...MONO, fontSize: '0.58rem', color: 'var(--text-dim)', padding: '4px 12px', borderTop: '1px solid var(--border)' }}>
              {doc.path}
            </div>
          </div>
        )}
      </div>
    </div>
  )
}
