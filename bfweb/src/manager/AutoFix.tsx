/**
 * AUTO-FIX: the auto-fix runner on this PC (bfmanager autofix.rs →
 * deploy/autofix/autofix.py). It reads every box's issue report and /fe_bug
 * reports, has Claude Code fix them, and publishes the fix by itself in
 * "auto" mode when it passes the gates; anything else waits here for a click.
 */
import { useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Play, Pause, Square, Rocket, Save, RefreshCw, Alert, CheckCircle2, Terminal, Config, Link } from '@icons'
import {
  mgr, opsCall, type AutofixState, type AutofixRun, type AutofixRunnerSettings, type AutofixManagerSettings,
  type BugReport,
} from './tauri'
import { Card, Btn, Dot, Field, Note, Pill, Result, Row } from './ui'
import { useAction, input, MONO, DIM, OK, AMBER, RED, fmtDur, fmtWhen } from './style'

const SOURCES: { key: string; label: string }[] = [
  { key: 'engine', label: 'Engine logs (bflib, bfrange)' },
  { key: 'bot', label: 'Bot plugin' },
  { key: 'bfdb', label: 'bfdb' },
  { key: 'discord', label: 'Discord /fe_bug reports' },
  { key: 'dcs', label: "DCS's own log (mostly DCS bugs)" },
]

const OUTCOME: Record<string, { label: string; color: string }> = {
  fixed: { label: 'fixed', color: OK },
  no_fix: { label: 'not ours', color: 'var(--text-dim)' },
  error: { label: 'error', color: RED },
  rejected: { label: 'rejected', color: RED },
  failed_checks: { label: 'checks failed', color: AMBER },
}

function compareUrl(s: AutofixState, branch: string): string {
  const repo = s.settings.github_repo ?? 'Dillen198/bfnext'
  return `https://github.com/${repo}/compare/${s.settings.base_branch ?? 'vector'}...${branch}`
}

function CopyLink({ url }: { url: string }) {
  const [copied, setCopied] = useState(false)
  return (
    <span className="inline-flex items-center gap-1" style={{ fontSize: '0.6rem', ...MONO }}>
      <Link size={10} />
      <span style={{ userSelect: 'all', color: 'var(--text-muted)' }}>{url}</span>
      <button style={{ color: 'var(--accent)', cursor: 'pointer', background: 'none', border: 'none', fontSize: '0.6rem' }}
              onClick={() => { void navigator.clipboard?.writeText(url); setCopied(true); setTimeout(() => setCopied(false), 1500) }}>
        {copied ? 'copied' : 'copy'}
      </button>
    </span>
  )
}

/** The open /fe_bug reports from players without a trusted role, on every watched box. */
function usePlayerReports(s: AutofixState | undefined) {
  const boxes = (s?.boxes ?? []).filter(b => b.watched && !b.problem)
  return useQuery({
    queryKey: ['mgr', 'autofix-bugs', boxes.map(b => b.id).join(',')],
    enabled: boxes.length > 0 && (s?.settings.sources ?? ['discord']).includes('discord'),
    refetchInterval: 30_000,
    queryFn: async () => {
      const out: (BugReport & { box: string })[] = []
      const errors: string[] = []
      for (const b of boxes) {
        try {
          const r = await opsCall<{ bugs: BugReport[] }>(b.id, b.name, 'GET', 'bugs')
          out.push(...r.bugs.map(x => ({ ...x, box: b.name })))
        } catch (e) {
          errors.push(`${b.name}: ${e instanceof Error ? e.message : String(e)}`)
        }
      }
      return { bugs: out, errors }
    },
  })
}

export default function AutoFix() {
  const act = useAction()
  const q = useQuery({ queryKey: ['mgr', 'autofix'], queryFn: mgr.autofixState, refetchInterval: 3000 })
  const s = q.data
  const reports = usePlayerReports(s)
  const [showLog, setShowLog] = useState(false)
  const log = useQuery({ queryKey: ['mgr', 'autofix-log'], queryFn: () => mgr.autofixLog(300), enabled: showLog, refetchInterval: 3000 })

  if (q.error) return <div className="p-5"><Note tone="bad">{q.error instanceof Error ? q.error.message : String(q.error)}</Note></div>
  if (!s) return <div className="p-5" style={{ fontSize: '0.72rem', color: 'var(--text-dim)' }}>Loading…</div>

  const busy = act.busy !== null
  const now = q.dataUpdatedAt / 1000          // "now" as of the last refresh (every 3 s)
  const auto = s.settings.publish_mode === 'auto'
  const runs = Object.entries(s.handled).sort((a, b) => (b[1].when ?? '').localeCompare(a[1].when ?? ''))
  const held = runs.filter(([, r]) => r.branch && r.held && !r.published)
  const handledIds = new Set(Object.keys(s.handled))
  const players = (reports.data?.bugs ?? []).filter(b => !b.trusted && b.status === 'open' && !handledIds.has(b.id)
                                                         && !s.queue.includes(b.id))
  const ready = !!s.claude && !s.repo_problem && s.boxes.some(b => b.watched && !b.problem)
  const state: 'ok' | 'warn' | 'bad' | 'off' = !s.running ? 'off' : s.paused ? 'warn' : 'ok'

  return (
    <div className="p-5 space-y-4" style={{ maxWidth: 1100 }}>
      <Result r={act.result} onClose={act.clear} />

      <Card icon={<Rocket size={13} style={{ color: 'var(--accent)' }} />} title="Auto-fix"
            badge={<Pill color={auto ? OK : AMBER}>{auto ? 'publishes by itself' : 'you approve each release'}</Pill>}>
        <div className="space-y-3">
          <div className="flex items-center gap-2" style={{ fontSize: '0.8rem' }}>
            <Dot state={state} />
            <b>{!s.running ? 'Stopped' : s.paused ? 'Paused' : 'Watching'}</b>
            {s.running && <span style={{ color: 'var(--text-dim)', fontSize: '0.65rem', ...MONO }}>pid {s.running}</span>}
            <span className="ml-auto flex gap-2">
              {!s.running ? (
                <Btn primary disabled={busy || !ready} onClick={() => act.run('run', () => mgr.autofixRun(true), 'runner started')}>
                  <Play size={11} />Start
                </Btn>
              ) : (
                <>
                  <Btn disabled={busy} onClick={() => act.run('pause', () => mgr.autofixCommand(s.paused ? 'resume' : 'pause'))}>
                    {s.paused ? <><Play size={11} />Resume</> : <><Pause size={11} />Pause</>}
                  </Btn>
                  <Btn danger disabled={busy} title="A fix in progress is abandoned and tried again later"
                       onClick={() => act.run('run', () => mgr.autofixRun(false), 'runner stopped')}>
                    <Square size={11} />Stop
                  </Btn>
                </>
              )}
            </span>
          </div>
          {s.current && (
            <Note tone="info">
              <b>{s.current.phase}</b>: <span style={MONO}>{s.current.id}</span>{' '}
              <span style={{ color: 'var(--text-muted)' }}>{s.current.signature}</span>{' '}
              <span style={{ color: 'var(--text-dim)' }}>({fmtDur(now - s.current.since)})</span>
            </Note>
          )}
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(220px, 1fr))', gap: 4 }}>
            <Row k="Claude Code">{s.claude ? <span style={{ color: OK }}>found</span> : <span style={{ color: RED }}>not on PATH</span>}
              {s.claude && <span style={{ color: 'var(--text-dim)' }}> · {s.api_key ? 'API key' : 'your Claude login'}</span>}</Row>
            <Row k="Repository">{s.repo ? <span style={MONO}>{s.repo}</span> : <span style={{ color: RED }}>not set</span>}</Row>
            <Row k="Fixes today">{s.runs_today} / {s.settings.max_runs_per_day ?? 3}</Row>
            <Row k="Releases today">{s.publishes_today} / {s.settings.max_publishes_per_day ?? 3}</Row>
            <Row k="Last pass">{s.last_pass ? `${fmtDur(now - s.last_pass)} ago` : '—'}</Row>
            <Row k="Servers">{s.boxes.filter(b => b.watched && !b.problem).map(b => b.name).join(', ') || <span style={{ color: RED }}>none</span>}</Row>
          </div>
          {!s.claude && (
            <Note tone="warn">
              Install Claude Code on this PC and sign in once: <span style={MONO}>npm install -g @anthropic-ai/claude-code</span>,
              then run <span style={MONO}>claude</span> in a terminal and log in. Restart this app afterwards so it finds it.
            </Note>
          )}
          {s.repo_problem && <Note tone="warn">{s.repo_problem}</Note>}
          {s.boxes.filter(b => b.watched && b.problem).map(b => (
            <Note key={b.id} tone="warn">{b.name}: {b.problem}</Note>
          ))}
          {auto && (
            <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
              A fix ships with nobody looking only when: the tests pass again on this PC, it changes at most{' '}
              {s.settings.auto_max_files ?? 8} files and {s.settings.auto_max_changed_lines ?? 200} lines, it touches no release
              or signing files, Claude didn't flag the report as trying to steer it, it came from the logs or a trusted
              reporter, and fewer than {s.settings.max_publishes_per_day ?? 3} releases went out today. The servers then
              apply it on probation and roll it back by themselves if DCS crashes or the engine won't load.
            </div>
          )}
        </div>
      </Card>

      {(held.length > 0 || players.length > 0) && (
        <Card icon={<Alert size={13} style={{ color: AMBER }} />} title="Waiting for you">
          <div className="space-y-3">
            {held.map(([id, r]) => (
              <div key={id} style={{ borderLeft: `2px solid ${AMBER}`, paddingLeft: 10 }} className="space-y-1">
                <div className="flex items-center gap-2" style={{ fontSize: '0.74rem' }}>
                  <span style={MONO}>{id}</span><b>{r.title ?? 'fix ready'}</b>
                  <span className="ml-auto">
                    <Btn primary disabled={busy} onClick={() => act.run(`pub-${id}`, () => mgr.autofixCommand('approve', id))}>
                      <Rocket size={11} />Publish
                    </Btn>
                  </span>
                </div>
                <div style={{ fontSize: '0.66rem', color: AMBER }}>Held: {r.held}</div>
                {r.summary && <div style={{ fontSize: '0.66rem', color: 'var(--text-muted)', whiteSpace: 'pre-wrap' }}>{r.summary}</div>}
                {r.branch && <CopyLink url={compareUrl(s, r.branch)} />}
              </div>
            ))}
            {players.map(b => (
              <div key={b.id} style={{ borderLeft: '2px solid var(--border)', paddingLeft: 10 }} className="space-y-1">
                <div className="flex items-center gap-2" style={{ fontSize: '0.72rem' }}>
                  <span style={MONO}>{b.id}</span>
                  <span style={{ color: 'var(--text-dim)' }}>player report · {b.box}{b.server ? ` · ${b.server}` : ''} · {fmtWhen(b.reported)}</span>
                  <span className="ml-auto flex gap-2">
                    <Btn disabled={busy} title="Claude works on it next; the fix goes through the same gates as any other"
                         onClick={() => act.run(`q-${b.id}`, () => mgr.autofixCommand('queue', b.id))}>Send to Claude</Btn>
                    <Btn disabled={busy} onClick={() => act.run(`x-${b.id}`, async () => {
                      const box = s.boxes.find(x => x.name === b.box)
                      await opsCall(box?.id ?? null, b.box, 'POST', 'bugs/status', { id: b.id, status: 'closed', note: 'dismissed by an admin' })
                      await reports.refetch()
                    }, 'dismissed')}>Dismiss</Btn>
                  </span>
                </div>
                <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', whiteSpace: 'pre-wrap' }}>{b.text}</div>
              </div>
            ))}
          </div>
        </Card>
      )}
      {reports.data?.errors.map(e => <Note key={e} tone="warn">Bug reports: {e}</Note>)}

      <Card icon={<CheckCircle2 size={13} style={{ color: 'var(--accent)' }} />} title="Recent fixes"
            badge={s.queue.length ? <Pill color={AMBER}>{s.queue.length} queued</Pill> : undefined}>
        {runs.length === 0 ? (
          <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)' }}>Nothing yet. New errors and bug reports show up here as Claude works on them.</div>
        ) : (
          <div className="space-y-2">
            {runs.slice(0, 40).map(([id, r]) => <RunRow key={id} id={id} r={r} s={s} />)}
          </div>
        )}
      </Card>

      <Card icon={<Terminal size={13} style={{ color: 'var(--accent)' }} />} title="Activity"
            badge={<Btn onClick={() => setShowLog(!showLog)}>{showLog ? 'Events' : 'Runner output'}</Btn>}>
        <div style={{ maxHeight: 260, overflow: 'auto', fontSize: '0.64rem', lineHeight: 1.55, ...MONO }}>
          {showLog
            ? (log.data ?? []).map((l, i) => <div key={i} style={{ whiteSpace: 'pre-wrap' }}>{l}</div>)
            : s.events.length === 0
              ? <span style={{ color: 'var(--text-dim)' }}>No activity yet.</span>
              : s.events.map((e, i) => (
                <div key={i}><span style={{ color: 'var(--text-dim)' }}>{new Date(e.t * 1000).toLocaleString([], { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' })}</span> {e.text}</div>
              ))}
        </div>
      </Card>

      <SettingsCard s={s} busy={busy} act={act} />
    </div>
  )
}

function RunRow({ id, r, s }: { id: string; r: AutofixRun; s: AutofixState }) {
  const o = OUTCOME[r.outcome ?? ''] ?? { label: r.outcome ?? '?', color: 'var(--text-dim)' }
  const [open, setOpen] = useState(false)
  return (
    <div style={{ borderBottom: '1px solid var(--border)', paddingBottom: 6 }}>
      <button type="button" className="flex items-center gap-2 w-full" onClick={() => setOpen(!open)}
              style={{ fontSize: '0.7rem', cursor: 'pointer', background: 'none', border: 'none', color: 'inherit', padding: 0, textAlign: 'left' }}>
        <span style={{ ...MONO, color: 'var(--text-muted)', minWidth: 110 }}>{id}</span>
        <Pill color={o.color}>{o.label}</Pill>
        {r.published && <Pill color={OK}>published</Pill>}
        {r.held && !r.published && <Pill color={AMBER}>held</Pill>}
        <span style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
          {r.title ?? r.signature ?? (r.summary ?? '').slice(0, 100)}
        </span>
        <span className="ml-auto" style={{ color: 'var(--text-dim)', whiteSpace: 'nowrap' }}>
          {r.kind === 'bug' ? 'bug report' : (r.boxes ?? []).join(', ')} · {typeof r.cost_usd === 'number' ? `$${r.cost_usd.toFixed(2)}` : '—'} · {fmtWhen(r.when)}
        </span>
      </button>
      {open && (
        <div className="space-y-1" style={{ fontSize: '0.66rem', color: 'var(--text-muted)', padding: '6px 0 0 118px' }}>
          {r.summary && <div style={{ whiteSpace: 'pre-wrap' }}>{r.summary}</div>}
          {r.tests && <div><span style={DIM}>tests </span>{r.tests}</div>}
          {r.player_note && <div><span style={DIM}>players </span>{r.player_note}</div>}
          {r.files ? <div><span style={DIM}>size </span>{r.files} files, {r.lines} lines</div> : null}
          {r.held && <div style={{ color: AMBER }}>Held: {r.held}</div>}
          {r.branch && <CopyLink url={compareUrl(s, r.branch)} />}
        </div>
      )}
    </div>
  )
}

function SettingsCard({ s, busy, act }: { s: AutofixState; busy: boolean; act: ReturnType<typeof useAction> }) {
  const initial = (): { runner: AutofixRunnerSettings; manager: AutofixManagerSettings } => ({
    runner: {
      publish_mode: s.settings.publish_mode ?? 'approve',
      discord_webhook: s.settings.discord_webhook ?? '',
      max_budget_usd: s.settings.max_budget_usd ?? 6,
      max_runs_per_day: s.settings.max_runs_per_day ?? 3,
      max_publishes_per_day: s.settings.max_publishes_per_day ?? 3,
      auto_max_files: s.settings.auto_max_files ?? 8,
      auto_max_changed_lines: s.settings.auto_max_changed_lines ?? 200,
      interval_minutes: s.settings.interval_minutes ?? 15,
      sources: s.settings.sources ?? ['engine', 'bot', 'discord'],
      publish_channel: s.settings.publish_channel ?? 'stable',
    },
    manager: { ...s.manager, repo: s.manager.repo ?? s.repo },
  })
  // null until the admin edits something: until then it follows what is saved
  const [edit, setEdit] = useState<ReturnType<typeof initial> | null>(null)
  const draft = edit ?? initial()
  const dirty = edit !== null
  const setR = <K extends keyof AutofixRunnerSettings>(k: K, v: AutofixRunnerSettings[K]) =>
    setEdit({ ...draft, runner: { ...draft.runner, [k]: v } })
  const setM = <K extends keyof AutofixManagerSettings>(k: K, v: AutofixManagerSettings[K]) =>
    setEdit({ ...draft, manager: { ...draft.manager, [k]: v } })
  const num = (k: 'max_budget_usd' | 'max_runs_per_day' | 'max_publishes_per_day' | 'auto_max_files' | 'auto_max_changed_lines' | 'interval_minutes',
               label: string, hint?: string, step = 1) => (
    <Field label={label} hint={hint}>
      <input style={input} type="number" step={step} min={0} value={draft.runner[k] ?? 0}
             onChange={e => setR(k, Number(e.target.value))} />
    </Field>
  )
  const sources = draft.runner.sources ?? []
  const watched = draft.manager.servers
  return (
    <Card icon={<Config size={13} style={{ color: 'var(--accent)' }} />} title="Auto-fix settings">
      <div className="space-y-3">
        <Field label="Publishing">
          <div className="space-y-1" style={{ fontSize: '0.72rem' }}>
            <label className="flex items-center gap-2" style={{ cursor: 'pointer' }}>
              <input type="radio" checked={draft.runner.publish_mode === 'auto'} onChange={() => setR('publish_mode', 'auto')} />
              <b>Automatic</b> -- a fix that passes every gate is merged, released and installed with nobody looking
            </label>
            <label className="flex items-center gap-2" style={{ cursor: 'pointer' }}>
              <input type="radio" checked={draft.runner.publish_mode !== 'auto'} onChange={() => setR('publish_mode', 'approve')} />
              <b>Approve</b> -- every fix waits here for Publish
            </label>
          </div>
        </Field>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(260px, 1fr))', gap: 12 }}>
          <Field label="bfnext repository" hint="The checkout Claude works in (a worktree per fix; your own files are not touched).">
            <input style={input} value={draft.manager.repo ?? ''} onChange={e => setM('repo', e.target.value || null)} />
          </Field>
          <Field label="Python" hint="Blank: python on PATH.">
            <input style={input} value={draft.manager.python ?? ''} placeholder="python" onChange={e => setM('python', e.target.value || null)} />
          </Field>
          <Field label="Discord webhook" hint="A private ops channel: every fix is posted there.">
            <input style={input} type="password" autoComplete="off" value={draft.runner.discord_webhook ?? ''}
                   onChange={e => setR('discord_webhook', e.target.value)} />
          </Field>
          <Field label="Release channel">
            <select style={input} value={draft.runner.publish_channel} onChange={e => setR('publish_channel', e.target.value as 'stable' | 'beta')}>
              <option value="stable">stable</option>
              <option value="beta">beta</option>
            </select>
          </Field>
        </div>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(170px, 1fr))', gap: 12 }}>
          {num('max_budget_usd', 'Cost cap per fix ($)', undefined, 0.5)}
          {num('max_runs_per_day', 'Fixes per day')}
          {num('max_publishes_per_day', 'Releases per day')}
          {num('auto_max_files', 'Auto: max files')}
          {num('auto_max_changed_lines', 'Auto: max lines')}
          {num('interval_minutes', 'Check every (min)')}
        </div>
        <Field label="What to fix">
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(240px, 1fr))', gap: 4, fontSize: '0.7rem' }}>
            {SOURCES.map(x => (
              <label key={x.key} className="flex items-center gap-2" style={{ cursor: 'pointer' }}>
                <input type="checkbox" checked={sources.includes(x.key)}
                       onChange={e => setR('sources', e.target.checked ? [...sources, x.key] : sources.filter(k => k !== x.key))} />
                {x.label}
              </label>
            ))}
          </div>
        </Field>
        <Field label="Servers to watch" hint="Added under ALL SERVERS. A change here applies when the runner is next started.">
          <div className="flex flex-wrap gap-4" style={{ fontSize: '0.7rem' }}>
            {s.boxes.map(b => (
              <label key={b.id} className="flex items-center gap-2" style={{ cursor: 'pointer' }}>
                <input type="checkbox" checked={watched ? watched.includes(b.id) : true}
                       onChange={e => {
                         const all = watched ?? s.boxes.map(x => x.id)
                         setM('servers', e.target.checked ? [...all, b.id] : all.filter(i => i !== b.id))
                       }} />
                {b.name}
              </label>
            ))}
          </div>
        </Field>
        <div className="flex gap-2">
          <Btn primary disabled={!dirty || busy} onClick={() => act.run('save', async () => {
            await mgr.autofixSave(draft.runner, draft.manager)
            setEdit(null)
          }, 'saved -- the runner uses it from its next pass')}><Save size={11} />Save</Btn>
          {dirty && <Btn onClick={() => setEdit(null)}><RefreshCw size={11} />Discard</Btn>}
        </div>
        <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
          Runner files: <span style={MONO}>{s.data_dir}</span> · player reports come from <span style={MONO}>/fe_bug</span>;
          reports from DCS Admins (or the bot's <span style={MONO}>bugs.trusted_roles</span>) go straight to Claude, others wait above.
        </div>
      </div>
    </Card>
  )
}
