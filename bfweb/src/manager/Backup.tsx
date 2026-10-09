import { useEffect, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { Save, Upload, Search, RotateCcw, X, Alert, CheckCircle2 } from '@icons'
import {
  mgr, type AppState, type BackupPlan, type BackupJob, type FoundBackup, type RestorePreview, type VerifyReport,
} from './tauri'
import { Card, Btn, Field, Note, Result, Pill } from './ui'
import { useAction, input, OK, AMBER, RED, MONO, DIM, fmtWhen } from './style'

function fmtBytes(b: number): string {
  if (b >= 1024 ** 3) return `${(b / 1024 ** 3).toFixed(1)} GB`
  if (b >= 1024 ** 2) return `${Math.round(b / 1024 ** 2)} MB`
  return `${Math.max(1, Math.round(b / 1024))} KB`
}

const check: React.CSSProperties = { fontSize: '0.72rem', cursor: 'pointer', display: 'flex', alignItems: 'flex-start', gap: 8 }

// ── the running / last job ───────────────────────────────────────────────────

const STATUS_COLOR = { ok: OK, warn: AMBER, bad: RED } as const
const STATUS_TEXT = { ok: 'OK', warn: 'CHECK', bad: 'BROKEN' } as const

/** What a backup zip was checked to hold, section by section. */
function ReportView({ r }: { r: VerifyReport }) {
  const [open, setOpen] = useState<Set<number>>(new Set())
  const toggle = (i: number) => {
    const n = new Set(open)
    if (n.has(i)) n.delete(i)
    else n.add(i)
    setOpen(n)
  }
  const unread = r.sections.reduce((n, s) => n + s.skipped.length, 0)
  return (
    <div className="space-y-2">
      <Note tone={!r.ok ? 'bad' : unread ? 'warn' : 'ok'}>
        {!r.ok
          ? <>✗ The check found problems. Don't wipe anything until a backup checks clean.</>
          : unread
            ? <>✓ The zip is complete and every file reads back clean ({r.entries} files, {fmtBytes(r.bytes)}), but {unread} file(s)
                couldn't be read when the backup was made -- see the sections marked CHECK. Usually a file in use; stop the bot (or DCS) and back up again if it matters.</>
            : <>✓ Everything is in the backup and every file reads back clean: {r.entries} files, {fmtBytes(r.bytes)} ({fmtBytes(r.zip_bytes)} zipped).</>}
        {r.problems.map((p, i) => <div key={i} style={{ marginTop: 4 }}>• {p}</div>)}
      </Note>
      <div style={{ border: '1px solid var(--border)', borderRadius: 3 }}>
        {r.sections.map((s, i) => {
          const bad = s.checks.filter(c => !c.ok && !c.info).length
          return (
            <div key={i} style={{ borderBottom: '1px solid var(--border)' }}>
              <button onClick={() => toggle(i)} className="flex items-center gap-2" style={{
                width: '100%', textAlign: 'left', background: 'none', border: 'none', cursor: 'pointer',
                padding: '7px 10px', color: 'var(--text)', fontSize: '0.7rem',
              }}>
                <Pill color={STATUS_COLOR[s.status]}>{STATUS_TEXT[s.status]}</Pill>
                <span style={{ flex: 1, minWidth: 0 }}>{s.label}</span>
                <span style={{ fontSize: '0.64rem', color: 'var(--text-muted)', ...MONO, whiteSpace: 'nowrap' }}>
                  {s.files_in_zip}{s.files_on_disk > 0 ? ` / ${s.files_on_disk}` : ''} files · {fmtBytes(s.bytes_in_zip)}
                </span>
                <span style={{ color: 'var(--text-dim)', fontSize: '0.6rem' }}>{open.has(i) || bad > 0 ? '▾' : '▸'}</span>
              </button>
              {(open.has(i) || bad > 0) && (
                <div style={{ padding: '0 10px 8px 34px', fontSize: '0.66rem', lineHeight: 1.7 }}>
                  <div style={{ color: 'var(--text-dim)', ...MONO, wordBreak: 'break-all' }}>{s.path}</div>
                  {s.checks.map((c, k) => (
                    <div key={k} style={{ color: c.ok ? 'var(--text-muted)' : c.info ? 'var(--text-dim)' : RED }}>
                      {c.ok ? '✓' : c.info ? '–' : '✗'} {c.text}
                    </div>
                  ))}
                  {s.skipped.length > 0 && (
                    <div style={{ color: AMBER, marginTop: 4 }}>
                      Not in the backup (couldn't be read):
                      {s.skipped.map((k, n) => <div key={n} style={{ ...MONO, marginLeft: 10 }}>{k}</div>)}
                    </div>
                  )}
                </div>
              )}
            </div>
          )
        })}
      </div>
      {r.corrupt.length > 0 && (
        <Note tone="bad">
          Files that don't read back ({r.corrupt.length}):
          {r.corrupt.slice(0, 30).map((c, i) => <div key={i} style={MONO}>{c}</div>)}
        </Note>
      )}
    </div>
  )
}

/** The job's whole log, filterable, with its file on disk. */
function LogView({ job }: { job: BackupJob }) {
  const [show, setShow] = useState(false)
  const [onlyWarn, setOnlyWarn] = useState(false)
  const lines = onlyWarn ? job.log.filter(l => /WARNING|!!|! |corrupt|failed|BROKEN/i.test(l)) : job.log
  return (
    <div>
      <div className="flex items-center gap-3">
        <button onClick={() => setShow(!show)} style={{ ...DIM, background: 'none', border: 'none', cursor: 'pointer', padding: 0 }}>
          {show ? '▾' : '▸'} log ({job.log.length} lines{job.warnings.length > 0 ? `, ${job.warnings.length} warnings` : ''})
        </button>
        {show && (
          <label style={{ ...check, fontSize: '0.64rem' }}>
            <input type="checkbox" checked={onlyWarn} onChange={e => setOnlyWarn(e.target.checked)} /> only warnings and errors
          </label>
        )}
        {job.log_file && (
          <span className="ml-auto flex items-center gap-2" style={{ fontSize: '0.62rem', color: 'var(--text-dim)', ...MONO }}>
            {job.log_file}
            <Btn onClick={() => { void mgr.revealBackup(job.log_file!) }}>Open log file</Btn>
          </span>
        )}
      </div>
      {show && (
        <pre style={{ fontSize: '0.62rem', ...MONO, maxHeight: 360, overflow: 'auto', background: 'var(--bg-input)',
                      padding: 8, marginTop: 6, border: '1px solid var(--border)', whiteSpace: 'pre-wrap' }}>
          {lines.length ? lines.join('\n') : 'nothing to show'}
        </pre>
      )}
    </div>
  )
}

function JobPanel({ job }: { job: BackupJob }) {
  const pct = job.total_bytes > 0 ? Math.min(100, (job.done_bytes / job.total_bytes) * 100) : 0
  const failed = !!job.error
  const title = job.kind === 'backup' ? 'Backup' : job.kind === 'verify' ? 'Backup check' : 'Restore'
  return (
    <Card
      icon={job.running ? <RotateCcw size={13} style={{ color: AMBER }} /> : failed || job.report?.ok === false ? <X size={13} style={{ color: RED }} /> : <CheckCircle2 size={13} style={{ color: OK }} />}
      title={`${title} ${job.running ? 'running' : failed ? 'failed' : 'finished'}`}
      badge={job.running ? <Btn danger onClick={() => { void mgr.backupCancel() }}>Cancel</Btn> : undefined}
    >
      <div className="space-y-3">
        <div style={{ fontSize: '0.74rem' }}>{job.phase}</div>
        {(job.running || job.total_bytes > 0) && (
          <div>
            <div style={{ height: 8, background: 'var(--bg-input)', borderRadius: 2, overflow: 'hidden', border: '1px solid var(--border)' }}>
              <div style={{ width: `${pct}%`, height: '100%', background: failed ? RED : 'var(--accent)', transition: 'width 0.5s' }} />
            </div>
            <div className="flex gap-3" style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 4, ...MONO }}>
              <span>{fmtBytes(job.done_bytes)} / {fmtBytes(job.total_bytes)}</span>
              {job.running && job.current && <span style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{job.current}</span>}
            </div>
          </div>
        )}
        {job.error && <Note tone="bad">{job.error}</Note>}
        {job.output && (
          <Note tone={job.report?.ok === false ? 'warn' : 'ok'}>
            {job.kind === 'backup' ? 'Saved ' : ''}<span style={MONO}>{job.output}</span>
            {job.zip_path && <> <Btn onClick={() => { void mgr.revealBackup(job.zip_path!) }}>Show in Explorer</Btn></>}
          </Note>
        )}
        {job.report && !job.running && <ReportView r={job.report} />}
        {job.warnings.length > 0 && (
          <Note tone="warn">
            {job.warnings.slice(0, 40).map((w, i) => <div key={i}>• {w}</div>)}
            {job.warnings.length > 40 && <div>… and {job.warnings.length - 40} more (see the log)</div>}
          </Note>
        )}
        {job.next_steps.length > 0 && !job.running && (
          <div>
            <div style={{ ...DIM, marginBottom: 6 }}>Next</div>
            <ol style={{ fontSize: '0.72rem', lineHeight: 1.7, paddingLeft: 18, margin: 0, listStyle: 'decimal' }}>
              {job.next_steps.map((s, i) => <li key={i}>{s}</li>)}
            </ol>
          </div>
        )}
        <LogView job={job} />
      </div>
    </Card>
  )
}

// ── check a zip ──────────────────────────────────────────────────────────────

function CheckCard({ busy }: { busy: boolean }) {
  const act = useAction()
  const qc = useQueryClient()
  const [zip, setZip] = useState('')
  const [found, setFound] = useState<FoundBackup[] | null>(null)
  useEffect(() => { mgr.findBackups().then(setFound).catch(() => setFound([])) }, [])
  const start = (p: string) => act.run('verify', async () => {
    await mgr.verifyBackup(p)
    qc.invalidateQueries({ queryKey: ['mgr', 'backup-job'] })
    return 'checking -- the report appears at the top'
  })
  return (
    <Card icon={<CheckCircle2 size={13} style={{ color: 'var(--accent)' }} />} title="Check a backup">
      <div className="space-y-3">
        <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
          Reads every file in a backup zip back and checks it against what was backed up: nothing missing, nothing corrupt,
          and the key files a restore needs are there. Run it again after copying the zip to the USB stick, on the copy.
        </div>
        <Result r={act.result} onClose={act.clear} />
        {found && found.length > 0 && (
          <div className="flex flex-col gap-1">
            {found.map(f => (
              <div key={f.path} className="flex items-center gap-2" style={{ fontSize: '0.68rem', ...MONO }}>
                <span style={{ flex: 1, minWidth: 0, wordBreak: 'break-all' }}>{f.path} <span style={{ color: 'var(--text-dim)' }}>· {fmtBytes(f.bytes)} · {fmtWhen(f.modified)}</span></span>
                <Btn disabled={busy} onClick={() => start(f.path)}>Check</Btn>
              </div>
            ))}
          </div>
        )}
        <div className="flex gap-2" style={{ alignItems: 'flex-end' }}>
          <div style={{ flex: 1 }}>
            <Field label="Or a zip anywhere (USB stick, another drive)">
              <input style={{ ...input, ...MONO }} value={zip} placeholder="F:\FowlEngine-backup-….zip" onChange={e => setZip(e.target.value)} />
            </Field>
          </div>
          <Btn disabled={busy || !zip.trim()} onClick={() => start(zip.trim().replace(/^"|"$/g, ''))}>Check</Btn>
        </div>
      </div>
    </Card>
  )
}

// ── back up ──────────────────────────────────────────────────────────────────

function BackupCard({ busy }: { busy: boolean }) {
  const act = useAction()
  const qc = useQueryClient()
  const [plan, setPlan] = useState<BackupPlan | null>(null)
  const [planErr, setPlanErr] = useState<string | null>(null)
  const [scanning, setScanning] = useState(false)
  const [picked, setPicked] = useState<Set<string>>(new Set())
  const [db, setDb] = useState(true)
  const [stopBot, setStopBot] = useState(false)
  const [shadow, setShadow] = useState(true)
  const [dest, setDest] = useState('')
  const [extra, setExtra] = useState('')

  async function scan() {
    setScanning(true)
    setPlanErr(null)
    try {
      const p = await mgr.backupPlan()
      setPlan(p)
      setPicked(new Set(p.roots.filter(r => r.include).map(r => r.id)))
      setDb(!!p.database && !p.database.problem)
      setDest(d => d || p.default_dest)
    } catch (e) {
      setPlanErr(e instanceof Error ? e.message : String(e))
    } finally {
      setScanning(false)
    }
  }
  useEffect(() => { void scan() }, [])

  const total = plan ? plan.roots.filter(r => picked.has(r.id)).reduce((a, r) => a + r.bytes, 0) : 0
  const onSystem = /^c:/i.test(dest.trim())
  const toggle = (id: string) => {
    const n = new Set(picked)
    if (n.has(id)) n.delete(id)
    else n.add(id)
    setPicked(n)
  }

  return (
    <Card icon={<Save size={13} style={{ color: 'var(--accent)' }} />} title="Back up this server"
          badge={<Btn disabled={scanning} onClick={() => { void scan() }}><Search size={11} />{scanning ? 'Scanning…' : 'Rescan'}</Btn>}>
      <div className="space-y-3">
        <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
          One zip with everything the server needs: DCSServerBot and its config (Discord token and passwords included), every DCS
          server's config, missions and campaign saves, the bfdb stats database, this app's settings and the bot's PostgreSQL database.
          On a new Windows, install this app and use <b>Restore</b> below to put it all back.
        </div>
        <Result r={act.result} onClose={act.clear} />
        {planErr && <Note tone="bad">{planErr}</Note>}
        {!plan && !planErr && <div style={DIM}>Looking at what's on this box…</div>}
        {plan && (
          <>
            <div style={{ border: '1px solid var(--border)', borderRadius: 3 }}>
              {plan.roots.map(r => (
                <label key={r.id} style={{ ...check, padding: '7px 10px', borderBottom: '1px solid var(--border)' }}>
                  <input type="checkbox" checked={picked.has(r.id)} onChange={() => toggle(r.id)} style={{ marginTop: 3 }} />
                  <span style={{ flex: 1, minWidth: 0 }}>
                    <span>{r.label}</span>
                    <span style={{ display: 'block', fontSize: '0.62rem', color: 'var(--text-dim)', ...MONO, wordBreak: 'break-all' }}>{r.path}</span>
                    {r.note && <span style={{ display: 'block', fontSize: '0.62rem', color: 'var(--text-dim)' }}>{r.note}</span>}
                  </span>
                  <span style={{ fontSize: '0.66rem', color: 'var(--text-muted)', ...MONO, whiteSpace: 'nowrap' }}>{fmtBytes(r.bytes)} · {r.files} files</span>
                </label>
              ))}
              {plan.database && (
                <label style={{ ...check, padding: '7px 10px', opacity: plan.database.problem ? 0.6 : 1 }}>
                  <input type="checkbox" checked={db} disabled={!!plan.database.problem} onChange={e => setDb(e.target.checked)} style={{ marginTop: 3 }} />
                  <span style={{ flex: 1 }}>
                    Bot database (PostgreSQL <span style={MONO}>{plan.database.name}</span> on {plan.database.host}:{plan.database.port})
                    <span style={{ display: 'block', fontSize: '0.62rem', color: plan.database.problem ? RED : 'var(--text-dim)' }}>
                      {plan.database.problem ?? 'players, stats, credits -- dumped with pg_dump'}
                    </span>
                  </span>
                </label>
              )}
            </div>

            {plan.programs.length > 0 && (
              <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
                Not in the backup (installed programs -- reinstall them on the new Windows):{' '}
                {plan.programs.filter(p => p.what !== "referenced by the bot's config").map(p => `${p.what} (${p.path})`).join(', ') || '—'}
              </div>
            )}

            <Field label="More folders to include (one per line, optional)">
              <textarea style={{ ...input, minHeight: 44, ...MONO }} value={extra} onChange={e => setExtra(e.target.value)}
                        placeholder="C:\VectorStrike\Tools\whisper" />
            </Field>

            <Field label="Save the zip in" hint={onSystem
              ? <span style={{ color: AMBER }}>This is the Windows drive -- a reinstall wipes it. Pick a USB stick or another drive, or copy the zip off this PC afterwards.</span>
              : 'A USB stick or another drive is best: a Windows reinstall wipes C:.'}>
              <input style={{ ...input, ...MONO }} value={dest} onChange={e => setDest(e.target.value)} />
            </Field>

            <label style={check}>
              <input type="checkbox" checked={shadow} onChange={e => setShadow(e.target.checked)} style={{ marginTop: 3 }} />
              <span>
                Copy from a Windows shadow copy (recommended): a snapshot of the drive at one instant, so files in use -- bfdb's database,
                the live stats DCS writes -- come out whole, and nothing has to stop. Players can keep flying.
              </span>
            </label>
            {plan.bot_running && (
              <label style={check}>
                <input type="checkbox" checked={stopBot} onChange={e => setStopBot(e.target.checked)} style={{ marginTop: 3 }} />
                <span>
                  Also stop DCSServerBot while copying (bfdb stops with it; both start again afterwards){shadow ? ' -- not needed with the shadow copy' : ''}.
                  {!plan.service_running && <span style={{ color: AMBER }}> The FowlEngine service isn't running, so this app can't stop it.</span>}
                </span>
              </label>
            )}
            {!shadow && (plan.dcs_running || plan.bfdb_running) && (
              <Note tone="warn">
                {plan.bfdb_running ? 'bfdb is running' : 'DCS is running'}: without the shadow copy, files they hold open (bfdb's database, the live
                stats) come out partial. Keep the shadow copy on, or stop the DCS servers and the bot first.
              </Note>
            )}
            {plan.warnings.length > 0 && <Note tone="warn">{plan.warnings.map((w, i) => <div key={i}>• {w}</div>)}</Note>}

            <div className="flex items-center gap-3">
              <Btn primary disabled={busy || picked.size === 0 || !dest.trim()} onClick={() => act.run('backup', async () => {
                await mgr.backupStart({
                  dest_dir: dest.trim(),
                  roots: [...picked],
                  extra_paths: extra.split('\n').map(s => s.trim()).filter(Boolean),
                  database: db,
                  stop_bot: stopBot && plan.bot_running,
                  shadow_copy: shadow,
                })
                qc.invalidateQueries({ queryKey: ['mgr', 'backup-job'] })
                return 'backup started'
              })}><Save size={11} />Back up now</Btn>
              <span style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>about {fmtBytes(total)} of files (the zip is smaller)</span>
            </div>
          </>
        )}
      </div>
    </Card>
  )
}

// ── restore ──────────────────────────────────────────────────────────────────

function RestoreCard({ busy, state }: { busy: boolean; state: AppState }) {
  const act = useAction()
  const qc = useQueryClient()
  const [zip, setZip] = useState('')
  const [found, setFound] = useState<FoundBackup[] | null>(null)
  const [prev, setPrev] = useState<RestorePreview | null>(null)
  const [targets, setTargets] = useState<Record<string, string>>({})
  const [skip, setSkip] = useState<Set<string>>(new Set())
  const [restoreDb, setRestoreDb] = useState(true)
  const [pgPw, setPgPw] = useState('')
  const [instPy, setInstPy] = useState(false)
  const [instPg, setInstPg] = useState(false)
  const [instSvc, setInstSvc] = useState(true)
  const [user, setUser] = useState('')
  const [confirm, setConfirm] = useState(false)

  useEffect(() => { mgr.findBackups().then(setFound).catch(() => setFound([])) }, [])

  async function inspect(path: string) {
    setZip(path)
    setConfirm(false)
    const p = await mgr.restoreInspect(path)
    setPrev(p)
    setTargets(Object.fromEntries(p.mappings.map(m => [m.id, m.to])))
    setSkip(new Set())
    setRestoreDb(p.has_database)
    setInstPy(p.checks.some(c => c.install === 'python' && !c.ok))
    setInstPg(p.has_database && !p.postgres_found)
    setInstSvc(!p.service_installed)
    setUser(p.desktop_user ?? '')
    return `backup from ${p.manifest.hostname}, ${fmtWhen(p.manifest.created)}`
  }

  const needPw = prev && (restoreDb || instPg)
  const anyExisting = prev?.mappings.some(m => !skip.has(m.id) && m.exists)

  return (
    <Card icon={<Upload size={13} style={{ color: 'var(--accent)' }} />} title="Restore from a backup">
      <div className="space-y-3">
        <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
          For a freshly installed Windows: pick the zip, check where everything goes, press Restore. Folders already on this PC are
          moved aside (renamed <span style={MONO}>…before-restore-…</span>), never deleted.
        </div>
        <Result r={act.result} onClose={act.clear} />
        {found && found.length > 0 && (
          <div>
            <div style={{ ...DIM, marginBottom: 6 }}>Backups found on this PC</div>
            <div className="flex flex-col gap-1">
              {found.map(f => (
                <button key={f.path} onClick={() => act.run('inspect', () => inspect(f.path))} style={{
                  textAlign: 'left', background: zip === f.path ? 'rgba(106,171,31,0.12)' : 'none', border: '1px solid var(--border)',
                  borderRadius: 3, padding: '6px 10px', cursor: 'pointer', color: 'var(--text)', fontSize: '0.68rem', ...MONO,
                }}>
                  {f.path} <span style={{ color: 'var(--text-dim)' }}>· {fmtBytes(f.bytes)} · {fmtWhen(f.modified)}</span>
                </button>
              ))}
            </div>
          </div>
        )}
        <div className="flex gap-2" style={{ alignItems: 'flex-end' }}>
          <div style={{ flex: 1 }}>
            <Field label="Backup zip" hint="Paste the full path (Shift + right-click the file → Copy as path).">
              <input style={{ ...input, ...MONO }} value={zip} placeholder="E:\FowlEngineBackups\FowlEngine-backup-….zip"
                     onChange={e => { setZip(e.target.value); setPrev(null) }} />
            </Field>
          </div>
          <div style={{ paddingBottom: 22 }}>
            <Btn disabled={!zip.trim() || act.busy !== null} onClick={() => act.run('inspect', () => inspect(zip.trim().replace(/^"|"$/g, '')))}>
              <Search size={11} />Open
            </Btn>
          </div>
        </div>

        {prev && (
          <>
            <div style={{ fontSize: '0.72rem', lineHeight: 1.7 }}>
              Backup of <b>{prev.manifest.hostname}</b> from {fmtWhen(prev.manifest.created)} · {fmtBytes(prev.zip_bytes)} ·
              manager {prev.manifest.manager_version}
              {prev.manifest.warnings.length > 0 && <span style={{ color: AMBER }}> · {prev.manifest.warnings.length} warning(s) when it was made</span>}
            </div>
            {prev.hostname_changed && (
              <Note tone="warn">
                This PC is called <b>{prev.hostname_now}</b>, the old one <b>{prev.manifest.hostname}</b>. The restore renames the node in
                DCSServerBot's nodes.yaml to match. (Or rename this PC to {prev.manifest.hostname} first and open the backup again.)
              </Note>
            )}

            <div>
              <div style={{ ...DIM, marginBottom: 6 }}>On this PC</div>
              <div style={{ border: '1px solid var(--border)', borderRadius: 3 }}>
                {prev.checks.map((c, i) => (
                  <div key={i} className="flex gap-2" style={{ padding: '6px 10px', borderBottom: '1px solid var(--border)', fontSize: '0.7rem', alignItems: 'flex-start' }}>
                    {c.ok ? <CheckCircle2 size={12} style={{ color: OK, marginTop: 2 }} /> : <Alert size={12} style={{ color: AMBER, marginTop: 2 }} />}
                    <span style={{ flex: 1 }}>
                      {c.what}
                      <span style={{ display: 'block', fontSize: '0.62rem', color: 'var(--text-dim)', whiteSpace: 'pre-wrap', ...MONO }}>{c.detail}</span>
                    </span>
                    {!c.ok && c.install === 'python' && (
                      <label style={check}><input type="checkbox" checked={instPy} onChange={e => setInstPy(e.target.checked)} /> install it (winget)</label>
                    )}
                    {!c.ok && c.install === 'postgres' && (
                      <label style={check}><input type="checkbox" checked={instPg} onChange={e => setInstPg(e.target.checked)} /> install it (winget)</label>
                    )}
                  </div>
                ))}
              </div>
            </div>

            <div>
              <div style={{ ...DIM, marginBottom: 6 }}>Where everything goes</div>
              <div style={{ border: '1px solid var(--border)', borderRadius: 3 }}>
                {prev.mappings.map(m => (
                  <div key={m.id} style={{ padding: '7px 10px', borderBottom: '1px solid var(--border)' }}>
                    <label style={check}>
                      <input type="checkbox" checked={!skip.has(m.id)} style={{ marginTop: 3 }} onChange={() => {
                        const n = new Set(skip)
                        if (n.has(m.id)) n.delete(m.id)
                        else n.add(m.id)
                        setSkip(n)
                      }} />
                      <span style={{ flex: 1 }}>{m.label}</span>
                      <span style={{ fontSize: '0.64rem', color: 'var(--text-muted)', ...MONO }}>{fmtBytes(m.bytes)}</span>
                    </label>
                    {!skip.has(m.id) && (
                      <div style={{ marginLeft: 24, marginTop: 4 }}>
                        <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', ...MONO }}>was {m.from}</div>
                        <input style={{ ...input, ...MONO, marginTop: 3, fontSize: '0.68rem' }} value={targets[m.id] ?? ''}
                               onChange={e => setTargets({ ...targets, [m.id]: e.target.value })} />
                        {m.exists && <div style={{ fontSize: '0.62rem', color: AMBER, marginTop: 2 }}>something is already there -- it is moved aside</div>}
                        {m.problem && <div style={{ fontSize: '0.62rem', color: RED, marginTop: 2 }}>{m.problem}</div>}
                      </div>
                    )}
                  </div>
                ))}
              </div>
              <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 4, lineHeight: 1.5 }}>
                When a folder or the Windows user changes, the old paths inside the bot's and DCS's config files are rewritten to the new ones.
              </div>
            </div>

            <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(260px, 1fr))', gap: 12 }}>
              <Field label="Windows user the bot runs as" hint="DCS runs on this user's desktop. Usually the admin you are signed in as.">
                <input style={{ ...input, ...MONO }} value={user} placeholder={`.\\${state.current_user}`} onChange={e => setUser(e.target.value)} />
              </Field>
              {prev.manifest.database && (
                <Field label="PostgreSQL 'postgres' password"
                       hint={instPg ? 'PostgreSQL is installed with this password -- write it down.' : "The one chosen when PostgreSQL was installed on this PC."}>
                  <input style={input} type="password" autoComplete="off" value={pgPw} onChange={e => setPgPw(e.target.value)} />
                </Field>
              )}
            </div>
            {prev.manifest.database && (
              <label style={{ ...check, opacity: prev.has_database ? 1 : 0.6 }}>
                <input type="checkbox" checked={restoreDb && prev.has_database} disabled={!prev.has_database} onChange={e => setRestoreDb(e.target.checked)} />
                {prev.has_database
                  ? <>Load the bot's database <span style={MONO}>{prev.manifest.database.name}</span> ({fmtBytes(prev.manifest.database.bytes)})</>
                  : "The bot's database is not in this backup"}
              </label>
            )}
            <label style={check}>
              <input type="checkbox" checked={instSvc} onChange={e => setInstSvc(e.target.checked)} />
              {prev.service_installed ? 'Reinstall the FowlEngine service' : 'Install and start the FowlEngine service'} (starts DCSServerBot at boot)
            </label>

            {!confirm ? (
              <Btn primary disabled={busy || (needPw && !pgPw) || false} onClick={() => setConfirm(true)}>
                <Upload size={11} />Restore…
              </Btn>
            ) : (
              <Note tone="warn">
                Restore {prev.mappings.length - skip.size} folder(s){restoreDb && prev.has_database ? ' and the database' : ''} from this backup?
                {anyExisting && ' Existing folders are moved aside first.'} The FowlEngine service is stopped while it runs.{' '}
                <Btn primary onClick={() => { setConfirm(false); void act.run('restore', async () => {
                  await mgr.restoreStart({
                    zip: prev.zip,
                    targets: prev.mappings.map(m => ({ id: m.id, to: skip.has(m.id) ? '' : (targets[m.id] ?? m.to) })),
                    restore_database: restoreDb && prev.has_database,
                    pg_password: pgPw || null,
                    install_python: instPy,
                    install_postgres: instPg,
                    install_service: instSvc,
                    desktop_user: user.trim() || null,
                  })
                  qc.invalidateQueries({ queryKey: ['mgr', 'backup-job'] })
                  return 'restore started'
                }) }}>Restore</Btn>{' '}
                <Btn onClick={() => setConfirm(false)}>Cancel</Btn>
              </Note>
            )}
          </>
        )}
      </div>
    </Card>
  )
}

export default function Backup({ state }: { state: AppState }) {
  const { data: job } = useQuery({
    queryKey: ['mgr', 'backup-job'],
    queryFn: mgr.backupJob,
    refetchInterval: q => (q.state.data?.running ? 800 : 4_000),
    // the window may sit in the tray while a long backup runs
    refetchIntervalInBackground: true,
    retry: false,
  })
  const busy = !!job?.running
  return (
    <div className="p-5 space-y-4" style={{ maxWidth: 980 }}>
      <div>
        <div style={{ fontSize: '1.1rem', letterSpacing: '0.1em', fontFamily: 'var(--font-display, "Bebas Neue")' }}>BACKUP &amp; RESTORE</div>
        <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginTop: 4, lineHeight: 1.6 }}>
          Moving to a new PC or reinstalling Windows: back up here, copy the zip off the PC, then restore it with this app on the new Windows.
          The zip holds secrets (Discord token, passwords, API keys) -- <Pill color={AMBER}>keep it private</Pill>
        </div>
      </div>
      {job && <JobPanel job={job} />}
      <BackupCard busy={busy} />
      <CheckCard busy={busy} />
      <RestoreCard busy={busy} state={state} />
    </div>
  )
}
