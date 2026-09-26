import { useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useNavigate } from 'react-router-dom'
import {
  api,
  type OpsStatus, type OpsServer, type OpsPending, type OpsUpdateConfig, type OpsApplyPolicy,
  type OpsLogName, type OpsResult, type OpsConfigDoc, type OpsIssue,
} from '../api'
import { useAuth } from '../context/AuthContext'
import PageHeader from '../components/PageHeader'
import {
  Server, Activity, Download, RefreshCw, RotateCw, RotateCcw, Alert, CheckCircle2,
  Terminal, Save, Shield, Clock, Trash2, Config, Play, X,
} from '@icons'

// ── shared styles (same vocabulary as AdminPage) ─────────────────────────────

const DIM: React.CSSProperties = {
  fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase',
}
const CELL: React.CSSProperties = {
  padding: '6px 10px', fontSize: '0.68rem', color: 'var(--text-muted)', borderBottom: '1px solid var(--border)',
  verticalAlign: 'top',
}
const MONO: React.CSSProperties = { fontFamily: 'var(--font-mono)' }
const RED = '#ef4444'
const AMBER = 'var(--yellow)'
const OK = 'var(--accent)'

function CardHeader({ icon, label, badge }: { icon: React.ReactNode; label: string; badge?: React.ReactNode }) {
  return (
    <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)', flexWrap: 'wrap' }}>
      {icon}
      <span style={{ ...DIM, fontSize: '0.65rem' }}>{label}</span>
      {badge && <span className="ml-auto">{badge}</span>}
    </div>
  )
}

function Pill({ color, children }: { color: string; children: React.ReactNode }) {
  return (
    <span style={{
      fontSize: '0.58rem', color, border: `1px solid ${color}`, padding: '1px 7px', borderRadius: 2,
      letterSpacing: '0.08em', textTransform: 'uppercase', whiteSpace: 'nowrap', ...MONO,
    }}>{children}</span>
  )
}

function Dot({ state }: { state: 'ok' | 'warn' | 'bad' | 'off' }) {
  const c = state === 'ok' ? OK : state === 'warn' ? AMBER : state === 'bad' ? RED : 'var(--text-dim)'
  return <span style={{ display: 'inline-block', width: 7, height: 7, borderRadius: '50%', background: c, flexShrink: 0 }} />
}

function Btn({ onClick, children, danger, disabled, title }: {
  onClick: () => void; children: React.ReactNode; danger?: boolean; disabled?: boolean; title?: string
}) {
  return (
    <button onClick={onClick} disabled={disabled} title={title} style={{
      display: 'inline-flex', alignItems: 'center', gap: 5, fontSize: '0.64rem', letterSpacing: '0.05em',
      color: danger ? RED : 'var(--text)', background: 'none',
      border: `1px solid ${danger ? 'rgba(239,68,68,0.4)' : 'var(--border)'}`,
      padding: '0.3rem 0.7rem', borderRadius: 3, cursor: disabled ? 'not-allowed' : 'pointer',
      opacity: disabled ? 0.5 : 1, whiteSpace: 'nowrap',
    }}>{children}</button>
  )
}

/** A button that asks once, inline, before doing something disruptive. */
function ConfirmBtn({ label, icon, confirm, onConfirm, danger = true, disabled }: {
  label: string; icon?: React.ReactNode; confirm: string; onConfirm: () => void; danger?: boolean; disabled?: boolean
}) {
  const [asking, setAsking] = useState(false)
  if (!asking) return <Btn danger={danger} disabled={disabled} onClick={() => setAsking(true)}>{icon}{label}</Btn>
  return (
    <span style={{
      display: 'inline-flex', alignItems: 'center', gap: 6, flexWrap: 'wrap', padding: '0.25rem 0.5rem',
      background: 'rgba(239,68,68,0.06)', border: '1px solid rgba(239,68,68,0.25)', borderRadius: 3,
    }}>
      <span style={{ fontSize: '0.64rem', color: danger ? RED : 'var(--text)' }}>{confirm}</span>
      <button onClick={() => { setAsking(false); onConfirm() }} style={{
        fontSize: '0.62rem', color: '#fff', background: danger ? RED : 'var(--accent-dim)', border: 'none',
        padding: '0.2rem 0.6rem', borderRadius: 3, cursor: 'pointer',
      }}>Confirm</button>
      <button onClick={() => setAsking(false)} style={{
        fontSize: '0.62rem', color: 'var(--text-dim)', background: 'none', border: '1px solid var(--border)',
        padding: '0.2rem 0.6rem', borderRadius: 3, cursor: 'pointer',
      }}>Cancel</button>
    </span>
  )
}

// ── formatting ──────────────────────────────────────────────────────────────

function fmtDur(secs?: number | null): string {
  if (secs == null || !isFinite(secs)) return '—'
  const s = Math.max(0, Math.floor(secs))
  const d = Math.floor(s / 86400), h = Math.floor((s % 86400) / 3600), m = Math.floor((s % 3600) / 60)
  if (d) return `${d}d ${h}h`
  if (h) return `${h}h ${m}m`
  if (m) return `${m}m`
  return `${s}s`
}
function fmtBytes(n?: number | null): string {
  if (n == null) return '—'
  if (n >= 1024 ** 3) return `${(n / 1024 ** 3).toFixed(1)} GB`
  if (n >= 1024 ** 2) return `${(n / 1024 ** 2).toFixed(1)} MB`
  return `${Math.round(n / 1024)} KB`
}
function fmtWhen(iso?: string | number | null): string {
  if (iso == null || iso === '') return '—'
  const d = typeof iso === 'number' ? new Date(iso * 1000) : new Date(iso)
  if (isNaN(d.getTime())) return String(iso)
  return d.toLocaleString([], { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' })
}
const short = (sha?: string | null) => (sha ? sha.slice(0, 10) : '—')

type Freshness = 'current' | 'staged' | 'available' | 'unknown' | 'none'

function freshness(latestSha: string | undefined, liveSha: string | null | undefined, pending?: OpsPending | null): Freshness {
  if (!latestSha) return 'none'
  if (!liveSha) return 'unknown'
  if (liveSha === latestSha) return 'current'
  if (pending?.sha256 === latestSha) return 'staged'
  return 'available'
}

function FreshPill({ f }: { f: Freshness }) {
  switch (f) {
    case 'current':   return <Pill color={OK}>up to date</Pill>
    case 'staged':    return <Pill color={AMBER}>staged</Pill>
    case 'available': return <Pill color={AMBER}>update available</Pill>
    case 'unknown':   return <Pill color="var(--text-dim)">not checked</Pill>
    default:          return <Pill color="var(--text-dim)">no release</Pill>
  }
}

function PendingNote({ p }: { p?: OpsPending | null }) {
  if (!p) return <span style={{ color: 'var(--text-dim)' }}>—</span>
  const who = p.source === 'autoupdate' ? `auto-update ${p.tag ?? ''}` : p.source === 'rollback' ? 'rollback' : `upload by ${p.uploader ?? '?'}`
  return (
    <span style={{ color: AMBER }}>
      {who} · <span style={MONO}>{short(p.sha256)}</span>
    </span>
  )
}

// ── action plumbing ─────────────────────────────────────────────────────────

function useAction() {
  const qc = useQueryClient()
  const [busy, setBusy] = useState<string | null>(null)
  const [result, setResult] = useState<{ ok: boolean; message: string } | null>(null)
  async function run(key: string, fn: () => Promise<OpsResult>) {
    setBusy(key)
    setResult(null)
    try {
      const r = await fn()
      setResult({ ok: r.ok !== false, message: r.message ?? r.error ?? 'done' })
    } catch (e) {
      setResult({ ok: false, message: e instanceof Error ? e.message : String(e) })
    } finally {
      setBusy(null)
      // a restart takes a moment to show up; refresh now and again shortly
      qc.invalidateQueries({ queryKey: ['ops', 'status'] })
      setTimeout(() => qc.invalidateQueries({ queryKey: ['ops', 'status'] }), 4000)
    }
  }
  return { busy, result, run, clear: () => setResult(null) }
}

function ResultLine({ result, onClose }: { result: { ok: boolean; message: string } | null; onClose: () => void }) {
  if (!result) return null
  return (
    <div style={{
      display: 'flex', alignItems: 'flex-start', gap: 8, padding: '0.5rem 0.75rem', margin: '0 0 12px',
      border: `1px solid ${result.ok ? 'var(--border)' : 'rgba(239,68,68,0.4)'}`, borderRadius: 3,
      fontSize: '0.68rem', color: result.ok ? OK : RED, background: 'var(--bg-elevated)',
    }}>
      <span style={{ flex: 1, whiteSpace: 'pre-wrap' }}>{result.ok ? '✓ ' : '✗ '}{result.message}</span>
      <button onClick={onClose} style={{ background: 'none', border: 'none', color: 'var(--text-dim)', cursor: 'pointer', padding: 0 }}><X size={11} /></button>
    </div>
  )
}

// ── cards ───────────────────────────────────────────────────────────────────

function Tile({ label, state, value, sub }: { label: string; state: 'ok' | 'warn' | 'bad' | 'off'; value: string; sub?: string }) {
  return (
    <div className="vs-card" style={{ padding: '12px 14px', minWidth: 0 }}>
      <div className="flex items-center gap-2" style={{ marginBottom: 6 }}>
        <Dot state={state} />
        <span style={DIM}>{label}</span>
      </div>
      <div style={{ fontSize: '0.9rem', color: 'var(--text)', ...MONO, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{value}</div>
      {sub && <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 3 }}>{sub}</div>}
    </div>
  )
}

function SummaryTiles({ s }: { s: OpsStatus }) {
  const svc = s.host.service
  const svcState: 'ok' | 'warn' | 'bad' = !svc.installed ? 'bad'
    : (svc.start_type ?? '').includes('AUTO') && svc.this_process_is_service ? 'ok' : 'warn'
  const b = s.bfdb
  const bfdbState = !b.managed ? 'off' : b.healthy ? (b.probation ? 'warn' : 'ok') : 'bad'
  const u = s.updates
  const upState = !u ? 'off' : !u.config.enabled ? 'off' : u.config.paused ? 'warn' : u.last_check && !u.last_check.ok ? 'bad' : 'ok'
  return (
    <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(170px, 1fr))', gap: 12 }}>
      <Tile label="Server box" state={s.host.reboot_pending ? 'warn' : 'ok'}
        value={`up ${fmtDur(s.host.uptime_secs)}`}
        sub={`booted ${fmtWhen(s.host.boot_time)}${s.host.reboot_pending ? ' · reboot pending' : ''}`} />
      <Tile label="Auto-start service" state={svcState}
        value={!svc.installed ? 'not installed' : (svc.state ?? '?').toLowerCase()}
        sub={!svc.installed ? 'survives no reboot yet' : svc.this_process_is_service ? `${svc.start_type ?? ''}`.toLowerCase() : 'bot is NOT running as the service'} />
      <Tile label="bfdb" state={bfdbState}
        value={!b.managed ? 'not managed' : b.healthy ? `healthy · ${fmtDur(b.uptime_secs)}` : b.running ? 'not answering' : 'down'}
        sub={b.probation ? `on probation (${b.probation.tag ?? 'manual upload'})` : `${b.relaunches ?? 0} relaunch(es) since bot start`} />
      <Tile label="Auto-update" state={upState}
        value={!u ? 'not loaded' : !u.config.enabled ? 'off' : u.config.paused ? 'paused' : u.latest?.tag ?? 'no release yet'}
        sub={u?.last_check ? `checked ${fmtWhen(u.last_check.at)}` : undefined} />
      {s.issues && (
        <Tile label="Issues" state={s.issues.new ? 'warn' : s.issues.errors ? 'bad' : 'ok'}
          value={`${s.issues.open} open${s.issues.new ? ` · ${s.issues.new} new` : ''}`}
          sub={s.issues.last_scan ? `logs scanned ${fmtWhen(s.issues.last_scan)}` : 'not scanned yet'} />
      )}
    </div>
  )
}

function HostCard({ s }: { s: OpsStatus }) {
  const h = s.host
  const svc = h.service
  const warnings: string[] = []
  if (!svc.installed) warnings.push('The bot is not installed as a Windows service, so after a reboot or blue screen nothing comes back until someone logs in. Install Fowl Engine Manager on this PC and run its Setup. (If the Manager is already installed, the bot is still running the plugin from before 0.1.5 -- restart the bot from the Manager so it picks up the current one.)')
  else {
    if (!(svc.start_type ?? '').includes('AUTO')) warnings.push(`The ${svc.name} service is set to ${svc.start_type ?? '?'} -- it must be automatic (delayed) to start on boot.`)
    if (!svc.this_process_is_service) warnings.push(`A ${svc.name} service exists, but this bot process was started by hand, not by it. Stop this one and start the service, or two bots will fight over DCS.`)
    if (svc.restart_on_failure === false) warnings.push(svc.name === 'FowlEngine' ? 'The service has no restart-on-failure action; reinstall it from Fowl Engine Manager (Setup, step 3).' : 'The service has no restart-on-failure action; re-run install-service.ps1.')
  }
  if (h.auto_reboot_on_bsod === false) warnings.push('Windows is set NOT to restart after a blue screen (System > Advanced > Startup and Recovery > "Automatically restart"). The box will sit on the crash screen.')
  if (h.reboot_pending) warnings.push('Windows has an update waiting for a reboot. It will restart on its own schedule -- set Active Hours so that lands outside play time.')
  if (h.disk && h.disk.free / h.disk.total < 0.1) warnings.push(`Disk ${h.disk.path} is ${Math.round(100 - (100 * h.disk.free) / h.disk.total)}% full -- DB snapshots and logs need room.`)
  const rows: [string, React.ReactNode][] = [
    ['Host', <span style={MONO}>{h.hostname}</span>],
    ['OS', h.os],
    ['Booted', `${fmtWhen(h.boot_time)} (${fmtDur(h.uptime_secs)} ago)`],
    ['CPU / RAM', `${h.cpu_percent ?? '—'}% · ${h.memory ? `${h.memory.percent}% of ${fmtBytes(h.memory.total)}` : '—'}`],
    ['Disk', h.disk ? `${fmtBytes(h.disk.free)} free of ${fmtBytes(h.disk.total)} (${h.disk.path})` : '—'],
    ['Service', svc.installed ? `${svc.name}: ${svc.state ?? '?'} · ${svc.start_type ?? '?'} · as ${svc.account ?? '?'}` : `${svc.name}: not installed`],
    ['Bot process', `pid ${s.bot.pid}${svc.this_process_is_service ? ' (service)' : ' (interactive)'} · DCSServerBot ${s.bot.dcsserverbot_version ?? '?'} · node ${s.bot.node ?? '?'}`],
    ['Restart after BSOD', h.auto_reboot_on_bsod == null ? '—' : h.auto_reboot_on_bsod ? 'yes' : 'NO'],
  ]
  return (
    <div className="vs-card">
      <CardHeader icon={<Shield size={13} style={{ color: OK }} />} label="Server box & auto-start" />
      <div className="p-4">
        {warnings.map((w, i) => (
          <div key={i} className="flex gap-2" style={{ fontSize: '0.67rem', color: AMBER, marginBottom: 8, lineHeight: 1.5 }}>
            <Alert size={12} style={{ flexShrink: 0, marginTop: 2 }} /> <span>{w}</span>
          </div>
        ))}
        <table style={{ width: '100%', borderCollapse: 'collapse' }}>
          <tbody>
            {rows.map(([k, v]) => (
              <tr key={k}>
                <td style={{ ...CELL, ...DIM, width: 150, whiteSpace: 'nowrap' }}>{k}</td>
                <td style={{ ...CELL, color: 'var(--text)' }}>{v}</td>
              </tr>
            ))}
          </tbody>
        </table>
        {h.unexpected_shutdowns.length > 0 && (
          <div style={{ marginTop: 12 }}>
            <div style={{ ...DIM, marginBottom: 6 }}>Recent crashes / power losses (Windows event log)</div>
            {h.unexpected_shutdowns.map((e, i) => (
              <div key={i} style={{ fontSize: '0.65rem', color: 'var(--text-muted)', ...MONO, padding: '2px 0' }}>
                {e.date} · {e.kind} (event {e.id})
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  )
}

function ProcessesCard({ s }: { s: OpsStatus }) {
  const act = useAction()
  const b = s.bfdb
  return (
    <div className="vs-card">
      <CardHeader icon={<Activity size={13} style={{ color: OK }} />} label="Processes"
        badge={<span style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>DCS servers restart themselves on crash; bfdb is relaunched by the bot</span>} />
      <div className="p-4">
        <ResultLine result={act.result} onClose={act.clear} />
        <div className="flex items-center gap-3" style={{ flexWrap: 'wrap', marginBottom: 14 }}>
          <Dot state={!b.managed ? 'off' : b.healthy ? 'ok' : 'bad'} />
          <span style={{ fontSize: '0.72rem', color: 'var(--text)', ...MONO }}>bfdb</span>
          <span style={{ fontSize: '0.65rem', color: 'var(--text-muted)' }}>
            {!b.managed ? 'not managed by the bot (bfdb.manage: false)'
              : `${b.running ? `pid ${b.pid} · up ${fmtDur(b.uptime_secs)}` : 'not running'} · ${b.relaunches ?? 0} relaunch(es)`
                + (b.last_exit_code != null ? ` · last exit ${b.last_exit_code}` : '')
                + ` · netidx resolver ${b.resolver_listening ? 'listening' : 'DOWN'}`}
          </span>
          <span className="ml-auto flex gap-2" style={{ flexWrap: 'wrap' }}>
            <ConfirmBtn label={act.busy === 'bfdb' ? 'Restarting…' : 'Restart bfdb'} icon={<RotateCw size={11} />}
              confirm="Restart bfdb? The dashboard and GCI blip for a few seconds." disabled={!b.managed || act.busy !== null}
              onConfirm={() => act.run('bfdb', api.ops.bfdbRestart)} />
            <ConfirmBtn label={act.busy === 'bot' ? 'Restarting…' : 'Restart bot'} icon={<RotateCcw size={11} />}
              confirm="Restart the bot? DCS keeps running; bfdb restarts with it." disabled={act.busy !== null}
              onConfirm={() => act.run('bot', api.ops.botRestart)} />
          </span>
        </div>
        <div style={{ overflowX: 'auto' }}>
          <table style={{ width: '100%', borderCollapse: 'collapse', minWidth: 640 }}>
            <thead>
              <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
                {['DCS server', 'Status', 'Players', 'Mission', 'Next restart', 'Node'].map(h => (
                  <th key={h} style={{ ...CELL, color: 'var(--text-dim)', textAlign: 'left', fontWeight: 600 }}>{h}</th>
                ))}
              </tr>
            </thead>
            <tbody>
              {s.servers.map(sv => (
                <tr key={sv.name}>
                  <td style={{ ...CELL, color: 'var(--text)' }}>{sv.name}{sv.kind === 'range' && <span style={{ color: 'var(--text-dim)' }}> · range</span>}</td>
                  <td style={CELL}>
                    <span className="flex items-center gap-2">
                      <Dot state={sv.status === 'RUNNING' ? 'ok' : sv.status === 'PAUSED' || sv.status === 'LOADING' ? 'warn' : 'off'} />
                      {sv.status.toLowerCase()}
                    </span>
                  </td>
                  <td style={{ ...CELL, ...MONO }}>{sv.players ?? '—'}</td>
                  <td style={CELL}>{sv.mission ?? '—'}</td>
                  <td style={CELL}>{fmtWhen(sv.restart_time)}</td>
                  <td style={{ ...CELL, ...MONO }}>{sv.node ?? '—'}{sv.remote ? ' (remote)' : ''}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}

function EnginesCard({ s }: { s: OpsStatus }) {
  const act = useAction()
  const latest = s.updates?.latest ?? null
  const lf = latest?.files ?? {}
  const b = s.bfdb
  const running = (sv: OpsServer) => sv.status === 'RUNNING' || sv.status === 'PAUSED'
  return (
    <div className="vs-card">
      <CardHeader icon={<Server size={13} style={{ color: OK }} />} label="Engine builds"
        badge={latest && <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)' }}>latest release <span style={{ ...MONO, color: 'var(--text)' }}>{latest.tag}</span></span>} />
      <div className="p-4">
        <ResultLine result={act.result} onClose={act.clear} />
        <div style={{ overflowX: 'auto' }}>
          <table style={{ width: '100%', borderCollapse: 'collapse', minWidth: 760 }}>
            <thead>
              <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
                {['Binary', 'Running build', 'On disk', 'Staged', 'vs latest', ''].map(h => (
                  <th key={h} style={{ ...CELL, color: 'var(--text-dim)', textAlign: 'left', fontWeight: 600 }}>{h}</th>
                ))}
              </tr>
            </thead>
            <tbody>
              {b.managed && (
                <tr>
                  <td style={{ ...CELL, color: 'var(--text)', ...MONO }}>bfdb.exe</td>
                  <td style={{ ...CELL, ...MONO }}>{s.bfdb_build ? `${s.bfdb_build.git} · ${fmtWhen(s.bfdb_build.built)}` : '—'}</td>
                  <td style={{ ...CELL, ...MONO }}>{short(b.exe_sha256)}</td>
                  <td style={CELL}><PendingNote p={b.pending} /></td>
                  <td style={CELL}><FreshPill f={freshness(lf['bfdb.exe']?.sha256, b.exe_sha256, b.pending)} /></td>
                  <td style={{ ...CELL, textAlign: 'right' }}>
                    <span className="inline-flex gap-2" style={{ flexWrap: 'wrap', justifyContent: 'flex-end' }}>
                      {b.pending && (
                        <>
                          <ConfirmBtn label="Apply now" icon={<Play size={11} />} danger={false}
                            confirm="Restart bfdb onto the staged build? (DB is snapshotted first)" disabled={act.busy !== null}
                            onConfirm={() => act.run('bfdb-apply', () => api.ops.apply('bfdb'))} />
                          <Btn onClick={() => act.run('bfdb-cancel', () => api.ops.cancelStaged('bfdb.exe'))} disabled={act.busy !== null} title="Discard the staged bfdb.exe"><Trash2 size={11} /></Btn>
                        </>
                      )}
                      <ConfirmBtn label="Roll back" icon={<RotateCcw size={11} />}
                        confirm="Put the previous bfdb.exe back and restore the DB snapshot from before the last update?"
                        disabled={act.busy !== null || !(s.backups?.bfdb_exe.length)}
                        onConfirm={() => act.run('bfdb-rollback', () => api.ops.rollback('bfdb'))} />
                    </span>
                  </td>
                </tr>
              )}
              {s.servers.filter(sv => sv.dll_name).map(sv => (
                <tr key={sv.name}>
                  <td style={{ ...CELL, color: 'var(--text)' }}>
                    <span style={MONO}>{sv.dll_name}</span>
                    <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>{sv.name}</div>
                    {!sv.has_bfbinaries && <div style={{ fontSize: '0.6rem', color: AMBER }}>no BFBinaries extension -- nothing swaps it at start</div>}
                  </td>
                  <td style={{ ...CELL, ...MONO }}>
                    {sv.loaded_build ? `${sv.loaded_build.git} · ${fmtWhen(sv.loaded_build.built)}` : sv.remote ? 'remote node' : '—'}
                  </td>
                  <td style={{ ...CELL, ...MONO }}>{sv.remote ? '—' : short(sv.dll_sha256)}</td>
                  <td style={CELL}><PendingNote p={sv.pending} /></td>
                  <td style={CELL}><FreshPill f={sv.remote ? 'unknown' : freshness(lf[sv.dll_name ?? '']?.sha256, sv.dll_sha256, sv.pending)} /></td>
                  <td style={{ ...CELL, textAlign: 'right' }}>
                    <span className="inline-flex gap-2" style={{ flexWrap: 'wrap', justifyContent: 'flex-end' }}>
                      {sv.pending && (
                        <>
                          <ConfirmBtn label="Apply now" icon={<Play size={11} />}
                            confirm={running(sv) ? `Restart DCS on ${sv.name} now?${sv.players ? ` ${sv.players} player(s) will be disconnected.` : ''}` : `Start ${sv.name} with the staged engine?`}
                            disabled={act.busy !== null}
                            onConfirm={() => act.run(`apply-${sv.name}`, () => api.ops.apply('dll', sv.name))} />
                          <Btn onClick={() => act.run(`cancel-${sv.name}`, () => api.ops.cancelStaged(sv.dll_name ?? '', sv.name))} disabled={act.busy !== null} title="Discard the staged DLL"><Trash2 size={11} /></Btn>
                        </>
                      )}
                      {!sv.remote && (
                        <ConfirmBtn label="Roll back" icon={<RotateCcw size={11} />}
                          confirm={`Restore ${sv.name}'s previous ${sv.dll_name} and restart DCS?`}
                          disabled={act.busy !== null || !(sv.backups?.length)}
                          onConfirm={() => act.run(`rollback-${sv.name}`, () => api.ops.rollback('dll', sv.name))} />
                      )}
                    </span>
                  </td>
                </tr>
              ))}
              {s.bftools && (
                <tr>
                  <td style={{ ...CELL, color: 'var(--text)', ...MONO }}>bftools.exe</td>
                  <td style={CELL}>—</td>
                  <td style={{ ...CELL, ...MONO }}>{short(s.bftools.sha256)}</td>
                  <td style={CELL}><PendingNote p={s.bftools.pending} /></td>
                  <td style={CELL}><FreshPill f={freshness(lf['bftools.exe']?.sha256, s.bftools.sha256, s.bftools.pending)} /></td>
                  <td style={{ ...CELL, textAlign: 'right' }}>
                    {s.bftools.pending && <Btn onClick={() => act.run('bftools', () => api.ops.apply('bftools'))} disabled={act.busy !== null}><Play size={11} />Apply now</Btn>}
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
        <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 10, lineHeight: 1.6 }}>
          "Running build" is what is actually loaded (bfdb's /api/version, bflib's load sidecar); "On disk" is the file's hash.
          A staged engine DLL swaps in whenever that DCS server next starts; bfdb.exe whenever bfdb next starts.
          Every swap goes on probation and is rolled back by itself if it crashes or never loads.
        </div>
      </div>
    </div>
  )
}

const POLICY_LABEL: Record<OpsApplyPolicy, string> = {
  next_restart: 'At the next scheduled restart',
  when_idle: 'As soon as the server is empty',
  immediately: 'Immediately (kicks players)',
}

function AutoUpdateCard({ s }: { s: OpsStatus }) {
  const act = useAction()
  const u = s.updates
  const [draft, setDraft] = useState<Partial<OpsUpdateConfig>>({})
  if (!u) {
    return (
      <div className="vs-card"><CardHeader icon={<Download size={13} style={{ color: OK }} />} label="Automatic updates" />
        <div className="p-4" style={DIM}>The auto-updater is not loaded in the bot.</div></div>
    )
  }
  const c = { ...u.config, ...draft }
  const dirty = Object.keys(draft).length > 0
  const set = <K extends keyof OpsUpdateConfig>(k: K, v: OpsUpdateConfig[K]) => setDraft(d => ({ ...d, [k]: v }))
  const field = (label: string, input: React.ReactNode, hint?: string) => (
    <label style={{ display: 'flex', flexDirection: 'column', gap: 4, minWidth: 0 }}>
      <span style={DIM}>{label}</span>
      {input}
      {hint && <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>{hint}</span>}
    </label>
  )
  const inputStyle: React.CSSProperties = {
    background: 'var(--bg-input)', border: '1px solid var(--border)', color: 'var(--text)',
    padding: '0.35rem 0.5rem', fontSize: '0.7rem', borderRadius: 2, width: '100%', boxSizing: 'border-box',
  }
  const check = (k: 'enabled' | 'paused' | 'rollback_on_crash', label: string) => (
    <label className="flex items-center gap-2" style={{ fontSize: '0.7rem', color: 'var(--text)', cursor: 'pointer' }}>
      <input type="checkbox" checked={!!c[k]} onChange={e => set(k, e.target.checked)} />
      {label}
    </label>
  )
  const src = u.config.source === 'github' ? `github.com/${u.config.repo}` : u.config.folder
  return (
    <div className="vs-card">
      <CardHeader icon={<Download size={13} style={{ color: OK }} />} label="Automatic updates"
        badge={u.busy ? <Pill color={AMBER}>{u.busy}…</Pill> : <Pill color={u.config.enabled && !u.config.paused ? OK : 'var(--text-dim)'}>{!u.config.enabled ? 'off' : u.config.paused ? 'paused' : 'on'}</Pill>} />
      <div className="p-4 flex flex-col gap-4">
        <ResultLine result={act.result} onClose={act.clear} />
        <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
          Source <span style={{ ...MONO, color: 'var(--text)' }}>{src}</span> · tags <span style={MONO}>{u.config.tag_prefix}*</span> ·
          files {u.config.files.join(', ')}{u.config.has_token ? ' · token set' : ''}.
          {' '}Where to fetch from is set in fowlengine.yaml (<span style={MONO}>autoupdate:</span>); the switches below override it.
        </div>
        <div className="flex gap-4" style={{ flexWrap: 'wrap' }}>
          {check('enabled', 'Check for and stage new releases')}
          {check('paused', 'Paused')}
          {check('rollback_on_crash', 'Roll back an engine that crashes DCS')}
        </div>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))', gap: 12 }}>
          {field('Channel', (
            <select style={inputStyle} value={c.channel} onChange={e => set('channel', e.target.value as 'stable' | 'beta')}>
              <option value="stable">stable</option>
              <option value="beta">beta (also pre-releases)</option>
            </select>
          ))}
          {field('Engine DLL applies', (
            <select style={inputStyle} value={c.apply} onChange={e => set('apply', e.target.value as OpsApplyPolicy)}>
              {Object.entries(POLICY_LABEL).map(([k, v]) => <option key={k} value={k}>{v}</option>)}
            </select>
          ), 'A DLL needs a full DCS restart to load.')}
          {field('bfdb.exe applies', (
            <select style={inputStyle} value={c.bfdb_apply} onChange={e => set('bfdb_apply', e.target.value as OpsApplyPolicy)}>
              {Object.entries(POLICY_LABEL).map(([k, v]) => <option key={k} value={k}>{v}</option>)}
            </select>
          ), 'Players stay connected; GCI/dashboard blip.')}
          {field('Empty for (minutes)', (
            <input style={inputStyle} type="number" min={0} value={c.idle_minutes}
              onChange={e => set('idle_minutes', Number(e.target.value))} />
          ), 'Before "when empty" restarts anything.')}
          {field('Only between (local)', (
            <input style={inputStyle} placeholder="e.g. 03:00-08:00" value={c.apply_window ?? ''}
              onChange={e => set('apply_window', e.target.value || null)} />
          ), 'Blank = any time.')}
          {field('Check every (minutes)', (
            <input style={inputStyle} type="number" min={1} value={c.check_minutes}
              onChange={e => set('check_minutes', Number(e.target.value))} />
          ))}
        </div>
        <div className="flex gap-2" style={{ flexWrap: 'wrap' }}>
          <Btn disabled={!dirty || act.busy !== null} onClick={() => act.run('save', () => api.ops.settings(draft)).then(() => setDraft({}))}>
            <Save size={11} />Save settings
          </Btn>
          {dirty && <Btn onClick={() => setDraft({})}>Discard</Btn>}
          <Btn disabled={act.busy !== null} onClick={() => act.run('check', api.ops.check)}>
            <RefreshCw size={11} />{act.busy === 'check' ? 'Checking…' : 'Check now'}
          </Btn>
          <ConfirmBtn label="Reset to fowlengine.yaml" danger={false} confirm="Drop the page overrides and use fowlengine.yaml as-is?"
            disabled={act.busy !== null} onConfirm={() => act.run('reset', api.ops.resetSettings)} />
        </div>

        <div style={{ borderTop: '1px solid var(--border)', paddingTop: 12 }}>
          <div style={{ ...DIM, marginBottom: 6 }}>Last check</div>
          <div style={{ fontSize: '0.68rem', color: u.last_check?.ok === false ? RED : 'var(--text-muted)' }}>
            {u.last_check ? `${fmtWhen(u.last_check.at)} (${u.last_check.reason ?? ''}) — ${u.last_check.message ?? u.last_check.error ?? ''}` : 'never'}
          </div>
          {u.latest && (
            <div style={{ marginTop: 10, fontSize: '0.68rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
              Latest release <span style={{ ...MONO, color: 'var(--text)' }}>{u.latest.tag}</span>
              {' '}· commit <span style={MONO}>{u.latest.git ?? '?'}</span> · built {fmtWhen(u.latest.built)}
              {u.latest.html_url && <> · <a href={u.latest.html_url} target="_blank" rel="noreferrer" style={{ color: OK }}>release page</a></>}
              {u.latest.notes && <div style={{ whiteSpace: 'pre-wrap', marginTop: 6, color: 'var(--text-dim)', maxHeight: 140, overflowY: 'auto' }}>{u.latest.notes}</div>}
            </div>
          )}
        </div>

        {u.probation.length > 0 && (
          <div style={{ borderTop: '1px solid var(--border)', paddingTop: 12 }}>
            <div style={{ ...DIM, marginBottom: 6 }}>On probation</div>
            {u.probation.map((p, i) => (
              <div key={i} className="flex items-center gap-2" style={{ fontSize: '0.68rem', color: 'var(--text)', padding: '2px 0', flexWrap: 'wrap' }}>
                <Clock size={11} style={{ color: AMBER }} />
                <span style={MONO}>{p.dll}</span> on {p.server ?? 'this box'} · {p.tag ?? 'manual upload'} ·{' '}
                {p.loaded ? (p.passes_in_secs != null ? `loaded, passes in ${fmtDur(p.passes_in_secs)}` : 'answering') : 'waiting for it to load'}
                {p.crashes > 0 && <span style={{ color: RED }}> · {p.crashes} crash(es)</span>}
              </div>
            ))}
          </div>
        )}

        {u.bad.length > 0 && (
          <div style={{ borderTop: '1px solid var(--border)', paddingTop: 12 }}>
            <div style={{ ...DIM, marginBottom: 6 }}>Rolled-back releases (never staged again)</div>
            {u.bad.map(tag => (
              <div key={tag} className="flex items-center gap-2" style={{ fontSize: '0.68rem', padding: '2px 0' }}>
                <span style={{ ...MONO, color: RED }}>{tag}</span>
                <Btn onClick={() => act.run(`unmark-${tag}`, () => api.ops.unmark(tag))} disabled={act.busy !== null}>Allow again</Btn>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  )
}

const EVENT_COLOR: Record<string, string> = {
  rollback: RED, marked_bad: RED, probation_passed: 'var(--accent)', found: 'var(--text)',
  staged: 'var(--yellow)', applied: 'var(--accent)', dcs_restart: 'var(--yellow)',
}

function HistoryCard({ s }: { s: OpsStatus }) {
  const h = s.updates?.history ?? []
  const snaps = s.backups?.db_snapshots ?? []
  return (
    <div className="vs-card">
      <CardHeader icon={<Clock size={13} style={{ color: OK }} />} label="Update history & backups" />
      <div className="p-4" style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))', gap: 16 }}>
        <div>
          <div style={{ ...DIM, marginBottom: 6 }}>Recent events</div>
          {h.length === 0 && <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>Nothing yet.</div>}
          <div style={{ maxHeight: 280, overflowY: 'auto' }}>
            {h.map((e, i) => (
              <div key={i} style={{ fontSize: '0.64rem', padding: '3px 0', borderBottom: '1px solid rgba(255,255,255,0.03)', lineHeight: 1.5 }}>
                <span style={{ ...MONO, color: 'var(--text-dim)' }}>{fmtWhen(e.ts)}</span>{' '}
                <span style={{ color: EVENT_COLOR[e.event] ?? 'var(--text-muted)', ...MONO }}>{e.event}</span>{' '}
                <span style={{ color: 'var(--text-muted)' }}>{e.detail}</span>
              </div>
            ))}
          </div>
        </div>
        <div>
          <div style={{ ...DIM, marginBottom: 6 }}>bfdb DB snapshots (taken before every bfdb update)</div>
          {snaps.length === 0 && <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>None yet.</div>}
          {snaps.map(sn => (
            <div key={sn.name} style={{ fontSize: '0.64rem', ...MONO, color: 'var(--text-muted)', padding: '2px 0' }}>{sn.name}</div>
          ))}
          <div style={{ ...DIM, margin: '12px 0 6px' }}>bfdb.exe backups</div>
          {(s.backups?.bfdb_exe ?? []).map(b => (
            <div key={b.name} style={{ fontSize: '0.64rem', ...MONO, color: 'var(--text-muted)', padding: '2px 0' }}>{b.name} · {fmtBytes(b.size)}</div>
          ))}
        </div>
      </div>
    </div>
  )
}

function ConfigCard() {
  const [doc, setDoc] = useState<OpsConfigDoc | null>(null)
  const [text, setText] = useState('')
  const [loadErr, setLoadErr] = useState<string | null>(null)
  const [restartBfdb, setRestartBfdb] = useState(true)
  const act = useAction()
  async function load() {
    setLoadErr(null)
    try {
      const d = await api.ops.config()
      setDoc(d)
      setText(d.yaml)
    } catch (e) {
      setLoadErr(e instanceof Error ? e.message : String(e))
    }
  }
  const dirty = doc !== null && text !== doc.yaml
  return (
    <div className="vs-card">
      <CardHeader icon={<Config size={13} style={{ color: OK }} />} label="Bot config (fowlengine.yaml)"
        badge={doc && <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', ...MONO }}>{doc.path}</span>} />
      <div className="p-4 flex flex-col gap-3">
        <ResultLine result={act.result} onClose={act.clear} />
        {!doc && (
          <div className="flex items-center gap-3" style={{ flexWrap: 'wrap' }}>
            <Btn onClick={load}><Config size={11} />Open editor</Btn>
            <span style={{ fontSize: '0.64rem', color: 'var(--text-dim)' }}>
              Everything the bot runs with: channels, bfdb flags, GCI, instances, auto-update. Passwords and keys show as masked and are kept unless you replace them.
            </span>
          </div>
        )}
        {loadErr && <div style={{ fontSize: '0.68rem', color: RED }}>{loadErr}</div>}
        {doc && (
          <>
            <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)', lineHeight: 1.5 }}>
              {doc.masked} secret value(s) show as <span style={MONO}>{doc.mask}</span> -- leave them as they are to keep the saved value.
              Saving validates the file, keeps a backup ({doc.backups.length} kept), and reloads the plugin.
              The engine's own campaign config is on the CONFIG page.
            </div>
            <textarea value={text} onChange={e => setText(e.target.value)} spellCheck={false}
              style={{
                width: '100%', minHeight: 420, background: 'var(--bg-input)', color: 'var(--text)',
                border: '1px solid var(--border)', borderRadius: 2, padding: '0.6rem', fontSize: '0.68rem',
                lineHeight: 1.5, boxSizing: 'border-box', resize: 'vertical', ...MONO,
              }} />
            <div className="flex items-center gap-3" style={{ flexWrap: 'wrap' }}>
              <ConfirmBtn label={act.busy === 'save' ? 'Saving…' : 'Save'} icon={<Save size={11} />} danger={false}
                confirm={restartBfdb ? 'Save, reload, and restart bfdb?' : 'Save and reload the plugin?'}
                disabled={!dirty || act.busy !== null}
                onConfirm={() => act.run('save', async () => {
                  const r = await api.ops.saveConfig(text, doc.mtime, restartBfdb)
                  await load()
                  return r
                })} />
              <label className="flex items-center gap-2" style={{ fontSize: '0.66rem', color: 'var(--text)' }}>
                <input type="checkbox" checked={restartBfdb} onChange={e => setRestartBfdb(e.target.checked)} />
                restart bfdb after saving (needed for bfdb: / gci: / instances: changes)
              </label>
              <Btn onClick={load} disabled={act.busy !== null}><RefreshCw size={11} />Reload from disk</Btn>
              {dirty && <Btn onClick={() => setText(doc.yaml)}>Discard edits</Btn>}
            </div>
          </>
        )}
      </div>
    </div>
  )
}

// ── issues found by the log analyzer ─────────────────────────────────────────

const LEVEL_COLOR: Record<string, string> = { CRASH: RED, PANIC: RED, ERROR: RED, WARN: AMBER }
const STATUS_COLOR: Record<string, string> = {
  new: AMBER, regressed: RED, open: 'var(--text-muted)', acknowledged: 'var(--text-dim)',
  ignored: 'var(--text-dim)', fixed: OK, quiet: 'var(--text-dim)',
}

type IssueStatus = 'open' | 'acknowledged' | 'ignored' | 'fixed'
type IssueRun = (key: string, fn: () => Promise<OpsResult>) => Promise<unknown>

const SMALL_BTN: React.CSSProperties = {
  fontSize: '0.6rem', padding: '0.15rem 0.5rem', borderRadius: 3, cursor: 'pointer', background: 'none',
  border: '1px solid var(--border)', color: 'var(--text)', whiteSpace: 'nowrap',
}

function IssueRow({ it, run, busy, selected, onSelect }: {
  it: OpsIssue; run: IssueRun; busy: boolean; selected: boolean; onSelect: (on: boolean) => void
}) {
  const [open, setOpen] = useState(false)
  const set = (st: IssueStatus) => run(`issue-${it.id}`, () => api.ops.issueStatus(it.id, st))
  const closed = ['ignored', 'fixed', 'quiet'].includes(it.status)
  return (
    <div style={{ borderBottom: '1px solid var(--border)', padding: '8px 0', background: selected ? 'rgba(127,127,127,0.06)' : undefined }}>
      <div className="flex items-start gap-2" style={{ flexWrap: 'wrap' }}>
        <input type="checkbox" checked={selected} onChange={e => onSelect(e.target.checked)}
          aria-label={`select issue ${it.id}`} style={{ marginTop: 2, cursor: 'pointer' }} />
        <span className="flex items-start gap-2" style={{ flexWrap: 'wrap', cursor: 'pointer', flex: 1, minWidth: 0 }} onClick={() => setOpen(o => !o)}>
          <Pill color={LEVEL_COLOR[it.level] ?? 'var(--text-dim)'}>{it.level}</Pill>
          <Pill color={STATUS_COLOR[it.status] ?? 'var(--text-dim)'}>{it.status}</Pill>
          <span style={{ ...MONO, fontSize: '0.62rem', color: 'var(--text-dim)' }}>{it.id}</span>
          <span style={{ ...MONO, fontSize: '0.62rem', color: 'var(--text-muted)' }}>×{it.count}</span>
          <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)' }}>{it.sources.join(', ')} · last {fmtWhen(it.last_seen)}</span>
          {it.github_url && <a href={it.github_url} target="_blank" rel="noreferrer" onClick={e => e.stopPropagation()} style={{ fontSize: '0.62rem', color: OK }}>GitHub issue</a>}
        </span>
        <span className="flex gap-1" style={{ flexShrink: 0 }}>
          {closed
            ? <button style={SMALL_BTN} disabled={busy} onClick={() => set('open')}>Reopen</button>
            : <>
                <button style={{ ...SMALL_BTN, color: OK, borderColor: OK }} disabled={busy} onClick={() => set('fixed')}
                  title="Take it off the list. If it shows up in the logs again it comes back as REGRESSED.">Fixed</button>
                <button style={SMALL_BTN} disabled={busy} onClick={() => set('ignored')}
                  title="Hide it for good -- it stays hidden even if it keeps happening.">Ignore</button>
              </>}
        </span>
      </div>
      <div style={{ ...MONO, fontSize: '0.66rem', color: 'var(--text)', marginTop: 4, marginLeft: 22, wordBreak: 'break-word', cursor: 'pointer' }} onClick={() => setOpen(o => !o)}>
        {it.signature}
      </div>
      {open && (
        <div style={{ marginTop: 8, marginLeft: 22 }}>
          <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginBottom: 6 }}>
            first seen {fmtWhen(it.first_seen)}{it.builds.length > 0 && <> · builds {it.builds.join(', ')}</>}{it.note && <> · note: {it.note}</>}
          </div>
          {it.samples.map((s, i) => (
            <pre key={i} style={{
              margin: '0 0 6px', padding: '6px 8px', background: 'var(--bg-input)', border: '1px solid var(--border)',
              fontSize: '0.6rem', lineHeight: 1.5, maxHeight: 220, overflow: 'auto', whiteSpace: 'pre-wrap', wordBreak: 'break-all', ...MONO,
            }}>
              <span style={{ color: 'var(--text-dim)' }}>{`${s.source} · ${s.at}\n${s.context.join('\n')}${s.context.length ? '\n' : ''}`}</span>
              <span style={{ color: 'var(--text)' }}>{s.text}</span>
            </pre>
          ))}
          <div className="flex gap-2" style={{ flexWrap: 'wrap' }}>
            {it.status !== 'acknowledged' && !closed && <Btn disabled={busy} onClick={() => set('acknowledged')}>Acknowledge (keep on the list, stop calling it new)</Btn>}
            <Btn danger disabled={busy} onClick={() => run(`forget-${it.id}`, () => api.ops.issuesForget([it.id]))}><Trash2 size={11} />Delete</Btn>
          </div>
        </div>
      )}
    </div>
  )
}

function IssuesCard() {
  const act = useAction()
  const qc = useQueryClient()
  const [showClosed, setShowClosed] = useState(false)
  const [copied, setCopied] = useState<string | null>(null)
  const [picked, setPicked] = useState<Set<string>>(new Set())
  const { data, error } = useQuery({
    queryKey: ['ops', 'issues', showClosed],
    queryFn: () => api.ops.issues(showClosed),
    refetchInterval: 30_000,
    retry: false,
  })
  const run: IssueRun = (key, fn) =>
    act.run(key, fn).then(() => qc.invalidateQueries({ queryKey: ['ops', 'issues'] }))
  const issues = data?.issues ?? []
  // only ids still on screen count -- a row that disappeared can't stay selected
  const selected = issues.filter(i => picked.has(i.id)).map(i => i.id)
  const allPicked = issues.length > 0 && selected.length === issues.length
  const busy = act.busy !== null
  function pick(id: string, on: boolean) {
    setPicked(p => {
      const n = new Set(p)
      if (on) n.add(id); else n.delete(id)
      return n
    })
  }
  function bulk(key: string, fn: () => Promise<OpsResult>) {
    run(key, fn).then(() => setPicked(new Set()))
  }
  async function copyReport() {
    try {
      const text = await api.ops.issuesReport(showClosed)
      await navigator.clipboard.writeText(text)
      setCopied('Report copied -- paste it to Claude.')
    } catch (e) {
      setCopied(`Could not copy: ${e instanceof Error ? e.message : String(e)}`)
    }
  }
  async function downloadReport() {
    const text = await api.ops.issuesReport(showClosed)
    const url = URL.createObjectURL(new Blob([text], { type: 'text/markdown' }))
    const a = document.createElement('a')
    a.href = url
    a.download = `fowl-issues-${new Date().toISOString().slice(0, 16).replace(/[:T]/g, '')}.md`
    a.click()
    URL.revokeObjectURL(url)
  }
  const sum = data?.summary
  const missing = sum?.sources.filter(s => !s.exists) ?? []
  return (
    <div className="vs-card">
      <CardHeader icon={<Alert size={13} style={{ color: sum?.new ? AMBER : OK }} />} label="Issues (log analyzer)"
        badge={sum && <span className="flex gap-2">
          {sum.new > 0 && <Pill color={AMBER}>{sum.new} new</Pill>}
          <Pill color={sum.errors ? RED : 'var(--text-dim)'}>{sum.open} open</Pill>
        </span>} />
      <div className="p-4">
        <ResultLine result={act.result} onClose={act.clear} />
        {error && <div style={{ fontSize: '0.68rem', color: RED, marginBottom: 8 }}>{error instanceof Error ? error.message : String(error)}</div>}
        <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)', lineHeight: 1.6, marginBottom: 10 }}>
          Every WARN/ERROR from the engine, DCS script errors and crashes, bfdb and the bot, grouped into distinct problems.
          Hand the report to Claude to fix -- or give Claude <span style={MONO}>/api/logs/issues?token=…</span> (bfdb's <span style={MONO}>--log-read-token</span>) to fetch it directly.
          {sum?.last_scan && <> Last scan {fmtWhen(sum.last_scan)}.</>}
          {sum?.github && <> New error-level issues are also filed on GitHub.</>}
          {' '}Once a fix is deployed, mark the issue <b style={{ color: OK }}>Fixed</b>: it leaves the list, and if it ever
          shows up in the logs again it comes back as <b style={{ color: RED }}>REGRESSED</b>.
        </div>
        {missing.length > 0 && (
          <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginBottom: 8 }}>
            Not found yet: {missing.map(m => m.source).join(', ')}
          </div>
        )}
        <div className="flex gap-2" style={{ flexWrap: 'wrap', marginBottom: 10 }}>
          <Btn onClick={copyReport}><Save size={11} />Copy report for Claude</Btn>
          <Btn onClick={downloadReport}><Download size={11} />Download .md</Btn>
          <Btn disabled={busy} onClick={() => run('scan', api.ops.issuesScan)}><RefreshCw size={11} />{act.busy === 'scan' ? 'Scanning…' : 'Scan now'}</Btn>
          <label className="flex items-center gap-2" style={{ fontSize: '0.64rem', color: 'var(--text)' }}>
            <input type="checkbox" checked={showClosed} onChange={e => setShowClosed(e.target.checked)} /> show fixed / ignored / quiet
          </label>
          <span className="ml-auto flex gap-2" style={{ flexWrap: 'wrap' }}>
            <ConfirmBtn label="Delete closed" icon={<Trash2 size={11} />} confirm="Delete every fixed / ignored / quiet issue?"
              disabled={busy} onConfirm={() => run('clear', () => api.ops.issuesClear('closed'))} />
            <ConfirmBtn label="Clear all" icon={<Trash2 size={11} />}
              confirm="Delete EVERY issue and start fresh? Anything still happening is found again on the next scan."
              disabled={busy} onConfirm={() => bulk('clear-all', () => api.ops.issuesClear('all'))} />
          </span>
        </div>
        {copied && <div style={{ fontSize: '0.64rem', color: OK, marginBottom: 8 }}>{copied}</div>}
        {issues.length > 0 && (
          <div className="flex items-center gap-2" style={{
            flexWrap: 'wrap', padding: '6px 0', borderBottom: '1px solid var(--border)', position: 'sticky', top: 0,
          }}>
            <label className="flex items-center gap-2" style={{ fontSize: '0.64rem', color: 'var(--text)', cursor: 'pointer' }}>
              <input type="checkbox" checked={allPicked}
                ref={el => { if (el) el.indeterminate = selected.length > 0 && !allPicked }}
                onChange={e => setPicked(e.target.checked ? new Set(issues.map(i => i.id)) : new Set())} />
              {selected.length ? `${selected.length} selected` : 'Select all'}
            </label>
            {selected.length > 0 && <>
              <Btn disabled={busy} onClick={() => bulk('bulk-fixed', () => api.ops.issuesStatusMany(selected, 'fixed'))}>
                <CheckCircle2 size={11} />Mark fixed</Btn>
              <Btn disabled={busy} onClick={() => bulk('bulk-ignored', () => api.ops.issuesStatusMany(selected, 'ignored'))}>Ignore</Btn>
              {showClosed && <Btn disabled={busy} onClick={() => bulk('bulk-open', () => api.ops.issuesStatusMany(selected, 'open'))}>Reopen</Btn>}
              <ConfirmBtn label="Delete" icon={<Trash2 size={11} />} confirm={`Delete ${selected.length} issue(s)?`}
                disabled={busy} onConfirm={() => bulk('bulk-forget', () => api.ops.issuesForget(selected))} />
            </>}
          </div>
        )}
        {data && issues.length === 0 && <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', marginTop: 6 }}>No open issues.</div>}
        <div style={{ maxHeight: 620, overflowY: 'auto' }}>
          {issues.map(it => <IssueRow key={it.id} it={it} run={run} busy={busy}
            selected={picked.has(it.id)} onSelect={on => pick(it.id, on)} />)}
        </div>
      </div>
    </div>
  )
}

/** Browse the persistent log archive: every log, every day, surviving restarts. */
function ArchiveCard() {
  const { data: idx, error: idxErr } = useQuery({ queryKey: ['ops', 'archive'], queryFn: api.ops.archive, refetchInterval: 120_000, retry: false })
  const [source, setSource] = useState('')
  const [date, setDate] = useState('')
  const [grep, setGrep] = useState('')
  const [applied, setApplied] = useState('')
  const src = source || idx?.sources[0]?.source || ''
  const days = idx?.sources.find(s => s.source === src)?.days ?? []
  const day = date && days.some(d => d.date === date) ? date : days[0]?.date ?? ''
  const { data, isFetching, error } = useQuery({
    queryKey: ['ops', 'archive-read', src, day, applied],
    queryFn: () => api.ops.archiveRead(src, day, 2000, applied),
    enabled: !!src && !!day,
    retry: false,
  })
  const sel: React.CSSProperties = { background: 'var(--bg-input)', border: '1px solid var(--border)', color: 'var(--text)', fontSize: '0.64rem', padding: '2px 4px' }
  return (
    <div className="vs-card" style={{ display: 'flex', flexDirection: 'column' }}>
      <CardHeader icon={<Clock size={13} style={{ color: OK }} />} label="Log archive (kept across restarts)"
        badge={idx?.root && <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', ...MONO }}>{idx.root} · {idx.keep_days ? `${idx.keep_days} days` : 'forever'}</span>} />
      <div className="flex items-center gap-2" style={{ padding: '6px 12px', borderBottom: '1px solid var(--border)', flexWrap: 'wrap', background: 'rgba(0,0,0,0.15)' }}>
        <select style={sel} value={src} onChange={e => { setSource(e.target.value); setDate('') }}>
          {(idx?.sources ?? []).map(s => <option key={s.source} value={s.source}>{s.source}</option>)}
        </select>
        <select style={sel} value={day} onChange={e => setDate(e.target.value)}>
          {days.map(d => <option key={d.date} value={d.date}>{d.date} · {fmtBytes(d.size)}{d.compressed ? ' (gz)' : ''}</option>)}
        </select>
        <input style={{ ...sel, flex: 1, minWidth: 120 }} placeholder="filter (e.g. ERROR, panic, objective name)…" value={grep}
          onChange={e => setGrep(e.target.value)} onKeyDown={e => { if (e.key === 'Enter') setApplied(grep) }} />
        <Btn onClick={() => setApplied(grep)}><RefreshCw size={10} />{isFetching ? '…' : 'Show'}</Btn>
      </div>
      <pre style={{
        margin: 0, height: 380, overflow: 'auto', padding: '8px 12px', fontSize: '0.62rem', lineHeight: 1.55,
        color: 'var(--text)', whiteSpace: 'pre-wrap', wordBreak: 'break-all', ...MONO,
      }}>
        {idxErr ? `Could not list the archive: ${idxErr instanceof Error ? idxErr.message : String(idxErr)}`
          : idx && idx.sources.length === 0 ? 'Nothing archived yet -- it fills in from the next scan.'
          : error ? `Could not read: ${error instanceof Error ? error.message : String(error)}`
          : (data?.lines ?? []).join('\n') || (src ? 'Loading… (last 2000 matching lines of the day)' : '')}
      </pre>
    </div>
  )
}

const LOGS: { key: OpsLogName; label: string }[] = [
  { key: 'bot', label: 'Bot' },
  { key: 'service', label: 'Service' },
  { key: 'bfdb', label: 'bfdb' },
  { key: 'bfdb_boot', label: 'bfdb boot' },
  { key: 'netidx', label: 'netidx' },
]

function LogsCard() {
  const [which, setWhich] = useState<OpsLogName>('bot')
  const [lines, setLines] = useState(300)
  const { data, isFetching, error, refetch } = useQuery({
    queryKey: ['ops', 'logs', which, lines],
    queryFn: () => api.ops.logs(which, lines),
    refetchInterval: 15_000,
    retry: false,
  })
  return (
    <div className="vs-card" style={{ display: 'flex', flexDirection: 'column' }}>
      <CardHeader icon={<Terminal size={13} style={{ color: OK }} />} label="Logs"
        badge={data?.path && <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', ...MONO }}>{data.path}</span>} />
      <div className="flex items-center gap-2" style={{ padding: '6px 12px', borderBottom: '1px solid var(--border)', flexWrap: 'wrap', background: 'rgba(0,0,0,0.15)' }}>
        {LOGS.map(l => (
          <button key={l.key} onClick={() => setWhich(l.key)} style={{
            fontSize: '0.6rem', padding: '2px 8px', borderRadius: 2, cursor: 'pointer', ...MONO,
            background: which === l.key ? 'var(--accent)' : 'transparent',
            color: which === l.key ? '#000' : 'var(--text-dim)',
            border: `1px solid ${which === l.key ? 'var(--accent)' : 'var(--border)'}`,
          }}>{l.label}</button>
        ))}
        <select value={lines} onChange={e => setLines(Number(e.target.value))} style={{
          marginLeft: 'auto', background: 'var(--bg-input)', border: '1px solid var(--border)', color: 'var(--text)', fontSize: '0.62rem', padding: '1px 4px',
        }}>
          {[100, 300, 1000, 3000].map(n => <option key={n} value={n}>{n} lines</option>)}
        </select>
        <Btn onClick={() => refetch()}><RefreshCw size={10} />{isFetching ? '…' : ''}</Btn>
      </div>
      <pre style={{
        margin: 0, height: 420, overflow: 'auto', padding: '8px 12px', fontSize: '0.62rem', lineHeight: 1.55,
        color: 'var(--text)', whiteSpace: 'pre-wrap', wordBreak: 'break-all', ...MONO,
      }}>
        {error ? `Could not read the log: ${error instanceof Error ? error.message : String(error)}`
          : data?.missing ? `No such file yet: ${data.path}`
          : (data?.lines ?? []).join('\n') || 'Loading…'}
      </pre>
    </div>
  )
}

// ── page ────────────────────────────────────────────────────────────────────

export default function OpsPage() {
  const { user, loading } = useAuth()
  const navigate = useNavigate()
  const { data, error, dataUpdatedAt, isFetching } = useQuery({
    queryKey: ['ops', 'status'],
    queryFn: api.ops.status,
    refetchInterval: 10_000,
    retry: false,
    enabled: !!user?.is_admin,
  })

  if (loading) return null
  if (!user) { navigate('/login'); return null }
  if (!user.is_admin) { navigate('/'); return null }

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader icon={Server} title="OPS" sub="Server box, processes, engine updates and bot config" />
      <div className="flex-1 overflow-auto p-4 space-y-4">
        {error && (
          <div className="vs-card flex items-start gap-2" style={{ padding: '0.75rem 1rem', borderColor: 'rgba(239,68,68,0.4)' }}>
            <Alert size={13} style={{ color: RED, flexShrink: 0, marginTop: 2 }} />
            <div style={{ fontSize: '0.7rem', color: RED, lineHeight: 1.5 }}>
              {error instanceof Error ? error.message : String(error)}
              {data && <span style={{ color: 'var(--text-dim)' }}> -- showing the last good snapshot from {new Date(dataUpdatedAt).toLocaleTimeString()}. (bfdb or the bot may be restarting.)</span>}
            </div>
          </div>
        )}
        {!data && !error && <div style={{ ...DIM, padding: '1rem' }}>Loading…</div>}
        {data && (
          <>
            <div className="flex items-center gap-2" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
              {isFetching ? <RefreshCw size={10} /> : <CheckCircle2 size={10} style={{ color: OK }} />}
              snapshot {new Date(data.generated).toLocaleTimeString()} · refreshes every 10 s
            </div>
            <SummaryTiles s={data} />
            <HostCard s={data} />
            <ProcessesCard s={data} />
            <EnginesCard s={data} />
            <AutoUpdateCard s={data} />
            <HistoryCard s={data} />
          </>
        )}
        <IssuesCard />
        <ConfigCard />
        <LogsCard />
        <ArchiveCard />
      </div>
    </div>
  )
}
