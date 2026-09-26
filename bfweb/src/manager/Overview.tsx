import { useState } from 'react'
import { Server, Activity, Download, Shield, RotateCw, Play, Square, RefreshCw, FolderCog } from '@icons'
import { mgr, type AppState, type CheckResult } from './tauri'
import { Card, Row, Btn, Pill, Note, Result } from './ui'
import { useAction, fmtDur, fmtWhen, OK, AMBER, RED, MONO } from './style'

export default function Overview({ state: s, onSetup }: { state?: AppState; onSetup: () => void }) {
  const act = useAction()
  const [check, setCheck] = useState<CheckResult | null>(null)
  if (!s) return <div className="p-6" style={{ fontSize: '0.72rem', color: 'var(--text-dim)' }}>Loading…</div>

  const svc = s.service
  const bot = s.agent?.bot
  const upd = check ?? s.agent?.update ?? null
  const busy = act.busy !== null
  const oldActive = s.old_service?.installed && s.old_service.start_type !== 'disabled'

  return (
    <div className="p-5 space-y-4">
      <Result r={act.result} onClose={act.clear} />
      {svc?.installed && (svc.account ?? 'LocalSystem').toLowerCase() !== 'localsystem' && (
        <Note tone="bad">
          The service runs as <b>{svc.account}</b>, so everything it starts lives in Windows' hidden service session,
          where DCS hangs while it creates its window (the bot then kills it as "hung"). Reinstall the service -- it
          then starts the bot on {svc.account}'s own desktop.{' '}<Btn primary onClick={onSetup}>Fix in Setup</Btn>
        </Note>
      )}
      {svc?.installed && !s.autologon.enabled && (
        <Note tone="warn">
          Automatic sign-in is off: after a reboot or a blue screen nothing starts until someone signs in to Windows
          (DCS needs a desktop session).{' '}<Btn onClick={onSetup}>Turn it on in Setup</Btn>
        </Note>
      )}
      {oldActive && (
        <Note tone="warn">
          The older <span style={MONO}>DCSServerBot</span> service (NSSM, deploy/windows-service) is still set to start
          ({s.old_service?.start_type}, {s.old_service?.state}). Two services would start two bots fighting over DCS.{' '}
          <Btn danger disabled={busy} onClick={() => act.run('old', mgr.disableOldService, 'old service stopped and disabled')}>Disable it</Btn>
        </Note>
      )}

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(380px, 1fr))', gap: 16 }}>
        <Card icon={<Shield size={13} style={{ color: OK }} />} title="Windows service"
          badge={<Pill color={!svc?.installed ? RED : svc.state === 'running' ? OK : AMBER}>{svc?.installed ? svc.state : 'not installed'}</Pill>}>
          {!svc?.installed ? (
            <Note tone="warn">Not installed -- nothing starts after a reboot. Run Setup.</Note>
          ) : (
            <>
              <Row k="Starts">{svc.start_type === 'automatic' ? 'at boot (delayed, after the network), no login needed' : svc.start_type}</Row>
              <Row k="Runs as">{svc.account ?? 'LocalSystem'}</Row>
              <Row k="Process">{svc.pid ? `pid ${svc.pid}` : '—'}</Row>
              {svc.foreign_exe && <Row k="Executable"><span style={{ color: AMBER }}>{svc.executable} (not this install -- reinstall it from Settings)</span></Row>}
              <div className="flex gap-2" style={{ marginTop: 12, flexWrap: 'wrap' }}>
                {svc.state !== 'running' && <Btn primary disabled={busy} onClick={() => act.run('start', () => mgr.serviceControl('start'), 'service started')}><Play size={11} />Start</Btn>}
                {svc.state === 'running' && <Btn disabled={busy} onClick={() => act.run('restart', () => mgr.serviceControl('restart'), 'service restarted')}><RotateCw size={11} />Restart</Btn>}
                {svc.state === 'running' && <Btn danger disabled={busy} onClick={() => act.run('stop', () => mgr.serviceControl('stop'), 'service stopped (DCS keeps running)')}><Square size={11} />Stop</Btn>}
              </div>
            </>
          )}
        </Card>

        <Card icon={<Activity size={13} style={{ color: OK }} />} title="DCSServerBot"
          badge={<Pill color={!s.agent_fresh ? 'var(--text-dim)' : bot?.running ? OK : RED}>
            {!s.agent_fresh ? 'no heartbeat' : bot?.running ? 'running' : bot?.paused ? 'paused' : 'down'}</Pill>}>
          {!s.agent_fresh && svc?.state === 'running' && <Note tone="warn">The service is running but hasn't reported for 30 s -- see Logs → agent.</Note>}
          {bot?.problem && <Note tone="bad">{bot.problem}</Note>}
          <Row k="Folder"><span style={MONO}>{s.config.bot_dir ?? '— (Setup)'}</span></Row>
          <Row k="Runs in">{bot?.session
            ? <span style={{ color: bot.no_desktop ? RED : undefined }}>{bot.session}</span>
            : '—'}</Row>
          <Row k="Process">{bot?.running ? `pid ${bot.pid} · up ${fmtDur(bot.uptime_secs)}` : bot?.next_start_in_secs != null ? `restarting in ${bot.next_start_in_secs}s` : '—'}</Row>
          <Row k="Starts">{bot?.starts ?? 0} since the service started{bot?.last_exit && <> · last exit <span style={MONO}>{bot.last_exit}</span> at {fmtWhen(bot.last_exit_at)}</>}</Row>
          <div className="flex gap-2" style={{ marginTop: 12, flexWrap: 'wrap' }}>
            <Btn disabled={busy || !s.agent_fresh} onClick={() => act.run('rb', () => mgr.agentCommand('restart-bot'), 'restart requested -- DCS keeps running')}><RotateCw size={11} />Restart bot</Btn>
            {bot?.paused
              ? <Btn primary disabled={busy || !s.agent_fresh} onClick={() => act.run('sb', () => mgr.agentCommand('start-bot'), 'start requested')}><Play size={11} />Start bot</Btn>
              : <Btn danger disabled={busy || !s.agent_fresh} onClick={() => act.run('xb', () => mgr.agentCommand('stop-bot'), 'stop requested -- the service keeps it stopped until you start it')}><Square size={11} />Stop bot</Btn>}
          </div>
        </Card>

        <Card icon={<Server size={13} style={{ color: OK }} />} title="Fowl Engine bot plugin"
          badge={<Pill color={s.plugin_pending.length ? AMBER : OK}>{s.plugin_pending.length ? `${s.plugin_pending.length} file(s) behind` : 'in sync'}</Pill>}>
          <Row k="Bundled">{s.bundle_version ?? '— (this build carries none)'}</Row>
          <Row k="Last sync">{s.agent?.last_sync ? `${fmtWhen(s.agent.last_sync.at)} · ${s.agent.last_sync.changed.length} file(s)${s.agent.last_sync.skipped_reason ? ` · skipped: ${s.agent.last_sync.skipped_reason}` : ''}` : '—'}</Row>
          {s.plugin_link && <Note tone="warn">{s.plugin_link}</Note>}
          {s.plugin_pending.length > 0 && (
            <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)', ...MONO, maxHeight: 90, overflow: 'auto', margin: '6px 0' }}>
              {s.plugin_pending.slice(0, 30).map(f => <div key={f}>{f}</div>)}
            </div>
          )}
          <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)', margin: '8px 0', lineHeight: 1.5 }}>
            The service copies the plugin shipped with this app into DCSServerBot before every bot start, backing up what it replaces.
            Engine binaries (bflib.dll, bfdb.exe) are updated by the plugin itself -- see Server OPS.
          </div>
          <Btn disabled={busy || !s.bot_dir_valid || s.plugin_pending.length === 0} onClick={() => act.run('sync', mgr.syncPluginNow)}><RefreshCw size={11} />Sync now</Btn>
        </Card>

        <Card icon={<Download size={13} style={{ color: OK }} />} title="Fowl Engine Manager updates"
          badge={upd?.update_available ? <Pill color={AMBER}>{upd.latest?.version} available</Pill> : <Pill color={OK}>v{s.version}</Pill>}>
          <Row k="Installed">v{s.version}</Row>
          <Row k="Latest">{upd?.latest ? <>v{upd.latest.version}{upd.latest.prerelease ? ' (beta)' : ''} · {fmtWhen(upd.latest.published)}</> : upd?.error ? <span style={{ color: RED }}>{upd.error}</span> : '—'}</Row>
          <Row k="Checked">{fmtWhen(upd?.checked_at)}{s.agent?.update_state && <> · <span style={{ color: AMBER }}>{s.agent.update_state}</span></>}</Row>
          <Row k="Automatic">{s.config.auto_update ? `yes, every ${s.config.check_hours} h${s.config.update_window ? `, between ${s.config.update_window}` : ''} (restarts the bot, not DCS)` : 'off -- Settings'}</Row>
          {upd?.latest?.notes && upd.update_available && (
            <div style={{ fontSize: '0.64rem', color: 'var(--text-muted)', whiteSpace: 'pre-wrap', maxHeight: 120, overflow: 'auto', margin: '8px 0' }}>{upd.latest.notes}</div>
          )}
          <div className="flex gap-2" style={{ marginTop: 12, flexWrap: 'wrap' }}>
            <Btn disabled={busy} onClick={() => act.run('check', async () => {
              const r = await mgr.checkUpdate()
              setCheck(r)
              return r.error ? Promise.reject(new Error(r.error)) : r.update_available ? `v${r.latest?.version} is available` : 'up to date'
            })}><RefreshCw size={11} />{act.busy === 'check' ? 'Checking…' : 'Check now'}</Btn>
            {upd?.update_available && (
              <Btn primary disabled={busy} onClick={() => act.run('install', mgr.installUpdate)}><Download size={11} />Update now</Btn>
            )}
          </div>
          <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 8, lineHeight: 1.5 }}>
            Every installer is checked against the release signing key before it runs.
          </div>
        </Card>
      </div>

      <Card icon={<FolderCog size={13} style={{ color: OK }} />} title="Folders">
        <div className="flex gap-2" style={{ flexWrap: 'wrap' }}>
          <Btn onClick={() => act.run('o1', () => mgr.openPath('bot'))}>DCSServerBot</Btn>
          <Btn onClick={() => act.run('o2', () => mgr.openPath('logs'))}>Logs</Btn>
          <Btn onClick={() => act.run('o3', () => mgr.openPath('backups'))}>Plugin backups</Btn>
          <Btn onClick={() => act.run('o4', () => mgr.openPath('data'))}>Manager data</Btn>
        </div>
        <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 8, ...MONO }}>{s.data_dir}</div>
      </Card>
    </div>
  )
}
