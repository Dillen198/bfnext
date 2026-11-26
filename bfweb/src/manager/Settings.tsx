import { useState } from 'react'
import { Save, Config, Shield, Trash2 } from '@icons'
import { mgr, type AppState, type ManagerConfig } from './tauri'
import { Card, Btn, Field, Note, Result } from './ui'
import { useAction, input, MONO } from './style'

export default function Settings({ state: s }: { state: AppState }) {
  const act = useAction()
  const [draft, setDraft] = useState<ManagerConfig>(s.config)
  const [confirmUninstall, setConfirmUninstall] = useState(false)

  const set = <K extends keyof ManagerConfig>(k: K, v: ManagerConfig[K]) => setDraft({ ...draft, [k]: v })
  const dirty = JSON.stringify(draft) !== JSON.stringify(s.config)
  const busy = act.busy !== null
  const check = (k: 'sync_plugin' | 'auto_update' | 'start_with_windows' | 'controller_only' | 'remote_backups', label: string) => (
    <label className="flex items-center gap-2" style={{ fontSize: '0.72rem', cursor: 'pointer' }}>
      <input type="checkbox" checked={draft[k]} onChange={e => set(k, e.target.checked)} /> {label}
    </label>
  )

  return (
    <div className="p-5 space-y-4" style={{ maxWidth: 980 }}>
      <Result r={act.result} onClose={act.clear} />
      <Card icon={<Config size={13} style={{ color: 'var(--accent)' }} />} title="Manager settings">
        <div className="space-y-3">
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(260px, 1fr))', gap: 12 }}>
            <Field label="DCSServerBot folder">
              <input style={input} value={draft.bot_dir ?? ''} onChange={e => set('bot_dir', e.target.value || null)} />
            </Field>
            <Field label="Start command" hint="Run in that folder by the service. run.cmd creates the Python venv and loops on the bot's own restarts.">
              <input style={input} value={draft.bot_command ?? ''} placeholder="run.cmd" onChange={e => set('bot_command', e.target.value || null)} />
            </Field>
          </div>
          {check('sync_plugin', 'Keep the Fowl Engine plugin in DCSServerBot synced to this app\'s bundle')}
          {check('auto_update', 'Install new Fowl Engine Manager releases automatically')}
          {check('start_with_windows', 'Start Fowl Engine Manager (in the tray) when Windows starts and an administrator signs in')}
          {s.config.start_with_windows && s.autostart && !(s.autostart.installed && s.autostart.current) && (
            <Note tone="warn">The start-with-Windows task isn't registered{s.autostart.installed ? ' for this copy of the app' : ''}. The app tries again every time it starts. (The FowlEngine service starts the server at boot either way.)</Note>
          )}
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))', gap: 12 }}>
            <Field label="Channel">
              <select style={input} value={draft.channel} onChange={e => set('channel', e.target.value as 'stable' | 'beta')}>
                <option value="stable">stable</option>
                <option value="beta">beta (also pre-releases)</option>
              </select>
            </Field>
            <Field label="Check every (hours)">
              <input style={input} type="number" min={0.25} step={0.25} value={draft.check_hours}
                     onChange={e => set('check_hours', Number(e.target.value) || 6)} />
            </Field>
            <Field label="Only update between (local)" hint="e.g. 04:00-08:00 -- an update restarts the bot (not DCS). Blank = any time.">
              <input style={input} value={draft.update_window ?? ''} placeholder="04:00-08:00" onChange={e => set('update_window', e.target.value || null)} />
            </Field>
          </div>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(260px, 1fr))', gap: 12 }}>
            <Field label="GitHub repository" hint="Where manager-v* releases are published.">
              <input style={input} value={draft.repo} onChange={e => set('repo', e.target.value)} />
            </Field>
            <Field label="GitHub token (optional)" hint="Only for a private repo. Stored in %ProgramData%\FowlEngine, readable by administrators only.">
              <input style={input} type="password" value={draft.github_token ?? ''} autoComplete="off"
                     onChange={e => set('github_token', e.target.value || null)} />
            </Field>
          </div>
          <div style={{ borderTop: '1px solid var(--border)', paddingTop: 12 }} className="space-y-2">
            <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>Remote control (Tailscale)</div>
            {check('controller_only', 'This PC only controls other server boxes (no DCSServerBot here) -- Setup stops asking for one')}
            {check('remote_backups', 'Let a controller PC start, list and check backups of this box through its OPS API (never restore)')}
            {draft.remote_backups && !draft.desktop_user && (
              <Note tone="warn">Set the desktop user the bot runs as (Setup → step 3): the bot's account is given access to the remote-backup folder, and only it.</Note>
            )}
            <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
              The servers this PC controls are under ALL SERVERS. Opening this box to a controller is in <span style={MONO}>deploy/manager-remote.md</span>:
              off by default; nothing is reachable from other PCs until you follow it.
            </div>
          </div>
          <div className="flex gap-2">
            <Btn primary disabled={!dirty || busy} onClick={() => act.run('save', async () => {
              await mgr.saveConfig(draft)
            }, 'saved -- the service picks it up within a few seconds')}><Save size={11} />Save</Btn>
            {dirty && <Btn onClick={() => setDraft(s.config)}>Discard</Btn>}
          </div>
        </div>
      </Card>

      <Card icon={<Shield size={13} style={{ color: 'var(--accent)' }} />} title="Service">
        <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', lineHeight: 1.6, marginBottom: 10 }}>
          To change the account or password, use Setup → step 3 (it reconfigures the existing service).
          Executable: <span style={MONO}>{s.service?.executable ?? '—'}</span>
        </div>
        {!confirmUninstall ? (
          <Btn danger disabled={busy || !s.service?.installed} onClick={() => setConfirmUninstall(true)}><Trash2 size={11} />Remove service</Btn>
        ) : (
          <Note tone="bad">
            Remove the FowlEngine service? DCSServerBot stops and nothing starts it after a reboot until you install the service again.{' '}
            <Btn danger onClick={() => { setConfirmUninstall(false); act.run('rm', mgr.uninstallService, 'service removed') }}>Remove</Btn>{' '}
            <Btn onClick={() => setConfirmUninstall(false)}>Cancel</Btn>
          </Note>
        )}
      </Card>
    </div>
  )
}
