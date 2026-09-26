import { useEffect, useState } from 'react'
import { Search, CheckCircle2, Shield, Server, FolderCog } from '@icons'
import { mgr, type AppState, type BotAccount, type BotDirCheck } from './tauri'
import { Btn, Note, Field, Result } from './ui'
import { useAction, input, OK, AMBER, RED, MONO, DIM } from './style'

function Step({ n, title, done, children }: { n: number; title: string; done: boolean; children: React.ReactNode }) {
  return (
    <div className="vs-card" style={{ borderColor: done ? 'rgba(106,171,31,0.35)' : undefined }}>
      <div className="flex items-center gap-3 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <span style={{
          width: 22, height: 22, borderRadius: '50%', display: 'inline-flex', alignItems: 'center', justifyContent: 'center',
          fontSize: '0.7rem', fontWeight: 700, background: done ? 'var(--accent)' : 'none', color: done ? '#000' : 'var(--text-muted)',
          border: `1px solid ${done ? 'var(--accent)' : 'var(--border)'}`,
        }}>{done ? '✓' : n}</span>
        <span style={{ ...DIM, fontSize: '0.68rem' }}>{title}</span>
      </div>
      <div className="p-4">{children}</div>
    </div>
  )
}

export default function Setup({ state: s, onDone }: { state: AppState; onDone: () => void }) {
  const act = useAction()
  const [dir, setDir] = useState(s.config.bot_dir ?? '')
  const [found, setFound] = useState<string[] | null>(null)
  const [check, setCheck] = useState<BotDirCheck | null>(null)
  const [password, setPassword] = useState('')
  const [accounts, setAccounts] = useState<BotAccount[] | null>(null)
  const [user, setUser] = useState(s.config.desktop_user ?? '')
  const [userTouched, setUserTouched] = useState(false)

  useEffect(() => {
    mgr.detectBotDirs().then(setFound).catch(() => setFound([]))
    mgr.detectBotAccounts().then(setAccounts).catch(() => setAccounts([]))
  }, [])

  const isSystem = (a?: string | null) => (a ?? 'LocalSystem').toLowerCase().endsWith('localsystem')
  // The bot runs on the desktop of the user DCSServerBot was set up as:
  // suggest the best match until the admin picks or types one.
  const desktopUser = userTouched || s.config.desktop_user
    ? user
    : accounts?.[0]?.account ?? `.\\${s.current_user}`
  const userName = desktopUser.split('\\').pop()?.trim().toLowerCase() ?? ''
  const signedIn = s.sessions.find(x => x.user.toLowerCase() === userName)
  const onUserAccount = !!s.service?.installed && !isSystem(s.service.account)
  const autologonMatches = !!s.autologon.account && s.autologon.account.split('\\').pop()?.toLowerCase() === userName

  const busy = act.busy !== null
  const step1 = s.bot_dir_valid
  const step2 = step1 && s.plugin_pending.length === 0 && !!s.bundle_version
  const step3 = !!s.service?.installed && s.service.state === 'running' && !s.service.foreign_exe && !onUserAccount
    && !!s.config.desktop_user
  const step4 = s.autologon.enabled && autologonMatches
  const oldActive = s.old_service?.installed && s.old_service.start_type !== 'disabled'

  async function applyDir(path: string) {
    setDir(path)
    const c = await mgr.checkBotDir(path)
    setCheck(c)
    if (!c.valid) throw new Error(`${path} is not a DCSServerBot folder (it needs run.py, core\\ and plugins\\)`)
    await mgr.saveConfig({ ...s.config, bot_dir: path })
    return `DCSServerBot folder set to ${path}`
  }

  return (
    <div className="p-5 space-y-4" style={{ maxWidth: 980 }}>
      <div>
        <div style={{ fontSize: '1.1rem', letterSpacing: '0.1em', fontFamily: 'var(--font-display, "Bebas Neue")' }}>SET UP THIS SERVER BOX</div>
        <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginTop: 4, lineHeight: 1.6 }}>
          Four steps. Afterwards the box runs itself: after a reboot, a blue screen or a power cut Windows signs the server's
          user in, the service starts DCSServerBot on that desktop, restarts it if it dies, keeps the Fowl Engine plugin current and updates this app.
          DCSServerBot's own first-run setup (Discord token, DCS servers) must already be done: run its <span style={MONO}>run.cmd</span> once by hand if not.
        </div>
      </div>
      <Result r={act.result} onClose={act.clear} />

      <Step n={1} title="Where is DCSServerBot?" done={step1}>
        {found && found.length > 0 && (
          <div style={{ marginBottom: 10 }}>
            <div style={{ ...DIM, marginBottom: 6 }}>Found on this PC</div>
            <div className="flex gap-2" style={{ flexWrap: 'wrap' }}>
              {found.map(f => (
                <Btn key={f} primary={f === s.config.bot_dir} disabled={busy} onClick={() => act.run('dir', () => applyDir(f))}>
                  <FolderCog size={11} />{f}
                </Btn>
              ))}
            </div>
          </div>
        )}
        {found && found.length === 0 && <Note tone="warn">No DCSServerBot folder found automatically -- type its path.</Note>}
        <div className="flex gap-2" style={{ alignItems: 'flex-end' }}>
          <div style={{ flex: 1 }}>
            <Field label="Folder (the one with run.cmd)">
              <input style={input} value={dir} placeholder="E:\Github\DCSServerBot" onChange={e => setDir(e.target.value)} />
            </Field>
          </div>
          <Btn disabled={busy || !dir.trim()} onClick={() => act.run('dir', () => applyDir(dir.trim()))}><Search size={11} />Use this folder</Btn>
        </div>
        {check && check.valid && (
          <div style={{ fontSize: '0.66rem', marginTop: 10, lineHeight: 1.7 }}>
            <div style={{ color: check.plugin_installed ? OK : AMBER }}>
              {check.plugin_installed ? '✓ Fowl Engine plugin present' : '• Fowl Engine plugin not installed yet -- step 2 installs it'}
            </div>
            <div style={{ color: check.ops_target ? OK : AMBER }}>
              {check.ops_target ? `✓ OPS API: ${check.ops_target}` : `• ${check.ops_error}`}
            </div>
          </div>
        )}
      </Step>

      <Step n={2} title="Install the Fowl Engine plugin" done={step2}>
        <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', lineHeight: 1.6, marginBottom: 10 }}>
          Copies the FowlEngine plugin and the bf* extensions shipped with this app (bundle <span style={MONO}>{s.bundle_version ?? '—'}</span>)
          into DCSServerBot, zipping whatever it replaces into the backups folder. Your <span style={MONO}>config\plugins\fowlengine.yaml</span> is never touched.
          From now on the service does this before every bot start.
        </div>
        {s.plugin_link && <Note tone="warn">{s.plugin_link}</Note>}
        {step1 && s.plugin_pending.length > 0 && <Note>{s.plugin_pending.length} file(s) to install or update.</Note>}
        <Btn primary={!step2} disabled={busy || !step1 || !s.bundle_version || s.plugin_pending.length === 0}
          onClick={() => act.run('sync', mgr.syncPluginNow)}><Server size={11} />{step2 ? 'Up to date' : 'Install plugin'}</Btn>
      </Step>

      <Step n={3} title="Run it as a Windows service" done={step3}>
        <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', lineHeight: 1.6, marginBottom: 12 }}>
          The service starts at boot and keeps DCSServerBot running -- <b>on the desktop of the Windows account below</b>, as
          that account (its <span style={MONO}>Saved Games\DCS.*</span> profiles, its DCSServerBot Python in{' '}
          <span style={MONO}>%USERPROFILE%\.dcssb</span>). A service's own session has no desktop, and DCS hangs there while it
          creates its window, so the bot and every DCS it starts run in that user's session: you see the DCS windows as usual.
          The service itself runs as LocalSystem, the only account Windows lets start programs in someone else's session.
        </div>
        {oldActive && (
          <Note tone="warn">
            The old NSSM <span style={MONO}>DCSServerBot</span> service is still enabled -- it will be stopped and disabled so only one bot runs.
          </Note>
        )}
        {onUserAccount && (
          <Note tone="bad">
            The service runs as <b>{s.service?.account}</b>, so the bot and DCS run in the hidden service session, where
            DCS hangs starting ("timeout while launching", then killed as hung). Reinstall it below.
          </Note>
        )}
        {accounts && accounts.length > 0 && (
          <div style={{ marginBottom: 12 }}>
            <div style={{ ...DIM, marginBottom: 6 }}>Accounts set up for DCSServerBot / DCS on this PC</div>
            <div className="flex gap-2" style={{ flexWrap: 'wrap' }}>
              {accounts.map(a => (
                <Btn key={a.account} primary={desktopUser === a.account}
                  onClick={() => { setUser(a.account); setUserTouched(true) }}>
                  {a.account}
                  <span style={{ fontSize: '0.6rem', opacity: 0.8 }}>
                    {a.has_venv ? 'DCSServerBot Python' : 'no bot Python yet'}
                    {a.dcs_instances.length ? ` · ${a.dcs_instances.join(', ')}` : ''}
                  </span>
                </Btn>
              ))}
            </div>
          </div>
        )}
        {accounts && accounts.length === 0 && (
          <Note tone="warn">
            No user on this PC has DCSServerBot's Python environment (<span style={MONO}>%USERPROFILE%\.dcssb</span>) yet.
            Run DCSServerBot's <span style={MONO}>run.cmd</span> once as the server's Windows user, then come back.
          </Note>
        )}
        <div style={{ maxWidth: 420 }}>
          <Field label="Run DCSServerBot and DCS on the desktop of" hint={<>
            <span style={MONO}>.\name</span> for a local account, <span style={MONO}>DOMAIN\name</span> otherwise.
          </>}>
            <input style={input} value={desktopUser} onChange={e => { setUser(e.target.value); setUserTouched(true) }} />
          </Field>
        </div>
        <div style={{ marginTop: 12 }}>
          <Btn primary={!step3} disabled={busy || !step1 || !desktopUser.trim()}
            onClick={() => act.run('svc', async () => {
              await mgr.saveConfig({ ...s.config, desktop_session: true, desktop_user: desktopUser.trim() })
              if (oldActive) await mgr.disableOldService()
              await mgr.installService(null, null)
              return `service installed and started -- it starts DCSServerBot on ${desktopUser.trim()}'s desktop now and after every boot`
            })}>
            <Shield size={11} />{s.service?.installed ? 'Reinstall service' : 'Install and start service'}
          </Btn>
        </div>
        {s.service?.installed && (
          <div style={{ fontSize: '0.66rem', color: s.service.state === 'running' ? OK : AMBER, marginTop: 10 }}>
            Service: {s.service.state} · {s.service.start_type} · {s.service.account}
            {signedIn ? ` · ${signedIn.user} is signed in (session ${signedIn.id}, ${signedIn.state})` : ` · ${desktopUser.trim() || 'the user'} is not signed in`}
          </div>
        )}
      </Step>

      <Step n={4} title="Sign in automatically after a reboot" done={step4}>
        <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', lineHeight: 1.6, marginBottom: 12 }}>
          DCS needs that desktop session, so after a reboot, a blue screen or a power cut Windows has to sign{' '}
          <b>{desktopUser || 'the user'}</b> in by itself -- then the service starts the bot and nobody has to be there.
          This is Windows' own automatic sign-in (what Sysinternals Autologon sets up): the password is checked, then kept
          encrypted by Windows (an LSA secret), never in the registry or this app's files.
        </div>
        {s.autologon.broken && (
          <Note tone="bad">Automatic sign-in is switched on but Windows has no password for it -- it will stop at the sign-in screen. Enter the password below.</Note>
        )}
        {s.autologon.enabled && !autologonMatches && (
          <Note tone="warn">Windows signs in <b>{s.autologon.account}</b> automatically, but the bot runs on {desktopUser}'s desktop. Turn it on for {desktopUser} below.</Note>
        )}
        {step4 && <Note tone="ok">On: Windows signs in {s.autologon.account} at every boot.</Note>}
        <div className="flex gap-2" style={{ alignItems: 'flex-end', flexWrap: 'wrap' }}>
          <div style={{ width: 280 }}>
            <Field label={`Windows password of ${desktopUser || 'the user'}`}>
              <input style={input} type="password" value={password} onChange={e => setPassword(e.target.value)} autoComplete="new-password" />
            </Field>
          </div>
          <Btn primary={!step4} disabled={busy || !desktopUser.trim() || !password}
            onClick={() => act.run('autologon', async () => {
              await mgr.enableAutologon(desktopUser.trim(), password)
              setPassword('')
              return `automatic sign-in on for ${desktopUser.trim()}`
            })}><Shield size={11} />{s.autologon.enabled ? 'Update' : 'Turn on'}</Btn>
          {s.autologon.enabled && (
            <Btn danger disabled={busy} onClick={() => act.run('autologoff', mgr.disableAutologon, 'automatic sign-in off')}>Turn off</Btn>
          )}
        </div>
        <label className="flex items-center gap-2" style={{ fontSize: '0.68rem', color: 'var(--text)', marginTop: 12 }}>
          <input type="checkbox" checked={s.config.lock_after_autologon} disabled={busy}
            onChange={e => act.run('lock', () => mgr.saveConfig({ ...s.config, lock_after_autologon: e.target.checked }),
              e.target.checked ? 'the desktop will be locked after an automatic sign-in' : 'the desktop stays unlocked after an automatic sign-in')} />
          Lock the screen right after the automatic sign-in (DCS and the bot keep running on a locked desktop)
        </label>
        <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)', marginTop: 8, lineHeight: 1.6 }}>
          Remote Desktop: sign in as {desktopUser || 'this user'} to see the windows. Closing the RDP window (disconnect) keeps
          everything running; <b>signing out</b> closes the bot and DCS with the session.
        </div>
      </Step>

      <div className="vs-card p-4">
        <div className="flex items-center gap-2" style={{ marginBottom: 8 }}>
          <CheckCircle2 size={14} style={{ color: step1 && step2 && step3 && step4 ? OK : 'var(--text-dim)' }} />
          <span style={{ ...DIM, fontSize: '0.68rem' }}>Check</span>
        </div>
        {[
          [step1, 'DCSServerBot folder set'],
          [step2, 'Fowl Engine plugin installed and current'],
          [step3, 'Service installed, automatic, running'],
          [step4, `Windows signs ${desktopUser || 'the user'} in automatically`],
          [!!s.agent?.bot.running && !s.agent?.bot.no_desktop, `DCSServerBot running on ${desktopUser || 'the user'}'s desktop`],
          [!!s.ops_target, s.ops_target ? 'bot OPS API configured' : `bot OPS API: ${s.ops_error ?? 'not yet'}`],
          [!oldActive, 'no second (NSSM) bot service'],
        ].map(([ok, label]) => (
          <div key={String(label)} style={{ fontSize: '0.7rem', color: ok ? OK : AMBER, padding: '2px 0' }}>{ok ? '✓' : '•'} {label}</div>
        ))}
        <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)', marginTop: 10, lineHeight: 1.6 }}>
          Last check that only you can do: restart the PC and don't sign in -- Windows signs in by itself, the DCS servers
          come up on that desktop and the dashboard comes back. In the BIOS set
          "Restore on AC power loss" to Power On so a power cut ends the same way.
        </div>
        {step1 && step3 && <div style={{ marginTop: 12 }}><Btn primary onClick={onDone}>Go to overview</Btn></div>}
        {!s.elevated && <div style={{ fontSize: '0.66rem', color: RED, marginTop: 8 }}>This app is not elevated -- the service steps need administrator rights.</div>}
      </div>
    </div>
  )
}
