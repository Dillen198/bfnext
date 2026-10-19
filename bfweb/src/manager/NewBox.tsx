import { useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { Rocket, CheckCircle2, X, Search, Play } from '@icons'
import { mgr, type NewBoxOptions, type NewBoxState, type DiscordCheck } from './tauri'
import { Card, Btn, Field, Note, Result, Pill } from './ui'
import { useAction, input, OK, AMBER, RED, MONO, DIM, fmtBytes } from './style'
import { JobPanel } from './Backup'

const check: React.CSSProperties = { fontSize: '0.72rem', cursor: 'pointer', display: 'flex', alignItems: 'flex-start', gap: 8 }
const grid: React.CSSProperties = { display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(260px, 1fr))', gap: 12 }

/** One section of the form: a numbered card. */
function Section({ n, title, children }: { n: number; title: string; children: React.ReactNode }) {
  return (
    <Card icon={<span style={{ ...MONO, fontSize: '0.66rem', color: 'var(--accent)' }}>{n}</span>} title={title}>
      <div className="space-y-3">{children}</div>
    </Card>
  )
}

function Text({ value, onChange, placeholder, secret, mono }: {
  value: string; onChange: (v: string) => void; placeholder?: string; secret?: boolean; mono?: boolean
}) {
  return (
    <input type={secret ? 'password' : 'text'} value={value} placeholder={placeholder} spellCheck={false}
           autoComplete="off" onChange={e => onChange(e.target.value)} style={{ ...input, ...(mono ? MONO : {}) }} />
  )
}

function Tick({ on, set, children }: { on: boolean; set: (v: boolean) => void; children: React.ReactNode }) {
  return (
    <label style={check}>
      <input type="checkbox" checked={on} onChange={e => set(e.target.checked)} style={{ marginTop: 2 }} />
      <span>{children}</span>
    </label>
  )
}

/** Set up a fresh Windows PC as a training range box, end to end. */
export default function NewBox() {
  const { data: st } = useQuery({ queryKey: ['mgr', 'newbox'], queryFn: mgr.newboxState, refetchInterval: 15_000 })
  if (!st) return <div className="p-6" style={DIM}>Looking at this PC…</div>
  return <NewBoxForm st={st} />
}

function NewBoxForm({ st }: { st: NewBoxState }) {
  const qc = useQueryClient()
  const act = useAction()
  const { data: job } = useQuery({
    queryKey: ['mgr', 'backup-job'],
    queryFn: mgr.backupJob,
    refetchInterval: q => (q.state.data?.running ? 800 : 4_000),
    refetchIntervalInBackground: true,
    retry: false,
  })
  // the form starts from this PC's defaults; later polls only refresh "This PC"
  const [o, setO] = useState<NewBoxOptions>(st.defaults)
  const [discord, setDiscord] = useState<DiscordCheck | null>(null)
  const [confirm, setConfirm] = useState(false)
  const [showChannels, setShowChannels] = useState(false)

  const set = <K extends keyof NewBoxOptions>(k: K, v: NewBoxOptions[K]) => setO({ ...o, [k]: v })
  const setCh = (k: keyof NewBoxOptions['channels'], v: string) => setO({ ...o, channels: { ...o.channels, [k]: v } })
  const busy = !!job?.running
  const have = (what: string) => st.have.find(h => h.what === what)?.ok ?? false
  const missing = [
    !o.pg_password && 'the PostgreSQL superuser password',
    !o.discord_token.trim() && 'the bot token',
    !/^\d+$/.test(o.guild_id.trim()) && 'the Discord server id',
    !/^\d+$/.test(o.owner_id.trim()) && 'your Discord user id',
    o.engine_source === 'github' && !o.engine_public_key.trim() && 'the engine release key',
    o.engine_source === 'folder' && !o.engine_folder.trim() && 'the engine folder',
  ].filter(Boolean) as string[]

  const checkDiscord = () => act.run('discord', async () => {
    const r = await mgr.newboxDiscordCheck(o.discord_token, o.guild_id)
    setDiscord(r)
    return r.in_guild ? `${r.bot_name} is in ${r.guild_name ?? 'the server'}` : `${r.bot_name}: not in that server yet -- invite it with the link below`
  })
  const start = () => act.run('newbox', async () => {
    await mgr.newboxStart(o)
    qc.invalidateQueries({ queryKey: ['mgr', 'backup-job'] })
    return 'New box started -- progress is at the top'
  })

  return (
    <div className="p-5 space-y-4" style={{ maxWidth: 980 }}>
      <div>
        <div style={{ fontSize: '1.1rem', letterSpacing: '0.1em', fontFamily: 'var(--font-display, "Bebas Neue")' }}>NEW BOX · TRAINING RANGE</div>
        <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginTop: 4, lineHeight: 1.6 }}>
          Turns this PC into the training range, on its own: Python, PostgreSQL, DCSServerBot {st.bot_version} with its own Discord bot,
          DCS World Server (Caucasus + Supercarrier), SRS, the range mission and engine, its own bfdb, and the FowlEngine service.
          Nothing on the campaign PC changes. Run it again any time: it skips what is already done and keeps existing config.
        </div>
      </div>

      {job && (job.kind === 'newbox' || job.running) && <JobPanel job={job} />}
      {job?.kind === 'newbox' && !job.running && have('DCS World Server') && (
        <Note>
          Log the DCS server in to your ED account once (tick "save password" and "auto login"):{' '}
          <Btn onClick={() => act.run('login', () => mgr.newboxDcsLogin(o.dcs_dir, o.instance), 'DCS is starting -- sign in, then close it')}>
            <Play size={11} />Open DCS login
          </Btn>
        </Note>
      )}

      <Card icon={<Search size={13} style={{ color: 'var(--accent)' }} />} title={`This PC · ${st.hostname} · ${st.account}`}>
        <div style={grid}>
          {st.have.map(h => (
            <div key={h.what} className="flex items-start gap-2" style={{ fontSize: '0.7rem' }}>
              {h.ok ? <CheckCircle2 size={12} style={{ color: OK, marginTop: 2 }} /> : <X size={12} style={{ color: 'var(--text-dim)', marginTop: 2 }} />}
              <span><span>{h.what}</span><br /><span style={{ ...MONO, fontSize: '0.62rem', color: 'var(--text-dim)', wordBreak: 'break-all' }}>{h.detail}</span></span>
            </div>
          ))}
        </div>
        {!st.winget && <Note tone="warn">winget is not available yet (Windows registers it at the first sign-in; Microsoft Store → App Installer). Python and PostgreSQL then need installing by hand.</Note>}
        {st.dcs_free_bytes != null && st.dcs_free_bytes < 80 * 1024 ** 3 && !have('DCS World Server') && (
          <Note tone="warn">Only {fmtBytes(st.dcs_free_bytes)} free where DCS goes; it needs about 60 GB.</Note>
        )}
      </Card>

      <Section n={1} title="Python and PostgreSQL">
        <Tick on={o.install_python} set={v => set('install_python', v)}>Install Python 3.13 for all users if it is missing (winget)</Tick>
        <Tick on={o.install_postgres} set={v => set('install_postgres', v)}>Install PostgreSQL 17 if it is missing (winget)</Tick>
        <div style={grid}>
          <Field label="PostgreSQL superuser password" hint={have('PostgreSQL') ? 'The postgres password this PC\'s PostgreSQL already has.' : 'Set by the install. Keep it: backups and restores need it.'}>
            <Text secret value={o.pg_password} onChange={v => set('pg_password', v)} />
          </Field>
          <Field label="Port"><Text value={String(o.pg_port)} onChange={v => set('pg_port', Number(v) || 5432)} /></Field>
        </div>
      </Section>

      <Section n={2} title="Discord: the range's own bot">
        <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
          A second application at discord.com/developers → Applications → New Application, with a bot (Bot → Reset Token),
          and <b>Server Members</b> and <b>Message Content</b> intents on. It lives in the same Discord server as the campaign bot.
        </div>
        <div style={grid}>
          <Field label="Bot token"><Text secret value={o.discord_token} onChange={v => set('discord_token', v)} /></Field>
          <Field label="Discord server id" hint="Right-click the server → Copy Server ID (Developer Mode on)."><Text mono value={o.guild_id} onChange={v => set('guild_id', v)} /></Field>
          <Field label="Your Discord user id" hint="The bot's owner."><Text mono value={o.owner_id} onChange={v => set('owner_id', v)} /></Field>
          <Field label="Admin role name" hint="Runs the bot's admin commands."><Text value={o.admin_role} onChange={v => set('admin_role', v)} /></Field>
        </div>
        <div className="flex items-center gap-3" style={{ flexWrap: 'wrap' }}>
          <Btn disabled={!o.discord_token.trim() || act.busy === 'discord'} onClick={checkDiscord}>Check the token</Btn>
          {discord && <Pill color={discord.in_guild ? OK : AMBER}>{discord.bot_name}{discord.in_guild ? ' · in the server' : ' · not invited yet'}</Pill>}
        </div>
        {discord && !discord.in_guild && (
          <Note tone="warn">
            Invite it (as a server admin, in a browser), then check again:{' '}
            <span style={{ ...MONO, wordBreak: 'break-all' }}>{discord.invite_url}</span>{' '}
            <Btn onClick={() => { void navigator.clipboard?.writeText(discord.invite_url) }}>Copy</Btn>
          </Note>
        )}
        <Tick on={o.create_channels} set={v => set('create_channels', v)}>
          Create a <b>Training Range</b> category with #range-status, #range-results, #greenie-board, #range-server, #range-chat, #range-admin, #range-audit (reuses any already there)
        </Tick>
        <button onClick={() => setShowChannels(!showChannels)} style={{ ...DIM, background: 'none', border: 'none', cursor: 'pointer', padding: 0 }}>
          {showChannels ? '▾' : '▸'} or give channel ids yourself
        </button>
        {showChannels && (
          <div style={grid}>
            {([
              ['range_status', 'Range status (live embed)'], ['range_results', 'Range results'], ['greenie', 'Greenie board'],
              ['server_status', 'Server status'], ['chat', 'In-game chat'], ['admin', 'Admin'], ['audit', 'Audit'],
            ] as const).map(([k, label]) => (
              <Field key={k} label={label}><Text mono value={o.channels[k]} onChange={v => setCh(k, v)} /></Field>
            ))}
          </div>
        )}
      </Section>

      <Section n={3} title="The range server">
        <div style={grid}>
          <Field label="Server name" hint="As players see it; the bot's config keys on it."><Text value={o.server_name} onChange={v => set('server_name', v)} /></Field>
          <Field label="Server password" hint="Blank: open to everyone."><Text secret value={o.server_password} onChange={v => set('server_password', v)} /></Field>
          <Field label="Max players"><Text value={String(o.max_players)} onChange={v => set('max_players', Number(v) || 32)} /></Field>
          <Field label="Saved Games folder" hint={`${st.saved_games}\\${o.instance}`}><Text mono value={o.instance} onChange={v => set('instance', v)} /></Field>
          <Field label="DCS port" hint="Forward it on the router (TCP+UDP)."><Text value={String(o.dcs_port)} onChange={v => set('dcs_port', Number(v) || 10328)} /></Field>
          <Field label="SRS port" hint="Forward it too."><Text value={String(o.srs_port)} onChange={v => set('srs_port', Number(v) || 5005)} /></Field>
        </div>
      </Section>

      <Section n={4} title="DCS World Server, SRS, Tacview">
        <Tick on={o.install_dcs} set={v => set('install_dcs', v)}>
          Install DCS World Server with Caucasus and the Supercarrier through ED's updater (tens of GB; it shows its own progress window)
        </Tick>
        <div style={grid}>
          <Field label="DCS folder"><Text mono value={o.dcs_dir} onChange={v => set('dcs_dir', v)} /></Field>
          <Field label="ED account (optional)" hint="The bot checks DCS versions with it. The server itself is signed in once, by you, after the install.">
            <Text value={o.ed_user} onChange={v => set('ed_user', v)} />
          </Field>
          <Field label="ED password (optional)" hint="The bot moves it into its .secret folder."><Text secret value={o.ed_password} onChange={v => set('ed_password', v)} /></Field>
        </div>
        <Tick on={o.install_srs} set={v => set('install_srs', v)}>Install SRS (latest release from GitHub, about 300 MB)</Tick>
        <Field label="SRS folder"><Text mono value={o.srs_dir} onChange={v => set('srs_dir', v)} /></Field>
        <Tick on={o.install_speech} set={v => set('install_speech', v)}>
          Install the range ATC's speech: whisper.cpp to hear pilots, Piper for a voice per frequency (about 600 MB)
        </Tick>
        <Field label="TacviewSetup.exe (optional)" hint={<>For the range site's Tacview downloads. Download it from tacview.net (its licence is yours to accept) and give its path; blank skips Tacview.</>}>
          <Text mono value={o.tacview_setup} placeholder="C:\Users\…\Downloads\TacviewSetup.exe" onChange={v => set('tacview_setup', v)} />
        </Field>
      </Section>

      <Section n={5} title="The engine">
        <div className="flex gap-4" style={{ flexWrap: 'wrap' }}>
          {([['github', 'Latest engine release on GitHub'], ['folder', 'A release folder (USB, network)'], ['skip', 'Later, by hand']] as const).map(([k, label]) => (
            <label key={k} style={check}>
              <input type="radio" checked={o.engine_source === k} onChange={() => set('engine_source', k)} style={{ marginTop: 2 }} />{label}
            </label>
          ))}
        </div>
        <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', lineHeight: 1.55 }}>
          {st.engine_files_needed.join(', ')} — checked against the release's manifest.json and its signature.
          A release folder is what <span style={MONO}>deploy\publish-release.ps1 -DryRun</span> leaves behind.
        </div>
        {o.engine_source === 'folder' && (
          <Field label="Release folder" hint="Holds manifest.json (and manifest.json.sig) and the files.">
            <Text mono value={o.engine_folder} onChange={v => set('engine_folder', v)} />
          </Field>
        )}
        {o.engine_source !== 'skip' && (
          <Field label="Engine release public key" hint={<>The contents of <span style={MONO}>fowl-engine.key.pub</span>. Also turns on the range's own auto-update.{o.engine_source === 'folder' ? ' Blank: files are checked against the manifest only.' : ''}</>}>
            <textarea value={o.engine_public_key} spellCheck={false} rows={2} onChange={e => set('engine_public_key', e.target.value)}
                      style={{ ...input, ...MONO, fontSize: '0.64rem', resize: 'vertical' }} />
          </Field>
        )}
        {o.engine_source === 'github' && (
          <div style={grid}>
            <Field label="Repository"><Text mono value={o.engine_repo} onChange={v => set('engine_repo', v)} /></Field>
            <Field label="Channel">
              <select value={o.engine_channel} onChange={e => set('engine_channel', e.target.value as 'stable' | 'beta')} style={input}>
                <option value="stable">stable</option><option value="beta">beta</option>
              </select>
            </Field>
          </div>
        )}
      </Section>

      <Section n={6} title="Public: the range site and its API">
        <div style={grid}>
          <Field label="This bfdb's public address" hint="The main PC's Caddy proxies it here."><Text mono value={o.public_api_url} onChange={v => set('public_api_url', v)} /></Field>
          <Field label="Range site"><Text mono value={o.range_site_url} onChange={v => set('range_site_url', v)} /></Field>
          <Field label="Main PC's LAN address" hint="Lets only the main PC's proxy reach this bfdb. Blank: bfdb stays on this PC only.">
            <Text mono value={o.main_pc_ip} placeholder="192.168.1.10" onChange={v => set('main_pc_ip', v)} />
          </Field>
          <Field label="Bot app's client secret (optional)" hint="Developer Portal → OAuth2. Turns on Discord login on the range site; add the redirect it prints.">
            <Text secret value={o.discord_client_secret} onChange={v => set('discord_client_secret', v)} />
          </Field>
        </div>
      </Section>

      <Section n={7} title="Finish">
        <Tick on={o.firewall} set={v => set('firewall', v)}>Open the Windows firewall for DCS and SRS{o.main_pc_ip.trim() ? `, and bfdb for ${o.main_pc_ip.trim()} only` : ''}</Tick>
        <Tick on={o.install_service} set={v => set('install_service', v)}>Install and start the FowlEngine service (starts the bot at boot)</Tick>
        <Tick on={o.overwrite_config} set={v => set('overwrite_config', v)}>
          <span style={{ color: o.overwrite_config ? AMBER : undefined }}>Rewrite the bot's config files even if they exist (the old ones are kept aside) — and a new database password</span>
        </Tick>
      </Section>

      <Result r={act.result} onClose={act.clear} />
      {missing.length > 0 && <div style={{ fontSize: '0.68rem', color: RED }}>Still needed: {missing.join(', ')}.</div>}
      {!confirm ? (
        <Btn primary disabled={busy || missing.length > 0} onClick={() => setConfirm(true)}><Rocket size={11} />Set up this box…</Btn>
      ) : (
        <Note tone="warn">
          Set this PC up as the training range? It installs programs, writes the bot's config and opens firewall ports.
          The DCS download alone can take hours.{' '}
          <Btn primary onClick={() => { setConfirm(false); void start() }}>Start</Btn>{' '}
          <Btn onClick={() => setConfirm(false)}>Cancel</Btn>
        </Note>
      )}
    </div>
  )
}
