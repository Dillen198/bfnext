/**
 * `npm run dev:manager` then open http://localhost:5190/?mock -- a fake
 * Tauri bridge with a believable server box, so the manager's screens can be
 * laid out and clicked through in a browser. Dev only: main.tsx imports this
 * behind `import.meta.env.DEV`, so it never ships.
 */
const now = () => new Date().toISOString()

const state = {
  version: '0.1.0',
  exe: 'C:\\Program Files\\Fowl Engine Manager\\FowlEngineManager.exe',
  data_dir: 'C:\\ProgramData\\FowlEngine',
  current_user: 'ATPAdmin',
  hostname: 'VS-SERVER',
  elevated: true,
  config: {
    bot_dir: 'E:\\Github\\DCSServerBot', bot_command: null, sync_plugin: true, auto_update: true,
    channel: 'stable', repo: 'Dillen198/bfnext', check_hours: 6, update_window: '04:00-08:00', github_token: null,
    desktop_session: true, desktop_user: '.\\ATPAdmin', lock_after_autologon: true,
  },
  service: {
    name: 'FowlEngine', installed: true, state: 'running', start_type: 'automatic', account: 'LocalSystem',
    executable: '"C:\\Program Files\\Fowl Engine Manager\\FowlEngineManager.exe" --service', foreign_exe: false, pid: 4412,
  },
  old_service: {
    name: 'DCSServerBot', installed: true, state: 'stopped', start_type: 'automatic', account: '.\\ATPAdmin',
    executable: 'C:\\tools\\nssm.exe', foreign_exe: true, pid: null,
  },
  agent: {
    version: '0.1.0', pid: 4412, started_at: now(), heartbeat: now(), bot_dir: 'E:\\Github\\DCSServerBot',
    bot: {
      running: false, pid: null, started_at: now(), uptime_secs: null, starts: 6,
      last_exit: 'exit code: 0', last_exit_at: now(), paused: false, next_start_in_secs: 8,
      problem: "Waiting for ATPAdmin to sign in to Windows. DCS needs a desktop to start in (in the service's own session it hangs), so the bot starts as soon as they have signed in. Setup -> automatic sign-in makes Windows do that by itself after every reboot.",
      session: null, no_desktop: false,
    },
    last_sync: { at: now(), bundle_version: '0.1.0+5772770d025f', changed: ['plugins/fowlengine/autoupdate.py'], skipped_reason: null, backup: 'C:\\ProgramData\\FowlEngine\\backups\\plugin-20260926-041500.zip' },
    update: {
      current: '0.1.0', update_available: true, checked_at: now(), error: null,
      latest: { tag: 'manager-v0.2.0', version: '0.2.0', notes: 'Faster status page; plugin sync backups pruned.', published: now(), html_url: null, prerelease: false, setup_name: 'Fowl Engine Manager_0.2.0_x64-setup.exe', size: 2700000 },
    },
    update_state: null,
  },
  agent_fresh: true,
  bot_dir_valid: true,
  bundle_version: '0.1.0+5772770d025f',
  plugin_pending: [],
  plugin_link: null,
  ops_target: 'http://127.0.0.1:9876/stats/fowlengine/ops',
  ops_error: null as string | null,
  autologon: { enabled: false, account: null as string | null, password_stored: false, broken: false },
  sessions: [] as { id: number; user: string; domain: string; state: 'active' | 'disconnected'; logon_age_secs: number | null }[],
}

const handlers: Record<string, (args: Record<string, unknown>) => unknown> = {
  get_state: () => ({ ...state, agent: { ...state.agent, heartbeat: now() } }),
  detect_bot_accounts: () => [
    { account: '.\\Administrator', has_venv: true, dcs_instances: ['DCS.vectorstrike_1', 'DCS.vectorstrike_2'] },
    { account: '.\\dille', has_venv: false, dcs_instances: ['DCS'] },
  ],
  detect_bot_dirs: () => ['E:\\Github\\DCSServerBot', 'C:\\DCSServerBot'],
  check_bot_dir: () => ({ valid: true, has_venv_hint: true, ops_target: state.ops_target, ops_error: null, plugin_installed: true }),
  save_config: (a) => { Object.assign(state.config, a.config as object); return null },
  check_update: () => state.agent.update,
  read_log: () => [
    '2026-09-26T04:15:00Z [INFO] Fowl Engine Manager 0.1.0 service loop starting (pid 4412)',
    '2026-09-26T04:15:00Z [INFO] plugin sync: 1 file(s) updated from bundle 0.1.0+5772770d025f',
    '2026-09-26T04:15:00Z [INFO] started DCSServerBot: cmd /c run.cmd in E:\\Github\\DCSServerBot (pid 9120)',
  ],
  ops_request: () => ({ status: 502, content_type: 'application/json', body: JSON.stringify({ error: 'mock: no bot here' }) }),
}

// ── BOT CONFIG: a few in-memory bot config files ────────────────────────────

const botFiles: Record<string, { text: string; modified: string }> = {
  'plugins/fowlengine.yaml': { modified: now(), text: [
    '# Fowl Engine DCSServerBot Plugin Configuration',
    'DEFAULT:',
    '  brand_name: "Vector Strike"',
    '  status_channel: 123456789012345678',
    '  bfdb:',
    '    manage: true',
    '    listen_address: "0.0.0.0:8880"',
    '    site_address: "127.0.0.1:8766"',
    '    dcsserverbot_url: "http://127.0.0.1:9876/stats"',
    '    dcsserverbot_api_key: "restapi-key"          # secret -- X-API-Key from restapi.yaml',
    '  autoupdate:',
    '    enabled: false                 # (OPS) check for + stage new releases',
    '    public_key: ""',
    '  ops_api:',
    '    enabled: true',
    '    service_name: DCSServerBot     # the Windows service install-service.ps1 created',
    '    # api_key: ""                  # its own long random value (see above)',
    '',
  ].join('\r\n') },
  'services/webservice.yaml': { modified: now(), text: 'DEFAULT:\n  listen: 0.0.0.0\n  port: 9876\n' },
  'main.yaml': { modified: now(), text: 'guild_id: 123456789012345678\nautoupdate: true\nlogging:\n  loglevel: INFO\n' },
  'nodes.yaml': { modified: now(), text: 'VS-SERVER:\n  DCS:\n    installation: C:\\Program Files\\Eagle Dynamics\\DCS World Server\n  instances:\n    vectorstrike_1:\n      home: C:\\Users\\ATPAdmin\\Saved Games\\DCS.vectorstrike_1\n' },
  'services/bot.yaml': { modified: now(), text: 'token: SECRET_TOKEN\nowner: 123456789012345678\n' },
}

function fakeSha(s: string): string {
  let h = 5381
  for (let i = 0; i < s.length; i++) h = (h * 33 + s.charCodeAt(i)) >>> 0
  return h.toString(16).padStart(8, '0')
}

function mockValidate(text: string) {
  const errors: { line: number; column: number; message: string }[] = []
  text.split(/\r?\n/).forEach((l, i) => {
    if (/^ *\t/.test(l)) errors.push({ line: i + 1, column: 1, message: 'found character that cannot start any token' })
    const code = l.replace(/\s#.*$/, '')
    if ((code.match(/"/g) ?? []).length % 2) errors.push({ line: i + 1, column: code.indexOf('"') + 1, message: 'found unexpected end of stream' })
  })
  return { ok: errors.length === 0, errors: errors.slice(0, 1), warnings: [] }
}

/** A crude stand-in for botcfg.rs's line editor: good enough to click through. */
function mockSet(rel: string, path: string[], value: string) {
  const f = botFiles[rel]
  if (!f) throw new Error(`${rel} not found`)
  const eol = f.text.includes('\r\n') ? '\r\n' : '\n'
  const lines = f.text.split(eol)
  const leaf = path[path.length - 1]
  const parent = path[path.length - 2]
  const pi = parent ? lines.findIndex(l => l.trim() === `${parent}:`) : -1
  const q = `"${value.replace(/"/g, '\\"')}"`
  const i = lines.findIndex((l, j) => j > pi && new RegExp(`^\\s*(# )?${leaf}:`).test(l))
  if (i >= 0) {
    const indent = lines[i].match(/^\s*/)![0]
    const comment = lines[i].replace(/^(\s*)# /, '$1').match(/\s{2,}#.*$/)?.[0] ?? ''
    const before = lines[i]
    lines[i] = `${indent}${leaf}: ${q}${comment}`
    return { lines, eol, change: { line: i + 1, before: [before], after: [lines[i]] } }
  }
  const indent = pi >= 0 ? lines[pi].match(/^\s*/)![0] + '  ' : ''
  lines.splice(pi + 1, 0, `${indent}${leaf}: ${q}`)
  return { lines, eol, change: { line: pi + 2, before: [], after: [lines[pi + 1]] } }
}

function mockMask(change: { line: number; before: string[]; after: string[] }, leaf: string) {
  if (!/key|secret|password|token/i.test(leaf)) return change
  const m = (l: string) => l.replace(/:\s*"([^"]*)"/, (_all, v: string) => `: ${v ? `${v.slice(0, 4)}… (${v.length} chars)` : '(empty)'}`)
  return { ...change, before: change.before.map(m), after: change.after.map(m) }
}

function mockChecks() {
  const fe = botFiles['plugins/fowlengine.yaml'].text
  const ws = botFiles['services/webservice.yaml'].text
  const key = fe.match(/^\s+api_key:\s*"([^"]*)"/m)?.[1] ?? ''
  const pk = fe.match(/public_key:\s*"([^"]*)"/)?.[1] ?? ''
  const listen = fe.match(/listen_address:\s*"([^"]*)"/)?.[1] ?? ''
  const wsListen = ws.match(/listen:\s*"?([^"\s]*)/)?.[1] ?? ''
  const act = (kind: string, label: string, rel: string, path: string[], value: string | null = null) => ({ kind, label, rel, path, value })
  const FE = 'plugins/fowlengine.yaml'
  return [
    pk
      ? { id: 'autoupdate_public_key', level: 'ok', title: 'Engine release key', message: 'autoupdate.public_key is set.', fix: null, current: 'key id 4B1E62A0F9D3C817', action: null }
      : { id: 'autoupdate_public_key', level: 'info', title: 'Engine release key',
          message: 'autoupdate.public_key is not set -- not needed until auto-update is turned on, but nothing is ever staged without it.',
          fix: "The RW... line of %USERPROFILE%\\.tauri\\fowl-engine.key.pub (the ENGINE release key, not the Manager's).", current: null,
          action: act('public_key', 'Load from .pub & set', FE, ['DEFAULT', 'autoupdate', 'public_key']) },
    key.length >= 32
      ? { id: 'ops_api_key', level: 'ok', title: 'OPS API key', message: 'ops_api.api_key is set, long, and separate from the RestAPI key.', fix: null,
          current: `${key.slice(0, 4)}… (${key.length} chars)`, action: null }
      : { id: 'ops_api_key', level: 'warn', title: 'OPS API key',
          message: 'ops_api.api_key is not set -- the OPS API falls back to the RestAPI key (bfdb.dcsserverbot_api_key), which more things hold.',
          fix: 'A long random value of its own (32+ characters). bfdb gets it from the plugin automatically.', current: null,
          action: act('generate_secret', 'Generate & set', FE, ['DEFAULT', 'ops_api', 'api_key']) },
    listen.startsWith('127.')
      ? { id: 'bfdb_listen_address', level: 'ok', title: 'bfdb.listen_address', message: "bfdb's API listens on loopback only.", fix: null, current: listen, action: null }
      : { id: 'bfdb_listen_address', level: 'warn', title: 'bfdb.listen_address', message: `bfdb's API listens on ${listen} -- reachable from other machines.`,
          fix: 'Bind it to 127.0.0.1:8880 and publish it through the reverse proxy (Caddy) instead of an open port.', current: listen,
          action: act('set', 'Set to 127.0.0.1', FE, ['DEFAULT', 'bfdb', 'listen_address'], '127.0.0.1:8880') },
    { id: 'bfdb_site_address', level: 'ok', title: 'bfdb.site_address', message: "bfdb's site listens on loopback only.", fix: null, current: '127.0.0.1:8766', action: null },
    wsListen.startsWith('127.')
      ? { id: 'webservice_listen', level: 'ok', title: 'WebService listen', message: 'the WebService listens on loopback only.', fix: null, current: wsListen, action: null }
      : { id: 'webservice_listen', level: 'warn', title: 'WebService listen',
          message: `the WebService (and the OPS API on it) listens on ${wsListen} -- reachable from other machines.`,
          fix: 'listen: 127.0.0.1 -- bfdb and this app call it from this box.', current: wsListen,
          action: act('set', 'Set to 127.0.0.1', 'services/webservice.yaml', ['DEFAULT', 'listen'], '127.0.0.1') },
  ]
}

function mockWrite(rel: string, text: string, sha: string) {
  const f = botFiles[rel]
  if (!f) throw new Error(`${rel} not found`)
  if (fakeSha(f.text) !== sha) throw new Error(`CONFLICT: ${rel} was changed on disk since it was opened -- reload it, or copy your edits first`)
  const v = mockValidate(text)
  if (!v.ok) throw new Error(`not saved -- ${rel} is not valid YAML: ${v.errors[0].message} (line ${v.errors[0].line})`)
  f.text = text
  f.modified = now()
  return { sha256: fakeSha(text), backup: `E:\\Github\\DCSServerBot\\config\\.fowl-backups\\${rel.replace(/\//g, '__')}.${now().replace(/[-:]/g, '')}` }
}

const BOT_LABELS: Record<string, string> = {
  'plugins/fowlengine.yaml': 'Fowl Engine plugin', 'services/webservice.yaml': 'WebService (OPS API listener)',
  'main.yaml': 'Bot main settings', 'nodes.yaml': 'Nodes: DCS installs + instances', 'services/bot.yaml': 'Discord bot',
}

const botHandlers: Record<string, (args: Record<string, unknown>) => unknown> = {
  list_bot_configs: () => ({
    dir: 'E:\\Github\\DCSServerBot\\config',
    files: Object.entries(botFiles).map(([rel, f]) => ({ rel, size: f.text.length, modified: f.modified, label: BOT_LABELS[rel] ?? rel })),
  }),
  read_bot_config: (a) => {
    const rel = a.rel as string
    const f = botFiles[rel]
    if (!f) throw new Error(`${rel} not found`)
    return { rel, path: `E:\\Github\\DCSServerBot\\config\\${rel.replace(/\//g, '\\')}`, text: f.text, sha256: fakeSha(f.text) }
  },
  validate_bot_config: (a) => mockValidate(a.text as string),
  write_bot_config: (a) => mockWrite(a.rel as string, a.text as string, a.expectedSha as string),
  config_checks: () => mockChecks(),
  generate_secret: () => {
    const b = crypto.getRandomValues(new Uint8Array(32))
    return btoa(String.fromCharCode(...b)).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '')
  },
  read_public_key: (a) => ({
    key: 'RWQXyNjt9MKgmh2L7sPRXoEVk3kqTjsSm0ttPaH1rpPTmD8QpvK9hMvR',
    key_id: '4B1E62A0F9D3C817',
    path: (a.path as string | null) || 'C:\\Users\\ATPAdmin\\.tauri\\fowl-engine.key.pub',
  }),
  preview_config_value: (a) => {
    const path = a.path as string[]
    const r = mockSet(a.rel as string, path, a.value as string)
    return { change: mockMask(r.change, path[path.length - 1]), sha256: fakeSha(botFiles[a.rel as string].text) }
  },
  set_config_value: (a) => {
    const rel = a.rel as string, path = a.path as string[]
    const r = mockSet(rel, path, a.value as string)
    const written = mockWrite(rel, r.lines.join(r.eol), a.expectedSha as string)
    return { written, change: mockMask(r.change, path[path.length - 1]) }
  },
}

// ── BACKUP tab: a fake job that runs for ~8 s ─────────────────────────────────
const P = 'C:\\Users\\ATPAdmin\\Saved Games'
const mockRoots = [
  { id: 'bot', kind: 'bot', label: 'DCSServerBot (bot, plugins, config + secrets)', path: 'C:\\VectorStrike\\Tools\\DCSServerBot', files: 2140, bytes: 96e6, include: true, note: 'without its Python venv (run.cmd builds it again), caches and logs' },
  { id: 'instance-DCS.vectorstrike_1-a1b2c3', kind: 'instance', label: 'DCS server DCS.vectorstrike_1 (config, missions, campaign saves, bfdb data)', path: `${P}\\DCS.vectorstrike_1`, files: 812, bytes: 2.4e9, include: true, note: 'without tracks, screenshots, shader caches and DCS logs' },
  { id: 'instance-DCS.vectorstrike_2-d4e5f6', kind: 'instance', label: 'DCS server DCS.vectorstrike_2 (config, missions, campaign saves, bfdb data)', path: `${P}\\DCS.vectorstrike_2`, files: 377, bytes: 610e6, include: true, note: 'without tracks, screenshots, shader caches and DCS logs' },
  { id: 'extra-whisper-0a0b0c', kind: 'extra', label: "Folder the bot's config points at", path: 'C:\\VectorStrike\\Tools\\whisper', files: 9, bytes: 1.6e9, include: false, note: '1.6 GB -- unticked because it is big; tick it if it can\'t be downloaded again' },
]
let mockJob: Record<string, unknown> | null = null
let mockJobStart = 0
function mockJobNow() {
  if (!mockJob) return null
  const t = (Date.now() - mockJobStart) / 8000
  const total = mockJob.total_bytes as number
  if (t >= 1 && mockJob.running) {
    mockJob.running = false
    mockJob.phase = 'done'
    mockJob.finished_at = now()
    mockJob.output = mockJob.kind === 'backup' ? 'D:\\FowlEngineBackups\\FowlEngine-backup-VS-SERVER-20261008-1412.zip (2.1 GB)'
      : mockJob.kind === 'verify' ? 'the backup is complete and reads back clean' : '3 folder(s) restored, database loaded'
    if (mockJob.kind !== 'restore') {
      const zip = 'D:\\FowlEngineBackups\\FowlEngine-backup-VS-SERVER-20261008-1412.zip'
      mockJob.zip_path = zip
      mockJob.log_file = zip.replace(/\.zip$/, '.log')
      const ok = (label: string, path: string, files: number, bytes: number, checks: string[], skipped: string[] = []) => ({
        label, kind: 'x', path, status: skipped.length ? 'warn' : 'ok', files_on_disk: files + skipped.length, files_in_zip: files, bytes_in_zip: bytes,
        checks: [...checks.map(text => ({ ok: true, info: false, text })), { ok: true, info: false, text: `all ${files} file(s) written are in the zip` },
                 ...(skipped.length ? [{ ok: false, info: false, text: `${skipped.length} of ${files + skipped.length} file(s) on disk couldn't be read at backup time` }] : [])],
        skipped,
      })
      mockJob.report = {
        zip, ok: true, created: now(), hostname: 'VS-SERVER', manager_version: '0.2.18', entries: 3341, bytes: 3.1e9, zip_bytes: 2.1e9, corrupt: [], problems: [],
        sections: [
          ok(mockRoots[0].label, mockRoots[0].path, 2140, 96e6, ['run.py (DCSServerBot itself)', 'config/main.yaml', 'config/nodes.yaml (DCS install, servers, database URL)', 'config/.secret (Discord token, passwords)', 'config/plugins/fowlengine.yaml (bfdb, GCI, keys)', 'the Fowl Engine plugin']),
          ok('SRS server (DCS-SimpleRadio Standalone, the whole program folder)', 'C:\\Program Files\\DCS-SimpleRadio-Standalone', 212, 140e6, ["the program's .exe files"]),
          ok(mockRoots[1].label, mockRoots[1].path, 811, 2.4e9, ['Config/serverSettings.lua (name, password, mission list)', '14 mission file(s) (.miz)', 'Scripts/bflib.dll (the engine)', 'bfdb/ (stats database)', 'Logs/stats (what bfdb reads)'], ['Logs\\stats\\0042.bin (only partly read: locked)']),
          ok(mockRoots[2].label, mockRoots[2].path, 377, 610e6, ['Config/serverSettings.lua (name, password, mission list)', '6 mission file(s) (.miz)', 'Scripts/bflib.dll (the engine)']),
          ok('netidx tools (netidx.exe -- runs the resolver the live map and stats use)', 'C:\\Users\\ATPAdmin\\.cargo\\bin', 1, 14e6, ['netidx.exe']),
          ok('netidx client config (where bflib and bfdb find the resolver)', 'C:\\Users\\ATPAdmin\\AppData\\Roaming\\netidx', 1, 300, ['client.json']),
          ok('Bot database (dcsserverbot)', '127.0.0.1:5432/dcsserverbot', 1, 48e6, ['database/dcsserverbot.dump is in the zip', 'a PostgreSQL custom-format dump (PGDMP header)', '46 MB -- 46 MB when it was taken']),
        ],
      }
      mockJob.log = [...(mockJob.log as string[]), 'Checking the zip: reading every file back', 'BACKUP CHECK: OK -- everything listed is in the zip and reads back clean',
                     'WARNING: C:\\Users\\ATPAdmin\\Saved Games\\DCS.vectorstrike_1\\Logs\\stats\\0042.bin was only partly read (locked)']
      mockJob.warnings = ['C:\\Users\\ATPAdmin\\Saved Games\\DCS.vectorstrike_1\\Logs\\stats\\0042.bin was only partly read (locked)']
    }
    mockJob.next_steps = mockJob.kind === 'backup'
      ? ['Copy D:\\FowlEngineBackups\\FowlEngine-backup-VS-SERVER-20261008-1412.zip OFF this PC (USB stick, another drive, cloud) before reinstalling Windows.', "Keep it private: it holds the bot's Discord token, database password and API keys.", 'On the new Windows: install DCS World Server (and SRS), install Fowl Engine Manager, open BACKUP → Restore and pick this zip.']
      : ['Setup → step 4: turn automatic sign-in on again for the server\'s user.', 'Watch the OVERVIEW tab: the bot\'s first start builds its Python venv, which takes a few minutes.']
  }
  return { ...mockJob, done_bytes: Math.round(Math.min(1, t) * total), current: mockJob.running ? `${P}\\DCS.vectorstrike_1\\bfdb\\db` : null }
}
function startMockJob(kind: string) {
  mockJobStart = Date.now()
  mockJob = { id: 1, kind, running: true, phase: kind === 'backup' ? 'Copying DCS server DCS.vectorstrike_1' : 'Restoring DCSServerBot', done_bytes: 0, total_bytes: 3.1e9, current: null,
              log: ['Looking at what to back up', 'Stopping DCSServerBot (bfdb stops with it)'], warnings: [], error: null, output: null, next_steps: [], started_at: now(), finished_at: null }
  return null
}

const backupHandlers: Record<string, (args: Record<string, unknown>) => unknown> = {
  backup_plan: () => ({
    roots: mockRoots, programs: [{ what: 'DCS World Server', path: 'C:\\Program Files\\Eagle Dynamics\\DCS World Server' }, { what: 'DCS-SimpleRadio Standalone', path: 'C:\\Program Files\\DCS-SimpleRadio-Standalone' }],
    database: { host: '127.0.0.1', port: 5432, name: 'dcsserverbot', user: 'dcsserverbot', pg_dump: 'C:\\Program Files\\PostgreSQL\\16\\bin\\pg_dump.exe', problem: null },
    default_dest: 'D:\\FowlEngineBackups', hostname: 'VS-SERVER', bot_running: true, service_running: true, bfdb_running: true, dcs_running: true, warnings: [],
  }),
  backup_start: () => startMockJob('backup'),
  restore_start: () => startMockJob('restore'),
  verify_backup: () => startMockJob('verify'),
  backup_job: () => mockJobNow(),
  find_backups: () => [{ path: 'D:\\FowlEngineBackups\\FowlEngine-backup-VS-SERVER-20261008-1412.zip', bytes: 2.1e9, modified: now() }],
  restore_inspect: (a) => ({
    zip: a.zip, zip_bytes: 2.1e9, hostname_now: 'DESKTOP-NEW123', hostname_changed: true, profile_now: 'C:\\Users\\Admin', desktop_user: '.\\Admin',
    has_database: true, postgres_found: null, service_installed: false,
    manifest: { format: 1, created: now(), hostname: 'VS-SERVER', manager_version: '0.2.16', desktop_user: '.\\ATPAdmin', profile: 'C:\\Users\\ATPAdmin',
                bot_dir: 'C:\\VectorStrike\\Tools\\DCSServerBot', roots: mockRoots.slice(0, 3), programs: [], python: 'Python 3.12.4', service_installed: true, warnings: [],
                database: { host: '127.0.0.1', port: 5432, name: 'dcsserverbot', user: 'dcsserverbot', dump: 'database/dcsserverbot.dump', bytes: 48e6, pg_version: 'pg_dump (PostgreSQL) 16.4' } },
    mappings: mockRoots.slice(0, 3).map(r => ({ id: r.id, kind: r.kind, label: r.label, from: r.path, to: r.path.replace('ATPAdmin', 'Admin'), files: r.files, bytes: r.bytes, exists: r.id === 'instance-DCS.vectorstrike_1-a1b2c3', problem: null })),
    checks: [
      { what: 'Python 3.11+ (runs DCSServerBot)', ok: false, detail: 'not found -- DCSServerBot can\'t start without it', install: 'python' },
      { what: "PostgreSQL (the bot's database)", ok: false, detail: 'not installed -- DCSServerBot can\'t start without it', install: 'postgres' },
      { what: 'DCS World Server', ok: true, detail: 'C:\\Program Files\\Eagle Dynamics\\DCS World Server', install: null },
      { what: 'DCS-SimpleRadio Standalone', ok: false, detail: 'not at C:\\Program Files\\DCS-SimpleRadio-Standalone -- install it there (or fix the path in the bot\'s nodes.yaml)', install: null },
    ],
  }),
}

export function installDevMock(): void {
  ;(window as unknown as { __TAURI_INTERNALS__: unknown }).__TAURI_INTERNALS__ = {
    invoke: async (cmd: string, args: Record<string, unknown>) => {
      await new Promise(r => setTimeout(r, 150))
      const h = handlers[cmd] ?? botHandlers[cmd] ?? backupHandlers[cmd]
      return h ? h(args ?? {}) : 'done (mock)'
    },
    transformCallback: () => 0,
  }
}
