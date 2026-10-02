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

export function installDevMock(): void {
  ;(window as unknown as { __TAURI_INTERNALS__: unknown }).__TAURI_INTERNALS__ = {
    invoke: async (cmd: string, args: Record<string, unknown>) => {
      await new Promise(r => setTimeout(r, 150))
      const h = handlers[cmd] ?? botHandlers[cmd]
      return h ? h(args ?? {}) : 'done (mock)'
    },
    transformCallback: () => 0,
  }
}
