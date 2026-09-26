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

export function installDevMock(): void {
  ;(window as unknown as { __TAURI_INTERNALS__: unknown }).__TAURI_INTERNALS__ = {
    invoke: async (cmd: string, args: Record<string, unknown>) => {
      await new Promise(r => setTimeout(r, 150))
      const h = handlers[cmd]
      return h ? h(args ?? {}) : 'done (mock)'
    },
    transformCallback: () => 0,
  }
}
