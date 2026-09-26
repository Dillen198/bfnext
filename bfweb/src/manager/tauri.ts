/**
 * Fowl Engine Manager <-> its Rust side (bfmanager/src-tauri/src/lib.rs).
 *
 * Outside the app (a plain browser on the dev server) there is no Tauri; the
 * calls then reject with a clear message instead of hanging, so the UI can
 * still be laid out and looked at.
 */
import type { Transport } from '../api'

type InvokeFn = <T>(cmd: string, args?: Record<string, unknown>) => Promise<T>

let invokeImpl: InvokeFn | null = null

// `?mock` on the dev server: a fake bridge with sample data (devMock.ts).
// import.meta.env.DEV is a literal `false` in a build, so none of it ships.
const devMock: boolean = import.meta.env.DEV && typeof window !== 'undefined'
  && new URLSearchParams(window.location.search).has('mock')

export const inTauri: boolean = typeof window !== 'undefined' && ('__TAURI_INTERNALS__' in window || devMock)

async function invoke<T>(cmd: string, args?: Record<string, unknown>): Promise<T> {
  if (import.meta.env.DEV && devMock && !('__TAURI_INTERNALS__' in window)) {
    (await import('./devMock')).installDevMock()
  }
  if (!inTauri) throw new Error('Not running inside Fowl Engine Manager (no Tauri) -- open the app itself.')
  if (!invokeImpl) {
    const mod = await import('@tauri-apps/api/core')
    invokeImpl = mod.invoke as InvokeFn
  }
  try {
    return await invokeImpl<T>(cmd, args)
  } catch (e) {
    throw new Error(typeof e === 'string' ? e : e instanceof Error ? e.message : JSON.stringify(e))
  }
}

// ── types (mirrors of the Rust structs) ─────────────────────────────────────

export interface ManagerConfig {
  bot_dir: string | null
  bot_command: string | null
  sync_plugin: boolean
  auto_update: boolean
  channel: 'stable' | 'beta'
  repo: string
  check_hours: number
  update_window: string | null
  github_token: string | null
  /** Start the bot on a signed-in user's desktop (DCS hangs without one). */
  desktop_session: boolean
  /** Whose desktop; null = whoever is signed in at the console. */
  desktop_user: string | null
  lock_after_autologon: boolean
}

export interface ServiceStatus {
  name: string
  installed: boolean
  state: string | null
  start_type: string | null
  account: string | null
  executable: string | null
  foreign_exe: boolean
  pid: number | null
}

export interface SyncReport {
  at: string
  bundle_version: string
  changed: string[]
  skipped_reason: string | null
  backup: string | null
}

export interface Release {
  tag: string
  version: string
  notes: string
  published: string | null
  html_url: string | null
  prerelease: boolean
  setup_name: string
  size: number
}

export interface CheckResult {
  current: string
  latest: Release | null
  update_available: boolean
  checked_at: string
  error: string | null
}

export interface AgentStatus {
  version: string
  pid: number
  started_at: string
  heartbeat: string
  bot_dir: string | null
  service_account?: string
  bot: {
    running: boolean
    pid: number | null
    started_at: string | null
    uptime_secs: number | null
    starts: number
    last_exit: string | null
    last_exit_at: string | null
    paused: boolean
    next_start_in_secs: number | null
    problem: string | null
    session?: string | null
    no_desktop?: boolean
  }
  last_sync: SyncReport | null
  update: CheckResult | null
  update_state: string | null
}

export interface AppState {
  version: string
  exe: string
  data_dir: string
  current_user: string
  hostname: string
  elevated: boolean
  config: ManagerConfig
  service: ServiceStatus | null
  old_service: ServiceStatus | null
  agent: AgentStatus | null
  agent_fresh: boolean
  bot_dir_valid: boolean
  bundle_version: string | null
  plugin_pending: string[]
  plugin_link: string | null
  ops_target: string | null
  ops_error: string | null
  autologon: Autologon
  sessions: UserSession[]
}

export interface Autologon {
  enabled: boolean
  account: string | null
  password_stored: boolean
  broken: boolean
}

export interface UserSession {
  id: number
  user: string
  domain: string
  state: 'active' | 'disconnected'
  logon_age_secs: number | null
}

export interface BotAccount {
  account: string
  has_venv: boolean
  dcs_instances: string[]
}

export interface BotDirCheck {
  valid: boolean
  has_venv_hint: boolean
  ops_target: string | null
  ops_error: string | null
  plugin_installed: boolean
}

// ── commands ─────────────────────────────────────────────────────────────────

export const mgr = {
  state:            () => invoke<AppState>('get_state'),
  saveConfig:       (config: ManagerConfig) => invoke<void>('save_config', { config }),
  detectBotDirs:    () => invoke<string[]>('detect_bot_dirs'),
  detectBotAccounts: () => invoke<BotAccount[]>('detect_bot_accounts'),
  checkBotDir:      (path: string) => invoke<BotDirCheck>('check_bot_dir', { path }),
  installService:   (account: string | null, password: string | null) =>
                      invoke<void>('install_service', { account, password }),
  serviceControl:   (action: 'start' | 'stop' | 'restart') => invoke<void>('service_control', { action }),
  uninstallService: () => invoke<void>('uninstall_service'),
  disableOldService: () => invoke<void>('disable_old_service'),
  enableAutologon:  (account: string, password: string) => invoke<void>('enable_autologon', { account, password }),
  disableAutologon: () => invoke<void>('disable_autologon'),
  agentCommand:     (name: 'restart-bot' | 'stop-bot' | 'start-bot' | 'check-update' | 'install-update') =>
                      invoke<void>('agent_command', { name }),
  syncPluginNow:    () => invoke<string>('sync_plugin_now'),
  checkUpdate:      () => invoke<CheckResult>('check_update'),
  installUpdate:    () => invoke<string>('install_update'),
  readLog:          (name: 'agent' | 'bot', lines: number) => invoke<string[]>('read_log', { name, lines }),
  openPath:         (which: 'data' | 'logs' | 'backups' | 'bot') => invoke<void>('open_path', { which }),
}

// ── the api.ts transport: the OPS page's requests, answered locally ──────────

const LOCAL_ADMIN = {
  user: { discord_id: 'local', username: 'Local admin', avatar: null, is_admin: true, ucid: null, side: null },
}

function json(status: number, body: unknown): Response {
  return new Response(JSON.stringify(body), { status, headers: { 'content-type': 'application/json' } })
}

/**
 * `/admin/ops/*` goes to the FowlEngine plugin's OPS API through the Rust
 * side (which reads the bot's key from its config); `/auth/me` says "local
 * admin" -- whoever can run this app elevated already owns the box.
 * Everything else the dashboard would ask bfdb for doesn't exist here.
 */
export const managerTransport: Transport = async (method, path, body) => {
  const clean = path.replace(/([?&])instance=[^&]*&?/, '$1').replace(/[?&]$/, '')
  if (clean.startsWith('/auth/me')) return json(200, LOCAL_ADMIN)
  if (clean.startsWith('/admin/ops/')) {
    try {
      const r = await invoke<{ status: number; content_type: string; body: string }>('ops_request', {
        method,
        path: clean.slice('/admin/ops/'.length),
        body: body === undefined ? null : JSON.stringify(body),
      })
      return new Response(r.body, { status: r.status, headers: { 'content-type': r.content_type } })
    } catch (e) {
      return json(502, { error: e instanceof Error ? e.message : String(e) })
    }
  }
  return json(404, { error: `not available in Fowl Engine Manager: ${clean}` })
}
