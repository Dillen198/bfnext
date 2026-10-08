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
  /** the bot runs a later plugin than this app's bundle (from an engine release): why sync leaves it */
  plugin_newer?: string | null
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

// ── BOT CONFIG (botcfg.rs) ──────────────────────────────────────────────────

/** Error prefixes the Rust side uses (a command error is just a string). */
export const CONFLICT = 'CONFLICT:'
export const MANUAL = 'MANUAL:'

export interface BotConfigFile {
  /** '/'-separated, relative to <bot>\config */
  rel: string
  size: number
  modified: string | null
  label: string
}

export interface BotConfigList {
  dir: string
  files: BotConfigFile[]
}

export interface BotConfigText {
  rel: string
  path: string
  text: string
  /** of the bytes on disk -- write_bot_config refuses if the file no longer matches */
  sha256: string
}

export interface ConfigDiag {
  line: number | null
  column: number | null
  message: string
}

export interface ConfigValidation {
  ok: boolean
  errors: ConfigDiag[]
  warnings: ConfigDiag[]
}

export interface ConfigWritten {
  sha256: string
  backup: string | null
}

export interface ConfigChange {
  line: number
  before: string[]
  after: string[]
}

export interface ConfigCheckAction {
  kind: 'generate_secret' | 'public_key' | 'set'
  label: string
  rel: string
  path: string[]
  value: string | null
}

export interface ConfigCheck {
  id: string
  level: 'ok' | 'warn' | 'info'
  title: string
  message: string
  fix: string | null
  /** secret values arrive masked */
  current: string | null
  action: ConfigCheckAction | null
}

export interface PublicKeyInfo {
  key: string
  key_id: string
  path: string
}

// ── BACKUP & RESTORE (backup.rs) ────────────────────────────────────────────

export type RootKind = 'bot' | 'instance' | 'bfdb' | 'extra' | 'program' | 'netidx'

export interface BackupRoot {
  id: string
  kind: RootKind
  label: string
  path: string
  files: number
  bytes: number
  include: boolean
  note: string | null
}

export interface BackupProgram { what: string; path: string }

export interface BackupPlan {
  roots: BackupRoot[]
  database: { host: string; port: number; name: string; user: string; pg_dump: string | null; problem: string | null } | null
  programs: BackupProgram[]
  default_dest: string
  hostname: string
  bot_running: boolean
  service_running: boolean
  bfdb_running: boolean
  dcs_running: boolean
  warnings: string[]
}

export interface BackupOptions {
  dest_dir: string
  roots: string[]
  extra_paths: string[]
  database: boolean
  stop_bot: boolean
  /** read every file from a Windows shadow copy (files in use come out whole) */
  shadow_copy: boolean
}

export interface BackupJob {
  id: number
  kind: 'backup' | 'restore' | 'verify'
  running: boolean
  phase: string
  done_bytes: number
  total_bytes: number
  current: string | null
  log: string[]
  warnings: string[]
  error: string | null
  output: string | null
  next_steps: string[]
  started_at: string
  finished_at: string | null
  report: VerifyReport | null
  zip_path: string | null
  log_file: string | null
}

export interface CheckLine { ok: boolean; info: boolean; text: string }

export interface VerifySection {
  label: string
  kind: string
  path: string
  status: 'ok' | 'warn' | 'bad'
  files_on_disk: number
  files_in_zip: number
  bytes_in_zip: number
  checks: CheckLine[]
  skipped: string[]
}

export interface VerifyReport {
  zip: string
  ok: boolean
  created: string
  hostname: string
  manager_version: string
  entries: number
  bytes: number
  zip_bytes: number
  sections: VerifySection[]
  corrupt: string[]
  problems: string[]
}

export interface FoundBackup { path: string; bytes: number; modified: string | null }

export interface BackupManifest {
  format: number
  created: string
  hostname: string
  manager_version: string
  desktop_user: string | null
  profile: string | null
  bot_dir: string | null
  roots: BackupRoot[]
  database: { host: string; port: number; name: string; user: string; dump: string | null; bytes: number; pg_version: string | null } | null
  programs: BackupProgram[]
  python: string | null
  service_installed: boolean
  warnings: string[]
}

export interface RestoreMapping {
  id: string
  kind: RootKind
  label: string
  from: string
  to: string
  files: number
  bytes: number
  exists: boolean
  problem: string | null
}

export interface RestoreCheck { what: string; ok: boolean; detail: string; install: 'python' | 'postgres' | null }

export interface RestorePreview {
  zip: string
  zip_bytes: number
  manifest: BackupManifest
  mappings: RestoreMapping[]
  hostname_now: string
  hostname_changed: boolean
  profile_now: string | null
  desktop_user: string | null
  checks: RestoreCheck[]
  has_database: boolean
  postgres_found: string | null
  service_installed: boolean
}

export interface RestoreOptions {
  zip: string
  targets: { id: string; to: string }[]
  restore_database: boolean
  pg_password: string | null
  install_python: boolean
  install_postgres: boolean
  install_service: boolean
  desktop_user: string | null
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
  openPath:         (which: 'data' | 'logs' | 'backups' | 'bot' | 'bot-config') => invoke<void>('open_path', { which }),
  listBotConfigs:   () => invoke<BotConfigList>('list_bot_configs'),
  readBotConfig:    (rel: string) => invoke<BotConfigText>('read_bot_config', { rel }),
  validateBotConfig: (rel: string, text: string) => invoke<ConfigValidation>('validate_bot_config', { rel, text }),
  writeBotConfig:   (rel: string, text: string, expectedSha: string) =>
                      invoke<ConfigWritten>('write_bot_config', { rel, text, expectedSha }),
  configChecks:     () => invoke<ConfigCheck[]>('config_checks'),
  generateSecret:   () => invoke<string>('generate_secret'),
  readPublicKey:    (path: string | null) => invoke<PublicKeyInfo>('read_public_key', { path }),
  previewConfigValue: (rel: string, path: string[], value: string) =>
                      invoke<{ change: ConfigChange; sha256: string }>('preview_config_value', { rel, path, value }),
  setConfigValue:   (rel: string, path: string[], value: string, expectedSha: string) =>
                      invoke<{ written: ConfigWritten; change: ConfigChange }>('set_config_value', { rel, path, value, expectedSha }),
  backupPlan:       () => invoke<BackupPlan>('backup_plan'),
  backupStart:      (opts: BackupOptions) => invoke<void>('backup_start', { opts }),
  backupJob:        () => invoke<BackupJob | null>('backup_job'),
  backupCancel:     () => invoke<void>('backup_cancel'),
  findBackups:      () => invoke<FoundBackup[]>('find_backups'),
  restoreInspect:   (zip: string) => invoke<RestorePreview>('restore_inspect', { zip }),
  restoreStart:     (opts: RestoreOptions) => invoke<void>('restore_start', { opts }),
  revealBackup:     (path: string) => invoke<void>('reveal_backup', { path }),
  verifyBackup:     (zip: string) => invoke<void>('verify_backup', { zip }),
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
