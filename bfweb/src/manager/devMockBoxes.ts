/**
 * devMock's server boxes: what `ops_request` answers for each one, so ALL
 * SERVERS, DCS CONTROL, LIVE LOGS, BACKUP and SERVER OPS can be clicked
 * through without a real box. Dev only (imported by devMock.ts).
 *
 *   campaign  VS-SERVER: vs1 (running, players on) + vs2 (shut down)
 *   range     RANGE-PC:  range (running); remote backups on
 *
 * Logs grow while you watch; DCS actions take a few seconds; a backup runs
 * for ~10 s. `?mock&oldrange` makes the range box an older plugin (404s for
 * the new routes) to see the "update the plugin" message.
 */

type St = 'RUNNING' | 'PAUSED' | 'STOPPED' | 'SHUTDOWN' | 'LOADING' | 'SHUTTING_DOWN'

interface Dcs {
  name: string
  kind: 'campaign' | 'range'
  status: St
  players: number
  mission: string
  map: string
  last_action: { action: string; state: 'running' | 'done' | 'failed'; at: string; finished: string | null; error: string | null } | null
  settleAt: number
  settleTo: St
}

interface Box {
  hostname: string
  bootedAt: number
  dcs: Dcs[]
  logsStart: number
  backup: { job: Record<string, unknown> | null; startedAt: number; backups: { path: string; bytes: number; modified: string }[]; handled: Record<string, unknown>[] } | null
  oldPlugin?: boolean
}

const now = () => new Date().toISOString()
const t0 = Date.now()

function dcs(name: string, kind: Dcs['kind'], status: St, players: number, mission: string, map: string): Dcs {
  return { name, kind, status, players, mission, map, last_action: null, settleAt: 0, settleTo: status }
}

const boxes: Record<string, Box> = {
  campaign: {
    hostname: 'VS-SERVER', bootedAt: t0 - 3 * 86400e3,
    dcs: [dcs('vs1', 'campaign', 'RUNNING', 14, 'Operation Fowl Intent', 'Caucasus'),
          dcs('vs2', 'campaign', 'SHUTDOWN', 0, 'Fowl Intent Syria', 'Syria')],
    logsStart: t0, backup: null,
  },
  range: {
    hostname: 'RANGE-PC', bootedAt: t0 - 9 * 3600e3,
    dcs: [dcs('range', 'range', 'RUNNING', 3, 'Vector Strike Training Range', 'Caucasus')],
    logsStart: t0,
    backup: { job: null, startedAt: 0, handled: [],
              backups: [{ path: 'D:\\FowlEngineBackups\\FowlEngine-backup-RANGE-PC-20261024-0400.zip', bytes: 1.4e9, modified: now() }] },
    oldPlugin: typeof window !== 'undefined' && new URLSearchParams(window.location.search).has('oldrange'),
  },
}

/** The box behind a picker id (this box's bot is the campaign server in the mock). */
export function boxFor(id: string | null): string {
  return id === 'range' ? 'range' : 'campaign'
}

function settle(b: Box) {
  for (const d of b.dcs) {
    if (d.settleAt && Date.now() >= d.settleAt) {
      d.status = d.settleTo
      d.settleAt = 0
      if (d.status !== 'RUNNING') d.players = 0
      if (d.last_action?.state === 'running') d.last_action = { ...d.last_action, state: 'done', finished: now() }
    }
  }
  const j = b.backup?.job
  if (j && j.running) {
    const t = (Date.now() - b.backup!.startedAt) / 10_000
    j.done_bytes = Math.round(Math.min(1, t) * (j.total_bytes as number))
    if (t >= 1) {
      const zip = `D:\\FowlEngineBackups\\FowlEngine-backup-${b.hostname}-${new Date().toISOString().slice(0, 16).replace(/[-:T]/g, '')}.zip`
      Object.assign(j, { running: false, phase: 'done', finished_at: now(), output: `${zip} (1.4 GB)`, zip_path: zip, current: null })
      if (j.kind === 'backup') b.backup!.backups.unshift({ path: zip, bytes: 1.4e9, modified: now() })
    }
  }
}

const can = (st: St) => {
  const up = st === 'RUNNING' || st === 'PAUSED' || st === 'STOPPED'
  const down = st === 'SHUTDOWN'
  return { start: down || st === 'STOPPED', stop: up || st === 'LOADING', restart: up || down, mission_restart: up }
}

// ── logs: lines appear over time ─────────────────────────────────────────────

const ENGINE = ['[INFO] (1) bflib::db: saved campaign state', '[INFO] (4) bflib::jtac: JTAC 1 lased target at BP-7',
  '[WARN] (2) bflib::spawnctx: slot 41 spawned with no warehouse stock', '[INFO] (1) bflib: tick 16 ms',
  '[ERROR] (7) bflib::cargo: crate 12 has no parent unit', '[DEBUG] (3) bflib::ewr: 4 contacts']
const RANGE = ['[INFO] (1) bfrange: pass scored 82/100 (Viper 1-1)', '[INFO] (2) bfrange::aar: contact made, 2.1 nm',
  '[WARN] (3) bfrange::strafe: pass too low, not scored', '[INFO] (1) bfrange: bombing run 14 ft CEP']
const BOT = ['INFO     FowlEngine/ops: OPS API on the WebService at /stats/fowlengine/ops/*',
  'INFO     ServiceBus: heartbeat', 'WARNING  FowlEngine/autoupdate: GitHub rate limit low (12 left)',
  'INFO     FowlEngine: status embed updated', 'ERROR    FowlEngine: bfdb did not answer /api/version in 4 s']
const DCSLOG = ['INFO    APP (Main): frame 16 ms', 'INFO    SCRIPTING (Main): bflib: tick',
  'WARNING LOG (5212): 3 duplicate message(s) skipped', 'ERROR   SCRIPTING (Main): [string "..."]:12: attempt to index a nil value']

function linesFor(b: Box, which: string): string[] {
  if (which.startsWith('engine:')) return b.hostname === 'RANGE-PC' ? RANGE : ENGINE
  if (which.startsWith('dcs:')) return DCSLOG
  return BOT
}

function stamp(ms: number, which: string): string {
  const d = new Date(ms)
  const hms = d.toTimeString().slice(0, 8)
  return which.startsWith('engine:') ? hms : `${d.toISOString().slice(0, 10)} ${hms}.${String(d.getMilliseconds()).padStart(3, '0')}`
}

/** Line n of a log (every 900 ms one more). */
function lineAt(b: Box, which: string, n: number): string {
  const pool = linesFor(b, which)
  return `${stamp(b.logsStart - 300 * 900 + n * 900, which)} ${pool[(n * 7 + which.length) % pool.length]}`
}

function follow(b: Box, which: string, offset: number | null) {
  const count = 300 + Math.floor((Date.now() - b.logsStart) / 900)
  const from = offset === null ? Math.max(0, count - 200) : offset > count ? Math.max(0, count - 200) : offset
  const lines: string[] = []
  for (let n = from; n < count; n++) lines.push(lineAt(b, which, n))
  return {
    which, path: pathOf(b, which), lines, offset: count, ident: '1:1', size: count * 80, more: false, skipped: 0,
    reset: offset !== null && offset > count, reason: offset === null ? 'start' : offset > count ? 'truncated' : null,
  }
}

function pathOf(b: Box, which: string): string {
  const home = b.hostname === 'RANGE-PC' ? 'C:\\Users\\Admin\\Saved Games' : 'C:\\Users\\ATPAdmin\\Saved Games'
  const [kind, server] = which.split(':')
  if (kind === 'engine') return `${home}\\DCS.${server}\\Logs\\${b.hostname === 'RANGE-PC' ? 'bfrange.txt' : 'bfnext.txt'}`
  if (kind === 'dcs') return `${home}\\DCS.${server}\\Logs\\dcs.log`
  if (kind === 'bfdb') return `${home}\\bfdb\\Logs\\bfdb.log`
  return `C:\\DCSServerBot\\logs\\dcssb-${b.hostname}.log`
}

function logList(b: Box) {
  const out = [
    { id: 'bot', label: 'DCSServerBot', group: 'bot', server: null },
    { id: 'bfdb', label: 'bfdb', group: 'bot', server: null },
    { id: 'netidx', label: 'netidx resolver', group: 'bot', server: null },
  ]
  for (const d of b.dcs) {
    out.push({ id: `engine:${d.name}`, label: `${d.name}: engine (${d.kind === 'range' ? 'bfrange.txt' : 'bfnext.txt'})`, group: 'engine', server: d.name as never })
    out.push({ id: `dcs:${d.name}`, label: `${d.name}: DCS (dcs.log)`, group: 'dcs', server: d.name as never })
  }
  return { logs: out.map(e => ({ ...e, path: pathOf(b, e.id), exists: true, size: 1.2e6, modified: Date.now() / 1000 })) }
}

// ── /status ──────────────────────────────────────────────────────────────────

function status(b: Box) {
  const range = b.hostname === 'RANGE-PC'
  return {
    generated: now(),
    host: {
      hostname: b.hostname, os: 'Windows 11 (10.0.26200)', python: '3.12.4', boot_time: new Date(b.bootedAt).toISOString(),
      uptime_secs: Math.round((Date.now() - b.bootedAt) / 1000), cpu_percent: range ? 22 : 41,
      memory: { total: 64e9, available: 30e9, percent: 53 }, disk: { path: 'C:', total: 1e12, free: 410e9 },
      service: { name: 'FowlEngine', installed: true, state: 'RUNNING', start_type: 'AUTO_START (DELAYED)', account: 'LocalSystem', restart_on_failure: true, this_process_is_service: true },
      auto_reboot_on_bsod: true, reboot_pending: false, unexpected_shutdowns: [],
    },
    bot: { pid: 9120, plugin_started_at: new Date(t0 - 3600e3).toISOString(), dcsserverbot_version: 'v3.0.4.27', node: b.hostname, cwd: 'C:\\DCSServerBot' },
    bfdb: { managed: true, exe: 'C:\\VectorStrike\\bfdb.exe', home: 'C:\\VectorStrike', running: true, pid: 7712, healthy: true,
            started_at: t0 / 1000 - 3600, uptime_secs: 3600 + Math.round((Date.now() - t0) / 1000), relaunches: 0, last_exit_code: null,
            failing_checks: 0, resolver_listening: true, resolver_owned: true, last_swap: null, probation: null, exe_sha256: 'ab12cd34ef56', pending: null },
    bfdb_build: { version: '0.9.0', git: range ? '16b1ce5' : 'f00dbabe', built: now() },
    servers: b.dcs.map(d => ({
      name: d.name, status: d.status, node: b.hostname, remote: false, kind: d.kind, players: d.players, restart_time: null,
      mission: d.mission, map: d.map, dll_name: d.kind === 'range' ? 'bfrange.dll' : 'bflib.dll', dll_path: `C:\\Saved Games\\DCS.${d.name}\\Scripts\\bflib.dll`,
      staging_dir: 'C:\\VectorStrike\\_staging', has_bfbinaries: true, dll_sha256: range ? '9f8e7d6c5b4a39281706' : 'a1b2c3d4e5f60718293a',
      dll_mtime: t0 / 1000, pending: d.name === 'vs2' ? { path: 'C:\\VectorStrike\\_staging\\bflib.dll.pending', size: 41e6, sha256: 'feedface01', source: 'autoupdate', tag: 'engine-2026.10.24-0400-ab12cd3' } : null,
      loaded_build: d.status === 'RUNNING' ? { name: 'bflib', version: '0.9.0', git: range ? '16b1ce5' : 'f00dbabe', built: now() } : undefined, backups: [],
    })),
    bftools: null,
    updates: {
      config: { enabled: true, source: 'github', repo: 'Dillen198/bfnext', folder: null, has_token: false, tag_prefix: 'engine-', channel: 'stable',
                check_minutes: 60, files: ['bflib.dll', 'bfdb.exe'], apply: 'when_idle', bfdb_apply: 'when_idle', idle_minutes: 10, apply_window: null,
                probation_minutes: 30, load_timeout_minutes: 10, rollback_on_crash: true, bot_plugin: true, campaigns: false, paused: false },
      busy: null,
      last_check: { ok: true, at: now(), message: 'up to date', latest: 'engine-2026.10.24-0400-ab12cd3', reason: 'scheduled' },
      latest: { tag: 'engine-2026.10.24-0400-ab12cd3', git: 'ab12cd3', built: now(), channel: 'stable', notes: 'Range: AAR scoring fixed.', files: {} },
      installed: {}, bad: [], probation: [], idle_since: {}, history: [],
    },
    issues: { enabled: true, last_scan: now(), open: range ? 0 : 2, new: 0, errors: range ? 0 : 1, github: false,
              sources: b.dcs.map(d => ({ source: `engine:${d.name}`, path: pathOf(b, `engine:${d.name}`), exists: true })) },
    backups: { bfdb_exe: [], db_snapshots: [] },
  }
}

// ── the router ───────────────────────────────────────────────────────────────

type Res = { status: number; content_type: string; body: string }
const ok = (body: unknown, status = 200): Res => ({ status, content_type: 'application/json', body: JSON.stringify(body) })
const err = (status: number, error: string): Res => ok({ ok: false, error }, status)
const NEW_ROUTES = /^(dcs|logs\/list|logs\/follow|backup)/

export function opsRequest(serverId: string | null, method: string, rawPath: string, body: string | null): Res {
  const b = boxes[boxFor(serverId)]
  settle(b)
  const [path, query] = rawPath.split('?', 2)
  const q = new URLSearchParams(query ?? '')
  const doc = body ? JSON.parse(body) as Record<string, unknown> : {}
  if (b.oldPlugin && NEW_ROUTES.test(path)) return ok({ detail: 'Not Found' }, 404)

  if (path === 'status') return ok(status(b))
  if (path === 'dcs' && method === 'GET') {
    return ok({ actions: ['start', 'stop', 'restart', 'mission_restart'], servers: b.dcs.map(d => ({
      name: d.name, status: d.status, node: b.hostname, players: d.players, mission: d.mission, map: d.map, kind: d.kind,
      can: can(d.status), blocked: null, last_action: d.last_action,
    })) })
  }
  const m = /^dcs\/(start|stop|restart|mission_restart)$/.exec(path)
  if (m) {
    const action = m[1]
    const d = b.dcs.find(x => x.name === doc.server)
    if (!d) return err(404, `no DCS server named '${String(doc.server)}' on this bot (it has: ${b.dcs.map(x => x.name).join(', ')})`)
    if (d.last_action?.state === 'running') return err(409, `a ${d.last_action.action} of ${d.name} is already under way`)
    if (!can(d.status)[action as keyof ReturnType<typeof can>]) return err(409, `${d.name} is ${d.status}: can't ${action.replace('_', ' ')} now`)
    const players = d.players
    d.last_action = { action, state: 'running', at: now(), finished: null, error: null }
    d.status = action === 'stop' ? 'SHUTTING_DOWN' : action === 'mission_restart' ? 'LOADING' : action === 'start' ? 'LOADING' : 'SHUTTING_DOWN'
    d.settleTo = action === 'stop' ? 'SHUTDOWN' : 'RUNNING'
    d.settleAt = Date.now() + (action === 'mission_restart' ? 3000 : 5000)
    const msg = { start: 'starting DCS', stop: 'shutting DCS down', restart: 'restarting DCS', mission_restart: 'restarting the mission' }[action]
    return ok({ ok: true, message: `${d.name}: ${msg}${players ? ` (${players} player(s) on)` : ''}`, server: d.name, action: d.last_action })
  }
  if (path === 'logs/list') return ok(logList(b))
  if (path === 'logs/follow') {
    const off = q.get('offset')
    return ok(follow(b, q.get('which') ?? 'bot', off === null ? null : Number(off)))
  }
  if (path === 'logs') {
    const which = q.get('which') ?? 'bot'
    return ok({ which, path: pathOf(b, which), lines: follow(b, which, null).lines, size: 1.2e6 })
  }
  if (path.startsWith('backup')) {
    if (!b.backup) return err(409, 'remote backups are not turned on in this box\'s Fowl Engine Manager (on the box: Manager -> SETTINGS -> Remote backups)')
    const bk = b.backup
    if (path === 'backup') {
      return ok({ ok: true, enabled: true, updated: now(), updated_unix: Date.now() / 1000, age_secs: 1, stale: false, hostname: b.hostname,
                  manager_version: '0.3.3', default_dest: 'D:\\FowlEngineBackups', job: bk.job, backups: bk.backups, handled: bk.handled, problem: null })
    }
    if (path === 'backup/restore') return err(403, 'a restore overwrites the box and is only done there')
    const kind = path.slice('backup/'.length)
    const id = `${Date.now()}000000-${kind}`
    if (kind === 'cancel') {
      if (bk.job?.running) Object.assign(bk.job, { running: false, phase: 'failed', error: 'cancelled', finished_at: now() })
    } else {
      if (bk.job?.running) return err(409, `a ${String(bk.job.kind)} is already running on this box`)
      bk.startedAt = Date.now()
      bk.job = { id: 1, kind: kind === 'verify' ? 'verify' : 'backup', running: true, phase: kind === 'verify' ? 'Reading every file back' : 'Copying DCS server DCS.range',
                 done_bytes: 0, total_bytes: 1.9e9, current: 'C:\\Users\\Admin\\Saved Games\\DCS.range\\bfdb\\db', log: ['Looking at what to back up', 'Shadow copy of C: made'],
                 warnings: [], error: null, output: null, next_steps: [], started_at: now(), finished_at: null, report: null, zip_path: null, log_file: null }
    }
    bk.handled.unshift({ id, kind, at: now(), accepted: true, message: kind === 'cancel' ? 'cancelling' : kind === 'verify' ? `checking ${String(doc.zip)}` : 'starting a backup of this box' })
    return ok({ ok: true, message: kind === 'verify' ? 'check asked for' : kind === 'cancel' ? 'cancel asked for' : 'backup asked for', request: id })
  }
  if (path === 'issues') return ok({ summary: status(b).issues, issues: [] })
  if (path === 'archive') return ok({ root: 'C:\\DCSServerBot\\logs\\archive', keep_days: 90, sources: [] })
  if (path === 'config') {
    return ok({ path: 'C:\\DCSServerBot\\config\\plugins\\fowlengine.yaml', mtime: t0 / 1000, masked: 2, mask: '__SECRET__', backups: [],
                yaml: 'DEFAULT:\n  brand_name: "Vector Strike"\n  bfdb:\n    manage: true\n    dcsserverbot_api_key: __SECRET__\n  ops_api:\n    api_key: __SECRET__\n    require_own_key: true\n' })
  }
  if (method === 'POST') return ok({ ok: true, message: `${b.hostname}: done (mock)` })
  return ok({ detail: 'Not Found' }, 404)
}
