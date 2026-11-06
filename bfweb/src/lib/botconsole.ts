// The bot console's pure parts: who may see it, which server controls a state
// allows, turning form text into an action's parameters, and finding the
// table in an action's answer. The page is pages/BotConsolePage.tsx.

import type { AuthUser } from '@shared/web/api'
import type { BotActionInfo, BotParam, BotRole } from '../api'

/** Whether a signed-in user gets the bot console at all: a full admin, or
 *  an ops or moderation role. Each action is then checked by the bot plugin. */
export function canSeeBotConsole(user: Pick<AuthUser, 'is_admin' | 'roles'> | null | undefined): boolean {
  if (!user) return false
  if (user.is_admin) return true
  return (user.roles ?? []).some(r => r === 'ops' || r === 'moderation')
}

/** The client-side twin of console.py's may_use, to grey out what the
 *  server would refuse. */
export function mayUse(roles: readonly string[], role: BotRole): boolean {
  if (roles.includes('admin')) return true
  if (role === 'admin') return false
  return roles.includes(role)
}

/** DCSServerBot's server states, as console/fleet reports them. */
export type ServerControl =
  | 'startup_server' | 'start_server' | 'pause_mission' | 'unpause_mission'
  | 'restart_server' | 'restart_mission' | 'stop_server' | 'shutdown_server' | 'message_server'

/** Which server controls make sense in a state (the same rule as the bot's
 *  own console). Maintenance on/off is offered in every state. */
export function serverControls(status: string | null | undefined): ServerControl[] {
  switch ((status ?? '').toUpperCase()) {
    case 'SHUTDOWN':
      return ['startup_server']
    case 'STOPPED':
      return ['start_server', 'shutdown_server']
    case 'RUNNING':
      return ['pause_mission', 'restart_mission', 'restart_server', 'stop_server', 'shutdown_server', 'message_server']
    case 'PAUSED':
      return ['unpause_mission', 'restart_mission', 'restart_server', 'stop_server', 'shutdown_server', 'message_server']
    default:
      return []
  }
}

/** The colour family a status reads as. */
export function statusTone(status: string | null | undefined): 'ok' | 'warn' | 'down' | 'busy' {
  switch ((status ?? '').toUpperCase()) {
    case 'RUNNING': return 'ok'
    case 'PAUSED': case 'STOPPED': return 'warn'
    case 'LOADING': case 'SHUTTING_DOWN': return 'busy'
    default: return 'down'
  }
}

export type InputKind = 'bool' | 'number' | 'json' | 'text'

/** How a parameter is edited, from its Python annotation and default. */
export function inputKind(p: BotParam): InputKind {
  const t = (p.type ?? '').replace(/\s+/g, '').toLowerCase()
  if (t === 'bool' || t === 'bool|none' || (!t && typeof p.default === 'boolean')) return 'bool'
  if (/^(list|dict|tuple)\b|^list\[|^dict\[/.test(t) || (!t && (Array.isArray(p.default) || (p.default !== null && typeof p.default === 'object')))) return 'json'
  if (t === 'int' || t === 'float' || t === 'int|none' || t === 'float|none' || (!t && typeof p.default === 'number')) return 'number'
  return 'text'
}

/** Whether a text parameter may also be a number ("int | str", e.g. a
 *  mission given by index or by name). */
function takesInt(p: BotParam): boolean {
  return /\bint\b/.test((p.type ?? '').toLowerCase())
}

export type FormValue = string | boolean

/** A parameter's starting value in a form. */
export function initialValue(p: BotParam): FormValue {
  const kind = inputKind(p)
  if (kind === 'bool') return p.default === true
  if (p.default === undefined || p.default === null) return ''
  if (kind === 'json') return JSON.stringify(p.default)
  return String(p.default)
}

/** Form values → the `params` an action is called with. A blank optional
 *  field is left out, so the bot's own default applies. */
export function buildParams(
  params: readonly BotParam[],
  values: Record<string, FormValue | undefined>,
): { params: Record<string, unknown>; errors: Record<string, string> } {
  const out: Record<string, unknown> = {}
  const errors: Record<string, string> = {}
  for (const p of params) {
    const raw = values[p.name]
    const kind = inputKind(p)
    if (kind === 'bool') {
      const v = raw === true || raw === 'true'
      if (p.required || v !== (p.default === true)) out[p.name] = v
      continue
    }
    const text = typeof raw === 'string' ? raw.trim() : ''
    if (!text) {
      if (p.required) errors[p.name] = 'required'
      continue
    }
    if (kind === 'number') {
      const n = Number(text)
      if (!Number.isFinite(n)) errors[p.name] = 'not a number'
      else out[p.name] = n
    } else if (kind === 'json') {
      try {
        out[p.name] = JSON.parse(text)
      } catch {
        errors[p.name] = 'not valid JSON'
      }
    } else {
      out[p.name] = takesInt(p) && /^-?\d+$/.test(text) ? Number(text) : text
    }
  }
  return { params: out, errors }
}

/** "server_name" → "Server name". */
export function label(name: string): string {
  const s = name.replace(/_/g, ' ').trim()
  return s.charAt(0).toUpperCase() + s.slice(1)
}

function isRecord(v: unknown): v is Record<string, unknown> {
  return v !== null && typeof v === 'object' && !Array.isArray(v)
}

/** A list of objects as a table: the columns are every key seen, in first-seen order. */
export function tableOf(v: unknown): { columns: string[]; rows: Record<string, unknown>[] } | null {
  if (!Array.isArray(v) || v.length === 0 || !v.every(isRecord)) return null
  const columns: string[] = []
  for (const row of v) for (const k of Object.keys(row)) if (!columns.includes(k)) columns.push(k)
  return { columns, rows: v }
}

/** The first list-of-objects in an action's data (bans, missions, rows,
 *  players, ...), with its key -- what a generic result view shows as a table. */
export function mainTable(data: unknown): { key: string; columns: string[]; rows: Record<string, unknown>[] } | null {
  if (!isRecord(data)) return null
  for (const [key, v] of Object.entries(data)) {
    const t = tableOf(v)
    if (t) return { key, ...t }
  }
  return null
}

/** One cell's text. */
export function cell(v: unknown): string {
  if (v === null || v === undefined || v === '') return '—'
  if (typeof v === 'boolean') return v ? 'yes' : 'no'
  if (typeof v === 'number') return Number.isInteger(v) ? String(v) : v.toFixed(2)
  if (typeof v === 'string') {
    // ISO timestamps read better short; 9999 is DCSServerBot's "never"
    if (/^9999-/.test(v)) return 'never'
    if (/^\d{4}-\d\d-\d\dT\d\d:\d\d/.test(v)) return v.slice(0, 16).replace('T', ' ')
    return v
  }
  if (Array.isArray(v)) return v.map(cell).join(', ')
  return JSON.stringify(v)
}

/** A cell whose column says it holds seconds (playtime, average_session,
 *  ...) reads as a duration; anything else as `cell`. */
export function cellFor(key: string, v: unknown): string {
  if (typeof v === 'number' && /(^|_)(playtime|duration|uptime|mission_time)$|^average_session$/.test(key)) {
    return duration(v)
  }
  return cell(v)
}

/** Seconds as "1h 02m" / "4m 05s". */
export function duration(seconds: number | null | undefined): string {
  if (seconds === null || seconds === undefined || !Number.isFinite(seconds) || seconds < 0) return '—'
  const s = Math.floor(seconds)
  const h = Math.floor(s / 3600)
  const m = Math.floor((s % 3600) / 60)
  if (h > 0) return `${h}h ${String(m).padStart(2, '0')}m`
  return `${m}m ${String(s % 60).padStart(2, '0')}s`
}

/** Catalog actions grouped by the DCSServerBot plugin that provides them,
 *  in a stable order (mission first: it has most of them). */
export function byPlugin(actions: readonly BotActionInfo[]): [string, BotActionInfo[]][] {
  const order = ['mission', 'scheduler', 'admin', 'punishment', 'monitoring', 'modmanager', 'music']
  const groups = new Map<string, BotActionInfo[]>()
  for (const a of actions) {
    const g = groups.get(a.plugin) ?? []
    g.push(a)
    groups.set(a.plugin, g)
  }
  return [...groups.entries()].sort(([a], [b]) => {
    const ia = order.indexOf(a), ib = order.indexOf(b)
    return (ia < 0 ? 99 : ia) - (ib < 0 ? 99 : ib) || a.localeCompare(b)
  })
}

// ── config editing ─────────────────────────────────────────────────────────

function same(a: unknown, b: unknown): boolean {
  return JSON.stringify(a) === JSON.stringify(b)
}

/** What a form edit changed, as the config save takes it: top-level keys to
 *  write (whole) and to drop. Untouched sections keep their comments. */
export function topLevelChanges(before: unknown, after: unknown): { set: Record<string, unknown>; delete: string[] } {
  const b = isRecord(before) ? before : {}
  const a = isRecord(after) ? after : {}
  const set: Record<string, unknown> = {}
  for (const [k, v] of Object.entries(a)) if (!(k in b) || !same(b[k], v)) set[k] = v
  return { set, delete: Object.keys(b).filter(k => !(k in a)) }
}

export type PathKey = string | number

export function getPath(v: unknown, path: readonly PathKey[]): unknown {
  let cur: unknown = v
  for (const k of path) {
    if (cur === null || typeof cur !== 'object') return undefined
    cur = (cur as Record<string | number, unknown>)[k]
  }
  return cur
}

/** `v` with the value at `path` replaced (undefined = removed), copying only
 *  what is on the way down. Missing objects on the way are created. */
export function setPath(v: unknown, path: readonly PathKey[], value: unknown): unknown {
  if (!path.length) return value
  const [head, ...rest] = path
  if (Array.isArray(v)) {
    const out = [...v]
    if (rest.length === 0 && value === undefined) out.splice(Number(head), 1)
    else out[Number(head)] = setPath(out[Number(head)], rest, value)
    return out
  }
  const obj: Record<string, unknown> = isRecord(v) ? { ...v } : {}
  if (rest.length === 0 && value === undefined) delete obj[String(head)]
  else obj[String(head)] = setPath(obj[String(head)], rest, value)
  return obj
}

/** The scheduler's `schedule` ({"00-12": "YYYYYYN", ...}: hours, then one
 *  letter a day from Monday) as rows. Y = up, N = down, P = up while populated. */
export function scheduleRows(schedule: unknown): { hours: string; days: ('Y' | 'N' | 'P' | '?')[] }[] {
  if (!isRecord(schedule)) return []
  return Object.entries(schedule).map(([hours, v]) => {
    const letters = String(v ?? '').toUpperCase().padEnd(7, '?').slice(0, 7).split('')
    return { hours, days: letters.map(c => (c === 'Y' || c === 'N' || c === 'P' ? c : '?')) }
  })
}

/** The weather fields of a DCSServerBot preset the Presets tab edits by name
 *  (presets_schema.yaml); everything else stays in the preset as it was. */
export const PRESET_FIELDS: { path: string[]; label: string; kind: 'text' | 'number' | 'bool' }[] = [
  { path: ['date'], label: 'Date (YYYY-MM-DD)', kind: 'text' },
  { path: ['start_time'], label: 'Start time (HH:MM)', kind: 'text' },
  { path: ['temperature'], label: 'Temperature °C', kind: 'number' },
  { path: ['qnh'], label: 'QNH (mmHg)', kind: 'number' },
  { path: ['clouds'], label: 'Clouds preset', kind: 'text' },
  { path: ['wind', 'atGround', 'speed'], label: 'Wind ground m/s', kind: 'number' },
  { path: ['wind', 'atGround', 'dir'], label: 'Wind ground °', kind: 'number' },
  { path: ['wind', 'at2000', 'speed'], label: 'Wind 2000 m m/s', kind: 'number' },
  { path: ['wind', 'at2000', 'dir'], label: 'Wind 2000 m °', kind: 'number' },
  { path: ['wind', 'at8000', 'speed'], label: 'Wind 8000 m m/s', kind: 'number' },
  { path: ['wind', 'at8000', 'dir'], label: 'Wind 8000 m °', kind: 'number' },
  { path: ['groundTurbulence'], label: 'Ground turbulence', kind: 'number' },
  { path: ['enable_fog'], label: 'Fog', kind: 'bool' },
  { path: ['fog', 'visibility'], label: 'Fog visibility m', kind: 'number' },
  { path: ['fog', 'thickness'], label: 'Fog thickness m', kind: 'number' },
  { path: ['enable_dust'], label: 'Dust', kind: 'bool' },
  { path: ['dust_density'], label: 'Dust density', kind: 'number' },
  { path: ['halo', 'preset'], label: 'Halo preset', kind: 'text' },
]

/** A preset value in its form field. */
export function presetFieldValue(preset: unknown, path: string[]): string | boolean {
  const v = getPath(preset, path)
  if (typeof v === 'boolean') return v
  if (v === undefined || v === null) return ''
  return typeof v === 'object' ? JSON.stringify(v) : String(v)
}

/** A form field back into the preset. A blank field removes the key. */
export function setPresetField(preset: unknown, path: string[], kind: 'text' | 'number' | 'bool', raw: string | boolean): unknown {
  if (kind === 'bool') return setPath(preset, path, raw === true)
  const text = String(raw).trim()
  if (!text) return setPath(preset, path, undefined)
  if (kind === 'number') {
    const n = Number(text)
    return Number.isFinite(n) ? setPath(preset, path, n) : preset
  }
  // clouds may be a map in the file: keep JSON as JSON
  if (/^[{[]/.test(text)) {
    try {
      return setPath(preset, path, JSON.parse(text))
    } catch {
      // not JSON: plain text
    }
  }
  return setPath(preset, path, text)
}

/** One server-sent `fleet` event's data, or null if it isn't one. */
export function parseFleetEvent(data: string): Record<string, unknown> | null {
  try {
    const v: unknown = JSON.parse(data)
    return isRecord(v) && Array.isArray(v.servers) && Array.isArray(v.nodes) ? v : null
  } catch {
    return null
  }
}
