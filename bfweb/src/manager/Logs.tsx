import { useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Play, Pause, Trash2 } from '@icons'
import { mgr, opsCall, OpsError, type ServerView } from './tauri'
import { opsId } from './pick'
import { Btn, Note, Pill } from './ui'
import { MONO, DIM, RED, AMBER, OK, input, fmtBytes } from './style'
import { appendCapped, levelOf, makeFilter, resetNote, type FollowAnswer, type Level } from './logLines'

interface LogEntry { id: string; label: string; group: string; server: string | null; path: string; exists: boolean; size: number }

/** This PC's own Manager logs (read from disk, not over OPS). */
const MGR_LOGS = [
  { id: 'mgr:agent', label: 'Manager service (agent.log)', name: 'agent' as const },
  { id: 'mgr:bot', label: 'DCSServerBot console (bot-console.log)', name: 'bot' as const },
]

const GROUPS: Record<string, string> = { bot: 'Bot, bfdb, netidx', engine: 'Engine (bflib / bfrange)', dcs: 'DCS', extra: 'Other' }
const POLL_MS = 1500
const NO_LINES: string[] = []

const COLOR: Record<NonNullable<Level>, string> = { error: RED, warn: AMBER, info: 'var(--text)', debug: 'var(--text-dim)' }
const RANK: Record<string, number> = { all: 0, warn: 1, error: 2 }

/** Follow one log over the OPS API: offsets from logs/follow, ~1.5 s apart
 *  while the window is visible and not paused, straight on while it says
 *  there is more. Unmounting (another tab) stops it. */
interface Followed {
  /** which server + log this is (state from another one is ignored) */
  key: string
  lines: string[]
  info: { path: string; size: number; missing: boolean } | null
  error: Error | null
}

function useFollow(target: { id: string | null; name: string } | null, which: string | null, paused: boolean) {
  const key = target && which ? `${target.id ?? 'local'}|${which}` : ''
  const [st, setSt] = useState<Followed>({ key: '', lines: [], info: null, error: null })
  const pausedRef = useRef(paused)
  useEffect(() => { pausedRef.current = paused }, [paused])
  const id = target?.id ?? null
  const name = target?.name ?? ''

  useEffect(() => {
    if (!key || !which) return
    let stop = false
    let timer: ReturnType<typeof setTimeout> | undefined
    let offset: number | null = null
    let ident: string | null = null
    const update = (f: (s: Followed) => Followed) =>
      setSt(s => f(s.key === key ? s : { key, lines: [], info: null, error: null }))
    const later = (ms: number) => { if (!stop) timer = setTimeout(tick, ms) }
    async function tick() {
      if (stop) return
      if (pausedRef.current || document.hidden) return later(1000)
      const q = new URLSearchParams({ which: which! })
      if (offset !== null) q.set('offset', String(offset))
      if (ident) q.set('ident', ident)
      try {
        const a = await opsCall<FollowAnswer>(id, name, 'GET', `logs/follow?${q}`)
        if (stop) return
        const note = a.reset || a.reason === 'skipped' ? resetNote(a) : null
        update(s => ({
          key, error: null, info: { path: a.path, size: a.size, missing: !!a.missing },
          lines: appendCapped(note ? [...s.lines, note] : s.lines, a.lines),
        }))
        offset = a.offset
        ident = a.ident
        later(a.more ? 50 : POLL_MS)
      } catch (e) {
        if (stop) return
        update(s => ({ ...s, error: e instanceof Error ? e : new Error(String(e)) }))
        // an old plugin won't grow the route: stop asking
        if (!(e instanceof OpsError && e.skew)) later(5000)
      }
    }
    const onVis = () => { if (!document.hidden && !stop) { clearTimeout(timer); void tick() } }
    document.addEventListener('visibilitychange', onVis)
    void tick()
    return () => { stop = true; clearTimeout(timer); document.removeEventListener('visibilitychange', onVis) }
  }, [key, id, name, which])

  const cur = st.key === key ? st : { key, lines: NO_LINES, info: null, error: null }
  return { ...cur, clear: () => setSt(s => ({ ...s, lines: [] })) }
}

/** LOGS: every log of the picked server box, followed live; plus this PC's
 *  own Manager logs. */
export default function Logs({ server }: { server: ServerView | null }) {
  const [pick, setPick] = useState<string | null>(null)
  const [paused, setPaused] = useState(false)
  const [filter, setFilter] = useState('')
  const [minLevel, setMinLevel] = useState<'all' | 'warn' | 'error'>('all')

  const list = useQuery({
    queryKey: ['mgr', 'logs-list', server?.id],
    queryFn: () => opsCall<{ logs: LogEntry[] }>(opsId(server), server!.name, 'GET', 'logs/list'),
    enabled: !!server,
    refetchInterval: 30_000,
    retry: false,
  })
  const opsLogs = useMemo(() => list.data?.logs ?? [], [list.data])
  // a pick from another server (switched at the top) that this one hasn't got: its first log
  const known = (id: string) => MGR_LOGS.some(m => m.id === id) || opsLogs.some(l => l.id === id)
  const which = pick && (known(pick) || !list.data) ? pick : opsLogs[0]?.id ?? MGR_LOGS[0].id
  const mgrLog = MGR_LOGS.find(m => m.id === which)
  const opsWhich = mgrLog ? null : which

  const target = server ? { id: opsId(server), name: server.name } : null
  const follow = useFollow(target, opsWhich, paused)
  const local = useQuery({
    queryKey: ['mgr', 'log', mgrLog?.name],
    queryFn: () => mgr.readLog(mgrLog!.name, 2000),
    enabled: !!mgrLog,
    refetchInterval: paused ? false : 2_000,
    retry: false,
  })
  const lines = mgrLog ? (local.data ?? NO_LINES) : follow.lines
  const err = mgrLog ? local.error : follow.error

  const shown = useMemo(() => {
    const f = makeFilter(filter)
    const min = RANK[minLevel]
    return lines.filter(l => {
      if (!f(l)) return false
      if (!min) return true
      const lv = levelOf(l)
      return lv === 'error' || (min === 1 && lv === 'warn')
    })
  }, [lines, filter, minLevel])

  // stay at the bottom while following, unless scrolled up to read
  const box = useRef<HTMLPreElement>(null)
  const atBottom = useRef(true)
  useLayoutEffect(() => {
    const el = box.current
    if (el && atBottom.current) el.scrollTop = el.scrollHeight
  }, [shown])

  const grouped = useMemo(() => {
    const g = new Map<string, LogEntry[]>()
    for (const l of opsLogs) g.set(l.group, [...(g.get(l.group) ?? []), l])
    return [...g.entries()]
  }, [opsLogs])
  const entry = opsLogs.find(l => l.id === which)
  const skew = list.error instanceof OpsError && list.error.skew

  return (
    <div className="p-5 flex flex-col" style={{ flex: 1, minHeight: 0 }}>
      <div className="flex items-center gap-2" style={{ marginBottom: 10, flexWrap: 'wrap' }}>
        <select aria-label="Which log" value={which} onChange={e => setPick(e.target.value)} style={{ ...input, width: 'auto', minWidth: 280, ...MONO, fontSize: '0.68rem' }}>
          {server && grouped.map(([g, ls]) => (
            <optgroup key={g} label={`${server.name} · ${GROUPS[g] ?? g}`}>
              {ls.map(l => <option key={l.id} value={l.id}>{l.label}{l.exists ? '' : ' (not there yet)'}</option>)}
            </optgroup>
          ))}
          <optgroup label="This PC · Fowl Engine Manager">
            {MGR_LOGS.map(m => <option key={m.id} value={m.id}>{m.label}</option>)}
          </optgroup>
        </select>
        <input aria-label="Filter" style={{ ...input, width: 220, fontSize: '0.68rem' }} value={filter} placeholder="filter: text, /regex/, !exclude"
               onChange={e => setFilter(e.target.value)} />
        <select aria-label="Level" value={minLevel} onChange={e => setMinLevel(e.target.value as typeof minLevel)} style={{ ...input, width: 'auto', fontSize: '0.68rem' }}>
          <option value="all">all levels</option>
          <option value="warn">warnings + errors</option>
          <option value="error">errors only</option>
        </select>
        <span className="ml-auto flex items-center gap-2">
          {!err && (paused ? <Pill color={AMBER}>paused</Pill> : <Pill color={OK}>{mgrLog ? 'refreshing' : 'live'}</Pill>)}
          <Btn onClick={() => setPaused(p => !p)}>{paused ? <><Play size={11} />Resume</> : <><Pause size={11} />Pause</>}</Btn>
          {!mgrLog && <Btn onClick={follow.clear}><Trash2 size={11} />Clear</Btn>}
          {mgrLog && <Btn onClick={() => { void mgr.openPath('logs') }}>Open folder</Btn>}
        </span>
      </div>
      {skew && (
        <Note tone="warn">{(list.error as Error).message} Until then, Server OPS → Logs shows the tail of its bot and bfdb logs.</Note>
      )}
      {list.error && !skew && server && <Note tone="bad">{list.error instanceof Error ? list.error.message : String(list.error)}</Note>}
      <div className="flex gap-3" style={{ ...DIM, marginBottom: 6, textTransform: 'none', letterSpacing: 0, flexWrap: 'wrap' }}>
        <span style={MONO}>{mgrLog ? 'this PC' : server?.name}</span>
        <span style={{ ...MONO, overflow: 'hidden', textOverflow: 'ellipsis' }}>{follow.info?.path ?? entry?.path ?? ''}</span>
        {!mgrLog && follow.info && <span>{fmtBytes(follow.info.size)}</span>}
        {!mgrLog && follow.info?.missing && <span style={{ color: AMBER }}>the file isn't there (yet) -- waiting for it</span>}
        <span className="ml-auto">{shown.length === lines.length ? `${lines.length} lines` : `${shown.length} of ${lines.length} lines`}</span>
      </div>
      <pre ref={box} className="vs-card" onScroll={e => {
        const el = e.currentTarget
        atBottom.current = el.scrollHeight - el.scrollTop - el.clientHeight < 40
      }} style={{
        flex: 1, minHeight: 300, margin: 0, overflow: 'auto', padding: '10px 12px', fontSize: '0.66rem', lineHeight: 1.55,
        whiteSpace: 'pre-wrap', wordBreak: 'break-all', ...MONO,
      }}>
        {err ? <span style={{ color: RED }}>{err instanceof Error ? err.message : String(err)}{'\n'}</span> : null}
        {shown.length === 0 && !err ? <span style={{ color: 'var(--text-dim)' }}>{lines.length ? 'Nothing matches the filter.' : 'Nothing logged yet.'}</span> : null}
        {shown.map((l, i) => {
          const lv = l.startsWith('── ') ? 'note' : levelOf(l)
          return <div key={i} style={{ color: lv === 'note' ? AMBER : lv ? COLOR[lv] : undefined }}>{l}</div>
        })}
      </pre>
    </div>
  )
}
