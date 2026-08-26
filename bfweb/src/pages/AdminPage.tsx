import { useState, useEffect, useRef, useCallback } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import {
  LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer,
} from 'recharts'
import { api, connectLiveLogs, connectEngineLogs, type LogLine, type PerfRow, type PerfTimelinePoint, type BotActionResult } from '../api'
import { useAuth } from '../context/AuthContext'
import PageHeader from '../components/PageHeader'
import {
  Shield, Users, Trash2, AlertTriangle, RotateCcw,
  Activity, Ban, UserX, Terminal, Search, ChevronsDown,
  Server, Play, Square, RotateCw, Pause, PlayCircle,
} from 'lucide-react'
import { useNavigate } from 'react-router-dom'

// ── shared styles ─────────────────────────────────────────────────────────────

const DIM: React.CSSProperties = {
  fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase',
}
const CELL: React.CSSProperties = {
  padding: '5px 10px', fontSize: '0.67rem', color: 'var(--text-muted)', borderBottom: '1px solid var(--border)',
}
const MONO: React.CSSProperties = { fontFamily: 'var(--font-mono)' }

function CardHeader({ icon, label, badge }: { icon: React.ReactNode; label: string; badge?: React.ReactNode }) {
  return (
    <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
      {icon}
      <span style={{ ...DIM, fontSize: '0.65rem' }}>{label}</span>
      {badge && <span className="ml-auto">{badge}</span>}
    </div>
  )
}

// ── colour helpers ────────────────────────────────────────────────────────────

const METRIC_COLORS: Record<string, string> = {
  frame: '#6aab1f', timed_events: '#8ec83f', slow_timed: '#ef4444',
  dcs_events: '#4a8fd4', spawn: '#c9a227', despawn: '#f97316',
  logistics: '#06b6d4', logistics_deliver: '#0891b2', frontline: '#a855f7',
  unit_positions: '#84cc16', ewr_tracks: '#fb923c', snapshot: '#94a3b8',
}

function perfColor(p99: number) {
  if (p99 > 10_000) return '#ef4444'
  if (p99 > 2_000) return '#f97316'
  if (p99 > 500) return '#f59e0b'
  return 'var(--text-muted)'
}

// ── perf chart ────────────────────────────────────────────────────────────────

type MetricKey = keyof Omit<PerfTimelinePoint, 'time'>

const CHART_GROUPS: { label: string; keys: MetricKey[] }[] = [
  { label: 'Core Loop',    keys: ['frame', 'timed_events', 'slow_timed', 'dcs_events'] },
  { label: 'Spawn / Unit', keys: ['spawn', 'despawn', 'unit_positions', 'ewr_tracks'] },
  { label: 'Logistics',    keys: ['logistics', 'logistics_deliver', 'frontline', 'snapshot'] },
]

function PerfChart({ data, group }: { data: PerfTimelinePoint[]; group: typeof CHART_GROUPS[0] }) {
  const chartData = data.map(d => {
    const row: Record<string, unknown> = { time: new Date(d.time).toLocaleDateString() }
    for (const k of group.keys) {
      const v = d[k] as { mean: number; p99: number }
      row[`${k}_p99`] = v.p99
    }
    return row
  })

  return (
    <div style={{ marginBottom: '1.5rem' }}>
      <div style={{ ...DIM, fontSize: '0.6rem', marginBottom: '0.5rem' }}>{group.label} — p99 latency (μs)</div>
      <ResponsiveContainer width="100%" height={160}>
        <LineChart data={chartData} margin={{ top: 4, right: 16, left: 0, bottom: 0 }}>
          <CartesianGrid strokeDasharray="2 4" stroke="rgba(255,255,255,0.05)" />
          <XAxis dataKey="time" tick={{ fontSize: 9, fill: 'var(--text-dim)' }} />
          <YAxis tick={{ fontSize: 9, fill: 'var(--text-dim)' }} width={48} unit="μs" />
          <Tooltip
            contentStyle={{ background: 'var(--bg-elevated)', border: '1px solid var(--border)', fontSize: 11 }}
            formatter={(v, name) => [`${Number(v).toLocaleString()} μs`, String(name).replace('_p99', '')]}
          />
          <Legend iconSize={8} wrapperStyle={{ fontSize: 10, color: 'var(--text-dim)' }} formatter={n => n.replace('_p99', '')} />
          {group.keys.map(k => (
            <Line
              key={k}
              type="monotone"
              dataKey={`${k}_p99`}
              stroke={METRIC_COLORS[k] ?? '#888'}
              strokeWidth={1.5}
              dot={false}
              activeDot={{ r: 3 }}
            />
          ))}
        </LineChart>
      </ResponsiveContainer>
    </div>
  )
}

// ── perf table ────────────────────────────────────────────────────────────────

function PerfPanel({ title, icon, rows, time, available }: {
  title: string; icon: React.ReactNode; rows?: PerfRow[]; time?: string; available?: boolean
}) {
  return (
    <div className="vs-card">
      <CardHeader
        icon={icon}
        label={title}
        badge={time && <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>Last session: {new Date(time).toLocaleString()}</span>}
      />
      {available === false && (
        <div style={{ padding: '1rem', ...DIM }}>No session data yet — perf stats are recorded at session end.</div>
      )}
      {available && rows && (
        <div style={{ overflowX: 'auto' }}>
          <table style={{ width: '100%', borderCollapse: 'collapse' }}>
            <thead>
              <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
                {['Metric', 'N', 'Mean', 'p50', 'p90', 'p99', 'p999'].map(h => (
                  <th key={h} style={{ ...CELL, textAlign: h === 'Metric' ? 'left' : 'right', color: 'var(--text-dim)', fontWeight: 600 }}>{h}</th>
                ))}
              </tr>
            </thead>
            <tbody>
              {rows.filter(r => r.n > 0).map(r => (
                <tr key={r.name}>
                  <td style={{ ...CELL, ...MONO, fontSize: '0.65rem', color: 'var(--text)' }}>{r.name}</td>
                  <td style={{ ...CELL, ...MONO, textAlign: 'right', color: 'var(--text-dim)' }}>{r.n.toLocaleString()}</td>
                  <td style={{ ...CELL, ...MONO, textAlign: 'right' }}>{r.mean} {r.unit}</td>
                  <td style={{ ...CELL, ...MONO, textAlign: 'right' }}>{r.p50} {r.unit}</td>
                  <td style={{ ...CELL, ...MONO, textAlign: 'right' }}>{r.p90} {r.unit}</td>
                  <td style={{ ...CELL, ...MONO, textAlign: 'right', color: perfColor(r.p99 * 1000) }}>{r.p99} {r.unit}</td>
                  <td style={{ ...CELL, ...MONO, textAlign: 'right', color: perfColor(r.p999 * 1000) }}>{r.p999} {r.unit}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  )
}

// ── log level styling ─────────────────────────────────────────────────────────

const LOG_MAX = 1000

const LEVEL_COLOR: Record<string, string> = {
  ERROR: '#ef4444',
  WARN:  '#f59e0b',
  INFO:  'var(--accent)',
  DEBUG: '#4a8fd4',
  TRACE: 'var(--text-dim)',
}

const LEVEL_BG: Record<string, string> = {
  ERROR: 'rgba(239,68,68,0.06)',
  WARN:  'rgba(245,158,11,0.04)',
  INFO:  '',
  DEBUG: '',
  TRACE: '',
}

const LEVELS = ['ALL', 'ERROR', 'WARN', 'INFO', 'DEBUG', 'TRACE'] as const
type LevelFilter = typeof LEVELS[number]

// ── log analyzer ─────────────────────────────────────────────────────────────

function LogAnalyzer() {
  const [lines,      setLines]      = useState<LogLine[]>([])
  const [filter,     setFilter]     = useState<LevelFilter>('ALL')
  const [search,     setSearch]     = useState('')
  const [autoScroll, setAutoScroll] = useState(true)
  const [wsStatus,   setWsStatus]   = useState<'connecting' | 'open' | 'closed' | 'error'>('connecting')
  const bottomRef  = useRef<HTMLDivElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const addLine = useCallback((line: LogLine) => {
    setLines(prev => {
      const next = [...prev, line]
      return next.length > LOG_MAX ? next.slice(next.length - LOG_MAX) : next
    })
  }, [])

  useEffect(() => {
    setWsStatus('connecting')
    const cleanup = connectLiveLogs(addLine, s => setWsStatus(s === 'open' ? 'open' : s === 'closed' ? 'closed' : 'error'))
    return cleanup
  }, [addLine])

  useEffect(() => {
    if (autoScroll) bottomRef.current?.scrollIntoView({ behavior: 'auto' })
  }, [lines, autoScroll])

  function onScroll() {
    const el = containerRef.current
    if (!el) return
    const atBottom = el.scrollHeight - el.scrollTop - el.clientHeight < 40
    if (!atBottom && autoScroll) setAutoScroll(false)
  }

  const levelOrder: Record<string, number> = { ERROR: 0, WARN: 1, INFO: 2, DEBUG: 3, TRACE: 4 }
  const filterLevel = filter === 'ALL' ? 999 : (levelOrder[filter] ?? 999)

  const visible = lines.filter(l => {
    if (filter !== 'ALL' && (levelOrder[l.level] ?? 999) > filterLevel) return false
    if (search) {
      const q = search.toLowerCase()
      if (!l.msg.toLowerCase().includes(q) && !l.target.toLowerCase().includes(q)) return false
    }
    return true
  })

  const statusCol = wsStatus === 'open' ? 'var(--accent)' : wsStatus === 'connecting' ? '#f59e0b' : '#ef4444'
  const statusLabel = wsStatus === 'open' ? 'LIVE' : wsStatus === 'connecting' ? 'CONNECTING' : wsStatus === 'closed' ? 'CLOSED' : 'ERROR'

  return (
    <div className="vs-card" style={{ display: 'flex', flexDirection: 'column', height: 520 }}>
      <CardHeader
        icon={<Terminal size={13} style={{ color: 'var(--accent)' }} />}
        label="BACKEND Log Analyzer"
        badge={
          <span style={{ fontSize: '0.6rem', color: statusCol, fontFamily: 'var(--font-mono)', border: `1px solid ${statusCol}`, padding: '1px 6px', letterSpacing: '0.1em' }}>
            {statusLabel}
          </span>
        }
      />

      {/* toolbar */}
      <div style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '6px 12px', borderBottom: '1px solid var(--border)', flexShrink: 0, background: 'rgba(0,0,0,0.15)', flexWrap: 'wrap' }}>
        {/* level filter buttons */}
        <div style={{ display: 'flex', gap: 3 }}>
          {LEVELS.map(lv => (
            <button key={lv} onClick={() => setFilter(lv)} style={{
              fontSize: '0.58rem', padding: '2px 7px', borderRadius: 2, cursor: 'pointer',
              fontFamily: 'var(--font-mono)', letterSpacing: '0.08em',
              background: filter === lv ? (LEVEL_COLOR[lv] ?? 'var(--accent)') : 'transparent',
              color:      filter === lv ? (lv === 'ALL' ? '#000' : '#fff') : (LEVEL_COLOR[lv] ?? 'var(--text-dim)'),
              border:     `1px solid ${filter === lv ? (LEVEL_COLOR[lv] ?? 'var(--accent)') : 'var(--border)'}`,
            }}>{lv}</button>
          ))}
        </div>

        {/* search */}
        <div style={{ display: 'flex', alignItems: 'center', gap: 5, background: 'var(--bg-input)', border: '1px solid var(--border)', padding: '2px 8px', flex: 1, minWidth: 140 }}>
          <Search size={9} style={{ color: 'var(--text-dim)', flexShrink: 0 }} />
          <input
            value={search}
            onChange={e => setSearch(e.target.value)}
            placeholder="Filter message / target…"
            style={{ background: 'none', border: 'none', outline: 'none', color: 'var(--text)', fontSize: '0.65rem', fontFamily: 'var(--font-mono)', width: '100%' }}
          />
          {search && <button onClick={() => setSearch('')} style={{ background: 'none', border: 'none', color: 'var(--text-dim)', cursor: 'pointer', padding: 0, fontSize: '0.6rem' }}>✕</button>}
        </div>

        {/* meta */}
        <div style={{ display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0 }}>
          <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>{visible.length}/{lines.length}</span>
          <button
            onClick={() => { setAutoScroll(true); bottomRef.current?.scrollIntoView({ behavior: 'smooth' }) }}
            title="Scroll to bottom / enable auto-scroll"
            style={{ background: autoScroll ? 'var(--accent)' : 'transparent', border: `1px solid ${autoScroll ? 'var(--accent)' : 'var(--border)'}`, color: autoScroll ? '#000' : 'var(--text-dim)', borderRadius: 2, cursor: 'pointer', padding: '2px 5px', display: 'flex', alignItems: 'center' }}>
            <ChevronsDown size={10} />
          </button>
          <button onClick={() => setLines([])}
            style={{ fontSize: '0.58rem', background: 'none', border: '1px solid var(--border)', color: 'var(--text-dim)', padding: '2px 8px', borderRadius: 2, cursor: 'pointer', fontFamily: 'var(--font-mono)' }}>CLR</button>
        </div>
      </div>

      {/* log output */}
      <div ref={containerRef} onScroll={onScroll} style={{ flex: 1, overflowY: 'auto', fontFamily: 'var(--font-mono)', fontSize: '0.63rem', lineHeight: 1.55 }}>
        {visible.length === 0 && (
          <div style={{ padding: '1rem', color: 'var(--text-dim)', textAlign: 'center', fontSize: '0.62rem', letterSpacing: '0.1em' }}>
            {wsStatus === 'connecting' ? 'CONNECTING…' : wsStatus !== 'open' ? 'DISCONNECTED' : lines.length === 0 ? 'WAITING FOR LOGS…' : 'NO MATCHING LOGS'}
          </div>
        )}
        {visible.map((l, i) => {
          const lc = LEVEL_COLOR[l.level] ?? 'var(--text-dim)'
          const bg = LEVEL_BG[l.level] ?? ''
          const time = l.ts.slice(11, 23) // HH:MM:SS.mmm
          return (
            <div key={i} style={{ display: 'flex', gap: 8, padding: '1px 10px', background: bg, borderBottom: '1px solid rgba(255,255,255,0.018)', alignItems: 'baseline' }}>
              <span style={{ color: 'var(--text-dim)', flexShrink: 0, userSelect: 'none' }}>{time}</span>
              <span style={{ color: lc, flexShrink: 0, width: 38, textAlign: 'right', fontWeight: 700 }}>{l.level}</span>
              <span style={{ color: '#4a8fd4', flexShrink: 0, maxWidth: 180, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }} title={l.target}>{l.target}</span>
              <span style={{ color: 'var(--text)', whiteSpace: 'pre-wrap', wordBreak: 'break-all', flex: 1 }}>{l.msg}</span>
            </div>
          )
        })}
        <div ref={bottomRef} />
      </div>
    </div>
  )
}

// ── engine log analyzer (live bflib logs from the running DCS mission) ────────

interface EngineLine { raw: string; level: string; text: string }

const LEVEL_RE = /\[(TRACE|DEBUG|INFO|WARN|ERROR)\]\s*(?:([\w:.]+):\s*)?/
// bflib's log lines start with a HH:MM:SS timestamp; anything that doesn't
// match this is a continuation of a multi-line message (e.g. pretty-printed
// debug output) that bfdb split into separate raw lines with no level tag of
// its own -- fold it into the previous entry instead of rendering it as its
// own spurious row.
const NEW_ENTRY_RE = /^\d{2}:\d{2}:\d{2}\s/

function parseEngineLine(raw: string): EngineLine {
  const m = raw.match(LEVEL_RE)
  if (!m) return { raw, level: 'INFO', text: raw }
  return { raw, level: m[1], text: raw }
}

function EngineLogAnalyzer() {
  const [lines,      setLines]      = useState<EngineLine[]>([])
  const [filter,     setFilter]     = useState<LevelFilter>('ALL')
  const [search,     setSearch]     = useState('')
  const [autoScroll, setAutoScroll] = useState(true)
  const [wsStatus,   setWsStatus]   = useState<'connecting' | 'open' | 'closed' | 'error'>('connecting')
  const bottomRef  = useRef<HTMLDivElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const addLine = useCallback((raw: string) => {
    setLines(prev => {
      let next: EngineLine[]
      if (prev.length > 0 && !NEW_ENTRY_RE.test(raw)) {
        const last = prev[prev.length - 1]
        next = [...prev.slice(0, -1), { ...last, text: `${last.text}\n${raw}` }]
      } else {
        next = [...prev, parseEngineLine(raw)]
      }
      return next.length > LOG_MAX ? next.slice(next.length - LOG_MAX) : next
    })
  }, [])

  useEffect(() => {
    setWsStatus('connecting')
    const cleanup = connectEngineLogs(addLine, s => setWsStatus(s === 'open' ? 'open' : s === 'closed' ? 'closed' : 'error'))
    return cleanup
  }, [addLine])

  useEffect(() => {
    if (autoScroll) bottomRef.current?.scrollIntoView({ behavior: 'auto' })
  }, [lines, autoScroll])

  function onScroll() {
    const el = containerRef.current
    if (!el) return
    const atBottom = el.scrollHeight - el.scrollTop - el.clientHeight < 40
    if (!atBottom && autoScroll) setAutoScroll(false)
  }

  const levelOrder: Record<string, number> = { ERROR: 0, WARN: 1, INFO: 2, DEBUG: 3, TRACE: 4 }
  const filterLevel = filter === 'ALL' ? 999 : (levelOrder[filter] ?? 999)

  const visible = lines.filter(l => {
    if (filter !== 'ALL' && (levelOrder[l.level] ?? 999) > filterLevel) return false
    if (search && !l.text.toLowerCase().includes(search.toLowerCase())) return false
    return true
  })

  const statusCol = wsStatus === 'open' ? 'var(--accent)' : wsStatus === 'connecting' ? '#f59e0b' : '#ef4444'
  const statusLabel = wsStatus === 'open' ? 'LIVE' : wsStatus === 'connecting' ? 'CONNECTING' : wsStatus === 'closed' ? 'CLOSED' : 'ERROR'

  return (
    <div className="vs-card" style={{ display: 'flex', flexDirection: 'column', height: 520 }}>
      <CardHeader
        icon={<Terminal size={13} style={{ color: 'var(--accent)' }} />}
        label="Engine Log (live mission)"
        badge={
          <span style={{ fontSize: '0.6rem', color: statusCol, fontFamily: 'var(--font-mono)', border: `1px solid ${statusCol}`, padding: '1px 6px', letterSpacing: '0.1em' }}>
            {statusLabel}
          </span>
        }
      />

      <div style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '6px 12px', borderBottom: '1px solid var(--border)', flexShrink: 0, background: 'rgba(0,0,0,0.15)', flexWrap: 'wrap' }}>
        <div style={{ display: 'flex', gap: 3 }}>
          {LEVELS.map(lv => (
            <button key={lv} onClick={() => setFilter(lv)} style={{
              fontSize: '0.58rem', padding: '2px 7px', borderRadius: 2, cursor: 'pointer',
              fontFamily: 'var(--font-mono)', letterSpacing: '0.08em',
              background: filter === lv ? (LEVEL_COLOR[lv] ?? 'var(--accent)') : 'transparent',
              color:      filter === lv ? (lv === 'ALL' ? '#000' : '#fff') : (LEVEL_COLOR[lv] ?? 'var(--text-dim)'),
              border:     `1px solid ${filter === lv ? (LEVEL_COLOR[lv] ?? 'var(--accent)') : 'var(--border)'}`,
            }}>{lv}</button>
          ))}
        </div>

        <div style={{ display: 'flex', alignItems: 'center', gap: 5, background: 'var(--bg-input)', border: '1px solid var(--border)', padding: '2px 8px', flex: 1, minWidth: 140 }}>
          <Search size={9} style={{ color: 'var(--text-dim)', flexShrink: 0 }} />
          <input
            value={search}
            onChange={e => setSearch(e.target.value)}
            placeholder="Filter message…"
            style={{ background: 'none', border: 'none', outline: 'none', color: 'var(--text)', fontSize: '0.65rem', fontFamily: 'var(--font-mono)', width: '100%' }}
          />
          {search && <button onClick={() => setSearch('')} style={{ background: 'none', border: 'none', color: 'var(--text-dim)', cursor: 'pointer', padding: 0, fontSize: '0.6rem' }}>✕</button>}
        </div>

        <div style={{ display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0 }}>
          <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>{visible.length}/{lines.length}</span>
          <button
            onClick={() => { setAutoScroll(true); bottomRef.current?.scrollIntoView({ behavior: 'smooth' }) }}
            title="Scroll to bottom / enable auto-scroll"
            style={{ background: autoScroll ? 'var(--accent)' : 'transparent', border: `1px solid ${autoScroll ? 'var(--accent)' : 'var(--border)'}`, color: autoScroll ? '#000' : 'var(--text-dim)', borderRadius: 2, cursor: 'pointer', padding: '2px 5px', display: 'flex', alignItems: 'center' }}>
            <ChevronsDown size={10} />
          </button>
          <button onClick={() => setLines([])}
            style={{ fontSize: '0.58rem', background: 'none', border: '1px solid var(--border)', color: 'var(--text-dim)', padding: '2px 8px', borderRadius: 2, cursor: 'pointer', fontFamily: 'var(--font-mono)' }}>CLR</button>
        </div>
      </div>

      <div ref={containerRef} onScroll={onScroll} style={{ flex: 1, overflowY: 'auto', fontFamily: 'var(--font-mono)', fontSize: '0.63rem', lineHeight: 1.55 }}>
        {visible.length === 0 && (
          <div style={{ padding: '1rem', color: 'var(--text-dim)', textAlign: 'center', fontSize: '0.62rem', letterSpacing: '0.1em' }}>
            {wsStatus === 'connecting' ? 'CONNECTING…' : wsStatus !== 'open' ? 'DISCONNECTED' : lines.length === 0 ? 'WAITING FOR ENGINE LOGS (mission must be running, bfdb needs --base)…' : 'NO MATCHING LOGS'}
          </div>
        )}
        {visible.map((l, i) => {
          const lc = LEVEL_COLOR[l.level] ?? 'var(--text-dim)'
          const bg = LEVEL_BG[l.level] ?? ''
          return (
            <div key={i} style={{ display: 'flex', gap: 8, padding: '1px 10px', background: bg, borderBottom: '1px solid rgba(255,255,255,0.018)', alignItems: 'baseline' }}>
              <span style={{ color: lc, flexShrink: 0, width: 38, textAlign: 'right', fontWeight: 700 }}>{l.level}</span>
              <span style={{ color: 'var(--text)', whiteSpace: 'pre-wrap', wordBreak: 'break-all', flex: 1 }}>{l.text}</span>
            </div>
          )
        })}
        <div ref={bottomRef} />
      </div>
    </div>
  )
}

// ── engine error feed (persisted server-side, survives the tab being closed) ──

function EngineErrorFeed({ lines }: { lines: string[] }) {
  const parsed = lines.map(parseEngineLine).reverse() // newest first

  return (
    <div className="vs-card">
      <CardHeader
        icon={<AlertTriangle size={13} style={{ color: '#ef4444' }} />}
        label="Engine Errors & Warnings"
        badge={<span style={{ ...MONO, fontSize: '0.65rem', color: 'var(--text-muted)' }}>{lines.length}</span>}
      />
      {parsed.length === 0 && (
        <div style={{ padding: '1rem', ...DIM }}>No errors or warnings recorded — this fills in as bflib logs them, even if nobody has this page open.</div>
      )}
      {parsed.length > 0 && (
        <div style={{ maxHeight: 320, overflowY: 'auto', fontFamily: 'var(--font-mono)', fontSize: '0.63rem', lineHeight: 1.6 }}>
          {parsed.map((l, i) => {
            const lc = LEVEL_COLOR[l.level] ?? 'var(--text-dim)'
            const bg = LEVEL_BG[l.level] ?? ''
            return (
              <div key={i} style={{ display: 'flex', gap: 8, padding: '3px 12px', background: bg, borderBottom: '1px solid rgba(255,255,255,0.018)', alignItems: 'baseline' }}>
                <span style={{ color: lc, flexShrink: 0, width: 38, textAlign: 'right', fontWeight: 700 }}>{l.level}</span>
                <span style={{ color: 'var(--text)', whiteSpace: 'pre-wrap', wordBreak: 'break-all', flex: 1 }}>{l.text}</span>
              </div>
            )
          })}
        </div>
      )}
    </div>
  )
}

// ── DCSServerBot server control ─────────────────────────────────────────────

interface BotAction {
  key: string
  label: string
  icon: typeof Server
  fn: () => Promise<BotActionResult>
  danger: boolean
  confirmText: string
}

const BOT_ACTIONS: BotAction[] = [
  { key: 'start',           label: 'Start',            icon: Play,       fn: api.admin.botStart,          danger: false, confirmText: '' },
  { key: 'stop',             label: 'Stop',              icon: Square,     fn: api.admin.botStop,            danger: true,  confirmText: 'Stop the DCS server? Every player will be disconnected.' },
  { key: 'restart',          label: 'Restart Server',    icon: RotateCw,   fn: api.admin.botRestart,          danger: true,  confirmText: 'Restart the DCS server? Every player will be disconnected.' },
  { key: 'mission_restart',  label: 'Restart Mission',   icon: RotateCcw,  fn: api.admin.botMissionRestart,   danger: true,  confirmText: 'Restart the current mission? Every player will be disconnected.' },
  { key: 'mission_pause',    label: 'Pause Mission',     icon: Pause,      fn: api.admin.botMissionPause,     danger: false, confirmText: '' },
  { key: 'mission_unpause',  label: 'Unpause Mission',   icon: PlayCircle, fn: api.admin.botMissionUnpause,   danger: false, confirmText: '' },
]

function ServerControlPanel() {
  const queryClient = useQueryClient()
  const { data: botStatus, isLoading: statusLoading } = useQuery({
    queryKey: ['admin', 'bot-status'], queryFn: api.admin.botStatus, refetchInterval: 15_000,
  })
  const [confirmKey, setConfirmKey] = useState<string | null>(null)
  const [busy,       setBusy]       = useState<string | null>(null)
  const [result,     setResult]     = useState<{ ok: boolean; message: string } | null>(null)

  async function run(action: BotAction) {
    setConfirmKey(null)
    setBusy(action.key)
    setResult(null)
    try {
      const r = await action.fn()
      setResult({ ok: true, message: r.message })
      queryClient.invalidateQueries({ queryKey: ['admin', 'bot-status'] })
    } catch (e) {
      setResult({ ok: false, message: e instanceof Error ? e.message : String(e) })
    } finally {
      setBusy(null)
    }
  }

  const statusColor = botStatus?.status === 'running' ? 'var(--accent)' : botStatus?.status === 'paused' ? '#f59e0b' : 'var(--text-dim)'
  const confirmAction = confirmKey ? BOT_ACTIONS.find(a => a.key === confirmKey) : undefined

  return (
    <div className="vs-card">
      <CardHeader
        icon={<Server size={13} style={{ color: 'var(--accent)' }} />}
        label="DCSServerBot Server Control"
        badge={botStatus?.configured && botStatus.status ? (
          <span style={{ fontSize: '0.6rem', color: statusColor, border: `1px solid ${statusColor}`, padding: '1px 8px', borderRadius: 2, textTransform: 'uppercase', letterSpacing: '0.08em' }}>
            {botStatus.name ?? 'server'}: {botStatus.status}
          </span>
        ) : undefined}
      />
      {!statusLoading && botStatus && !botStatus.configured && (
        <div style={{ padding: '1rem', ...DIM }}>
          DCSServerBot isn't configured (--dcsserverbot-url / --dcsserverbot-api-key on bfdb) — these controls are disabled.
        </div>
      )}
      {(!botStatus || botStatus.configured) && (
        <div className="p-4 flex flex-col gap-3">
          <div className="flex flex-wrap gap-2">
            {BOT_ACTIONS.map(a => (
              <button key={a.key}
                disabled={busy !== null}
                onClick={() => a.danger ? setConfirmKey(a.key) : run(a)}
                style={{
                  display: 'flex', alignItems: 'center', gap: 6,
                  fontSize: '0.68rem', letterSpacing: '0.06em',
                  color: a.danger ? '#ef4444' : 'var(--text)',
                  background: 'none', border: `1px solid ${a.danger ? 'rgba(239,68,68,0.4)' : 'var(--border)'}`,
                  padding: '0.4rem 0.9rem', borderRadius: 3,
                  cursor: busy !== null ? 'not-allowed' : 'pointer', opacity: busy !== null ? 0.6 : 1,
                }}>
                <a.icon size={12} />
                {busy === a.key ? 'Working…' : a.label}
              </button>
            ))}
          </div>
          {confirmAction && (
            <div style={{ display: 'flex', alignItems: 'center', gap: 10, padding: '0.6rem 0.8rem', background: 'rgba(239,68,68,0.06)', border: '1px solid rgba(239,68,68,0.25)', borderRadius: 3 }}>
              <span style={{ fontSize: '0.68rem', color: '#ef4444', flex: 1 }}>{confirmAction.confirmText}</span>
              <button onClick={() => run(confirmAction)}
                style={{ fontSize: '0.65rem', color: '#fff', background: '#ef4444', border: 'none', padding: '0.3rem 0.8rem', borderRadius: 3, cursor: 'pointer' }}>
                Confirm
              </button>
              <button onClick={() => setConfirmKey(null)}
                style={{ fontSize: '0.65rem', color: 'var(--text-dim)', background: 'none', border: '1px solid var(--border)', padding: '0.3rem 0.8rem', borderRadius: 3, cursor: 'pointer' }}>
                Cancel
              </button>
            </div>
          )}
          {result && (
            <div style={{ fontSize: '0.68rem', color: result.ok ? 'var(--accent)' : '#ef4444' }}>
              {result.ok ? '✓ ' : '✗ '}{result.message}
            </div>
          )}
        </div>
      )}
    </div>
  )
}

// ── main component ────────────────────────────────────────────────────────────

export default function AdminPage() {
  const { user, loading } = useAuth()
  const navigate = useNavigate()
  const queryClient = useQueryClient()

  const [resetConfirm,  setResetConfirm]  = useState(false)
  const [resetLoading,  setResetLoading]  = useState(false)
  const [resetDone,     setResetDone]     = useState(false)
  const [banTarget,     setBanTarget]     = useState<{ ucid: string; name: string } | null>(null)
  const [banReason,     setBanReason]     = useState('')
  const [banUntil,      setBanUntil]      = useState('')
  const [unbanTarget,   setUnbanTarget]   = useState<string | null>(null)
  const [kickInfo,      setKickInfo]      = useState<{ name: string } | null>(null)
  const [perfTab,       setPerfTab]       = useState<'charts' | 'table-engine' | 'table-api'>('charts')

  const { data: sessions = [], isLoading: sessionsLoading } = useQuery({ queryKey: ['admin', 'sessions'],     queryFn: api.admin.sessions,    refetchInterval: 15_000 })
  const { data: online = [],   isLoading: onlineLoading }   = useQuery({ queryKey: ['online'],                queryFn: api.online,            refetchInterval: 10_000 })
  const { data: banned = [] }                                = useQuery({ queryKey: ['admin', 'banned'],       queryFn: api.admin.banned,      refetchInterval: 30_000 })
  const { data: engineErrors = [] }                          = useQuery({ queryKey: ['admin', 'engine-errors'], queryFn: api.admin.engineErrors, refetchInterval: 20_000 })
  const { data: perf }                                       = useQuery({ queryKey: ['admin', 'perf'],         queryFn: api.admin.perf,        refetchInterval: 30_000 })
  const { data: perfHistory }                                = useQuery({ queryKey: ['admin', 'perf-history'], queryFn: api.admin.perfHistory, refetchInterval: 60_000 })

  // Wait for the auth check to actually resolve before deciding to bounce --
  // on a hard refresh `user` starts out null while /api/auth/me is still in
  // flight, and redirecting on that transient state kicked an already-logged
  // -in admin back to /login every time they reloaded this page.
  if (loading) return null
  if (!user) { navigate('/login'); return null }
  if (!user.is_admin) { navigate('/'); return null }

  async function handleReset() {
    setResetLoading(true)
    try {
      await api.admin.reset()
      queryClient.invalidateQueries()
      setResetDone(true)
      setResetConfirm(false)
    } finally {
      setResetLoading(false)
    }
  }

  async function handleBan() {
    if (!banTarget) return
    await api.admin.ban(banTarget.ucid, banTarget.name, banReason, banUntil || undefined)
    queryClient.invalidateQueries({ queryKey: ['admin', 'banned'] })
    setBanTarget(null); setBanReason(''); setBanUntil('')
  }

  async function handleUnban(ucid: string) {
    await api.admin.unban(ucid)
    queryClient.invalidateQueries({ queryKey: ['admin', 'banned'] })
    setUnbanTarget(null)
  }

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader title="ADMIN PANEL" sub="Server management — restricted access" />

      <div className="flex-1 overflow-auto p-4 space-y-4" style={{ background: 'var(--bg)' }}>

        {/* ── Online players ── */}
        <div className="vs-card">
          <CardHeader
            icon={<Users size={13} style={{ color: 'var(--accent)' }} />}
            label="Online Players"
            badge={<span style={{ ...MONO, fontSize: '0.65rem', color: 'var(--text-muted)' }}>{online.length}</span>}
          />
          {onlineLoading && <div style={{ padding: '1rem', ...DIM }}>Loading…</div>}
          {!onlineLoading && online.length === 0 && <div style={{ padding: '1rem', ...DIM }}>No players online</div>}
          {online.length > 0 && (
            <table style={{ width: '100%', borderCollapse: 'collapse' }}>
              <thead>
                <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
                  {['Name', 'Side', 'Slot', 'Actions'].map(h => (
                    <th key={h} style={{ ...CELL, color: 'var(--text-dim)', textAlign: 'left', fontWeight: 600 }}>{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {online.map(p => (
                  <tr key={p.ucid}>
                    <td style={{ ...CELL, color: 'var(--text)', ...MONO, fontSize: '0.67rem' }}>{p.name}</td>
                    <td style={{ ...CELL }}>
                      <span style={{ fontSize: '0.62rem', color: p.side === 'Blue' ? 'var(--blue)' : p.side === 'Red' ? 'var(--red)' : 'var(--text-dim)' }}>
                        {p.side}
                      </span>
                    </td>
                    <td style={{ ...CELL, ...MONO, fontSize: '0.62rem', color: 'var(--text-dim)' }}>{p.aircraft ?? '—'}</td>
                    <td style={{ ...CELL }}>
                      <div className="flex gap-2">
                        <button
                          title="Copy kick chat command"
                          onClick={() => setKickInfo({ name: p.name })}
                          style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', padding: 0 }}
                        >
                          <UserX size={12} />
                        </button>
                        <button
                          title="Ban player"
                          onClick={() => { setBanTarget({ ucid: p.ucid, name: p.name }); setBanReason(''); setBanUntil('') }}
                          style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', padding: 0 }}
                        >
                          <Ban size={12} />
                        </button>
                      </div>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          )}
        </div>

        {/* ── Kick helper modal ── */}
        {kickInfo && (
          <div style={{ position: 'fixed', inset: 0, background: 'rgba(0,0,0,0.7)', zIndex: 9000, display: 'flex', alignItems: 'center', justifyContent: 'center' }}
               onClick={() => setKickInfo(null)}>
            <div onClick={e => e.stopPropagation()} style={{ background: 'var(--bg-elevated)', border: '1px solid var(--border)', borderRadius: 3, padding: '1.5rem', minWidth: 380, maxWidth: 480 }}>
              <div style={{ ...DIM, marginBottom: '0.75rem', color: '#f59e0b' }}>Kick Player</div>
              <div style={{ fontSize: '0.75rem', color: 'var(--text)', marginBottom: '0.5rem' }}>
                Run this command in the DCS server F10 admin chat:
              </div>
              <div style={{ ...MONO, fontSize: '0.72rem', background: 'var(--bg-card)', border: '1px solid var(--border)', padding: '0.5rem 0.75rem', borderRadius: 2, color: 'var(--accent)', marginBottom: '1rem', wordBreak: 'break-all' }}>
                .kick {kickInfo.name}
              </div>
              <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginBottom: '1rem' }}>
                Kick requires direct DCS engine access. Live kick via netidx is available when bfdb is configured with <code>--base</code>.
              </div>
              <div className="flex justify-end gap-2">
                <button
                  onClick={() => { navigator.clipboard.writeText(`.kick ${kickInfo.name}`); setKickInfo(null) }}
                  style={{ fontSize: '0.68rem', color: 'var(--accent)', background: 'none', border: '1px solid var(--accent)', padding: '0.4rem 1rem', borderRadius: 2, cursor: 'pointer' }}
                >
                  Copy & Close
                </button>
                <button
                  onClick={() => setKickInfo(null)}
                  style={{ fontSize: '0.68rem', color: 'var(--text-dim)', background: 'none', border: '1px solid var(--border)', padding: '0.4rem 0.8rem', borderRadius: 2, cursor: 'pointer' }}
                >
                  Close
                </button>
              </div>
            </div>
          </div>
        )}

        {/* ── Ban modal ── */}
        {banTarget && (
          <div style={{ position: 'fixed', inset: 0, background: 'rgba(0,0,0,0.7)', zIndex: 9000, display: 'flex', alignItems: 'center', justifyContent: 'center' }}
               onClick={() => setBanTarget(null)}>
            <div onClick={e => e.stopPropagation()} style={{ background: 'var(--bg-elevated)', border: '1px solid rgba(239,68,68,0.4)', borderRadius: 3, padding: '1.5rem', minWidth: 380, maxWidth: 480 }}>
              <div style={{ ...DIM, marginBottom: '0.75rem', color: '#ef4444' }}>Ban Player</div>
              <div style={{ fontSize: '0.72rem', color: 'var(--text)', marginBottom: '1rem' }}>
                <span style={{ ...MONO, color: 'var(--accent)' }}>{banTarget.name}</span>
              </div>
              <div style={{ marginBottom: '0.75rem' }}>
                <label style={{ ...DIM, display: 'block', marginBottom: '0.3rem' }}>Reason</label>
                <input
                  value={banReason}
                  onChange={e => setBanReason(e.target.value)}
                  placeholder="Optional reason"
                  style={{ width: '100%', background: 'var(--bg-card)', border: '1px solid var(--border)', color: 'var(--text)', padding: '0.4rem 0.6rem', fontSize: '0.7rem', borderRadius: 2, boxSizing: 'border-box' }}
                />
              </div>
              <div style={{ marginBottom: '1rem' }}>
                <label style={{ ...DIM, display: 'block', marginBottom: '0.3rem' }}>Ban Until (leave empty = permanent)</label>
                <input
                  type="datetime-local"
                  value={banUntil}
                  onChange={e => setBanUntil(e.target.value)}
                  style={{ width: '100%', background: 'var(--bg-card)', border: '1px solid var(--border)', color: 'var(--text)', padding: '0.4rem 0.6rem', fontSize: '0.7rem', borderRadius: 2, boxSizing: 'border-box' }}
                />
              </div>
              <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginBottom: '1rem' }}>
                This adds the player to bfdb's ban list. For in-game enforcement, also run <code style={{ color: 'var(--accent)' }}>.ban forever {banTarget.name}</code> in DCS chat.
              </div>
              <div className="flex justify-end gap-2">
                <button onClick={() => setBanTarget(null)}
                  style={{ fontSize: '0.68rem', color: 'var(--text-dim)', background: 'none', border: '1px solid var(--border)', padding: '0.4rem 0.8rem', borderRadius: 2, cursor: 'pointer' }}>
                  Cancel
                </button>
                <button onClick={handleBan}
                  style={{ fontSize: '0.68rem', color: '#fff', background: '#ef4444', border: 'none', padding: '0.4rem 1rem', borderRadius: 2, cursor: 'pointer' }}>
                  Ban
                </button>
              </div>
            </div>
          </div>
        )}

        {/* ── Banned players ── */}
        <div className="vs-card">
          <CardHeader
            icon={<Ban size={13} style={{ color: '#ef4444' }} />}
            label="Banned Players"
            badge={<span style={{ ...MONO, fontSize: '0.65rem', color: 'var(--text-muted)' }}>{banned.length}</span>}
          />
          {banned.length === 0 && <div style={{ padding: '1rem', ...DIM }}>No banned players</div>}
          {banned.length > 0 && (
            <table style={{ width: '100%', borderCollapse: 'collapse' }}>
              <thead>
                <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
                  {['Name', 'UCID', 'Reason', 'Until', 'Source', ''].map(h => (
                    <th key={h + '_h'} style={{ ...CELL, color: 'var(--text-dim)', textAlign: 'left', fontWeight: 600 }}>{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {banned.map(b => (
                  <tr key={b.ucid}>
                    <td style={{ ...CELL, color: 'var(--text)' }}>{b.name}</td>
                    <td style={{ ...CELL, ...MONO, fontSize: '0.6rem', color: 'var(--text-dim)' }}>{b.ucid}</td>
                    <td style={{ ...CELL, color: 'var(--text-dim)', fontSize: '0.62rem' }}>{b.reason || '—'}</td>
                    <td style={{ ...CELL, ...MONO, fontSize: '0.62rem' }}>{b.until ? new Date(b.until).toLocaleDateString() : 'Forever'}</td>
                    <td style={{ ...CELL }}>
                      <span style={{ fontSize: '0.55rem', letterSpacing: '0.08em', color: b.source === 'web' ? 'var(--accent)' : 'var(--text-dim)', border: `1px solid ${b.source === 'web' ? 'var(--accent)' : 'var(--border)'}`, padding: '1px 5px', borderRadius: 2 }}>
                        {b.source === 'web' ? 'WEB' : 'ENGINE'}
                      </span>
                    </td>
                    <td style={{ ...CELL, textAlign: 'right' }}>
                      {b.source === 'web' && (
                        unbanTarget === b.ucid ? (
                          <span className="flex items-center gap-2 justify-end">
                            <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)' }}>Confirm?</span>
                            <button onClick={() => handleUnban(b.ucid)}
                              style={{ fontSize: '0.62rem', color: 'var(--accent)', background: 'none', border: '1px solid var(--accent)', padding: '1px 8px', borderRadius: 2, cursor: 'pointer' }}>Yes</button>
                            <button onClick={() => setUnbanTarget(null)}
                              style={{ fontSize: '0.62rem', color: 'var(--text-dim)', background: 'none', border: '1px solid var(--border)', padding: '1px 8px', borderRadius: 2, cursor: 'pointer' }}>No</button>
                          </span>
                        ) : (
                          <button onClick={() => setUnbanTarget(b.ucid)}
                            style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', padding: 0 }} title="Unban">
                            <Trash2 size={12} />
                          </button>
                        )
                      )}
                      {b.source === 'engine' && (
                        <span style={{ fontSize: '0.58rem', color: 'var(--text-dim)' }}>engine-managed</span>
                      )}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          )}
        </div>

        {/* ── Active sessions ── */}
        <div className="vs-card">
          <CardHeader
            icon={<Users size={13} style={{ color: 'var(--accent)' }} />}
            label="Active Web Sessions"
            badge={<span style={{ ...MONO, fontSize: '0.65rem', color: 'var(--text-muted)' }}>{sessions.length}</span>}
          />
          {sessionsLoading && <div style={{ padding: '1rem', ...DIM }}>Loading…</div>}
          {!sessionsLoading && sessions.length === 0 && <div style={{ padding: '1rem', ...DIM }}>No active sessions</div>}
          {sessions.length > 0 && (
            <div style={{ maxHeight: 320, overflowY: 'auto' }}>
              <table style={{ width: '100%', borderCollapse: 'collapse' }}>
                <thead>
                  <tr style={{ background: 'rgba(0,0,0,0.2)', position: 'sticky', top: 0, zIndex: 1 }}>
                    {['User', 'Discord ID', 'Admin', 'Expires'].map(h => (
                      <th key={h} style={{ ...CELL, color: 'var(--text-dim)', textAlign: 'left', fontWeight: 600, background: 'var(--bg-card)' }}>{h}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {sessions.map(s => (
                    <tr key={s.discord_id} style={{ background: s.discord_id === user.discord_id ? 'rgba(56,189,248,0.04)' : 'transparent' }}>
                      <td style={CELL}>
                        <div className="flex items-center gap-2">
                          {s.avatar
                            ? <img src={`https://cdn.discordapp.com/avatars/${s.discord_id}/${s.avatar}.webp?size=24`} alt="" style={{ width: 20, height: 20, borderRadius: '50%' }} />
                            : <div style={{ width: 20, height: 20, borderRadius: '50%', background: 'var(--accent)', flexShrink: 0 }} />}
                          <span style={{ color: 'var(--text)', fontSize: '0.7rem' }}>{s.username}</span>
                          {s.discord_id === user.discord_id && <span style={{ fontSize: '0.5rem', color: 'var(--accent)', border: '1px solid var(--accent)', padding: '0 3px', borderRadius: 2 }}>YOU</span>}
                        </div>
                      </td>
                      <td style={{ ...CELL, ...MONO, fontSize: '0.67rem' }}>{s.discord_id}</td>
                      <td style={CELL}>{s.is_admin && <Shield size={11} style={{ color: '#f59e0b' }} />}</td>
                      <td style={{ ...CELL, ...MONO, fontSize: '0.62rem' }}>{new Date(s.expires).toLocaleString()}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>

        {/* ── Perf history charts ── */}
        <div className="vs-card">
          <CardHeader
            icon={<Activity size={13} style={{ color: 'var(--accent)' }} />}
            label="Engine Performance History"
            badge={
              <div className="flex gap-1">
                {(['charts', 'table-engine', 'table-api'] as const).map(t => (
                  <button key={t} onClick={() => setPerfTab(t)}
                    style={{ fontSize: '0.58rem', letterSpacing: '0.08em', padding: '2px 8px', borderRadius: 2, cursor: 'pointer', textTransform: 'uppercase',
                      background: perfTab === t ? 'var(--accent)' : 'none',
                      color: perfTab === t ? '#000' : 'var(--text-dim)',
                      border: `1px solid ${perfTab === t ? 'var(--accent)' : 'var(--border)'}` }}>
                    {t === 'charts' ? 'Charts' : t === 'table-engine' ? 'Engine' : 'API'}
                  </button>
                ))}
              </div>
            }
          />

          {(!perfHistory || perfHistory.timeline.length === 0) && (
            <div style={{ padding: '1rem', ...DIM }}>No historical data yet — charts populate after each session ends.</div>
          )}

          {perfHistory && perfHistory.timeline.length > 0 && perfTab === 'charts' && (
            <div style={{ padding: '1rem' }}>
              {CHART_GROUPS.map(g => <PerfChart key={g.label} data={perfHistory.timeline} group={g} />)}
              <RedlinePanel sessions={perfHistory.sessions} />
            </div>
          )}

          {perfTab === 'table-engine' && (
            <PerfPanel title="" icon={null} rows={perf?.engine} time={perf?.time} available={perf?.available} />
          )}
          {perfTab === 'table-api' && (
            <PerfPanel title="" icon={null} rows={perf?.api} time={perf?.time} available={perf?.available} />
          )}
        </div>

        {/* ── Engine error/warning feed (persisted server-side) ── */}
        <EngineErrorFeed lines={engineErrors} />

        {/* ── Log analyzer ── */}
        <LogAnalyzer />

        {/* ── Engine log analyzer (live bflib logs, requires bfdb --base) ── */}
        <EngineLogAnalyzer />

        {/* ── DCSServerBot server control ── */}
        <ServerControlPanel />

        {/* ── Danger zone ── */}
        <div className="vs-card" style={{ border: '1px solid rgba(239,68,68,0.25)' }}>
          <CardHeader
            icon={<AlertTriangle size={13} style={{ color: '#ef4444' }} />}
            label="Danger Zone"
          />
          <div className="p-4 flex items-start justify-between gap-6">
            <div>
              <div style={{ fontSize: '0.75rem', color: 'var(--text)', fontWeight: 600, marginBottom: '0.3rem' }}>Reset All Campaign Data</div>
              <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', lineHeight: 1.6, maxWidth: 420 }}>
                Permanently deletes all rounds, kills, objectives, pilot stats, sorties, trails and weather data.
                Admin sessions are preserved.
              </div>
            </div>
            <div className="flex flex-col items-end gap-2 flex-shrink-0">
              {resetDone && <div style={{ fontSize: '0.65rem', color: 'var(--accent)', letterSpacing: '0.1em' }}>✓ Reset complete</div>}
              {!resetConfirm ? (
                <button onClick={() => { setResetConfirm(true); setResetDone(false) }}
                  className="flex items-center gap-1.5"
                  style={{ fontSize: '0.68rem', color: '#ef4444', background: 'none', border: '1px solid rgba(239,68,68,0.4)', padding: '0.4rem 0.9rem', borderRadius: 3, cursor: 'pointer', letterSpacing: '0.08em' }}>
                  <RotateCcw size={11} /> RESET DATABASE
                </button>
              ) : (
                <div className="flex flex-col items-end gap-1.5">
                  <div style={{ fontSize: '0.62rem', color: '#ef4444', letterSpacing: '0.06em' }}>This cannot be undone. Are you sure?</div>
                  <div className="flex gap-2">
                    <button onClick={() => setResetConfirm(false)}
                      style={{ fontSize: '0.65rem', color: 'var(--text-dim)', background: 'none', border: '1px solid var(--border)', padding: '0.3rem 0.8rem', borderRadius: 3, cursor: 'pointer' }}>Cancel</button>
                    <button onClick={handleReset} disabled={resetLoading}
                      style={{ fontSize: '0.65rem', color: '#fff', background: '#ef4444', border: 'none', padding: '0.3rem 0.8rem', borderRadius: 3, cursor: resetLoading ? 'not-allowed' : 'pointer', opacity: resetLoading ? 0.7 : 1 }}>
                      {resetLoading ? 'Resetting…' : 'YES, RESET'}
                    </button>
                  </div>
                </div>
              )}
            </div>
          </div>
        </div>

      </div>
    </div>
  )
}

// ── Redline analysis panel ───────────────────────────────────────────────────

function RedlinePanel({ sessions }: { sessions: { time: string; metrics: PerfRow[] }[] }) {
  if (sessions.length === 0) return null

  // Find worst p99 per metric across all sessions
  const worstByMetric: Record<string, { p99: number; session: string }> = {}
  for (const s of sessions) {
    for (const m of s.metrics) {
      if (!m.n) continue
      const prev = worstByMetric[m.name]
      if (!prev || m.p99 > prev.p99) {
        worstByMetric[m.name] = { p99: m.p99, session: s.time }
      }
    }
  }

  const sorted = Object.entries(worstByMetric)
    .sort(([, a], [, b]) => b.p99 - a.p99)
    .slice(0, 10)

  const maxP99 = sorted[0]?.[1].p99 ?? 1

  return (
    <div style={{ marginTop: '1rem' }}>
      <div style={{ ...DIM, fontSize: '0.6rem', marginBottom: '0.75rem', color: '#f59e0b' }}>
        ⚠ Top Hotspots — worst p99 across all recorded sessions
      </div>
      <div style={{ display: 'flex', flexDirection: 'column', gap: '0.4rem' }}>
        {sorted.map(([name, { p99, session }]) => {
          const pct = (p99 / maxP99) * 100
          const col = perfColor(p99 * 1000)
          return (
            <div key={name}>
              <div className="flex justify-between" style={{ marginBottom: '2px' }}>
                <span style={{ ...MONO, fontSize: '0.62rem', color: 'var(--text)' }}>{name}</span>
                <span style={{ ...MONO, fontSize: '0.6rem', color: col }}>{p99.toLocaleString()} μs p99</span>
              </div>
              <div style={{ height: 6, background: 'var(--bg-card)', borderRadius: 1, overflow: 'hidden' }}>
                <div style={{ width: `${pct}%`, height: '100%', background: col, borderRadius: 1 }} />
              </div>
              <div style={{ fontSize: '0.55rem', color: 'var(--text-dim)', marginTop: '1px' }}>
                {new Date(session).toLocaleDateString()}
              </div>
            </div>
          )
        })}
      </div>
    </div>
  )
}
