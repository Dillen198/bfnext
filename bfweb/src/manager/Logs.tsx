import { useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { RefreshCw } from '@icons'
import { mgr } from './tauri'
import { Btn } from './ui'
import { MONO } from './style'

const LOGS = [
  { key: 'agent' as const, label: 'Service (agent.log)' },
  { key: 'bot' as const, label: 'DCSServerBot console (bot-console.log)' },
]

/** The manager's own logs. Everything else (engine, DCS, bfdb, the log
 *  archive and the issue list) is on Server OPS. */
export default function Logs() {
  const [which, setWhich] = useState<'agent' | 'bot'>('agent')
  const [lines, setLines] = useState(500)
  const { data, error, isFetching, refetch } = useQuery({
    queryKey: ['mgr', 'log', which, lines],
    queryFn: () => mgr.readLog(which, lines),
    refetchInterval: 5_000,
    retry: false,
  })
  return (
    <div className="p-5 flex flex-col" style={{ flex: 1, minHeight: 0 }}>
      <div className="flex items-center gap-2" style={{ marginBottom: 10, flexWrap: 'wrap' }}>
        {LOGS.map(l => (
          <button key={l.key} onClick={() => setWhich(l.key)} style={{
            fontSize: '0.66rem', padding: '4px 10px', borderRadius: 2, cursor: 'pointer', ...MONO,
            background: which === l.key ? 'var(--accent)' : 'transparent', color: which === l.key ? '#000' : 'var(--text-dim)',
            border: `1px solid ${which === l.key ? 'var(--accent)' : 'var(--border)'}`,
          }}>{l.label}</button>
        ))}
        <select value={lines} onChange={e => setLines(Number(e.target.value))} className="ml-auto" style={{
          background: 'var(--bg-input)', border: '1px solid var(--border)', color: 'var(--text)', fontSize: '0.66rem', padding: '3px 6px',
        }}>
          {[200, 500, 2000, 10000].map(n => <option key={n} value={n}>{n} lines</option>)}
        </select>
        <Btn onClick={() => refetch()}><RefreshCw size={11} />{isFetching ? '…' : 'Refresh'}</Btn>
        <Btn onClick={() => mgr.openPath('logs')}>Open folder</Btn>
      </div>
      <pre className="vs-card" style={{
        flex: 1, minHeight: 300, margin: 0, overflow: 'auto', padding: '10px 12px', fontSize: '0.66rem', lineHeight: 1.55,
        whiteSpace: 'pre-wrap', wordBreak: 'break-all', ...MONO,
      }}>
        {error ? String(error instanceof Error ? error.message : error) : (data ?? []).join('\n') || 'Empty -- nothing logged yet.'}
      </pre>
    </div>
  )
}
