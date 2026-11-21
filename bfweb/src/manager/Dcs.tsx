import { useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { Play, Square, RotateCw, RotateCcw, RefreshCw, Server } from '@icons'
import { opsCall, OpsError, type ServerView } from './tauri'
import { opsId } from './pick'
import { Card, Row, Btn, Pill, Note, Result, ConfirmDialog, Dot } from './ui'
import { useAction, fmtWhen, OK, AMBER, RED, MONO } from './style'

type Action = 'start' | 'stop' | 'restart' | 'mission_restart'

export interface DcsRow {
  name: string
  status: string
  node: string | null
  players: number | null
  mission: string | null
  map: string | null
  kind: string | null
  can: Record<Action, boolean>
  blocked: string | null
  last_action: { action: Action; state: 'running' | 'done' | 'failed'; at: string; finished: string | null; error: string | null } | null
}

const LABEL: Record<Action, string> = { start: 'Start', stop: 'Stop', restart: 'Restart DCS', mission_restart: 'Restart mission' }

function what(a: Action, r: DcsRow): { title: string; body: string } {
  const pl = r.players ? ` ${r.players} player(s) on it will be disconnected.` : ' Nobody is on it.'
  switch (a) {
    case 'stop': return { title: `Shut DCS down on ${r.name}?`, body: `The DCS process stops and stays down until you start it.${pl}` }
    case 'restart': return { title: `Restart DCS on ${r.name}?`, body: `DCS is shut down and started again (a staged engine swaps in on the way up).${pl}` }
    case 'mission_restart': return { title: `Restart the mission on ${r.name}?`, body: `DCS keeps running; ${r.mission ?? 'the mission'} loads again from its start.${pl}` }
    default: return { title: `Start DCS on ${r.name}?`, body: '' }
  }
}

function stateOf(st: string): 'ok' | 'warn' | 'bad' | 'off' {
  if (st === 'RUNNING') return 'ok'
  if (st === 'PAUSED' || st === 'LOADING' || st === 'STOPPED' || st === 'SHUTTING_DOWN') return 'warn'
  if (st === 'SHUTDOWN') return 'bad'
  return 'off'
}

/** DCS CONTROL: each DCS instance on the picked box -- start, stop, restart,
 *  restart the mission -- through that box's DCSServerBot. */
export default function Dcs({ server }: { server: ServerView }) {
  const act = useAction()
  const qc = useQueryClient()
  const [ask, setAsk] = useState<{ row: DcsRow; action: Action } | null>(null)
  const key = ['mgr', 'dcs', server.id]
  const { data, error, isFetching, refetch } = useQuery({
    queryKey: key,
    queryFn: () => opsCall<{ servers: DcsRow[] }>(opsId(server), server.name, 'GET', 'dcs'),
    refetchInterval: q => (q.state.data?.servers.some(s => s.last_action?.state === 'running' || s.status === 'LOADING') ? 1500 : 5000),
    retry: false,
  })

  function run(row: DcsRow, action: Action) {
    setAsk(null)
    void act.run(`${row.name}-${action}`, async () => {
      const r = await opsCall<{ message: string }>(opsId(server), server.name, 'POST', `dcs/${action}`, { server: row.name })
      qc.invalidateQueries({ queryKey: key })
      return `${server.name} · ${r.message}`
    })
  }

  const skew = error instanceof OpsError && error.skew
  return (
    <div className="p-5 space-y-4" style={{ maxWidth: 1100 }}>
      <div className="flex items-center gap-3">
        <div style={{ fontSize: '1.1rem', letterSpacing: '0.1em', fontFamily: 'var(--font-display)' }}>DCS SERVERS ON {server.name}</div>
        <span className="ml-auto"><Btn onClick={() => { void refetch() }}><RefreshCw size={11} />{isFetching ? '…' : 'Refresh'}</Btn></span>
      </div>
      <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
        Through that box's DCSServerBot, the same as its Discord commands. Refused while the box's auto-updater is checking for or
        staging a release, or while that server is already being restarted. Restart the bot or bfdb, and apply or roll back engine
        builds, under Server OPS.
      </div>
      <Result r={act.result} onClose={act.clear} />
      {skew && <Note tone="warn">{(error as Error).message}</Note>}
      {error && !skew && <Note tone="bad">{error instanceof Error ? error.message : String(error)}</Note>}
      {!data && !error && <div style={{ fontSize: '0.72rem', color: 'var(--text-dim)' }}>Asking {server.name}…</div>}
      {data && data.servers.length === 0 && <Note>{server.name}'s DCSServerBot has no DCS servers.</Note>}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(340px, 1fr))', gap: 16 }}>
        {data?.servers.map(r => {
          const busy = act.busy !== null || !!r.blocked || r.last_action?.state === 'running'
          const la = r.last_action
          return (
            <Card key={r.name} icon={<Server size={13} style={{ color: OK }} />} title={`${r.name}${r.kind ? ` · ${r.kind}` : ''}`}
                  badge={<span className="flex items-center gap-2"><Dot state={stateOf(r.status)} /><Pill color={r.status === 'RUNNING' ? OK : r.status === 'SHUTDOWN' ? RED : AMBER}>{r.status}</Pill></span>}>
              <Row k="Players">{r.players ?? '—'}</Row>
              <Row k="Mission">{r.mission ? <>{r.mission}{r.map ? <span style={{ color: 'var(--text-dim)' }}> · {r.map}</span> : null}</> : '—'}</Row>
              {r.node && <Row k="Node"><span style={MONO}>{r.node}</span></Row>}
              {la && (
                <Row k="Last action">
                  <span style={{ color: la.state === 'failed' ? RED : la.state === 'running' ? AMBER : undefined }}>
                    {LABEL[la.action] ?? la.action} · {la.state}{la.state !== 'running' && la.finished ? ` ${fmtWhen(la.finished)}` : ` since ${fmtWhen(la.at)}`}
                    {la.error && <> -- {la.error}</>}
                  </span>
                </Row>
              )}
              {r.blocked && <div style={{ marginTop: 8 }}><Note tone="warn">{r.blocked}</Note></div>}
              <div className="flex gap-2" style={{ marginTop: 12, flexWrap: 'wrap' }}>
                <Btn primary disabled={busy || !r.can.start} onClick={() => run(r, 'start')}><Play size={11} />Start</Btn>
                <Btn disabled={busy || !r.can.mission_restart} onClick={() => setAsk({ row: r, action: 'mission_restart' })}><RotateCcw size={11} />Restart mission</Btn>
                <Btn disabled={busy || !r.can.restart} onClick={() => setAsk({ row: r, action: 'restart' })}><RotateCw size={11} />Restart DCS</Btn>
                <Btn danger disabled={busy || !r.can.stop} onClick={() => setAsk({ row: r, action: 'stop' })}><Square size={11} />Stop</Btn>
              </div>
            </Card>
          )
        })}
      </div>
      {ask && (
        <ConfirmDialog server={server.name} local={server.local} title={what(ask.action, ask.row).title}
                       confirmLabel={LABEL[ask.action]} onCancel={() => setAsk(null)} onConfirm={() => run(ask.row, ask.action)}>
          {what(ask.action, ask.row).body}
        </ConfirmDialog>
      )}
    </div>
  )
}
