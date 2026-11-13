import { useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { Save, CheckCircle2, RefreshCw } from '@icons'
import { opsCall, OpsError, type RemoteBackupStatus, type ServerView } from './tauri'
import { opsId } from './pick'
import { Card, Btn, Note, Result, Field, ConfirmDialog, Pill } from './ui'
import { JobPanel, PasswordPair } from './Backup'
import { useAction, input, fmtBytes, fmtWhen, MONO, AMBER, OK, RED } from './style'

/** BACKUP for a remote box: its Manager service runs the job (as the box's
 *  own BACKUP tab would, nothing stopped), asked through its OPS API. */
export default function RemoteBackup({ server }: { server: ServerView }) {
  const act = useAction()
  const qc = useQueryClient()
  const key = ['mgr', 'remote-backup', server.id]
  const { data, error, refetch, isFetching } = useQuery({
    queryKey: key,
    queryFn: () => opsCall<RemoteBackupStatus>(opsId(server), server.name, 'GET', 'backup'),
    refetchInterval: q => (q.state.data?.job?.running ? 1500 : 5000),
    retry: false,
  })
  const [password, setPassword] = useState<{ pw: string | null; problem: string | null }>({ pw: null, problem: null })
  const [asking, setAsking] = useState(false)
  const [verifyPw, setVerifyPw] = useState('')
  const running = !!data?.job?.running
  const post = (path: string, body: unknown, ok: string) => act.run(path, async () => {
    const r = await opsCall<{ message?: string }>(opsId(server), server.name, 'POST', path, body)
    qc.invalidateQueries({ queryKey: key })
    return `${server.name}: ${r.message ?? ok}`
  })
  const skew = error instanceof OpsError && error.skew

  return (
    <div className="p-5 space-y-4" style={{ maxWidth: 980 }}>
      <div className="flex items-center gap-3">
        <div style={{ fontSize: '1.1rem', letterSpacing: '0.1em', fontFamily: 'var(--font-display)' }}>BACKUPS OF {server.name}</div>
        <span className="ml-auto"><Btn onClick={() => { void refetch() }}><RefreshCw size={11} />{isFetching ? '…' : 'Refresh'}</Btn></span>
      </div>
      <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
        That box's Fowl Engine Manager service makes the backup on the box itself, the way its own BACKUP tab would with nothing
        changed: its usual folders, the bot's database, into its usual backup folder, read from a shadow copy so nothing has to stop.
        The zip stays on that box. <b>Restoring is only ever done on the box itself</b> (Fowl Engine Manager there → BACKUP → Restore):
        it overwrites the box, and someone has to be at it.
      </div>
      <Result r={act.result} onClose={act.clear} />
      {skew && <Note tone="warn">{(error as Error).message}</Note>}
      {error && !skew && (
        <Note tone="warn">
          {error instanceof Error ? error.message : String(error)}
          <div style={{ marginTop: 6, color: 'var(--text-muted)' }}>
            On {server.name}: Fowl Engine Manager → SETTINGS → tick <b>Let a controller PC start backups of this box</b> (and set the
            desktop user the bot runs as).
          </div>
        </Note>
      )}
      {data && (
        <>
          <div className="flex gap-3" style={{ fontSize: '0.66rem', color: 'var(--text-dim)', flexWrap: 'wrap', ...MONO }}>
            <span>{data.hostname}</span>
            <span>Manager {data.manager_version}</span>
            <span>saves to {data.default_dest ?? '?'}</span>
            <span style={{ color: data.stale ? RED : undefined }}>status {data.age_secs ?? 0}s old{data.stale ? ' -- is its service running?' : ''}</span>
          </div>
          {data.problem && <Note tone="warn">{data.problem}</Note>}
          {data.job && (
            <JobPanel job={data.job} remote={server.name}
                      onCancel={() => { void post('backup/cancel', {}, 'cancelling') }} />
          )}

          <Card icon={<Save size={13} style={{ color: OK }} />} title={`Back up ${server.name}`}>
            <div className="space-y-3">
              <PasswordPair onChange={(pw, problem) => setPassword({ pw, problem })} />
              <Btn primary disabled={act.busy !== null || running || !!password.problem || !!data.stale} onClick={() => setAsking(true)}>
                <Save size={11} />Back up {server.name} now
              </Btn>
            </div>
          </Card>

          <Card icon={<CheckCircle2 size={13} style={{ color: OK }} />} title={`Backups on ${server.name}`}
                badge={<Pill color="var(--text-dim)">{data.backups.length}</Pill>}>
            <div className="space-y-2">
              {data.backups.length === 0 && <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)' }}>None found in the usual places on that box.</div>}
              {data.backups.map(b => (
                <div key={b.path} className="flex items-center gap-2" style={{ fontSize: '0.68rem', ...MONO }}>
                  <span style={{ flex: 1, minWidth: 0, wordBreak: 'break-all' }}>{b.path} <span style={{ color: 'var(--text-dim)' }}>· {fmtBytes(b.bytes)} · {fmtWhen(b.modified)}</span></span>
                  <Btn disabled={act.busy !== null || running || !!data.stale} onClick={() => { void post('backup/verify', { zip: b.path, password: verifyPw || null }, 'checking') }}>Check</Btn>
                </div>
              ))}
              {data.backups.length > 0 && (
                <div style={{ maxWidth: 320 }}>
                  <Field label="Password (only for an encrypted backup)">
                    <input style={input} type="password" autoComplete="off" value={verifyPw} onChange={e => setVerifyPw(e.target.value)} />
                  </Field>
                </div>
              )}
            </div>
          </Card>

          {data.handled.length > 0 && (
            <Card title="Recent requests">
              {data.handled.map(h => (
                <div key={h.id} style={{ fontSize: '0.66rem', padding: '3px 0', color: h.accepted ? 'var(--text-muted)' : AMBER }}>
                  {h.accepted ? '✓' : '✗'} {fmtWhen(h.at)} · {h.kind}: {h.message}
                </div>
              ))}
            </Card>
          )}
        </>
      )}
      {asking && (
        <ConfirmDialog server={server.name} local={server.local} danger={false} confirmLabel="Back up"
                       title={`Back up ${server.name} now?`} onCancel={() => setAsking(false)}
                       onConfirm={() => { setAsking(false); void post('backup/start', { password: password.pw }, 'backup asked for') }}>
          Everything its own plan includes, into {data?.default_dest ?? 'its usual backup folder'}{password.pw ? ', encrypted with your password' : ''}.
          It reads a shadow copy, so players can keep flying; the disk is busy for a while.
        </ConfirmDialog>
      )}
    </div>
  )
}
