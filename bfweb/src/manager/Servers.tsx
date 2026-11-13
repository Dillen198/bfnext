import { useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { Server, Plus, Pencil, Trash2, CheckCircle2, Terminal, Activity } from '@icons'
import type { OpsStatus } from '../api'
import { mgr, opsCall, type AppState, type ServerInput, type ServerTest, type ServerView } from './tauri'
import { opsId, pickServer } from './pick'
import { Card, Row, Btn, Pill, Note, Field, Result, Dot, ConfirmDialog } from './ui'
import { useAction, fmtDur, fmtWhen, input, OK, AMBER, RED, MONO, DIM } from './style'

type Go = 'dcs' | 'logs' | 'ops'

function short(sha?: string | null): string {
  return sha ? sha.slice(0, 10) : '—'
}

/** One box's status card, from its GET status. */
function StatusCard({ s, current, onGo }: { s: ServerView; current: boolean; onGo: (tab: Go) => void }) {
  const { data, error, dataUpdatedAt } = useQuery({
    queryKey: ['mgr', 'srv-status', s.id],
    queryFn: () => opsCall<OpsStatus>(opsId(s), s.name, 'GET', 'status'),
    enabled: !s.problem,
    refetchInterval: 10_000,
    retry: false,
  })
  const up = data?.servers.filter(x => x.status === 'RUNNING' || x.status === 'PAUSED').length ?? 0
  const players = data?.servers.reduce((a, x) => a + (x.players ?? 0), 0) ?? 0
  const upd = data?.updates
  const staged = data?.servers.filter(x => x.pending).length ?? 0
  const bfdb = data?.bfdb
  const reach = s.problem ? 'bad' : error ? 'bad' : data ? 'ok' : 'off'
  return (
    <div className="vs-card" style={{ borderColor: current ? 'rgba(106,171,31,0.55)' : undefined, display: 'flex', flexDirection: 'column' }}>
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Dot state={reach} />
        <span style={{ fontFamily: 'var(--font-display)', letterSpacing: '0.1em', fontSize: '1rem' }}>{s.name}</span>
        <Pill color={s.local ? 'var(--text-dim)' : AMBER}>{s.local ? 'this PC' : 'remote'}</Pill>
        {current && <span className="ml-auto"><Pill color={OK}>selected</Pill></span>}
      </div>
      <div className="p-4" style={{ flex: 1 }}>
        {s.problem && <Note tone="bad">{s.problem}</Note>}
        {error && <Note tone="bad">{error instanceof Error ? error.message : String(error)}</Note>}
        {!data && !error && !s.problem && <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)' }}>Asking {s.name}…</div>}
        {data && (
          <>
            <Row k="Host">{data.host.hostname}{data.host.uptime_secs != null && <span style={{ color: 'var(--text-dim)' }}> · up {fmtDur(data.host.uptime_secs)}</span>}
              {data.host.reboot_pending && <span style={{ color: AMBER }}> · reboot pending</span>}</Row>
            <Row k="DCS">
              <span style={{ color: up === data.servers.length && up > 0 ? OK : up ? AMBER : RED }}>{up} of {data.servers.length} up</span>
              {' · '}{players} player(s)
              <div style={{ marginTop: 4 }}>
                {data.servers.map(x => (
                  <div key={x.name} className="flex items-center gap-2" style={{ fontSize: '0.66rem' }}>
                    <Dot state={x.status === 'RUNNING' ? 'ok' : x.status === 'SHUTDOWN' ? 'bad' : 'warn'} />
                    <span style={MONO}>{x.name}</span>
                    <span style={{ color: 'var(--text-dim)' }}>{x.status.toLowerCase()}{x.players ? ` · ${x.players} on` : ''}{x.mission ? ` · ${x.mission}` : ''}</span>
                  </div>
                ))}
              </div>
            </Row>
            <Row k="bfdb">{!bfdb?.managed ? 'not run by the bot' : bfdb.running
              ? <span style={{ color: bfdb.healthy === false ? AMBER : OK }}>running{bfdb.healthy === false ? ' (not answering)' : ''}{bfdb.uptime_secs != null ? ` · up ${fmtDur(bfdb.uptime_secs)}` : ''}</span>
              : <span style={{ color: RED }}>down</span>}</Row>
            <Row k="Bot">DCSServerBot {data.bot.dcsserverbot_version ?? '?'}<span style={{ color: 'var(--text-dim)' }}> · plugin up since {fmtWhen(data.bot.plugin_started_at)}</span></Row>
            <Row k="Engine">
              {data.servers.map(x => (
                <div key={x.name} style={{ fontSize: '0.66rem', ...MONO }}>
                  {x.name}: {x.loaded_build?.git ?? short(x.dll_sha256)}{x.loaded_build?.built ? <span style={{ color: 'var(--text-dim)' }}> · built {fmtWhen(x.loaded_build.built)}</span> : null}
                </div>
              ))}
              {data.bfdb_build && <div style={{ fontSize: '0.66rem', ...MONO }}>bfdb: {data.bfdb_build.git ?? data.bfdb_build.version ?? '?'}</div>}
            </Row>
            <Row k="Updates">{!upd ? 'auto-updater not loaded' : upd.busy
              ? <span style={{ color: AMBER }}>{upd.busy}</span>
              : <>
                  {upd.config.enabled ? 'auto-update on' : 'auto-update off'}
                  {staged > 0 && <span style={{ color: AMBER }}> · {staged} staged</span>}
                  {upd.latest?.tag && <span style={{ color: 'var(--text-dim)' }}> · latest {upd.latest.tag}</span>}
                  {upd.probation.length > 0 && <span style={{ color: AMBER }}> · {upd.probation.length} on probation</span>}
                  {upd.last_check?.ok === false && <span style={{ color: RED }}> · last check failed</span>}
                </>}</Row>
            {data.issues && <Row k="Issues">{data.issues.open ? <span style={{ color: AMBER }}>{data.issues.open} open</span> : 'none open'}</Row>}
          </>
        )}
      </div>
      <div className="flex gap-2 px-4 pb-4" style={{ flexWrap: 'wrap' }}>
        <Btn primary={!current} onClick={() => onGo('dcs')}><Server size={11} />DCS control</Btn>
        <Btn onClick={() => onGo('logs')}><Terminal size={11} />Live logs</Btn>
        <Btn onClick={() => onGo('ops')}><Activity size={11} />Server OPS</Btn>
        {dataUpdatedAt > 0 && <span className="ml-auto" style={{ ...DIM, alignSelf: 'center', textTransform: 'none' }}>{new Date(dataUpdatedAt).toLocaleTimeString()}</span>}
      </div>
    </div>
  )
}

const EMPTY: ServerInput = { id: null, name: '', url: '', key: '' }

/** Add / edit one remote box. */
function ServerForm({ start, local, onDone }: { start: ServerInput; local: boolean; onDone: () => void }) {
  const act = useAction()
  const [d, setD] = useState<ServerInput>(start)
  const [test, setTest] = useState<ServerTest | null>(null)
  const editing = !!start.id
  const keyInput = d.key === null ? null : d.key
  const payload = (): ServerInput => ({ ...d, key: keyInput === '' && editing ? null : keyInput })
  return (
    <div className="space-y-3" style={{ border: '1px solid var(--border)', borderRadius: 3, padding: 12, marginTop: 10 }}>
      <Result r={act.result} onClose={act.clear} />
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(240px, 1fr))', gap: 12 }}>
        <Field label="Name" hint="Shown on every confirm -- make it one you can't mix up (CAMPAIGN, RANGE).">
          <input style={input} value={d.name} maxLength={40} onChange={e => setD({ ...d, name: e.target.value })} placeholder="RANGE" />
        </Field>
        {!local && (
          <Field label="OPS address (Tailscale)" hint={<>The box's <b>Tailscale</b> address (<span style={MONO}>tailscale ip -4</span> on it: 100.64.x.x–100.127.x.x, or name.tailnet.ts.net),
              its WebService port and prefix. That box's Manager shows its OPS address at the bottom (with 127.0.0.1).</>}>
            <input style={{ ...input, ...MONO }} value={d.url} onChange={e => setD({ ...d, url: e.target.value })} placeholder="http://100.101.2.86:9876/stats" />
          </Field>
        )}
        {!local && (
          <Field label={editing ? 'OPS key (blank = keep the saved one)' : 'OPS key'}
                 hint={<>That box's <span style={MONO}>ops_api.api_key</span> (fowlengine.yaml). Stored encrypted on this PC; never shown again.</>}>
            <input style={{ ...input, ...MONO }} type="password" autoComplete="off" value={d.key ?? ''} onChange={e => setD({ ...d, key: e.target.value })} />
          </Field>
        )}
      </div>
      {test && <Note tone={test.ok ? (test.plugin_current ? 'ok' : 'warn') : 'bad'}>{test.message}</Note>}
      <div className="flex gap-2">
        {!local && (
          <Btn disabled={act.busy !== null || !d.url.trim()} onClick={() => act.run('test', async () => {
            const r = await mgr.testServer(payload())
            setTest(r)
            if (!r.ok) throw new Error(r.message)
            return `${r.hostname ?? d.name}: connected`
          })}><CheckCircle2 size={11} />{act.busy === 'test' ? 'Testing…' : 'Test connection'}</Btn>
        )}
        <Btn primary disabled={act.busy !== null || !d.name.trim() || (!local && !d.url.trim()) || (!editing && !local && !d.key)} onClick={() => act.run('save', async () => {
          await mgr.saveServer(payload())
          onDone()
          return 'saved'
        })}>Save</Btn>
        <Btn onClick={onDone}>Cancel</Btn>
      </div>
    </div>
  )
}

/** ALL SERVERS: every box side by side, and the list of boxes this PC controls. */
export default function Servers({ state, current, onGo }: {
  state: AppState; current: ServerView | null; onGo: (id: string, tab: Go) => void
}) {
  const act = useAction()
  const qc = useQueryClient()
  const [editing, setEditing] = useState<string | null>(null)
  const [adding, setAdding] = useState(false)
  const [removing, setRemoving] = useState<ServerView | null>(null)
  const done = () => { setEditing(null); setAdding(false); qc.invalidateQueries({ queryKey: ['mgr'] }) }

  return (
    <div className="p-5 space-y-4">
      <div>
        <div style={{ fontSize: '1.1rem', letterSpacing: '0.1em', fontFamily: 'var(--font-display)' }}>ALL SERVERS</div>
        <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginTop: 4, lineHeight: 1.6 }}>
          Every server box this PC controls, side by side. Remote boxes are reached over Tailscale through their Fowl Engine OPS API
          (see <span style={MONO}>deploy/manager-remote.md</span> for opening it on each box). The picker at the top decides which box every other tab acts on.
        </div>
      </div>
      {state.servers.length === 0 && (
        <Note tone="warn">No servers yet. Add the boxes this PC should control below{state.config.controller_only ? '' : ' (or run Setup if this PC is a server box itself)'}.</Note>
      )}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(360px, 1fr))', gap: 16 }}>
        {state.servers.map(s => (
          <StatusCard key={s.id} s={s} current={current?.id === s.id} onGo={tab => onGo(s.id, tab)} />
        ))}
      </div>

      <Card icon={<Server size={13} style={{ color: OK }} />} title="Servers this PC controls"
            badge={!adding && <Btn onClick={() => { setAdding(true); setEditing(null) }}><Plus size={11} />Add a server</Btn>}>
        <Result r={act.result} onClose={act.clear} />
        {state.servers.map(s => (
          <div key={s.id} style={{ borderBottom: '1px solid var(--border)', padding: '8px 0' }}>
            <div className="flex items-center gap-3" style={{ flexWrap: 'wrap' }}>
              <span style={{ fontFamily: 'var(--font-display)', letterSpacing: '0.08em', minWidth: 120 }}>{s.name}</span>
              <span style={{ ...MONO, fontSize: '0.66rem', color: 'var(--text-dim)', flex: 1, minWidth: 200, wordBreak: 'break-all' }}>
                {s.url ?? '—'}{s.local ? ' (this PC: key from its own fowlengine.yaml)' : ''}
              </span>
              {!s.local && <Pill color={s.has_key ? OK : RED}>{s.has_key ? 'key saved' : 'no key'}</Pill>}
              <Btn onClick={() => { setEditing(s.id); setAdding(false); pickServer(s.id) }}><Pencil size={11} />Edit</Btn>
              {!s.local && <Btn danger onClick={() => setRemoving(s)}><Trash2 size={11} />Remove</Btn>}
            </div>
            {editing === s.id && (
              <ServerForm local={s.local} onDone={done}
                          start={{ id: s.id, name: s.name, url: s.local ? '' : s.url ?? '', key: '' }} />
            )}
          </div>
        ))}
        {adding && <ServerForm local={false} start={EMPTY} onDone={done} />}
        <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 10, lineHeight: 1.6 }}>
          Only Tailscale addresses are accepted: the OPS API is plain HTTP with the key in a header, so it must only travel inside the
          tailnet's encrypted tunnel. Keys are stored DPAPI-encrypted in <span style={MONO}>%ProgramData%\FowlEngine\manager.json</span> and never sent to this window.
        </div>
      </Card>

      {removing && (
        <ConfirmDialog server={removing.name} title={`Remove ${removing.name} from this PC's list?`} confirmLabel="Remove"
                       onCancel={() => setRemoving(null)} onConfirm={() => {
                         const id = removing.id
                         setRemoving(null)
                         void act.run('rm', async () => { await mgr.removeServer(id); return 'removed' })
                       }}>
          Its saved key is deleted from this PC. Nothing changes on the box itself.
        </ConfirmDialog>
      )}
    </div>
  )
}
