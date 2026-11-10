// Servers: the fleet table with each server's controls, and one server's
// detail -- its missions, players, DCS settings and coalition passwords.

import { useRef, useState } from 'react'
import { api, type BotFleet, type BotServer } from '../../api'
import { cloudsName, duration, getPath, scheduleRows, serverControls, statusTone, type ServerControl } from '../../lib/botconsole'
import { useQuery } from '@tanstack/react-query'
import { useSearchParams } from 'react-router-dom'
import { Btn, Card, ConfirmBtn, Empty, LinkBtn, Pill, Table, useAction, useRead, useRunner } from './ui'
import { CELL, DIM, INPUT, MONO, RED, TH, errText } from './style'
import {
  ChevronDown, ChevronRight, Cloud, Comms, Config, Download, Pause, Play, PlayCircle, RotateCcw, RotateCw, Save,
  Server, Square, Trash2, Upload, ArrowUp, ArrowDown, KeyRound,
} from '@icons'
import { PlayerRows } from './PlayersTab'

const CONTROL: Record<ServerControl, { text: string; icon: React.ReactNode; confirm?: string }> = {
  startup_server: { text: 'Start up', icon: <Play size={10} /> },
  start_server: { text: 'Start', icon: <PlayCircle size={10} /> },
  pause_mission: { text: 'Pause', icon: <Pause size={10} /> },
  unpause_mission: { text: 'Unpause', icon: <PlayCircle size={10} /> },
  restart_mission: { text: 'Restart mission', icon: <RotateCcw size={10} />, confirm: 'Restart the mission?' },
  restart_server: { text: 'Restart', icon: <RotateCw size={10} />, confirm: 'Stop and start the server?' },
  stop_server: { text: 'Stop', icon: <Square size={10} />, confirm: 'Stop the mission (DCS stays up)?' },
  shutdown_server: { text: 'Shut down', icon: <Square size={10} />, confirm: 'Shut DCS down?' },
  message_server: { text: 'Message', icon: <Comms size={10} /> },
}

function ServerControls({ s }: { s: BotServer }) {
  const runner = useRunner()
  const [opts, setOpts] = useState<null | 'startup' | 'shutdown' | 'message'>(null)
  const maint = useAction(s.maintenance ? 'clear_maintenance' : 'set_maintenance')
  const has = {
    startup_server: !!useAction('startup_server'), start_server: !!useAction('start_server'),
    pause_mission: !!useAction('pause_mission'), unpause_mission: !!useAction('unpause_mission'),
    restart_mission: !!useAction('restart_mission'), restart_server: !!useAction('restart_server'),
    stop_server: !!useAction('stop_server'), shutdown_server: !!useAction('shutdown_server'),
    message_server: !!useAction('message_server'),
  } satisfies Record<ServerControl, boolean>
  const busy = runner.busy?.endsWith(`:${s.name}`) ?? false
  const go = (name: string, params: Record<string, unknown> = {}) =>
    void runner.act(name, { server_name: s.name, ...params }, s.name)
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 6 }}>
      <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
        {serverControls(s.status).filter(c => has[c]).map(c => {
          const def = CONTROL[c]
          if (c === 'startup_server') return <Btn key={c} disabled={busy} onClick={() => setOpts(o => o === 'startup' ? null : 'startup')} active={opts === 'startup'}>{def.icon}{def.text}…</Btn>
          if (c === 'shutdown_server') return <Btn key={c} danger disabled={busy} onClick={() => setOpts(o => o === 'shutdown' ? null : 'shutdown')} active={opts === 'shutdown'}>{def.icon}{def.text}…</Btn>
          if (c === 'message_server') return <Btn key={c} disabled={busy} onClick={() => setOpts(o => o === 'message' ? null : 'message')} active={opts === 'message'}>{def.icon}{def.text}…</Btn>
          return def.confirm
            ? <ConfirmBtn key={c} label={def.text} icon={def.icon} disabled={busy}
                confirm={s.players.length ? `${def.confirm} ${s.players.length} on it.` : def.confirm} onConfirm={() => go(c)} />
            : <Btn key={c} disabled={busy} onClick={() => go(c)}>{def.icon}{def.text}</Btn>
        })}
        {maint && (
          <Btn disabled={busy} onClick={() => go(maint.name)} title="Maintenance: the scheduler leaves the server alone">
            {s.maintenance ? 'End maintenance' : 'Maintenance'}
          </Btn>
        )}
        {busy && <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>working…</span>}
      </div>
      {opts === 'startup' && <StartupForm onGo={p => { setOpts(null); go('startup_server', p) }} />}
      {opts === 'shutdown' && <ShutdownForm players={s.players.length} onGo={p => { setOpts(null); go('shutdown_server', p) }} />}
      {opts === 'message' && <MessageForm onGo={p => { setOpts(null); go('message_server', p) }} />}
    </div>
  )
}

function Check({ checked, onChange, children }: { checked: boolean; onChange: (v: boolean) => void; children: React.ReactNode }) {
  return (
    <label className="flex items-center gap-1" style={{ fontSize: '0.64rem', color: 'var(--text-muted)' }}>
      <input type="checkbox" checked={checked} onChange={e => onChange(e.target.checked)} />{children}
    </label>
  )
}

function StartupForm({ onGo }: { onGo: (p: Record<string, unknown>) => void }) {
  const [modify, setModify] = useState(true)
  const [orig, setOrig] = useState(true)
  const [maint, setMaint] = useState(true)
  return (
    <div className="flex items-center gap-3" style={{ flexWrap: 'wrap', padding: '6px 8px', border: '1px solid var(--border)', borderRadius: 3 }}>
      <Check checked={modify} onChange={setModify}>apply presets (MizEdit)</Check>
      <Check checked={orig} onChange={setOrig}>from the original .miz</Check>
      <Check checked={maint} onChange={setMaint}>end maintenance</Check>
      <Btn onClick={() => onGo({ modify_mission: modify, use_orig: orig, maintenance: maint })}><Play size={10} />Start up</Btn>
    </div>
  )
}

function ShutdownForm({ players, onGo }: { players: number; onGo: (p: Record<string, unknown>) => void }) {
  const [force, setForce] = useState(false)
  const [maint, setMaint] = useState(true)
  return (
    <div className="flex items-center gap-3" style={{ flexWrap: 'wrap', padding: '6px 8px', border: '1px solid rgba(239,68,68,0.3)', borderRadius: 3 }}>
      {players > 0 && <span style={{ fontSize: '0.64rem', color: RED }}>{players} player{players === 1 ? '' : 's'} on it.</span>}
      <Check checked={force} onChange={setForce}>force (kill DCS)</Check>
      <Check checked={maint} onChange={setMaint}>maintenance after</Check>
      <Btn danger onClick={() => onGo({ force, maintenance: maint })}><Square size={10} />Shut down</Btn>
    </div>
  )
}

function MessageForm({ onGo }: { onGo: (p: Record<string, unknown>) => void }) {
  const [message, setMessage] = useState('')
  const [mode, setMode] = useState('popup')
  const [coalition, setCoalition] = useState('all')
  return (
    <div className="flex items-center gap-2" style={{ flexWrap: 'wrap', padding: '6px 8px', border: '1px solid var(--border)', borderRadius: 3 }}>
      <input value={message} maxLength={1024} onChange={e => setMessage(e.target.value)} placeholder="Message to everyone on the server" style={{ ...INPUT, flex: '1 1 260px' }} />
      <select value={mode} onChange={e => setMode(e.target.value)} style={INPUT}>
        <option value="popup">popup</option><option value="chat">chat</option>
      </select>
      <select value={coalition} onChange={e => setCoalition(e.target.value)} style={INPUT}>
        <option value="all">all</option><option value="blue">blue</option><option value="red">red</option>
      </select>
      <Btn disabled={!message.trim()} onClick={() => onGo({ message: message.trim(), mode, coalition })}><Comms size={10} />Send</Btn>
    </div>
  )
}

// ── missions ─────────────────────────────────────────────────────────────────

interface MissionRow { index: number; name: string; path?: string; current?: boolean; active?: boolean; resolved?: boolean }

function MissionsCard({ s }: { s: BotServer }) {
  const runner = useRunner()
  const list = useRead<{ missions?: MissionRow[]; listStartIndex?: number }>('get_mission_list', { server_name: s.name })
  const [adding, setAdding] = useState(false)
  const addable = useRead<{ missions?: Record<string, unknown>[] }>('get_addable_missions', { server_name: s.name }, { enabled: adding })
  const can = {
    load: !!useAction('load_mission'), active: !!useAction('set_active_mission'), del: !!useAction('delete_mission'),
    move: !!useAction('reorder_mission'), add: !!useAction('add_mission')
  }
  const [fromDisk, setFromDisk] = useState(false)
  const file = useRef<HTMLInputElement>(null)
  const [upLoad, setUpLoad] = useState(false)
  const [upForce, setUpForce] = useState(false)
  const [uploading, setUploading] = useState(false)
  const missions = list.data?.data.missions ?? []
  const act = (name: string, p: Record<string, unknown>) => void runner.act(name, { server_name: s.name, ...p }, s.name)
  const upload = async () => {
    const f = file.current?.files?.[0]
    if (!f) return
    setUploading(true)
    try {
      const r = await api.bot.uploadMission(s.name, f, f.name, { force: upForce, load: upLoad })
      runner.say(r.ok, r.message || (r.ok ? `Uploaded ${f.name}` : `Upload refused`))
      void list.refetch()
    } catch (e) {
      runner.say(false, `Upload: ${errText(e)}`)
    } finally {
      setUploading(false)
      if (file.current) file.current.value = ''
    }
  }
  return (
    <Card icon={<Server size={13} style={{ color: 'var(--accent)' }} />} title={`Missions · ${s.name}`}
      right={<>
        {can.add && <Btn onClick={() => setAdding(a => !a)} active={adding}>Add from disk…</Btn>}
        <Btn onClick={() => void list.refetch()}><RotateCw size={10} /></Btn>
      </>}>
      {list.error && <Empty>{errText(list.error)}</Empty>}
      {list.data && !list.data.ok && <Empty>{list.data.message}</Empty>}
      {missions.length === 0 && list.data?.ok && <Empty>No missions in the list.</Empty>}
      {missions.length > 0 && (
        <table style={{ width: '100%', borderCollapse: 'collapse' }}>
          <thead><tr>{['#', 'Mission', '', ''].map((h, i) => <th key={i} style={TH}>{h}</th>)}</tr></thead>
          <tbody>
            {missions.map((m, i) => (
              <tr key={`${m.index}-${m.name}`}>
                <td style={{ ...CELL, ...MONO, width: 30 }}>{m.index}</td>
                <td style={CELL}>
                  <span style={{ color: 'var(--text)' }}>{m.name}</span>
                  {m.current && <> <Pill tone="ok">running</Pill></>}
                  {m.active && <> <Pill>start</Pill></>}
                  {m.resolved === false && <> <Pill tone="warn" title="The file is missing">missing</Pill></>}
                  {m.path && <div style={{ fontSize: '0.58rem', color: 'var(--text-dim)', ...MONO }}>{m.path}</div>}
                </td>
                <td style={{ ...CELL, whiteSpace: 'nowrap' }}>
                  {can.move && <>
                    <Btn disabled={i === 0} onClick={() => act('reorder_mission', { mission: m.index, direction: 'up', expected_name: m.name })} title="Up"><ArrowUp size={10} /></Btn>{' '}
                    <Btn disabled={i === missions.length - 1} onClick={() => act('reorder_mission', { mission: m.index, direction: 'down', expected_name: m.name })} title="Down"><ArrowDown size={10} /></Btn>
                  </>}
                </td>
                <td style={{ ...CELL, whiteSpace: 'nowrap', textAlign: 'right' }}>
                  <span className="inline-flex items-center gap-1">
                    {can.load && <ConfirmBtn danger={s.players.length > 0} label={m.current ? 'Reload' : 'Load'} icon={<Play size={10} />}
                      confirm={s.players.length ? `Load it? ${s.players.length} on the server.` : 'Load it now?'}
                      onConfirm={() => act('load_mission', { mission: m.index, expected_name: m.name })} />}
                    {can.active && !m.active && <Btn onClick={() => act('set_active_mission', { mission: m.index, expected_name: m.name })}>Set start</Btn>}
                    {api.bot.files() && <LinkBtn href={api.bot.missionUrl(s.name, m.index)} title="Download"><Download size={10} /></LinkBtn>}
                    {can.del && <ConfirmBtn label="" icon={<Trash2 size={10} />} confirm={fromDisk ? 'Remove and delete the file?' : 'Remove from the list?'}
                      onConfirm={() => act('delete_mission', { mission: m.index, expected_name: m.name, delete_from_disk: fromDisk })} />}
                  </span>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
      <div className="flex items-center gap-3" style={{ marginTop: 10, flexWrap: 'wrap' }}>
        {can.del && <Check checked={fromDisk} onChange={setFromDisk}>removing also deletes the file</Check>}
        {api.bot.files() ? <>
          <span style={{ ...DIM, marginLeft: 'auto' }}>Upload .miz</span>
          <input ref={file} type="file" accept=".miz,.sav" style={{ fontSize: '0.62rem', color: 'var(--text-muted)' }} />
          <Check checked={upForce} onChange={setUpForce}>overwrite</Check>
          <Check checked={upLoad} onChange={setUpLoad}>load it</Check>
          <Btn disabled={uploading} onClick={() => void upload()}><Upload size={10} />{uploading ? 'Uploading…' : 'Upload'}</Btn>
        </> : <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', marginLeft: 'auto' }}>Uploads and downloads: the web dashboard's SERVER BOT page.</span>}
      </div>
      {adding && (
        <div style={{ marginTop: 10 }}>
          {addable.isLoading && <Empty>Reading the missions folder…</Empty>}
          {addable.data && !addable.data.ok && <Empty>{addable.data.message}</Empty>}
          {(addable.data?.data.missions ?? []).length === 0 && addable.data?.ok && <Empty>Every mission on disk is already in the list.</Empty>}
          {(addable.data?.data.missions ?? []).length > 0 && (
            <Table columns={Object.keys(addable.data!.data.missions![0]).slice(0, 3)} rows={addable.data!.data.missions!}
              actions={r => <Btn onClick={() => act('add_mission', { path: String(r.path ?? r.name ?? '') })}>Add</Btn>} />
          )}
        </div>
      )}
    </Card>
  )
}

// ── DCS settings and coalition passwords ────────────────────────────────────

function ConfigCard({ s }: { s: BotServer }) {
  const runner = useRunner()
  const can = !!useAction('set_server_config')
  const q = useRead<{ values?: Record<string, unknown>; editable?: string[]; overridden?: string[] }>('get_server_config', { server_name: s.name })
  const [edits, setEdits] = useState<Record<string, unknown>>({})
  const values = q.data?.data.values ?? {}
  const editable = new Set(q.data?.data.editable ?? [])
  const overridden = new Set(q.data?.data.overridden ?? [])
  const down = ['SHUTDOWN', 'STOPPED', 'UNREGISTERED'].includes((s.status ?? '').toUpperCase())
  const save = async () => {
    const r = await runner.act('set_server_config', { server_name: s.name, values: edits }, s.name)
    if (r?.ok) setEdits({})
  }
  return (
    <Card icon={<Config size={13} style={{ color: 'var(--accent)' }} />} title={`DCS settings · ${s.name}`}
      right={can && <Btn disabled={!Object.keys(edits).length || !down} onClick={() => void save()} title={down ? '' : 'Only while the server is down'}><Save size={10} />Save {Object.keys(edits).length || ''}</Btn>}>
      {q.data && !q.data.ok && <Empty>{q.data.message}</Empty>}
      {!down && can && <Empty>Settings can be changed while the server is shut down or stopped.</Empty>}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(220px, 1fr))', gap: 8 }}>
        {Object.entries(values).map(([k, v]) => {
          const cur = k in edits ? edits[k] : v
          const ro = !can || !editable.has(k) || !down
          const secret = v === '<set>' || v === '<unset>'
          const set = (x: unknown) => setEdits(e => {
            const n = { ...e }
            if (x === v) delete n[k]
            else n[k] = x
            return n
          })
          return (
            <label key={k} style={{ display: 'flex', flexDirection: 'column', gap: 3 }}>
              <span style={{ ...DIM, letterSpacing: '0.06em', textTransform: 'none' }}>
                {k}{overridden.has(k) && <span style={{ color: 'var(--yellow)' }} title="servers.yaml sets this"> · servers.yaml</span>}
              </span>
              {typeof v === 'boolean'
                ? <input type="checkbox" disabled={ro} checked={cur === true} onChange={e => set(e.target.checked)} style={{ alignSelf: 'flex-start' }} />
                : secret
                  // the bot never sends a secret, only whether it is set
                  ? <input disabled={ro} type="password" value={k in edits ? String(edits[k] ?? '') : ''} style={INPUT}
                      placeholder={v === '<set>' ? 'set -- type to change' : 'not set'}
                      onChange={e => setEdits(x => { const n = { ...x }; if (e.target.value) n[k] = e.target.value; else delete n[k]; return n })} />
                  : <input disabled={ro} type={typeof v === 'number' ? 'number' : 'text'} value={String(cur ?? '')} style={INPUT}
                      onChange={e => set(typeof v === 'number' ? Number(e.target.value) : e.target.value)} />}
            </label>
          )
        })}
      </div>
    </Card>
  )
}

function CoalitionsCard({ s }: { s: BotServer }) {
  const runner = useRunner()
  const can = !!useAction('set_coalition_password')
  const q = useRead<{ blue?: string | null; red?: string | null }>('get_server_coalitions', { server_name: s.name })
  const [blue, setBlue] = useState('')
  const [red, setRed] = useState('')
  if (!useAction('get_server_coalitions')) return null
  const save = (values: Record<string, string | null>) =>
    void runner.act('set_coalition_password', { server_name: s.name, values }, s.name).then(() => { setBlue(''); setRed('') })
  return (
    <Card icon={<KeyRound size={13} style={{ color: 'var(--accent)' }} />} title={`Coalition passwords · ${s.name}`}>
      {q.data && !q.data.ok && <Empty>{q.data.message}</Empty>}
      {q.data?.ok && (
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(260px, 1fr))', gap: 10 }}>
          {([['blue', blue, setBlue], ['red', red, setRed]] as const).map(([side, val, setVal]) => (
            <div key={side} className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
              <span style={{ ...DIM, width: 40 }}>{side}</span>
              <span style={{ fontSize: '0.64rem', ...MONO, color: 'var(--text-muted)' }}>{q.data!.data[side] ? 'set' : 'none'}</span>
              {can && <>
                <input value={val} onChange={e => setVal(e.target.value)} placeholder="new password" style={{ ...INPUT, width: 130 }} />
                <Btn disabled={!val.trim()} onClick={() => save({ [side]: val })}>Set</Btn>
                {q.data!.data[side] && <ConfirmBtn label="Clear" confirm={`Remove the ${side} password?`} onConfirm={() => save({ [side]: null })} />}
              </>}
            </div>
          ))}
        </div>
      )}
    </Card>
  )
}

// ── the tab ──────────────────────────────────────────────────────────────────

// ── overview: weather, extensions, schedule ─────────────────────────────────

const DAYS = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']

function num(v: unknown, digits = 0): string | null {
  return typeof v === 'number' && Number.isFinite(v) ? v.toFixed(digits) : null
}

function OverviewCard({ s }: { s: BotServer }) {
  const [, setParams] = useSearchParams()
  const q = useQuery({ queryKey: ['bot', 'server', s.name], queryFn: () => api.bot.server(s.name), refetchInterval: 60_000, retry: false })
  const d = q.data
  const w = d?.weather ?? {}
  const wind = (getPath(w, ['wind', 'atGround']) ?? {}) as Record<string, unknown>
  const facts: [string, string | null][] = [
    ['Temperature', num(getPath(w, ['season', 'temperature'])) && `${num(getPath(w, ['season', 'temperature']))} °C`],
    ['QNH', num(getPath(w, ['qnh'])) && `${num(getPath(w, ['qnh']))} mmHg`],
    ['Wind (ground)', num(wind.speed, 1) && `${num(wind.dir)}° / ${num(wind.speed, 1)} m/s`],
    ['Clouds', cloudsName(d?.clouds)],
    ['Visibility', d?.visibility ? `${Math.round(d.visibility).toLocaleString()} m (live)` : num(getPath(w, ['visibility', 'distance'])) && `${num(getPath(w, ['visibility', 'distance']))} m`],
    ['Mission date', s.mission?.date ? String(s.mission.date).slice(0, 10) : null],
  ]
  const shown = facts.filter(([, v]) => v)
  const sched = scheduleRows(d?.schedule)
  return (
    <Card icon={<Cloud size={13} style={{ color: 'var(--accent)' }} />} title={`Overview · ${s.name}`}
      right={<Btn onClick={() => setParams(p => { const n = new URLSearchParams(p); n.set('tab', 'settings'); n.set('file', 'plugins/scheduler.yaml'); return n })} title="Edit the scheduler's config">Edit schedule</Btn>}>
      {q.error && <Empty>{errText(q.error)}</Empty>}
      <div style={{ display: 'flex', flexDirection: 'column', gap: 12 }}>
        {shown.length > 0 && (
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(150px, 1fr))', gap: '6px 14px' }}>
            {shown.map(([k, v]) => (
              <div key={k}>
                <div style={{ ...DIM, letterSpacing: '0.08em' }}>{k}</div>
                <div style={{ fontSize: '0.72rem', color: 'var(--text)', ...MONO }}>{v}</div>
              </div>
            ))}
          </div>
        )}
        {d && !shown.length && <Empty>No weather: no mission is loaded.</Empty>}
        {(d?.extensions ?? []).length > 0 && (
          <div className="flex gap-4" style={{ flexWrap: 'wrap' }}>
            {d!.extensions.map((e, i) => (
              <div key={i}>
                <div style={{ ...DIM, letterSpacing: '0.08em' }}>{String(e.name ?? 'extension')}</div>
                <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', ...MONO }}>{String(e.value ?? '')}</div>
              </div>
            ))}
          </div>
        )}
        {sched.length > 0 && (
          <table style={{ borderCollapse: 'collapse', alignSelf: 'flex-start' }}>
            <thead><tr><th style={TH}>Hours</th>{DAYS.map(x => <th key={x} style={{ ...TH, textAlign: 'center' }}>{x}</th>)}</tr></thead>
            <tbody>
              {sched.map(r => (
                <tr key={r.hours}>
                  <td style={{ ...CELL, ...MONO }}>{r.hours}</td>
                  {r.days.map((c, i) => (
                    <td key={i} style={{ ...CELL, textAlign: 'center' }} title={c === 'Y' ? 'up' : c === 'N' ? 'down' : c === 'P' ? 'up while people are on' : '?'}>
                      <Pill tone={c === 'Y' ? 'ok' : c === 'N' ? 'down' : c === 'P' ? 'warn' : 'dim'}>{c}</Pill>
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </div>
    </Card>
  )
}

function ServerDetail({ s }: { s: BotServer }) {
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 12, padding: '10px 0 14px' }}>
      <ServerControls s={s} />
      <OverviewCard s={s} />
      {s.players.length > 0 && (
        <Card title={`Players · ${s.players.length}`} pad={false}><PlayerRows servers={[s]} /></Card>
      )}
      <MissionsCard s={s} />
      <ConfigCard s={s} />
      <CoalitionsCard s={s} />
    </div>
  )
}

export default function ServersTab({ fleet }: { fleet: BotFleet }) {
  const [open, setOpen] = useState<string | null>(fleet.servers.length === 1 ? fleet.servers[0].name : null)
  if (!fleet.servers.length) return <Empty>The bot knows no servers.</Empty>
  return (
    <div className="vs-card" style={{ padding: 0 }}>
      <table style={{ width: '100%', borderCollapse: 'collapse' }}>
        <thead>
          <tr>{['', 'Server', 'State', 'Mission', 'Map', 'Up', 'Players', 'Node', 'Restart'].map(h => <th key={h} style={TH}>{h}</th>)}</tr>
        </thead>
        <tbody>
          {fleet.servers.map(s => {
            const isOpen = open === s.name
            return [
              <tr key={s.name} onClick={() => setOpen(isOpen ? null : s.name)} style={{ cursor: 'pointer', background: isOpen ? 'rgba(255,255,255,0.03)' : undefined }}>
                <td style={{ ...CELL, width: 20 }}>{isOpen ? <ChevronDown size={11} /> : <ChevronRight size={11} />}</td>
                <td style={{ ...CELL, color: 'var(--text)' }}>
                  {s.display_name || s.name}
                  {s.display_name && s.display_name !== s.name && <div style={{ fontSize: '0.58rem', color: 'var(--text-dim)', ...MONO }}>{s.name}</div>}
                </td>
                <td style={CELL}>
                  <Pill tone={statusTone(s.status)}>{s.status ?? '?'}</Pill>
                  {s.maintenance && <> <Pill tone="warn">maint</Pill></>}
                  {s.password && <> <Pill title="Password set">locked</Pill></>}
                </td>
                <td style={CELL}>{s.mission?.name ?? '—'}</td>
                <td style={CELL}>{s.mission?.map ?? '—'}</td>
                <td style={{ ...CELL, ...MONO }}>{duration(s.mission?.mission_time)}</td>
                <td style={{ ...CELL, ...MONO }}>{s.players.length}{s.max_players ? ` / ${s.max_players}` : ''}</td>
                <td style={CELL}>{s.node ?? '—'}{s.is_remote && <> <Pill>remote</Pill></>}</td>
                <td style={{ ...CELL, ...MONO }}>{s.restart_time ? new Date(s.restart_time).toLocaleTimeString() : '—'}</td>
              </tr>,
              isOpen && (
                <tr key={`${s.name}-detail`}>
                  <td colSpan={9} style={{ padding: '0 12px', borderBottom: '1px solid var(--border)' }}><ServerDetail s={s} /></td>
                </tr>
              ),
            ]
          })}
        </tbody>
      </table>
    </div>
  )
}
