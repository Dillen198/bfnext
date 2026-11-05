// The rest of the bot console: statistics, the bot's audit trail, and every
// action the bot offers (mods and music among them) as a form.

import { useRef, useState } from 'react'
import { api, type BotFleet } from '../../api'
import { byPlugin, cell, label } from '../../lib/botconsole'
import { ActionForm, Btn, Card, Empty, ResultView, Table, useAction, useCatalog, useRead, useRunner } from './ui'
import { DIM, INPUT, errText } from './style'
import { Activity, ChevronDown, ChevronRight, RotateCw, Search, Shield, TrendingUp, Upload } from '@icons'
import { useQuery, useQueryClient } from '@tanstack/react-query'

const PERIODS = ['day', 'week', 'month', 'quarter', 'year', 'all']
const SCALES = ['1h', '6h', '24h', '7d', '30d']

export function StatsTab({ fleet }: { fleet: BotFleet }) {
  const [period, setPeriod] = useState('month')
  const [server, setServer] = useState('')
  const [scale, setScale] = useState('24h')
  const stats = useRead('get_server_statistics', { period, ...(server ? { server } : {}) })
  const load = useRead('get_load_statistics', { scale, ...(server ? { server } : {}) })
  const { data: cat } = useCatalog()
  const has = (n: string) => !!cat?.actions.find(a => a.name === n && a.available && a.allowed)
  if (!has('get_server_statistics') && !has('get_load_statistics')) {
    return <Empty>No statistics on this bot: they need its monitoring plugin.</Empty>
  }
  const picker = (
    <select value={server} onChange={e => setServer(e.target.value)} style={INPUT}>
      <option value="">all servers</option>
      {fleet.servers.map(s => <option key={s.name} value={s.name}>{s.display_name || s.name}</option>)}
    </select>
  )
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>
      {has('get_server_statistics') && (
        <Card icon={<TrendingUp size={13} style={{ color: 'var(--accent)' }} />} title="Players and playtime"
          right={<>{PERIODS.map(p => <Btn key={p} active={period === p} onClick={() => setPeriod(p)}>{p}</Btn>)}{picker}</>}>
          {stats.isLoading && <Empty>Reading…</Empty>}
          {stats.error && <Empty>{errText(stats.error)}</Empty>}
          {stats.data && <ResultView result={stats.data} />}
        </Card>
      )}
      {has('get_load_statistics') && (
        <Card icon={<Activity size={13} style={{ color: 'var(--accent)' }} />} title="Load"
          right={<>{SCALES.map(s => <Btn key={s} active={scale === s} onClick={() => setScale(s)}>{s}</Btn>)}</>}>
          {load.isLoading && <Empty>Reading…</Empty>}
          {load.error && <Empty>{errText(load.error)}</Empty>}
          {load.data && <ResultView result={load.data} />}
        </Card>
      )}
    </div>
  )
}

export function AuditTab() {
  const [q, setQ] = useState('')
  const [applied, setApplied] = useState('')
  const { data, error, refetch, isFetching } = useQuery({
    queryKey: ['bot', 'audit', applied],
    queryFn: () => api.bot.audit(applied, 300),
    retry: false,
  })
  const rows = data?.rows ?? []
  const preferred = ['time', 'actor', 'node', 'server_name', 'event', 'discord_id', 'ucid']
  const columns = rows.length ? preferred.filter(c => rows.some(r => c in r)) : []
  return (
    <Card icon={<Shield size={13} style={{ color: 'var(--accent)' }} />} title={`Bot audit trail${rows.length ? ` · ${rows.length}` : ''}`} pad={false}
      right={<>
        <input value={q} onChange={e => setQ(e.target.value)} onKeyDown={e => { if (e.key === 'Enter') setApplied(q.trim()) }}
          placeholder="search events, servers, UCIDs" style={{ ...INPUT, width: 220 }} />
        <Btn onClick={() => setApplied(q.trim())}><Search size={10} /></Btn>
        <Btn onClick={() => void refetch()}><RotateCw size={10} />{isFetching ? '…' : ''}</Btn>
      </>}>
      {error && <div style={{ padding: '0.75rem 1rem' }}><Empty>{errText(error)}</Empty></div>}
      {data && rows.length === 0 && <div style={{ padding: '0.75rem 1rem' }}><Empty>Nothing recorded{applied ? ' that matches' : ''}.</Empty></div>}
      {rows.length > 0 && <Table columns={columns} rows={rows} render={(c, v) => c === 'event'
        ? <span style={{ color: 'var(--text)', whiteSpace: 'pre-wrap' }}>{cell(v)}</span> : cell(v)} />}
      <div style={{ ...DIM, padding: '0.5rem 1rem', textTransform: 'none', letterSpacing: 0 }}>
        Everything the bot did, from Discord, its own console and this dashboard (dashboard entries read [web:fowlengine/…]).
      </div>
    </Card>
  )
}

/** Every action the bot offers, grouped by plugin, each as a form. `only`
 *  narrows it to some plugins (the Mods and Music tabs). */
export function ActionsTab({ only, fleet }: { only?: string[]; fleet: BotFleet }) {
  const { data, error } = useCatalog()
  const [open, setOpen] = useState<string | null>(null)
  const [filter, setFilter] = useState('')
  const [server, setServer] = useState('')
  if (error) return <Empty>{errText(error)}</Empty>
  if (!data) return <Empty>Loading…</Empty>
  const needle = filter.trim().toLowerCase()
  const actions = data.actions.filter(a => (!only || only.includes(a.plugin)) && (!needle || a.name.includes(needle)))
  const groups = byPlugin(actions)
  if (!groups.length) return <Empty>{only ? `This bot has no ${only.join('/')} plugin actions.` : 'No actions.'}</Empty>
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>
      <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
        <input value={filter} onChange={e => setFilter(e.target.value)} placeholder="filter actions" style={{ ...INPUT, width: 200 }} />
        <span style={DIM}>server for server actions</span>
        <select value={server} onChange={e => setServer(e.target.value)} style={INPUT}>
          <option value="">(type it in the form)</option>
          {fleet.servers.map(s => <option key={s.name} value={s.name}>{s.display_name || s.name}</option>)}
        </select>
      </div>
      {groups.map(([plugin, list]) => (
        <Card key={plugin} title={`${plugin} · ${list.filter(a => a.available).length}/${list.length}`} pad={false}>
          {list.map(a => {
            const isOpen = open === a.name
            const fixed = server && a.params.some(p => p.name === 'server_name') ? { server_name: server } : {}
            return (
              <div key={a.name} style={{ borderBottom: '1px solid var(--border)' }}>
                <button type="button" onClick={() => setOpen(isOpen ? null : a.name)} className="flex items-center gap-2"
                  style={{ width: '100%', padding: '7px 14px', background: 'none', border: 0, cursor: 'pointer', textAlign: 'left',
                    color: a.available && a.allowed ? 'var(--text)' : 'var(--text-dim)', fontSize: '0.68rem' }}>
                  {isOpen ? <ChevronDown size={11} /> : <ChevronRight size={11} />}
                  {label(a.name)}
                  <span style={{ ...DIM, marginLeft: 'auto', letterSpacing: '0.08em' }}>
                    {a.kind}{!a.available ? ' · not loaded' : !a.allowed ? ` · needs ${a.role}` : ''}
                  </span>
                </button>
                {isOpen && <div style={{ padding: '4px 14px 14px 34px' }}><ActionForm key={`${a.name}-${server}`} action={a} fixed={fixed} /></div>}
              </div>
            )
          })}
        </Card>
      ))}
    </div>
  )
}

/** Music: upload a track into the library, then every music action. */
export function MusicTab({ fleet }: { fleet: BotFleet }) {
  const runner = useRunner()
  const can = !!useAction('add_playlist_track')
  const file = useRef<HTMLInputElement>(null)
  const [folder, setFolder] = useState('')
  const [node, setNode] = useState('')
  const [busy, setBusy] = useState(false)
  const qc = useQueryClient()
  const upload = async () => {
    const f = file.current?.files?.[0]
    if (!f) return
    setBusy(true)
    try {
      const r = await api.bot.uploadMusic(f, f.name, folder.trim(), node)
      runner.say(r.ok, r.message || (r.ok ? `Uploaded ${f.name}` : 'Upload refused'))
      void qc.invalidateQueries({ queryKey: ['bot'] })
    } catch (e) {
      runner.say(false, `Upload: ${errText(e)}`)
    } finally {
      setBusy(false)
      if (file.current) file.current.value = ''
    }
  }
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>
      {can && (
        <Card icon={<Upload size={13} style={{ color: 'var(--accent)' }} />} title="Upload a track">
          <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
            <input ref={file} type="file" accept=".mp3,.ogg" style={{ fontSize: '0.62rem', color: 'var(--text-muted)' }} />
            <input value={folder} onChange={e => setFolder(e.target.value)} placeholder="folder in the library (blank = top)" style={{ ...INPUT, width: 230 }} />
            <select value={node} onChange={e => setNode(e.target.value)} style={INPUT}>
              <option value="">this node</option>
              {fleet.nodes.filter(n => n.online).map(n => <option key={n.name} value={n.name}>{n.name}</option>)}
            </select>
            <Btn disabled={busy} onClick={() => void upload()}><Upload size={10} />{busy ? 'Uploading…' : 'Upload'}</Btn>
          </div>
          <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', marginTop: 6 }}>.mp3 or .ogg. Then add it to a playlist below (add playlist track).</div>
        </Card>
      )}
      <ActionsTab only={['music']} fleet={fleet} />
    </div>
  )
}
