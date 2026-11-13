// Players: who is on each server now (with kick, ban, mute, message, watch,
// screenshot), the bot's player directory, and one player's record.

import { useEffect, useRef, useState } from 'react'
import { api, type BotFleet, type BotPlayer, type BotServer } from '../../api'
import { cell, duration, mainTable } from '../../lib/botconsole'
import { ActionForm, Btn, Card, ConfirmBtn, Empty, Pill, ResultView, Table, useAction, useCatalog, useRead, useRunner } from './ui'
import { CELL, DIM, INPUT, MONO, TH } from './style'
import { Ban, Comms, Eye, EyeOff, Image, Pilot, Search, UserX, X } from '@icons'

type Open = null | 'kick' | 'ban' | 'message' | 'watch'

function PlayerActions({ s, p, onOpenRecord }: { s: BotServer; p: BotPlayer; onOpenRecord?: (ucid: string) => void }) {
  const runner = useRunner()
  const [open, setOpen] = useState<Open>(null)
  const [text, setText] = useState('')
  const [days, setDays] = useState('')
  const [mode, setMode] = useState('popup')
  const [shot, setShot] = useState<string | null>(null)
  const [shooting, setShooting] = useState(false)
  const has = {
    kick: !!useAction('kick_player'), ban: !!useAction('ban_player'), mute: !!useAction('mute_player'),
    unmute: !!useAction('unmute_player'), message: !!useAction('message_player'), watch: !!useAction('watch_player'),
    unwatch: !!useAction('unwatch_player'), shot: !!useAction('screenshot_player'),
  }
  if (!p.ucid) return null
  const ucid = p.ucid
  const go = (name: string, extra: Record<string, unknown> = {}, withServer = true) => {
    setOpen(null)
    setText('')
    void runner.act(name, { ...(withServer ? { server_name: s.name } : {}), ucid, ...extra }, `${s.name}/${ucid}`)
  }
  const toggle = (o: Open) => { setOpen(cur => cur === o ? null : o); setText('') }
  // The bot waits for DCS to write the picture, then says where it is.
  const screenshot = async () => {
    setShooting(true)
    try {
      const r = await runner.act('screenshot_player', { server_name: s.name, ucid }, `${s.name}/${ucid}`)
      const key = r?.ok ? r.data.screenshot : null
      if (typeof key === 'string' && key && !api.bot.files()) {
        runner.say(true, `Screenshot of ${p.name} taken -- the web dashboard's SERVER BOT page shows it (${key})`)
        return
      }
      setShot(typeof key === 'string' && key ? key : null)
    } finally {
      setShooting(false)
    }
  }
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 6 }}>
      <div className="inline-flex items-center gap-1" style={{ whiteSpace: 'nowrap' }}>
        {has.message && <Btn onClick={() => toggle('message')} active={open === 'message'} title="Message"><Comms size={10} /></Btn>}
        {p.muted ? (has.unmute && <Btn onClick={() => go('unmute_player')}>Unmute</Btn>) : (has.mute && <Btn onClick={() => go('mute_player')}>Mute</Btn>)}
        {has.shot && <Btn disabled={shooting} onClick={() => void screenshot()} title="Screenshot (if the server allows it)"><Image size={10} />{shooting ? '…' : ''}</Btn>}
        {p.watchlist
          ? has.unwatch && <Btn onClick={() => go('unwatch_player', {}, false)} title="Take off the watch list"><EyeOff size={10} /></Btn>
          : has.watch && <Btn onClick={() => toggle('watch')} active={open === 'watch'} title="Watch list"><Eye size={10} /></Btn>}
        {has.kick && <Btn danger onClick={() => toggle('kick')} active={open === 'kick'}><UserX size={10} />Kick</Btn>}
        {has.ban && <Btn danger onClick={() => toggle('ban')} active={open === 'ban'}><Ban size={10} />Ban</Btn>}
        {onOpenRecord && <Btn onClick={() => onOpenRecord(ucid)} title="Record"><Pilot size={10} /></Btn>}
      </div>
      {shot && (
        <div style={{ position: 'relative', alignSelf: 'flex-end' }}>
          <a href={api.bot.screenshotUrl(s.name, shot)} target="_blank" rel="noreferrer" title="Open full size">
            <img src={api.bot.screenshotUrl(s.name, shot)} alt={`${p.name}'s screen`} style={{ maxWidth: 420, maxHeight: 240, border: '1px solid var(--border)', borderRadius: 3, display: 'block' }} />
          </a>
          <button type="button" onClick={() => setShot(null)} title="Close" style={{ position: 'absolute', top: 4, right: 4, background: 'rgba(0,0,0,0.6)', border: 0, color: '#fff', cursor: 'pointer', borderRadius: 2 }}><X size={10} /></button>
        </div>
      )}
      {open && (
        <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
          <input value={text} onChange={e => setText(e.target.value)} style={{ ...INPUT, flex: '1 1 200px' }}
            maxLength={open === 'ban' ? 80 : open === 'message' ? 1024 : 200}
            placeholder={open === 'message' ? 'Message' : open === 'watch' ? 'Why watch them' : 'Reason'} />
          {open === 'ban' && <input value={days} onChange={e => setDays(e.target.value)} placeholder="days (blank = forever)" style={{ ...INPUT, width: 140 }} />}
          {open === 'message' && (
            <select value={mode} onChange={e => setMode(e.target.value)} style={INPUT}>
              <option value="popup">popup</option><option value="chat">chat</option>
            </select>
          )}
          {open === 'kick' && <ConfirmBtn label="Kick" confirm={`Kick ${p.name}?`} onConfirm={() => go('kick_player', { reason: text })} />}
          {open === 'ban' && <ConfirmBtn label="Ban" confirm={`Ban ${p.name}${days ? ` for ${days} days` : ' for good'}?`}
            onConfirm={() => go('ban_player', { reason: text, ...(days.trim() ? { days: days.trim() } : {}) })} />}
          {open === 'message' && <Btn disabled={!text.trim()} onClick={() => go('message_player', { message: text.trim(), mode })}>Send</Btn>}
          {open === 'watch' && <Btn disabled={!text.trim()} onClick={() => go('watch_player', { reason: text.trim() }, false)}>Watch</Btn>}
          <Btn onClick={() => setOpen(null)}><X size={10} /></Btn>
        </div>
      )}
    </div>
  )
}

/** Everyone on these servers, with the per-player controls. */
export function PlayerRows({ servers, onOpenRecord, filter = '' }: { servers: BotServer[]; onOpenRecord?: (ucid: string) => void; filter?: string }) {
  const needle = filter.trim().toLowerCase()
  const rows = servers.flatMap(s => s.players.map(p => ({ s, p })))
    .filter(({ p }) => !needle || (p.name ?? '').toLowerCase().includes(needle) || (p.ucid ?? '').includes(needle))
  if (!rows.length) return <div style={{ padding: '0.75rem 1rem' }}><Empty>{needle ? 'Nobody matches.' : 'Nobody is on.'}</Empty></div>
  const many = servers.length > 1
  return (
    <div style={{ overflowX: 'auto' }}>
      <table style={{ width: '100%', borderCollapse: 'collapse' }}>
        <thead><tr>{['Player', 'Side', 'Unit', ...(many ? ['Server'] : []), ''].map(h => <th key={h} style={TH}>{h}</th>)}</tr></thead>
        <tbody>
          {rows.map(({ s, p }) => (
            <tr key={`${s.name}-${p.ucid}-${p.id}`}>
              <td style={CELL}>
                <span style={{ color: 'var(--text)' }}>{p.name}</span>
                {p.discord && <span style={{ color: 'var(--text-dim)' }}> · {p.discord}</span>}
                <div className="flex gap-1" style={{ marginTop: 2 }}>
                  {p.muted && <Pill tone="warn">muted</Pill>}
                  {p.watchlist && <Pill tone="warn">watched</Pill>}
                  {p.vip && <Pill>vip</Pill>}
                  {!p.verified && p.discord === null && <Pill title="No Discord link">unlinked</Pill>}
                </div>
                <div style={{ fontSize: '0.56rem', color: 'var(--text-dim)', ...MONO }}>{p.ucid}</div>
              </td>
              <td style={CELL}>{p.side ? <Pill tone={p.side === 'BLUE' ? 'busy' : p.side === 'RED' ? 'down' : 'dim'}>{p.side}</Pill> : '—'}</td>
              <td style={CELL}>{p.unit_type && p.side !== 'SPECTATOR' ? <>{p.unit_type}{p.group_name && <div style={{ fontSize: '0.58rem', color: 'var(--text-dim)' }}>{p.group_name}</div>}</> : '—'}</td>
              {many && <td style={CELL}>{s.display_name || s.name}</td>}
              <td style={{ ...CELL, textAlign: 'right' }}><PlayerActions s={s} p={p} onOpenRecord={onOpenRecord} /></td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

// ── the directory ────────────────────────────────────────────────────────────

function Directory({ onOpen }: { onOpen: (ucid: string) => void }) {
  const [q, setQ] = useState('')
  const [applied, setApplied] = useState('')
  const [state, setState] = useState('all')
  const [page, setPage] = useState(1)
  const dir = useRead<{ rows?: Record<string, unknown>[]; total?: number; pages?: number; page?: number }>(
    'list_player_directory', { q: applied, state, page })
  if (!useAction('list_player_directory')) return null
  const rows = dir.data?.data.rows ?? []
  const t = mainTable({ rows })
  const ucidOf = (r: Record<string, unknown>): string | null => {
    if (typeof r.ucid === 'string') return r.ucid
    if (Array.isArray(r.ucids) && typeof r.ucids[0] === 'string') return r.ucids[0]
    return null
  }
  return (
    <Card icon={<Search size={13} style={{ color: 'var(--accent)' }} />} title={`Player directory${dir.data?.data.total !== undefined ? ` · ${dir.data.data.total}` : ''}`}
      right={<>
        <input value={q} onChange={e => setQ(e.target.value)} onKeyDown={e => { if (e.key === 'Enter') { setApplied(q); setPage(1) } }}
          placeholder="name or UCID" style={{ ...INPUT, width: 180 }} />
        <select value={state} onChange={e => { setState(e.target.value); setPage(1) }} style={INPUT}>
          <option value="all">all</option><option value="banned">banned</option><option value="watched">watched</option>
        </select>
        <Btn onClick={() => { setApplied(q); setPage(1) }}><Search size={10} /></Btn>
      </>} pad={false}>
      {dir.data && !dir.data.ok && <div style={{ padding: '0.75rem 1rem' }}><Empty>{dir.data.message}</Empty></div>}
      {t && <Table columns={t.columns.filter(c => !['ucids', 'names'].includes(c)).slice(0, 7)} rows={t.rows}
        render={(c, v) => c === 'playtime' && typeof v === 'number' ? duration(v) : cell(v)}
        actions={r => { const u = ucidOf(r); return u ? <Btn onClick={() => onOpen(u)}>Open</Btn> : null }} />}
      {dir.data?.ok && rows.length === 0 && <div style={{ padding: '0.75rem 1rem' }}><Empty>No players.</Empty></div>}
      {(dir.data?.data.pages ?? 1) > 1 && (
        <div className="flex items-center gap-2" style={{ padding: '0.5rem 1rem' }}>
          <Btn disabled={page <= 1} onClick={() => setPage(p => p - 1)}>Prev</Btn>
          <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)' }}>page {dir.data?.data.page ?? page} of {dir.data?.data.pages}</span>
          <Btn disabled={page >= (dir.data?.data.pages ?? 1)} onClick={() => setPage(p => p + 1)}>Next</Btn>
        </div>
      )}
    </Card>
  )
}

// ── one player's record ─────────────────────────────────────────────────────

function PlayerRecord({ ucid, onClose }: { ucid: string; onClose: () => void }) {
  const [period, setPeriod] = useState('all')
  const detail = useRead('get_player_detail', { ucid })
  const stats = useRead('get_player_statistics', { ucid, period })
  const inf = useRead('get_player_infractions', { ucid })
  const actions = ['ban_ucid', 'watch_player', 'edit_watch', 'unwatch_player', 'punish_player', 'forgive_player']
  const [which, setWhich] = useState<string | null>(null)
  const { data: cat } = useCatalog()
  const avail = actions.map(n => cat?.actions.find(a => a.name === n && a.available && a.allowed))
  const pick = avail.find(a => a?.name === which)
  const d = detail.data?.data as Record<string, unknown> | undefined
  const top = useRef<HTMLDivElement>(null)
  useEffect(() => { top.current?.scrollIntoView({ block: 'start', behavior: 'smooth' }) }, [])
  return (
    <div ref={top}>
    <Card icon={<Pilot size={13} style={{ color: 'var(--accent)' }} />} title={`Player · ${String(d?.name || ucid)}`}
      right={<Btn onClick={onClose}><X size={10} />Close</Btn>}>
      <div style={{ display: 'flex', flexDirection: 'column', gap: 14 }}>
        {detail.data && <ResultView result={detail.data} />}
        <div>
          <div className="flex items-center gap-2" style={{ marginBottom: 6 }}>
            <span style={DIM}>Statistics</span>
            {['all', 'month', 'quarter', 'year'].map(p => <Btn key={p} active={period === p} onClick={() => setPeriod(p)}>{p}</Btn>)}
          </div>
          {stats.data && <ResultView result={stats.data} />}
        </div>
        {inf.data && (
          <div>
            <div style={{ ...DIM, marginBottom: 6 }}>Infractions</div>
            <ResultView result={inf.data} />
          </div>
        )}
        <div>
          <div className="flex items-center gap-2" style={{ marginBottom: 6, flexWrap: 'wrap' }}>
            <span style={DIM}>Act</span>
            {avail.map(a => a && <Btn key={a.name} active={which === a.name} danger={a.name === 'ban_ucid'} onClick={() => setWhich(w => w === a.name ? null : a.name)}>
              {a.name.replace(/_/g, ' ')}
            </Btn>)}
          </div>
          {pick && <ActionForm key={pick.name} action={pick} fixed={{ ucid }} />}
        </div>
      </div>
    </Card>
    </div>
  )
}

export default function PlayersTab({ fleet }: { fleet: BotFleet }) {
  const [record, setRecord] = useState<string | null>(null)
  const [filter, setFilter] = useState('')
  const online = fleet.servers.reduce((n, s) => n + s.players.length, 0)
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>
      {record && <PlayerRecord key={record} ucid={record} onClose={() => setRecord(null)} />}
      <Card icon={<Pilot size={13} style={{ color: 'var(--accent)' }} />} title={`Online · ${online}`} pad={false}
        right={<input value={filter} onChange={e => setFilter(e.target.value)} placeholder="filter" style={{ ...INPUT, width: 140 }} />}>
        <PlayerRows servers={fleet.servers} onOpenRecord={setRecord} filter={filter} />
      </Card>
      <Directory onOpen={setRecord} />
    </div>
  )
}
