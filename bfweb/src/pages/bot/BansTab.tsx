// Bans: the bot's ban list -- lift, edit, and ban a UCID that isn't online.

import { useState } from 'react'
import { cell } from '../../lib/botconsole'
import { Btn, Card, ConfirmBtn, Empty, useAction, useRead, useRunner } from './ui'
import { CELL, DIM, INPUT, MONO, TH } from './style'
import { Ban, Pencil, RotateCw, Search, X } from '@icons'

interface BanRow {
  ucid?: string
  name?: string | null
  banned_by?: string | null
  reason?: string | null
  banned_at?: string | null
  banned_until?: string | null
  server_name?: string | null
  servers?: string[] | null
  [k: string]: unknown
}

function EditBan({ b, onDone }: { b: BanRow; onDone: () => void }) {
  const runner = useRunner()
  const [reason, setReason] = useState(b.reason ?? '')
  const [until, setUntil] = useState(b.banned_until && !b.banned_until.startsWith('9999') ? b.banned_until.slice(0, 10) : '')
  return (
    <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
      <input value={reason} maxLength={80} onChange={e => setReason(e.target.value)} style={{ ...INPUT, flex: '1 1 200px' }} />
      <input value={until} onChange={e => setUntil(e.target.value)} placeholder="until YYYY-MM-DD (blank = never)" style={{ ...INPUT, width: 200 }} />
      <Btn onClick={() => void runner.act('edit_ban', { ucid: b.ucid, banned_at: b.banned_at ?? '', reason, ...(until.trim() ? { until: until.trim() } : {}) }).then(onDone)}>Save</Btn>
      <Btn onClick={onDone}><X size={10} /></Btn>
    </div>
  )
}

function AddBan() {
  const runner = useRunner()
  const [q, setQ] = useState('')
  const [ucid, setUcid] = useState('')
  const [reason, setReason] = useState('')
  const [days, setDays] = useState('')
  const [search, setSearch] = useState('')
  const found = useRead<{ players?: { name: string; ucid: string }[] }>('search_players', { query: search }, { enabled: search.length >= 2 })
  if (!useAction('ban_ucid')) return null
  return (
    <Card icon={<Ban size={13} style={{ color: '#ef4444' }} />} title="Ban a player">
      <div style={{ display: 'flex', flexDirection: 'column', gap: 8 }}>
        <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
          <input value={q} onChange={e => setQ(e.target.value)} onKeyDown={e => { if (e.key === 'Enter') setSearch(q.trim()) }}
            placeholder="find by name" style={{ ...INPUT, width: 180 }} />
          <Btn onClick={() => setSearch(q.trim())}><Search size={10} /></Btn>
          {(found.data?.data.players ?? []).slice(0, 8).map(p => (
            <Btn key={p.ucid} active={ucid === p.ucid} onClick={() => setUcid(p.ucid)}>{p.name}</Btn>
          ))}
        </div>
        <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
          <input value={ucid} onChange={e => setUcid(e.target.value)} placeholder="UCID" style={{ ...INPUT, ...MONO, width: 280 }} />
          <input value={reason} maxLength={80} onChange={e => setReason(e.target.value)} placeholder="reason" style={{ ...INPUT, flex: '1 1 200px' }} />
          <input value={days} onChange={e => setDays(e.target.value)} placeholder="days (blank = forever)" style={{ ...INPUT, width: 150 }} />
          <ConfirmBtn label="Ban" disabled={!ucid.trim() || !reason.trim()} confirm={`Ban ${ucid.slice(0, 8)}…${days ? ` for ${days} days` : ' for good'}?`}
            onConfirm={() => void runner.act('ban_ucid', { ucid: ucid.trim(), reason: reason.trim(), ...(days.trim() ? { days: days.trim() } : {}) })
              .then(r => { if (r?.ok) { setUcid(''); setReason(''); setDays('') } })} />
        </div>
      </div>
    </Card>
  )
}

export default function BansTab() {
  const runner = useRunner()
  const list = useRead<{ bans?: BanRow[] }>('list_bans', {})
  const canLift = !!useAction('unban')
  const canEdit = !!useAction('edit_ban')
  const [editing, setEditing] = useState<string | null>(null)
  const [filter, setFilter] = useState('')
  if (!useAction('list_bans')) return <Empty>This bot has no ban list (needs DCSServerBot 3.0.5, and the moderation role).</Empty>
  const needle = filter.trim().toLowerCase()
  const bans = (list.data?.data.bans ?? []).filter(b => !needle ||
    [b.name, b.ucid, b.reason, b.banned_by].some(v => String(v ?? '').toLowerCase().includes(needle)))
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>
      <AddBan />
      <Card icon={<Ban size={13} style={{ color: 'var(--accent)' }} />} title={`Bans · ${bans.length}`} pad={false}
        right={<>
          <input value={filter} onChange={e => setFilter(e.target.value)} placeholder="filter" style={{ ...INPUT, width: 160 }} />
          <Btn onClick={() => void list.refetch()}><RotateCw size={10} /></Btn>
        </>}>
        {list.data && !list.data.ok && <div style={{ padding: '0.75rem 1rem' }}><Empty>{list.data.message}</Empty></div>}
        {list.data?.ok && bans.length === 0 && <div style={{ padding: '0.75rem 1rem' }}><Empty>No bans.</Empty></div>}
        {bans.length > 0 && (
          <div style={{ overflowX: 'auto' }}>
            <table style={{ width: '100%', borderCollapse: 'collapse' }}>
              <thead><tr>{['Player', 'Reason', 'By', 'Since', 'Until', 'Where', ''].map(h => <th key={h} style={TH}>{h}</th>)}</tr></thead>
              <tbody>
                {bans.map(b => {
                  const key = `${b.ucid}|${b.banned_at}`
                  return (
                    <tr key={key}>
                      <td style={CELL}>
                        <span style={{ color: 'var(--text)' }}>{b.name || '—'}</span>
                        <div style={{ fontSize: '0.56rem', color: 'var(--text-dim)', ...MONO }}>{b.ucid}</div>
                      </td>
                      <td style={CELL}>{editing === key ? <EditBan b={b} onDone={() => setEditing(null)} /> : cell(b.reason)}</td>
                      <td style={CELL}>{cell(b.banned_by)}</td>
                      <td style={{ ...CELL, ...MONO }}>{cell(b.banned_at)}</td>
                      <td style={{ ...CELL, ...MONO }}>{cell(b.banned_until)}</td>
                      <td style={CELL}>{cell(b.servers ?? b.server_name ?? 'everywhere')}</td>
                      <td style={{ ...CELL, whiteSpace: 'nowrap' }}>
                        <span className="inline-flex gap-1">
                          {canEdit && editing !== key && <Btn onClick={() => setEditing(key)} title="Edit"><Pencil size={10} /></Btn>}
                          {canLift && <ConfirmBtn label="Lift" confirm={`Unban ${b.name || b.ucid}?`}
                            onConfirm={() => void runner.act('unban', { ucid: b.ucid, banned_at: b.banned_at ?? '' })} />}
                        </span>
                      </td>
                    </tr>
                  )
                })}
              </tbody>
            </table>
          </div>
        )}
        <div style={{ ...DIM, padding: '0.5rem 1rem', textTransform: 'none', letterSpacing: 0 }}>
          The bot's own ban list: a ban here holds on every server the bot runs, and Discord's /ban sees the same rows.
        </div>
      </Card>
    </div>
  )
}
