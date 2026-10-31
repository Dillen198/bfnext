// Admin: who commands on this server. Rank earns command (the engine
// config's `command.commander_rank`); an admin can grant it to anyone or take
// it from anyone. A change applies on every server straight away, and the
// Discord bot moves the Commander role on its next pass.
import { useMemo, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api, type CommanderStatus } from '../api'
import { rankFor } from '../ranks'
import { Shield } from '@icons'

const DIM: React.CSSProperties = {
  fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase',
}
const BTN: React.CSSProperties = {
  fontSize: '0.6rem', background: 'none', border: '1px solid var(--border)', padding: '2px 8px',
  borderRadius: 3, cursor: 'pointer', letterSpacing: '0.06em',
}
const SIDE_COL = { Blue: '#4a8fd4', Red: '#cc4444' } as const

type Filter = 'commanders' | 'all'

export default function CommandersPanel() {
  const qc = useQueryClient()
  const { data, isError, error } = useQuery({
    queryKey: ['admin', 'commanders'],
    queryFn: api.command.roster,
    refetchInterval: 60_000,
    retry: false,
  })
  const [filter, setFilter] = useState<Filter>('commanders')
  const [q, setQ] = useState('')
  const [busy, setBusy] = useState<string | null>(null)
  const [msg, setMsg] = useState<{ ok: boolean; text: string } | null>(null)

  const rows = useMemo(() => {
    const needle = q.trim().toLowerCase()
    return (data?.pilots ?? [])
      .filter((p) => filter === 'all' || p.commander || p.grant != null)
      .filter((p) => !needle || p.name.toLowerCase().includes(needle) || p.ucid.includes(needle))
      .slice(0, 200)
  }, [data, filter, q])

  const set = async (p: CommanderStatus, grant: 'granted' | 'revoked' | null) => {
    setBusy(p.ucid)
    setMsg(null)
    try {
      await api.command.setGrant(p.ucid, grant)
      setMsg({
        ok: true,
        text: grant === 'granted'
          ? `${p.name} is a commander now.`
          : grant === 'revoked'
            ? `${p.name} can no longer command.`
            : `${p.name}'s access follows their rank again.`,
      })
      await qc.invalidateQueries({ queryKey: ['admin', 'commanders'] })
    } catch (e) {
      setMsg({ ok: false, text: (e as Error).message })
    } finally {
      setBusy(null)
    }
  }

  const count = (data?.pilots ?? []).filter((p) => p.commander)
  const blue = count.filter((p) => p.side === 'Blue').length
  const red = count.filter((p) => p.side === 'Red').length

  return (
    <div className="vs-card" style={{ marginBottom: 16 }}>
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Shield size={13} style={{ color: 'var(--accent)' }} />
        <span style={{ ...DIM, fontSize: '0.65rem' }}>Commanders</span>
        {data && (
          <span className="ml-auto font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--text-muted)' }}>
            <span style={{ color: SIDE_COL.Blue }}>{blue} BLUE</span> · <span style={{ color: SIDE_COL.Red }}>{red} RED</span>
          </span>
        )}
      </div>
      <div style={{ padding: '10px 16px 14px' }}>
        {isError ? (
          <div style={{ fontSize: '0.68rem', color: '#ef4444' }}>
            Commander roster unavailable: {(error as Error)?.message ?? 'error'}. A bfdb older than commander access has no roster.
          </div>
        ) : !data ? (
          <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)' }}>Loading…</div>
        ) : (
          <>
            <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)', marginBottom: 10, lineHeight: 1.6 }}>
              {data.require_commander
                ? <>Orders (ground formations, the HQ, the command map) need a commander. Command unlocks at <b style={{ color: 'var(--text)' }}>{rankFor(data.commander_score, 'Blue').title}</b> (campaign score {data.commander_score}), set by <code>command.commander_rank</code> in the engine config.</>
                : <>This server lets any pilot on a side order the ground war (<code>command.require_commander</code> is off); commanders still steer the HQ.</>}
              {' '}A grant or revoke beats rank, on every server. Dashboard admins always command.
            </div>
            <div className="flex items-center gap-2" style={{ marginBottom: 8 }}>
              {(['commanders', 'all'] as const).map((f) => (
                <button key={f} onClick={() => setFilter(f)}
                  style={{ ...BTN, color: filter === f ? 'var(--accent)' : 'var(--text-dim)', borderColor: filter === f ? 'var(--accent)' : 'var(--border)' }}>
                  {f === 'commanders' ? 'COMMANDERS + OVERRIDES' : 'ALL PILOTS'}
                </button>
              ))}
              <input value={q} onChange={(e) => setQ(e.target.value)} placeholder="search name or ucid"
                style={{ marginLeft: 'auto', fontSize: '0.66rem', background: 'var(--bg-input, transparent)', border: '1px solid var(--border)', color: 'var(--text)', padding: '3px 8px', borderRadius: 3, minWidth: 180 }} />
            </div>
            {msg && <div style={{ fontSize: '0.65rem', marginBottom: 8, color: msg.ok ? 'var(--accent)' : '#ef4444' }}>{msg.text}</div>}
            <div style={{ overflowX: 'auto' }}>
              <table style={{ width: '100%', borderCollapse: 'collapse', fontSize: '0.66rem' }}>
                <thead>
                  <tr style={{ color: 'var(--text-dim)', textAlign: 'left' }}>
                    <th style={{ padding: '3px 8px 3px 0' }}>Pilot</th>
                    <th style={{ padding: '3px 8px' }}>Side</th>
                    <th style={{ padding: '3px 8px', textAlign: 'right' }}>Score</th>
                    <th style={{ padding: '3px 8px' }}>Rank</th>
                    <th style={{ padding: '3px 8px' }}>Command</th>
                    <th style={{ padding: '3px 0 3px 8px', textAlign: 'right' }}>Override</th>
                  </tr>
                </thead>
                <tbody>
                  {rows.length === 0 && (
                    <tr><td colSpan={6} style={{ padding: '8px 0', color: 'var(--text-dim)' }}>
                      {filter === 'commanders' ? 'Nobody commands yet. Show all pilots to grant access.' : 'No pilots match.'}
                    </td></tr>
                  )}
                  {rows.map((p) => {
                    const rank = rankFor(p.score, p.side)
                    const why = p.admin ? 'admin' : p.grant === 'granted' ? 'granted' : p.grant === 'revoked' ? 'revoked' : p.commander ? 'rank' : !p.side ? 'no side' : ''
                    return (
                      <tr key={p.ucid} style={{ borderTop: '1px solid var(--border)' }}>
                        <td style={{ padding: '4px 8px 4px 0', color: 'var(--text)' }} title={p.ucid}>{p.name}</td>
                        <td style={{ padding: '4px 8px', color: p.side ? SIDE_COL[p.side] : 'var(--text-dim)' }}>{p.side ?? '—'}</td>
                        <td className="font-mono-vs" style={{ padding: '4px 8px', textAlign: 'right', color: 'var(--text-muted)' }}>{Math.round(p.score)}</td>
                        <td style={{ padding: '4px 8px', color: 'var(--text-muted)' }}>{rank.title}</td>
                        <td style={{ padding: '4px 8px' }}>
                          <span style={{ color: p.commander ? 'var(--accent)' : 'var(--text-dim)', fontWeight: p.commander ? 600 : 400 }}>
                            {p.commander ? 'COMMANDER' : '—'}
                          </span>
                          {why && <span style={{ color: 'var(--text-dim)', marginLeft: 6 }}>({why}{p.grant_by ? ` by ${p.grant_by}` : ''})</span>}
                        </td>
                        <td style={{ padding: '4px 0 4px 8px', textAlign: 'right', whiteSpace: 'nowrap' }}>
                          {p.grant !== 'granted' && (
                            <button disabled={busy === p.ucid} onClick={() => set(p, 'granted')} style={{ ...BTN, color: 'var(--accent)' }}>GRANT</button>
                          )}
                          {p.grant !== 'revoked' && (
                            <button disabled={busy === p.ucid} onClick={() => set(p, 'revoked')} style={{ ...BTN, color: '#ef4444', marginLeft: 4 }}>REVOKE</button>
                          )}
                          {p.grant != null && (
                            <button disabled={busy === p.ucid} onClick={() => set(p, null)} style={{ ...BTN, color: 'var(--text-muted)', marginLeft: 4 }} title="Go back to what their rank says">BY RANK</button>
                          )}
                        </td>
                      </tr>
                    )
                  })}
                </tbody>
              </table>
            </div>
          </>
        )}
      </div>
    </div>
  )
}
