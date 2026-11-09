import { useState } from 'react'
import { useSearchParams } from 'react-router-dom'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { Link } from 'react-router-dom'
import { api, type Squadron } from '../api'
import { useAuth } from '../context/AuthContext'
import QueryState from '../components/QueryState'
import PageHeader from '../components/PageHeader'
import { useDashLook } from '../lib/look'
import { Card, Tape } from '../components/Levant'
import { Pilot as PilotIcon } from '@icons'
import './levant/rounds.css'

/** A date bfdb sent, or nothing when it sent none it can read. */
function day(iso: string | undefined): string | null {
  if (!iso) return null
  const t = Date.parse(iso)
  return Number.isNaN(t) ? null : new Date(t).toLocaleDateString()
}

function Detail({ tag, myUcid, mine }: { tag: string; myUcid?: string; mine: boolean }) {
  const look = useDashLook()
  const qc = useQueryClient()
  const q = useQuery({ queryKey: ['squadron', tag], queryFn: () => api.squadron(tag) })
  const kick = useMutation({
    mutationFn: (ucid: string) => api.kickFromSquadron(ucid),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['squadron', tag] })
      qc.invalidateQueries({ queryKey: ['squadrons'] })
    },
  })
  if (!q.data) return <QueryState isLoading={q.isLoading} isError={q.isError} error={q.error} what="squadron" />
  const s = q.data
  const owner = mine && myUcid === s.owner
  if (look.profile) {
    return (
      <div className="sq-members-wrap">
        <table className="sq-members">
          <thead>
            <tr>
              <th className="l">Pilot</th>
              <th>XP for the squadron</th>
              <th>Joined</th>
              {owner && <th><span className="sr-only">Actions</span></th>}
            </tr>
          </thead>
          <tbody>
            {(s.members ?? []).map(m => (
              <tr key={m.ucid}>
                <td className="l who">
                  <span>
                    <Link to={`/pilots?ucid=${m.ucid}`}>{m.name || m.ucid}</Link>
                    {m.ucid === s.owner && <Tape tone="n" className="sm">Owner</Tape>}
                  </span>
                </td>
                <td>{m.xp.toLocaleString()}</td>
                <td>{new Date(m.joined).toLocaleDateString()}</td>
                {owner && (
                  <td className="act">
                    {m.ucid !== s.owner && (
                      <button type="button" className="ds-btn ghost sq-btn sm" onClick={() => kick.mutate(m.ucid)}>Remove</button>
                    )}
                  </td>
                )}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    )
  }
  return (
    <table className="data-table" style={{ width: '100%', marginTop: 8 }}>
      <thead>
        <tr>
          <th style={{ textAlign: 'left' }}>Pilot</th>
          <th>XP for the squadron</th>
          <th>Joined</th>
          {owner && <th><span className="sr-only">Actions</span></th>}
        </tr>
      </thead>
      <tbody>
        {(s.members ?? []).map(m => (
          <tr key={m.ucid}>
            <td style={{ textAlign: 'left' }}>
              <Link to={`/pilots?ucid=${m.ucid}`}>{m.name || m.ucid}</Link>
              {m.ucid === s.owner ? ' · owner' : ''}
            </td>
            <td>{m.xp.toLocaleString()}</td>
            <td>{new Date(m.joined).toLocaleDateString()}</td>
            {owner && (
              <td>
                {m.ucid !== s.owner && (
                  <button type="button" className="vs-btn" onClick={() => kick.mutate(m.ucid)}>Remove</button>
                )}
              </td>
            )}
          </tr>
        ))}
      </tbody>
    </table>
  )
}

/** Squadrons: tag, name, members, and the XP members earned while in them. */
export default function SquadronsPage() {
  const { user } = useAuth()
  const qc = useQueryClient()
  const list = useQuery({ queryKey: ['squadrons'], queryFn: api.squadrons })
  const me = useQuery({
    queryKey: ['pilot-progression', user?.ucid],
    queryFn: () => api.pilotProgression(user!.ucid!),
    enabled: !!user?.ucid,
    retry: false,
  })
  const myTag = me.data?.squadron ?? null
  // ?tag= (from the Rankings squadron table) opens that squadron.
  const [params] = useSearchParams()
  const [open, setOpen] = useState<string | null>(() => params.get('tag'))
  const [tag, setTag] = useState('')
  const [name, setName] = useState('')
  const [error, setError] = useState<string | null>(null)
  const refresh = () => {
    qc.invalidateQueries({ queryKey: ['squadrons'] })
    qc.invalidateQueries({ queryKey: ['pilot-progression', user?.ucid] })
    if (open) qc.invalidateQueries({ queryKey: ['squadron', open] })
  }
  const run = (p: Promise<unknown>) => {
    setError(null)
    p.then(refresh).catch(e => setError(e instanceof Error ? e.message : String(e)))
  }
  const squadrons: Squadron[] = list.data ?? []
  const look = useDashLook()

  if (look.profile) {
    const mySquadron = myTag ? squadrons.find(s => s.tag === myTag) : undefined
    const top = Math.max(1, ...squadrons.map(s => s.xp))
    return (
      <div className="flex flex-col flex-1 overflow-hidden">
        <PageHeader art="flanker" title="SQUADRONS" sub="Ranked by XP members earned while flying for them" icon={PilotIcon} />
        <div className="flex-1 overflow-auto sq-page">
          <div className="sq-wrap">
            {user?.ucid ? (
              myTag ? (
                <Card tape="Your squadron" tone="rust" className="sq-me" bodyClass="sq-me-body">
                  <div className="sq-me-id">
                    <Tape tone="rust" className="sm">{myTag}</Tape>
                    <span className="sq-me-name">{mySquadron?.name ?? `[${myTag}]`}</span>
                  </div>
                  <button type="button" className="ds-btn ghost sq-btn" onClick={() => run(api.leaveSquadron())}>Leave</button>
                  {error && <span className="sq-err">{error}</span>}
                </Card>
              ) : (
                <Card tape="Found a squadron" tone="rust" className="sq-me" bodyClass="sq-me-body">
                  <div className="sq-form">
                    <input className="vs-input sq-tag" placeholder="TAG" maxLength={5} value={tag} onChange={e => setTag(e.target.value)}
                      aria-label="Squadron tag" title="2 to 5 letters (A-Z) or digits; № counts as NO" />
                    <input className="vs-input sq-name-in" placeholder="Squadron name" maxLength={40} value={name} onChange={e => setName(e.target.value)}
                      aria-label="Squadron name" />
                    <button type="button" className="ds-btn sq-btn" disabled={!tag || !name} onClick={() => run(api.createSquadron(tag, name))}>Create</button>
                    <span className="sq-or">or join one below</span>
                  </div>
                  <span className="sq-hint">The tag is 2 to 5 letters (A-Z) or digits, no spaces or symbols. № counts as NO.</span>
                  {error && <span className="sq-err">{error}</span>}
                </Card>
              )
            ) : (
              <Card tape="Fly for a squadron" className="sq-me" bodyClass="sq-me-body">
                <p>Sign in with Discord and link your game account (<code>-linkme</code> in DCS chat) to found or join a squadron.</p>
              </Card>
            )}

            <QueryState isLoading={list.isLoading} isError={list.isError} error={list.error} isEmpty={squadrons.length === 0} what="squadrons" emptyText="No squadrons yet -- found the first one." />
            {squadrons.length > 0 && (
              <Card tape={<>Standings · {squadrons.length}</>}>
                <ol className="sq-list">
                  {squadrons.map((s, i) => {
                    const mine = myTag === s.tag
                    const since = day(s.created)
                    return (
                      <li key={s.tag} className={`sq-row${i === 0 ? ' first' : ''}${mine ? ' mine' : ''}`}>
                        <div className="sq-line">
                          <span className="sq-rk">{i + 1}</span>
                          <div className="sq-id">
                            <div className="sq-id-top">
                              <Tape tone={mine ? 'rust' : undefined} className="sm">{s.tag}</Tape>
                              {mine && <span className="sq-yours">Your squadron</span>}
                            </div>
                            <strong className="sq-name" title={s.name}>{s.name}</strong>
                            <span className="sq-bar"><i style={{ width: `${(s.xp / top) * 100}%` }} /></span>
                            {(s.owner_name || since) && (
                              <span className="lv-sub">
                                {s.owner_name ? `Led by ${s.owner_name}` : ''}{s.owner_name && since ? ' · ' : ''}{since ? `since ${since}` : ''}
                              </span>
                            )}
                          </div>
                          <dl className="sq-stats">
                            <div><dt>XP</dt><dd>{s.xp.toLocaleString()}</dd></div>
                            <div><dt>Pilots</dt><dd>{s.member_count}</dd></div>
                          </dl>
                          <div className="sq-acts">
                            {user?.ucid && !myTag && (
                              <button type="button" className="ds-btn sq-btn" onClick={() => run(api.joinSquadron(s.tag))}>Join</button>
                            )}
                            <button type="button" className="ds-btn ghost sq-btn" aria-expanded={open === s.tag}
                              onClick={() => setOpen(open === s.tag ? null : s.tag)}>
                              {open === s.tag ? 'Hide' : 'Members'}
                            </button>
                          </div>
                        </div>
                        {open === s.tag && (
                          <div className="sq-detail">
                            <Detail tag={s.tag} myUcid={user?.ucid ?? undefined} mine={mine} />
                          </div>
                        )}
                      </li>
                    )
                  })}
                </ol>
              </Card>
            )}
          </div>
        </div>
      </div>
    )
  }

  return (
    <div className="page">
      <PageHeader art="flanker" title="SQUADRONS" sub="Ranked by XP members earned while flying for them" icon={PilotIcon} />
      {user?.ucid ? (
        <div className="card" style={{ padding: 12, marginBottom: 12, display: 'flex', flexWrap: 'wrap', gap: 8, alignItems: 'center' }}>
          {myTag ? (
            <>
              <span>You fly for <strong>[{myTag}]</strong>.</span>
              <button type="button" className="vs-btn" onClick={() => run(api.leaveSquadron())}>Leave</button>
            </>
          ) : (
            <>
              <span>Found a squadron:</span>
              <input className="vs-input" placeholder="TAG" maxLength={5} value={tag} onChange={e => setTag(e.target.value)} style={{ width: 80 }}
                title="2 to 5 letters (A-Z) or digits; № counts as NO" />
              <input className="vs-input" placeholder="Squadron name" maxLength={40} value={name} onChange={e => setName(e.target.value)} />
              <button type="button" className="vs-btn" disabled={!tag || !name} onClick={() => run(api.createSquadron(tag, name))}>Create</button>
              <span style={{ color: 'var(--text-dim)' }}>or join one below.</span>
              <span style={{ color: 'var(--text-dim)', fontSize: '0.7rem', width: '100%' }}>
                The tag is 2 to 5 letters (A-Z) or digits, no spaces or symbols. № counts as NO.
              </span>
            </>
          )}
          {error && <span style={{ color: '#f87171', width: '100%' }}>{error}</span>}
        </div>
      ) : (
        <div className="card" style={{ padding: 12, marginBottom: 12 }}>
          Sign in with Discord and link your game account (<code>-linkme</code> in DCS chat) to found or join a squadron.
        </div>
      )}
      <QueryState isLoading={list.isLoading} isError={list.isError} error={list.error} isEmpty={squadrons.length === 0} what="squadrons" emptyText="No squadrons yet -- found the first one." />
      <div style={{ display: 'grid', gap: 8 }}>
        {squadrons.map((s, i) => (
          <div key={s.tag} className="card">
            <div style={{ display: 'flex', flexWrap: 'wrap', gap: 16, alignItems: 'center', padding: '10px 12px' }}>
              <strong>#{i + 1}</strong>
              <strong>[{s.tag}]</strong>
              <span>{s.name}</span>
              <span style={{ color: 'var(--text-dim)' }}>{s.member_count} pilots</span>
              <span>{s.xp.toLocaleString()} XP</span>
              <span style={{ marginLeft: 'auto', display: 'flex', gap: 8 }}>
                {user?.ucid && !myTag && (
                  <button type="button" className="vs-btn" onClick={() => run(api.joinSquadron(s.tag))}>Join</button>
                )}
                <button type="button" className="vs-btn" onClick={() => setOpen(open === s.tag ? null : s.tag)}>
                  {open === s.tag ? 'Hide' : 'Members'}
                </button>
              </span>
            </div>
            {open === s.tag && (
              <div style={{ padding: '0 12px 12px' }}>
                <Detail tag={s.tag} myUcid={user?.ucid ?? undefined} mine={myTag === s.tag} />
              </div>
            )}
          </div>
        ))}
      </div>
    </div>
  )
}
