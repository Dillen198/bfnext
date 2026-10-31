import { useState } from 'react'
import { useSearchParams } from 'react-router-dom'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { Link } from 'react-router-dom'
import { api, type Squadron } from '../api'
import { useAuth } from '../context/AuthContext'
import QueryState from '../components/QueryState'
import PageHeader from '../components/PageHeader'
import { Pilot as PilotIcon } from '@icons'

function Detail({ tag, myUcid, mine }: { tag: string; myUcid?: string; mine: boolean }) {
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
