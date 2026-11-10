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
    <div style={{ overflowX: 'auto' }}>
    <table className="vs-table" style={{ width: '100%', borderCollapse: 'collapse', fontSize: '0.72rem' }}>
      <thead>
        <tr>
          <th style={{ textAlign: 'left' }}>Pilot</th>
          <th style={{ textAlign: 'right' }}>XP for the squadron</th>
          <th style={{ textAlign: 'right' }}>Joined</th>
          {owner && <th><span className="sr-only">Actions</span></th>}
        </tr>
      </thead>
      <tbody>
        {(s.members ?? []).map(m => (
          <tr key={m.ucid}>
            <td style={{ textAlign: 'left' }}>
              <Link to={`/pilots?ucid=${m.ucid}`} style={{ color: 'var(--text)', textDecoration: 'none', fontWeight: 600 }}>{m.name || m.ucid}</Link>
              {m.ucid === s.owner && <span className="stamp stamp--sm stamp--blue" style={{ marginLeft: 8 }}>CO</span>}
            </td>
            <td style={{ textAlign: 'right' }}><span className="rsr-fig" style={{ fontSize: '1.1rem' }}>{m.xp.toLocaleString()}</span></td>
            <td className="font-mono" style={{ textAlign: 'right', color: 'var(--text-muted)' }}>{new Date(m.joined).toLocaleDateString()}</td>
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
    </div>
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
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader art="flanker" title="SQUADRON ROLL" file="Squadron roll"
        stamp={<span className="stamp stamp--blue">On strength</span>}
        sub="Squadrons ranked by the XP their members earned flying for them" icon={PilotIcon} />
      <div className="vs-page" style={{ gap: 12 }}>
      {user?.ucid ? (
        <div className="vs-card" style={{ padding: '12px 16px', display: 'flex', flexWrap: 'wrap', gap: 10, alignItems: 'center', fontSize: '0.75rem' }}>
          {myTag ? (
            <>
              <span>You fly for <strong style={{ color: 'var(--accent)' }}>[{myTag}]</strong>.</span>
              <button type="button" className="vs-btn vs-btn-outline" onClick={() => run(api.leaveSquadron())}>Leave</button>
            </>
          ) : (
            <>
              <span className="rsr-form-label">Found a squadron</span>
              <input className="vs-input" placeholder="TAG" maxLength={12} value={tag} onChange={e => setTag(e.target.value)} style={{ width: 80 }}
                title="2 to 5 letters (A-Z) or digits; brackets and symbols are left out" />
              <input className="vs-input" placeholder="Squadron name" maxLength={40} value={name} onChange={e => setName(e.target.value)} />
              <button type="button" className="vs-btn" disabled={!tag || !name} onClick={() => run(api.createSquadron(tag, name))}>Create</button>
              <span style={{ color: 'var(--text-dim)' }}>or join one below.</span>
              <span style={{ color: 'var(--text-dim)', fontSize: '0.7rem', width: '100%' }}>
                The tag is 2 to 5 letters (A-Z) or digits. Brackets and symbols are left out, so [.ID] becomes ID; your DCS name keeps them.
              </span>
            </>
          )}
          {error && <span style={{ color: 'var(--red)', width: '100%' }}>{error}</span>}
        </div>
      ) : (
        <div className="vs-card" style={{ padding: '12px 16px', fontSize: '0.75rem', color: 'var(--text-muted)' }}>
          Sign in with Discord and link your game account (<code>-linkme</code> in DCS chat) to found or join a squadron.
        </div>
      )}
      <QueryState isLoading={list.isLoading} isError={list.isError} error={list.error} isEmpty={squadrons.length === 0} what="squadrons" emptyText="No squadrons yet -- found the first one." />
      <div style={{ display: 'grid', gap: 8 }}>
        {squadrons.map((s, i) => (
          <div key={s.tag} className="vs-card" style={{ overflow: 'hidden', borderLeft: myTag === s.tag ? '3px solid var(--accent)' : undefined }}>
            <div style={{ display: 'flex', flexWrap: 'wrap', gap: '6px 16px', alignItems: 'center', padding: '10px 16px' }}>
              <span className="font-mono" style={{ fontSize: '0.7rem', color: 'var(--text-dim)', minWidth: 22 }}>{i + 1}</span>
              <span style={{ fontFamily: 'var(--font-display)', fontSize: '1.15rem', letterSpacing: '0.1em', color: 'var(--accent)' }}>[{s.tag}]</span>
              <span style={{ fontSize: '0.8rem', fontWeight: 600, color: 'var(--text)', minWidth: 0, overflowWrap: 'anywhere' }}>{s.name}</span>
              <span className="font-mono" style={{ fontSize: '0.66rem', color: 'var(--text-muted)' }}>{s.member_count} pilots</span>
              <span><span className="rsr-fig" style={{ fontSize: '1.35rem' }}>{s.xp.toLocaleString()}</span> <span className="rsr-form-label">XP</span></span>
              <span style={{ marginLeft: 'auto', display: 'flex', gap: 8 }}>
                {user?.ucid && !myTag && (
                  <button type="button" className="vs-btn" onClick={() => run(api.joinSquadron(s.tag))}>Join</button>
                )}
                <button type="button" className="vs-btn vs-btn-outline" aria-expanded={open === s.tag} onClick={() => setOpen(open === s.tag ? null : s.tag)}>
                  {open === s.tag ? 'Hide' : 'Members'}
                </button>
              </span>
            </div>
            {open === s.tag && (
              <div style={{ borderTop: '1px solid var(--border)' }}>
                <Detail tag={s.tag} myUcid={user?.ucid ?? undefined} mine={myTag === s.tag} />
              </div>
            )}
          </div>
        ))}
      </div>
      </div>
    </div>
  )
}
