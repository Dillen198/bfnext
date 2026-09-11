import { useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Download, RefreshCw } from 'lucide-react'
import { api, type Briefing, type SituationReport } from '../api'
import PageHeader from '../components/PageHeader'
import { useAuth } from '../context/AuthContext'
import KneeboardTab, { buildPdf } from './KneeboardTab'
import { mockBriefing, mockSituation } from './briefingMocks'
import SituationTab from './SituationTab'

type Side = 'Blue' | 'Red'
type Tab = 'situation' | 'kneeboard'

const MOCK = import.meta.env.DEV && new URLSearchParams(location.search).has('mock')

/**
 * The pre-flight briefing, coalition-locked.
 *
 * Two halves, because they go stale at completely different rates:
 *
 * * **Situation** — the engine's auto-generated read of right now: where the
 *   round stands, ranked tasking, hotspots, the air-defence areas *your* side
 *   has actually earned intel on, the air picture, logistics and the comms
 *   card, all laid over a map. Refreshes on a timer.
 * * **Kneeboard** — navaids, radios, artillery, deployables and HARM codes.
 *   Reference you print once and fly with.
 *
 * Both come from endpoints that resolve the session cookie to a coalition
 * server-side, so a browser never receives the other side's picture. Only a
 * dashboard admin with no in-game side of their own can switch sides.
 */
export default function BriefingPage() {
  const { user } = useAuth()
  const ownSide: Side | null = user?.side === 'Red' ? 'Red' : user?.side === 'Blue' ? 'Blue' : null
  const canSwitch = !!user?.is_admin && !ownSide
  const [adminSide, setAdminSide] = useState<Side>('Blue')
  const side: Side = canSwitch ? adminSide : (ownSide ?? 'Blue')
  const [tab, setTab] = useState<Tab>('situation')

  const {
    data: fetchedSituation,
    isLoading: sitLoading,
    error: sitError,
    isFetching: sitFetching,
    refetch: refetchSit,
  } = useQuery<SituationReport>({
    queryKey: ['situation', side],
    queryFn: () => api.situation(canSwitch ? side : undefined),
    // The tasking and the air picture move on a minute scale; anything faster
    // just costs the engine an RPC per viewer.
    refetchInterval: 45_000,
    enabled: !MOCK,
  })
  const situation = MOCK ? mockSituation(side) : fetchedSituation

  const { data: fetchedBrief, isLoading: briefLoading, error: briefError } = useQuery<Briefing>({
    queryKey: ['briefing', side],
    queryFn: () => api.briefing(canSwitch ? side : undefined),
    refetchInterval: 120_000,
    enabled: !MOCK,
  })
  const briefing = MOCK ? mockBriefing(side) : fetchedBrief

  const tabBtn = (id: Tab, label: string, badge?: string) => (
    <button
      key={id}
      onClick={() => setTab(id)}
      style={{
        padding: '5px 14px', fontSize: '0.68rem', fontWeight: 700, letterSpacing: '0.08em',
        border: 'none', cursor: 'pointer', textTransform: 'uppercase',
        background: tab === id ? 'var(--accent)' : 'var(--bg-elevated)',
        color: tab === id ? '#0b0f08' : 'var(--text-dim)',
      }}
    >
      {label}
      {badge && (
        <span style={{ marginLeft: 6, fontFamily: 'var(--font-mono)', fontWeight: 800, opacity: 0.85 }}>
          {badge}
        </span>
      )}
    </button>
  )

  const critical = situation?.tasking.filter(t => t.urgency === 'critical').length ?? 0

  return (
    <div style={{ display: 'flex', flexDirection: 'column', flex: 1, minWidth: 0, minHeight: 0, overflow: 'hidden' }}>
      <PageHeader
        title="BRIEFING"
        sub={
          tab === 'situation'
            ? situation
              ? `${situation.tasking.length} tasks · ${situation.hotspots.length} hotspots · ${situation.threats.length} known threats`
              : 'live situation'
            : briefing
              ? `${briefing.navaids.length} navaids · ${briefing.radios.length} stations · ${briefing.threats.length} threat types`
              : 'kneeboard data'
        }
        right={
          <div style={{ display: 'flex', gap: 8, alignItems: 'center' }}>
            <div style={{ display: 'flex', border: '1px solid var(--border)', borderRadius: 4, overflow: 'hidden' }}>
              {tabBtn('situation', 'Situation', critical > 0 ? `${critical}!` : undefined)}
              {tabBtn('kneeboard', 'Kneeboard')}
            </div>
            {canSwitch ? (
              <div style={{ display: 'flex', border: '1px solid var(--border)', borderRadius: 4, overflow: 'hidden' }}>
                {(['Blue', 'Red'] as Side[]).map(s => (
                  <button
                    key={s}
                    onClick={() => setAdminSide(s)}
                    style={{
                      padding: '4px 12px', fontSize: '0.68rem', fontWeight: 700, letterSpacing: '0.06em',
                      border: 'none', cursor: 'pointer',
                      background: side === s ? (s === 'Blue' ? '#1d4ed8' : '#b91c1c') : 'var(--bg-elevated)',
                      color: side === s ? '#fff' : 'var(--text-dim)',
                    }}
                  >{s.toUpperCase()}</button>
                ))}
              </div>
            ) : (
              <span style={{
                padding: '4px 12px', fontSize: '0.68rem', fontWeight: 700, letterSpacing: '0.06em',
                borderRadius: 4, background: side === 'Blue' ? '#1d4ed8' : '#b91c1c', color: '#fff',
              }}>{side.toUpperCase()}</span>
            )}
            <button
              onClick={() => refetchSit()}
              disabled={sitFetching}
              className="vs-btn"
              title="Rebuild the situation from the engine now"
              style={{ display: 'flex', alignItems: 'center', gap: 6, fontSize: '0.68rem', padding: '5px 10px' }}
            >
              <RefreshCw size={12} style={{ animation: sitFetching ? 'spin 1s linear infinite' : undefined }} />
            </button>
            <button
              onClick={() => briefing && buildPdf(briefing, situation)}
              disabled={!briefing}
              className="vs-btn"
              title="Situation, tasking, comms card and the full kneeboard reference"
              style={{ display: 'flex', alignItems: 'center', gap: 6, fontSize: '0.68rem', padding: '5px 12px' }}
            >
              <Download size={12} /> KNEEBOARD PDF
            </button>
          </div>
        }
      />

      <div style={{
        flex: 1, minHeight: 0, minWidth: 0, overflowY: tab === 'situation' ? 'hidden' : 'auto',
        overflowX: 'hidden', padding: '1.25rem', display: 'flex', flexDirection: 'column', gap: 12,
      }}>
        {tab === 'situation' && (
          <>
            {sitLoading && (
              <div className="vs-card" style={{ padding: 20, color: 'var(--text-dim)' }}>
                Building the situation from the engine…
              </div>
            )}
            {sitError && (
              <div className="vs-card" style={{ padding: 20, color: '#f87171' }}>
                Situation unavailable — the engine may be unreachable, no round is active, or the running
                bflib.dll predates this feature. The Kneeboard tab still works.
              </div>
            )}
            {situation && <SituationTab report={situation} />}
          </>
        )}

        {tab === 'kneeboard' && (
          <>
            {briefLoading && (
              <div className="vs-card" style={{ padding: 20, color: 'var(--text-dim)' }}>Loading kneeboard…</div>
            )}
            {briefError && (
              <div className="vs-card" style={{ padding: 20, color: '#f87171' }}>
                Kneeboard unavailable — the engine may be unreachable or no round is active.
              </div>
            )}
            {briefing && <KneeboardTab briefing={briefing} />}
          </>
        )}
      </div>
    </div>
  )
}
