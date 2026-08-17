import React, { useEffect, useState } from 'react'
import { useParams, Link } from 'react-router-dom'
import { api, type PilotSortie, type TheaterBreakdown, type PilotKill } from '../api'

const BOX_BG = 'rgba(20,25,16,0.6)'
const BOX_BORDER = '1px solid rgba(255,255,255,0.05)'

export const PilotPage: React.FC = () => {
  const { ucid } = useParams<{ ucid: string }>()
  const [name, setName] = useState<string>('Pilot Profile')
  const [sorties, setSorties] = useState<PilotSortie[]>([])
  const [breakdown, setBreakdown] = useState<TheaterBreakdown[]>([])
  const [kills, setKills] = useState<PilotKill[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    if (!ucid) return
    setLoading(true)
    Promise.all([
      // Try to get pilot name
      api.pilot(ucid).catch(() => null),
      api.pilotSorties(ucid).catch(() => []),
      api.pilotBreakdown(ucid).catch(() => []),
      api.pilotKills(ucid).catch(() => [])
    ]).then(([p, s, b, k]) => {
      if (p && (p as any).name) setName((p as any).name)
      setSorties(s)
      setBreakdown(b)
      setKills(k)
      setLoading(false)
    })
  }, [ucid])

  if (loading) return <div style={{ padding: 20 }}>Loading profile...</div>

  // Aggregates
  const totalHours = breakdown.reduce((acc, b) => acc + b.hours, 0)
  const totalAirKills = breakdown.reduce((acc, b) => acc + b.air_kills, 0)
  const totalGndKills = breakdown.reduce((acc, b) => acc + b.ground_kills, 0)
  const totalDeaths = breakdown.reduce((acc, b) => acc + b.deaths, 0)
  const totalRepairs = breakdown.reduce((acc, b) => acc + b.repairs, 0)
  const totalDeploys = breakdown.reduce((acc, b) => acc + b.deploys, 0)
  const totalSorties = sorties.length
  const kd = totalDeaths > 0 ? ((totalAirKills + totalGndKills) / totalDeaths).toFixed(2) : '∞'

  return (
    <div style={{ padding: '20px 40px', maxWidth: 1200, margin: '0 auto', fontFamily: 'Inter, sans-serif' }}>
      <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', marginBottom: 30 }}>
        <div>
          <h1 style={{ margin: 0, fontSize: '2rem', fontWeight: 600, color: '#f8fafc' }}>
            {name}
          </h1>
        </div>
        <Link to="/stats" style={{ color: '#0ea5e9', textDecoration: 'none', background: BOX_BG, padding: '8px 16px', borderRadius: 4, border: BOX_BORDER }}>
          ← Back to Stats
        </Link>
      </div>

      {/* Stats Grid */}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))', gap: 15, marginBottom: 40 }}>
        <StatBox label="Sorties" value={totalSorties} />
        <StatBox label="Flight Hours" value={totalHours.toFixed(1)} />
        <StatBox label="Air Kills" value={totalAirKills} />
        <StatBox label="Ground Kills" value={totalGndKills} />
        <StatBox label="Deaths" value={totalDeaths} color="#ef4444" />
        <StatBox label="K/D Ratio" value={kd} color="#10b981" />
        <StatBox label="Logistics Runs" value={totalRepairs + totalDeploys} />
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 30 }}>
        {/* Sorties Table */}
        <div style={{ background: BOX_BG, border: BOX_BORDER, borderRadius: 6, padding: 20 }}>
          <h2 style={{ margin: '0 0 15px 0', fontSize: '1.2rem', color: '#e2e8f0' }}>Recent Sorties</h2>
          <div style={{ overflowY: 'auto', maxHeight: 400 }}>
            <table style={{ width: '100%', borderCollapse: 'collapse', fontSize: '0.9rem' }}>
              <thead>
                <tr style={{ color: '#94a3b8', textAlign: 'left', borderBottom: BOX_BORDER }}>
                  <th style={{ padding: '8px 4px' }}>Date</th>
                  <th style={{ padding: '8px 4px' }}>Aircraft</th>
                  <th style={{ padding: '8px 4px' }}>Duration</th>
                  <th style={{ padding: '8px 4px' }}>Status</th>
                </tr>
              </thead>
              <tbody>
                {sorties.slice(0, 50).map((s, i) => (
                  <tr key={i} style={{ borderBottom: '1px solid rgba(255,255,255,0.02)' }}>
                    <td style={{ padding: '8px 4px' }}>{new Date(s.takeoff).toLocaleDateString()}</td>
                    <td style={{ padding: '8px 4px', color: '#bae6fd' }}>{s.aircraft}</td>
                    <td style={{ padding: '8px 4px' }}>{Math.round(s.duration_secs / 60)} min</td>
                    <td style={{ padding: '8px 4px', color: s.landed ? '#10b981' : '#ef4444' }}>
                      {s.landed ? 'Landed' : 'Lost'}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
            {sorties.length === 0 && <div style={{ padding: 10, color: '#64748b' }}>No sorties recorded.</div>}
          </div>
        </div>

        {/* Recent Kills */}
        <div style={{ background: BOX_BG, border: BOX_BORDER, borderRadius: 6, padding: 20 }}>
          <h2 style={{ margin: '0 0 15px 0', fontSize: '1.2rem', color: '#e2e8f0' }}>Recent Kills</h2>
          <div style={{ overflowY: 'auto', maxHeight: 400 }}>
            <table style={{ width: '100%', borderCollapse: 'collapse', fontSize: '0.9rem' }}>
              <thead>
                <tr style={{ color: '#94a3b8', textAlign: 'left', borderBottom: BOX_BORDER }}>
                  <th style={{ padding: '8px 4px' }}>Time</th>
                  <th style={{ padding: '8px 4px' }}>Target</th>
                  <th style={{ padding: '8px 4px' }}>Weapon</th>
                </tr>
              </thead>
              <tbody>
                {kills.slice(0, 50).map((k, i) => (
                  <tr key={i} style={{ borderBottom: '1px solid rgba(255,255,255,0.02)' }}>
                    <td style={{ padding: '8px 4px' }}>{new Date(k.time).toLocaleTimeString()}</td>
                    <td style={{ padding: '8px 4px', color: '#fca5a5' }}>
                      {k.target_type || k.victim_ucid || 'Unknown'}
                    </td>
                    <td style={{ padding: '8px 4px', color: '#cbd5e1' }}>{k.weapon || '-'}</td>
                  </tr>
                ))}
              </tbody>
            </table>
            {kills.length === 0 && <div style={{ padding: 10, color: '#64748b' }}>No kills recorded.</div>}
          </div>
        </div>
      </div>
    </div>
  )
}

const StatBox: React.FC<{ label: string; value: string | number; color?: string }> = ({ label, value, color }) => (
  <div style={{ background: BOX_BG, border: BOX_BORDER, borderRadius: 6, padding: '20px 15px', textAlign: 'center' }}>
    <div style={{ fontSize: '2rem', fontWeight: 700, color: color || '#f1f5f9', lineHeight: 1.2 }}>{value}</div>
    <div style={{ fontSize: '0.8rem', color: '#94a3b8', textTransform: 'uppercase', letterSpacing: 1, marginTop: 5 }}>{label}</div>
  </div>
)
