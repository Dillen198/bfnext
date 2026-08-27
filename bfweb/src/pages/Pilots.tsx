import React, { useState, useEffect, useMemo } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useSearchParams } from 'react-router-dom'
import { api, type Pilot, type PilotSortie, type TheaterBreakdown, type PilotKill, type PilotDeploy } from '../api'
import PageHeader from '../components/PageHeader'
import {
  Search, Users, Crosshair, Clock, Award, ChevronLeft, ChevronRight,
  Link, Plane, Shield, RotateCcw, ChevronDown, ChevronUp, Package,
} from 'lucide-react'
import { useAuth } from '../context/AuthContext'

// ── Helpers ──────────────────────────────────────────────────────────────────

function kd(air: number, ground: number, deaths: number): string {
  const k = air + ground
  return deaths > 0 ? (k / deaths).toFixed(2) : k > 0 ? '∞' : '0.00'
}

function sl(sorties: number, landings: number): string {
  return sorties > 0 ? (landings / sorties).toFixed(2) : '—'
}

function fmtHours(h: number): string {
  const total = Math.round(h * 60)
  const hh = Math.floor(total / 60)
  const mm = total % 60
  return `${hh}h ${mm.toString().padStart(2, '0')}m`
}

function fmtDuration(secs: number): string {
  if (secs <= 0) return '—'
  const h = Math.floor(secs / 3600)
  const m = Math.floor((secs % 3600) / 60)
  if (h > 0) return `${h}h ${m}m`
  return `${m}m`
}

function fmtTime(iso: string): string {
  return new Date(iso).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', hour12: false })
}

function fmtDate(iso: string): string {
  const d = new Date(iso)
  return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, '0')}-${String(d.getDate()).padStart(2, '0')}`
}

function bestStreak(sorties: PilotSortie[]): number {
  let best = 0, cur = 0
  for (const s of sorties) {
    if (s.landed) { cur++; best = Math.max(best, cur) } else { cur = 0 }
  }
  return best
}

function curStreak(sorties: PilotSortie[]): number {
  let cur = 0
  for (let i = sorties.length - 1; i >= 0; i--) {
    if (sorties[i].landed) cur++; else break
  }
  return cur
}

function mostFlownAircraft(sorties: PilotSortie[]): string | null {
  const counts: Record<string, number> = {}
  for (const s of sorties) counts[s.aircraft] = (counts[s.aircraft] ?? 0) + 1
  let best: string | null = null, bestN = 0
  for (const [ac, n] of Object.entries(counts)) {
    if (n > bestN) { bestN = n; best = ac }
  }
  return best
}

// Aircraft image: served from /images/aircraft/{type}.jpg
// Falls back to a placeholder icon if not found
function AircraftImage({ type }: { type: string }) {
  const [err, setErr] = useState(false)
  const src = `/images/aircraft/${encodeURIComponent(type)}.jpg`
  if (err) {
    return (
      <div style={{ width: '100%', aspectRatio: '16 / 9', display: 'flex', alignItems: 'center', justifyContent: 'center', opacity: 0.15 }}>
        <Plane size={64} />
      </div>
    )
  }
  return (
    <img
      src={src}
      alt={type}
      onError={() => setErr(true)}
      style={{ width: '100%', aspectRatio: '16 / 9', objectFit: 'cover', display: 'block' }}
    />
  )
}

// ── Sub-components ────────────────────────────────────────────────────────────

type SortDir = 'asc' | 'desc'

function SortHeader<T extends string>({
  col, label, sort, setSort,
}: {
  col: T
  label: string
  sort: { col: T; dir: SortDir }
  setSort: (s: { col: T; dir: SortDir }) => void
}) {
  const active = sort.col === col
  return (
    <th
      onClick={() => setSort({ col, dir: active && sort.dir === 'desc' ? 'asc' : 'desc' })}
      style={{
        padding: '6px 10px', fontSize: '0.6rem', color: active ? 'var(--accent)' : 'var(--text-dim)',
        letterSpacing: '0.14em', textTransform: 'uppercase', textAlign: 'left',
        fontWeight: 600, cursor: 'pointer', userSelect: 'none', whiteSpace: 'nowrap',
        borderBottom: '1px solid var(--border)',
      }}
    >
      <span className="flex items-center gap-1">
        {label}
        {active
          ? sort.dir === 'desc'
            ? <ChevronDown size={9} />
            : <ChevronUp size={9} />
          : <ChevronDown size={9} style={{ opacity: 0.3 }} />}
      </span>
    </th>
  )
}

// ── Flight log ────────────────────────────────────────────────────────────────

type SortieCol = 'date' | 'aircraft' | 'duration' | 'outcome'

function FlightLog({ sorties, breakdown }: { sorties: PilotSortie[]; breakdown: TheaterBreakdown[] }) {
  const [sort, setSort] = useState<{ col: SortieCol; dir: SortDir }>({ col: 'date', dir: 'desc' })
  const [page, setPage] = useState(0)
  const PER_PAGE = 10

  const ridToScenario = useMemo(() => {
    const m: Record<number, string> = {}
    for (const r of breakdown) m[r.round_id] = r.scenario
    return m
  }, [breakdown])

  const sorted = useMemo(() => {
    const arr = [...sorties]
    arr.sort((a, b) => {
      let v = 0
      if (sort.col === 'date')     v = a.takeoff.localeCompare(b.takeoff)
      if (sort.col === 'aircraft') v = a.aircraft.localeCompare(b.aircraft)
      if (sort.col === 'duration') v = a.duration_secs - b.duration_secs
      if (sort.col === 'outcome')  v = (a.landed ? 1 : 0) - (b.landed ? 1 : 0)
      return sort.dir === 'desc' ? -v : v
    })
    return arr
  }, [sorties, sort])

  const totalPages = Math.ceil(sorted.length / PER_PAGE)
  const page_sorties = sorted.slice(page * PER_PAGE, (page + 1) * PER_PAGE)

  const cell: React.CSSProperties = { padding: '5px 10px', fontSize: '0.67rem', color: 'var(--text-muted)', borderBottom: '1px solid rgba(42,42,42,0.5)' }

  return (
    <div className="vs-card">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Clock size={12} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
          Flight Log
        </span>
        <span className="ml-auto font-mono" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
          {sorties.length} sorties
        </span>
        {/* Pagination */}
        {totalPages > 1 && (
          <div className="flex items-center gap-1 ml-2">
            <button onClick={() => setPage(p => Math.max(0, p - 1))} disabled={page === 0}
              style={{ background: 'none', border: 'none', color: page === 0 ? '#333' : 'var(--text-dim)', cursor: page === 0 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronLeft size={12} />
            </button>
            <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace' }}>
              {page + 1} / {totalPages}
            </span>
            <button onClick={() => setPage(p => Math.min(totalPages - 1, p + 1))} disabled={page === totalPages - 1}
              style={{ background: 'none', border: 'none', color: page === totalPages - 1 ? '#333' : 'var(--text-dim)', cursor: page === totalPages - 1 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronRight size={12} />
            </button>
          </div>
        )}
      </div>
      {sorties.length === 0 ? (
        <div style={{ padding: '1.5rem', textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.7rem' }}>No sorties recorded</div>
      ) : (
        <table style={{ width: '100%', borderCollapse: 'collapse' }}>
          <thead>
            <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
              <SortHeader col="date" label="Date" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="aircraft" label="Aircraft" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>Theater</th>
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>Takeoff</th>
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>End</th>
              <SortHeader col="duration" label="Duration" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="outcome" label="Outcome" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
            </tr>
          </thead>
          <tbody>
            {page_sorties.map((s, i) => (
              <tr key={i} style={{ background: i % 2 === 0 ? 'transparent' : 'rgba(0,0,0,0.12)' }}>
                <td style={{ ...cell, fontFamily: 'monospace', color: '#64748b' }}>{fmtDate(s.takeoff)}</td>
                <td style={{ ...cell, color: '#60a5fa' }}>{s.aircraft}</td>
                <td style={{ ...cell, color: '#94a3b8' }}>{ridToScenario[s.round_id] ?? `Round ${s.round_id}`}</td>
                <td style={{ ...cell, fontFamily: 'monospace' }}>{fmtTime(s.takeoff)}</td>
                <td style={{ ...cell, fontFamily: 'monospace' }}>{s.land ? fmtTime(s.land) : '—'}</td>
                <td style={{ ...cell, fontFamily: 'monospace', color: s.duration_secs > 0 ? 'var(--text)' : '#64748b' }}>{fmtDuration(s.duration_secs)}</td>
                <td style={cell}>
                  {s.landed ? (
                    <span style={{ color: '#22c55e', fontSize: '0.62rem', letterSpacing: '0.06em' }}>✈ Landed</span>
                  ) : (
                    <span style={{ color: '#ef4444', fontSize: '0.62rem', letterSpacing: '0.06em' }}>✕ Lost</span>
                  )}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
    </div>
  )
}

// ── Kill log ──────────────────────────────────────────────────────────────────

type KillCol = 'time' | 'target_type' | 'weapon' | 'victim'

function KillLog({ kills, allPilots, breakdown }: {
  kills: PilotKill[]
  allPilots: Pilot[]
  breakdown: TheaterBreakdown[]
}) {
  const [sort, setSort] = useState<{ col: KillCol; dir: SortDir }>({ col: 'time', dir: 'desc' })
  const [page, setPage] = useState(0)
  const PER_PAGE = 15

  const ridToScenario = useMemo(() => {
    const m: Record<number, string> = {}
    for (const r of breakdown) m[r.round_id] = r.scenario
    return m
  }, [breakdown])

  const ucidToName = useMemo(() => {
    const m: Record<string, string> = {}
    for (const p of allPilots) m[p.ucid] = p.name
    return m
  }, [allPilots])

  const sorted = useMemo(() => {
    const arr = [...kills]
    arr.sort((a, b) => {
      let v = 0
      if (sort.col === 'time')        v = a.time.localeCompare(b.time)
      if (sort.col === 'target_type') v = (a.target_type ?? '').localeCompare(b.target_type ?? '')
      if (sort.col === 'weapon')      v = (a.weapon ?? '').localeCompare(b.weapon ?? '')
      if (sort.col === 'victim')      v = (a.victim_ucid ?? '').localeCompare(b.victim_ucid ?? '')
      return sort.dir === 'desc' ? -v : v
    })
    return arr
  }, [kills, sort])

  const victimSideColor = (side: string) => side === 'Blue' ? '#3b82f6' : side === 'Red' ? '#ef4444' : '#64748b'

  const totalPages = Math.ceil(sorted.length / PER_PAGE)
  const page_kills = sorted.slice(page * PER_PAGE, (page + 1) * PER_PAGE)

  const cell: React.CSSProperties = { padding: '5px 10px', fontSize: '0.67rem', color: 'var(--text-muted)', borderBottom: '1px solid rgba(42,42,42,0.5)' }

  return (
    <div className="vs-card">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Crosshair size={12} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
          Kill Log
        </span>
        <span className="ml-auto font-mono" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
          {kills.length} kills
        </span>
        {totalPages > 1 && (
          <div className="flex items-center gap-1 ml-2">
            <button onClick={() => setPage(p => Math.max(0, p - 1))} disabled={page === 0}
              style={{ background: 'none', border: 'none', color: page === 0 ? '#333' : 'var(--text-dim)', cursor: page === 0 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronLeft size={12} />
            </button>
            <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace' }}>
              {page + 1} / {totalPages}
            </span>
            <button onClick={() => setPage(p => Math.min(totalPages - 1, p + 1))} disabled={page === totalPages - 1}
              style={{ background: 'none', border: 'none', color: page === totalPages - 1 ? '#333' : 'var(--text-dim)', cursor: page === totalPages - 1 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronRight size={12} />
            </button>
          </div>
        )}
      </div>
      {kills.length === 0 ? (
        <div style={{ padding: '1.5rem', textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.7rem' }}>No kills recorded</div>
      ) : (
        <table style={{ width: '100%', borderCollapse: 'collapse' }}>
          <thead>
            <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
              <SortHeader col="time" label="Zulu" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="target_type" label="Target Unit" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="victim" label="Victim Pilot" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>Airframe</th>
              <SortHeader col="weapon" label="Weapon" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>Theater</th>
            </tr>
          </thead>
          <tbody>
            {page_kills.map((k, i) => {
              const victimName = k.victim_ucid ? (ucidToName[k.victim_ucid] ?? 'Unknown') : null
              return (
                <tr key={i} style={{ background: i % 2 === 0 ? 'transparent' : 'rgba(0,0,0,0.12)' }}>
                  <td style={{ ...cell, fontFamily: 'monospace', color: '#64748b', whiteSpace: 'nowrap' }}>
                    {fmtDate(k.time)} {fmtTime(k.time)}
                  </td>
                  <td style={{ ...cell }}>
                    <span style={{ color: victimSideColor(k.victim_side), fontSize: '0.6em' }}>●</span>
                    {' '}
                    <span style={{ color: 'var(--text-muted)' }}>{k.target_type ?? '—'}</span>
                  </td>
                  <td style={cell}>
                    {victimName ? (
                      <span style={{ color: '#f59e0b', fontWeight: 600 }}>{victimName}</span>
                    ) : (
                      <span style={{ color: '#475569' }}>AI / env</span>
                    )}
                  </td>
                  <td style={{ ...cell, color: '#60a5fa', fontFamily: 'monospace', whiteSpace: 'nowrap' }}>
                    {k.killer_airframe ? (
                      <span style={{ display: 'flex', alignItems: 'center', gap: 3 }}>
                        <Plane size={9} style={{ opacity: 0.6 }} />
                        {k.killer_airframe}
                      </span>
                    ) : <span style={{ color: '#374151' }}>—</span>}
                  </td>
                  <td style={{ ...cell, color: '#fbbf24', fontFamily: 'monospace' }}>{k.weapon ?? <span style={{ color: '#374151' }}>—</span>}</td>
                  <td style={{ ...cell, color: '#475569' }}>{ridToScenario[k.round_id] ?? `Round ${k.round_id}`}</td>
                </tr>
              )
            })}
          </tbody>
        </table>
      )}
    </div>
  )
}

// ── Deploy log ────────────────────────────────────────────────────────────────

type DeployCol = 'time' | 'deployable' | 'aircraft' | 'method'

function DeployLog({ deploys }: { deploys: PilotDeploy[] }) {
  const [sort, setSort] = useState<{ col: DeployCol; dir: SortDir }>({ col: 'time', dir: 'desc' })
  const [page, setPage] = useState(0)
  const PER_PAGE = 15

  const sorted = useMemo(() => {
    const arr = [...deploys]
    arr.sort((a, b) => {
      let v = 0
      if (sort.col === 'time')       v = a.time.localeCompare(b.time)
      if (sort.col === 'deployable') v = a.deployable.localeCompare(b.deployable)
      if (sort.col === 'aircraft')   v = (a.aircraft ?? '').localeCompare(b.aircraft ?? '')
      if (sort.col === 'method')     v = (a.method ?? '').localeCompare(b.method ?? '')
      return sort.dir === 'desc' ? -v : v
    })
    return arr
  }, [deploys, sort])

  const totalPages = Math.ceil(sorted.length / PER_PAGE)
  const page_deploys = sorted.slice(page * PER_PAGE, (page + 1) * PER_PAGE)

  const cell: React.CSSProperties = { padding: '5px 10px', fontSize: '0.67rem', color: 'var(--text-muted)', borderBottom: '1px solid rgba(42,42,42,0.5)' }

  const methodBadge = (method: string | null) => {
    if (method === 'AirDrop') return <span style={{ color: '#60a5fa', fontSize: '0.62rem', letterSpacing: '0.06em' }}>▼ Air Drop</span>
    if (method === 'ManualUnpack') return <span style={{ color: '#f59e0b', fontSize: '0.62rem', letterSpacing: '0.06em' }}>✋ Manual Unpack</span>
    return <span style={{ color: '#475569' }}>—</span>
  }

  return (
    <div className="vs-card">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Package size={12} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
          Deploy Log
        </span>
        <span className="ml-auto font-mono" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
          {deploys.length} deploys
        </span>
        {totalPages > 1 && (
          <div className="flex items-center gap-1 ml-2">
            <button onClick={() => setPage(p => Math.max(0, p - 1))} disabled={page === 0}
              style={{ background: 'none', border: 'none', color: page === 0 ? '#333' : 'var(--text-dim)', cursor: page === 0 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronLeft size={12} />
            </button>
            <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace' }}>
              {page + 1} / {totalPages}
            </span>
            <button onClick={() => setPage(p => Math.min(totalPages - 1, p + 1))} disabled={page === totalPages - 1}
              style={{ background: 'none', border: 'none', color: page === totalPages - 1 ? '#333' : 'var(--text-dim)', cursor: page === totalPages - 1 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronRight size={12} />
            </button>
          </div>
        )}
      </div>
      {deploys.length === 0 ? (
        <div style={{ padding: '1.5rem', textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.7rem' }}>No deploys recorded</div>
      ) : (
        <table style={{ width: '100%', borderCollapse: 'collapse' }}>
          <thead>
            <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
              <SortHeader col="time" label="When" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="deployable" label="What" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="aircraft" label="Aircraft" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="method" label="Method" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
            </tr>
          </thead>
          <tbody>
            {page_deploys.map((d, i) => (
              <tr key={i} style={{ background: i % 2 === 0 ? 'transparent' : 'rgba(0,0,0,0.12)' }}>
                <td style={{ ...cell, fontFamily: 'monospace', color: '#64748b', whiteSpace: 'nowrap' }}>
                  {fmtDate(d.time)} {fmtTime(d.time)}
                </td>
                <td style={{ ...cell, color: 'var(--text)' }}>{d.deployable}</td>
                <td style={{ ...cell, color: '#60a5fa', fontFamily: 'monospace' }}>
                  {d.aircraft ?? <span style={{ color: '#374151' }}>—</span>}
                </td>
                <td style={cell}>{methodBadge(d.method)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
    </div>
  )
}

// ── Theater breakdown ─────────────────────────────────────────────────────────

function TheaterCard({ td }: { td: TheaterBreakdown }) {
  const kd_val = kd(td.air_kills, td.ground_kills, td.deaths)
  const kd_color = td.deaths === 0 ? '#22c55e' : parseFloat(kd_val) >= 5 ? '#22c55e' : parseFloat(kd_val) >= 2 ? '#f59e0b' : '#ef4444'
  const dim: React.CSSProperties = { fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.1em', textTransform: 'uppercase' }
  const statRow = (label: string, value: string | number, highlight?: string) => (
    <div className="flex justify-between items-center" style={{ padding: '3px 0', borderBottom: '1px solid rgba(42,42,42,0.4)' }}>
      <span style={dim}>{label}</span>
      <span style={{ fontSize: '0.7rem', color: highlight ?? 'var(--text-muted)', fontFamily: 'monospace', fontWeight: 600 }}>{value}</span>
    </div>
  )

  return (
    <div className="vs-card p-0" style={{ overflow: 'hidden' }}>
      {/* Header */}
      <div className="px-3 py-2.5" style={{ background: 'rgba(0,0,0,0.25)', borderBottom: '1px solid var(--border)' }}>
        <div style={{ fontSize: '0.78rem', color: 'var(--text)', fontWeight: 700, letterSpacing: '0.04em' }}>
          {td.scenario || `Round ${td.round_id}`}
        </div>
        <div style={{ fontSize: '0.58rem', color: 'var(--text-dim)', marginTop: '0.1rem' }}>
          Round #{td.round_id}
        </div>
      </div>
      <div className="px-3 py-2">
        {statRow('A/A Kills', td.air_kills, '#3b82f6')}
        {statRow('A/G Kills', td.ground_kills, '#f97316')}
        {statRow('Total Kills', td.air_kills + td.ground_kills, 'var(--text)')}
        {statRow('AA K/D', td.deaths > 0 ? (td.air_kills / td.deaths).toFixed(2) : td.air_kills > 0 ? '∞' : '0.00')}
        {statRow('AG K/D', td.deaths > 0 ? (td.ground_kills / td.deaths).toFixed(2) : td.ground_kills > 0 ? '∞' : '0.00')}
        {statRow('K/D Ratio', kd_val, kd_color)}
        {statRow('Deaths', td.deaths, td.deaths > 0 ? '#ef4444' : 'var(--text-dim)')}
        {statRow('Captures', td.captures, td.captures > 0 ? '#eab308' : undefined)}
        {statRow('Hours', fmtHours(td.hours))}
      </div>
    </div>
  )
}

// ── Main component ────────────────────────────────────────────────────────────

export default function Pilots() {
  const { user } = useAuth()
  const queryClient = useQueryClient()
  const [searchParams] = useSearchParams()
  const { data: pilots = [], isLoading } = useQuery({
    queryKey: ['leaderboard'],
    queryFn: api.leaderboard,
    refetchInterval: 60_000,
  })
  const [selected, setSelected] = useState<string | null>(null)
  const [search, setSearch] = useState('')
  const [searchFocus, setSearchFocus] = useState(false)

  // Auto-select from ?ucid= URL param, then logged-in user, on first load
  useEffect(() => {
    if (pilots.length === 0 || selected !== null) return
    const paramUcid = searchParams.get('ucid')
    if (paramUcid) {
      const p = pilots.find(p => p.ucid === paramUcid)
      setSelected(paramUcid)
      setSearch(p?.name ?? '')
      return
    }
    if (user?.ucid) {
      setSelected(user.ucid)
      setSearch(pilots.find(p => p.ucid === user.ucid)?.name ?? '')
    }
  }, [user?.ucid, pilots.length])

  const pilot = pilots.find(p => p.ucid === selected) ?? null
  const pilotRank = pilot ? pilots.findIndex(p => p.ucid === pilot.ucid) + 1 : null

  // Search results shown as dropdown
  const showDropdown = searchFocus && search.length > 0
  const filtered = pilots.filter(p => p.name.toLowerCase().includes(search.toLowerCase())).slice(0, 12)

  // Pilot detail queries (only when pilot selected)
  const { data: sorties = [] } = useQuery({
    queryKey: ['pilot-sorties', selected],
    queryFn: () => api.pilotSorties(selected!),
    enabled: !!selected,
  })
  const { data: breakdown = [] } = useQuery({
    queryKey: ['pilot-breakdown', selected],
    queryFn: () => api.pilotBreakdown(selected!),
    enabled: !!selected,
  })
  const { data: kills = [] } = useQuery({
    queryKey: ['pilot-kills', selected],
    queryFn: () => api.pilotKills(selected!),
    enabled: !!selected,
  })
  const { data: deploys = [] } = useQuery({
    queryKey: ['pilot-deploys', selected],
    queryFn: () => api.pilotDeploys(selected!),
    enabled: !!selected,
  })

  // Derived stats
  const totalLandings = sorties.filter(s => s.landed).length
  const totalSorties  = sorties.length
  const bestStreakVal  = bestStreak(sorties)
  const curStreakVal   = curStreak(sorties)
  const mostFlown     = mostFlownAircraft(sorties)
  const bestAA        = breakdown.length > 0 ? Math.max(...breakdown.map(t => t.air_kills)) : 0
  const bestAG        = breakdown.length > 0 ? Math.max(...breakdown.map(t => t.ground_kills)) : 0

  const dim: React.CSSProperties = { fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader title="PILOTS" sub={`${pilots.length} registered pilots`} />

      <div className="flex-1 overflow-auto p-4 space-y-4" style={{ background: 'var(--bg)' }}>
        {/* ── Search ── */}
        <div className="vs-card p-0" style={{ overflow: 'visible', position: 'relative', zIndex: 10 }}>
          <div className="flex items-center gap-2 px-3 py-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
            <Search size={11} style={{ color: 'var(--accent)', flexShrink: 0 }} />
            <span style={dim}>PILOT SEARCH</span>
          </div>
          <div style={{ position: 'relative', padding: '0.75rem' }}>
            <input
              type="text"
              placeholder="Search pilots by name…"
              value={search}
              onChange={e => { setSearch(e.target.value); if (!e.target.value) setSelected(null) }}
              onFocus={() => setSearchFocus(true)}
              onBlur={() => setTimeout(() => setSearchFocus(false), 150)}
              className="vs-input w-full"
              style={{ fontSize: '0.8rem', padding: '0.5rem 0.75rem' }}
            />
            {/* Search dropdown */}
            {showDropdown && filtered.length > 0 && (
              <div style={{
                position: 'absolute', top: '100%', left: '0.75rem', right: '0.75rem',
                background: 'var(--bg-elevated)', border: '1px solid var(--border)', borderRadius: '3px',
                boxShadow: '0 8px 24px rgba(0,0,0,0.5)', zIndex: 50, maxHeight: 300, overflowY: 'auto',
              }}>
                {filtered.map((p, i) => {
                  const isMe = user?.ucid === p.ucid
                  return (
                    <button
                      key={p.ucid}
                      onMouseDown={() => { setSelected(p.ucid); setSearch(p.name) }}
                      style={{
                        width: '100%', textAlign: 'left', background: 'none', border: 'none',
                        borderBottom: i < filtered.length - 1 ? '1px solid var(--border)' : 'none',
                        padding: '0.5rem 0.75rem', cursor: 'pointer',
                        display: 'flex', alignItems: 'center', gap: '0.75rem',
                      }}
                      onMouseEnter={e => (e.currentTarget.style.background = 'rgba(77,124,15,0.07)')}
                      onMouseLeave={e => (e.currentTarget.style.background = 'none')}
                    >
                      <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', fontFamily: 'monospace', width: 20, textAlign: 'right' }}>
                        {pilots.indexOf(p) + 1}
                      </span>
                      <div style={{ flex: 1, minWidth: 0 }}>
                        <div style={{ fontSize: '0.75rem', color: 'var(--text)', fontWeight: 600 }}>
                          {p.name}
                          {isMe && <span style={{ fontSize: '0.5rem', color: 'var(--accent)', border: '1px solid var(--accent)', padding: '0 3px', borderRadius: 2, marginLeft: 6 }}>YOU</span>}
                        </div>
                        <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace', marginTop: 1 }}>
                          {p.air_kills + p.ground_kills}k · {p.hours.toFixed(1)}h · {p.deaths}d
                        </div>
                      </div>
                    </button>
                  )
                })}
              </div>
            )}
          </div>
        </div>

        {/* ── Link account banner ── */}
        {user && !user.ucid && (
          <div className="vs-card px-4 py-3 flex items-center gap-3" style={{ borderColor: '#5865F2', background: 'rgba(88,101,242,0.05)' }}>
            <Link size={14} style={{ color: '#5865F2', flexShrink: 0 }} />
            <div style={{ flex: 1, fontSize: '0.7rem', color: 'var(--text-muted)' }}>
              Not linked yet — run <code style={{ color: 'var(--text)' }}>/linkme</code> in Discord for a token, then <code style={{ color: 'var(--text)' }}>-linkme &lt;token&gt;</code> in DCS chat to see your profile highlighted.
            </div>
            <button
              onClick={() => queryClient.invalidateQueries({ queryKey: ['auth', 'me'] })}
              style={{ fontSize: '0.65rem', color: '#5865F2', background: 'none', border: '1px solid #5865F2', padding: '2px 10px', borderRadius: 2, cursor: 'pointer', letterSpacing: '0.08em', fontFamily: "'Bebas Neue', sans-serif" }}
            >
              CHECK AGAIN
            </button>
          </div>
        )}

        {!pilot && !isLoading && (
          <div className="flex flex-col items-center justify-center py-20 gap-3" style={{ color: 'var(--text-dim)' }}>
            <Users size={40} style={{ opacity: 0.15 }} />
            <div style={{ fontSize: '0.72rem', letterSpacing: '0.16em', textTransform: 'uppercase' }}>Search for a pilot above</div>
          </div>
        )}

        {pilot && (
          <div className="grid grid-cols-1 lg:grid-cols-[360px_1fr] gap-4 items-start">

            {/* ══════════ LEFT — profile sidebar ══════════ */}
            <div className="space-y-4">

              {/* ── Pilot name card ── */}
              <div className="vs-card px-4 py-3 flex items-center gap-3">
                <div style={{ width: 32, height: 32, background: 'rgba(77,124,15,0.12)', border: '1px solid rgba(77,124,15,0.25)', borderRadius: 2, display: 'flex', alignItems: 'center', justifyContent: 'center', flexShrink: 0 }}>
                  <Users size={14} style={{ color: 'var(--accent)' }} />
                </div>
                <div style={{ flex: 1, minWidth: 0 }}>
                  <div style={{ fontSize: '1.1rem', fontWeight: 700, color: 'var(--text)', letterSpacing: '0.03em', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{pilot.name}</div>
                  <div className="flex items-center gap-2 mt-0.5">
                    {pilotRank && pilotRank <= 3 && (
                      <span style={{ fontSize: '0.9rem' }}>{['🥇', '🥈', '🥉'][pilotRank - 1]}</span>
                    )}
                    {pilotRank && (
                      <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace' }}>
                        Rank #{pilotRank}
                      </span>
                    )}
                    {user?.ucid === pilot.ucid && (
                      <span style={{ fontSize: '0.5rem', color: 'var(--accent)', border: '1px solid var(--accent)', padding: '0 4px', borderRadius: 2, letterSpacing: '0.08em' }}>YOU</span>
                    )}
                  </div>
                </div>
                <button
                  onClick={() => { setSelected(null); setSearch('') }}
                  style={{ background: 'none', border: 'none', color: 'var(--text-dim)', cursor: 'pointer', display: 'flex', alignItems: 'center', gap: 4, fontSize: '0.62rem', flexShrink: 0 }}
                >
                  <RotateCcw size={10} /> Clear
                </button>
              </div>

              {/* ── Most flown airframe (hero) ── */}
              {mostFlown && (
                <div className="vs-card p-0" style={{ overflow: 'hidden' }}>
                  <AircraftImage type={mostFlown} />
                  <div className="px-4 py-3">
                    <div style={dim}>Most Flown Airframe</div>
                    <div style={{ fontSize: '1.15rem', color: '#60a5fa', fontWeight: 700, letterSpacing: '0.05em', marginTop: 2 }}>{mostFlown}</div>
                  </div>
                </div>
              )}

              {/* ── Career stats ── */}
              <div className="vs-card p-0">
                <div className="flex items-center gap-2 px-4 pt-3.5 pb-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
                  <Crosshair size={12} style={{ color: 'var(--accent)' }} />
                  <span style={dim}>Career Stats</span>
                </div>
                <div className="px-4 py-1">
                  {[
                    { label: 'A/A Victories', value: pilot.air_kills, color: '#60a5fa' },
                    { label: 'A/G Victories', value: pilot.ground_kills, color: '#f97316' },
                    { label: 'V/L Ratio', value: kd(pilot.air_kills, pilot.ground_kills, pilot.deaths), color: '#22c55e' },
                    { label: 'S/L Ratio', value: sl(totalSorties, totalLandings), color: '#a78bfa' },
                    { label: 'Flight Time', value: fmtHours(pilot.hours), color: 'var(--text)' },
                    { label: 'Current Streak', value: curStreakVal, color: '#22c55e' },
                    { label: 'Best Streak', value: bestStreakVal, color: '#f59e0b' },
                    { label: 'Best A/A (round)', value: bestAA, color: '#60a5fa' },
                    { label: 'Best A/G (round)', value: bestAG, color: '#f97316' },
                  ].map(s => (
                    <div key={s.label} className="flex justify-between items-center" style={{ padding: '0.5rem 0', borderBottom: '1px solid rgba(42,42,42,0.5)' }}>
                      <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.08em' }}>{s.label}</span>
                      <span style={{ fontSize: '0.85rem', fontWeight: 700, color: s.color, fontFamily: 'monospace' }}>{s.value}</span>
                    </div>
                  ))}
                </div>
              </div>

              {/* ── Contributions ── */}
              <div className="vs-card p-0">
                <div className="flex items-center gap-2 px-4 pt-3.5 pb-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
                  <Award size={12} style={{ color: 'var(--accent)' }} />
                  <span style={dim}>Contributions</span>
                </div>
                <div className="grid grid-cols-2 gap-0">
                  {[
                    { label: 'Captures',     value: pilot.captures },
                    { label: 'Repairs',      value: pilot.repairs },
                    { label: 'Supply Runs',  value: pilot.supply_transfers },
                    { label: 'Troops',       value: pilot.troops },
                    { label: 'FARPs',        value: pilot.farps },
                    { label: 'Deploys',      value: pilot.deploys },
                    { label: 'Actions',      value: pilot.actions },
                    { label: 'Donated Pts',  value: pilot.donated_points },
                  ].map((s, i) => (
                    <div key={s.label} style={{ padding: '0.65rem 1rem', borderRight: i % 2 === 0 ? '1px solid var(--border)' : 'none', borderBottom: i < 6 ? '1px solid var(--border)' : 'none' }}>
                      <div style={{ fontSize: '0.56rem', color: 'var(--text-dim)', letterSpacing: '0.1em', textTransform: 'uppercase', marginBottom: '0.2rem' }}>{s.label}</div>
                      <div style={{ fontSize: '0.95rem', fontWeight: 700, color: 'var(--text-muted)', fontFamily: 'monospace' }}>{s.value}</div>
                    </div>
                  ))}
                </div>
              </div>
            </div>

            {/* ══════════ RIGHT — detail tables ══════════ */}
            <div className="space-y-4" style={{ minWidth: 0 }}>

              {/* ── Theater breakdown ── */}
              {breakdown.length > 0 && (
                <div className="vs-card p-0">
                  <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
                    <Shield size={12} style={{ color: 'var(--accent)' }} />
                    <span style={dim}>Theater Breakdown</span>
                    <span className="ml-auto font-mono" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>{breakdown.length}</span>
                  </div>
                  <div className="p-3 grid gap-3" style={{ gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))' }}>
                    {breakdown.map(td => (
                      <TheaterCard key={td.round_id} td={td} />
                    ))}
                  </div>
                </div>
              )}

              {/* ── Flight log ── */}
              <FlightLog sorties={sorties} breakdown={breakdown} />

              {/* ── Kill log ── */}
              <KillLog kills={kills} allPilots={pilots} breakdown={breakdown} />

              {/* ── Deploy log ── */}
              <DeployLog deploys={deploys} />
            </div>
          </div>
        )}
      </div>
    </div>
  )
}
