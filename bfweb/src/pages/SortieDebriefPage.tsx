// Full-page debrief for one sortie (/pilots/:ucid/sortie/:id), opened from the
// Flight Log. The top half is bfdb's own record of the sortie -- outcome,
// kills, weapons, the loss, logistics -- shared with the inline panel. The
// bottom half comes from the Tacview recording of the same flight when there
// is one: the flight profile, the ground track, and every shot with how close
// it came.

import React, { useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Link as RouterLink, useParams } from 'react-router-dom'
import {
  ResponsiveContainer, AreaChart, Area, XAxis, YAxis, CartesianGrid, Tooltip, ReferenceLine,
} from 'recharts'
import { api } from '../api'
import PageHeader from '../components/PageHeader'
import QueryState from '../components/QueryState'
import { fmtDuration, fmtTimeZ } from '../lib/format'
import { OUTCOME, replayFor } from '../lib/sortie'
import { useRoundNo } from '../lib/rounds'
import SortieDetail from './SortieDetail'
import {
  fetchMeta, fetchObjectTrack, sampleSeg, shotsOf, objLabel, M_TO_FT, MS_TO_KT,
  type RecMeta, type Seg, type Shot,
} from '../replay/data'
import { flightStats, tasOf } from '../replay/analysis'
import { Aircraft, ChevronLeft, PlayCircle } from '@icons'

const NM = 1852
const PROFILE_POINTS = 400
const SHOT_MARK = '#ca8a04'

const label: React.CSSProperties = {
  fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase',
}

/** Seconds as "1:04:09" / "4:09". */
function clock(secs: number): string {
  const s = Math.max(0, Math.round(secs))
  const h = Math.floor(s / 3600), m = Math.floor((s % 3600) / 60), r = String(s % 60).padStart(2, '0')
  return h > 0 ? `${h}:${String(m).padStart(2, '0')}:${r}` : `${m}:${r}`
}

function Card({ title, right, children }: { title: string; right?: React.ReactNode; children: React.ReactNode }) {
  return (
    <div className="vs-card p-0">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>{title}</span>
        {right && <span className="ml-auto">{right}</span>}
      </div>
      {children}
    </div>
  )
}

function Fact({ name, value, color }: { name: string; value: React.ReactNode; color?: string }) {
  return (
    <div style={{ minWidth: 0 }}>
      <div style={label}>{name}</div>
      <div className="font-mono" style={{ fontSize: '0.9rem', color: color ?? 'var(--text)', marginTop: 2, whiteSpace: 'nowrap' }}>{value}</div>
    </div>
  )
}

// ── Flight profile ────────────────────────────────────────────────────────────

interface ProfilePoint { s: number; alt: number; kt: number }

function profile(seg: Seg, t0: number): ProfilePoint[] {
  const a = seg.t[0], b = seg.t[seg.t.length - 1]
  const n = Math.min(PROFILE_POINTS, seg.t.length)
  const out: ProfilePoint[] = []
  for (let i = 0; i < n; i++) {
    const t = a + ((b - a) * i) / Math.max(1, n - 1)
    const st = sampleSeg(seg, t)
    if (!st) continue
    const kt = tasOf(st) * MS_TO_KT
    // A dropped frame reads as a teleport; leave the impossible out.
    if (kt > 2500) continue
    out.push({ s: (t - t0) / 1000, alt: Math.round(st.alt * M_TO_FT), kt: Math.round(kt) })
  }
  return out
}

function ProfileTip({ active, payload, label: x, unit }: {
  active?: boolean; label?: number | string; unit: string
  payload?: ReadonlyArray<{ value?: unknown }>
}) {
  if (!active || !payload?.length) return null
  return (
    <div style={{
      background: 'var(--bg-elevated-solid)', border: '1px solid var(--border-light)', borderRadius: 3,
      padding: '5px 8px', fontSize: 11, color: 'var(--text-muted)',
    }}>
      <span style={{ color: 'var(--text)', fontWeight: 600 }}>{Number(payload[0].value).toLocaleString()} {unit}</span>
      <span> at {clock(Number(x))}</span>
    </div>
  )
}

function ProfileChart({ data, dataKey, title, unit, shots }: {
  data: ProfilePoint[]; dataKey: 'alt' | 'kt'; title: string; unit: string; shots: number[]
}) {
  return (
    <div>
      <div style={{ ...label, marginBottom: 6 }}>{title}</div>
      <ResponsiveContainer width="100%" height={160}>
        <AreaChart data={data} margin={{ left: -8, right: 8, top: 6, bottom: 0 }}>
          <CartesianGrid vertical={false} stroke="var(--border)" />
          <XAxis dataKey="s" type="number" domain={['dataMin', 'dataMax']} tickFormatter={clock}
            tick={{ fill: 'var(--text-dim)', fontSize: 10 }} axisLine={{ stroke: 'var(--border-light)' }} tickLine={false} minTickGap={24} />
          <YAxis tick={{ fill: 'var(--text-dim)', fontSize: 10 }} axisLine={false} tickLine={false} width={48}
            tickFormatter={(v: number) => v >= 1000 ? `${Math.round(v / 100) / 10}k` : String(v)} />
          <Tooltip cursor={{ stroke: 'var(--text-dim)', strokeWidth: 1 }} content={<ProfileTip unit={unit} />} />
          {shots.map((s, i) => <ReferenceLine key={i} x={s} stroke={SHOT_MARK} strokeOpacity={0.7} />)}
          <Area type="monotone" dataKey={dataKey} stroke="var(--accent)" strokeWidth={2} fill="var(--accent)" fillOpacity={0.1}
            dot={false} activeDot={{ r: 4, stroke: 'var(--bg-card-solid)', strokeWidth: 2 }} isAnimationActive={false} />
        </AreaChart>
      </ResponsiveContainer>
    </div>
  )
}

// ── Ground track ──────────────────────────────────────────────────────────────

const TRACK_W = 600
const TRACK_H = 300
const TRACK_PAD = 24

/** The flight's path on a flat local projection, north up, with a scale bar
 *  -- enough to see the shape of the sortie without loading a map. */
function GroundTrack({ seg, shotTimes, endColor }: { seg: Seg; shotTimes: number[]; endColor: string }) {
  const g = useMemo(() => {
    const n = seg.t.length
    const step = Math.max(1, Math.floor(n / 800))
    const lat0 = seg.lat.reduce((a, b) => a + b, 0) / n
    const kx = Math.cos((lat0 * Math.PI) / 180)
    const pts: [number, number][] = []
    for (let i = 0; i < n; i += step) pts.push([seg.lon[i] * kx, -seg.lat[i]])
    pts.push([seg.lon[n - 1] * kx, -seg.lat[n - 1]])
    const xs = pts.map(p => p[0]), ys = pts.map(p => p[1])
    const minX = Math.min(...xs), maxX = Math.max(...xs), minY = Math.min(...ys), maxY = Math.max(...ys)
    const spanDeg = Math.max(maxX - minX, maxY - minY, 1e-4)
    const scale = Math.min((TRACK_W - 2 * TRACK_PAD) / Math.max(maxX - minX, 1e-6), (TRACK_H - 2 * TRACK_PAD) / Math.max(maxY - minY, 1e-6))
    const ox = (TRACK_W - (maxX - minX) * scale) / 2, oy = (TRACK_H - (maxY - minY) * scale) / 2
    const proj = (lon: number, lat: number): [number, number] => [ox + (lon * kx - minX) * scale, oy + (-lat - minY) * scale]
    const path = pts.map(([x, y], i) => `${i ? 'L' : 'M'}${(ox + (x - minX) * scale).toFixed(1)},${(oy + (y - minY) * scale).toFixed(1)}`).join('')
    // Scale bar: a round number of nm near a quarter of the width.
    const nmPerPx = (spanDeg / Math.max(...[(maxX - minX) * scale, (maxY - minY) * scale, 1])) * 60
    const target = nmPerPx * TRACK_W * 0.25
    const nice = [1, 2, 5, 10, 20, 25, 50, 100, 200].find(v => v >= target) ?? 500
    return { path, proj, barPx: nice / nmPerPx, barNm: nice }
  }, [seg])

  const shotPts = shotTimes.flatMap(t => {
    const st = sampleSeg(seg, t)
    return st ? [g.proj(st.lon, st.lat)] : []
  })
  const n = seg.t.length
  const [sx, sy] = g.proj(seg.lon[0], seg.lat[0])
  const [ex, ey] = g.proj(seg.lon[n - 1], seg.lat[n - 1])

  return (
    <svg viewBox={`0 0 ${TRACK_W} ${TRACK_H}`} width="100%" role="img" aria-label="Ground track"
      style={{ display: 'block', maxHeight: 340, background: 'rgba(0,0,0,0.18)', border: '1px solid var(--border)', borderRadius: 2 }}>
      <path d={g.path} fill="none" stroke="var(--accent)" strokeWidth={2} strokeLinejoin="round" strokeLinecap="round" />
      {shotPts.map(([x, y], i) => (
        <path key={i} d={`M ${x} ${y - 7} l 6 10 h -12 z`} fill={SHOT_MARK} stroke="var(--bg-card-solid)" strokeWidth={2} paintOrder="stroke">
          <title>Shot {i + 1}</title>
        </path>
      ))}
      <circle cx={sx} cy={sy} r={5} fill="var(--accent)" stroke="var(--bg-card-solid)" strokeWidth={2}><title>Start</title></circle>
      <circle cx={ex} cy={ey} r={5} fill={endColor} stroke="var(--bg-card-solid)" strokeWidth={2}><title>End</title></circle>
      {/* North arrow + scale */}
      <text x={TRACK_W - 14} y={20} textAnchor="end" fontSize={11} fill="var(--text-dim)">N ↑</text>
      <line x1={14} x2={14 + g.barPx} y1={TRACK_H - 14} y2={TRACK_H - 14} stroke="var(--text-dim)" strokeWidth={1} />
      <line x1={14} x2={14} y1={TRACK_H - 18} y2={TRACK_H - 10} stroke="var(--text-dim)" strokeWidth={1} />
      <line x1={14 + g.barPx} x2={14 + g.barPx} y1={TRACK_H - 18} y2={TRACK_H - 10} stroke="var(--text-dim)" strokeWidth={1} />
      <text x={18 + g.barPx} y={TRACK_H - 10} fontSize={10} fill="var(--text-dim)">{g.barNm} nm</text>
    </svg>
  )
}

// ── Shot log ──────────────────────────────────────────────────────────────────

const RESULT: Record<Shot['result'], { text: string; color: string }> = {
  kill:    { text: 'Kill',    color: '#16a34a' },
  hit:     { text: 'Hit',     color: 'var(--text)' },
  miss:    { text: 'Miss',    color: 'var(--text-muted)' },
  unknown: { text: '—',       color: 'var(--text-dim)' },
}

function ShotLog({ shots, meta, t0 }: { shots: Shot[]; meta: RecMeta; t0: number }) {
  if (shots.length === 0) {
    return <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>No weapons fired on this flight.</div>
  }
  const th: React.CSSProperties = { ...label, padding: '4px 8px', textAlign: 'left', fontWeight: 600, borderBottom: '1px solid var(--border)', whiteSpace: 'nowrap' }
  const td: React.CSSProperties = { padding: '4px 8px', fontSize: '0.67rem', color: 'var(--text-muted)', borderBottom: '1px solid rgba(42,42,42,0.5)' }
  return (
    <div style={{ overflowX: 'auto' }}>
      <table style={{ width: '100%', minWidth: 480, borderCollapse: 'collapse' }}>
        <thead><tr>
          <th style={th}>Time</th><th style={th}>Weapon</th><th style={th}>Target</th>
          <th style={{ ...th, textAlign: 'right' }} title="Closest the weapon came to its target">Closest</th>
          <th style={th}>Result</th>
        </tr></thead>
        <tbody>
          {shots.map((sh, i) => {
            const e = sh.ev
            const w = e.w != null ? meta.objects[e.w] : undefined
            const tg = e.tg != null ? meta.objects[e.tg] : undefined
            const r = RESULT[sh.result]
            return (
              <tr key={i}>
                <td style={{ ...td, fontFamily: 'monospace', color: '#64748b' }}>{clock((e.t - t0) / 1000)}</td>
                <td style={{ ...td, color: 'var(--text)' }}>{w?.n?.replace(/_/g, ' ') ?? '—'}</td>
                <td style={td}>{tg ? objLabel(tg) : '—'}</td>
                <td style={{ ...td, textAlign: 'right', fontVariantNumeric: 'tabular-nums' }}>
                  {e.md != null ? (e.md >= 1000 ? `${(e.md / NM).toFixed(1)} nm` : `${Math.round(e.md)} m`) : '—'}
                </td>
                <td style={{ ...td, color: r.color }}>{r.text}</td>
              </tr>
            )
          })}
        </tbody>
      </table>
    </div>
  )
}

// ── Page ──────────────────────────────────────────────────────────────────────

function Tacview({ rec, idx, endColor }: { rec: string; idx: number; endColor: string }) {
  const meta = useQuery({ queryKey: ['replay-meta', rec], queryFn: () => fetchMeta(rec), staleTime: Infinity })
  const track = useQuery({
    queryKey: ['replay-track', rec, idx],
    queryFn: () => fetchObjectTrack(rec, meta.data!, idx),
    enabled: !!meta.data,
    staleTime: Infinity,
  })
  const m = meta.data
  const seg = track.data ?? null
  const obj = m?.objects[idx]
  const t0 = obj?.t0 ?? 0
  const shots = useMemo(() => (m ? shotsOf(m).filter(s => s.ev.o === idx) : []), [m, idx])
  const data = useMemo(() => (seg ? profile(seg, t0) : []), [seg, t0])
  const stats = useMemo(() => (seg && seg.t.length > 1 ? flightStats(seg) : null), [seg])

  if (meta.isLoading || track.isLoading) {
    return <div style={{ padding: 16, ...label }}>Loading the recording…</div>
  }
  if (meta.isError || track.isError || !m || !seg || !obj) {
    return <div style={{ padding: 16, fontSize: '0.67rem', color: 'var(--text-dim)' }}>The Tacview recording of this flight couldn't be loaded.</div>
  }
  const shotSecs = shots.map(s => (s.ev.t - t0) / 1000)
  const hits = shots.filter(s => s.result === 'kill' || s.result === 'hit').length

  return (
    <div className="p-4" style={{ display: 'flex', flexDirection: 'column', gap: 18 }}>
      {stats && (
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(110px, 1fr))', gap: 14 }}>
          <Fact name="Max altitude" value={`${Math.round(stats.maxAlt * M_TO_FT).toLocaleString()} ft`} />
          <Fact name="Max speed" value={`${Math.round(stats.maxSpeed * MS_TO_KT)} kt`} />
          <Fact name="Max Mach" value={stats.maxMach.toFixed(2)} />
          <Fact name="G range" value={`${stats.minG.toFixed(1)} / ${stats.maxG.toFixed(1)}`} />
          <Fact name="Distance" value={`${Math.round(stats.distanceM / NM)} nm`} />
          <Fact name="Shots · hits" value={`${shots.length} · ${hits}`} />
        </div>
      )}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))', gap: 18 }}>
        <ProfileChart data={data} dataKey="alt" title="Altitude (ft MSL)" unit="ft" shots={shotSecs} />
        <ProfileChart data={data} dataKey="kt" title="Speed (kt, true est.)" unit="kt" shots={shotSecs} />
      </div>
      <div className="flex flex-wrap items-center" style={{ marginTop: -8, gap: '2px 12px', fontSize: '0.6rem', color: 'var(--text-dim)' }}>
        <span className="flex items-center gap-1.5" style={{ whiteSpace: 'nowrap' }}><span style={{ width: 2, height: 10, background: SHOT_MARK, display: 'inline-block' }} />shot fired</span>
        <span>Time since spawn · speed is ground speed plus climb; Tacview carries no wind</span>
      </div>
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: 18, alignItems: 'start' }}>
        <div>
          <div style={{ ...label, marginBottom: 6 }}>Ground track</div>
          <GroundTrack seg={seg} shotTimes={shots.map(s => s.ev.t)} endColor={endColor} />
        </div>
        <div>
          <div style={{ ...label, marginBottom: 6 }}>Shot log</div>
          <ShotLog shots={shots} meta={m} t0={t0} />
        </div>
      </div>
    </div>
  )
}

export default function SortieDebriefPage() {
  const { ucid = '', id = '' } = useParams<{ ucid: string; id: string }>()
  const sortieId = Number(id)
  const pilot = useQuery({ queryKey: ['pilot', ucid], queryFn: () => api.pilot(ucid), enabled: !!ucid })
  const detail = useQuery({
    queryKey: ['pilot-sortie', ucid, sortieId],
    queryFn: () => api.pilotSortie(ucid, sortieId),
    enabled: !!ucid && Number.isFinite(sortieId),
    staleTime: 60_000,
  })
  const breakdown = useQuery({ queryKey: ['pilot-breakdown', ucid], queryFn: () => api.pilotBreakdown(ucid), enabled: !!ucid })
  const replays = useQuery({ queryKey: ['pilot-replays', ucid], queryFn: () => api.pilotReplays(ucid), enabled: !!ucid })

  const roundNum = useRoundNo()
  const d = detail.data
  const replay = d ? replayFor(d, replays.data ?? []) : undefined
  const theater = d ? breakdown.data?.find(b => b.round_id === d.round_id)?.scenario : undefined
  const name = pilot.data?.name ?? 'Pilot'
  const back = `/pilots?ucid=${encodeURIComponent(ucid)}`

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader art="hornet" icon={Aircraft} title="SORTIE DEBRIEF"
        sub={d ? `${name} · ${d.aircraft.replace(/_/g, ' ')}${theater ? ` · ${theater}` : ''}` : name}
        right={
          <RouterLink to={back} className="flex items-center gap-1"
            style={{ color: 'var(--text-muted)', fontSize: '0.65rem', letterSpacing: '0.1em', textDecoration: 'none' }}>
            <ChevronLeft size={12} /> {name.toUpperCase()}
          </RouterLink>
        } />

      <div className="flex-1 overflow-auto p-4 space-y-4">
        {!d ? (
          <QueryState what="this sortie" isLoading={detail.isLoading} isError={detail.isError || !detail.isLoading}
            error={detail.error ?? new Error('No such sortie')} onRetry={detail.refetch} />
        ) : (
          <>
            <Card title="Mission"
              right={replay && (
                <RouterLink to={`/replay/${encodeURIComponent(replay.rec)}?focus=${replay.i}`}
                  style={{ display: 'inline-flex', alignItems: 'center', gap: 4, color: 'var(--accent-bright)', fontSize: '0.6rem', letterSpacing: '0.08em', textDecoration: 'none' }}>
                  <PlayCircle size={12} /> WATCH REPLAY
                </RouterLink>
              )}>
              <div className="p-4" style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(120px, 1fr))', gap: 14 }}>
                <Fact name="Date" value={new Date(d.takeoff).toLocaleDateString([], { day: 'numeric', month: 'short', year: 'numeric' })} />
                <Fact name="Aircraft" value={d.aircraft.replace(/_/g, ' ')} />
                <Fact name="Theater" value={theater ?? `Round ${roundNum(d.round_id)}`} />
                <Fact name="Takeoff" value={fmtTimeZ(d.takeoff)} />
                <Fact name="End" value={d.land ? fmtTimeZ(d.land) : d.death ? fmtTimeZ(d.death.time) : '—'} />
                <Fact name="Duration" value={fmtDuration(d.duration_secs)} />
                <Fact name="Outcome" value={OUTCOME[d.outcome].label} color={OUTCOME[d.outcome].color} />
              </div>
            </Card>

            <Card title="Combat & logistics">
              <SortieDetail ucid={ucid} sortieId={sortieId} replay={replay} debriefLink={false} />
            </Card>

            <Card title="Flight profile · Tacview">
              {replay
                ? <Tacview rec={replay.rec} idx={replay.i} endColor={OUTCOME[d.outcome].color} />
                : (
                  <div style={{ padding: 16, fontSize: '0.67rem', color: 'var(--text-dim)' }}>
                    {replays.isLoading ? 'Looking for a recording…' : 'No Tacview recording covers this sortie, so there is no flight profile or shot log for it.'}
                  </div>
                )}
            </Card>
          </>
        )}
      </div>
    </div>
  )
}
