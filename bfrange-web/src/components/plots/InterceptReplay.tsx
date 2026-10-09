/**
 * Missile-trainer shot, replayed in plan view (north up, metres from the
 * launch point) with a time slider, plus both altitudes over time.
 */
import { useMemo } from 'react'
import { CartesianGrid, Line, LineChart, ReferenceLine, ResponsiveContainer, Tooltip, XAxis, YAxis } from 'recharts'
import { Pause, Play } from '@icons'
import { fmt, mToFt } from '../../lib/format'
import { NM, bearingDeg, distanceM, norm180, toLocal } from '../../lib/geo'
import type { MissileResult, TrackPt } from '../../types'
import { indexAtTime, linear, niceCeil, usePlayback } from './scale'

export function InterceptReplay({ missile, target, r }: { missile: TrackPt[]; target: TrackPt[]; r: MissileResult }) {
  const origin = missile[0] ?? target[0]
  const t0 = Math.min(missile[0]?.t ?? 0, target[0]?.t ?? 0)
  const t1 = Math.max(missile[missile.length - 1]?.t ?? 0, target[target.length - 1]?.t ?? 0)
  const pb = usePlayback(t0, t1, 3)

  const loc = useMemo(() => {
    const f = (p: TrackPt) => ({ ...toLocal(origin, p), t: p.t, alt: p.alt_m, v: p.speed_kts })
    return { m: missile.map(f), g: target.map(f) }
  }, [missile, target, origin])

  const W = 900, H = 520, pad = 24
  const all = [...loc.m, ...loc.g]
  const minE = Math.min(...all.map(p => p.east)), maxE = Math.max(...all.map(p => p.east))
  const minN = Math.min(...all.map(p => p.north)), maxN = Math.max(...all.map(p => p.north))
  const span = Math.max(maxE - minE, (maxN - minN) * (W / H), 2000) * 1.1
  const sc = (W - 2 * pad) / span
  const cE = (minE + maxE) / 2, cN = (minN + maxN) / 2
  const X = (e: number) => W / 2 + (e - cE) * sc
  const Y = (n: number) => H / 2 - (n - cN) * sc

  const mi = indexAtTime(missile, pb.t)
  const gi = indexAtTime(target, pb.t)
  const mNow = loc.m[mi], gNow = loc.g[gi]
  const liveMissile = pb.t <= (missile[missile.length - 1]?.t ?? 0)
  const range = mNow && gNow ? Math.hypot(mNow.east - gNow.east, mNow.north - gNow.north, mNow.alt - gNow.alt) : null
  // target heading from its last step, to show where the missile sits off its nose
  const gPrev = target[Math.max(0, gi - 1)]
  const gHdg = gi > 0 ? bearingDeg(gPrev, target[gi]) : null
  const offNose = gHdg !== null && missile[mi] ? Math.abs(norm180(bearingDeg(target[gi], missile[mi]) - gHdg)) : null

  // closest approach
  const cpa = useMemo(() => {
    let best = { d: Infinity, i: 0 }
    missile.forEach((p, i) => {
      const g = target[indexAtTime(target, p.t)]
      if (!g) return
      const d = Math.hypot(distanceM(p, g), p.alt_m - g.alt_m)
      if (d < best.d) best = { d, i }
    })
    return best
  }, [missile, target])

  const scaleBar = niceCeil(span / 6)
  const alts = useMemo(() => {
    const byT = new Map<number, { t: number; missile?: number; target?: number }>()
    for (const p of missile) byT.set(p.t, { ...(byT.get(p.t) ?? { t: p.t }), missile: Math.round(mToFt(p.alt_m)) })
    for (const p of target) byT.set(p.t, { ...(byT.get(p.t) ?? { t: p.t }), target: Math.round(mToFt(p.alt_m)) })
    return [...byT.values()].sort((a, b) => a.t - b.t)
  }, [missile, target])

  const trail = (pts: { east: number; north: number; t: number }[], upTo: number) =>
    pts.filter(p => p.t <= upTo).map(p => `${X(p.east)},${Y(p.north)}`).join(' ')

  const cpaM = loc.m[cpa.i]
  const tx = linear([t0, t1 || 1], [0, 1])
  void tx

  return (
    <div className="flex flex-col gap-3">
      <div className="flex flex-wrap items-center gap-3">
        <button className="btn-range sm" onClick={pb.playing ? pb.pause : pb.play}>
          {pb.playing ? <Pause size={14} /> : <Play size={14} />} {pb.playing ? 'Pause' : 'Replay'}
        </button>
        <input className="scrub flex-1 min-w-[160px]" type="range" min={t0} max={t1} step={0.1} value={pb.t} onChange={e => pb.setT(Number(e.target.value))} aria-label="Time since launch" />
        <span className="mono text-[12px] muted w-[70px] text-right">t+{fmt(pb.t - t0, 1)} s</span>
      </div>
      <div className="readout">
        <span>missile <b>{liveMissile && mNow ? `${fmt(mNow.v)} kt` : 'gone'}</b></span>
        <span>range <b>{range !== null && liveMissile ? `${fmt(range)} m` : '—'}</b></span>
        <span>off the target's nose <b>{offNose !== null && liveMissile ? `${fmt(offNose)}°` : '—'}</b></span>
        <span>target alt <b>{gNow ? `${fmt(mToFt(gNow.alt))} ft` : '—'}</b></span>
        <span className="dim">closest {fmt(r.min_distance_m)} m · kill radius {fmt(r.kill_radius_m)} m</span>
      </div>
      <svg viewBox={`0 0 ${W} ${H}`} className="plot" role="img" aria-label="Missile and target tracks, plan view">
        {Array.from({ length: 13 }, (_, i) => i - 6).map(k => (
          <g key={k}>
            <line className="grid" x1={X(cE + k * scaleBar)} x2={X(cE + k * scaleBar)} y1={0} y2={H} />
            <line className="grid" y1={Y(cN + k * scaleBar)} y2={Y(cN + k * scaleBar)} x1={0} x2={W} />
          </g>
        ))}
        <polyline points={trail(loc.g, t1)} fill="none" stroke="var(--sky)" strokeWidth={1} opacity={0.3} />
        <polyline points={trail(loc.m, t1)} fill="none" stroke="var(--wave)" strokeWidth={1} opacity={0.3} />
        <polyline points={trail(loc.g, pb.t)} fill="none" stroke="var(--sky)" strokeWidth={2.4} />
        <polyline points={trail(loc.m, pb.t)} fill="none" stroke="var(--wave)" strokeWidth={2} strokeDasharray="5 3" />
        {cpaM && (
          <g>
            <circle cx={X(cpaM.east)} cy={Y(cpaM.north)} r={Math.max(4, r.kill_radius_m * sc)} fill="none" stroke={r.outcome === 'kill' ? 'var(--wave)' : 'var(--datum)'} strokeDasharray="3 3" />
            <text x={X(cpaM.east) + 10} y={Y(cpaM.north) - 8} style={{ fill: r.outcome === 'kill' ? 'var(--wave)' : 'var(--datum)', fontWeight: 600 }}>
              {r.outcome === 'kill' ? `trainer kill · ${fmt(r.min_distance_m)} m` : `closest ${fmt(r.min_distance_m)} m`}
            </text>
          </g>
        )}
        {gNow && <circle cx={X(gNow.east)} cy={Y(gNow.north)} r={6} fill="var(--sky)" stroke="var(--plot-bg)" strokeWidth={2} />}
        {mNow && liveMissile && <circle cx={X(mNow.east)} cy={Y(mNow.north)} r={4.5} fill="var(--wave)" stroke="var(--plot-bg)" strokeWidth={2} />}
        <circle cx={X(loc.m[0]?.east ?? 0)} cy={Y(loc.m[0]?.north ?? 0)} r={4} fill="none" stroke="var(--chalk)" />
        <text x={X(loc.m[0]?.east ?? 0) + 8} y={Y(loc.m[0]?.north ?? 0) + 4} style={{ fill: 'var(--chalk)' }}>launch · {r.shooter.name}</text>
        <g transform={`translate(${W - 24}, 16)`}>
          <line x1={0} y1={30} x2={0} y2={2} stroke="var(--chalk)" strokeWidth={1.5} />
          <path d="M-4 8 0 0 4 8z" fill="var(--chalk)" />
          <text x={0} y={44} textAnchor="middle" style={{ fill: 'var(--chalk)', fontWeight: 700 }}>N</text>
        </g>
        <g transform={`translate(${pad}, ${H - 18})`}>
          <line x1={0} x2={scaleBar * sc} y1={0} y2={0} stroke="var(--chalk)" strokeWidth={2} />
          <text x={0} y={-5}>{scaleBar >= NM ? `${fmt(scaleBar / NM, 1)} NM` : `${fmt(scaleBar)} m`}</text>
        </g>
      </svg>
      <div className="flex flex-wrap gap-x-4 text-[11.5px] muted">
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--wave)' }} /> {r.weapon.replace(/_/g, '-')}</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--sky)' }} /> {r.target.name}</span>
      </div>
      <figure className="m-0">
        <figcaption className="caps mb-1">Altitude</figcaption>
        <div style={{ height: 170 }}>
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={alts} margin={{ top: 6, right: 12, bottom: 0, left: 0 }}>
              <CartesianGrid stroke="var(--plot-grid)" vertical={false} />
              <XAxis dataKey="t" type="number" domain={[t0, t1]} tickFormatter={v => `${Math.round(v - t0)}s`} stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} />
              <YAxis stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} width={52} />
              <Tooltip contentStyle={{ background: 'var(--panel)', border: '1px solid var(--line-2)', fontFamily: 'var(--font-mono)', fontSize: 12 }} labelFormatter={v => `t+${fmt(Number(v) - t0, 1)} s`} formatter={(v, n) => [`${fmt(Number(v))} ft`, String(n)]} />
              <Line type="monotone" dataKey="target" stroke="var(--sky)" dot={false} strokeWidth={2} isAnimationActive={false} connectNulls />
              <Line type="monotone" dataKey="missile" stroke="var(--wave)" dot={false} strokeWidth={1.6} strokeDasharray="5 3" isAnimationActive={false} connectNulls />
              <ReferenceLine x={pb.t} stroke="var(--ball)" />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </figure>
    </div>
  )
}
