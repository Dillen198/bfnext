/**
 * AAR session in the tanker's frame: side (fore-aft vs up) and top (fore-aft
 * vs right) scatters of the receiver, a contact timeline and the fuel curve,
 * all tied to one time cursor.
 */
import { useMemo, useState } from 'react'
import { Area, AreaChart, CartesianGrid, ReferenceLine, ResponsiveContainer, Tooltip, XAxis, YAxis } from 'recharts'
import { Pause, Play } from '@icons'
import { fmt, fmtClock } from '../../lib/format'
import { AAR_STABILITY_LIMITS } from '../../lib/grading'
import type { AarResult, RelSample } from '../../types'
import { indexAtTime, linear, ticks, usePlayback } from './scale'

const KG_TO_LB = 2.20462

function Scatter({
  samples, cur, h, v, hLabel, vLabel, domH, domV, mean, sd,
}: {
  samples: RelSample[]
  cur: RelSample | null
  h: (s: RelSample) => number
  v: (s: RelSample) => number
  hLabel: string
  vLabel: string
  domH: [number, number]
  domV: [number, number]
  mean: [number, number]
  sd: [number, number]
}) {
  const W = 440, H = 300, m = { l: 40, r: 12, t: 12, b: 30 }
  const x = linear(domH, [m.l, W - m.r])
  const y = linear(domV, [H - m.b, m.t])
  const inside = (s: RelSample) => h(s) >= domH[0] && h(s) <= domH[1] && v(s) >= domV[0] && v(s) <= domV[1]
  return (
    <svg viewBox={`0 0 ${W} ${H}`} className="plot" role="img" aria-label={`${vLabel} against ${hLabel}`}>
      {ticks(domH[0], domH[1], 6).map(t => (
        <g key={`h${t}`}>
          <line className="grid" x1={x(t)} x2={x(t)} y1={m.t} y2={H - m.b} />
          <text x={x(t)} y={H - m.b + 13} textAnchor="middle">{t}</text>
        </g>
      ))}
      {ticks(domV[0], domV[1], 5).map(t => (
        <g key={`v${t}`}>
          <line className="grid" x1={m.l} x2={W - m.r} y1={y(t)} y2={y(t)} />
          <text x={m.l - 5} y={y(t) + 3} textAnchor="end">{t}</text>
        </g>
      ))}
      <text className="axis-label" x={W - m.r} y={H - 3} textAnchor="end">{hLabel}</text>
      <text className="axis-label" x={11} y={m.t} transform={`rotate(-90 11 ${m.t})`} textAnchor="end">{vLabel}</text>
      {/* the stability box: ±1 SD around the mean contact position */}
      <rect x={x(mean[0] - sd[0])} y={y(mean[1] + sd[1])} width={x(mean[0] + sd[0]) - x(mean[0] - sd[0])} height={y(mean[1] - sd[1]) - y(mean[1] + sd[1])}
        fill="var(--datum-soft)" stroke="var(--datum)" strokeDasharray="3 3" />
      {samples.filter(inside).map((s, i) => (
        <circle key={i} cx={x(h(s))} cy={y(v(s))} r={s.connected ? 1.9 : 1.4} fill={s.connected ? 'var(--datum)' : 'var(--haze)'} opacity={s.connected ? 0.65 : 0.4} />
      ))}
      {cur && inside(cur) && <circle cx={x(h(cur))} cy={y(v(cur))} r={6} fill="var(--ball)" stroke="var(--plot-bg)" strokeWidth={2} />}
    </svg>
  )
}

export function AarPlots({ samples, r }: { samples: RelSample[]; r: AarResult }) {
  const [zoom, setZoom] = useState<'contact' | 'session'>('contact')
  const t0 = samples[0]?.t ?? 0
  const t1 = samples[samples.length - 1]?.t ?? 0
  const pb = usePlayback(t0, t1, 12)
  const cur = samples.length ? samples[indexAtTime(samples, pb.t)] : null
  const st = r.stability

  const doms = useMemo(() => {
    if (zoom === 'contact') {
      return {
        fwd: [st.mean_fwd_m - 22, st.mean_fwd_m + 10] as [number, number],
        up: [st.mean_up_m - 10, st.mean_up_m + 8] as [number, number],
        right: [st.mean_right_m - 14, st.mean_right_m + 14] as [number, number],
      }
    }
    const f = samples.map(s => s.fwd_m), u = samples.map(s => s.up_m), g = samples.map(s => s.right_m)
    const pad = (lo: number, hi: number): [number, number] => [lo - (hi - lo) * 0.05, hi + (hi - lo) * 0.05]
    return { fwd: pad(Math.min(...f), Math.max(...f)), up: pad(Math.min(...u), Math.max(...u)), right: pad(Math.min(...g), Math.max(...g)) }
  }, [zoom, samples, st])

  // contact segments for the timeline
  const segs = useMemo(() => {
    const out: { a: number; b: number }[] = []
    let start: number | null = null
    samples.forEach((s, i) => {
      if (s.connected && start === null) start = s.t
      if ((!s.connected || i === samples.length - 1) && start !== null) {
        out.push({ a: start, b: s.t })
        start = null
      }
    })
    return out
  }, [samples])

  const fuel = useMemo(() => samples.filter((_, i) => i % 3 === 0).map(s => ({ t: s.t, lb: Math.round(s.fuel_kg * KG_TO_LB) })), [samples])
  const TW = 900
  const tx = linear([t0, t1 || 1], [8, TW - 8])

  return (
    <div className="flex flex-col gap-3">
      <div className="flex flex-wrap items-center gap-3">
        <button className="btn-range sm" onClick={pb.playing ? pb.pause : pb.play} aria-label={pb.playing ? 'Pause' : 'Play'}>
          {pb.playing ? <Pause size={14} /> : <Play size={14} />} {pb.playing ? 'Pause' : 'Replay'}
        </button>
        <input className="scrub flex-1 min-w-[160px]" type="range" min={t0} max={t1} step={1} value={pb.t} onChange={e => pb.setT(Number(e.target.value))} aria-label="Session time" />
        <span className="mono text-[12px] muted w-[64px] text-right">{fmtClock(pb.t - t0)}</span>
        <div className="seg" role="group" aria-label="Zoom">
          <button aria-pressed={zoom === 'contact'} onClick={() => setZoom('contact')}>Contact zone</button>
          <button aria-pressed={zoom === 'session'} onClick={() => setZoom('session')}>Whole session</button>
        </div>
      </div>
      {cur && (
        <div className="readout">
          <span>{cur.connected ? <b style={{ color: 'var(--datum)' }}>CONNECTED</b> : <b>not connected</b>}</span>
          <span>fwd <b>{fmt(cur.fwd_m, 1)} m</b></span>
          <span>right <b>{fmt(cur.right_m, 1)} m</b></span>
          <span>up <b>{fmt(cur.up_m, 1)} m</b></span>
          <span>closure <b>{fmt(cur.closure_kts, 1)} kt</b></span>
          <span>taken <b>{fmt(cur.fuel_kg * KG_TO_LB)} lb</b></span>
        </div>
      )}
      <div className="grid md:grid-cols-2 gap-3">
        <figure className="m-0">
          <figcaption className="caps mb-1">Side · behind the tanker</figcaption>
          <Scatter samples={samples} cur={cur} h={s => s.fwd_m} v={s => s.up_m} hLabel="m ahead of the tanker ref" vLabel="m above"
            domH={doms.fwd} domV={doms.up} mean={[st.mean_fwd_m, st.mean_up_m]} sd={[st.fore_aft_sd_m, st.vertical_sd_m]} />
        </figure>
        <figure className="m-0">
          <figcaption className="caps mb-1">Top · behind the tanker</figcaption>
          <Scatter samples={samples} cur={cur} h={s => s.fwd_m} v={s => s.right_m} hLabel="m ahead of the tanker ref" vLabel="m right"
            domH={doms.fwd} domV={doms.right} mean={[st.mean_fwd_m, st.mean_right_m]} sd={[st.fore_aft_sd_m, st.lateral_sd_m]} />
        </figure>
      </div>
      <div className="text-[11.5px] muted flex flex-wrap gap-x-4">
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--datum)' }} /> connected</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--haze)' }} /> not connected</span>
        <span>dashed box = ±1 SD in contact · grading limits fore-aft {AAR_STABILITY_LIMITS.fore_aft} m, lateral {AAR_STABILITY_LIMITS.lateral} m, vertical {AAR_STABILITY_LIMITS.vertical} m</span>
      </div>
      <figure className="m-0">
        <figcaption className="caps mb-1">Contact timeline · {r.contacts} contact{r.contacts === 1 ? '' : 's'}, {r.disconnects} disconnect{r.disconnects === 1 ? '' : 's'}</figcaption>
        <svg viewBox={`0 0 ${TW} 44`} className="plot" role="img" aria-label="Contact timeline">
          <rect x={8} y={12} width={TW - 16} height={14} fill="var(--plot-grid)" />
          {segs.map((s, i) => (
            <rect key={i} x={tx(s.a)} y={12} width={Math.max(2, tx(s.b) - tx(s.a))} height={14} fill="var(--datum)" />
          ))}
          {r.join_time_s !== null && (
            <g>
              <line x1={tx(t0 + r.join_time_s)} x2={tx(t0 + r.join_time_s)} y1={6} y2={32} stroke="var(--haze)" strokeDasharray="2 3" />
              <text x={tx(t0 + r.join_time_s) + 4} y={40}>first contact {fmtClock(r.join_time_s)}</text>
            </g>
          )}
          <line x1={tx(pb.t)} x2={tx(pb.t)} y1={4} y2={34} stroke="var(--ball)" strokeWidth={2} />
        </svg>
      </figure>
      <figure className="m-0">
        <figcaption className="caps mb-1">Fuel taken · {fmt(r.fuel_lbs)} lb at {fmt(r.onload_rate_lbs_min)} lb/min</figcaption>
        <div style={{ height: 180 }}>
          <ResponsiveContainer width="100%" height="100%">
            <AreaChart data={fuel} margin={{ top: 8, right: 12, bottom: 0, left: 0 }}>
              <CartesianGrid stroke="var(--plot-grid)" vertical={false} />
              <XAxis dataKey="t" type="number" domain={[t0, t1]} tickFormatter={v => fmtClock(v - t0)} stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} />
              <YAxis stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} width={48} />
              <Tooltip
                contentStyle={{ background: 'var(--panel)', border: '1px solid var(--line-2)', fontFamily: 'var(--font-mono)', fontSize: 12 }}
                labelFormatter={v => fmtClock(Number(v) - t0)}
                formatter={v => [`${fmt(Number(v))} lb`, 'taken']}
              />
              <Area type="stepAfter" dataKey="lb" stroke="var(--datum)" fill="var(--datum-soft)" strokeWidth={2} isAnimationActive={false} />
              <ReferenceLine x={pb.t} stroke="var(--ball)" strokeWidth={1.5} />
            </AreaChart>
          </ResponsiveContainer>
        </div>
      </figure>
    </div>
  )
}
