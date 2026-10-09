/**
 * Tanker planning: fuel at the tanker, how much to take, and how long you
 * will be plugged in. Transfer rates default to what this range has actually
 * measured per tanker type, when it has.
 */
import { useMemo, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Area, AreaChart, CartesianGrid, ReferenceLine, ResponsiveContainer, Tooltip, XAxis, YAxis } from 'recharts'
import { api } from '../../api'
import { NumberField, Panel, Select } from '../../components/Controls'
import { fmt, fmtClock } from '../../lib/format'
import { RECEIVERS, TANKERS, planAar } from '../../lib/aarPlan'

export function AarCalc() {
  const [rx, setRx] = useState(RECEIVERS[0].id)
  const preset = RECEIVERS.find(r => r.id === rx)!
  const [tanker, setTanker] = useState(TANKERS[1].type)
  const [fuel, setFuel] = useState(4800)
  const [burn, setBurn] = useState(preset.burn_lb_min)
  const [dist, setDist] = useState(80)
  const [gs, setGs] = useState(preset.gs_kts)
  const [bingo, setBingo] = useState(preset.bingo_lb)
  const [mission, setMission] = useState(5000)
  const [cap, setCap] = useState(preset.capacity_lb)
  const [rateOverride, setRateOverride] = useState<number | null>(null)

  // measured transfer rates from recent AAR sessions on this range
  const recent = useQuery({ queryKey: ['results', 'aar-rates'], queryFn: () => api.results({ kind: 'aar', limit: 200, days: 90 }), staleTime: 10 * 60_000 })
  const measured = useMemo(() => {
    const m = new Map<string, { sum: number; n: number }>()
    for (const s of recent.data?.items ?? []) {
      if (s.result.kind !== 'aar' || !(s.result.onload_rate_lbs_min > 0)) continue
      const e = m.get(s.result.tanker_type) ?? { sum: 0, n: 0 }
      e.sum += s.result.onload_rate_lbs_min
      e.n++
      m.set(s.result.tanker_type, e)
    }
    return m
  }, [recent.data])
  const tk = TANKERS.find(t => t.type === tanker)!
  const meas = measured.get(tanker)
  const defaultRate = meas && meas.n >= 3 ? Math.round(meas.sum / meas.n) : tk.transfer_lb_min
  const rate = rateOverride ?? defaultRate

  function applyPreset(id: string) {
    const p = RECEIVERS.find(r => r.id === id)!
    setRx(id)
    setBurn(p.burn_lb_min)
    setGs(p.gs_kts)
    setBingo(p.bingo_lb)
    setCap(p.capacity_lb)
    const t = TANKERS.find(x => x.method === p.method)
    if (t) setTanker(t.type)
    setRateOverride(null)
  }

  const plan = useMemo(() => planAar({
    fuel_lb: fuel, burn_lb_min: burn, dist_nm: dist, gs_kts: gs, bingo_lb: bingo,
    mission_lb: mission, capacity_lb: cap, transfer_lb_min: rate,
  }), [fuel, burn, dist, gs, bingo, mission, cap, rate])

  const curve = useMemo(() => {
    const pts: { t: number; fuel: number }[] = [{ t: 0, fuel: Math.round(fuel) }]
    pts.push({ t: +plan.transit_min.toFixed(2), fuel: Math.round(plan.fuel_at_tanker_lb) })
    if (plan.feasible && Number.isFinite(plan.time_connected_min)) {
      pts.push({ t: +(plan.transit_min + plan.time_connected_min).toFixed(2), fuel: Math.round(plan.fuel_at_tanker_lb + plan.onload_lb) })
    }
    return pts
  }, [plan, fuel])
  const methodMismatch = tk.method !== preset.method

  return (
    <div className="grid gap-4 lg:grid-cols-[340px_minmax(0,1fr)]">
      <Panel title="You and the tanker">
        <div className="flex flex-col gap-3">
          <div className="grid grid-cols-2 gap-3">
            <Select label="Aircraft" value={rx} onChange={applyPreset} options={RECEIVERS.map(r => ({ value: r.id, label: r.label }))} />
            <Select label="Tanker" value={tanker} onChange={v => { setTanker(v); setRateOverride(null) }} options={TANKERS.map(t => ({ value: t.type, label: t.label }))} />
          </div>
          {methodMismatch && <div className="chip bad self-start">{tk.method === 'boom' ? 'Boom tanker, probe receiver' : 'Drogue tanker, boom receiver'}: you cannot refuel from this one</div>}
          <div className="grid grid-cols-2 gap-3">
            <NumberField label="Fuel now" unit="lb" value={fuel} onChange={setFuel} step={100} min={0} />
            <NumberField label="Cruise burn" unit="lb/min" value={burn} onChange={setBurn} step={5} min={0} />
            <NumberField label="Distance to tanker" unit="nm" value={dist} onChange={setDist} step={5} min={0} />
            <NumberField label="Transit speed" unit="kt GS" value={gs} onChange={setGs} step={10} min={1} />
            <NumberField label="Bingo" unit="lb" value={bingo} onChange={setBingo} step={100} min={0} />
            <NumberField label="Needed after, above bingo" unit="lb" value={mission} onChange={setMission} step={250} min={0} />
            <NumberField label="Tank capacity" unit="lb" value={cap} onChange={setCap} step={100} min={0} />
            <NumberField label="Transfer rate" unit="lb/min" value={rate} onChange={v => setRateOverride(v)} step={50} min={0}
              hint={meas && meas.n >= 3 ? `measured here: ${fmt(meas.sum / meas.n)} lb/min (${meas.n} sessions)` : 'typical planning figure'} />
          </div>
        </div>
      </Panel>
      <div className="flex flex-col gap-4 min-w-0">
        <div className="panel panel-b grid gap-4 grid-cols-2 md:grid-cols-4">
          <div className="kpi"><span className="caps">Onload needed</span><span className="v" style={{ color: 'var(--ball)', fontSize: 30 }}>{fmt(plan.onload_lb)}<small>lb</small></span><span className="mono text-[11px] dim">{fmt(plan.transferred_lb)} lb passed incl. burn</span></div>
          <div className="kpi"><span className="caps">Time on the boom</span><span className="v" style={{ fontSize: 30 }}>{plan.feasible ? fmtClock(plan.time_connected_min * 60) : '—'}</span><span className="mono text-[11px] dim">at {fmt(rate)} lb/min</span></div>
          <div className="kpi"><span className="caps">Fuel at the tanker</span><span className="v" style={{ color: plan.arrive_below_bingo ? 'var(--wave)' : undefined }}>{fmt(plan.fuel_at_tanker_lb)}<small>lb</small></span><span className="mono text-[11px] dim">after {fmt(plan.transit_min, 1)} min transit</span></div>
          <div className="kpi"><span className="caps">Leave with</span><span className="v">{fmt(plan.fill_to_lb)}<small>lb</small></span><span className="mono text-[11px] dim">{plan.capped ? 'tanks full: capacity limits you' : 'bingo + what you need'}</span></div>
        </div>
        {(plan.arrive_below_bingo || !plan.feasible || plan.capped) && (
          <div className="flex flex-col gap-2">
            {plan.arrive_below_bingo && <div className="panel panel-b text-[13px]" style={{ borderColor: 'var(--wave)' }}><span className="chip bad mr-2">BELOW BINGO</span>You reach the tanker with {fmt(plan.fuel_at_tanker_lb)} lb, under your {fmt(bingo)} lb bingo. If you miss the tanker you cannot get home.</div>}
            {!plan.feasible && <div className="panel panel-b text-[13px]" style={{ borderColor: 'var(--wave)' }}><span className="chip bad mr-2">NO GAIN</span>The transfer rate does not beat your burn rate; you would never fill up.</div>}
            {plan.capped && <div className="panel panel-b text-[13px]"><span className="chip warn mr-2">CAPACITY</span>You need more than your tanks hold; plan a second tanker or a shorter mission.</div>}
          </div>
        )}
        <Panel title="Fuel through the plan">
          <div style={{ height: 240 }}>
            <ResponsiveContainer width="100%" height="100%">
              <AreaChart data={curve} margin={{ top: 8, right: 16, bottom: 4, left: 4 }}>
                <CartesianGrid stroke="var(--plot-grid)" />
                <XAxis dataKey="t" type="number" domain={[0, 'dataMax']} tickFormatter={v => `${fmt(v, 0)}m`} stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} />
                <YAxis stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} width={56} domain={[0, (max: number) => Math.max(max, cap)]} />
                <Tooltip contentStyle={{ background: 'var(--panel)', border: '1px solid var(--line-2)', fontFamily: 'var(--font-mono)', fontSize: 12 }} labelFormatter={v => `${fmt(Number(v), 1)} min`} formatter={v => [`${fmt(Number(v))} lb`, 'fuel']} />
                <ReferenceLine y={bingo} stroke="var(--wave)" strokeDasharray="4 4" label={{ value: 'bingo', fill: 'var(--wave)', fontSize: 10, position: 'insideTopRight' }} />
                <ReferenceLine y={cap} stroke="var(--dim)" strokeDasharray="2 4" label={{ value: 'full', fill: 'var(--dim)', fontSize: 10, position: 'insideTopRight' }} />
                <Area type="linear" dataKey="fuel" stroke="var(--ball)" fill="var(--ball-soft)" strokeWidth={2} isAnimationActive={false} />
              </AreaChart>
            </ResponsiveContainer>
          </div>
        </Panel>
        <p className="text-[12px] dim m-0">Planning numbers only: burn, bingo and transfer rates vary with loadout, altitude and how steady you are in contact.</p>
      </div>
    </div>
  )
}
