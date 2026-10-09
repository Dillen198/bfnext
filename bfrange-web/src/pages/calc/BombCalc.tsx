/**
 * Bomb release calculator. Runs the same point-mass model bfdb calibrates
 * against real drops (src/lib/ballistics.ts, a port of
 * bfdb/src/range/ballistics.rs), with the weapon's fitted drag scale when the
 * range has enough recorded drops of it.
 *
 * The target and weather come from the running mission by default: the
 * chosen station's ground height and DCS's own wind, temperature and pressure
 * over it at every height, so the bomb is flown through the atmosphere DCS
 * will really fly it through (LIVE · DCS). Typing a wind or an elevation
 * turns that into a what-if with one constant wind and a standard atmosphere
 * (EDITED); a debrief's "open in the calculator" link starts there too.
 */
import { useMemo, useState } from 'react'
import { useSearchParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { CartesianGrid, Legend, Line, LineChart, ResponsiveContainer, Tooltip, XAxis, YAxis } from 'recharts'
import { api } from '../../api'
import { NumberField, Panel, Select } from '../../components/Controls'
import { ErrorState, Loading } from '../../components/States'
import {
  KT,
  atmoAt,
  bodyFromWeapon,
  cdRef,
  groundSpeedFromTas,
  makeAtmo,
  metAt,
  trajectory,
  windComponents,
  type Launch,
} from '../../lib/ballistics'
import { fmt, fmtSigned, pad3, weaponName } from '../../lib/format'
import { FT, NM } from '../../lib/geo'
import { useLivePicture } from '../../lib/useLivePicture'
import type { AtmoLayer, RangeLive, StationKind } from '../../types'
import { SourceChip, SourceNote, type Source } from './LiveSource'

const num = (v: string | null, d: number) => (v !== null && v !== '' && Number.isFinite(Number(v)) ? Number(v) : d)

/** Stations you drop bombs on, in the order the default is picked from. */
const BOMBING: StationKind[] = ['bomb_circle', 'tactical_array', 'laser_target', 'coord_target', 'convoy', 'ship_target']

/** What the fields hold when there is nothing live to start from. */
const DEFAULT_WIND_FROM = 270
const DEFAULT_WIND_KTS = 15
const DEFAULT_ELEV_FT = 0

function Out({ label, value, unit, sub, accent }: { label: string; value: string; unit?: string; sub?: string; accent?: boolean }) {
  return (
    <div className="kpi">
      <span className="caps">{label}</span>
      <span className="v" style={accent ? { color: 'var(--ball)', fontSize: 30 } : undefined}>
        {value}{unit && <small>{unit}</small>}
      </span>
      {sub && <span className="mono text-[11.5px] dim">{sub}</span>}
    </div>
  )
}

const windText = (from: number, kts: number) => (kts < 0.5 ? 'calm' : `${pad3(from)}° / ${fmt(kts)} kt`)

/**
 * The chosen station (the `?station=` one, else the first bombing station),
 * its ground height and DCS's atmosphere over it -- or over the range
 * reference point when the engine sent none for the station.
 */
function readLive(picture: RangeLive | null, stationId: string) {
  const stations = picture?.stations ?? []
  const station =
    stations.find(s => s.id === stationId) ??
    BOMBING.map(k => stations.find(s => s.kind === k)).find(Boolean) ??
    stations[0] ??
    null
  const profile = [station?.atmo, picture?.wind.layers].find(l => makeAtmo(l, 0) !== null) ?? null
  const elev = station ? (station.elev_m ?? station.pos.alt_m) : null
  return { picture, stations, station, profile, elevM: elev !== null && Number.isFinite(elev) ? elev : null }
}

/**
 * DCS's winds over the station, the way a pilot reads a winds-aloft chart,
 * with the release height slotted in at the wind the calculator uses there.
 */
function WindsAloft({ layers, releaseMsl }: { layers: AtmoLayer[]; releaseMsl: number }) {
  const rel = metAt(layers, releaseMsl)
  const rows = [
    ...layers.map(l => ({ alt_m: l.alt_m, from: l.wind_from_deg, kts: l.wind_kts, temp: l.temp_c, release: false })),
    ...(rel ? [{ alt_m: releaseMsl, from: rel.from_deg, kts: rel.kts, temp: rel.temp_c, release: true }] : []),
  ].sort((a, b) => a.alt_m - b.alt_m)
  return (
    <details className="text-[12px]">
      <summary className="cursor-pointer muted">DCS winds aloft over this station</summary>
      <div className="table-scroll mt-2">
        <table className="table-range">
          <thead>
            <tr><th className="n">Altitude MSL</th><th className="n">Wind</th><th className="n">Temp</th></tr>
          </thead>
          <tbody>
            {rows.map((r, i) => (
              <tr key={i} style={r.release ? { color: 'var(--ball)' } : undefined}>
                <td className="n">{r.release ? 'release ' : ''}{fmt(r.alt_m / FT)} ft</td>
                <td className="n">{windText(r.from, r.kts)}</td>
                <td className="n">{fmt(r.temp)} °C</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </details>
  )
}

export function BombCalc() {
  const [sp] = useSearchParams()
  const q = useQuery({ queryKey: ['weapons'], queryFn: () => api.weapons(), staleTime: 10 * 60_000 })
  const live = useLivePicture()
  const [weapon, setWeapon] = useState(sp.get('w') ?? '')
  const [altFt, setAltFt] = useState(num(sp.get('alt'), 8000))
  const [tas, setTas] = useState(num(sp.get('tas'), 450))
  const [dive, setDive] = useState(num(sp.get('dive'), 20))
  const [track, setTrack] = useState(num(sp.get('hdg'), 90))
  // the user's own conditions, used once they have edited (or the link
  // carried them, e.g. a debrief's recorded release)
  const [windFrom, setWindFrom] = useState(num(sp.get('wf'), DEFAULT_WIND_FROM))
  const [windKts, setWindKts] = useState(num(sp.get('wk'), DEFAULT_WIND_KTS))
  const [elevFt, setElevFt] = useState(num(sp.get('elev'), DEFAULT_ELEV_FT))
  const [edited, setEdited] = useState(['wf', 'wk', 'elev'].some(k => sp.get(k) !== null && sp.get(k) !== ''))
  const [stationId, setStationId] = useState(sp.get('station') ?? '')

  const bombs = useMemo(() => {
    const list = q.data?.db?.bombs ?? []
    return [...list].sort((a, b) => (a.class === b.class ? a.name.localeCompare(b.name) : a.class === 'unguided' ? -1 : 1))
  }, [q.data])
  const w = bombs.find(b => b.name === weapon) ?? bombs.find(b => b.class === 'unguided') ?? bombs[0]
  const cal = q.data?.calibration.find(c => c.weapon === w?.name)

  // ── the live picture: which station, its ground and DCS's air over it
  const lv = useMemo(() => readLive(live.data?.live ?? null, stationId), [live.data, stationId])
  const { picture, stations, station, profile } = lv
  const liveOk = profile !== null && lv.elevM !== null
  const source: Source = edited ? 'edited' : live.isLoading ? 'loading' : liveOk ? 'live' : 'none'
  const useLive = source === 'live'

  // what the model flies with, in either mode
  const elevM = useLive && lv.elevM !== null ? lv.elevM : elevFt * FT
  const releaseMsl = elevM + altFt * FT
  const atRelease = useLive ? metAt(profile, releaseMsl) : null
  const atGround = useLive ? metAt(profile, elevM) : null
  const shownWindFrom = atRelease ? atRelease.from_deg : windFrom
  const shownWindKts = atRelease ? atRelease.kts : windKts
  const shownElevFt = useLive ? elevM / FT : elevFt

  /** Any edit to the weather or ground turns the live values into a what-if. */
  function editConditions(apply: () => void) {
    if (useLive) {
      // start the what-if from what DCS says right now, not the old defaults
      setWindFrom(Math.round(shownWindFrom))
      setWindKts(Math.round(shownWindKts))
      setElevFt(Math.round(shownElevFt))
    }
    apply()
    setEdited(true)
  }

  const out = useMemo(() => {
    if (!w) return null
    const body = bodyFromWeapon(w)
    if (!body) return { error: 'This weapon has no usable mass or caliber in the DCS database.' as const }
    // bfdb reports the Cd_ref it fitted with; use it so k means the same thing
    if (cal?.cd_ref !== undefined) body.cd_ref = cal.cd_ref
    // live: DCS's layered wind and density; edited: constant wind + ISA
    const atmo = useLive ? makeAtmo(lv.profile, track) : null
    let wa: number, wc: number
    if (atmo) {
      const air = atmoAt(atmo, releaseMsl)
      wa = air.wind_along_mps
      wc = air.wind_cross_mps
    } else {
      ;[wa, wc] = windComponents(track, windFrom, windKts)
    }
    const gs = groundSpeedFromTas(tas * KT, dive, wa, wc)
    if (!Number.isFinite(gs) || gs <= 0) return { error: 'The wind is stronger than the airspeed: no release solution.' as const }
    const launch: Launch = { z0_m: altFt * FT, ground_msl_m: elevM, gs_mps: gs, dive_deg: dive, wind_along_mps: wa, wind_cross_mps: wc, atmo }
    if (!(launch.z0_m > 0)) return { error: 'Release altitude must be above the target.' as const }
    const k = cal?.drag_scale ?? 1
    const tr = trajectory(body, launch, k)
    const vac = trajectory(body, launch, 0)
    return { tr, vac, gs, k, body, wc }
  }, [w, cal, useLive, lv, track, windFrom, windKts, tas, dive, altFt, elevM, releaseMsl])

  const chart = useMemo(() => {
    if (!out || 'error' in out) return []
    // merge both curves on a shared downrange axis (metres -> feet above target)
    const a = out.tr.points.map(p => ({ x: Math.round(p.x), drag: Math.round(p.z / FT) }))
    const b = out.vac.points.map(p => ({ x: Math.round(p.x), vacuum: Math.round(p.z / FT) }))
    return [...a, ...b].sort((m, n) => m.x - n.x)
  }, [out])

  if (q.isLoading) return <Loading label="Loading the weapon database" />
  if (q.error) return <ErrorState error={q.error} retry={() => q.refetch()} />
  if (!bombs.length) {
    return <div className="panel panel-b muted">The range server has not published its weapon database yet, so there is nothing to calculate with. Try again when the range is up.</div>
  }

  const liveHint = (s: string) => (useLive ? <span style={{ color: 'var(--datum)' }}>{s}</span> : undefined)

  return (
    <div className="grid gap-4 lg:grid-cols-[340px_minmax(0,1fr)]">
      <div className="flex flex-col gap-4 min-w-0">
        <Panel title="Release">
          <div className="flex flex-col gap-3">
            <Select
              label="Weapon"
              value={w?.name ?? ''}
              onChange={setWeapon}
              options={bombs.map(b => ({
                value: b.name,
                label: `${b.display_name || weaponName(b.name)}${b.class === 'guided' ? ' (guided, flown ballistic)' : ''}${q.data?.calibration.some(c => c.weapon === b.name) ? ' · calibrated' : ''}`,
              }))}
            />
            <div className="grid grid-cols-2 gap-3">
              <NumberField label="Height above target" unit="ft" value={altFt} onChange={setAltFt} step={100} min={0}
                hint={`${fmt(releaseMsl / FT)} ft above sea level`} />
              <NumberField label="True airspeed" unit="kt" value={tas} onChange={setTas} step={5} min={0} hint="TAS, not indicated" />
              <NumberField label="Dive angle" unit="°" value={dive} onChange={setDive} step={1} min={-80} max={85} hint="0 = level, + = diving" />
              <NumberField label="Attack course" unit="° true" value={track} onChange={setTrack} step={1} min={0} max={360} hint="your track over the ground" />
            </div>
            {w && (
              <div className="text-[12px] muted flex flex-col gap-1 border-t border-[var(--line)] pt-3">
                <span className="mono">{fmt(w.mass_kg)} kg · Ø {fmt(w.caliber_m * 1000)} mm · Cd_ref {fmt(cal?.cd_ref ?? cdRef(w.cx_coeff), 2)}</span>
                {cal ? (
                  <span style={{ color: 'var(--datum)' }}>
                    Calibrated from {cal.samples} real drops, ±{fmt(cal.residual_m, 1)} m (drag × {fmt(cal.drag_scale, 3)})
                    {cal.layered ? `, ${cal.layered} of them through the weather DCS recorded at the time` : ''}
                  </span>
                ) : (
                  <span style={{ color: 'var(--ball)' }}>Uncalibrated: not enough recorded drops of this weapon yet (drag × 1.0)</span>
                )}
                {q.data?.db?.dcs_version && <span className="dim">DCS {q.data.db.dcs_version} weapon data</span>}
              </div>
            )}
          </div>
        </Panel>

        <Panel title="Target and weather" right={<SourceChip source={source} />}>
          <div className="flex flex-col gap-3">
            {stations.length > 0 && (
              <Select
                label="Station"
                value={station?.id ?? ''}
                onChange={id => { setStationId(id); setEdited(false) }}
                options={stations.map(s => ({ value: s.id, label: s.name }))}
              />
            )}
            <div className="grid grid-cols-2 gap-3">
              <NumberField label="Wind from" unit="° true" value={Math.round(shownWindFrom)} step={5} min={0} max={360}
                onChange={v => editConditions(() => setWindFrom(v))}
                hint={liveHint('at release height') ?? 'the direction it blows from'} />
              <NumberField label="Wind speed" unit="kt" value={Math.round(shownWindKts)} step={1} min={0}
                onChange={v => editConditions(() => setWindKts(v))}
                hint={liveHint('at release height') ?? 'the same at every height'} />
              <NumberField label="Target elevation" unit="ft MSL" value={Math.round(shownElevFt)} step={50}
                onChange={v => editConditions(() => setElevFt(v))}
                hint={liveHint('ground height, from DCS') ?? 'ground height above sea level'} />
            </div>
            {useLive && atRelease && atGround && (
              <dl className="kv m-0">
                <dt>Surface wind</dt>
                <dd>{windText(atGround.from_deg, atGround.kts)}</dd>
                <dt>Wind at release ({fmt(releaseMsl / FT)} ft)</dt>
                <dd>{windText(atRelease.from_deg, atRelease.kts)}</dd>
                <dt>Air at the target</dt>
                <dd>{fmt(atGround.temp_c)} °C · {fmt(atGround.pressure_hpa)} hPa</dd>
              </dl>
            )}
            <SourceNote
              source={source}
              updatedAt={live.dataUpdatedAt}
              canUseLive={liveOk}
              onUseLive={() => setEdited(false)}
              live={<>The bomb is flown through DCS’s own wind and air at every height from release to impact over {station?.name ?? 'the range'}.</>}
              edited={<>Your numbers: one wind at every height and a standard atmosphere.{liveOk ? '' : ' The range server is not sending live weather right now.'}</>}
              none={<>{picture ? 'The range server is not sending DCS weather for this station' : 'The range server is not running'}, so these are defaults: enter the wind and target elevation yourself.</>}
            />
            {useLive && profile && <WindsAloft layers={profile} releaseMsl={releaseMsl} />}
          </div>
        </Panel>
      </div>

      <div className="flex flex-col gap-4 min-w-0">
        {!out ? null : 'error' in out ? (
          <div className="panel panel-b" style={{ borderColor: 'var(--wave)' }}>{out.error}</div>
        ) : (
          <>
            <div className="panel panel-b grid gap-4 grid-cols-2 md:grid-cols-4">
              <Out accent label="Release before target" value={fmt(out.tr.impact.along_m)} unit="m"
                sub={`${fmt(out.tr.impact.along_m / NM, 2)} nm · ${fmt(out.tr.impact.along_m / FT)} ft`} />
              <Out label="Time of fall" value={fmt(out.tr.impact.tof_s, 1)} unit="s" />
              <Out label="Impact angle" value={fmt(out.tr.impact_angle_deg, 1)} unit="°" />
              <Out label="Impact speed" value={fmt(out.tr.impact_speed_mps / KT)} unit="kt" />
              <Out label="Ground speed" value={fmt(out.gs / KT)} unit="kt" sub="crabbed onto the attack course" />
              <Out label="Wind drift" value={fmtSigned(out.tr.impact.cross_m, 1)} unit="m"
                sub={Math.abs(out.tr.impact.cross_m) < 0.5 ? 'none' : `aim ${fmt(Math.abs(out.tr.impact.cross_m), 1)} m ${out.tr.impact.cross_m > 0 ? 'left' : 'right'} of the target`} />
              <Out label="Slant range" value={fmt(Math.hypot(out.tr.impact.range_m, altFt * FT))} unit="m" />
              <Out label="Drag effect" value={fmt(out.vac.impact.along_m - out.tr.impact.along_m)} unit="m" sub="shorter than in a vacuum" />
            </div>
            <Panel title="Trajectory · height above the target against distance from release">
              <div style={{ height: 300 }}>
                <ResponsiveContainer width="100%" height="100%">
                  <LineChart data={chart} margin={{ top: 8, right: 16, bottom: 4, left: 4 }}>
                    <CartesianGrid stroke="var(--plot-grid)" />
                    <XAxis dataKey="x" type="number" domain={[0, 'dataMax']} tickFormatter={v => `${fmt(v / 1000, 1)}k`} stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} label={{ value: 'metres downrange', position: 'insideBottomRight', offset: -2, fill: 'var(--dim)', fontSize: 10 }} />
                    <YAxis stroke="var(--dim)" tick={{ fontSize: 10, fontFamily: 'var(--font-mono)' }} width={54} label={{ value: 'ft', angle: -90, position: 'insideLeft', fill: 'var(--dim)', fontSize: 10 }} />
                    <Tooltip contentStyle={{ background: 'var(--panel)', border: '1px solid var(--line-2)', fontFamily: 'var(--font-mono)', fontSize: 12 }} labelFormatter={v => `${fmt(Number(v))} m downrange`} formatter={(v, n) => [`${fmt(Number(v))} ft`, n === 'drag' ? 'with drag' : 'vacuum']} />
                    <Legend formatter={v => (v === 'drag' ? `with drag (× ${fmt(out.k, 2)})` : 'vacuum')} wrapperStyle={{ fontSize: 12 }} />
                    <Line type="monotone" dataKey="drag" stroke="var(--ball)" strokeWidth={2.2} dot={false} connectNulls isAnimationActive={false} />
                    <Line type="monotone" dataKey="vacuum" stroke="var(--haze)" strokeDasharray="4 4" strokeWidth={1.4} dot={false} connectNulls isAnimationActive={false} />
                  </LineChart>
                </ResponsiveContainer>
              </div>
            </Panel>
            <p className="text-[12px] dim m-0">
              {useLive
                ? 'Point-mass model flown through the wind, temperature and pressure DCS reports at every height over the station, '
                : 'Point-mass model with one constant wind and a standard atmosphere, '}
              with an RK4 integrator at 0.02 s: the same equations bfdb uses to calibrate each weapon’s drag against the drops recorded on this range.
              It does not model the release itself (ejector velocity, pitch rate) or a bomb’s time of flight to fuze arming.
            </p>
          </>
        )}
      </div>
    </div>
  )
}
