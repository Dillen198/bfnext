import { Link } from 'react-router-dom'
import { Carrier, Refuel } from '@icons'
import { Panel } from '../../components/Controls'
import { Empty } from '../../components/States'
import { STATION_KIND_LABEL, airframe, fmt, pad3 } from '../../lib/format'
import type { LiveCarrier, LiveStation, LiveTanker, StationKind } from '../../types'

const TANKER_STATE: Record<LiveTanker['state'], { label: string; cls: string }> = {
  on_station: { label: 'ON STATION', cls: 'live' },
  spawning: { label: 'SPAWNING', cls: 'warn' },
  rtb: { label: 'RTB', cls: 'outline' },
  dead: { label: 'LOST', cls: 'bad' },
}

export function TankersPanel({ tankers }: { tankers: LiveTanker[] }) {
  const sorted = [...tankers].sort((a, b) => (a.state === 'on_station' ? 0 : 1) - (b.state === 'on_station' ? 0 : 1))
  return (
    <Panel title={<span className="inline-flex items-center gap-2"><Refuel size={14} /> Tankers on station</span>} bodyClass="">
      {sorted.length === 0 ? (
        <Empty title="No tankers up">Spawn one from the Spawn page.</Empty>
      ) : (
        <ul className="m-0 p-0 list-none">
          {sorted.map(t => (
            <li key={t.id} className="px-3 py-2.5 border-b border-[var(--line)] last:border-b-0">
              <div className="flex items-center gap-2">
                <span className="font-semibold text-[13.5px]">{t.callsign}</span>
                <span className="muted text-[12px]">{airframe(t.unit_type)} · {t.method}</span>
                <span className={`chip ${TANKER_STATE[t.state].cls} ml-auto`}>{TANKER_STATE[t.state].label}</span>
              </div>
              <div className="grid grid-cols-4 gap-2 mt-1.5 mono text-[12px]">
                <span><span className="dim block text-[10px]">TACAN</span>{t.tacan ?? '—'}</span>
                <span><span className="dim block text-[10px]">FREQ</span>{t.freq_mhz.toFixed(3)}</span>
                <span><span className="dim block text-[10px]">ALT</span>FL{Math.round(t.alt_ft / 100)}</span>
                <span><span className="dim block text-[10px]">SPEED</span>{fmt(t.speed_kts)} kt</span>
              </div>
              {(t.receivers.length > 0 || t.owner || t.recovery_for) && (
                <div className="text-[12px] muted mt-1.5 flex flex-wrap gap-x-3">
                  {t.receivers.length > 0 && <span>Receivers: <span style={{ color: 'var(--chalk)' }}>{t.receivers.join(', ')}</span></span>}
                  {t.recovery_for && <span>Recovery tanker</span>}
                  {t.owner && <span>Spawned by {t.owner}</span>}
                </div>
              )}
            </li>
          ))}
        </ul>
      )}
    </Panel>
  )
}

export function CarrierPanel({ carriers }: { carriers: LiveCarrier[] }) {
  return (
    <Panel title={<span className="inline-flex items-center gap-2"><Carrier size={14} /> Carrier ops</span>} bodyClass="">
      {carriers.length === 0 ? (
        <Empty title="No carrier on the range" />
      ) : (
        carriers.map(c => (
          <div key={c.id} className="px-3 py-3 border-b border-[var(--line)] last:border-b-0">
            <div className="flex items-center gap-2 flex-wrap">
              <span className="font-semibold">{c.name}</span>
              <span className={`chip ${c.recovery_open ? 'live' : 'outline'} ml-auto`}>
                {c.recovery_open ? <><span className="dot live" /> RECOVERY OPEN</> : 'DECK CLOSED'}
              </span>
            </div>
            {c.next_window && <div className="text-[12px] muted mt-0.5">Window {c.next_window}</div>}
            <div className="grid grid-cols-3 gap-2 mt-2.5">
              <div className="kpi"><span className="caps">BRC</span><span className="v">{pad3(c.brc_deg)}</span></div>
              <div className="kpi"><span className="caps">FB</span><span className="v">{pad3(c.fb_deg)}</span></div>
              <div className="kpi">
                <span className="caps">WOD</span>
                <span className="v" style={{ color: c.wind_over_deck_kts >= 25 && c.wind_over_deck_kts <= 30 ? 'var(--datum)' : undefined }}>
                  {fmt(c.wind_over_deck_kts, 1)}<small>kt</small>
                </span>
              </div>
            </div>
            <dl className="kv mt-2.5">
              <dt>Wind off the angled deck</dt><dd>{c.wind_over_deck_angle_deg > 0 ? 'R' : 'L'} {fmt(Math.abs(c.wind_over_deck_angle_deg), 1)}°</dd>
              <dt>Ship speed · Case</dt><dd>{fmt(c.speed_kts)} kt · {['I', 'II', 'III'][c.case - 1] ?? c.case}</dd>
              <dt>TACAN · ICLS</dt><dd>{c.tacan ?? '—'} · {c.icls !== null ? `ch ${c.icls}` : '—'}</dd>
              <dt>Link-4 · tower</dt><dd>{c.link4_mhz !== null ? c.link4_mhz.toFixed(1) : '—'} · {c.tower_mhz !== null ? c.tower_mhz.toFixed(3) : '—'}</dd>
              <dt>Recovery tanker</dt><dd>{c.recovery_tanker ?? '—'}</dd>
            </dl>
            <div className="text-[12px] mt-2">
              <span className="muted">In the pattern: </span>
              {c.pattern.length ? c.pattern.join(', ') : <span className="dim">nobody</span>}
            </div>
          </div>
        ))
      )}
    </Panel>
  )
}

const GROUPABLE: StationKind[] = ['bomb_circle', 'tactical_array', 'laser_target', 'coord_target', 'convoy']

export function StationsPanel({ stations }: { stations: LiveStation[] }) {
  return (
    <Panel title="Stations" bodyClass="">
      <ul className="m-0 p-0 list-none">
        {stations.map(s => {
          const hot = s.hot_by.length > 0
          const row = (
            <>
              <div className="flex items-center gap-2 min-w-0">
                <span className={`dot ${hot ? 'live' : ''}`} style={hot ? undefined : { background: 'var(--line-2)' }} />
                <span className="font-medium text-[13px] truncate">{s.name}</span>
                <span className="mono text-[12px] ml-auto shrink-0" title="targets alive">
                  {s.targets_alive}/{s.targets_total}
                </span>
              </div>
              <div className="text-[12px] muted pl-[15px] flex flex-wrap gap-x-2">
                <span>{STATION_KIND_LABEL[s.kind]}</span>
                {s.laser_code !== null && <span className="mono">laser {s.laser_code}</span>}
                {hot ? <span style={{ color: 'var(--datum)' }}>hot: {s.hot_by.join(', ')}</span> : <span>cold</span>}
              </div>
            </>
          )
          return (
            <li key={s.id} className="border-b border-[var(--line)] last:border-b-0">
              {GROUPABLE.includes(s.kind) ? (
                <Link to={`/stations/${encodeURIComponent(s.id)}`} className="block px-3 py-2 hover:bg-[var(--panel-2)]" title="Open this station's impact group">
                  {row}
                </Link>
              ) : (
                <div className="px-3 py-2">{row}</div>
              )}
            </li>
          )
        })}
      </ul>
    </Panel>
  )
}
