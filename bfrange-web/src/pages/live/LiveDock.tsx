/**
 * The control dock beside the live map: the situation (clock, wind,
 * altimeter) on top, then one tab per job instead of a wall of panels:
 * results as they land, traffic (pilots, tankers, spawns), the boat, threats
 * and stations. Every row selects its thing on the map, and a click on the
 * map opens the right tab with the row in view.
 */
import { useEffect, useRef, type ReactNode } from 'react'
import { Link } from 'react-router-dom'
import { Activity, Aircraft, ArrowRight, Bombing, Carrier, Csar, Helicopter, HotZone, Refuel, Sam, Ship, Wind } from '@icons'
import { Empty } from '../../components/States'
import { useAuth } from '../../context/AuthContext'
import { GROUPABLE_STATIONS, STATION_KIND_LABEL, airframe, fmt, fmtClock, pad3 } from '../../lib/format'
import type { DockTab } from '../../lib/select'
import { useNow } from '../../lib/useNow'
import type { LiveCarrier, LiveTanker, RangeLive } from '../../types'
import { LiveFeed } from './LiveFeed'

const ROTARY = /^(UH|CH|AH|Mi|Ka|SA342|OH)/

const TANKER_STATE: Record<LiveTanker['state'], { label: string; cls: string }> = {
  on_station: { label: 'ON STATION', cls: 'live' },
  spawning: { label: 'SPAWNING', cls: 'warn' },
  rtb: { label: 'RTB', cls: 'outline' },
  dead: { label: 'LOST', cls: 'bad' },
}

const sideColor = (side: string) => (side === 'blue' ? 'var(--sky)' : side === 'red' ? 'var(--wave)' : 'var(--haze)')

/** "12 min" since an ISO time, against the server clock. */
function minsSince(iso: string, now: string): number {
  return Math.max(0, Math.round((new Date(now).getTime() - new Date(iso).getTime()) / 60_000))
}

// ─── situation ────────────────────────────────────────────────────────────

function Situation({ live }: { live: RangeLive }) {
  const w = live.wind
  const up = live.players.filter(p => p.in_air).length
  return (
    <div className="rdock-sit">
      <div className="flex items-end gap-3">
        <div className="min-w-0">
          <div className="caps" style={{ fontSize: 9.5 }}>{live.theatre} · {live.mission_date}</div>
          <div className="mono rdock-clock">{live.mission_time}</div>
        </div>
        {live.night && <span className="chip outline mb-1.5">NIGHT</span>}
        <div className="ml-auto text-right mb-1">
          <div className="mono text-[15px]"><b>{up}</b><span className="muted">/{live.players.length}</span></div>
          <div className="caps" style={{ fontSize: 9.5 }}>pilots up</div>
        </div>
      </div>
      <div className="rdock-wx">
        <div><span className="caps"><Wind size={10} /> Surface</span><span className="mono">{pad3(w.surface_from_deg)}/{fmt(w.surface_kts)}<small>kt</small></span></div>
        <div><span className="caps">6,600 ft</span><span className="mono">{pad3(w.alt_from_deg)}/{fmt(w.alt_kts)}<small>kt</small></span></div>
        <div><span className="caps">Temp</span><span className="mono">{fmt(w.temperature_c)}<small>°C</small></span></div>
        <div title={`QNH ${fmt(w.qnh_hpa)} hPa = ${(w.qnh_hpa * 0.02953).toFixed(2)} inHg`}>
          <span className="caps">QNH</span>
          <span className="mono">{Math.round(w.qnh_hpa)}<small> {(w.qnh_hpa * 0.02953).toFixed(2)}</small></span>
        </div>
      </div>
      <div className="dim text-[11px] mono">server up {fmtClock(live.uptime_s)}</div>
    </div>
  )
}

// ─── shared row ───────────────────────────────────────────────────────────

function Row({ k, selected, onSelect, children, dim = false }: {
  k: string
  selected: string | null
  onSelect: (k: string) => void
  children: ReactNode
  dim?: boolean
}) {
  const sel = k === selected
  return (
    <li>
      <div
        role="button"
        tabIndex={0}
        data-key={k}
        className={`rdock-row${sel ? ' sel' : ''}`}
        style={dim ? { opacity: 0.55 } : undefined}
        aria-pressed={sel}
        onClick={() => onSelect(k)}
        onKeyDown={e => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); onSelect(k) } }}
        title="Show on the map"
      >
        {children}
      </div>
    </li>
  )
}

function Section({ title, icon, count, right, children }: { title: string; icon?: ReactNode; count?: ReactNode; right?: ReactNode; children: ReactNode }) {
  return (
    <section className="rdock-sec">
      <header className="rdock-sec-h">
        {icon}
        <h3 className="caps m-0">{title}</h3>
        {count !== undefined && <span className="mono dim text-[11px]">{count}</span>}
        {right && <span className="ml-auto">{right}</span>}
      </header>
      {children}
    </section>
  )
}

const Mini = ({ k, v }: { k: string; v: ReactNode }) => (
  <span className="min-w-0"><span className="dim block text-[9.5px] tracking-[.08em]">{k}</span><span className="truncate block">{v}</span></span>
)

// ─── tabs ─────────────────────────────────────────────────────────────────

function Traffic({ live, selected, onSelect }: TabProps) {
  const { me } = useAuth()
  const now = useNow(15_000)
  const players = [...live.players].sort((a, b) => Number(b.ucid === me?.ucid) - Number(a.ucid === me?.ucid) || Number(b.in_air) - Number(a.in_air) || a.name.localeCompare(b.name))
  const tankers = [...live.tankers].sort((a, b) => (a.state === 'on_station' ? 0 : 1) - (b.state === 'on_station' ? 0 : 1))
  return (
    <>
      <Section title="Pilots" icon={<Aircraft size={13} />} count={`${live.players.filter(p => p.in_air).length} up · ${live.players.length}`}>
        {players.length === 0 ? <Empty title="Nobody on the range" /> : (
          <ul className="rdock-list">
            {players.map(p => {
              const I = ROTARY.test(p.unit_type) ? Helicopter : Aircraft
              const mine = p.ucid === me?.ucid
              return (
                <Row key={p.ucid} k={`player:${p.ucid}`} selected={selected} onSelect={onSelect} dim={!p.in_air}>
                  <div className="flex items-center gap-2 min-w-0">
                    <span style={{ color: sideColor(p.side), transform: `rotate(${p.heading_deg}deg)`, display: 'inline-flex' }}><I size={14} /></span>
                    <span className="font-semibold text-[13px] truncate">{p.name}</span>
                    {mine && <span className="chip warn" style={{ height: 16, fontSize: 9.5 }}>YOU</span>}
                    <span className="muted text-[12px] truncate">{airframe(p.unit_type)}</span>
                    <Link to={`/pilot/${encodeURIComponent(p.ucid)}`} className="rdock-go ml-auto" onClick={e => e.stopPropagation()} title={`${p.name}'s range record`} aria-label={`${p.name}'s range record`}>
                      <ArrowRight size={13} />
                    </Link>
                  </div>
                  <div className="flex gap-3 text-[11.5px] muted pl-[22px] mono">
                    <span className="truncate font-sans">{p.in_air ? p.activity ?? 'airborne' : 'on the ground'}</span>
                    {p.in_air && <span className="ml-auto shrink-0">{pad3(p.heading_deg)}° · {fmt(p.speed_kts)} kt</span>}
                  </div>
                </Row>
              )
            })}
          </ul>
        )}
      </Section>

      <Section title="Tankers" icon={<Refuel size={13} />} count={tankers.filter(t => t.state === 'on_station').length + ' on station'}>
        {tankers.length === 0 ? <Empty title="No tankers up"><Link to="/spawn" className="underline">Spawn one</Link>.</Empty> : (
          <ul className="rdock-list">
            {tankers.map(t => (
              <Row key={t.id} k={`tanker:${t.id}`} selected={selected} onSelect={onSelect} dim={t.state === 'dead'}>
                <div className="flex items-center gap-2 min-w-0">
                  <span className="font-semibold text-[13px]">{t.callsign}</span>
                  <span className="muted text-[12px] truncate">{airframe(t.unit_type)} · {t.method}</span>
                  <span className={`chip ${TANKER_STATE[t.state].cls} ml-auto`}>{TANKER_STATE[t.state].label}</span>
                </div>
                <div className="grid grid-cols-4 gap-2 mt-1 mono text-[12px]">
                  <Mini k="TACAN" v={t.tacan ?? '—'} />
                  <Mini k="FREQ" v={t.freq_mhz.toFixed(3)} />
                  <Mini k="ALT" v={`FL${Math.round(t.alt_ft / 100)}`} />
                  <Mini k="SPEED" v={`${fmt(t.speed_kts)} kt`} />
                </div>
                {(t.receivers.length > 0 || t.recovery_for || t.owner) && (
                  <div className="text-[11.5px] muted mt-1 truncate">
                    {t.receivers.length > 0 && <>On the hose: <span style={{ color: 'var(--chalk)' }}>{t.receivers.join(', ')}</span> </>}
                    {t.recovery_for && <>· recovery tanker </>}
                    {t.owner && <>· spawned by {t.owner}</>}
                  </div>
                )}
              </Row>
            ))}
          </ul>
        )}
      </Section>

      {live.spawns.length > 0 && (
        <Section title="Spawned" count={live.spawns.length} right={<Link to="/spawn" className="text-[11.5px] muted hover:text-[var(--chalk)]">Manage →</Link>}>
          <ul className="rdock-list">
            {live.spawns.map(s => {
              const left = s.expires ? Math.max(0, Math.round((new Date(s.expires).getTime() - now) / 60_000)) : null
              return (
                <Row key={s.id} k={`spawn:${s.id}`} selected={selected} onSelect={onSelect}>
                  <div className="flex items-center gap-2 min-w-0">
                    <span style={{ width: 8, height: 8, background: 'var(--wave)', transform: 'rotate(45deg)', flex: 'none' }} />
                    <span className="text-[12.5px] font-medium truncate">{s.label}</span>
                    <span className="muted text-[11.5px] ml-auto shrink-0">{s.owner_name}{left !== null ? ` · ${left} min` : ''}</span>
                  </div>
                </Row>
              )
            })}
          </ul>
        </Section>
      )}
    </>
  )
}

function CarrierBlock({ c }: { c: LiveCarrier }) {
  const wodOk = c.wind_over_deck_kts >= 25 && c.wind_over_deck_kts <= 30
  return (
    <>
      <div className="flex items-center gap-2 flex-wrap">
        <span className="font-semibold text-[14px]">{c.name}</span>
        <span className={`chip ${c.recovery_open ? 'live' : 'outline'} ml-auto`}>
          {c.recovery_open ? <><span className="dot live" /> RECOVERY OPEN</> : 'DECK CLOSED'}
        </span>
      </div>
      {c.next_window && <div className="text-[12px] muted mt-0.5">Window {c.next_window}</div>}
      <div className="grid grid-cols-3 gap-2 mt-2">
        <div className="kpi"><span className="caps">BRC</span><span className="v">{pad3(c.brc_deg)}</span></div>
        <div className="kpi"><span className="caps">FB</span><span className="v">{pad3(c.fb_deg)}</span></div>
        <div className="kpi">
          <span className="caps">WOD</span>
          <span className="v" style={{ color: wodOk ? 'var(--datum)' : undefined }} title={wodOk ? 'In the 25–30 kt window' : 'Outside the 25–30 kt window'}>
            {fmt(c.wind_over_deck_kts, 1)}<small>kt</small>
          </span>
        </div>
      </div>
      <dl className="kv mt-2">
        <dt>Wind off the angled deck</dt><dd>{c.wind_over_deck_angle_deg > 0 ? 'R' : 'L'} {fmt(Math.abs(c.wind_over_deck_angle_deg), 1)}°</dd>
        <dt>Ship speed · Case</dt><dd>{fmt(c.speed_kts)} kt · {['I', 'II', 'III'][c.case - 1] ?? c.case}</dd>
        <dt>TACAN · ICLS</dt><dd>{c.tacan ?? '—'} · {c.icls !== null ? `ch ${c.icls}` : '—'}</dd>
        <dt>Link-4 · tower</dt><dd>{c.link4_mhz !== null ? c.link4_mhz.toFixed(1) : '—'} · {c.tower_mhz !== null ? c.tower_mhz.toFixed(3) : '—'}</dd>
        <dt>Recovery tanker</dt><dd>{c.recovery_tanker ?? '—'}</dd>
      </dl>
      <div className="text-[12px] mt-1.5">
        <span className="muted">In the pattern: </span>
        {c.pattern.length ? c.pattern.join(', ') : <span className="dim">nobody</span>}
      </div>
    </>
  )
}

function Boat({ live, selected, onSelect }: TabProps) {
  const csar = live.csar ?? []
  const decks = live.ship_decks ?? []
  return (
    <>
      <Section title="Carrier ops" icon={<Carrier size={13} />} right={<Link to="/calc" className="text-[11.5px] muted hover:text-[var(--chalk)]">WOD calculator →</Link>}>
        {live.carriers.length === 0 ? <Empty title="No carrier on the range" /> : (
          <ul className="rdock-list">
            {live.carriers.map(c => (
              <Row key={c.id} k={`carrier:${c.id}`} selected={selected} onSelect={onSelect}><CarrierBlock c={c} /></Row>
            ))}
          </ul>
        )}
      </Section>
      {decks.length > 0 && (
        <Section title="Ship decks" icon={<Ship size={13} />} count={decks.length}>
          <ul className="rdock-list">
            {decks.map(d => (
              <Row key={d.id} k={`deck:${d.id}`} selected={selected} onSelect={onSelect}>
                <div className="flex items-center gap-2 min-w-0">
                  <span className="font-medium text-[13px] truncate">{d.name}</span>
                  <span className="muted text-[12px] truncate">{d.unit_type}</span>
                  <span className="mono text-[12px] ml-auto shrink-0">{pad3(d.heading_deg)}° · {fmt(d.speed_kts)} kt</span>
                </div>
                {d.tacan && <div className="text-[12px] muted mono">TACAN {d.tacan}</div>}
              </Row>
            ))}
          </ul>
        </Section>
      )}
      {csar.length > 0 && (
        <Section title="CSAR" icon={<Csar size={13} />} count={csar.filter(c => !c.picked_up).length + ' down'}>
          <ul className="rdock-list">
            {csar.map(c => (
              <li key={`${c.area}/${c.pilot}`} className="rdock-row static">
                <div className="flex items-center gap-2">
                  <span className="font-semibold text-[13px] truncate">{c.pilot}</span>
                  <span className="muted text-[12px] truncate">{c.area}</span>
                  <span className={`chip ${c.picked_up ? 'live' : 'warn'} ml-auto`}>{c.picked_up ? 'ON BOARD' : 'MAYDAY'}</span>
                </div>
                <div className="text-[12px] muted">down {minsSince(c.started, live.server_time)} min{c.hostile ? ' · hostile: troops hunting him' : ''}</div>
              </li>
            ))}
          </ul>
        </Section>
      )}
    </>
  )
}

function Threats({ live, selected, onSelect }: TabProps) {
  const zones = live.hot_zones ?? []
  const iads = live.iads ?? []
  const jammers = live.jammers ?? []
  if (zones.length + iads.length + jammers.length === 0) return <Empty title="No threats on the range">Hot zones, air defences and jammers show here when the mission has them.</Empty>
  return (
    <>
      {zones.length > 0 && (
        <Section title="Hot zones" icon={<HotZone size={13} />} count={zones.length}>
          <ul className="rdock-list">
            {zones.map(h => (
              <Row key={h.id} k={`hz:${h.id}`} selected={selected} onSelect={onSelect}>
                <div className="flex items-center gap-2">
                  <span className="font-semibold text-[13px] truncate">{h.name}</span>
                  <span className={`chip ${h.bandits ? 'bad' : 'outline'} ml-auto`}>{h.bandits ? `${h.bandits} BANDIT${h.bandits === 1 ? '' : 'S'}` : 'QUIET'}</span>
                </div>
                <div className="grid grid-cols-3 gap-2 mt-1 mono text-[12px]">
                  <Mini k="GROUND" v={`${h.ground_alive}/${h.ground_total}`} />
                  <Mini k="RADIUS" v={`${fmt(h.radius_m / 1852)} nm`} />
                  <Mini k="AWACS" v={h.awacs ?? '—'} />
                </div>
                <div className="text-[12px] mt-1 truncate">
                  <span className="muted">In the zone: </span>{h.players.length ? h.players.join(', ') : <span className="dim">nobody</span>}
                </div>
              </Row>
            ))}
          </ul>
        </Section>
      )}
      {iads.map(n => {
        const up = n.sites.filter(s => s.emitting && s.units_alive > 0).length
        return (
          <Section key={n.id} title={n.name} icon={<Sam size={13} style={{ color: sideColor(n.side) }} />}
            right={<span className={`chip ${up ? 'bad' : 'outline'}`}>{up ? `${up} RADAR${up === 1 ? '' : 'S'} UP` : 'DARK'}</span>}>
            {!n.weapons_free && <div className="text-[12px] muted px-3 pb-1">Weapons hold: radars honest, no launches</div>}
            <ul className="rdock-list">
              {n.sites.map(s => {
                const dead = s.units_alive === 0
                return (
                  <Row key={s.id} k={`site:${n.id}/${s.id}`} selected={selected} onSelect={onSelect} dim={dead}>
                    <div className="flex items-center gap-2 text-[12.5px] min-w-0">
                      <span className="dot" style={{ background: dead ? 'var(--dim)' : s.emitting ? 'var(--wave)' : 'var(--line-2)', boxShadow: s.emitting && !dead ? '0 0 0 3px var(--wave-soft)' : undefined }} />
                      <span className="truncate font-medium" style={{ textDecoration: dead ? 'line-through' : undefined }}>{s.name}</span>
                      <span className="muted truncate">{s.system}</span>
                      <span className="mono ml-auto shrink-0" title="units alive">{s.units_alive}/{s.units_total}</span>
                    </div>
                  </Row>
                )
              })}
            </ul>
          </Section>
        )
      })}
      {jammers.length > 0 && (
        <Section title="Jammers" icon={<Activity size={13} />} count={jammers.filter(j => j.alive).length + ' alive'}>
          <ul className="rdock-list">
            {jammers.map(j => (
              <Row key={j.id} k={`jam:${j.id}`} selected={selected} onSelect={onSelect} dim={!j.alive}>
                <div className="flex items-center gap-2">
                  <span className="dot" style={{ background: sideColor(j.side) }} />
                  <span className="font-medium text-[13px] truncate">{j.name}</span>
                  <span className="chip outline ml-auto" style={{ color: j.alive ? '#00C8B4' : undefined }}>{j.alive ? 'JAMMING' : 'DESTROYED'}</span>
                </div>
                <div className="text-[12px] muted pl-[15px]">GPS {j.gps} · radio {j.radio} · {fmt(j.radius_m / 1852)} nm radius</div>
              </Row>
            ))}
          </ul>
        </Section>
      )}
    </>
  )
}

function Stations({ live, selected, onSelect }: TabProps) {
  const hot = live.stations.filter(s => s.hot_by.length > 0)
  const sorted = [...live.stations].sort((a, b) => Number(b.hot_by.length > 0) - Number(a.hot_by.length > 0))
  return (
    <Section title="Stations" icon={<Bombing size={13} />} count={`${hot.length} hot · ${live.stations.length}`} right={<Link to="/brief" className="text-[11.5px] muted hover:text-[var(--chalk)]">Coordinates →</Link>}>
      {sorted.length === 0 ? <Empty title="No stations" /> : (
        <ul className="rdock-list">
          {sorted.map(s => {
            const isHot = s.hot_by.length > 0
            return (
              <Row key={s.id} k={`station:${s.id}`} selected={selected} onSelect={onSelect}>
                <div className="flex items-center gap-2 min-w-0">
                  <span className={`dot ${isHot ? 'live' : ''}`} style={isHot ? undefined : { background: 'var(--line-2)' }} />
                  <span className="font-medium text-[13px] truncate">{s.name}</span>
                  <span className="mono text-[12px] ml-auto shrink-0" title="targets alive">{s.targets_alive}/{s.targets_total}</span>
                  {GROUPABLE_STATIONS.includes(s.kind) && (
                    <Link to={`/stations/${encodeURIComponent(s.id)}`} className="rdock-go" onClick={e => e.stopPropagation()} title="Impact group: CEP and mean point of impact" aria-label={`${s.name} impact group`}>
                      <ArrowRight size={13} />
                    </Link>
                  )}
                </div>
                <div className="text-[12px] muted pl-[15px] flex flex-wrap gap-x-2">
                  <span>{STATION_KIND_LABEL[s.kind]}</span>
                  {s.laser_code !== null && <span className="mono">laser {s.laser_code}</span>}
                  {isHot ? <span style={{ color: 'var(--datum)' }}>hot: {s.hot_by.join(', ')}</span> : <span>cold</span>}
                </div>
              </Row>
            )
          })}
        </ul>
      )}
    </Section>
  )
}

interface TabProps {
  live: RangeLive
  selected: string | null
  onSelect: (k: string) => void
}

export function LiveDock({ live, tab, setTab, selected, onSelect }: {
  live: RangeLive | null
  tab: DockTab
  setTab: (t: DockTab) => void
  selected: string | null
  onSelect: (k: string) => void
}) {
  const body = useRef<HTMLDivElement>(null)
  // bring the selected row into view (after the tab it is on has rendered)
  useEffect(() => {
    if (!selected) return
    const el = body.current?.querySelector<HTMLElement>(`[data-key="${CSS.escape(selected)}"]`)
    el?.scrollIntoView({ block: 'nearest', behavior: 'smooth' })
  }, [selected, tab])

  const radars = (live?.iads ?? []).reduce((n, i) => n + i.sites.filter(s => s.emitting && s.units_alive > 0).length, 0)
  const bandits = (live?.hot_zones ?? []).reduce((n, h) => n + h.bandits, 0)
  const tabs: { id: DockTab; label: string; badge?: ReactNode }[] = [
    { id: 'feed', label: 'Feed' },
    { id: 'traffic', label: 'Traffic', badge: live ? <span className="mono">{live.players.filter(p => p.in_air).length}</span> : undefined },
    { id: 'boat', label: 'Boat', badge: live?.carriers.some(c => c.recovery_open) ? <span className="dot live" title="Recovery open" /> : undefined },
    { id: 'threats', label: 'Threats', badge: live && radars + bandits > 0 ? <span className="mono" style={{ color: 'var(--wave)' }}>{radars + bandits}</span> : undefined },
    { id: 'stations', label: 'Stations', badge: live ? <span className="mono" style={{ color: 'var(--datum)' }}>{live.stations.filter(s => s.hot_by.length).length || ''}</span> : undefined },
  ]

  return (
    <aside className="rdock" aria-label="Range control">
      {live && <Situation live={live} />}
      <div className="rdock-tabs" role="tablist">
        {tabs.map(t => (
          <button key={t.id} role="tab" aria-selected={tab === t.id} onClick={() => setTab(t.id)} disabled={!live && t.id !== 'feed'}>
            {t.label}
            {t.badge}
          </button>
        ))}
      </div>
      <div className="rdock-body" ref={body}>
        {tab === 'feed' || !live ? (
          <LiveFeed />
        ) : tab === 'traffic' ? (
          <Traffic live={live} selected={selected} onSelect={onSelect} />
        ) : tab === 'boat' ? (
          <Boat live={live} selected={selected} onSelect={onSelect} />
        ) : tab === 'threats' ? (
          <Threats live={live} selected={selected} onSelect={onSelect} />
        ) : (
          <Stations live={live} selected={selected} onSelect={onSelect} />
        )}
      </div>
    </aside>
  )
}
