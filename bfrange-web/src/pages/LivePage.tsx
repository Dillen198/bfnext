/**
 * `/` — the range as it is right now: the picture on the map, what the
 * tankers and the boat are doing, which stations are hot, and every result
 * as it lands. Live state polls every 2 s while the tab is visible.
 */
import { useQuery } from '@tanstack/react-query'
import { Wind } from '@icons'
import { api } from '../api'
import { ErrorState, Loading } from '../components/States'
import { fmt, fmtClock, pad3 } from '../lib/format'
import type { RangeLive } from '../types'
import { LiveFeed } from './live/LiveFeed'
import { LiveMap } from './live/LiveMap'
import { CarrierPanel, StationsPanel, TankersPanel } from './live/LivePanels'

function StatusStrip({ live }: { live: RangeLive }) {
  const w = live.wind
  const airborne = live.players.filter(p => p.in_air).length
  const items: [string, React.ReactNode][] = [
    ['Theatre', live.theatre],
    ['Mission', <>{live.mission_date} · <b>{live.mission_time}</b>{live.night ? ' · night' : ''}</>],
    ['Surface wind', `${pad3(w.surface_from_deg)}/${fmt(w.surface_kts)} kt`],
    ['6,600 ft', `${pad3(w.alt_from_deg)}/${fmt(w.alt_kts)} kt`],
    ['Temp · QNH', `${fmt(w.temperature_c)}°C · ${fmt(w.qnh_hpa)}`],
    ['Pilots', `${live.players.length} · ${airborne} airborne`],
    ['Up', fmtClock(live.uptime_s)],
  ]
  return (
    <div className="flex flex-wrap items-stretch gap-x-6 gap-y-2 py-3">
      <div className="flex items-center gap-2">
        <span className="dot live" />
        <span className="display text-[26px]" style={{ color: 'var(--chalk)' }}>Range live</span>
      </div>
      {items.map(([k, v]) => (
        <div key={k} className="flex flex-col justify-center min-w-0">
          <span className="caps flex items-center gap-1" style={{ fontSize: 9.5 }}>{k === 'Surface wind' && <Wind size={11} />}{k}</span>
          <span className="mono text-[13px]">{v}</span>
        </div>
      ))}
    </div>
  )
}

export default function LivePage() {
  const q = useQuery({
    queryKey: ['live'],
    queryFn: () => api.live(),
    refetchInterval: 2000,
    staleTime: 1000,
  })
  const live = q.data?.live ?? null

  return (
    <div className="wrap pb-12">
      {q.isLoading ? (
        <Loading label="Contacting the range" />
      ) : q.error ? (
        <div className="py-4"><ErrorState error={q.error} retry={() => q.refetch()} /></div>
      ) : !live ? (
        <div className="py-4">
          <div className="panel panel-b flex flex-wrap items-center gap-3">
            <span className="chip outline">RANGE OFFLINE</span>
            <span>{q.data?.reason ?? 'The range server is not publishing a live picture right now.'}</span>
            <span className="muted text-[12px]">Results already flown are still below.</span>
          </div>
        </div>
      ) : (
        <StatusStrip live={live} />
      )}

      <div className="grid gap-3 xl:grid-cols-[340px_minmax(0,1fr)_360px] lg:grid-cols-[minmax(0,1fr)_360px]">
        <div className="order-2 xl:order-1 min-w-0">
          <LiveFeed />
        </div>
        <div className="order-1 xl:order-2 min-w-0 lg:col-span-2 xl:col-span-1">
          {live && <LiveMap live={live} height="min(72vh, 760px)" />}
        </div>
        <div className="order-3 flex flex-col gap-3 min-w-0">
          {live && (
            <>
              <CarrierPanel carriers={live.carriers} />
              <TankersPanel tankers={live.tankers} />
              <StationsPanel stations={live.stations} />
            </>
          )}
        </div>
      </div>
    </div>
  )
}
