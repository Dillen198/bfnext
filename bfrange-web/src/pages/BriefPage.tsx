/**
 * `/brief` — the range on one card, the way it goes on a kneeboard: every
 * frequency and TACAN, the weather and altimeter, every target with its
 * coordinates in the format your jet takes, laser codes, air defences and
 * the sectors. Built from the live picture, so it is right for the mission
 * that is running now, with the range chart on top. Two kneeboards: light
 * (black on white, for the day) and dark (pale on black, for night). Each
 * prints, and saves as a 768 x 1024 page for the DCS kneeboard folder.
 */
import { useEffect, useState, type ReactNode } from 'react'
import { Link } from 'react-router-dom'
import { Check, Copy, Download, Moon, Printer, Sun } from '@icons'
import { KneeboardChart, type KbTheme } from '../components/KneeboardChart'
import { RangeLockup } from '../components/RangeMark'
import { ErrorState, Loading } from '../components/States'
import { useToast } from '../context/ToastContext'
import { COORD_FORMATS, fmtCoord, type CoordFormat } from '../lib/coords'
import { saveBlob } from '../lib/download'
import { STATION_KIND_LABEL, airframe, fmt, mToFt, pad3 } from '../lib/format'
import { kneeboardPng } from '../lib/kneeboard'
import { kindInfo } from '../lib/sectors'
import { useLive } from '../lib/useLive'
import { usePref } from '../lib/usePref'
import type { RangeLive } from '../types'

const isFormat = (v: unknown): v is CoordFormat => COORD_FORMATS.some(f => f.v === v)
const isKb = (v: unknown): v is KbTheme => v === 'light' || v === 'dark'

/** Saves the kneeboard page as a PNG. */
function PngBtn({ live, kb }: { live: RangeLive; kb: KbTheme }) {
  const [busy, setBusy] = useState(false)
  const toast = useToast()
  return (
    <button
      className="btn-range"
      disabled={busy}
      title="A 1536 x 2048 page for Saved Games\DCS\Kneeboard"
      onClick={async () => {
        setBusy(true)
        try {
          const stamp = live.mission_date.replace(/[^0-9]/g, '')
          saveBlob(await kneeboardPng(live, kb), `range-kneeboard-${kb}${stamp ? `-${stamp}` : ''}.png`)
        } catch (e) {
          toast(`The kneeboard page did not save: ${e instanceof Error ? e.message : String(e)}`, false)
        } finally {
          setBusy(false)
        }
      }}
    >
      <Download size={15} /> {busy ? 'Drawing…' : 'Kneeboard PNG'}
    </button>
  )
}

function CopyBtn({ text, label }: { text: string; label: string }) {
  const [done, setDone] = useState(false)
  const toast = useToast()
  return (
    <button
      className="copy-btn no-print"
      aria-label={`Copy ${label}`}
      title={`Copy ${label}`}
      onClick={async () => {
        try {
          await navigator.clipboard.writeText(text)
          setDone(true)
          setTimeout(() => setDone(false), 1200)
        } catch {
          toast('The browser would not let the page copy. Select the text instead.', false)
        }
      }}
    >
      {done ? <Check size={13} /> : <Copy size={13} />}
    </button>
  )
}

function Card({ title, n, children, right }: { title: string; n: string; children: ReactNode; right?: ReactNode }) {
  return (
    <section className="brief-card">
      <header>
        <span className="brief-n mono">{n}</span>
        <h2 className="caps m-0">{title}</h2>
        {right && <span className="ml-auto">{right}</span>}
      </header>
      <div className="table-scroll">{children}</div>
    </section>
  )
}

const dash = (v: string | number | null | undefined) => (v === null || v === undefined || v === '' ? '—' : v)

function Comms({ live }: { live: RangeLive }) {
  const rows: { who: string; what: string; freq: string; tacan: string; extra: string }[] = []
  for (const c of live.carriers) {
    rows.push({
      who: c.name, what: 'Carrier', freq: c.tower_mhz !== null ? c.tower_mhz.toFixed(3) : '—', tacan: dash(c.tacan) as string,
      extra: [c.icls !== null ? `ICLS ${c.icls}` : null, c.link4_mhz !== null ? `Link-4 ${c.link4_mhz.toFixed(1)}` : null, `BRC ${pad3(c.brc_deg)}`, `Case ${['I', 'II', 'III'][c.case - 1] ?? c.case}`].filter(Boolean).join(' · '),
    })
  }
  for (const t of live.tankers.filter(t => t.state !== 'dead')) {
    rows.push({
      who: t.callsign, what: `Tanker · ${t.method}`, freq: t.freq_mhz.toFixed(3), tacan: dash(t.tacan) as string,
      extra: `${airframe(t.unit_type)} · FL${Math.round(t.alt_ft / 100)} · ${fmt(t.speed_kts)} kt${t.recovery_for ? ' · recovery' : ''}`,
    })
  }
  for (const d of live.ship_decks ?? []) {
    rows.push({ who: d.name, what: 'Helo deck', freq: '—', tacan: dash(d.tacan) as string, extra: `${d.unit_type} · ${pad3(d.heading_deg)}° ${fmt(d.speed_kts)} kt` })
  }
  for (const h of live.hot_zones ?? []) {
    if (h.awacs) rows.push({ who: h.awacs, what: `AWACS · ${h.name}`, freq: '—', tacan: '—', extra: `${fmt(h.radius_m / 1852)} nm zone` })
  }
  if (!rows.length) return <p className="muted px-3 py-3 m-0 text-[13px]">No carriers, tankers or controllers up.</p>
  return (
    <table className="table-range brief-table">
      <thead><tr><th>Callsign</th><th>Role</th><th className="n">Freq MHz</th><th>TACAN</th><th>Notes</th></tr></thead>
      <tbody>
        {rows.map(r => (
          <tr key={r.who + r.what}>
            <td className="font-semibold">{r.who}</td>
            <td className="muted">{r.what}</td>
            <td className="n">{r.freq}</td>
            <td className="mono">{r.tacan}</td>
            <td className="muted text-[12px]">{r.extra}</td>
          </tr>
        ))}
      </tbody>
    </table>
  )
}

function Weather({ live }: { live: RangeLive }) {
  const w = live.wind
  const layers = (w.layers ?? []).filter((l, i, a) => i === 0 || l.alt_m - a[i - 1].alt_m >= 400).slice(0, 9)
  return (
    <>
      <div className="brief-wx">
        <div><span className="caps">Surface</span><b className="mono">{pad3(w.surface_from_deg)}/{fmt(w.surface_kts)} kt</b></div>
        <div><span className="caps">6,600 ft</span><b className="mono">{pad3(w.alt_from_deg)}/{fmt(w.alt_kts)} kt</b></div>
        <div><span className="caps">Temp</span><b className="mono">{fmt(w.temperature_c)} °C</b></div>
        <div><span className="caps">QNH</span><b className="mono">{fmt(w.qnh_hpa)} hPa · {(w.qnh_hpa * 0.02953).toFixed(2)} inHg</b></div>
      </div>
      {layers.length > 1 && (
        <table className="table-range brief-table">
          <thead><tr><th className="n">Alt ft</th><th className="n">Wind</th><th className="n">Temp °C</th><th className="n">hPa</th></tr></thead>
          <tbody>
            {layers.map(l => (
              <tr key={l.alt_m}>
                <td className="n">{fmt(Math.round(mToFt(l.alt_m) / 100) * 100)}</td>
                <td className="n">{pad3(l.wind_from_deg)}/{fmt(l.wind_kts)}</td>
                <td className="n">{fmt(l.temp_c)}</td>
                <td className="n">{fmt(l.pressure_hpa)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
    </>
  )
}

function Targets({ live, f }: { live: RangeLive; f: CoordFormat }) {
  return (
    <table className="table-range brief-table">
      <thead><tr><th>Station</th><th>Type</th><th>Coordinates</th><th className="n">Elev ft</th><th className="n">Laser</th><th className="n">Alive</th></tr></thead>
      <tbody>
        {live.stations.map(s => {
          const c = fmtCoord(s.pos, f)
          return (
            <tr key={s.id}>
              <td className="font-semibold">
                <Link to={`/?focus=${encodeURIComponent(`station:${s.id}`)}`} className="hover:underline">{s.name}</Link>
                {s.note && <div className="muted text-[11.5px] font-normal">{s.note}</div>}
              </td>
              <td className="muted">{STATION_KIND_LABEL[s.kind]}</td>
              <td className="mono whitespace-nowrap">{c} <CopyBtn text={c} label={`${s.name} coordinates`} /></td>
              <td className="n">{s.elev_m != null ? fmt(mToFt(s.elev_m)) : '—'}</td>
              <td className="n">{s.laser_code ?? '—'}</td>
              <td className="n">{s.targets_alive}/{s.targets_total}</td>
            </tr>
          )
        })}
      </tbody>
    </table>
  )
}

function AirDefence({ live, f }: { live: RangeLive; f: CoordFormat }) {
  const rows = (live.iads ?? []).flatMap(n => n.sites.map(s => ({ n, s })))
  return (
    <table className="table-range brief-table">
      <thead><tr><th>Site</th><th>System</th><th>Coordinates</th><th className="n">Range nm</th><th>Status</th></tr></thead>
      <tbody>
        {rows.map(({ n, s }) => {
          const c = fmtCoord(s.pos, f)
          const dead = s.units_alive === 0
          return (
            <tr key={`${n.id}/${s.id}`} style={{ opacity: dead ? 0.55 : 1 }}>
              <td className="font-semibold">{s.name}<div className="muted text-[11.5px] font-normal">{n.name}{n.weapons_free ? '' : ' · weapons hold'}</div></td>
              <td>{s.system}</td>
              <td className="mono whitespace-nowrap">{c} <CopyBtn text={c} label={`${s.name} coordinates`} /></td>
              <td className="n">{s.range_m > 0 ? fmt(s.range_m / 1852) : '—'}</td>
              <td>{dead ? 'destroyed' : s.emitting ? <span style={{ color: 'var(--wave)' }}>emitting</span> : 'dark'}</td>
            </tr>
          )
        })}
        {(live.jammers ?? []).map(j => {
          const c = fmtCoord(j.pos, f)
          return (
            <tr key={j.id} style={{ opacity: j.alive ? 1 : 0.55 }}>
              <td className="font-semibold">{j.name}<div className="muted text-[11.5px] font-normal">{j.side} jammer</div></td>
              <td>GPS {j.gps} · radio {j.radio}</td>
              <td className="mono whitespace-nowrap">{c} <CopyBtn text={c} label={`${j.name} coordinates`} /></td>
              <td className="n">{fmt(j.radius_m / 1852)}</td>
              <td>{j.alive ? 'jamming' : 'destroyed'}</td>
            </tr>
          )
        })}
      </tbody>
    </table>
  )
}

function Sectors({ live }: { live: RangeLive }) {
  const sectors = live.sectors ?? []
  return (
    <table className="table-range brief-table">
      <thead><tr><th>Sector</th><th>Kind</th><th>For</th><th>What it is for</th></tr></thead>
      <tbody>
        {sectors.map(s => {
          const k = kindInfo(s.kind)
          return (
            <tr key={s.id}>
              <td className="font-semibold whitespace-nowrap">
                <span className="inline-block mr-2 align-middle" style={{ width: 9, height: 9, borderRadius: 2, background: k.color }} />
                {s.name}
              </td>
              <td className="muted whitespace-nowrap">{k.label}</td>
              <td className="muted">{s.side === 'all' ? 'both' : s.side}</td>
              <td className="text-[12.5px]">{s.purpose}</td>
            </tr>
          )
        })}
      </tbody>
    </table>
  )
}

export default function BriefPage() {
  const q = useLive(15_000)
  const live = q.data?.live
  const [f, setF] = usePref<CoordFormat>('range.brief.coords', 'ddm', isFormat)
  const [kb, setKb] = usePref<KbTheme>('range.brief.kneeboard', 'light', isKb)

  // the print styles read the kneeboard's ink off the root (index.css)
  useEffect(() => {
    const root = document.documentElement
    root.dataset.kb = kb
    return () => { delete root.dataset.kb }
  }, [kb])

  return (
    <div className="wrap page brief">
      <div className="page-head">
        <div>
          <div className="print-only mb-2"><RangeLockup size={34} /></div>
          <h1 className="display">Range brief</h1>
          <p className="sub m-0 mt-1">
            {live
              ? <>{live.theatre} · {live.mission_date} · <span className="mono">{live.mission_time}</span>{live.night ? ' · night' : ''}. Everything on this card is from the mission running now.</>
              : 'The range on one card: frequencies, TACANs, coordinates, laser codes and the weather.'}
          </p>
        </div>
        <div className="actions no-print">
          <div className="seg" role="group" aria-label="Coordinate format">
            {COORD_FORMATS.map(o => (
              <button key={o.v} aria-pressed={f === o.v} onClick={() => setF(o.v)}>{o.label}</button>
            ))}
          </div>
          <div className="seg" role="group" aria-label="Kneeboard">
            <button aria-pressed={kb === 'light'} onClick={() => setKb('light')} title="Black on white, for the day"><Sun size={14} /> Light</button>
            <button aria-pressed={kb === 'dark'} onClick={() => setKb('dark')} title="Pale on black, for night"><Moon size={14} /> Dark</button>
          </div>
          {live && <PngBtn live={live} kb={kb} />}
          <button className="btn-range primary" onClick={() => window.print()} disabled={!live}>
            <Printer size={15} /> Print kneeboard
          </button>
        </div>
      </div>

      {q.isLoading ? (
        <Loading label="Reading the range" />
      ) : q.error ? (
        <ErrorState error={q.error} retry={() => q.refetch()} />
      ) : !live ? (
        <div className="panel panel-b flex flex-wrap items-center gap-3">
          <span className="chip bad">RANGE OFFLINE</span>
          <span>{q.data?.reason ?? 'The brief is built from the live mission, and the range is not publishing one right now.'}</span>
        </div>
      ) : (
        <div className={`brief-kb kb-${kb}`}>
          <section className="brief-card brief-chart">
            <header>
              <span className="brief-n mono">00</span>
              <h2 className="caps m-0">Range chart</h2>
              <span className="ml-auto mono dim text-[11px]">{kb === 'dark' ? 'NIGHT' : 'DAY'} · north up · NM</span>
            </header>
            <div className="table-scroll">
              <div className="brief-chart-svg"><KneeboardChart live={live} theme={kb} width={1000} height={640} /></div>
            </div>
          </section>
        <div className="brief-grid">
          <Card n="01" title="Comms & nav"><Comms live={live} /></Card>
          <Card n="02" title="Weather & altimeter"><Weather live={live} /></Card>
          <Card n="03" title="Targets" right={<span className="mono dim text-[11px]">{COORD_FORMATS.find(o => o.v === f)?.label}</span>}>
            <Targets live={live} f={f} />
          </Card>
          {((live.iads ?? []).length + (live.jammers ?? []).length > 0) && (
            <Card n="04" title="Air defences & EW"><AirDefence live={live} f={f} /></Card>
          )}
          {(live.sectors ?? []).length > 0 && <Card n="05" title="Sectors"><Sectors live={live} /></Card>}
        </div>
        </div>
      )}
    </div>
  )
}
