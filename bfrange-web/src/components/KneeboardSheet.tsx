/**
 * One kneeboard page (768 × 1024, the DCS kneeboard's 3:4): the range chart
 * with the weather above it and the comms and targets under it, in the light
 * or the dark ink. lib/kneeboard.ts turns it into the PNG.
 */
import type { ReactNode } from 'react'
import { airframe, fmt, pad3 } from '../lib/format'
import type { RangeLive } from '../types'
import { KB_INK, KB_MONO, KB_SANS, type KbTheme } from '../lib/kneeboardInk'
import { ChartBody } from './KneeboardChart'

export const SHEET_W = 768
export const SHEET_H = 1024

const clip = (s: string, n: number) => (s.length > n ? `${s.slice(0, n - 1)}…` : s)

export function KneeboardSheet({ live, theme }: { live: RangeLive; theme: KbTheme }) {
  const ink = KB_INK[theme]
  const w = live.wind
  const M = 20
  const CHART_Y = 106
  const CHART_H = 610

  const comms: [string, string, string][] = []
  for (const c of live.carriers) comms.push([clip(c.name, 26), c.tower_mhz !== null ? c.tower_mhz.toFixed(3) : '—', [c.tacan, c.icls != null ? `ICLS ${c.icls}` : null].filter(Boolean).join(' ') || '—'])
  for (const t of live.tankers.filter(t => t.state !== 'dead')) comms.push([clip(`${t.callsign} ${airframe(t.unit_type)}`, 26), t.freq_mhz.toFixed(3), `${t.tacan ?? '—'} FL${Math.round(t.alt_ft / 100)}`])
  for (const d of live.ship_decks ?? []) comms.push([clip(d.name, 26), '—', d.tacan ?? '—'])
  const targets: [string, string, string][] = live.stations.map(s => [clip(s.name, 38), s.laser_code != null ? String(s.laser_code) : '—', s.elev_m != null ? fmt(s.elev_m / 0.3048) : '—'])

  const TOP = CHART_Y + CHART_H + 22
  const ROW = 15
  const rows = Math.floor((SHEET_H - 30 - TOP - 22) / ROW)
  const table = (x: number, title: string, head: [string, string, string], cols: [number, number, number], data: [string, string, string][]): ReactNode => (
    <g>
      <text x={x} y={TOP} fontSize={10} fontWeight={700} letterSpacing={1.6} fill={ink.ink}>{title}</text>
      {head.map((h, i) => (
        <text key={h} x={x + cols[i]} y={TOP + 16} fontSize={7.5} fontWeight={600} letterSpacing={0.8} fill={ink.dim}>{h}</text>
      ))}
      <line x1={x} x2={x + 352} y1={TOP + 20} y2={TOP + 20} stroke={ink.dim} strokeWidth={0.6} />
      {data.slice(0, rows).map((r, j) => (
        <g key={j}>
          {r.map((c, i) => (
            <text key={i} x={x + cols[i]} y={TOP + 20 + (j + 1) * ROW} fontSize={i ? 9.5 : 10} fontWeight={i ? 400 : 600} fontFamily={i ? KB_MONO : KB_SANS} fill={ink.ink}>{c}</text>
          ))}
        </g>
      ))}
      {data.length > rows && <text x={x} y={TOP + 20 + (rows + 1) * ROW} fontSize={8} fill={ink.dim}>+{data.length - rows} more on the range brief</text>}
      {!data.length && <text x={x} y={TOP + 20 + ROW} fontSize={9} fill={ink.dim}>None up.</text>}
    </g>
  )

  return (
    <svg xmlns="http://www.w3.org/2000/svg" width={SHEET_W} height={SHEET_H} viewBox={`0 0 ${SHEET_W} ${SHEET_H}`} fontFamily={KB_SANS}>
      <rect width={SHEET_W} height={SHEET_H} fill={ink.paper} />
      {/* title block */}
      <text x={M} y={36} fontSize={22} fontWeight={800} letterSpacing={2.5} fill={ink.ink}>VECTOR STRIKE RANGE</text>
      <text x={M} y={56} fontSize={11} fontWeight={600} letterSpacing={1.4} fill={ink.amber}>RANGE CHART · {live.theatre.toUpperCase()}</text>
      <text x={SHEET_W - M} y={34} fontSize={12} textAnchor="end" fontFamily={KB_MONO} fill={ink.ink}>{live.mission_date}</text>
      <text x={SHEET_W - M} y={54} fontSize={12} textAnchor="end" fontFamily={KB_MONO} fill={ink.ink}>{live.mission_time}{live.night ? ' · NIGHT' : ''}</text>
      <line x1={M} x2={SHEET_W - M} y1={68} y2={68} stroke={ink.ink} strokeWidth={1.4} />
      {/* weather */}
      {[
        ['SFC WIND', `${pad3(w.surface_from_deg)}/${fmt(w.surface_kts)} KT`],
        ['6,600 FT', `${pad3(w.alt_from_deg)}/${fmt(w.alt_kts)} KT`],
        ['QNH', `${fmt(w.qnh_hpa)} hPa · ${(w.qnh_hpa * 0.02953).toFixed(2)}`],
        ['TEMP', `${fmt(w.temperature_c)} °C`],
      ].map(([k, val], i) => (
        <g key={k} transform={`translate(${M + i * 182},84)`}>
          <text fontSize={7.5} fontWeight={600} letterSpacing={0.9} fill={ink.dim}>{k}</text>
          <text y={14} fontSize={11.5} fontFamily={KB_MONO} fill={ink.ink}>{val}</text>
        </g>
      ))}
      <svg x={M} y={CHART_Y} width={SHEET_W - 2 * M} height={CHART_H} viewBox={`0 0 ${SHEET_W - 2 * M} ${CHART_H}`}>
        <ChartBody live={live} theme={theme} width={SHEET_W - 2 * M} height={CHART_H} ratio={2} />
      </svg>
      {table(M, 'COMMS & NAV', ['CALLSIGN', 'MHZ', 'TACAN'], [0, 168, 230], comms)}
      {table(SHEET_W / 2 + 8, 'TARGETS', ['STATION', 'LASER', 'ELEV FT'], [0, 236, 292], targets)}
      <line x1={M} x2={SHEET_W - M} y1={SHEET_H - 26} y2={SHEET_H - 26} stroke={ink.dim} strokeWidth={0.6} />
      <text x={M} y={SHEET_H - 12} fontSize={8} fill={ink.dim}>range.vectorstrike.org/brief · from the mission running at {live.server_time.replace('T', ' ').slice(0, 16)}Z</text>
      <text x={SHEET_W - M} y={SHEET_H - 12} fontSize={8} textAnchor="end" fontWeight={600} letterSpacing={1} fill={ink.dim}>{theme === 'dark' ? 'NIGHT' : 'DAY'} CARD</text>
    </svg>
  )
}
