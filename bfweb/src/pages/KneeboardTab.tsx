import { useMemo, useState } from 'react'
import { jsPDF } from 'jspdf'
import {
  Comms,
  Heading,
  Cas,
  Supply,
  Sam,
  type IconComponent,
  ArrowUp,
  ArrowDown,
  Headphones,
} from '../icons'
import type { Briefing, SituationReport } from '../api'

export function fmtCoord(lat: number, lon: number): string {
  if (!lat && !lon) return '—'
  const c = (v: number, pos: string, neg: string) => {
    const h = v >= 0 ? pos : neg
    const a = Math.abs(v)
    const d = Math.floor(a)
    const m = (a - d) * 60
    return `${h}${d}°${m.toFixed(2)}'`
  }
  return `${c(lat, 'N', 'S')} ${c(lon, 'E', 'W')}`
}

function navaidCells(n: Briefing['navaids'][number]): string {
  const parts: string[] = []
  if (n.tacan) parts.push(`TACAN ${n.tacan}`)
  if (n.ndb_khz) parts.push(`NDB ${n.ndb_khz}`)
  if (n.icls) parts.push(`ICLS ${n.icls}`)
  if (n.link4_mhz) parts.push(`Link-4 ${n.link4_mhz.toFixed(1)}`)
  if (n.acls) parts.push('ACLS')
  if (n.brc != null) parts.push(`BRC ${String(n.brc).padStart(3, '0')}°`)
  return parts.join('   ') || '—'
}

// ── Generic sortable / filterable table ────────────────────────────────────
type Align = 'left' | 'right'
interface Column<T> {
  key: string
  label: string
  align?: Align
  /** value used for sorting + filtering */
  value: (row: T) => string | number
  render?: (row: T) => React.ReactNode
  color?: string
  /** relative column width (flex-grow-like). default 1 */
  w?: number
}

function DataTable<T>({
  columns, rows, initialSortKey, initialSortDir = 'asc', empty, maxHeight = 300, minWidth = 620,
}: {
  columns: Column<T>[]
  rows: T[]
  initialSortKey?: string
  initialSortDir?: 'asc' | 'desc'
  empty: string
  /** height (px) of the scrollable body before an inner vertical scrollbar appears */
  maxHeight?: number
  /** table min width (px); below this the body scrolls horizontally (mobile) */
  minWidth?: number
}) {
  const [sortKey, setSortKey] = useState(initialSortKey ?? columns[0].key)
  const [sortDir, setSortDir] = useState<'asc' | 'desc'>(initialSortDir)
  const [q, setQ] = useState('')

  const view = useMemo(() => {
    const needle = q.trim().toLowerCase()
    const r = needle
      ? rows.filter(row => columns.some(c => String(c.value(row)).toLowerCase().includes(needle)))
      : rows.slice()
    const col = columns.find(c => c.key === sortKey)
    if (col) {
      r.sort((a, b) => {
        const av = col.value(a), bv = col.value(b)
        const cmp = typeof av === 'number' && typeof bv === 'number'
          ? av - bv
          : String(av).localeCompare(String(bv), undefined, { numeric: true })
        return sortDir === 'asc' ? cmp : -cmp
      })
    }
    return r
  }, [rows, columns, q, sortKey, sortDir])

  const toggle = (key: string) => {
    if (key === sortKey) setSortDir(d => (d === 'asc' ? 'desc' : 'asc'))
    else { setSortKey(key); setSortDir('asc') }
  }

  const th: React.CSSProperties = {
    padding: '7px 10px', fontSize: '0.6rem', letterSpacing: '0.09em', textTransform: 'uppercase',
    fontWeight: 700, color: 'var(--text-dim)', userSelect: 'none', cursor: 'pointer',
    position: 'sticky', top: 0, zIndex: 1, background: 'var(--bg-card)',
    boxShadow: 'inset 0 -1px 0 var(--border)',
  }
  const td: React.CSSProperties = {
    padding: '6px 10px', fontSize: '0.72rem', fontFamily: 'var(--font-mono)',
    color: 'var(--text)', borderBottom: '1px solid var(--border)', verticalAlign: 'top',
    overflowWrap: 'anywhere', wordBreak: 'break-word',
  }
  const totalW = columns.reduce((s, c) => s + (c.w ?? 1), 0)

  return (
    <div>
      <div style={{ padding: '8px 12px', borderBottom: '1px solid var(--border)', display: 'flex', alignItems: 'center', gap: 8 }}>
        <input
          value={q}
          onChange={e => setQ(e.target.value)}
          placeholder={`Filter ${rows.length} rows…`}
          className="vs-input"
          style={{ fontSize: '0.7rem', padding: '3px 8px', width: 200 }}
        />
        <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>
          {view.length}{view.length !== rows.length ? ` / ${rows.length}` : ''}
        </span>
      </div>
      {/* Table body scrolls on its own: vertical past maxHeight, horizontal below minWidth (mobile). */}
      <div style={{ maxHeight, overflow: 'auto' }}>
        <table style={{ borderCollapse: 'collapse', width: '100%', minWidth, tableLayout: 'fixed' }}>
          <colgroup>
            {columns.map(c => (
              <col key={c.key} style={{ width: `${((c.w ?? 1) / totalW) * 100}%` }} />
            ))}
          </colgroup>
          <thead>
            <tr>
              {columns.map(c => {
                const active = c.key === sortKey
                return (
                  <th
                    key={c.key}
                    onClick={() => toggle(c.key)}
                    style={{ ...th, textAlign: c.align ?? 'left', color: active ? 'var(--accent)' : th.color }}
                  >
                    {c.label}
                    <span style={{ marginLeft: 4, display: 'inline-flex', verticalAlign: 'middle', opacity: active ? 1 : 0.3 }}>
                      {active
                        ? (sortDir === 'asc' ? <ArrowUp size={10} /> : <ArrowDown size={10} />)
                        : <ArrowUp size={10} />}
                    </span>
                  </th>
                )
              })}
            </tr>
          </thead>
          <tbody>
            {view.map((row, i) => (
              <tr key={i} className="kill-row">
                {columns.map(c => (
                  <td key={c.key} style={{ ...td, textAlign: c.align ?? 'left', color: c.color ?? td.color }}>
                    {c.render ? c.render(row) : String(c.value(row))}
                  </td>
                ))}
              </tr>
            ))}
            {view.length === 0 && (
              <tr>
                <td colSpan={columns.length} style={{ ...td, color: 'var(--text-dim)', textAlign: 'center', padding: '20px' }}>
                  {rows.length === 0 ? empty : 'No rows match the filter.'}
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
    </div>
  )
}

function Section({
  title, icon: Icon, count, children,
}: { title: string; icon: IconComponent; count: number; children: React.ReactNode }) {
  return (
    <div className="vs-card" style={{ overflow: 'hidden', flexShrink: 0 }}>
      <div style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '10px 14px', borderBottom: '1px solid var(--border)' }}>
        <Icon size={13} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.72rem', fontWeight: 700, letterSpacing: '0.1em', textTransform: 'uppercase' }}>{title}</span>
        <span style={{ marginLeft: 'auto', fontSize: '0.65rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>{count}</span>
      </div>
      {children}
    </div>
  )
}

// ── GCI / AWACS voice reference (static — same on every round) ─────────────
function GciReference() {
  const [open, setOpen] = useState(false)
  const ask: [string, string][] = [
    ['"Magic, radio check"', '"Loud and clear" — confirms two-way.'],
    ['"Magic, checking in"', '"Radar contact, copy the picture, bullseye is …" + group count.'],
    ['"Magic, bogey dope"', 'Nearest hostile group: bearing, range, altitude, aspect, type.'],
    ['"Magic, picture"', 'The whole picture — every group GCI holds for you, nearest first.'],
    ['"Magic, declare"', 'Hostile / clean for the contact nearest you (or a bullseye point you name).'],
    ['"Magic, alpha check"', 'Your own position as bullseye bearing/range.'],
    ['"Magic, commit"', 'GCI takes you onto the group and feeds running intercept vectors to the merge.'],
  ]
  const hear: [string, string][] = [
    ['Threat', 'A hostile is close and dangerous — highest priority, jumps the quiet timer.'],
    ['Bogey → bandit → hostile', 'A fresh hit ripens in identity as the track firms up.'],
    ['North / south / center group', 'Shared group names when several flights see the same raid.'],
    ['Cold · splitting · merged · faded', 'Turned away · split up · inside 3 nm · lost radar contact.'],
    ['SAM launch / SAM threat — "defend, defend"', 'Missile in the air, or a SAM ring now covers you.'],
    ['Splash', 'A hostile you were warned about went down.'],
    ['All players: chute / tumbleweed / support', 'CSAR cue · radar net down · tanker & AWACS location.'],
  ]
  const cmds: [string, string][] = [
    ['-gci', 'Show your current settings'],
    ['-gci on / off', 'Unmute / mute all GCI calls to you'],
    ['-gci imperial / metric', 'Nautical miles + feet, or km + metres'],
    ['-gci braa / bulls / clock', 'Contact position from your jet, the bullseye, or a clock code'],
    ['-gci auto', 'Follow the server defaults'],
  ]
  const row = (k: string, v: string) => (
    <div key={k} style={{ display: 'grid', gridTemplateColumns: '210px 1fr', gap: 10, padding: '5px 0', borderBottom: '1px solid var(--border)' }}>
      <span style={{ fontFamily: 'var(--font-mono)', fontSize: '0.68rem', color: 'var(--accent)' }}>{k}</span>
      <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)', lineHeight: 1.5 }}>{v}</span>
    </div>
  )
  const sub = (t: string) => (
    <div style={{ fontSize: '0.62rem', fontWeight: 700, letterSpacing: '0.12em', textTransform: 'uppercase', color: 'var(--text-dim)', margin: '12px 0 4px' }}>{t}</div>
  )
  return (
    <div className="vs-card" style={{ overflow: 'hidden', flexShrink: 0 }}>
      <button
        onClick={() => setOpen(o => !o)}
        style={{
          width: '100%', display: 'flex', alignItems: 'center', gap: 8, padding: '10px 14px',
          borderBottom: open ? '1px solid var(--border)' : 'none', background: 'none', border: 'none',
          cursor: 'pointer', color: 'var(--text)', textAlign: 'left',
        }}
      >
        <Headphones size={13} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.72rem', fontWeight: 700, letterSpacing: '0.1em', textTransform: 'uppercase' }}>GCI / AWACS Voice</span>
        <span style={{ marginLeft: 'auto', fontSize: '0.62rem', color: 'var(--text-dim)' }}>{open ? 'HIDE' : 'HOW TO TALK TO GCI'}</span>
      </button>
      {open && (
        <div style={{ padding: '10px 14px 14px' }}>
          <p style={{ fontSize: '0.72rem', color: 'var(--text-muted)', lineHeight: 1.6, margin: '0 0 4px' }}>
            A live controller works your coalition's radar picture over SRS and calls contacts to you by name.
            The frequency is on a "GCI: <span style={{ fontFamily: 'var(--font-mono)' }}>&lt;callsign&gt; on &lt;freq&gt;</span>"
            note when you slot in, and on the <strong style={{ color: 'var(--text)' }}>Comms Card</strong> under
            Situation. Blue is usually <strong style={{ color: 'var(--text)' }}>Magic</strong>, red{' '}
            <strong style={{ color: 'var(--text)' }}>Overlord</strong>.
            It talks on its own — keying up to ask only works where the server enabled speech recognition.
          </p>
          {sub('Ask GCI (say the callsign first)')}
          {ask.map(([k, v]) => row(k, v))}
          {sub('Calls you hear unprompted')}
          {hear.map(([k, v]) => row(k, v))}
          {sub('Tune your own calls — chat, or F10 → EWR → GCI Voice')}
          {cmds.map(([k, v]) => row(k, v))}
        </div>
      )}
    </div>
  )
}

// ── PDF ────────────────────────────────────────────────────────────────────
type PdfCol = { h: string; w: number }

/**
 * The kneeboard PDF. `sit` is optional — when the situational briefing loaded,
 * its narrative, tasking and comms card lead the document, because that is the
 * part that goes stale and the part a pilot actually reads on the ramp.
 */
export function buildPdf(b: Briefing, sit?: SituationReport) {
  const doc = new jsPDF({ unit: 'pt', format: 'a4', orientation: 'portrait' })
  const W = doc.internal.pageSize.getWidth()
  const H = doc.internal.pageSize.getHeight()
  const M = 32
  const ROW = 13
  let first = true

  const bg = () => { doc.setFillColor(15, 18, 20); doc.rect(0, 0, W, H, 'F') }

  const fit = (s: string, w: number): string => {
    if (doc.getTextWidth(s) <= w) return s
    let lo = 0, hi = s.length
    while (lo < hi) {
      const mid = (lo + hi + 1) >> 1
      if (doc.getTextWidth(s.slice(0, mid) + '…') <= w) lo = mid
      else hi = mid - 1
    }
    return s.slice(0, lo) + '…'
  }

  const heading = (title: string) => {
    if (!first) doc.addPage()
    first = false
    bg()
    doc.setTextColor(142, 200, 63)
    doc.setFont('helvetica', 'bold')
    doc.setFontSize(15)
    doc.text(title, M, M + 4)
    doc.setFont('courier', 'normal')
    doc.setFontSize(8)
    doc.setTextColor(120, 140, 110)
    doc.text(
      `${b.side.toUpperCase()}   ${new Date(b.generated).toISOString().slice(0, 16).replace('T', ' ')}Z`,
      W - M, M + 4, { align: 'right' },
    )
    doc.setDrawColor(60, 80, 40)
    doc.line(M, M + 12, W - M, M + 12)
  }

  const page = (title: string, cols: PdfCol[], rows: string[][]) => {
    heading(title)
    const x: number[] = []
    let acc = M
    for (const c of cols) { x.push(acc); acc += c.w }

    let y = M + 30
    const drawHeader = () => {
      doc.setFont('courier', 'bold')
      doc.setFontSize(8)
      doc.setTextColor(200, 200, 130)
      cols.forEach((c, i) => doc.text(c.h, x[i], y))
      y += 4
      doc.setDrawColor(50, 60, 40)
      doc.line(M, y, W - M, y)
      y += 12
      doc.setFont('courier', 'normal')
      doc.setTextColor(200, 230, 160)
    }
    drawHeader()

    if (rows.length === 0) {
      doc.setTextColor(120, 120, 120)
      doc.text('— none —', M, y)
      return
    }
    for (const row of rows) {
      if (y > H - M) { doc.addPage(); bg(); y = M + 20; drawHeader() }
      row.forEach((cell, i) => doc.text(fit(cell, cols[i].w - 8), x[i], y))
      y += ROW
    }
  }

  /** A prose page: wrapped paragraphs and indented bullet lines. */
  const textPage = (title: string, blocks: { text: string; bold?: boolean; indent?: number }[]) => {
    heading(title)
    let y = M + 30
    for (const blk of blocks) {
      doc.setFont('courier', blk.bold ? 'bold' : 'normal')
      doc.setFontSize(blk.bold ? 9 : 8)
      doc.setTextColor(blk.bold ? 200 : 200, blk.bold ? 200 : 230, blk.bold ? 130 : 160)
      const indent = blk.indent ?? 0
      const lines = doc.splitTextToSize(blk.text, W - M * 2 - indent) as string[]
      for (const line of lines) {
        if (y > H - M) { doc.addPage(); bg(); y = M + 20 }
        doc.text(line, M + indent, y)
        y += blk.bold ? 14 : 11
      }
      y += 3
    }
  }

  if (sit) {
    const p = sit.posture
    const blocks: { text: string; bold?: boolean; indent?: number }[] = [
      { text: sit.headline },
    ]
    if (p.last_stand) blocks.push({ text: p.last_stand, bold: true })
    blocks.push({
      text:
        `Territory ${p.territory_pct.toFixed(0)}%  |  yours ${p.friendly_objectives} (${p.friendly_primary} primary)  |  ` +
        `enemy ${p.enemy_objectives} (${p.enemy_primary} primary)  |  neutral ${p.neutral_objectives}\n` +
        `Treasury ${p.treasury} pts  |  pilots ${p.players_friendly} v ${p.players_enemy}  |  convoys ${sit.logistics.convoys_active}` +
        (sit.mission_time ? `  |  mission ${sit.mission_time}` : ''),
    })
    if (sit.weather) blocks.push({ text: `WX: ${sit.weather.summary}` })
    if (p.victory_condition) blocks.push({ text: `Victory: ${p.victory_condition}` })
    blocks.push({ text: 'AIR PICTURE', bold: true })
    blocks.push({
      text: sit.air.radar_blind
        ? 'Radar net reporting nothing — blind, not clear.'
        : `${sit.air.hostile_tracks} hostile track(s) (${sit.air.stale_tracks} coasting), ` +
          `${sit.air.friendly_airborne} friendly airborne. ${sit.air.axis ?? ''}` +
          (sit.air.nearest
            ? `\nNearest: ${sit.air.nearest.class} ${String(sit.air.nearest.bearing_deg).padStart(3, '0')}°/` +
              `${sit.air.nearest.range_nm.toFixed(0)}nm off ${sit.air.nearest.near}, ` +
              `${sit.air.nearest.alt_ft} ft, ${sit.air.nearest.speed_kts} kt`
            : ''),
      indent: 10,
    })
    blocks.push({ text: 'KNOWN AIR DEFENCE (your intel only)', bold: true })
    if (sit.threats.length === 0) {
      blocks.push({ text: 'Nothing held. Assume the SAM picture is unmapped.', indent: 10 })
    } else {
      for (const t of sit.threats.slice(0, 12)) {
        blocks.push({
          text:
            `${t.label} near ${t.near ?? '?'} — ` +
            `${t.radius_m ? `${(t.radius_m / 1852).toFixed(0)}nm ring` : 'ring unknown'}, ` +
            `${(t.confidence * 100).toFixed(0)}% conf, ±${(t.uncertainty_m / 1000).toFixed(1)}km, ${t.source}, ${t.age_s}s old`,
          indent: 10,
        })
      }
    }
    textPage('SITUATION', blocks)

    const taskBlocks: { text: string; bold?: boolean; indent?: number }[] = []
    if (sit.tasking.length === 0) {
      taskBlocks.push({ text: 'Nothing pressing. Soften an enemy objective, or resupply your own.' })
    } else {
      sit.tasking.forEach((t, i) => {
        const br = t.bearing_deg != null && t.range_nm != null
          ? `  ${String(t.bearing_deg).padStart(3, '0')}°/${t.range_nm.toFixed(0)}nm`
          : ''
        taskBlocks.push({ text: `${i + 1}. [${t.urgency.toUpperCase()}] ${t.title}${br}`, bold: true })
        taskBlocks.push({ text: t.detail, indent: 14 })
        if (t.success) taskBlocks.push({ text: `Done when: ${t.success}`, indent: 14 })
        if (t.roles.length) taskBlocks.push({ text: `Wants: ${t.roles.join(', ')}`, indent: 14 })
        taskBlocks.push({ text: `${t.lat.toFixed(4)}, ${t.lon.toFixed(4)}   ${fmtCoord(t.lat, t.lon)}`, indent: 14 })
      })
    }
    textPage('TASKING', taskBlocks)

    page('COMMS CARD',
      [{ h: 'CH', w: 34 }, { h: 'FREQ MHz', w: 66 }, { h: 'MOD', w: 34 }, { h: 'STATION', w: 175 }, { h: 'PURPOSE / LIVE', w: 222 }],
      sit.comms.map(c => [
        c.preset != null ? String(c.preset) : '—',
        c.freq_mhz.toFixed(3),
        c.modulation,
        c.label + (c.live ? ' [UP]' : ''),
        c.note ?? c.purpose ?? '—',
      ]).concat(
        sit.flight_channels.map(([n, f]) => ['—', f.toFixed(3), 'AM', n, 'intra-flight']),
      ))

    page('HOTSPOTS',
      [{ h: 'OBJECTIVE', w: 130 }, { h: 'OWNER', w: 50 }, { h: 'HP', w: 34 }, { h: 'LOGI', w: 38 }, { h: 'SUP', w: 34 }, { h: 'STATUS', w: 245 }],
      sit.hotspots.map(h => [
        h.objective, h.owner, `${h.health}%`, `${h.logi}%`, `${h.supply}%`, h.status,
      ]))
  }

  page('NAVAIDS',
    [{ h: 'OBJECTIVE', w: 150 }, { h: 'TYPE', w: 60 }, { h: 'AIDS', w: 205 }, { h: 'POSITION', w: 105 }],
    b.navaids.map(n => [n.deck ? `${n.objective} · ${n.deck}` : n.objective, n.kind, navaidCells(n), fmtCoord(n.lat, n.lon)]))

  page('RADIOS & SUPPORT',
    [{ h: 'STATION', w: 165 }, { h: 'TYPE', w: 55 }, { h: 'FREQ MHz', w: 65 }, { h: 'TACAN / NOTE', w: 235 }],
    b.radios.map(r => [r.label, r.kind, r.freq_mhz != null ? r.freq_mhz.toFixed(3) : '—', r.tacan ?? r.extra ?? '—']))

  page('ARTILLERY',
    [{ h: 'BATTERY', w: 110 }, { h: 'TYPE', w: 150 }, { h: 'MIN', w: 48 }, { h: 'MAX', w: 48 }, { h: 'GUNS', w: 40 }, { h: 'POSITION', w: 124 }],
    b.artillery.map(a => [a.group, a.typ, `${(a.min_range_m / 1000).toFixed(1)}km`, `${(a.max_range_m / 1000).toFixed(1)}km`, String(a.alive), fmtCoord(a.lat, a.lon)]))

  page('DEPLOYABLES',
    [{ h: 'ITEM', w: 200 }, { h: 'COST', w: 50 }, { h: 'CRATES', w: 55 }, { h: 'LIMIT', w: 50 }, { h: 'OUT', w: 45 }, { h: 'TAGS', w: 120 }],
    b.deployables.map(d => [d.name, String(d.cost), String(d.crates_required), String(d.limit), String(d.deployed), d.tags.join(' ')]))

  page('RWR THREATS / HARM CODES',
    [{ h: 'SAM / RADAR TYPE', w: 225 }, { h: 'HARM', w: 70 }, { h: 'BAND', w: 70 }, { h: 'RANGE', w: 70 }, { h: 'SEEN', w: 55 }],
    b.threats.map(t => [t.typ, t.harm_code ?? '—', t.band ?? '—', t.max_range_km != null ? `${t.max_range_km.toFixed(0)}km` : '—', String(t.count)]))

  doc.save(`briefing-${b.side.toLowerCase()}-${Date.now()}.pdf`)
}

// ── Tab ────────────────────────────────────────────────────────────────────

/**
 * The static reference half of the briefing: navaids, radios, artillery,
 * deployables and HARM codes. Nothing here changes minute to minute — it's the
 * kneeboard you print once and fly with. The live picture is the Situation tab.
 */
export default function KneeboardTab({ briefing: b }: { briefing: Briefing }) {
  return (
    <>
      <GciReference />

      <Section title="Navaids" icon={Heading} count={b.navaids.length}>
        <DataTable
          rows={b.navaids}
          initialSortKey="objective"
          empty="No generated navaids."
          columns={[
            {
              key: 'objective', label: 'Objective', w: 3,
              value: n => n.objective + (n.deck ? ` ${n.deck}` : ''),
              render: n => n.deck
                ? <div>{n.objective}<div style={{ color: 'var(--text-dim)', fontSize: '0.66rem' }}>{n.deck}</div></div>
                : n.objective,
            },
            { key: 'kind', label: 'Type', value: n => n.kind, w: 2 },
            { key: 'aids', label: 'Aids', value: n => navaidCells(n), color: '#facc15', w: 5 },
            { key: 'pos', label: 'Position', value: n => n.lat, render: n => fmtCoord(n.lat, n.lon), color: 'var(--text-muted)', w: 3 },
          ]}
        />
      </Section>

      <Section title="Radios & Support" icon={Comms} count={b.radios.length}>
        <DataTable
          rows={b.radios}
          initialSortKey="kind"
          empty="No active AWACS, tankers, or JTACs."
          columns={[
            { key: 'label', label: 'Station', value: r => r.label, w: 4 },
            { key: 'kind', label: 'Type', value: r => r.kind, w: 2 },
            { key: 'freq', label: 'Freq MHz', align: 'right', value: r => r.freq_mhz ?? 0, render: r => r.freq_mhz != null ? r.freq_mhz.toFixed(3) : '—', color: '#facc15', w: 2 },
            { key: 'note', label: 'TACAN / Note', value: r => r.tacan ?? r.extra ?? '', render: r => r.tacan ?? r.extra ?? '—', color: 'var(--text-muted)', w: 5 },
          ]}
        />
      </Section>

      <Section title="Artillery" icon={Cas} count={b.artillery.length}>
        <DataTable
          rows={b.artillery}
          initialSortKey="group"
          empty="No friendly artillery batteries."
          columns={[
            { key: 'group', label: 'Battery', value: a => a.group, w: 3 },
            { key: 'typ', label: 'Type', value: a => a.typ, w: 4 },
            { key: 'min', label: 'Min', align: 'right', value: a => a.min_range_m, render: a => `${(a.min_range_m / 1000).toFixed(1)}km`, w: 1.4 },
            { key: 'max', label: 'Max', align: 'right', value: a => a.max_range_m, render: a => `${(a.max_range_m / 1000).toFixed(1)}km`, color: '#facc15', w: 1.4 },
            { key: 'guns', label: 'Guns', align: 'right', value: a => a.alive, w: 1 },
            { key: 'pos', label: 'Position', value: a => a.lat, render: a => fmtCoord(a.lat, a.lon), color: 'var(--text-muted)', w: 3 },
          ]}
        />
      </Section>

      <Section title="Deployables" icon={Supply} count={b.deployables.length}>
        <DataTable
          rows={b.deployables}
          initialSortKey="name"
          empty="No deployables configured for this side."
          columns={[
            { key: 'name', label: 'Item', value: d => d.name, w: 5 },
            { key: 'cost', label: 'Cost', align: 'right', value: d => d.cost, w: 1.3 },
            { key: 'crates', label: 'Crates', align: 'right', value: d => d.crates_required, w: 1.3 },
            { key: 'limit', label: 'Limit', align: 'right', value: d => d.limit, w: 1.3 },
            { key: 'out', label: 'Out', align: 'right', value: d => d.deployed, render: d => <span style={{ color: d.deployed >= d.limit ? '#f87171' : '#facc15' }}>{d.deployed}</span>, w: 1 },
            { key: 'tags', label: 'Tags', value: d => d.tags.join(' '), render: d => d.tags.join(' ') || '—', color: 'var(--text-muted)', w: 2.5 },
          ]}
        />
      </Section>

      <Section title="RWR Threats / HARM Codes" icon={Sam} count={b.threats.length}>
        <DataTable
          rows={b.threats}
          initialSortKey="count"
          initialSortDir="desc"
          empty="No enemy SAM radars detected in play."
          columns={[
            { key: 'typ', label: 'SAM / Radar Type', value: t => t.typ, w: 5 },
            { key: 'harm', label: 'HARM', value: t => t.harm_code ?? '', render: t => <span style={{ fontWeight: 700, color: t.harm_code ? '#f87171' : 'var(--text-dim)' }}>{t.harm_code ?? '—'}</span>, w: 1.4 },
            { key: 'band', label: 'Band', value: t => t.band ?? '', render: t => t.band ?? '—', w: 1.4 },
            { key: 'range', label: 'Range', align: 'right', value: t => t.max_range_km ?? 0, render: t => t.max_range_km != null ? `${t.max_range_km.toFixed(0)}km` : '—', w: 1.5 },
            { key: 'count', label: 'Seen', align: 'right', value: t => t.count, w: 1 },
          ]}
        />
        {b.threats.some(t => !t.harm_code) && (
          <div style={{ padding: '8px 12px', fontSize: '0.62rem', color: 'var(--text-dim)' }}>
            HARM codes: built-in table for the common DCS emitters, overridable per type via the server's <code>harm_codes</code> config. A “—” means that exact emitter type isn't in either.
          </div>
        )}
      </Section>
    </>
  )
}
