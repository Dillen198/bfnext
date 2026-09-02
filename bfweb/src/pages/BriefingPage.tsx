import { useMemo, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { jsPDF } from 'jspdf'
import { Radio, Download, Navigation, Crosshair, Package, ShieldAlert, ArrowUp, ArrowDown } from 'lucide-react'
import { api, type Briefing } from '../api'
import PageHeader from '../components/PageHeader'

type Side = 'Blue' | 'Red'

function fmtCoord(lat: number, lon: number): string {
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
  /** value used for sorting + filtering (defaults to render output when a string) */
  value: (row: T) => string | number
  render?: (row: T) => React.ReactNode
  color?: string
  /** column may wrap onto multiple lines (long free text) */
  wrap?: boolean
}

function DataTable<T>({
  columns, rows, initialSortKey, initialSortDir = 'asc', empty,
}: {
  columns: Column<T>[]
  rows: T[]
  initialSortKey?: string
  initialSortDir?: 'asc' | 'desc'
  empty: string
}) {
  const [sortKey, setSortKey] = useState(initialSortKey ?? columns[0].key)
  const [sortDir, setSortDir] = useState<'asc' | 'desc'>(initialSortDir)
  const [q, setQ] = useState('')

  const view = useMemo(() => {
    const needle = q.trim().toLowerCase()
    let r = needle
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
    fontWeight: 700, color: 'var(--text-dim)', borderBottom: '1px solid var(--border)',
    userSelect: 'none', cursor: 'pointer', whiteSpace: 'nowrap', position: 'sticky', top: 0,
    background: 'var(--bg-card)',
  }
  const td: React.CSSProperties = {
    padding: '6px 10px', fontSize: '0.72rem', fontFamily: 'var(--font-mono)',
    color: 'var(--text)', borderBottom: '1px solid var(--border)', verticalAlign: 'top',
  }

  return (
    <div>
      <div style={{ padding: '8px 12px', borderBottom: '1px solid var(--border)' }}>
        <input
          value={q}
          onChange={e => setQ(e.target.value)}
          placeholder={`Filter ${rows.length} rows…`}
          className="vs-input"
          style={{ fontSize: '0.7rem', padding: '3px 8px', width: 200 }}
        />
      </div>
      <div style={{ overflowX: 'auto', maxWidth: '100%' }}>
        <table style={{ borderCollapse: 'collapse', width: '100%' }}>
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
                  <td
                    key={c.key}
                    style={{
                      ...td,
                      textAlign: c.align ?? 'left',
                      color: c.color ?? td.color,
                      whiteSpace: c.wrap ? 'normal' : 'nowrap',
                      minWidth: c.wrap ? 160 : undefined,
                    }}
                  >
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
}: { title: string; icon: typeof Radio; count: number; children: React.ReactNode }) {
  return (
    <div className="vs-card" style={{ overflow: 'hidden' }}>
      <div style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '10px 14px', borderBottom: '1px solid var(--border)' }}>
        <Icon size={13} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.72rem', fontWeight: 700, letterSpacing: '0.1em', textTransform: 'uppercase' }}>{title}</span>
        <span style={{ marginLeft: 'auto', fontSize: '0.65rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>{count}</span>
      </div>
      {children}
    </div>
  )
}

// ── PDF ────────────────────────────────────────────────────────────────────
type PdfCol = { h: string; w: number }

function buildPdf(b: Briefing) {
  const doc = new jsPDF({ unit: 'pt', format: 'a4', orientation: 'landscape' })
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

  const page = (title: string, cols: PdfCol[], rows: string[][]) => {
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

  page('NAVAIDS',
    [{ h: 'OBJECTIVE', w: 150 }, { h: 'TYPE', w: 95 }, { h: 'AIDS', w: 315 }, { h: 'POSITION', w: 195 }],
    b.navaids.map(n => [n.objective, n.kind, navaidCells(n), fmtCoord(n.lat, n.lon)]))

  page('RADIOS & SUPPORT',
    [{ h: 'STATION', w: 220 }, { h: 'TYPE', w: 80 }, { h: 'FREQ MHz', w: 90 }, { h: 'TACAN / NOTE', w: 365 }],
    b.radios.map(r => [r.label, r.kind, r.freq_mhz != null ? r.freq_mhz.toFixed(3) : '—', r.tacan ?? r.extra ?? '—']))

  page('ARTILLERY',
    [{ h: 'BATTERY', w: 165 }, { h: 'TYPE', w: 200 }, { h: 'MIN', w: 65 }, { h: 'MAX', w: 65 }, { h: 'GUNS', w: 55 }, { h: 'POSITION', w: 185 }],
    b.artillery.map(a => [a.group, a.typ, `${(a.min_range_m / 1000).toFixed(1)}km`, `${(a.max_range_m / 1000).toFixed(1)}km`, String(a.alive), fmtCoord(a.lat, a.lon)]))

  page('DEPLOYABLES',
    [{ h: 'ITEM', w: 330 }, { h: 'COST', w: 70 }, { h: 'CRATES', w: 70 }, { h: 'LIMIT', w: 70 }, { h: 'OUT', w: 60 }, { h: 'TAGS', w: 155 }],
    b.deployables.map(d => [d.name, String(d.cost), String(d.crates_required), String(d.limit), String(d.deployed), d.tags.join(' ')]))

  page('RWR THREATS / HARM CODES',
    [{ h: 'SAM / RADAR TYPE', w: 320 }, { h: 'HARM', w: 90 }, { h: 'BAND', w: 90 }, { h: 'RANGE', w: 90 }, { h: 'SEEN', w: 70 }],
    b.threats.map(t => [t.typ, t.harm_code ?? '—', t.band ?? '—', t.max_range_km != null ? `${t.max_range_km.toFixed(0)}km` : '—', String(t.count)]))

  doc.save(`briefing-${b.side.toLowerCase()}-${Date.now()}.pdf`)
}

const MOCK = import.meta.env.DEV && new URLSearchParams(location.search).has('mock')

function mockBriefing(side: Side): Briefing {
  return {
    side, generated: new Date().toISOString(),
    navaids: [
      { objective: 'Incirlik', kind: 'Airbase', lat: 37.002, lon: 35.42, tacan: '21X INC', ndb_khz: 350, icls: null, link4_mhz: null, acls: false, brc: null },
      { objective: 'Blue Strike Group', kind: 'Carrier Group', lat: 35.1, lon: 34.9, tacan: '74Y CVN', ndb_khz: null, icls: 11, link4_mhz: 336, acls: true, brc: 82 },
      { objective: 'Kingsfield Logistics Hub Alpha', kind: 'Logistics Hub', lat: 34.98, lon: 33.0, tacan: '2Y KIN', ndb_khz: 375, icls: null, link4_mhz: null, acls: false, brc: null },
    ],
    radios: [
      { label: 'AWACS Magic', kind: 'AWACS', freq_mhz: 251.0, tacan: '52Y MAG', extra: null },
      { label: 'Tanker Texaco 1-1 heavy drogue basket', kind: 'TANKER', freq_mhz: 274.0, tacan: '38Y TEX', extra: null },
      { label: 'JTAC 1042', kind: 'JTAC', freq_mhz: null, tacan: null, extra: 'laser 1688 near Ahmed al Jaber' },
    ],
    artillery: [
      { group: 'BLUE ARTY 3', typ: 'M109 Paladin 155mm SP Howitzer', lat: 34.5, lon: 33.1, min_range_m: 4000, max_range_m: 22000, alive: 3 },
      { group: 'BLUE MLRS 1', typ: 'M142 HIMARS', lat: 34.6, lon: 33.05, min_range_m: 8000, max_range_m: 70000, alive: 2 },
    ],
    deployables: [
      { name: 'Deployables / SAM / SA-11 Buk Battery', cost: 1200, crates_required: 3, limit: 2, deployed: 1, tags: [] },
      { name: 'Deployables / Radar / Early Warning Radar 55G6', cost: 400, crates_required: 1, limit: 4, deployed: 4, tags: ['EWR'] },
      { name: 'Deployables / JTAC / Ground JTAC Humvee', cost: 200, crates_required: 1, limit: 3, deployed: 0, tags: ['JTAC'] },
    ],
    threats: [
      { typ: 'SA-11 Buk LN 9A310M1', count: 4, band: 'Xband', harm_code: '115', max_range_km: 35 },
      { typ: 'Kub 1S91 str', count: 2, band: 'Cband', harm_code: '108', max_range_km: 24 },
      { typ: 'ZSU-23-4 Shilka', count: 6, band: null, harm_code: '121', max_range_km: 2 },
      { typ: 'p-19 s-125 sr', count: 1, band: null, harm_code: null, max_range_km: null },
    ],
  }
}

export default function BriefingPage() {
  const [side, setSide] = useState<Side>('Blue')
  const { data: fetched, isLoading, error } = useQuery({
    queryKey: ['briefing', side],
    queryFn: () => api.briefing(side),
    refetchInterval: 30_000,
    enabled: !MOCK,
  })
  const b = MOCK ? mockBriefing(side) : fetched

  return (
    <div className="flex flex-col flex-1 overflow-hidden" style={{ minWidth: 0 }}>
      <PageHeader
        title="BRIEFING"
        sub={b ? `${b.navaids.length} navaids · ${b.radios.length} stations · ${b.threats.length} threat types` : 'kneeboard data'}
        right={
          <div style={{ display: 'flex', gap: 8, alignItems: 'center' }}>
            <div style={{ display: 'flex', border: '1px solid var(--border)', borderRadius: 4, overflow: 'hidden' }}>
              {(['Blue', 'Red'] as Side[]).map(s => (
                <button
                  key={s}
                  onClick={() => setSide(s)}
                  style={{
                    padding: '4px 12px', fontSize: '0.68rem', fontWeight: 700, letterSpacing: '0.06em',
                    border: 'none', cursor: 'pointer',
                    background: side === s ? (s === 'Blue' ? '#1d4ed8' : '#b91c1c') : 'var(--bg-elevated)',
                    color: side === s ? '#fff' : 'var(--text-dim)',
                  }}
                >{s.toUpperCase()}</button>
              ))}
            </div>
            <button
              onClick={() => b && buildPdf(b)}
              disabled={!b}
              className="vs-btn"
              style={{ display: 'flex', alignItems: 'center', gap: 6, fontSize: '0.68rem', padding: '5px 12px' }}
            >
              <Download size={12} /> KNEEBOARD PDF
            </button>
          </div>
        }
      />

      <div className="flex-1 overflow-auto vs-page" style={{ display: 'flex', flexDirection: 'column', gap: 12, minWidth: 0 }}>
        {isLoading && <div className="vs-card" style={{ padding: 20, color: 'var(--text-dim)' }}>Loading briefing…</div>}
        {error && (
          <div className="vs-card" style={{ padding: 20, color: '#f87171' }}>
            Briefing unavailable — the engine may be unreachable or no round is active.
          </div>
        )}
        {b && (
          <>
            <Section title="Navaids" icon={Navigation} count={b.navaids.length}>
              <DataTable
                rows={b.navaids}
                initialSortKey="objective"
                empty="No generated navaids."
                columns={[
                  { key: 'objective', label: 'Objective', value: n => n.objective },
                  { key: 'kind', label: 'Type', value: n => n.kind },
                  { key: 'aids', label: 'Aids', value: n => navaidCells(n), color: '#facc15', wrap: true },
                  { key: 'pos', label: 'Position', value: n => n.lat, render: n => fmtCoord(n.lat, n.lon), color: 'var(--text-muted)' },
                ]}
              />
            </Section>

            <Section title="Radios & Support" icon={Radio} count={b.radios.length}>
              <DataTable
                rows={b.radios}
                initialSortKey="kind"
                empty="No active AWACS, tankers, or JTACs."
                columns={[
                  { key: 'label', label: 'Station', value: r => r.label },
                  { key: 'kind', label: 'Type', value: r => r.kind },
                  { key: 'freq', label: 'Freq MHz', align: 'right', value: r => r.freq_mhz ?? 0, render: r => r.freq_mhz != null ? r.freq_mhz.toFixed(3) : '—', color: '#facc15' },
                  { key: 'note', label: 'TACAN / Note', value: r => r.tacan ?? r.extra ?? '', render: r => r.tacan ?? r.extra ?? '—', color: 'var(--text-muted)', wrap: true },
                ]}
              />
            </Section>

            <Section title="Artillery" icon={Crosshair} count={b.artillery.length}>
              <DataTable
                rows={b.artillery}
                initialSortKey="group"
                empty="No friendly artillery batteries."
                columns={[
                  { key: 'group', label: 'Battery', value: a => a.group },
                  { key: 'typ', label: 'Type', value: a => a.typ, wrap: true },
                  { key: 'min', label: 'Min', align: 'right', value: a => a.min_range_m, render: a => `${(a.min_range_m / 1000).toFixed(1)}km` },
                  { key: 'max', label: 'Max', align: 'right', value: a => a.max_range_m, render: a => `${(a.max_range_m / 1000).toFixed(1)}km`, color: '#facc15' },
                  { key: 'guns', label: 'Guns', align: 'right', value: a => a.alive },
                  { key: 'pos', label: 'Position', value: a => a.lat, render: a => fmtCoord(a.lat, a.lon), color: 'var(--text-muted)' },
                ]}
              />
            </Section>

            <Section title="Deployables" icon={Package} count={b.deployables.length}>
              <DataTable
                rows={b.deployables}
                initialSortKey="name"
                empty="No deployables configured for this side."
                columns={[
                  { key: 'name', label: 'Item', value: d => d.name, wrap: true },
                  { key: 'cost', label: 'Cost', align: 'right', value: d => d.cost },
                  { key: 'crates', label: 'Crates', align: 'right', value: d => d.crates_required },
                  { key: 'limit', label: 'Limit', align: 'right', value: d => d.limit },
                  { key: 'out', label: 'Out', align: 'right', value: d => d.deployed, render: d => <span style={{ color: d.deployed >= d.limit ? '#f87171' : '#facc15' }}>{d.deployed}</span> },
                  { key: 'tags', label: 'Tags', value: d => d.tags.join(' '), render: d => d.tags.join(' ') || '—', color: 'var(--text-muted)' },
                ]}
              />
            </Section>

            <Section title="RWR Threats / HARM Codes" icon={ShieldAlert} count={b.threats.length}>
              <DataTable
                rows={b.threats}
                initialSortKey="count"
                initialSortDir="desc"
                empty="No enemy SAM radars detected in play."
                columns={[
                  { key: 'typ', label: 'SAM / Radar Type', value: t => t.typ, wrap: true },
                  { key: 'harm', label: 'HARM', value: t => t.harm_code ?? '', render: t => <span style={{ fontWeight: 700, color: t.harm_code ? '#f87171' : 'var(--text-dim)' }}>{t.harm_code ?? '—'}</span> },
                  { key: 'band', label: 'Band', value: t => t.band ?? '', render: t => t.band ?? '—' },
                  { key: 'range', label: 'Range', align: 'right', value: t => t.max_range_km ?? 0, render: t => t.max_range_km != null ? `${t.max_range_km.toFixed(0)}km` : '—' },
                  { key: 'count', label: 'Seen', align: 'right', value: t => t.count },
                ]}
              />
              {b.threats.some(t => !t.harm_code) && (
                <div style={{ padding: '8px 12px', fontSize: '0.62rem', color: 'var(--text-dim)' }}>
                  HARM codes come from the server's <code>harm_codes</code> config (DCS type → ALIC code). Types without a code aren't mapped yet.
                </div>
              )}
            </Section>
          </>
        )}
      </div>
    </div>
  )
}
