import { useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { jsPDF } from 'jspdf'
import { Radio, Download, Navigation, Crosshair, Package, ShieldAlert } from 'lucide-react'
import { api, type Briefing } from '../api'
import PageHeader from '../components/PageHeader'

type Side = 'Blue' | 'Red'

const th: React.CSSProperties = {
  textAlign: 'left', padding: '6px 10px', fontSize: '0.6rem', letterSpacing: '0.1em',
  textTransform: 'uppercase', color: 'var(--text-dim)', fontWeight: 600,
  borderBottom: '1px solid var(--border)', whiteSpace: 'nowrap',
}
const td: React.CSSProperties = {
  padding: '5px 10px', fontSize: '0.72rem', fontFamily: 'var(--font-mono)',
  color: 'var(--text)', borderBottom: '1px solid var(--border)', whiteSpace: 'nowrap',
}

function Section({
  title, icon: Icon, count, children,
}: { title: string; icon: typeof Radio; count: number; children: React.ReactNode }) {
  return (
    <div className="vs-card" style={{ overflow: 'hidden' }}>
      <div style={{
        display: 'flex', alignItems: 'center', gap: 8, padding: '10px 14px',
        borderBottom: '1px solid var(--border)',
      }}>
        <Icon size={13} style={{ color: 'var(--accent, #38bdf8)' }} />
        <span style={{ fontSize: '0.72rem', fontWeight: 700, letterSpacing: '0.1em', textTransform: 'uppercase' }}>
          {title}
        </span>
        <span style={{ marginLeft: 'auto', fontSize: '0.65rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>
          {count}
        </span>
      </div>
      <div style={{ overflowX: 'auto' }}>{children}</div>
    </div>
  )
}

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

// ── PDF ────────────────────────────────────────────────────────────────────
type Col = { h: string; w: number }

function buildPdf(b: Briefing) {
  const doc = new jsPDF({ unit: 'pt', format: 'a4', orientation: 'landscape' })
  const W = doc.internal.pageSize.getWidth()
  const H = doc.internal.pageSize.getHeight()
  const M = 32
  const ROW = 13
  let first = true

  const bg = () => { doc.setFillColor(15, 18, 20); doc.rect(0, 0, W, H, 'F') }

  // Truncate a string to fit `w` pt at the current font.
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

  const page = (title: string, cols: Col[], rows: string[][]) => {
    if (!first) doc.addPage()
    first = false
    bg()
    // header band
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

    // column x offsets
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

  page(
    'NAVAIDS',
    [{ h: 'OBJECTIVE', w: 150 }, { h: 'TYPE', w: 95 }, { h: 'AIDS', w: 315 }, { h: 'POSITION', w: 195 }],
    b.navaids.map(n => [n.objective, n.kind, navaidCells(n), fmtCoord(n.lat, n.lon)]),
  )

  page(
    'RADIOS & SUPPORT',
    [{ h: 'STATION', w: 220 }, { h: 'TYPE', w: 80 }, { h: 'FREQ MHz', w: 90 }, { h: 'TACAN / NOTE', w: 365 }],
    b.radios.map(r => [
      r.label, r.kind,
      r.freq_mhz != null ? r.freq_mhz.toFixed(3) : '—',
      r.tacan ?? r.extra ?? '—',
    ]),
  )

  page(
    'ARTILLERY',
    [{ h: 'BATTERY', w: 165 }, { h: 'TYPE', w: 200 }, { h: 'MIN', w: 65 }, { h: 'MAX', w: 65 }, { h: 'GUNS', w: 55 }, { h: 'POSITION', w: 185 }],
    b.artillery.map(a => [
      a.group, a.typ,
      `${(a.min_range_m / 1000).toFixed(1)}km`,
      `${(a.max_range_m / 1000).toFixed(1)}km`,
      String(a.alive), fmtCoord(a.lat, a.lon),
    ]),
  )

  page(
    'DEPLOYABLES',
    [{ h: 'ITEM', w: 330 }, { h: 'COST', w: 70 }, { h: 'CRATES', w: 70 }, { h: 'LIMIT', w: 70 }, { h: 'OUT', w: 60 }, { h: 'TAGS', w: 155 }],
    b.deployables.map(d => [
      d.name, String(d.cost), String(d.crates_required),
      String(d.limit), String(d.deployed), d.tags.join(' '),
    ]),
  )

  page(
    'RWR THREATS / HARM CODES',
    [{ h: 'SAM / RADAR TYPE', w: 320 }, { h: 'HARM', w: 90 }, { h: 'BAND', w: 90 }, { h: 'RANGE', w: 90 }, { h: 'SEEN', w: 70 }],
    b.threats.map(t => [
      t.typ, t.harm_code ?? '—', t.band ?? '—',
      t.max_range_km != null ? `${t.max_range_km.toFixed(0)}km` : '—',
      String(t.count),
    ]),
  )

  doc.save(`briefing-${b.side.toLowerCase()}-${Date.now()}.pdf`)
}

export default function BriefingPage() {
  const [side, setSide] = useState<Side>('Blue')
  const { data: b, isLoading, error } = useQuery({
    queryKey: ['briefing', side],
    queryFn: () => api.briefing(side),
    refetchInterval: 30_000,
  })

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
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

      <div className="flex-1 overflow-auto vs-page" style={{ display: 'flex', flexDirection: 'column', gap: 12 }}>
        {isLoading && <div className="vs-card" style={{ padding: 20, color: 'var(--text-dim)' }}>Loading briefing…</div>}
        {error && (
          <div className="vs-card" style={{ padding: 20, color: '#f87171' }}>
            Briefing unavailable — the engine may be unreachable or no round is active.
          </div>
        )}
        {b && (
          <>
            <Section title="Navaids" icon={Navigation} count={b.navaids.length}>
              <table style={{ borderCollapse: 'collapse', width: '100%', minWidth: 560 }}>
                <thead><tr><th style={th}>Objective</th><th style={th}>Type</th><th style={th}>Aids</th><th style={th}>Position</th></tr></thead>
                <tbody>
                  {b.navaids.map((n, i) => (
                    <tr key={i}>
                      <td style={td}>{n.objective}</td>
                      <td style={td}>{n.kind}</td>
                      <td style={{ ...td, color: '#facc15' }}>{navaidCells(n)}</td>
                      <td style={{ ...td, color: 'var(--text-muted)' }}>{fmtCoord(n.lat, n.lon)}</td>
                    </tr>
                  ))}
                  {b.navaids.length === 0 && <tr><td style={{ ...td, color: 'var(--text-dim)' }} colSpan={4}>No generated navaids.</td></tr>}
                </tbody>
              </table>
            </Section>

            <Section title="Radios & Support" icon={Radio} count={b.radios.length}>
              <table style={{ borderCollapse: 'collapse', width: '100%', minWidth: 480 }}>
                <thead><tr><th style={th}>Station</th><th style={th}>Type</th><th style={th}>Freq (MHz)</th><th style={th}>TACAN / Note</th></tr></thead>
                <tbody>
                  {b.radios.map((r, i) => (
                    <tr key={i}>
                      <td style={td}>{r.label}</td>
                      <td style={td}>{r.kind}</td>
                      <td style={{ ...td, color: '#facc15' }}>{r.freq_mhz != null ? r.freq_mhz.toFixed(3) : '—'}</td>
                      <td style={{ ...td, color: 'var(--text-muted)' }}>{r.tacan ?? r.extra ?? '—'}</td>
                    </tr>
                  ))}
                  {b.radios.length === 0 && <tr><td style={{ ...td, color: 'var(--text-dim)' }} colSpan={4}>No active AWACS, tankers, or JTACs.</td></tr>}
                </tbody>
              </table>
            </Section>

            <Section title="Artillery" icon={Crosshair} count={b.artillery.length}>
              <table style={{ borderCollapse: 'collapse', width: '100%', minWidth: 520 }}>
                <thead><tr><th style={th}>Battery</th><th style={th}>Type</th><th style={th}>Min</th><th style={th}>Max</th><th style={th}>Guns</th><th style={th}>Position</th></tr></thead>
                <tbody>
                  {b.artillery.map((a, i) => (
                    <tr key={i}>
                      <td style={td}>{a.group}</td>
                      <td style={td}>{a.typ}</td>
                      <td style={td}>{(a.min_range_m / 1000).toFixed(1)}km</td>
                      <td style={{ ...td, color: '#facc15' }}>{(a.max_range_m / 1000).toFixed(1)}km</td>
                      <td style={td}>{a.alive}</td>
                      <td style={{ ...td, color: 'var(--text-muted)' }}>{fmtCoord(a.lat, a.lon)}</td>
                    </tr>
                  ))}
                  {b.artillery.length === 0 && <tr><td style={{ ...td, color: 'var(--text-dim)' }} colSpan={6}>No friendly artillery batteries.</td></tr>}
                </tbody>
              </table>
            </Section>

            <Section title="Deployables" icon={Package} count={b.deployables.length}>
              <table style={{ borderCollapse: 'collapse', width: '100%', minWidth: 520 }}>
                <thead><tr><th style={th}>Item</th><th style={th}>Cost</th><th style={th}>Crates</th><th style={th}>Limit</th><th style={th}>Out</th><th style={th}>Tags</th></tr></thead>
                <tbody>
                  {b.deployables.map((d, i) => (
                    <tr key={i}>
                      <td style={td}>{d.name}</td>
                      <td style={td}>{d.cost}</td>
                      <td style={td}>{d.crates_required}</td>
                      <td style={td}>{d.limit}</td>
                      <td style={{ ...td, color: d.deployed >= d.limit ? '#f87171' : '#facc15' }}>{d.deployed}</td>
                      <td style={{ ...td, color: 'var(--text-muted)' }}>{d.tags.join(' ') || '—'}</td>
                    </tr>
                  ))}
                  {b.deployables.length === 0 && <tr><td style={{ ...td, color: 'var(--text-dim)' }} colSpan={6}>No deployables configured for this side.</td></tr>}
                </tbody>
              </table>
            </Section>

            <Section title="RWR Threats / HARM Codes" icon={ShieldAlert} count={b.threats.length}>
              <table style={{ borderCollapse: 'collapse', width: '100%', minWidth: 520 }}>
                <thead><tr><th style={th}>SAM / Radar Type</th><th style={th}>HARM</th><th style={th}>Band</th><th style={th}>Range</th><th style={th}>Seen</th></tr></thead>
                <tbody>
                  {b.threats.map((t, i) => (
                    <tr key={i}>
                      <td style={td}>{t.typ}</td>
                      <td style={{ ...td, color: t.harm_code ? '#f87171' : 'var(--text-dim)', fontWeight: 700 }}>{t.harm_code ?? '—'}</td>
                      <td style={td}>{t.band ?? '—'}</td>
                      <td style={td}>{t.max_range_km != null ? `${t.max_range_km.toFixed(0)}km` : '—'}</td>
                      <td style={td}>{t.count}</td>
                    </tr>
                  ))}
                  {b.threats.length === 0 && <tr><td style={{ ...td, color: 'var(--text-dim)' }} colSpan={5}>No enemy SAM radars detected in play.</td></tr>}
                </tbody>
              </table>
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
