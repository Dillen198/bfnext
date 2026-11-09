// Two Pilots-page cards: Rivals (who keeps shooting this pilot down, who they
// keep shooting down, their best sortie) and Compare (this pilot against any
// other, stat by stat).

import React, { useMemo, useState } from 'react'
import { Link as RouterLink } from 'react-router-dom'
import type { Pilot, PilotKill, PilotSortie } from '../api'
import { fmtDuration, fmtHours, kd } from '../lib/format'
import { Award, Rankings, ArrowRight } from '@icons'
import { Card as LvCard } from '../components/Levant'
import { useDashLook } from '../lib/look'
import './levant/pilots.css'

/** bfdb's "no pilot" UCID is all zeros; it is not a rival. */
const realUcid = (u: string | null | undefined): string | null => (u && !/^0+$/.test(u) ? u : null)

const label: React.CSSProperties = { fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }

function Card({ title, icon: Icon, right, children }: { title: string; icon: typeof Award; right?: React.ReactNode; children: React.ReactNode }) {
  return (
    <div className="vs-card p-0">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Icon size={12} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>{title}</span>
        {right && <span className="ml-auto">{right}</span>}
      </div>
      {children}
    </div>
  )
}

function PilotName({ ucid, name }: { ucid?: string | null; name: string }) {
  return ucid
    ? <RouterLink to={`/pilots?ucid=${ucid}`} style={{ color: 'var(--text)', textDecoration: 'none', fontWeight: 600 }}>{name}</RouterLink>
    : <span style={{ color: 'var(--text-muted)' }}>{name}</span>
}

function TopList({ rows, empty, unit }: { rows: { key: string; ucid?: string | null; name: string; n: number }[]; empty: string; unit: string }) {
  if (rows.length === 0) return <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>{empty}</div>
  return (
    <div style={{ display: 'grid', gap: 4 }}>
      {rows.map((r, i) => (
        <div key={r.key} className="flex items-baseline gap-2" style={{ fontSize: '0.72rem' }}>
          <span style={{ color: 'var(--text-dim)', width: 12 }}>{i + 1}</span>
          <PilotName ucid={r.ucid} name={r.name} />
          <span className="ml-auto font-mono" style={{ color: 'var(--text-muted)', fontSize: '0.66rem' }}>{r.n} {unit}{r.n === 1 ? '' : 's'}</span>
        </div>
      ))}
    </div>
  )
}

/** TopList in the desert look: a rank, the name (a link when it is a
 *  player), the count. */
function TopListDesert({ rows, empty, unit }: { rows: { key: string; ucid?: string | null; name: string; n: number }[]; empty: string; unit: string }) {
  if (rows.length === 0) return <p className="pl-none">{empty}</p>
  return (
    <ol className="pl-top">
      {rows.map((r, i) => (
        <li key={r.key} className={i === 0 ? 'first' : undefined}>
          <span className="rk">{i + 1}</span>
          {r.ucid
            ? <RouterLink className="nm" to={`/pilots?ucid=${r.ucid}`}>{r.name}</RouterLink>
            : <span className="nm ai">{r.name}</span>}
          <span className="n"><b>{r.n}</b> {unit}{r.n === 1 ? '' : 's'}</span>
        </li>
      ))}
    </ol>
  )
}

export function RivalsCard({ ucid, sorties, kills, pilots }: { ucid: string; sorties: PilotSortie[]; kills: PilotKill[]; pilots: Pilot[] }) {
  const look = useDashLook()
  const names = useMemo(() => new Map(pilots.map(p => [p.ucid, p.name])), [pilots])
  // Sortie rows carry their killer only from the per-sortie stats server.
  const knowsKillers = sorties.some(s => s.killed_by !== undefined)

  const nemesis = useMemo(() => {
    const m = new Map<string, { ucid: string | null; name: string; n: number }>()
    for (const s of sorties) {
      if (!s.killed_by) continue
      const u = realUcid(s.killed_by_ucid)
      const key = u ?? `type:${s.killed_by}`
      const e = m.get(key) ?? { ucid: u, name: (u && names.get(u)) || s.killed_by, n: 0 }
      e.n++
      m.set(key, e)
    }
    return [...m.entries()].map(([key, e]) => ({ key, ...e })).sort((a, b) => b.n - a.n).slice(0, 3)
  }, [sorties, names])

  const targets = useMemo(() => {
    const m = new Map<string, number>()
    for (const k of kills) {
      const v = realUcid(k.victim_ucid)
      if (v && v !== ucid) m.set(v, (m.get(v) ?? 0) + 1)
    }
    return [...m.entries()].sort((a, b) => b[1] - a[1]).slice(0, 3)
      .map(([u, n]) => ({ key: u, ucid: u, name: names.get(u) ?? u.slice(0, 8), n }))
  }, [kills, names, ucid])

  const best = useMemo(() => {
    let top: PilotSortie | null = null
    const score = (s: PilotSortie) => (s.air_kills ?? 0) * 3 + (s.ground_kills ?? 0)
    for (const s of sorties) if (score(s) > 0 && (!top || score(s) > score(top))) top = s
    return top
  }, [sorties])

  if (look.profile) {
    const needs = 'Needs the updated stats server.'
    return (
      <LvCard tape="Rivals">
        <div className="pl-rivals">
          <div>
            <span className="ds-k">Nemesis · shot them down</span>
            {knowsKillers ? <TopListDesert rows={nemesis} empty="Never shot down." unit="time" /> : <p className="pl-none">{needs}</p>}
          </div>
          <div>
            <span className="ds-k">Favourite targets · players</span>
            <TopListDesert rows={targets} empty="No player kills yet." unit="kill" />
          </div>
          <div>
            <span className="ds-k">Best sortie</span>
            {best ? (
              <div className="pl-best">
                <strong>{best.aircraft.replace(/_/g, ' ')}</strong>
                <dl>
                  <div><dt>A/A</dt><dd className="a">{best.air_kills ?? 0}</dd></div>
                  <div><dt>A/G</dt><dd className="g">{best.ground_kills ?? 0}</dd></div>
                  <div><dt>Time</dt><dd>{fmtDuration(best.duration_secs)}</dd></div>
                </dl>
                <span className="lv-sub">{new Date(best.takeoff).toLocaleDateString([], { day: 'numeric', month: 'short', year: 'numeric' })}</span>
                {best.sortie_id !== undefined && (
                  <RouterLink className="ds-link pl-debrief" to={`/pilots/${encodeURIComponent(ucid)}/sortie/${best.sortie_id}`}>
                    Full debrief <ArrowRight size={11} />
                  </RouterLink>
                )}
              </div>
            ) : (
              <p className="pl-none">{knowsKillers ? 'No kills on a sortie yet.' : needs}</p>
            )}
          </div>
        </div>
      </LvCard>
    )
  }

  return (
    <Card title="Rivals" icon={Award}>
      <div className="p-4" style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(180px, 1fr))', gap: 18 }}>
        <div>
          <div style={{ ...label, marginBottom: 6 }}>Nemesis · shot them down</div>
          {knowsKillers
            ? <TopList rows={nemesis} empty="Never shot down." unit="time" />
            : <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>Needs the updated stats server.</div>}
        </div>
        <div>
          <div style={{ ...label, marginBottom: 6 }}>Favourite targets · players</div>
          <TopList rows={targets} empty="No player kills yet." unit="kill" />
        </div>
        <div>
          <div style={{ ...label, marginBottom: 6 }}>Best sortie</div>
          {best ? (
            <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', display: 'grid', gap: 2 }}>
              <span style={{ color: 'var(--text)', fontWeight: 600 }}>{best.aircraft.replace(/_/g, ' ')}</span>
              <span>{best.air_kills ?? 0} A/A · {best.ground_kills ?? 0} A/G · {fmtDuration(best.duration_secs)}</span>
              <span style={{ color: 'var(--text-dim)' }}>{new Date(best.takeoff).toLocaleDateString([], { day: 'numeric', month: 'short', year: 'numeric' })}</span>
              {best.sortie_id !== undefined && (
                <RouterLink to={`/pilots/${encodeURIComponent(ucid)}/sortie/${best.sortie_id}`}
                  style={{ color: 'var(--accent-bright)', fontSize: '0.6rem', letterSpacing: '0.08em', textDecoration: 'none', marginTop: 2 }}>
                  FULL DEBRIEF →
                </RouterLink>
              )}
            </div>
          ) : (
            <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>{knowsKillers ? 'No kills on a sortie yet.' : 'Needs the updated stats server.'}</div>
          )}
        </div>
      </div>
    </Card>
  )
}

// ── Compare ───────────────────────────────────────────────────────────────────

const ME = 'var(--accent)'
const THEM = '#c9a227'

const logistics = (p: Pilot) => p.repairs + p.supply_transfers + p.troops + p.farps + p.deploys
const kdNum = (p: Pilot) => { const k = p.air_kills + p.ground_kills; return p.deaths > 0 ? k / p.deaths : k }

const STATS: { name: string; v: (p: Pilot) => number; fmt: (p: Pilot) => string; lowerIsBetter?: boolean }[] = [
  { name: 'Air kills', v: p => p.air_kills, fmt: p => String(p.air_kills) },
  { name: 'Ground kills', v: p => p.ground_kills, fmt: p => String(p.ground_kills) },
  { name: 'K/D', v: kdNum, fmt: p => kd(p.air_kills, p.ground_kills, p.deaths) },
  { name: 'Captures', v: p => p.captures, fmt: p => String(p.captures) },
  { name: 'Logistics', v: logistics, fmt: p => String(logistics(p)) },
  { name: 'Deaths', v: p => p.deaths, fmt: p => String(p.deaths), lowerIsBetter: true },
  { name: 'Flight time', v: p => p.hours, fmt: p => fmtHours(p.hours) },
]

export function CompareCard({ pilot, pilots }: { pilot: Pilot; pilots: Pilot[] }) {
  const look = useDashLook()
  const others = useMemo(() => pilots.filter(p => p.ucid !== pilot.ucid).sort((a, b) => a.name.localeCompare(b.name)), [pilots, pilot.ucid])
  const [otherId, setOtherId] = useState<string>('')
  const other = others.find(p => p.ucid === otherId) ?? null
  // Each stat, scaled to the larger of the two, and who has the better of it.
  const rows = other ? STATS.map(st => {
    const a = st.v(pilot), b = st.v(other)
    const max = Math.max(a, b, 1e-9)
    return {
      st, a, b, max,
      aWins: st.lowerIsBetter ? a < b : a > b,
      bWins: st.lowerIsBetter ? b < a : b > a,
    }
  }) : []
  const picker = (
    <select value={otherId} onChange={e => setOtherId(e.target.value)} className="vs-input" aria-label="Pilot to compare with"
      style={look.profile ? undefined : { fontSize: '0.68rem', padding: '0.2rem 0.4rem', maxWidth: 200 }}>
      <option value="">Pick a pilot…</option>
      {others.map(p => <option key={p.ucid} value={p.ucid}>{p.name}</option>)}
    </select>
  )

  if (look.profile) {
    return (
      <LvCard tape="Compare">
        <div className="pl-cmp">
          <div className="pl-cmp-pick"><span className="ds-k">Against</span>{picker}</div>
          {!other ? (
            <p className="pl-none">Pick another pilot to see the two side by side.</p>
          ) : (
            <>
              <div className="pl-cmp-names">
                <span className="me">{pilot.name}</span>
                <span className="ds-k">vs</span>
                <RouterLink className="them" to={`/pilots?ucid=${other.ucid}`}>{other.name}</RouterLink>
              </div>
              {/* Mirrored bars: this pilot grows left, the other right. */}
              <div className="pl-cmp-grid">
                {rows.map(({ st, a, b, max, aWins, bWins }) => (
                  <React.Fragment key={st.name}>
                    <span className={`v${aWins ? ' win' : ''}`}>{st.fmt(pilot)}</span>
                    <div className="bar me"><i style={{ width: `${(a / max) * 100}%`, minWidth: a > 0 ? 2 : 0 }} /></div>
                    <span className="ds-k nm">{st.name}</span>
                    <div className="bar them"><i style={{ width: `${(b / max) * 100}%`, minWidth: b > 0 ? 2 : 0 }} /></div>
                    <span className={`v them${bWins ? ' win' : ''}`}>{st.fmt(other)}</span>
                  </React.Fragment>
                ))}
              </div>
            </>
          )}
        </div>
      </LvCard>
    )
  }

  return (
    <Card title="Compare" icon={Rankings}
      right={picker}>
      {!other ? (
        <div style={{ padding: 16, fontSize: '0.68rem', color: 'var(--text-dim)' }}>Pick another pilot to see the two side by side.</div>
      ) : (
        <div className="p-4">
          <div className="flex items-center" style={{ marginBottom: 10, fontSize: '0.72rem' }}>
            <span className="flex items-center gap-1.5" style={{ color: 'var(--text)', fontWeight: 600 }}>
              <span style={{ width: 8, height: 8, borderRadius: 2, background: ME, display: 'inline-block' }} />{pilot.name}
            </span>
            <span className="ml-auto flex items-center gap-1.5" style={{ color: 'var(--text)', fontWeight: 600 }}>
              <PilotName ucid={other.ucid} name={other.name} />
              <span style={{ width: 8, height: 8, borderRadius: 2, background: THEM, display: 'inline-block' }} />
            </span>
          </div>
          {/* Mirrored bars: this pilot grows left, the other right, each
              scaled to the larger of the two on that stat. */}
          <div style={{ display: 'grid', gridTemplateColumns: 'auto 1fr minmax(80px, auto) 1fr auto', gap: '6px 8px', alignItems: 'center' }}>
            {rows.map(({ st, a, b, max, aWins, bWins }) => {
              return (
                <React.Fragment key={st.name}>
                  <span className="font-mono" style={{ fontSize: '0.7rem', color: aWins ? 'var(--text)' : 'var(--text-muted)', fontWeight: aWins ? 700 : 400, textAlign: 'right' }}>{st.fmt(pilot)}</span>
                  <div style={{ display: 'flex', justifyContent: 'flex-end' }}>
                    <div style={{ width: `${(a / max) * 100}%`, height: 8, background: ME, borderRadius: '4px 0 0 4px', minWidth: a > 0 ? 2 : 0 }} />
                  </div>
                  <span style={{ ...label, textAlign: 'center', whiteSpace: 'nowrap' }}>{st.name}</span>
                  <div>
                    <div style={{ width: `${(b / max) * 100}%`, height: 8, background: THEM, borderRadius: '0 4px 4px 0', minWidth: b > 0 ? 2 : 0 }} />
                  </div>
                  <span className="font-mono" style={{ fontSize: '0.7rem', color: bWins ? 'var(--text)' : 'var(--text-muted)', fontWeight: bWins ? 700 : 400 }}>{st.fmt(other)}</span>
                </React.Fragment>
              )
            })}
          </div>
        </div>
      )}
    </Card>
  )
}
