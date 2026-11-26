import React, { useState, useMemo, useRef, useCallback } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import QueryState from '../components/QueryState'
import { kd, sl, fmtHours, fmtDuration, shownTag } from '../lib/format'
import { Link as RouterLink, useSearchParams } from 'react-router-dom'
import type { FlightRow } from '../replay/data'
import { api, type Pilot, type PilotSortie, type TheaterBreakdown, type PilotKill, type PilotDeploy, type PilotProgression, type Challenge, type SortieOutcome } from '../api'
import PageHeader from '../components/PageHeader'
import { Card, Tape } from '../components/Levant'
import { useDashLook } from '../lib/look'
import { campaign } from '../config/campaign'
import { rankFor, nextRankAt } from '../ranks'
import SortieDetail from './SortieDetail'
import PilotCharts from './PilotCharts'
import { RivalsCard, CompareCard } from './PilotRivals'
import { OUTCOME, outcomeOf, replayFor } from '../lib/sortie'
import { useAuth } from '../context/AuthContext'
import { useRoundNo } from '../lib/rounds'
import {
  Pilot as PilotIcon,
  KillFeed as KillIcon,
  Award,
  Aircraft,
  Shield,
  Supply,
  Search,
  Clock,
  ChevronLeft,
  ChevronRight,
  Link,
  RotateCcw,
  ChevronDown,
  ChevronUp,
  PlayCircle,
  Check,
  ExternalLink,
} from '@icons'
import './levant/pilots.css'

// ── Helpers ──────────────────────────────────────────────────────────────────

function fmtTime(iso: string): string {
  return new Date(iso).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', hour12: false })
}

function fmtDate(iso: string): string {
  const d = new Date(iso)
  return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, '0')}-${String(d.getDate()).padStart(2, '0')}`
}

function bestStreak(sorties: PilotSortie[]): number {
  let best = 0, cur = 0
  for (const s of sorties) {
    if (s.landed) { cur++; best = Math.max(best, cur) } else { cur = 0 }
  }
  return best
}

function curStreak(sorties: PilotSortie[]): number {
  let cur = 0
  for (let i = sorties.length - 1; i >= 0; i--) {
    if (sorties[i].landed) cur++; else break
  }
  return cur
}

function mostFlownAircraft(sorties: PilotSortie[]): string | null {
  const counts: Record<string, number> = {}
  for (const s of sorties) counts[s.aircraft] = (counts[s.aircraft] ?? 0) + 1
  let best: string | null = null, bestN = 0
  for (const [ac, n] of Object.entries(counts)) {
    if (n > bestN) { bestN = n; best = ac }
  }
  return best
}

// Aircraft image: served from /images/aircraft/{type}.jpg
// Falls back to a placeholder icon if not found
function AircraftImage({ type }: { type: string }) {
  const [err, setErr] = useState(false)
  const src = `/images/aircraft/${encodeURIComponent(type)}.jpg`
  if (err) {
    return (
      <div style={{ width: '100%', aspectRatio: '16 / 9', display: 'flex', alignItems: 'center', justifyContent: 'center', opacity: 0.15 }}>
        <Aircraft size={64} />
      </div>
    )
  }
  return (
    <img
      src={src}
      alt={type}
      onError={() => setErr(true)}
      style={{ width: '100%', aspectRatio: '16 / 9', objectFit: 'cover', display: 'block' }}
    />
  )
}

// ── Sub-components ────────────────────────────────────────────────────────────

type SortDir = 'asc' | 'desc'

function SortHeader<T extends string>({
  col, label, sort, setSort,
}: {
  col: T
  label: string
  sort: { col: T; dir: SortDir }
  setSort: (s: { col: T; dir: SortDir }) => void
}) {
  const active = sort.col === col
  return (
    <th
      onClick={() => setSort({ col, dir: active && sort.dir === 'desc' ? 'asc' : 'desc' })}
      style={{
        padding: '6px 10px', fontSize: '0.6rem', color: active ? 'var(--accent)' : 'var(--text-dim)',
        letterSpacing: '0.14em', textTransform: 'uppercase', textAlign: 'left',
        fontWeight: 600, cursor: 'pointer', userSelect: 'none', whiteSpace: 'nowrap',
        borderBottom: '1px solid var(--border)',
      }}
    >
      <span className="flex items-center gap-1">
        {label}
        {active
          ? sort.dir === 'desc'
            ? <ChevronDown size={9} />
            : <ChevronUp size={9} />
          : <ChevronDown size={9} style={{ opacity: 0.3 }} />}
      </span>
    </th>
  )
}

// ── Levant Ablaze: shared parts ───────────────────────────────────────────────
// The desert look draws the page from classes (levant/pilots.css) instead of
// the olive HUD's inline styles; each component below branches on the look
// and shares its data work with the plain path.

const sideTone = (s: Pilot['side'] | string): 'b' | 'r' | 'n' => (s === 'Blue' ? 'b' : s === 'Red' ? 'r' : 'n')
const sideLabel = (s: Pilot['side']) => (s === 'Blue' ? campaign.blueLabel : s === 'Red' ? campaign.redLabel : 'No side on record')
/** An outcome's words without the glyph the plain look leads with. */
const outcomeText = (o: SortieOutcome) => OUTCOME[o].label.replace(/^\S+\s/, '')

/** Campaign score, counted as the leaderboard counts it, for the rank. */
function campaignScore(p: Pilot): number {
  return (p.air_kills * 3) + (p.ground_kills * 2) + (p.captures * 5) +
    p.repairs + p.supply_transfers + (p.troops * 0.5) + (p.farps * 4) +
    (p.deploys * 2) + p.actions - (p.deaths * 2)
}

/** A log's column heads; the sortable ones are buttons. On a phone only the
 *  buttons stay, as a strip. */
function LogHead<T extends string>({ cols, sort, onSort, className }: {
  cols: { col?: T; label: string; area: string }[]
  sort: { col: T; dir: SortDir }
  onSort: (s: { col: T; dir: SortDir }) => void
  className: string
}) {
  return (
    <div className={`pl-head ${className}`}>
      {cols.map(c => {
        const col = c.col
        if (col === undefined) return <span key={c.label} className="ds-k" style={{ gridArea: c.area }}>{c.label}</span>
        const active = sort.col === col
        return (
          <button key={c.label} type="button" style={{ gridArea: c.area }} className={active ? 'on' : undefined}
            aria-pressed={active} title={`Sort by ${c.label.toLowerCase()}`}
            onClick={() => onSort({ col, dir: active && sort.dir === 'desc' ? 'asc' : 'desc' })}>
            {c.label}
            {active ? (sort.dir === 'desc' ? <ChevronDown size={10} /> : <ChevronUp size={10} />) : <ChevronDown size={10} className="off" />}
          </button>
        )
      })}
    </div>
  )
}

function Pager({ page, total, setPage }: { page: number; total: number; setPage: React.Dispatch<React.SetStateAction<number>> }) {
  if (total <= 1) return null
  return (
    <div className="pl-pager">
      <button type="button" aria-label="Previous page" onClick={() => setPage(p => Math.max(0, p - 1))} disabled={page === 0}>
        <ChevronLeft size={12} />
      </button>
      <span>{page + 1} / {total}</span>
      <button type="button" aria-label="Next page" onClick={() => setPage(p => Math.min(total - 1, p + 1))} disabled={page === total - 1}>
        <ChevronRight size={12} />
      </button>
    </div>
  )
}

// ── Flight log ────────────────────────────────────────────────────────────────

type SortieCol = 'date' | 'aircraft' | 'duration' | 'kills' | 'outcome'

// Sorts best-to-worst ending.
const OUTCOME_RANK = { landed: 3, no_landing: 2, lost: 1, killed: 0 } as const

function FlightLog({ ucid, sorties, breakdown, replays }: { ucid: string; sorties: PilotSortie[]; breakdown: TheaterBreakdown[]; replays: FlightRow[] }) {
  const look = useDashLook()
  const [sort, setSort] = useState<{ col: SortieCol; dir: SortDir }>({ col: 'date', dir: 'desc' })
  const [page, setPage] = useState(0)
  const [open, setOpen] = useState<number | null>(null)
  // The table scrolls sideways in a narrow card; an open sortie is pinned to
  // the visible width instead of the table's, so none of it scrolls away.
  // A callback ref, since the table mounts only once the sorties arrive.
  const [boxW, setBoxW] = useState<number | null>(null)
  const observer = useRef<ResizeObserver | null>(null)
  const scrollRef = useCallback((el: HTMLDivElement | null) => {
    observer.current?.disconnect()
    observer.current = null
    if (!el) return
    observer.current = new ResizeObserver(() => setBoxW(el.clientWidth))
    observer.current.observe(el)
  }, [])
  const PER_PAGE = 10

  const roundNum = useRoundNo()
  const ridToScenario = useMemo(() => {
    const m: Record<number, string> = {}
    for (const r of breakdown) m[r.round_id] = r.scenario
    return m
  }, [breakdown])

  const sorted = useMemo(() => {
    const arr = [...sorties]
    arr.sort((a, b) => {
      let v = 0
      if (sort.col === 'date')     v = a.takeoff.localeCompare(b.takeoff)
      if (sort.col === 'aircraft') v = a.aircraft.localeCompare(b.aircraft)
      if (sort.col === 'duration') v = a.duration_secs - b.duration_secs
      if (sort.col === 'outcome')  v = OUTCOME_RANK[outcomeOf(a)] - OUTCOME_RANK[outcomeOf(b)]
      if (sort.col === 'kills')    v = ((a.air_kills ?? 0) + (a.ground_kills ?? 0)) - ((b.air_kills ?? 0) + (b.ground_kills ?? 0))
      return sort.dir === 'desc' ? -v : v
    })
    return arr
  }, [sorties, sort])

  const totalPages = Math.ceil(sorted.length / PER_PAGE)
  const page_sorties = sorted.slice(page * PER_PAGE, (page + 1) * PER_PAGE)

  const cell: React.CSSProperties = { padding: '5px 10px', fontSize: '0.67rem', color: 'var(--text-muted)', borderBottom: '1px solid rgba(42,42,42,0.5)' }

  if (look.profile) {
    return (
      <Card tape={<>Flight log · {sorties.length}</>} tone="rust" right={<Pager page={page} total={totalPages} setPage={setPage} />}>
        {sorties.length === 0 ? <p className="lv-empty">No sorties recorded</p> : (
          <div className="pl-log pl-log-sorties">
            <LogHead className="pl-log-cols" sort={sort} onSort={s => { setSort(s); setPage(0) }} cols={[
              { col: 'date', label: 'Date', area: 'when' },
              { col: 'aircraft', label: 'Aircraft', area: 'ac' },
              { label: 'Theatre', area: 'thr' },
              { col: 'duration', label: 'Duration', area: 'dur' },
              { col: 'kills', label: 'Kills', area: 'k' },
              { col: 'outcome', label: 'Outcome', area: 'out' },
              { label: 'Replay', area: 'rp' },
            ]} />
            <ol>
              {page_sorties.map((s, i) => {
                const id = s.sortie_id
                const isOpen = id !== undefined && open === id
                const o = outcomeOf(s)
                const replay = replayFor(s, replays)
                const cells = (
                  <>
                    <span className="pl-when"><b>{fmtDate(s.takeoff)}</b><span>{fmtTime(s.takeoff)}–{s.land ? fmtTime(s.land) : '—'}</span></span>
                    <span className="pl-ac"><span className="pl-chip">{s.aircraft}</span></span>
                    <span className="pl-thr">{ridToScenario[s.round_id] ?? `Round ${roundNum(s.round_id)}`}</span>
                    <span className={`pl-dur${s.duration_secs > 0 ? '' : ' dim'}`}>{fmtDuration(s.duration_secs)}</span>
                    <span className="pl-k">
                      {s.air_kills === undefined ? <span className="dim">—</span> : (
                        <>
                          <b className={s.air_kills ? 'a' : undefined} title="Air-to-air">{s.air_kills}</b>
                          <span className="dim">/</span>
                          <b className={s.ground_kills ? 'g' : undefined} title="Air-to-ground">{s.ground_kills ?? 0}</b>
                        </>
                      )}
                    </span>
                    <span className="pl-out"><Tape className={`sm pl-otape pl-o-${o}`}>{outcomeText(o)}</Tape></span>
                  </>
                )
                return (
                  <li key={id ?? i} className={`pl-o-${o}${isOpen ? ' open' : ''}`}>
                    <div className="pl-log-line">
                      {id === undefined
                        ? <div className="pl-log-main">{cells}</div>
                        : (
                          <button type="button" className="pl-log-main" aria-expanded={isOpen}
                            title={isOpen ? 'Hide sortie details' : 'Show sortie details'}
                            onClick={() => setOpen(isOpen ? null : id)}>
                            {cells}
                          </button>
                        )}
                      {replay
                        ? <RouterLink className="pl-watch" to={`/replay/${encodeURIComponent(replay.rec)}?focus=${replay.i}`}><PlayCircle size={13} /> Watch</RouterLink>
                        : <span className="pl-watch none">—</span>}
                    </div>
                    {isOpen && <SortieDetail ucid={ucid} sortieId={id} replay={replay} />}
                  </li>
                )
              })}
            </ol>
          </div>
        )}
      </Card>
    )
  }

  return (
    <div className="vs-card">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Clock size={12} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
          Flight Log
        </span>
        <span className="ml-auto font-mono" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
          {sorties.length} sorties
        </span>
        {/* Pagination */}
        {totalPages > 1 && (
          <div className="flex items-center gap-1 ml-2">
            <button aria-label="Previous page" className="vs-touch-target" onClick={() => setPage(p => Math.max(0, p - 1))} disabled={page === 0}
              style={{ background: 'none', border: 'none', color: page === 0 ? '#333' : 'var(--text-dim)', cursor: page === 0 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronLeft size={12} />
            </button>
            <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace' }}>
              {page + 1} / {totalPages}
            </span>
            <button aria-label="Next page" className="vs-touch-target" onClick={() => setPage(p => Math.min(totalPages - 1, p + 1))} disabled={page === totalPages - 1}
              style={{ background: 'none', border: 'none', color: page === totalPages - 1 ? '#333' : 'var(--text-dim)', cursor: page === totalPages - 1 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronRight size={12} />
            </button>
          </div>
        )}
      </div>
      {sorties.length === 0 ? (
        <div style={{ padding: '1.5rem', textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.7rem' }}>No sorties recorded</div>
      ) : (
        <div ref={scrollRef} style={{ overflowX: 'auto', WebkitOverflowScrolling: 'touch' }}>
        <table style={{ width: '100%', minWidth: 620, borderCollapse: 'collapse' }}>
          <thead>
            <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
              <SortHeader col="date" label="Date" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="aircraft" label="Aircraft" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>Theater</th>
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>Takeoff</th>
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>End</th>
              <SortHeader col="duration" label="Duration" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="kills" label="Kills" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="outcome" label="Outcome" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>Replay</th>
            </tr>
          </thead>
          <tbody>
            {page_sorties.map((s, i) => {
              const id = s.sortie_id
              const isOpen = id !== undefined && open === id
              const out = OUTCOME[outcomeOf(s)]
              const replay = replayFor(s, replays)
              return (
              <React.Fragment key={id ?? i}>
              <tr
                onClick={id === undefined ? undefined : () => setOpen(isOpen ? null : id)}
                aria-expanded={id === undefined ? undefined : isOpen}
                title={id === undefined ? undefined : isOpen ? 'Hide sortie details' : 'Show sortie details'}
                style={{
                  background: isOpen ? 'rgba(96,165,250,0.08)' : i % 2 === 0 ? 'transparent' : 'rgba(0,0,0,0.12)',
                  cursor: id === undefined ? undefined : 'pointer',
                }}>
                <td style={{ ...cell, fontFamily: 'monospace', color: '#64748b' }}>{fmtDate(s.takeoff)}</td>
                <td style={{ ...cell, color: '#60a5fa' }}>{s.aircraft}</td>
                <td style={{ ...cell, color: '#94a3b8' }}>{ridToScenario[s.round_id] ?? `Round ${roundNum(s.round_id)}`}</td>
                <td style={{ ...cell, fontFamily: 'monospace' }}>{fmtTime(s.takeoff)}</td>
                <td style={{ ...cell, fontFamily: 'monospace' }}>{s.land ? fmtTime(s.land) : '—'}</td>
                <td style={{ ...cell, fontFamily: 'monospace', color: s.duration_secs > 0 ? 'var(--text)' : '#64748b' }}>{fmtDuration(s.duration_secs)}</td>
                <td style={{ ...cell, fontFamily: 'monospace', whiteSpace: 'nowrap' }}>
                  {s.air_kills === undefined ? '—' : (
                    <>
                      <span style={{ color: s.air_kills ? '#60a5fa' : '#475569' }} title="Air-to-air">{s.air_kills}</span>
                      <span style={{ color: '#475569' }}> / </span>
                      <span style={{ color: s.ground_kills ? '#f97316' : '#475569' }} title="Air-to-ground">{s.ground_kills ?? 0}</span>
                    </>
                  )}
                </td>
                <td style={cell}>
                  <span style={{ color: out.color, fontSize: '0.62rem', letterSpacing: '0.06em', whiteSpace: 'nowrap' }}>{out.label}</span>
                </td>
                <td style={cell}>
                  {(() => {
                    const r = replay
                    return r ? (
                      <RouterLink
                        onClick={e => e.stopPropagation()}
                        to={`/replay/${encodeURIComponent(r.rec)}?focus=${r.i}`}
                        style={{ display: 'inline-flex', alignItems: 'center', gap: 4, color: 'var(--accent-bright)', fontSize: '0.62rem', letterSpacing: '0.08em', textDecoration: 'none' }}
                      >
                        <PlayCircle size={12} /> WATCH
                      </RouterLink>
                    ) : <span style={{ color: 'var(--text-dim)' }}>—</span>
                  })()}
                </td>
              </tr>
              {isOpen && (
                <tr>
                  <td colSpan={9} style={{ padding: 0, borderBottom: '1px solid var(--border)' }}>
                    <div style={{ position: 'sticky', left: 0, width: boxW ?? undefined }}>
                      <SortieDetail ucid={ucid} sortieId={id} replay={replay} />
                    </div>
                  </td>
                </tr>
              )}
              </React.Fragment>
              )
            })}
          </tbody>
        </table>
        </div>
      )}
    </div>
  )
}

// ── Kill log ──────────────────────────────────────────────────────────────────

type KillCol = 'time' | 'target_type' | 'weapon' | 'victim'

function KillLog({ kills, allPilots, breakdown }: {
  kills: PilotKill[]
  allPilots: Pilot[]
  breakdown: TheaterBreakdown[]
}) {
  const look = useDashLook()
  const [sort, setSort] = useState<{ col: KillCol; dir: SortDir }>({ col: 'time', dir: 'desc' })
  const [page, setPage] = useState(0)
  const PER_PAGE = 15

  const roundNum = useRoundNo()
  const ridToScenario = useMemo(() => {
    const m: Record<number, string> = {}
    for (const r of breakdown) m[r.round_id] = r.scenario
    return m
  }, [breakdown])

  const ucidToName = useMemo(() => {
    const m: Record<string, string> = {}
    for (const p of allPilots) m[p.ucid] = p.name
    return m
  }, [allPilots])

  const sorted = useMemo(() => {
    const arr = [...kills]
    arr.sort((a, b) => {
      let v = 0
      if (sort.col === 'time')        v = a.time.localeCompare(b.time)
      if (sort.col === 'target_type') v = (a.target_type ?? '').localeCompare(b.target_type ?? '')
      if (sort.col === 'weapon')      v = (a.weapon ?? '').localeCompare(b.weapon ?? '')
      if (sort.col === 'victim')      v = (a.victim_ucid ?? '').localeCompare(b.victim_ucid ?? '')
      return sort.dir === 'desc' ? -v : v
    })
    return arr
  }, [kills, sort])

  const victimSideColor = (side: string) => side === 'Blue' ? '#3b82f6' : side === 'Red' ? '#ef4444' : '#64748b'

  const totalPages = Math.ceil(sorted.length / PER_PAGE)
  const page_kills = sorted.slice(page * PER_PAGE, (page + 1) * PER_PAGE)

  const cell: React.CSSProperties = { padding: '5px 10px', fontSize: '0.67rem', color: 'var(--text-muted)', borderBottom: '1px solid rgba(42,42,42,0.5)' }

  if (look.profile) {
    return (
      <Card tape={<>Kill log · {kills.length}</>} right={<Pager page={page} total={totalPages} setPage={setPage} />}>
        {kills.length === 0 ? <p className="lv-empty">No kills recorded</p> : (
          <div className="pl-log pl-log-kills">
            <LogHead className="pl-log-cols" sort={sort} onSort={s => { setSort(s); setPage(0) }} cols={[
              { col: 'time', label: 'When', area: 'when' },
              { col: 'target_type', label: 'Target', area: 'tgt' },
              { col: 'victim', label: 'Victim pilot', area: 'vic' },
              { label: 'Airframe', area: 'af' },
              { col: 'weapon', label: 'Weapon', area: 'wpn' },
              { label: 'Theatre', area: 'thr' },
            ]} />
            <ol>
              {page_kills.map((k, i) => {
                const victimName = k.victim_ucid ? (ucidToName[k.victim_ucid] ?? 'Unknown') : null
                return (
                  <li key={i} className={`pl-side-${sideTone(k.victim_side)}`}>
                    <div className="pl-log-main">
                      <span className="pl-when"><b>{fmtDate(k.time)}</b><span>{fmtTime(k.time)}</span></span>
                      <span className="pl-tgt">{k.target_type ?? '—'}</span>
                      <span className="pl-vic">{victimName ? <b>{victimName}</b> : <span className="dim">AI / env</span>}</span>
                      <span className="pl-af">{k.killer_airframe ? <span className="pl-chip">{k.killer_airframe}</span> : <span className="dim">—</span>}</span>
                      <span className="pl-wpn">{k.weapon ?? <span className="dim">—</span>}</span>
                      <span className="pl-thr">{ridToScenario[k.round_id] ?? `Round ${roundNum(k.round_id)}`}</span>
                    </div>
                  </li>
                )
              })}
            </ol>
          </div>
        )}
      </Card>
    )
  }

  return (
    <div className="vs-card">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <KillIcon size={12} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
          Kill Log
        </span>
        <span className="ml-auto font-mono" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
          {kills.length} kills
        </span>
        {totalPages > 1 && (
          <div className="flex items-center gap-1 ml-2">
            <button aria-label="Previous page" className="vs-touch-target" onClick={() => setPage(p => Math.max(0, p - 1))} disabled={page === 0}
              style={{ background: 'none', border: 'none', color: page === 0 ? '#333' : 'var(--text-dim)', cursor: page === 0 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronLeft size={12} />
            </button>
            <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace' }}>
              {page + 1} / {totalPages}
            </span>
            <button aria-label="Next page" className="vs-touch-target" onClick={() => setPage(p => Math.min(totalPages - 1, p + 1))} disabled={page === totalPages - 1}
              style={{ background: 'none', border: 'none', color: page === totalPages - 1 ? '#333' : 'var(--text-dim)', cursor: page === totalPages - 1 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronRight size={12} />
            </button>
          </div>
        )}
      </div>
      {kills.length === 0 ? (
        <div style={{ padding: '1.5rem', textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.7rem' }}>No kills recorded</div>
      ) : (
        <div style={{ overflowX: 'auto', WebkitOverflowScrolling: 'touch' }}>
        <table style={{ width: '100%', minWidth: 560, borderCollapse: 'collapse' }}>
          <thead>
            <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
              <SortHeader col="time" label="Zulu" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="target_type" label="Target Unit" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="victim" label="Victim Pilot" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>Airframe</th>
              <SortHeader col="weapon" label="Weapon" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <th style={{ padding: '6px 10px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase', fontWeight: 600, textAlign: 'left', borderBottom: '1px solid var(--border)' }}>Theater</th>
            </tr>
          </thead>
          <tbody>
            {page_kills.map((k, i) => {
              const victimName = k.victim_ucid ? (ucidToName[k.victim_ucid] ?? 'Unknown') : null
              return (
                <tr key={i} style={{ background: i % 2 === 0 ? 'transparent' : 'rgba(0,0,0,0.12)' }}>
                  <td style={{ ...cell, fontFamily: 'monospace', color: '#64748b', whiteSpace: 'nowrap' }}>
                    {fmtDate(k.time)} {fmtTime(k.time)}
                  </td>
                  <td style={{ ...cell }}>
                    <span style={{ color: victimSideColor(k.victim_side), fontSize: '0.6em' }}>●</span>
                    {' '}
                    <span style={{ color: 'var(--text-muted)' }}>{k.target_type ?? '—'}</span>
                  </td>
                  <td style={cell}>
                    {victimName ? (
                      <span style={{ color: '#f59e0b', fontWeight: 600 }}>{victimName}</span>
                    ) : (
                      <span style={{ color: '#475569' }}>AI / env</span>
                    )}
                  </td>
                  <td style={{ ...cell, color: '#60a5fa', fontFamily: 'monospace', whiteSpace: 'nowrap' }}>
                    {k.killer_airframe ? (
                      <span style={{ display: 'flex', alignItems: 'center', gap: 3 }}>
                        <Aircraft size={9} style={{ opacity: 0.6 }} />
                        {k.killer_airframe}
                      </span>
                    ) : <span style={{ color: '#374151' }}>—</span>}
                  </td>
                  <td style={{ ...cell, color: '#fbbf24', fontFamily: 'monospace' }}>{k.weapon ?? <span style={{ color: '#374151' }}>—</span>}</td>
                  <td style={{ ...cell, color: '#475569' }}>{ridToScenario[k.round_id] ?? `Round ${roundNum(k.round_id)}`}</td>
                </tr>
              )
            })}
          </tbody>
        </table>
        </div>
      )}
    </div>
  )
}

// ── Deploy log ────────────────────────────────────────────────────────────────

type DeployCol = 'time' | 'deployable' | 'aircraft' | 'method'

function DeployLog({ deploys, total }: { deploys: PilotDeploy[]; total: number }) {
  const look = useDashLook()
  const [sort, setSort] = useState<{ col: DeployCol; dir: SortDir }>({ col: 'time', dir: 'desc' })
  const [page, setPage] = useState(0)
  const PER_PAGE = 15

  const sorted = useMemo(() => {
    const arr = [...deploys]
    arr.sort((a, b) => {
      let v = 0
      if (sort.col === 'time')       v = a.time.localeCompare(b.time)
      if (sort.col === 'deployable') v = a.deployable.localeCompare(b.deployable)
      if (sort.col === 'aircraft')   v = (a.aircraft ?? '').localeCompare(b.aircraft ?? '')
      if (sort.col === 'method')     v = (a.method ?? '').localeCompare(b.method ?? '')
      return sort.dir === 'desc' ? -v : v
    })
    return arr
  }, [deploys, sort])

  const totalPages = Math.ceil(sorted.length / PER_PAGE)
  const page_deploys = sorted.slice(page * PER_PAGE, (page + 1) * PER_PAGE)

  const cell: React.CSSProperties = { padding: '5px 10px', fontSize: '0.67rem', color: 'var(--text-muted)', borderBottom: '1px solid rgba(42,42,42,0.5)' }

  const methodBadge = (method: string | null) => {
    if (method === 'AirDrop') return <span style={{ color: '#60a5fa', fontSize: '0.62rem', letterSpacing: '0.06em' }}>▼ Air Drop</span>
    if (method === 'ManualUnpack') return <span style={{ color: '#f59e0b', fontSize: '0.62rem', letterSpacing: '0.06em' }}>✋ Manual Unpack</span>
    return <span style={{ color: '#475569' }}>—</span>
  }

  if (look.profile) {
    const none = total > 0 ? `${total} deploy${total === 1 ? '' : 's'} recorded before per-deploy logging was added` : 'No deploys recorded'
    return (
      <Card tape={<>Deploy log · {deploys.length}</>} right={<Pager page={page} total={totalPages} setPage={setPage} />}>
        {total > deploys.length && deploys.length > 0 && (
          <p className="pl-note">+{total - deploys.length} more before per-deploy logging</p>
        )}
        {deploys.length === 0 ? <p className="lv-empty">{none}</p> : (
          <div className="pl-log pl-log-deploys">
            <LogHead className="pl-log-cols" sort={sort} onSort={s => { setSort(s); setPage(0) }} cols={[
              { col: 'time', label: 'When', area: 'when' },
              { col: 'deployable', label: 'What', area: 'what' },
              { col: 'aircraft', label: 'Aircraft', area: 'ac' },
              { col: 'method', label: 'Method', area: 'how' },
            ]} />
            <ol>
              {page_deploys.map((d, i) => (
                <li key={i}>
                  <div className="pl-log-main">
                    <span className="pl-when"><b>{fmtDate(d.time)}</b><span>{fmtTime(d.time)}</span></span>
                    <span className="pl-what">{d.deployable}</span>
                    <span className="pl-ac">{d.aircraft ? <span className="pl-chip">{d.aircraft}</span> : <span className="dim">—</span>}</span>
                    <span className="pl-how">
                      {d.method === 'AirDrop' ? <Tape className="sm">Air drop</Tape>
                        : d.method === 'ManualUnpack' ? <Tape tone="n" className="sm">Manual unpack</Tape>
                        : <span className="dim">—</span>}
                    </span>
                  </div>
                </li>
              ))}
            </ol>
          </div>
        )}
      </Card>
    )
  }

  return (
    <div className="vs-card">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Supply size={12} style={{ color: 'var(--accent)' }} />
        <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
          Deploy Log
        </span>
        <span className="ml-auto font-mono" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
          {deploys.length} deploys
          {total > deploys.length && ` (+${total - deploys.length} before logging)`}
        </span>
        {totalPages > 1 && (
          <div className="flex items-center gap-1 ml-2">
            <button aria-label="Previous page" className="vs-touch-target" onClick={() => setPage(p => Math.max(0, p - 1))} disabled={page === 0}
              style={{ background: 'none', border: 'none', color: page === 0 ? '#333' : 'var(--text-dim)', cursor: page === 0 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronLeft size={12} />
            </button>
            <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace' }}>
              {page + 1} / {totalPages}
            </span>
            <button aria-label="Next page" className="vs-touch-target" onClick={() => setPage(p => Math.min(totalPages - 1, p + 1))} disabled={page === totalPages - 1}
              style={{ background: 'none', border: 'none', color: page === totalPages - 1 ? '#333' : 'var(--text-dim)', cursor: page === totalPages - 1 ? 'default' : 'pointer', padding: 0 }}>
              <ChevronRight size={12} />
            </button>
          </div>
        )}
      </div>
      {deploys.length === 0 ? (
        <div style={{ padding: '1.5rem', textAlign: 'center', color: 'var(--text-dim)', fontSize: '0.7rem' }}>
          {total > 0 ? `${total} deploy${total === 1 ? '' : 's'} recorded before per-deploy logging was added` : 'No deploys recorded'}
        </div>
      ) : (
        <div style={{ overflowX: 'auto', WebkitOverflowScrolling: 'touch' }}>
        <table style={{ width: '100%', minWidth: 420, borderCollapse: 'collapse' }}>
          <thead>
            <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
              <SortHeader col="time" label="When" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="deployable" label="What" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="aircraft" label="Aircraft" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
              <SortHeader col="method" label="Method" sort={sort} setSort={s => { setSort(s); setPage(0) }} />
            </tr>
          </thead>
          <tbody>
            {page_deploys.map((d, i) => (
              <tr key={i} style={{ background: i % 2 === 0 ? 'transparent' : 'rgba(0,0,0,0.12)' }}>
                <td style={{ ...cell, fontFamily: 'monospace', color: '#64748b', whiteSpace: 'nowrap' }}>
                  {fmtDate(d.time)} {fmtTime(d.time)}
                </td>
                <td style={{ ...cell, color: 'var(--text)' }}>{d.deployable}</td>
                <td style={{ ...cell, color: '#60a5fa', fontFamily: 'monospace' }}>
                  {d.aircraft ?? <span style={{ color: '#374151' }}>—</span>}
                </td>
                <td style={cell}>{methodBadge(d.method)}</td>
              </tr>
            ))}
          </tbody>
        </table>
        </div>
      )}
    </div>
  )
}

// ── Progression ───────────────────────────────────────────────────────────────

function ChallengeRow({ c }: { c: Challenge }) {
  const pct = Math.min(100, (c.progress / Math.max(1, c.goal)) * 100)
  return (
    <div style={{ display: 'grid', gap: 3 }}>
      <div className="flex items-center gap-2" style={{ fontSize: '0.68rem' }}>
        <span style={{ color: c.done ? 'var(--accent)' : 'var(--text)' }}>{c.done ? '✓ ' : ''}{c.label}</span>
        <span className="ml-auto font-mono" style={{ color: 'var(--text-dim)' }}>{c.progress}/{c.goal} · +{c.xp} XP</span>
      </div>
      <div style={{ height: 4, background: 'var(--border)', borderRadius: 2 }}>
        <div style={{ width: `${pct}%`, height: '100%', background: 'var(--accent)', borderRadius: 2 }} />
      </div>
    </div>
  )
}

/** The pilot card, drawn in the page's own theme. bfdb renders the same card
 *  as a PNG for sharing (Discord `/card`); the page links to that rather than
 *  showing it, so the card here follows the theme and never shows a stale
 *  image from an older server. */
function PilotCardView({ pilot, p }: { pilot: Pilot; p: PilotProgression }) {
  const logistics = pilot.repairs + pilot.supply_transfers + pilot.troops + pilot.farps + pilot.deploys
  const badges = p.achievements.filter(a => a.tier > 0).length
  const stats: [string, string | number][] = [
    ['Air kills', pilot.air_kills],
    ['Ground kills', pilot.ground_kills],
    ['Captures', pilot.captures],
    ['Logistics', logistics],
    ['K/D', kd(pilot.air_kills, pilot.ground_kills, pilot.deaths)],
    ['Flight time', fmtHours(pilot.hours)],
  ]
  return (
    <div style={{
      position: 'relative', background: 'var(--bg-card-solid)', border: '1px solid var(--border-light)',
      borderLeft: '3px solid var(--accent)', borderRadius: 2, padding: '14px 16px 12px',
      boxShadow: 'inset 0 2px 0 var(--accent-border)',
    }}>
      <div className="flex items-start gap-3">
        <div style={{ minWidth: 0 }}>
          <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.7rem', letterSpacing: '0.06em', lineHeight: 1.05, color: 'var(--text)', overflowWrap: 'anywhere' }}>
            {p.squadron && <span style={{ color: 'var(--accent-bright)' }}>{shownTag(p.squadron)} </span>}
            {pilot.name}
          </div>
          <div className="font-mono" style={{ fontSize: '0.62rem', letterSpacing: '0.12em', color: 'var(--accent-bright)', marginTop: 4 }}>
            LEVEL {p.level} · {p.xp.toLocaleString()} XP · {badges} ACHIEVEMENT{badges === 1 ? '' : 'S'}
          </div>
        </div>
        <span className="ml-auto" style={{ fontSize: '0.55rem', letterSpacing: '0.24em', color: 'var(--text-dim)', whiteSpace: 'nowrap' }}>VECTOR STRIKE</span>
      </div>
      <div style={{ height: 1, background: 'var(--border-light)', margin: '12px 0 10px' }} />
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(84px, 1fr))', gap: '10px 12px' }}>
        {stats.map(([name, value]) => (
          <div key={name}>
            <div style={{ fontSize: '1.05rem', fontWeight: 700, color: 'var(--text)', whiteSpace: 'nowrap' }}>{value}</div>
            <div style={{ fontSize: '0.55rem', letterSpacing: '0.12em', textTransform: 'uppercase', color: 'var(--text-dim)', marginTop: 1 }}>{name}</div>
          </div>
        ))}
      </div>
    </div>
  )
}

/** A challenge in the desert look: its name, its count, a thin meter. */
function ChallengeLine({ c }: { c: Challenge }) {
  const pct = Math.min(100, (c.progress / Math.max(1, c.goal)) * 100)
  return (
    <li className={c.done ? 'done' : undefined}>
      <div className="pl-chal-top">
        <span>{c.done && <Check size={11} />}{c.label}</span>
        <b>{c.progress}/{c.goal}</b>
        <em>+{c.xp} XP</em>
      </div>
      <div className="pl-bar thin"><i style={{ width: `${pct}%` }} /></div>
    </li>
  )
}

function ProgressionDesert({ p, ucid, pct }: { p: PilotProgression; ucid: string; pct: number }) {
  const got = p.achievements.filter(a => a.tier > 0).length
  return (
    <Card tape="Progression" right={
      <a className="lv-more pl-share" href={api.pilotCardUrl(ucid)} target="_blank" rel="noreferrer"
        title="Open the card image to share (Discord /card posts the same one)">
        Share card <ExternalLink size={10} />
      </a>
    }>
      <div className="pl-prog">
        <div className="pl-level">
          <div className="pl-level-n"><span className="ds-k">Level</span><b>{p.level}</b></div>
          <div className="pl-level-xp">
            <div className="pl-level-top">
              <span className="pl-xp">{p.xp.toLocaleString()}<small>XP</small></span>
              <span className="lv-sub">{(p.next_level_xp - p.xp).toLocaleString()} XP to level {p.level + 1}</span>
            </div>
            <div className="pl-bar" role="img" aria-label={`${Math.round(pct)}% of the way to level ${p.level + 1}`}>
              <i style={{ width: `${pct}%` }} />
            </div>
          </div>
        </div>
        <div className="pl-chal">
          {([['Today', p.daily], ['This week', p.weekly]] as const).map(([name, list]) => (
            <div key={name}>
              <span className="ds-k">{name}</span>
              <ul>{list.map(c => <ChallengeLine key={c.label} c={c} />)}</ul>
            </div>
          ))}
        </div>
        {p.achievements.length > 0 && (
          <div>
            <span className="ds-k">Achievements · {got} of {p.achievements.length}</span>
            <ul className="pl-ach">
              {p.achievements.map(a => (
                <li key={a.key} className={a.tier > 0 ? 'got' : undefined}>
                  <strong>{a.label}</strong>
                  <span className="pl-pips" role="img" aria-label={`Tier ${a.tier} of ${a.max_tier}`}>
                    {Array.from({ length: Math.min(a.max_tier, 10) }, (_, i) => <i key={i} className={i < a.tier ? 'on' : undefined} />)}
                  </span>
                  <span className="lv-sub">Tier {a.tier}/{a.max_tier} · {a.value}{a.next != null ? ` (next ${a.next})` : ''}</span>
                </li>
              ))}
            </ul>
          </div>
        )}
      </div>
    </Card>
  )
}

function ProgressionCard({ p, dim, ucid, pilot }: { p: PilotProgression; dim: React.CSSProperties; ucid: string; pilot: Pilot }) {
  const look = useDashLook()
  const span = Math.max(1, p.next_level_xp - p.level_xp)
  const pct = Math.min(100, ((p.xp - p.level_xp) / span) * 100)
  // The pilot card itself is the profile's hero in the desert look.
  if (look.profile) return <ProgressionDesert p={p} ucid={ucid} pct={pct} />
  return (
    <div className="vs-card p-0">
      <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
        <Shield size={12} style={{ color: 'var(--accent)' }} />
        <span style={dim}>Progression</span>
        <span className="ml-auto font-mono" style={{ fontSize: '0.7rem', color: 'var(--text)' }}>
          {p.squadron ? `${shownTag(p.squadron)} · ` : ''}LEVEL {p.level} · {p.xp.toLocaleString()} XP
        </span>
        <a href={api.pilotCardUrl(ucid)} target="_blank" rel="noreferrer" title="Open the card image to share (Discord /card posts the same one)"
          style={{ fontSize: '0.58rem', letterSpacing: '0.1em', color: 'var(--accent-bright)', textDecoration: 'none', whiteSpace: 'nowrap' }}>
          SHARE CARD ↗
        </a>
      </div>
      <div className="p-3 space-y-3">
        <PilotCardView pilot={pilot} p={p} />
        <div>
          <div style={{ height: 6, background: 'var(--border)', borderRadius: 3 }}>
            <div style={{ width: `${pct}%`, height: '100%', background: 'var(--accent)', borderRadius: 3 }} />
          </div>
          <div className="font-mono" style={{ fontSize: '0.6rem', color: 'var(--text-dim)', marginTop: 3 }}>
            {(p.next_level_xp - p.xp).toLocaleString()} XP to level {p.level + 1}
          </div>
        </div>
        <div className="grid gap-3" style={{ gridTemplateColumns: 'repeat(auto-fit, minmax(220px, 1fr))' }}>
          <div className="space-y-2">
            <span style={dim}>Today</span>
            {p.daily.map(c => <ChallengeRow key={c.label} c={c} />)}
          </div>
          <div className="space-y-2">
            <span style={dim}>This week</span>
            {p.weekly.map(c => <ChallengeRow key={c.label} c={c} />)}
          </div>
        </div>
        <div className="grid gap-2" style={{ gridTemplateColumns: 'repeat(auto-fit, minmax(180px, 1fr))' }}>
          {p.achievements.map(a => (
            <div key={a.key} className="vs-card" style={{ padding: '6px 8px', fontSize: '0.66rem', opacity: a.tier > 0 ? 1 : 0.55 }}>
              <div style={{ color: 'var(--text)' }}>{a.label}</div>
              <div className="font-mono" style={{ color: 'var(--text-dim)' }}>
                Tier {a.tier}/{a.max_tier} · {a.value}{a.next != null ? ` (next ${a.next})` : ''}
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  )
}

// ── Theater breakdown ─────────────────────────────────────────────────────────

/** K/D in three bands: good (no deaths, or 5+), fair (2+), poor. */
function kdBand(kdVal: string, deaths: number): 'good' | 'fair' | 'poor' {
  return deaths === 0 || parseFloat(kdVal) >= 5 ? 'good' : parseFloat(kdVal) >= 2 ? 'fair' : 'poor'
}

function TheaterCard({ td }: { td: TheaterBreakdown }) {
  const look = useDashLook()
  const roundNum = useRoundNo()
  const kd_val = kd(td.air_kills, td.ground_kills, td.deaths)
  const kd_color = td.deaths === 0 ? '#22c55e' : parseFloat(kd_val) >= 5 ? '#22c55e' : parseFloat(kd_val) >= 2 ? '#f59e0b' : '#ef4444'
  const ratio = (k: number) => (td.deaths > 0 ? (k / td.deaths).toFixed(2) : k > 0 ? '∞' : '0.00')
  if (look.profile) {
    const rows: [string, string | number, string?][] = [
      ['A/A kills', td.air_kills, 'a'],
      ['A/G kills', td.ground_kills, 'g'],
      ['AA K/D', ratio(td.air_kills)],
      ['AG K/D', ratio(td.ground_kills)],
      ['Deaths', td.deaths, td.deaths > 0 ? 'bad' : 'dim'],
      ['Captures', td.captures, td.captures > 0 ? 'warn' : undefined],
      ['Hours', fmtHours(td.hours)],
    ]
    return (
      <article className="pl-thr-card">
        <header>
          <strong>{td.scenario || `Round ${roundNum(td.round_id)}`}</strong>
          <span className="ds-k">Round #{roundNum(td.round_id)}</span>
        </header>
        <div className="pl-thr-big">
          <div><span className="ds-k">Kills</span><b>{td.air_kills + td.ground_kills}</b></div>
          <div className={kdBand(kd_val, td.deaths)}><span className="ds-k">K/D</span><b>{kd_val}</b></div>
        </div>
        <dl className="pl-rows sm">
          {rows.map(([k, v, c]) => <div key={k}><dt>{k}</dt><dd className={c}>{v}</dd></div>)}
        </dl>
      </article>
    )
  }
  const dim: React.CSSProperties = { fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.1em', textTransform: 'uppercase' }
  const statRow = (label: string, value: string | number, highlight?: string) => (
    <div className="flex justify-between items-center" style={{ padding: '3px 0', borderBottom: '1px solid rgba(42,42,42,0.4)' }}>
      <span style={dim}>{label}</span>
      <span style={{ fontSize: '0.7rem', color: highlight ?? 'var(--text-muted)', fontFamily: 'monospace', fontWeight: 600 }}>{value}</span>
    </div>
  )

  return (
    <div className="vs-card p-0" style={{ overflow: 'hidden' }}>
      {/* Header */}
      <div className="px-3 py-2.5" style={{ background: 'rgba(0,0,0,0.25)', borderBottom: '1px solid var(--border)' }}>
        <div style={{ fontSize: '0.78rem', color: 'var(--text)', fontWeight: 700, letterSpacing: '0.04em' }}>
          {td.scenario || `Round ${roundNum(td.round_id)}`}
        </div>
        <div style={{ fontSize: '0.58rem', color: 'var(--text-dim)', marginTop: '0.1rem' }}>
          Round #{roundNum(td.round_id)}
        </div>
      </div>
      <div className="px-3 py-2">
        {statRow('A/A Kills', td.air_kills, '#3b82f6')}
        {statRow('A/G Kills', td.ground_kills, '#f97316')}
        {statRow('Total Kills', td.air_kills + td.ground_kills, 'var(--text)')}
        {statRow('AA K/D', ratio(td.air_kills))}
        {statRow('AG K/D', ratio(td.ground_kills))}
        {statRow('K/D Ratio', kd_val, kd_color)}
        {statRow('Deaths', td.deaths, td.deaths > 0 ? '#ef4444' : 'var(--text-dim)')}
        {statRow('Captures', td.captures, td.captures > 0 ? '#eab308' : undefined)}
        {statRow('Hours', fmtHours(td.hours))}
      </div>
    </div>
  )
}

// ── Levant Ablaze: the profile hero ───────────────────────────────────────────

/** The airframe the pilot flies most, framed in the hero; gone if no photo. */
function HeroAirframe({ type }: { type: string }) {
  const [err, setErr] = useState(false)
  return (
    <figure className={`pl-hero-ac${err ? ' none' : ''}`}>
      {err
        ? <Aircraft size={54} aria-hidden="true" />
        : <img src={`/images/aircraft/${encodeURIComponent(type)}.jpg`} alt={type} onError={() => setErr(true)} />}
      <figcaption><span className="ds-k">Most flown</span><b>{type}</b></figcaption>
    </figure>
  )
}

/** The pilot card: name, side, rank and the headline numbers, drawn as the
 *  SITREP's scoreboard. bfdb renders the same card as a PNG for sharing (the
 *  Progression card links to it). */
function PilotHero({ pilot, standing, isMe, prog, mostFlown, sorties, onClear }: {
  pilot: Pilot
  standing: number | null
  isMe: boolean
  prog?: PilotProgression
  mostFlown: string | null
  sorties: number
  onClear: () => void
}) {
  const score = campaignScore(pilot)
  const rank = rankFor(score, pilot.side)
  const next = nextRankAt(score)
  const RankIcon = rank.icon
  const t = sideTone(pilot.side)
  const logistics = pilot.repairs + pilot.supply_transfers + pilot.troops + pilot.farps + pilot.deploys
  const badges = prog ? prog.achievements.filter(a => a.tier > 0).length : 0
  const board: [string, string | number, string?][] = [
    ['Air kills', pilot.air_kills, 'a'],
    ['Ground kills', pilot.ground_kills, 'g'],
    ['K/D', kd(pilot.air_kills, pilot.ground_kills, pilot.deaths)],
    ['Flight time', fmtHours(pilot.hours)],
    ['Sorties', sorties],
    ['Captures', pilot.captures],
    ['Logistics', logistics],
    ['Deaths', pilot.deaths, 'dim'],
  ]
  return (
    <section className={`pl-hero ${t}`} aria-label={`${pilot.name}: pilot record`}>
      <div className={`pl-hero-top${mostFlown ? ' art' : ''}`}>
        <div className="pl-hero-id">
          <div className="pl-hero-tags">
            <Tape tone={t}>{sideLabel(pilot.side)}</Tape>
            {standing !== null && standing > 0 && (
              <Tape tone={standing <= 3 ? 'rust' : 'n'} className="sm">No. {standing} on the board</Tape>
            )}
            {isMe && <Tape tone="warn" className="sm">You</Tape>}
          </div>
          <h2 className="pl-hero-name" style={{ '--chars': Math.max(6, pilot.name.length) } as React.CSSProperties}>
            {prog?.squadron && <span>{shownTag(prog.squadron)}</span>}
            {pilot.name}
          </h2>
          <div className="pl-hero-rank">
            <RankIcon size={38} strokeWidth={1.6} />
            <div>
              <b>{rank.title}</b>
              <span>
                {next !== null ? `${next - Math.round(score)} score to the next rank` : 'Top of the ladder'}
                {' · '}score {Math.round(score).toLocaleString()}
              </span>
            </div>
          </div>
          {prog && (
            <div className="pl-hero-lvl">
              Level <b>{prog.level}</b> · <b>{prog.xp.toLocaleString()}</b> XP · <b>{badges}</b> achievement{badges === 1 ? '' : 's'}
            </div>
          )}
        </div>
        {mostFlown && <HeroAirframe key={mostFlown} type={mostFlown} />}
        <button type="button" className="pl-clear" onClick={onClear}><RotateCcw size={11} /> Clear</button>
      </div>
      <dl className="pl-board">
        {board.map(([k, v, c]) => (
          <div key={k} className={c}><dt>{k}</dt><dd>{v}</dd></div>
        ))}
      </dl>
    </section>
  )
}

// ── Main component ────────────────────────────────────────────────────────────

export default function Pilots() {
  const look = useDashLook()
  const { user } = useAuth()
  const queryClient = useQueryClient()
  const [searchParams] = useSearchParams()
  const { data: pilots = [], isLoading, isError, error, refetch } = useQuery({
    queryKey: ['leaderboard'],
    queryFn: api.leaderboard,
    refetchInterval: 60_000,
  })
  // What the user picked or typed; null until they do. Until then the page
  // shows the pilot from ?ucid=, else the signed-in user's own record.
  const [picked, setSelected] = useState<string | null>(null)
  const [typed, setSearch] = useState<string | null>(null)
  // Set once the user empties the box, so the default does not come back.
  const [cleared, setCleared] = useState(false)
  const [searchFocus, setSearchFocus] = useState(false)

  const paramUcid = searchParams.get('ucid')
  const auto = useMemo(() => {
    if (pilots.length === 0) return null
    const ucid = paramUcid ?? user?.ucid
    return ucid ? { ucid, name: pilots.find(p => p.ucid === ucid)?.name ?? '' } : null
  }, [pilots, paramUcid, user?.ucid])
  const followAuto = picked === null && !cleared
  const selected = followAuto ? auto?.ucid ?? null : picked
  const search = typed ?? (followAuto ? auto?.name ?? '' : '')
  const clearSelection = () => { setSelected(null); setCleared(true) }

  const pilot = pilots.find(p => p.ucid === selected) ?? null
  const pilotRank = pilot ? pilots.findIndex(p => p.ucid === pilot.ucid) + 1 : null

  // Search results shown as dropdown
  const showDropdown = searchFocus && search.length > 0
  const filtered = pilots.filter(p => p.name.toLowerCase().includes(search.toLowerCase())).slice(0, 12)

  // Pilot detail queries (only when pilot selected)
  const { data: sorties = [] } = useQuery({
    queryKey: ['pilot-sorties', selected],
    queryFn: () => api.pilotSorties(selected!),
    enabled: !!selected,
  })
  const { data: breakdown = [] } = useQuery({
    queryKey: ['pilot-breakdown', selected],
    queryFn: () => api.pilotBreakdown(selected!),
    enabled: !!selected,
  })
  const { data: progression } = useQuery({
    queryKey: ['pilot-progression', selected],
    queryFn: () => api.pilotProgression(selected!),
    enabled: !!selected,
    // An older bfdb has no progression route.
    retry: false,
  })
  const { data: kills = [] } = useQuery({
    queryKey: ['pilot-kills', selected],
    queryFn: () => api.pilotKills(selected!),
    enabled: !!selected,
  })
  const { data: replays = [] } = useQuery({
    queryKey: ['pilot-replays', selected],
    queryFn: () => api.pilotReplays(selected!),
    enabled: !!selected,
    // An older bfdb has no replay routes; the Flight Log just shows no links.
    retry: false,
  })
  const { data: deploys = [] } = useQuery({
    queryKey: ['pilot-deploys', selected],
    queryFn: () => api.pilotDeploys(selected!),
    enabled: !!selected,
  })

  // Derived stats
  const totalLandings = sorties.filter(s => s.landed).length
  const totalSorties  = sorties.length
  const bestStreakVal  = bestStreak(sorties)
  const curStreakVal   = curStreak(sorties)
  const mostFlown     = mostFlownAircraft(sorties)
  const bestAA        = breakdown.length > 0 ? Math.max(...breakdown.map(t => t.air_kills)) : 0
  const bestAG        = breakdown.length > 0 ? Math.max(...breakdown.map(t => t.ground_kills)) : 0

  const dim: React.CSSProperties = { fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }

  // The career record and contributions, for both looks; `cls` is the desert
  // look's colour for the value, `color` the plain look's.
  const career = pilot ? [
    { label: 'A/A Victories', value: pilot.air_kills, color: '#60a5fa', cls: 'a' },
    { label: 'A/G Victories', value: pilot.ground_kills, color: '#f97316', cls: 'g' },
    { label: 'V/L Ratio', value: kd(pilot.air_kills, pilot.ground_kills, pilot.deaths), color: '#22c55e', cls: 'hi' },
    { label: 'S/L Ratio', value: sl(totalSorties, totalLandings), color: '#a78bfa', cls: undefined },
    { label: 'Flight Time', value: fmtHours(pilot.hours), color: 'var(--text)', cls: undefined },
    { label: 'Current Streak', value: curStreakVal, color: '#22c55e', cls: 'ok' },
    { label: 'Best Streak', value: bestStreakVal, color: '#f59e0b', cls: 'warn' },
    { label: 'Best A/A (round)', value: bestAA, color: '#60a5fa', cls: 'a' },
    { label: 'Best A/G (round)', value: bestAG, color: '#f97316', cls: 'g' },
  ] : []
  const contributions = pilot ? [
    { label: 'Captures',     value: pilot.captures },
    { label: 'Repairs',      value: pilot.repairs },
    { label: 'Supply Runs',  value: pilot.supply_transfers },
    { label: 'Troops',       value: pilot.troops },
    { label: 'FARPs',        value: pilot.farps },
    { label: 'Deploys',      value: pilot.deploys },
    { label: 'Actions',      value: pilot.actions },
    { label: 'Donated Pts',  value: pilot.donated_points },
  ] : []
  const onSearch = (v: string) => { setSearch(v); if (!v) clearSelection() }
  const pick = (p: Pilot) => { setSelected(p.ucid); setSearch(p.name) }

  if (look.profile) {
    return (
      <div className="flex flex-col flex-1 overflow-hidden">
        <PageHeader art="hornet-top" icon={PilotIcon} title="PILOTS" sub={`${pilots.length} registered pilots`} />

        <div className="pl-page">
          {/* ── Search ── */}
          <Card tape="Pilot search" className="pl-search" right={<span className="pl-count">{pilots.length} on record</span>}>
            <div className="pl-search-box">
              <Search size={14} aria-hidden="true" />
              <input
                type="text"
                placeholder="Search pilots by name…"
                aria-label="Search pilots by name"
                value={search}
                onChange={e => onSearch(e.target.value)}
                onFocus={() => setSearchFocus(true)}
                onBlur={() => setTimeout(() => setSearchFocus(false), 150)}
                className="vs-input"
              />
              {showDropdown && filtered.length > 0 && (
                <div className="pl-drop">
                  {filtered.map(p => (
                    <button key={p.ucid} type="button" className={`pl-drop-row pl-side-${sideTone(p.side)}`}
                      onMouseDown={() => pick(p)} onClick={() => pick(p)}>
                      <span className="rk">{pilots.indexOf(p) + 1}</span>
                      <span className="nm">
                        <b>{p.name}</b>
                        {user?.ucid === p.ucid && <Tape tone="warn" className="sm">You</Tape>}
                      </span>
                      <span className="st">
                        <b>{p.air_kills + p.ground_kills}</b>k · <b>{p.hours.toFixed(1)}</b>h · <b>{p.deaths}</b>d
                      </span>
                    </button>
                  ))}
                </div>
              )}
            </div>
          </Card>

          {/* ── Link account banner ── */}
          {user && !user.ucid && (
            <Card tape="Not linked yet" tone="rust" className="pl-link">
              <div className="pl-link-body">
                <Link size={16} aria-hidden="true" />
                <p>
                  Run <code>/linkme</code> in Discord for a token, then <code>-linkme &lt;token&gt;</code> in DCS chat to see your profile highlighted.
                </p>
                <button type="button" className="ds-btn ghost pl-btn-sm" onClick={() => queryClient.invalidateQueries({ queryKey: ['auth', 'me'] })}>
                  Check again
                </button>
              </div>
            </Card>
          )}

          {isError && (
            <QueryState what="the pilot roster" isLoading={false} isError error={error} onRetry={refetch} />
          )}

          {!pilot && !isLoading && !isError && (
            <div className="pl-blank">
              <PilotIcon size={44} aria-hidden="true" />
              <Tape tone="n">No pilot selected</Tape>
              <p>Search for a pilot above</p>
            </div>
          )}

          {pilot && (
            <div className="pl-profile">
              <PilotHero pilot={pilot} standing={pilotRank} isMe={user?.ucid === pilot.ucid} prog={progression}
                mostFlown={mostFlown} sorties={totalSorties} onClear={() => { clearSelection(); setSearch('') }} />

              <div className="pl-col pl-aside">
                <Card tape="Career record">
                  <dl className="pl-rows">
                    {career.map(s => <div key={s.label}><dt>{s.label}</dt><dd className={s.cls}>{s.value}</dd></div>)}
                  </dl>
                </Card>
                <Card tape="Contributions">
                  <dl className="pl-contrib">
                    {contributions.map(s => <div key={s.label}><dt>{s.label}</dt><dd className={s.value ? undefined : 'dim'}>{s.value}</dd></div>)}
                  </dl>
                </Card>
              </div>

              <div className="pl-col pl-main">
                {progression && selected && <ProgressionCard p={progression} dim={dim} ucid={selected} pilot={pilot} />}
                <PilotCharts key={pilot.ucid} sorties={sorties} />
                <RivalsCard ucid={pilot.ucid} sorties={sorties} kills={kills} pilots={pilots} />
                <CompareCard key={`cmp-${pilot.ucid}`} pilot={pilot} pilots={pilots} />
                {breakdown.length > 0 && (
                  <Card tape={<>Theatre breakdown · {breakdown.length}</>}>
                    <div className="pl-thr-grid">
                      {breakdown.map(td => <TheaterCard key={td.round_id} td={td} />)}
                    </div>
                  </Card>
                )}
                {/* Keyed by pilot: a different pilot starts on page one, nothing open. */}
                <FlightLog key={pilot.ucid} ucid={pilot.ucid} sorties={sorties} breakdown={breakdown} replays={replays} />
                <KillLog kills={kills} allPilots={pilots} breakdown={breakdown} />
                <DeployLog deploys={deploys} total={pilot.deploys} />
              </div>
            </div>
          )}
        </div>
      </div>
    )
  }

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader art="hornet-top" icon={PilotIcon} title="PILOTS" sub={`${pilots.length} registered pilots`} />

      <div className="flex-1 overflow-auto p-4 space-y-4">
        {/* ── Search ── */}
        <div className="vs-card p-0" style={{ overflow: 'visible', position: 'relative', zIndex: 10 }}>
          <div className="flex items-center gap-2 px-3 py-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
            <Search size={11} style={{ color: 'var(--accent)', flexShrink: 0 }} />
            <span style={dim}>PILOT SEARCH</span>
          </div>
          <div style={{ position: 'relative', padding: '0.75rem' }}>
            <input
              type="text"
              placeholder="Search pilots by name…"
              value={search}
              onChange={e => onSearch(e.target.value)}
              onFocus={() => setSearchFocus(true)}
              onBlur={() => setTimeout(() => setSearchFocus(false), 150)}
              className="vs-input w-full"
              style={{ fontSize: '0.8rem', padding: '0.5rem 0.75rem' }}
            />
            {/* Search dropdown */}
            {showDropdown && filtered.length > 0 && (
              <div style={{
                position: 'absolute', top: '100%', left: '0.75rem', right: '0.75rem',
                background: 'var(--bg-elevated)', border: '1px solid var(--border)', borderRadius: '3px',
                boxShadow: '0 8px 24px rgba(0,0,0,0.5)', zIndex: 50, maxHeight: 300, overflowY: 'auto',
              }}>
                {filtered.map((p, i) => {
                  const isMe = user?.ucid === p.ucid
                  return (
                    <button
                      key={p.ucid}
                      onMouseDown={() => pick(p)}
                      style={{
                        width: '100%', textAlign: 'left', background: 'none', border: 'none',
                        borderBottom: i < filtered.length - 1 ? '1px solid var(--border)' : 'none',
                        padding: '0.5rem 0.75rem', cursor: 'pointer',
                        display: 'flex', alignItems: 'center', gap: '0.75rem',
                      }}
                      onMouseEnter={e => (e.currentTarget.style.background = 'rgba(77,124,15,0.07)')}
                      onMouseLeave={e => (e.currentTarget.style.background = 'none')}
                    >
                      <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', fontFamily: 'monospace', width: 20, textAlign: 'right' }}>
                        {pilots.indexOf(p) + 1}
                      </span>
                      <div style={{ flex: 1, minWidth: 0 }}>
                        <div style={{ fontSize: '0.75rem', color: 'var(--text)', fontWeight: 600 }}>
                          {p.name}
                          {isMe && <span style={{ fontSize: '0.5rem', color: 'var(--accent)', border: '1px solid var(--accent)', padding: '0 3px', borderRadius: 2, marginLeft: 6 }}>YOU</span>}
                        </div>
                        <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace', marginTop: 1 }}>
                          {p.air_kills + p.ground_kills}k · {p.hours.toFixed(1)}h · {p.deaths}d
                        </div>
                      </div>
                    </button>
                  )
                })}
              </div>
            )}
          </div>
        </div>

        {/* ── Link account banner ── */}
        {user && !user.ucid && (
          <div className="vs-card px-4 py-3 flex items-center gap-3" style={{ borderColor: '#5865F2', background: 'rgba(88,101,242,0.05)' }}>
            <Link size={14} style={{ color: '#5865F2', flexShrink: 0 }} />
            <div style={{ flex: 1, fontSize: '0.7rem', color: 'var(--text-muted)' }}>
              Not linked yet — run <code style={{ color: 'var(--text)' }}>/linkme</code> in Discord for a token, then <code style={{ color: 'var(--text)' }}>-linkme &lt;token&gt;</code> in DCS chat to see your profile highlighted.
            </div>
            <button
              onClick={() => queryClient.invalidateQueries({ queryKey: ['auth', 'me'] })}
              style={{ fontSize: '0.65rem', color: '#5865F2', background: 'none', border: '1px solid #5865F2', padding: '2px 10px', borderRadius: 2, cursor: 'pointer', letterSpacing: '0.08em', fontFamily: 'var(--font-display)' }}
            >
              CHECK AGAIN
            </button>
          </div>
        )}

        {isError && (
          <QueryState what="the pilot roster" isLoading={false} isError error={error} onRetry={refetch} />
        )}

        {!pilot && !isLoading && !isError && (
          <div className="flex flex-col items-center justify-center py-20 gap-3" style={{ color: 'var(--text-dim)' }}>
            <PilotIcon size={40} style={{ opacity: 0.15 }} />
            <div style={{ fontSize: '0.72rem', letterSpacing: '0.16em', textTransform: 'uppercase' }}>Search for a pilot above</div>
          </div>
        )}

        {pilot && (
          <div className="grid grid-cols-1 lg:grid-cols-[360px_1fr] gap-4 items-start">

            {/* ══════════ LEFT — profile sidebar ══════════ */}
            <div className="space-y-4">

              {/* ── Pilot name card ── */}
              <div className="vs-card px-4 py-3 flex items-center gap-3">
                <div style={{ width: 32, height: 32, background: 'rgba(77,124,15,0.12)', border: '1px solid rgba(77,124,15,0.25)', borderRadius: 2, display: 'flex', alignItems: 'center', justifyContent: 'center', flexShrink: 0 }}>
                  <PilotIcon size={14} style={{ color: 'var(--accent)' }} />
                </div>
                <div style={{ flex: 1, minWidth: 0 }}>
                  <div style={{ fontSize: '1.1rem', fontWeight: 700, color: 'var(--text)', letterSpacing: '0.03em', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{pilot.name}</div>
                  <div className="flex items-center gap-2 mt-0.5">
                    {pilotRank && pilotRank <= 3 && (
                      <span style={{ fontSize: '0.9rem' }}>{['🥇', '🥈', '🥉'][pilotRank - 1]}</span>
                    )}
                    {pilotRank && (
                      <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', fontFamily: 'monospace' }}>
                        Rank #{pilotRank}
                      </span>
                    )}
                    {user?.ucid === pilot.ucid && (
                      <span style={{ fontSize: '0.5rem', color: 'var(--accent)', border: '1px solid var(--accent)', padding: '0 4px', borderRadius: 2, letterSpacing: '0.08em' }}>YOU</span>
                    )}
                  </div>
                </div>
                <button
                  onClick={() => { clearSelection(); setSearch('') }}
                  className="vs-touch-target"
                  style={{ background: 'none', border: 'none', color: 'var(--text-dim)', cursor: 'pointer', display: 'flex', alignItems: 'center', gap: 4, fontSize: '0.62rem', flexShrink: 0 }}
                >
                  <RotateCcw size={10} /> Clear
                </button>
              </div>

              {/* ── Most flown airframe (hero) ── */}
              {mostFlown && (
                <div className="vs-card p-0" style={{ overflow: 'hidden' }}>
                  <AircraftImage type={mostFlown} />
                  <div className="px-4 py-3">
                    <div style={dim}>Most Flown Airframe</div>
                    <div style={{ fontSize: '1.15rem', color: '#60a5fa', fontWeight: 700, letterSpacing: '0.05em', marginTop: 2 }}>{mostFlown}</div>
                  </div>
                </div>
              )}

              {/* ── Career stats ── */}
              <div className="vs-card p-0">
                <div className="flex items-center gap-2 px-4 pt-3.5 pb-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
                  <KillIcon size={12} style={{ color: 'var(--accent)' }} />
                  <span style={dim}>Career Stats</span>
                </div>
                <div className="px-4 py-1">
                  {career.map(s => (
                    <div key={s.label} className="flex justify-between items-center" style={{ padding: '0.5rem 0', borderBottom: '1px solid rgba(42,42,42,0.5)' }}>
                      <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.08em' }}>{s.label}</span>
                      <span style={{ fontSize: '0.85rem', fontWeight: 700, color: s.color, fontFamily: 'monospace' }}>{s.value}</span>
                    </div>
                  ))}
                </div>
              </div>

              {/* ── Contributions ── */}
              <div className="vs-card p-0">
                <div className="flex items-center gap-2 px-4 pt-3.5 pb-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
                  <Award size={12} style={{ color: 'var(--accent)' }} />
                  <span style={dim}>Contributions</span>
                </div>
                <div className="grid grid-cols-2 gap-0">
                  {contributions.map((s, i) => (
                    <div key={s.label} style={{ padding: '0.65rem 1rem', borderRight: i % 2 === 0 ? '1px solid var(--border)' : 'none', borderBottom: i < 6 ? '1px solid var(--border)' : 'none' }}>
                      <div style={{ fontSize: '0.56rem', color: 'var(--text-dim)', letterSpacing: '0.1em', textTransform: 'uppercase', marginBottom: '0.2rem' }}>{s.label}</div>
                      <div style={{ fontSize: '0.95rem', fontWeight: 700, color: 'var(--text-muted)', fontFamily: 'monospace' }}>{s.value}</div>
                    </div>
                  ))}
                </div>
              </div>
            </div>

            {/* ══════════ RIGHT — detail tables ══════════ */}
            <div className="space-y-4" style={{ minWidth: 0 }}>

              {/* ── Progression ── */}
              {progression && selected && <ProgressionCard p={progression} dim={dim} ucid={selected} pilot={pilot} />}

              {/* ── Career overview ── */}
              <PilotCharts key={pilot.ucid} sorties={sorties} />

              {/* ── Rivals & compare ── */}
              <RivalsCard ucid={pilot.ucid} sorties={sorties} kills={kills} pilots={pilots} />
              <CompareCard key={`cmp-${pilot.ucid}`} pilot={pilot} pilots={pilots} />

              {/* ── Theater breakdown ── */}
              {breakdown.length > 0 && (
                <div className="vs-card p-0">
                  <div className="flex items-center gap-2 px-4 pt-4 pb-3" style={{ borderBottom: '1px solid var(--border)' }}>
                    <Shield size={12} style={{ color: 'var(--accent)' }} />
                    <span style={dim}>Theater Breakdown</span>
                    <span className="ml-auto font-mono" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>{breakdown.length}</span>
                  </div>
                  <div className="p-3 grid gap-3" style={{ gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))' }}>
                    {breakdown.map(td => (
                      <TheaterCard key={td.round_id} td={td} />
                    ))}
                  </div>
                </div>
              )}

              {/* ── Flight log ── */}
              {/* Keyed by pilot: a different pilot starts on page one, nothing open. */}
              <FlightLog key={pilot.ucid} ucid={pilot.ucid} sorties={sorties} breakdown={breakdown} replays={replays} />

              {/* ── Kill log ── */}
              <KillLog kills={kills} allPilots={pilots} breakdown={breakdown} />

              {/* ── Deploy log ── */}
              <DeployLog deploys={deploys} total={pilot.deploys} />
            </div>
          </div>
        )}
      </div>
    </div>
  )
}
