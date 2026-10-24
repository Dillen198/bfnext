// One sortie, opened from the Flight Log on the Pilots page: what the pilot
// killed, with what, what they carried, and how the flight ended -- drawn as a
// timeline from takeoff to the end of the flight, with the weapons and the
// event list underneath.
//
// bfdb attributes kills, shots, deaths and deploys to a sortie by time (see
// `pilot_sortie_stats`); shots only reach it inside a kill record, so the shot
// figures here are "shots at targets that died", never a true hit rate.

import React, { useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Link as RouterLink } from 'react-router-dom'
import type { FlightRow } from '../replay/data'
import { api, type SortieDetail as Detail } from '../api'
import { OUTCOME } from '../lib/sortie'
import { fmtDuration } from '../lib/format'
import { PlayCircle } from '@icons'

const AIR = '#60a5fa'
const GROUND = '#f97316'
const DEPLOY = '#22c55e'
const DEATH = '#ef4444'

const sideColor = (side: string | null | undefined) =>
  side === 'Blue' ? '#3b82f6' : side === 'Red' ? '#ef4444' : '#64748b'

/** Seconds since takeoff as "T+1:04:09" / "T+4:09". */
function fmtElapsed(secs: number): string {
  const s = Math.max(0, Math.round(secs))
  const h = Math.floor(s / 3600)
  const m = Math.floor((s % 3600) / 60)
  const ss = String(s % 60).padStart(2, '0')
  return h > 0 ? `T+${h}:${String(m).padStart(2, '0')}:${ss}` : `T+${m}:${ss}`
}

function fmtClock(iso: string): string {
  return new Date(iso).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false })
}

type Ev =
  | { kind: 'kill'; t: number; air: boolean; text: string; sub: string | null; side: string }
  | { kind: 'deploy'; t: number; text: string; sub: string | null }
  | { kind: 'death'; t: number; text: string; sub: string | null }

function events(d: Detail): Ev[] {
  const t0 = Date.parse(d.takeoff)
  const at = (iso: string) => (Date.parse(iso) - t0) / 1000
  const out: Ev[] = []
  for (const k of d.kills) {
    out.push({
      kind: 'kill', t: at(k.time), air: k.air, side: k.victim_side,
      text: k.victim_name ? `${k.victim_name} (${k.target_type})` : k.target_type,
      sub: k.weapon,
    })
  }
  for (const dp of d.deploys) {
    out.push({
      kind: 'deploy', t: at(dp.time), text: dp.deployable,
      sub: dp.method === 'AirDrop' ? 'air drop' : dp.method === 'ManualUnpack' ? 'unpacked' : null,
    })
  }
  if (d.death) {
    const x = d.death
    const by = x.killer_name ?? x.killer_type
    out.push({
      kind: 'death', t: at(x.time),
      text: d.outcome === 'killed'
        ? `Shot down${by ? ` by ${by}` : ''}${x.teamkill ? ' (teamkill)' : ''}`
        : 'Lost the aircraft',
      sub: d.outcome === 'killed'
        ? [x.killer_name ? x.killer_type : null, x.weapon].filter(Boolean).join(' · ') || null
        : null,
    })
  }
  return out.sort((a, b) => a.t - b.t)
}

// ── Mission timeline ──────────────────────────────────────────────────────────

// Laid out in percentages rather than an SVG viewBox, so the markers and the
// labels stay their real size at any card width.
const LANE = 22

function Timeline({ d, evs }: { d: Detail; evs: Ev[] }) {
  // The flight's own length when known; otherwise stretch to the last event
  // so nothing falls off the right edge.
  const span = Math.max(d.duration_secs, ...evs.map(e => e.t), 60)
  const pct = (t: number) => `${(Math.min(Math.max(t, 0), span) / span) * 100}%`
  const ticks = [0, 0.25, 0.5, 0.75, 1].map(f => f * span)
  const flown = d.duration_secs > 0 ? d.duration_secs : span
  const at = (t: number): React.CSSProperties => ({
    position: 'absolute', left: pct(t), top: LANE, transform: 'translate(-50%, -50%)', lineHeight: 0,
  })

  return (
    <div role="img" aria-label="Sortie timeline" style={{ position: 'relative', height: 48, margin: '0 8px' }}>
      {/* Flight bar: the whole axis dim, the flown part lit. */}
      <div style={{ position: 'absolute', left: 0, right: 0, top: LANE - 1, height: 2, background: '#334155' }} />
      <div style={{ position: 'absolute', left: 0, width: pct(flown), top: LANE - 1, height: 2, background: 'var(--accent)', opacity: 0.6 }} />
      <span style={{ ...at(0), width: 8, height: 8, borderRadius: 4, background: 'var(--accent)' }} title="Takeoff" />
      {d.duration_secs > 0 && (
        <span style={{ ...at(d.duration_secs), width: 8, height: 8, borderRadius: 4, background: OUTCOME[d.outcome].color }}
          title={OUTCOME[d.outcome].label} />
      )}
      {ticks.map((t, i) => (
        <span key={i} className="font-mono" style={{
          position: 'absolute', left: pct(t), top: LANE + 9, fontSize: '0.56rem', color: '#64748b', whiteSpace: 'nowrap',
          transform: i === 0 ? 'none' : i === ticks.length - 1 ? 'translateX(-100%)' : 'translateX(-50%)',
        }}>
          {fmtElapsed(t)}
        </span>
      ))}
      {evs.map((e, i) => {
        const tip = `${fmtElapsed(e.t)}  ${e.text}${e.sub ? ` — ${e.sub}` : ''}`
        if (e.kind === 'kill') {
          const c = e.air ? AIR : GROUND
          return (
            <span key={i} title={tip} style={{ ...at(e.t), top: LANE - 12, fontSize: 12, color: c, lineHeight: 1, cursor: 'default' }}>▼</span>
          )
        }
        if (e.kind === 'deploy') {
          return (
            <span key={i} title={tip} style={{ ...at(e.t), width: 9, height: 9, background: DEPLOY, transform: 'translate(-50%, -50%) rotate(45deg)', cursor: 'default' }} />
          )
        }
        return (
          <span key={i} title={tip} style={{ ...at(e.t), fontSize: 15, fontWeight: 700, color: DEATH, lineHeight: 1, cursor: 'default' }}>✕</span>
        )
      })}
    </div>
  )
}

// ── Panel ─────────────────────────────────────────────────────────────────────

const label: React.CSSProperties = {
  fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase',
}

function Tile({ name, value, color, note }: { name: string; value: string | number; color?: string; note?: string }) {
  return (
    <div style={{ padding: '8px 10px', background: 'rgba(0,0,0,0.25)', border: '1px solid var(--border)', borderRadius: 4, minWidth: 0 }}
      title={note}>
      <div style={label}>{name}</div>
      <div className="font-mono" style={{
        fontSize: typeof value === 'string' && value.length > 6 ? '0.78rem' : '1rem',
        color: color ?? 'var(--text)', marginTop: 2, whiteSpace: 'nowrap', lineHeight: '1.3rem',
      }}>{value}</div>
    </div>
  )
}

function Weapons({ d }: { d: Detail }) {
  if (d.weapons.length === 0) return null
  const max = Math.max(...d.weapons.map(w => w.shots), 1)
  return (
    <div>
      <div style={{ ...label, marginBottom: 6 }}>Weapons</div>
      <div style={{ display: 'grid', gridTemplateColumns: 'minmax(90px, 160px) 1fr auto', gap: '4px 10px', alignItems: 'center' }}>
        {d.weapons.map(w => (
          <React.Fragment key={w.weapon}>
            <span style={{ fontSize: '0.66rem', color: 'var(--text-muted)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}
              title={w.weapon}>{w.weapon}</span>
            {/* Shots (track), hits (fill) */}
            <div style={{ position: 'relative', height: 8, background: 'rgba(148,163,184,0.12)', borderRadius: 2 }}>
              <div style={{ position: 'absolute', inset: 0, width: `${(w.shots / max) * 100}%`, background: 'rgba(148,163,184,0.35)', borderRadius: 2 }} />
              <div style={{ position: 'absolute', inset: 0, width: `${(w.hits / max) * 100}%`, background: 'var(--accent)', borderRadius: 2 }} />
            </div>
            <span className="font-mono" style={{ fontSize: '0.62rem', color: 'var(--text-dim)', whiteSpace: 'nowrap' }}>
              {w.shots} fired · {w.hits} hit · <span style={{ color: w.kills > 0 ? 'var(--text)' : undefined }}>{w.kills} kill{w.kills === 1 ? '' : 's'}</span>
            </span>
          </React.Fragment>
        ))}
      </div>
    </div>
  )
}

function EventList({ evs, takeoff }: { evs: Ev[]; takeoff: string }) {
  const t0 = Date.parse(takeoff)
  return (
    <div>
      <div style={{ ...label, marginBottom: 6 }}>Events</div>
      {evs.length === 0 ? (
        <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>No kills, deploys or losses recorded on this sortie.</div>
      ) : (
        <div style={{ display: 'grid', gridTemplateColumns: 'auto auto 1fr', gap: '3px 10px', alignItems: 'baseline' }}>
          {evs.map((e, i) => {
            const color = e.kind === 'kill' ? (e.air ? AIR : GROUND) : e.kind === 'deploy' ? DEPLOY : DEATH
            const tag = e.kind === 'kill' ? (e.air ? 'A/A KILL' : 'A/G KILL') : e.kind === 'deploy' ? 'DEPLOY' : 'LOSS'
            return (
              <React.Fragment key={i}>
                <span className="font-mono" style={{ fontSize: '0.62rem', color: '#64748b', whiteSpace: 'nowrap' }}
                  title={fmtClock(new Date(t0 + e.t * 1000).toISOString())}>
                  {fmtElapsed(e.t)}
                </span>
                <span style={{ fontSize: '0.56rem', color, letterSpacing: '0.1em', whiteSpace: 'nowrap' }}>{tag}</span>
                <span style={{ fontSize: '0.67rem', color: 'var(--text-muted)' }}>
                  {e.kind === 'kill' && <span style={{ color: sideColor(e.side), fontSize: '0.6em' }}>● </span>}
                  {e.text}
                  {e.sub && <span style={{ color: 'var(--text-dim)' }}> — {e.sub}</span>}
                </span>
              </React.Fragment>
            )
          })}
        </div>
      )}
    </div>
  )
}

/** `debriefLink` adds a link to the full-page debrief; the debrief page
 *  itself renders this panel without one. */
export default function SortieDetail({ ucid, sortieId, replay, debriefLink = true }: {
  ucid: string
  sortieId: number
  replay?: FlightRow
  debriefLink?: boolean
}) {
  const { data: d, isLoading, isError } = useQuery({
    queryKey: ['pilot-sortie', ucid, sortieId],
    queryFn: () => api.pilotSortie(ucid, sortieId),
    staleTime: 60_000,
  })
  const evs = useMemo(() => (d ? events(d) : []), [d])

  if (isLoading) {
    return <div style={{ padding: '12px 16px', fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em' }}>LOADING SORTIE…</div>
  }
  if (isError || !d) {
    return <div style={{ padding: '12px 16px', fontSize: '0.67rem', color: '#f87171' }}>Couldn't load this sortie.</div>
  }

  const kills = d.air_kills + d.ground_kills
  const hitPct = d.shots > 0 ? `${Math.round((d.hits / d.shots) * 100)}%` : '—'
  const shotNote = 'Counts only shots at targets that died: DCS reports a shot to the stats server inside the kill record, so misses on survivors are not seen.'

  return (
    <div style={{ padding: '12px 16px 16px', display: 'flex', flexDirection: 'column', gap: 14, background: 'rgba(0,0,0,0.18)' }}>
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(96px, 1fr))', gap: 8 }}>
        <Tile name="Outcome" value={OUTCOME[d.outcome].label} color={OUTCOME[d.outcome].color} />
        <Tile name="Duration" value={fmtDuration(d.duration_secs)} />
        <Tile name="A/A Kills" value={d.air_kills} color={d.air_kills ? AIR : undefined} />
        <Tile name="A/G Kills" value={d.ground_kills} color={d.ground_kills ? GROUND : undefined} />
        <Tile name="Shots on kills" value={d.shots} note={shotNote} />
        <Tile name="Hit rate" value={hitPct} note={shotNote} />
        <Tile name="Deploys" value={d.deploys.length} color={d.deploys.length ? DEPLOY : undefined} />
        {replay && (
          <Tile name="Tacview" value={`${replay.shots} / ${replay.kills}`}
            note="Shots fired / kills in the Tacview recording of this flight -- this one does count misses." />
        )}
      </div>

      <div>
        <div className="flex items-center gap-3" style={{ marginBottom: 4, flexWrap: 'wrap', rowGap: 2, whiteSpace: 'nowrap' }}>
          <span style={label}>Timeline</span>
          <span style={{ fontSize: '0.58rem', color: AIR }}>▼ A/A</span>
          <span style={{ fontSize: '0.58rem', color: GROUND }}>▼ A/G</span>
          <span style={{ fontSize: '0.58rem', color: DEPLOY }}>◆ Deploy</span>
          <span style={{ fontSize: '0.58rem', color: DEATH }}>✕ Loss</span>
          <span className="ml-auto" />
          {debriefLink && (
            <RouterLink to={`/pilots/${encodeURIComponent(ucid)}/sortie/${sortieId}`}
              style={{ color: 'var(--accent-bright)', fontSize: '0.6rem', letterSpacing: '0.08em', textDecoration: 'none', flexShrink: 0 }}>
              FULL DEBRIEF →
            </RouterLink>
          )}
          {replay && (
            <RouterLink to={`/replay/${encodeURIComponent(replay.rec)}?focus=${replay.i}`}
              style={{ display: 'inline-flex', alignItems: 'center', gap: 4, flexShrink: 0, color: 'var(--accent-bright)', fontSize: '0.6rem', letterSpacing: '0.08em', textDecoration: 'none' }}>
              <PlayCircle size={12} /> WATCH REPLAY
            </RouterLink>
          )}
        </div>
        <Timeline d={d} evs={evs} />
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))', gap: 16 }}>
        <EventList evs={evs} takeoff={d.takeoff} />
        {kills > 0 || d.weapons.length > 0 ? <Weapons d={d} /> : null}
      </div>
    </div>
  )
}
