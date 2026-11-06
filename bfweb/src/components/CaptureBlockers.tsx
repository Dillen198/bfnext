import { useMemo, useState, type CSSProperties } from 'react'
import Map, { Marker } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'
import type { CaptureReport } from '../api'
import { useTheme } from '@shared/web/theme'
import { mapStyleFor } from '../lib/mapStyle'
import {
  blockerTone, captureHeading, isDeadEnd, locatedTargets, targetBounds, targetColor, targetLine,
} from './captureReport'
import type { CaptureTarget } from '../api'
import { useDashLook } from '../lib/look'
import '../pages/levant/objectives.css'

/**
 * "Why can't this be captured?" -- the engine's capture report for one
 * objective, in the drawer. Players kept filing bugs for bases that were
 * simply still defended (or, on a badly placed zone, had nothing to destroy
 * at all); this names each blocker, what to do about it, and where the
 * things still standing are.
 */

const rowStyle = { padding: '6px 14px', fontSize: '0.68rem', lineHeight: 1.5 } as const

/** Levant Ablaze's marker colours: the same three meanings in its palette. */
function desertTargetColor(t: CaptureTarget): string {
  if (t.kind === 'building') return 'var(--yellow)'
  if (t.infantry) return 'var(--red)'
  return 'var(--rust-bright)'
}

function CaptureMap({ report, selected, onSelect, color = targetColor, className }: {
  report: CaptureReport
  selected: number | null
  onSelect: (i: number | null) => void
  color?: (t: CaptureTarget) => string
  className?: string
}) {
  const { theme } = useTheme()
  const mapStyle = useMemo(() => mapStyleFor(theme), [theme])
  const pts = useMemo(() => locatedTargets(report), [report])
  const bounds = useMemo(
    () => targetBounds(pts.map(({ t }) => ({ lat: t.lat!, lon: t.lon! }))),
    [pts],
  )
  if (!bounds) return null
  return (
    <div className={className} style={className ? undefined : { height: 190, margin: '4px 14px 8px', border: '1px solid var(--border)' }}>
      <Map
        key={theme}
        mapStyle={mapStyle}
        style={{ width: '100%', height: '100%' }}
        initialViewState={{ bounds, fitBoundsOptions: { padding: 24, maxZoom: 15 } }}
        dragRotate={false}
        attributionControl={false}
      >
        {pts.map(({ t, i }) => {
          const on = selected === i
          return (
            <Marker key={i} latitude={t.lat!} longitude={t.lon!}>
              <button
                type="button"
                aria-label={targetLine(t).what}
                title={`${targetLine(t).what} — ${targetLine(t).where}`}
                onClick={(e) => { e.stopPropagation(); onSelect(on ? null : i) }}
                style={{
                  width: on ? 16 : 10, height: on ? 16 : 10, padding: 0, cursor: 'pointer',
                  borderRadius: t.kind === 'building' ? 2 : '50%',
                  background: color(t), border: on ? '2px solid #fff' : '1px solid rgba(0,0,0,0.8)',
                  boxShadow: on ? '0 0 0 4px rgba(255,255,255,0.25)' : undefined,
                }}
              />
            </Marker>
          )
        })}
      </Map>
    </div>
  )
}

export default function CaptureBlockers({ report, objectiveName }: {
  report: CaptureReport
  objectiveName: string
}) {
  const look = useDashLook()
  const [selected, setSelected] = useState<number | null>(null)
  const [showMap, setShowMap] = useState(true)
  const located = locatedTargets(report).length
  const deadEnd = report.blockers.some(b => isDeadEnd(b.kind))
  const standing = report.targets.reduce((n, t) => n + t.count, 0)
  const squads = report.troops.map(t =>
    `${t.name}${t.can_capture ? '' : " (can't capture)"} — ${t.in_zone ? 'in the zone' : `${(t.outside_by_m / 1000).toFixed(1)} km out`}`,
  ).join('; ')

  // Levant Ablaze: the verdict on tape (rust when it can be taken, red for a
  // dead end), the blockers numbered in Oswald, what is still standing as a
  // list with its grid, and the mini-map in a charred frame.
  if (look.profile) {
    const verdict = report.open || report.in_progress ? 'rust' : deadEnd ? 'r' : 'n'
    return (
      <section aria-label={`Capture status of ${objectiveName}`} className="ob-cb">
        <div className="ob-cb-head">
          <span className={`ds-tape sm ${verdict}`}>{captureHeading(report)}</span>
        </div>
        <p className={`ob-cb-summary${report.open ? ' open' : ''}`}>{report.summary}</p>

        {report.blockers.length > 0 && (
          <ol className="ob-cb-blockers">
            {report.blockers.map((b, i) => (
              <li key={b.kind} style={{ '--tone': blockerTone(b.kind) } as CSSProperties}>
                <span className="n">{i + 1}</span>
                <span>
                  <span>{b.text}</span>
                  {b.action && <span className="act">→ {b.action}</span>}
                </span>
              </li>
            ))}
          </ol>
        )}

        {deadEnd && (
          <p className="ob-cb-note">
            This is a problem with how the mission is built, not something you can fix in the air.
            Report it to the admins with the objective name ({objectiveName}).
          </p>
        )}

        {report.targets.length > 0 && (
          <>
            <div className="ob-sec-k">
              <span className="ds-k">Still standing ({standing})</span>
              {located > 0 && (
                <button type="button" className="ob-textbtn" onClick={() => setShowMap(v => !v)}>
                  {showMap ? 'Hide map' : 'Show on map'}
                </button>
              )}
            </div>
            {showMap && located > 0 && (
              <CaptureMap report={report} selected={selected} onSelect={setSelected} color={desertTargetColor} className="ob-cb-map" />
            )}
            <ul className="ob-cb-targets">
              {report.targets.map((t, i) => {
                const { what, where } = targetLine(t)
                const placeable = t.lat != null && t.lon != null
                return (
                  <li key={i}>
                    <button type="button" disabled={!placeable} aria-pressed={selected === i}
                      className={selected === i ? 'on' : undefined}
                      onClick={() => setSelected(selected === i ? null : i)}>
                      <i aria-hidden className={t.kind === 'building' ? 'bld' : undefined} style={{ background: desertTargetColor(t) }} />
                      <span className="what">{what}</span>
                      <span className={`where${placeable ? '' : ' unk'}`}>{where}</span>
                    </button>
                  </li>
                )
              })}
            </ul>
          </>
        )}

        {report.troops.length > 0 && (
          <p className="ob-cb-note">Your squads nearby: {squads}</p>
        )}

        {report.notes.map((n, i) => <p key={i} className="ob-cb-note dim">{n}</p>)}
      </section>
    )
  }

  return (
    <section aria-label={`Capture status of ${objectiveName}`} style={{ borderBottom: '1px solid var(--border)' }}>
      <div className="lb-card-head">{captureHeading(report)}</div>
      <div style={{ ...rowStyle, fontWeight: 600, color: report.open ? 'var(--accent-bright)' : 'var(--text)' }}>
        {report.summary}
      </div>

      {report.blockers.length > 0 && (
        <ol style={{ listStyle: 'none', margin: 0, padding: 0 }}>
          {report.blockers.map((b, i) => (
            <li key={b.kind} style={{ ...rowStyle, display: 'flex', gap: 8 }}>
              <span className="font-mono-vs" style={{ color: blockerTone(b.kind), flexShrink: 0 }}>{i + 1}.</span>
              <span>
                <span>{b.text}</span>
                {b.action && <span style={{ display: 'block', color: 'var(--text-muted)' }}>→ {b.action}</span>}
              </span>
            </li>
          ))}
        </ol>
      )}

      {deadEnd && (
        <div style={{ ...rowStyle, color: 'var(--text-dim)' }}>
          This is a problem with how the mission is built, not something you can fix in the air.
          Report it to the admins with the objective name ({objectiveName}).
        </div>
      )}

      {report.targets.length > 0 && (
        <>
          <div style={{
            display: 'flex', alignItems: 'baseline', gap: 6, padding: '9px 14px 3px',
            fontFamily: 'var(--font-mono)', fontSize: '0.56rem', letterSpacing: '0.16em',
            textTransform: 'uppercase', color: 'var(--text-dim)',
          }}>
            <span>Still standing ({standing})</span>
            {located > 0 && (
              <button type="button" onClick={() => setShowMap(v => !v)}
                style={{ marginLeft: 'auto', background: 'none', border: 'none', cursor: 'pointer', color: 'var(--accent-bright)', font: 'inherit', letterSpacing: 'inherit', textTransform: 'inherit', padding: 0 }}>
                {showMap ? 'Hide map' : 'Show on map'}
              </button>
            )}
          </div>
          {showMap && located > 0 && <CaptureMap report={report} selected={selected} onSelect={setSelected} />}
          <ul style={{ listStyle: 'none', margin: 0, padding: '0 0 6px' }}>
            {report.targets.map((t, i) => {
              const { what, where } = targetLine(t)
              const placeable = t.lat != null && t.lon != null
              return (
                <li key={i}>
                  <button
                    type="button"
                    disabled={!placeable}
                    onClick={() => setSelected(selected === i ? null : i)}
                    style={{
                      display: 'flex', width: '100%', gap: 8, alignItems: 'center', textAlign: 'left',
                      padding: '2px 14px', fontSize: '0.66rem', border: 'none', font: 'inherit',
                      background: selected === i ? 'var(--bg-elevated)' : 'none',
                      color: 'inherit', cursor: placeable ? 'pointer' : 'default',
                    }}
                  >
                    <span aria-hidden style={{ width: 7, height: 7, flexShrink: 0, background: targetColor(t), borderRadius: t.kind === 'building' ? 1 : '50%' }} />
                    <span style={{ flex: 1, minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{what}</span>
                    <span className="font-mono-vs" style={{ fontSize: '0.6rem', color: placeable ? 'var(--text-muted)' : 'var(--text-dim)', flexShrink: 0 }}>{where}</span>
                  </button>
                </li>
              )
            })}
          </ul>
        </>
      )}

      {report.troops.length > 0 && (
        <div style={{ ...rowStyle, color: 'var(--text-muted)' }}>
          Your squads nearby:{' '}
          {squads}
        </div>
      )}

      {report.notes.map((n, i) => (
        <div key={i} style={{ ...rowStyle, color: 'var(--text-dim)' }}>{n}</div>
      ))}
    </section>
  )
}
