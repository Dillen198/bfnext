import { useMemo, useState } from 'react'
import Map, { Marker } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'
import type { CaptureReport } from '../api'
import { useTheme } from '@shared/web/theme'
import { mapStyleFor } from '../lib/mapStyle'
import {
  blockerTone, captureHeading, isDeadEnd, locatedTargets, targetBounds, targetColor, targetLine,
} from './captureReport'

/**
 * "Why can't this be captured?" -- the engine's capture report for one
 * objective, in the drawer. Players kept filing bugs for bases that were
 * simply still defended (or, on a badly placed zone, had nothing to destroy
 * at all); this names each blocker, what to do about it, and where the
 * things still standing are.
 */

const rowStyle = { padding: '6px 14px', fontSize: '0.68rem', lineHeight: 1.5 } as const

function CaptureMap({ report, selected, onSelect }: {
  report: CaptureReport
  selected: number | null
  onSelect: (i: number | null) => void
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
    <div style={{ height: 190, margin: '4px 14px 8px', border: '1px solid var(--border)' }}>
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
                  background: targetColor(t), border: on ? '2px solid #fff' : '1px solid rgba(0,0,0,0.8)',
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
  const [selected, setSelected] = useState<number | null>(null)
  const [showMap, setShowMap] = useState(true)
  const located = locatedTargets(report).length
  const deadEnd = report.blockers.some(b => isDeadEnd(b.kind))

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
            <span>Still standing ({report.targets.reduce((n, t) => n + t.count, 0)})</span>
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
          {report.troops.map(t =>
            `${t.name}${t.can_capture ? '' : " (can't capture)"} — ${t.in_zone ? 'in the zone' : `${(t.outside_by_m / 1000).toFixed(1)} km out`}`,
          ).join('; ')}
        </div>
      )}

      {report.notes.map((n, i) => (
        <div key={i} style={{ ...rowStyle, color: 'var(--text-dim)' }}>{n}</div>
      ))}
    </section>
  )
}
