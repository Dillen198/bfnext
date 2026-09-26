/**
 * Tracks on the real map. Used for a bomb's release-to-impact path and, with
 * a time slider, for engagement / helicopter / CAS paths.
 */
import { useMemo } from 'react'
import { Layer, Marker, Source } from 'react-map-gl/maplibre'
import type { FeatureCollection } from 'geojson'
import { Pause, Play } from '@icons'
import { fmt, fmtClock, mToFt } from '../../lib/format'
import { boundsOf, type LatLon } from '../../lib/geo'
import type { TrackPt } from '../../types'
import { RangeMap } from '../RangeMap'
import { indexAtTime, usePlayback } from './scale'

export interface MapPoint extends LatLon {
  id: string
  label: string
  color: string
  shape?: 'dot' | 'ring' | 'diamond'
}

const PALETTE = ['var(--ball)', 'var(--sky)', 'var(--wave)', 'var(--datum)', '#c28cff', '#ff8fd1']
/** Resolved colours for maplibre paint (it cannot read CSS variables). */
function resolve(c: string): string {
  if (!c.startsWith('var(')) return c
  const name = c.slice(4, -1)
  const v = getComputedStyle(document.documentElement).getPropertyValue(name).trim()
  return v || '#ffb23e'
}

function PointMarker({ p }: { p: MapPoint }) {
  const shape = p.shape ?? 'dot'
  return (
    <Marker latitude={p.lat} longitude={p.lon} anchor="left" offset={[-6, 0]}>
      <div className="flex items-center gap-1.5">
        <span
          style={{
            width: 12, height: 12, flex: 'none', display: 'inline-block',
            background: shape === 'ring' ? 'transparent' : p.color,
            border: `2px solid ${shape === 'ring' ? p.color : 'var(--map-bg)'}`,
            borderRadius: shape === 'diamond' ? 1 : '50%',
            transform: shape === 'diamond' ? 'rotate(45deg) scale(0.85)' : undefined,
          }}
        />
        <span className="map-label">{p.label}</span>
      </div>
    </Marker>
  )
}

/** Static tracks + labelled points. */
export function TrackMap({ lines, points, height = 360 }: {
  lines: { id: string; pts: LatLon[]; color: string; dashed?: boolean }[]
  points: MapPoint[]
  height?: number
}) {
  const bounds = useMemo(() => boundsOf([...lines.flatMap(l => l.pts), ...points], 400), [lines, points])
  const fc = useMemo<FeatureCollection>(() => ({
    type: 'FeatureCollection',
    features: lines.map(l => ({
      type: 'Feature',
      properties: { color: resolve(l.color), dashed: l.dashed ? 1 : 0 },
      geometry: { type: 'LineString', coordinates: l.pts.map(p => [p.lon, p.lat]) },
    })),
  }), [lines])
  return (
    <RangeMap height={height} bounds={bounds}>
      <Source id="tracks" type="geojson" data={fc}>
        <Layer id="tracks-solid" type="line" filter={['==', ['get', 'dashed'], 0]} paint={{ 'line-color': ['get', 'color'], 'line-width': 2.5 }} />
        <Layer id="tracks-dashed" type="line" filter={['==', ['get', 'dashed'], 1]} paint={{ 'line-color': ['get', 'color'], 'line-width': 2, 'line-dasharray': [2, 2] }} />
      </Source>
      {points.map(p => <PointMarker key={p.id} p={p} />)}
    </RangeMap>
  )
}

/** Several named paths with a shared time slider. */
export function PathReplay({ paths, height = 420 }: { paths: Record<string, TrackPt[]>; height?: number }) {
  const names = Object.keys(paths)
  const all = names.flatMap(n => paths[n])
  const t0 = Math.min(...all.map(p => p.t))
  const t1 = Math.max(...all.map(p => p.t))
  const pb = usePlayback(t0, t1, 6)
  const bounds = useMemo(() => boundsOf(all, 300), [all])
  const colors = names.map((_, i) => PALETTE[i % PALETTE.length])

  const fc = useMemo<FeatureCollection>(() => ({
    type: 'FeatureCollection',
    features: names.map((n, i) => ({
      type: 'Feature',
      properties: { color: resolve(colors[i]) },
      geometry: { type: 'LineString', coordinates: paths[n].filter(p => p.t <= pb.t).map(p => [p.lon, p.lat]) },
    })),
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }), [paths, pb.t])
  const ghost = useMemo<FeatureCollection>(() => ({
    type: 'FeatureCollection',
    features: names.map((n, i) => ({
      type: 'Feature',
      properties: { color: resolve(colors[i]) },
      geometry: { type: 'LineString', coordinates: paths[n].map(p => [p.lon, p.lat]) },
    })),
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }), [paths])

  const now = names.map(n => paths[n][Math.max(0, indexAtTime(paths[n], pb.t))])

  return (
    <div className="flex flex-col gap-2">
      <div className="flex flex-wrap items-center gap-3">
        <button className="btn-range sm" onClick={pb.playing ? pb.pause : pb.play}>
          {pb.playing ? <Pause size={14} /> : <Play size={14} />} {pb.playing ? 'Pause' : 'Replay'}
        </button>
        <input className="scrub flex-1 min-w-[160px]" type="range" min={t0} max={t1} step={0.5} value={pb.t} onChange={e => pb.setT(Number(e.target.value))} aria-label="Time" />
        <span className="mono text-[12px] muted w-[64px] text-right">{fmtClock(pb.t - t0)}</span>
      </div>
      <div className="readout">
        {names.map((n, i) => now[i] && (
          <span key={n}>
            <span className="dot" style={{ background: colors[i], marginRight: 6 }} />
            {n} <b>{fmt(mToFt(now[i].alt_m))} ft</b> <b>{fmt(now[i].speed_kts)} kt</b>
          </span>
        ))}
      </div>
      <RangeMap height={height} bounds={bounds}>
        <Source id="ghost" type="geojson" data={ghost}>
          <Layer id="ghost" type="line" paint={{ 'line-color': ['get', 'color'], 'line-width': 1, 'line-opacity': 0.3 }} />
        </Source>
        <Source id="paths" type="geojson" data={fc}>
          <Layer id="paths" type="line" paint={{ 'line-color': ['get', 'color'], 'line-width': 2.5 }} />
        </Source>
        {names.map((n, i) => now[i] && (
          <PointMarker key={n} p={{ id: n, lat: now[i].lat, lon: now[i].lon, label: n, color: colors[i] }} />
        ))}
      </RangeMap>
    </div>
  )
}
