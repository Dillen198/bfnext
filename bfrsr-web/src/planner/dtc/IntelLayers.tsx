// The briefing map's picture on the cartridge map: frontline, objectives,
// intel threat rings, tasking, and what JTACs, AWACS and recon see now.
// Everything is a pick: click it to see its coordinates and send it into
// the cartridge.

import { useMemo } from 'react'
import { Layer, Marker, Source } from 'react-map-gl/maplibre'
import { useQuery } from '@tanstack/react-query'
import { api, type Frontlines, type GroundContact } from '../../api'
import { airPick, contactPick, objectivePick, taskPick, threatPick, type Intel, type LayerKey, type Pick, type PickKind } from './picks'


const OWNER = (o: string) => (o === 'Blue' ? '#40a6ff' : o === 'Red' ? '#b84dff' : '#d8d8d8')
const URGENCY = (u: string) => (u === 'critical' ? '#f04747' : u === 'high' ? '#f0a030' : '#8ec83f')
/** One letter per ground class, on a red square: hostile ground. */
const CLASS_LETTER: Record<GroundContact['class'], string> = {
  armor: 'A', airdefense: 'S', artillery: 'R', infantry: 'I', airbase: 'B', naval: 'N', unknown: '?',
}

function ring(lat: number, lon: number, r: number, n = 48): number[][] {
  const dLat = r / 110_574
  const dLon = r / (111_320 * Math.cos((lat * Math.PI) / 180) || 1)
  return Array.from({ length: n + 1 }, (_, i) => {
    const a = (i / n) * Math.PI * 2
    return [lon + Math.cos(a) * dLon, lat + Math.sin(a) * dLat]
  })
}

const label: React.CSSProperties = {
  position: 'absolute', left: '50%', top: '100%', transform: 'translate(-50%, 2px)', whiteSpace: 'nowrap',
  fontSize: 9, fontWeight: 700, color: '#fff', textShadow: '0 0 3px #000, 0 0 3px #000', pointerEvents: 'none',
}

export default function IntelLayers({ intel, side, on, picked, onPick }: {
  intel: Intel
  side: string
  on: Record<LayerKey, boolean>
  /** Keys of the picks selected, drawn ringed. */
  picked: Set<string>
  onPick: (p: Pick, kind: PickKind) => void
}) {
  const r = intel.report
  const tac = intel.tac
  const { data: fronts } = useQuery<Frontlines>({ queryKey: ['frontline'], queryFn: () => api.frontline(), refetchInterval: 60_000, enabled: on.front })

  const frontGeo = useMemo(() => ({
    type: 'FeatureCollection' as const,
    features: !fronts ? [] : (['mid', 'blue', 'red'] as const).flatMap(k => (fronts[k] ?? []).map(line => ({
      type: 'Feature' as const, properties: { kind: k },
      geometry: { type: 'LineString' as const, coordinates: line.map(([lat, lon]) => [lon, lat]) },
    }))),
  }), [fronts])

  const contacts = useMemo(() => (tac?.ground ?? []).filter(c => c.side !== side), [tac, side])
  const ringGeo = useMemo(() => {
    const feats: { type: 'Feature'; properties: { kind: string; confidence: number }; geometry: { type: 'Polygon'; coordinates: number[][][] } }[] = []
    if (on.threat) for (const t of r?.threats ?? []) {
      if ((t.radius_m ?? 0) > 0) feats.push({ type: 'Feature', properties: { kind: 'ring', confidence: t.confidence }, geometry: { type: 'Polygon', coordinates: [ring(t.lat, t.lon, t.radius_m!)] } })
    }
    if (on.contact) for (const c of contacts) {
      if (c.threat_range_m) feats.push({ type: 'Feature', properties: { kind: 'ring', confidence: c.confidence }, geometry: { type: 'Polygon', coordinates: [ring(c.lat, c.lon, c.threat_range_m)] } })
      // How far off the position may be, as seen.
      if (c.uncertainty_m > 300) feats.push({ type: 'Feature', properties: { kind: 'error', confidence: c.confidence }, geometry: { type: 'Polygon', coordinates: [ring(c.lat, c.lon, c.uncertainty_m, 32)] } })
    }
    return { type: 'FeatureCollection' as const, features: feats }
  }, [r, contacts, on.threat, on.contact])

  const click = (p: Pick) => (e: { originalEvent: MouseEvent }) => { e.originalEvent.stopPropagation(); onPick(p, p.kind) }
  const sel = (p: Pick) => (picked.has(p.key) ? { boxShadow: '0 0 0 3px #fff, 0 0 8px 3px #fff8' } : {})
  const bull = (tac?.bullseye ?? []).find(b => b.side === side)

  return (
    <>
      {on.front && (
        <Source id="dtc-front" type="geojson" data={frontGeo}>
          <Layer id="dtc-front-line" type="line" paint={{
            'line-color': ['match', ['get', 'kind'], 'blue', '#4a8fd4', 'red', '#cc4444', '#cccccc'] as never,
            'line-width': ['match', ['get', 'kind'], 'mid', 2, 1.5] as never, 'line-opacity': 0.75, 'line-dasharray': [2, 2],
          }} />
        </Source>
      )}
      <Source id="dtc-rings" type="geojson" data={ringGeo}>
        <Layer id="dtc-rings-fill" type="fill" filter={['==', ['get', 'kind'], 'ring']} paint={{ 'fill-color': '#f04747', 'fill-opacity': 0.07 }} />
        <Layer id="dtc-rings-line" type="line" filter={['==', ['get', 'kind'], 'ring']}
          paint={{ 'line-color': '#f04747', 'line-width': 1.3, 'line-dasharray': [3, 2], 'line-opacity': ['max', 0.3, ['get', 'confidence']] as never }} />
        <Layer id="dtc-error-line" type="line" filter={['==', ['get', 'kind'], 'error']}
          paint={{ 'line-color': '#ffb3b3', 'line-width': 1, 'line-dasharray': [1, 2], 'line-opacity': 0.6 }} />
      </Source>

      {on.objective && (r?.map ?? []).filter(o => o.lat || o.lon).map(o => {
        const p = objectivePick(o, side)
        const s = o.primary ? 14 : 10
        return (
          <Marker key={p.key} latitude={o.lat} longitude={o.lon} onClick={click(p)}>
            <div title={`${o.name}: ${p.detail}`} style={{ position: 'relative', width: s, height: s, borderRadius: '50%', background: OWNER(o.owner), opacity: 0.4 + (o.health / 100) * 0.6, border: '1px solid #000a', cursor: 'pointer', ...sel(p) }}>
              <span style={{ ...label, color: OWNER(o.owner) }}>{o.name}</span>
            </div>
          </Marker>
        )
      })}

      {on.threat && (r?.threats ?? []).filter(t => t.lat || t.lon).map((t, i) => {
        const p = threatPick(t, i)
        return (
          <Marker key={p.key} latitude={t.lat} longitude={t.lon} onClick={click(p)}>
            <div title={`${t.label}: ${p.detail}`} style={{ width: 12, height: 12, transform: 'rotate(45deg)', background: '#f04747', border: '1px solid #200', cursor: 'pointer', ...sel(p) }} />
          </Marker>
        )
      })}

      {on.task && (r?.tasking ?? []).filter(t => t.lat || t.lon).map((t, i) => {
        const p = taskPick(t, i + 1)
        return (
          <Marker key={p.key} latitude={t.lat} longitude={t.lon} onClick={click(p)}>
            <div title={`${t.title}: ${t.detail}`} style={{
              minWidth: 18, height: 18, padding: '0 4px', borderRadius: 4, background: URGENCY(t.urgency), color: '#0b0f08',
              fontFamily: 'var(--font-mono)', fontSize: 10, fontWeight: 800, display: 'flex', alignItems: 'center', justifyContent: 'center',
              border: '1px solid #0008', cursor: 'pointer', ...sel(p),
            }}>T{i + 1}</div>
          </Marker>
        )
      })}

      {on.contact && contacts.map(c => {
        const p = contactPick(c)
        return (
          <Marker key={p.key} latitude={c.lat} longitude={c.lon} onClick={click(p)}>
            <div title={`${p.name}: ${p.detail}`} style={{
              position: 'relative', width: 16, height: 16, background: '#e23b3b', color: '#fff', border: '1.5px solid #fff',
              fontSize: 10, fontWeight: 800, display: 'flex', alignItems: 'center', justifyContent: 'center', cursor: 'pointer',
              opacity: Math.max(0.45, c.confidence), ...sel(p),
            }}>
              {CLASS_LETTER[c.class]}
              <span style={label}>{c.count > 1 ? `${c.count}x ` : ''}{c.source.toUpperCase()}</span>
            </div>
          </Marker>
        )
      })}

      {on.air && (tac?.air ?? []).filter(a => a.iff === 'hostile').map(a => {
        const p = airPick(a)
        return (
          <Marker key={p.key} latitude={a.lat} longitude={a.lon} onClick={click(p)}>
            <div title={`${p.name}: ${p.detail}`} style={{ width: 14, height: 14, transform: `rotate(${a.heading}deg)`, clipPath: 'polygon(50% 0%, 100% 100%, 50% 78%, 0% 100%)', background: '#f04747', cursor: 'pointer', ...sel(p) }} />
          </Marker>
        )
      })}

      {on.bullseye && bull && (
        <Marker latitude={bull.lat} longitude={bull.lon}>
          <div title="Bullseye" style={{ width: 22, height: 22, borderRadius: '50%', border: '2px solid #40a6ff', display: 'flex', alignItems: 'center', justifyContent: 'center', pointerEvents: 'none' }}>
            <div style={{ width: 8, height: 8, borderRadius: '50%', border: '2px solid #40a6ff' }} />
          </div>
        </Marker>
      )}
    </>
  )
}
