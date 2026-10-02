/**
 * The live range picture on the map: the range's sectors underneath, then
 * stations with their scoring rings, tankers with TACAN/frequency, carriers
 * with BRC and final bearing, arenas, on-demand spawns, and every player.
 */
import { useLayoutEffect, useMemo, useRef, useState } from 'react'
import { Layer, Marker, Source, type MapLayerMouseEvent } from 'react-map-gl/maplibre'
import type { Feature, FeatureCollection } from 'geojson'
import { Aircraft, Carrier, Helicopter, Refuel, Strike, Crosshair, Ship, Armor, Sam, type IconComponent } from '@icons'
import { RangeMap } from '../../components/RangeMap'
import { useTheme } from '../../context/ThemeContext'
import { airframe, fmt, pad3 } from '../../lib/format'
import { boundsOf, circleRing, destination, NM, type LatLon } from '../../lib/geo'
import { drawSectors, showsFor, type SideFilter } from '../../lib/sectors'
import { usePref } from '../../lib/usePref'
import type { RangeLive, Sector, StationKind } from '../../types'
import { SECTOR_HIT_LAYER, SectorCard, SectorLabels, SectorLegend, SectorShapes, SectorTip } from './LiveSectors'

const ROTARY = /^(UH|CH|AH|Mi|Ka|SA342|OH)/

const STATION_ICON: Record<StationKind, IconComponent> = {
  bomb_circle: Strike,
  strafe_pit: Crosshair,
  tactical_array: Strike,
  convoy: Armor,
  coord_target: Strike,
  laser_target: Strike,
  ship_target: Ship,
  gunnery_lane: Armor,
  sam_site: Sam,
}

type Layers = { stations: boolean; air: boolean; labels: boolean }

/** The sector layer's settings, remembered per viewer. */
interface SectorPref {
  on: boolean
  side: SideFilter
  /** the colour key is open */
  key: boolean
}

const isSectorPref = (v: unknown): v is SectorPref => {
  const p = v as SectorPref | null
  return !!p && typeof p.on === 'boolean' && typeof p.key === 'boolean' && ['all', 'blue', 'red'].includes(p.side)
}

const SIDES: { v: SideFilter; label: string; title: string }[] = [
  { v: 'all', label: 'All', title: 'Every sector' },
  { v: 'blue', label: 'Blue', title: "Blue's sectors and the shared ones" },
  { v: 'red', label: 'Red', title: "Red's sectors and the shared ones" },
]

const hasHover = () => {
  try { return window.matchMedia('(hover: hover)').matches } catch { return true }
}
const isNarrow = () => {
  try { return window.matchMedia('(max-width: 640px)').matches } catch { return false }
}

/** The pointer on the map, and the map's size, in CSS pixels. */
type Pointer = { x: number; y: number; w: number; h: number }

/** Put the hover card beside the pointer, flipped to stay inside the map. */
function placeTip(el: HTMLDivElement | null, p: Pointer | null) {
  if (!el || !p) return
  const gap = 14
  let x = p.x + gap
  let y = p.y + gap
  if (x + el.offsetWidth > p.w - 6) x = p.x - el.offsetWidth - gap
  if (y + el.offsetHeight > p.h - 6) y = p.y - el.offsetHeight - gap
  el.style.transform = `translate(${Math.max(6, x)}px, ${Math.max(6, y)}px)`
}

export function LiveMap({ live, height }: { live: RangeLive; height: number | string }) {
  const { theme } = useTheme()
  const light = theme === 'light'
  const [show, setShow] = useState<Layers>({ stations: true, air: true, labels: true })
  const [zoom, setZoom] = useState(8)
  // how much text the labels carry: -1 none, 0 names only for the big
  // things, 1 names everywhere, 2 full detail (frequencies, BRC, targets)
  const detail = !show.labels ? -1 : zoom >= 10.5 ? 2 : zoom >= 9 ? 1 : 0

  // Sectors come from the mission config, so they rarely change: only redraw
  // when their content does, not on every 2 s poll.
  const sectorsKey = useMemo(() => JSON.stringify(live.sectors ?? []), [live.sectors])
  const drawn = useMemo(() => drawSectors(JSON.parse(sectorsKey) as Sector[]), [sectorsKey])
  const [canHover] = useState(hasHover)
  const [sec, setSec] = usePref<SectorPref>(
    'range.live.sectors',
    // the key starts open where there is room for it
    () => ({ on: true, side: 'all', key: !isNarrow() }),
    isSectorPref,
  )
  const sectorsOn = sec.on && drawn.length > 0
  const shown = useMemo(() => (sectorsOn ? drawn.filter(d => showsFor(d.sector, sec.side)) : []), [drawn, sectorsOn, sec.side])
  const shownIds = useMemo(() => new Set(shown.map(d => d.sector.id)), [shown])
  const [hoverId, setHoverId] = useState<string | null>(null)
  const [pinnedId, setPinnedId] = useState<string | null>(null)
  const pinned = shown.find(d => d.sector.id === pinnedId) ?? null
  const hovered = canHover && hoverId !== pinned?.sector.id ? shown.find(d => d.sector.id === hoverId) ?? null : null
  const highlight = pinned?.sector.id ?? hovered?.sector.id ?? null

  // the hover card follows the pointer without re-rendering the map
  const tipRef = useRef<HTMLDivElement>(null)
  const pointer = useRef<Pointer | null>(null)
  useLayoutEffect(() => placeTip(tipRef.current, pointer.current), [hovered])

  const onMouseMove = (e: MapLayerMouseEvent) => {
    const id = (e.features?.[0]?.properties?.id as string | undefined) ?? null
    const box = e.target.getContainer()
    pointer.current = { x: e.point.x, y: e.point.y, w: box.clientWidth, h: box.clientHeight }
    if (id !== hoverId) setHoverId(id)
    else placeTip(tipRef.current, pointer.current)
  }
  // Ask the map what is under the tap itself: `e.features` is the last hover,
  // and a tap on a phone need not move the pointer there first.
  const onClick = (e: MapLayerMouseEvent) => {
    let id: string | null = null
    try {
      const hit = e.target.queryRenderedFeatures(e.point, { layers: [SECTOR_HIT_LAYER] })
      id = (hit[0]?.properties?.id as string | undefined) ?? null
    } catch { /* style not loaded yet */ }
    setPinnedId(id)
  }
  const closeCard = () => { setPinnedId(null); setHoverId(null) }

  // frame everything once, on first render (the sectors only when nothing else is up)
  const [bounds] = useState(() => {
    const pts: LatLon[] = [...live.stations.map(s => s.pos), ...live.carriers.map(c => c.pos), ...live.tankers.map(t => t.pos), ...live.players.map(p => p.pos)]
    if (!pts.length) pts.push(...drawn.flatMap(d => d.ring.map(([lon, lat]) => ({ lat, lon }))))
    return boundsOf(pts, 4000)
  })

  const shapes = useMemo<FeatureCollection>(() => {
    const f: Feature[] = []
    for (const s of live.stations) {
      for (const r of s.rings_m) {
        f.push({ type: 'Feature', properties: { k: 'ring', hot: s.hot_by.length > 0 ? 1 : 0 }, geometry: { type: 'Polygon', coordinates: [circleRing(s.pos, r, 48)] } })
      }
    }
    for (const a of live.arenas) {
      f.push({ type: 'Feature', properties: { k: 'arena' }, geometry: { type: 'Polygon', coordinates: [circleRing(a.pos, a.radius_m, 96)] } })
    }
    for (const c of live.carriers) {
      const bow = destination(c.pos, c.brc_deg, 6 * NM)
      const aft = destination(c.pos, c.fb_deg + 180, 4 * NM)
      f.push({ type: 'Feature', properties: { k: 'brc' }, geometry: { type: 'LineString', coordinates: [[c.pos.lon, c.pos.lat], [bow.lon, bow.lat]] } })
      f.push({ type: 'Feature', properties: { k: 'fb' }, geometry: { type: 'LineString', coordinates: [[aft.lon, aft.lat], [c.pos.lon, c.pos.lat]] } })
    }
    return { type: 'FeatureCollection', features: f }
  }, [live])

  return (
    <div className="relative">
      <RangeMap
        height={height} bounds={bounds}
        onZoom={e => setZoom(e.viewState.zoom)} onLoad={e => setZoom(e.target.getZoom())}
        interactiveLayerIds={[SECTOR_HIT_LAYER]}
        onMouseMove={onMouseMove} onMouseOut={() => setHoverId(null)} onClick={onClick}
        cursor={hovered ? 'pointer' : undefined}
      >
        {/* first, so everything live draws on top of it */}
        <SectorShapes drawn={shown} visible={sectorsOn} highlight={highlight} light={light} />
        <SectorLabels drawn={drawn} visible={shownIds} zoom={zoom} detail={detail} highlight={highlight} light={light} />

        <Source id="live-shapes" type="geojson" data={shapes}>
          <Layer id="arena-fill" type="fill" filter={['==', ['get', 'k'], 'arena']} paint={{ 'fill-color': '#5aa9ff', 'fill-opacity': 0.04 }} />
          <Layer id="arena-line" type="line" filter={['==', ['get', 'k'], 'arena']} paint={{ 'line-color': '#5aa9ff', 'line-width': 1, 'line-dasharray': [4, 3], 'line-opacity': 0.6 }} />
          {show.stations && (
            <Layer id="rings" type="line" filter={['==', ['get', 'k'], 'ring']}
              paint={{ 'line-color': ['case', ['==', ['get', 'hot'], 1], '#38d77c', '#93a3b5'], 'line-width': 1, 'line-dasharray': [1, 2], 'line-opacity': 0.8 }} />
          )}
          <Layer id="brc" type="line" filter={['==', ['get', 'k'], 'brc']} paint={{ 'line-color': '#ffb23e', 'line-width': 1.6 }} />
          <Layer id="fb" type="line" filter={['==', ['get', 'k'], 'fb']} paint={{ 'line-color': '#38d77c', 'line-width': 1.2, 'line-dasharray': [3, 3] }} />
        </Source>

        {detail >= 0 && live.arenas.map(a => (
          <Marker key={a.id} latitude={destination(a.pos, 0, a.radius_m).lat} longitude={a.pos.lon} anchor="bottom">
            <div className="map-label" style={{ color: 'var(--sky)' }}>{a.name}{detail >= 1 ? ` · ${a.status}` : ''}</div>
          </Marker>
        ))}

        {show.stations && live.stations.map(s => {
          const I = STATION_ICON[s.kind]
          const hot = s.hot_by.length > 0
          return (
            <Marker key={s.id} latitude={s.pos.lat} longitude={s.pos.lon} anchor="left" offset={[-10, 0]}>
              <div className="flex items-center gap-1" title={`${s.name}${s.note ? ` — ${s.note}` : ''}`}>
                <span className="grid place-items-center" style={{ width: 20, height: 20, borderRadius: 2, background: 'var(--panel)', border: `1.5px solid ${hot ? 'var(--datum)' : 'var(--line-2)'}`, color: hot ? 'var(--datum)' : 'var(--haze)' }}>
                  <I size={13} />
                </span>
                {detail >= 1 && (
                  <span className="map-label" style={{ color: hot ? 'var(--datum)' : undefined }}>
                    {s.name.replace(/^Range /, '')}{detail >= 2 && s.targets_total > 1 ? ` ${s.targets_alive}/${s.targets_total}` : ''}
                  </span>
                )}
              </div>
            </Marker>
          )
        })}

        {live.carriers.map(c => (
          <Marker key={c.id} latitude={c.pos.lat} longitude={c.pos.lon} anchor="left" offset={[-11, 0]}>
            <div className="flex items-center gap-1.5">
              <span style={{ color: 'var(--ball)', transform: `rotate(${c.brc_deg}deg)`, display: 'inline-flex' }}><Carrier size={22} /></span>
              {detail >= 0 && (
                <span className="map-label">
                  <b>{c.name.split(' ')[0]}</b>{detail >= 1 && <> BRC {pad3(c.brc_deg)} · FB {pad3(c.fb_deg)}</>}
                  {detail >= 2 && <><br />WOD {fmt(c.wind_over_deck_kts, 1)} kt · {c.recovery_open ? <span style={{ color: 'var(--datum)' }}>RECOVERY OPEN</span> : 'deck closed'}</>}
                </span>
              )}
            </div>
          </Marker>
        ))}

        {show.air && live.tankers.filter(t => t.state !== 'dead').map(t => (
          <Marker key={t.id} latitude={t.pos.lat} longitude={t.pos.lon} anchor="left" offset={[-9, 0]}>
            <div className="flex items-center gap-1.5" style={{ opacity: t.state === 'rtb' ? 0.55 : 1 }}>
              <span style={{ color: 'var(--sky)', transform: `rotate(${t.heading_deg}deg)`, display: 'inline-flex' }}><Aircraft size={18} /></span>
              {detail >= 0 && (
                <span className="map-label" style={{ color: 'var(--sky)' }}>
                  <Refuel size={11} style={{ display: 'inline', verticalAlign: '-1px' }} /> {t.callsign}{detail >= 1 && <> · {t.tacan ?? airframe(t.unit_type)}</>}
                  {detail >= 2 && <><br />{airframe(t.unit_type)} · {t.freq_mhz.toFixed(3)} · FL{Math.round(t.alt_ft / 100)}</>}
                </span>
              )}
            </div>
          </Marker>
        ))}

        {show.air && live.spawns.map(s => (
          <Marker key={s.id} latitude={s.pos.lat} longitude={s.pos.lon} anchor="left" offset={[-5, 0]}>
            <div className="flex items-center gap-1.5">
              <span style={{ width: 10, height: 10, background: 'var(--wave)', transform: 'rotate(45deg)', display: 'inline-block', border: '1.5px solid var(--map-bg)' }} />
              {detail >= 1 && <span className="map-label" style={{ color: 'var(--wave)' }}>{s.label.split(' · ')[0]} ({s.owner_name})</span>}
            </div>
          </Marker>
        ))}

        {show.air && live.players.map(p => {
          const I = ROTARY.test(p.unit_type) ? Helicopter : Aircraft
          return (
            <Marker key={p.ucid} latitude={p.pos.lat} longitude={p.pos.lon} anchor="left" offset={[-8, 0]}>
              <div className="flex items-center gap-1" title={`${p.name} · ${airframe(p.unit_type)}${p.activity ? ` · ${p.activity}` : ''}`}>
                <span style={{ color: p.in_air ? 'var(--chalk)' : 'var(--dim)', transform: `rotate(${p.heading_deg}deg)`, display: 'inline-flex', filter: 'drop-shadow(0 0 2px var(--map-bg))' }}>
                  <I size={16} strokeWidth={2} />
                </span>
                {detail >= 1 && <span className="map-label">{p.name}</span>}
              </div>
            </Marker>
          )
        })}
      </RangeMap>
      <div className="absolute left-2 top-2 flex flex-col items-start gap-1.5" style={{ maxWidth: 'calc(100% - 56px)' }}>
        <div className="flex flex-wrap items-start gap-1.5">
          <div className="seg map-seg" role="group" aria-label="Map layers">
            {drawn.length > 0 && (
              <button aria-pressed={sec.on} onClick={() => setSec(s => ({ ...s, on: !s.on }))} title="The range's areas, as the F10 map draws them">Sectors</button>
            )}
            <button aria-pressed={show.stations} onClick={() => setShow(s => ({ ...s, stations: !s.stations }))}>Stations</button>
            <button aria-pressed={show.air} onClick={() => setShow(s => ({ ...s, air: !s.air }))}>Traffic</button>
            <button aria-pressed={show.labels} onClick={() => setShow(s => ({ ...s, labels: !s.labels }))}>Labels</button>
          </div>
          {sectorsOn && (
            <>
              <div className="seg map-seg" role="group" aria-label="Show sectors for">
                {SIDES.map(o => (
                  <button key={o.v} aria-pressed={sec.side === o.v} title={o.title} onClick={() => setSec(s => ({ ...s, side: o.v }))}>
                    {o.v !== 'all' && <span className="dot" style={{ background: o.v === 'blue' ? 'var(--sky)' : 'var(--wave)', marginRight: 5, verticalAlign: '1px' }} />}
                    {o.label}
                  </button>
                ))}
              </div>
              <div className="seg map-seg">
                <button aria-pressed={sec.key} aria-expanded={sec.key} onClick={() => setSec(s => ({ ...s, key: !s.key }))} title="What the colours mean">Key</button>
              </div>
            </>
          )}
        </div>
        {sectorsOn && sec.key && <SectorLegend drawn={shown} light={light} canHover={canHover} />}
      </div>
      <SectorTip ref={tipRef} d={hovered} light={light} />
      {pinned && <SectorCard d={pinned} light={light} onClose={closeCard} />}
    </div>
  )
}
