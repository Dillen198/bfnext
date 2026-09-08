// Ported near-verbatim from peace-eye (pbzweihander/peace-eye, MIT — see
// LICENSE.peace-eye). The Tacview desktop plumbing is replaced by
// `useScopeFeed`, which bridges the engine's fog-of-war `/ws/tacmap` feed
// into the same TacviewState shape peace-eye's components expect.
import circle from '@turf/circle'
import type { Feature, FeatureCollection } from 'geojson'
import geomagnetism from 'geomagnetism'
import { useMemo, useState, type ReactElement } from 'react'
import { useQuery } from '@tanstack/react-query'
import Map, { AttributionControl, Layer, Marker, Source } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'
import './scope.css'

import { api, type Frontlines } from '../api'
import AirportMarker from './AirportMarker'
import BraaInfo from './BraaInfo'
import ControlPanel from './ControlPanel'
import CursorInfo from './CursorInfo'
import ObjectInfo from './ObjectInfo'
import ObjectMarker from './ObjectMarker'
import SettingsModal from './SettingsModal'
import Spinner from './Spinner'
import { colorMode, filterObject } from './entity'
import {
  defaultObjectSettings,
  type ObjectSettingsInventory,
} from './objectSettings'
import { defaultSettings } from './settings'
import { type TacviewObject } from './tacview'
import { useScopeFeed } from './useScopeFeed'
import { moveCoords, nmToMeter } from './util'

// Inline raster style off the same Esri dark-canvas tiles /map uses — a
// hosted vector style (CARTO) is blocked by the dashboard's CSP in prod and
// renders black.
const MAP_STYLE = {
  version: 8 as const,
  sources: {
    esri: {
      type: 'raster' as const,
      tiles: [
        'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}',
      ],
      tileSize: 256,
      attribution: 'Esri',
    },
  },
  layers: [
    { id: 'bg', type: 'background' as const, paint: { 'background-color': '#0a0d07' } },
    { id: 'esri', type: 'raster' as const, source: 'esri', paint: { 'raster-opacity': 0.85 } },
  ],
}

const OBJ_COLOR = (owner: string) =>
  owner === 'Blue' ? '#4a8fd4' : owner === 'Red' ? '#cc4444' : '#6a7a5a'

export default function ScopePage(): ReactElement {
  const { state, terrain, denied, reason, status, threatRanges, empty } = useScopeFeed()

  // Campaign context layers (public REST, same as the /map page).
  const { data: objectives = [] } = useQuery({
    queryKey: ['objectives'],
    queryFn: () => api.objectives(),
    refetchInterval: 30_000,
  })
  const { data: fronts = { mid: [], blue: [], red: [] } } = useQuery<Frontlines>({
    queryKey: ['frontline'],
    queryFn: () => api.frontline(),
    refetchInterval: 30_000,
  })

  const [settings, setSettings] = useState(defaultSettings())
  const [objectSettingsInventory, setObjectSettingsInventory] =
    useState<ObjectSettingsInventory>({})
  const [cursorCoords, setCursorCoords] = useState<[number, number]>([0, 0])
  const [selectedObjectId, setSelectedObjectId] = useState<number | undefined>(undefined)
  const [selectedAirportIndex, setSelectedAirportIndex] = useState<number | undefined>(undefined)
  const [rulerStartCoords, setRulerStartCoords] = useState<[number, number] | undefined>(undefined)

  const geomagnetismModel = useMemo(() => {
    // The published WMM epoch peace-eye ships expires; pin a recent one.
    try { return geomagnetism.model(new Date(2024, 5, 1)) }
    catch { try { return geomagnetism.model() } catch { return { point: () => ({ decl: 0 }) } } }
  }, [])

  const referenceLatitude = state.globalProperties.referenceLatitude
  const referenceLongitude = state.globalProperties.referenceLongitude

  const ownedBullseye = state.blueBullseye
  const bullseyeCoords: [number, number] | undefined =
    referenceLatitude !== undefined &&
    referenceLongitude !== undefined &&
    ownedBullseye?.coords?.latitude !== undefined &&
    ownedBullseye?.coords?.longitude !== undefined
      ? [
          referenceLatitude + ownedBullseye.coords.latitude,
          referenceLongitude + ownedBullseye.coords.longitude,
        ]
      : undefined

  const rulerGeoJson: FeatureCollection = useMemo(() => {
    if (rulerStartCoords === undefined) return { type: 'FeatureCollection', features: [] }
    return {
      type: 'FeatureCollection',
      features: [
        {
          type: 'Feature',
          properties: {},
          geometry: {
            type: 'LineString',
            coordinates: [
              [rulerStartCoords[1], rulerStartCoords[0]],
              [cursorCoords[1], cursorCoords[0]],
            ],
          },
        },
      ],
    }
  }, [rulerStartCoords, cursorCoords])

  const trackVectorLineGeoJson: FeatureCollection = useMemo(() => {
    if (referenceLatitude === undefined || referenceLongitude === undefined)
      return { type: 'FeatureCollection', features: [] }
    return {
      type: 'FeatureCollection',
      features: Object.values(state.objects)
        .filter(
          (object) =>
            filterObject(object, settings) &&
            (object.type?.includes('Air') === true || object.type?.includes('Missile') === true) &&
            object.coords?.latitude !== undefined &&
            object.coords?.longitude !== undefined &&
            object.coords?.heading !== undefined,
        )
        .map((object) => {
          const endCoords = moveCoords(
            referenceLatitude + object.coords!.latitude!,
            referenceLongitude + object.coords!.longitude!,
            object.coords!.heading!,
            object.estimatedSpeed * 0.514444 * 60,
          )
          return {
            type: 'Feature',
            properties: { coalition: object.coalition },
            geometry: {
              type: 'LineString',
              coordinates: [
                [
                  referenceLongitude + object.coords!.longitude!,
                  referenceLatitude + object.coords!.latitude!,
                ],
                [endCoords[1], endCoords[0]],
              ],
            },
          }
        }),
    }
  }, [state.objects, settings, referenceLatitude, referenceLongitude])

  const rangeGeoJson: FeatureCollection = useMemo((): FeatureCollection => {
    if (referenceLatitude === undefined || referenceLongitude === undefined)
      return { type: 'FeatureCollection', features: [] }

    const feats: Feature[] = []
    const seen = new Set<number>()

    const ringsAt = (
      coords: [number, number],
      warnNm: number,
      threatNm: number,
    ) => {
      if (warnNm > 0) {
        feats.push(
          circle(coords, nmToMeter(warnNm) / 1000, {
            steps: 64,
            units: 'kilometers',
            properties: { type: 'warning' },
          }),
        )
      }
      if (threatNm > 0) {
        feats.push(
          circle(coords, nmToMeter(threatNm) / 1000, {
            steps: 64,
            units: 'kilometers',
            properties: { type: 'threat' },
          }),
        )
      }
    }

    // manual per-contact WR/TR overrides
    for (const [id, os] of Object.entries(objectSettingsInventory)) {
      const nid = Number(id)
      const o = state.objects[nid]
      if (o?.coords?.latitude === undefined || o?.coords?.longitude === undefined) continue
      seen.add(nid)
      ringsAt(
        [referenceLongitude + o.coords.longitude, referenceLatitude + o.coords.latitude],
        os.warningRange,
        os.threatRange,
      )
    }
    // engine-identified SAM threat ranges (auto)
    for (const [id, m] of Object.entries(threatRanges)) {
      const nid = Number(id)
      if (seen.has(nid)) continue
      const o = state.objects[nid]
      if (o?.coords?.latitude === undefined || o?.coords?.longitude === undefined) continue
      const tr = m / 1852
      ringsAt(
        [referenceLongitude + o.coords.longitude, referenceLatitude + o.coords.latitude],
        tr * 1.3,
        tr,
      )
    }

    return { type: 'FeatureCollection', features: feats }
  }, [state.objects, objectSettingsInventory, threatRanges, referenceLatitude, referenceLongitude])

  // Frontline: blue-dominance / no-man's-land / red-dominance polylines
  // ([lat,lon] from the engine -> [lon,lat] for maplibre).
  const frontlineGeoJson: FeatureCollection = useMemo(() => {
    const line = (l: [number, number][], k: string): Feature => ({
      type: 'Feature',
      properties: { k },
      geometry: { type: 'LineString', coordinates: l.map(([lat, lon]) => [lon, lat]) },
    })
    return {
      type: 'FeatureCollection',
      features: [
        ...fronts.blue.filter((l) => l.length > 1).map((l) => line(l, 'blue')),
        ...fronts.red.filter((l) => l.length > 1).map((l) => line(l, 'red')),
        ...fronts.mid.filter((l) => l.length > 1).map((l) => line(l, 'mid')),
      ],
    }
  }, [fronts])

  const shownObjectives = useMemo(
    () => objectives.filter((o) => o.lat !== 0 || o.lon !== 0),
    [objectives],
  )

  const watchingObjects = useMemo(() => {
    return Object.entries(objectSettingsInventory)
      .map(([id, os]): [number, TacviewObject | undefined] => {
        const nid = Number(id)
        return os.watch ? [nid, state.objects[nid]] : [nid, undefined]
      })
      .filter(([, object]) => object !== undefined)
      .map(([id, object]): [number, TacviewObject] => [id, object!])
  }, [state.objects, objectSettingsInventory])

  if (denied) {
    return (
      <div className="scope-root theme-locked-dark flex h-full flex-col items-center justify-center gap-2 p-6 text-center text-sm text-slate-300" style={{ background: 'var(--bg)' }}>
        <div className="text-lg tracking-wide text-white" style={{ fontFamily: 'var(--font-display)', letterSpacing: '0.14em' }}>
          {reason === 'nocoalition' ? 'NO COALITION' : 'NOT SIGNED IN'}
        </div>
        <div className="max-w-md leading-relaxed">
          {reason === 'nocoalition'
            ? 'You are signed in but the dashboard can’t resolve your coalition. Link Discord (-linkme in DCS chat) and register a side this campaign.'
            : 'Sign in and register a coalition to see the live scope. Fog of war is enforced server-side — you only ever see what your side’s sensors detect.'}
        </div>
      </div>
    )
  }

  // Only hold the loading screen while we genuinely have no map reference at
  // all. useScopeFeed resolves a theatre from a contact, the bullseye, or a
  // campaign objective, so this normally clears the moment /api/objectives
  // returns.
  if (referenceLatitude === undefined || referenceLongitude === undefined || terrain === undefined) {
    return (
      <div className="scope-root theme-locked-dark flex h-full flex-col items-center justify-center gap-3" style={{ background: 'var(--bg)' }}>
        <Spinner />
        <div className="text-xs text-slate-400">
          {status === 'open' ? 'Waiting for the tactical picture…' : 'Connecting…'}
        </div>
      </div>
    )
  }

  const initialViewState = {
    latitude: terrain.center[0],
    longitude: terrain.center[1],
    zoom: 6,
  }

  let selectedEntity: TacviewObject | undefined
  let isObjectSelected = false
  if (selectedObjectId !== undefined) {
    selectedEntity = state.objects[selectedObjectId]
    isObjectSelected = true
  } else if (selectedAirportIndex !== undefined) {
    const airport = terrain.airports[selectedAirportIndex]
    selectedEntity = {
      estimatedSpeed: 0,
      estimatedAltitudeRate: 0,
      coords: {
        latitude: airport.position[0] - referenceLatitude,
        longitude: airport.position[1] - referenceLongitude,
      },
      name: airport.name,
    }
  }

  return (
    <div className="scope-root theme-locked-dark relative h-full w-full">
      <Map
        mapStyle={MAP_STYLE}
        style={{ width: '100%', height: '100%' }}
        initialViewState={initialViewState}
        doubleClickZoom={false}
        dragRotate={false}
        keyboard={false}
        attributionControl={false}
        onMouseMove={(e) => setCursorCoords([e.lngLat.lat, e.lngLat.lng])}
        onMouseDown={(e) => {
          if (e.originalEvent.button === 2) {
            e.originalEvent.preventDefault()
            setRulerStartCoords([e.lngLat.lat, e.lngLat.lng])
          }
        }}
        onMouseUp={(e) => {
          if (e.originalEvent.button === 2) {
            e.originalEvent.preventDefault()
            setRulerStartCoords(undefined)
          }
        }}
        onContextMenu={(e) => e.preventDefault()}
      >
        <AttributionControl position="bottom-left" />
        <div className="absolute left-0 top-0 m-2">
          {selectedEntity !== undefined && (
            <ObjectInfo
              object={selectedEntity}
              referenceLatitude={referenceLatitude}
              referenceLongitude={referenceLongitude}
              bullseyeCoords={bullseyeCoords}
              onClose={() => {
                setSelectedObjectId(undefined)
                setSelectedAirportIndex(undefined)
              }}
              objectSettings={
                isObjectSelected
                  ? objectSettingsInventory[selectedObjectId!] ?? defaultObjectSettings()
                  : undefined
              }
              setObjectSettings={
                isObjectSelected
                  ? (os) =>
                      setObjectSettingsInventory((inv) => ({ ...inv, [selectedObjectId!]: os }))
                  : undefined
              }
              terrain={terrain}
              geomagnetismModel={geomagnetismModel}
              useMagneticHeading={settings.view.useMagneticHeading}
            />
          )}
        </div>
        <div className="absolute right-0 top-0 m-2">
          <ControlPanel
            objects={Object.entries(state.objects).map(([id, object]) => [Number(id), object])}
            watchingObjects={watchingObjects}
            onObjectClick={(id) => {
              setSelectedObjectId(id)
              setSelectedAirportIndex(undefined)
            }}
          />
        </div>
        {empty && (
          <div
            className="pointer-events-none absolute left-1/2 top-3 -translate-x-1/2 rounded-sm px-3 py-1.5 text-center text-xs"
            style={{
              background: 'color-mix(in srgb, var(--bg-card) 92%, transparent)',
              border: '1px solid var(--accent-border)',
              color: 'var(--text-muted)',
              fontFamily: 'var(--font-mono)',
            }}
          >
            No sensor contacts in the picture · EWR / AWACS / JTAC / recon only
            {state.globalProperties.referenceTime ? '' : ' · engine feed unavailable'}
          </div>
        )}
        <CursorInfo
          cursorCoords={cursorCoords}
          bullseyeCoords={bullseyeCoords}
          terrain={terrain}
          geomagnetismModel={geomagnetismModel}
          useMagneticHeading={settings.view.useMagneticHeading}
          showCursorCoords={settings.view.showCursorCoords}
        />
        <Source id="ruler" type="geojson" data={rulerGeoJson}>
          <Layer id="ruler" type="line" paint={{ 'line-width': 2, 'line-color': '#ffff00' }} />
        </Source>
        <Source id="track-vector-line" type="geojson" data={trackVectorLineGeoJson}>
          <Layer
            id="track-vector-line"
            type="line"
            paint={{
              'line-width': 1,
              'line-color': [
                'match',
                ['get', 'coalition'],
                'Enemies', colorMode.Friend,
                'Allies', colorMode.Hostile,
                colorMode.Neutral,
              ],
            }}
          />
        </Source>
        <Source id="range-circle" type="geojson" data={rangeGeoJson}>
          <Layer
            id="range-circle"
            type="line"
            paint={{
              'line-opacity': 0.75,
              'line-width': 1,
              'line-color': [
                'match',
                ['get', 'type'],
                'warning', '#fbbd23',
                'threat', '#f87272',
                colorMode.Neutral,
              ],
            }}
          />
        </Source>
        {rulerStartCoords !== undefined && (
          <BraaInfo
            start={rulerStartCoords}
            end={cursorCoords}
            terrain={terrain}
            geomagnetismModel={geomagnetismModel}
            useMagneticHeading={settings.view.useMagneticHeading}
          />
        )}

        {/* ── Campaign frontline ─────────────────────────────────── */}
        {settings.view.showFrontline && (
          <Source id="frontline" type="geojson" data={frontlineGeoJson}>
            <Layer
              id="frontline-edges"
              type="line"
              filter={['!=', ['get', 'k'], 'mid']}
              paint={{
                'line-width': 2,
                'line-opacity': 0.8,
                'line-dasharray': [6, 4],
                'line-color': ['match', ['get', 'k'], 'blue', '#2f7dff', 'red', '#ff3b3b', '#ffffff'],
              }}
            />
            <Layer
              id="frontline-mid"
              type="line"
              filter={['==', ['get', 'k'], 'mid']}
              paint={{ 'line-width': 1.5, 'line-opacity': 0.9, 'line-dasharray': [2, 3], 'line-color': '#ffffff' }}
            />
          </Source>
        )}

        {/* ── Campaign objectives ────────────────────────────────── */}
        {settings.view.showObjectives &&
          shownObjectives.map((o) => (
            <Marker key={o.id} latitude={o.lat} longitude={o.lon} anchor="center">
              <div className="pointer-events-none flex flex-col items-center" style={{ opacity: o.health <= 0 ? 0.4 : 1 }}>
                <div
                  style={{
                    width: 9,
                    height: 9,
                    borderRadius: '50%',
                    background: OBJ_COLOR(o.owner),
                    border: '1.5px solid rgba(0,0,0,0.7)',
                  }}
                />
                <div
                  style={{
                    marginTop: 2,
                    fontFamily: 'var(--font-mono)',
                    fontSize: 9,
                    color: 'var(--text-muted)',
                    textShadow: '0 0 3px #000, 0 0 3px #000',
                    whiteSpace: 'nowrap',
                  }}
                >
                  {o.name}
                </div>
              </div>
            </Marker>
          ))}

        {settings.view.showAirports &&
          terrain.airports.map((airport, idx) => (
            <AirportMarker
              key={airport.name}
              airport={airport}
              selected={selectedAirportIndex === idx}
              onClick={() => {
                setSelectedObjectId(undefined)
                setSelectedAirportIndex(idx)
              }}
            />
          ))}
        {Object.entries(state.objects)
          .filter(([id, object]) => selectedObjectId === Number(id) || filterObject(object, settings))
          .map(([id, object]) => (
            <ObjectMarker
              key={id}
              object={object}
              referenceLatitude={referenceLatitude}
              referenceLongitude={referenceLongitude}
              selected={selectedObjectId === Number(id)}
              onClick={() => {
                setSelectedObjectId(Number(id))
                setSelectedAirportIndex(undefined)
              }}
            />
          ))}
      </Map>
      <SettingsModal
        settings={settings}
        setSettings={(s) => setSettings({ ...s })}
      />
    </div>
  )
}
