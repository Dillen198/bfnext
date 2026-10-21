// TACMAP: the coalition's war on one map, for every pilot on a side. It is
// the command map without the command: the same feeds, markers, layers, log
// and drawers (./groundwar/), and nothing that gives an order -- no command
// card, no order catalogue or launch panel, no asset verbs, no posture, no
// waypoints. Commanders order from /command.
//
// What it reads is coalition-locked on the server and already scoped to the
// viewer's side: /ws/groundwar (our formations in full, the enemy where we
// have seen it, battles, our pilots), /api/command for our own assets only
// (its treasury, catalogue and launch list are never shown here),
// /ws/tacmap for the enemy air and air defences we can see, and the
// warehouse read for the base drawer. The public objective list fills in
// bases the ground picture leaves out (it skips the sea, and is empty on a
// server without the ground war).
import { useCallback, useEffect, useMemo, useRef, useState, type MouseEvent, type ReactElement } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Link } from 'react-router-dom'
import Map, { type MapRef, type ViewStateChangeEvent } from 'react-map-gl/maplibre'
import type { MapLibreEvent, Map as MlMap } from 'maplibre-gl'
import 'maplibre-gl/dist/maplibre-gl.css'

import {
  api, type CaptureEvent, type CombatLogLine, type Frontlines, type GroundBattle, type GroundEvent,
  type GroundObjective, type GroundPicture, type LatLon, type Objective, type Warehouse,
} from '../api'
import { useAuth } from '../context/AuthContext'
import { useInstance } from '../context/InstanceContext'
import { useTheme } from '../context/ThemeContext'
import { BattlefieldEngine } from './groundwar/engine'
import { Drawer, EventFeed, HelpOverlay, TopHud } from './groundwar/hud'
import { attachInput, type InputHandle } from './groundwar/input'
import { BattlefieldLayers } from './groundwar/layers'
import { styleFor, type MapLook } from './groundwar/mapStyles'
import { BattleLabel, DestMarker, EnemyMarker, FormationMarker, ObjectiveMarker } from './groundwar/markers'
import { installVehicleImages } from './groundwar/sprites'
import { NEAR_ZOOM, reducedMotion, type Side } from './groundwar/theme'
import { useGroundFeed } from './groundwar/useGroundFeed'
import { visibleNames } from './groundwar/declutter'
import { AssetCard, AssetLines, AssetMarker, EnemyAirMarker, LayerToggles, type AssetNote } from './groundwar/command'
import { ALL_LAYERS, enemyAirOf, shown, useCommandFeed, useTacPicture, type Layers } from './groundwar/commandFeed'
import { Coverage, SupplyNetwork, Territory, Threats } from './groundwar/overlays'
import { BaseDrawer, ContactDrawer } from './groundwar/baseDrawer'
import { createGroundwarMock } from './groundwarMock'
import { createCommandMock } from './commandMock'
import { mockCaptures, mockObjectives, mockWarehouse } from './tacmapMock'
import './groundwar/groundwar.css'
import './groundwar/tacmap.css'

const NO_FRONTS: Frontlines = { mid: [], blue: [], red: [] }
const NO_OBJECTIVES: Objective[] = []
const NO_CAPTURES: CaptureEvent[] = []
/** Below this zoom only the selected asset keeps its tag. */
const FAR_ZOOM = 8.5

function loadPref<T>(key: string, fallback: T): T {
  try {
    const v = localStorage.getItem(key)
    return v == null ? fallback : (JSON.parse(v) as T)
  } catch {
    return fallback
  }
}
function savePref(key: string, v: unknown) {
  try {
    localStorage.setItem(key, JSON.stringify(v))
  } catch {
    /* private window: preferences just don't stick */
  }
}

/** A server without the ground war still has bases, assets and air. */
function emptyPicture(side: Side): GroundPicture {
  return {
    side, enabled: false, max_formations: 0, live: 0, max_live: 0, player_lock_secs: 0,
    formations: [], enemy: [], battles: [], objectives: [], players: [], events: [],
    time: Math.floor(Date.now() / 1000), spot_m: 0, engage_m: 0, can_command: false, god_mode: false,
  }
}

/** Small and stable, for a base the ground picture has no id for. */
function nameId(name: string): number {
  let h = 0
  for (let i = 0; i < name.length; i++) h = (Math.imul(h, 31) + name.charCodeAt(i)) | 0
  return 1_000_000 + (Math.abs(h) % 1_000_000)
}

/** The ground picture's bases plus the public list's others (at sea, or all
 *  of them with no ground war), in the picture's shape. Their numbers stay
 *  ours-only, like the engine's. */
function withAllBases(pic: GroundPicture, pub: Objective[]): GroundPicture {
  const have = new Set(pic.objectives.map((o) => o.name))
  const ids = new Set(pic.objectives.map((o) => o.id))
  const extra: GroundObjective[] = []
  for (const p of pub) {
    if (have.has(p.name) || (p.lat === 0 && p.lon === 0)) continue
    const n = Number(/\d+/.exec(p.id)?.[0])
    const id = Number.isFinite(n) && !ids.has(n) ? n : nameId(p.name)
    ids.add(id)
    const ours = p.owner === pic.side
    extra.push({
      id, name: p.name, pos: [p.lat, p.lon], owner: p.owner, kind: p.kind.toLowerCase(),
      health: ours ? p.health : null, threatened: ours ? !!p.threatened : null, can_raise: null,
      being_captured: false, supply: ours ? p.supply : null, garrison: null,
    })
  }
  return extra.length ? { ...pic, objectives: [...pic.objectives, ...extra] } : pic
}

export default function TacMapPage(): ReactElement {
  const { user } = useAuth()
  const { theme } = useTheme()
  const { selected: instance } = useInstance()
  // An admin with no side of their own picks which one to look at.
  const adminPick = !!user?.is_admin && !user?.side
  const [viewSide, setViewSide] = useState<Side>('Blue')
  const sideParam = adminPick ? viewSide : undefined
  // Dev builds only: `?mock` runs the command map's simulated picture and
  // never calls the API. Keep the guard literal so a production build drops
  // the fixtures.
  const mockParam = import.meta.env.DEV ? new URLSearchParams(location.search).get('mock') : null
  const mock = useMemo(() => (import.meta.env.DEV && mockParam != null ? createGroundwarMock(mockParam) : null), [mockParam])
  const cmdMock = useMemo(() => (mock ? createCommandMock() : null), [mock])
  useEffect(() => {
    if (!cmdMock) return
    const iv = window.setInterval(() => cmdMock.step(2), 2000)
    return () => window.clearInterval(iv)
  }, [cmdMock])

  const feed = useGroundFeed(sideParam, instance, mock)
  const { data: pubObjs = NO_OBJECTIVES, isFetched: objsFetched, isError: objsErr } = useQuery({
    queryKey: ['objectives', instance, mock ? 'mock' : ''],
    queryFn: () => (mock ? Promise.resolve(mockObjectives(mock.picture())) : api.objectives()),
    refetchInterval: 30_000,
  })
  const ownSide: Side | null = adminPick ? viewSide : user?.side ?? null
  const basePic = feed.pic ?? (feed.reason === 'disabled' && ownSide ? emptyPicture(ownSide) : null)
  const pic = useMemo(() => (basePic ? withAllBases(basePic, pubObjs) : null), [basePic, pubObjs])
  const pubByName = useMemo(() => {
    const m: Record<string, Objective> = {}
    for (const o of pubObjs) m[o.name] = o
    return m
  }, [pubObjs])

  const [layers, setLayersState] = useState<Layers>(() => loadPref('tm.layers', ALL_LAYERS))
  const setLayers = (l: Layers) => { setLayersState(l); savePref('tm.layers', l) }
  const cmdQ = useCommandFeed(sideParam, instance, cmdMock, !!pic)
  const cp = cmdQ.data ?? null
  const assets = cp?.assets ?? []
  const cmdErr = cmdQ.error instanceof Error ? cmdQ.error.message : ''
  const assetNote: AssetNote | null = mock ? null
    : cp?.stale_secs != null
      ? { top: `${cp.stale_secs}s`, bottom: 'OLD', title: 'The game server is busy and missed the last updates: our assets are shown where it last reported them.' }
      : cmdQ.isError
        ? /did not answer|not answering|not reachable|busy/i.test(cmdErr)
          ? { top: 'ENGINE', bottom: 'BUSY', title: "The game server isn't answering for our assets right now. Retrying every few seconds." }
          : { top: 'ASSETS', bottom: 'OFFLINE', title: "Our aircraft, convoys and batteries aren't on this map: this server's engine and bfdb need the update that adds them." }
        : null
  const tac = useTacPicture(!mock && !!pic && (layers.enemyAir || layers.threats !== false || layers.cover !== false))
  const enemyAir = useMemo(() => (layers.enemyAir ? enemyAirOf(tac) : []), [tac, layers.enemyAir])

  // The round's saved combat log, merged into the live feed.
  const logQ = useQuery({
    queryKey: ['command-log', sideParam, instance],
    queryFn: () => api.command.log(sideParam),
    refetchInterval: 15_000,
    enabled: !mock && !!pic,
    retry: false,
  })
  const [olderLog, setOlderLog] = useState<CombatLogLine[]>([])
  const [logEnd, setLogEnd] = useState(false)
  const [logBusy, setLogBusy] = useState(false)
  const savedLog = useMemo(() => [...(logQ.data?.lines ?? []), ...olderLog], [logQ.data, olderLog])
  const loadOlderLog = async () => {
    const oldest = savedLog[savedLog.length - 1]
    if (!oldest) return
    setLogBusy(true)
    try {
      const r = await api.command.log(sideParam, oldest.ns)
      if (r.lines.length === 0) setLogEnd(true)
      setOlderLog((o) => [...o, ...r.lines])
    } catch {
      setLogEnd(true)
    } finally {
      setLogBusy(false)
    }
  }
  const { data: fronts = NO_FRONTS } = useQuery<Frontlines>({
    queryKey: ['frontline'],
    queryFn: () => (mock ? Promise.resolve(NO_FRONTS) : api.frontline()),
    refetchInterval: 60_000,
  })
  const side: Side = pic?.side ?? 'Blue'

  // View preferences, per browser, apart from the command map's.
  const [look, setLookState] = useState<MapLook>(() => loadPref('tm.look', 'sat'))
  const [fog, setFogState] = useState<boolean>(() => loadPref('tm.fog', false))
  const [territory, setTerritoryState] = useState<boolean>(() => loadPref('tm.territory', true))
  const setLook = (l: MapLook) => { setLookState(l); savePref('tm.look', l) }
  const setFog = (b: boolean) => { setFogState(b); savePref('tm.fog', b) }
  const setTerritory = (b: boolean) => { setTerritoryState(b); savePref('tm.territory', b) }
  const mapStyle = useMemo(() => styleFor(look, theme), [look, theme])

  const [follow, setFollow] = useState(false)
  const [help, setHelp] = useState(false)
  const [near, setNear] = useState(false)
  const [far, setFar] = useState(false)
  const [sel, setSel] = useState<number[]>([])
  const [selObjId, setSelObjId] = useState<number | null>(null)
  const [selEnemyId, setSelEnemyId] = useState<number | null>(null)
  const [selAssetId, setSelAssetId] = useState<number | null>(null)
  const [names, setNames] = useState<Set<number> | null>(null)

  const mapRef = useRef<MapRef>(null)
  const boxRef = useRef<HTMLDivElement>(null)
  const inputRef = useRef<InputHandle | null>(null)
  const unhookImages = useRef<(() => void) | null>(null)
  const [engine] = useState(() => new BattlefieldEngine())

  const selIds = useMemo(() => sel.filter((id) => pic?.formations.some((f) => f.id === id)), [sel, pic])
  const selSet = useMemo(() => new Set(selIds), [selIds])
  const selForms = useMemo(() => (pic?.formations ?? []).filter((f) => selSet.has(f.id)), [pic, selSet])
  const selObj = pic?.objectives.find((o) => o.id === selObjId) ?? null
  const selEnemy = pic?.enemy.find((e) => e.id === selEnemyId) ?? null
  const selAsset = assets.find((a) => a.id === selAssetId) ?? null
  const selfPlayer = pic?.players.find((p) => p.is_self) ?? null

  // The selected base's stock and supply line, as the engine scopes it for us.
  // bfdb answers it for a pilot with a side this round, or an admin: anyone
  // else got a 401 or 403, asked again every minute and on every focus.
  const canSeeStock = !!mock || !!user?.side || !!user?.is_admin
  const whQ = useQuery<Warehouse>({
    queryKey: ['warehouse', instance, sideParam ?? '', selObj?.name ?? '', mock ? 'mock' : ''],
    queryFn: () => (mock && pic ? Promise.resolve(mockWarehouse(pic, selObj!.name)) : api.warehouse(selObj!.name)),
    enabled: !!selObj && canSeeStock,
    refetchInterval: 60_000,
    // A busy engine misses one now and then; once more after a pause.
    retry: 1,
    retryDelay: 4000,
  })
  const { data: captures = NO_CAPTURES } = useQuery({
    queryKey: ['capture-events', instance, mock ? 'mock' : ''],
    queryFn: () => (mock ? Promise.resolve(mockCaptures()) : api.captureEvents(undefined, 200)),
    enabled: !!selObj,
    staleTime: 60_000,
  })

  // ── Engine wiring ──────────────────────────────────────────────────────
  useEffect(() => {
    engine.setReducedMotion(reducedMotion())
    engine.setFollowBroken(() => setFollow(false))
    return () => {
      inputRef.current?.detach()
      unhookImages.current?.()
      engine.destroy()
    }
  }, [engine])
  useEffect(() => {
    if (pic) engine.setPicture(pic, feed.gap)
  }, [engine, pic, feed.gap])
  useEffect(() => engine.setSelection(selSet), [engine, selSet])
  useEffect(() => engine.setOptions({ fog, territory: false, follow }), [engine, fog, follow])

  // ── Camera ─────────────────────────────────────────────────────────────
  const getMap = (): MlMap | null => mapRef.current?.getMap() ?? null
  const flyTo = (p: LatLon, zoom?: number) => {
    const map = getMap()
    if (!map) return
    map.flyTo({ center: [p[1], p[0]], zoom: zoom ?? Math.max(map.getZoom(), 11), duration: reducedMotion() ? 0 : 750 })
  }
  const centre = () => {
    const p = selForms.length === 1 ? selForms[0].pos : selObj?.pos ?? selAsset?.pos ?? selEnemy?.pos
    if (p) flyTo(p)
  }

  // ── Selection ──────────────────────────────────────────────────────────
  const clearAll = () => {
    setSel([])
    setSelObjId(null)
    setSelEnemyId(null)
    setSelAssetId(null)
  }
  const selectOnly = (ids: number[]) => {
    clearAll()
    setSel(ids)
  }
  const pickFormation = (e: MouseEvent, id: number) => {
    if (inputRef.current?.wasDrag()) return
    e.stopPropagation()
    selectOnly([id])
  }
  const pickObjective = (e: MouseEvent, id: number) => {
    if (inputRef.current?.wasDrag()) return
    e.stopPropagation()
    clearAll()
    setSelObjId(id)
  }
  const pickEnemy = (e: MouseEvent, id: number) => {
    if (inputRef.current?.wasDrag()) return
    e.stopPropagation()
    clearAll()
    setSelEnemyId(id)
  }
  const pickAsset = (e: MouseEvent, id: number) => {
    if (inputRef.current?.wasDrag()) return
    e.stopPropagation()
    clearAll()
    setSelAssetId(id)
  }
  const pickBaseByName = (name: string) => {
    const o = pic?.objectives.find((x) => x.name === name)
    if (!o) return
    clearAll()
    setSelObjId(o.id)
    flyTo(o.pos)
  }
  const toggleFollow = () => {
    if (selfPlayer) setFollow((f) => !f)
  }
  const cycle = (dir: 1 | -1) => {
    const fs = [...(pic?.formations ?? [])].sort((a, b) => a.id - b.id)
    if (!fs.length) return
    const cur = selIds.length === 1 ? fs.findIndex((f) => f.id === selIds[0]) : -1
    const next = fs[(cur + dir + fs.length) % fs.length]
    selectOnly([next.id])
    flyTo(next.pos)
  }

  // ── Live state for the listeners ───────────────────────────────────────
  const live = useRef({ pic, help, follow, selObjId, pickFormation, pickObjective, pickEnemy, pickAsset, clearAll, selectOnly, centre, toggleFollow, cycle })
  useEffect(() => {
    live.current = { pic, help, follow, selObjId, pickFormation, pickObjective, pickEnemy, pickAsset, clearAll, selectOnly, centre, toggleFollow, cycle }
  })

  const declutter = useCallback(() => {
    const map = mapRef.current?.getMap()
    const L = live.current
    if (!map || !L.pic) return
    const el = map.getContainer()
    const w = el.clientWidth
    const h = el.clientHeight
    setNames(visibleNames(L.pic.objectives, (o) => {
      const p = map.project([o.pos[1], o.pos[0]])
      return p.x < -60 || p.y < -60 || p.x > w + 60 || p.y > h + 60 ? null : { x: p.x, y: p.y }
    }, L.pic.side, L.selObjId))
  }, [])
  const objKey = (pic?.objectives ?? []).map((o) => `${o.id}${o.owner[0]}${o.being_captured ? 'c' : ''}`).join()
  useEffect(() => { declutter() }, [declutter, objKey, selObjId])

  const onPickFormation = useCallback((e: MouseEvent, id: number) => live.current.pickFormation(e, id), [])
  const onPickObjective = useCallback((e: MouseEvent, id: number) => live.current.pickObjective(e, id), [])
  const onPickEnemy = useCallback((e: MouseEvent, id: number) => live.current.pickEnemy(e, id), [])
  const onPickAsset = useCallback((e: MouseEvent, id: number) => live.current.pickAsset(e, id), [])

  // Wire up as soon as the style is in, not on 'load' (that waits for every
  // first tile). The input layer is the command map's, minus the orders: a
  // click on bare map deselects, a right-click does nothing.
  const wiredTo = useRef<MlMap | null>(null)
  const onLoad = (e: MapLibreEvent) => {
    const map = e.target as MlMap
    if (wiredTo.current === map) return
    wiredTo.current = map
    unhookImages.current?.()
    unhookImages.current = installVehicleImages(map)
    engine.attach(map)
    map.on('moveend', declutter)
    declutter()
    inputRef.current?.detach()
    if (!boxRef.current) return
    inputRef.current = attachInput(map, boxRef.current, {
      pic: () => live.current.pic,
      hover: () => {},
      leave: () => {},
      click: () => live.current.clearAll(),
      context: () => {},
      box: () => {},
    })
  }
  const onZoom = (e: ViewStateChangeEvent) => {
    setNear(e.viewState.zoom >= NEAR_ZOOM)
    setFar(e.viewState.zoom < FAR_ZOOM)
  }

  // ── Hotkeys: looking around only ───────────────────────────────────────
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      const t = e.target as HTMLElement | null
      if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.tagName === 'SELECT' || t.isContentEditable)) return
      if (e.metaKey || e.ctrlKey || e.altKey) return
      const L = live.current
      if (e.key === '?' || (e.code === 'Slash' && e.shiftKey)) {
        e.preventDefault()
        setHelp((h) => !h)
        return
      }
      if (e.key === 'Escape') {
        if (L.help) setHelp(false)
        else if (L.follow) setFollow(false)
        else L.clearAll()
        return
      }
      if (L.help || !L.pic) return
      switch (e.key.toLowerCase()) {
        case 'f': L.toggleFollow(); break
        case ' ':
          if (t?.tagName === 'BUTTON') return
          e.preventDefault()
          L.centre()
          break
        case 'tab':
          e.preventDefault()
          L.cycle(e.shiftKey ? -1 : 1)
          break
        default:
          return
      }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [])

  // ── First view: wait for the base list when the picture has none ───────
  const ready = !!pic && (pic.objectives.length > 0 || pic.formations.length > 0 || objsFetched || objsErr)
  const initialView = useMemo(() => {
    const pts = [...(pic?.objectives ?? []).map((o) => o.pos), ...(pic?.formations ?? []).map((f) => f.pos)]
    if (!pts.length) return { latitude: 42, longitude: 43.5, zoom: 7 }
    const lats = pts.map((p) => p[0])
    const lons = pts.map((p) => p[1])
    return {
      bounds: [[Math.min(...lons), Math.min(...lats)], [Math.max(...lons), Math.max(...lats)]] as [[number, number], [number, number]],
      fitBoundsOptions: {
        padding: window.innerWidth > 900 ? { top: 80, bottom: 40, left: 330, right: 70 } : { top: 70, bottom: 30, left: 30, right: 30 },
      },
    }
    // Only the first picture sets the view.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [ready])

  if (!pic || !ready) {
    return <Standby reason={feed.reason} error={feed.error} link={feed.link} />
  }

  const single = selForms.length === 1 ? selForms[0] : null
  const onEvent = (e: GroundEvent) => {
    if (e.pos) flyTo(e.pos, Math.max(getMap()?.getZoom() ?? 0, 11.5))
    if (e.formation != null && pic.formations.some((f) => f.id === e.formation)) selectOnly([e.formation])
  }
  const onBattle = (b: GroundBattle) => flyTo(b.pos, 12.5)
  const shownAssets = assets.filter((a) => shown(a, layers))
  const noGroundWar = feed.reason === 'disabled'

  return (
    <div className={`gw-root tm-root${near ? ' gw-near' : ''}${far ? ' gw-far' : ''}`} style={{ ['--own' as string]: side === 'Blue' ? '#4a8fd4' : '#cc4444' }}>
      <Map
        ref={mapRef}
        mapStyle={mapStyle}
        initialViewState={initialView}
        style={{ position: 'absolute', inset: 0 }}
        dragRotate={false}
        pitchWithRotate={false}
        touchPitch={false}
        boxZoom={false}
        doubleClickZoom={false}
        maxPitch={0}
        attributionControl={false}
        onLoad={onLoad}
        onStyleData={onLoad}
        onZoom={onZoom}
      >
        <Territory objectives={pic.objectives} visible={territory} />
        <SupplyNetwork lines={cp?.supply ?? []} visible={layers.supply !== false} />
        <Coverage defences={cp?.defences ?? []} tac={tac} side={side} visible={layers.cover !== false} />
        <Threats tac={tac} side={side} visible={layers.threats !== false} />
        <BattlefieldLayers formations={pic.formations} fronts={fronts} selected={selSet} side={side} territory={territory} />
        {pic.objectives.map((o) => (
          // The +N "can raise" badge is a commander's cue; the drawer still says it.
          <ObjectiveMarker key={`o${o.id}`} o={o.can_raise ? { ...o, can_raise: null } : o} side={side} selected={o.id === selObjId}
            showName={!names || names.has(o.id)} onPick={onPickObjective} />
        ))}
        {pic.formations.map((f) => (
          <DestMarker key={`d${f.id}`} f={f} side={side} selected={selSet.has(f.id)} />
        ))}
        {pic.enemy.map((e) => (
          <EnemyMarker key={`e${e.id}`} e={e} side={side} near={near} onPick={onPickEnemy} />
        ))}
        {pic.formations.map((f) => (
          <FormationMarker key={`f${f.id}`} f={f} side={side} selected={selSet.has(f.id)} near={near} group={null}
            onPick={onPickFormation} />
        ))}
        {pic.battles.map((b) => <BattleLabel key={`b${b.id}`} b={b} onPick={onBattle} />)}
        {cp && <AssetLines assets={shownAssets} sel={selAsset} side={side} />}
        {shownAssets.map((a) => (
          <AssetMarker key={`a${a.id}`} a={a} side={side} selected={a.id === selAssetId} near={near} onPick={onPickAsset} />
        ))}
        {layers.enemyAir && enemyAir.map((t) => <EnemyAirMarker key={`h${t.id}`} t={t} side={side} />)}
      </Map>

      <div className="gw-vignette" />
      <div ref={boxRef} className="gw-box" />

      <TopHud
        pic={pic}
        title="TACMAP"
        link={feed.link}
        frameAt={feed.at}
        look={look}
        setLook={setLook}
        fog={fog}
        setFog={setFog}
        territory={territory}
        setTerritory={setTerritory}
        onHelp={() => setHelp(true)}
        adminPick={adminPick}
        viewSide={viewSide}
        setViewSide={(s) => { setViewSide(s); clearAll() }}
        tools={<Link className="tm-tool" to="/scope" title="The radar scope: raw contacts, BRAA ruler, bullseye and the GCI seat">SCOPE</Link>}
      />
      <div className="gw-topstack">
        {feed.reason === 'unavailable' && (
          <div className="gw-banner bad">The game server stopped answering. This is the last picture it sent.</div>
        )}
        {noGroundWar && (
          <div className="gw-banner">No ground war on this server: bases, our assets and the air picture only.</div>
        )}
        {follow && <div className="gw-modebar follow">FOLLOWING {selfPlayer?.name.toUpperCase() ?? 'YOU'} · DRAG OR F TO STOP</div>}
      </div>

      <EventFeed events={pic.events} saved={savedLog} time={pic.time} onPick={onEvent}
        onOlder={() => void loadOlderLog()} hasOlder={!logEnd && savedLog.length >= 200} olderBusy={logBusy} />
      {single && (
        <Drawer f={single} side={side} pic={pic} lockMins={Math.round(pic.player_lock_secs / 60)} onClose={clearAll} />
      )}
      {selAsset && (
        <AssetCard a={selAsset} onClose={clearAll}>
          {selAsset.dest && <p className="gw-drawer-note">The dashed line is where it is headed.</p>}
        </AssetCard>
      )}
      {selObj && (
        <BaseDrawer
          o={selObj}
          pub={pubByName[selObj.name] ?? null}
          side={side}
          pic={pic}
          assets={assets}
          captures={captures}
          intel={{
            w: whQ.data ?? null,
            loading: whQ.isLoading,
            error: !canSeeStock
              ? (user ? 'they are your coalition’s intel. Register a side on the server to see them.' : 'they are your coalition’s intel. Sign in to see them.')
              : whQ.error instanceof Error ? whQ.error.message : null,
          }}
          onClose={clearAll}
          onPickBase={pickBaseByName}
          onPickFormation={(id) => {
            selectOnly([id])
            const f = pic.formations.find((x) => x.id === id)
            if (f) flyTo(f.pos)
          }}
        />
      )}
      {selEnemy && <ContactDrawer e={selEnemy} side={side} onClose={clearAll} />}
      <LayerToggles layers={layers} set={setLayers} note={assetNote} />
      {help && <HelpOverlay onClose={() => setHelp(false)} canCommand={false} readOnly />}
    </div>
  )
}

function Standby({ reason, error, link }: { reason: string | null; error: string | null; link: string }): ReactElement {
  let title = 'ESTABLISHING LINK'
  let body = 'Waiting for the first picture from the game server.'
  // Without the socket the reason only comes back as the poll's error text.
  if (!reason && error && /not logged in|session expired/i.test(error)) reason = 'login'
  else if (!reason && error && /no coalition/i.test(error)) reason = 'nocoalition'
  if (reason === 'login') {
    title = 'SIGN IN FOR THE TACMAP'
    body = "The tactical map shows your coalition's picture. Sign in with Discord to see your side's war."
  } else if (reason === 'nocoalition') {
    title = 'NO COALITION'
    body = "The tactical map is locked to your coalition, and the server can't tell which side you're on. Link your Discord (-linkme in DCS chat) and take a slot this campaign, then reload."
  } else if (reason === 'unavailable') {
    title = 'GAME SERVER NOT ANSWERING'
    body = 'bfdb is up, but the engine is not answering. It may be restarting; this page reconnects by itself.'
  } else if (error && link === 'offline') {
    title = 'TACMAP UNAVAILABLE'
    body = error
  }
  return (
    <div className="gw-root gw-standby">
      <div>
        <div className="gw-standby-pulse" />
        <h1>{title}</h1>
        <p>{body}</p>
      </div>
    </div>
  )
}
