// GROUND COMMAND: the coalition's ground war as an RTS table. The picture is
// built by the engine for the viewer's own registered coalition only -- our
// formations in full (every vehicle where it really is), the enemy only where
// we have seen it, the battles, and our pilots live in DCS -- and streamed on
// /ws/groundwar (polling /api/groundwar when the socket can't be had). Orders
// go to /api/groundwar/command and are checked again by the engine against
// the side the viewer's own pilot is on. Fog of war is the server's job; this
// page draws exactly what the picture contains.
//
// The pieces live in ./groundwar/: the feed hook, the per-frame engine
// (vehicles, players, battle effects), the map layers and markers, the HUD
// and the mouse controller.
import { useCallback, useEffect, useMemo, useRef, useState, type MouseEvent, type ReactElement } from 'react'
import { useQuery } from '@tanstack/react-query'
import Map, { type MapRef, type ViewStateChangeEvent } from 'react-map-gl/maplibre'
import type { MapLibreEvent, Map as MlMap } from 'maplibre-gl'
import 'maplibre-gl/dist/maplibre-gl.css'
import { Aircraft, ChevronsLeft, Defend, Info, Pin, Plus, Shield, Strike, X } from '@icons'

import { api, type CommandMe, type CommandOrder, type Frontlines, type GroundBattle, type GroundCommand, type GroundEvent, type GroundObjective, type GroundPicture, type LatLon } from '../api'
import { useAuth } from '../context/AuthContext'
import { useInstance } from '../context/InstanceContext'
import { useTheme } from '../context/ThemeContext'
import { BattlefieldEngine, type Verb, VERB_COLOR } from './groundwar/engine'
import { km } from './groundwar/geo'
import { CommandBar, Drawer, EventFeed, HelpOverlay, TopHud, Toasts, type CmdButton, type Toast } from './groundwar/hud'
import { attachInput, type InputHandle } from './groundwar/input'
import { BattlefieldLayers } from './groundwar/layers'
import { styleFor, type MapLook } from './groundwar/mapStyles'
import { BattleLabel, DestMarker, EnemyMarker, FormationMarker, ObjectiveMarker } from './groundwar/markers'
import { installVehicleImages } from './groundwar/sprites'
import { NEAR_ZOOM, reducedMotion, type Side } from './groundwar/theme'
import { useGroundFeed } from './groundwar/useGroundFeed'
import { createGroundwarMock } from './groundwarMock'
import { createCommandMock } from './commandMock'
import { visibleNames } from './groundwar/declutter'
import { AssetLines, AssetMarker, AssetPanel, BaseOrders, EnemyAirMarker, LaunchPanel, LayerToggles } from './groundwar/command'
import { ALL_LAYERS, MODE_TEXT, shown, useCommandFeed, useEnemyAir, type AssetMode, type Layers } from './groundwar/commandFeed'
import { rankFor } from '../ranks'
import './groundwar/groundwar.css'

type Mode = 'none' | 'attack' | 'defend' | 'raise'

const NO_FRONTS: Frontlines = { mid: [], blue: [], red: [] }
/** Below this zoom only the selected asset keeps its tag. */
const FAR_ZOOM = 8.5
let toastSeq = 0

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

/** What clicking this base would do with the current selection and mode. */
function resolveOrder(
  pic: GroundPicture | null, mode: Mode, hasSel: boolean, objId: number | null,
): { verb: Verb; text: string } | null {
  if (!pic) return null
  const o = objId != null ? pic.objectives.find((x) => x.id === objId) : undefined
  const side = pic.side
  if (!o) {
    if (mode === 'attack') return { verb: 'invalid', text: 'ATTACK · CLICK AN ENEMY BASE' }
    if (mode === 'defend') return { verb: 'invalid', text: 'DEFEND · CLICK ONE OF OUR BASES' }
    if (mode === 'raise') return { verb: 'invalid', text: 'RAISE · CLICK ONE OF OUR BASES' }
    return null
  }
  const name = o.name.toUpperCase()
  const ours = o.owner === side
  if (mode === 'raise') {
    return ours && (o.can_raise ?? 0) > 0
      ? { verb: 'raise', text: `RAISE AT ${name}` }
      : { verb: 'invalid', text: ours ? `${name} CAN'T SPARE TROOPS` : `NOT OURS · CAN'T RAISE AT ${name}` }
  }
  if (!hasSel) return null
  if (mode === 'attack') return ours ? { verb: 'invalid', text: `${name} IS OURS · PRESS D TO DEFEND` } : { verb: 'attack', text: `ATTACK ${name}` }
  if (mode === 'defend') return ours ? { verb: 'defend', text: `DEFEND ${name}` } : { verb: 'invalid', text: `${name} ISN'T OURS · PRESS A TO ATTACK` }
  return ours ? { verb: 'defend', text: `MOVE · DEFEND ${name}` } : { verb: 'attack', text: `ATTACK ${name}` }
}

export default function GroundWarPage(): ReactElement {
  const { user } = useAuth()
  const { theme } = useTheme()
  const { selected: instance } = useInstance()
  // An admin with no side of their own picks which one to look at.
  const adminPick = !!user?.is_admin && !user?.side
  const [viewSide, setViewSide] = useState<Side>('Blue')
  const sideParam = adminPick ? viewSide : undefined
  // Dev builds only: `?mock` (or `?mock=view`, `?mock=god`) runs a simulated
  // picture and never calls the API. Keep the guard literal so a production
  // build drops the fixture.
  const mockParam = import.meta.env.DEV ? new URLSearchParams(location.search).get('mock') : null
  const mock = useMemo(() => (import.meta.env.DEV && mockParam != null ? createGroundwarMock(mockParam) : null), [mockParam])

  const feed = useGroundFeed(sideParam, instance, mock)
  const pic = feed.pic
  const cmdMock = useMemo(() => (mock ? createCommandMock() : null), [mock])
  useEffect(() => {
    if (!cmdMock) return
    const iv = window.setInterval(() => cmdMock.step(2), 2000)
    return () => window.clearInterval(iv)
  }, [cmdMock])
  const [layers, setLayersState] = useState<Layers>(() => loadPref('cm.layers', ALL_LAYERS))
  const setLayers = (l: Layers) => { setLayersState(l); savePref('cm.layers', l) }
  const cmdQ = useCommandFeed(sideParam, instance, cmdMock, !!pic?.enabled)
  const cp = cmdQ.data ?? null
  const enemyAir = useEnemyAir(!mock && layers.enemyAir && !!pic?.enabled)
  const { data: fronts = NO_FRONTS } = useQuery<Frontlines>({
    queryKey: ['frontline'],
    queryFn: () => (mock ? Promise.resolve(NO_FRONTS) : api.frontline()),
    refetchInterval: 60_000,
  })
  const side: Side = pic?.side ?? 'Blue'
  // What stands between this viewer and command: rank, or an admin's say-so.
  const { data: me } = useQuery<CommandMe>({
    queryKey: ['command-me', instance],
    queryFn: api.command.me,
    enabled: !mock && !!pic && !pic.can_command && !pic.god_mode,
    staleTime: 60_000,
    retry: false,
  })
  const notCommander = notCommanderText(me)

  // View preferences, per browser.
  const [look, setLookState] = useState<MapLook>(() => loadPref('gw.look', 'sat'))
  const [fog, setFogState] = useState<boolean>(() => loadPref('gw.fog', false))
  const [territory, setTerritoryState] = useState<boolean>(() => loadPref('gw.territory', true))
  const setLook = (l: MapLook) => { setLookState(l); savePref('gw.look', l) }
  const setFog = (b: boolean) => { setFogState(b); savePref('gw.fog', b) }
  const setTerritory = (b: boolean) => { setTerritoryState(b); savePref('gw.territory', b) }
  const mapStyle = useMemo(() => styleFor(look, theme), [look, theme])

  const [follow, setFollow] = useState(false)
  const [help, setHelp] = useState(false)
  const [near, setNear] = useState(false)
  // Zoomed out far enough that asset tags would only clutter.
  const [far, setFar] = useState(false)
  const [sel, setSel] = useState<number[]>([])
  const [selObjId, setSelObjId] = useState<number | null>(null)
  const [selEnemyId, setSelEnemyId] = useState<number | null>(null)
  const [selAssetId, setSelAssetId] = useState<number | null>(null)
  const [assetMode, setAssetMode] = useState<AssetMode>('none')
  const [showLaunch, setShowLaunch] = useState(false)
  const [busy, setBusy] = useState(false)
  // Base names that fit on screen right now (`declutter`); null = all.
  const [names, setNames] = useState<Set<number> | null>(null)
  const [mode, setMode] = useState<Mode>('none')
  const [groups, setGroups] = useState<Record<number, number[]>>(() => loadPref('gw.groups', {}))
  const [toasts, setToasts] = useState<Toast[]>([])
  const [cursor, setCursor] = useState<{ text: string; color: string } | null>(null)

  const mapRef = useRef<MapRef>(null)
  const boxRef = useRef<HTMLDivElement>(null)
  const cursorRef = useRef<HTMLDivElement>(null)
  const inputRef = useRef<InputHandle | null>(null)
  const unhookImages = useRef<(() => void) | null>(null)
  const lastDigit = useRef<{ n: number; t: number } | null>(null)
  const [engine] = useState(() => new BattlefieldEngine())

  // What is selected, limited to what still exists.
  const selIds = useMemo(() => sel.filter((id) => pic?.formations.some((f) => f.id === id)), [sel, pic])
  const selSet = useMemo(() => new Set(selIds), [selIds])
  const selForms = useMemo(() => (pic?.formations ?? []).filter((f) => selSet.has(f.id)), [pic, selSet])
  const selObj = pic?.objectives.find((o) => o.id === selObjId) ?? null
  const selEnemy = pic?.enemy.find((e) => e.id === selEnemyId) ?? null
  const selAsset = cp?.assets.find((a) => a.id === selAssetId) ?? null
  const canOrder = !!cp?.can_command || !!(mock && pic?.can_command)
  const groupOf = useMemo(() => {
    const m: Record<number, number> = {}
    for (const [k, ids] of Object.entries(groups)) for (const id of ids) m[id] = Number(k)
    return m
  }, [groups])
  const selfPlayer = pic?.players.find((p) => p.is_self) ?? null

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
  useEffect(() => engine.setOptions({ fog, territory, follow }), [engine, fog, territory, follow])

  // ── Toasts and orders ──────────────────────────────────────────────────
  const toast = useCallback((text: string, ok: boolean) => {
    const id = ++toastSeq
    setToasts((t) => [...t.slice(-4), { id, ok, text }])
    window.setTimeout(() => setToasts((t) => t.filter((x) => x.id !== id)), ok ? 5000 : 7000)
  }, [])

  const lockReason = !pic || pic.can_command
    ? null
    : pic.god_mode ? 'ADMIN VIEW · ORDERS NEED A PILOT ON A SIDE' : notCommander ? 'VIEW ONLY · COMMANDERS GIVE ORDERS' : 'VIEW ONLY'

  const viewOnly = () =>
    toast(pic?.god_mode
      ? 'Admin view: you can watch either side, but orders need a pilot registered on one.'
      : notCommander ?? 'View only: link your Discord (-linkme in DCS chat) and take a slot this campaign to give orders.', false)
  const issue = async (cmds: GroundCommand[]) => {
    if (!pic || !cmds.length) return
    if (!pic.can_command) return viewOnly()
    const replies = await Promise.all(cmds.map((c) =>
      (mock ? Promise.resolve(mock.command(c)) : api.groundwar.command(c))
        .then((r) => ({ ok: r.ok, text: r.message }))
        .catch((e: Error) => ({ ok: false, text: e.message })),
    ))
    for (const r of replies) toast(r.text, r.ok)
    feed.refresh()
  }

  /** A command-map order: the engine checks it and says what happened. */
  const order = async (o: CommandOrder, confirm?: string) => {
    if (!canOrder) return viewOnly()
    if (confirm && !window.confirm(confirm)) return
    setBusy(true)
    try {
      const r = cmdMock ? cmdMock.order(o) : await api.command.order(o, sideParam)
      toast(r.message, r.ok)
    } catch (e) {
      toast((e as Error).message, false)
    } finally {
      setBusy(false)
      void cmdQ.refetch()
      feed.refresh()
    }
  }
  /** Carry out the armed command-map order at `at`. */
  const applyAssetMode = (at: LatLon) => {
    const a = selAsset
    const m = assetMode
    setAssetMode('none')
    if (m === 'barrage') return void order({ barrage: { at } }, 'Every battery of ours in range fires on this point. Pay for a barrage from the treasury?')
    if (m === 'fmove') {
      if (!selIds.length) return toast('Select formations first.', false)
      void Promise.all(selIds.map((id) => order({ move_formation: { formation: id, to: at } })))
      return
    }
    if (!a) return toast('Select one of our assets first.', false)
    if (m === 'move') return void order({ move: { group: a.id, to: at } })
    if (m === 'fire') return void order({ fire: { group: a.id, at } })
    if (m === 'station') return void order({ station: { group: a.id, at } })
    if (m === 'sail') return void order({ sail: { group: a.id, to: at } })
  }
  /** Right-click with an asset selected: its main order at that point. */
  const assetDefault = (at: LatLon): boolean => {
    const a = selAsset
    if (!a) return false
    const v = a.orders.find((x) => x === 'station' || x === 'move' || x === 'sail' || x === 'fire')
    if (!v) {
      toast(`${a.name} runs itself: nothing to order.`, false)
      return true
    }
    if (v === 'move') void order({ move: { group: a.id, to: at } })
    if (v === 'station') void order({ station: { group: a.id, at } })
    if (v === 'sail') void order({ sail: { group: a.id, to: at } })
    if (v === 'fire') void order({ fire: { group: a.id, at } })
    return true
  }
  const armAsset = (m: AssetMode) => {
    if (!canOrder) return viewOnly()
    if (m === 'fmove' && !selIds.length) return toast('Select formations first, then M and click the map.', false)
    setMode('none')
    setAssetMode((cur) => (cur === m ? 'none' : m))
  }
  const assetKey = (verb: 'move' | 'fire' | 'station' | 'rtb' | 'sail'): boolean => {
    const a = selAsset
    if (!a) return false
    if (!a.orders.includes(verb)) {
      toast(`${a.name} can't ${verb === 'rtb' ? 'return to base' : verb}.`, false)
      return true
    }
    if (verb === 'rtb') void order({ rtb: { group: a.id } })
    else armAsset(verb)
    return true
  }
  const pickAsset = (e: MouseEvent, id: number) => {
    if (inputRef.current?.wasDrag()) return
    e.stopPropagation()
    setSelAssetId(id)
    setSel([])
    setSelObjId(null)
    setSelEnemyId(null)
    setAssetMode('none')
  }

  const ownObjs = (p: GroundPicture) => p.objectives.filter((o) => o.owner === p.side)

  const orderTo = (verb: 'attack' | 'defend', objId: number) => {
    if (!selIds.length) return
    void issue(selIds.map((id) => ({ kind: verb, formation: id, objective: objId })))
    setMode('none')
  }
  const hold = () => void issue(selIds.map((id) => ({ kind: 'hold', formation: id })))
  const release = () => void issue(selForms.filter((f) => f.commander).map((f) => ({ kind: 'release', formation: f.id })))
  const withdraw = () => {
    if (!pic) return
    const own = ownObjs(pic)
    const cmds: GroundCommand[] = []
    for (const f of selForms) {
      const home = own.find((o) => o.id === f.home)
      const to = home ?? [...own].sort((a, b) => km(f.pos, a.pos) - km(f.pos, b.pos))[0]
      if (to) cmds.push({ kind: 'withdraw', formation: f.id, objective: to.id })
      else toast(`${f.name}: no base of ours left to fall back on.`, false)
    }
    void issue(cmds)
  }
  const raiseAt = (o: GroundObjective) => {
    setMode('none')
    void issue([{ kind: 'raise', objective: o.id }])
  }
  const raiseTarget = (hoverObj: number | null): GroundObjective | null => {
    if (!pic) return null
    const ok = (o: GroundObjective | null | undefined) => !!o && o.owner === pic.side && (o.can_raise ?? 0) > 0
    const hov = pic.objectives.find((o) => o.id === hoverObj)
    if (ok(hov)) return hov ?? null
    if (ok(selObj)) return selObj
    return null
  }

  // ── Camera ─────────────────────────────────────────────────────────────
  const getMap = (): MlMap | null => mapRef.current?.getMap() ?? null
  const flyTo = (p: LatLon, zoom?: number) => {
    const map = getMap()
    if (!map) return
    map.flyTo({ center: [p[1], p[0]], zoom: zoom ?? Math.max(map.getZoom(), 11), duration: reducedMotion() ? 0 : 750 })
  }
  const centre = (ids: number[]) => {
    const map = getMap()
    const fs = (pic?.formations ?? []).filter((f) => ids.includes(f.id))
    if (!map || !fs.length) return
    if (fs.length === 1) return flyTo(fs[0].pos)
    const lats = fs.map((f) => f.pos[0])
    const lons = fs.map((f) => f.pos[1])
    const el = map.getContainer()
    // Keep the group clear of the HUD panels: log left, drawer right, bar below.
    const w = el.clientWidth
    const h = el.clientHeight
    const padding = w > 900
      ? { top: Math.min(100, h / 6), bottom: Math.min(250, h / 3), left: Math.min(340, w / 4), right: Math.min(320, w / 4) }
      : { top: 60, bottom: Math.min(290, h / 2.5), left: 30, right: 30 }
    map.fitBounds([[Math.min(...lons), Math.min(...lats)], [Math.max(...lons), Math.max(...lats)]], {
      padding, maxZoom: 12, duration: reducedMotion() ? 0 : 750,
    })
  }

  // ── Selection ──────────────────────────────────────────────────────────
  const selectOnly = (ids: number[]) => {
    setSel(ids)
    setSelObjId(null)
    setSelEnemyId(null)
    setSelAssetId(null)
  }
  const pickFormation = (e: MouseEvent, id: number) => {
    if (inputRef.current?.wasDrag()) return
    e.stopPropagation()
    // While an order is armed, a click anywhere aims it at the base under the cursor.
    if (mode !== 'none') return applyMode(hoverObj.current)
    setSelObjId(null)
    setSelEnemyId(null)
    setSelAssetId(null)
    if (e.shiftKey) setSel((s) => (s.includes(id) ? s.filter((x) => x !== id) : [...s, id]))
    else setSel([id])
  }
  const pickKind = (e: MouseEvent, id: number) => {
    const map = getMap()
    const f = pic?.formations.find((x) => x.id === id)
    if (!map || !f || !pic) return
    e.stopPropagation()
    const b = map.getBounds()
    selectOnly(pic.formations.filter((x) => x.kind === f.kind && b.contains([x.pos[1], x.pos[0]])).map((x) => x.id))
  }
  const pickObjective = (e: MouseEvent, id: number) => {
    if (inputRef.current?.wasDrag()) return
    e.stopPropagation()
    const o = pic?.objectives.find((x) => x.id === id)
    if (!o || !pic) return
    if (mode !== 'none') return applyMode(id)
    setSelObjId(id)
    setSel([])
    setSelEnemyId(null)
    setSelAssetId(null)
  }
  const pickEnemy = (e: MouseEvent, id: number) => {
    if (inputRef.current?.wasDrag()) return
    e.stopPropagation()
    if (mode !== 'none') return applyMode(hoverObj.current)
    setSelEnemyId(id)
    setSel([])
    setSelObjId(null)
  }
  const applyMode = (objId: number | null) => {
    const r = resolveOrder(pic, mode, selIds.length > 0, objId)
    if (!r || objId == null) {
      if (mode !== 'none') toast(r?.text ? `${r.text.split(' · ')[0]}: click a base, or Esc to cancel.` : 'Click a base, or Esc to cancel.', false)
      return
    }
    if (r.verb === 'invalid') return toast(r.text.replace(' · ', ': ').toLowerCase().replace(/^./, (c) => c.toUpperCase()), false)
    const o = pic?.objectives.find((x) => x.id === objId)
    if (r.verb === 'raise' && o) return raiseAt(o)
    if (r.verb === 'attack' || r.verb === 'defend') orderTo(r.verb, objId)
  }
  const clearAll = () => {
    setSel([])
    setSelObjId(null)
    setSelEnemyId(null)
    setSelAssetId(null)
    setAssetMode('none')
  }

  const needSel = selIds.length ? null : 'Select formations first (click, Shift+drag or 1-9)'
  const enterMode = (m: Mode) => {
    if (pic && !pic.can_command) return viewOnly()
    if (m !== 'raise' && needSel) return toast(needSel, false)
    setMode((cur) => (cur === m ? 'none' : m))
  }
  const raiseKey = (hoverObj: number | null) => {
    const o = raiseTarget(hoverObj)
    if (o) return raiseAt(o)
    if (!pic || !ownObjs(pic).some((x) => (x.can_raise ?? 0) > 0)) return toast('No base of ours can spare troops right now (under threat, or garrison too thin).', false)
    setMode('raise')
  }
  const toggleFollow = () => {
    if (!selfPlayer) return toast("Follow needs you in a slot on the server: your aircraft isn't live in DCS right now.", false)
    setFollow((f) => !f)
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
  const hoverObj = useRef<number | null>(null)
  const live = useRef({
    pic, mode, selIds, help, follow, groups, pickFormation, pickKind, pickObjective, pickEnemy,
    applyMode, orderTo, hold, withdraw, release, raiseKey, enterMode, clearAll, centre, toggleFollow, cycle,
    selectOnly, toast, resolve: (o: number | null) => resolveOrder(pic, mode, selIds.length > 0, o),
    assetMode, applyAssetMode, assetDefault, armAsset, assetKey, selAsset, pickAsset, selObjId,
  })
  useEffect(() => {
    live.current = {
      pic, mode, selIds, help, follow, groups, pickFormation, pickKind, pickObjective, pickEnemy,
      applyMode, orderTo, hold, withdraw, release, raiseKey, enterMode, clearAll, centre, toggleFollow, cycle,
      selectOnly, toast, resolve: (o: number | null) => resolveOrder(pic, mode, selIds.length > 0, o),
      assetMode, applyAssetMode, assetDefault, armAsset, assetKey, selAsset, pickAsset, selObjId,
    }
  })

  // Re-place the base names whenever the view, the bases or the selection change.
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

  // Stable handlers, so a new picture doesn't re-render every marker.
  const onPickFormation = useCallback((e: MouseEvent, id: number) => live.current.pickFormation(e, id), [])
  const onPickKind = useCallback((e: MouseEvent, id: number) => live.current.pickKind(e, id), [])
  const onPickObjective = useCallback((e: MouseEvent, id: number) => live.current.pickObjective(e, id), [])
  const onPickEnemy = useCallback((e: MouseEvent, id: number) => live.current.pickEnemy(e, id), [])
  const onPickAsset = useCallback((e: MouseEvent, id: number) => live.current.pickAsset(e, id), [])

  const lastCursor = useRef('')
  const showHover = useCallback((x: number, y: number, obj: number | null) => {
    hoverObj.current = obj
    const r = live.current.resolve(obj)
    engine.setHover(r ? { x, y, obj: obj, verb: r.verb } : null)
    const el = cursorRef.current
    if (el) el.style.transform = `translate(${x + 18}px, ${y + 16}px)`
    const key = r ? `${r.text}|${r.verb}` : ''
    if (key !== lastCursor.current) {
      lastCursor.current = key
      setCursor(r ? { text: r.text, color: VERB_COLOR[r.verb] } : null)
    }
  }, [engine])

  // The cursor label and order preview follow mode/selection changes even
  // when the mouse is still.
  useEffect(() => {
    const el = cursorRef.current
    if (!el) return
    const m = /translate\(([-\d.]+)px, ([-\d.]+)px\)/.exec(el.style.transform)
    if (m) showHover(Number(m[1]) - 18, Number(m[2]) - 16, hoverObj.current)
  }, [mode, selIds, showHover])

  // Wire up as soon as the style is in, not on 'load': that waits for every
  // first tile, and a slow imagery server would leave the map dead until then.
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
      hover: showHover,
      leave: () => {
        hoverObj.current = null
        engine.setHover(null)
        lastCursor.current = ''
        setCursor(null)
      },
      click: (obj, at) => {
        const L = live.current
        if (L.assetMode !== 'none') L.applyAssetMode(at)
        else if (L.mode !== 'none') L.applyMode(obj)
        else L.clearAll()
      },
      context: (obj, at) => {
        const L = live.current
        if (L.assetMode !== 'none') return setAssetMode('none')
        if (L.mode !== 'none' && L.mode !== 'raise' && obj != null) return L.applyMode(obj)
        if (L.mode !== 'none') return setMode('none')
        if (L.selAsset) return void L.assetDefault(at)
        if (!L.selIds.length) return
        // Away from any base, a right-click moves the selection to that point.
        if (obj == null) {
          L.armAsset('fmove')
          return L.applyAssetMode(at)
        }
        const r = L.resolve(obj)
        if (r && (r.verb === 'attack' || r.verb === 'defend')) L.orderTo(r.verb, obj)
      },
      box: (ids, add) => {
        const L = live.current
        L.selectOnly(add ? [...new Set([...L.selIds, ...ids])] : ids)
      },
    })
  }
  const onZoom = (e: ViewStateChangeEvent) => {
    setNear(e.viewState.zoom >= NEAR_ZOOM)
    setFar(e.viewState.zoom < FAR_ZOOM)
  }

  // ── Hotkeys ────────────────────────────────────────────────────────────
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      const t = e.target as HTMLElement | null
      if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.tagName === 'SELECT' || t.isContentEditable)) return
      if (e.metaKey) return
      const L = live.current
      if (e.key === '?' || (e.code === 'Slash' && e.shiftKey)) {
        e.preventDefault()
        setHelp((h) => !h)
        return
      }
      if (e.key === 'Escape') {
        if (L.help) setHelp(false)
        else if (L.assetMode !== 'none') setAssetMode('none')
        else if (L.mode !== 'none') setMode('none')
        else if (L.follow) setFollow(false)
        else L.clearAll()
        return
      }
      if (L.help || !L.pic) return
      const digit = /^Digit([1-9])$/.exec(e.code)
      if (digit) {
        const n = Number(digit[1])
        if (e.ctrlKey || e.altKey) {
          e.preventDefault()
          if (!L.selIds.length) return L.toast('Select formations first, then Ctrl+' + n + ' to make them a group.', false)
          const next = { ...L.groups }
          for (const k of Object.keys(next)) next[Number(k)] = next[Number(k)].filter((id) => !L.selIds.includes(id))
          next[n] = [...L.selIds]
          setGroups(next)
          savePref('gw.groups', next)
          L.toast(`Group ${n}: ${L.selIds.length} formation${L.selIds.length === 1 ? '' : 's'}.`, true)
          return
        }
        const ids = (L.groups[n] ?? []).filter((id) => L.pic?.formations.some((f) => f.id === id))
        if (!ids.length) return L.toast(`Group ${n} is empty. Select formations and press Ctrl+${n} (or Alt+${n}).`, false)
        const prev = lastDigit.current
        lastDigit.current = { n, t: performance.now() }
        L.selectOnly(ids)
        if (prev && prev.n === n && performance.now() - prev.t < 450) L.centre(ids)
        return
      }
      if (e.ctrlKey || e.altKey) return
      const isButton = t?.tagName === 'BUTTON'
      switch (e.key.toLowerCase()) {
        case 'm':
          if (L.selAsset) L.assetKey(L.selAsset.orders.includes('sail') ? 'sail' : 'move')
          else if (L.selIds.length) L.armAsset('fmove')
          else L.toast('Select formations or one of our assets, then M and click the map.', false)
          break
        case 'g': if (!L.assetKey('fire')) L.toast('Select a battery to fire.', false); break
        case 's': if (!L.assetKey('station')) L.toast('Select an AI flight to station.', false); break
        case 'b': if (!L.assetKey('rtb')) L.toast('Select an AI flight to send home.', false); break
        case 'v': L.armAsset('barrage'); break
        case 'l': setShowLaunch((s) => !s); break
        case 'a': L.enterMode('attack'); break
        case 'd': L.enterMode('defend'); break
        case 'h': if (L.selIds.length) L.hold(); else L.toast('Select formations to hold.', false); break
        case 'w': if (L.selIds.length) L.withdraw(); else L.toast('Select formations to withdraw.', false); break
        case 'x': if (L.selIds.length) L.release(); else L.toast('Select formations to hand back to the AI.', false); break
        case 'r': L.raiseKey(hoverObj.current); break
        case 'f': L.toggleFollow(); break
        case ' ':
          if (isButton) return
          e.preventDefault()
          L.centre(L.selIds)
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

  // ── First view ─────────────────────────────────────────────────────────
  const hasPic = !!pic
  const initialView = useMemo(() => {
    const pts = [...(pic?.objectives ?? []).map((o) => o.pos), ...(pic?.formations ?? []).map((f) => f.pos)]
    if (!pts.length) return { latitude: 42, longitude: 43.5, zoom: 7 }
    const lats = pts.map((p) => p[0])
    const lons = pts.map((p) => p[1])
    return {
      bounds: [[Math.min(...lons), Math.min(...lats)], [Math.max(...lons), Math.max(...lats)]] as [[number, number], [number, number]],
      // Clear of the combat log on the left and the command bar along the bottom.
      fitBoundsOptions: {
        padding: window.innerWidth > 900 ? { top: 80, bottom: 230, left: 330, right: 70 } : { top: 70, bottom: 280, left: 30, right: 30 },
      },
    }
    // Only the first picture sets the view.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [hasPic])

  // ── Not ready ──────────────────────────────────────────────────────────
  if (!pic || !pic.enabled) {
    return <Standby pic={pic} reason={feed.reason} error={feed.error} link={feed.link} />
  }

  // ── Command card ───────────────────────────────────────────────────────
  const noCmd = pic.can_command
    ? null
    : pic.god_mode
      ? 'Admin view: orders need a pilot on a side'
      : notCommander ? 'Commanders give orders' : 'View only: link Discord and fly this campaign to command'
  const selReason = noCmd ?? needSel
  const anyRaisable = ownObjs(pic).some((o) => (o.can_raise ?? 0) > 0)
  const buttons: CmdButton[] = [
    { key: 'A', label: 'ATTACK', icon: Strike, tone: 'attack', active: mode === 'attack', disabled: selReason, run: () => enterMode('attack') },
    { key: 'D', label: 'DEFEND', icon: Defend, active: mode === 'defend', disabled: selReason, run: () => enterMode('defend') },
    { key: 'M', label: 'MOVE', icon: Pin, active: assetMode === 'fmove', disabled: selReason, run: () => armAsset('fmove') },
    { key: 'H', label: 'HOLD', icon: Shield, disabled: selReason, run: hold },
    { key: 'W', label: 'WITHDRAW', icon: ChevronsLeft, tone: 'withdraw', disabled: selReason, run: withdraw },
    {
      key: 'X', label: 'RELEASE', icon: X,
      disabled: selReason ?? (selForms.some((f) => f.commander) ? null : 'Already under AI command'),
      run: release,
    },
    {
      key: 'R', label: 'RAISE', icon: Plus, active: mode === 'raise',
      disabled: noCmd ?? (anyRaisable ? null : 'No base can spare troops right now'),
      run: () => raiseKey(null),
    },
    { key: '␣', label: 'CENTRE', icon: Pin, disabled: selIds.length ? null : 'Nothing selected', run: () => centre(selIds) },
    { key: 'F', label: follow ? 'FOLLOWING' : 'FOLLOW ME', icon: Aircraft, active: follow, disabled: selfPlayer ? null : 'Your aircraft is not live in DCS', run: toggleFollow },
    { key: 'L', label: 'COMMAND', icon: Strike, active: showLaunch, disabled: cp ? null : cmdQ.isError ? "This server's engine predates the command map" : 'The command picture has not arrived', run: () => setShowLaunch((s) => !s) },
    { key: '?', label: 'CONTROLS', icon: Info, disabled: null, run: () => setHelp(true) },
  ]

  const single = selForms.length === 1 ? selForms[0] : null
  const onEvent = (e: GroundEvent) => {
    if (e.pos) flyTo(e.pos, Math.max(getMap()?.getZoom() ?? 0, 11.5))
    if (e.formation != null && pic.formations.some((f) => f.id === e.formation)) selectOnly([e.formation])
  }
  const onBattle = (b: GroundBattle) => (mode !== 'none' ? applyMode(hoverObj.current) : flyTo(b.pos, 12.5))

  return (
    <div className={`gw-root gw-mode-${mode}${near ? ' gw-near' : ''}${far ? ' gw-far' : ''}`} style={{ ['--own' as string]: side === 'Blue' ? '#4a8fd4' : '#cc4444' }}>
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
        <BattlefieldLayers formations={pic.formations} fronts={fronts} selected={selSet} side={side} territory={territory} />
        {pic.objectives.map((o) => (
          <ObjectiveMarker key={`o${o.id}`} o={o} side={side} selected={o.id === selObjId} showName={!names || names.has(o.id)} onPick={onPickObjective} />
        ))}
        {pic.formations.map((f) => (
          <DestMarker key={`d${f.id}`} f={f} side={side} selected={selSet.has(f.id)} />
        ))}
        {pic.enemy.map((e) => (
          <EnemyMarker key={`e${e.id}`} e={e} side={side} near={near} onPick={onPickEnemy} />
        ))}
        {pic.formations.map((f) => (
          <FormationMarker
            key={`f${f.id}`}
            f={f}
            side={side}
            selected={selSet.has(f.id)}
            near={near}
            group={groupOf[f.id] ?? null}
            onPick={onPickFormation}
            onDouble={onPickKind}
          />
        ))}
        {pic.battles.map((b) => <BattleLabel key={`b${b.id}`} b={b} onPick={onBattle} />)}
        {cp && <AssetLines assets={cp.assets.filter((a) => shown(a, layers))} sel={selAsset} side={side} />}
        {cp?.assets.filter((a) => shown(a, layers)).map((a) => (
          <AssetMarker key={`a${a.id}`} a={a} side={side} selected={a.id === selAssetId} near={near} onPick={onPickAsset} />
        ))}
        {layers.enemyAir && enemyAir.map((t) => <EnemyAirMarker key={`h${t.id}`} t={t} side={side} />)}
      </Map>

      <div className="gw-vignette" />
      <div ref={boxRef} className="gw-box" />
      <div ref={cursorRef} className="gw-cursor" style={{ color: cursor?.color, opacity: cursor ? 1 : 0 }}>
        {cursor?.text}
      </div>

      <TopHud
        pic={pic}
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
      />
      <div className="gw-topstack">
        {(!pic.can_command || feed.reason === 'unavailable') && (
          <div className={`gw-banner${feed.reason === 'unavailable' ? ' bad' : ''}`}>
            {feed.reason === 'unavailable'
              ? 'The game server stopped answering. This is the last picture it sent.'
              : pic.god_mode
                ? `Admin view of ${side}: you can watch either side, but orders need a pilot registered on one.`
                : notCommander ?? 'View only: link your Discord (-linkme in DCS chat) and take a slot this campaign to give orders.'}
          </div>
        )}
        {!mock && cmdQ.isError && (
          <div className="gw-banner">
            Our aircraft, convoys and batteries aren't on this map yet: this server's engine and bfdb need the update that adds the command map.
          </div>
        )}
        {assetMode !== 'none' && (
          <div className="gw-modebar">{MODE_TEXT[assetMode]} · ESC TO CANCEL</div>
        )}
        {mode !== 'none' && (
          <div className="gw-modebar">
            {mode === 'attack' ? 'ATTACK' : mode === 'defend' ? 'DEFEND' : 'RAISE'} · CLICK A BASE · ESC TO CANCEL
          </div>
        )}
        {follow && <div className="gw-modebar follow">FOLLOWING {selfPlayer?.name.toUpperCase() ?? 'YOU'} · DRAG OR F TO STOP</div>}
      </div>

      <EventFeed events={pic.events} time={pic.time} onPick={onEvent} />
      {single && !selAsset && (
        <Drawer f={single} side={side} pic={pic} lockMins={Math.round(pic.player_lock_secs / 60)} onClose={clearAll} />
      )}
      {selAsset && (
        <AssetPanel a={selAsset} canCommand={canOrder} mode={assetMode} setMode={armAsset}
          onRtb={() => void order({ rtb: { group: selAsset.id } })} onClose={clearAll} />
      )}
      {showLaunch && cp && (
        <LaunchPanel cp={cp} selObj={selObj} side={side} canCommand={canOrder} busy={busy}
          onOrder={(o, c) => void order(o, c)} onFocus={(p) => flyTo(p, 11)} onBarrage={() => armAsset('barrage')}
          onClose={() => setShowLaunch(false)} />
      )}
      <LayerToggles layers={layers} set={setLayers} />
      <Toasts toasts={toasts} onDismiss={(id) => setToasts((t) => t.filter((x) => x.id !== id))} />
      <CommandBar
        pic={pic}
        side={side}
        selected={selForms}
        selObj={selObj}
        selEnemy={selEnemy}
        groups={groups}
        buttons={buttons}
        locked={lockReason}
        onSelect={(id) => { selectOnly([id]); const f = pic.formations.find((x) => x.id === id); if (f) flyTo(f.pos) }}
        onFocusObj={(o) => flyTo(o.pos)}
        baseExtra={selObj && (
          <BaseOrders obj={selObj} side={side} cp={cp} canCommand={canOrder} busy={busy} onOrder={(o, c) => void order(o, c)} />
        )}
      />
      {help && <HelpOverlay onClose={() => setHelp(false)} canCommand={pic.can_command} />}
    </div>
  )
}

function Standby({ pic, reason, error, link }: { pic: GroundPicture | null; reason: string | null; error: string | null; link: string }): ReactElement {
  let title = 'ESTABLISHING LINK'
  let body = 'Waiting for the first ground picture from the game server.'
  if (pic && !pic.enabled || reason === 'disabled') {
    title = 'NO GROUND WAR ON THIS SERVER'
    body = "The dynamic ground war is switched off in this server's campaign config."
  } else if (reason === 'login') {
    title = 'SIGN IN TO COMMAND'
    body = "The ground picture is locked to your coalition. Sign in with Discord to see your side's war."
  } else if (reason === 'nocoalition') {
    title = 'NO COALITION'
    body = "The ground picture is locked to your coalition, and the server can't tell which side you're on. Link your Discord (-linkme in DCS chat) and take a slot this campaign, then reload."
  } else if (reason === 'unavailable') {
    title = 'GAME SERVER NOT ANSWERING'
    body = 'bfdb is up, but the engine is not answering for the ground picture. It may be restarting; this page reconnects by itself.'
  } else if (error && link === 'offline') {
    title = 'COMMAND UNAVAILABLE'
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

/** Why a pilot on a side can't give orders, from /api/command/me: what rank
 *  unlocks command and how far off it is. Null when it isn't rank (not linked,
 *  or the server doesn't require commanders). */
function notCommanderText(me: CommandMe | undefined): string | null {
  if (!me || me.can_command || !me.require_commander) return null
  const st = me.status
  if (st?.grant === 'revoked') return 'An admin has withdrawn your commander access on this server.'
  const need = rankFor(me.commander_score, me.side).title
  if (!st) return `Commanders give orders here. Command unlocks at ${need} (campaign score ${me.commander_score}).`
  const now = rankFor(st.score, me.side).title
  return `Commanders give orders here. Command unlocks at ${need} (campaign score ${me.commander_score}); `
    + `you are ${now} with ${Math.round(st.score)}. An admin can also grant it.`
}
