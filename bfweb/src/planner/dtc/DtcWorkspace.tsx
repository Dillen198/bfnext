import { useEffect, useMemo, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { ChevronDown, ChevronLeft, ChevronRight, ChevronUp, Download, Pencil, Search, X } from '@icons'
import { AnnotationIndex, applyOverrides, toPointer, withOverride, type Json, type Overrides, type Seg } from '../model'
import { DCS_MAPS, offsetOnGrid, terrainAt, terrainNamed, toDcsXY, type DcsTerrain } from '../nav'
import { shareString } from '../dcsdtc'
import { download, ExportError, safeName } from '../exports'
import { dcsdtcTarget, loadModel, type Target } from '../targets'
import { aircraftDef, MAX_WAYPOINTS, type NativeType, type Plan, type PlanWaypoint } from '../types'
import { Body, HitRow } from './editor'
import { collectLeaves, offsetSupport, playerHelp, type Ctx, type PointOption } from './core'
import { buildFor, METHOD, type Msg } from './build'
import { buildPages, pageChanges, routeFilled, type Page } from './pages'
import { flagPointers, isOn, switchesFor, type LoadSwitch } from './switches'
import { DcsDtcGuide, NativeGuide } from './guides'
import DtcMap from './DtcMap'
import OffsetsEditor from './OffsetsEditor'
import MissionPanel from './MissionPanel'
import { missionFills, type MissionData, type MissionFill } from './mission'
import './dtc.css'

/** A page path nothing lies under: search results and settings-only pages
 *  show the whole cartridge faintly, with nothing to draw. */
const NOWHERE: Seg[] = ['(none)']

/** Pages that are the route's steerpoints: where offsets are edited. */
const STEERPOINT_PAGES = new Set(['/MPD/NAV_PTS', '/MPD/VIPVRP', '/WYPT/NAV_PTS', '/Waypoints'])

interface Props {
  plan: Plan
  targets: Target[]
  target: Target
  onTarget: (t: Target) => void
  readOnly: boolean
  onSystems: (key: string, ov: Overrides) => void
  /** Change the plan's route: drawn on the map here, or its offsets. */
  onWaypoints: (w: PlanWaypoint[]) => void
  /** The campaign's DCS map, for a plan with no route yet. */
  homeMap?: string | null
  /** Set the plan's radio presets (from the comms card). */
  onComms: (comm1: Plan['comm1'], comm2: Plan['comm2']) => void
  /** What the campaign knows that this cartridge can take. */
  mission: MissionData | null
  tosSeconds: (i: number) => number | null
  tosText: (i: number) => string | null
  onClose: () => void
}

export default function DtcWorkspace({ plan, targets, target, onTarget, readOnly, onSystems, onWaypoints, homeMap, onComms, mission, tosSeconds, tosText, onClose }: Props) {
  const def = aircraftDef(plan.aircraft)
  const { data: model, error } = useQuery({ queryKey: ['planner-model', target.key], queryFn: () => loadModel(target), staleTime: Infinity })
  const ov = (plan.systems?.[target.key] ?? {}) as Overrides
  const [pageKey, setPageKey] = useState<string | null>(null)
  const [query, setQuery] = useState('')
  const [changedOnly, setChangedOnly] = useState(false)
  const [missionOpen, setMissionOpen] = useState(false)
  const [msg, setMsg] = useState<Msg | null>(null)
  // The download and steps stay folded away until asked for.
  const [loadOpen, setLoadOpen] = useState(false)
  // The map can fold away when a page needs the room (the RWR tables).
  const [mapOpen, setMapOpen] = useState(true)

  // Escape closes, and the page behind does not scroll.
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') onClose() }
    window.addEventListener('keydown', onKey)
    const prev = document.body.style.overflow
    document.body.style.overflow = 'hidden'
    return () => { window.removeEventListener('keydown', onKey); document.body.style.overflow = prev }
  }, [onClose])

  // The route's map once it has a point; before that the campaign's, or the one picked.
  const [mapName, setMapName] = useState<string | null>(null)
  const routeMap = plan.waypoints.length ? terrainAt(plan.waypoints[0].lat, plan.waypoints[0].lon) : null
  const map = routeMap ?? terrainNamed(mapName ?? homeMap ?? 'Caucasus')
  // Drawing starts on when there is nothing to draw on yet.
  const [drawing, setDrawing] = useState(plan.waypoints.length === 0 && !readOnly)
  const route = drawing && !readOnly ? {
    add: (lat: number, lon: number) => {
      if (plan.waypoints.length >= MAX_WAYPOINTS) return
      const prev = plan.waypoints[plan.waypoints.length - 1]
      onWaypoints([...plan.waypoints, { name: '', lat, lon, alt_ft: prev?.alt_ft ?? 20000, elev_ft: 0, target: false, note: '' }])
    },
    move: (i: number, lat: number, lon: number) => onWaypoints(plan.waypoints.map((w, j) => (j === i ? { ...w, lat, lon } : w))),
    remove: (i: number) => onWaypoints(plan.waypoints.filter((_, j) => j !== i)),
  } : null
  const points: PointOption[] = useMemo(() => !map ? [] : plan.waypoints.map((w, i) => {
    const { x, y } = toDcsXY(w.lat, w.lon, map.terrain)
    return { label: `${i + 1} ${w.name || `WP${i + 1}`}`, x, y, lat: w.lat, lon: w.lon, elev_m: w.elev_ft * 0.3048 }
  }), [plan.waypoints, map])

  const derived = useMemo(() => {
    if (!model) return null
    const idx = new AnnotationIndex(model.annotations)
    const switches = switchesFor(target, model.defaults)
    const hidden = flagPointers(switches)
    const value = applyOverrides(model.defaults, ov)
    return { idx, switches, hidden, value }
  }, [model, target, ov])

  // What the exported file would say: the source of truth for "will load".
  const preview = useMemo(() => {
    if (!model) return null
    try {
      return JSON.parse(buildFor(plan, target, model.defaults, ov, tosSeconds, tosText, false).text) as Json
    } catch {
      return null
    }
  }, [model, plan, target, ov, tosSeconds, tosText])
  const file = preview && target.tool === 'native' ? (preview as { data: Json }).data : preview

  if (error) return <Shell onClose={onClose} title={def.label}><div style={{ padding: 20, color: '#f04747' }}>{(error as Error).message}</div></Shell>
  if (!model || !derived) return <Shell onClose={onClose} title={def.label}><div style={{ padding: 20, color: 'var(--text-dim)' }}>Loading the {def.label} cartridge…</div></Shell>

  const { idx, switches, hidden, value } = derived
  const pages = buildPages(value, idx, switches, hidden)
  const page = pages.find(p => p.key === pageKey) ?? pages[0]
  const ctx: Ctx = {
    idx, defaults: model.defaults, overrides: ov, value, readOnly, hidden, points,
    set: (segs, v) => onSystems(target.key, withOverride(model.defaults, ov, segs, v)),
    setMany: pairs => onSystems(target.key, pairs.reduce((o, [segs, v]) => withOverride(model.defaults, o, segs, v), ov)),
  }

  /** On, and whether that is automatic (from changes/route) or the pilot's choice. */
  const switchState = (sw: LoadSwitch) => {
    const manual = sw.flag in ov
    const on = file ? isOn(file, sw) : manual ? ov[sw.flag] === sw.load : sw.covers.some(c => Object.keys(ov).some(k => k === c || k.startsWith(c + '/')))
    return { on, manual }
  }
  const setSwitch = (sw: LoadSwitch, on: boolean | 'auto') => {
    const next = { ...ov }
    if (on === 'auto') delete next[sw.flag]
    else next[sw.flag] = on ? sw.load : !sw.load
    onSystems(target.key, next)
  }

  const run = async (f: () => Promise<{ filename: string; text: string; warnings: string[] }> | { filename: string; text: string; warnings: string[] }, done: string) => {
    try {
      const out = await f()
      download(out.filename, out.text, 'application/json')
      setMsg(out.warnings.length ? { kind: 'warn', text: `${done} ${out.warnings.join(' ')}` } : { kind: 'ok', text: done })
    } catch (e) {
      setMsg({ kind: 'err', text: e instanceof ExportError ? e.message : String(e) })
    }
  }
  const downloadNative = () => run(() => buildFor(plan, target, model.defaults, ov, tosSeconds, tosText), 'Cartridge downloaded.')
  const downloadPreset = () => run(() => buildFor(plan, target, model.defaults, ov, tosSeconds, tosText, true), 'Preset downloaded.')
  const downloadWhole = () => run(() => buildFor(plan, target, model.defaults, ov, tosSeconds, tosText, false), 'Full preset downloaded.')
  const copyShare = async () => {
    try {
      const out = buildFor(plan, target, model.defaults, ov, tosSeconds, tosText, true)
      await navigator.clipboard.writeText(await shareString(out.text))
      setMsg({ kind: 'ok', text: 'Share code copied. In DCS-DTC: Load/Save > Load from clipboard.' })
    } catch (e) {
      setMsg({ kind: 'err', text: e instanceof ExportError ? e.message : String(e) })
    }
  }
  const fills = mission ? missionFills(ctx, target, plan, map, mission) : []
  const fillOne = (f: MissionFill) => {
    if (f.comms) onComms(f.comms.comm1, f.comms.comm2)
    if (f.writes?.length) ctx.setMany?.(f.writes)
  }
  const doFill = (f: MissionFill) => {
    if (!f.done && f.replaces && !confirm(`${f.title}: replace the ${f.replaces} entr${f.replaces > 1 ? 'ies' : 'y'} there now?`)) return
    fillOne(f)
    setMsg({ kind: 'ok', text: `${f.title}: ${f.count} filled.` })
    // Show where it went.
    const p = f.pageSegs && toPointer(f.pageSegs)
    const to = p == null ? undefined : pages.find(x => x.key === p) ?? pages.find(x => x.key && p.startsWith(x.key + '/'))
    if (to) { setPageKey(to.key); setMissionOpen(false) }
  }
  const fillAll = () => {
    const todo = fills.filter(f => !f.done)
    const lost = todo.reduce((n, f) => n + f.replaces, 0)
    if (lost && !confirm(`This replaces ${lost} entr${lost > 1 ? 'ies' : 'y'} you have now. Fill all ${todo.length}?`)) return
    // The cartridge writes go in one change, the comms to the plan.
    const writes = todo.flatMap(f => f.writes ?? [])
    if (writes.length) ctx.setMany?.(writes)
    const comms = todo.find(f => f.comms)?.comms
    if (comms) onComms(comms.comm1, comms.comm2)
    setMsg({ kind: 'ok', text: `Filled from the mission: ${todo.map(f => f.title.toLowerCase()).join(', ')}.` })
  }
  const noRoute = plan.waypoints.length === 0 ? 'Draw your route on the map first.' : undefined
  const nativeName = `${safeName(plan.name || 'Flight plan')}.dtc`

  const hits = query.trim()
    ? collectLeaves(ctx).filter(h => h.path.join(' ').toLowerCase().includes(query.trim().toLowerCase())).slice(0, 200)
    : null
  const changedHits = changedOnly && !hits
    ? collectLeaves(ctx).filter(h => Object.keys(ov).some(k => { const p = '/' + h.segs.join('/'); return p === k || p.startsWith(k + '/') }))
    : null
  const totalChanges = Object.keys(ov).filter(k => !hidden.has(k)).length

  const groups: [string | undefined, Page[]][] = []
  for (const p of pages) {
    const last = groups[groups.length - 1]
    if (last && last[0] === p.group) last[1].push(p)
    else groups.push([p.group, [p]])
  }

  return (
    <Shell onClose={onClose} title={def.label}
      methods={targets.length > 1 ? (
        <div style={{ display: 'flex', gap: 8, flex: '1 1 460px', flexWrap: 'wrap' }} role="group" aria-label="How you will load it">
          {targets.map(t => (
            <button key={t.key} type="button" className="dtc-method" aria-pressed={t.key === target.key} onClick={() => { onTarget(t); setPageKey(null) }}>
              <div style={{ fontWeight: 700, fontSize: '0.76rem' }}>{METHOD[t.tool].name}</div>
              <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)' }}>{METHOD[t.tool].blurb}</div>
            </button>
          ))}
        </div>
      ) : (
        <div style={{ flex: '1 1 300px', fontSize: '0.7rem', color: 'var(--text-dim)' }}>
          <b style={{ color: 'var(--text)' }}>{METHOD[target.tool].name}</b> · {METHOD[target.tool].blurb}
        </div>
      )}>
      <div className={`dtc-body${mapOpen ? '' : ' dtc-map-closed'}`}>
        <aside className="dtc-side" aria-label="Cartridge pages">
          <div style={{ padding: 10, display: 'flex', flexDirection: 'column', gap: 6 }}>
            <label style={{ position: 'relative', display: 'block' }}>
              <Search size={12} style={{ position: 'absolute', left: 8, top: 8, color: 'var(--text-dim)' }} />
              <input className="vs-input" value={query} onChange={e => setQuery(e.target.value)} placeholder="Search settings (e.g. chaff, bingo, preset)"
                aria-label="Search settings" style={{ width: '100%', padding: '5px 8px 5px 26px', fontSize: '0.72rem' }} />
            </label>
            {mission && (
              <button type="button" className={missionOpen ? 'vs-btn' : 'vs-btn-outline'} aria-pressed={missionOpen}
                onClick={() => { setMissionOpen(v => !v); setQuery(''); setChangedOnly(false) }}
                style={{ display: 'flex', gap: 6, alignItems: 'center', justifyContent: 'center', padding: '5px 8px', fontSize: '0.7rem' }}>
                <Download size={12} /> From the mission{fills.filter(f => !f.done).length ? ` (${fills.filter(f => !f.done).length})` : ''}
              </button>
            )}
            <label style={{ display: 'flex', gap: 6, alignItems: 'center', fontSize: '0.68rem', color: 'var(--text-dim)' }}>
              <input type="checkbox" checked={changedOnly} onChange={e => setChangedOnly(e.target.checked)} />
              Show only what I changed ({totalChanges})
            </label>
            {totalChanges > 0 && !readOnly && (
              <button type="button" className="vs-btn-ghost" style={{ fontSize: '0.64rem', padding: '2px 6px', alignSelf: 'flex-start' }}
                onClick={() => { if (confirm('Put every setting of this cartridge back to the default?')) onSystems(target.key, {}) }}>
                Reset all to defaults
              </button>
            )}
          </div>
          <select className="vs-input dtc-page-select" value={page?.key} onChange={e => { setPageKey(e.target.value); setQuery(''); setChangedOnly(false) }} aria-label="Page">
            {pages.map(p => <option key={p.key} value={p.key}>{p.group ? `${p.group} › ` : ''}{p.label}</option>)}
          </select>
          <nav className="dtc-pages">
            {groups.map(([g, ps], gi) => (
              <div key={gi} style={{ marginBottom: 6 }}>
                {g && <div style={{ fontSize: '0.6rem', letterSpacing: '0.08em', textTransform: 'uppercase', color: 'var(--text-dim)', padding: '6px 8px 2px' }}>{g}</div>}
                {ps.map(p => {
                  const n = pageChanges(p, ov)
                  const route = routeFilled(p, idx, value)
                  const st = p.sw ? switchState(p.sw) : null
                  return (
                    <button key={p.key} type="button" className="dtc-page-btn" aria-current={!hits && !changedHits && p.key === page?.key ? 'page' : undefined}
                      onClick={() => { setPageKey(p.key); setQuery(''); setChangedOnly(false); setMissionOpen(false) }}>
                      <span style={{ flex: 1, minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis' }}>{p.label}</span>
                      {route && <Chip tone="info">route</Chip>}
                      {n > 0 && <Chip tone="accent">{n}</Chip>}
                      {st && <span title={st.on ? 'Loads into the jet' : 'Does not load: the jet keeps the mission\'s data'} style={{ color: st.on ? 'var(--accent)' : 'var(--text-dim)', fontSize: '0.7rem' }}>{st.on ? '●' : '○'}</span>}
                    </button>
                  )
                })}
              </div>
            ))}
            <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', padding: '8px', lineHeight: 1.6 }}>
              <span style={{ color: 'var(--accent)' }}>●</span> loads into the jet · ○ the jet keeps the mission&apos;s · <Chip tone="info">route</Chip> filled from your route
            </div>
          </nav>
        </aside>

        <main className="dtc-main">
          {missionOpen && !hits && !changedHits ? (
            <MissionPanel fills={fills} loaded={!!mission} readOnly={readOnly} onFill={doFill} onFillAll={fillAll} />
          ) : hits ? (
            <>
              <h3 style={h3}>Search: “{query}”</h3>
              {hits.length === 0 && <div style={dim}>Nothing matches. Try another word.</div>}
              {hits.map(h => <HitRow key={h.segs.join('/')} ctx={ctx} hit={h} />)}
            </>
          ) : changedHits ? (
            <>
              <h3 style={h3}>What you changed</h3>
              {changedHits.length === 0 && <div style={dim}>Nothing yet: every setting is the DCS default.</div>}
              {changedHits.map(h => <HitRow key={h.segs.join('/')} ctx={ctx} hit={h} />)}
            </>
          ) : page ? (
            <>
              <div style={{ display: 'flex', gap: 12, alignItems: 'flex-start', flexWrap: 'wrap', marginBottom: 8 }}>
                <div style={{ flex: '1 1 300px' }}>
                  <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', letterSpacing: '0.06em', textTransform: 'uppercase' }}>{page.group ?? def.label}</div>
                  <h3 style={{ ...h3, marginTop: 2 }}>{page.label}</h3>
                  {playerHelp(page.help) && <p style={{ ...dim, margin: '4px 0 0', maxWidth: 720 }}>{playerHelp(page.help)}</p>}
                </div>
                {page.sw && <LoadToggle state={switchState(page.sw)} readOnly={readOnly} onSet={on => setSwitch(page.sw!, on)} />}
              </div>
              {page.managed
                ? <>
                    <div style={{ ...dim, padding: '6px 0 12px' }}>This page is filled from your route on the map: {plan.waypoints.length} point{plan.waypoints.length === 1 ? '' : 's'}. Edit the route to change it.</div>
                    {STEERPOINT_PAGES.has(page.key) && offsetSupport(target.code) && (
                      <OffsetsEditor waypoints={plan.waypoints} support={offsetSupport(target.code)!} readOnly={readOnly} onChange={onWaypoints} />
                    )}
                  </>
                : <Body key={page.key} ctx={ctx} segs={page.segs} scalarsOnly={page.scalarsOnly} />}
            </>
          ) : null}
        </main>

        {!mapOpen ? (
          <section className="dtc-mapcol dtc-mapcol-closed" aria-label="Map">
            <button type="button" className="vs-btn-ghost" onClick={() => setMapOpen(true)} title="Show the map" aria-expanded={false}
              style={{ writingMode: 'vertical-rl', padding: '10px 4px', fontSize: '0.66rem', display: 'flex', gap: 6, alignItems: 'center' }}>
              <ChevronLeft size={12} /> Show map
            </button>
          </section>
        ) : (
        <section className="dtc-mapcol" aria-label="Map">
          <div style={{ display: 'flex', alignItems: 'center', gap: 8, marginBottom: 6, flexWrap: 'wrap' }}>
            <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)', letterSpacing: '0.06em', textTransform: 'uppercase' }}>Map</span>
            {routeMap ? (
              <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)' }}>{routeMap.terrain.name}</span>
            ) : (
              <select className="vs-input" value={map?.terrain.name ?? ''} onChange={e => setMapName(e.target.value)} aria-label="DCS map"
                style={{ fontSize: '0.66rem', padding: '2px 4px' }}>
                {DCS_MAPS.map(n => <option key={n} value={n}>{n}</option>)}
              </select>
            )}
            {!readOnly && (
              <span style={{ marginLeft: 'auto', display: 'flex', gap: 6 }}>
                {drawing && plan.waypoints.length > 0 && (
                  <button type="button" className="vs-btn-ghost" style={{ fontSize: '0.64rem', padding: '3px 8px' }}
                    onClick={() => onWaypoints(plan.waypoints.slice(0, -1))}>Undo last</button>
                )}
                <button type="button" className={drawing ? 'vs-btn' : 'vs-btn-outline'} aria-pressed={drawing} onClick={() => setDrawing(v => !v)}
                  style={{ display: 'inline-flex', gap: 5, alignItems: 'center', fontSize: '0.64rem', padding: '3px 10px' }}>
                  <Pencil size={11} /> {drawing ? 'Done drawing' : 'Draw route'}
                </button>
              </span>
            )}
            <button type="button" className="vs-btn-ghost" onClick={() => setMapOpen(false)} title="Hide the map: more room for the page" aria-label="Hide the map" aria-expanded
              style={{ padding: '3px 6px', marginLeft: readOnly ? 'auto' : undefined }}><ChevronRight size={12} /></button>
          </div>
          {map && (
            <div style={{ flex: 1, minHeight: 0 }}>
              <DtcMap key={map.terrain.name} ctx={ctx} plan={plan} map={map} pageSegs={page && !hits && !changedHits && !page.scalarsOnly ? page.segs : NOWHERE}
                extras={offsetMarks(plan.waypoints, map)} route={route} />
            </div>
          )}
        </section>
        )}
      </div>

      {loadOpen && (
        <div className="dtc-foot-steps" id="dtc-load-steps">
          {target.tool === 'native' ? (
            <NativeGuide type={target.code as NativeType} map={map?.terrain.name} fileName={nativeName} disabled={!!noRoute} why={noRoute} onDownload={downloadNative} />
          ) : (
            <DcsDtcGuide code={target.code} label={def.label} disabled={!!noRoute} storeOnly={dcsdtcTarget(target.code)?.storeOnly}
              onLoadFile={downloadPreset} onWhole={downloadWhole} onShare={copyShare} />
          )}
          <Summary pages={pages} switchState={switchState} />
        </div>
      )}
      <footer className="dtc-foot">
        <button type="button" className={loadOpen ? 'vs-btn-outline' : 'vs-btn'} aria-expanded={loadOpen} aria-controls="dtc-load-steps"
          onClick={() => setLoadOpen(v => !v)} style={{ display: 'inline-flex', gap: 6, alignItems: 'center', padding: '6px 12px', fontSize: '0.72rem' }}>
          <Download size={12} /> Load into the jet {loadOpen ? <ChevronDown size={12} /> : <ChevronUp size={12} />}
        </button>
        {msg && <span role="status" style={{ ...footHint, flex: '1 1 300px', color: msg.kind === 'err' ? '#f04747' : msg.kind === 'warn' ? '#f0a030' : 'var(--accent)' }}>{msg.text}</span>}
      </footer>
    </Shell>
  )
}

/** Offset aimpoints and VIP/VRP points of the route, for the map. */
function offsetMarks(w: PlanWaypoint[], map: DcsTerrain) {
  const out: { lat: number; lon: number; label: string; from: { lat: number; lon: number } }[] = []
  w.forEach((p, i) => {
    const at = (o: { range_ft: number; bearing_deg: number }, label: string) =>
      out.push({ ...offsetOnGrid(p.lat, p.lon, o, map.terrain), label, from: { lat: p.lat, lon: p.lon } })
    ;(p.oa ?? []).forEach((o, k) => at(o, `OA${k + 1}·${i + 1}`))
    if (p.vip) { at(p.vip.point, 'VIP TGT'); at(p.vip.pup, 'VIP PUP') }
    if (p.vrp) { at(p.vrp.point, 'VRP'); at(p.vrp.pup, 'VRP PUP') }
  })
  return out
}

const h3 = { margin: 0, fontSize: '0.95rem', fontWeight: 700, color: 'var(--text)' } as const
const dim = { fontSize: '0.7rem', color: 'var(--text-dim)', lineHeight: 1.6 } as const
const footHint = { fontSize: '0.66rem', color: 'var(--text-dim)', lineHeight: 1.5 } as const

function Chip({ tone, children }: { tone: 'info' | 'accent'; children: React.ReactNode }) {
  return (
    <span style={{
      fontSize: '0.56rem', fontWeight: 700, padding: '1px 5px', borderRadius: 8, letterSpacing: '0.03em',
      background: tone === 'accent' ? 'var(--accent)' : 'rgba(64,166,255,0.18)', color: tone === 'accent' ? '#0b0f08' : '#7cc4ff',
    }}>{children}</span>
  )
}

/** The page's "load into the jet" switch, in words. */
function LoadToggle({ state, readOnly, onSet }: { state: { on: boolean; manual: boolean }; readOnly: boolean; onSet: (on: boolean | 'auto') => void }) {
  return (
    <div style={{ border: '1px solid var(--border)', borderRadius: 6, padding: '8px 10px', minWidth: 240, background: 'var(--bg-elevated)' }}>
      <label style={{ display: 'flex', gap: 8, alignItems: 'center', fontSize: '0.74rem', fontWeight: 700, cursor: readOnly ? 'default' : 'pointer' }}>
        <input type="checkbox" checked={state.on} disabled={readOnly} onChange={e => onSet(e.target.checked)} style={{ width: 16, height: 16 }} />
        Load this page into the jet
      </label>
      <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 3, lineHeight: 1.45 }}>
        {state.manual
          ? <>You set this. <button type="button" className="vs-btn-ghost" style={{ padding: '0 4px', fontSize: '0.62rem' }} disabled={readOnly} onClick={() => onSet('auto')}>Back to automatic</button></>
          : state.on ? 'On automatically: you changed something here, or your route fills it.' : 'Off: the jet keeps the mission\'s own data. Change something here and it switches on.'}
      </div>
    </div>
  )
}

function Summary({ pages, switchState }: { pages: Page[]; switchState: (sw: LoadSwitch) => { on: boolean } }) {
  const on = pages.filter(p => p.sw && switchState(p.sw).on).map(p => p.label)
  const always = pages.filter(p => !p.sw).map(p => p.label)
  return (
    <div style={{ marginTop: 14, fontSize: '0.66rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
      <div style={{ fontWeight: 700, color: 'var(--text)', marginBottom: 2 }}>What loads</div>
      {on.length > 0 && <div><span style={{ color: 'var(--accent)' }}>●</span> {on.join(', ')}</div>}
      {on.length === 0 && pages.some(p => p.sw) && <div>Nothing switchable is on yet: change a page, or switch it on.</div>}
      {always.length > 0 && <div style={{ marginTop: 4 }}>Always loaded with the cartridge: {always.join(', ')}.</div>}
    </div>
  )
}

function Shell({ title, onClose, methods, children }: { title: string; onClose: () => void; methods?: React.ReactNode; children: React.ReactNode }) {
  return (
    <div className="dtc-overlay" role="presentation" onMouseDown={e => { if (e.target === e.currentTarget) onClose() }}>
      <div className="dtc-panel" role="dialog" aria-modal="true" aria-label={`Data cartridge: ${title}`}>
        <header className="dtc-head">
          <div style={{ flex: '0 0 auto' }}>
            <div style={{ fontSize: '0.6rem', letterSpacing: '0.1em', textTransform: 'uppercase', color: 'var(--text-dim)' }}>Data cartridge</div>
            <div style={{ fontSize: '1rem', fontWeight: 800 }}>{title}</div>
          </div>
          {methods}
          <button type="button" className="vs-btn-ghost" onClick={onClose} aria-label="Close" style={{ marginLeft: 'auto', padding: 6 }}><X size={14} /></button>
        </header>
        {children}
      </div>
    </div>
  )
}

