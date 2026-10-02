// GROUND COMMAND: the coalition's ground war on one map, and the controls to
// run it. Everything on it comes from /api/groundwar, which the engine builds
// for the viewer's own side only -- our formations in full, the enemy only
// where we are in contact, and the battles both sides are in. Orders go to
// /api/groundwar/command and are checked again by the engine against the
// side the viewer's own pilot is on.
import circle from '@turf/circle'
import type { Feature, FeatureCollection } from 'geojson'
import ms from 'milsymbol'
import { useMemo, useRef, useState, type CSSProperties, type ReactElement } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import Map, { Layer, Marker, Popup, Source, type MapRef } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'

import {
  api,
  type Frontlines,
  type GroundBattle,
  type GroundCommand,
  type GroundFormation,
  type GroundObjective,
  type GroundPicture,
  type LatLon,
} from '../api'
import { useAuth } from '../context/AuthContext'
import { useTheme } from '../context/ThemeContext'
import { mapStyleFor } from '../lib/mapStyle'
import { groundwarMock, groundwarMockCommand } from './groundwarMock'

const SIDE_COLOR = { Blue: '#4a8fd4', Red: '#cc4444', Neutral: '#8a8f80' } as const
const BATTLE = '#ff8c1a'

/** km between two points, flat-earth -- fine for "which is nearer". */
function km(a: LatLon, b: LatLon): number {
  const kx = 111.32 * Math.cos((a[0] * Math.PI) / 180)
  const dx = (a[1] - b[1]) * kx
  const dy = (a[0] - b[0]) * 110.57
  return Math.sqrt(dx * dx + dy * dy)
}

const symCache: Record<string, string> = {}
/** A NATO company symbol (2525C letter SIDC) as an image URL. */
function unitSymbol(kind: 'armour' | 'mechanised' | 'infantry', hostile: boolean, size = 26): string {
  const key = `${kind}:${hostile}:${size}`
  if (symCache[key]) return symCache[key]
  const fn = kind === 'armour' ? 'UCA---' : kind === 'mechanised' ? 'UCIZ--' : 'UCI---'
  const sidc = `S${hostile ? 'H' : 'F'}GP${fn}-E---`
  const svg = new ms.Symbol(sidc, { size, fill: true, frame: true }).asSVG()
  symCache[key] = `data:image/svg+xml;utf8,${encodeURIComponent(svg)}`
  return symCache[key]
}

function formationKind(f: GroundFormation): 'armour' | 'mechanised' | 'infantry' {
  const n = f.name.toLowerCase()
  if (n.includes('mech')) return 'mechanised'
  if (n.includes('armd') || n.includes('armour') || n.includes('armor')) return 'armour'
  return f.has_infantry ? 'infantry' : 'armour'
}

function orderText(f: GroundFormation): string {
  switch (f.order) {
    case 'hold': return 'Holding position'
    case 'attack': return `Attacking ${f.target_name ?? '?'}`
    case 'defend': return `Defending ${f.target_name ?? '?'}`
    case 'withdraw': return `Withdrawing to ${f.target_name ?? '?'}`
  }
}

function since(unix: number): string {
  const m = Math.max(0, Math.round((Date.now() / 1000 - unix) / 60))
  return m < 60 ? `${m} min` : `${Math.floor(m / 60)} h ${m % 60} min`
}

const panel: CSSProperties = {
  background: 'var(--bg-card)',
  border: '1px solid var(--border-light)',
  borderRadius: 3,
  padding: '8px 10px',
}
const label: CSSProperties = {
  fontFamily: 'var(--font-mono)',
  fontSize: '0.62rem',
  letterSpacing: '0.14em',
  color: 'var(--text-dim)',
}
function btn(color: string, disabled = false): CSSProperties {
  return {
    background: 'transparent',
    border: `1px solid ${color}`,
    color,
    fontFamily: 'var(--font-mono)',
    fontSize: '0.66rem',
    letterSpacing: '0.08em',
    padding: '4px 8px',
    borderRadius: 2,
    cursor: disabled ? 'not-allowed' : 'pointer',
    opacity: disabled ? 0.4 : 1,
  }
}

function Badge({ color, children }: { color: string; children: string }) {
  return (
    <span style={{
      fontFamily: 'var(--font-mono)', fontSize: '0.56rem', letterSpacing: '0.1em',
      padding: '1px 5px', borderRadius: 2, border: `1px solid ${color}`, color,
    }}>{children}</span>
  )
}

function StrengthBar({ alive, total }: { alive: number; total: number }) {
  const pct = total > 0 ? Math.round((alive / total) * 100) : 0
  const c = pct >= 70 ? 'var(--accent-bright)' : pct >= 40 ? 'var(--yellow)' : 'var(--red)'
  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
      <div style={{ flex: 1, height: 4, background: 'rgba(0,0,0,0.45)', borderRadius: 1, overflow: 'hidden' }}>
        <div style={{ width: `${pct}%`, height: '100%', background: c }} />
      </div>
      <span style={{ fontFamily: 'var(--font-mono)', fontSize: '0.62rem', color: c }}>{alive}/{total}</span>
    </div>
  )
}

export default function GroundWarPage(): ReactElement {
  const { user } = useAuth()
  const { theme } = useTheme()
  const mapStyle = useMemo(() => mapStyleFor(theme), [theme])
  const qc = useQueryClient()
  const mapRef = useRef<MapRef>(null)
  // An admin with no side of their own picks which one to look at.
  const adminPick = !!user?.is_admin && !user?.side
  const [viewSide, setViewSide] = useState<'Blue' | 'Red'>('Blue')
  const sideParam = adminPick ? viewSide : undefined
  // Dev builds only: `?mock` renders from a fixture and never calls the API.
  // Keep the guard literal so a production build drops the fixture.
  const mock = import.meta.env.DEV && new URLSearchParams(location.search).has('mock')

  const { data: pic, error, isLoading } = useQuery<GroundPicture>({
    queryKey: ['groundwar', sideParam],
    queryFn: () => (mock ? Promise.resolve(groundwarMock) : api.groundwar.picture(sideParam)),
    // Every 5 s while it answers; once a minute, and no retries, while it
    // doesn't -- a server whose bflib.dll predates the ground war never will,
    // and hammering it only ties up bfdb.
    retry: false,
    refetchInterval: (q) => (q.state.error ? 60_000 : 5_000),
  })
  const { data: fronts = { mid: [], blue: [], red: [] } } = useQuery<Frontlines>({
    queryKey: ['frontline'],
    queryFn: () => api.frontline(),
    refetchInterval: 60_000,
  })

  const [selected, setSelected] = useState<number | null>(null)
  const [picked, setPicked] = useState<GroundObjective | null>(null)
  const [result, setResult] = useState<{ ok: boolean; text: string } | null>(null)

  const command = useMutation({
    mutationFn: (cmd: GroundCommand) =>
      mock ? Promise.resolve(groundwarMockCommand(cmd)) : api.groundwar.command(cmd),
    onSuccess: (r) => {
      setResult({ ok: r.ok, text: r.message })
      if (r.ok && r.formation != null) setSelected(r.formation)
      qc.invalidateQueries({ queryKey: ['groundwar'] })
    },
    onError: (e: Error) => setResult({ ok: false, text: e.message }),
  })
  const send = (cmd: GroundCommand) => {
    setPicked(null)
    command.mutate(cmd)
  }

  const side = pic?.side ?? 'Blue'
  const ours = SIDE_COLOR[side]
  const canCommand = !!pic?.can_command && !command.isPending
  const sel = pic?.formations.find((f) => f.id === selected) ?? null

  const frontGeo: FeatureCollection = useMemo(() => ({
    type: 'FeatureCollection',
    features: (['blue', 'red', 'mid'] as const).flatMap((k) =>
      fronts[k].filter((l) => l.length > 1).map((l): Feature => ({
        type: 'Feature',
        properties: { k },
        geometry: { type: 'LineString', coordinates: l.map(([lat, lon]) => [lon, lat]) },
      })),
    ),
  }), [fronts])

  const pathGeo: FeatureCollection = useMemo(() => ({
    type: 'FeatureCollection',
    features: (pic?.formations ?? []).filter((f) => f.path.length > 1).map((f): Feature => ({
      type: 'Feature',
      properties: { sel: f.id === selected ? 1 : 0, attack: f.order === 'attack' ? 1 : 0 },
      geometry: { type: 'LineString', coordinates: f.path.map(([lat, lon]) => [lon, lat]) },
    })),
  }), [pic, selected])

  const battleGeo: FeatureCollection = useMemo(() => ({
    type: 'FeatureCollection',
    features: (pic?.battles ?? []).map((b) =>
      circle([b.pos[1], b.pos[0]], b.radius_m / 1000, { units: 'kilometers', steps: 48, properties: { live: b.live ? 1 : 0 } }),
    ),
  }), [pic])

  const initialView = useMemo(() => {
    const objs = pic?.objectives ?? []
    if (!objs.length) return null
    const lat = objs.reduce((a, o) => a + o.pos[0], 0) / objs.length
    const lon = objs.reduce((a, o) => a + o.pos[1], 0) / objs.length
    return { latitude: lat, longitude: lon, zoom: 7 }
    // Only the first picture sets the view.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [!!pic])

  const flyTo = (p: LatLon, zoom = 10) =>
    mapRef.current?.flyTo({ center: [p[1], p[0]], zoom, duration: 800 })

  if (isLoading) {
    return <div style={{ padding: 24, ...label }}>LOADING THE GROUND PICTURE…</div>
  }
  if (error || !pic) {
    return (
      <div style={{ padding: 24, color: 'var(--text-muted)', fontSize: '0.85rem' }}>
        <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.4rem', letterSpacing: '0.12em', color: 'var(--text)' }}>
          GROUND COMMAND UNAVAILABLE
        </div>
        {(error as Error | null)?.message ?? 'No picture from the game server.'}
      </div>
    )
  }
  if (!pic.enabled) {
    return (
      <div style={{ padding: 24, color: 'var(--text-muted)', fontSize: '0.85rem' }}>
        <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.4rem', letterSpacing: '0.12em', color: 'var(--text)' }}>
          NO GROUND WAR ON THIS SERVER
        </div>
        The dynamic ground war is switched off in this server's campaign config.
      </div>
    )
  }

  const enemyObjs = (from: LatLon) =>
    pic.objectives
      .filter((o) => o.owner !== side)
      .map((o) => ({ o, d: km(from, o.pos) }))
      .sort((a, b) => a.d - b.d)
      .slice(0, 12)
  const ownObjs = (from: LatLon) =>
    pic.objectives
      .filter((o) => o.owner === side)
      .map((o) => ({ o, d: km(from, o.pos) }))
      .sort((a, b) => a.d - b.d)
      .slice(0, 12)
  const raisable = pic.objectives
    .filter((o) => o.owner === side && (o.can_raise ?? 0) > 0)
    .sort((a, b) => a.name.localeCompare(b.name))
  const homeOwned = (f: GroundFormation) =>
    pic.objectives.find((o) => o.id === f.home && o.owner === side) ?? null

  return (
    <div style={{ display: 'flex', height: '100%', minHeight: 0 }}>
      <div style={{ flex: 1, position: 'relative', minWidth: 0 }}>
        {initialView && (
          <Map
            ref={mapRef}
            key={theme}
            mapStyle={mapStyle}
            initialViewState={initialView}
            style={{ width: '100%', height: '100%' }}
            dragRotate={false}
            attributionControl={false}
            onClick={() => setPicked(null)}
          >
            <Source id="gw-front" type="geojson" data={frontGeo}>
              <Layer
                id="gw-front"
                type="line"
                paint={{
                  'line-width': ['match', ['get', 'k'], 'mid', 2, 1.2],
                  'line-color': ['match', ['get', 'k'], 'blue', SIDE_COLOR.Blue, 'red', SIDE_COLOR.Red, '#d8d8c8'],
                  'line-opacity': 0.6,
                  'line-dasharray': [3, 2],
                }}
              />
            </Source>
            <Source id="gw-battles" type="geojson" data={battleGeo}>
              <Layer id="gw-battle-fill" type="fill" paint={{ 'fill-color': BATTLE, 'fill-opacity': ['match', ['get', 'live'], 1, 0.22, 0.1] }} />
              <Layer id="gw-battle-line" type="line" paint={{ 'line-color': BATTLE, 'line-width': 1.5, 'line-dasharray': [2, 2] }} />
            </Source>
            <Source id="gw-paths" type="geojson" data={pathGeo}>
              <Layer
                id="gw-paths"
                type="line"
                paint={{
                  'line-color': ours,
                  'line-width': ['match', ['get', 'sel'], 1, 3, 1.5],
                  'line-opacity': ['match', ['get', 'sel'], 1, 0.95, 0.55],
                  'line-dasharray': [2, 1.5],
                }}
              />
            </Source>

            {pic.objectives.map((o) => (
              <Marker
                key={`o${o.id}`}
                latitude={o.pos[0]}
                longitude={o.pos[1]}
                anchor="center"
                onClick={(e) => {
                  e.originalEvent.stopPropagation()
                  setPicked(o)
                }}
              >
                <div title={o.name} style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', cursor: 'pointer' }}>
                  <div style={{
                    width: 11, height: 11, transform: 'rotate(45deg)',
                    background: SIDE_COLOR[o.owner], border: '1px solid #000',
                    boxShadow: o.being_captured ? `0 0 0 3px ${BATTLE}` : undefined,
                  }} />
                  <div style={{
                    marginTop: 3, fontFamily: 'var(--font-mono)', fontSize: 9, whiteSpace: 'nowrap',
                    color: '#e8eadf', textShadow: '0 0 3px #000, 0 0 2px #000',
                  }}>{o.name}</div>
                </div>
              </Marker>
            ))}

            {pic.battles.map((b) => (
              <Marker key={`b${b.id}`} latitude={b.pos[0]} longitude={b.pos[1]} anchor="bottom">
                <div style={{
                  fontFamily: 'var(--font-mono)', fontSize: 9, fontWeight: 700, letterSpacing: '0.08em',
                  color: '#000', background: BATTLE, padding: '1px 5px', borderRadius: 2, whiteSpace: 'nowrap',
                  animation: b.live ? 'gwPulse 1.4s ease-in-out infinite' : undefined,
                }}>
                  ⚔ BATTLE{b.near ? ` · ${b.near}` : ''}
                </div>
              </Marker>
            ))}

            {pic.enemy.map((e, i) => (
              <Marker key={`e${i}`} latitude={e.pos[0]} longitude={e.pos[1]} anchor="center">
                <img
                  src={unitSymbol(e.kind, true, 22)}
                  title={`Enemy ${e.kind}, about ${e.approx_vehicles} vehicles`}
                  style={{ opacity: 0.85 }}
                />
              </Marker>
            ))}

            {pic.formations.map((f) => (
              <Marker
                key={`f${f.id}`}
                latitude={f.pos[0]}
                longitude={f.pos[1]}
                anchor="center"
                onClick={(e) => {
                  e.originalEvent.stopPropagation()
                  setSelected(f.id)
                }}
              >
                <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', cursor: 'pointer' }}>
                  <img
                    src={unitSymbol(formationKind(f), false, f.id === selected ? 32 : 26)}
                    style={{ filter: f.id === selected ? `drop-shadow(0 0 4px ${ours})` : undefined }}
                  />
                  <div style={{
                    fontFamily: 'var(--font-mono)', fontSize: 9, whiteSpace: 'nowrap', color: '#fff',
                    textShadow: '0 0 3px #000, 0 0 2px #000',
                  }}>{f.name.split(' (')[0]} · {f.alive}/{f.total}</div>
                </div>
              </Marker>
            ))}

            {picked && (
              <Popup
                latitude={picked.pos[0]}
                longitude={picked.pos[1]}
                anchor="top"
                closeOnClick={false}
                onClose={() => setPicked(null)}
                maxWidth="260px"
              >
                <ObjectivePopup
                  o={picked}
                  side={side}
                  sel={sel}
                  canCommand={canCommand}
                  onSend={send}
                />
              </Popup>
            )}
          </Map>
        )}
        <style>{`@keyframes gwPulse { 0%,100% { opacity: 1 } 50% { opacity: 0.45 } }
          .maplibregl-popup-content { background: var(--bg-elevated-solid); color: var(--text); border: 1px solid var(--border-light); padding: 8px 10px; }
          .maplibregl-popup-tip { display: none; }
          .maplibregl-popup-close-button { color: var(--text-dim); }`}</style>
      </div>

      <aside style={{
        width: 360, flexShrink: 0, overflowY: 'auto', padding: 10, display: 'flex', flexDirection: 'column', gap: 8,
        background: 'var(--bg-chrome)', borderLeft: '1px solid var(--border)',
      }}>
        <div style={{ ...panel, borderColor: ours }}>
          <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
            <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.35rem', letterSpacing: '0.12em' }}>
              GROUND COMMAND
            </div>
            <span style={{ marginLeft: 'auto', color: ours, fontFamily: 'var(--font-mono)', fontSize: '0.7rem', letterSpacing: '0.12em' }}>
              {side.toUpperCase()}
            </span>
          </div>
          <div style={{ ...label, marginTop: 2 }}>
            {pic.formations.length}/{pic.max_formations} FORMATIONS · {pic.battles.length} BATTLE{pic.battles.length === 1 ? '' : 'S'} · {pic.live}/{pic.max_live} IN DCS
          </div>
          {adminPick && (
            <div style={{ display: 'flex', gap: 6, marginTop: 6 }}>
              {(['Blue', 'Red'] as const).map((s) => (
                <button key={s} style={btn(SIDE_COLOR[s], false)} onClick={() => { setViewSide(s); setSelected(null) }}>
                  {viewSide === s ? '● ' : ''}VIEW {s.toUpperCase()}
                </button>
              ))}
            </div>
          )}
          {!pic.can_command && (
            <div style={{ marginTop: 6, fontSize: '0.72rem', color: 'var(--yellow)' }}>
              {pic.god_mode
                ? 'Admin view: you can watch either side, but orders need a pilot registered on a side.'
                : 'View only: link your Discord (-linkme in DCS chat) and take a slot this campaign to give orders.'}
            </div>
          )}
        </div>

        {result && (
          <div
            onClick={() => setResult(null)}
            style={{ ...panel, cursor: 'pointer', borderColor: result.ok ? 'var(--accent)' : 'var(--red)', fontSize: '0.74rem' }}
          >
            {result.ok ? '✓ ' : '✕ '}{result.text}
          </div>
        )}

        {pic.battles.length > 0 && (
          <div style={panel}>
            <div style={label}>BATTLES</div>
            {pic.battles.map((b: GroundBattle) => (
              <div
                key={b.id}
                onClick={() => flyTo(b.pos, 11)}
                style={{ display: 'flex', alignItems: 'center', gap: 6, padding: '4px 0', cursor: 'pointer', fontSize: '0.76rem' }}
              >
                <span style={{ color: BATTLE }}>⚔</span>
                <span>{b.near ? `Near ${b.near}` : 'Ground battle'}</span>
                <span style={{ marginLeft: 'auto', ...label }}>
                  {b.live ? 'LIVE · ' : ''}{since(b.since)}{b.ours.length ? ` · ${b.ours.length} OURS` : ''}
                </span>
              </div>
            ))}
          </div>
        )}

        <div style={panel}>
          <div style={label}>FORMATIONS</div>
          {pic.formations.length === 0 && (
            <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', padding: '4px 0' }}>
              None in the field. Raise one from a base below.
            </div>
          )}
          {pic.formations.map((f) => (
            <div
              key={f.id}
              onClick={() => { setSelected(f.id); flyTo(f.pos, 9) }}
              style={{
                padding: '6px 6px', margin: '4px -6px 0', borderRadius: 2, cursor: 'pointer',
                background: f.id === selected ? 'var(--bg-hover)' : undefined,
                borderLeft: `2px solid ${f.id === selected ? ours : 'transparent'}`,
              }}
            >
              <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                <img src={unitSymbol(formationKind(f), false, 16)} />
                <span style={{ fontSize: '0.8rem', fontWeight: 600 }}>{f.name}</span>
              </div>
              <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', margin: '2px 0 3px' }}>
                {orderText(f)}
                {f.posture === 'moving' && ` · ${f.km_to_go.toFixed(0)} km`}
                {f.eta_mins != null && f.posture === 'moving' && ` · ETA ${f.eta_mins} min`}
              </div>
              <StrengthBar alive={f.alive} total={f.total} />
              <div style={{ display: 'flex', flexWrap: 'wrap', gap: 4, marginTop: 4 }}>
                {f.posture === 'assaulting' && <Badge color={BATTLE}>ASSAULTING</Badge>}
                {f.engaged && <Badge color={BATTLE}>IN CONTACT</Badge>}
                {f.halted && <Badge color="var(--yellow)">HALTED</Badge>}
                {f.live && <Badge color="var(--accent-bright)">IN DCS</Badge>}
                {!f.has_infantry && <Badge color="var(--text-dim)">CAN'T CAPTURE</Badge>}
                <Badge color={f.commander ? ours : 'var(--text-dim)'}>
                  {f.commander ? `${f.commander}${f.locked_mins != null ? ` · ${f.locked_mins}m` : ''}` : 'AI'}
                </Badge>
              </div>
            </div>
          ))}
        </div>

        {sel && (
          <FormationOrders
            key={sel.id}
            f={sel}
            ours={ours}
            canCommand={canCommand}
            enemy={enemyObjs(sel.pos)}
            own={ownObjs(sel.pos)}
            home={homeOwned(sel)}
            onSend={send}
          />
        )}

        <div style={panel}>
          <div style={label}>RAISE A FORMATION</div>
          {raisable.length === 0 && (
            <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', padding: '4px 0' }}>
              No base can spare troops right now (under threat, or garrison too thin).
            </div>
          )}
          {raisable.map((o) => (
            <div key={o.id} style={{ display: 'flex', alignItems: 'center', gap: 6, padding: '3px 0', fontSize: '0.76rem' }}>
              <span style={{ cursor: 'pointer' }} onClick={() => flyTo(o.pos, 10)}>{o.name}</span>
              <span style={{ ...label }}>{o.can_raise} GROUPS</span>
              <button
                style={{ ...btn(ours, !canCommand), marginLeft: 'auto' }}
                disabled={!canCommand}
                onClick={() => send({ kind: 'raise', objective: o.id })}
              >
                RAISE
              </button>
            </div>
          ))}
        </div>

        <div style={{ ...label, lineHeight: 1.6, padding: '0 2px 8px' }}>
          Click a formation, then a base on the map to send it there. Your order
          keeps the AI off it for {Math.round(pic.player_lock_secs / 60)} min.
          Enemy formations only show where our forces are in contact.
        </div>
      </aside>
    </div>
  )
}

function FormationOrders({
  f, ours, canCommand, enemy, own, home, onSend,
}: {
  f: GroundFormation
  ours: string
  canCommand: boolean
  enemy: { o: GroundObjective; d: number }[]
  own: { o: GroundObjective; d: number }[]
  home: GroundObjective | null
  onSend: (c: GroundCommand) => void
}) {
  const [attack, setAttack] = useState<string>('')
  const [defend, setDefend] = useState<string>('')
  const fallback = home ?? own[0]?.o ?? null
  const select: CSSProperties = {
    flex: 1, minWidth: 0, background: 'var(--bg-input)', color: 'var(--text)',
    border: '1px solid var(--border-light)', fontSize: '0.72rem', padding: '3px 4px',
  }
  return (
    <div style={{ ...panel, borderColor: ours }}>
      <div style={label}>ORDERS · {f.name.toUpperCase()}</div>
      <div style={{ display: 'flex', gap: 6, marginTop: 6 }}>
        <select style={select} value={attack} onChange={(e) => setAttack(e.target.value)} disabled={!canCommand}>
          <option value="">Attack…</option>
          {enemy.map(({ o, d }) => (
            <option key={o.id} value={o.id}>{o.name} ({o.owner}, {d.toFixed(0)} km)</option>
          ))}
        </select>
        <button
          style={btn('var(--red)', !canCommand || !attack)}
          disabled={!canCommand || !attack}
          onClick={() => onSend({ kind: 'attack', formation: f.id, objective: Number(attack) })}
        >ATTACK</button>
      </div>
      <div style={{ display: 'flex', gap: 6, marginTop: 6 }}>
        <select style={select} value={defend} onChange={(e) => setDefend(e.target.value)} disabled={!canCommand}>
          <option value="">Defend / move to…</option>
          {own.map(({ o, d }) => (
            <option key={o.id} value={o.id}>{o.name} ({d.toFixed(0)} km)</option>
          ))}
        </select>
        <button
          style={btn(ours, !canCommand || !defend)}
          disabled={!canCommand || !defend}
          onClick={() => onSend({ kind: 'defend', formation: f.id, objective: Number(defend) })}
        >DEFEND</button>
      </div>
      <div style={{ display: 'flex', flexWrap: 'wrap', gap: 6, marginTop: 8 }}>
        <button style={btn('var(--text)', !canCommand)} disabled={!canCommand}
          onClick={() => onSend({ kind: 'hold', formation: f.id })}>HOLD</button>
        {fallback && (
          <button style={btn('var(--yellow)', !canCommand)} disabled={!canCommand}
            onClick={() => onSend({ kind: 'withdraw', formation: f.id, objective: fallback.id })}>
            WITHDRAW TO {fallback.name.toUpperCase()}
          </button>
        )}
        {f.commander && (
          <button style={btn('var(--text-dim)', !canCommand)} disabled={!canCommand}
            onClick={() => onSend({ kind: 'release', formation: f.id })}>HAND BACK TO AI</button>
        )}
      </div>
      {!f.has_infantry && (
        <div style={{ marginTop: 6, fontSize: '0.7rem', color: 'var(--text-muted)' }}>
          No infantry or troop carriers left: it can break a base but not take it.
        </div>
      )}
    </div>
  )
}

function ObjectivePopup({
  o, side, sel, canCommand, onSend,
}: {
  o: GroundObjective
  side: 'Blue' | 'Red'
  sel: GroundFormation | null
  canCommand: boolean
  onSend: (c: GroundCommand) => void
}) {
  const own = o.owner === side
  return (
    <div style={{ fontSize: '0.74rem', minWidth: 180 }}>
      <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.05rem', letterSpacing: '0.08em' }}>{o.name}</div>
      <div style={{ color: 'var(--text-dim)', fontSize: '0.66rem', marginBottom: 6 }}>
        {o.kind.toUpperCase()} · <span style={{ color: SIDE_COLOR[o.owner] }}>{o.owner.toUpperCase()}</span>
        {own && o.health != null && ` · ${o.health}% HEALTH`}
        {o.being_captured && ' · BEING CAPTURED'}
        {own && o.threatened && ' · THREATENED'}
      </div>
      {sel ? (
        <div style={{ display: 'flex', flexDirection: 'column', gap: 5 }}>
          <div style={{ color: 'var(--text-muted)', fontSize: '0.66rem' }}>{sel.name}:</div>
          {!own && (
            <button style={btn('var(--red)', !canCommand)} disabled={!canCommand}
              onClick={() => onSend({ kind: 'attack', formation: sel.id, objective: o.id })}>
              ATTACK {o.name.toUpperCase()}
            </button>
          )}
          {own && (
            <>
              <button style={btn(SIDE_COLOR[side], !canCommand)} disabled={!canCommand}
                onClick={() => onSend({ kind: 'defend', formation: sel.id, objective: o.id })}>
                DEFEND / MOVE HERE
              </button>
              <button style={btn('var(--yellow)', !canCommand)} disabled={!canCommand}
                onClick={() => onSend({ kind: 'withdraw', formation: sel.id, objective: o.id })}>
                WITHDRAW HERE & REFIT
              </button>
            </>
          )}
        </div>
      ) : (
        <div style={{ color: 'var(--text-muted)', fontSize: '0.66rem' }}>Select a formation to give it orders.</div>
      )}
      {own && (o.can_raise ?? 0) > 0 && (
        <button style={{ ...btn(SIDE_COLOR[side], !canCommand), marginTop: 6, width: '100%' }} disabled={!canCommand}
          onClick={() => onSend({ kind: 'raise', objective: o.id })}>
          RAISE A FORMATION HERE ({o.can_raise} GROUPS)
        </button>
      )}
    </div>
  )
}
