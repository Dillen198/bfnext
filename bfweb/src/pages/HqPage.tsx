// THEATRE HQ: the coalition's AI commander. What it is trying to do (posture,
// main effort, intent), what it has under way, what pilots have asked it for,
// how its operations have gone, and -- for whoever is cleared to -- the
// controls to take command. Everything comes from /api/hq, which the engine
// builds for the viewer's own side from what that side can see.
import { useMemo, useRef, useState, type CSSProperties, type ReactElement } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import Map, { Marker, type MapRef } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'

import {
  api,
  type HqCommand,
  type HqLine,
  type HqObjective,
  type HqOp,
  type HqOpKind,
  type HqPosture,
  type HqRequestKind,
  type HqView,
  type LatLon,
} from '../api'
import { useAuth } from '../context/AuthContext'
import { useTheme } from '../context/ThemeContext'
import { mapStyleFor } from '../lib/mapStyle'
import { hqMock, hqMockCommand } from './hqMock'

const SIDE_COLOR = { Blue: '#4a8fd4', Red: '#cc4444', Neutral: '#8a8f80' } as const
const EFFORT = '#ff8c1a'
const LINES: HqLine[] = ['air', 'fires', 'logistics', 'troops', 'ground']

const OP_LABEL: Record<HqOpKind, string> = {
  cap: 'CAP',
  strike: 'STRIKE',
  sead: 'SEAD',
  recon: 'RECON',
  artillery: 'ARTILLERY',
  missile_strike: 'MISSILES',
  ambush: 'AMBUSH',
  convoy: 'CONVOY',
  helo_supply: 'HELO SUPPLY',
  helo_troops: 'HELO TROOPS',
  reinforce: 'REINFORCE',
  bomber: 'BOMBER',
  awacs: 'AWACS',
  tanker: 'TANKER',
  naval_strike: 'NAVAL STRIKE',
  air_repair: 'AIR REPAIR',
}
const REQUESTS: { kind: HqRequestKind; label: string; friendly: boolean }[] = [
  { kind: 'cas', label: 'CAS', friendly: false },
  { kind: 'sead', label: 'SEAD', friendly: false },
  { kind: 'fires', label: 'FIRES', friendly: false },
  { kind: 'recon', label: 'RECON', friendly: false },
  { kind: 'troops', label: 'TROOPS', friendly: false },
  { kind: 'cap', label: 'CAP', friendly: true },
  { kind: 'supply', label: 'RESUPPLY', friendly: true },
  { kind: 'tanker', label: 'TANKER', friendly: true },
  { kind: 'awacs', label: 'AWACS', friendly: true },
]
const SOURCE_TEXT: Record<string, string> = {
  rules: "HQ's own assessment",
  strategist: "Strategist's directive",
  human: 'Human orders',
  '': '—',
}
const STATUS_COLOR: Record<HqOp['status'], string> = {
  active: 'var(--accent-bright)',
  succeeded: 'var(--text-muted)',
  failed: 'var(--red)',
  cancelled: 'var(--text-dim)',
}

function minsAgo(iso: string): string {
  const m = Math.max(0, Math.round((Date.now() - Date.parse(iso)) / 60_000))
  return m < 60 ? `${m}m` : `${Math.floor(m / 60)}h${m % 60}m`
}
function minsLeft(iso: string): string {
  const m = Math.max(0, Math.round((Date.parse(iso) - Date.now()) / 60_000))
  return `${m} min`
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
const select: CSSProperties = {
  flex: 1, minWidth: 0, background: 'var(--bg-input)', color: 'var(--text)',
  border: '1px solid var(--border-light)', fontSize: '0.72rem', padding: '3px 4px',
}
function btn(color: string, disabled = false, on = false): CSSProperties {
  return {
    background: on ? color : 'transparent',
    border: `1px solid ${color}`,
    color: on ? '#000' : color,
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
      padding: '1px 5px', borderRadius: 2, border: `1px solid ${color}`, color, whiteSpace: 'nowrap',
    }}>{children}</span>
  )
}

function Weights({ w, color }: { w: Partial<Record<HqLine, number>>; color: string }) {
  return (
    <div style={{ display: 'grid', gridTemplateColumns: '68px 1fr 30px', gap: '3px 6px', alignItems: 'center', marginTop: 4 }}>
      {LINES.map((l) => {
        const v = w[l] ?? 1
        return (
          <div key={l} style={{ display: 'contents' }}>
            <span style={{ ...label, fontSize: '0.58rem' }}>{l.toUpperCase()}</span>
            <div style={{ height: 4, background: 'rgba(0,0,0,0.45)', borderRadius: 1, overflow: 'hidden' }}>
              <div style={{ width: `${Math.min(100, (v / 2.5) * 100)}%`, height: '100%', background: v > 1.05 ? color : v < 0.95 ? 'var(--text-dim)' : 'var(--text-muted)' }} />
            </div>
            <span style={{ fontFamily: 'var(--font-mono)', fontSize: '0.6rem', color: 'var(--text-muted)', textAlign: 'right' }}>{v.toFixed(1)}</span>
          </div>
        )
      })}
    </div>
  )
}

export default function HqPage(): ReactElement {
  const { user } = useAuth()
  const { theme } = useTheme()
  const mapStyle = useMemo(() => mapStyleFor(theme), [theme])
  const qc = useQueryClient()
  const mapRef = useRef<MapRef>(null)
  const adminPick = !!user?.is_admin && !user?.side
  const [viewSide, setViewSide] = useState<'Blue' | 'Red'>('Blue')
  const sideParam = adminPick ? viewSide : undefined
  // Dev builds only: `?mock` renders from a fixture and never calls the API.
  const mock = import.meta.env.DEV && new URLSearchParams(location.search).has('mock')

  const { data: v, error, isLoading } = useQuery<HqView>({
    queryKey: ['hq', sideParam],
    queryFn: () => (mock ? Promise.resolve(hqMock) : api.hq.view(sideParam)),
    retry: false,
    refetchInterval: (q) => (q.state.error ? 60_000 : 10_000),
  })
  const [result, setResult] = useState<{ ok: boolean; text: string } | null>(null)
  const command = useMutation({
    mutationFn: (cmd: HqCommand) => (mock ? Promise.resolve(hqMockCommand(cmd)) : api.hq.command(cmd, sideParam)),
    onSuccess: (r) => {
      setResult({ ok: r.ok, text: r.message })
      qc.invalidateQueries({ queryKey: ['hq'] })
    },
    onError: (e: Error) => setResult({ ok: false, text: e.message }),
  })
  const send = (cmd: HqCommand) => command.mutate(cmd)

  const initialView = useMemo(() => {
    const objs = v?.picture.objectives ?? []
    if (!objs.length) return null
    const lat = objs.reduce((a, o) => a + o.pos[0], 0) / objs.length
    const lon = objs.reduce((a, o) => a + o.pos[1], 0) / objs.length
    return { latitude: lat, longitude: lon, zoom: 7 }
    // Only the first view sets the camera.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [!!v])
  const flyTo = (p: LatLon, zoom = 10) => mapRef.current?.flyTo({ center: [p[1], p[0]], zoom, duration: 800 })

  if (isLoading) return <div style={{ padding: 24, ...label }}>CONTACTING HQ…</div>
  if (error || !v) {
    return (
      <div style={{ padding: 24, color: 'var(--text-muted)', fontSize: '0.85rem' }}>
        <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.4rem', letterSpacing: '0.12em', color: 'var(--text)' }}>
          HQ UNAVAILABLE
        </div>
        {(error as Error | null)?.message ?? 'No answer from the game server.'}
      </div>
    )
  }
  if (!v.enabled) {
    return (
      <div style={{ padding: 24, color: 'var(--text-muted)', fontSize: '0.85rem' }}>
        <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.4rem', letterSpacing: '0.12em', color: 'var(--text)' }}>
          NO THEATRE HQ ON THIS SERVER
        </div>
        The AI commander (smart_commander.hq) is not switched on for {v.side} in this server's campaign config.
      </div>
    )
  }

  const side = v.side
  const ours = SIDE_COLOR[side]
  const theirs = SIDE_COLOR[side === 'Blue' ? 'Red' : 'Blue']
  const objs = v.picture.objectives
  const isEffort = (id: number) => v.main_effort?.id === id
  const isDefend = (id: number) => v.defend.some((d) => d.id === id)
  const isSupply = (id: number) => v.supply_priority.some((d) => d.id === id)
  const active = v.ops.filter((o) => o.status === 'active')
  const finished = v.ops.filter((o) => o.status !== 'active').slice(0, 10)
  // Operations on the same spot share one stacked map label.
  const opStacks = Object.values(
    active.reduce<Record<string, { pos: LatLon; ops: HqOp[] }>>((acc, op) => {
      const k = `${op.pos[0].toFixed(3)},${op.pos[1].toFixed(3)}`
      ;(acc[k] ??= { pos: op.pos, ops: [] }).ops.push(op)
      return acc
    }, {}),
  )
  const busy = command.isPending

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
          >
            {objs.map((o) => (
              <Marker key={`o${o.id}`} latitude={o.pos[0]} longitude={o.pos[1]} anchor="center">
                <ObjMark o={o} ours={ours} theirs={theirs} effort={isEffort(o.id)} defend={isDefend(o.id)} supply={isSupply(o.id)} />
              </Marker>
            ))}
            {v.picture.enemy_sams.map((c, i) => (
              <Marker key={`s${i}`} latitude={c.pos[0]} longitude={c.pos[1]} anchor="center">
                <div title={`Known air defence x${c.count}, ${c.age_mins} min old`} style={{
                  fontFamily: 'var(--font-mono)', fontSize: 8, color: theirs, border: `1px solid ${theirs}`,
                  padding: '0 3px', borderRadius: 2, background: 'rgba(0,0,0,0.55)',
                }}>SAM</div>
              </Marker>
            ))}
            {opStacks.map(({ pos, ops }) => (
              <Marker key={`op${ops[0].id}`} latitude={pos[0]} longitude={pos[1]} anchor="bottom" offset={[0, -14]}>
                <div style={{ display: 'flex', flexDirection: 'column', gap: 1, alignItems: 'center' }}>
                  {ops.map((op) => (
                    <div key={op.id} style={{
                      fontFamily: 'var(--font-mono)', fontSize: 8, fontWeight: 700, letterSpacing: '0.06em',
                      color: '#000', background: ours, padding: '1px 4px', borderRadius: 2, whiteSpace: 'nowrap',
                    }}>{OP_LABEL[op.kind]}</div>
                  ))}
                </div>
              </Marker>
            ))}
          </Map>
        )}
        <style>{`@keyframes hqPulse { 0%,100% { transform: scale(1); opacity: .9 } 50% { transform: scale(1.35); opacity: .35 } }`}</style>
      </div>

      <aside style={{
        width: 380, flexShrink: 0, overflowY: 'auto', padding: 10, display: 'flex', flexDirection: 'column', gap: 8,
        background: 'var(--bg-chrome)', borderLeft: '1px solid var(--border)',
      }}>
        <div style={{ ...panel, borderColor: ours }}>
          <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
            <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.35rem', letterSpacing: '0.12em' }}>THEATRE HQ</div>
            <span style={{ marginLeft: 'auto', color: ours, fontFamily: 'var(--font-mono)', fontSize: '0.7rem', letterSpacing: '0.12em' }}>
              {side.toUpperCase()}
            </span>
          </div>
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: 4, marginTop: 6 }}>
            {v.posture && <Badge color={v.posture === 'offensive' ? EFFORT : v.posture === 'defensive' ? 'var(--yellow)' : 'var(--text-muted)'}>{v.posture.toUpperCase()}</Badge>}
            <Badge color={v.source === 'human' ? ours : 'var(--text-muted)'}>{SOURCE_TEXT[v.source] ?? v.source}</Badge>
            {v.paused && <Badge color="var(--yellow)">PLANNING PAUSED</Badge>}
            <Badge color="var(--text-dim)">{`NEXT PASS ${v.next_think_secs}s`}</Badge>
          </div>
          {adminPick && (
            <div style={{ display: 'flex', gap: 6, marginTop: 6 }}>
              {(['Blue', 'Red'] as const).map((s) => (
                <button key={s} style={btn(SIDE_COLOR[s])} onClick={() => setViewSide(s)}>
                  {viewSide === s ? '● ' : ''}VIEW {s.toUpperCase()}
                </button>
              ))}
            </div>
          )}
        </div>

        {result && (
          <div onClick={() => setResult(null)} style={{ ...panel, cursor: 'pointer', borderColor: result.ok ? 'var(--accent)' : 'var(--red)', fontSize: '0.74rem' }}>
            {result.ok ? '✓ ' : '✕ '}{result.text}
          </div>
        )}

        <div style={panel}>
          <div style={label}>COMMANDER'S INTENT</div>
          <div style={{ fontSize: '0.86rem', lineHeight: 1.45, marginTop: 4 }}>{v.intent || '—'}</div>
          <div style={{ display: 'grid', gridTemplateColumns: 'auto 1fr', gap: '3px 8px', marginTop: 8, fontSize: '0.74rem' }}>
            <span style={label}>MAIN EFFORT</span>
            <span>{v.main_effort
              ? <a style={{ color: EFFORT, cursor: 'pointer' }} onClick={() => flyTo(v.main_effort!.pos)}>{v.main_effort.name}</a>
              : '—'}</span>
            <span style={label}>HOLD</span>
            <span>{v.defend.map((d) => d.name).join(', ') || '—'}</span>
            <span style={label}>RESUPPLY</span>
            <span>{v.supply_priority.map((d) => d.name).join(', ') || '—'}</span>
            {v.avoid.length > 0 && <><span style={label}>AVOID</span><span>{v.avoid.map((d) => d.name).join(', ')}</span></>}
          </div>
          <Weights w={v.weights} color={ours} />
          {v.directive?.directive.rationale && (
            <div style={{ marginTop: 8, fontSize: '0.72rem', color: 'var(--text-muted)', borderLeft: `2px solid var(--border-light)`, paddingLeft: 6 }}>
              <span style={label}>STRATEGIST · {minsAgo(v.directive.received)} AGO · {minsLeft(v.directive.expires)} LEFT</span>
              <div>{v.directive.directive.rationale}</div>
            </div>
          )}
          {v.override && (
            <div style={{ marginTop: 8, fontSize: '0.72rem', color: ours }}>
              Orders from {v.override.by}, standing {minsLeft(v.override.expires)} more.
            </div>
          )}
          {v.reasons.length > 0 && (
            <details style={{ marginTop: 6 }}>
              <summary style={{ ...label, cursor: 'pointer' }}>HQ ASSESSMENT</summary>
              <ul style={{ margin: '4px 0 0 16px', padding: 0, fontSize: '0.7rem', color: 'var(--text-muted)' }}>
                {v.reasons.map((r, i) => <li key={i}>{r}</li>)}
              </ul>
            </details>
          )}
        </div>

        <div style={panel}>
          <div style={label}>RESOURCES</div>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(3, 1fr)', gap: 6, marginTop: 4 }}>
            <Stat k="TREASURY" v={v.treasury.toLocaleString()} />
            <Stat k="RESERVE" v={v.reserve.toLocaleString()} />
            <Stat k="HQ EFFORT" v={`${Math.round(v.gap_factor * 100)}%`} />
            <Stat k="PILOTS" v={String(v.picture.humans)} />
            <Stat k="TERRITORY" v={`${v.picture.territory_pct}%`} />
            <Stat k="ENEMY AIR" v={String(v.picture.enemy_air_detected)} />
          </div>
          <div style={{ ...label, marginTop: 8 }}>CAN RUN</div>
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: 4, marginTop: 3 }}>
            {(Object.entries(v.available) as [HqOpKind, number][]).map(([k, cost]) => (
              <span key={k} title={`${cost} treasury`}><Badge color="var(--text-muted)">{`${OP_LABEL[k]} ${cost}`}</Badge></span>
            ))}
            {Object.keys(v.available).length === 0 && <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)' }}>Nothing configured.</span>}
          </div>
          <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)', marginTop: 6 }}>
            HQ effort falls as more pilots join: it fills the gaps, it doesn't fly over you.
          </div>
        </div>

        <div style={panel}>
          <div style={label}>OPERATIONS · {active.length} ACTIVE</div>
          {active.length === 0 && <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', padding: '4px 0' }}>Nothing under way.</div>}
          {[...active, ...finished].map((op) => (
            <div key={op.id} style={{ display: 'flex', alignItems: 'center', gap: 6, padding: '4px 0', fontSize: '0.74rem', borderTop: '1px solid var(--border)' }}>
              <span style={{ fontFamily: 'var(--font-mono)', fontSize: '0.62rem', color: STATUS_COLOR[op.status], minWidth: 74 }}>{OP_LABEL[op.kind]}</span>
              <span style={{ flex: 1, minWidth: 0, cursor: 'pointer', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}
                onClick={() => flyTo(op.pos)} title={op.detail}>
                {op.target_name}
                {op.status !== 'active' && <span style={{ color: STATUS_COLOR[op.status] }}> · {op.status}</span>}
                {op.request != null && <span style={{ color: 'var(--text-dim)' }}> · req #{op.request}</span>}
                {op.support.length > 0 && (
                  <span style={{ color: 'var(--text-dim)' }}>
                    {' · '}
                    {op.support.filter((x) => x === 'escort').length > 0 && `+${op.support.filter((x) => x === 'escort').length} ESCORT `}
                    {op.support.includes('sead') && '+SEAD'}
                  </span>
                )}
              </span>
              <span style={label}>{minsAgo(op.started)}</span>
              {op.status === 'active' && v.can_command && (
                <button style={btn('var(--red)', busy)} disabled={busy} onClick={() => send({ kind: 'cancel_op', op: op.id })}>✕</button>
              )}
            </div>
          ))}
        </div>

        <RequestPanel v={v} ours={ours} busy={busy} onSend={send} />

        {v.can_command ? (
          <CommandPanel key={v.side} v={v} ours={ours} busy={busy} onSend={send} />
        ) : (
          <div style={{ ...panel, fontSize: '0.72rem', color: 'var(--text-muted)' }}>
            {v.god_mode
              ? 'Admin view.'
              : "Only the side's designated commanders can override the HQ. Support requests are open to every pilot on the side."}
          </div>
        )}

        {v.record.length > 0 && (
          <div style={panel}>
            <div style={label}>RECORD</div>
            {v.record.map((r) => {
              const done = r.succeeded + r.failed
              return (
                <div key={r.kind} style={{ display: 'flex', gap: 6, fontSize: '0.72rem', padding: '2px 0' }}>
                  <span style={{ fontFamily: 'var(--font-mono)', fontSize: '0.62rem', minWidth: 90 }}>{OP_LABEL[r.kind]}</span>
                  <span style={{ color: 'var(--text-muted)' }}>{r.launched} flown</span>
                  <span style={{ marginLeft: 'auto', color: done && r.failed > r.succeeded ? 'var(--red)' : 'var(--text)' }}>
                    {done ? `${Math.round((r.succeeded / done) * 100)}% success` : '—'}
                  </span>
                </div>
              )
            })}
          </div>
        )}

        <div style={panel}>
          <div style={label}>HQ LOG</div>
          {v.log.slice(0, 15).map((l, i) => (
            <div key={i} style={{ fontSize: '0.68rem', color: 'var(--text-muted)', padding: '2px 0' }}>
              <span style={{ ...label, marginRight: 6 }}>{minsAgo(l.at)}</span>{l.text.replace(/^HQ: /, '')}
            </div>
          ))}
        </div>
      </aside>
    </div>
  )
}

function Stat({ k, v }: { k: string; v: string }) {
  return (
    <div>
      <div style={{ ...label, fontSize: '0.55rem' }}>{k}</div>
      <div style={{ fontFamily: 'var(--font-mono)', fontSize: '0.86rem' }}>{v}</div>
    </div>
  )
}

function ObjMark({ o, ours, theirs, effort, defend, supply }: {
  o: HqObjective; ours: string; theirs: string; effort: boolean; defend: boolean; supply: boolean
}) {
  const color = o.owner === 'own' ? ours : o.owner === 'enemy' ? theirs : SIDE_COLOR.Neutral
  const tags = [effort && 'MAIN EFFORT', defend && 'HOLD', supply && 'RESUPPLY', o.being_captured && 'FALLING'].filter(Boolean) as string[]
  return (
    <div title={`${o.name} · ${o.kind} · health ${o.health}% · logi ${o.logi}% · supply ${o.supply}%`}
      style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', position: 'relative' }}>
      {effort && (
        <div style={{
          position: 'absolute', top: -7, width: 25, height: 25, borderRadius: '50%',
          border: `2px solid ${EFFORT}`, animation: 'hqPulse 1.6s ease-in-out infinite',
        }} />
      )}
      <div style={{
        width: 11, height: 11, transform: 'rotate(45deg)', background: color, border: '1px solid #000',
        boxShadow: defend ? `0 0 0 2px ${ours}` : o.capturable && o.owner === 'enemy' ? `0 0 0 2px ${EFFORT}` : undefined,
      }} />
      <div style={{
        marginTop: 3, fontFamily: 'var(--font-mono)', fontSize: 9, whiteSpace: 'nowrap',
        color: '#e8eadf', textShadow: '0 0 3px #000, 0 0 2px #000',
      }}>{o.name}</div>
      {tags.length > 0 && (
        <div style={{ fontFamily: 'var(--font-mono)', fontSize: 8, fontWeight: 700, color: effort ? EFFORT : ours, textShadow: '0 0 3px #000' }}>
          {tags.join(' · ')}
        </div>
      )}
    </div>
  )
}

function RequestPanel({ v, ours, busy, onSend }: { v: HqView; ours: string; busy: boolean; onSend: (c: HqCommand) => void }) {
  const [kind, setKind] = useState<HqRequestKind>('cas')
  const [target, setTarget] = useState<string>('')
  const friendly = REQUESTS.find((r) => r.kind === kind)?.friendly ?? false
  const choices = v.picture.objectives
    .filter((o) => (friendly ? o.owner === 'own' : o.owner === 'enemy'))
    .sort((a, b) => a.front_km - b.front_km)
  const open = v.requests.filter((r) => r.status === 'open')
  const recent = v.requests.filter((r) => r.status !== 'open').slice(0, 5)
  return (
    <div style={panel}>
      <div style={label}>SUPPORT REQUESTS</div>
      {v.can_request ? (
        <>
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: 4, marginTop: 6 }}>
            {REQUESTS.map((r) => (
              <button key={r.kind} style={btn(ours, false, kind === r.kind)} onClick={() => { setKind(r.kind); setTarget('') }}>{r.label}</button>
            ))}
          </div>
          <div style={{ display: 'flex', gap: 6, marginTop: 6 }}>
            <select style={select} value={target} onChange={(e) => setTarget(e.target.value)}>
              <option value="">{friendly ? 'Friendly objective…' : 'Enemy objective…'}</option>
              {choices.map((o) => <option key={o.id} value={o.id}>{o.name} ({o.front_km.toFixed(0)} km from the front)</option>)}
            </select>
            <button style={btn(ours, busy || !target)} disabled={busy || !target}
              onClick={() => onSend({ kind: 'request', request: kind, objective: Number(target) })}>ASK HQ</button>
          </div>
        </>
      ) : (
        <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginTop: 4 }}>
          Link your Discord (-linkme in DCS chat) and fly for this side to ask for support. In game: F10 &gt; Info &gt; HQ, or -request.
        </div>
      )}
      {[...open, ...recent].map((r) => (
        <div key={r.id} style={{ display: 'flex', alignItems: 'center', gap: 6, padding: '4px 0', fontSize: '0.72rem', borderTop: '1px solid var(--border)', marginTop: 4 }}>
          <span style={{ fontFamily: 'var(--font-mono)', fontSize: '0.62rem', minWidth: 52, color: r.status === 'open' ? 'var(--yellow)' : 'var(--text-dim)' }}>
            #{r.id} {r.kind.toUpperCase()}
          </span>
          <span style={{ flex: 1, minWidth: 0 }} title={r.answer}>
            {r.target_name} <span style={{ color: 'var(--text-dim)' }}>· {r.by} · {r.status}</span>
          </span>
          <span style={label}>{minsAgo(r.created)}</span>
          {r.status === 'open' && (v.can_request || v.can_command) && (
            <button style={btn('var(--text-dim)', busy)} disabled={busy} title="Withdraw"
              onClick={() => onSend({ kind: 'cancel_request', request_id: r.id })}>✕</button>
          )}
        </div>
      ))}
    </div>
  )
}

function CommandPanel({ v, ours, busy, onSend }: { v: HqView; ours: string; busy: boolean; onSend: (c: HqCommand) => void }) {
  const standing = v.override?.directive
  const [posture, setPosture] = useState<HqPosture | null>(standing?.posture ?? null)
  const [effort, setEffort] = useState<string>(standing?.main_effort != null ? String(standing.main_effort) : '')
  const [defend, setDefend] = useState<string>(standing?.defend?.[0] != null ? String(standing.defend[0]) : '')
  const [disabled, setDisabled] = useState<HqOpKind[]>(v.override?.disabled_ops ?? [])
  const [hours, setHours] = useState<number>(2)
  const enemy = v.picture.objectives.filter((o) => o.owner === 'enemy').sort((a, b) => a.front_km - b.front_km)
  const own = v.picture.objectives.filter((o) => o.owner === 'own').sort((a, b) => a.front_km - b.front_km)
  const kinds = Object.keys(v.available) as HqOpKind[]
  const order = (paused: boolean) =>
    onSend({
      kind: 'override',
      directive: {
        posture,
        main_effort: effort ? Number(effort) : null,
        defend: defend ? [Number(defend)] : [],
        ttl_secs: hours * 3600,
      },
      disabled_ops: disabled,
      paused,
    })
  return (
    <div style={{ ...panel, borderColor: ours }}>
      <div style={label}>TAKE COMMAND</div>
      <div style={{ display: 'flex', gap: 4, marginTop: 6 }}>
        {(['offensive', 'balanced', 'defensive'] as const).map((p) => (
          <button key={p} style={btn(ours, false, posture === p)} onClick={() => setPosture(posture === p ? null : p)}>{p.toUpperCase()}</button>
        ))}
      </div>
      <div style={{ display: 'flex', gap: 6, marginTop: 6 }}>
        <select style={select} value={effort} onChange={(e) => setEffort(e.target.value)}>
          <option value="">Main effort: HQ's call</option>
          {enemy.map((o) => <option key={o.id} value={o.id}>{o.name} ({o.health}% · {o.capturable ? 'no logi' : `logi ${o.logi}%`})</option>)}
        </select>
      </div>
      <div style={{ display: 'flex', gap: 6, marginTop: 6 }}>
        <select style={select} value={defend} onChange={(e) => setDefend(e.target.value)}>
          <option value="">Hold: HQ's call</option>
          {own.map((o) => <option key={o.id} value={o.id}>{o.name}{o.threatened ? ' (threatened)' : ''}</option>)}
        </select>
      </div>
      <div style={{ ...label, marginTop: 8 }}>OPERATIONS THE HQ MAY RUN</div>
      <div style={{ display: 'flex', flexWrap: 'wrap', gap: 4, marginTop: 4 }}>
        {kinds.map((k) => {
          const off = disabled.includes(k)
          return (
            <button key={k} style={btn(off ? 'var(--text-dim)' : ours, false, !off)} title={`${v.available[k]} treasury`}
              onClick={() => setDisabled(off ? disabled.filter((d) => d !== k) : [...disabled, k])}>
              {OP_LABEL[k]}
            </button>
          )
        })}
      </div>
      <div style={{ display: 'flex', alignItems: 'center', gap: 6, marginTop: 8 }}>
        <span style={label}>STANDS FOR</span>
        <select style={{ ...select, flex: 'none', width: 70 }} value={hours} onChange={(e) => setHours(Number(e.target.value))}>
          {[1, 2, 4].map((h) => <option key={h} value={h}>{h} h</option>)}
        </select>
      </div>
      <div style={{ display: 'flex', flexWrap: 'wrap', gap: 6, marginTop: 8 }}>
        <button style={btn(ours, busy)} disabled={busy} onClick={() => order(false)}>ISSUE ORDERS</button>
        <button style={btn('var(--yellow)', busy)} disabled={busy} onClick={() => order(!v.paused)}>
          {v.paused ? 'RESUME PLANNING' : 'PAUSE PLANNING'}
        </button>
        {v.override && (
          <button style={btn('var(--text-dim)', busy)} disabled={busy} onClick={() => onSend({ kind: 'clear_override' })}>HAND BACK TO HQ</button>
        )}
      </div>
      <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)', marginTop: 6 }}>
        Your orders outrank the strategist and the HQ's own judgement for as long as they stand. Anything left on "HQ's call" the HQ still decides.
      </div>
    </div>
  )
}
