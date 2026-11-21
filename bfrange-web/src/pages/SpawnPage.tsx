/**
 * `/spawn` — put adversaries, targets, tankers and ships on the range, and
 * take them off again. Laid out like a console: the catalogue on the left
 * (search it, or narrow it by category), the one item you picked set up on
 * the right with the spawn button, and what is up now under it. Every form
 * is built from the engine's catalogue (`ParamSpec`), so a new catalogue
 * item needs no site change. The last few spawns are kept per browser for
 * one-click "again".
 */
import { useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import { useMutation, useQueryClient, useQuery } from '@tanstack/react-query'
import { Alert, RotateCcw, Search, Trash2 as Trash, X } from '@icons'
import { api } from '../api'
import { LinkGuidance, LoginPrompt } from '../components/AuthGate'
import { Select } from '../components/Controls'
import { Empty, ErrorState, Loading } from '../components/States'
import { useAuth } from '../context/AuthContext'
import { useToast } from '../context/ToastContext'
import { CATEGORY_LABEL, fmtAgo } from '../lib/format'
import { useLive } from '../lib/useLive'
import { useNow } from '../lib/useNow'
import { usePref } from '../lib/usePref'
import type { CatalogItem, LiveSpawn, ParamSpec, SpawnCategory } from '../types'

const ORDER: SpawnCategory[] = ['air_to_air', 'air_to_ground', 'sead', 'ew', 'tanker', 'naval', 'ground', 'helo', 'jtac']

interface Recent {
  item: string
  params: Record<string, string>
}
const isRecents = (v: unknown): v is Recent[] =>
  Array.isArray(v) && v.every(r => r && typeof r.item === 'string' && r.params && typeof r.params === 'object')

/** "Su-27 ×2 · 40 nm": the chosen values, as the spawn list will show them. */
function summary(item: CatalogItem, params: Record<string, string>): string {
  return item.params
    .map(p => {
      const v = params[p.key] ?? p.default
      if (p.kind.type === 'choice') return p.kind.options.find(o => o.value === v)?.label ?? v
      return `${Number(v).toLocaleString('en-US')} ${p.kind.unit}`.trim()
    })
    .join(' · ')
}

function ParamField({ p, value, onChange, disabled }: { p: ParamSpec; value: string; onChange: (v: string) => void; disabled: boolean }) {
  if (p.kind.type === 'choice') {
    // a few options read better as buttons than as a menu
    if (p.kind.options.length <= 3 && p.kind.options.reduce((n, o) => n + o.label.length, 0) <= 26) {
      return (
        <div className="field">
          <span>{p.label}</span>
          <div className="seg w-full" role="group" aria-label={p.label}>
            {p.kind.options.map(o => (
              <button key={o.value} className="flex-1" aria-pressed={value === o.value} disabled={disabled} onClick={() => onChange(o.value)}>{o.label}</button>
            ))}
          </div>
        </div>
      )
    }
    return (
      <label className="field">
        <span>{p.label}</span>
        <select className="select-range" value={value} disabled={disabled} onChange={e => onChange(e.target.value)}>
          {p.kind.options.map(o => <option key={o.value} value={o.value}>{o.label}</option>)}
        </select>
      </label>
    )
  }
  const k = p.kind
  const n = Number(value)
  return (
    <label className="field">
      <span className="flex items-baseline gap-2">
        {p.label}
        <span className="ml-auto mono" style={{ color: 'var(--chalk)' }}>{Number.isFinite(n) ? n.toLocaleString('en-US') : '—'} {k.unit}</span>
      </span>
      <input
        type="range"
        className="scrub"
        min={k.min}
        max={k.max}
        step={k.step}
        value={Number.isFinite(n) ? n : k.min}
        disabled={disabled}
        onChange={e => onChange(e.target.value)}
        aria-label={p.label}
      />
      <span className="flex justify-between dim mono text-[10.5px]"><span>{k.min.toLocaleString('en-US')}</span><span>{k.max.toLocaleString('en-US')}</span></span>
    </label>
  )
}

function Configure({ item, initial, canSpawn, admin, airborne, full, pending, onSpawn, onClose }: {
  item: CatalogItem
  initial?: Record<string, string>
  canSpawn: boolean
  admin: boolean
  /** null = cannot tell (not linked or the range is offline) */
  airborne: boolean | null
  full: boolean
  pending: boolean
  onSpawn: (params: Record<string, string>) => void
  onClose: () => void
}) {
  const defaults = () => Object.fromEntries(item.params.map(p => [p.key, initial?.[p.key] ?? p.default]))
  const [params, setParams] = useState<Record<string, string>>(defaults)
  const locked = item.instructor_only && !admin
  const notUp = item.relative_to_player && airborne === false
  const why = locked ? 'Instructors only' : !canSpawn ? 'Log in and link your DCS account to spawn' : full ? 'You are at your limit: despawn something first' : ''
  const disabled = !!why
  return (
    <section className="panel spawn-cfg">
      <header className="panel-h">
        <h2 className="caps m-0">{CATEGORY_LABEL[item.category]}</h2>
        <button className="btn-range ghost sm right" onClick={onClose} aria-label="Close"><X size={14} /></button>
      </header>
      <div className="panel-b flex flex-col gap-3">
        <div>
          <div className="flex items-center gap-2 flex-wrap">
            <span className="font-semibold text-[16px]">{item.label}</span>
            {item.relative_to_player && <span className="chip outline" title="Spawns around your aircraft">AROUND YOU</span>}
            {item.instructor_only && <span className="chip warn">INSTRUCTOR</span>}
          </div>
          <p className="muted text-[13px] mt-1 mb-0">{item.description}</p>
        </div>
        {notUp && (
          <div className="spawn-warn">
            <Alert size={15} />
            <span>This one is placed around your aircraft, and the range does not see you airborne. Get in a slot and take off first, or the engine will refuse it.</span>
          </div>
        )}
        {item.params.length > 0 && (
          <div className="flex flex-col gap-3">
            {item.params.map(p => (
              <ParamField key={p.key} p={p} value={params[p.key] ?? p.default} disabled={disabled} onChange={v => setParams(s => ({ ...s, [p.key]: v }))} />
            ))}
            <button className="text-[12px] muted self-start hover:text-[var(--chalk)]" onClick={() => setParams(Object.fromEntries(item.params.map(p => [p.key, p.default])))} disabled={disabled}>
              Reset to defaults
            </button>
          </div>
        )}
        <div className="tip" data-tip={why || undefined}>
          <button className="btn-range primary w-full justify-center h-10 text-[14px]" disabled={disabled || pending} onClick={() => onSpawn(params)}>
            {pending ? 'Spawning…' : `Spawn ${item.label}`}
          </button>
        </div>
        {why && <p className="dim text-[12px] m-0 text-center">{why}</p>}
      </div>
    </section>
  )
}

function SpawnRow({ s, onDespawn, busy, onMap = true }: { s: LiveSpawn; onDespawn?: () => void; busy: boolean; onMap?: boolean }) {
  const now = useNow(5_000)
  const total = s.expires ? new Date(s.expires).getTime() - new Date(s.created).getTime() : 0
  const left = s.expires ? Math.max(0, new Date(s.expires).getTime() - now) : 0
  const pct = total > 0 ? Math.max(0, Math.min(1, left / total)) : null
  return (
    <li className="px-3 py-2.5 border-b border-[var(--line)] last:border-b-0">
      <div className="flex items-start gap-2">
        <span className="mt-1.5" style={{ width: 8, height: 8, background: 'var(--wave)', transform: 'rotate(45deg)', flex: 'none' }} />
        <div className="min-w-0 flex-1">
          <div className="text-[13px] font-medium truncate">
            {onMap ? <Link to={`/?focus=${encodeURIComponent(`spawn:${s.id}`)}`} className="hover:underline" title="Show on the map">{s.label}</Link> : s.label}
          </div>
          <div className="text-[11.5px] muted mono">
            {s.owner_name} · {s.units} unit{s.units === 1 ? '' : 's'} · {fmtAgo(s.created)}
            {pct !== null && ` · ${Math.ceil(left / 60_000)} min left`}
          </div>
        </div>
        {onDespawn && (
          <button className="btn-range danger sm" onClick={onDespawn} disabled={busy} aria-label={`Despawn ${s.label}`} title="Despawn">
            <Trash size={13} />
          </button>
        )}
      </div>
      {pct !== null && (
        <div className="spawn-life" aria-hidden><span style={{ width: `${pct * 100}%`, background: pct < 0.15 ? 'var(--wave)' : 'var(--ball)' }} /></div>
      )}
    </li>
  )
}

export default function SpawnPage() {
  const { me, loading } = useAuth()
  const toast = useToast()
  const qc = useQueryClient()
  const catalog = useQuery({ queryKey: ['catalog'], queryFn: () => api.catalog(), staleTime: 5 * 60_000 })
  const live = useLive(5000)
  const canSpawn = !!me?.logged_in && !!me.ucid
  const admin = !!me?.admin
  const [cat, setCat] = useState<SpawnCategory | 'all'>('all')
  const [text, setText] = useState('')
  const [pick, setPick] = useState<{ id: string; params?: Record<string, string>; n: number } | null>(null)
  const [station, setStation] = useState('')
  const [recents, setRecents] = usePref<Recent[]>('range.spawn.recent', [], isRecents)

  const items = useMemo(() => catalog.data?.items ?? [], [catalog.data])
  const byId = useMemo(() => new Map(items.map(i => [i.id, i])), [items])
  const counts = useMemo(() => {
    const m = new Map<SpawnCategory, number>()
    items.forEach(i => m.set(i.category, (m.get(i.category) ?? 0) + 1))
    return m
  }, [items])
  const shown = useMemo(() => {
    const t = text.trim().toLowerCase()
    return ORDER.flatMap(c => items.filter(i => i.category === c))
      .filter(i => cat === 'all' || i.category === cat)
      .filter(i => !t || `${i.label} ${i.description} ${CATEGORY_LABEL[i.category]}`.toLowerCase().includes(t))
  }, [items, cat, text])

  const spawn = useMutation({
    mutationFn: ({ item, params }: { item: CatalogItem; params: Record<string, string> }) => api.spawn(item.id, params),
    onSuccess: (r, { item, params }) => {
      toast(r.message, r.ok, r.ok && r.spawn_id ? { label: 'Show on the map', to: `/?focus=${encodeURIComponent(`spawn:${r.spawn_id}`)}` } : undefined)
      if (r.ok) {
        const key = JSON.stringify([item.id, params])
        setRecents(rs => [{ item: item.id, params }, ...rs.filter(x => JSON.stringify([x.item, x.params]) !== key)].slice(0, 5))
      }
      qc.invalidateQueries({ queryKey: ['live'] })
    },
    onError: e => toast(e instanceof Error ? e.message : String(e), false),
  })
  const despawn = useMutation({
    mutationFn: (target: { spawn_id: string } | 'all') => api.despawn(target),
    onSuccess: r => {
      toast(r.message, r.ok)
      qc.invalidateQueries({ queryKey: ['live'] })
    },
    onError: e => toast(e instanceof Error ? e.message : String(e), false),
  })
  const reset = useMutation({
    mutationFn: (st: string) => api.resetStation(st),
    onSuccess: r => toast(r.message, r.ok),
    onError: e => toast(e instanceof Error ? e.message : String(e), false),
  })

  const lv = live.data?.live
  const spawns = lv?.spawns ?? []
  const mine = spawns.filter(s => me?.ucid && s.owner_ucid === me.ucid)
  const others = spawns.filter(s => !(me?.ucid && s.owner_ucid === me.ucid))
  const cfg = catalog.data
  const full = !!cfg && mine.length >= cfg.max_active_per_player
  const meNow = me?.ucid && lv ? lv.players.find(p => p.ucid === me.ucid) : undefined
  const airborne = !canSpawn || !lv ? null : !!meNow?.in_air
  const picked = pick ? byId.get(pick.id) : undefined
  const usableRecents = recents.filter(r => byId.has(r.item))

  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <h1 className="display">Spawn</h1>
          <p className="sub m-0 mt-1">
            Put adversaries, targets, tankers and ships on the range.
            {cfg && <> Up to <b>{cfg.max_active_per_player}</b> at a time per pilot; each goes after <b>{Math.round(cfg.despawn_after_s / 60)} min</b> or when you leave your slot.</>}
          </p>
        </div>
        {canSpawn && cfg && (
          <div className="actions">
            <span className={`chip ${full ? 'bad' : 'outline'}`} style={{ height: 26, fontSize: 12 }}>{mine.length}/{cfg.max_active_per_player} ACTIVE</span>
            {lv && <span className={`chip ${meNow?.in_air ? 'live' : 'outline'}`} style={{ height: 26, fontSize: 12 }}>{meNow ? (meNow.in_air ? 'YOU ARE AIRBORNE' : 'YOU ARE ON THE GROUND') : 'NOT IN A SLOT'}</span>}
          </div>
        )}
      </div>

      {!loading && !me?.logged_in && <div className="mb-4"><LoginPrompt why="to spawn on the range" /></div>}
      {!loading && me?.logged_in && !me.ucid && <div className="mb-4"><LinkGuidance /></div>}

      <div className="grid gap-4 lg:grid-cols-[minmax(0,1fr)_380px] items-start">
        <div className="min-w-0 flex flex-col gap-3">
          {usableRecents.length > 0 && canSpawn && (
            <div className="flex flex-wrap items-center gap-2">
              <span className="caps flex items-center gap-1.5"><RotateCcw size={12} /> Again</span>
              {usableRecents.map(r => {
                const it = byId.get(r.item)!
                return (
                  <button key={JSON.stringify(r)} className="recent-chip" onClick={() => setPick({ id: it.id, params: r.params, n: Date.now() })} title={`Set up ${it.label} as last time`}>
                    <b>{it.label}</b> <span className="muted">{summary(it, r.params)}</span>
                  </button>
                )
              })}
            </div>
          )}

          <div className="panel">
            <div className="flex flex-wrap items-center gap-2 p-2 border-b border-[var(--line)]">
              <label className="spawn-search shrink-0">
                <Search size={14} className="muted" />
                <input value={text} onChange={e => setText(e.target.value)} placeholder="Search the catalogue" aria-label="Search the catalogue" />
              </label>
              <div className="flex gap-1 overflow-x-auto min-w-0 flex-1" style={{ scrollbarWidth: 'none' }}>
                <button className="cat-chip" aria-pressed={cat === 'all'} onClick={() => setCat('all')}>All <span className="mono">{items.length}</span></button>
                {ORDER.filter(c => counts.get(c)).map(c => (
                  <button key={c} className="cat-chip" aria-pressed={cat === c} onClick={() => setCat(c)}>
                    {CATEGORY_LABEL[c].replace(/ adversaries| targets| forces| drills/, '')} <span className="mono">{counts.get(c)}</span>
                  </button>
                ))}
              </div>
            </div>
            {catalog.isLoading ? (
              <Loading label="Loading the catalogue" />
            ) : catalog.error ? (
              <div className="p-3"><ErrorState error={catalog.error} retry={() => catalog.refetch()} /></div>
            ) : shown.length === 0 ? (
              <Empty title="Nothing matches">Try another word, or All.</Empty>
            ) : (
              <ul className="m-0 p-1 list-none">
                {shown.map((i, n) => {
                  const head = cat === 'all' && !text && (n === 0 || shown[n - 1].category !== i.category)
                  const locked = i.instructor_only && !admin
                  return (
                    <li key={i.id}>
                      {head && <div className="spawn-group">{CATEGORY_LABEL[i.category]}</div>}
                      <button className={`spawn-item${pick?.id === i.id ? ' sel' : ''}`} onClick={() => setPick({ id: i.id, n: Date.now() })} style={{ opacity: locked ? 0.55 : 1 }}>
                        <span className="min-w-0 flex-1">
                          <span className="flex items-center gap-2 flex-wrap">
                            <span className="font-semibold text-[13.5px]">{i.label}</span>
                            {i.relative_to_player && <span className="chip outline" style={{ height: 17, fontSize: 9.5 }}>AROUND YOU</span>}
                            {i.instructor_only && <span className="chip warn" style={{ height: 17, fontSize: 9.5 }}>INSTRUCTOR</span>}
                          </span>
                          <span className="block muted text-[12.5px] truncate">{i.description}</span>
                        </span>
                        <span className="dim text-[11.5px] shrink-0 hidden sm:inline">{i.params.length ? `${i.params.length} setting${i.params.length === 1 ? '' : 's'}` : 'one click'}</span>
                      </button>
                    </li>
                  )
                })}
              </ul>
            )}
          </div>
        </div>

        <aside className="flex flex-col gap-3 lg:sticky lg:top-[68px] w-full min-w-0">
          {picked ? (
            <Configure
              key={`${picked.id}/${pick?.n}`}
              item={picked}
              initial={pick?.params}
              canSpawn={canSpawn}
              admin={admin}
              airborne={airborne}
              full={full}
              pending={spawn.isPending && spawn.variables?.item.id === picked.id}
              onSpawn={params => spawn.mutate({ item: picked, params })}
              onClose={() => setPick(null)}
            />
          ) : (
            <div className="panel panel-b text-center py-6">
              <div className="caps" style={{ color: 'var(--chalk)' }}>Pick something to spawn</div>
              <p className="muted text-[13px] mt-1.5 mb-0">Choose from the catalogue to set it up here. Items marked <span className="chip outline" style={{ height: 17, fontSize: 9.5 }}>AROUND YOU</span> appear around your aircraft, so be airborne; everything else goes to its range station.</p>
            </div>
          )}

          <section className="panel">
            <header className="panel-h">
              <h2 className="caps m-0">Mine on the range{cfg ? ` · ${mine.length}/${cfg.max_active_per_player}` : ''}</h2>
              {mine.length > 1 && (
                <div className="right"><button className="btn-range danger sm" onClick={() => despawn.mutate('all')} disabled={despawn.isPending}>Despawn all</button></div>
              )}
            </header>
            {!canSpawn ? (
              <Empty title="Not linked">Your spawns show here once you are logged in with a linked DCS account.</Empty>
            ) : !lv ? (
              <Empty title="Range offline">{live.data?.reason ?? 'No live picture from the range server.'}</Empty>
            ) : mine.length === 0 ? (
              <Empty title="Nothing up">Whatever you spawn shows here with the time it has left.</Empty>
            ) : (
              <ul className="m-0 p-0 list-none">
                {mine.map(s => <SpawnRow key={s.id} s={s} busy={despawn.isPending} onDespawn={() => despawn.mutate({ spawn_id: s.id })} />)}
              </ul>
            )}
          </section>

          {others.length > 0 && (
            <section className="panel">
              <header className="panel-h"><h2 className="caps m-0">Others on the range · {others.length}</h2></header>
              <ul className="m-0 p-0 list-none">
                {others.map(s => <SpawnRow key={s.id} s={s} busy={despawn.isPending} onDespawn={admin ? () => despawn.mutate({ spawn_id: s.id }) : undefined} />)}
              </ul>
            </section>
          )}

          {admin && (
            <section className="panel">
              <header className="panel-h"><h2 className="caps m-0">Instructor</h2></header>
              <div className="panel-b flex flex-col gap-2">
                <Select
                  label="Reset a station (respawn all its targets)"
                  value={station}
                  onChange={setStation}
                  options={[{ value: '', label: 'Choose a station' }, ...(lv?.stations ?? []).map(s => ({ value: s.id, label: s.name }))]}
                />
                <button className="btn-range" disabled={!station || reset.isPending} onClick={() => reset.mutate(station)}>Reset station</button>
              </div>
            </section>
          )}
        </aside>
      </div>
    </div>
  )
}
