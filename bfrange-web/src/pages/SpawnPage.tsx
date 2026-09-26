/**
 * `/spawn` — put adversaries, targets, tankers and ships on the range, and
 * take them off again. Every form is built from the engine's catalogue
 * (`ParamSpec`), so a new catalogue item needs no site change.
 */
import { useMemo, useState } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { Info, Trash2 as Trash } from '@icons'
import { api } from '../api'
import { LinkGuidance, LoginPrompt } from '../components/AuthGate'
import { Panel, Select } from '../components/Controls'
import { Empty, ErrorState, Loading } from '../components/States'
import { useAuth } from '../context/AuthContext'
import { useToast } from '../context/ToastContext'
import { CATEGORY_LABEL, fmtAgo } from '../lib/format'
import { useNow } from '../lib/useNow'
import type { CatalogItem, LiveSpawn, ParamSpec, SpawnCategory } from '../types'

const ORDER: SpawnCategory[] = ['air_to_air', 'air_to_ground', 'tanker', 'naval', 'ground', 'helo', 'jtac']

function ParamField({ p, value, onChange, disabled }: { p: ParamSpec; value: string; onChange: (v: string) => void; disabled: boolean }) {
  if (p.kind.type === 'choice') {
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
    </label>
  )
}

function ItemCard({ item, canSpawn, admin, onSpawn, pending }: {
  item: CatalogItem
  canSpawn: boolean
  admin: boolean
  onSpawn: (item: CatalogItem, params: Record<string, string>) => void
  pending: boolean
}) {
  const [params, setParams] = useState<Record<string, string>>(() => Object.fromEntries(item.params.map(p => [p.key, p.default])))
  const locked = item.instructor_only && !admin
  const disabled = !canSpawn || locked
  const why = locked ? 'Instructors only' : !canSpawn ? 'Log in and link your DCS account to spawn' : ''
  return (
    <div className="panel p-3 flex flex-col gap-3" style={{ opacity: locked ? 0.6 : 1 }}>
      <div>
        <div className="flex items-center gap-2 flex-wrap">
          <span className="font-semibold text-[14.5px]">{item.label}</span>
          {item.relative_to_player && <span className="chip outline" title="Spawns around your aircraft: be airborne in DCS first">RELATIVE TO YOU</span>}
          {item.instructor_only && <span className="chip warn">INSTRUCTOR</span>}
        </div>
        <p className="muted text-[12.5px] mt-1 mb-0">{item.description}</p>
      </div>
      {item.params.length > 0 && (
        <div className="grid gap-2.5 sm:grid-cols-2">
          {item.params.map(p => (
            <ParamField key={p.key} p={p} value={params[p.key] ?? p.default} disabled={disabled} onChange={v => setParams(s => ({ ...s, [p.key]: v }))} />
          ))}
        </div>
      )}
      <div className="mt-auto tip" data-tip={why || undefined}>
        <button className="btn-range primary w-full justify-center" disabled={disabled || pending} onClick={() => onSpawn(item, params)}>
          {pending ? 'Spawning…' : `Spawn ${item.label}`}
        </button>
      </div>
    </div>
  )
}

function SpawnRow({ s, onDespawn, busy }: { s: LiveSpawn; onDespawn: () => void; busy: boolean }) {
  const now = useNow()
  const expiresIn = s.expires ? Math.max(0, Math.round((new Date(s.expires).getTime() - now) / 60_000)) : null
  return (
    <li className="flex items-start gap-2 px-3 py-2.5 border-b border-[var(--line)] last:border-b-0">
      <span className="mt-1.5" style={{ width: 8, height: 8, background: 'var(--wave)', transform: 'rotate(45deg)', flex: 'none' }} />
      <div className="min-w-0 flex-1">
        <div className="text-[13px] font-medium">{s.label}</div>
        <div className="text-[11.5px] muted mono">
          {s.owner_name} · {s.units} unit{s.units === 1 ? '' : 's'} · spawned {fmtAgo(s.created)}
          {expiresIn !== null && ` · despawns in ${expiresIn} min`}
        </div>
      </div>
      <button className="btn-range danger sm" onClick={onDespawn} disabled={busy} aria-label={`Despawn ${s.label}`}>
        <Trash size={13} /> Despawn
      </button>
    </li>
  )
}

export default function SpawnPage() {
  const { me, loading } = useAuth()
  const toast = useToast()
  const qc = useQueryClient()
  const catalog = useQuery({ queryKey: ['catalog'], queryFn: () => api.catalog(), staleTime: 5 * 60_000 })
  const live = useQuery({ queryKey: ['live'], queryFn: () => api.live(), refetchInterval: 5000 })
  const canSpawn = !!me?.logged_in && !!me.ucid
  const admin = !!me?.admin
  const [pendingId, setPendingId] = useState<string | null>(null)
  const [station, setStation] = useState('')

  const spawn = useMutation({
    mutationFn: ({ item, params }: { item: CatalogItem; params: Record<string, string> }) => api.spawn(item.id, params),
    onMutate: ({ item }) => setPendingId(item.id),
    onSuccess: r => {
      toast(r.message, r.ok)
      qc.invalidateQueries({ queryKey: ['live'] })
    },
    onError: e => toast(e instanceof Error ? e.message : String(e), false),
    onSettled: () => setPendingId(null),
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

  const groups = useMemo(() => {
    const items = catalog.data?.items ?? []
    return ORDER.map(c => ({ c, items: items.filter(i => i.category === c) })).filter(g => g.items.length)
  }, [catalog.data])
  const spawns = live.data?.live?.spawns ?? []
  const mine = spawns.filter(s => me?.ucid && s.owner_ucid === me.ucid)
  const others = spawns.filter(s => !(me?.ucid && s.owner_ucid === me.ucid))
  const cat = catalog.data

  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <h1 className="display">Spawn</h1>
          <p className="sub m-0 mt-1">
            Put adversaries, targets, tankers and ships on the range.
            {cat && <> Up to <b>{cat.max_active_per_player}</b> active per pilot; each despawns after <b>{Math.round(cat.despawn_after_s / 60)} min</b> or when you leave your slot.</>}
          </p>
        </div>
      </div>

      {!loading && !me?.logged_in && <div className="mb-4"><LoginPrompt why="to spawn on the range" /></div>}
      {!loading && me?.logged_in && !me.ucid && <div className="mb-4"><LinkGuidance /></div>}

      <div className="flex items-start gap-2 mb-5 text-[13px] panel panel-b" style={{ borderColor: 'color-mix(in srgb, var(--ball) 35%, var(--line))' }}>
        <Info size={16} style={{ color: 'var(--ball)', flex: 'none', marginTop: 1 }} />
        <span>
          Items marked <span className="chip outline">RELATIVE TO YOU</span> are placed around your aircraft, so you must be <b>in a slot and airborne</b> in DCS when you press spawn.
          Everything else goes to its range station.
        </span>
      </div>

      <div className="grid gap-5 lg:grid-cols-[minmax(0,1fr)_360px]">
        <div className="min-w-0 order-2 lg:order-1">
          {catalog.isLoading ? (
            <Loading label="Loading the catalogue" />
          ) : catalog.error ? (
            <ErrorState error={catalog.error} retry={() => catalog.refetch()} />
          ) : (
            groups.map(g => (
              <section key={g.c} className="mb-6">
                <h2 className="caps mb-2" style={{ color: 'var(--chalk)' }}>{CATEGORY_LABEL[g.c]}</h2>
                <div className="grid gap-3" style={{ gridTemplateColumns: 'repeat(auto-fill, minmax(min(100%, 320px), 1fr))' }}>
                  {g.items.map(i => (
                    <ItemCard key={i.id} item={i} canSpawn={canSpawn} admin={admin} pending={pendingId === i.id} onSpawn={(item, params) => spawn.mutate({ item, params })} />
                  ))}
                </div>
              </section>
            ))
          )}
        </div>

        <aside className="order-1 lg:order-2 flex flex-col gap-3 lg:sticky lg:top-[72px] self-start w-full">
          <Panel
            title={`My active spawns${cat ? ` · ${mine.length}/${cat.max_active_per_player}` : ''}`}
            bodyClass=""
            right={mine.length > 0 && (
              <button className="btn-range danger sm" onClick={() => despawn.mutate('all')} disabled={despawn.isPending}>Despawn all</button>
            )}
          >
            {!canSpawn ? (
              <Empty title="Not linked">Your spawns show here once you are logged in with a linked DCS account.</Empty>
            ) : !live.data?.live ? (
              <Empty title="Range offline">{live.data?.reason ?? 'No live picture from the range server.'}</Empty>
            ) : mine.length === 0 ? (
              <Empty title="Nothing spawned">Pick something from the catalogue.</Empty>
            ) : (
              <ul className="m-0 p-0 list-none">
                {mine.map(s => <SpawnRow key={s.id} s={s} busy={despawn.isPending} onDespawn={() => despawn.mutate({ spawn_id: s.id })} />)}
              </ul>
            )}
          </Panel>

          {others.length > 0 && (
            <Panel title={`Others on the range · ${others.length}`} bodyClass="">
              <ul className="m-0 p-0 list-none">
                {others.map(s => admin ? (
                  <SpawnRow key={s.id} s={s} busy={despawn.isPending} onDespawn={() => despawn.mutate({ spawn_id: s.id })} />
                ) : (
                  <li key={s.id} className="px-3 py-2 border-b border-[var(--line)] last:border-b-0 text-[12.5px]">
                    <span className="font-medium">{s.label}</span> <span className="muted">· {s.owner_name}</span>
                  </li>
                ))}
              </ul>
            </Panel>
          )}

          {admin && (
            <Panel title="Instructor">
              <div className="flex flex-col gap-2">
                <Select
                  label="Reset a station (respawn all its targets)"
                  value={station}
                  onChange={setStation}
                  options={[{ value: '', label: 'Choose a station' }, ...(live.data?.live?.stations ?? []).map(s => ({ value: s.id, label: s.name }))]}
                />
                <button className="btn-range" disabled={!station || reset.isPending} onClick={() => reset.mutate(station)}>Reset station</button>
              </div>
            </Panel>
          )}
        </aside>
      </div>
    </div>
  )
}
