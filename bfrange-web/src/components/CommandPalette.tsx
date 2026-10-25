/**
 * Ctrl/⌘ K (or `/`): jump anywhere by typing. Pages, the range's stations
 * and assets (shown on the control map), pilots flying now and every pilot
 * with a record (searched on the server), plus the few actions that have no
 * page of their own.
 */
import { useEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import { useNavigate } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { Aircraft, Bombing, Carrier, HotZone, LogOut, Moon, Pilot, Refuel, Search, Sun, type IconComponent } from '@icons'
import { api } from '../api'
import { useAuth } from '../context/AuthContext'
import { useTheme } from '@shared/web/theme'
import { GROUPABLE_STATIONS, STATION_KIND_LABEL, airframe } from '../lib/format'
import { useLive } from '../lib/useLive'
import { NAV } from './nav'

interface Item {
  id: string
  group: string
  label: string
  hint?: ReactNode
  icon: IconComponent
  run: () => void
}

function useDebounced<T>(v: T, ms: number): T {
  const [d, setD] = useState(v)
  useEffect(() => {
    const t = setTimeout(() => setD(v), ms)
    return () => clearTimeout(t)
  }, [v, ms])
  return d
}

const norm = (s: string) => s.toLowerCase().normalize('NFKD').replace(/[^\p{L}\p{N} ]/gu, '')

export function CommandPalette({ onClose }: { onClose: () => void }) {
  const nav = useNavigate()
  const { me, login, logout } = useAuth()
  const { theme, toggle } = useTheme()
  const live = useLive().data?.live
  const [text, setText] = useState('')
  const [sel, setSel] = useState(0)
  const input = useRef<HTMLInputElement>(null)
  const list = useRef<HTMLUListElement>(null)
  const q = norm(text.trim())
  const dq = useDebounced(text.trim(), 180)
  const pilots = useQuery({
    queryKey: ['pilots', dq],
    queryFn: () => api.pilots(dq),
    enabled: dq.length >= 2,
    staleTime: 60_000,
  })

  useEffect(() => { input.current?.focus() }, [])

  const items = useMemo<Item[]>(() => {
    const go = (to: string) => () => { nav(to); onClose() }
    const focus = (key: string) => go(`/?focus=${encodeURIComponent(key)}`)
    const out: Item[] = NAV.map(n => ({ id: `page:${n.to}`, group: 'Pages', label: n.label, hint: n.hint, icon: n.icon, run: go(n.to) }))
    if (live) {
      for (const p of live.players) {
        out.push({ id: `player:${p.ucid}`, group: 'Flying now', label: p.name, hint: `${airframe(p.unit_type)} · ${p.in_air ? p.activity ?? 'airborne' : 'on the ground'} · show on map`, icon: Aircraft, run: focus(`player:${p.ucid}`) })
      }
      for (const s of live.stations) {
        const grouped = GROUPABLE_STATIONS.includes(s.kind)
        out.push({
          id: `station:${s.id}`, group: 'Stations', label: s.name,
          hint: `${STATION_KIND_LABEL[s.kind]}${s.laser_code !== null ? ` · laser ${s.laser_code}` : ''} · ${grouped ? 'impact group' : 'show on map'}`,
          icon: Bombing, run: grouped ? go(`/stations/${encodeURIComponent(s.id)}`) : focus(`station:${s.id}`),
        })
      }
      for (const c of live.carriers) {
        out.push({ id: `carrier:${c.id}`, group: 'Range assets', label: c.name, hint: `${c.recovery_open ? 'recovery open' : 'deck closed'}${c.tacan ? ` · TACAN ${c.tacan}` : ''} · show on map`, icon: Carrier, run: focus(`carrier:${c.id}`) })
      }
      for (const t of live.tankers) {
        out.push({ id: `tanker:${t.id}`, group: 'Range assets', label: t.callsign, hint: `${airframe(t.unit_type)} · ${t.tacan ?? ''} ${t.freq_mhz.toFixed(3)} · show on map`, icon: Refuel, run: focus(`tanker:${t.id}`) })
      }
      for (const h of live.hot_zones ?? []) {
        out.push({ id: `hz:${h.id}`, group: 'Range assets', label: h.name, hint: `hot zone · ${h.bandits} bandits · show on map`, icon: HotZone, run: focus(`hz:${h.id}`) })
      }
    }
    for (const p of pilots.data ?? []) {
      out.push({ id: `pilot:${p.ucid}`, group: 'Pilot records', label: p.name, hint: `${p.count} result${p.count === 1 ? '' : 's'}`, icon: Pilot, run: go(`/pilot/${encodeURIComponent(p.ucid)}`) })
    }
    out.push({ id: 'act:theme', group: 'Actions', label: theme === 'dark' ? 'Light theme' : 'Dark theme', icon: theme === 'dark' ? Sun : Moon, run: () => { toggle(); onClose() } })
    if (me?.logged_in) out.push({ id: 'act:logout', group: 'Actions', label: 'Log out', icon: LogOut, run: () => { logout(); onClose() } })
    else out.push({ id: 'act:login', group: 'Actions', label: 'Log in with Discord', icon: Pilot, run: () => { login(); onClose() } })
    return out
  }, [live, pilots.data, nav, onClose, theme, toggle, me?.logged_in, login, logout])

  const shown = useMemo(() => {
    if (!q) return items.filter(i => i.group === 'Pages' || i.group === 'Actions' || i.group === 'Flying now')
    const words = q.split(/\s+/)
    return items
      .map(i => {
        const hay = norm(`${i.label} ${typeof i.hint === 'string' ? i.hint : ''} ${i.group}`)
        const label = norm(i.label)
        if (!words.every(w => hay.includes(w))) return null
        return { i, rank: label.startsWith(q) ? 0 : label.includes(q) ? 1 : 2 }
      })
      .filter((x): x is { i: Item; rank: number } => !!x)
      .sort((a, b) => a.rank - b.rank)
      .map(x => x.i)
      .slice(0, 40)
  }, [items, q])

  const [lastQ, setLastQ] = useState(q)
  if (lastQ !== q) {
    setLastQ(q)
    setSel(0)
  }
  const cur = Math.min(sel, Math.max(0, shown.length - 1))

  useEffect(() => {
    list.current?.querySelector<HTMLElement>(`[data-i="${cur}"]`)?.scrollIntoView({ block: 'nearest' })
  }, [cur])

  const onKey = (e: React.KeyboardEvent) => {
    if (e.key === 'Escape') { e.preventDefault(); onClose() }
    else if (e.key === 'ArrowDown') { e.preventDefault(); setSel(Math.min(cur + 1, shown.length - 1)) }
    else if (e.key === 'ArrowUp') { e.preventDefault(); setSel(Math.max(cur - 1, 0)) }
    else if (e.key === 'Enter') { e.preventDefault(); shown[cur]?.run() }
  }

  return (
    <div className="palette-scrim" role="presentation" onMouseDown={e => { if (e.target === e.currentTarget) onClose() }}>
      {/* The dialog's own keys (arrows, Enter, Escape) drive the list inside it. */}
      {/* eslint-disable-next-line jsx-a11y/no-noninteractive-element-interactions */}
      <div className="palette" role="dialog" aria-modal="true" aria-label="Search the range" onKeyDown={onKey}>
        <div className="palette-in">
          <Search size={17} className="muted shrink-0" />
          <input
            ref={input}
            value={text}
            onChange={e => setText(e.target.value)}
            placeholder="Pages, stations, tankers, pilots…"
            aria-label="Search"
            aria-controls="palette-list"
            aria-activedescendant={shown[cur] ? `pal-${cur}` : undefined}
          />
          <kbd>Esc</kbd>
        </div>
        <ul ref={list} id="palette-list" role="listbox" className="palette-list">
          {shown.length === 0 && (
            <li className="px-4 py-6 text-center muted text-[13px]">
              {pilots.isFetching ? 'Searching pilots…' : 'Nothing matches.'}
            </li>
          )}
          {shown.map((it, n) => {
            const head = n === 0 || shown[n - 1].group !== it.group
            return (
              <li key={it.id} role="presentation">
                {head && <div className="palette-group">{it.group}</div>}
                <button
                  id={`pal-${n}`}
                  data-i={n}
                  role="option"
                  aria-selected={n === cur}
                  className="palette-item"
                  onMouseMove={() => n !== cur && setSel(n)}
                  onClick={it.run}
                >
                  <it.icon size={16} className="shrink-0" />
                  <span className="font-medium truncate">{it.label}</span>
                  {it.hint && <span className="muted text-[12px] truncate">{it.hint}</span>}
                </button>
              </li>
            )
          })}
        </ul>
        <div className="palette-foot">
          <span><kbd>↑</kbd><kbd>↓</kbd> move</span>
          <span><kbd>Enter</kbd> open</span>
          {dq.length < 2 && <span>type 2 letters to search pilot records</span>}
        </div>
      </div>
    </div>
  )
}
