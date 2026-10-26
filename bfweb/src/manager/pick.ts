/** Which server box the Manager's tabs act on (the picker at the top), kept
 *  outside React so the OPS page's transport can read it too. Remembered per
 *  PC in localStorage; a box that was removed falls back to the first one. */
import { useSyncExternalStore } from 'react'
import { setOpsServer, type AppState, type ServerView } from './tauri'

const KEY = 'fowl-manager-server'
const subs = new Set<() => void>()

function load(): string | null {
  try { return localStorage.getItem(KEY) } catch { return null }
}

let picked: string | null = load()

export function pickServer(id: string): void {
  picked = id
  try { localStorage.setItem(KEY, id) } catch { /* private mode: not remembered */ }
  subs.forEach(f => f())
}

export function usePickedServer(): string | null {
  return useSyncExternalStore(cb => { subs.add(cb); return () => { subs.delete(cb) } }, () => picked)
}

/** The server every tab acts on now; also points the OPS transport at it. */
export function resolveServer(state: AppState | undefined, id: string | null): ServerView | null {
  const list = state?.servers ?? []
  const s = list.find(x => x.id === id) ?? list[0] ?? null
  setOpsServer(s && !s.local ? s.id : null)
  return s
}

/** null for this box's bot (what ops_request takes as "local"). */
export function opsId(s: ServerView | null): string | null {
  return s && !s.local ? s.id : null
}
