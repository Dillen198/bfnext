/** Non-component helpers for the Fowl Engine Manager screens (kept out of
 *  ui.tsx so that file only exports components). */
import { useState } from 'react'
import { useQueryClient } from '@tanstack/react-query'
import type { AppState } from './tauri'

export const DIM: React.CSSProperties = {
  fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase',
}
export const MONO: React.CSSProperties = { fontFamily: 'var(--font-mono)' }
export const RED = '#ef4444'
export const AMBER = 'var(--yellow)'
export const OK = 'var(--accent)'

export const input: React.CSSProperties = {
  background: 'var(--bg-input)', border: '1px solid var(--border)', color: 'var(--text)',
  padding: '0.45rem 0.6rem', fontSize: '0.74rem', borderRadius: 2, width: '100%', boxSizing: 'border-box',
}

/** Run an action, show its result, refresh the app state afterwards. */
export function useAction() {
  const qc = useQueryClient()
  const [busy, setBusy] = useState<string | null>(null)
  const [result, setResult] = useState<{ ok: boolean; message: string } | null>(null)
  async function run(key: string, fn: () => Promise<unknown>, okMessage?: string) {
    setBusy(key)
    setResult(null)
    try {
      const r = await fn()
      setResult({ ok: true, message: typeof r === 'string' ? r : okMessage ?? 'done' })
    } catch (e) {
      setResult({ ok: false, message: e instanceof Error ? e.message : String(e) })
    } finally {
      setBusy(null)
      qc.invalidateQueries({ queryKey: ['mgr'] })
    }
  }
  return { busy, result, run, clear: () => setResult(null) }
}

export function fmtDur(secs?: number | null): string {
  if (secs == null) return '—'
  const s = Math.max(0, Math.floor(secs))
  const d = Math.floor(s / 86400), h = Math.floor((s % 86400) / 3600), m = Math.floor((s % 3600) / 60)
  if (d) return `${d}d ${h}h`
  if (h) return `${h}h ${m}m`
  if (m) return `${m}m ${s % 60}s`
  return `${s}s`
}

export function fmtWhen(iso?: string | null): string {
  if (!iso) return '—'
  const d = new Date(iso)
  return isNaN(d.getTime()) ? iso : d.toLocaleString([], { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' })
}

/** First run: no bot folder yet, or no service. */
export function needsSetup(s?: AppState): boolean {
  return !!s && (!s.bot_dir_valid || !s.service?.installed)
}
