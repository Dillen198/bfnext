import type { ReactNode } from 'react'
import { Lens } from './Lens'

export function Loading({ label = 'Loading' }: { label?: string }) {
  return (
    <div className="flex items-center gap-3 py-10 justify-center muted" role="status">
      <Lens size={30} loading />
      <span className="caps">{label}…</span>
    </div>
  )
}

export function ErrorState({ error, retry }: { error: unknown; retry?: () => void }) {
  const msg = error instanceof Error ? error.message : String(error)
  return (
    <div className="panel panel-b flex flex-wrap items-center gap-3" role="alert" style={{ borderColor: 'color-mix(in srgb, var(--wave) 40%, var(--line))' }}>
      <span className="chip bad">ERROR</span>
      <span>Could not load this from the range server: <span className="mono">{msg}</span></span>
      {retry && (
        <button className="btn-range sm ml-auto" onClick={retry}>
          Try again
        </button>
      )}
    </div>
  )
}

export function Empty({ title, children }: { title: string; children?: ReactNode }) {
  return (
    <div className="py-8 px-4 text-center">
      <div className="caps" style={{ color: 'var(--chalk)' }}>{title}</div>
      {children && <div className="muted mt-1.5 text-[13px] max-w-md mx-auto">{children}</div>}
    </div>
  )
}
