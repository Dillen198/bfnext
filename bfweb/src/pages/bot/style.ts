// The bot console's visual vocabulary (the OPS page's), apart from the
// components so React fast refresh keeps working.

import type { CSSProperties } from 'react'

export const DIM: CSSProperties = {
  fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase',
}
export const CELL: CSSProperties = {
  padding: '6px 10px', fontSize: '0.68rem', color: 'var(--text-muted)', borderBottom: '1px solid var(--border)',
  verticalAlign: 'top',
}
export const TH: CSSProperties = { ...CELL, ...DIM, textAlign: 'left', whiteSpace: 'nowrap', color: 'var(--text-dim)' }
export const MONO: CSSProperties = { fontFamily: 'var(--font-mono)' }
export const RED = '#ef4444'
export const AMBER = 'var(--yellow)'
export const OK = 'var(--accent)'
export const INPUT: CSSProperties = {
  background: 'var(--bg-input)', border: '1px solid var(--border)', color: 'var(--text)',
  fontSize: '0.66rem', padding: '3px 6px', borderRadius: 2, minWidth: 0,
}

export function errText(e: unknown): string {
  return e instanceof Error ? e.message : String(e)
}
