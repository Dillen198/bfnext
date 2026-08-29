import React, { useMemo, useState } from 'react'
import { ChevronDown, ChevronUp } from 'lucide-react'

export type SortDir = 'asc' | 'desc'
export type SortValue = string | number | Date | null | undefined

/**
 * Generic client-side table sort. `cols` maps each sortable key to a value
 * accessor for a row. Define `cols` as a module-level const (stable identity) so
 * the memoised sort doesn't recompute every render.
 */
export function useTableSort<
  T,
  C extends Record<string, (row: T) => SortValue>,
>(rows: T[], cols: C, init: { key: keyof C & string; dir?: SortDir }) {
  type K = keyof C & string
  const [sortKey, setSortKey] = useState<K>(init.key)
  const [sortDir, setSortDir] = useState<SortDir>(init.dir ?? 'desc')

  function onSort(k: K) {
    if (k === sortKey) setSortDir(d => (d === 'asc' ? 'desc' : 'asc'))
    else {
      setSortKey(k)
      setSortDir('desc')
    }
  }

  const sorted = useMemo(() => {
    const acc = cols[sortKey]
    return [...rows].sort((a, b) => {
      const va = acc(a)
      const vb = acc(b)
      let c: number
      if (va == null && vb == null) c = 0
      else if (va == null) c = 1
      else if (vb == null) c = -1
      else if (typeof va === 'number' && typeof vb === 'number') c = va - vb
      else if (va instanceof Date && vb instanceof Date) c = va.getTime() - vb.getTime()
      else c = String(va).localeCompare(String(vb), undefined, { numeric: true })
      return sortDir === 'asc' ? c : -c
    })
  }, [rows, sortKey, sortDir, cols])

  return { sorted, sortKey, sortDir, onSort }
}

/** A clickable table header cell with an asc/desc indicator. */
export function SortTh<K extends string>({
  label,
  colKey,
  sortKey,
  sortDir,
  onSort,
  style,
  title,
}: {
  label: string
  /** omit to make this header non-sortable */
  colKey?: K
  sortKey: K
  sortDir: SortDir
  onSort: (k: K) => void
  style?: React.CSSProperties
  title?: string
}) {
  const active = colKey != null && colKey === sortKey
  return (
    <th
      title={title}
      onClick={colKey ? () => onSort(colKey) : undefined}
      style={{
        whiteSpace: 'nowrap',
        ...style,
        cursor: colKey ? 'pointer' : title ? 'help' : undefined,
        userSelect: 'none',
        ...(active ? { color: 'var(--accent)' } : null),
      }}
    >
      <span style={{ display: 'inline-flex', alignItems: 'center', gap: 3 }}>
        {label}
        {colKey &&
          (active ? (
            sortDir === 'asc' ? <ChevronUp size={10} /> : <ChevronDown size={10} />
          ) : (
            <ChevronDown size={10} style={{ opacity: 0.2 }} />
          ))}
      </span>
    </th>
  )
}
