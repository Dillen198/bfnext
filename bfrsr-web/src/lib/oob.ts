// The air order of battle (`/api/oob`): rows grouped by role, and the totals
// the ORDER OF BATTLE page and the SITUATION board's losses line print.
//
// Kept apart from the components so the sums can be tested without a DOM.

import { useQuery } from '@tanstack/react-query'
import { api, type OobEntry, type OrderOfBattle } from '../api'
import { oobMockParam } from '../pages/oobMock'
import { classifyTargetClass } from './format'

/** `/api/oob` for a round (undefined: the active one), shared by the ORDER
 *  OF BATTLE page and the SITUATION board's losses line. `?mock` (dev
 *  builds) answers from the fixture instead. */
export function useOrderOfBattle(round: number | undefined) {
  const mock = oobMockParam()
  return useQuery({
    queryKey: ['oob', round ?? null, mock ? `mock:${mock.red.some(r => r.stock != null) ? 'admin' : 'pilot'}` : ''],
    queryFn: () => (mock ? Promise.resolve(mock) : api.oob(round)),
    refetchInterval: 60_000,
  })
}

/** The roles a regiment roll is filed under, in the order they are printed.
 *  The engine sends only DCS type names, so the role is what the dashboard's
 *  own type classifier can tell: rotary wing or not. */
export const OOB_ROLES = ['Fixed wing', 'Rotary wing'] as const
export type OobRole = (typeof OOB_ROLES)[number]

export function oobRole(type: string): OobRole {
  return classifyTargetClass(type) === 'HELO' ? 'Rotary wing' : 'Fixed wing'
}

/** A side's rows by role, each role's types in name order ("MiG-21bis"
 *  before "MiG-29A"). Roles with no rows are left out. */
export function oobByRole(rows: OobEntry[]): { role: OobRole; rows: OobEntry[] }[] {
  const byName = (a: OobEntry, b: OobEntry) => a.type.localeCompare(b.type, 'en', { numeric: true, sensitivity: 'base' })
  return OOB_ROLES
    .map(role => ({ role, rows: rows.filter(r => oobRole(r.type) === role).sort(byName) }))
    .filter(g => g.rows.length > 0)
}

export interface OobTotals {
  /** Airframes on strength; null when this side's stock is classified to the
   *  viewer (every row's `stock` is null). */
  stock: number | null
  /** Lost this round. Always known: losses are public. */
  lost: number
  types: number
  /** Some type never runs out (an unlimited-aircraft base holds it), so the
   *  stock is a floor rather than the whole of it. */
  unlimited: boolean
}

export function oobTotals(rows: OobEntry[]): OobTotals {
  const known = rows.filter(r => r.stock != null)
  return {
    stock: rows.length > 0 && known.length === 0 ? null : known.reduce((n, r) => n + (r.stock ?? 0), 0),
    lost: rows.reduce((n, r) => n + r.lost, 0),
    types: rows.length,
    unlimited: rows.some(r => r.unlimited),
  }
}

/** Nothing on file: the feature is off, or the round has not sent one yet. */
export function oobEmpty(oob: OrderOfBattle | undefined | null): boolean {
  return !oob || (oob.blue.length === 0 && oob.red.length === 0)
}

/**
 * How much of the type's strength at the start of the round is left: stock
 * against stock plus losses, 0-1. Null when the stock is classified. A type
 * that never runs out reads full.
 */
export function oobStrength(r: OobEntry): number | null {
  if (r.stock == null) return null
  if (r.unlimited) return 1
  const start = r.stock + r.lost
  return start > 0 ? r.stock / start : 0
}
