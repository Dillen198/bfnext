// Dev-only fixture for the ORDER OF BATTLE page (`/oob?mock`) and the
// SITUATION board's losses line. As a NATO pilot sees it: NATO's own stock,
// the Soviet stock classified. `?mock=admin` shows both, as an admin sees it.
import type { OobEntry, OrderOfBattle } from '../api'

const BLUE: OobEntry[] = [
  { type: 'F-14A', stock: 18, lost: 3, unlimited: false },
  { type: 'FA-18C_hornet', stock: 10, lost: 1, unlimited: false },
  { type: 'F-15C', stock: 22, lost: 2, unlimited: false },
  { type: 'F-16C_50', stock: 14, lost: 0, unlimited: false },
  { type: 'F-4E-45MC', stock: 26, lost: 5, unlimited: true },
  { type: 'A-10A', stock: 12, lost: 4, unlimited: false },
  { type: 'Tornado IDS', stock: 8, lost: 2, unlimited: false },
  { type: 'F-117A', stock: 4, lost: 0, unlimited: false },
  { type: 'E-3A', stock: 2, lost: 0, unlimited: false },
  { type: 'KC-135', stock: 3, lost: 1, unlimited: false },
  { type: 'S-3B', stock: 4, lost: 0, unlimited: false },
  { type: 'UH-1H', stock: 16, lost: 2, unlimited: false },
  { type: 'SH-60B', stock: 6, lost: 0, unlimited: false },
]

const RED: OobEntry[] = [
  { type: 'MiG-29A', stock: 9, lost: 7, unlimited: false },
  { type: 'Su-27', stock: 14, lost: 3, unlimited: false },
  { type: 'MiG-21Bis', stock: 30, lost: 2, unlimited: true },
  { type: 'MiG-23MLD', stock: 20, lost: 6, unlimited: false },
  { type: 'MiG-25PD', stock: 6, lost: 1, unlimited: false },
  { type: 'MiG-31', stock: 4, lost: 0, unlimited: false },
  { type: 'Su-24M', stock: 12, lost: 4, unlimited: false },
  { type: 'Su-25', stock: 16, lost: 5, unlimited: false },
  { type: 'Su-17M4', stock: 10, lost: 1, unlimited: false },
  { type: 'Tu-22M3', stock: 1, lost: 5, unlimited: false },
  { type: 'A-50', stock: 1, lost: 0, unlimited: false },
  { type: 'IL-78M', stock: 2, lost: 0, unlimited: false },
  { type: 'Mi-24P', stock: 14, lost: 3, unlimited: false },
  { type: 'Mi-8MT', stock: 18, lost: 1, unlimited: false },
]

/** What `/api/oob` returns: the caller's own side in full, the other side's
 *  stock null and `unlimited` false (bfdb hides both), losses for everyone. */
function hide(rows: OobEntry[]): OobEntry[] {
  return rows.map(r => ({ ...r, stock: null, unlimited: false }))
}

export function oobMock(admin = false): OrderOfBattle {
  return { round: 4, blue: BLUE, red: admin ? RED : hide(RED) }
}

/** Dev builds only: `?mock` (as a NATO pilot) or `?mock=admin`. Null when off. */
export function oobMockParam(): OrderOfBattle | null {
  if (!import.meta.env.DEV || typeof location === 'undefined') return null
  const p = new URLSearchParams(location.search).get('mock')
  return p == null ? null : oobMock(p === 'admin')
}
