// Imperial (Tacview's default for DCS: ft, kt, nm, lb) or metric readouts.

import { M_TO_FT, MS_TO_KT } from './data'

export type Units = 'imperial' | 'metric'

const n = (x: number) => Math.round(x).toLocaleString()

export const fmtAlt = (m: number, u: Units) => (u === 'metric' ? `${n(m)} m` : `${n(m * M_TO_FT)} ft`)
export const fmtSpeed = (ms: number, u: Units) => (u === 'metric' ? `${n(ms * 3.6)} km/h` : `${n(ms * MS_TO_KT)} kt`)
export const fmtVs = (ms: number, u: Units) => (u === 'metric' ? `${(ms).toFixed(1)} m/s` : `${n(ms * M_TO_FT * 60)} fpm`)
export const fmtWeight = (kg: number, u: Units) => (u === 'metric' ? `${n(kg)} kg` : `${n(kg * 2.20462)} lb`)
export function fmtDist(m: number, u: Units): string {
  if (u === 'metric') return m >= 10_000 ? `${(m / 1000).toFixed(0)} km` : `${(m / 1000).toFixed(1)} km`
  const nm = m / 1852
  return nm >= 10 ? `${nm.toFixed(0)} nm` : `${nm.toFixed(1)} nm`
}
/** The altitude part of a label: flight level, or metres. */
export const labelAlt = (m: number, u: Units) =>
  u === 'metric' ? `${n(m)}m` : `FL${String(Math.max(0, Math.round((m * M_TO_FT) / 100))).padStart(3, '0')}`
export const labelSpeed = (ms: number, u: Units) => (u === 'metric' ? `${n(ms * 3.6)}kmh` : `${n(ms * MS_TO_KT)}kt`)
