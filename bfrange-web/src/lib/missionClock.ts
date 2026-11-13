/**
 * The mission clock, ticking. The live picture carries the mission's local
 * time as of its last poll; between polls the clock runs on from there, so
 * the readouts count every second instead of jumping every few. Zulu is the
 * local time less the theatre's UTC offset (DCS keeps each map on one fixed
 * offset, no daylight saving), and is left out for a theatre not listed.
 */
import { useLive } from './useLive'
import { useNow } from './useNow'

/** Hours east of UTC, by DCS theatre name. */
export const THEATRE_UTC: Record<string, number> = {
  Caucasus: 4,
  PersianGulf: 4,
  MarianaIslands: 10,
  Syria: 3,
  SinaiMap: 2,
  Iraq: 3,
  Kola: 3,
  Afghanistan: 4.5,
}

const pad = (n: number) => String(n).padStart(2, '0')

function hms(sec: number): string {
  const s = ((Math.floor(sec) % 86400) + 86400) % 86400
  return `${pad(Math.floor(s / 3600))}:${pad(Math.floor(s / 60) % 60)}:${pad(s % 60)}`
}

/** "HH:MM:SS" -> seconds after midnight (NaN when it is not a time). */
export function parseClock(t: string): number {
  const m = /^(\d{1,2}):(\d{2})(?::(\d{2}))?/.exec(t)
  return m ? Number(m[1]) * 3600 + Number(m[2]) * 60 + Number(m[3] ?? 0) : NaN
}

export interface MissionClock {
  /** local mission time, "HH:MM:SS" */
  local: string
  /** Zulu, "HH:MM:SS"; null for a theatre whose offset this site does not know */
  zulu: string | null
  /** the Zulu day of month, for a METAR's time group */
  zuluDay: number | null
  /** seconds after local midnight */
  sec: number
}

/** The clock from a picture taken `ageS` seconds ago. */
export function missionClock(theatre: string, missionTime: string, missionDate: string, ageS: number): MissionClock | null {
  const base = parseClock(missionTime)
  if (!Number.isFinite(base)) return null
  // never run on for more than half a minute: a stalled poll should not show a clock still going
  const sec = base + Math.min(30, Math.max(0, ageS))
  const off = THEATRE_UTC[theatre]
  if (off === undefined) return { local: hms(sec), zulu: null, zuluDay: null, sec }
  const zsec = sec - off * 3600
  const d = /^(\d{4})\D(\d{1,2})\D(\d{1,2})/.exec(missionDate)
  let zuluDay: number | null = null
  if (d) {
    const day = new Date(Date.UTC(Number(d[1]), Number(d[2]) - 1, Number(d[3])))
    day.setUTCDate(day.getUTCDate() + Math.floor(zsec / 86400))
    zuluDay = day.getUTCDate()
  }
  return { local: hms(sec), zulu: hms(zsec), zuluDay, sec }
}

/** The live mission clock, ticking once a second; null while the range is down. */
export function useMissionClock(): MissionClock | null {
  const q = useLive()
  const now = useNow(1000)
  const live = q.data?.live
  if (!live) return null
  return missionClock(live.theatre, live.mission_time, live.mission_date, (now - q.dataUpdatedAt) / 1000)
}
