import type { ServerProfile } from '../config/servers'
import { roundDay, useServerStates, useTheatreLive, type ServerRow } from '../live'
import { roundNo } from '../api'
import { useNotices } from '../status'

/**
 * One server's live state as its page shows it: the status row, front line,
 * pilots on and the round. The range has no instance on this bfdb, so it gets
 * its notices and nothing else.
 */
export function useServerLive(profile: ServerProfile) {
  const { rows } = useServerStates()
  const { forServer } = useNotices()
  const row: ServerRow | undefined = profile.instanceId ? rows.find(r => r.theatre.key === profile.instanceId) : undefined
  const up = row?.state === 'live' || row?.state === 'standby'
  const { front, frontLoading, airborne } = useTheatreLive(up ? row?.theatre.instance : undefined)
  const round = up ? row?.theatre.instance?.active_round ?? undefined : undefined
  return {
    row,
    state: row?.state ?? 'unknown',
    up,
    front,
    frontLoading,
    airborne,
    round: round ? { n: roundNo(round), day: roundDay(round.start) } : undefined,
    notices: row?.notices ?? forServer(profile.slug),
  }
}
