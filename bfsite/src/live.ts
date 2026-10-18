import { useQueries, useQuery } from '@tanstack/react-query'
import { api, type Health, type ServerInstance } from './api'
import { SERVERS, type ServerProfile } from './config/servers'
import { useNotices, type Notice, type ServerState } from './status'

/** A theatre as the page shows it: the static pitch, plus the live instance when bfdb answers. */
export interface Theatre {
  key: string
  profile: ServerProfile | undefined
  instance: ServerInstance | undefined
  label: string
  blueLabel: string
  redLabel: string
}

/**
 * The servers to show. With bfdb reachable that is whatever `/api/instances`
 * lists, in its order; without it, the configured profiles, so the page still
 * says what each server is while the status reads offline.
 */
export function useTheatres() {
  const q = useQuery({
    queryKey: ['instances'],
    queryFn: api.instances,
    refetchInterval: 60_000,
  })
  const instances = q.data?.instances ?? []
  const theatres: Theatre[] = instances.length
    ? instances.map(i => {
        const profile = SERVERS.find(s => s.instanceId === i.id)
        return {
          key: i.id,
          profile,
          instance: i,
          label: profile?.label ?? i.label,
          blueLabel: profile?.blueLabel ?? 'BLUFOR',
          redLabel: profile?.redLabel ?? 'REDFOR',
        }
      })
    : SERVERS.map(p => ({
        key: p.instanceId,
        profile: p,
        instance: undefined,
        label: p.label,
        blueLabel: p.blueLabel,
        redLabel: p.redLabel,
      }))
  return {
    theatres,
    defaultInstance: q.data?.default,
    loading: q.isLoading,
    /** bfdb did not answer at all: the game host, or its tunnel, is down. */
    offline: q.isError,
    checkedAt: Math.max(q.dataUpdatedAt, q.errorUpdatedAt) || undefined,
  }
}

export interface ServerRow {
  theatre: Theatre
  state: ServerState
  notices: Notice[]
  health: Health | undefined
}

const NOTICE_STATE: Partial<Record<Notice['level'], ServerState>> = {
  outage: 'outage',
  maintenance: 'maintenance',
  degraded: 'degraded',
}
const NOTICE_RANK: ServerState[] = ['outage', 'maintenance', 'degraded']

/**
 * Every server's state, from the hand-written notices first and bfdb's own
 * probes second. A notice explains an outage; the probes notice one nobody
 * has written up yet.
 */
export function useServerStates() {
  const { theatres, offline, loading, checkedAt } = useTheatres()
  const { notices, upcoming, forServer } = useNotices()
  const healths = useQueries({
    queries: theatres.map(t => ({
      queryKey: ['health', t.instance?.id],
      queryFn: () => api.health(t.instance?.id),
      enabled: !!t.instance && !offline,
      refetchInterval: 60_000,
    })),
  })

  const rows: ServerRow[] = theatres.map((t, i) => {
    const ns = forServer(t.key)
    const health = healths[i]?.data
    const fromNotice = NOTICE_RANK.find(s => ns.some(n => NOTICE_STATE[n.level] === s))
    let state: ServerState
    if (fromNotice) state = fromNotice
    else if (offline) state = 'offline'
    else if (loading || !t.instance) state = 'unknown'
    else if (health && !health.engine.reachable) state = 'engine-down'
    else state = t.instance.active_round ? 'live' : 'standby'
    return { theatre: t, state, notices: ns, health }
  })

  return { rows, offline, loading, notices, upcoming, checkedAt }
}

export interface FrontLine {
  blue: number
  red: number
  neutral: number
  total: number
}

/** Objectives by owner and pilots online by side, for one server. */
export function useTheatreLive(instance: ServerInstance | undefined) {
  const id = instance?.id
  const objectives = useQuery({
    queryKey: ['objectives', id],
    queryFn: () => api.objectives(id),
    enabled: !!id,
    refetchInterval: 60_000,
  })
  const online = useQuery({
    queryKey: ['online', id],
    queryFn: () => api.online(id),
    enabled: !!id && !!instance?.active_round,
    refetchInterval: 30_000,
  })

  const objs = objectives.data ?? []
  const front: FrontLine | undefined = objectives.data
    ? {
        blue: objs.filter(o => o.owner === 'Blue').length,
        red: objs.filter(o => o.owner === 'Red').length,
        neutral: objs.filter(o => o.owner === 'Neutral').length,
        total: objs.length,
      }
    : undefined

  const pilots = online.data ?? []
  const airborne = {
    blue: pilots.filter(p => p.side === 'Blue').length,
    red: pilots.filter(p => p.side === 'Red').length,
    total: pilots.length,
    known: !!instance?.active_round && !!online.data,
  }

  return { front, frontLoading: !!id && objectives.isLoading, airborne }
}

/** "Day 3" of a round, counted from its start in UTC. */
export function roundDay(start: string): number | undefined {
  const t = Date.parse(start)
  if (Number.isNaN(t)) return undefined
  return Math.max(1, Math.floor((Date.now() - t) / 86_400_000) + 1)
}
