import { useQuery } from '@tanstack/react-query'
import { api, type ServerInstance } from './api'
import { SERVERS, type ServerProfile } from './config/servers'

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
 * says what each server is while the status reads "unavailable".
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
    offline: q.isError,
  }
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
  const airborne = instance?.active_round
    ? {
        blue: pilots.filter(p => p.side === 'Blue').length,
        red: pilots.filter(p => p.side === 'Red').length,
        total: pilots.length,
        known: !!online.data,
      }
    : { blue: 0, red: 0, total: 0, known: !!instance }

  return { front, frontLoading: !!id && objectives.isLoading, airborne }
}

/** "Day 3" of a round, counted from its start in UTC. */
export function roundDay(start: string): number | undefined {
  const t = Date.parse(start)
  if (Number.isNaN(t)) return undefined
  return Math.max(1, Math.floor((Date.now() - t) / 86_400_000) + 1)
}
