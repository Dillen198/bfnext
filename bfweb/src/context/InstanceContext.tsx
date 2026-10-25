import { createContext, Fragment, useCallback, useContext, useEffect, useMemo, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api, getInstance, setInstance, type ServerInstance } from '../api'
import { useAuth } from './AuthContext'
import { campaign } from '../config/campaign'

/**
 * Which DCS server the dashboard is looking at.
 *
 * One bfdb can front several DCS servers on the same machine (see
 * `deploy/multi-instance.md`). The id lives in `api.ts` -- it is a plain
 * module variable so every request can pick it up without a hook -- and this
 * context is just the React-visible mirror of it, plus the list of instances
 * to choose from.
 *
 * Changing the instance drops the whole react-query cache: essentially every
 * cached response is scoped to one server, so keeping any of it would show the
 * previous server's data until each query happened to refetch.
 *
 * Dropping, not invalidating. The instance is not part of any query key -- it
 * rides along as a module variable in `api.ts` -- so an invalidated query keeps
 * serving its old data while it refetches, and react-query has no way to know
 * that data now belongs to a different server. Switching from the Syria server
 * to the Caucasus one therefore left the map sitting on Syria for as long as
 * the slowest request took (up to 8s while an engine is unreachable) before
 * everything swapped at once. Removing the entries instead makes each page show
 * its own loading state immediately and redraw as the new server's data lands.
 */
interface InstanceContextValue {
  /** Every instance this bfdb fronts, in configured order. Empty while loading. */
  instances: ServerInstance[]
  /** The selected instance's id, or undefined while it is still bfdb's default. */
  selected: string | undefined
  /** The selected instance's record, if the list has loaded. */
  current: ServerInstance | undefined
  /** True when there is more than one server to choose between. */
  multi: boolean
  select: (id: string) => void
}

/**
 * The side labels from /api/config, before any server's own names replace
 * them. Read once, on the first switch: main.tsx has applied the config by
 * the time anything renders.
 */
let configLabels: { blue: string; red: string } | null = null

/**
 * Put the selected server's names for its sides ("NATO" and "RSII",
 * "Georgia" and "Russia") into `campaign`, where every page reads them.
 * The campaign config holds one pair for the whole dashboard, so with two
 * servers both used to say BLUFOR and REDFOR whatever they were fighting.
 */
function applySideLabels(inst: ServerInstance | undefined): string {
  configLabels ??= { blue: campaign.blueLabel, red: campaign.redLabel }
  campaign.blueLabel = inst?.blue_label || configLabels.blue
  campaign.redLabel = inst?.red_label || configLabels.red
  return `${campaign.blueLabel}|${campaign.redLabel}`
}

/** Query keys whose responses span every server, not one of them. */
const INSTANCE_AGNOSTIC = new Set(['instances', 'rounds'])

const InstanceContext = createContext<InstanceContextValue>({
  instances: [],
  selected: undefined,
  current: undefined,
  multi: false,
  select: () => {},
})

export function InstanceProvider({ children }: { children: React.ReactNode }) {
  const qc = useQueryClient()
  const { user } = useAuth()
  const [chosen, setSelected] = useState<string | undefined>(() => getInstance())

  // The instance list is not instance-scoped, and changes only when bfdb is
  // restarted with a different --instances file -- but *who is asking* matters:
  // bfdb omits non-public (test/staging) instances from the list for anyone who
  // isn't a dashboard admin. Keying the query on the viewer's admin flag makes
  // it refetch when they sign in, otherwise an admin would keep the list that
  // was cached while they were still anonymous and never see the test server.
  const isAdmin = !!user?.is_admin
  const { data } = useQuery({
    queryKey: ['instances', isAdmin],
    queryFn: api.instances,
    staleTime: 5 * 60_000,
    retry: 1,
  })

  const instances = useMemo(() => data?.instances ?? [], [data])

  // Drop a remembered id that is no longer configured (an instance was removed
  // or renamed) rather than sending it and getting a 400 on every request.
  const stale = !!data && !!chosen && !instances.some(i => i.id === chosen)
  const selected = stale ? undefined : chosen
  useEffect(() => {
    if (!stale) return
    setInstance(undefined)
    qc.removeQueries({ predicate: q => !INSTANCE_AGNOSTIC.has(q.queryKey[0] as string) })
  }, [stale, qc])

  const select = useCallback((id: string) => {
    if (id === selected) return
    setInstance(id)
    setSelected(id)
    // Everything cached belongs to the server we just left -- except the two
    // queries that are about the whole bfdb rather than one server. Keeping
    // those stops the server selector itself blanking out mid-switch.
    qc.removeQueries({ predicate: q => !INSTANCE_AGNOSTIC.has(q.queryKey[0] as string) })
  }, [selected, qc])

  const current = useMemo(
    () => instances.find(i => i.id === selected) ?? instances.find(i => i.default),
    [instances, selected],
  )

  const value = useMemo<InstanceContextValue>(() => ({
    instances,
    selected,
    current,
    multi: instances.length > 1,
    select,
  }), [instances, selected, current, select])

  // Applied while rendering, so the pages below read the new names on this
  // very render. They read `campaign` directly rather than through a hook,
  // so the tree is remounted when the names change (a server switch, or the
  // list arriving) -- the switch has already dropped every cached query.
  const labels = useMemo(() => applySideLabels(current), [current])

  return (
    <InstanceContext.Provider value={value}>
      <Fragment key={labels}>{children}</Fragment>
    </InstanceContext.Provider>
  )
}

// eslint-disable-next-line react-refresh/only-export-components
export function useInstance() {
  return useContext(InstanceContext)
}
