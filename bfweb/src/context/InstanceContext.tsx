import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api, getInstance, setInstance, type ServerInstance } from '../api'
import { useAuth } from './AuthContext'

/**
 * Which DCS server the dashboard is looking at.
 *
 * One bfdb can front several DCS servers on the same machine (see
 * `deploy/multi-instance.md`). The id lives in `api.ts` -- it is a plain
 * module variable so every request can pick it up without a hook -- and this
 * context is just the React-visible mirror of it, plus the list of instances
 * to choose from.
 *
 * Changing the instance invalidates the whole react-query cache: essentially
 * every cached response is scoped to one server, so keeping any of it would
 * show the previous server's data until each query happened to refetch.
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
  const [selected, setSelected] = useState<string | undefined>(() => getInstance())

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
  useEffect(() => {
    if (!data || !selected) return
    if (!instances.some(i => i.id === selected)) {
      setInstance(undefined)
      setSelected(undefined)
      qc.invalidateQueries()
    }
  }, [data, instances, selected, qc])

  const select = useCallback((id: string) => {
    if (id === selected) return
    setInstance(id)
    setSelected(id)
    // Everything cached belongs to the server we just left.
    qc.invalidateQueries()
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

  return <InstanceContext.Provider value={value}>{children}</InstanceContext.Provider>
}

export function useInstance() {
  return useContext(InstanceContext)
}
