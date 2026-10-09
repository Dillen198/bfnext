import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api, getInstance, setInstance, type ServerInstance, type WikiFacts } from '../api'
import { useAuth } from './AuthContext'

/**
 * Which DCS server the wiki is describing.
 *
 * One bfdb can front several DCS servers, and each runs its own engine config
 * — so "an air kill is worth N points" has a different answer per server. The
 * prose is shared; the numbers come from `/api/wiki/facts` for the selected
 * instance and are substituted into `{{cfg:...}}` placeholders at render time
 * (see `WikiMarkdown`).
 *
 * The selected id lives in `api.ts` as a plain module variable so every request
 * picks it up without a hook; this context is the React-visible mirror, plus
 * the instance list and the resolved fact set.
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
  /** That instance's campaign numbers, or undefined while loading / unavailable. */
  facts: WikiFacts | undefined
  select: (id: string) => void
}

const InstanceContext = createContext<InstanceContextValue>({
  instances: [],
  selected: undefined,
  current: undefined,
  multi: false,
  facts: undefined,
  select: () => {},
})

export function InstanceProvider({ children }: { children: React.ReactNode }) {
  const qc = useQueryClient()
  const { user } = useAuth()
  const [selected, setSelected] = useState<string | undefined>(() => getInstance())

  // Keyed on the viewer's admin flag: bfdb omits non-public (test/staging)
  // instances from the list for anyone who isn't an admin, so an admin signing
  // in has to refetch or they keep the anonymous list.
  const isAdmin = !!user?.is_admin
  const { data } = useQuery({
    queryKey: ['instances', isAdmin],
    queryFn: api.instances,
    staleTime: 5 * 60_000,
    retry: 1,
  })

  const instances = useMemo(() => data?.instances ?? [], [data])

  // The numbers themselves. Scoped to the selected instance by `withInstance`
  // in api.ts, so the query key has to carry the id or a switch would serve the
  // previous server's values out of cache.
  const { data: facts } = useQuery({
    queryKey: ['wiki', 'facts', selected ?? 'default'],
    queryFn: api.wiki.facts,
    staleTime: 5 * 60_000,
    retry: 1,
  })

  // Drop a remembered id that is no longer configured (renamed or removed)
  // rather than sending it and getting a 400 on every request.
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
    // Page content is shared, but the fact set is not.
    qc.invalidateQueries({ queryKey: ['wiki', 'facts'] })
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
    facts,
    select,
  }), [instances, selected, current, facts, select])

  return <InstanceContext.Provider value={value}>{children}</InstanceContext.Provider>
}

export function useInstance() {
  return useContext(InstanceContext)
}
