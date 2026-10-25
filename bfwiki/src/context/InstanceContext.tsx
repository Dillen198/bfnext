import { createContext, useCallback, useContext, useMemo } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api, instanceStore, type ServerInstance, type WikiFacts } from '../api'
import { useInstanceSelection, type InstanceSelection } from '@shared/web/instance'
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
interface InstanceContextValue extends InstanceSelection<ServerInstance> {
  /** That instance's campaign numbers, or undefined while loading / unavailable. */
  facts: WikiFacts | undefined
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

  // Page content is shared between servers, but the fact set is not. A
  // remembered id that has gone stale may have had anything cached against it.
  const onSwitch = useCallback((reason: 'select' | 'stale') => {
    qc.invalidateQueries(reason === 'select' ? { queryKey: ['wiki', 'facts'] } : undefined)
  }, [qc])
  const selection = useInstanceSelection(instanceStore, data, onSwitch)
  const { selected } = selection

  // The numbers themselves. Scoped to the selected instance by `withInstance`
  // in api.ts, so the query key has to carry the id or a switch would serve the
  // previous server's values out of cache.
  const { data: facts } = useQuery({
    queryKey: ['wiki', 'facts', selected ?? 'default'],
    queryFn: api.wiki.facts,
    staleTime: 5 * 60_000,
    retry: 1,
  })

  const value = useMemo<InstanceContextValue>(() => ({ ...selection, facts }), [selection, facts])

  return <InstanceContext.Provider value={value}>{children}</InstanceContext.Provider>
}

// eslint-disable-next-line react-refresh/only-export-components
export function useInstance() {
  return useContext(InstanceContext)
}
