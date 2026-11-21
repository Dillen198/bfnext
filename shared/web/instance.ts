// The React side of the selected DCS server instance, for apps that let the
// reader switch servers (the dashboard and the wiki). The id itself lives in
// an `InstanceStore` (api.ts) so every request can read it without a hook;
// this keeps a React-visible mirror of it, plus the list to choose from.

import { useCallback, useEffect, useMemo, useState } from 'react'
import type { InstanceList, InstanceStore } from './api'

export interface InstanceLike {
  id: string
  default: boolean
}

export interface InstanceSelection<I extends InstanceLike> {
  /** Every instance this bfdb fronts, in configured order. Empty while loading. */
  instances: I[]
  /** The selected instance's id, or undefined while it is still bfdb's default. */
  selected: string | undefined
  /** The selected instance's record, if the list has loaded. */
  current: I | undefined
  /** True when there is more than one server to choose between. */
  multi: boolean
  select: (id: string) => void
}

/**
 * @param data    `/api/instances`, once loaded (the app runs the query).
 * @param onSwitch Called after the selection changes, to drop or invalidate
 *   whatever the app cached for the previous server: on `select`, and when a
 *   remembered id turns out to be no longer configured (`stale`). Pass a
 *   stable function (useCallback).
 */
export function useInstanceSelection<I extends InstanceLike>(
  store: InstanceStore,
  data: InstanceList<I> | undefined,
  onSwitch: (reason: 'select' | 'stale') => void,
): InstanceSelection<I> {
  const [chosen, setChosen] = useState<string | undefined>(() => store.get())
  const instances = useMemo(() => data?.instances ?? [], [data])

  // A remembered id that is no longer configured (an instance was removed or
  // renamed) is dropped rather than sent, which would get a 400 on every request.
  const stale = !!data && !!chosen && !instances.some(i => i.id === chosen)
  const selected = stale ? undefined : chosen
  useEffect(() => {
    if (!stale) return
    store.set(undefined)
    onSwitch('stale')
  }, [stale, store, onSwitch])

  const select = useCallback((id: string) => {
    if (id === selected) return
    store.set(id)
    setChosen(id)
    onSwitch('select')
  }, [selected, store, onSwitch])

  const current = useMemo(
    () => instances.find(i => i.id === selected) ?? instances.find(i => i.default),
    [instances, selected],
  )

  return useMemo(() => ({
    instances,
    selected,
    current,
    multi: instances.length > 1,
    select,
  }), [instances, selected, current, select])
}
