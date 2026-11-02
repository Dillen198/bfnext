import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react'
import { SERVERS, type ServerProfile } from '@shared/servers/profiles'
import { useInstance } from './InstanceContext'

/**
 * Which server the wiki is dressed as.
 *
 * Picking a server in the top bar puts the whole wiki in that server's look
 * (shared/servers/themes.css), the way each server has its own page on the
 * public site. Picking a campaign also points the {{cfg:...}} numbers at it,
 * where this bfdb fronts it; the range runs on its own box, so picking it
 * changes the look only. No pick is the plain Vector Strike wiki.
 *
 * Remembered per browser. Storage can throw (private windows, blocked site
 * data), and then the choice lasts for the visit.
 */
const KEY = 'bfwiki-server'

function load(): string | undefined {
  try {
    return localStorage.getItem(KEY) ?? undefined
  } catch {
    return undefined
  }
}

interface ServerLookValue {
  /** The server the wiki is dressed as, or undefined for the plain wiki. */
  server: ServerProfile | undefined
  choose: (slug: string | undefined) => void
}

const ServerLookContext = createContext<ServerLookValue>({ server: undefined, choose: () => {} })

export function ServerLookProvider({ children }: { children: React.ReactNode }) {
  const [slug, setSlug] = useState<string | undefined>(load)
  const { instances, current, select } = useInstance()
  const server = SERVERS.find(s => s.slug === slug)

  // A new choice, or a remembered one once the server list arrives, points
  // the numbers at its campaign. Only then: a reader who picks other numbers
  // from a page's own selector afterwards keeps them.
  const listed = instances.length > 0
  useEffect(() => {
    const id = server?.instanceId
    if (id && listed && current?.id !== id && instances.some(i => i.id === id)) select(id)
    // eslint-disable-next-line react-hooks/exhaustive-deps -- once per choice and per list, not on every switch
  }, [server, listed])

  const choose = useCallback((next: string | undefined) => {
    setSlug(next)
    try {
      if (next) localStorage.setItem(KEY, next)
      else localStorage.removeItem(KEY)
    } catch {
      // Not remembered; it still applies for this visit.
    }
  }, [])

  const value = useMemo(() => ({ server, choose }), [server, choose])
  return <ServerLookContext.Provider value={value}>{children}</ServerLookContext.Provider>
}

// eslint-disable-next-line react-refresh/only-export-components
export function useServerLook() {
  return useContext(ServerLookContext)
}
