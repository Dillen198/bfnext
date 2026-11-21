import { createContext } from 'react'

/** Which server box the OPS page acts on, when Fowl Engine Manager drives
 *  several (it sets this from its server picker). Every confirm on the page
 *  then names that box. Null on the dashboard: it only ever has one. */
export const OpsTargetContext = createContext<string | null>(null)
