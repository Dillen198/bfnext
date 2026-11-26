import type { AuthUser } from '../api'

/**
 * A fake signed-in user, for development only.
 *
 * Most of the interesting pages are gated -- BRIEFING and RECON INTEL need a
 * resolvable coalition, ADMIN and CONFIG need an admin flag, MY PROFILE needs
 * a linked ucid -- and the only way to satisfy any of that for real is to
 * complete a Discord OAuth round trip against the production bfdb. That makes
 * routing, nav and layout work on those pages needlessly painful.
 *
 * WHAT THIS DOES AND DOES NOT DO
 * It substitutes the *client's* idea of who is signed in. It does not create a
 * server session, and it cannot: bfdb checks a real session cookie. So:
 *
 *   works  -- nav/lock states, route gating, MY PROFILE, page layout and
 *             chrome, anything served by a public endpoint
 *   fails  -- /api/briefing, /api/situation, /api/intel/*, /api/admin/*
 *             still answer 401/403, and the page will say so (QueryState)
 *
 * For the briefing specifically, pair this with `?mock` which renders that
 * page from fixtures and never calls the API at all.
 *
 * Every entry point here is behind `import.meta.env.DEV`, which Vite replaces
 * with the literal `false` in a production build -- so the whole module folds
 * away and there is no way to reach it from a deployed dashboard.
 *
 * KEEP THE GUARDS LITERAL. Consumers must test `import.meta.env.DEV` directly
 * rather than an imported boolean: when the JSX guard was the re-exported
 * DEV_AUTH_ENABLED, Rollup dropped the component but still shipped
 * DEV_PERSONAS and the "DEV SESSION" label into the production bundle.
 * Verify with: vite build, then grep the emitted js for "bfweb.devUser".
 */

const KEY = 'bfweb.devUser'

export const DEV_AUTH_ENABLED = import.meta.env.DEV

export interface DevPersona {
  id: string
  label: string
  hint: string
  user: AuthUser
}

function persona(id: string, label: string, hint: string, over: Partial<AuthUser>): DevPersona {
  return {
    id,
    label,
    hint,
    user: {
      discord_id: `dev-${id}`,
      username: `dev:${id}`,
      avatar: null,
      is_admin: false,
      ucid: null,
      side: null,
      ...over,
    },
  }
}

export const DEV_PERSONAS: DevPersona[] = !DEV_AUTH_ENABLED ? [] : [
  persona('blue', 'Blue pilot', 'Coalition pages unlock, no admin nav', { side: 'Blue' }),
  persona('red', 'Red pilot', 'Same, on the other side', { side: 'Red' }),
  persona('admin', 'Admin', 'Everything unlocks, including ADMIN + CONFIG', { is_admin: true, side: 'Blue' }),
  persona('nosside', 'Linked, no coalition', 'Exercises the NO COALITION explainer', {}),
]

/** The dev user currently in effect, if any. Always null in a real build. */
export function getDevUser(): AuthUser | null {
  if (!DEV_AUTH_ENABLED) return null
  try {
    const raw = localStorage.getItem(KEY)
    return raw ? (JSON.parse(raw) as AuthUser) : null
  } catch {
    return null
  }
}

/** Set (or with null, clear) the dev user. Callers reload so every cached
 *  query is rebuilt against the new identity. */
export function setDevUser(user: AuthUser | null): void {
  if (!DEV_AUTH_ENABLED) return
  try {
    if (user) localStorage.setItem(KEY, JSON.stringify(user))
    else localStorage.removeItem(KEY)
  } catch { /* private mode / storage disabled */ }
}

/** Attach a real ucid so MY PROFILE and the pilot pages resolve to someone
 *  with actual stats. Best effort -- a dev user without one still works. */
export function withUcid(user: AuthUser, ucid: string | null): AuthUser {
  return { ...user, ucid }
}
