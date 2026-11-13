# shared/web

Code the four front ends (`bfweb`, `bfsite`, `bfwiki`, `bfrange-web`) would
otherwise each keep a copy of, and let drift. Like `shared/icons` it is plain
source, not a package: each app compiles it as part of its own build, and each
app keeps its own `package.json`, lockfile and Vercel deploy.

| File | What |
|------|------|
| `api.ts` | Request deadlines, bfdb error text, websocket URLs, `AuthUser`, the selected-instance store |
| `auth.tsx` | `AuthProvider` / `useAuth` over `/api/auth/me` (dashboard, wiki) |
| `instance.ts` | `useInstanceSelection`, the React mirror of the instance store (dashboard, wiki) |
| `theme.tsx` | `ThemeProvider`, `useTheme`, `ThemeToggle` |
| `LogoMark.tsx` | The campaign logo, light/dark |
| `ErrorBoundary.tsx` | Crash screen; reloads once on a stale route chunk after a deploy |
| `status.ts` | The hand-written status file (`bfsite/public/status.json`) and its helpers |
| `snapshot.ts` | Fallback to bfdb's public snapshot while bfdb is down (`VITE_SNAPSHOT_URL`), the offline state and banner text; see `deploy/snapshot-fallback.md` |
| `notifications.ts` | Notification bell read state |
| `urls.ts` | The production `*.vectorstrike.org` addresses |

## Importing

Each app resolves `@shared` to this repo's `shared/` folder, declared in its
`vite.config.ts` (`resolve.alias`) and mirrored in `tsconfig.app.json`
(`paths`), next to the `@icons` alias:

```ts
import { ThemeProvider } from '@shared/web/theme'
```

Inside `shared/web`, import siblings relatively (`./api`, `../icons`).

## Dependencies

`shared/` has no `node_modules`. Its only imports are `react` and
`@tanstack/react-query`, which every app has; each app's Vite config and
tsconfig map those two to its own copy (exact-match regex aliases in Vite,
`paths` in TypeScript). Adding any other dependency here means adding the
same mapping to all four apps, so prefer not to.

Code here is type-checked under each app's compiler options, so it has to pass
the strictest of them (`noUnusedLocals`, `verbatimModuleSyntax`,
`erasableSyntaxOnly`). Its tests live in `bfweb/src/shared.test.ts(x)`.
