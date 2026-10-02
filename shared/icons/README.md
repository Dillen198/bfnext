# Vector Strike icon set

One set, shared by every front end in this repo — `bfweb` (dashboard),
`bfsite` and `bfwiki`. It lives outside all three so there is a single
definition of each glyph rather than three copies that drift.

Each app resolves it through the `@icons` alias, declared in that app's
`vite.config.ts` (`resolve.alias`, plus `server.fs.allow` so the dev server may
read outside its own root) and mirrored in its `tsconfig.app.json` `paths`.

```tsx
import { Airbase, Comms, Search } from '@icons'
```

Drawing conventions, and why the chrome glyphs are named after their lucide
counterparts, are documented at the top of `index.tsx`. Two rules worth
repeating here:

- **Write paths out literally.** `bfweb/scripts/icon-proof.mjs` reads this file
  statically to build the contact sheet; an icon assembled from a helper call
  renders blank there instead of failing loudly.
- **Keep one name per glyph.** Call sites that want a different word should
  alias on import (`import { Comms as Radio } from '@icons'`) rather than adding
  a second export.
