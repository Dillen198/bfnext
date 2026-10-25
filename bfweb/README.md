# bfweb

The Fowl Engine campaign dashboard (React + Vite): the live map, objectives,
kill feed, pilots and leaderboards, rounds, news, replays, the ground war and
HQ pages, recon intel, and the admin, config and ops pages. It talks to
`bfdb` over `/api` and `/ws`.

It ships three ways:

- **Embedded in bfdb.** `bfdb` compiles `bfweb/dist` in with `rust-embed` and
  serves it on its own origin. Build bfweb before bfdb (or give bfdb an empty
  `bfweb/dist`, which is what CI does).
- **On its own**, e.g. Vercel at `dashboard.vectorstrike.org`, with
  `VITE_API_BASE` pointing at bfdb. See `deploy/README.md` for the CORS and
  cookie side of that.
- **Inside DCS**: the cockpit overlay (`bfcockpit/`) shows `/cockpit` in DCS
  World's embedded Chromium 106, which is why `vite.config.ts` sets
  `build.target: 'chrome106'`.

## The second entry: Fowl Engine Manager

`manager.html` (`src/manager/`) is the UI of Fowl Engine Manager, the Tauri
desktop app in `bfmanager/`. It is built from this project so it can reuse
the dashboard's components, design tokens and whole pages: the OPS page runs
in it unchanged, over Tauri IPC instead of HTTP (`setTransport` in
`src/api.ts`, `src/manager/tauri.ts`).

```bash
npm run dev:manager     # dev server on :5190 (bfmanager's `tauri dev` starts it)
npm run build:manager   # -> ../bfmanager/dist (bfmanager's `tauri build` runs it)
```

The output goes to `bfmanager/dist`, outside this folder, with `manager.html`
renamed to `index.html` for Tauri.

## Development

```bash
npm ci
npm run dev                 # http://localhost:5173, /api and /ws proxied to a local bfdb on :8880
npm run dev -- --mode live  # proxied to the production API (.env.live): real data, read-only
```

Pages with a `?mock` flag (the briefing, the ground war, the HQ, the tacmap)
render from fixtures in dev builds, with no server at all. `src/lib/devAuth.ts`
offers fake identities (a pilot on each side, an admin, a pilot with no coalition) in dev builds only.

Checks, the same ones CI runs:

```bash
npm run typecheck   # tsc -b: the app, vite configs, and shared/
npm run lint        # eslint, plus shared/web (npm run lint:shared)
npm test            # vitest
npm run build       # -> dist/
```

## Environment

All read at build time; Vite inlines them.

| Variable | Default | What it does |
|----------|---------|--------------|
| `VITE_API_BASE` | `''` (same origin) | bfdb's origin, no trailing slash. Leave unset when bfdb serves the dashboard. |
| `VITE_STATUS_URL` | `https://www.vectorstrike.org/status.json` | The hand-written server notices (bfsite's `public/status.json`). |
| `VITE_BFDB_URL` | `http://localhost:8880` | Dev server only: where `/api` and `/ws` are proxied. |

The production addresses of the other sites live in `shared/web/urls.ts`;
code shared with the other front ends is in `shared/web` and `shared/icons`
(see their READMEs).

## Layout

| Path | What |
|------|------|
| `src/api.ts` | Every bfdb endpoint and its types |
| `src/pages/` | One file (or folder) per route; `groundwar/` is the ground war map |
| `src/scope/` | The radar scope (from peace-eye, see `scope/LICENSE.peace-eye`) |
| `src/replay/` | The flight replay (web Tacview) |
| `src/manager/` | Fowl Engine Manager's UI |
| `src/context/` | Auth, selected server instance, selected round |
| `scripts/` | One-off generators: replay models, terrain, wireframes, flags, the icon contact sheet |
| `public/` | Static files: models, aircraft images, wireframes, logos |

## Security headers (documented, not enabled)

`vercel.json` only rewrites routes to `index.html`. The headers below are the
recommended hardening for the standalone Vercel deployment. They are **not**
turned on: enable them deliberately, then check the map, the replay, intel
uploads and sign-in still work. Adjust `connect-src` if bfdb is not at
`api.vectorstrike.org`.

```json
{
  "headers": [
    {
      "source": "/(.*)",
      "headers": [
        { "key": "X-Content-Type-Options", "value": "nosniff" },
        { "key": "Referrer-Policy", "value": "strict-origin-when-cross-origin" },
        { "key": "Content-Security-Policy", "value": "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline' https://fonts.googleapis.com; font-src 'self' https://fonts.gstatic.com; img-src 'self' data: blob: https://api.vectorstrike.org https://server.arcgisonline.com https://cdn.discordapp.com; connect-src 'self' https://api.vectorstrike.org wss://api.vectorstrike.org https://www.vectorstrike.org https://server.arcgisonline.com; worker-src 'self' blob:; frame-ancestors 'none'; base-uri 'self'; form-action 'self'" }
      ]
    }
  ],
  "rewrites": [
    { "source": "/(.*)", "destination": "/index.html" }
  ]
}
```

Why each part:

- `frame-ancestors 'none'` stops other sites framing the dashboard
  (clickjacking on the admin pages). The cockpit overlay loads the page
  directly in DCS, not in a frame, so it is unaffected.
- `nosniff` stops a browser guessing a content type for a served file.
- `style-src 'unsafe-inline'` is needed: the dashboard styles with inline
  `style` attributes throughout.
- `worker-src blob:` is for MapLibre, which starts its workers from blobs.
- `img-src` / `connect-src` list bfdb (photos, news images, the API and its
  websockets), the satellite tiles, Discord avatars and the status file.
