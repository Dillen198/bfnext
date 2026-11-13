# bfrsr-web — the Red Storm Rising dashboard

The campaign dashboard for the **Red Storm Rising: Southern Flank** campaign
(NATO against the Soviet Union, 1985, on the Caucasus map). The campaign's
design is in `miz/Scenarios/Red Storm Rising/`; this app's in its
`DASHBOARD.md`.

It started as a copy of the main campaign dashboard (`bfweb/`) and is
redesigned for 1985: NATO Southern Command's situation room. It reads the
same bfdb API as bfweb, for one instance. Changes here never touch bfweb,
and fixes made in bfweb have to be ported by hand.

## Shared assets

The map imagery and 3D models (`images/`, `models/`, about 120 MB) are
bfweb's. `vite.config.ts` serves them from `../bfweb/public` in dev and
copies them into `dist` on build, so the repository holds one copy. A build
therefore needs bfweb's `public/` next to this folder.

## Develop

```sh
npm ci
npm run dev                 # /api proxied to a local bfdb on :8880
npm run dev -- --mode live  # the production API, read-only (.env.live)
npm run typecheck && npm run lint && npm test
```

In a local checkout `node_modules` can be a link to bfweb's (the lockfile
is the same apart from the name); the caches are kept in `.vite` and `.tmp`
so the two apps don't share them.

## Settings

| Variable | What |
|---|---|
| `VITE_API_BASE` | bfdb's public URL when the site is hosted apart from it |
| `VITE_INSTANCE` | The bfdb instance id of the Red Storm campaign. Set: the dashboard shows only that campaign and hides the server selector |

## Deploy

Like `bfrange-web`: a Vercel project with root `bfrsr-web/`, build
`npm run build`, output `dist`, with `VITE_API_BASE` and `VITE_INSTANCE`
set, on its own domain. bfdb runs the campaign as an instance with
`"theme": "redstorm"` (`deploy/multi-instance.md`).
