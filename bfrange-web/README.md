# bfrange-web — the training range site

The dashboard for the Vector Strike **training range** server
(`https://range.vectorstrike.org`): the live range picture, every graded
result as a card, the greenie board, per-pilot debriefs with insights and
qualifications, a spawner for adversaries/targets/tankers/ships, and
bombing / wind-over-deck / tanker calculators.

It is a static Vite + React app. Everything it shows comes from bfdb's
`/api/range/*` endpoints (see `bfdb/src/range/api.rs`); the wire types are
mirrored by hand from `bfprotocols/src/range/mod.rs` in `src/types.ts`.

| Route | Page |
|---|---|
| `/` | Live: map (range sectors with a colour key and side filter, players, stations + rings, tankers, carriers, arenas, spawns), carrier ops, tankers on station, stations, live results feed |
| `/greenie` | Greenie board |
| `/results` | All results, filterable (URL holds the filters) |
| `/stations/:id` | One target's impact group: CEP, mean point of impact |
| `/pilot/:ucid`, `/me` | Pilot record: scorecards, insights, qualifications, trends |
| `/result/:id` | Debrief: facts, interactive plot from the track, the rendered card, PNG / Tacview downloads |
| `/spawn` | Spawner (login + linked DCS account) |
| `/calc` | Bomb release, wind over deck, tanker planner |
| `/leaderboards` | Bombing CEP, strafe, LSO points, AAR, duel ELO, missile defence |

## Develop

```sh
npm install
npm run dev:mock   # no backend at all: fixtures from src/mock/ (http://localhost:5177)
npm run dev        # against the real API (VITE_API_BASE, default https://api.vectorstrike.org)
```

### Mock mode (`VITE_MOCK=1`)

`npm run dev:mock` is `vite --mode mock`, which loads `.env.mock`
(`VITE_MOCK=1`). The whole API client (`src/api.ts`) is then swapped for an
in-browser stand-in, `src/mock/server.ts`, which implements every endpoint the
site calls:

* ~45 days of history from a fixed seed (same data on every reload), every
  result kind with a realistic track: trap sheets whose shape follows the LSO
  comment, bomb tracks from the same ballistics model as the calculator,
  missile shots from a small pursuit simulation, AAR sessions in the tanker
  frame, BFM paths, helo approaches.
* The two reference cards from the design brief at fixed URLs:
  `/result/mock-ref-trap` (Casper, T-45, Case I, `WO` `AAX FIM (SLO)AR _HAW_`,
  wake alt 451 ft) and `/result/mock-ref-bomb` (GBU-16, 37 m @ 260°,
  INEFFECTIVE, F-14B at 10,611 ft / 317 kt / 78°).
* A live picture that moves with the clock, and a new result every 25 s so the
  feed animates.
* Spawn / despawn kept in memory, with the engine's rules (max 3 each,
  instructor-only items, relative-to-player needs you airborne).
* Card images are SVGs drawn in the browser (`src/mock/cards.ts`).

Who you are in mock mode: the login button logs you in as **Casper** (a
linked pilot). To test the other states set `localStorage['rangeMock.user']`
to `admin` (instructor), `unlinked` (Discord but no UCID) or `none`, and
`localStorage['rangeMock.offline'] = '1'` to make the range report offline.

A normal build never contains the mock: `__RANGE_MOCK__` is a compile-time
constant (`vite.config.ts` `define`), so the branch and its chunk are dropped.
`npm run build:mock` builds a static mock site if you need one to share.

### Against a local bfdb

Either allow `http://localhost:5177` with bfdb's `--cors-origin` and set
`VITE_API_BASE=http://localhost:8880`, or use the dev proxy: in `.env.local`

```sh
VITE_API_BASE=
VITE_BFDB_URL=http://localhost:8880
```

and the site calls a same-origin `/api` that Vite forwards to bfdb.

## Environment

All read at **build** time (Vite inlines them). See `.env.example`.

| Variable | Default | Meaning |
|---|---|---|
| `VITE_API_BASE` | `https://api.vectorstrike.org` | bfdb origin, no trailing slash. Empty = same-origin `/api` (dev proxy). |
| `VITE_RANGE_INSTANCE` | unset | Only set when one bfdb fronts several range servers: appended as `?instance=` to every call. Unset, bfdb picks its range instance. |
| `VITE_MOCK` | unset | `1` = serve everything from `src/mock/`. |
| `VITE_BFDB_URL` | `http://localhost:8880` | Dev proxy target when `VITE_API_BASE` is empty. |

Every request sends cookies (`credentials: 'include'`). Login is the same
Discord OAuth session as the dashboard and wiki:
`${API}/api/auth/login?return_to=<origin>/` (bfdb only accepts exactly the
origin plus `/`; the site remembers the page you were on and goes back to it).
`/api/range/me` returns `ucid: null` until the Discord account is linked to a
DCS pilot (`/linkme` in Discord, then `-linkme <token>` in DCS chat); the site
shows that guidance instead of the spawner and your record.

## Build and test

```sh
npm run build    # tsc -b && vite build -> dist/
npm test         # vitest: LSO parser, ballistics, WOD/AAR maths, mock coverage
npm run lint
```

`src/lib/ballistics.ts` is a line-for-line port of
`bfdb/src/range/ballistics.rs` (RK4 point mass, ISA, `k * 0.5 * rho * v^2 *
Cd_ref * A`). bfdb fits each weapon's drag scale `k` against real drops with
its copy, so the two must stay identical; `ballistics.test.ts` pins golden
values printed by the Rust itself. Change one, change both, and regenerate
those numbers.

## Deploy (Vercel)

Same shape as `bfwiki/` and `bfweb/`:

1. New Vercel project, **root directory `bfrange-web`**, framework Vite,
   build `npm run build`, output `dist`. `vercel.json` rewrites every path to
   `index.html` so deep links (`/result/...`) work.
2. Environment variables: `VITE_API_BASE=https://api.vectorstrike.org` (or
   leave unset for that default), and `VITE_RANGE_INSTANCE` only if needed.
3. Domains: add `range.vectorstrike.org`; at the DNS provider add
   `CNAME range → cname.vercel-dns.com`.
4. bfdb must allow the origin: add `https://range.vectorstrike.org` to
   `cors_origins` under `bfdb:` in `fowlengine.yaml` (procman passes each as
   `--cors-origin`), then `/feops bfdb_restart`. Cross-origin mode makes the
   session cookie `SameSite=None; Secure`, so bfdb must be on TLS. A missing
   origin shows up as CORS errors in the browser console while `curl`
   against the API works. Full server-side steps: `deploy/range.md`.

## Layout

```
src/
  api.ts            API client (+ lazy mock switch)
  types.ts          wire types, mirrored from bfprotocols/src/range/mod.rs
  lib/              pure logic: lso (parser port), grading (thresholds),
                    ballistics (bfdb port), wod, aarPlan, geo, format, headline
  components/       shared UI; plots/ holds the SVG debrief plots
                    (TrapSheet, BombPlot, StrafePit, AarPlots,
                    InterceptReplay, TrackMap/PathReplay)
  pages/            one file per route; live/, debrief/, calc/ hold their parts
  context/          auth (/api/range/me), theme, toasts
  mock/             VITE_MOCK fixtures and the in-browser API
```

Icons come from the repo-wide set in `shared/icons` via the `@icons` alias
(`Refuel` and `Calculator` were added there for this site).

Design notes: one score scale everywhere (the greenie board colours are also
the colours of every bomb/strafe/precision grade and 0–5 score), the amber
"meatball" of the Fresnel lens is the accent and the brand mark, numbers are
always monospace. Dark by default, with a light theme (remembered per
browser).

---

© 2026 Dillen Weerasinghe. All rights reserved. Proprietary — see the repository NOTICE file.
