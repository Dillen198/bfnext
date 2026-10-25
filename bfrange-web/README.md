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
| `/` | Range control: the live map fills the frame (sectors with a colour key and side filter, players, stations + rings, tankers, carriers, arenas, spawns, threats); the dock beside it has the situation (clock, wind, QNH) and tabs for the results feed, traffic, the boat, threats and stations. Picking a thing on the map or in the dock picks it in both; `?focus=<key>` opens with the camera on it |
| `/greenie` | Greenie board |
| `/results` | All results, filterable (URL holds the filters) |
| `/stations/:id` | One target's impact group: CEP, mean point of impact |
| `/pilot/:ucid`, `/me` | Pilot record: scorecards, insights, qualifications, trends |
| `/result/:id` | Debrief: facts, the interactive 2D plot from the track or the **3D replay** (2D / 3D switch), the result card (the LSO pass record / range score sheet / record form), card PNG, Tacview clip, **Web Tacview** (the session in the dashboard's flight replay, following the pilot, just before the result) and the full session download |
| `/spawn` | Spawner (login + linked DCS account): searchable catalogue, one item set up at a time, "again" for your last five, time left on everything you have up, a warning when an around-you spawn needs you airborne |
| `/brief` | Range brief: frequencies, TACANs, weather, every target and SAM site with coordinates (DDM / DMS / MGRS / decimal), laser codes, sectors; prints as a kneeboard card |
| `/calc` | Bomb release, wind over deck, tanker planner |
| `/leaderboards` | Bombing CEP, strafe, LSO points, AAR, duel ELO, missile defence |
| `/brand/` | Static brand sheet (`public/brand/`): the mark, favicon and lockups, with PNG downloads |

Every page sits in the same frame (`components/Shell.tsx`): a rail grouped by
what you are doing (range, debrief, compete, tools), a top bar that always
shows whether the range is live, and search on Ctrl/⌘ K or `/`
(`components/CommandPalette.tsx`: pages, stations, tankers, pilots flying now,
every pilot record). On a phone the rail becomes a drawer and the five
cockpit pages sit in a bottom tab bar. A linked pilot gets a toast with a
debrief link the moment one of their own results is graded, on any page.

## Develop

```sh
npm install
npm run dev:mock   # no backend at all: fixtures from src/mock/ (http://localhost:5177)
npm run dev        # /api proxied to a local bfdb on :8880
npm run dev -- --mode live   # /api proxied to production (.env.live), read-only
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

`npm run dev` already does this: the site calls a same-origin `/api` that
Vite forwards to `VITE_BFDB_URL` (default `http://localhost:8880`). To call
bfdb directly instead, allow `http://localhost:5177` with bfdb's
`--cors-origin` and set `VITE_API_BASE=http://localhost:8880` in `.env.local`.

## Environment

All read at **build** time (Vite inlines them). See `.env.example`.

| Variable | Default | Meaning |
|---|---|---|
| `VITE_API_BASE` | unset; `.env.production` sets `https://api.vectorstrike.org` | bfdb origin, no trailing slash. Unset = same-origin `/api` (the dev proxy). |
| `VITE_SNAPSHOT_URL` | unset (off) | bfdb's public snapshot, e.g. `https://snapshot.vectorstrike.org`, for the offline banner. See `deploy/snapshot-fallback.md`. |
| `VITE_RANGE_INSTANCE` | unset | Only set when one bfdb fronts several range servers: appended as `?instance=` to every call. Unset, bfdb picks its range instance. |
| `VITE_DASHBOARD_URL` | `https://dashboard.vectorstrike.org` (`shared/web/urls.ts`) | The dashboard; "Web Tacview" opens its `/replay/<rec>?focus=&t=` there. |
| `VITE_MOCK` | unset | `1` = serve everything from `src/mock/`. |
| `VITE_BFDB_URL` | `http://localhost:8880` | Dev proxy target when `VITE_API_BASE` is unset; `.env.live` points it at production. |

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
2. Environment variables: none needed, `.env.production` supplies
   `VITE_API_BASE=https://api.vectorstrike.org` (a project variable of the
   same name overrides it); `VITE_RANGE_INSTANCE` only if needed.
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
                    ballistics (bfdb port), wod, aarPlan, geo, coords (DDM/DMS/
                    MGRS), select (one key per live thing, shared by map, dock
                    and search), format, headline
  components/       shared UI (Shell, CommandPalette, RangeMark); plots/ holds the SVG debrief plots
                    (TrapSheet, BombPlot, StrafePit, AarPlots,
                    InterceptReplay, TrackMap/PathReplay)
  pages/            one file per route; live/ (map, dock, feed), debrief/, calc/ hold their parts
  components/three/ the 3D debrief: our own models built to each type's real
                    dimensions (models.ts), the carrier from the engine's deck
                    geometry (carrier.ts), the scene and cameras (scene.ts)
  context/          auth (/api/range/me), theme, toasts
  mock/             VITE_MOCK fixtures and the in-browser API
```

Icons come from the repo-wide set in `shared/icons` via the `@icons` alias
(`Refuel`, `Calculator`, `Copy`, `Check` and `Printer` were added there for this site).

Design notes: one score scale everywhere (the greenie board colours are also
the colours of every bomb/strafe/precision grade and 0–5 score), the amber
"meatball" of the Fresnel lens is the accent, numbers are always monospace.
Dark by default, with a light theme (remembered per browser).

### The result cards

bfdb draws them (`bfdb/src/range/forms.rs`, `cards.rs`) and posts the same
PNG to Discord: ink on paper, laid out as service forms with a
classification line and a form number. The carrier pass is an LSO pass
record (ship and pilot blocks, the grade and wire boxes, the LSO's
shorthand as written and in plain words, glideslope / lineup / AoA /
closure by groove position from the track, and the carrier class as a
wireframe in side elevation, plan and deck detail with the pass drawn on
it, from `bfdb/src/range/deckplan.rs`); a drop is a range score sheet; a
field landing is a pattern record (the pattern in plan with every leg
coloured, height against time, the final against a 3° glidepath, the
rollout and the taxi). Every other discipline has its own form on one shared
layout (`bfdb/src/range/sheets.rs`): the event and its parameters in boxed
fields, the score block, a drawing made from what that result records (the
strafe pit, the receiver in the tanker's frame, the missile engagement and
its range against time, the fight in plan, the anti-ship attack and the
weapon's height, the drop zone or the pad, the troop timeline, the gunnery
lane, CAS distances, the SEAD shot, the hot zone tally, the low-level route
and its gates, the CSAR timeline) and the instructor's remarks.

Every discipline has its own icon: the range pack in the shared icon set
(`shared/icons/index.tsx`, "Training range"), used for the kinds and the
stations here. The bot's Discord emoji are rendered from the same drawings
by bfdb (`WRITE_RANGE_ICONS=1 cargo test -p bfdb write_range_icons`,
`bfdb/src/range/icons.rs`), and every result post leads with its
discipline's emoji. In mock mode the trap, bomb and field
landing cards are bfdb's own rendering of its test fixtures (`src/mock/cards/`,
regenerate with `CARD_OUT=<dir> cargo test -p bfdb write_card_samples`).

### Connected to the dashboard's web Tacview

The dashboard's flight replay (`bfweb/src/replay`) reads every instance's
Tacview recordings, the range's included. A debrief's **Web Tacview** button
asks bfdb where the result is in it (`/api/range/result/<id>/replay`: the
recording, the pilot's flight, a moment 25 s before) and opens it there;
once the server has stopped writing that recording. The other way, a
recording of the range server gets a **Range** tab in the replay listing
every graded result in it (`/api/range/replay/<rec>/results`): jump to the
moment following that pilot, or open the debrief here.

### The debrief, 2D and 3D

The carrier pass sheet, the 3D model and bfdb's LSO card all draw the ship
from one file, `shared/carriers/decks.json` (read here by
`lib/carrierDeck.ts`, in bfdb by `bfdb/src/range/deckplan.rs`): Nimitz,
Forrestal, Admiral Kuznetsov, Charles de Gaulle and the Tarawa-class LHA,
traced from scale plans and overhead photographs (sources in the file):
flight deck, waterline, island, elevators, catapults or ski-jump runs,
helicopter spots, lens, LSO platform and a side elevation. Its grading
block is the engine's deck geometry (`CarrierKind::deck`: the landing point
on the target wire, the stern's offset from the landing-area centreline,
the deck angle, the wires), checked by a bfdb test, so the track meets the
deck where the engine graded it. Its "Deck" zoom shows the ramp, the wires (the caught one
highlighted), the touchdown point, the lens and the LSO platform.

A field landing debrief reads the pattern the way the engine did
(`lib/patternLegs.ts`, a port of `bfprotocols/src/range/pattern_legs.rs`):
initial, break, downwind, base and final walked back from the touchdown,
drawn to scale on the runway frame with the break, abeam, base and
wings-level marks, then height and speed against time, the final against a
3° ± 1° glidepath to the aim point, the touchdown on the runway, and the
rollout and taxi on imagery (red where it went over 30 kt). The engine keeps
following the aircraft after a full stop until it parks, its pilot leaves,
it takes off again or ten minutes pass, so the record has the taxi too:
time to clear the runway, the exit, taxi distance and time, top speed and
stops.

The 3D view plays a clip bfdb cuts from the server's Tacview recording
around the result (`/api/range/result/<id>/clip`, `bfdb/src/range/clip.rs`):
the pilot's aircraft with its recorded attitude, its weapons, the boat,
aircraft that came within 15 km and what they fired, ground units around the
impacts. Cameras: chase, cockpit, LSO (carrier passes, with the lens and
the LSO's numbers on the HUD), target (ground attack, with time to impact),
free orbit. Over land the ground is Esri imagery laid flat at the targets'
height (no terrain relief yet); at sea it is the sea. Aircraft, helicopters,
weapons, escorts and ground units are the dashboard's real models
(`bfweb/public/models`, one copy served here at `/models/` by
`vite.config.ts`; credits and licences in `CREDITS.txt` there, linked
from the 3D view), picked by the shared catalogue
(`shared/models/catalog.ts`) -- but only a type's own model: where the
dashboard lends a lookalike (a T-45 as an F-16), the range keeps its own
build to the type's published length, span and height, as it does while a
model loads. The carrier you land on keeps its traced, graded deck (paint,
wires, lens, LSO platform); for Nimitz class (the dashboard's CVN, the
Enterprise) and Kuznetsov the real hull and island go under it, fitted
stern to stern, keel to keel and flight deck to deck height from
`shared/models/ship-fit.json` (`bfweb/scripts/fit-ship-models.mjs`).
Forrestal, the LHA and Charles de Gaulle have no model of their own and
keep the traced build. The weapon models are CC BY-NC-SA (non-commercial).
three.js is only loaded when the 3D view is opened. In mock mode the
clip is made from the record's track (`src/mock/clip.ts`), attitude worked
out from the path.

The brand mark (`components/RangeMark.tsx`, the same drawing as
`public/brand/*.svg`) comes from the Vector Strike logo: the same ring broken
at the top right, the same strike arrow leaving through the gap, with the
letters swapped for a dashed scoring ring, the range's cross ticks and the
amber shack. Under ~24 px it switches to the favicon drawing. As the loading
indicator its scoring ring turns.

daisyUI is loaded for its form resets and owns some short class names
(`.dock`, `.drawer`, `.stat`, `.toast`...): the site's own classes avoid them
(`.rdock`, `.nav-drawer`, `.kpi`, `.toast-stack`).

---

© 2026 Dillen Weerasinghe. All rights reserved. Proprietary — see the repository NOTICE file.
