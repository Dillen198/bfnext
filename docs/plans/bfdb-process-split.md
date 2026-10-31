# Plan: split voice/ATC/LLM/news out of bfdb, and main.rs into route modules

Status: **built** (audit finding f120). Phase 0 and an opt-in external worker
(phases 1-3, in a different shape than first proposed) are in. The default
deployment is unchanged. Operator guide: `deploy/bfdb-worker.md`.

## What was built

### Phase 0: route modules

`main.rs` keeps the command line, startup and `build_routes` (the route
order, with its comments, stays in one place). Handlers moved, unchanged, to
`http.rs` (error reply, paging, auth gates, shared filters), `botlink.rs`,
`engine.rs`, `logs.rs`, `tasks.rs` and `routes/{campaign, pilots, command,
ops, auth, admin, cockpit, wiki, intel, live, static_files}.rs`. The move
was one commit with the route tests passing unchanged.

### The worker: `bfdb --worker`, opt-in with `--worker-mode external`

- **Same exe, second mode**, not a second binary or crate: nothing new to
  build, sign, stage, auto-update or roll back; the worker is always the same
  build as bfdb, which the link checks (`PROTOCOL`).
- **Interface: one loopback WebSocket with a shared secret**, opened by the
  worker to a listener of bfdb's own (`--worker-listen`, default
  127.0.0.1:8890; not the API port, so never reachable through Caddy; loopback
  peers with the secret only). Not direct DB access: sled takes an exclusive
  lock (a second process cannot open it), the storage migration may swap the
  backend under `StatsDb`, and a voice process holding the DB is the coupling
  the split is meant to remove. Not the plan's "bfvoice serves its own
  loopback API": a worker that dials in needs no port of its own, and bfdb
  learns at once when it is gone.
- **Engine calls go through bfdb** (`worker/host.rs`, `EngineHost`): the
  services call `rpc_str` / `rpc_optional`, which in-process is the old
  `StatsDb` call and in the worker is a request over the link that bfdb makes
  with the same function. The instance's breaker, sortie discovery and netidx
  subscriptions stay in one place; the worker needs no netidx config. bfdb
  allows only the six procedures the services use.
- **Outputs come back over the link** (`worker/hub.rs`): GCI transcript lines
  (with a backlog on reconnect) into the same channel `/ws/gci` and
  `/api/gci/transcript` read; range ATC live-feed frames (audio as binary)
  into the same hub `/api/range/atc/live` reads. The range ATC transcript and
  audio, and dashboard GCI transmissions, are requests to the worker. Public
  URLs and their auth/CSRF/rate limits are unchanged.
- **The war diary stays in bfdb** (option 1 below): bfdb builds the digest
  and keeps the pictures' caps, metadata and store; only the prose call
  (`news_write`) and the picture draw (`news_draw`) go to the worker, which
  holds the model and provider keys. The writer the worker announces is kept
  across a worker restart, so a missed call keeps the day's prose rather than
  filing it from templates.
- **The HQ strategist** runs whole in the worker (it needs no DB).
- **One startup path for voice** (`worker::spawn_voice`), used by both modes,
  so they cannot start different services.
- **Never two voice stacks**: the worker starts nothing before bfdb welcomes
  it (only an external-mode bfdb has a listener), and exits (code 3) after
  60 s without bfdb; its supervisor restarts it and it waits again.
- **Supervision**: procman (`bfdb.worker: true`) starts the worker behind
  bfdb, relaunches it alone when it dies (30 s cooldown, a notice unless it
  was the code-3 give-up), stops it before bfdb, and gives the model/voice
  keys to it only. An older exe without `--worker-mode` keeps voice
  in-process. bfmanager's service and the release workflow need no change
  (same exe); the Manager could show `bfdb.worker` from the OPS status.

Not done: a separate crate for the voice code (phase 1's compile-time
boundary). `EngineHost` is the explicit interface it would build on.

The original plan follows.

## Why

bfdb is the stats database, the HTTP/WebSocket API for every web app, and
also about 21k lines of unrelated real-time and third-party work:

| Module | Lines (approx.) | What it does |
|---|---|---|
| `gci.rs` | 2.6k | proactive AWACS calls over SRS |
| `atc.rs` | 1.0k | campaign tower calls |
| `milatc/` | 9k | the training range's military ATC: STT, intent, controllers, live feed |
| `voice/` | 1.5k | SRS client, Opus (opus.dll via libloading), TTS (Piper/SAPI), STT |
| `news.rs`, `news_llm.rs`, `news_image.rs` | 5.4k | the war diary: digest, LLM prose, pictures |
| `hq_strategist.rs` | ~0.6k | LLM posture for the engine's theatre HQ |

What that costs today:

- **Blast radius.** A panic, deadlock or leak in a voice loop, a native DLL
  (opus.dll) misbehaving, or a hung LLM/image HTTP call shares a process
  with the API every dashboard, the bot's health check and the DCS stats
  ingest depend on. procman restarts bfdb on a failed `/api/health`, which
  takes the API down to recover from a voice fault.
- **Build and review cost.** `main.rs` is ~10.5k lines with ~130 handlers;
  `build_routes` alone is ~1.4k lines. Every change rebuilds and re-links
  the whole thing.
- **Secrets.** LLM, image-provider and SRS credentials live in the same
  process as the session store and admin API.

## Constraint that shapes everything: sled is single-process

The stats database is a sled `Db`. sled takes an exclusive lock: a second
process cannot open it. So a split-out process **cannot share `StatsDb`**.
Whatever moves out must either need no database, or reach it through bfdb.

What each candidate actually touches (from a grep of `crate::` and `db.`):

- **gci / atc / milatc / voice**: almost no database. They use
  `InstanceState` for engine RPCs (`call_engine_rpc_str`, which is netidx --
  any process can do it), the instance registry, and their own config
  (`gci.json`). Their outputs are transcripts (`/ws/gci`,
  `/api/gci/transcript`, `/api/range/atc*`), the range live feed and audio,
  and one input: `POST /api/gci/transmit` (`gci::human_call`).
  **Clean cut.**
- **news / news_llm / news_image**: read kills, captures, objectives, pilot
  names, round starts; write the `news_json`, `news_image_*` trees.
  **Database-bound.**
- **hq_strategist**: reads the campaign through engine RPCs, writes back
  through engine RPCs. **Clean cut** (it shares only the LLM client with
  news).

## Proposal

### Phase 0 -- route modules (no behaviour change, do first)

`build_routes(RouteCtx)` and `route_tests.rs` exist since f70, which makes
this mechanical and checkable.

1. Move shared HTTP plumbing out of `main.rs` into `http.rs`: `Error`,
   `json_response`, `page`/`paged_response`, `Q`, `require_session` /
   `require_admin` / `require_role`, `require_coalition`, `audit`,
   `with_db` / `with_instance` / `with_bot_link_cfg`, `extract_session_cookie`.
2. One module per area under `src/routes/`, each exposing
   `pub(crate) fn routes(rc: &RouteCtx) -> BoxedFilter<(Response,)>` and
   owning its handlers and body types:
   `public.rs` (rounds, leaderboard, pilots, objectives, stats ...),
   `auth.rs`, `admin.rs` (incl. OPS proxy, cfg editor, bans, audit),
   `cockpit.rs`, `intel.rs`, `command.rs` (+ groundwar/hq orders), `wiki.rs`
   (+ news routes), `ws.rs` (units, tacmap, groundwar, logs, gci),
   `static_files.rs` (embedded SPA/site, the three bfsite paths).
3. `build_routes` keeps only the composition. **Order matters** (the CSRF
   guard first, the rate guard, the instance guard, the GET group before the
   SPA catch-all, POST groups after): keep the composition in one place with
   the existing comments, and grow `route_tests.rs` to pin every ordering
   rule before moving anything.
4. Keep `RouteCtx` as the single dependency bag; modules take `&RouteCtx`
   and clone what they need.

Done = `main.rs` holds `Args`, startup and `build_routes`; every existing
route test passes unchanged; `cargo build` time for a handler change drops
measurably.

### Phase 1 -- voice as a library crate, still in-process

Move `gci.rs`, `atc.rs`, `milatc/`, `voice/` into a workspace crate
`bfvoice` (lib). Its interface to bfdb is a trait it is handed at start:

```rust
trait VoiceHost: Send + Sync + 'static {
    async fn engine_rpc(&self, inst: &str, proc: &str, args: Vec<(&str, Value)>) -> Result<String>;
    fn instances(&self) -> &Registry;
}
```

plus channels out (transcript lines, live ATC frames) and in (human GCI
calls). bfdb implements `VoiceHost` with what it has today. Nothing changes
for operators; the crate boundary forces the coupling to be explicit and
lets `bfvoice` build and test on its own.

### Phase 2 -- `bfvoice` as its own process (opt-in)

- New binary `bfvoice` in the same crate: loads the `--instances` file and
  the GCI configs, implements `VoiceHost` itself (its own netidx
  subscriber -- RPCs already go engine-direct), and serves a loopback-only
  HTTP/WebSocket API: transcript snapshot, transcript stream, range ATC
  transcript/live/audio, and `POST /transmit`.
- bfdb gains `--voice external=<http://127.0.0.1:PORT>` (default: the
  in-process library, i.e. today). When set, bfdb starts no voice tasks and
  proxies the existing public routes (`/ws/gci`, `/api/gci/*`,
  `/api/range/atc*`) to bfvoice, keeping its own auth/CSRF/rate limits in
  front. Public URLs do not change.
- Supervision: DCSServerBot procman and bfmanager's service start/stop
  bfvoice beside bfdb, health-check it separately, and restart it without
  touching bfdb. Release packaging ships `bfvoice.exe` and opus/Piper
  alongside.

### Phase 3 -- the war diary and the HQ strategist

- `hq_strategist` moves into a `bfllm` process (or into bfvoice's process
  if one extra service is preferable to two), sharing the LLM client with
  the diary's writer. It needs no database.
- The diary is database-bound. Two options:
  1. **Keep the digest in bfdb, move only the slow calls out.** bfdb builds
     `NewsDigest` (cheap, DB reads) and posts it to bfllm, which calls the
     LLM and image provider and posts the prose/picture back to a
     loopback-only bfdb endpoint that writes the trees. bfdb never waits on
     a third-party HTTP call or holds those credentials. **Recommended.**
  2. Move all of news out and give it read access through an internal bfdb
     API. More endpoints, more chances to diverge; not worth it for a
     once-a-day job.

## Risks and how to handle them

- **Route order regressions** (Phase 0). The history in `build_routes`'
  comments is a list of exactly these. Mitigation: extend `route_tests.rs`
  first; move one area per commit.
- **Two processes, one SRS server.** bfvoice must be the only SRS client
  set: never run in-process voice and bfvoice together (bfdb refuses to
  start voice tasks when `--voice external` is set, and bfvoice should take
  a lock file).
- **Deployment surface.** A new service in procman, bfmanager, the release
  workflow and the docs. Keep the in-process default until the external
  mode has run on the test server for a while.
- **Config drift.** Both processes read the same `--instances` and GCI
  files; document that, and have bfvoice log the instance ids it loaded so a
  mismatch is visible.
- **Latency.** Proxying the transcript WebSocket adds a hop on loopback --
  negligible next to SRS/TTS latency.

## Order and rough size

1. Phase 0: 2--3 days, mostly mechanical, high value on its own.
2. Phase 1: 2 days.
3. Phase 2: 3--5 days incl. procman/bfmanager/release changes and a soak on
   the test server.
4. Phase 3 (option 1): 2 days.

Each phase ships on its own and leaves the default deployment unchanged.
