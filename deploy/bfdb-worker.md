# The bfdb voice/model worker (opt-in)

By default `bfdb.exe` runs everything itself: the stats database and the API,
and also GCI (AWACS calls over SRS), the campaign towers (ATC), the training
range's military ATC, the HQ strategist and the war diary's model and picture
calls. **Nothing changes unless you turn the worker on.**

With the worker on, a second process -- the same `bfdb.exe`, started as
`bfdb.exe --worker` -- runs those services instead:

```
 DCS + bflib/bfrange ──netidx──► bfdb.exe (API, DB, every engine RPC)
                                    ▲  127.0.0.1:8890, /internal/worker
                                    │  one WebSocket, shared secret
                         bfdb.exe --worker (GCI, ATC, range ATC, HQ strategist,
                                            diary prose and pictures)
                                    │
                                    ├──► SRS (voice), opus.dll, Piper / SAPI / Whisper
                                    └──► LLM, image and speech providers
```

What it buys:

- A voice fault, a misbehaving `opus.dll`, a model call that hangs or a leak
  in a voice loop takes down the worker, which is restarted on its own. The
  API, the dashboards, the bot's health check and the stats ingest are not
  touched. In-process, procman's only remedy was restarting bfdb.
- The LLM, picture and ATC voice keys are given to the worker only, not to
  the process that holds the login sessions and the admin API.

What it does not change: public URLs, the dashboards, `/ws/gci`,
`/api/gci/*` and `/api/range/atc*` (bfdb serves them from what the worker
sends), and how engine calls are made (the worker asks bfdb, which calls the
engine with its own netidx subscriptions and breaker).

## Turning it on (bot-managed box)

In `config/plugins/fowlengine.yaml`:

```yaml
bfdb:
  worker: true
  # worker_listen: "127.0.0.1:8890"   # only if 8890 is taken
```

Restart bfdb (`/feops bfdb_restart`, or the Manager). procman then starts
bfdb with `--worker-mode external --worker-listen 127.0.0.1:8890` and, right
behind it, `bfdb.exe --worker` with the same instances, GCI config, campaign
config and `news_llm_*` / `news_image_*` settings. `news_llm_key`,
`news_image_key`, `atc_llm_key` and `atc_tts_key` go to the worker's
environment only.

A `bfdb.exe` that predates the worker does not list `--worker-mode`; procman
then drops the flags and starts no worker, so voice stays in bfdb.

Turning it off again: remove `worker: true` and restart bfdb.

## By hand (no bot)

```powershell
bfdb.exe --db C:\FowlEngine\bfdb --instances instances.json ... --worker-mode external
bfdb.exe --worker --db C:\FowlEngine\bfdb --instances instances.json --config campaign.json `
         --news-llm-url ... --news-image-provider ... --log-file Logs\bfdb-worker.log
```

- The worker takes the same `--instances` (or `--base`/`--sortie`/
  `--gci-config`), `--config`, `--hq-*`, `--news-llm-*` and `--news-image-*`
  flags bfdb would. Give both the same files: the worker logs the instance ids
  it runs, and bfdb warns when the worker runs one it does not know.
- `--db` is only where the shared secret lives: `<db>.worker-secret`. bfdb
  creates it with a fresh secret at startup when it is missing; the worker
  waits for it. Or set `$BFDB_WORKER_SECRET` for both, or point both at the
  same `--worker-secret-file`. The worker never opens the database.
- `--worker-listen` (bfdb) and `--worker-url` (worker) must agree; the default
  is `127.0.0.1:8890`. The listener is separate from bfdb's API port, takes
  loopback callers with the secret only, and is never reachable through Caddy.

## Safety rules the worker follows

- **Voice never runs twice.** The worker starts nothing until bfdb welcomes
  it, and only a bfdb in external mode listens for it. A worker that loses
  bfdb reconnects for 60 s and then exits with code 3 (procman relaunches it,
  and it waits for bfdb again). Never run an in-process bfdb and a worker
  against the same SRS server.
- The worker may only ask bfdb for the engine procedures the voice services
  use (`query-gci`, `query-atc`, `query-range`, `range-atc`, `query-hq`,
  `hq-directive`).
- Both ends must be the same build: a protocol mismatch is refused with a log
  line naming both versions.

## What to check after turning it on

- bfdb's log: `worker mode external: ...`, `worker link listening on
  ws://127.0.0.1:8890/internal/worker`, then `worker link: worker connected
  (bfdb <ver>; instances [...]; diary writer <model>|none; pictures yes|no)`.
- The worker's log (`Logs\bfdb-worker.log` beside `bfdb.log`, or
  `procman-worker-boot.log` in bfdb's home): `connected to bfdb`, then the
  usual `live GCI enabled` / `range ATC enabled` lines.
- The OPS API's status (`bfdb.worker` in its JSON, what the Manager and OPS page read):
  `running: true`, and `relaunches` staying at 0.
- A GCI call on SRS, the dashboard's GCI transcript, a dashboard GCI
  transmission, and on a range box the ATC transcript and the live ATC feed.
- The next diary tick (every 10 min) and, if pictures are on, the next filed
  day's picture.

## Failure modes

| Symptom | Cause |
| --- | --- |
| `worker link: refused a connection with a wrong or missing secret` | The two read different secret files; check `--db` / `--worker-secret-file` / `$BFDB_WORKER_SECRET` |
| `speaks protocol N, this bfdb M` | Different builds; update both (the same exe) |
| GCI transmit says `the voice worker is not connected` | The worker is down or still waiting for its welcome |
| Diary keeps yesterday's prose, log says `writer failed ... worker:` | The worker is away; the day is retried with backoff and keeps its prose |
| Worker exits with code 3 every minute | bfdb is not in external mode, or its worker listener could not bind (see bfdb's log) |
