# Running several DCS server instances on one machine

One `bfdb.exe` fronts every DCS server on the box. There is **one database, one
dashboard, one public site and one wiki**; the dashboard grows a server selector
in its header, and every API route takes `?instance=<id>` (or `?server=<DCS
server name>`) to say which server it is about.

Nothing here changes `bflib.dll` — the engine is unaware there is a second
server. What has to be unique per instance is everything the two servers would
otherwise collide on.

---

## 1. What must be unique per instance

bfdb refuses to start if any of the first three collide, naming the offending
key. The rest fail more quietly, so check them by hand.

| Thing | Where it is set | Why |
|---|---|---|
| `netidx_base` | the instance's engine CFG (`ODFv2_CFG`) **and** its entry in the instances file | bflib publishes stats + RPCs under `<netidx_base>/<sortie>`. Two servers sharing a base cross their streams. |
| `export_port` | the instance's entry **and** `BF_PORT` in its `Scripts/Export.lua` | The `Export.lua` live-unit feed. Two servers on one UDP port means one listener never binds and that server's TACMAP stays empty. |
| `stats_jsonl` | the instance's entry | Each bflib writes its own; a shared file interleaves two campaigns. |
| `stats_dir` | the instance's entry | Same, for the netidx stats archive. |
| `engine_config` | the instance's entry | The admin CFG editor edits one server's CFG. |
| SRS port | `nodes.yaml` extension config, `srs_url`, and the instance's `gci.srs_port` | Each DCS instance runs its own SRS server. |
| GCI frequencies | the instance's `gci:` override | Two AWACS on one frequency talk over each other. |
| DCS ports (`dcs_port`, `bot_port`, `webgui_port`) | `nodes.yaml` | Standard DCSServerBot per-instance setup. |

A **single netidx resolver** serves both instances — they are separated by their
bases, not by separate resolvers. Don't run two.

Sortie (mission) names *may* repeat: rounds are tagged with their instance id in
the DB, and round selection filters on that tag rather than on the scenario name.

---

## 2. bfdb: the instances file

Start bfdb with `--instances <file.json>` instead of the single-server flags
(`--base`, `--sortie`, `--stats-jsonl`, `--stats-dir`, `--engine-config`,
`--srs-url`, `--gci-config`, `--export-port`). Passing both is an error.

See [`instances.sample.json`](../instances.sample.json) for a commented copy.

```json
{
  "default": "vs1",
  "instances": [
    { "id": "vs1", "label": "Vector Strike #1",
      "base": "/local/fowl/vs1",
      "stats_jsonl": "C:\\...\\DCS.vectorstrike_1\\Logs\\stats.jsonl",
      "stats_dir":   "C:\\...\\DCS.vectorstrike_1\\Logs\\stats",
      "engine_config": "C:\\...\\DCS.vectorstrike_1\\ODFv2_CFG",
      "export_port": 42001,
      "srs_url": "http://127.0.0.1:5002",
      "gci_config": "C:\\...\\DCS.vectorstrike_1\\gci.json",
      "dcs_server_name": "[VS] Vector Strike #1" },
    { "id": "vs2", "label": "Vector Strike #2",
      "base": "/local/fowl/vs2",
      "stats_jsonl": "C:\\...\\DCS.vectorstrike_2\\Logs\\stats.jsonl",
      "stats_dir":   "C:\\...\\DCS.vectorstrike_2\\Logs\\stats",
      "engine_config": "C:\\...\\DCS.vectorstrike_2\\ODFv2_CFG",
      "export_port": 42002,
      "srs_url": "http://127.0.0.1:5003",
      "gci_config": "C:\\...\\DCS.vectorstrike_2\\gci.json",
      "dcs_server_name": "[VS] Vector Strike #2 | Training" }
  ]
}
```

`id` is permanent: every round in the Sled DB is tagged with it, and it appears
in dashboard URLs. `dcs_server_name` must match the DCSServerBot server name
exactly — it is what lets the Discord plugin address an instance without knowing
bfdb's ids.

### Upgrading an existing single-server deployment

Nothing to do, and nothing to migrate. Without `--instances`, bfdb synthesizes
one instance called `default` from the flags it already had, and every round
already in the DB (which carries no instance tag) is treated as that instance's.
The old single-entry JSONL/archive replay cursors are moved into the new
per-instance trees on first start, so an upgraded bfdb resumes where it left off
rather than re-ingesting the whole stats history.

When you later add more instances, the existing history follows whichever
instance you name in `"default"`. Untagged rounds — everything recorded before
the upgrade — resolve to the default instance, so point `"default"` at the
server that has been running all along and its rounds, kills and pilot stats
stay exactly where they are. The `id` itself can be anything; it is the
`"default"` key that decides, not the literal string `default`.

---

## 3. Per-instance GCI

Each instance needs its own `gci.json`: its own SRS port, frequencies and
callsigns. Point at it with `gci_config` in the instances file.

If the DCSServerBot plugin manages bfdb (`bfdb.manage: true`), don't write these
by hand — put a `gci:` block on each instance in `fowlengine.yaml` and procman
renders `<home>\gci.<id>.json` for you, merging the instance's overrides over the
shared top-level `gci:` block.

---

## 4. Export.lua

Copy `scripts/Export.lua` into each instance's `Scripts/` folder, then give each
one its port. Either edit `BF_PORT` in that copy, or (so the same file works
everywhere) drop a one-line `Scripts/bf_export_port.lua` next to it:

```lua
return 42002
```

It must match that instance's `export_port`. If it doesn't, bfdb logs
`[<id>] DCS export listener on UDP 0.0.0.0:<port>` for a port nothing sends to,
and that server's `/ws/units` and TACMAP contacts stay empty.

---

## 5. DCSServerBot

### `nodes.yaml`

Standard per-instance setup, plus the `BFBinaries` extension pointed at each
instance's own `bflib.dll` and staging directory:

```yaml
  instances:
    DCS.vectorstrike_1:
      home: C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1
      bot_port: 6666
      webgui_port: 8088
      dcs_port: 10308
      extensions:
        SRS:
          config: 'C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\Config\SRS.cfg'
          port: 5002
        BFBinaries:
          bflib_dll_path: 'C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\Scripts\bflib.dll'
          staging_dir:    'C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\_staging'
    DCS.vectorstrike_2:
      home: C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_2
      bot_port: 6667
      webgui_port: 8089
      dcs_port: 10309
      extensions:
        SRS:
          config: 'C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_2\Config\SRS.cfg'
          port: 5003
        BFBinaries:
          bflib_dll_path: 'C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_2\Scripts\bflib.dll'
          staging_dir:    'C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_2\_staging'
```

A `bflib.dll` dropped into the admin channel is staged for **every** instance
that has a `BFBinaries` block, and each swaps it in on its own next DCS restart.

### `fowlengine.yaml`

Add an `instances:` list under `bfdb:` (see the commented block in
`fowlengine.sample.yaml`). procman then renders `instances.json` + the per-
instance `gci.<id>.json` files and starts bfdb with `--instances`; the flat
`netidx_base` / `stats_jsonl` / `stats_dir` / `engine_config` / `srs_url` /
`sortie` keys are ignored once `instances:` is present.

There is still exactly **one** bfdb process, one `--db`, one `listen_address`
and one `site_address`. Only the DCS-server-specific settings are per instance.

Everything the plugin polls or streams — status embed, alerts, achievements,
engine log relay, GCI transcript, perf embed, commander terminal — is already
per-DCS-server and now tags its requests with `?server=<name>`.

### Channels: one set per instance

Give **every** per-server channel its own id in that server's section, and
leave them out of `DEFAULT` entirely. A channel left in `DEFAULT` is inherited
by every server, so two servers post their campaign status to the same channel
and each overwrites the other's embed — which reads as the bot duplicating or
flip-flopping. The plugin logs a warning at startup naming any channel two
servers share.

```yaml
DEFAULT:
  brand_name: "Vector Strike"
  api_url: "http://localhost:8880"
  # Guild-wide, shared on purpose -- these stay in DEFAULT:
  welcome_channel: 900000000000000001   # fires on a Discord join
  ops_channel:     900000000000000002   # bfdb relaunch / staged binary swap
  # ... shared settings ...

'[VS] Vector Strike #1':
  cfg_path: 'C:\Users\...\DCS.vectorstrike_1\ODFv2_CFG'
  status_channel:         111111111111111111
  alerts_channel:         111111111111111112
  achievements_channel:   111111111111111113
  engine_log_channel:     111111111111111114
  perf_channel:           111111111111111115
  gci_transcript_channel: 111111111111111116

'[VS] Vector Strike #2 | Training':
  cfg_path: 'C:\Users\...\DCS.vectorstrike_2\ODFv2_CFG'
  status_channel:         222222222222222221
  alerts_channel:         222222222222222222
  achievements_channel:   222222222222222223
  # null (or omitted) = this server doesn't post that feed at all
  engine_log_channel:     null
  perf_channel:           null
  gci_transcript_channel: null
```

`alerts_channel` additionally fans out into per-faction **threads** under
itself — `Blue Ops`, `Red Ops` and `Neutral / Contested` — created per server,
so two instances sharing one alerts channel would also end up with two sets of
threads in it.

Per server: `status_channel`, `alerts_channel`, `achievements_channel`,
`engine_log_channel`, `perf_channel`, `gci_transcript_channel`,
`server_info_channel`. Guild-wide: `welcome_channel` (with `welcome_server` to
choose which server it describes) and `ops_channel` (there is only one bfdb).

---

## 6. Test / staging instances (`public: false`)

An instance with `"public": false` is internal:

* it is **omitted from `GET /api/instances`** unless the caller is a dashboard
  admin, so it never appears in a player's server selector (an admin sees it
  marked `[test]`);
* its rounds are **excluded from the all-time pilot totals** — the leaderboard,
  pilot profiles and the `/api/stats` round count are computed as if it did not
  exist, so testing cannot inflate anyone's record.

Everything else works normally: its rounds, kills, captures and objectives are
recorded, per-round views on it show real numbers, and an admin can select it
and use TACMAP, the objective list, the engine log and the commander terminal.

How the exclusion works is worth knowing, because it is not reversible after the
fact: `Pilot.total` is simply **not written** for a non-public instance's rounds,
so it *is* the public total by construction. It is deliberately not derived by
re-summing the per-round `aggregates` tree — that tree only gets a row when the
pilot was in a slot with a known vehicle, so a re-sum would silently under-count
everyone. The consequence: flipping an instance from `public: false` to `true`
does **not** retroactively credit what was already flown on it.

This is a visibility and accounting rule, **not an authorization boundary**. A
caller who knows the id can still read that instance's per-round data from the
public routes. The routes that expose genuinely sensitive things (`/ws/units`,
`/api/admin/*`, `/api/commander/*`, coalition-locked intel and briefing) keep
their own admin/coalition gates regardless.

---

## 7. What is shared and what is not

**Per instance:** rounds, kills, sorties, captures, deploys, objectives, trails,
recon intel (TARPS), the live unit feed, the TACMAP picture, the engine log,
weather, GCI, the admin CFG editor, the perf embed, campaign reset.

**Shared across instances:** pilot identities and their lifetime totals (a pilot
is a person, not a per-server account), the leaderboard, Discord account links,
the admin ban list, wiki *content*, and login sessions.

Wiki content is shared but **the numbers in it are not**. A page never hard-codes
a campaign figure; it writes `{{cfg:points.air_kill|350}}` and bfwiki resolves
that against `GET /api/wiki/facts?instance=<id>`, which serves an allow-listed,
player-safe subset of that instance's own `engine_config` (`WIKI_FACT_KEYS` in
`bfdb/src/main.rs` — the admin table, ban list, netidx base and CheckWX key are
excluded). The wiki grows a server selector in its top bar as soon as more than
one instance is configured, and pages that quote numbers say which server's they
are showing. An instance with no `engine_config` set returns an empty fact set,
and every placeholder falls back to the default written into the page.

That split is deliberate: `/api/leaderboard` and `/api/pilots` are global, while
`/api/rounds`, `/api/objectives`, `/api/kills` and friends default to the
selected instance. `/api/rounds?instance=all` returns every server's rounds, each
row tagged with its `instance`.

**Campaign reset** (`POST /api/admin/reset`, the dashboard's reset button) purges
only the selected instance's rounds when more than one instance is configured,
and re-derives pilot lifetime totals from what survives. With a single instance
it is the whole-database wipe it always was.

**Stats rebuild** (`--rebuild-stats`, `POST /api/admin/rebuild-stats`) is
inherently whole-database: the derived trees are shared, so it rewinds and
re-ingests every instance's stats. That is the intended behaviour, not an
oversight — rebuilding one instance alone would delete the others' derived data
with nothing to replay it back from.

---

## 8. Checking it works

```
GET /api/instances
```

returns every instance with its label, whether it has a live engine, and the
sortie currently publishing:

```json
{ "default": "vs1",
  "instances": [
    { "id": "vs1", "label": "Vector Strike #1", "default": true, "live": true,
      "sortie": "ODFv2", "active_round": { "id": 42, "scenario": "ODFv2", "start": "..." },
      "dcs_server_name": "[VS] Vector Strike #1" },
    { "id": "vs2", "label": "Vector Strike #2", "default": false, "live": true,
      "sortie": null, "active_round": null, "dcs_server_name": "[VS] Vector Strike #2 | Training" }
  ] }
```

`"sortie": null` on a `"live": true` instance means bfdb has a netidx base for it
but the mission has not reported in — that server is down, or its
`netidx_base`/sortie don't match. The dashboard's selector shows those as
`(down)`.

### Degraded objective reads

`GET /api/objectives` serves the persisted snapshot with the *running* engine's
owner/health overlaid on top — but only when its RPC to bflib succeeds. When
that call times out, the response silently falls back to stale persisted values.

Successive polls alternating between live and stale values look exactly like
objectives flipping owner and crossing health thresholds, which is what produced
endless `[NEUTRAL] X has gone neutral` and `[READY TO CAPTURE]` streams in
Discord. The response now says which it is:

* header `x-fowl-live: 1|0`, and
* a `"live": true|false` field on every entry.

Anything that **diffs successive polls to detect change** must ignore a response
where this is `0` — the Discord alert poller does, and additionally requires two
consecutive polls to agree before announcing an ownership change, with
hysteresis on the health threshold (weak at ≤20, clears only at ≥35).

In the bfdb log every per-instance line is prefixed with the instance id:

```
instance "vs2" (Vector Strike #2): base=/local/fowl/vs2 sortie=auto jsonl=... export_port=42002
[vs2] DCS export listener on UDP 0.0.0.0:42002
[vs2] starting JSONL reader from "...\stats.jsonl" at offset 0
[vs2] live GCI enabled
```

An unknown `?instance=` is a `400` with the id echoed back, not a silent fallback
to another server.
