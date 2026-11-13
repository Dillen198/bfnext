# Outside monitoring

Everything on the box watches everything else on the box. procman restarts
bfdb, the Manager restarts the bot, and the bot posts to `ops_channel`.
None of that helps when the box itself, its network or the bot goes down,
because the thing that would send the alert is the thing that died. Two
checks from outside close that gap. Both are optional, and both work on
the free tiers of the usual services.

## 1. Is the site up? Poll `/api/health`

`https://api.vectorstrike.org/api/health` is bfdb's cheap liveness probe.
It makes no database queries and caches the engine probe for 15 s, so
polling it every minute costs nothing. It always answers **200** while
bfdb is running. The body says whether the engine behind it answers:

```json
{ "ok": true, "bfdb": "ok", "instance": "vs1",
  "engine": { "reachable": true, "latency_ms": 41, "error": null },
  "active_round": { "id": 12, "number": 3, "start": "..." },
  "ingest": { "last_stat_at": "...", "lag_s": 4 },
  "db": { "size_bytes": 123, "disk_free_bytes": 456 }, "build": { } }
```

What the result tells you:

| What you see | Meaning |
|---|---|
| no answer, or 5xx | Caddy, the box or its network is down (502: Caddy is up but bfdb isn't) |
| 200 with `"ok": false` | bfdb is up but the engine (DCS + bflib) isn't answering: the mission is loading, DCS crashed, or netidx is down |
| 200 with `"ok": true` | the whole chain is up |

Set up an HTTP(S) **keyword** monitor (UptimeRobot, Better Stack, Uptime
Kuma, healthchecks.io's HTTP checks):

* URL `https://api.vectorstrike.org/api/health`, interval 1–5 min.
* Keyword `"ok":true` must be present. Alert after 2–3 failures in a row:
  DCS restarts for missions and updates, and `ok` is false for a few
  minutes each time.
* On a multi-instance box the plain URL checks the default instance. Add
  `?instance=<id>` (the id from `instances.json`) for each of the others:
  one monitor per instance you care about.
* Optional: a second monitor with no keyword, so "box down" and "engine
  down" alert separately.
* Optional: alert when `ingest.lag_s` grows large or
  `db.disk_free_bytes` gets small, if your service can check JSON fields.

Do this for the range box too, on its own API host.

## 2. Is the bot alive? A heartbeat

A check from outside can't see the bot. Discord alerts, auto-update,
procman and the OPS page all live in it. So the bot pings out instead,
and the monitor alerts when the pings **stop**. In
`config/plugins/fowlengine.yaml` (DEFAULT section):

```yaml
heartbeat:
  url: "https://hc-ping.com/<your-check-uuid>"
  interval_seconds: 60
  fail_when_bfdb_down: true
```

* **healthchecks.io**: create a check with period = `interval_seconds` and
  a grace of a few minutes, then paste its ping URL. While procman's bfdb is
  down the bot pings `<url>/fail`, so you hear about that too. Each ping's
  body is a one-line status ("bot up, bfdb up").
* **Uptime Kuma**: use a "Push" monitor and its push URL, with
  `fail_when_bfdb_down: false`. Kuma has no `/fail` path, so bfdb outages
  are left to check 1.
* **Better Stack / Cronitor**: use their heartbeat URL, as for Kuma.

The pings are sent every `interval_seconds` (minimum 15) from the bot's
event loop. If the bot hangs, the pings stop too. A failed ping is logged
once and retried on the next interval, and never affects the bot. Remove
`url` to turn the heartbeat off. Implementation: `heartbeat.py` in the
fowlengine plugin.

## Where alerts should go

Send them somewhere that doesn't depend on the box: email, a phone push or
SMS, or a Discord webhook. A webhook is fine, because Discord doesn't need
the bot to deliver it. Don't send them only to a channel that the bot
itself posts to.
