# Snapshot fallback: last known state while the game server is down

The dashboard, the public site, the wiki and the range site are on Vercel, but
everything they show comes from bfdb on the game server box, through
`api.vectorstrike.org`. When that box is down they used to go blank. The only
thing still readable was the hand-written `status.json` on the public site.

With this set up, bfdb copies its **public** read routes into a Cloudflare R2
bucket every minute, plus straight away whenever a round starts or ends. When
the API doesn't answer (network error, timeout, or a 5xx from in front of
bfdb, such as Caddy's 502 or Cloudflare's 52x), the front ends load the same
data from the bucket instead and say so:

> Server offline: showing data from Sun 25 Oct, 12:36 UTC (25 min ago)

A 5xx that bfdb writes itself (a JSON error, for example while only the DCS
engine is down) means bfdb is up, so that error is shown as before.

- **Dashboard**: every public view (SITREP, objectives, front line, rankings,
  rounds, war diary, stats) shows the last known state. Live-only views
  (TACMAP, the GCI scope, the admin log streams) show **Server offline**
  instead of spinning. The top-bar badge reads OFFLINE.
- **Public site**: the cards keep the last known front line. The status page
  says *Offline since <time> · last mission ODFv2, round 4, 12 pilots on* for
  each server.
- **Wiki**: every page and the campaign numbers still load. The wiki is
  read-only (no edit or new-page buttons, and saving is paused) until bfdb is
  back.
- **Range site**: a "Server offline" banner, and an offline state on the live
  ATC page. Range results are not in the snapshot.

**A notice in `status.json` still wins.** When you write one, the banners show
it rather than the snapshot line.

Everything here is **opt-in**. With no `--snapshot-endpoint` bfdb publishes
nothing, and with no `VITE_SNAPSHOT_URL` the front ends behave exactly as
before.

```
 browsers ──► dashboard / site / wiki / range  (Vercel)
                 │                     │
                 │ live API            │ only when the API does not answer
                 ▼                     ▼
       api.vectorstrike.org     snapshot.vectorstrike.org   (R2 public bucket)
                 │                     ▲
                 ▼                     │ signed PUT of changed files, every 60 s
            bfdb on the box  ──────────┘
```

## What is published, and what never is

Only what an anonymous visitor can already read from the API. bfdb produces
each file by calling the route's own handler, so the JSON is the same as the
live answer, byte for byte:

| File | Same as |
|------|---------|
| `manifest.json` | When the snapshot was made (`generated_at`), the default instance, and the SHA-256 of every file |
| `g/instances.json`, `g/instances~kind-range.json` | `/api/instances` (signed out, so no test servers), `?kind=range` |
| `g/wiki/pages.json`, `g/wiki/pages/<slug>.json` | `/api/wiki/pages`, `/api/wiki/pages/<slug>` |
| `i/<id>/online.json` … `stats.json`, `news.json` | `/api/online`, `objectives`, `frontline`, `rounds`, `leaderboard`, `stats`, `news` |
| `i/<id>/rounds/<round>/summary.json` | `/api/rounds/<round>/summary`, for the running round and the last finished one |
| `i/<id>/wiki/facts.json` | `/api/wiki/facts` |
| `i/<id>/state.json` | This server's last known state: online, `last_seen` (UTC), mission, round, players, DCS and bfdb versions |

**Never published**: briefing, warehouse, situation, ground war, HQ, intel,
command, cockpit, units, trails, logs, sessions, health, or anything behind a
coalition or admin check. Instances with `"public": false` are never
published either. bfdb only uploads keys on an allow-list (`allowed_key` in
`bfdb/src/snapshot.rs`, which a test pins), and the front ends only ask for
the same set (`snapshotKey` in `shared/web/snapshot.ts`).

Uploads are change-only: a file whose hash matches the copy already in the
bucket is skipped. Wiki pages therefore go up once and then only when edited.
Files over 1 KB are stored gzip'd. Every object gets `Cache-Control: public,
max-age=30` (the manifest gets 10 s). A file that bfdb can't render this
minute (an engine RPC timing out, say) keeps its last copy. A file that is
gone (a deleted wiki page, a round summary that rotated out) is deleted.

## 1. Cloudflare: the bucket

In the Cloudflare dashboard for the account that has `vectorstrike.org`:

1. **R2 Object Storage → Create bucket.** Name it `vs-snapshot`. Location:
   Automatic. Default storage class: Standard.
2. **Make it public on our own domain.** Open the bucket, go to
   **Settings → Custom Domains → Connect Domain**, and enter
   `snapshot.vectorstrike.org`. Cloudflare creates the DNS record itself. Wait
   until the domain shows **Active**. Leave the `r2.dev` public URL
   **disabled**: it is rate-limited and not meant for production.
3. **CORS.** The front ends fetch from other origins. Under
   **Settings → CORS Policy → Add CORS policy**, paste:
   ```json
   [
     {
       "AllowedOrigins": [
         "https://www.vectorstrike.org",
         "https://vectorstrike.org",
         "https://dashboard.vectorstrike.org",
         "https://wiki.vectorstrike.org",
         "https://range.vectorstrike.org"
       ],
       "AllowedMethods": ["GET", "HEAD"],
       "AllowedHeaders": ["*"],
       "MaxAgeSeconds": 3600
     }
   ]
   ```
   Everything in the bucket is public anyway, so `"AllowedOrigins": ["*"]`
   is also fine. Use it if you want Vercel preview deployments to work too.
4. **A token that can only write to this bucket.** Go to
   **R2 Object Storage → Manage API tokens → Create API token**:
   - Permissions: **Object Read & Write**
   - Specify bucket(s): **Apply to specific buckets only → `vs-snapshot`**
   - TTL: Forever (or set a date and put a reminder in to rotate it)

   Create it, then copy the **Access Key ID** and **Secret Access Key** (the
   secret is only shown once). Also copy the **S3 endpoint**, which looks
   like `https://<account id>.r2.cloudflarestorage.com`. bfdb also needs
   *read* access: after a restart it reads `manifest.json` back, so it
   doesn't upload everything again.

## 2. The box: turn bfdb's publisher on

Put the two keys in files only the bfdb account can read, for example:

```
C:\bfdb\secrets\snapshot-key-id.txt        (the Access Key ID, one line)
C:\bfdb\secrets\snapshot-secret.txt        (the Secret Access Key, one line)
```

Then give bfdb these settings. Each flag can also be an environment variable;
a flag wins:

| Flag | Environment | Value |
|------|-------------|-------|
| `--snapshot-endpoint` | `BFDB_SNAPSHOT_ENDPOINT` | `https://<account id>.r2.cloudflarestorage.com` |
| `--snapshot-bucket` | `BFDB_SNAPSHOT_BUCKET` | `vs-snapshot` |
| `--snapshot-access-key-id-file` | `BFDB_SNAPSHOT_ACCESS_KEY_ID` (the value itself) | the key id file |
| `--snapshot-secret-access-key-file` | `BFDB_SNAPSHOT_SECRET_ACCESS_KEY` (the value itself) | the secret file |
| `--snapshot-public-url` | `BFDB_SNAPSHOT_PUBLIC_URL` | `https://snapshot.vectorstrike.org` (optional: bfdb checks once that the manifest is readable there and that CORS answers) |
| `--snapshot-interval` | `BFDB_SNAPSHOT_INTERVAL` | seconds between snapshots, default `60`, minimum 15 |
| `--snapshot-region` | `BFDB_SNAPSHOT_REGION` | leave unset for R2 (`auto`); on AWS, the bucket's region |
| `--snapshot-prefix` | `BFDB_SNAPSHOT_PREFIX` | optional folder inside the bucket, e.g. `prod/` |
| `--snapshot-dir` | `BFDB_SNAPSHOT_DIR` | instead of a bucket, write to a local folder (testing, or another static host) |

**bfdb started by DCSServerBot (`bfdb.manage: true`).** The `fowlengine`
plugin builds bfdb's command line from `fowlengine.yaml` and has no
`snapshot` settings yet. It does pass its own environment on to bfdb, so use
the environment variables. Set them for the Windows account the bot runs
under (a user variable, so other accounts on the box can't read the secret),
then restart the bot so it and bfdb pick them up:

```powershell
# run as the account DCSServerBot runs under; values are examples
[Environment]::SetEnvironmentVariable('BFDB_SNAPSHOT_ENDPOINT', 'https://<account id>.r2.cloudflarestorage.com', 'User')
[Environment]::SetEnvironmentVariable('BFDB_SNAPSHOT_BUCKET', 'vs-snapshot', 'User')
[Environment]::SetEnvironmentVariable('BFDB_SNAPSHOT_PUBLIC_URL', 'https://snapshot.vectorstrike.org', 'User')
[Environment]::SetEnvironmentVariable('BFDB_SNAPSHOT_ACCESS_KEY_ID', (Get-Content C:\bfdb\secrets\snapshot-key-id.txt), 'User')
[Environment]::SetEnvironmentVariable('BFDB_SNAPSHOT_SECRET_ACCESS_KEY', (Get-Content C:\bfdb\secrets\snapshot-secret.txt), 'User')
```

**bfdb started by `bfsystem.ps1` or by hand.** Add the flags to its argument
list:

```
--snapshot-endpoint https://<account id>.r2.cloudflarestorage.com
--snapshot-bucket vs-snapshot
--snapshot-access-key-id-file C:\bfdb\secrets\snapshot-key-id.txt
--snapshot-secret-access-key-file C:\bfdb\secrets\snapshot-secret.txt
--snapshot-public-url https://snapshot.vectorstrike.org
```

**Check it.** `bfdb.log` should say, at startup:

```
snapshot: publishing the public snapshot to https://<account id>.r2.cloudflarestorage.com/vs-snapshot/ every 60s
snapshot: readable at https://snapshot.vectorstrike.org
```

Then open `https://snapshot.vectorstrike.org/manifest.json` in a browser:
`generated_at` should be under a minute old. A misconfiguration is logged as
`snapshot: the public snapshot is OFF -- …` and bfdb carries on without it.
An upload failure is logged once (`snapshot: N file(s) failed, first: …`),
not every minute, and the next cycle retries.

## 3. Vercel: point the front ends at it

In each of the four Vercel projects (dashboard `bfweb`, site `bfsite`, wiki
`bfwiki`, range `bfrange-web`), go to **Settings → Environment Variables**,
add the following for **Production** (and Preview if you want it), then
**redeploy**. It is a build-time variable.

```
VITE_SNAPSHOT_URL = https://snapshot.vectorstrike.org
```

No trailing slash. Leaving it unset turns the fallback off for that app.

To check it, block `api.vectorstrike.org` in the browser's dev tools
(Network → right-click a request → Block request domain) and reload. The
banner should appear, and the pages should fill from `snapshot.vectorstrike.org`.

## Free tier

R2's free allowance (per month): 10 GB-month of storage, 1 million Class A
operations (writes, including PUT and DELETE), 10 million Class B operations
(reads), and no charge for egress.

- **Storage**: a few hundred KB per server plus the wiki, so well under 10 MB.
- **Writes**: each cycle uploads the manifest plus only the files that
  changed. With people flying, that is roughly 5 to 10 files per busy server
  per minute. Two busy servers around the clock is at most about 15 PUTs a
  minute, or **about 650,000 a month**. An idle server changes almost
  nothing. To leave more headroom, set `--snapshot-interval 120`, which
  halves it.
- **Reads**: browsers only read the bucket while the API is down. A manifest
  plus a handful of files per open page per minute is far inside 10 million.
  bfdb itself reads two or three files once at startup.

## Building and testing locally

- `cargo test -p bfdb snapshot` runs the publisher tests. They cover the
  allow-list against a real render, change-only uploads, a mock S3 that
  checks the signed requests, and AWS's own SigV4 example.
- `bfweb/src/snapshot.test.ts` tests the client: which request maps to which
  file, fallback on 5xx and network errors, POSTs never falling back, and the
  banner text.
- To see it in a browser: run bfdb with `--snapshot-dir <folder>`, serve that
  folder with any static server that sends `Access-Control-Allow-Origin: *`,
  and start a front end with `VITE_SNAPSHOT_URL` pointing at that server and
  `VITE_BFDB_URL` pointing at a port nothing listens on.

## Limits

- Images inside wiki pages and war-diary pictures are still served by bfdb,
  so they don't load while it is down. The text does.
- A round other than the running one and the last finished one has no
  summary in the snapshot, and the per-round views (`?round=`) and pilot
  pages fall back to their usual error.
- The snapshot is only as fresh as bfdb's last minute online. If the whole box
  dies, that is what `generated_at` and each server's `last_seen` show.
