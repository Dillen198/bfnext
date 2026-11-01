# Red Storm Rising: switching the 2008 server to Southern Flank

The campaign box runs two campaign servers: **Modern Syria** (`vs1`,
`DCS.vectorstrike_1`) and **Caucasus 2008** (`vs2`, `DCS.vectorstrike_2`).
This retires the 2008 campaign and runs **Red Storm Rising: Southern Flank
1985** in its place: same box, same DCS instance, same ports, a new bfdb
instance id. Syria is not touched.

`<SG>` below is the box's Saved Games folder (`C:\Users\<user>\Saved Games`).
The config copies in this repo are older than the box's; edit the files on the
box.

## What you need first

| Item | Where it comes from |
|---|---|
| `RSR_SF.miz`, `RSR_SF_CFG` | `miz/Scenarios/Red Storm Rising/SouthernFlank/` (built from `E:\Saved Games\DCS\Missions\Vector\redstorm\`) |
| Engine build with E3-E7 | `bflib.dll` and `bfdb.exe` from the `vector` branch once that work is committed (release builds) |
| Bot plugin | DCSServerBot `plugins/fowlengine` from the same commit (branding, theme pass-through) |
| Dashboard | Already deployed: https://bfrsr-web.vercel.app, then https://redstorm.vectorstrike.org once DNS is set |

## Names used

| | Value |
|---|---|
| bfdb instance id | `redstorm` (the dashboard is built with `VITE_INSTANCE=redstorm`; don't change one without the other) |
| DCS server name | `[VS] Vector Strike | Red Storm Rising 1985` (pick yours; it must match in `serverSettings.lua`, `servers.yaml`, `fowlengine.yaml` and `bfdb.instances`) |
| Mission sortie | `RSR_SF`, so bflib reads `RSR_SF_CFG` and saves state to `RSR_SF` |
| netidx base | `/local/fowl/redstorm` |
| Brand | `Southern Flank 1985` (placeholder; one setting) |

## Alternative: its own DCS instance (`DCS.RSR`)

The live box went this way: Red Storm runs on a new DCSServerBot instance,
`DCS.RSR`, next to `DCS.vectorstrike_2`, so 2008 can stay up or be retired
later. A copy of the 2008 folder brings the 2008 ports, mission list and
weather templates with it, and each one has to change. Bot log symptoms
when they don't:

- `nodes.yaml: Schema validation failed: Port ... is already in use`;
- `Server ... shares its DCS port with server ...! Registration aborted`.
  This stops **every** server registering, Syria included;
- `bfdb.exe exited immediately ... reuses UDP export port`. That takes
  every dashboard down, Syria's too.

| | 2008 (`DCS.vectorstrike_2`) | Red Storm (`DCS.RSR`) |
|---|---|---|
| `dcs_port` | 10318 | 10328 |
| `webgui_port` | 8090 | 8092 |
| `bot_port` | (its own) | 6669 (already unique) |
| SRS port (`extensions.SRS.port`, bfdb `gci.srs_port`) | 5004 | 5006 |
| Tacview real-time / remote control | 42676 / 42677 | 42678 / 42679 |
| Export port (`bf_export_port.lua`, bfdb `export_port`) | 42002 | 42003 |
| bfdb instance id / netidx base | `vs2` / `/local/fowl/2008` | `redstorm` / `/local/fowl/redstorm` |
| Server name | `[VS] Vector Strike \| Dynamic Campaign PVP 2008` | `[VS] Vector Strike \| RSR` |

The range runs on its own box, so the ports `deploy/range.md` planned for a
range instance here (10328, 8092) are free; the live box uses them.

In `<SG>\DCS.RSR`:

- `Scripts\Hooks\` has bflib's hook loader (`bflib/lua/hooks.lua`, under the
  name the other campaign instances use: the file that ends in
  `bflib.initHooks()`). Without it the mission loads but bflib logs `waiting
  for the mission to finish loading` every second, forever, because the hook
  is what tells it the load has ended. Hooks load when DCS starts, so restart
  the DCS server after adding it, not only the mission.
- `Scripts\bf_export_port.lua` holds one line: `return 42003`.
- In `Config\serverSettings.lua`, `["name"]` is the server name above.
  `missionList` has only `<SG>\DCS.RSR\Missions\RSR_SF.miz`; the copy still
  lists `DCS.vectorstrike_2\Missions\rgw2008_*.miz`.
- `RSR_SF_CFG` is in the folder root, with `RSR_SF.miz` in `Missions\`.
- There is no `RGW2008_CFG` and no `RGW2008` save. Leftovers make it easy to
  run a second 2008 campaign by mistake.
- `Logs\stats.jsonl` and `Logs\stats\` are absent or empty.
- `Config\SRS.cfg` and the Tacview settings in `Config\options.lua` match
  the ports above if the bot didn't rewrite them. Set Tacview's
  `tacviewExportPath` to `C:\VectorStrike\Tacview\RSR` (the instance's
  `tacview_dir`); the copy still writes to `VS2`.

In `nodes.yaml`, under `DCS.RSR`:

- set the ports from the table;
- point `BFBinaries` at `DCS.RSR`;
- for **`BFWeather`**, either point `base`/`weapon`/`options`/`warehouse` at
  the Red Storm templates with `metar_station: UGSB`, or remove the block.
  Copied as it is, it rebuilds the 2008 mission into this server.

Open UDP/TCP 10328 in the firewall the way 10318 is.

The bfdb instance and the per-server section of `fowlengine.yaml` are the
same as in steps 3 and 4, with `DCS.RSR` paths, `export_port: 42003`,
`gci.srs_port: 5006` and the server name above. `vs2` keeps its entry while
2008 runs; retire it to the archive entry in step 3 when 2008 closes.

### Water probe (once per map layout)

The build checks garrison positions against the water it knows about, but
only DCS knows every river. If a garrison unit lands in water, bflib stops
the mission at start with `placing group <name>: you can't spawn this unit in
water`. The probe asks DCS about every position at once:

1. Copy `tools\probe\bfprobe.lua` (from the redstorm build folder) into
   `<SG>\DCS.RSR\Scripts\Hooks\` and restart the DCS server. Any Caucasus
   mission will do.
2. Once the DCS log shows `bfprobe: wrote ...bfprobe_out.txt`, copy
   `<SG>\DCS.RSR\bfprobe_out.txt` to the build PC (`E:\Saved Games\DCS\` or
   Downloads) and **delete `bfprobe.lua` from Hooks**.
3. On the build PC, `python tools\apply_probe2.py` moves every wet zone to a
   dry spot the same probe confirmed, then `rebuild-all.ps1` rebuilds.

## 1. Close out the 2008 campaign

1. Announce it, and let the current round end or stop it.
2. Stop the DCS server for `DCS.vectorstrike_2` from Discord, then stop bfdb
   (`/feops bfdb_stop` or the bot) so nothing is mid-write.
3. Archive the 2008 files in `<SG>\DCS.vectorstrike_2`. Move, don't delete:
   - `RGW2008` (the campaign save state) and `RGW2008_CFG`;
   - `Logs\stats.jsonl` and `Logs\stats\`. **This step matters.** The new
     instance reads the same paths, and an old `stats.jsonl` would be
     ingested as Red Storm's first round;
   - the 2008 `.miz` files in `Missions\`;
   - its Tacview folder (`C:\VectorStrike\Tacview\VS2`) if you want the
     replays kept.

   Put them in e.g. `<SG>\DCS.vectorstrike_2\_archive-2008\`.
4. bfdb keeps every 2008 round in its database under `vs2`, so the history
   stays on the dashboard (step 3).

## 2. Mission and engine files

Into `<SG>\DCS.vectorstrike_2`:

| File | To |
|---|---|
| `RSR_SF.miz` | `Missions\` (and set it as the server's mission in `serverSettings.lua` / the bot's mission list) |
| `RSR_SF_CFG` | `<SG>\DCS.vectorstrike_2\RSR_SF_CFG`. Fill in `admins` before the first run; without the file bflib writes a default config |
| `bflib.dll` | Stage it as usual (drag it into the bot's admin channel) or copy to `Scripts\bflib.dll` while DCS is down |
| `bfdb.exe` | Stage it the same way; it is shared by both servers, so Syria gets it too |

**Live weather (BFWeather).** If `DCS.vectorstrike_2` uses the BFWeather
extension in `nodes.yaml`, point its `base`, `weapon`, `options` and
`warehouse` at the Red Storm templates (`build\RSR_SF_base.miz` etc. from
the redstorm folder) and set `metar_station: UGSB` (Batumi). Live weather
also sets the in-game date to today; turn BFWeather off for this instance if
you want the date to read 1985.

## 3. bfdb instances (`config/plugins/fowlengine.yaml`, `bfdb:`)

Keep `vs2` as a hidden archive (its rounds stay readable by admins) with no
live sources, and add `redstorm`:

```yaml
    default_instance: vs1
    instances:
      - id: vs1
        # ... unchanged ...

      # 2008 Caucasus: retired. No live sources, so nothing is ingested under
      # this id any more; its rounds stay in the database for admins.
      - id: vs2
        label: "2008 - Caucasus (archive)"
        public: false

      - id: redstorm
        label: "Red Storm Rising 1985"
        dcs_server_name: "[VS] Vector Strike | Red Storm Rising 1985"
        netidx_base: "/local/fowl/redstorm"
        engine_config: "<SG>\\DCS.vectorstrike_2\\RSR_SF_CFG"
        stats_jsonl:   "<SG>\\DCS.vectorstrike_2\\Logs\\stats.jsonl"
        stats_dir:     "<SG>\\DCS.vectorstrike_2\\Logs\\stats"
        srs_url:       "<SG>\\DCS.vectorstrike_2\\Config\\clients-list.json"
        tacview_dir:   "C:\\VectorStrike\\Tacview\\VS2"
        export_port: 42002
        gci:
          srs_port: 5004
        # The Red Storm look and voice
        theme: "redstorm"
        brand_name: "Southern Flank 1985"
        blue_faction: "NATO"
        blue_adjective: "NATO"
        red_faction: "the Soviet Union"
        red_adjective: "Soviet"
```

And add the dashboard's two addresses to `cors_origins`:

```yaml
    cors_origins:
      # ... the existing vectorstrike.org origins ...
      - "https://redstorm.vectorstrike.org"
      - "https://bfrsr-web.vercel.app"
```

An entry with no `netidx_base` and no stats paths is valid: bfdb only rejects
duplicate ids, netidx bases, export ports and stats files
(`bfdb/src/instance.rs`), and skips the live loops for an instance with no
base. The `netidx_base` here must equal `netidx_base` in `RSR_SF_CFG`.

## 4. Per-server bot settings (`fowlengine.yaml`)

Rename the 2008 server's section to the new server name and point it at the
new config. The Discord channels can stay as they are (rename them in Discord
if you like):

```yaml
'[VS] Vector Strike | Red Storm Rising 1985':
  cfg_path: "<SG>\\DCS.vectorstrike_2\\RSR_SF_CFG"
  # bfbinaries, status/alerts/achievements/... channels: as they were for 2008
```

Branding for the Discord posts (optional; with `theme: redstorm` on the
instance these are already the defaults):

```yaml
  branding:
    redstorm:
      brand_name: "Southern Flank 1985"
      logo: "/redstorm/emblem.png"
      colour: "#b3261e"
      diary_title: "The Allied Dispatch"
```

With `theme: redstorm` (or `dashboard_url` in the block) the posts' links
and a path `logo` go to `https://redstorm.vectorstrike.org`, not to the
plugin's Vector Strike `dashboard_url`.

A complete, ready-to-copy `fowlengine.yaml` with steps 3 and 4 applied to the
live config is kept locally (gitignored) at
`DCSServerBot/config/plugins/fowlengine.redstorm.yaml`.

## 5. DCSServerBot server name

The server name is the key in several places. Change the 2008 name to the new
one in all of them:

- `<SG>\DCS.vectorstrike_2\Config\serverSettings.lua`: `["name"]`
- DCSServerBot `config\servers.yaml`: the section key
- any other per-server plugin configs keyed by name (`config\plugins\*.yaml`)

`nodes.yaml` is keyed by the instance folder (`DCS.vectorstrike_2`), so it
needs no change apart from BFWeather (step 2).

## 6. DNS

Cloudflare, `vectorstrike.org`, DNS only (grey cloud), like `range` and
`dashboard`:

```
CNAME  redstorm  →  2a31b5ef7dcbd692.vercel-dns-017.com
```

The domain is already added to the `bfrsr-web` Vercel project.

## 7. Start and check

1. Start bfdb (or the bot). In `Logs\bfdb.log`, look for the `redstorm`
   instance loading and its netidx base resolving once DCS is up.
2. Start the `DCS.vectorstrike_2` server. bflib loads `RSR_SF_CFG`; the
   mission must not stop with "objectives missing a warehouse" (see the
   redstorm README).
3. Dashboard: https://redstorm.vectorstrike.org (or the vercel.app address).
   It shows the console banner, live counts and the plot. Will to fight shows
   once the first `war_will` update arrives. The browser console must have
   no CORS errors.
4. Discord: the status embed and alerts for the new server carry the Red Storm
   name, emblem and colour.
5. The first war diary is filed after 00:00 UTC, in both voices if the news
   LLM is configured (`news_llm_*`).

## Rolling back

Stop the server, put the 2008 files back from `_archive-2008`, restore the
`vs2` entry and the old server name, and remove the `redstorm` entry. Rounds
from the Red Storm test stay in the database under `redstorm`.
