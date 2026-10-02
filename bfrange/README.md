# bfrange: the training range engine

`bfrange.dll` runs the Vector Strike **training range**: a DCS server where
nothing is a campaign. There are no lives, no points and no slot gating, and
every sortie is graded. It is loaded like bflib: into the server's hooks Lua
state (`bfrange_hooks.lua`) and into the mission by a trigger script
(`bfrange_mizinit.lua`). It shares only `dcso3` and `bfprotocols` with bflib;
none of the campaign code runs here.

For the server, bot, bfdb and website setup see `deploy/range.md`. This file
covers the engine, the range mission, and how each discipline is graded.

## Build

```powershell
. .\setup-build.ps1
cargo build --release --package=bfrange
```

Output: `target\release\bfrange.dll`. Copy it to the range server's
`Saved Games\<server>\Scripts\bfrange.dll`, or drop it into the bot's admin
channel so it is staged as `_bfrange.dll` and swapped in on the next restart.

## What the engine writes

| File | What | Read by |
|---|---|---|
| `Logs\range.jsonl` | one `RangeRecord` per graded event (`bfprotocols::range`) | bfdb, which builds the result cards, greenie board and debriefs |
| `Logs\stats.jsonl` | identity stats only: NewRound (once per range), Register, Connect, Slot, Takeoff, Land, Disconnect | bfdb, so range pilots are the same people as on the campaign servers |
| `Logs\bfrange.txt` | the engine log; every result is logged as `RESULT <id>: <headline>` | you |
| `<sortie>_RANGE` | the range layout (JSON), written empty on first start if missing | the engine |

Its netidx RPCs are published under `<netidx_base>/<sortie>/api/`: `query-range`,
`query-range-catalog`, `query-weapons`, `range-spawn`, `range-despawn` and
`range-reset-station`.

## The range mission

**The ready-made one:** `python bfrange/mission/build_range_miz.py` builds
`VS_Range_Caucasus.miz` (sortie `VSRANGE`) and its config `VSRANGE_RANGE`,
derived from the RGW2008 Caucasus mission. It has the Supercarrier CVN-72
and LHA-1 with deck slots, dynamic spawn with every airframe at 7 blue and 6
red fields, and Combined Arms slots. `bfrange/RANGE_CFG.sample.json` is the
same config. `deploy/range.md` ("Setup, start to finish") walks through
deploying it.

**Sectors.** The whole theatre is divided into 27 sectors with one job each,
defined once in `bfrange/mission/layout.py`. The builder turns that table
into the trigger zones (`<SECTOR>-<WHAT>`, e.g. `R1-BOMB`, `H2-PINNACLE`),
the config's `sectors`, and Mission Editor drawings: each sector outlined and
lightly filled in its discipline's colour with a label, a summary box per
side and a colour legend. A side's sectors are drawn on that side's layer
(only that coalition sees them), the shared ones on Common. The engine lists
them on **F10 > Range > Sectors** (nearest first, bearing and range) and tells
a player what a sector is for when they fly into it (once per 15 min per
sector).

| Sector | Side | For |
|---|---|---|
| R-1 SAMGORI | blue | bomb circle, strafe pit (east of Vaziani) |
| R-2 IORI | blue | tactical array (laser 1688), convoy, JDAM targets, JTAC Axeman |
| R-3 TETRI TSKARO | blue | SA-8 threat range (hills south-west of Tbilisi) |
| G-1 LILO | blue | Combined Arms gunnery lane |
| H-1 VAZIANI, H-2 TSKALTUBO | blue | helicopter courses; H-2 is mountain flying north of Kutaisi |
| MOA KAKHETI | blue | BFM over the mountains, floor 8,000 ft |
| AR-4 TEXACO 2 | blue | KC-135 track over Kartli |
| R-11 STEPNOYE | red | bomb circle, strafe pit (steppe north of Mozdok) |
| R-12 KARST | red | tactical array (laser 1511), convoy, JDAM targets, JTAC Topor |
| R-13 ACHIKULAK | red | SA-8 threat range |
| R-14 KUBAN | red | bomb circle, strafe pit, tactical array (laser 1512) for Maykop and Krymsk |
| G-11 STARODUB | red | Combined Arms gunnery lane |
| H-11 MOZDOK, H-12 NALCHIK | red | helicopter courses; H-12 is foothill flying |
| MOA NOGAI | red | BFM over the steppe |
| AR-5, AR-6 | red | IL-78M tracks, north and over the sea west |
| CV OPAREA | all | CVN-72 and LHA-1 |
| W-1, W-2 / W-3 / W-4 | all | BFM / BVR / blue-vs-red duels, over the sea |
| AS-1 SHIPPING | all | merchant ships for anti-ship weapons |
| AR-1, AR-2, AR-3 | blue | KC-135, KC-135MPRS, KC-130 over the sea |

Ground sectors were placed against DCS's own 1:1M raster chart of the
theatre: open, low ground, clear of towns, rivers and each other. They were not
probed in the sim, so open the mission in the Mission Editor once and look at
the target zones; the pinnacles (`H*-PINNACLE`) especially want a hill top.
Move a sector by editing `layout.py` and re-running the builder: zones,
config and drawings move together.

**Building your own**, on any map. The mission only has to contain what a
script cannot create; the engine spawns everything else from the config.

1. **Sortie name.** In the Mission Editor set *Sortie* (mission options); the
   config file is `<sortie>_RANGE` next to the other Saved Games files. With no
   sortie it is `RANGE_RANGE`.
2. **Coalitions.** Put **CJTF Blue** in blue and **CJTF Red** in red. The
   engine spawns tankers, adversaries, targets and ships as those countries,
   and DCS refuses to spawn a country the mission's coalitions don't include.
   Add **UN Peacekeepers** to neutrals if you use neutral targets.
3. **Client slots.** Use dynamic spawn on the home airfields (open the
   airbase's warehouse, enable *Dynamic spawn* and *Allow hot start*, set
   unlimited aircraft and weapons). That gives every module without hundreds
   of slots. Scripts cannot air-start players, so if you want air starts next
   to the tanker tracks or BFM boxes, place ordinary air-start client groups
   there.
4. **Carriers.** Players spawn on carriers, so they have to be in the mission.
   Place the Supercarrier (and a Tarawa for the Harrier) with client slots on
   the deck, and name each carrier **unit** exactly as the config's
   `unit_name`. The engine steers them into the wind, lights
   TACAN/ICLS/ACLS/Link-4 and launches the recovery tanker. The Supercarrier
   module is what makes DCS's own LSO grade passes.
5. **Trigger zones** mark where things go; place them on real terrain in the
   ME and reference them by name in the config (`{"zone": "R1-BOMB"}`).
   Anything over water (tanker tracks, ship targets, BFM boxes, the carrier op
   area) can be a lat/lon instead (`{"lat": 42.2, "lon": 40.7}`). A target
   that still lands in water is moved to the nearest dry ground (up to 600 m).
6. **The loader.** MISSION START, no condition, DO SCRIPT:

   ```lua
   package.cpath = package.cpath .. ";" .. lfs.writedir() .. "\\Scripts\\?.dll"
   local bfrange = require("bfrange")
   bfrange.initMiz()
   ```

If something in the config can't be placed (a zone missing, a type DCS doesn't
know), the engine logs it and carries on without that one item. If the config
itself doesn't parse, every player sees **THE RANGE CANNOT START** with the
reason.

## The F10 menu

**F10 > Range** is the same for everyone:

| Menu | What |
|---|---|
| Range status | the sector you are in; stations, tankers and carriers with bearing and range from you |
| Sectors: what is where | your side's sectors and the shared ones, nearest first, with bearing, range and what each is for |
| Air-to-Ground | per station: info, smoke, reset; ground targets 10 nm ahead; SAM site 20 nm ahead (instructors) |
| Air-to-Air | pick set-up, adversary, weapons (guns / Fox 2 / Fox 1 / Fox 3), skill, BVR range and number, then **FIGHT'S ON**; duels; missile trainer on/off |
| Tankers | who is on station; a tanker of any type (incl. the A-6E) on your position |
| Carrier | BRC/FB, wind over deck, TACAN/ICLS; your last pass |
| Anti-ship | an undefended ship 30 nm ahead |
| Helicopter | pads, where to deliver dynamic cargo, sling-load courses, troops load/unload |
| CAS / JTAC | check in with a JTAC for a nine-line |
| My spawns | list, despawn all |
| Results | your last results, help |

The website's spawn page offers the same catalogue (`query-range-catalog`),
with every parameter. Chat: `-range trainer on|off`.

Adversary loadouts are built from DCS's own pylon tables at runtime, matched by
store name. Fox 2 is IR (R-73, AIM-9, PL-5/8, R-27T/ET), Fox 1 semi-active
(R-27R/ER, AIM-7), Fox 3 active (R-77, AIM-120, SD-10, PL-12, PL-15). So a type
only ever gets missiles its pylons accept. A config `loadouts` entry for an
adversary overrides this.

## How things are graded

| Discipline | How it is measured | Grade |
|---|---|---|
| Bombs, rockets, guided weapons | Every player weapon is followed from release to impact at 20 Hz; the impact is where its last position and velocity meet the ground (`land.getIP`). Release altitude, TAS, dive angle, heading and wind are captured at the shot. | Miss distance to the nearest station target: SHACK ≤ 1.53 m, EXCELLENT ≤ ½ GOOD, GOOD ≤ 25 m unguided / 10 m guided / 30 m rockets, INEFFECTIVE ≤ 2× GOOD, else POOR. Also reports the clock position against the attack heading (12 = long). A rocket ripple is one result (best impact). |
| Strafe | Pass opens on trigger pull inside a pit's box; rounds fired = gun ammo count before and after; hits = HIT events on the pit's targets. | Accuracy: DEADEYE ≥ 90 %, EXCELLENT ≥ 75, GOOD ≥ 50, INEFFECTIVE ≥ 25, else POOR. Firing inside the foul line (610 m) voids the pass. |
| Carrier passes | DCS's Supercarrier LSO grade (LANDING_QUALITY_MARK), when it gives one. Alongside it, our own groove tracker samples at 10 Hz in the landing-area frame: glideslope error against 3.5°, lineup off the angled deck, AoA from velocity and attitude against the type's on-speed band, groove time, hook position; touchdown from RUNWAY_TOUCH, bolter from RUNWAY_TAKEOFF, wire estimated from the stop point. | `_OK_` 5, OK 4, (OK) 3, B 2.5, -- 2, OWO 2, WO 1, C 0. The card shows the LSO comment decoded line by line, and the trap sheet. |
| Air-to-air refuelling | Session from 1 nm in trail to leaving. Receiver position in the tanker's body frame at 5 Hz; contact = REFUELING event (receiver found by proximity, because DCS names the tanker for client receivers on a dedicated server) or rising fuel. | A–F from join time, disconnects, the spread of the receiver's position while connected, closure at pre-contact and overshoots. |
| Missile trainer | Every AAM/SAM is followed, checked more often as it closes (5 s → every tick inside 1 km). Inside 200 m (500 m for big warheads) it is deleted: "you'd be dead". | Defender: DEFEATED is the good result. Launch geometry, reaction time and time spent hot/beaming/dragging are recorded. |
| BFM / BVR / duels | Engagement from spawn/accept to a trainer kill, a real kill, N gun hits (duels), timeout or abort. | WIN / LOSS / DRAW. Guns cannot be protected: gun damage is real. |
| Anti-ship | Weapons aimed at, hitting, or landing within 500 m of a ship. | HIT/MISS, damage, launch range. |
| Helicopter | Pads: distance from the mark at LAND, the steepest descent in the last second (a hard landing caps the grade), heading error, hover time. Sling courses: the cargo is polled; delivered when back on the ground and still. **DCS dynamic cargo** (the ground crew's cargo loader at a home field): every package (`<player>\|HH:MM\|PKG<n>`) is watched from its birth, marked *internal* once seen in the cabin (`getCargosOnBoard`), and graded when set down at any pad, drop zone or LZ -- no F10 step. Troops: land in the LZ. | PERFECT inside the pad's radius, then EXCELLENT ×2, GOOD ×4, FAIR ×8, POOR. |
| CAS drill | The AI JTAC lases the target and passes a nine-line; your first impact near the station is scored. | Time from the nine-line, miss distance, right target, danger close (friendlies within 150 m). |
| CA gunnery | Hits and kills by a Combined Arms player on a gunnery lane's targets. | Fraction of the lane killed. |

## Wind and ship heading: always DCS's own

No wind, temperature or ship heading is ever taken from the config, the
mission file or a standard day. They are read from the running mission when
they are used:

| Where | What DCS is asked |
|---|---|
| Carrier steering | `atmosphere.getWind` at the deck every 30 s. The BRC and speed are solved so that the ship's own headwind plus that wind come straight down the angled deck at the config's `wind_over_deck_kts` (`grading::recovery_course`); a straight deck points into the wind. The log line says what it saw and chose. |
| Carrier passes and status | The ship's heading (BRC) and velocity from its own unit (`getPosition`, `getVelocity`) at every sample; FB = that heading plus the deck angle; wind over deck = DCS wind at the landing point minus the ship's velocity. The recovery tanker and plane guard are placed off the ship's heading at that moment. |
| Bomb results | Wind (`getWind`) and temperature (`getTemperatureAndPressure`) at the release point for TAS and Mach; plus a profile of wind, temperature and pressure from the target's ground up to the release altitude, stored with the result. bfdb's drag calibration flies each recorded drop through that profile. |
| Range site | Each station's live picture carries DCS's air over it, ground to 10 km, and its ground height; each carrier its heading, speed, deck angle and the true wind at the deck. The release and wind-over-deck calculators start from those values (marked **LIVE · DCS**) and fall back to typed-in values only when edited or when the server is down. |

## Limits worth knowing

- Server-side positions of clients arrive at network update rate. Groove and
  AAR numbers are good for teaching, not exact: expect the estimated wire to be
  ±1 and some noise on the deviations. DCS's own LSO grade, when present, is
  the official one on the card.
- The Supercarrier LSO does not grade every pass, and it grades only on the
  Supercarrier and Tarawa.
- REFUELING events are unreliable for client receivers on a dedicated server,
  so contacts are also detected from rising fuel.
- Scripts cannot air-start players or add slots.
- Guns cannot be made harmless.
- The A-6E is DCS's Heatblur AI aircraft, refuelling from its D-704 buddy
  store on the centreline, which the engine loads automatically. That it
  passes fuel to players is still to be confirmed in game.

---

© 2026 Dillen Weerasinghe. All rights reserved. Proprietary — see the repository NOTICE file.
