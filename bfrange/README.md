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
`VS_Range_Marianas.miz` (sortie `VSRANGE`) and its config `VSRANGE_RANGE`,
derived from the Marianas SUA mission (`bfrange/mission/donor/`). It has the
Supercarrier CVN-72 and LHA-1 with deck slots, dynamic spawn with every
airframe at Andersen AFB and Won Pat, and Combined Arms slots. Blue only: the
opposition is AI. `bfrange/RANGE_CFG.sample.json` is the same config.
`deploy/range.md` ("Setup, start to finish") walks through deploying it.

**The real airspace.** The layout follows the Mariana Islands Range Complex
as the US uses it. Its special use airspace is drawn on F10 in dotted grey
with the real names: ATCAA 2, 3A/B/C, 5 and 6, and R-7201A (12 nm round
Farallon de Medinilla). The published refuelling tracks are drawn as well:
RIO, MIRAGE and LUXOR with their entry lines (ARIP to ARCP) and exit lines.
The boundaries come from the Marianas SUA mission's drawings and land on
whole arc minutes. W-517 is traced from the NAVFAC "Airspace in the Mariana
Islands Range Complex" chart, so it is good to about a minute. Positions use
the terrain's own transverse Mercator (`bfrange/mission/tmproj.py`, with the
same parameters pydcs uses).

**Sectors.** All 29 sectors are defined once in
`bfrange/mission/layout.py`, each with one job. Every block of the real
airspace is a sector, with the job it has in life, and is drawn as a thin
closed line. The ranges inside the airspace are drawn as filled outlines. Where two sectors overlap,
the engine reports the smaller one, so R-7201 inside W-13A reads the way it
does on a chart. From that table the builder makes:

* the trigger zones (`<SECTOR>-<WHAT>`, for example `R7201-BOMB`, `H1-PINNACLE`),
* the config's `sectors`,
* the Mission Editor drawings: each sector outlined and lightly filled in its
  discipline's colour with a two-line tag (name and kind; tanker tracks and
  the carrier keep their frequencies), plus SAM threat rings, the jammer
  radius, the low-level route and a colour legend.

The engine lists the sectors on **F10 > Range > Sectors** (nearest first,
with bearing and range). It tells a player what a sector is for when they
fly into it, once per 15 minutes per sector.

| Sector | Where | For |
|---|---|---|
| W-517 | warning area SW of Guam | BFM / ACM set-ups, live missile shots |
| ATCAA 2 (W-12) | round W-517 | duels, 1v1 / 2v2 by agreement |
| ATCAA 5 | west of Guam | BFM away from the W-517 traffic |
| ATCAA 1 (W-11) | south-east of Guam | BVR presentations, Fox 1 / Fox 3 defence |
| ATCAA 3A (W-13A) | round FDM | large-force strikes into R-7201 against red air |
| ATCAA 3B (W-13B) | east of Tinian and Saipan | maritime strike, with the AS-2 convoy in it |
| ATCAA 3C (W-13C) | east of the chain | intercepts, Fox 3 at range |
| ATCAA 6 | over the islands | intercepts and DCA above FL150 (arena floor 15,000 ft) |
| R-7201A | 12 nm round FDM | holding and run-ins for R-7201 |
| AR RIO, AR MIRAGE, AR LUXOR | the published tracks | KC-135 51Y 251.0 FL250 300 KIAS, KC-135MPRS 52Y 252.0 FL210 270 KIAS, KC-135 54Y 254.0 FL240 300 KIAS |
| AR SHELL | west of Guam | KC-130 53Y 253.0, 14,000 ft, 220 KIAS |

Tanker speeds are set as indicated airspeed (`speed_kias`), the way they
are flown. DCS flies AI at true airspeed, so the engine converts them at
the track's altitude: 300 KIAS at FL250 is about 430 kts true. A bare
`speed_kts` is still read as true and wins if both are set. Without
either, the type's usual refuelling speed is used: KC-135 300, MPRS 270,
KC-130 220, S-3B and A-6E 250, IL-78M 290.
| R-7201 FDM | Farallon de Medinilla | bomb circle, strafe pit (run in 209 from the NNE, rounds into the sea), coordinate targets |
| TN-1 NORTH FIELD | Tinian | tactical array (laser 1688), convoy on the old runways, JDAM targets, JTAC Axeman 133.0 |
| TN-2 TINIAN MLA | Tinian | **tiered** targets: easy, medium (AAA), hard (SHORAD) |
| EW-1 TINIAN SOUTH | Tinian | **GPS jammer** over coordinate targets |
| R-3 AGUIJAN | Aguijan | SA-8 threat emitter, radar on, weapons hold |
| S-1 ROTA | Rota | **live SEAD/IADS**: EWR, SA-2/3/6/11/10, Pantsir |
| HZ-1 SAIPAN | Saipan | **hot zone**: AI CAP, SHORAD and targets that fight back; AWACS Overlord 1 (251.5) |
| H-1 ANDERSEN | Guam | helicopter courses: pads at Andersen, confined LZ at Andersen South, pinnacle on Mt Santa Rosa |
| G-1 NORTHWEST FIELD | Guam | Combined Arms gunnery lane |
| PT-1 ANDERSEN | Guam | pattern field (every runway landing everywhere is graded) |
| CS-1 SOUTH GUAM | Guam | **CSAR** round Mt Lamlam, beacon 350 kHz |
| LL-1 GUAM COAST | Guam | **low-level route**, Ritidian to Orote, 8 gates, below 500 ft AGL at 420 kts |
| CV OPAREA | south-east of Guam | CVN-72 and LHA-1 |
| FD-1 APRA | off Apra Harbor | **frigate decks**: Perry 41X and Arleigh Burke 42X |
| AS-1 SINKEX | in W-517 | undefended hulks for anti-ship weapons |
| AS-2 ESCORTED CONVOY | in W-13B | red warships (Krivak, Neustrashimy, Tor-armed Project 22160), weapons free |

**Markings on the ground** (`bfrange/mission/markers.py`) make the ranges look
like real ones:

* tyre rings round the bomb circle and each target array
* a tyre foul line across the strafe pit's run-in
* red flags round the coordinate targets
* a firing line and distance flags on the gunnery lane
* a tyre pad, a windsock, the confined LZ's corners and cones round each
  pickup and drop zone for the helicopters

They are neutral (UN Peacekeepers), so hitting one isn't a team kill. They
are hidden on F10 and don't respawn.

The target zones were placed on satellite imagery (Esri World Imagery) and
not probed in the sim. Open the mission in the Mission Editor once and check
them; `H1-PINNACLE` especially should be on the hill top. To move a sector,
edit `layout.py` and re-run the builder: the zones, config and drawings move
together.

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

**F10 > Range** is the same for everyone (DCS shows ten entries a page, so
the root has exactly ten):

| Menu | What |
|---|---|
| Range status | the sector you are in; stations, tankers and carriers with bearing and range from you |
| Sectors: what is where | your side's sectors and the shared ones, nearest first, with bearing, range and what each is for |
| Air-to-Ground | per station: info, smoke, reset; ground targets 10 nm ahead; SAM site 20 nm ahead (instructors); **SEAD / IADS ranges** (site list, rebuild a network: instructors); **EW: GPS jammers**; **CAS / JTAC** check-in |
| Air-to-Air | pick set-up, adversary, weapons (guns / Fox 2 / Fox 1 / Fox 3), skill, BVR range and number, then **FIGHT'S ON**; duels; missile trainer on/off; **hot zone AWACS picture** and status |
| Tankers / Carrier | who is on station; a tanker of any type (incl. the A-6E) on your position; carrier BRC/FB, wind over deck, TACAN/ICLS; your last pass |
| Anti-ship | an undefended ship 30 nm ahead |
| Helicopter | pads, where to deliver dynamic cargo, sling-load courses, troops load/unload, **CSAR** (pilot down cold or hot, per area), **ship decks** |
| Airlift | **airdrop drop zones** for the C-130J and other fixed-wing cargo aircraft (a point of impact, a perfect radius, an optional run-in) |
| Low level / Landings | low-level routes with gates, headings and planned times; how landings are graded |
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
| Airlift | **Airdrop**: the same dynamic cargo, carried by a fixed-wing aircraft (a C-130J's ramp). The moment a package leaves the cargo bay in flight is the release (height AGL and MSL, ground speed, heading); where it comes to rest is scored against the nearest drop zone within `airdrop.reach_m`, with long/short and left/right along the run-in, time under canopy and drift from the release point. Unloaded on the ground at another field: an **air-land** delivery. Fixed-wing loads are never graded as helicopter set-downs. | PERFECT inside the drop zone's radius (default 50 m), then EXCELLENT ×2, GOOD ×4, FAIR ×8, POOR. |
| CAS drill | The AI JTAC lases the target and passes a nine-line; your first impact near the station is scored. | Time from the nine-line, miss distance, right target, danger close (friendlies within 150 m). |
| CA gunnery | Hits and kills by a Combined Arms player on a gunnery lane's targets. | Fraction of the lane killed. |
| SEAD / DEAD | An IADS behaves like one: EWRs always up, SAM radars dark until the network sees a target inside that site's ring (only aircraft inside the IADS sector wake it), point defence watches for itself, a site an ARM is fired at goes dark for 45 s, destroyed sites are rebuilt after 15 min. Weapons free; the missile trainer is the safety net. Every emitter, launcher or command post a player kills is graded. | 5: an emitter killed while its radar was up; 4: killed dark, or the kill that took the site down; 3: anything else. The card carries the weapon, launch range, and how many SAMs the network fired at you and how many would have killed you. |
| Tiered ranges | Easy (trucks), medium (armour under AAA: the guns are real), hard (modern armour under SHORAD, weapons free). | Scored as bombs; the card carries the tier. |
| EW / GPS jamming | A GPS jammer/spoofer (DCS's ZIL SKP-11 and `ActivateJammer`) over a set of coordinate targets. You are warned when you fly into its radius. | Bombs scored as usual; the card says **GPS denied** when the release was inside the radius. Kill the jammer to clear it (rebuilt after 15 min). |
| Hot zone | AI CAP launched from the far edge while a player of the other side is inside (a new flight 5 min after one is lost; home after 10 min empty), ground defences and targets that respawn, and an AWACS on your side (datalink, and F10 BRAA picture). | One card per trip: time inside, air and ground kills, missiles defeated, trainer deaths, egressed / landed / shot down. |
| Low-level route | The clock starts at gate 1 below the ceiling; each gate is timed against the planned speed; height above ground sampled every second. | Start at 5; a point per missed gate, for more than 5% of the route above the ceiling (2 for 20%), for a time on target outside the tolerance (2 for 3x), and for time below the safety floor. |
| Field landings | Every runway landing at every airfield: runway and landing end from the touchdown point and your own track; touchdown against a 300 m aim point and the centreline; sink rate in the last second; glideslope and lineup at 1 nm and 1/2 nm; a take-off within 15 s makes it a touch-and-go. | Start at 5; points off for the aim point (150 m / 300 m), centreline (5 m / 10 m), sink rate (600 / 900 fpm) and an unstable approach. Short of the threshold is POOR. |
| CSAR | A survivor on open ground somewhere in the area; the MAYDAY gives a rough circle, the beacon is on an ADF frequency, he pops smoke (flare at night) when he hears you within 3 km. Land within 50 m (or hover low over him) for 8 s, then land at any airfield, FARP or ship. Hot missions put enemy troops near him. | PERFECT home inside 20 min, EXCELLENT 30, GOOD 45, then FAIR; picked up but not delivered is FAIR, not reached is POOR. |
| Ship decks | Frigates steaming a leg; a landing on one is graded in the ship's frame. | Distance off the deck centreline against 1.5 m, sink rate relative to the deck and heading against the ship's; the card carries the ship's speed. |

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
