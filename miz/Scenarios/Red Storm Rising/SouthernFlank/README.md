# Red Storm Rising: Southern Flank 1985 — Fowl Engine campaign (Caucasus)

A persistent Fowl Engine (BFNEXT) campaign on the Caucasus map, set in the
southern flank of the war in the novel: **Blue = NATO** (US Navy, US Marines,
US Army, Turkey, a British Rapier battery), broken out of the Operation GOLDEN
FLEECE beachhead and holding western Georgia and Abkhazia, with a carrier
group offshore; **Red = the Soviet Transcaucasus Front**, holding eastern
Georgia and everything north of the mountains. The start is even: 23
objectives each. Start: 06:00 local, **15 June 1985**.

The design is `miz/Scenarios/Red Storm Rising/` in the engine repo
(CAUCASUS.md, DESIGN.md, ENGINE.md, story.sample.json). This folder is the
build. It is generated, like the 2008 campaign it is built from
(`..\caucasus\`, which this build only ever reads).

---

## Files

| File | What it is |
|---|---|
| `RSR_SF.miz` | **The mission DCS loads.** Assembled by bftools. |
| `RSR_SF_CFG` | **The campaign config.** bflib reads `<writedir>\<sortie>_CFG`, and the sortie is `RSR_SF` (`bflib/src/lib.rs`, `Cfg::load`). |
| `RSR_SF.warehouse-report.txt` | bftools' report of what every airbase stocks. |
| `build/weapons_to_zero.txt` | Which weapon rows are zeroed in the warehouse and which still need zeroing by hand in the Mission Editor. |
| `assemble.ps1` | `bftools special-sam` + `bftools miz` over the templates in `build/`. |
| `rebuild-all.ps1` | Regenerates every template, assembles, writes the config, loads it with the engine's own code, verifies. |
| `build/rsr_sf_base.miz` | Base mission: objectives, garrison zones, all templates. |
| `build/rsr_sf_sams.miz` | The 46 hidden SAM sites. |
| `build/rsr_sf_warehouse.miz` | **Edit this one in the Mission Editor** (weapons). Coalition inventories. |
| `build/rsr_sf_options.miz` | Date, weather, briefing, forced options. |
| `build/special_sam_sites.json` | The SAM sites as config entries (merged into the config by `build_cfg.py`). |
| `tools/` | The generators, the terrain data, the probe, the checkers. |

## Deploying

The campaign box runs it as its own DCSServerBot instance, `DCS.RSR`
(export port 42003, SRS 5006). `miz/Scenarios/Red Storm Rising/DEPLOY.md` in
the engine repo has the full setup.

1. `RSR_SF.miz` → `<Saved Games>\DCS.RSR\Missions\`.
2. `RSR_SF_CFG` → `<Saved Games>\DCS.RSR\RSR_SF_CFG`. If it is missing
   bflib writes a *default* config there instead. `admins` in this copy is
   a placeholder: put the real UCIDs in before deploying.
3. `bflib.dll` (release build of the current engine: it needs `ai_fill`,
   `war_will`, `story`, `ai_air_stock` and the HQ's anti-ship op) →
   `<Saved Games>\DCS.RSR\Scripts\bflib.dll`, with bflib's hook loader in
   `Scripts\Hooks\`. An older dll logs `unknown key ai_fill is ignored`, and
   the same for the other Red Storm keys.
4. State lands at `<Saved Games>\DCS.RSR\RSR_SF`.
5. `netidx_base` is `/local/fowl/redstorm`, bfdb's `redstorm` instance. Do
   not share it with another server: the stats would cross.

---

## The campaign

**46 objectives, 23 each.** CAUCASUS.md 2.1 started NATO on the beachhead
alone (7 objectives to 31); the campaign now starts even, with NATO holding
everything west of the Surami ridge and the coast up to the Psou.

| | NATO (Blue) | Transcaucasus Front (Red) |
|---|---|---|
| Airbases | 6 (Batumi, Kobuleti, Senaki-Kolkhi, Kutaisi, Sukhumi, Gudauta) | 13 |
| Logistics hubs | 3 (Batumi port, Samtredia, Zestafoni) | 3 (Surami tunnel, Gori, Ordzhonikidze) |
| FOBs | 9 (Ozurgeti, Lanchhuti, Martvili, Zugdidi, Tkibuli, Sachhere, Ambrolauri, Ochamchire, Gagra) | 2 (Tskhinvali, Kazbegi) |
| Factories | 3 (Batumi oil terminal, Enguri dam, Chiatura) | 2 (Tuapse refinery, Krasnodar refinery) |
| Naval bases | 1 (Poti) | 1 (Novorossiysk) |
| Command centres | 0 | 1 (Tbilisi) |
| Carrier groups | CVBG | Black Sea Fleet |

**Three fronts.** On the coast, Gagra and Gudauta face Sochi-Adler across the
Psou. In the centre, Zestafoni (the rail junction below the pass) and
Sachhere face the Surami tunnel (Khashuri) on the only east-west railway, Gori
and Tskhinvali on the Roki road. In the mountains, the Mamison road comes down
to Ambrolauri and the Georgian Military Road runs through Kazbegi to
Ordzhonikidze. Behind Red's lines: the refineries at Tuapse and Krasnodar.

Every objective is a real place: airbases on the exact DCS airfield point,
towns from the terrain's `Map/towns.lua`, and four hand-placed points (the
oil terminal north of Batumi port, the Enguri power station at Dzhvari, the
two refineries), each checked against the terrain data.

| | |
|---|---|
| spacing | min 6.3 km, median 25.1 km |
| under 10 km | Batumi port / oil terminal 6.3, Batumi / Batumi port 6.8, Lochini / Vaziani 7.3 (both real airbases), Krasnodar refinery / Pashkovsky 8.2 |
| garrison zones | 415, all inside exactly one objective, none within 150 m of known water |
| garrison units (logistics excluded) | Blue 1,401, Red 1,358 |

### Carrier groups

The engine indexes a ship group as a carrier only if its name contains
**CARRIER** (`bflib/src/db/mizinit.rs`). Each group also has a `CG` trigger
zone, so the objectives carry the names the story uses:

- **CVBG** (`BCARRIER`, off Batumi): CVN-71 (Nimitz class, for a 1985 carrier),
  two Oliver Hazard Perrys, a Ticonderoga. No Arleigh Burke.
- **Black Sea Fleet** (`RCARRIER`, off Novorossiysk): the Slava-class *Moskva*
  leading two Krivak II (*Rezky*). No Kuznetsov, no Type 052.

### Ground forces, 1985

Every template in the 2008 donor was retyped (`tools/plan.py`):

- **NATO:** M1 Abrams, M60A3, Leopard 1A3 (Turkey), M2 Bradley, M113,
  LAV-25, HMMWV TOW, M109, M270 MLRS, M163 Vulcan, M48 Chaparral, Stinger,
  I-Hawk, Roland, Rapier, Patriot (one battery, Batumi only), AN/FPS-117.
- **Soviet:** T-72B, T-80B, T-55, BMP-1, BMP-2, BTR-70, BRDM-2, ZSU-23-4,
  ZU-23, KS-19, SA-6, SA-8, SA-9, SA-13, SA-11, SA-3, SA-2, S-300PS (deep rear
  only), 2S1, 2S3, 2S9, BM-21, BM-27, Scud-B, P-18/P-19/55G6.

Nothing later than 1985 is in the mission: no Tor, Tunguska, Pantsir, Smerch,
S-300V, Iskander, T-90, BMP-3, BTR-80/82, Msta, Arleigh Burke, Type 052,
Kuznetsov, NASAMS, HIMARS, Avenger, Linebacker, Stryker, MRAP, C-RAM or GPS
spoofer. `plan.BANNED_TYPES` lists them and `verify.py` fails the build on any.
DCS has no T-64; T-72B and T-80B stand in.

### Objective scenery

Every objective except airbases and the carrier groups has a compound of
statics, so it doesn't look like an empty field (`tools/build_statics.py`,
649 statics at 25 objectives):

| Objective | Compound |
|---|---|
| FOB | tents, camouflage nets over parked trucks, fuel and ammo dumps, a radio mast, a gate |
| Logistics hub | container yard, site office, watchtower, generator, trucks and a fuel truck |
| Refinery or oil terminal | tank farm, fuel bladders, plant building, tanker trucks, a fence |
| Factory or dam | works buildings, excavators, containers, a radio mast, trucks |
| Naval base | tower crane, container stacks, fuel tanks, port office |
| Command centre (Tbilisi) | command bunker, two radio masts, tents, staff cars under nets |

Trucks are each side's own: M818 and HEMTT for NATO, GAZ-66, ATZ-5 and UAZ for
the Soviets. The statics are neutral. bflib respawns any neutral static inside
an objective zone that gets destroyed, so the scenery survives strikes and never
changes hands. Each compound stands on ground other missions spawn vehicles on,
150 m clear of garrisons, FARP pads and known water, and off the roads. Tkibuli
and Krasnodar refinery have no room for a full compound, so they get the first
12 pieces only. Only Fortifications statics and parked vehicles are used.
Warehouse and heliport statics would open DCS warehouses alongside the engine's
logistics, and cargo statics can be sling-loaded.

### Garrisons, vehicle by vehicle

The mission has no `G` trigger zones. Every garrison group is in the
config's `garrison_layouts` (`tools/build_garrisons.py`: 326 groups, 5,498
vehicles counting both sides' versions), each vehicle with its own position
and heading, the way the special SAM sites are. Two missions in `layouts/`
decide how they stand. Both are edited in the Mission Editor, and
`layouts/README.md` explains how:

- **`RSR_FORMATIONS.miz`**: how each kind of group deploys. It is stamped at
  every garrison site, turned to face the enemy.
- **`RSR_SITES.miz`**: every objective's defences and scenery, placed at the
  real place and taken exactly as placed. It holds drafts for all 44
  objectives (the fleets excepted): 329 garrison groups and the 649 scenery
  statics of the FOBs, depots, factories, ports and the HQ, all to check in
  the editor.
  - **Airfields** are laid out from the runway: SAMs beside both approaches,
    guns covering the field from either side, armour on the enemy's side,
    carriers on the flanks.
  - **Everything else** is defended round its scenery compound: guns near
    it, SAMs out toward the threat, armour forward and on the flanks.
  - **Scenery** compounds (FOB, depot, refinery, works, port, HQ) are also
    templates in `RSR_FORMATIONS.miz`. They apply to any objective that has
    no scenery of its own in the sites file.

The first formations follow 1980s doctrine. These are the US Army's manuals
on the Soviet Army and on its own units: FM 100-2-1, FM 30-102, FM 44-3,
FM 71-1 and FM 17-15.

| Group | Soviet | NATO |
|---|---|---|
| Anti-aircraft guns | ring round the fire-control radar, guns 45 m apart, transport 150 m behind | Vulcans in pairs 50 m apart at the corners of the site, 300 m out |
| Short-range SAMs | ZSU-23-4 pair 200 m apart up front, Strelas 250 m behind | Chaparrals in a 400 m diamond round a Vulcan |
| Medium-range SAMs | Osas on a 250 m arc ahead of the radar | Rolands 350 m round their radar |
| Armour | one echelon: tanks and carriers in one line 100 m apart, staggered | tanks 150 m apart, carriers 200 m behind, support 600 m back |
| Supply trucks | dispersed 60 m apart | dispersed 60 m apart |

The manuals give no internal spacing for Osa and Roland batteries, and no
KS-19 layout. Those follow the nearest system the manuals do describe.

"Toward the threat" means the nearest objective the other side holds. It is
worked out separately for each side's version of a group, so when a base
changes hands the new owner's units face the new owner's enemy. build_base
still picks each group's site (ground other missions spawn vehicles on, clear
of water), and the layout is centred on it. Every vehicle is then checked on
its own and moved up to 56 m if needed: it must be at least 110 m from known
water, off the roads, clear of the scenery and 9 m from every other vehicle. A
vehicle with no clear spot nearby keeps the template's own offset, as a G zone
would have placed it. That's 7%, nearly all of them on the mountain FOBs where
no stock mission has parked a vehicle.

**This needs a bflib with `garrison_layouts`.** An older engine ignores the
key, with an "unknown key" warning, and every objective starts with no
garrison. The engine leaves out a unit that DCS reports in water, with a
warning, instead of stopping the mission. The water probe checks every
vehicle's exact spot.

### Hidden air defences: 46 special SAM sites

| Red (25) | Blue (21) |
|---|---|
| S-300PS at Mozdok, Krymsk, Mineralnye Vody | Patriot at Batumi |
| SA-11 at Tbilisi, Sochi-Adler, the Surami tunnel | I-Hawk at Kobuleti, Poti, Ozurgeti, Kutaisi, Senaki, Zestafoni, Gudauta, Sukhumi |
| SA-6 at Gori, Tskhinvali, Tbilisi-Lochini | Roland at the oil terminal, Zugdidi, Enguri, Chiatura, Gagra |
| SA-3 at Novorossiysk, Tuapse, Krasnodar-Center, Vaziani | Chaparral at Batumi port, Samtredia, Sachhere, Ambrolauri, Ochamchire |
| SA-2 at the Krasnodar refinery, Maykop, Ordzhonikidze | AN/FPS-117 near Kobuleti and Kutaisi |
| SA-8 at Tskhinvali, Gori; SA-13 at Kazbegi, Anapa | |
| 55G6 at Gelendzhik, Beslan; P-18 at Lochini, Pashkovsky, Nalchik | |

Every site sits outside every objective zone plus its own 600 m capture
radius and 2.5 km margin, and at least 1.2 km from known water. Some sit
on stock-mission vehicle evidence, the rest on road/town evidence: the probe
(below) is what turns that into measurement. `iadn.anti_radiation_weapons` is
the 1985 set: AGM-88, AGM-45, Kh-58, Kh-25MP.

### Air order of battle (warehouse production per delivery, per hub)

| Blue | | Red | |
|---|---|---|---|
| F-14A-135-GR | 24 | MiG-29A | 24 |
| F/A-18C | 24 | Su-27 | 16 |
| F-15C | 24 | MiG-21bis | 30 |
| F-16C | 24 | Su-25 | 30 |
| F-4E | 24 | L-39C | 20 |
| A-10A | 24 | Mi-24P | 24 |
| AV-8B | 16 | Mi-8MT | 30 |
| UH-1H | 24 | *AI:* MiG-23MLD 24, MiG-25PD 8, MiG-31 8 | |
| C-130J-30 (as C-130H) | 12 | *AI:* Su-24M 16, Su-24MR 4, Su-17M4 16, MiG-27K 16 | |
| *AI:* SH-60B 12, AH-1W 12, CH-47D 6, UH-60A 6 | | *AI:* Tu-22M3 12, Tu-95MS 2, A-50 3, Il-78M 4, Il-76MD 6, Mi-26 4 | |
| *AI:* E-3A 4, E-2C 4, KC-135 4, S-3B Tanker 4, S-3B 4, B-52H 6, F-117A 6, Tornado IDS 12, C-130 6 | | | |

AI-only types are stocked because **`ai_air_stock` is on**: an AI flight
takes its airframes from the field it launches from, so a side whose
Tu-22M3s are gone flies no more Backfire raids. `verify.py` checks every AI
template the actions, the HQ rosters and reactive CAP use against its side's
stock. The `era` block keeps players to the 16 flyable 1985 types; every
other stocked type is priced out of reach (`points.airframe_cost` 99999).

### Weapons and the era

`era` (current `1985`) bans by case-insensitive substring of the weapon name:
AIM-120, AIM-9X, R-77, GBU-31/32/38/54 (JDAM, laser JDAM), AGM-154 JSOW, SLAM
and SLAM-ER, AGM-84H/K, APKWS, Kh-38, KAB-500S, LITENING, ATFLIR, Sniper, plus
what entered service after June 1985: AIM-54C, AIM-7P, CBU-97/103/105,
GBU-39, Kh-31, Kh-59M. Take off with one: 300 s to land. Fire one: the
aircraft is destroyed. `points.weapon_cost` prices the same weapons at 99999
with `strict` on, so carrying one blocks takeoff outright.

**The warehouse.** On its first run `build_warehouse.py` copied the 2008
warehouse, reset its 2008 bans (which had banned the 1985 ARMs) and zeroed the
16 banned rows it can identify for certain from DCS's `wsTypes.lua`
(AIM-120/120C, AIM-9X, R-77, GBU-31, GBU-38, JSOW, SLAM, LITENING, CBU-97/103/105,
Kh-31A/P, Kh-59M). Since then it never touches the weapons table. **Still to
zero in the ME** (plugin weapons with no fixed id; listed in
`build/weapons_to_zero.txt`): GBU-32, GBU-54, AGM-84H, APKWS, Kh-38,
KAB-500S, ATFLIR, Sniper, AIM-54C, AIM-7P, GBU-39, and any AIM-120/JDAM
variant rows the ME shows under other names. Until then the era ban covers
them by name.

---

## Actions and the HQ

| Blue | Red |
|---|---|
| B-52 runway raid (`Strike` Runway, B-52H) | **Backfire raid** (`Strike` AntiShip, Tu-22M3 with Kh-22) |
| F-117 precision strike (`Strike` Precision, GBU-10) | Su-24 strike (`Strike` Precision, KAB-500L) |
| Tornado low-level raid (`Strike` Precision, Mk-82 at 150 m) | Su-24 runway attack (`Strike` Runway, BetAB-500) |
| Turkish F-4E strike (locked until Senaki falls) | MiG-23 / MiG-29 / Su-27 fighters |
| Carrier TLAM (`NavalCruiseMissileStrike`, 500 km) | Su-24M SEAD (Kh-58) |
| F-15C CAP, F-4E SEAD (AGM-45) | Airborne assault (`Paratrooper`, Il-76; locked until Kobuleti is in danger) |
| B-52 ALCM | Black Sea Fleet missile strike |
| E-3A, E-2C, KC-135 (boom), S-3B (drogue) | A-50, Il-78 |

Payloads come from DCS itself, never guessed: the stock AI payload library
(`MissionEditor/data/scripts/UnitPayloads`) or a pylon set the 3,900 shipped
missions under `D:\DCS World` fly (`tools/payload_harvest.json`), all 1985
weapons (`tools/payloads.py`).

**`smart_commander.hq`** runs both sides: `steer_ground_war` on,
`anti_ship_range_m` 600 km (Backfires from Mozdok and Krymsk reach the whole
Black Sea), `costs.anti_ship` 400 and `limits.anti_ship` 1 (scaled by the
2008 `cost_scale` 196), `max_air_range_m` 260 km. Rosters: Blue CAP F-15C /
F-14A / F/A-18C / F-16C, strike AH-1W / A-10A / AV-8B / Tornado, SEAD F-4E;
Red CAP MiG-23 / MiG-29 / Su-27 / MiG-25PD / MiG-31, strike Mi-24P / Su-25 /
MiG-27K / Su-17M4 / Su-24M, SEAD Su-24M.

**New blocks.** `ai_fill` (defaults: floor 0.35, full at 8 enemy pilots);
`war_will` (defaults; carrier damaged 8, sunk 25, worth more than any
airfield; bombers, tankers and AWACS counted as large aircraft); `story` (the
18 events of `story.sample.json`, every objective and action name matched to
this build); `ai_air_stock` (on, landing returns airframes, tankers, AWACS
and transports exempt). `max_round_days` 28.

**Modern defaults off:** GPS jamming (`modern_war.ew`), the ES44AH train (a
period locomotive and wagons instead), Iskander/ATACMS raids (Scud-B only,
from Red's deployed launchers), boat raids, civil air traffic, live weather.

---

## Balance

The start is even on the ground: 23 objectives each, three logistics hubs
each, about 1,400 garrison units each (logistics excluded), 21 Blue SAM sites
to Red's 25. What differs is the kind of strength:

- **Red has more airfields and bigger rosters.** 13 airfields to Blue's 6
  plus the carrier, and Red's per-hub production (`tools/build_warehouse.py`)
  is larger in airframes. Most of those fields are north of the mountains or
  up the coast: slots and targets more than front.
- **Blue has the better aircraft and the fuel.** RINVENTORY's jet fuel is
  35% and diesel 40% of Blue's. Red's posture and war-will fuel lines sit at
  20% and 8%, below its start, so the shortage bites only once Blue's strikes
  on the refineries, the Surami tunnel and the passes take effect.
- **The fronts are close.** Gagra is 30 km from Sochi-Adler, Zestafoni 49 km
  from the Surami tunnel, Sachhere 49 km from Tskhinvali.
- **The AI fills the gap** (`ai_fill`): one pilot meets an enemy matched to
  them, a full server is decided by people.

If one side runs away with it in testing, the levers are `ai_fill.floor`,
the per-hub production in `tools/build_warehouse.py`, and Red's fuel
percentages in the same file.

## Deliberate concessions

1. **An even start, not the beachhead.** CAUCASUS.md 2.1 starts NATO with
   the beachhead alone (Batumi, Kobuleti, Poti, Ozurgeti). The campaign now
   starts after the breakout, 23 objectives each, so neither side begins a
   PvP round already losing. Eight objectives were added for it: Zestafoni
   (a Blue logistics hub), Chiatura, Lanchhuti, Martvili, Tkibuli,
   Sachhere, Ambrolauri and Gagra.
2. **`warehouse.supply_source` stays `BINVENTORY` / `RINVENTORY`.** The key
   names the warehouse bflib reads as the *production list* at first start.
   bftools gives every real airfield its base-mission `aircrafts` table back,
   so naming `Batumi` would make that roster the production list. The spec's
   intent is met by geography: Blue's production lands at Batumi port, Red's
   rear hub is Ordzhonikidze.
3. **Soganlug is left out**: 2.5 km from Tbilisi-Lochini, one zone would
   hold two airbases and the engine refuses the mission.
4. **Stand-ins**: CVN-71 for a 1985 carrier; F-14A-135-GR, F/A-18C and
   F-16C held to 1985 weapons by the era; C-130J-30 as a C-130H; AH-1W
   (1986) for the Marines' Cobras; GBU-10 for the F-117 (GBU-27 is 1988);
   AGM-86C (conventional) for the B-52's ALCM; the Su-27's AI load carries
   R-27ER/ET (the only Su-27 payloads DCS ships), which the era does not ban.
5. **SH-60B is AI only**: DCS has no flyable SH-60.
6. **No Blue command centre**: Blue's SAM sites have no CC to cue them and run
   as ordinary DCS AI, always radiating. That is the spec; add a Blue CC if
   NATO's IADS should get EMCON.

---

## How it was built (and what was checked)

`rebuild-all.ps1` runs, in order:

1. `build_oob.py` → `oob1985.py`: the layout above, with a spacing report.
2. `build_base.py`: copies the 2008 base (read only), purges its geography
   (551 zones, 23 FARP pads), places 334 garrison zones on the strongest
   evidence (probed land, then stock-mission vehicle clusters, then towns;
   nothing within 300 m of probed river points or 200 m of `terrain_water`,
   nothing within 400 m of a position DCS has refused), adds an Invisible
   FARP to each of the 5 FOBs, retypes 172 ground units and 46 aircraft,
   adds 11 action templates, recomposes the carrier groups, rewires the
   coalitions (Blue CJTF Blue, USA, Turkey, UK; Red CJTF Red, USSR) and sets
   each airfield's starting owner.
3. `build_options.py`, `build_warehouse.py`, `build_sams.py`.
4. `assemble.ps1` (bftools `special-sam` and `miz`).
5. `build_cfg.py` → `RSR_SF_CFG`.
6. `check_cfg.py`: loads the config **with the engine's own code**
   (`tools/cfgcheck`, a small program outside the engine repo that calls
   `bfprotocols::cfg::Cfg::load` and `StoryCfg::validate`). Result: loads and
   validates, no unknown keys; one inherited warning
   (`NavalCruiseMissileStrike.supply_cost` is read by nothing).
7. `verify.py`: the 2008 checks (names, one airbase or pad per AB/FO, SAMs
   outside objectives, every config template in the mission, every unit type
   classified, every stocked airframe priced, AI options decodable) plus the
   1985 ones (spacing, water, carriers, banned types, AI stock, story names).
   **All pass.**

### Not yet measured in DCS

The terrain probe was **not** run for this build: it needs DCS launched
headless with a hook in `Saved Games\DCS\Scripts\Hooks`. Everything is ready:

    python tools/make_probe2.py      # writes tools/probe/bfprobe.lua (26,210 points)
    copy tools\probe\bfprobe.lua  <Saved Games>\DCS\Scripts\Hooks\
    DCS.exe --server --norender with RSR_SF.miz in serverSettings.lua
    delete the hook again
    python tools/apply_probe2.py --moves-only
    .\rebuild-all.ps1

Until then "not in water" is inference from the terrain evidence, as the 2008
README warns. The refinery at Tuapse is all on road/town evidence and is the
first place to look if a spawn is refused (`python tools/whichzone.py <group>`).
Also untested in DCS: the CG zones naming the carrier objectives, the B-52's
bomb load (harvested GUID pylons), the Tu-22M3 anti-ship attack on the CVBG,
and the AI F-14/F/A-18 launching from the carrier.
