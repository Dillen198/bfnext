# Garrison layouts: the two Mission Editor files

Garrisons are laid out vehicle by vehicle (the engine's `garrison_layouts`),
not from G trigger zones. Two missions decide how they stand. Open them in the
DCS Mission Editor, move and turn vehicles, save, then run `rebuild-all.ps1`.

| File | What it is | How the build uses it |
|---|---|---|
| `RSR_FORMATIONS.miz` | How each kind of unit deploys, drawn once | Stamped at every garrison site, turned to face the nearest objective the other side holds |
| `RSR_SITES.miz` | Defences placed by hand at a real place (an airfield) | Taken exactly as placed. That objective then uses nothing else |

`tools/make_layout_files.py` wrote the first versions. It never overwrites
these files unless you pass `--force`, so your edits are safe from rebuilds.

## Your four objectives set the standard

Four objectives in `RSR_SITES.miz` were laid out by hand, and every other
objective copies the one for its kind (`tools/patterns.py`):

| Kind | Copies |
|---|---|
| Airbase, and the HQ (Tbilisi) | Batumi |
| FOB | Ozurgeti |
| Logistics hub, and the ports (Poti, Novorossiysk) | Batumi port |
| Factory, refinery, dam | Batumi oil terminal |

To change a kind everywhere, edit its hand-built objective, save, then run:

```
python tools\make_layout_files.py --patterns
.\rebuild-all.ps1
```

`--patterns` backs up `RSR_SITES.miz` to `RSR_SITES.before-patterns.miz.bak`.
It then rewrites every other objective's vehicle and ship groups. Statics and
your four objectives are kept as they are, so hand edits to a copied objective
are lost the next time it runs.

How a copy lands:

- **Airfields:** each vehicle keeps its place along the runway, measured as a
  share of half the runway, so a long or a short runway gets the same layout.
  It also keeps its distance off the runway in metres. The side the enemy is
  on is mirrored to match.
- **Everything else:** each vehicle keeps its offset from the middle of the
  compound, turned so the enemy lies the same way it does at the source.
- **Clearing:** a vehicle that lands on water, a road, a static, a map
  building (once the probe has listed them) or an airfield's paved area moves
  to the nearest clear spot.
- **Ships:** Batumi port, Poti and Novorossiysk each get a warship (Perry for
  NATO, Krivak II for the Soviets) and two transports (Elnya and Tilde
  supply). They sit on known water at least 120 m off land and 150 m apart.

Two types are changed for 1985 wherever they are used: T-72B3 becomes T-72B,
and NEUSTRASH (a 1993 frigate) becomes REZKY (Krivak II).

The sections below explain how the first drafts were made. The copies above
now decide where every vehicle stands.

## RSR_FORMATIONS.miz

On the steppe north of Mineralnye Vody, the Soviet versions are in the top row
and the NATO versions are 3 km south of them. Each formation has:

- **An anchor**: a red flag static named `F <formation> / anchor`. Its
  heading is "forward", toward the enemy. The flags are placed facing north.
- **Its groups**: one or more vehicle groups named
  `F <formation> / <CLASS> <label>`, for example `F AAA / AAA battery`.

`<formation>` is the garrison stem the objective profiles use: `AAA`, `SR`,
`SMLR`, `ARMOR`, `ARMORB` or `SERVICES`.

**Scenery templates** sit in two more rows further south. Each one is the
compound an objective of that kind gets: `FOB`, `DEPOT` (logistics hub),
`REFINERY` (refineries and the oil terminal), `WORKS` (other factories and the
Enguri dam), `PORT` and `HQ`.

- **Naming:** statics named `S <kind> / <n>` round a red flag named
  `S <kind> / anchor`.
- **Sides:** the Soviet version is under Red, the NATO version under Blue.
  They differ only in the parked trucks.
- **In the mission:** the build makes them neutral, and the engine puts any
  destroyed one back.
- **Where they are used:** only where an objective has no scenery of its own
  in `RSR_SITES.miz`.

- **Variants**: add `AAA#2`, `AAA#3`... for variety. Each garrison site picks
  one of a stem's variants, the same one every build.
- **Groups**: a formation with several groups gives several garrison groups at
  that site.
- **Sides**: a formation under Red is the Soviet version, under Blue the NATO
  one. Delete one side's version and the build makes it from the other by
  swapping vehicle types (`SWAP` in `tools/formations.py`, for example
  KS-19 → Vulcan, T-72B → M1).

Moving a vehicle moves it in every garrison stamped from that formation. You
can change what a formation is made of too: add, remove or retype vehicles.
That changes the garrison's strength everywhere it is used.

## RSR_SITES.miz: the airfields

The 19 airfields are laid out from DCS's own aerodrome charts (the GND
charts in `Mods\terrains\Caucasus\Kneeboard`), by `tools/airfields.py`:

1. **Tie the chart to the map.** Each chart prints its two runway thresholds'
   coordinates and its scale. The script finds the runway bar on the image,
   so any pixel on the chart becomes a map position. On Kutaisi the chart's
   printed reference point (ARP) lands exactly on the ⊕ symbol drawn on the
   chart.
2. **Place the package by what the chart shows.** Guns go by the aircraft
   stands and ramps, the gate post on the access road and the fuel point at
   the fuel depot. Short-range SAMs go round the runway: beside both
   approaches first, then north and south of the field.
3. **Check every vehicle against the chart and the map.** It must stand on
   open ground (white on the chart, never a runway, taxiway, apron, stand or
   building), stay out of water and stay inside the objective.

The package is the same for both sides at the same tier (`PACKAGE` in
`tools/airfields.py`). A base's medium- or long-range SAM is already one of
the special SAM sites, so it isn't repeated.

Every element is a whole unit, the same size on both sides:

- **Gun platoon:** 4 guns in two pairs.
- **Short-range SAM platoon:** 4 launchers.
- **Medium-range battery:** 4 launchers and a search radar.
- **Tank or infantry platoon:** 4 vehicles.

| Tier | Airfields | Per airfield |
|---|---|---|
| main | NATO: Kutaisi, Senaki, Batumi, Kobuleti, Sukhumi, Gudauta. Soviet: Sochi-Adler, Tbilisi-Lochini, Vaziani, Beslan, Nalchik, Mozdok | 3 gun platoons (12), a flak battery (6 Gepard, or 6 S-60 and a SON-9 radar), 2 short-range SAM platoons (8), a medium-range battery (5), a tank platoon (4), 2 fuel trucks: 37 (38 Soviet) |
| rear | Soviet: Krasnodar-Center, Krasnodar-Pashkovsky, Krymsk, Anapa, Gelendzhik, Maykop, Mineralnye Vody | the same four arms, one unit of each: a gun platoon, a short-range SAM platoon, a medium-range battery, a platoon of older tanks, plus 1 fuel truck: 18 |

Vehicle types by side:

| Role | NATO | Soviet |
|---|---|---|
| AAA | Vulcan | ZSU-23-4 Shilka (both radar-directed guns) |
| Short-range SAM | Chaparral | Strela-10 |
| Medium-range SAM | Roland (4 + radar) | Osa (4 + P-19 radar) |
| Tanks | M1, or M60 at a rear field or light FOB | T-72B, or T-55 |
| Infantry carriers | 2 Bradley + 2 M113, or 4 M113 | 2 BMP-2 + 2 BTR-70, or 4 BTR-70 |

Placement: the medium-range battery sits about 900 m off the runway's middle,
and the tanks 1.1–1.7 km out toward the enemy.

`airfields/` holds an overlay of each airfield's layout on its chart, and
`airfields_all.png` shows all 19 together.

**Editing:** move groups in this file and the build takes them exactly as
placed. To redraw one airfield from scratch instead, change its sheet in
`tools/airfields.py` (chart pixels for the guns, the gate post and the fuel
point) and run `python tools/make_layout_files.py --airfields`. That rewrites
this file, so it loses hand edits.

## RSR_SITES.miz: the FOBs

There are 19 FOBs: the 11 you built in the Mission Editor and 8 new ones.

**New FOBs** (27 v 27 now):

- **NATO:** Gali, Oni, Mestia, Abasha.
- **Soviet, north of the ridge:** Alagir, Baksan, Cherkessk, Belorechensk.

**Compounds:**

- **Where they come from:** every FOB uses your design. The 11 you placed
  are as you placed them. The 8 new ones were stamped from your Ozurgeti
  compound, which is also the library's FOB scenery template (`S FOB`).
  All 19 are saved in `fob_compounds.json` and written into this file as
  `<FOB> - scenery <n>`.
- **Objective position:** each FOB objective is centred on its compound.
  Your Gagra, Kazbegi, Zugdidi and Lanchhuti compounds stood outside their
  zones, so those objectives moved to them. Gagra is now 9.8 km up the coast
  from the town.
- **Moving a compound:** if you move one, move its objective too (in
  `tools/build_oob.py`, under `FOB_POINTS`).

**Pads:** each of your compounds has three heliports: a FARP and two Zell
launchers. The engine refuses two airbases in one objective zone, so the
build keeps the FARP as the FOB's pad, owned by its side and named after the
FOB, and leaves the two Zells out. The ski ramps stay as scenery.

**Defences** round each compound (`tools/fobs.py`) use a small package that
is the same for both sides at each tier:

| Tier | AAA | SAM (MANPADS) | Short SAM | IFV and APC | MBT | Fuel | Supply | Vehicles |
|---|---|---|---|---|---|---|---|---|
| fob_hv (front) | 2 platoons | section (4) | 1 platoon | 1 platoon (Bradley/M113, BMP-2/BTR-70) | M1 / T-72B platoon | 2 tankers | 4 trucks | 34 |
| fob | 1 platoon | section | 1 platoon | 1 platoon (Bradley/M113, BMP-2/BTR-70) | M1 / T-72B platoon | 2 tankers | 2 trucks | 24 |
| fob_lt | 1 platoon | section | 1 platoon | 1 platoon (M113, BTR-70) | M60 / T-55 platoon | 2 tankers | 2 trucks | 24 |

The tankers are HEMTTs for NATO and ATZ-5s for the Soviets. The supply trucks
are M818s and Ural-375s. Both park behind the compound.

- **Gun platoons:** 220–280 m from the compound, the two pairs 150 m apart.
- **MANPADS section (FOBs):** 300 m behind the compound, the gunners 80 m apart.
- **Medium-range battery (hubs, factories, ports, HQ):** 450–500 m behind the compound, the launchers 200 m round the radar.
- **SAM platoons:** 650–750 m out, with the launchers 250 m apart.
- **Infantry carriers:** lining the nearest road into the compound, not blocking it.
- **Tanks:** 550–650 m toward the enemy, 100 m apart.

## Scenery templates

`RSR_FORMATIONS.miz` holds a scenery template for every kind of compound, in
the rows south of the formations: Soviet versions in the first row, NATO
versions in the second. They differ only in the parked trucks.

| Template | Used by | Pieces | What it is |
|---|---|---|---|
| FOB | the 19 FOBs | 205 | your hand-built FOB (Ozurgeti) |
| DEPOT | logistics hubs (Batumi port, Samtredia, Gori) | 155 | warehouses, a container yard, pallets, fuel tanks and a truck park behind a fenced front, gate, watchtowers and light masts |
| DEPOT_FWD | forward hubs (Zestafoni, Surami tunnel, Ordzhonikidze) | 157 | a HESCO-walled yard: ammunition bays in HESCO, fuel bladders, containers, trucks under nets, tents and a radio mast |
| REFINERY | Tuapse, Krasnodar, the Batumi oil terminal | 110 | a 12-tank farm, process units and stacks, fuel bladders, a tanker loading row, fenced front |
| WORKS | Chiatura, the Enguri power station | 115 | sheds, a tower crane, excavators, containers and stock |
| PORT | Poti, Novorossiysk | 174 | container stacks, two tower cranes, fuel tanks, the port office, a truck park |
| HQ | Tbilisi | 96 | the command bunker, an antenna farm, tents, staff cars and trucks under nets, all inside HESCO |

`tools/scenery_templates.py` wrote the six new ones (`make_layout_files.py
--scenery-templates`), in the hand-built FOB's style and spacing:

- HESCO segments 11.8 m apart and concrete barriers 4.1 m.
- Containers side by side 3.6 m apart.
- Light masts about 40 m apart and a watchtower on each corner.

**Editing:** change a template in the editor and every objective of that kind
that has no compound of its own changes with it. To re-stamp an objective's
compound from the template, delete its `<Objective> - scenery` statics from
`RSR_SITES.miz` and its entry from `compounds.json`, then rebuild.

**Placement:** each compound was placed where the whole template fits: off
water and roads, inside its objective, and where it could, at least 500 m
from a town's centre. There is no building data, and a compound on a street
block stands in the houses.

- **In town:** Chiatura (a mining town in a gorge) and Samtredia had no open
  ground to spare, so their compounds stand in town.
- **In big cities:** Tbilisi, Batumi port, Poti and Gori are still in built-up
  areas.

Check these in the editor.

## RSR_SITES.miz: logistics hubs, factories, ports and the HQ

These are done the same way as the FOBs. Each has a scenery compound (from
the templates above, saved in `compounds.json`, written here as
`<Objective> - scenery <n>`) and a point defence round it (`tools/fobs.py`).
The package is the same for both sides for each kind:

| Profile | Package | Vehicles |
|---|---|---|
| lo (logistics hub) | 2 gun platoons, a SAM platoon, an infantry platoon at the gate, 1 fuel truck | 17 |
| lo_hv (forward hub: Zestafoni, Surami tunnel, Ordzhonikidze) | the same, plus a medium-range battery and a tank platoon | 26 |
| fac (refinery, terminal, works) | 2 gun platoons, a medium-range battery, a SAM platoon, an infantry platoon at the gate | 21 |
| nb (naval base) | the same as a factory | 21 |
| cc_hv (Tbilisi HQ) | 3 gun platoons, a medium-range battery, 2 SAM platoons, an infantry platoon, a tank platoon | 33 |

An element with no room at its own spot (Novorossiysk's harbour side, the
Surami gorge) swings round the compound until it fits whole.

**Flak:** every main airbase, logistics hub, factory, port and the HQ has a
heavy flak battery, 13 per side. The Soviet battery is six S-60 57 mm guns in
a hexagon about 45 m across round their SON-9 fire-control radar (FM 30-102).
NATO had no towed heavy flak in 1985. Its battery is six Gepards, a
radar-directed gun the engine rates the same class as the S-60.

**Checking:** `tools/audit_garrisons.py` runs at the end of every build. It
checks every objective, both sides' versions, against its package: every
group there, every vehicle there, every type right. It also checks that each
airbase has AAA, short-range SAM, medium-range SAM and MBT, and that each FOB
has AAA, MANPADS, short-range SAM, IFV/APC, MBT, fuel and supply trucks.
A missing unit stops the build.

**Balance:** every objective gets the same package as the other side's
objectives of its kind and tier. The totals are 744 NATO against 718 Soviet
alive at the start. Some roles differ because of the map: the Soviets hold
13 airbases to NATO's 6, so they have more medium-range SAMs, and NATO holds
13 FOBs to their 6, so it has more MANPADS and infantry. `BALANCE.md` lists
every objective.

**Map buildings:** nothing has been checked against map buildings yet, because
there is no building data on the build PC. `bfprobe.lua` now writes
`bfscenery_out.txt`, which lists every building round every objective. Copy it
with the other probe files and the build keeps vehicles and scenery out of
buildings. `verify.py` then names any garrison or static still standing in
one.

`RSR_FORMATIONS.miz` is only used for an objective you delete from this file.

## Runways from DCS

`bfprobe.lua` now also writes `bfrunways_out.txt`, with every runway DCS has.
Copy it next to `bfprobe_out.txt` (`E:\Saved Games\DCS\` or Downloads). Then
run `make_layout_files.py --force` to redraw the airfield drafts on the real
runways. Only do this before you have edited `RSR_SITES.miz`, because it
rewrites the file. Afterwards, move the groups by hand.

## CLASS

`AAA SR MR LR ARMOR SERVICES INFANTRY LOGI OTHER`, or a stem: `SMLR` means
OTHER and `ARMORB` means ARMOR, the same classes the engine gave those
templates. The class decides repair order and how the group counts toward
objective health. Without a class word, the build works it out from the
vehicle types.

## Checks every build runs

Stamped vehicles are moved up to 90 m to the nearest spot that passes these
checks, then left out if none does:

- off known water (110 m)
- off the roads
- clear of the scenery and of other vehicles
- inside the objective

`verify.py` then checks every vehicle against water again, along with the
unit types and the objective names. The water probe (`tools/make_probe2.py`)
asks DCS about every vehicle's exact spot.

## Where the first formations come from

The constants at the top of `tools/make_layout_files.py` carry a source each.
Values marked "judgement" have none.

- **FM 30-102** (1977): S-60 battery, 6 guns in a ring 30–50 m apart round
  the radar and generator.
- **FM 100-2-1** (1984): ZSU-23-4 in pairs 150–250 m apart. Company
  strongpoint 500–1,000 m wide, up to 500 m deep, one echelon, vehicles about
  100 m apart. Vehicles up to 100 m apart under air threat.
- **FM 44-3** (1977, change 1981):
  - Airbase defence by Vulcan and Chaparral.
  - Vulcans in pairs on or near the asset, each within 1,000 m of another.
  - Chaparrals out from the asset all round, each within 2,000 m of another.
- **FM 71-1** (1988) and **FM 17-15**: fighting positions at least 100 m
  apart, tanks 100–250 m, combat trains 500–1,000 m behind.

Not found in any source: internal spacing of SA-6, SA-8 and Roland batteries,
a KS-19 layout, depot spacing, and runway-relative siting at either side's
airfields. Those are judgement, to correct in the editor.
