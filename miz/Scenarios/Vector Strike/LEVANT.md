# Levant Ablaze: the Syria campaign, rebuilt

The modern Syria campaign rebuilt around its story, the way Southern Flank was
built for Red Storm Rising. The story is `levant_ablaze.story.json` beside this
file (and on the public site and in the wiki, `shared/servers/syria.ts`). The
build lives outside the repo, in `Saved Games\DCS\Missions\Vector\levant\`, and
reads the 2.x Syria build (`..\Syria\`) only as a donor.

Decided with the user on 2 November 2026:

| | |
|---|---|
| Objectives | Redrawn around the story's three fronts, an even start, every place the story names |
| Ground truth | Syria is not installed on the build PC. Units go only on ground the donor proves; a terrain probe run on the server box confirms them before the campaign goes live |
| Spawning | Dynamic spawn only, as Southern Flank |
| Going live | A new sortie, `LEVANT` (`LEVANT_CFG`, save `LEVANT`). The ODFv2 round finishes untouched; the vs1 server switches at a round's end |

## The map: every airbase, the front line on the border

Every objective keeps the donor's position and zone, so its ground is proven:
the live server has spawned on every one of them. Joined on 3 November 2026:
Khalkhalah and Tha'lah (SAM sites stood on both fields) and six of the donor's
helipad FOBs, HJ04, HS39, HJ01 and HT02 for NATO, HS27 and HL11 for the RSII;
then every airbase on the map: the donor's (Paphos, Ercan, KingsField,
Gecitkale, Teyman, Nevatim, Sanliurfa, Diyarbakir were the last) and the 16
fields the donor never used, each given a zone round its runways (Ben Gurion,
Kedem, Megiddo, Zarqa, Ruwayshid, H4 Emergency, Nicosia, Gazipasa, Konya for
NATO; Deir ez-Zor, T2, H3, H3 Northwest, H3 Southwest, Marj Ruhayyil, Qabr as
Sitt for the RSII). 104 objectives, NATO 57 and the RSII 47: most of the
map's fields are on NATO ground.

**The front line is the border.** Every objective and SAM site starts on its
own side's ground: the RSII holds Syria, Lebanon and Iraq; NATO Turkey,
Israel, Jordan and Cyprus. The Golan follows the 1974 ceasefire line (Israel
holds the Heights). The build checks it (`tools/border.py`, a few km at worst;
`verify.py` fails anything over the border); nothing in the campaign was
over it, the line only had to be drawn.

| Front | NATO (Blue) | RSII (Red) |
|---|---|---|
| **South**: the Galilee, the Golan, Lebanon | Kiryat Shmona, Rosh Pina, Ramat David, Haifa, King Hussein Air College, Eyn Shemer, Herzliya, Tel Nof, Hatzor, Palmachim, King Abdullah II, Marka, Prince Hassan, Muwaffaq Salti (airbases); HS33, HI01, HI12, HJ02, HJ04, HS39 (FOBs); HI03 (factory); HI22 (logistics hub); HI06 HQ (command centre); Factory Ashdod | Naqoura, HL06, HL04, HS42, HS40, Marj as Sultan North, HS27, HL11 (FOBs); Mezzeh, Beirut-Rafic Hariri, Rayak, Khalkhalah, Tha'lah, Wujah Al Hajar, Al Dumayr, An Nasiriyah, Sayqal (airbases); HL01, Damascus (logistics hubs); SyADF 159th Regiment (command centre) |
| **North**: Hatay and Gaziantep against the Aleppo belt | Hatay, Gaziantep, Kahramanmaras, Adana Sakirpasa, Chukurova (airbases); Incirlik (logistics hub); Incirlik HQ (command centre); Mersin (naval base); HT01 Factory; Sanliurfa Heliport, HT02 (FOBs) | Taftanaz, Minakh, Aleppo, Kuweires, Jirah (airbases); Abu al-Duhur (logistics hub); AL Safira Military Base, Durayhim Military Base (factories) |
| **Centre** | | Air Defence Academy HQ (command centre); Shayrat, Rene Mouawad, Al Qusayr, Hama, Tiyas (airbases); SAA 312th Brigade (factory) |
| **Sea**: Cyprus and the Syrian coast | Akrotiri, Larnaca (airbases); HC04 (logistics hub); HC05 (factory) | Latakia (naval base); Bassel Al Assad (airbase) |
| **Euphrates** | HJ01 (FOB, east Jordan) | Sharqiyah Mine (factory); Tabqa, Palmyra (airbases) |

| | NATO | RSII |
|---|---|---|
| Airbases | 38 | 29 |
| FOBs | 9 | 8 |
| Logistics hubs | 3 | 3 |
| Factories | 4 | 4 |
| Command centres | 2 | 2 |
| Naval bases | 1 | 1 |
| Carrier groups | CVBG | RSII naval group |

## Forces

- **NATO**: M1A2C, Leopard 2, Merkava Mk4, M2 Bradley, Gepard, M6 Linebacker,
  M1097 Avenger, HEMTT, infantry with Stingers.
- **RSII** (Russia, Syria, Iran and Iraq): T-90M, BMPT, BMP-3, BMP-2,
  2S6 Tunguska, ZSU-23-4 Shilka, Osa, Strela, infantry with Igla.
- **Fleets**: NATO's carrier group in the Gulf of Iskenderun (CVN-72, Tarawa,
  a Ticonderoga, two Arleigh Burkes); the RSII naval group off the Syrian
  coast (Kuznetsov, the cruiser Moskva, a Neustrashimy, a Ropucha). Both stay
  where the donor's are: proven water. No Type 052 (not an RSII ship).
- **Air defence**: 89 hidden SAM sites (44 RSII SAM sites and 10 EWRs, 29
  NATO and 6 EWRs), laid out by the owner in `layouts\LEVANT_SAMS.miz`
  (below). They started as 116 chosen from the owner's Levant SAM template and
  the 2.x campaign's proven sites, strongest first, 12 km apart, within 80 km
  of their side's objectives (75 sites, 18 km and 60 km until 3 November
  2026), on the template's own unit spots:
  - the third of the RSII's SA-2 and SA-3 sites nearest the front are
    SA-11 Buk and SA-15 Tor (with a Pantsir) instead;
  - the seven NATO Hawk sites nearest the front are NASAMS and IRIS-T in turn;
  - 64 of the sites, every long- and medium-range one, have a point defence
    group of their own (`red_defence`/`blue_defence`): the RSII's S-300s and
    SA-5s a Pantsir and a Tor, its SA-11s two Tunguskas, its SA-2/3/6s a
    Shilka and a Strela; Cyprus' sites two Tors, NATO's Patriots two C-RAMs
    and two Avengers, its Hawks, NASAMS and IRIS-T an Avenger and a Gepard
    (or the C-RAMs the live site already had). The guns and missiles already
    in a site move into it; the rest stand on the site's truck spots, then on
    a ring 250 to 450 m out (32 of them), on spots the terrain probe cleared.
    The IADN's emission control leaves the defence group alone, so it stays
    hot while the SAM is dark, and its units count toward the site's health:
    taking a site means killing its guard too.

  Every site is networked through its side's nearest command centre: the
  Air Defence Academy cues the RSII's centre, north, coast and Euphrates, the
  159th Regiment the south; Incirlik HQ and HI06 HQ split NATO's north and
  south. Lose the command centre and its sites fall back to plain DCS AI.

- **Base air defence**: 67 bases have a long-range SAM in their garrison, an
  LR group the base's capture needs killed: every airbase, logistics hub, HQ
  and naval base with no long-range site of its own side within 8 km (31
  RSII bases with the SA-11 Buk, 36 NATO with the I-Hawk; the captor gets its
  own). Not HI06 HQ, whose 152 m zone is too small for a battery. The sites
  that stood on Khalkhalah and Tha'lah are those bases' MR groups now, and
  the Hawks that stood on Eyn Shemer and Sanliurfa their LR groups.

Garrisons are laid out unit by unit (`garrison_layouts`), each base on its own
ground (the build's `site_layout.py`, from the terrain probe: height and
surface every 50 m out to 1.45 radii, the buildings, runways, parking spots
and pads): guns, IR and short-range SAMs and tanks at evenly spaced bearings
all the way round the base's outline, each on the best ground near its spot;
the long-range battery behind on open ground; nothing on a runway, taxiway,
apron, parking spot, helipad, building or water. No two bases are alike.
They are in
`layouts\LEVANT_SITES.miz` in the build folder. The SAM sites and their
defence groups are laid out the same way, in `layouts\LEVANT_SAMS.miz`: move
or add them in the Mission Editor and rebuild. Both files also hold a palette
(`PALETTE - ...` groups, never built) of the Chinese kit (DCS's ChinaAssetPack:
HQ-7B, Type 59, ZTZ-96B, ZBD-04A, PLZ-05) and the Currenthill Assets Pack
[CH] units that ship with DCS (PGZ-09, ZTQ-15, T-90M, BMPT, TOS-1A, Iskander,
Stryker, HIMARS and more), to copy from. DCS has no Chinese long-range SAM.

## The campaign config

The donor's `SYRIA_CFG` (with the 2.1 story and war will) plus what Southern
Flank runs: the theatre HQ for both sides (`smart_commander.hq`), `ai_fill`,
`ai_air_stock`, `garrison_layouts`. Strike, CAS and SEAD templates are added to
the mission with loads from DCS's own payload library.

## Status (3 November 2026)

Built, checked and probed (`levant\README.md` in the build folder): LEVANT.miz
and LEVANT_CFG, 385 garrison groups (about 2,020 vehicles a side; only the
holder's stand), 89 SAM sites,
the HQ, ai_fill, ai_air_stock and the 14 story events. The config loads with
the engine's own loader and every cross-check passes. The terrain probe ran in
a local DCS with the Syria map: 78 garrison vehicles on water, in buildings or
by a runway were moved, and two SAM sites with a unit in the sea (the Naqoura
and Latakia EWRs) were left out. The three HQ zones, missing from every Syria
base after 20 September, are copied from the mission the live round runs.
Not deployed.

3 November 2026, the air defence redone with the owner: more sites, the mix
and the defence groups above, and the IADN linking fixed in bflib. The
defence group is new in the engine (`SpecialSamSiteCfg::red_defence` and
`blue_defence`, spawned as a second, Sr-class group); a campaign saved before
it keeps its sites without one, so it needs a fresh LEVANT campaign. The
second probe checked the 3,168 ring spots round the 88 sites: 3,147 clear. The links to command centres used
to be made before the SAM sites existed, so on a fresh campaign no site was
networked until the first restart; now they are made right after the sites,
on a fresh start and on every load. The "shield goes blind" story event now
names the area the Air Defence Academy actually cues (Homs to Aleppo, the coast
and the Euphrates; the 159th Regiment cues the south). The public site's Syria
data (`shared/servers/syria.ts`, "48 hidden SAM sites") still describes the
live 2.x map and changes when Levant goes live.

3 November 2026, later, with the owner: the SAM sites are the owner's own
layout (sites moved, removed and added in `LEVANT_SAMS.miz`; the live server's
"(red)"/"(blue)" tags dropped from their names), Khalkhalah and Tha'lah became
RSII airbases, and 26 bases have an LR SAM in their garrison. All built,
checked and probed again: 57,162 points, nothing left on water, a runway or a
building. The probe's runway test had been reading DCS's runway course as the
heading (it is minus the heading), so it tested each runway's mirror image;
with that fixed, 42 garrison vehicles by a real runway or in a building were
moved, and four coastal bases' batteries were put on a probed land grid.

Then the garrisons were cleared: DCS gives almost no Syria building a size,
so the probe had counted each as a 3 m dot and vehicles stood inside hangars,
warehouses and houses, and some on the map's helipads, where dynamic spawn
puts helicopters. With a footprint per building type and the pads found from
their edge lights, 181 garrison vehicles (27 of them at the six new FOBs) and
50 SAM-site units were moved to clear ground. The last probe run found
nothing left: 58,294 points, no vehicle on water, a runway, a pad, a building
or another vehicle.

Later still: twenty more airbases (40 a side), with 17 new LR batteries. Built,
checked and probed: 73 garrison vehicles there moved off buildings and water
(the Herzliya and Wujah Al Hajar batteries had stood in the sea; Wujah Al
Hajar's was placed again on the probe's land grid). The last run found
nothing left: 72,817 points, no vehicle on water, a runway, a pad, a building
or another vehicle.

Last: every airbase on the map (104 objectives) and the border check. The 24
new airbases' garrisons were cleared (168 vehicles off the new fields'
runways, where the generated groups had stood at the zone centre, then 41 off
buildings and water; Paphos's battery, in the sea, was placed again on the
probe's land grid). The last probe found nothing left: 82,692 points.

Then every garrison was laid out again on its own ground and round its whole
base (the owner: not bunched, not all alike, and clear of runways, taxiways,
aprons and parking). The probe now walks every base's terrain (1,057,528
points) and reads its buildings (406,769) and parking spots (4,413). The last
run found nothing on water, a runway, an apron, a pad, a building or another
vehicle.

9 November 2026, with the owner: a few garrison types changed in place
(ZSU-57-2 to PGZ-09, the Ural command and armoured trucks to the ZIL-131 Kung
and HX77, the APA-80 to the APA-5D), and the paint scheme. The build folder's
`layouts\LEVANT_PAINT.miz` has one vehicle of every type; the paint chosen
for each there goes into the config's new `ground_liveries` (unit type to
livery), which bflib applies to every garrison and SAM-site unit it spawns.
Desert where DCS has it, to start. Not yet seen in DCS.

## 9 November 2026: the live round's problems

From the Syria round's Discord reports ("Bombers are too powerful and
spammy", "All active bases get low on fuel", "Server performance very low")
and the owner's choices.

**Engine fixes (bflib, this branch):**

- **Convoy ambushes were uncapped.** The theatre HQ forgot an ambush the
  moment it launched, so it could send a new force every think, all after
  the same convoy. It also ignored `ambush_enabled: false`. Now
  `campaign_events.ambush_max_per_side` (default 1) caps the forces out at
  once, each after its own convoy, and the HQ respects `ambush_enabled`.
- **A wiped, neutral base kept sending forces.** The event scheduler's list
  of friendly bases was only refreshed on captures. It now rebuilds when any
  base on it changes owner, and an ambush re-checks its base (still ours,
  alive, not a SAM site) when it launches.
- **"30 vehicles" from a destroyed SAM site.** An ambush could take a SAM
  battery, dead or not, as its template. Now only armour with something
  alive qualifies. Dead bases and special SAM sites are also no longer
  departure points for helo troop road moves, reinforcements or ground-war
  formations.
- **Bombers.** `BomberCfg::strike_radius_m` limits a run to contacts that
  close to the aim point; it used to reach every friendly JTAC's whole
  view, up to ~30 km, which is how one run cleared three bases. A contact
  two JTACs see counts once, and a miss falls in any direction, not only
  north-east. Bombers launch from a field 100 km back when one has room,
  instead of air-starting over the front.
- **Every action can have a cooldown** (`Action::cooldown_secs`, per side,
  whoever calls it).
- **Fuel.** A base's fuel figure is now its scarcest fuel, not the average:
  averaged with three full liquids, an airfield with no jet fuel read ~70%
  and stayed green, so no alert, airlift or HQ resupply ever went to it. AI
  fixed-wing flights no longer take off from a field below
  `warehouse.ai_launch_min_fuel_pct`. C-130 fuel and weapons runs now pay
  points like other supply runs.
- **Paint:** `ground_liveries` (unit type to livery) for every garrison and
  SAM-site unit.

**Config (`levant_balance.py` in the build folder):**

- Bombers: 6 targets within 3 km, power 400, 60 m off; one a side at a time,
  30 min apart, 300 points.
- Points: new players start with 1,000 (was 30,000); kill points count only
  once the pilot lands (`provisional`); +25 every 15 min in a slot. Supply
  runs pay 600, repairs 500, captures 1,500, a destroyed convoy truck 25.
- Actions, Blue 25 and Red 26 (was 11 and 12), each with a limit and a
  cooldown: fighter cover, CAS, attack helicopters, SEAD, precision strike,
  paradrops, AI supply and repair runs, reinforcements, recon flights and
  fire missions, besides the AWACS, tankers, drones, naval strike and bomber
  the config had.
- Deployables, Blue 33 and Red 39: NASAMS, Rapier, TOW Humvees, an M-ATV
  scout and a Leopard/Bradley platoon for NATO; BTR-D Konkurs, BRDM-2, Cobra
  and a T-80UD/BMP-2 platoon for the RSII.
- Crates by tier, the same for both sides (the Syria config gave Red crates
  twice Blue's weight for the same kind of unit): light 500 kg (scouts,
  MRAPs, JTAC, logistics 1; EWRs 2), medium 1,000 kg (IFVs and guns 1,
  BMPT, C-RAM, Tunguska and short-range SAMs 2, Pantsir, Rapier and HQ-7 3,
  IRIS-T, NASAMS, SA-6 and Tor 4, Hawk and SA-3 5), heavy 1,500 kg (tanks 3,
  tank platoons 6, guns and light rocket launchers 2, HIMARS, Smerch, Uragan
  and TOS-1A 3, ballistic missiles 4). SAM kits keep their parts (launchers,
  radars, command post); the launchers make up the total. The FARP is as it
  was. Red's Tor is now named, and NASAMS's three crate types too.
- Supply: a delivery every 80 min (was 120), a supply crate moves 35% (was
  25), and both sides produce 1,000 t of each fuel (Red had 100).
- Ground war on, tight: 3 formations a side, 4 live in all, 2 groups each.
- **Formations from deployed units** (a Discord suggestion, 8 November): F10
  Ground Forces > "Form a formation from my deployed units" takes up to 6
  of the player's own deployed groups that can drive, within 3 km of them,
  into a formation that takes orders like any other (attack, defend, hold,
  withdraw, hand to the AI). Each formation's menu has "Disband": one formed
  this way goes back to being the player's deployed units where it stands;
  one raised from a garrison goes back to its posts. Formed ones count
  toward the 3 a side. Engine: `Db::form_from_deployed`,
  `Formation::formed`, `GroundWarCfg::form_from_deployed`, `form_radius_m`,
  `form_max_groups`.
- **Supply crates by kind** (`warehouse.supply_crates`). Five Base Supply
  crates a side, each carrying one kind of stock, in place of the
  all-purpose fuel and ammo crates: Fuel Resupply (1,200 kg; every fuel),
  A/A Missile Resupply (800 kg), A/G Munition Resupply (1,500 kg; air-to-
  ground missiles, bombs, rockets), Gun, Tank & Pod Resupply (900 kg) and
  Ground Equipment Resupply (1,500 kg; vehicles and materiel). Each moves
  35% of every item of its kind, from the base it was loaded at. Missiles are
  told apart by their DCS type (wsType level 3, read from the resource map at
  load), everything else by its warehouse prefix. A crate that would move
  nothing is refused (helo) or pays nothing (C-130). Engine: `SupplyClass`,
  `SupplyCrate`, `classify_item`, `Db::supply_crate_load`; the helo, truck
  and C-130 paths share the one filter, and the CARGO tab of the cockpit
  panel says what each crate carries.
- **Fuel and ammo crates now carry what they say.** On every campaign, the
  helo/truck fuel crate used to move everything (fuel, weapons, vehicles) and
  so did the ammo crate; only the C-130 split them. Now the fuel crate moves
  fuel and the weapons crate everything else, by any route.
- **Wingmen you choose, pay for and command** (`air_life.wingman.types`,
  `clone`). F10 > Wingman > Request lists what the aircraft can call, each
  with its price: Fighter escort 300 (one CAP fighter), CAS pair 400,
  Strike pair 500, SEAD pair 500, Attack helicopter 250 (helicopter pilots),
  and a copy of the player's own aircraft, 400 (250 for a helicopter; no
  C-130 or Chinook), which spawns from the player's slot group, or any slot
  of the type, with its loadout and paint. Points are taken once it is
  actually on its way. F10 > Wingman > Orders: cover me, engage air threats,
  attack near me, attack my latest map mark, hold here, weapons free /
  return fire / hold, close or spread formation. Each role engages what it
  is for (SEAD covers against air defences, strike and CAS attack ground
  units). Offered however many pilots are up (`max_side_pilots` 0), 60 min,
  10 min after one is lost. Engine: `WingmanRole`, `WingmanType`,
  `WingmanCloneCfg`, `airlife::order_wingman`, `Ephemeral::clone_templates`.
- **Naval convoys** (`warehouse.sea_logistics.lanes`, new). The engine's
  sea logistics only ever sailed from a naval base to a carrier group, and
  Levant has no carrier, so nothing ever sailed. A lane is a naval base, the
  objective it supplies and a path in open water; the convoy sails the path
  and unloads at its last point, on the carrier runs' rules (destination
  below `supply_threshold`, one pair a side per interval, route cap). Points
  on land are warned about at load and moved to the nearest water when a
  convoy sails; the F10 map draws the lane. Levant's lanes, all offshore and
  at 35 km/h: Mersin to Hatay via Iskenderun (~4 h) and to Nicosia via
  Kyrenia (~5 h); Latakia to Bassel Al Assad via Jableh (1 h), Rene Mouawad
  via Kleyate (~3.5 h) and Beirut (~5.5 h). Convoys (`build_base.py`):
  NATO's BNAVCONVOY, an Oliver Hazard Perry and two supply ships; the
  RSII's RNAVCONVOY, a Krivak II, the Atlantic Conveyor and a Ropucha.
  Sent when the destination is below 80%, at most two pairs a side at sea,
  written off after 7 h. Engine: `SeaLane`, `Db::live_sea_lanes`,
  `check_sea_lanes`, `SeaLogisticsRoute::path`.
- **Naval patrols** (`Cfg::naval_patrols`, new; engine `naval.rs`).
  Warship groups sail a fixed loop for whoever holds their naval base; if
  the base changes hands the patrol is withdrawn and the new owner's ships
  take it over. A sunk patrol is replaced after 60 min, and its side is
  told. Session state: a restart puts them back to sea. Levant's four, at
  25 km/h, every point 7 km or more offshore: Gulf of Iskenderun and the
  Cyprus strait (from Mersin), the Latakia coast and the Lebanese coast
  (from Latakia). NATO sails BPATROL (Arleigh Burke and Perry), the RSII
  RPATROL (Neustrashimy and Krivak II). The Cyprus strait and Latakia
  patrols are about 45 km apart.
- **F10 menus all page.** The formation submenu, Base Supply and five range
  lists were the last unpaged ones.

All built and checked (the engine's own loader, every template present, AI
airframes stocked); engine tests 258 + 108 pass, clippy clean. Not yet seen
in DCS.
