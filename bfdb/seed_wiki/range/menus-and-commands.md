# Range Menus and Commands

On the range almost everything is done from one F10 menu, **F10 > Range**. It
is the same for every aircraft. There is a single chat command, and the range
site has a spawner that offers the same things as the menu with more choice.

## The F10 menu

DCS shows ten entries a page, so the Range menu has exactly ten. Long lists
(the stations, for one) continue under `More >>`. Click a command to see what
the server answers:

```f10menu
{"root":"F10 Other","items":[{"label":"Range","items":[
 {"label":"Range status","says":"Vector Strike Range - 4 players flying\nYou are in PT-1 ANDERSEN (LANDING PATTERN)\nStations:\n  R-7201 FDM - Bomb Circle [bomb circle] ... 1/1 targets, cold  16.0175 146.0592\n  ...\nTankers:\n  Texaco1 KC-135 (boom) 25000 ft, 251.000 MHz, TACAN 51Y TEX\n  ...\nCVN-72 Abraham Lincoln: BRC 045 FB 036, 27 kt WOD, deck OPEN, TACAN 72X ABL, ICLS 11","note":"The sector you are in, then every station, tanker and carrier with bearing and range from you."},
 {"label":"Sectors: what is where","note":"Every sector nearest first, with bearing, range and what it is for. See Range Sectors and Airspace."},
 {"label":"Air-to-Ground","items":[
  {"label":"Stations","items":[
   {"label":"R-7201 FDM - Bomb Circle","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
   {"label":"R-7201 FDM - Strafe Pit","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
   {"label":"R-7201 FDM - Coordinate Targets","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
   {"label":"TN-1 North Field - Tactical Array","items":[
    {"label":"Info / bearing","says":"TN-1 North Field - Tactical Array: 012 for 18.4 nm, elevation 541 ft. 6 of 6 targets up. Laser code 1688. AI laser on the first live target, code 1688."},
    {"label":"Smoke the target","says":"TN-1 North Field - Tactical Array: red smoke on the target"},
    {"label":"Reset targets","says":"Targets respawned"}]},
   {"label":"TN-1 North Field - Moving Convoy","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
   {"label":"TN-1 North Field - Coordinate Targets","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
   {"label":"R-3 Aguijan - SA-8 Threat","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets","note":"Instructors only: this station is locked."}]},
   {"label":"G-1 Northwest Field - CA Gunnery Lane","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
   {"label":"TN-2 Tinian MLA - Easy","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
   {"label":"More >>","items":[
    {"label":"TN-2 Tinian MLA - Medium","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
    {"label":"TN-2 Tinian MLA - Hard","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
    {"label":"EW-1 Tinian jammer - GPS-denied targets","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
    {"label":"AS-2 Escorted Convoy","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]},
    {"label":"AS-1 SINKEX - Hulks","items":[{"label":"Info / bearing"},{"label":"Smoke the target"},{"label":"Reset targets"}]}]}]},
  {"label":"Ground targets 10 nm ahead","items":[
   {"label":"Armour platoon (4x T-72B)","says":"Armour platoon (4x T-72B) spawned 087 for 10 nm"},
   {"label":"Trucks (4x Ural-375)"},
   {"label":"Mech. section (2x BMP-2, 2x Ural)"},
   {"label":"Convoy with ZSU-23-4","note":"The Shilka is weapons free."}]},
  {"label":"SAM site 20 nm ahead (instructor)","items":[
   {"label":"SA-8 Osa"},{"label":"SA-15 Tor"},{"label":"SA-6 Kub"},{"label":"SA-11 Buk"},{"label":"SA-3 Goa"},{"label":"SA-2 Guideline"},
   {"label":"ZSU-23-4 Shilka pair","says":"Unable: sam_site is instructor-only","note":"Every SAM here is for instructors."}]},
  {"label":"SEAD / IADS ranges","items":[
   {"label":"Enemy air defence sites"},
   {"label":"Rebuild a network (instructor)","items":[{"label":"S-1 Rota IADS"}]}]},
  {"label":"EW: GPS jammers"},
  {"label":"CAS / JTAC","items":[{"label":"Check in with Axeman","note":"Axeman passes you a nine-line. See Bombing and Strafing."}]}]},
 {"label":"Air-to-Air","items":[
  {"label":"Set-up selection","items":[
   {"label":"Set-up","items":[{"label":"BFM offensive"},{"label":"BFM defensive"},{"label":"BFM neutral pass"},{"label":"BFM high perch"},{"label":"BFM head-on"},{"label":"BVR hot"},{"label":"BVR flanking"},{"label":"BVR beaming"},{"label":"Missile defence drill"}]},
   {"label":"Adversary","items":[{"label":"MiG-29S Fulcrum"},{"label":"Su-27 Flanker"},{"label":"MiG-21bis Fishbed"},{"label":"F-5E Tiger II"},{"label":"F-16C Viper"},{"label":"F-15C Eagle"},{"label":"F/A-18C Hornet"},{"label":"J-11A Flanker"},{"label":"JF-17 Thunder"}]},
   {"label":"Weapons","items":[{"label":"Guns only"},{"label":"Fox 2 (IR missiles)"},{"label":"Fox 1 (semi-active radar)"},{"label":"Fox 3 (active radar)"}]},
   {"label":"Skill","items":[{"label":"Average"},{"label":"Good"},{"label":"High"},{"label":"Excellent"}]},
   {"label":"BVR range","items":[{"label":"10 nm"},{"label":"20 nm"},{"label":"30 nm"},{"label":"40 nm"},{"label":"60 nm"}]},
   {"label":"Number","items":[{"label":"1"},{"label":"2"},{"label":"4"}]},
   {"label":"Show selection","says":"Selection: offensive vs MiG-29S x1, guns (High), BVR range 30 nm. F10 Range > Air-to-Air > FIGHT'S ON to spawn."}]},
  {"label":"FIGHT'S ON (spawn selection)","says":"BFM offensive: MiG-29S Fulcrum x1 spawned 042 for 0.5 nm. Fight's on!"},
  {"label":"Duel (PvP)","items":[{"label":"Challenge nearest player"},{"label":"Accept challenge"},{"label":"Cancel / knock it off","says":"Knock it off: duel cancelled"}]},
  {"label":"Missile trainer ON","says":"Missile trainer ON: a missile that would kill you is removed and scored"},
  {"label":"Missile trainer OFF (live missiles)","says":"Missile trainer OFF: missiles are LIVE"},
  {"label":"Hot zone: AWACS picture"},
  {"label":"Hot zones: status"}]},
 {"label":"Tankers / Carrier","items":[
  {"label":"Tankers on station"},
  {"label":"Tanker on my position","items":[{"label":"KC-135"},{"label":"KC135MPRS"},{"label":"KC130"},{"label":"KC130J"},{"label":"S-3B Tanker"},{"label":"A6E"},{"label":"IL-78M"}]},
  {"label":"Carrier status (BRC/FB/WOD, TACAN, ICLS)"},
  {"label":"My last pass","note":"Your last few results, the same as Results > My last results."}]},
 {"label":"Anti-ship","items":[{"label":"Cargo ship 30 nm ahead"},{"label":"Tanker (Elnya) 30 nm ahead"},{"label":"Bulk carrier 30 nm ahead"},{"label":"Supply ship 30 nm ahead"}]},
 {"label":"Helicopter","items":[
  {"label":"Landing pads"},
  {"label":"Dynamic cargo: where to deliver"},
  {"label":"Sling-load courses","items":[{"label":"H-1 Andersen - Sling Course"},{"label":"H-1 Andersen - Heavy Lift"}]},
  {"label":"Troops: load at","items":[{"label":"H-1 Andersen - Air Assault"}]},
  {"label":"Troops: unload here"},
  {"label":"CSAR","items":[{"label":"CSAR areas and missions"},{"label":"CS-1 South Guam","items":[{"label":"Pilot down (cold)"},{"label":"Pilot down, enemy troops near (HOT)"}]}]},
  {"label":"Ship decks: where are they"}]},
 {"label":"Low level / Landings","items":[
  {"label":"Low-level routes: gates and times"},
  {"label":"How landings are graded","says":"LANDINGS: every runway landing is graded: touchdown against the aim point (300 m past the threshold), the centreline, sink rate in the last second, and your approach at 1 nm and 1/2 nm against a 3 deg glideslope. Fly a stable straight-in or a proper overhead break; touch-and-gos are graded too. Your card follows 15 s after touchdown."}]},
 {"label":"My spawns","items":[{"label":"List"},{"label":"Despawn all","says":"2 spawn(s) removed"}]},
 {"label":"Results","items":[
  {"label":"My last results","note":"The headlines of your last five results this session."},
  {"label":"Help","says":"Range help: every bomb, strafe pass, carrier pass, tanker session, missile shot and drill is graded and posted to range.vectorstrike.org with a full debrief. Chat: -range trainer on|off."}]}
]}]}
```

The menu appears once you are in a slot. When a request can't be done, the
answer starts with `Unable:` and says why, for example
`Unable: you need to be airborne for an air-to-air set-up`.

## Where each part is explained

| Menu | Page |
| --- | --- |
| Range status, Sectors | [Range Sectors and Airspace](./sectors.md) |
| Air-to-Ground | [Bombing and Strafing](./bombing-and-strafing.md), and SEAD on [Missile Trainer, Threats and Air Combat](./missile-defence.md) |
| Air-to-Air | [Missile Trainer, Threats and Air Combat](./missile-defence.md) |
| Tankers / Carrier | [Air-to-Air Refuelling](./refuelling.md), [Carrier Passes and Landings](./carrier-passes.md) |
| Anti-ship | [Missile Trainer, Threats and Air Combat](./missile-defence.md#anti-ship) |
| Helicopter, Low level / Landings | [Helicopters, Low Level and Ground](./helicopters-and-ground.md), [Carrier Passes and Landings](./carrier-passes.md#field-landings) |
| Results | [Range Results and Debriefs](./results.md) |

## Chat command

There is one:

| Command | What it does |
| --- | --- |
| `-range trainer off` | Missiles are live for you: `Missile trainer OFF: missiles are live` |
| `-range trainer on` | The missile trainer protects you again |
| `-range` (anything else) | A one-line reminder that everything is on F10 > Range, and the site's address |

The line you type isn't shown to anyone else. `-linkme <code>` also works
here; it is DCSServerBot's, for linking your Discord (see
[The Training Range](./overview.md#the-range-site)).

## The spawner

Anything you spawn, from the F10 menu or from the **Spawn** page on
range.vectorstrike.org, follows the same rules:

| Rule | |
| --- | --- |
| Spawns up at once | 3 per pilot |
| Between requests | 20 seconds |
| Server limit | 60 AI units spawned by players at once |
| Expiry | 1 hour (an air-to-air adversary, 30 minutes at most) |
| When you leave the server | Everything you spawned is removed |

The CAS drill doesn't count against them. **F10 > Range > My spawns > List**
shows what you have up; **Despawn all** removes it.

The site's spawner needs you logged in with a linked DCS account, and in an
aircraft on the range server. It has a searchable catalogue with every
parameter, your last five set-ups to repeat, the time left on everything you
have up, and a warning when something that spawns around you needs you
airborne. What it offers:

| Item | Choices |
| --- | --- |
| **BFM set-up** | Offensive, defensive, neutral pass, high perch, head-on; adversary, skill, weapons; 1v1 or 1v2 |
| **BVR presentation** | Adversary, range 10 to 80 nm, hot / flanking / beaming / hot high / hot low, skill, Fox 2 / 1 / 3, single, pair or four-ship |
| **Missile defence drill** | Shooter, range 8 to 40 nm, hot / flanking / beaming, Fox 3 / 1 / 2 |
| **Tanker on your position** | Type, altitude 8,000 to 30,000 ft, leg 10 to 40 nm |
| **Ground targets ahead** | The four compositions, 5 to 30 nm ahead, stationary or driving |
| **Ship target** | Cargo ship, tanker, bulk carrier or supply ship, 1 to 3, 10 to 80 nm ahead, under way or stopped |
| **Sling-load course** | Either course |
| **CAS drill** | Axeman |
| **Defended warship** (instructors) | Molniya, Grisha, Krivak or Neustrashimy, 1 or 2, 20 to 100 nm ahead, weapons hold or free |
| **SAM / AAA site** (instructors) | SA-8, SA-15, SA-6, SA-11, SA-3, SA-2 or a Shilka pair, 10 to 40 nm ahead, weapons hold or free |

BFM, BVR, the missile drill and a tanker need you airborne. Ground targets and
SAM sites have to land on land (`... ahead of you is water - point at land and
try again`), and ship targets on open water.

### Instructors

Instructors can also despawn anyone's spawns, reset locked stations such as the
SA-8 at R-3, and rebuild the SEAD network early.
