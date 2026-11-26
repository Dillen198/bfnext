# Red Storm Rising: scenario design

How to recreate the war in Tom Clancy and Larry Bond's *Red Storm Rising* in
Fowl Engine. [RESEARCH.md](RESEARCH.md) has the novel's campaign and the DCS
inventory this is built on, with sources. This file covers:

- what fits where;
- what works with today's engine;
- what needs new engine code;
- what DCS can't do at all, and how to fake it.

File and line references are to this repository as of October 2026.

## Summary

- **Two campaigns on two maps, not one.**
  - A Fowl Engine campaign is one DCS mission, so one map. Nothing in bflib
    spans maps. Several servers can share a dashboard, but each runs its own
    campaign (`deploy/multi-instance.md`).
  - The book has three theatres:
    - Central Front → **Cold War Germany**.
    - Iceland and the Atlantic, and Norway and Kola → both on **Kola**.
  - DCS has no Iceland map and none has been announced, so Iceland moves to
    an island on the Kola map (see 2.2).
- **The Central Front is the strongest fit.** Today's engine already covers:
  - the AI ground war, with fuel and ammunition that runs out;
  - supply lines that can be cut;
  - per-side production, the era bans, AWACS/EWR/IADN;
  - stealth strikes, through `Strike` with real weapons;
  - recon troops as JTACs.
  The book's "fuel war" is almost exactly the existing ground-war supply
  model.
- **The naval war is the weakest fit.** Three gaps block it:
  - DCS has no working submarine or ASW model;
  - the engine has one carrier group per side;
  - the engine has no sea-lift convoys.
  The bomber raids on carriers work only if a player or commander orders them:
  the AI commander values carrier groups at 0 (`bflib/src/hq/picture.rs:84`).
- **The story needs phases.** The engine has random and reactive events plus
  cyclic offensive/regroup tempo, but no "when X happens, do Y" scenario
  script and no ceasefire. Ten pieces of engine work turn "an 80s map with 80s
  units" into "Red Storm Rising" (section 7). The scenario-events system is
  the one that matters most.

---

## 1. Ground rules

1. **No mods.** Every player would need them, and they break on patches. The
   design uses only core DCS, the two maps, and modules that are on sale.
2. **1985 hardware.** Player kit is held to 1985 by the era rules (section 5).
   Where DCS lacks the book's hardware, use the stand-ins in 1.1 and keep the
   book's names in the briefings and callsigns.
3. **Paraphrase the book.** Briefings, wiki pages and messages describe events
   in our own words: no quoted text and no copied chapter structure. Character
   names are only flavour (callsigns, VIP names), so they can be dropped if
   that is a concern.
4. **Players make the story.** The book's events are *opportunities* (a phase
   opens, an objective becomes important, a bonus pays out), not scripted
   outcomes. If Red players hold Alfeld, the war goes Red's way.

### 1.1 Stand-ins

| Book | DCS stand-in | Note |
|---|---|---|
| F-19 "Frisbee" | **F-117A** (AI) | AI only. `Strike` holds weapons until 20 km out "so a stealth jet stays quiet" (`bflib/src/db/actions.rs` Strike). It can't carry AIM-9s, so the A-50 kills go to players |
| Tu-16 Badger (strike, decoy, tanker) | Tu-22M3 for strike, **Il-78M** for tanker, **Tu-142** for decoy/scout | H-6J is the only Badger lookalike but it is Chinese; avoid it |
| Tu-22 Blinder | Tu-22M3 | |
| Tu-95 Bear (scout) | Tu-142 | |
| Mainstay | **A-50** | Exact |
| F-111 / EF-111 | Tornado IDS (AI) for low-level raids; **F-4E with ALQ-131** or a `Jammer`-tagged AI for jamming | The engine's `UnitTag::Jammer` degrades radar detection within `iadn.jamming_range_m` |
| F-4G Wild Weasel | F-4E or F-16C with HARM, in a `Sead` action | |
| A-6 Intruder | **A-6E** (AI, Heatblur) | Exact |
| French F-8 on *Foch* | F/A-18C restricted to AIM-7 and AIM-9 | Players on a "French" carrier |
| P-3 Orion | S-3B (AI) with Harpoon | No P-3 in core |
| *Kirov* | **Pyotr Velikiy** | Exact class |
| Kiev class, *Foch* | Kuznetsov (Red), Forrestal (*Foch*/*Saratoga*) | |
| *Nimitz* / *Independence* | Supercarrier or Vinson / Forrestal | |
| *Saipan* (LHA) | **Tarawa** | Exact class |
| Knox / Spruance / Iowa | Perry / Ticonderoga / no stand-in | Battleship gunfire becomes `Artillery` or `NavalCruiseMissileStrike` |
| Victor / Charlie / Alfa / Los Angeles | Kilo / Tango as shapes only | They can't fight. See 4.4 |
| *Julius Fučík* | Ropucha or a cargo ship | |
| Convoy merchant ships | **Atlantic Conveyor**, Handy Wind, Seawise Giant | |
| M1 (1985) | M1 (later variant) or M60 | |
| T-64 | T-72B / T-80B | |

---

## 2. Theatres and maps

### 2.1 Campaign A: "Central Front" on Cold War Germany

**Map fit.**
- Phase 1 covers central East and West Germany and is built for the 1980s.
- The book's ground war happens there: Stendal, the Elbe crossings, Hannover,
  Alfeld on the Leine, Hameln on the Weser.
- Airfields reported in Phase 1 include Wunstorf, Gütersloh, Fassberg,
  Bückeburg (helicopters), Bitburg, Spangdahlem, Ramstein and Hahn.

**Check in the mission editor first.**
- Phase 1's western and northern edges: is Bitburg near the edge? Is Hamburg
  inside?
- Whether the Leine and Weser bridges at Alfeld and Hameln are separate,
  destroyable scenery objects.
- Which GDR airfields Phase 1 has.

**Objective layout (sketch).** Placement follows real geography, west to east.

| Zone | Kind | Side | Book role |
|---|---|---|---|
| Bitburg | AB | B | Stealth fighter base |
| Spangdahlem, Ramstein, Hahn | AB | B | NATO rear air |
| Gütersloh, Wunstorf, Fassberg | AB | B | NATO forward air |
| Bückeburg | FO | B | Helicopters |
| Hannover | LO | B | NATO's main supply hub |
| Hameln | LO | B | **Weser crossing.** Red's operational goal |
| Alfeld | FO | B | **Leine crossing.** The set-piece battle |
| Fölziehausen / Duingen | FO | B | Late Soviet attack |
| Hildesheim, Salzgitter, Braunschweig | FO / FAC | B | Front-line towns; FAC for industry |
| Inner German border posts | FO | N | Neutral no-man's-land at D-Day. Neutral objectives get no garrison (`bflib/src/db/mizinit.rs:1824`) |
| Magdeburg, Tangermünde | LO | R | **Elbe crossings.** Dreamland's bridge targets; Red's supply hubs |
| Stendal | CC | R | Soviet Western front HQ (Command Center: IADN and HQ depend on it) |
| Red fuel and ammunition dumps (3-5) | FAC | R | **"Fuel war" targets.** Factories with `production_rate`, named "Fuel Dump" |
| GDR airbases (from the Phase 1 list) | AB | R | MiG-29, MiG-23, Su-25 and Su-24 bases |

**Size.** Aim for 50-70 objectives. Scalability is untested beyond about 100
objectives and 1,000 units (`ROADMAP.md:1343`). CWG also needs 32 GB of RAM
on its own. Use the cull settings (`unit_cull_distance`,
`ground_vehicle_cull_distance`, `lr_cull_distance`).

### 2.2 Campaign B: "Northern Flank" on Kola

DCS has no Iceland map, so the Iceland story is rebuilt around **Andøya**.

**Why Andøya.**
- It is a real island airbase (in the Vesterålen islands, northern Norway),
  and it was a P-3 base in 1985: a Keflavík in miniature.
- It is about 600 km from the Kola bomber bases.
- The Norwegian Sea to its west is the carriers' and convoys' water.

Everything the book does at Keflavík happens there: the seizure, the B-52
raids, the MiG-29 trap, the stay-behind team, the recapture.

| Book place | Kola stand-in | Kind | Side at start |
|---|---|---|---|
| NAS Keflavík | **Andøya** | AB | **R** (campaign starts at D+2, after Polar Glory; see 3.2) |
| Reykjavík harbour (*Fučík* unloads) | Andenes harbour, or a coastal FOB on Andøya | FO | R |
| Stykkishólmur (recon target, landing beach) | A coastal village on Langøya or Hinnøya. Pick one in the editor that has a beach and a road to Andøya | FO | R |
| Stay-behind team's hill | A hilltop on Andøya | n/a (spawned recon troops) | B |
| RAF Stornoway (F-14s, Tornados, Blinder raid) | **Bodø** | AB | B |
| Narvik (*Kirov* action) | Narvik waters; Evenes and Bardufoss as NATO fields | AB | B |
| Norwegian front (Soviet pressure) | Banak, Kirkenes | AB | B (contested) |
| Northern Fleet, Severomorsk | **Severomorsk-1/-3** and a naval base at Severomorsk/Polyarny | AB, NB | R |
| Backfire bases (Doolittle targets) | **Olenya, Monchegorsk, Kilpyavr, Severomorsk-3** | AB | R |
| Sweden, Finland | Neutral | N | No objectives. Overflying them could be penalised later |
| *Nimitz* carrier group | Blue CG (`BCARRIER`) | CG | B |
| Soviet surface group (*Kirov*) | Red CG (`RCARRIER`): Kuznetsov + Pyotr Velikiy + escorts | CG | R |

**Kola coverage.** The map ends at about 65°N, so Bodø (67.3°N) is in and
Trondheim is out.

**Alternatives considered for Iceland.**
- **South Atlantic** (the Falklands as Iceland): a truer isolated island, but
  it loses the Kola bomber bases and Doolittle.
- **Cyprus** on the Syria map: wrong ocean.

Kola keeps four of the five naval set pieces in one geography.

### 2.3 Optional Campaign C: "Gulf what-if" on Persian Gulf

The book's war aim, which never happens. A "what if Red Storm had worked"
spin-off: a Soviet push into Iran against a US Central Command response.
Lowest priority, and not part of the book's events.

### 2.4 Linking the two campaigns (later)

Each campaign runs on its own server and finishes on its own, so linking
them is optional. The DCSServerBot `fowlengine` plugin could:

- read one server's events (Andøya recaptured, Doolittle done);
- call admin RPCs on the other server (`Broadcast`, `ReduceInventory`; see
  `bflib/src/admin.rs:88-200`).

For example, if the Northern Flank stops the Backfires, Central Front Red
gets less resupply. That is new bot code (section 7, item 10).

---

## 3. The war as campaign phases

### 3.1 Central Front phases

| Phase | Book | What opens or changes | Ends when |
|---|---|---|---|
| 0. Briefing | Maskirovka, Kremlin bomb | Briefing and wiki only. Start state: border neutral, Red massed at the Elbe | Server start |
| 1. **Dreamland** | D-Day night | Night start. Red A-50 stations (HQ-managed AWACS). Blue: F-117 `Strike` actions unlocked at low cost; **bonus points** for an A-50 kill and for each Elbe hub hit. Red: the ground offensive starts (tempo set to offensive) | The first in-game day |
| 2. **Drive to the Leine** | Week 1-2 | Red formations aim for Alfeld and Hildesheim. Red can call a **MiG-23 mass raid** on the E-3 stations (a `Fighters` action with a big template) | Alfeld falls or holds for N hours |
| 3. **Alfeld** | Breakthrough | Alfeld becomes the main effort for both sides (`SetObjectivePriority`). Red's capture puts Hameln in reach | Red holds Alfeld, or Red loses its bridgehead |
| 4. **Fuel war** | Weeks 3-4 | **Intel reveal:** Red fuel dumps are marked for Blue. Bonus for dump and tanker-truck kills. Red production starts falling | Red fuel stock under a threshold, or Hameln falls |
| 5. **Weser or counterstroke** | Endgame | Red: the last offensive toward Hameln and the Weser. Blue: counterstroke at Alfeld | **Ceasefire** (3.3) |

### 3.2 Northern Flank phases

| Phase | Book | What opens or changes | Ends when |
|---|---|---|---|
| 0. Polar Glory (start state) | The seizure | Campaign starts with Andøya Red-held and garrisoned (MiG-29 slots, SAMs), Bodø and Evenes Blue, the Blue CG west of Lofoten. A Blue **recon team** is spawned on the Andøya hill as a JTAC | Server start |
| 1. **Vampires** | Backfire raids | Red: Tu-22M3 `Strike` (AntiShip) against the Blue CG, plus Tu-142/Il-78 support. Blue: CAP over the CG and from Bodø. Losing the CG costs Blue heavily | Blue CG survives N raids, or is lost |
| 2. **Nordic Hammer and the MiG trap** | Week 1-3 | Blue: B-52 `Strike` (Runway) on Andøya; Sead and jammer support; bonus per MiG-29 killed over Andøya | Andøya's runway is down, or its MiG force falls under N airframes |
| 3. **Doolittle** | Weeks 3-4 | Blue: `NavalCruiseMissileStrike` / `CruiseMissileSpawn` (Tomahawk) on Olenya, Monchegorsk, Kilpyavr and Severomorsk-3. **Bonus when it lands within N minutes of a Red bomber raid returning.** A successful strike stops Red's bomber actions (higher cost, or locked) | The four airbases are struck |
| 4. **Recapture** | Iceland retaken | Blue: recon at the "Stykkishólmur" village (troops with `can_capture`), then airlift and helicopter troop insertion from the carrier and Bodø, with `Artillery` / `NavalCruiseMissileStrike` as battleship gunfire. *Kirov* side quest: a Red CG sortie toward Narvik | Andøya captured, or Red holds until the round timer |

### 3.3 How it ends: the ceasefire

The book ends with a ceasefire in place, not a total victory. The engine
today has three end conditions:

- `auto_reset` with `MapOwned{fraction}`: equal-weight objective count
  (`bfprotocols/src/cfg/rules.rs:73-88`);
- `max_round_days` (a draw);
- `last_stand`.

None of them can express "Red takes the Weser crossing" or "the Soviets ask
for terms".

**Recommended ceasefire rule** (needs new code, section 7, item 1):

- **Red wins** if it holds Hameln (the Weser crossing) and Alfeld at the same
  time for N hours.
- **Blue wins** if Red's fuel stock (the RED supply source's diesel and jet
  fuel) stays under X% for N hours, *or* Blue retakes Alfeld after Phase 4.
- **Ceasefire (draw)** after the round's day limit, scored on the line held.
  This matches the book's ending.

**Northern Flank:** Blue wins on Andøya captured plus Doolittle done. Red wins
if the Blue CG is lost and Andøya is still Red at the time limit.

**Today, without that code:**

- use `max_round_days` for the time limit;
- set `last_stand` on a small set of primary objectives;
- have an admin announce the ceasefire with `Broadcast`;
- run phase changes by hand with the admin and bot commands
  (`SetObjectivePriority`, `ReduceInventory`, `HqDirective`), or from DCS
  mission triggers through the `vector_strike` Lua API
  (`bflib/src/api.rs:1-14`).

---

## 4. Set pieces against the engine

How to read the table:

- **Today**: what config and `.miz` work give us now.
- **Gap**: the engine work needed for the full version.
- **DCS limit**: what can't be done at all.

| # | Set piece | Today | Gap | DCS limit |
|---|---|---|---|---|
| 1 | Dreamland: kill the A-50s, drop the Elbe bridges | Red HQ-managed AWACS stations (A-50) are real targets. Blue players hunt them. AI `Strike` Precision with F-117 hits hub buildings | Bridges aren't objectives (no bridge or river code anywhere in `bflib/src`). A **crossing** objective kind (item 3) | No player stealth jet. F-117 RCS is a flat value **[uncertain]** |
| 2 | MiG-23 mass raid on the E-3s | Red `Fighters` action with a big MiG-23MLD template, aimed at the AWACS mark | None. Maybe a "max group size" check on performance | None |
| 3 | Polar Glory (seizing the island) | Not playable as a live surprise attack: capture needs `health <= 20 && infantry == 0` (`bflib/src/db/objective.rs:526`). **Start the campaign after it** | Coup-de-main capture: mass paradrop plus a seizure rule (item 7) | No AI paradrop. Script it |
| 4 | Dance of the Vampires | Red `Strike` AntiShip with a Tu-22M3/Kh-22 template, ordered by players or a commander. It targets an enemy CG within 40 km of the mark (`bflib/src/db/actions.rs:3228`). Blue fleet SAMs fight the missiles | The AI commander never plans it: CG value 0 (`bflib/src/hq/picture.rs:84`), HQ doesn't use `Strike`. Decoy wave. **One CG per side** (`bflib/src/db/mizinit.rs:1009`), so *Nimitz*, *Saratoga* and *Foch* can't all be separate objectives | Tu-22M3 salvo behaviour needs "expend all" **[test]** |
| 5 | Nordic Hammer (B-52 runway raid) | Blue `Strike` Runway with a B-52H template; `Sead` with F-4E/F-16C; a `Jammer` unit tag on an escort | None | No EF-111 or stand-off jammer AI |
| 6 | Low-level raid on the runways and power station | `Strike` Runway / Precision with Tornado IDS AI | None | None |
| 7 | MiG-29 trap | Players: Blue F-14A against Red MiG-29A. A `Jammer`-tagged B-52 bait flight | A **bonus-points rule** for kills over a named objective (part of item 1) | None |
| 8 | Blinders on Stornoway (Bodø) | Red `Strike` Precision or Runway with Tu-22M3 on Bodø | None | None |
| 9 | Tanker hunt | Red HQ-managed Il-78 tanker stations are targets | None | None |
| 10 | Ferry-flight ambush | Blue F-15C slots; Red bomber flights returning to base are on the map anyway | None | None |
| 11 | Convoy escort and ASW | Merchant ships exist only as cosmetic `air_life.civil_traffic.ships`. There is no convoy carrying supply | **Sea-lift convoys** between naval bases (item 2). **Scripted ASW** (item 8) | **No sonar, no ASW weapons, AI subs don't attack** |
| 12 | *Kirov* off Narvik | Red CG with Pyotr Velikiy, moved by `CarrierWaypoint`. Blue F/A-18 Harpoon strikes at it with player weapons. Blue `Strike` AntiShip works against a Red CG | A "submarine attack" action to fake *Kobben* (item 8) | No working submarine torpedoes |
| 13 | Doolittle | Blue `NavalCruiseMissileStrike` from the CG's Ticonderoga at the Kola airbases' centres (`bflib/src/db/actions.rs:2443`). The "after the bombers land" timing is the players' job | Time-on-target bonus, and Red bombers losing strike power (item 1). Submarine launch platform (item 8) | Tomahawk is land-attack only; that is fine here |
| 14 | Final strike on the island | `Sead` + `Fighters` + player HARM shooters against Andøya's SAMs | None | None |
| 15 | Landing at Stykkishólmur | Recon troops with `jtac` (`bfprotocols/src/cfg/deployables.rs:261`). Player helicopters carry troops from the CG (its zone counts as friendly logistics) **[inference: test]**. `Paratrooper`. `Artillery` as gunfire support | **Amphibious landing** (item 7). Recon troops feeding `SpecialForces` intel (item 9: nothing produces that source today) | No beaching or unloading from ships |
| 16 | Alfeld breakthrough | `ground_war` + `modern_war.tempo`; HQ steering; Red templates bigger than Blue's; A-10A, Mi-24P, Su-25 players | Crossings (item 3). Quality versus numbers: combat firepower is per role, not per side or type (`bflib/src/db/combat.rs:61`) (item 4). `max_live_formations` defaults to 6 |
| 17 | Fuel war | **Supported.** Formations carry fuel and ammunition; supply lines can be cut (`supply_line_cut_m`); out of supply means half speed and less firepower (`bfprotocols/src/cfg/ground_war.rs:210-345`). Red's `supply_source` is short on fuel. Fuel-dump factories | A phase that reveals the dumps (item 1) | None |
| 18 | The commander's helicopter | A Red Mi-8 VIP flight as a `campaign_events`-style event, with a points bonus | A VIP event: `vip_reward_points` exists but is unread (`bfprotocols/src/cfg/events.rs:125-142`) | None |
| 19 | Final battle and ceasefire | `max_round_days` | Ceasefire and weighted victory (item 1) | None |

### 4.1 Numbers against quality

The book's balance is Soviet numbers against NATO quality. Today it can only
come from the `.miz` and per-side config:

- **Red:** bigger garrison templates; more `ground_war` formations; a higher
  `supply_source` production; more AI fighter rosters in `hq.air[Red]`.
- **Blue:** better aircraft slots (F-14A, F-15C, A-10A); more expensive but
  stronger actions (`Strike` with F-117 and Tornado).
- **Global, not per side:** combat firepower, `treasury_start`, `economy`,
  `points` and `max_formations_per_side`. An M1 and a T-72 count the same
  when the battle is resolved off-map.

Item 4 adds a per-unit-type firepower table, so a T-55 reserve division is
weaker than a T-80 guards division.

### 4.2 The fuel crisis

The fuel crisis is how the book's war ends, and the engine models it well:

1. `warehouse.supply_source` for RED holds little jet fuel and diesel. Each
   delivery is limited by what the source has
   (`bflib/src/db/logistics.rs:1872-1950`).
2. Red's fuel and ammunition dumps are **Factory** objectives. Losing them cuts
   Red's production (`production_scaling` uses factory weights).
3. Ground formations burn fuel. Once supply runs short they slow to half
   speed and lose firepower, which is the book's Alfeld offensive running dry.
4. Blue air power on Red truck convoys (convoy interdiction points) makes it
   worse.

The missing piece is the moment NATO *learns* the war is about oil: a Phase 4
intel reveal that marks the dumps for Blue (item 1).

### 4.3 Bridges and river lines

DCS bridges can be destroyed, but AI ground units route around wrecked bridges
unreliably, and DCS has no bridging units. The engine has no bridge or river
concept at all.

- **Today:** put a small FOB or Logistics objective at each crossing town
  (Alfeld, Hameln, Magdeburg, Tangermünde). Taking it means "taking the
  crossing". Turn on `warehouse.front_line_routing`, so a hub can't supply
  across enemy-held objectives.
- **Item 3:**
  - Add a **Crossing** objective kind tied to one or more DCS bridge scenery
    objects. **To verify:** that bflib can read a bridge's life or its death
    event.
  - When the bridge is down, formations can't route across it within N km,
    and convoys detour or stall.
  - An engineer deployable (a "pontoon" crate costing materiel) restores it
    for its owner after a delay.

  This gives Dreamland's Elbe strikes and Alfeld's demolitions real weight.

### 4.4 Submarines and ASW

DCS has no sonar, no working ASW weapons, and AI submarines don't attack.
The book's submarine war can only be **abstracted**.

- **Contacts.** "Submarine contacts" are engine-side tracks with an uncertain
  position that drift across the sea. They are not DCS units, or they are a
  DCS Kilo/Tango kept as a visual shape only.
- **Detection.** Blue ASW (S-3B AI, SH-60B AI, player helicopters hovering
  low over a contact area, frigates) sets a detection probability.
  - A "sonobuoy" action lays a field.
  - A player helicopter in hover inside the field acts as a dipping sonar.
- **Kill.** A located contact can be "attacked" (an action or a player in
  range), with a kill probability.
- **Red submarines.** Undetected Red submarines cost the side with the ships
  something: lost merchant ships (item 2), or damage to the CG.
- **Narrative strikes.** *Kobben*'s attack on *Kirov* and the Doolittle
  Tomahawk boats become actions with a hidden launch platform.

The `command.hunters` system already sails real ships against the enemy.
Red's default "Submarine hunt" uses a `Type_093` (`bflib/src/command/orders.rs`).
For 1985 that is anachronistic, so switch it to a Kilo or a surface hunter.

This is item 8, the largest optional piece. Skip it and the Atlantic war is
air against ships only, which is still most of the book's naval set pieces.

---

## 5. Era and slots (1985)

**`era`.** Restricts airframes at slot time and bans weapons by substring.

- Takeoff with a banned weapon: the player must land within
  `banned_weapon_land_secs`.
- Firing a banned weapon destroys the aircraft
  (`bfprotocols/src/cfg/campaign.rs:126-160`).
- Starting ban list: AIM-120, AIM-9X, GBU-31, GBU-32, GBU-38, GBU-54, JSOW
  (AGM-154), SLAM, AGM-84H/K, laser JDAM, APKWS, R-77, Kh-38, KAB-500S. Match
  each against the DCS weapon names when writing the config.
- Targeting pods (LITENING, ATFLIR, Sniper) are pylons. Leave them out of the
  loadout templates and warehouses, and ban them too.

**Warehouses.**
- `warehouse.miz`'s `BINVENTORY` / `RINVENTORY` carry only 1985 items.
- Anything at 0 in a side's supply source is zeroed on all its warehouses
  (`bflib/src/db/logistics.rs:1917-1946`). That is the real era enforcement for
  pylons.

**Slots.**

| | Central Front | Northern Flank |
|---|---|---|
| Blue | F-15C, F-16C (restricted), A-10A, F-4E, F/A-18C (restricted), Mirage 2000C / F1 (as allies), UH-1H, OH-58D (unarmed scout), Gazelle, C-130J (as a C-130H) | F-14A (Navy, on the CG), F/A-18C (restricted; also the "French F-8"), F-15C, F-16C (Norwegian), F-5E (Norwegian), AV-8B (Marines, if owned), UH-1H, C-130J |
| Red | MiG-29A, Su-27, Su-25, MiG-21bis, L-39, Mi-24P, Mi-8 | MiG-29A (Andøya regiment), Su-27, MiG-21bis, Mi-8, Mi-24P |
| AI | F-117A, Tornado IDS, B-52H, E-3A, KC-135; A-50, Su-24M, MiG-23MLD, MiG-27K, MiG-25 | Tu-22M3, Tu-142, Il-78M, A-50; S-3B, E-2C, B-52H, A-6E, SH-60B |

**Modern defaults to turn off or replace.**
- `modern_war.ew` (GPS jamming).
- Rail with ES44AH locomotives.
- Raids with Iskander or ATACMS.
- Hunters with Type 093 or Arleigh Burke IIa.
- Modern civil air types.
- `harm_codes` / `anti_radiation_weapons`: keep only AGM-88 and Kh-58/Kh-25MP.

**Coalitions.** Some engine-spawned units are hardcoded to CJTF Blue and CJTF
Red: hunters, sea drones, trains and GPS jammers
(`bflib/src/command/orders.rs:983`, `bflib/src/modern_war/boats.rs:134`,
`bflib/src/modern_war/rail.rs:170`, `bflib/src/db/group.rs:1374`).

- Blue: USA, UK, West Germany, Belgium, Netherlands, Norway, France, plus CJTF
  Blue.
- Red: USSR, GDR, Poland, plus CJTF Red.

**Germany magnetic variation.** The ATIS table has no Germany entry; it falls
back to 0 (`bflib/src/atis.rs:136-170`). Set `magnetic_variation_deg` in the
Central Front config from what the DCS map uses, or add the entry (item 10).

---

## 6. Config sketch

This is the shape, not a working file. Key names come from
`bfprotocols/src/cfg/` and `miz/SAMPLE_CFG.json`. Write the real files by
copying `miz/SAMPLE_CFG.json` (the richest reference) and checking every key
against it.

Files:
- `miz/Scenarios/Red Storm Rising/Central Front/`: `base.miz`, `warehouse.miz`,
  weapons template, `RSR_CENTRAL_CFG`.
- `miz/Scenarios/Red Storm Rising/Northern Flank/`: the same, with
  `RSR_NORTH_CFG`.

Settings that differ from Vector Strike:

- **`era`:** the 1985 list and bans (section 5).
- **`warehouse`:**
  - Red `supply_source` low on fuel;
  - `front_line_routing: true`;
  - `materiel` enabled, so repairs cost war stocks;
  - convoys on.
- **`factory`:** Red fuel dumps with weight in `production_scaling`.
- **`ground_war`:**
  - on, `live_only`;
  - Red with more formations;
  - `supply_line_cut_m` set so interdiction matters.
- **`modern_war`:**
  - `tempo` on (Red offensive, Blue regroup at start);
  - `ew` off;
  - rail and raids either off or re-templated to 1985 (e.g., Scud-B for Red
    raids).
- **`smart_commander.hq`:**
  - `steer_ground_war` on;
  - Red `hq.air` with A-50 and Il-78 stations;
  - Blue with E-3 and KC-135.
- **`iadn`:**
  - on, with Command Centers (Stendal for Red);
  - jammers on.
- **`actions_red` / `actions_blue`:** see the set-piece table. Key entries:
  - Blue `Strike` with F-117 (Precision) and B-52 (Runway);
  - Red `Strike` AntiShip with Tu-22M3 (Northern Flank);
  - `NavalCruiseMissileStrike` (Blue);
  - `Paratrooper`;
  - recon troops with `jtac`.
- **`campaign_events`:** helicopter patrols and reactive CAP on; modern events
  off.
- **End of round:**
  - `max_round_days` about 5-6 in-game weeks, scaled to server time;
  - `last_stand` on Hannover/Hameln for Blue and Stendal/Magdeburg for Red,
    until item 1 exists.
- **`magnetic_variation_deg`:** set it (Central Front).

---

## 7. Engine work, in priority order

Sizes: **S** = a day or two, **M** = about a week, **L** = several weeks.

| # | Work | Size | Why | Where |
|---|---|---|---|---|
| 1 | **Scenario events and phases.** A `scenario_events` config: a list of `{when, then}` rules. *When*: game time, objective owner, an objective being held for N hours, a side's stock under X, kills of a type over a zone, another rule having fired. *Then*: broadcast and briefing text, set objective priority, unlock or lock or reprice an action, bonus points, reveal intel, change tempo, change inventory, **end the round with a result** (win, loss or ceasefire). Rules persist so a restart doesn't refire them | **L** | Turns a sandbox into this story. Also gives every future scenario weighted victory | New `bfprotocols/src/cfg/scenario.rs`, evaluated in bflib's slow tick next to `auto_reset` |
| 2 | **More than one carrier group per side**, plus **sea-lift convoys**: ship groups between naval bases, or from an off-map entry point to a naval base, that deliver supply and can be sunk | **M + M** | The Atlantic war: *Nimitz*, *Saratoga* and *Foch* as separate targets; REFORGER convoys; bombers with something to hunt | Carrier binding in `bflib/src/db/mizinit.rs:989-1030`; extend `sea_logistics` in `bflib/src/db/logistics.rs:7456` |
| 3 | **Crossing objectives** tied to DCS bridges, gating formation routes, with engineer repair | **M** | Elbe, Leine and Weser | Objective kind, formation routing (`bflib/src/db/formation.rs`) |
| 4 | **Per-side and per-unit-type combat strength** (and per-side treasury) | **S-M** | Numbers against quality | `bflib/src/db/combat.rs:61-100` |
| 5 | **AI anti-ship raid planning**: carrier groups worth more than 0 to the HQ; the HQ plans `Strike` AntiShip | **S-M** | Backfire raids without a human Red commander | `bflib/src/hq/picture.rs:84`, `bflib/src/hq/planner.rs` |
| 6 | **Per-airframe radar cross-section** in the engine's EWR/GCI/IADN picture | **S** | F-117 is otherwise as visible to our radar model as an F-15 | `bflib/src/ewr.rs:551-569` |
| 7 | **Airborne and amphibious assault**: a mass drop of several squads from one action, a seizure rule for a lightly held objective, and a landing action that spawns troops on a beach next to a friendly ship | **M** | Polar Glory as a playable event; the Marine landing | `bflib/src/db/actions.rs` Paratrooper, `bflib/src/db/objective.rs` capture rules |
| 8 | **Abstract submarine and ASW layer** (4.4) | **L** | The convoy and submarine war | New module |
| 9 | **Recon troops produce `SpecialForces` intel** (the source exists, nothing feeds it) | **S** | The stay-behind team as an intel source | Troop JTAC scan → intel DB |
| 10 | Germany magnetic variation entry; 1985 defaults (Scud-B raids, Kilo hunter); optional **cross-server link** in the bot | **S / M** | Polish; linking the campaigns | `bflib/src/atis.rs:136`; DCSServerBot `fowlengine` plugin |

**Also relevant: a bug.** Carrier auto-repair is free.
- `check_carrier_auto_repair` says supplies were deducted but takes nothing
  (`bflib/src/db/objective.rs:4098-4170`).
- The manual action does charge (`bflib/src/db/actions.rs:2349`).
- The naval war's economy depends on it.

**Minimum to launch a recognisable Central Front:** config and `.miz` work,
item 10's magnetic variation, item 4, and a first version of item 1 (phase
unlocks, bonuses, ceasefire). The rest can follow.

---

## 8. Checks to run in DCS before building

1. **CWG Phase 1 coverage.** Alfeld, Hameln, Hannover, Stendal, Magdeburg,
   Bitburg: on the map or not? Which GDR fields exist?
2. **Bridges.** Are the bridges at Alfeld (Leine), Hameln (Weser) and
   Tangermünde/Magdeburg (Elbe) separate scenery objects that can be
   destroyed? What does AI ground pathing do when they are down?
3. **Tu-22M3 against a carrier group.** Does a flight of 6-12 with Kh-22 and
   "expend all" fire a full salvo in the current build? How many get through
   a Ticonderoga + Perry screen?
4. **F-117 `Strike` Precision.** Does it hit hub buildings at night through a
   SA-6/SA-11 belt?
5. **Kola Andøya.** Does the island have a road from the beach village to the
   airfield? Is there harbour space for the "*Fučík*" ship group?
6. **Helicopter troop pickup on a CG.** Does a player helicopter load troops
   on the carrier deck (cargo rules, `bflib/src/db/cargo.rs:462-487`)?
7. **Performance.** CWG with 60 objectives, 6 live formations, and 40 players
   on the server hardware.

---

## 9. Decisions for the owner

1. **Which campaign first?** Central Front is recommended: best map fit, most
   features already there.
2. **Iceland stand-in:** Andøya on Kola (recommended), or the South Atlantic.
3. **Character names in briefings and callsigns:** use them (flavour only), or
   keep everything generic.
4. **ASW (item 8):** build the abstract layer, or leave the submarine war out.
5. **One server or two:** run Central Front and Northern Flank in parallel
   (two servers, and later the item 10 link), or one after the other on one
   server.
