# Red Storm Rising: Southern Flank (Caucasus)

The first Red Storm Rising campaign, on the **Caucasus** map.

- **One dynamic campaign on one PvP server.** Both sides are always open to
  players. When a side is short of players the AI fills the gap, so a quiet
  server plays like PvE and a full one is decided by people.
- **A story set off by players.** Story events fire when players (or the
  AI) do something that matters: burn a refinery, hit the carrier, take an
  airfield. Each one changes the war and makes the news, crediting whoever
  caused it. Nothing fires on a clock ([ENGINE.md](ENGINE.md) 3.4).

[DESIGN.md](DESIGN.md) is the general design (era rules, stand-ins).
[ENGINE.md](ENGINE.md) is the engine work this campaign needs. File
references are to this repository as of October 2026.

## Summary

- **The book has no Caucasus theatre, so this is the book's war on its
  southern flank.** The setting fits: in 1985 the Transcaucasus Military
  District and the Black Sea Fleet are where a Soviet drive on Turkey and
  the Gulf oil would start. The novel's war is a fight over oil, so its
  ingredients carry over: the fuel war, Backfire raids on a carrier group,
  a stealth strike on bridges, an amphibious hook, and a war that ends when
  one side can't go on.
- **NATO arrives by sea.** Georgia is Soviet territory in 1985, so Blue
  starts in a beachhead at Batumi, Kobuleti and Poti with a carrier group
  offshore. Red holds the rest. The front is the **Rioni valley**.
- **The AI fills in for missing players** (section 5). It already does for
  a side's own head count; it needs to match the *other* side's too, so one
  pilot gets a fight their size.
- **The Caucasus is a good first map for building.** It is released and
  finished, its magnetic variation is in the engine already
  (`bflib/src/atis.rs:138`), it has a real Black Sea for the naval war, and
  mountain passes and rail tunnels that act like the book's river lines.

---

## 1. The setting

**Before the campaign starts (briefing only).** The war in Germany is a week
old. The Transcaucasus Front is moving south through Georgia and Armenia
towards Turkey and, beyond it, the Gulf. NATO's answer is to go for the
Front's supply lines. A US carrier group passes the Bosporus, and US Marines
with Turkish troops land on the Adjarian coast in **Operation Golden Fleece**
(western Georgia is ancient Colchis). They take Batumi, Kobuleti and Poti.

That is the only fixed story: the starting position. From there on the
story is told by story events that players set off (section 3).

**What each side is fighting for.**

- **Blue:** starve the Front. Cut the coastal railway, the Surami rail
  tunnel and the roads over the mountains, burn the refineries, break out
  up the Rioni. Keep the carrier alive.
- **Red:** throw the landing back into the sea before the fuel runs out.
  Sink the carrier. Keep fuel moving south.

**How it ends** (section 3): one side's will to fight breaks (fuel gone,
losses too heavy, key ground lost), the beachhead falls, or the round runs
out.

1985 place names in briefings: Ordzhonikidze (now Vladikavkaz), Sukhumi,
Tbilisi. Flavour only.

---

## 2. The map

### 2.1 Layout

Objective kinds are the engine's zone prefixes: OAB airbase, FO forward
operating base, LO logistics hub, NB naval base, CG carrier group, FAC
factory, CC command centre.

**Blue start: the beachhead**

| Objective | Kind | Notes |
|---|---|---|
| Batumi | OAB | Main Blue airfield. Blue `supply_source` |
| Kobuleti | OAB | Second airfield, closest to the front |
| Poti | NB | Port at the mouth of the Rioni. Sealift arrives here (E10) |
| Batumi oil terminal | FAC | Blue's fuel. A Red strike target |
| Ozurgeti | FO | Inland flank |
| CVBG (Nimitz class standing in for a 1985 carrier, Ticonderoga, Perry) | CG | Off Batumi. Montreux lets Turkey admit allied warships in wartime |

**Front: Rioni valley and Mingrelia**

| Objective | Kind | Notes |
|---|---|---|
| Senaki-Kolkhi | OAB | First airfield Red counter-attacks or Blue takes |
| Samtredia | LO | Rail junction where the lines to Poti and Batumi split. A crossing (E9) |
| Zugdidi / Enguri bridge | FO | Crossing on the coastal road and railway |
| Enguri dam (Jvari) | FAC | Power station. The book's low-level power-station raid |
| Kutaisi | OAB | Red's front airfield. Blue's first big goal |

**Red depth: Georgia**

| Objective | Kind | Notes |
|---|---|---|
| Surami tunnel / Khashuri | LO | The one rail line between east and west Georgia. The most important interdiction target on the map |
| Gori | LO | Rail and the road to the Roki tunnel |
| Tskhinvali / Roki road | FO | South end of the Roki route over the mountains **[check: tunnel modelled]** |
| Kazbegi (Georgian Military Road) | FO | Second road over the mountains |
| Tbilisi | CC | Transcaucasus Front HQ |
| Tbilisi-Lochini, Vaziani, Soganlug | OAB | Front air army |

**Red depth: the coast and the north**

| Objective | Kind | Notes |
|---|---|---|
| Sukhumi-Babushara, Gudauta | OAB | MiG-23 and MiG-29 regiments covering the coast |
| Ochamchire | FO | Beach for a Marine hook |
| Sochi-Adler | OAB | |
| Tuapse refinery | FAC | Red fuel |
| Novorossiysk | NB | Black Sea Fleet base. Red CG home (Slava class: the real *Slava* was Black Sea Fleet in 1985) |
| Krasnodar refinery | FAC | Red fuel |
| Krymsk, Anapa, Gelendzhik, Krasnodar-Center, Pashkovsky, Maykop | OAB | Red fighter, strike and bomber bases |
| Mozdok, Mineralnye Vody | OAB | Backfire bases |
| Nalchik, Beslan | OAB | |
| Ordzhonikidze | LO | Railhead at the north end of both mountain roads. Red `supply_source` |

About 40 objectives. Blue starts with 6 of them.

### 2.2 Choke points

The map gives the book's "river lines" for free, as mountains:

- **Coastal line:** the railway and road from Tuapse through Sochi and
  Sukhumi to the Enguri bridge.
- **Over the mountains:** the Roki route and the Georgian Military Road.
  They are slow and few, and ground formations must use them.
- **East to west inside Georgia:** only through the Surami pass and its rail
  tunnel.

Cut all three and every Red formation west of Surami runs on what it
carries. Formations already burn fuel and slow to half speed once supply
runs short (DESIGN.md 4.2). With the dynamic war model (ENGINE.md 3) a
starved Red also turns defensive and loses the will to go on.

### 2.3 Balance

Red has far more ground and airfields. That is the book (Soviet numbers
against NATO quality), and it holds in PvP because:

- the war is not won by owning the map. Red's northern bases are depth,
  targets and slots;
- Blue fights from a carrier and two good airfields with better aircraft;
- Red's fuel is short (`supply_source` low on fuel) and its supply lines
  are long.

---

## 3. How the war moves

There are no fixed phases. The shape of the war comes from state that
players and the AI change (ENGINE.md 3):

- **Stocks.** Fuel, weapons and airframes at each base, refilled by
  production and transport, emptied by use and by strikes.
- **Orders of battle.** AI aircraft come from base stock, so losses are
  permanent until replaced. When Red's MiG-29s are gone, the MiG-21s fly.
- **Will to fight**, per side. Losses, lost ground and empty fuel tanks
  lower it; successes raise it.
- **Posture.** Each side's AI goes on the offensive, holds or falls back
  from its own state, not on a timer.
- **Story events** (ENGINE.md 3.4). When a player's action crosses a line
  (Tuapse refinery destroyed, carrier hit, Senaki taken, six Backfires shot
  down), an event fires: the news reports it and names who did it, and its
  effects change stocks, will, slots or what the HQ does next.

What players will typically see, though no run is the same:

1. Red has the numbers early and counter-attacks down the Rioni. Backfires
   go for the carrier as soon as it is in range.
2. If Blue's strikes reach the refineries, the Surami tunnel and the passes,
   Red's fuel falls, its offensive stalls and its posture turns defensive.
3. If Blue then breaks out, Red's will sinks. If Red's raids sink the
   carrier and its attacks reach Kobuleti, Blue's does.

**Ends of the round:**

- A side's will stays under the threshold for N hours: it asks for terms
  and the other side wins (E5).
- Red takes Batumi and Kobuleti: Red wins (an objective-loss rule inside
  E5's will model, or `MapOwned` until then).
- `max_round_days`: draw.

Until E5 exists, `MapOwned` and `max_round_days` end the round.

---

## 4. The book's set pieces on this map

None is scripted. Each is something the map, the stocks and the AI make
likely, and most set off a story event when they happen.

| # | Book | How it can happen here | Today | Needs |
|---|---|---|---|---|
| 1 | Dreamland: stealth strike on A-50s and bridges | F-117s on the Enguri and Rioni bridges and the Surami tunnel; the A-50 orbiting north of the mountains is a target | A-50 station as HQ support, F-117 `Strike` Precision | E9 crossings, E6 for AI F-117s to be a real stock |
| 2 | MiG-23 raid on the E-3s | Red HQ fighter sweep from Sukhumi and Gudauta at the E-3/E-2 over the sea | HQ CAP and sweeps | — |
| 3 | Polar Glory (airborne seizure) | Red HQ drops the 104th Guards Airborne (a real Transcaucasus unit) on a weakly held Blue base | `Paratrooper` actions, players only | E11 |
| 4 | Dance of the Vampires | Red HQ sends Tu-22M3s from Mozdok and Krymsk at the carrier when it is in range | Players or a Red commander order `Strike` AntiShip | E3 so the AI does it when Red is short of players |
| 5 | Nordic Hammer (B-52 on runways) | B-52H on the Mozdok and Krymsk runways | `Strike` Runway | E6 makes it hurt Red's sortie stock |
| 6 | Low-level raid on the power station | Tornado IDS on the Enguri dam | `Strike` Precision | — |
| 7 | MiG-29 trap | Player fight over Gudauta | Yes | — |
| 8 | Blinders on Stornoway | Su-24 and Tu-22M3 on Batumi | HQ strikes | — |
| 9 | Tanker hunt | Il-78 and KC-135 stations | HQ support stations | — |
| 10 | Ferry-flight ambush | Blue reinforcements fly in from the west edge, MiG-25s go after them | Partly | E6 (ferry flights refill stock) |
| 11 | Convoys and ASW | Sealift from the Bosporus edge to Poti, Kilo submarines from Novorossiysk | Cosmetic ships only | E10, E12. DCS: no ASW |
| 12 | *Kirov* surface action | Slava cruiser group sorties from Novorossiysk | Red CG moved by `CarrierWaypoint` | — |
| 13 | Doolittle (Tomahawks on the bomber bases) | Blue HQ fires carrier cruise missiles at Mozdok and Krymsk | **Works now** (`bflib/src/hq/planner.rs:800`) | E6 makes it cost Red bombers |
| 14 | Final strike on the island | The Kutaisi air-defence belt before a breakout | `Sead`, `Fighters`, players | — |
| 15 | Marine landing | Blue HQ or players land Marines at Ochamchire behind Sukhumi | Helicopters from the CG, recon troops as JTAC | E11 |
| 16 | Alfeld breakthrough | Red armour down the Rioni towards Senaki and Poti | `ground_war`, HQ steering | E8 (T-55 vs T-80), E9 |
| 17 | Fuel war | Refineries, Surami, the passes, Red truck convoys | **Supported** | E5 and E7 make it change Red's will and posture |
| 18 | The commander's helicopter | Not as a scripted event. A Red Mi-8 VIP flight could be an HQ op later | — | — |
| 19 | Ceasefire | A side's will breaks (section 3) | `MapOwned`, `max_round_days` | E5 |

---

## 5. PvP with the AI filling in

### 5.1 What players get

- **Both sides always open.** `lock_sides` on, one switch per round, as on
  any Fowl Engine PvP server.
- **A side with few players gets AI help:** the HQ flies its CAP, strikes,
  SEAD, bombers, AWACS, tankers and transports, and the ground AI commands
  its formations.
- **The more players a side has, the more the AI steps back** and leaves the
  war to them.
- **One player online** fights a war their size: their side's AI helps them
  and the other side's AI fights back, matched to them (E4).
- **Nobody online:** no new attacks and no AI packages, so an empty server
  isn't rolled up overnight. Production and logistics carry on.

| Online | What happens |
|---|---|
| 0 | War paused: no new attacks, AI packages grounded |
| 1-3 Blue, 0 Red | Red is run by the AI at a strength matched to Blue's 1-3. Blue's AI supports its pilots. Plays like PvE |
| 10 Blue, 8 Red | Both HQs mostly stand back. PvP |
| 20 Blue, 3 Red | Red's AI fills the gap between 3 and 20. Blue's AI stands back |

### 5.2 What exists today

- **The HQ fades out with its own side's players** (`gap_factor`,
  `bflib/src/hq/mod.rs:361`): full strength up to `full_until_players` (2),
  down to `min_factor` (25%) at `fade_out_players` (16). Each human already
  doing a job cuts the HQ's spending on that job (`per_human_cut`).
- **AI packages** fly for a side with at most `max_side_pilots` humans
  (`modern_war.air_life.packages`, `bflib/src/airlife.rs:397`).
- **The ground AI** commands both sides and pauses new attacks on an empty
  server (`ground_war.ai.pause_when_empty`, `bflib/src/groundwar/ai.rs:295`).
- **Empty-side protection:** captures against a side with nobody in a slot
  take longer (`population_scaling`).

### 5.3 What is missing (E4)

All of the above looks at a side's **own** players only. A side with
nobody in it runs at full strength whether the other side has one pilot or
thirty, so one player is swamped and thirty walk through it.

E4 makes each AI's strength a function of **both** head counts: it fills
the gap between its side and the other, with a floor so a side is never
helpless and a ceiling so the AI never outnumbers the humans it is facing
by too much. One shared rule drives the HQ, the AI packages and the ground
AI's attack rate, and the dashboard shows each side's state ("Red: AI-led,
2 pilots").

**Empty-side protection** also needs a look: a side with no players but a
full AI is not really undefended. With E4 in, `population_scaling` should
count AI strength, or be turned down, so a lone player can still take
ground.

---

## 6. Slots and kit

The era rules from DESIGN.md section 5 apply unchanged: 1985 warehouses, ban
list, no targeting pods. Coalitions: Blue USA, Turkey, UK plus CJTF Blue; Red
USSR plus CJTF Red.

| | Players | AI (fills in, or always AI) |
|---|---|---|
| Blue | F-14A and F/A-18C (restricted) on the carrier; F-15C, F-16C (restricted), F-4E (Turkish and USAF), A-10A at Batumi and Kobuleti; AV-8B if owned; UH-1H, SH-60; C-130J as a C-130H | Same types when filling in, plus E-3A, E-2C, KC-135, S-3B, B-52H, F-117A, Tornado IDS, Ticonderoga, Perry, Tarawa |
| Red | MiG-29A, Su-27, MiG-21bis, Su-25, L-39, Mi-24P, Mi-8 | Same types when filling in, plus MiG-23MLD, MiG-25PD, MiG-31, Su-24M, Su-17M4, MiG-27K, Tu-22M3, A-50, Il-78M, Slava, Krivak, Kilo (static), Scud-B raids |

---

## 7. Build order

1. **Engine first** (ENGINE.md section 4). The minimum before building the
   mission: E1 (done), E3, E4, E5, E6, E7.
2. **Mission:** the objectives in section 2.1 in `base.miz`, a 1985
   `warehouse.miz`, era ban list, templates, and one config
   `RSR_SF_CFG`.
3. **Test** on the test server: one pilot alone, then small PvP, then a full
   server.
4. **Then the depth:** E8, E9, E10, E11.
5. **The dashboard** ([DASHBOARD.md](DASHBOARD.md)): the theme, the news
   voice and Discord branding can be built now; the new pages follow the
   engine items they show.

---

## 8. Checks to run in DCS before building

1. **Map edges.** How much of Turkey and Armenia is on the map? Can Blue's
   reinforcements come from the south edge, or only by sea?
2. **Surami tunnel and the Roki tunnel.** Are they modelled? Can AI ground
   units use them? Can the bridges on the Enguri and Rioni be destroyed, and
   what does AI pathing do when they are down?
3. **Ground routes over the mountains.** Do `ground_war` formations path
   over the Georgian Military Road and the Roki road in sensible time?
4. **Tu-22M3 against the carrier** from Mozdok and Krymsk (DESIGN.md check
   3).
5. **Carrier sea room.** Is there space off Batumi for the carrier to steam
   into the wind without leaving the sea or running into Turkish waters at
   the map edge?
6. **Performance** with about 40 objectives and both HQs at full strength
   on a quiet server.

---

## 9. Decisions for the owner

1. **Framing:** the NATO beachhead (recommended), or a "what if Georgia were
   NATO" layout with a more even start.
2. **Will-to-fight inputs and weights** (ENGINE.md 3.2): what should count
   most (the carrier, airbases, fuel, aircraft losses)?
3. **E4 matching:** how close the AI should keep a lone pilot's fight. For
   example, the AI side flies about one flight per human enemy pilot, never
   fewer than one and never more than the full HQ.
