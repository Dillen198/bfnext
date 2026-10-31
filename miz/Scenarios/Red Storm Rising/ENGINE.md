# Red Storm Rising: engine readiness

What the engine needs before the Southern Flank campaign
([CAUCASUS.md](CAUCASUS.md)) is built, under one rule from the owner:

> **The story is triggered by what players do.** Story events fire, but
> never on a clock or in a fixed order: each one is set off by a player's
> (or the AI's) action, and each one changes the war.
>
> **One PvP server. When few players are on, the AI fills in and it plays
> like PvE.**

This replaces DESIGN.md item 1 (timed `{when, then}` scenario phases). There
are no fixed phases. File references are to this repository as of October
2026.

## 1. What is already dynamic

A lot of it:

- **Logistics.** Fuel, weapons and airframes live in each base's warehouse.
  Convoys, cargo flights, ships and trains move them, and each can be
  destroyed on the way.
- **Production follows the map.** Output scales with the airbases,
  logistics hubs, factories and command centres a side still holds, and with
  their logistics health (`ProductionScalingConfig`,
  `bfprotocols/src/cfg/logistics.rs:270`). Bomb a refinery and fuel output
  drops.
- **The ground war runs on supply.** Formations burn fuel and ammunition;
  cut off, they slow to half speed and lose firepower.
- **The HQ reads the war.** Its posture comes from territory, threatened
  bases and openings (`bflib/src/hq/strategy.rs:105`). It trusts kinds of
  operation that work and avoids ones that keep failing (`Record::trust`,
  `bflib/src/hq/mod.rs:112`). It plans only from what its radars and intel
  see.

## 2. What is not, yet

| Gap | Why it breaks "every action changes the war" |
|---|---|
| **AI aircraft are free and endless.** An AI flight costs treasury points; it never draws an airframe from a base, and a shot-down MiG-29 regiment comes back at the next pass | Air losses don't matter. The book's quality curve (Red's best jets burn out, its reserves are older) can't happen |
| **Rounds end only by map share or by time** (`bflib/src/db/objective.rs:2589`) | The book ends when one side can't go on: fuel gone, fleet gone, losses too high. Nothing measures that |
| **Posture ignores the side's own state.** A side with no fuel and half its air force gone still goes on the offensive if the map looks good | The fuel war doesn't change what the enemy *does*, only how fast its trucks move |
| **The AI never attacks carrier groups.** The HQ values a CG at 0 (`bflib/src/hq/picture.rs:84`) and has no anti-ship operation | No Backfire raids unless a Red player orders them, so none while Red is short of players |
| **Carrier auto-repair is free** (`bflib/src/db/objective.rs:4098`) | Hitting the carrier costs Blue nothing |
| **Combat strength is per role, not per type** (`bflib/src/db/combat.rs:61`) | A T-55 counts the same as a T-80. Losing the good units doesn't hurt more |
| **The AI only counts its own side's players.** A side with nobody in it runs its HQ at full strength (`gap_factor`, `bflib/src/hq/mod.rs:361`) whether the other side has one pilot or thirty | A lone pilot is swamped; a full server walks through an empty side (CAUCASUS.md 5.3) |

## 3. The model

Three pieces, each fed by everything that happens and each read by the AI
and the players.

### 3.1 Real orders of battle

AI aircraft come from the same warehouse airframe stock players use.

- An AI flight takes its airframes from the base it launches from. A base
  without that type can't launch it.
- Shot down: gone. Lands: back in stock. Ejected: the airframe is gone and,
  with CSAR, the pilot can be rescued.
- Replacements come the way everything else does: production, supply
  transfers, ferry flights.
- The HQ chooses among templates by what its bases hold. When the MiG-29s are
  gone it sends MiG-21s; when the Tu-22M3s are gone there are no more
  Backfire raids.
- A runway strike or a strike on parked aircraft now matters to the AI too.

Ground formations already cost treasury and supply. With per-type strength
(item 4) losing the good units hurts more.

### 3.2 War will

Each side has a **will to fight**, 0-100, saved with the campaign.

- **Moved by what happens**, weighted by value:
  - losses: airframes by type, ships, the carrier hit or sunk, formations
    destroyed, pilots lost;
  - ground: objectives taken and lost, the command centre above all;
  - stocks: days of fuel left in the side's network (refineries, the supply
    source, the dumps), supply lines cut;
  - success: enemy losses and captures raise it.
- **Recovers slowly** while nothing bad happens.
- **Read by the AI:** low will pushes the posture defensive and makes the HQ
  avoid costly kinds of operation; high will lets it take risks.
- **Read by players:** shown on the dashboard and the F10 map, with the
  biggest recent causes ("Tuapse refinery burning: -6").
- **Ends the round:** a side below the threshold for N hours asks for terms.
  The other side wins. This is the book's ending, reached by play, not
  scripted. It sits next to the existing `MapOwned` and `max_round_days`
  ends.

All weights and the threshold are config, so a campaign decides how much a
carrier is worth against an airbase.

### 3.3 Posture from the side's own state

The HQ's posture score (`strategy.rs:105`) gets three new inputs: will, days
of fuel, and air strength against the enemy's (from its radar picture).

- Short of fuel: defensive, whatever the map says. That is the book's
  Soviet offensive stalling.
- Air force depleted: fewer fighter sweeps over enemy air defences.
- Will high and stocks full: offensive.

Posture changes are already announced to the side, so players see the war
turn.

### 3.4 Story events, triggered by players

The story is a set of **story events** written for the campaign. Each one
waits for something players or the AI do, then fires once, tells everyone
what happened, and changes the war. Which events fire, in what order and
for which side depends entirely on play, so no two rounds tell the same
story.

**An event has three parts:**

- **Trigger: an action, or the state actions leave.** Never a time of day.
  The engine already records every action as a stat (`bfprotocols/src/stats.rs`):
  kills with unit type and killer, captures, buildings destroyed, objective
  health, convoys and supply flights destroyed or delivered, actions
  ordered. Triggers read those and the campaign state:
  - an objective destroyed, captured or held for a time ("Tuapse refinery
    burning", "Blue holds Senaki for 2 h");
  - kills of a type, in a zone, in a window ("6 Tu-22M3 shot down within
    an hour");
  - a unit hit ("the carrier below 75%");
  - a stock or a rate ("Red fuel under 30%", "3 Red convoys destroyed on
    the coast road today");
  - another event having fired, or not having fired (chains and branches);
  - will (3.2) crossing a line.
- **Effects: they change the war.** Any mix of:
  - will up or down for either side;
  - production, stock or supply-source changes ("refinery output -40%");
  - unlock, lock or reprice an action or a slot ("Turkish F-4Es arrive at
    Senaki", "B-52 raids authorised");
  - send reinforcements: an AI formation, a squadron's airframes, a ship
    group;
  - point the HQ: a new main effort, a target it must defend, an operation
    it now plans;
  - reveal intel to a side;
  - a points bonus to the players who caused it.
- **Story: what players are told.** A news story and a briefing line, naming
  the pilots or units that caused it, plus in-game messages to each side.

**Rules that keep it dynamic:**

- Every event is caused by someone. The news credits them.
- Events fire once per round unless marked repeatable (with a cooldown).
  Fired events are saved, so a restart doesn't refire them.
- Branches: two events can exclude each other, so whichever side gets there
  first decides which way the story goes.
- An event only changes numbers the engine already models (will, stocks,
  production, slots, HQ priorities). It never spawns a scripted battle.

**Examples for the Southern Flank** (written in the campaign config, not in
code):

| Event | Trigger (a player action) | Effect on the war |
|---|---|---|
| Tuapse burns | Tuapse refinery destroyed | Red fuel production -40%, Red will -6, Red HQ adds CAP over the other refineries |
| The carrier is hit | Blue CG under 75% after a Red strike | Blue will -10, the carrier pulls west, the strike's pilots get a bonus |
| Backfire regiment broken | 6 Tu-22M3 shot down within an hour | Red loses its anti-ship strike until the regiment is rebuilt |
| Surami tunnel cut | Surami objective destroyed | Red formations west of it lose supply from the east |
| Senaki falls | Blue captures Senaki | Turkish F-4E slots unlock there, Red will -5 |
| Beachhead in danger | Red ground within reach of Kobuleti | Red airborne assault (E11) becomes available to the Red HQ and players |
| Front commander lost | The Red VIP Mi-8 shot down | Red HQ plans half as often for two hours |
| Fuel crisis | Red fuel under 30% (a result of strikes) | Red HQ turns defensive; news "rationing at the front" |
| Moscow asks for terms | Red will under the threshold for N hours | Ceasefire, Blue wins |

### 3.5 What happens to the book's set pieces

None of them is scripted. Each becomes something that can happen, and most
set off a story event when it does:

| Set piece | Why it happens |
|---|---|
| Backfire raids on the carrier | The HQ sees the carrier in range and has Tu-22M3s in stock (E3) |
| Stealth strike on bridges | Crossings are high-value targets to the HQ (item 3), and F-117s are in stock |
| Tomahawks on the bomber bases | The HQ already plans carrier cruise-missile strikes on airbases in range (`bflib/src/hq/planner.rs:800`) |
| Airborne seizure, Marine landing | The HQ plans one when a lightly held objective is in reach and it has transports and troops (item 7) |
| The fuel war and the ceasefire | Fuel and losses lower will; low will ends the round |
| The MiG-29 trap, tanker hunts, convoy fights | Players and AI meeting over targets that matter |

## 4. Work list

Sizes: **S** = a day or two, **M** = about a week, **L** = several weeks.

| # | Work | Size | Status |
|---|---|---|---|
| E1 | Carrier auto-repair charges what it says | S | **Done** (`bflib/src/db/objective.rs`, `check_carrier_auto_repair`), not yet tested in DCS |
| E3 | HQ plans anti-ship strikes on carrier groups | S-M | |
| E4 | AI fills the gap between the two sides' head counts: one rule for the HQ, AI packages and the ground AI's attack rate; empty-side protection counts AI strength (CAUCASUS.md 5.3) | S-M | |
| E5 | War will (3.2), with its end of round | M | Needs owner OK on the inputs |
| E5b | Story events (3.4): triggers on actions and state, effects on the war, news that credits the players | M-L | After E5 |
| E6 | Real orders of battle for AI aircraft (3.1) | M-L | Design above |
| E7 | Posture from will, fuel and air strength (3.3) | S | After E5 |
| E8 | Per-type combat strength (item 4) | S-M | |
| E9 | Crossings: bridges and tunnels as objectives (item 3) | M | |
| E10 | Sealift convoys and more than one carrier group (item 2) | M + M | |
| E11 | HQ-planned airborne and amphibious assaults (item 7) | M | |
| E12 | Abstract submarine and ASW layer (item 8) | L | Optional |

E2 (closing a side to players) was dropped: the campaign stays PvP and the
AI fills in instead (E4).

**Order:** E3, E4, E5, E5b, E6, E7 before the mission is built. E8-E12
after. The Red Storm dashboard (CAUCASUS.md section 10) can be built
alongside, since it only needs E5's will and E5b's events to show.
