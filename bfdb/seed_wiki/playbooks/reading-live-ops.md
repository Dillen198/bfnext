# Reading the Live Ops Dashboard

The **Live Ops dashboard** at **[dashboard.vectorstrike.org](https://dashboard.vectorstrike.org)**
is the campaign's web command post. It updates live from the running server, so
you can plan a sortie before you connect, or keep it on a second monitor while
you fly. This page walks through every screen in the sidebar and what to look
at on each.

## A pre-sortie routine

```steps
[
 {"title":"1. SITREP: who is winning?","text":"Open the dashboard. The **TERRITORY CONTROL** bar is the score: a round is won on ground held, not kills. Then read **FRONT LINE**: every base that is ready to capture, under attack, low on defences, freshly taken or running dry, most urgent first. Click one to open its drawer."},
 {"title":"2. OBJECTIVES: pick the target","text":"Sort the table by **Health**, lowest first. The softest enemy base that matters is your target; the softest of your own is the one to defend or resupply. Click a row and read its capture report: *Why can't this be captured?* lists what is still standing."},
 {"title":"3. TACMAP: plan the route","text":"Sign in. On the TACMAP turn on **THR** (enemy air defences you know of) and **COV** (your own radar and SAM cover). Thread a route between the threat rings and find the seam in the enemy line."},
 {"title":"4. BRIEFING: write it down","text":"The **Situation** tab ranks your side's tasks on a map. The **Kneeboard** tab has the navaids, radios, artillery, deployables and HARM codes. **KNEEBOARD PDF** prints all of it. Planning a route? The **Planner** tab builds it and loads it into your jet."},
 {"title":"5. Fly it, then debrief","text":"Connect, slot, fly. After the mission, your **Flight Log** on MY PROFILE has the sortie, its full debrief and a **WATCH** link to replay it in the browser."}
]
```

In short: SITREP for the score and the front line, OBJECTIVES for the target,
TACMAP for the route, BRIEFING for the frequencies, then fly.

## Signing in and your side

The sidebar has three groups. The first is open to everyone; the second is
locked to your coalition; the third is the stats.

| Group | Pages | Who can open them |
| --- | --- | --- |
| Operational picture | **SITREP**, **TACMAP**, **OBJECTIVES**, **WAR DIARY** | Everyone, except TACMAP, which needs you signed in with a side |
| Your coalition | **BRIEFING**, **RECON INTEL**, **THEATRE HQ**, **COMMAND** | Signed in, and on a side this campaign |
| Stats and people | **RANKINGS**, **PILOTS**, **KILL FEED**, **REPLAY**, **ROUNDS**, **SQUADRONS**, **MY PROFILE** | Everyone (you sign in to make or join a squadron) |

- **Login** (top right) signs you in with Discord.
- The coalition pages show a lock until the dashboard knows your side. Signed
  out, the lock reads `Sign in with Discord to view`. Signed in but with no
  side, it reads `Locked to your coalition - link your Discord in game with
  -linkme`, and the page itself says **NO COALITION**.
- **Linking your pilot**: type `/linkme` in Discord to get a token, then
  `-linkme <token>` in DCS chat, once, and take a slot on the server this
  campaign. That tells the dashboard (and the Discord bot) which pilot is
  yours, so your side's pages open and **MY PROFILE** shows your stats. Until
  then MY PROFILE says `No DCS account linked - run -linkme in game chat`.
- You only ever see **your own side's** coalition pages: the enemy can't read
  your briefing or your map, and you can't read theirs.
- **ABOUT** has the credits and the version of the server software.

## The top bar

- **LIVE**, **NO ENGINE** (the round is open but the DCS server is not
  answering) or **OFFLINE** under the campaign name.
- The UTC clock, how long the mission has been running, and a **Restart**
  countdown when a restart is scheduled (it turns amber in the last 30
  minutes).
- Pilots online per side.
- The **campaign selector**: the live round, or any past round. Pick a past
  round and SITREP, OBJECTIVES and the kill feed show that round.
  Where one dashboard fronts several servers, the selector also picks the
  server.
- The **bell**: round results, server notices and maintenance. The badge counts
  what is new since you last opened it. Only an outage still shows a banner
  across the page.
- The light/dark theme switch.

## SITREP: the front page

Read it top to bottom.

**The stat strip:**

| Cell | What it counts |
| --- | --- |
| **ONLINE** | Pilots connected, per side |
| **AIRBORNE** | How many of them are in the air |
| **BLUE OBJ / RED OBJ** | Objectives each side holds, and its share of the territory (the side names are the server's own) |
| **AT RISK** | Bases ready to capture, under attack, or with defences under 40% |
| **KILLS · 1H** | Kills in the last hour, per side |
| **TOTAL PILOTS** | Everyone registered this campaign, per side |

**Left column:**

- **TACTICAL OVERVIEW**: a small map of every base by owner, with the front
  line. The **TACMAP** button opens the full map.
- **WAR NEWS**: the latest daily dispatches from the
  [War Diary](../gameplay/war-diary.md), with a **TODAY** tag while the day is
  still being written.

**Middle column:**

- **WEATHER BRIEF**: wind, temperature, QNH and cloud from the server's METAR,
  when the server reports one. See [Weather, ATIS & Time](../gameplay/weather.md).
- **TERRITORY CONTROL**: the win condition as a bar, with a **THIS ROUND**
  chart of how many bases each side has held over the round.
- **FRONT LINE**: every base that needs a look, most urgent first. Each row
  carries up to two tags:

| Tag | Means |
| --- | --- |
| `Ready to capture` | It is capturable right now |
| `Under attack` | Enemies are near it |
| `Defences 31%` | Health is under 40% |
| `Taken by Blue 12m ago` | It changed hands recently |
| `Supply 18%` / `Fuel 22%` | Its stock is under 25% |

  A quiet war reads `QUIET FRONT — NOTHING UNDER ATTACK OR FAILING`. **Click a
  row** to open that base's drawer (below).

**Right column:**

- **AIR PICTURE**: who is airborne, per side, and in what. Pilots still on the
  ground are listed under **ON GROUND**; click one for their profile.
- **TOP SHOOTERS**: the five pilots with the most kills.
- **ENGAGEMENT LOG**: the latest kills, with the shooter, airframe, victim and
  weapon.

## The base drawer

Clicking a base on SITREP's FRONT LINE, or on the OBJECTIVES page (its map, its
**Needs Attention** list or a table row), opens a drawer on the right.

- **HP / LOGI / SUP / FUEL**: health, logistics, supply and fuel. A dash means
  your side doesn't know (an enemy base you have no recon on).
- **The capture report**: why the base can't be captured yet and what to do
  about it, or what the enemy still needs to take it if it is yours. It lists
  every blocker with the fix, what is still standing (with grid references and
  a mini-map: **Show on map** / **Hide map**), and which of your squads are near
  the zone. The headings, blockers and messages are all listed in
  [Capturing Objectives: the dashboard capture report](../gameplay/capturing-objectives.md#the-dashboard-capture-report).
- `Warehouse knocked out — this base cannot store or issue materiel.` when its
  warehouse is destroyed.
- For an **enemy** base: either `Enemy objective — showing what recon has seen.
  Last observed 14m ago.` or `Enemy objective — no recon on file. Fly a
  reconnaissance sortie to reveal its state.` See
  [Reconnaissance](../f10-menu/recon.md).
- For **your own** base: its **Supply line** (the hub that supplies it, the
  bases it feeds, and any cut link), and its **Stock**: every line of
  airframes, vehicles, weapons and liquids, the most depleted first, with a
  count of lines under 20%.

A base's condition and stock are your coalition's intel. Signed out, the
drawer says `Sign in to see them`; signed in without a side, `Register a side
on the server this round to see them`.

## TACMAP: the tactical map *(signed in, your side)*

Your side's live picture, drawn the same way as the commanders'
[COMMAND map](../advanced/command.md) but read-only: nothing on it gives
orders. It shows only what your side knows.

**Layers** along the top bar (hover each for what it is):

| Button | Shows |
| --- | --- |
| **AIR** | Your side's AI flights |
| **GND** | Deployed units, troops, batteries |
| **SEA** | Carrier groups |
| **LOG** | Supply convoys |
| **HOS** | Enemy aircraft your side can see |
| **SUP** | Your supply network, hub to base, red where it is cut |
| **COV** | Your air defences and radar coverage |
| **THR** | Enemy air defences you know of (faded when old or unsure) |
| **TERR** | The front line and the territory wash |

- **Enemy aircraft and formations** appear only where your side can see them,
  and are remembered where they were last seen.
- The **COMBAT LOG** runs down the side; **LOAD OLDER** pulls in the rest of
  the round.
- **SCOPE** opens the radar scope (below).
- Where the server has no ground war, the map shows bases, your assets and the
  air picture only.

**Click a base** for its drawer:

- its status: **SECURE**, **THREATENED**, **UNDER ASSAULT** (yours, being
  captured), **OUR TROOPS ARE ASSAULTING IT**, **CAPTURABLE: DEFENCES DOWN**,
  **ENEMY HELD** or **UNCLAIMED** (neutral);
- **CONDITION**: HP, LOG, SUP and FUEL, chips for **UNLIMITED SUPPLY** or
  **UNLIMITED AIRCRAFT**, and a note if its warehouse is knocked out. For an
  enemy base, how old your recon on it is, or `No recon on file. A
  reconnaissance sortie reveals its state.`;
- the same **capture report** as above;
- for your own bases, **GROUND**: its garrison and spare troops, its supply
  line (links marked **CUT**), the formations **BASED HERE**, **DEFENDING**,
  **ATTACKING** or **FALLING BACK**, your assets there, and hostile contacts
  near it;
- **HISTORY**: **WE TOOK IT** / **THEY TOOK IT**, when, and by whom;
- for your own bases, the full **WAREHOUSE**: airframes, vehicles, weapons,
  fortifications and liquids, most depleted first, with **N LINES UNDER 20%**.
  If the game server is busy the drawer says so and asks again every minute.

**Click an enemy contact** for what's known about it: **IN SIGHT** or **LAST
SEEN 4m AGO**, its **SIZE** (about how many vehicles), whether it is
**MOVING** and on what heading, and **SEEN UP CLOSE** with the vehicle types
when your side has had a close look.

Keys: `?` help, `F` follow yourself, Space centre, Tab / Shift-Tab cycle
through, Esc clear.

### SCOPE

The **SCOPE** button opens the radar scope: your side's raw radar contacts
with BRAA, a bullseye, and cursor and object positions in DDM, DMS and MGRS.
It is fog-of-war true like the TACMAP. Commanders can take the **GCI SEAT**
here and speak to pilots on the GCI frequency: see
[Live GCI](../gameplay/gci.md#the-human-gci-seat).

## OBJECTIVES: every base

- **A map** of every base; click one for its drawer.
- **Needs Attention**: the same list as SITREP's FRONT LINE.
- **Holdings by Type**: how many airbases, FARPs, FOBs, factories, logistics
  hubs, naval bases, carrier groups and command centers each side holds. Click
  a type to filter the table to it.
- **Capture Timeline**: captures over the round.
- **The table**, filtered **All / Red / Blue / Neutral** and searchable, sorted
  by clicking a header. Hover a header for what it means:

| Column | What it is |
| --- | --- |
| Name, Type, Owner | |
| **Health** | Share of the base's defending units still alive |
| **Logistics** | Its logistics: the logistics defenders and buildings. Governs repair speed, not a stock level |
| **Supply** | How full its warehouse's equipment stock is |
| **Fuel** | How full its fuel stock is |
| **Navaids** | Its generated TACAN, NDB, ICLS, Link 4, ACLS and carrier BRC |
| **Last Taken** | The latest capture this round: by which side and who flew it |
| **Last Change** | When it last changed hands |

Sort by Health ascending to find the enemy base closest to capturable, and your
own the same way to find what to reinforce. See
[Objectives](../gameplay/objectives.md) and
[Capturing Objectives](../gameplay/capturing-objectives.md) for what the
numbers gate.

## WAR DIARY

A newspaper front page for each day of the campaign: captures, where the
fighting was, the air war, the ace of the day and the ledger of losses. Both
sides read the same paper. See [The War Diary](../gameplay/war-diary.md).

## BRIEFING *(your side)*

Three tabs, a side badge, a refresh button and **KNEEBOARD PDF**:

- **Situation**: the [auto-generated briefing](../gameplay/briefing.md) on a
  live map: tasking numbered against map pins, hotspots and known threats. The
  tab shows a red count when there are critical tasks.
- **Kneeboard**: **Navaids**, **Radios & Support** (AWACS, tankers, JTACs and
  the GCI frequency), **Artillery**, **Deployables** and **RWR Threats / HARM
  Codes**, plus a **GCI / AWACS Voice** panel on how to talk to GCI.
- **Planner**: plan a route, share it with your side and load it into your jet.
  See [Mission Planner](./mission-planner.md).
- **KNEEBOARD PDF** exports the situation, tasking, comms card and hotspots,
  then the kneeboard reference pages.

## RECON INTEL, THEATRE HQ and COMMAND *(your side)*

- **RECON INTEL**: your side's photo map, built from F-14 TARPS runs, with
  shared markup that also appears on the F10 map. See
  [Recon Intel Map (TARPS)](../advanced/recon-intel-map.md).
- **THEATRE HQ**: what your side's AI commander intends, the operations it is
  running, and support requests. See [Theatre HQ](../advanced/theatre-hq.md).
- **COMMAND**: the commanders' map, where every asset your side has in the
  field can be given orders. Everyone on your side can look; only commanders
  can order, and the page tells you how far you are from command. See
  [Command](../advanced/command.md).

## RANKINGS

- Four views: **Overall**, **A/A**, **A/G** and **Logistics**, each with a
  podium; plus **Squadrons** and **XP** leaderboards.
- **All time** or this round, and either side; **Search pilot…**.
- Columns: **A/A**, **A/G**, **Logi** (repairs, supply transfers, troops, FARPs
  and deployments), **K/D**, **Deaths**, **Cap** (captures), **Hours**,
  **Score**, **Rep** (repairs), **Sup** (supply transfers), **Dep**
  (deployments) and **Pts** (points donated). Hover a header for its meaning.
- Under the table: **Airframe usage** (what is actually being flown) and
  **Points economy** per side.

## PILOTS and MY PROFILE

Search for any pilot (**Search pilots by name…**). A profile has:

- **Career Stats**: A/A and A/G victories, V/L and S/L ratios, flight time,
  current and best streak, best A/A and A/G in a round;
- **Contributions**: captures, repairs, supply runs, troops, FARPs, deploys,
  actions, donated points;
- **Progression**: level, XP, today's and this week's challenges, and your
  pilot card (see [XP, Levels & Squadrons](../gameplay/progression.md));
- **Most Flown Airframe**, **Theater Breakdown**, charts (how sorties ended,
  activity, hours per week, kills over time, airframes) over 12 weeks, 26
  weeks, a year or all time, and **Rivals** (your nemesis, favourite targets,
  best sortie, and **Compare** with another pilot);
- the **Flight Log**: every sortie with its date, aircraft, duration, kills and
  outcome. Click a row for the sortie's kills, weapons, events and timeline;
  **FULL DEBRIEF →** opens a page with the flight profile, ground track and
  shot log from the Tacview recording; **WATCH** (in the **Replay** column)
  opens the replay on your aircraft.

## KILL FEED

Every kill this round: **Time (Z)**, **Category**, **Killer**, **Airframe**,
**Weapon**, **Target Unit**, **Victim**. One filter row covers the page: a
target category (**All targets**, or aircraft, helicopters, naval, armour, air
defence, artillery, infantry, vehicles, radar, structures) and a search box
(**Pilot, weapon, unit…**). Above the **Event Log**: activity over time, what
each side destroyed, and the top killers and weapons. A kill with no shooter
(a crash) shows as **ENV**.

## REPLAY

Every recorded sortie, replayed in 3D or on the map in the browser, both sides
visible. See [Debriefing With Flight Replay](./flight-replay.md).

## ROUNDS

Every round this server has played, who won and how long it ran. Open one for
its report: **Territory over time**, **Kill exchange**, **Most contested**
bases, and **Who carried it** (the **MVP** and a table of the top pilots).

## SQUADRONS

Make a squadron (a **TAG** of 2 to 5 letters or digits, and a name, then
**Create**), or **Join** one. Your own shows **You fly for** with a **Leave**
button; its owner can **Remove** members. See
[XP, Levels & Squadrons](../gameplay/progression.md#squadrons).

## See also

- [Your First Sortie](./first-sortie.md)
- [Mission Planner](./mission-planner.md)
- [Debriefing With Flight Replay](./flight-replay.md)
- [Capturing Objectives](../gameplay/capturing-objectives.md)
- [Reconnaissance](../f10-menu/recon.md)
- [Early Warning Radar](../f10-menu/ewr.md)
- [The War Diary](../gameplay/war-diary.md)
