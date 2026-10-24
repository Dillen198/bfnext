# Reading the Live Ops Dashboard

The **Live Ops dashboard** at **[dashboard.vectorstrike.org](https://dashboard.vectorstrike.org)**
is the campaign's web command post. It updates live from the running server, so
you can plan a sortie before you connect, or run it on a second monitor while
you fly. This page walks through each screen and what to actually look at.

## Logging in

- **Open to everyone, no login:** SITREP, OBJECTIVES, WAR DIARY, RANKINGS,
  PILOTS, KILL FEED, REPLAY, ROUNDS and SQUADRONS (you need to sign in to
  make or join a squadron).
- **Your side only:** TACMAP, SCOPE, BRIEFING, RECON INTEL, THEATRE HQ and
  COMMAND. Log in with Discord and you see them for your registered side
  only: the enemy can't read your briefing or see your map, and you can't see
  theirs. If you're signed in but the dashboard doesn't know your side yet,
  these pages say **NO COALITION**. Link your pilot to fix it.
- **Linking your pilot:** type `/linkme` in Discord to get a token, then
  `-linkme <token>` in DCS chat, once. That tells the dashboard (and the
  Discord bot) which pilot is yours, so your side's pages open and **MY
  PROFILE** tracks your stats.
- **ABOUT** has the credits and the version of the server software.

## SITREP — the front page

The at-a-glance state of the war. Read it top to bottom:

- **KPI strip** — pilots ONLINE, how many AIRBORNE, objective count per side,
  **CRITICAL** (friendly objectives in danger right now), kills this round.
- **TERRITORY CONTROL** — the win condition. A round is won by **share of
  objectives held**, not kills. This bar is the score.
- **CRITICAL ASSETS** — objectives low on health or logistics, or under threat.
  If you only read one panel, read this one: it's your target list (enemy) or
  your defense list (friendly).
- **WEATHER BRIEF** — current winds/cloud/QNH, the same data your kneeboard
  shows.
- **AIR PICTURE / ENGAGEMENT LOG / TOP SHOOTERS** — who's up and what's been
  shot in the last while.

## TACMAP: the tactical map *(your side only)*

Your side's live picture, drawn the same way as the commanders'
[COMMAND map](../advanced/command.md), but read-only: nothing on it gives
orders. It shows only what your side knows.

- **Layers** you can toggle: your own assets (**AIR / GND / SEA / LOG / HOS**),
  zones of control (**TERR**), the supply network (**SUP**, cut routes in red),
  your radar and air-defence coverage (**COV**), and the enemy air defences
  you know of (**THR**). The front line is drawn too.
- **Enemy aircraft and formations** appear only where your side can see them,
  and are remembered where they were last seen.
- **The combat log** runs down the side. **LOAD OLDER** pulls in the rest of
  the round.
- Where the server has no ground war, the map shows bases, your assets and the
  air picture only.

**Click a base** to open its drawer:

- its status: **SECURE**, **THREATENED**, **UNDER ASSAULT**, **ENEMY HELD** or
  **UNCLAIMED**;
- **CONDITION**: health, logistics, supply and fuel, with chips for unlimited
  supply or aircraft, and whether its warehouse has been knocked out. For an
  enemy base, how old your recon on it is, or "No recon on file";
- **GROUND**: its garrison and spare troops, its supply line, and any battles
  and formations based there, defending, attacking or falling back;
- **HISTORY**: who took it and when;
- for your own bases, the full **WAREHOUSE**: every line of airframes,
  vehicles, weapons, fortifications and fuel, most depleted first, with a
  count of lines under 20%. If the game server is busy the drawer says so and
  asks again every minute.

**Click an enemy contact** for what's known about it: its kind, whether it's
**IN SIGHT** or when it was **LAST SEEN**, roughly how many vehicles, which way
it was moving, and whether it has been seen up close.

Keys: `?` help, `F` follow yourself, Space centre, Tab / Shift-Tab cycle
through, Esc clear.

Use it to pick an ingress route that threads between threat rings, and to find
the seam in the enemy line.

### SCOPE

The **SCOPE** button on the TACMAP's top bar opens the radar scope: raw radar
contacts with BRAA from you and from bullseye. It's for your side only too.
Commanders can take the **GCI seat** from here and speak to pilots on the GCI
frequency: see [Live GCI](../gameplay/gci.md#the-human-gci-seat).

## OBJECTIVES — the sortable list

Every objective as a table you can sort and filter.

- Columns: owner, type (airbase / FARP / FOB / logistics hub / factory / naval
  base / SAM site / command center), **health**, **logi**, **supply**, **fuel**,
  threat state. Hover a column header for what it means.
- **Critical Objectives** card at the top is the short list of what's about to
  fall — for either side.
- Sort by health ascending to find the enemy base closest to capturable; sort
  your own the same way to find what to reinforce.
- See [Objectives](../gameplay/objectives.md) and
  [Capturing Objectives](../gameplay/capturing-objectives.md) for what the
  numbers gate.

## BRIEFING — your kneeboard, on the web *(coalition-locked)*

Everything you'd want written down before you start engines:

- **Navaids** — TACAN / ILS / ICLS / NDB channels for friendly fields and
  carriers.
- **Radios & Support** — AWACS, tanker, and JTAC frequencies; the GCI
  frequency for [Live GCI](../gameplay/gci.md) voice calls.
- **Artillery** and **Deployables** currently in the field and callable.
- **RWR Threats / HARM Codes** — the threat emitters known to be up, with codes
  for your HARM/ARM loadout.

## RECON INTEL — the photo map *(coalition-locked)*

Player-gathered reconnaissance: F-14 TARPS photo runs and recon-aircraft passes,
uploaded and marked up on a shared coalition map. If you're flying strike, check
here for hand-annotated target folders before you plan. See
[Recon Intel Map (TARPS)](../advanced/recon-intel-map.md).

## THEATRE HQ and COMMAND *(your side only)*

- **THEATRE HQ**: what your side's AI commander intends, and the operations
  it is running. See [Theatre HQ](../advanced/theatre-hq.md).
- **COMMAND**: the commanders' map, where every asset your side has in the
  field can be given orders. Everyone on your side can look; only commanders
  can order. See [Command](../advanced/command.md).

## WAR DIARY: the daily front page

A newspaper front page for each day of the campaign: captures, where the
fighting was, losses, and the ace of the day. Both sides read the same paper.
See [The War Diary](../gameplay/war-diary.md).

## RANKINGS / PILOTS / KILL FEED / REPLAY: the stats

- **RANKINGS**: sortable leaderboard (air kills, ground kills, K/D, logistics
  score, capture participation, overall score).
- **PILOTS**: every pilot with a profile; your own page has your level and
  XP, per-role life history and a flight log with a **WATCH** link for each
  recorded sortie.
- **KILL FEED**: the live running log of every kill, with shooter, victim,
  weapon, and whether it was air, ground, or friendly fire.
- **REPLAY**: every recorded sortie, replayable in 2D or 3D in the browser.
  See [Debriefing With Flight Replay](./flight-replay.md).

## ROUNDS and SQUADRONS

- **ROUNDS** (Round History): every round this server has played, who won and
  how long it ran. Open one for its MVP and top pilots.
- **SQUADRONS**: make or join a squadron, and see the squadron leaderboard.
  See [XP, Levels & Squadrons](../gameplay/progression.md#squadrons).

## A pre-sortie routine

1. **SITREP** → is my side winning or losing on territory? Any CRITICAL
   friendly asset?
2. **OBJECTIVES** → sort enemy by health; pick the softest target that matters.
3. **TACMAP** → plan a route around the threat rings between me and it.
4. **BRIEFING** → copy tanker/AWACS/JTAC freqs and HARM codes.
5. Connect, slot, fly it.

## See also

- [Your First Sortie](./first-sortie.md)
- [Reconnaissance](../f10-menu/recon.md)
- [Early Warning Radar](../f10-menu/ewr.md)
- [Debriefing With Flight Replay](./flight-replay.md)
- [The War Diary](../gameplay/war-diary.md)
