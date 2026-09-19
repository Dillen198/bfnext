# Reading the Live Ops Dashboard

The **Live Ops dashboard** at **[dashboard.vectorstrike.org](https://dashboard.vectorstrike.org)**
is the campaign's web command post. It updates live from the running server, so
you can plan a sortie before you connect, or run it on a second monitor while
you fly. This page walks through each screen and what to actually look at.

## Logging in

- Most pages (SITREP, TACMAP, OBJECTIVES, RANKINGS, PILOTS, KILL FEED) are
  **open** — no login needed.
- **BRIEFING** and **RECON INTEL** are **coalition-locked**. Log in with Discord
  and you'll see them for your registered side only — the enemy can't read your
  briefing and you can't read theirs.
- Logging in also links the dashboard to your pilot (use `-bind` in-game once,
  from the token on your PILOTS profile) so **MY PROFILE** tracks your stats.

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

## TACMAP — the tactical map

A full-fidelity map of the theater with NATO symbology. This is the same
picture the F10 map gives you in the cockpit, minus the fog of war rules that
apply in-game.

- **Objective rings** colored by owner (BLUFOR / REDFOR / neutral). A **white
  inner ring** means that objective is **capturable right now**.
- Click an objective for its health, logistics, supply, fuel, garrison, and
  supply-line links.
- Known enemy units (from EWR, JTAC eyes-on, and recon passes) plot here with
  threat rings on SAMs.
- Use it to pick an ingress route that threads between threat rings, and to
  find the seam in the enemy line.

## OBJECTIVES — the sortable list

Everything on TACMAP as a table you can sort and filter.

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

## RANKINGS / PILOTS / KILL FEED — the stats

- **RANKINGS** — sortable leaderboard: air kills, ground kills, K/D, logistics
  score, capture participation, overall score.
- **PILOTS** — every pilot with a profile; your own page has your `-bind`
  token and per-role life history.
- **KILL FEED** — the live running log of every kill, with shooter, victim,
  weapon, and whether it was air, ground, or friendly fire.

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
