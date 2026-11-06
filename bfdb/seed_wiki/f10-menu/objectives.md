# Objectives Menu

`F10 → Objectives` is the base-by-base picture: who owns what, what is falling,
what you can take, and, through the **Capture Advisor**, why the base you are
circling will not flip.

Everything here is free, read-only and available from every slot. Each report
is a panel message on your screen; detail cards stay up for 45 seconds, lists
for 30.

## The menu

```f10menu
{"root":"F10 Other","items":[
 {"label":"Objectives","items":[
  {"label":"Nearest Base (detail)","says":"========= Abu al-Duhur =========\nAirbase - owned by BLUE\nLL:   N35°43.94' E037°06.23'\nMGRS: 37SBV 23456 54321\nFrom you: 067° / 18.4 nm\nZone radius: 2500 m\n----------------------------------\nHealth  78%   Logi  75%\nMunitions  88%   Fuel  92%   Aircraft  40%\nInfantry defenders: 100% alive\nRepair: active -- next pulse in ~7m\nCapture: not eligible -- need health <=20% (now 78%) and every infantry defender dead (now 100% of them alive)\nNavaids:\nTACAN 37X ADH","note":"The closest Blue or Red base your side can see. Neutral bases are skipped."},
  {"label":"Capturable / Contested","says":"=== Capturable / Contested ===\nKuweires [AB] RED 021°/41nm HP:15% -- capturable\n    eligible -- move your capture troops into the zone\nTaftanaz [FARP] NEU 352°/27nm HP:0% -- capturable\n    eligible -- no capture troops near the zone\nAbu al-Duhur [AB] BLU 067°/18nm HP:12% -- BEING TAKEN\n    being taken by Red -- held 40s (~180s needed)","note":"Both directions: enemy and neutral bases you can take, and your own that are exposed. Nearest first."},
  {"label":"Capture Advisor: Nearest","says":"===== CAPTURE ADVISOR: Kuweires =====\nAB - owned by RED\nLL:   N36°11.22' E037°34.80'\nMGRS: 37SCA 12345 06789\nFrom you: 021° / 41.2 nm\nZone radius: 2000 m\nHealth 34%   Logi 100%   Supply medium\n----------------------------------\nSTATUS: objective NOT eligible yet --\n  - objective health must be <=20% (now 34%)\n  - clear the infantry defenders\nREPAIR: WILL self-repair -- next pulse ~12m; act fast or it heals back above 20%\n----------------------------------\nYOUR TROOPS NEAR THIS BASE:\n  - Standard: 1340 m outside the zone edge\n  - Igla: in the zone  <-- CANNOT capture (troop type)\n----------------------------------\nBOTTOM LINE: objective health must be <=20% (now 34%)","note":"Picks the nearest enemy or neutral base that is capturable or being taken; if none is, the nearest one you don't own."},
  {"label":"Bases Under Threat","says":"=== Friendly Bases Under Threat ===\nAbu al-Duhur [AB] 067°/18nm HP:78% L:75%\nTaftanaz [FARP] 352°/27nm HP:100% L:100%","note":"Your bases with an enemy in contact, most recent first."},
  {"label":"Friendly Status","items":[
   {"label":"Show (First Page)","says":"=== Friendly Objectives (by range) pg 1/2 ===\nAbu al-Duhur [AB] 067°/18nm HP: 78% L: 75% S: 88% F: 92% A: 40% [THREAT]\nHama [AB] 182°/35nm HP:100% L:100% S:100% F:100% A:100%\nMasyaf [FOB] 205°/44nm HP: 52% L: 50% S: 21% F: 64% A:  0% [PRIORITY]"},
   {"label":"Next Page >>","says":"=== Friendly Objectives (by range) pg 2/2 ===\nTartus [NAVAL] 231°/71nm HP:100% L:100% S:100% F:100% A:100%"},
   {"label":"<< Previous Page","says":"=== Friendly Objectives (by range) pg 1/2 ===\nAbu al-Duhur [AB] 067°/18nm HP: 78% L: 75% S: 88% F: 92% A: 40% [THREAT]"}
  ]},
  {"label":"Enemy Status","items":[
   {"label":"Show (First Page)","says":"=== Enemy Objectives (by range) pg 1/1 ===\nKuweires [AB] 021°/41nm HP: 15% L:100% [CAP]\nJirah [AB] 048°/52nm HP: 34% L: 75% [CONSOLIDATING 40%]\nAleppo [AB] 012°/58nm HP:100% L:100%"},
   {"label":"Next Page >>","says":"=== Enemy Objectives (by range) pg 1/1 ===\nKuweires [AB] 021°/41nm HP: 15% L:100% [CAP]"},
   {"label":"<< Previous Page","says":"=== Enemy Objectives (by range) pg 1/1 ===\nKuweires [AB] 021°/41nm HP: 15% L:100% [CAP]"}
  ]},
  {"label":"Neutral Status","items":[
   {"label":"Show (First Page)","says":"=== Neutral Objectives (by range) pg 1/1 ===\nTaftanaz [FARP] 352°/27nm HP:  0% L: 50% [CAP]"},
   {"label":"Next Page >>","says":"=== Neutral Objectives (by range) pg 1/1 ===\nTaftanaz [FARP] 352°/27nm HP:  0% L: 50% [CAP]"},
   {"label":"<< Previous Page","says":"=== Neutral Objectives (by range) pg 1/1 ===\nTaftanaz [FARP] 352°/27nm HP:  0% L: 50% [CAP]"}
  ]},
  {"label":"Base Detail","items":[
   {"label":"Abu al-Duhur","says":"========= Abu al-Duhur =========\nAirbase - owned by BLUE\n...\nRepair: FROZEN -- 3 enemy ground unit(s) within 10 km, nearest 3-5 km; clear them to resume repairs\nCapture: not eligible -- need health <=20% (now 78%) and every infantry defender dead (now 100% of them alive)\nTHREAT: enemy units within sight of the base"},
   {"label":"Hama","says":"========= Hama =========\nAirbase - owned by BLUE\n...\nMunitions 100%   Fuel 100%   Aircraft 100%  (UNLIMITED)\nInfantry defenders: 100% alive\nRepair: at full strength\nCapture: not eligible -- need health <=20% (now 100%) and every infantry defender dead (now 100% of them alive)"},
   {"label":"Masyaf","says":"========= Masyaf =========\nFOB - owned by BLUE\n...\nRepair: stalled -- supply 3% below the {{cfg:repair_supply_cost|5}}% each pulse needs\nNOTE: logistics detached -- no automatic resupply"},
   {"label":"Tartus","says":"========= Tartus =========\nNaval Base - owned by BLUE\n...\nNavaids: none assigned"}
  ],"note":"Only bases you own. Past ten bases the list splits into numbered submenus such as \"1. Abu al-Duh - Hama\"."},
  {"label":"Capture Advisor","items":[
   {"label":"1. Aleppo - Jirah","items":[
    {"label":"Aleppo","says":"===== CAPTURE ADVISOR: Aleppo =====\nAB - owned by RED\n...\nHealth 100%   Logi 100%   Supply high\n----------------------------------\nSTATUS: objective NOT eligible yet --\n  - objective health must be <=20% (now 100%)\n  - clear the infantry defenders\n----------------------------------\nYOUR CAPTURE TROOPS: none within 30 nm.\n  Deploy capture-capable troops and move them into the zone.\n----------------------------------\nBOTTOM LINE: objective health must be <=20% (now 100%)"},
    {"label":"Jirah","says":"===== CAPTURE ADVISOR: Jirah =====\nAB - owned by RED\n...\n----------------------------------\nSTATUS: post-capture hold -- takeable NOW by either side.\n  consolidation 40% -- about 180s before the garrison spawns.\n----------------------------------\nYOUR CAPTURE TROOPS: none within 30 nm.\n  Deploy capture-capable troops and move them into the zone.\n----------------------------------\nBOTTOM LINE: eligible -- no capture troops near the zone"}
   ]},
   {"label":"2. Kuweires - Taftanaz","items":[
    {"label":"Kuweires","says":"===== CAPTURE ADVISOR: Kuweires =====\nAB - owned by RED\n...\nHealth 15%   Logi 100%   Supply medium   Infantry 0% alive\n----------------------------------\nSTATUS: objective is ELIGIBLE for capture.\nREPAIR: FROZEN while you keep units within sight of the base -- and the repair countdown starts over from the moment you leave\nIN PROGRESS: your side is capturing -- held 45s of ~90s (more squads in the zone shorten it). Hold the zone.\n----------------------------------\nYOUR TROOPS NEAR THIS BASE:\n  - Standard: in the zone\n  - Anti Tank: in the zone\n----------------------------------\nBOTTOM LINE: being taken by Blue -- held 45s (~90s needed)"},
    {"label":"Taftanaz","says":"===== CAPTURE ADVISOR: Taftanaz =====\nFARP - owned by NEU\n...\nSTATUS: objective is ELIGIBLE for capture.\nCOOLDOWN: base changed hands recently -- no capture timer can start for 75s.\nREPAIR: NEUTRAL -- does not self-repair, must be retaken with troops\n----------------------------------\nBOTTOM LINE: just changed hands -- 75s cooldown before a new attempt"}
   ]}
  ],"note":"Only enemy and neutral bases your side can see. Long lists split into numbered submenus, ten per level."}
 ]}
]}
```

The top four entries work on your current position. The three status reports
page through a list. **Base Detail** and **Capture Advisor** list bases by name,
and that list is built when you enter the slot: a base that changes hands
while you fly does not move between them until you re-slot. `Capture Advisor:
Nearest` and `Capturable / Contested` always read the current world, so use
those when something has just changed.

## The Capture Advisor

This is the entry worth learning. Most failed captures are not a lack of
firepower: they are a missing precondition nobody could see. The advisor prints
the preconditions.

Pick `Capture Advisor: Nearest`, or drill into a specific base under
`Capture Advisor`, and you get a card like this:

```dcsmessage
===== CAPTURE ADVISOR: Kuweires =====
AB - owned by RED
LL:   N36°11.22' E037°34.80'
MGRS: 37SCA 12345 06789
From you: 021° / 41.2 nm
Zone radius: 2000 m
Health 34%   Logi 100%   Supply medium
----------------------------------
STATUS: objective NOT eligible yet --
  - objective health must be <=20% (now 34%)
  - clear the infantry defenders
REPAIR: WILL self-repair -- next pulse ~12m; act fast or it heals back above 20%
----------------------------------
YOUR TROOPS NEAR THIS BASE:
  - Standard: 1340 m outside the zone edge
  - Igla: in the zone  <-- CANNOT capture (troop type)
----------------------------------
BOTTOM LINE: objective health must be <=20% (now 34%)
```

The header tells you no more than the F10 map does: supply is the hex colour
as a word (`high`, `medium`, `low`), and the infantry figure (`Infantry 0%
alive`, a share of the garrison's infantry, not a head count) only appears once
health is at or below 20%.

Read the rest top to bottom.

**STATUS** is the verdict. One of:

| Line | Meaning |
| --- | --- |
| `objective is ELIGIBLE for capture.` | The base itself is ready. All that's left is capture-capable troops in the zone. |
| `objective NOT eligible yet --` and a list | Each `-` line is a blocker still failing. Fix them. |
| `post-capture hold -- takeable NOW by either side.` | Somebody just took it and is [consolidating](../gameplay/capturing-objectives.md#step-5-consolidation). The next line says how far along their clock is, or `the holder's troops are OUT of the zone -- their clock is stopped.` |

The blockers you can see:

| Blocker | What to do |
| --- | --- |
| `objective health must be <=20% (now 34%)` | Destroy more of the garrison. |
| `clear the infantry defenders` (and, once health is at 20% or less, `(12% still alive)`) | Hunt the infantry. Every one must die. |
| `destroy 50% of the defending units first (now 31%)` | Only on servers that set a minimum share of the garrison destroyed. |
| `sink the SUPPLY ship first (logi must reach 0%, now 100%)` | Carrier groups only. See [Carrier Operations](../gameplay/carrier-ops.md). |
| `destroy the SAM site first (health 60%)` | Special SAM sites only. This line is out of date: the engine lets troops take a special SAM site at any health. See [Capturing Objectives](../gameplay/capturing-objectives.md#special-sam-sites). |

**COOLDOWN** appears when the base changed hands (or fell Neutral) in the last
{{cfg:campaign_events.capture_cooldown_secs|120}} seconds: `COOLDOWN: base
changed hands recently -- no capture timer can start for 75s.` Don't waste a
troop insertion on it yet.

**REPAIR** says whether the base will heal itself back out of capture range:

| Line | Meaning |
| --- | --- |
| `WILL self-repair -- next pulse ~12m; act fast or it heals back above 20%` | Logistics and supply are working. Out-pace it or cut its supply. |
| `FROZEN while you keep units within sight of the base -- and the repair countdown starts over from the moment you leave` | Your presence is holding its repairs. |
| `FROZEN while the capture timer is running` | A capture is under way. |
| `STARVED -- supply is below the {{cfg:repair_supply_cost|5}}% a repair pulse costs; cannot heal until resupplied` | Its supply is too low to pay for a repair. |
| `logistics destroyed (logi 0%) -- cannot self-repair` | Its logistics buildings are gone. |
| `garrison wiped -- base will fall to NEUTRAL, then must be retaken with troops` | Health 0: it drops to Neutral on its own. |
| `NEUTRAL -- does not self-repair, must be retaken with troops` | Neutral bases never heal. |

**DEFENDERS OFFLINE** appears when the owner has almost nobody in a slot and
the server runs empty-server protection, for example `DEFENDERS OFFLINE: Red
has 0 pilot(s) in a slot (under 1) -- captures take 2x longer.` See
[Capturing Objectives](../gameplay/capturing-objectives.md#step-4-hold-the-zone-until-the-timer-runs).

**IN PROGRESS** appears when a capture timer is running. Yours: `IN PROGRESS:
your side is capturing -- held 45s of ~90s (more squads in the zone shorten
it). Hold the zone.` Theirs: `IN PROGRESS: Red is capturing this base -- held
40s. Kill their troops to stop it.`

**YOUR TROOPS NEAR THIS BASE** lists your squads (and dismounted infantry)
within 30 nm, each either `in the zone` or so many metres outside the zone
edge. `<-- CANNOT capture (troop type)` is the most common wasted sortie in the
campaign: **not every troop type can take a base.** With no squads near you get
`YOUR CAPTURE TROOPS: none within 30 nm.`

**BOTTOM LINE** is the one-sentence version, the same line the
`Capturable / Contested` list prints under each base:

| Bottom line | Meaning |
| --- | --- |
| `being taken by Blue -- held 45s (~90s needed)` | A timer is running. |
| `just changed hands -- 75s cooldown before a new attempt` | Cooldown. |
| the first blocker, e.g. `objective health must be <=20% (now 34%)` | Not eligible. |
| `eligible -- capture troops in the zone, hold position` | Your troops are in. |
| `eligible -- move your capture troops into the zone` | Your squad is near but outside. |
| `eligible -- no capture troops near the zone` | Nobody is there. |

If the base is yours, the card stops after the header with `You already own
this objective.` and, if it is damaged, its `REPAIR:` line.

## Nearest Base (detail) and Base Detail

`Nearest Base (detail)` gives the full card on whichever Blue or Red base is
closest to you (neutral bases are skipped). `Base Detail` gives the same card
for any base you own, picked by name. If the engine has no position for you,
`Nearest Base` answers `Get airborne first -- nearest base needs your
position.`

```dcsmessage
========= Abu al-Duhur =========
Airbase - owned by BLUE
LL:   N35°43.94' E037°06.23'
MGRS: 37SBV 23456 54321
From you: 067° / 18.4 nm
Zone radius: 2500 m
----------------------------------
Health  78%   Logi  75%
Munitions  88%   Fuel  92%   Aircraft  40%
Infantry defenders: 100% alive
Repair: active -- next pulse in ~7m
Capture: not eligible -- need health <=20% (now 78%) and every infantry defender dead (now 100% of them alive)
Navaids:
TACAN 37X ADH
```

What each line tells you:

- **Munitions / Fuel / Aircraft**, the infantry line, `Repair:` and `Navaids:`
  only appear on bases **you own**. An enemy base gets name, position, Health,
  Logi and the `Capture:` line. `(UNLIMITED)` after the stock line means the
  base never runs out of weapons and fuel. `Aircraft` is what is on the ramp:
  if it is low, the airframe you want may not be slottable. See
  [Materiel & the War Economy](../gameplay/war-economy.md).
- **`- COMMANDER PRIORITY`** after the owner: your command has flagged this
  base. Only its owner sees the flag.
- **`Repair:`** for your own base: `at full strength`, `active -- next pulse in
  ~7m` (with ` (undefended: repairs 1.5x faster)` when empty-server protection
  is speeding it up), `stalled -- supply 3% below the {{cfg:repair_supply_cost|5}}% each pulse needs`,
  `stalled -- logistics defense destroyed`, `FROZEN -- an enemy capture timer
  is running`, or a threat line such as `FROZEN -- 3 enemy ground unit(s)
  within 10 km, nearest 3-5 km; clear them to resume repairs`, `FROZEN --
  enemy aircraft within threat range of the base` or `FROZEN -- threat has
  cleared, repairs resume in ~4m`.
- **`NOTE: logistics detached -- no automatic resupply`**: this base gets no
  supply down the chain at all. It has to be kept alive by crates, C-130 drops
  or an AI helo run.
- **`BRC:`** on a carrier group: its base recovery course, with the recovery
  case in `Info → Weather`. See [Carrier Operations](../gameplay/carrier-ops.md).
- **`Capture:`** how far the base is from capturable. On your own base it gives
  the exact infantry share; on an enemy base it never tells you whether
  infantry is left until health is down. On a consolidating base: `HELD --
  consolidating 40% (180s left); you can't start a capture timer here, wipe the
  holding troops to force it Neutral`, or `HELD, but consolidation is PAUSED --
  the holding troops are outside the zone; get them back in, or wipe them to
  force it Neutral`. On a carrier: `sink the SUPPLY ship first (Logi must reach
  0%)` or `dead in the water -- board with capture troops`.
- **`THREAT: enemy units within sight of the base`**: the base is threatened.
- **`Navaids:`** what your base broadcasts, or `Navaids: none assigned`. The
  enemy never gets this line. See [Navaids & Approaches](../gameplay/navaids.md).

## Capturable / Contested

Every base that is takeable right now or has a capture timer running, in both
directions: enemy and neutral bases you can move on, and your own that are
exposed. Each line gives kind, owner (`BLU`, `RED`, `NEU`), bearing and range,
health, `capturable` or `BEING TAKEN`, and the bottom line from the Capture
Advisor underneath. Nearest first. When nothing qualifies: `Nothing is
capturable right now.`

## Bases Under Threat

Your bases with an enemy in contact (see
[Threatened](../gameplay/objectives.md#threatened)), most recently threatened
first, with health and logistics. When none are: `No friendly bases are under
threat.` A base whose own capture timer is running shows as `BEING TAKEN` in
`Capturable / Contested`: that is the emergency.

## Friendly, Enemy and Neutral Status

Paged roll-ups of every base one side holds, ten per page, nearest first (by
name if the engine has no position for you). Each has three fixed commands:
`Show (First Page)`, `Next Page >>` and `<< Previous Page`, which wrap round at
the ends.

```dcsmessage
=== Friendly Objectives (by range) pg 1/2 ===
Abu al-Duhur [AB] 067°/18nm HP: 78% L: 75% S: 88% F: 92% A: 40% [THREAT]
Hama [AB] 182°/35nm HP:100% L:100% S:100% F:100% A:100%
Masyaf [FOB] 205°/44nm HP: 52% L: 50% S: 21% F: 64% A:  0% [PRIORITY]
```

`HP` is health, `L` logistics. Your own bases add `S` supply, `F` fuel and `A`
aircraft. Kind tags: `AB` airbase, `FOB`, `FARP`, `LOGI` logistics hub,
`NAVAL` naval base, `CVN` carrier group, `FACT` factory, `SAM` special SAM
site, `CC` command center. The enemy list never shows their carriers or SAM
sites: those are drawn for their owner only.

State flags at the end of a line:

| Flag | Meaning |
| --- | --- |
| `[THREAT]` | An enemy is in contact with the base. |
| `[CAP]` | Capturable now. |
| `[CONSOLIDATING 40%]` | Just captured, holding troops are consolidating. |
| `[CONSOLIDATION PAUSED]` | Just captured, but the holding troops have left the zone. |
| `[BEING TAKEN]` | A capture timer is running on it. |
| `[PRIORITY]` | Your command has flagged it. Only on your own bases. |

## Where the AI helo missions went

`AI Helo Missions` (troop insertions and resupply runs) is built from this
menu's code but hangs off `F10 → Actions`. See
[AI Helo Missions](../advanced/helo-missions.md).

## See Also

- [Capturing Objectives](../gameplay/capturing-objectives.md): the full capture and consolidation rules
- [Objectives](../gameplay/objectives.md): objective types and what each one does
- [Capturing a Base](../playbooks/capturing-a-base.md): the step-by-step walkthrough
- [Reading the F10 Map](../gameplay/reading-the-map.md): the same state as rings and hexes
- [Info Menu](./info.md): the other reporting menu
