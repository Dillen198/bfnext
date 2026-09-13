# Objectives Menu

`F10 → Objectives` is the base-by-base picture: who owns what, what is falling,
what you can take, and — through the **Capture Advisor** — exactly why the base
you are circling will not flip.

Everything here is free and available from every slot.

```
F10 → Objectives
├── Nearest Base (detail)      full card on the base you are closest to
├── Capturable / Contested     everything takeable right now, both directions
├── Capture Advisor: Nearest   why the nearest enemy base won't flip
├── Bases Under Threat         your bases with enemies inside the wire
├── Friendly Status            paged roll-up of every base you own
├── Enemy Status               paged roll-up of every base you don't
├── Base Detail                per-base card, your bases, A–Z submenus
└── Capture Advisor            per-base advisor, enemy/neutral bases, A–Z submenus
```

## The Capture Advisor

This is the feature worth learning. Most failed captures are not a lack of
firepower — they are a missing precondition nobody could see. The advisor prints
the preconditions.

Pick `Capture Advisor: Nearest`, or drill into a specific base under
`Capture Advisor`, and you get a card:

```
===== CAPTURE ADVISOR: Al Dhafra =====
AIRBASE - owned by RED
LL:   24°14'56"N 054°32'51"E
MGRS: 39R YH 12345 67890
From you: 067° / 18.4 nm
Zone radius: 2500 m
Health 34%   Logi 61%   Supply 88%   Infantry 6
----------------------------------
STATUS: objective NOT eligible yet --
  - objective health must be <=20% (now 34%)
  - clear the infantry defenders (6 left)
REPAIR: active -- next pulse in ~7m
----------------------------------
YOUR TROOPS NEAR THIS BASE:
  - Standard: 1340 m outside the zone edge
  - ATGM: in the zone  <-- CANNOT capture (troop type)
----------------------------------
BOTTOM LINE: grind health to 20% and kill the infantry first
```

Read it top to bottom:

**STATUS** is the verdict. One of:

- `objective is ELIGIBLE for capture` — the base itself is ready; all that's
  left is getting capture-capable troops into the zone.
- `NOT eligible yet` followed by a list of **blockers** — the specific
  conditions still failing. Fix those, in that order.
- `post-capture hold -- takeable NOW by either side` — somebody just took it and
  is [consolidating](../gameplay/capturing-objectives.md). It is at its most
  fragile. The card tells you how far along their clock is, and whether their
  troops have left the zone and stopped it.

Eligibility rules differ by objective kind, and the advisor says which one
applies: a **special SAM site** just has to be destroyed outright (health 0%); a
**carrier group** needs its SUPPLY ship sunk (logi 0%); everything else needs
health at or below 20% with the infantry defenders cleared.

**COOLDOWN** appears if the base changed hands recently — no capture timer can
even start for the listed number of seconds. Do not waste a troop insertion.

**REPAIR** tells you whether the base is healing itself and when the next repair
pulse lands. This is the line that answers "we keep bombing it and it keeps
coming back": a base with working logistics and materiel on hand repairs on a
timer, so you have to out-pace it, or cut its supply first.

**IN PROGRESS** appears when a capture timer is already running — whose it is,
how long they have held, roughly how long is needed. If it is the enemy's,
killing their troops in the zone stops it.

**YOUR TROOPS NEAR THIS BASE** lists your capture-relevant groups within 30 nm,
and for each one either `in the zone` or how many metres short of the zone edge
it is. The `<-- CANNOT capture (troop type)` flag is the single most common
wasted sortie in the campaign: **not every troop type can take a base.** Check
[Deployable Units](../reference/deployables.md) before you fly the insertion.

**BOTTOM LINE** is the one-sentence version. If you read nothing else, read this.

## The other reports

### Nearest Base (detail)

Full card on whichever objective you are closest to:

```
========= Al Dhafra =========
AIRBASE - owned by BLUE
LL:   24°14'56"N 054°32'51"E
MGRS: 39R YH 12345 67890
From you: 067° / 18.4 nm
Zone radius: 2500 m
----------------------------------
Health  78%   Logi  61%
Munitions  88%   Fuel  92%   Aircraft  40%
Infantry defenders: 4
Repair: active -- next pulse in ~7m
Capture: not eligible (health 78%)
Navaids:
  TACAN 37Y ALD
```

The fastest way to answer "what am I actually looking at". Things to look for:

- **Munitions / Fuel / Aircraft** only appear for objectives you own. `Aircraft`
  is what is physically on the ramp — if it is low, the airframe you want may
  not be slottable. See [Materiel & the War Economy](../gameplay/war-economy.md).
- **`Repair:`** — whether the base is healing itself and when the next pulse is.
- **`NOTE: logistics detached`** — this base gets **no automatic resupply at
  all**. It has to be sustained by hand with crates, C-130 drops or an AI helo run.
- **`THREAT:`** — enemy units within sight of the base.
- **`BRC:`** on a carrier group — the base recovery course, with the recovery
  case in `Info → Weather`.
- **Navaids** it broadcasts. See [Navaids & Approaches](../gameplay/navaids.md).

### Capturable / Contested

Everything that is takeable right now, in both directions — enemy bases you can
move on, and your own that are exposed. Bases already mid-capture are flagged.
This is the "where should I go" list.

### Bases Under Threat

Your bases with enemy units inside or near the wire, with how bad it is. Tagged
`[BEING TAKEN]` when a capture timer is actually running against them — that one
is an emergency; a base flips while everyone is busy elsewhere.

### Friendly Status / Enemy Status

Paged roll-ups of every base on a side, one line each:

```
Al Dhafra [AIRBASE]
  HP: 78% L: 61%   S: 88% F: 92% A: 40%
```

`HP` health, `L` logistics, `S` supply, `F` fuel, `A` airframes. Use `Next Page
>>` / `<< Previous Page` to move through them. The stock line tells you at a
glance which of your bases is about to stop generating sorties — see
[Logistics & Supply](../gameplay/logistics.md).

Objectives carrying extra state are tagged inline: `[THREAT]`, `[BEING TAKEN]`,
`[CONSOLIDATING 40%]`, `[CONSOLIDATION PAUSED]`, `[CAP]`, `[PRIORITY]`,
`- COMMANDER PRIORITY`.

### Base Detail

The same full card as `Nearest Base`, but for any base **you own**, picked from
alphabetical submenus (`1. Abu Su - Damascu`, `2. Deir ez - Kiryat`, …).

## Why some bases aren't in a list

The lists are filtered on purpose:

- **Base Detail** — only bases you own.
- **Capture Advisor** — only bases you do *not* own (enemy and neutral).
- **Capturable / Contested** — only what is actually takeable right now.

A base that flips owner mid-flight moves between these lists, and the menu was
built when you entered the slot. `Capture Advisor: Nearest` and
`Capturable / Contested` always reflect the current world, so use those when
something has just changed.

## See Also

- [Capturing Objectives](../gameplay/capturing-objectives.md) — the full capture and consolidation rules
- [Objectives](../gameplay/objectives.md) — objective types and what each one does
- [Capturing a Base](../playbooks/capturing-a-base.md) — the step-by-step walkthrough
- [AI Helo Missions](../advanced/helo-missions.md) — getting troops into a zone without flying them yourself
