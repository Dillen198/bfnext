# Capturing Objectives

Capturing objectives is how the campaign is won. This page covers the whole
process, every message you will see along the way, and every way it can fail.

![Troops fast-roping from a helicopter to capture an objective](/api/wiki/images/3ce5c418-9d0b-429a-be7e-687032cb147f)

A round is won by **territory, not kill count**. See
[Round Rules](./round-rules.md) for the victory condition and the Last Stand.

![Flow chart of a capture, with engine default times. Step 1, knock the base down to health 20% or less with every infantry defender dead; if left alone it repairs itself. Step 2, get a capture-capable squad alive inside the ring, at least 120 seconds after the base last changed hands; the wrong squad type, or capture troops of both sides in the zone, stop it starting. Step 3, a capture timer of 180 seconds divided by the squads in the zone, never under 30 seconds; it resets if all your squads are dead or out of the zone for 10 seconds. Step 4, the owner flips and a light garrison is put on the books. Step 5, a consolidation hold of 300 seconds of progress while the troops stay in the zone; leaving the zone pauses it, and losing every holding squad drops the base to Neutral. Step 6, consolidated: the troops stand down and the garrison moves in.](/api/wiki/images/09871940-18cb-4c19-bcf3-dee1c0a051da)

The diagram shows engine defaults. Your server's numbers are the ones in the
text below.

## What has to be true

A capture timer starts when **all** of these hold at once:

| Condition | Detail |
| --- | --- |
| Health at or below 20% | Health is the share of the garrison still alive. See [Objectives](./objectives.md#health). |
| No infantry defender alive | Every one. A single survivor blocks the capture. |
| Enough of the garrison destroyed | Only on servers that set `capture_min_unit_pct_destroyed`, currently {{cfg:campaign_events.capture_min_unit_pct_destroyed|0}} (0 = off). |
| Capture-capable troops of yours alive inside the zone | Troops in a helicopter don't count. Not every squad type can capture. |
| No capture troops of another side in the zone | If two sides both have capture troops inside, neither side's timer runs. Only matters at a Neutral base: the owner's own troops never block a capture. |
| Not in a consolidation hold | A base just taken can't have a fresh timer started on it. Its holding troops have to be killed first. |
| Off cooldown | For {{cfg:campaign_events.capture_cooldown_secs|120}} seconds after a base changes hands or falls Neutral, no new timer can start. |

Logistics does not gate a capture. Low logistics stops a base repairing and
supplying, but you take it by killing its garrison. The exception is a base
with no garrison units at all: its health follows its logistics buildings. See
[Buildings at an Objective](./objective-buildings.md).

Special SAM sites and carrier groups play by different rules, below.

## Step 1: Knock it down

Destroy the garrison: armour, AAA, SAM launchers and above all the
**infantry**, which has to be completely wiped out.

- CAS with bombs, rockets, guns and guided weapons. Sweep the whole zone for
  infantry.
- JTAC-directed [artillery](../advanced/artillery.md) and
  [cruise missiles](../advanced/alcm.md) are good against dug-in troops.
- The garrison is only in DCS while someone is near the base, so a quiet base
  can look empty from a distance. Its units still count.

**Watch progress on the F10 map.** The health hex runs from green to red. When
health is at or below 20% with infantry still alive, the label adds
`Infantry: 12% — clear all defenders to capture`. When the base is capturable
the inner ring turns **white** and the label says who can take it. See
[Reading the F10 Map](./reading-the-map.md).

**The last defenders are marked for you.** Once a base is at 20% health or
less, is threatened, and has eight or fewer infantry left, the attacking side
gets a map pin near each survivor, refreshed every minute and placed within
about 150 m of the man:

```dcsmessage
Last defenders of Kuweires: infantry here (3 left)
```

**The garrison fights back.** Enemy ground units near a base set its armour and
infantry moving out to meet them, and if they stay the owner may send a
formation as a quick reaction force. The defenders get:

```dcsmessage
Kuweires: enemy armour at the perimeter, the garrison is moving to engage.
```

```dcsmessage
GROUND COMMAND: 3rd Armoured Bn is the quick reaction force for Kuweires.
```

See [Ground War](../advanced/ground-war.md).

**When it becomes capturable**, everyone is told at once:

```dcsmessage
Kuweires (Red) is undefended -- Blue can capture it with troops in the zone.
```

and a few moments later, if it is still capturable:

```dcsmessage
Kuweires (Red) is now capturable by Blue
```

A neutral base reads `Taftanaz is neutral and capturable -- either side, get
troops into the zone.` and `Taftanaz (neutral) is now capturable`.

**Health 0 drops a base to Neutral.** A base whose garrison is wiped out
completely falls out of its owner's hands on its own:

```dcsmessage
Kuweires: the garrison has been wiped out -- the base is now Neutral and must be retaken with troops
```

A Neutral base locks its slots, never repairs, and has to be captured with
troops by either side. Carrier groups, command centers and special SAM sites
never go Neutral this way.

**Don't leave it alone.** A damaged base repairs one group per repair pulse
while nobody is near it. See [How bases repair](#how-bases-repair-and-how-to-stop-them).

## Step 2: Capture troops into the ring

Not every squad can capture. The usual four (costs and weights are this
server's, if it publishes them):

| Squad | Cost | Can capture? | Weight |
|---|---|---|---|
| Standard | {{cfg:troops.Blue[0].cost|0}} pts | **yes**, the workhorse | {{cfg:troops.Blue[0].weight|800}} kg |
| Anti Tank | {{cfg:troops.Blue[1].cost|1}} pts | **yes**, and kills armour | {{cfg:troops.Blue[1].weight|1000}} kg |
| Mortar | {{cfg:troops.Blue[2].cost|5}} pts | **yes**, plus indirect fire | {{cfg:troops.Blue[2].weight|1200}} kg |
| Igla / Stinger (MANPADS) | {{cfg:troops.Blue[3].cost|5}} pts | **no** | {{cfg:troops.Blue[3].weight|150}} kg |

This server's Blue squads, straight from its config:

{{list:troops.Blue|_Squad list unavailable for this server: check F10 → Troops → Squads in game._}}

Dismounted infantry from a killed APC can capture too, where the server sets
that vehicle's dismounts as capture-capable.

**Load from a different base.** `F10 → Troops → Squads → Load Standard squad`
works only inside a friendly base that still has logistics and is not itself
capturable. Otherwise you get `not near friendly logistics (or this base is
too damaged to supply from)`. See [Troop Transport](../f10-menu/troops.md).

**Unload inside the ring.** Land inside the zone (the dashed owner ring on the
F10 map) and use `F10 → Troops → Unload`. Your side sees `Viper dropped
Standard troops into the field`, and you get a hint about the nearest enemy or
neutral base within 5 km of its edge:

| Hint | Meaning |
| --- | --- |
| `troops are in Kuweires's ring -- keep them alive to capture it` | Good. The timer should start. |
| `troops are in Kuweires's ring, but it isn't capturable yet -- see Objectives > Capture Advisor` | The base isn't knocked down yet. |
| `these troops are 340m outside Kuweires's ring -- they only capture from inside it` | Move them in. |
| `Igla troops can't capture -- bring a squad that can to take Kuweires` | Wrong squad. |

The enemy being near the target is no reason not to unload there. The only
unload that is blocked by enemies is inside **your own** base while it is
threatened: `you can't deploy troops here while enemies are near`.

No helicopter? [AI Helo Missions](../advanced/helo-missions.md) and a
[ground war](../advanced/ground-war.md) formation with infantry can both put
capture troops in a zone.

## Step 3: Hold the zone until the timer runs

The moment the conditions hold, your side and theirs are told:

```dcsmessage
Capturing Kuweires (90 sec)
```

```dcsmessage
Kuweires is being captured! Eliminate enemy troops!
```

The F10 label adds `Capturing: 50%`, the base's owner ring and icon flash, and
the base cannot repair while the timer runs.

- **Timer length:** {{cfg:campaign_events.capture_time_secs|180}} seconds,
  divided by the number of capture squads in the zone, never under 30 seconds.
  With the default: one squad 180 s, two 90 s, three 60 s.
- **It resets** if no capture squad of yours has been in the zone for 10
  seconds (all dead, or moved out), or if another side's capture troops come
  in. A short gap in position updates is covered by that 10-second grace.
- **Undefended bases take longer.** Where the server runs empty-server
  protection, a capture against a side with fewer than
  {{cfg:population_scaling.min_defenders|1}} pilot(s) in a slot takes
  {{cfg:population_scaling.capture_time_mult_when_undefended|2}}x as long (up
  to {{cfg:population_scaling.max_capture_time_mult|4}}x on servers that scale
  it by how badly the defenders are outnumbered). Both messages then end with
  ` (defenders offline: capture takes 2x longer)`. It is checked continuously:
  a defender slotting in shortens it at once. Neutral bases are never scaled.

The Capture Advisor shows the timer from your side:

```dcsmessage
IN PROGRESS: your side is capturing -- held 45s of ~90s (more squads in the zone shorten it). Hold the zone.
```

## Step 4: The base changes hands

When the timer completes, everyone sees:

```dcsmessage
BASE CAPTURE: Kuweires has been taken by Blue!
```

and a map pin `Kuweires captured by Blue` marks it for 30 minutes. In the same
moment:

- **The previous owner's garrison is overrun**: every unit and building they
  still had at the base is removed.
- **You get a light garrison**: {{cfg:campaign_events.capture_garrison_revive_fraction|0.25}}
  of it (as a fraction), AAA and infantry first, then armour, always including
  at least one infantry group. **No SAMs** unless the server sets
  `capture_garrison_revive_include_sam`. Everyone is told:

```dcsmessage
Kuweires: light garrison in place (~22% -- AAA and infantry). Its SAM cover has to be rebuilt or flown in.
```

- The DCS airfield, its warehouse stock and the supply lines pass to you.
- Logistics gets one repair step, and the base's services come back three
  minutes later.
- **Points**: {{cfg:points.capture|15}} points, split equally between the
  players whose squads were in the zone (a squad sent by an AI helo mission
  counts for the player who called it).
- The base's fund (points donated to it) mostly goes: you keep
  {{cfg:economy.capture_fund_keep|0.25}} of it as a fraction. See
  [Materiel & the War Economy](./war-economy.md).

The light garrison is **not on the map yet**. It moves in when consolidation
finishes. Until then the base is held only by your assault troops.

## Step 5: Consolidation

For {{cfg:capture_consolidation_secs|300}} seconds of progress the base is in
a **consolidation hold**. On servers that set this to 0 there is no hold and
your troops are removed at once.

- The F10 label shows `>> CONSOLIDATING 42% (2m54s left) - hold the zone with
  troops`.
- **Your troops must stay in the zone.** Out for more than
  {{cfg:consolidation_zone_grace_secs|15}} seconds and the clock stops. Progress
  is kept, it just stops accruing until they are back. The label switches to
  `>> CONSOLIDATION PAUSED - no troops in the zone`.
- You can't fly them out: `F10 → Troops → Extract` answers `those troops are
  holding Kuweires while it consolidates -- they can't be extracted until the
  capture is settled`.
- **You can't slot in there yet**: `Objective is still consolidating (174s
  left) -- hold it with troops, or land a logistics crate to speed it up`.
- **Beat the clock.** Each extra squad in the zone adds
  {{cfg:consolidation_squad_bonus|0.5}} to the rate (0.5: two squads 1.5x,
  three 2x). Landing a **logistics repair kit** or a **supply crate** at the
  base jumps it forward {{cfg:consolidation_crate_progress_secs|120}} seconds,
  even when its logistics are already at 100%.
- **The enemy cannot start a capture timer** on it. Their only answer is to
  kill your holding troops.

The holding side gets these messages:

```dcsmessage
Kuweires: holding troops have left the zone -- CONSOLIDATION PAUSED
```

```dcsmessage
Kuweires: troops back in the zone -- consolidation resumed
```

```dcsmessage
Kuweires: logistics repair kit delivered -- consolidation advanced, 60s to go
```

```dcsmessage
Kuweires: one minute to consolidation -- hold the zone
```

A supply crate reads `supplies delivered` in place of `logistics repair kit
delivered`, and a crate that finishes the job reads `-- consolidation complete,
garrison moving in`.

**If every holding squad is killed**, the base drops to **Neutral**, its
leftovers are cleared out, and the {{cfg:campaign_events.capture_cooldown_secs|120}}-second
cooldown starts. Then either side can capture it with troops:

```dcsmessage
Kuweires: the assault force was wiped out -- the base is contested (Neutral)
```

Because the light garrison is not on the map until consolidation ends, it
cannot hold the base for you: losing the assault troops loses the base.

**If they survive**, the base consolidates. The assault troops are stood down
and the garrison moves in:

```dcsmessage
Kuweires consolidated -- garrison moving in, assault troops stood down
```

## After the capture

- A freshly taken base is a **soft target**: about a quarter of a garrison and
  no SAMs. The rest rebuilds through auto-repair, or you fly it in as
  [deployable crates](../playbooks/running-crates.md).
- You cannot slot in at a base of yours that is itself capturable: the slot
  answers `Objective is capturable`. A Neutral base answers `Blue does not own
  the objective associated with this slot`.
- Expect a counter-attack from the air in the first minutes. Top cover earns
  its keep here.

## Special cases

### Special SAM sites

A special SAM site is drawn on the F10 map for its owner only, so the enemy
has to find it. The engine treats it as **always open to capture**: no health
or infantry requirement. Get capture troops into its zone and the normal
capture timer runs (divided by squads, 30 s minimum), followed by the normal
consolidation hold. Its launchers and radars are still live while you do it.

Two in-game texts are out of date here. The Capture Advisor still lists
`destroy the SAM site first (health 60%)`, the Base Detail card says
`eligible NOW -- SAM destroyed, capture is instant`, and `Info → Help →
Objectives & Capturing` says capture is instant. The dashboard's capture report
has it right: `A special SAM site can be captured at any time: get capture
troops into the zone.`

A captured SAM site re-arms under its new owner. If a Command Center is
destroyed or captured, the SAM sites linked to it lose their networking; see
[Objectives](./objectives.md#command-centers).

### Carrier groups

A carrier is boarded, not captured the normal way:

1. **Sink its SUPPLY ship.** Every carrier group has an escort with `SUPPLY`
   in its name. Sink it and the carrier's Logi drops to 0%: dead in the water.
2. **Board it** with capture troops in its zone. Your side sees `Boarding
   Stennis (180 sec)`, theirs `Stennis is being boarded! Eliminate enemy
   troops!`. The boarding timer is the full
   {{cfg:campaign_events.capture_time_secs|180}} seconds (more squads do not
   shorten it). There is no consolidation hold; the troops are removed and
   the points paid at once.

A disabled carrier also follows its linked naval base: take the naval base
(before or after the supply ship sinks) and the carrier flips to you once its
Logi is 0. How a captured carrier is repaired and re-crewed is on
[Carrier Operations](./carrier-ops.md).

### Command Centers

A Command Center never drops to Neutral at health 0. Troops have to capture it.

## When the defenders are offline

Where the server runs empty-server protection, the Capture Advisor adds a line
such as `DEFENDERS OFFLINE: Red has 0 pilot(s) in a slot (under 1) -- captures
take 2x longer, repairs run 1.5x faster.` and the defending side's repair
pulses can run faster too
({{cfg:population_scaling.repair_speed_mult_when_undefended|1}}x).

## Why won't it capture?

Ask the engine. Three places tell you:

1. **In the cockpit:** `F10 → Objectives → Capture Advisor: Nearest`, or the
   base by name under `Capture Advisor`. It lists the blockers, the cooldown,
   whether the base is out-repairing you, and which of your squads are in the
   zone, including any that **cannot capture because of their troop type**.
   See [Objectives Menu](../f10-menu/objectives.md#the-capture-advisor).
2. **On the F10 map:** the label and rings. See
   [Reading the F10 Map](./reading-the-map.md).
3. **On the dashboard:** click a base on the Live Ops map, the Objectives page,
   the TacMap or the Command page. Its drawer carries a capture report.

### The dashboard capture report

The panel heading depends on the state and on who is reading it:
`Why can't this be captured?` (an enemy or neutral base), `What the enemy
needs to capture it` (your own base), `Capturable now` / `Open to capture`, or
`Capture in progress`. Under it is a one-line summary, for example
`Not capturable yet: destroy 4 more targets, then kill the infantry.`,
`Capturable now: get capture troops into the zone.`, `Being captured by Blue:
held 45s of about 1m30s.`, or `Just changed hands: 1m15s before a new capture
can start.`

Then each blocker, numbered, with what to do about it:

| Blocker | What to do |
| --- | --- |
| `Health is 34%. A capture needs 20% or less.` | `Destroy 4 more garrison units.` (or `logistics buildings` on a base whose health is its buildings) |
| `3 infantry defenders are still alive. Every one must die.` | `Hunt down the infantry: strafe them, or bring ground forces.` |
| `This campaign needs 50% of the defending units destroyed first (now 31%).` | `Destroy more of the garrison.` |
| `It just changed hands. No new capture can start for 1m15s.` | `Wait for the cooldown, with capture troops ready.` |
| `Just captured by Red and still held by its assault troops while they consolidate, about 2m00s left.` | Attacker: `Kill the holding troops in the zone. That drops it to neutral, and then either side can capture it.` Owner: `Keep the holding troops alive and inside the zone until it consolidates.` |
| `Capture troops from more than one side are in the zone, so neither side's capture runs.` | `Clear the other side's troops out of the zone.` |
| `Your nearest capture squad (Standard) is 1.3 km outside the zone.` | `Move them into the zone and hold.` |
| `Igla is in the zone but can't capture.` | `Bring a capture-capable squad.` |
| `None of your capture troops are near it.` | `Fly or drive capture-capable troops into the zone and hold it.` |
| `Nothing here counts toward its health: it has no garrison units and no logistics buildings in its zone. Its health is stuck at 100%, so it can't be captured.` | Nothing you can do in the air: report it to the admins with the objective name. |

**Still standing** lists what is left to kill: infantry first, then the rest,
then the logistics buildings, each with an MGRS grid where your side can place
it, and a mini-map (`Show on map` / `Hide map`). Enemy units are only placed
where your side's intel has a contact near them; the rest are counted by type
with `position unknown`, and a note says how many `defenders are not on your
intel picture. Recon, a JTAC or a closer look will place them.` Logistics
buildings are always placed, since their F10 pins are public. Other notes you
may see: the garrison being off the map while nobody is near, and
`Destroying its logistics buildings lowers logistics, not health.`

## Failed captures at a glance

| What happened | Result |
| --- | --- |
| You left the base alone | It repairs a group per pulse and climbs back above 20%. |
| Your only squad is MANPADS | Nothing. They can't capture. |
| Your squads died or left the zone for 10 s | The capture timer resets. |
| Capture troops of two sides in a Neutral zone | Neither timer runs. |
| The base changed hands under two minutes ago | No timer can start until the cooldown ends. |
| Your holding troops left the zone during consolidation | The clock pauses; progress is kept. |
| Every holding squad was killed during consolidation | The base goes Neutral and has to be taken again. |

## How bases repair (and how to stop them)

A damaged base you own rebuilds itself **one destroyed group at a time**. Each
pulse comes {{cfg:repair_time|1800}} seconds after the last change at full
logistics, longer as logistics falls (at 50% logistics, twice as long), never
at 0% logistics, and costs the base materiel or supply. A base whose supply is
below {{cfg:repair_supply_cost|5}}% can't pay and doesn't repair.

- **Repairs freeze while the enemy is at the base.** The base is threatened
  while an enemy pilot is within threat range with line of sight, or an armed
  enemy ground unit is within {{cfg:ground_vehicle_cull_distance|10000}} m. AI
  aircraft don't count. It only counts as clear
  {{cfg:threatened_cooldown|300}} seconds after the last one leaves.
- **The countdown starts over after every attack.** It runs from the last
  moment an enemy was seen, so a base you hit, left, and came back to has not
  quietly finished its repair while you were away.
- **Repairs also freeze while a capture timer is running.**
- **A wiped garrison does not heal at all.** Health 0 drops it to Neutral, and
  Neutral bases never repair.
- **Emergency repair crate.** Where the server has it, logistics pilots can fly
  an emergency repair crate into a damaged, quiet friendly base for one repair
  on the spot. See [Logistics & Supply](./logistics.md).
- **Starve it.** Cut its supply line, or destroy its logistics buildings, and
  the repairs stop on their own.

The Capture Advisor's `REPAIR:` line tells you, for any enemy base, whether it
is repairing, frozen or starved, and roughly when the next group comes back.

## See Also

- [Capturing a Base](../playbooks/capturing-a-base.md): the step-by-step playbook
- [Objectives Menu](../f10-menu/objectives.md): the Capture Advisor
- [Objectives](./objectives.md): objective types and their states
- [Reading the F10 Map](./reading-the-map.md): rings, hexes and labels
- [AI Helo Missions](../advanced/helo-missions.md): getting capture troops into a zone without flying them yourself
- [The Tasking Board](./tasking-board.md): posting a capture task
- [Logistics & Supply](./logistics.md): holding what you took
- [Materiel & the War Economy](./war-economy.md): captured airframes and base funds
- [Deployable Units](../reference/deployables.md)
- [Carrier Operations](./carrier-ops.md)
