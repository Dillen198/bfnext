# Capturing Objectives

Capturing objectives is the primary way to win the campaign. This guide covers the complete capture process.

![Troops fast-roping from a helicopter to capture an objective](/api/wiki/images/3ce5c418-9d0b-429a-be7e-687032cb147f)

A round is won by **territory, not kill count**. The coalition that controls the required share of all objectives on the map wins the round — contested or neutral objectives don't count toward either side.

## Prerequisites for Capture

An objective can only be captured when **ALL** of these conditions are met:

1. **Health ≤ 20%** — the objective's defending units are almost wiped out ✓
2. **No infantry left** — every infantry defender in the zone is dead ✓
3. **Capture troops in zone** ✓
4. **Troops are correct type** ✓
5. **No enemy contest** ✓
6. **Not in a post-capture hold** — a base that just changed hands is held by
   its new owner's assault troops and can't have a fresh capture timer started
   against it (see Step 5) ✓
7. **Off cooldown** — for ~2 minutes after a base changes owner or falls
   Neutral, no new capture timer can start against it ✓

> Health/infantry — not logistics — is what gates capture. Logistics still
> matters (it drives repair speed and the objective's own garrison), but you
> capture a base by killing its defenders, not by emptying its warehouse.

> A base bombed all the way to **Health 0** flips to **Neutral** on its own —
> no troops needed to knock it loose, though you still need troops to *take*
> the Neutral base afterwards.

> You **cannot load troops or pull crates from a base that is itself
> capturable** — its logistics are considered cut off. Stage your assault from
> another friendly objective.

## The Capture Process

### Step 1: Grind the objective down to ≤ 20% health, no infantry

You have to destroy the objective's defending units — armour, AAA, SAMs,
and especially the **infantry**, which must be *completely* eliminated.
Methods:

**Ground Strikes**:
- CAS with bombs, rockets, guns, GBUs, AGMs
- Sweep the zone for infantry — a single surviving squad blocks the capture

**Artillery**:
- JTAC-directed artillery / MLRS fire
- Area bombardment against troop concentrations

**Cruise / Ballistic Missiles**:
- Long-range strikes via the Actions menu
- Good against hardened defenders

**Monitoring Progress**:
- F10 map marker shows the objective's Health % and Infantry %
- **The inner ring turns WHITE** on the F10 map when it's capturable
  (instead of the owner's colour)
- If troops are in the zone but it isn't taking, the system announces why —
  e.g. *"health still above 20% (34%)"* or *"enemy infantry still defending
  (12% left)"*

### Step 2: Deploy Capture Troops

Not all troops can capture! You need specific infantry units.

**Getting capture troops**:

| Squad | Cost | Can capture? | Weight |
|---|---|---|---|
| Standard | {{cfg:troops.Blue[0].cost|0}} pts | **yes** — the workhorse | 700 kg |
| Anti Tank | {{cfg:troops.Blue[1].cost|1}} pts | **yes**, and kills armour | 750 kg |
| Mortar | {{cfg:troops.Blue[2].cost|5}} pts | **yes**, plus indirect fire | 900 kg |
| Stinger / Igla (MANPADS) | {{cfg:troops.Blue[3].cost|5}} pts | **no** | 150 kg |

All of them act as an 8 km JTAC once on the ground.

**If in doubt, check the Capture Advisor** — `F10 → Objectives → Capture
Advisor` flags any of your squads near the base with
`<-- CANNOT capture (troop type)`, which is the fastest way to catch this
mistake before you have flown 60 nm with the wrong squad.

**Loading Troops**:
1. Land at friendly objective/FARP
2. F10 → Troops → Load → Select troop type
3. Transport via helicopter (Mi-8, UH-1H, CH-47, etc.)

See [Troop Transport](../f10-menu/troops.md) for detailed instructions.

### Step 3: Enter the Capture Zone

The capture zone is the designated area around the objective.

**Identifying the Zone**:
- Check F10 map markers
- Usually circular around the objective center
- Radius varies by objective type

**Positioning Troops**:
1. Fly/drive into the capture zone
2. Land or stop vehicle
3. Unload troops using F10 → Troops → Unload
4. Troops must remain in zone

**Requirements**:
- Infantry must be **on the ground** and **alive**
- Troops still in helicopters don't count
- Dead troops don't count
- Objective must NOT be threatened (cannot unload at threatened objectives)

### Step 4: Hold the Zone Until the Timer Runs

**Capture Timer**:
- Base capture time is **180 seconds** of holding the zone with all
  conditions met.
- **Bring more squads to go faster.** The timer is divided by the number of
  capturing troop groups in the zone: 1 group ≈ 180 s, 2 ≈ 90 s, 3 ≈ 60 s
  (floored at 30 s — it never goes instant).
- If enemy troops enter, or your troops die/leave, or logistics repairs the
  objective back above the threshold, the timer pauses/resets.

### Step 5: Consolidate

The base flips owner the moment the timer completes — but it isn't fully
yours yet. Your assault troops **stay in the zone and hold** for a
**consolidation window** (~5 minutes) while the new garrison moves in:

- The F10 label runs a live countdown: **"CONSOLIDATING 42% (3m left) — hold
  the zone with troops"**. You **cannot slot in** at the base until it
  finishes; trying tells you how long is left.
- Your troops must be **physically inside the zone**. Fly them out and the
  clock stops — the label switches to **"CONSOLIDATION PAUSED — no troops in
  the zone"**. Progress is never lost, it just stops accruing until they are
  back in. A few seconds outside is covered by a grace window, so ordinary
  repositioning doesn't stall you.
- **You can beat the clock.** Consolidation is progress, not a wall clock:
  - each **extra squad** holding the zone adds 50% to the rate (two squads
    consolidate in ~2/3 the time, three in half)
  - landing a **Logistics Repair Kit** or a **supply crate** at the base
    jumps it forward **~2 minutes outright** — even if the base's logistics
    are already at 100%, because the point is the sortie
  - so flying the logistics run is faster than orbiting and waiting
- **The enemy cannot start a fresh capture timer** against a base in its
  hold. To take it back they have to physically **wipe out your holding
  troops** (air, artillery, or their own troops fighting yours).
- If your holding troops are killed **and** the base is still shot up
  (Health ≤ 20%), it drops to **Neutral** (contested) and must be taken
  again from scratch.
- If your **new garrison is standing** (Health > 20% — see "After Capture"
  below), the base **consolidates and stays yours** even if the assault
  troops die. The assault force did its job.
- If your troops survive the whole window, it consolidates normally.
- Either way, once consolidated the holding troops are removed and the
  garrison takes over.
- For ~2 minutes after any capture (or Neutral flip) a **cooldown** blocks
  a new capture timer entirely, giving the new owner time to set up.

## Success!

When capture succeeds:

**System Announces**:
- Team-wide notification
- Objective changes color on map
- New owner displayed

**Rewards**:
- Points awarded to capturing players
- Points divided among all participants
- Capture points vary by objective value

**Immediate Effects**:
- Airbase coalition changes
- **The previous owner's garrison is overrun** — any of their surviving
  armour, AAA, SAM, or infantry at the base is destroyed on the spot
- **You get a light garrison back** — AAA and infantry first, a bit of
  armour, at roughly 25% health. **No SAMs.** The base's SAM cover (and the
  rest of the garrison) rebuilds slowly through auto-repair, or you fly it
  in with deployable crates. So a freshly-taken base is a soft target that
  you have to build up — and it can still be contested from the air right
  after it flips.
- Logistics begins one step of repair; services follow shortly
- Supply lines and warehouse stock transfer to the new owner

## Failed Captures

Captures can fail if:

**Logistics Restored**:
- Automatic repair can raise logi above 0%
- Capture becomes impossible until logi reduced again

**Troops Killed**:
- All capturing troops die before the timer completes → deploy fresh troops
- All *holding* troops die during the consolidation window **and** the base
  is still at Health ≤ 20% → it goes **Neutral** and must be re-captured. If
  your new garrison has already brought it above 20%, it consolidates and
  stays yours.

**Zone Contested**:
- Enemy troops enter the zone
- Capture pauses until contest resolved

**Troops Leave Zone**:
- Troops moved out by player
- Troops ordered to relocate
- Reset capture progress

## Special Capture Mechanics

Not every objective type captures the same way as a standard airbase/FARP/FOB. A few are special cases:

### Capturing a SAM Site

SAM sites capture **the instant** your troops hold the zone — there is **no capture timer**. But the site's launchers, radars, and infantry are still live threats: clear them first or your troop transport won't survive the approach.

- Position is classified — not shown on the F10 map or dashboard, so you have to find it yourself (EWR tracks help)
- Ownership fully flips on capture — a site that was defending the enemy (or sitting neutral) becomes yours outright
- Once it reactivates, it re-arms with your coalition's own loadout and starts covering *your* airspace instead of theirs

### Capturing a Naval Carrier Group

A carrier can't be boarded while it's combat-effective:

1. **Disable it**: Knock its logistics rating to 0 by sinking its escort and support ships
2. **Take it**: Either move capture-capable troops aboard directly, or simply take the naval base it's linked to (every carrier auto-links to the nearest friendly naval base at mission start)

A disabled carrier flips to whoever holds its naval base, in whichever order the two fall — destroy the carrier then take the base, or take the base first and finish the carrier off later, both work.

On capture the enemy task force is **replaced by your own coalition's carrier group** in the same location, with your deck slots — the carrier group is now yours to sail and fly from. It comes across at **50%** and needs repairing before it's fully operational:

- **Carrier repair crates** — request them from the cargo menu, then air-drop or fly them out to the carrier. Stack several to cut the repair time (roughly 30 minutes on a single crate).
- **Repair Carrier / Respawn Carrier** actions — where the server configures them, off the linked naval base's actions menu and paid for out of that base's supplies. Respawn needs the carrier fully sunk first. They are not part of the live mission's action set; see [Carrier Operations](./carrier-ops.md).
- If the carrier's naval base stays friendly and stocked, it also **auto-repairs** on its own over time.

**Note**: A captured carrier keeps whatever aircraft it had aboard, even types your coalition doesn't otherwise have access to — but those "foreign" airframes stay grounded until the carrier's repairs finish (you'll get a "flyable once carrier repairs finish" message if you try to slot one early). Your own coalition's normal aircraft are never affected by this.

### Last Stand

When a coalition is reduced to its **last primary objective** (an airbase, naval base, or FARP), a countdown timer arms. If they can't retake ground before it expires, that coalition loses the round outright.

### What Capture Does On Success

Capturing an airbase or naval base flips its coalition and repairs one step of its logistics and services automatically. Warehouse stock and supply routes transfer to the new owner immediately. The previous owner's surviving combat units at the base are destroyed. The new owner gets back only a **light garrison — AAA and infantry, ~25% health, no SAMs**; everything heavier rebuilds slowly through auto-repair or has to be delivered by crate. This is deliberate: a freshly-taken base is a soft target you invest logistics into, not an instant fortress, and it can still be worked over from the air right after the flip. (`capture_garrison_revive_fraction`, and `capture_garrison_revive_include_sam` if a server wants SAMs back too.)

## Point Rewards

Capture points are awarded based on:
- Objective strategic value
- Server point configuration
- Number of participants

**Example**:
```
Capture objective: 50 points
3 participants: ~17 points each
1 participant: 50 points
```

Points divided equally among all players whose troops participated in the capture.

## After Capture

When capture succeeds:
- Objective changes owner immediately
- Airbase coalition switches
- The previous garrison is wiped; you get a light AAA/infantry garrison
  (~25%, no SAMs) and rebuild the rest via logistics or crates
- Logistics repairs one step automatically
- Supply lines recalculate
- Capturing troops stay to hold the zone through the consolidation window,
  then are removed once the garrison is established
- The base is on a ~2-minute capture cooldown; the enemy can't immediately
  start taking it back
- Points awarded to participants

## Stuck? Ask the engine

Before flying another sortie at a base that won't flip, open
**`F10 → Objectives → Capture Advisor: Nearest`**. It prints the exact blockers,
whether a cooldown is running, whether the base is out-repairing you, and which
of your troops are in the zone — including any that **cannot capture because of
their troop type**. See [Objectives Menu](../f10-menu/objectives.md).

## See Also

- [Objectives Menu](../f10-menu/objectives.md) — the Capture Advisor
- [AI Helo Missions](../advanced/helo-missions.md) — getting capture troops into a zone without flying them yourself
- [The Tasking Board](./tasking-board.md) — posting a CAPTURE task that closes itself out
- [Logistics & Supply](./logistics.md) — holding what you took
- [Materiel & the War Economy](./war-economy.md) — captured airframes, and why a fresh base can't repair
- [Deployable Units](../reference/deployables.md) — which troop types can actually capture
- [Carrier Operations](./carrier-ops.md)
