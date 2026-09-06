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

> Health/infantry — not logistics — is what gates capture. Logistics still
> matters (it drives repair speed and the objective's own garrison), but you
> capture a base by killing its defenders, not by emptying its warehouse.

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

**Getting Capture Troops** (PG Tempest):

Troops that **CAN capture**:
- **Standard Infantry**: 0 points, 8km JTAC (no line-of-sight)
- **Anti-Tank Infantry**: 2 points, 8km JTAC (line-of-sight required)
- **Mortar Infantry**: 5 points, 8km JTAC (line-of-sight required)

Troops that **CANNOT capture**:
- **Igla/Stinger (MANPADS)**: 25 points

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
yours yet. Your assault troops **stay on the ground and hold** for a
**consolidation window** (~5 minutes) while the new garrison moves in:

- The F10 label shows **"NOT CONSOLIDATED — hold with troops or it goes
  Neutral"** during this window.
- If the enemy wipes out your holding troops before it consolidates, the
  base goes **Neutral** (contested) — nobody owns it, and it has to be
  taken again from scratch.
- If your troops survive the window, the garrison spawns and the base is
  firmly yours. The holding troops are then removed.

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
- Logistics begins one step of repair
- Supply lines and warehouse stock transfer to the new owner
- The garrison respawns for the new owner **only once your troops finish
  consolidating** (see Step 5)

## Failed Captures

Captures can fail if:

**Logistics Restored**:
- Automatic repair can raise logi above 0%
- Capture becomes impossible until logi reduced again

**Troops Killed**:
- All capturing troops die before the timer completes → deploy fresh troops
- All *holding* troops die during the consolidation window → the base goes
  **Neutral** and must be re-captured

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
- **Repair Carrier / Respawn Carrier** actions — off the linked naval base's actions menu, paid for out of that base's supplies. Respawn needs the carrier fully sunk first.
- If the carrier's naval base stays friendly and stocked, it also **auto-repairs** on its own over time.

**Note**: A captured carrier keeps whatever aircraft it had aboard, even types your coalition doesn't otherwise have access to — but those "foreign" airframes stay grounded until the carrier's repairs finish (you'll get a "flyable once carrier repairs finish" message if you try to slot one early). Your own coalition's normal aircraft are never affected by this.

### Last Stand

When a coalition is reduced to its **last primary objective** (an airbase, naval base, or FARP), a countdown timer arms. If they can't retake ground before it expires, that coalition loses the round outright.

### What Capture Does On Success

Capturing an airbase or naval base flips its coalition and repairs one step of its logistics and services automatically. Warehouse stock and supply routes transfer to the new owner immediately, and the new owner's garrison is revived on the spot so the base isn't left sitting at 0% health, wide open to being taken straight back.

## Point Rewards

Capture points are awarded based on:
- Objective strategic value
- Server point configuration
- Number of participants

**Example** (PG Tempest):
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
- Logistics repairs one step automatically
- Supply lines recalculate
- Capturing troops stay to hold the zone through the consolidation window,
  then are removed once the garrison is established
- Points awarded to participants

## Next Steps

Learn about the [Logistics & Supply](./logistics.md) system to maintain your captured objectives!

For detailed troop specifications, see the [Deployable Units Reference](../reference/deployables.md).
