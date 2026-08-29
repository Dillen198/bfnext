# Capturing Objectives

Capturing objectives is the primary way to win the campaign. This guide covers the complete capture process.

## Prerequisites for Capture

An objective can only be captured when **ALL** of these conditions are met:

1. **Health ≤ 20%** — its defending units are almost wiped out ✓
2. **No infantry left** — every infantry defender in the zone is dead ✓
3. **Capture troops in zone** ✓
4. **Troops are correct type** ✓
5. **No enemy contest** ✓

Health and infantry — not logistics — gate the capture. Logistics still
matters for repair speed and the garrison, but you take a base by killing
its defenders.

## The Capture Process

### Step 1: Grind it down to ≤ 20% health, no infantry

Destroy the objective's defending units — armour, AAA, SAMs, and especially
the **infantry**, which must be completely eliminated (one surviving squad
blocks the capture):

**Ground Strikes**: CAS with bombs, rockets, guns, GBUs, AGMs — sweep the
zone for infantry.

**Artillery**: JTAC-directed artillery / MLRS against troop concentrations.

**Cruise / Ballistic Missiles**: long-range strikes via the Actions menu.

**Monitoring Progress**:
- The F10 map marker shows Health % and Infantry %
- **The inner ring turns WHITE** on the F10 map when capturable (instead of
  the owner's colour)
- If it isn't taking, the system says why — e.g. *"health still above 20%
  (34%)"* or *"enemy infantry still defending (12% left)"*

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
- **Bring more squads to go faster** — the timer is divided by the number
  of capturing troop groups in the zone (1 ≈ 180 s, 2 ≈ 90 s, 3 ≈ 60 s,
  floored at 30 s).
- Enemy troops entering, your troops dying/leaving, or logistics repairing
  the objective all pause or reset the timer.

### Step 5: Consolidate

The base flips owner when the timer completes, but your assault troops
**stay and hold** for a **~5-minute consolidation window** while the
garrison moves in:

- The F10 label reads **"NOT CONSOLIDATED — hold with troops or it goes
  Neutral"**.
- If the enemy wipes out your holding troops first, the base goes
  **Neutral** and must be taken again from scratch.
- If they survive the window, the garrison spawns, the base is firmly
  yours, and the holding troops are removed.

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
- Defensive units respawn for new owner
- Logistics begins one step of repair
- Supply lines recalculate

## Failed Captures

Captures can fail if:

**Logistics Restored**:
- Automatic repair can raise logi above 0%
- Capture becomes impossible until logi reduced again

**Troops Killed**:
- All capturing troops die
- Need to deploy fresh troops

**Zone Contested**:
- Enemy troops enter the zone
- Capture pauses until contest resolved

**Troops Leave Zone**:
- Troops moved out by player
- Troops ordered to relocate
- Reset capture progress

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
- Supply lines and warehouse stock transfer to the new owner
- Capturing troops stay to hold the zone through the consolidation window,
  then are removed once the garrison is established
- Points awarded to participants

## Next Steps

Learn about the [Logistics & Supply](./logistics.md) system to maintain your captured objectives!

For detailed troop specifications, see the [Deployable Units Reference](../reference/deployables.md).

