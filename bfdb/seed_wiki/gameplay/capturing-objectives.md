# Capturing Objectives

Capturing objectives is the primary way to win the campaign. This guide covers the complete capture process.

![Troops fast-roping from a helicopter to capture an objective](/api/wiki/images/3ce5c418-9d0b-429a-be7e-687032cb147f)

A round is won by **territory, not kill count**. The coalition that controls the required share of all objectives on the map wins the round — contested or neutral objectives don't count toward either side.

## Prerequisites for Capture

An objective can only be captured when **ALL** of these conditions are met:

1. **Logistics = 0%** ✓
2. **Capture troops in zone** ✓
3. **Troops are correct type** ✓
4. **No enemy contest** ✓

## The Capture Process

### Step 1: Reduce Logistics to 0%

Logistics must be destroyed before capture. Methods:

**Ground Strikes**:
- Attack logistics buildings with bombs
- Use precision weapons (GBUs, AGMs)
- Target specific logi structures

**Artillery**:
- JTAC-directed artillery fire
- Area bombardment
- Sustained fire missions

**Cruise Missiles**:
- Long-range strikes
- High accuracy on logi buildings
- Coordinate via Actions menu

**Monitoring Progress**:
- Check F10 map marker: `Logi: 0` indicates ready (fully destroyed)
- **Circle turns WHITE** on F10 map when capturable (instead of owner's color)
- System announces when objective becomes capturable

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

### Step 4: Hold the Zone

**Capture Timer**:
- Capture is not instantaneous
- System checks periodically
- All conditions must remain met until capture completes

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

**Note**: A captured carrier keeps flying whatever aircraft it had aboard, even types your coalition doesn't otherwise have access to — but those "foreign" airframes are grounded until the carrier's repairs finish. Your own coalition's normal aircraft are never affected by this.

### Last Stand

When a coalition is reduced to its **last primary objective** (an airbase, naval base, or FARP), a countdown timer arms. If they can't retake ground before it expires, that coalition loses the round outright.

### What Capture Does On Success

Capturing an airbase or naval base flips its coalition and repairs one step of its logistics and services automatically. Warehouse stock and supply routes transfer to the new owner immediately.

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
- Capturing troops are deleted
- Points awarded to participants

## Next Steps

Learn about the [Logistics & Supply](./logistics.md) system to maintain your captured objectives!

For detailed troop specifications, see the [Deployable Units Reference](../reference/deployables.md).
