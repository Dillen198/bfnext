# JTAC System

The Joint Terminal Attack Controller (JTAC) system provides advanced targeting, fire coordination, and battlefield intelligence. Close Air Support (CAS) coordinated through JTAC is one of the most effective ways to break an enemy ground advance — always coordinate before you roll in.

![JTAC calling in a 9-line CAS brief](/api/wiki/images/1e34aa38-f253-4652-b725-c30cc1553a38)

## What is JTAC?

JTACs are specialized units that:
- Detect and track enemy units
- Designate targets with laser
- Coordinate artillery and missile strikes
- Provide detailed target information

## The 9-Line Brief

When you request JTAC support, it passes a 9-line CAS brief on the tactical frequency — copy it before your attack run:

1. **IP to Target** — initial point reference
2. **Heading** from IP to Target
3. **Distance** from IP to Target
4. **Target Elevation**
5. **Target Description**
6. **Target Location** (MGRS/LL)
7. **Mark Type** (laser / smoke / IR)
8. **Friendlies** (location relative to target)
9. **Egress Direction**

**Deconfliction**: Confirm ground force positions before running in. Never run a CAS attack without positive JTAC clearance — friendlies may be inside the target area, and fratricide penalties apply. Report BDA (Battle Damage Assessment) to JTAC after each pass.

## JTAC Types

### Drone JTAC (Large - MQ-9 Reaper)
- **Range**: **18 km** (18,000m); lases out to 18.5 km
- **Line-of-sight**: Not required (can see through terrain)
- **Cost**: 100 points to deploy
- **Duration**: 12 hours
- **Function**: Long-range surveillance and targeting

### Drone JTAC (Small)
- **Range**: **12 km** (12,000m)
- **Line-of-sight**: Not required (can see through terrain)
- **Cost**: 50 points to deploy
- **Duration**: 12 hours
- **Function**: Reconnaissance and targeting

### Ground JTAC
- Infantry or vehicle-based units with JTAC capability
- 360° coverage
- May require line-of-sight (check unit type)
- Range varies by unit type

### Player JTAC
- You can act as JTAC in certain aircraft
- Access via F10 menu
- Control your own targeting
- Range depends on your aircraft sensors

## Accessing JTAC

**F10 Menu**:
1. Press F10
2. Select "JTAC"
3. Choose JTAC unit from list
4. Access that JTAC's functions

**Format**: JTACs listed by ID (e.g., "JTAC 12345")

## JTAC Status

### Checking Status

```
F10 → JTAC → [JTAC ID] → Status
```

**Status Report Shows**:
- JTAC position (bearing/distance from objective)
- Current target (if any)
- Laser code
- Visual contacts
- Nearby artillery units
- Nearby cruise missile units
- Autoshift setting
- IR pointer setting
- Filter settings
- **Radio** — for a JTAC drone, the frequency it answers on in DCS's own
  comms menu (tune it to get target coordinates / a 9-line read out)

### Reading Status

Example:
```
JTAC Reaper (12345) [1688] status
lasing T-72B code 1688 -- 312°M 6.4 km from the JTAC
position 045°M 5.2km from Batumi, lases out to 18.5 km

In laser range: T-72B x3, BMP-3 x2
Seen, out of laser range: SA-13

mode: auto, IR pointer: off
filter: [Tank, APC]
available artillery: [54321]
available ALCM: [65432(4)]
```

### Seeing vs. Lasing

A JTAC **sees** out to its detection range (a Reaper spots far beyond
18 km) but **lases** only out to **18.5 km (10 nm)**. Contacts past that are
reported under "Seen, out of laser range" and never lased. When everything it
sees is too far, the status says so instead of a bare "no target":

```
no target: 3 contact(s) in view, none in laser range. Nearest is Tor 087°M 23.4 km from the JTAC -- it lases out to 18.5 km
move it closer: -action DRONE Waypoint 12345 <mark text>
```

Move the drone (Actions → DRONE Waypoint, or `-jtac <id> move <mark text>`)
and it starts lasing on its own as soon as a target is inside 18.5 km.

## Target Management

### Shifting Targets

**Manual Shift**:
```
F10 → JTAC → [JTAC ID] → Shift Target
```
Moves laser to next detected target.

**Auto-Shift**:
```
F10 → JTAC → [JTAC ID] → Toggle Auto-Shift
```
Automatically cycles through targets.

### Target Priority

JTACs pick targets by:
1. In laser range (and inside the focus area, if one is set)
2. Unit type priority (configurable -- SAMs first on the live servers)
3. Distance (from the focus mark if set, else from the JTAC)

In auto mode a JTAC stays on its current target while it is still as
important as the best one, rather than hopping between two tanks that are
the same distance away.

### Target Filters

```
F10 → JTAC → [JTAC ID] → Filter → [Unit Type]
```

**Filter Options**:
- Tank
- APC/IFV
- Artillery
- SAM
- Helicopter
- Infantry

**Clear Filter**:
```
F10 → JTAC → [JTAC ID] → Clear Filter
```

## Laser Designation

### Laser Codes

JTACs designate targets with laser codes:
- Default: Usually 1688
- Change via F10 menu
- Must match your weapon settings

**Changing Code**:
```
F10 → JTAC → [JTAC ID] → Code → [Hundreds/Tens/Ones]
```

Example: To change from 1688 to 1511:
1. Select "1" (changes thousands to 1000)
2. Select "500" (changes hundreds to 1500)
3. Select "11" (changes to 1511)

**Important**: Set your LGB/missile code to match!

### Using Laser Designation

**Steps**:
1. Check JTAC status for code
2. Set weapon laser code to match
3. JTAC must be lasing target
4. Attack target with LGBs/Mavericks
5. Guide weapon to impact

**Tips**:
- Weapon must "see" laser
- Stay within parameters
- Don't break laser lock
- Multiple aircraft can use same code

### IR Pointer

```
F10 → JTAC → [JTAC ID] → Toggle IR Pointer
```

Adds infrared pointer:
- Visible in night vision
- Helps locate target
- Doesn't affect laser

## Fire Missions

### Artillery Missions

**Request Fire Mission**:
```
F10 → JTAC → [JTAC ID] → Artillery → [Battery ID] → [Rounds]
```

**Process**:
1. JTAC must have target
2. Artillery must be in range
3. Select rounds (1, 3, 5, etc.)
4. Battery fires on target

**Round Options**:
- Usually 1, 3, 5, 10, or "All"
- Check ammunition available
- Battery listed with ammo count

**Adjustments**:
```
F10 → JTAC → [JTAC ID] → Artillery → [Battery] → Adjust Fire
```

Options:
- Short/Long (range adjustment)
- Left/Right (lateral adjustment)
- Typically 50-100m increments

### Cruise Missile Missions

**Request ALCM Strike**:
```
F10 → JTAC → [JTAC ID] → ALCM → [Unit ID] → [Settings]
```

**Parameters**:
- Missiles per target
- Magazine expenditure
- Targets multiple contacts

**Cost**: May cost points

**See**: [ALCM Guide](../advanced/alcm.md) for details

### Smoke Marker

```
F10 → JTAC → [JTAC ID] → Smoke Target
```

**Creates smoke at target**:
- Visual marking
- Helps locate target
- 60-second cooldown
- Color varies by team

## F10 Map Intel

Any target a JTAC has eyes-on -- confirmed by line-of-sight, not just in range --
is automatically fed into the coalition's intel picture and marked on the F10
map, the same way a [Recon Pass](./recon.md) is. While the JTAC keeps watching it
the mark stays fresh; once the JTAC loses it (killed, out of range, LOS blocked),
the mark lingers for a long time (about an hour by default) before fading, so a
target a JTAC spotted stays visible well after you've moved on.
**Marks clean up after themselves.** A mark whose unit is destroyed is removed
right away, and a target that drives off doesn't leave its old mark behind —
the mark follows it. Only a target the JTAC simply loses sight of lingers and
fades as described above. This works for
every JTAC type -- ground, drone, and player -- and needs no extra setup.

## Common Issues

### "No JTAC available"
- No JTAC units deployed
- JTACs killed
- Out of detection range
- Check F10 JTAC menu

### "No target"
- JTAC hasn't detected enemies
- Everything it sees is past its 18.5 km laser range -- the status names the
  nearest contact and its distance; move the drone closer
- A filter or a focus mark excludes everything it sees (Clear the filter /
  Clear Focus)
- Line-of-sight blocked
- Wait for detection

### "Artillery out of range"
- Battery too far from target (max range: 300km)
- Deploy closer artillery
- Use different battery
- Check JTAC status for available units

### "ALCM out of range"
- Cruise missile platform too far from target (max range: 300km)
- Reposition platform using waypoint commands
- Deploy new platform closer
- Check JTAC status for available ALCM units

### "Laser not tracking"
- Wrong laser code
- Out of laser parameters
- Target moved
- JTAC lost line-of-sight

## See Also

- [Artillery Operations](../advanced/artillery.md) - Fire mission details
- [ALCM Operations](../advanced/alcm.md) - Cruise missile strikes
