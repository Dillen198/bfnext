# Objectives

Objectives are the heart of the Fowl Engine dynamic campaign. Understanding how they work is essential for strategic success.

## What Are Objectives?

Objectives represent strategic locations on the battlefield:
- **Airbases**: Major air facilities with full services
- **FARPs**: Forward Arming and Refueling Points
- **FOBs**: Forward Operating Bases for ground operations
- **Logistics Hubs**: Supply distribution centers
- **Factories**: Produce ground units over time
- **Naval Bases**: Spawn and resupply naval assets
- **SAM Sites**: High-value integrated air defense, hidden and networked
- **Command Centers**: IADN network nodes that keep nearby SAM sites smart

![Overview of objective types across the campaign map](/api/wiki/images/934a41dc-d2ad-4b52-8dfd-60cbdc4deb78)

## Objective Types

### Airbases
- Full repair and rearm capabilities
- Spawning locations for aircraft
- Critical for air superiority
- Highest strategic value

### FARPs (Forward Arming and Refueling Points)
- Forward helicopter bases
- Limited repair capabilities
- Mobile deployment possible
- Tactical importance

### FOBs (Forward Operating Bases)
- Ground unit staging areas
- Limited but essential support
- Strategic positions for ground war
- Supply storage

### Logistics Hubs
- Central supply distribution
- Connect to multiple objectives
- Critical for sustained operations
- Often heavily defended

### Factories
- Produce ground units over time for their owning coalition
- Destroy enemy factories to starve their offensive
- Distinct from Logistics Hubs (supply) — factories generate new units

### Naval Bases
- Spawn and resupply naval assets
- Control coastal and maritime operations
- Every carrier group auto-links to its nearest friendly naval base — see [Capturing Objectives](./capturing-objectives.md) for how that link affects capturing a carrier

![Naval carrier strike group at sea](/api/wiki/images/f2ad53a9-2e98-4925-bced-de97267fc7e6)

### SAM Sites
High-value integrated air defense sites, networked with other sensors:
- Search radars stay dark until a real threat is confirmed, then power up
- **Position is classified** — not shown on the F10 map or the dashboard; you have to locate it yourself (EWR tracks help)
- Ownership can flip either way, including from neutral — a captured site re-arms under its new owner's coalition and defends *their* airspace, not the enemy's
- Captures instantly once troops hold the zone (no capture timer) — see [Capturing Objectives](./capturing-objectives.md)

![Hidden SAM site tucked into rocky terrain](/api/wiki/images/0ce0f6d9-ab3e-4f5b-87e3-799b0e2964cd)

**Warning**: SAM sites don't just sit there with radars blaring — they share detections across the coalition's whole sensor network and only light up their radar for a confirmed threat, so a "quiet" site isn't necessarily undefended. Fire a HARM or other anti-radiation missile at one and it'll go dark to deny you a lock — and some sites keep a short-range point-defense system (Pantsir, Shilka) alert the entire time specifically to shoot your missile down before it arrives. Don't treat radar silence as an all-clear.

### Command Centers

The network node behind the IADN (Integrated Air Defence Network) described above. A Command Center is a pure trigger-zone objective — no airbase or pad association, no warehouse, no supply-chain consumption of its own — whose only job is keeping nearby SAM sites smart.

- At mission start, every SAM site auto-links to its **nearest friendly Command Center**
- As long as that Command Center is alive and still owned by the same coalition, its linked SAM sites get the full networked behavior: shared sensor picture, radars dark until a confirmed threat, HARM defense
- **If the Command Center is destroyed or captured, every SAM site linked to it immediately loses that networking** — it falls back to plain DCS AI (`Auto` alarm state: always-on radar, no coordination, no HARM-defense reaction), whether or not the SAM site itself was ever touched

**Tactical implication**: a Command Center is a legitimate high-value target in its own right. Taking one out (or capturing it) quietly de-fangs every SAM site it was covering — often a cheaper way to open a corridor through a defended sector than trying to individually locate and kill each classified SAM position first. The flip side applies when defending: protect your Command Centers as hard as the SAM sites themselves, since losing one degrades a whole cluster of air defense at once.

## Objective Ownership

### Current Owner
Each objective is controlled by:
- **Blue Coalition**
- **Red Coalition**
- **Neutral** (rare, usually initial state)

### Ownership Display
Check ownership via:
- **F10 Map Markers**: Color-coded (Blue/Red)
- **Objective Name**: Prefix indicates owner
- **JTAC Reports**: Include ownership info

## Objective Status

### Health
Indicates physical damage to facilities:
- **100%**: Fully operational
- **75-99%**: Minor damage
- **50-74%**: Moderate damage
- **25-49%**: Heavy damage
- **0-24%**: Critical condition

**Effects of Low Health**:
- Reduced repair speeds
- Limited aircraft spawns
- Slower logistics processing

### Logistics (Logi)
Represents infrastructure for supply operations:
- **0**: Completely destroyed, **can be captured**
- **1-100**: Infrastructure present, **cannot be captured**

**Key Rule**: An objective can only be captured when its Logi is at **0%** (destroyed).

### Supply Level
Resources available for operations:
- **100%**: Fully supplied
- **50-99%**: Adequate supplies
- **25-49%**: Low supplies
- **0-24%**: Critical shortage

### Fuel Level
Aviation fuel availability:
- **100%**: Full fuel stocks
- **0%**: No fuel available

## Objective States

### Threatened
An objective becomes "threatened" when:
- Enemy units are nearby (within aircraft-specific threat distance)
- Enemy ground forces are close
- Recently captured by enemy

**Cooldown**: 300 seconds (5 minutes) - objective stays threatened for 5 minutes after last enemy contact

**Effects**:
- ⚠️ **Blocks cargo and troop deployment** - cannot unload at threatened objectives!
- May trigger defensive unit spawns
- Tracked internally by system

**Important**: You cannot deploy units (troops or crates) at threatened objectives. The system will show: "you can't deploy troops here while enemies are near"

### Capturable
Ready to be captured when:
- Logi = 0
- Troops in capture zone
- Correct troop type present

**Visual Indicator**: Capturable objectives show a **white circle** on F10 map instead of the owner's color.

## Objective Information

## Reading Map Markers

Typical objective marker format:
```
Musa Airbase
Health: 100
Logi: 100
Supply: 99
Fuel: 100
Points: 0
```

Breakdown:
- **Objective name** - First line
- **Health**: 100 - Facility condition (0-100)
- **Logi**: 100 - Infrastructure (0-100, must be 0 to capture)
- **Supply**: 99 - Equipment stocks (0-100)
- **Fuel**: 100 - Fuel stocks (0-100)
- **Points**: 0 - Point value for capturing

**Capturable Example**:
```
Enemy Base
Health: 65
Logi: 0
Supply: 45
Fuel: 30
Points: 0
```
Note `Logi: 0` means this objective CAN be captured!

**Visual Indicator**: When an objective becomes capturable (Logi: 0), the **circle around the airbase on the F10 map turns WHITE** instead of the owner's color. This is an easy way to spot capturable objectives at a glance!

## Objective Zones

### Capture Zone
The physical area for capturing:
- Defined by mission design (circular or polygonal)
- Infantry must be inside to capture
- Check F10 map markers for zone location
- Only one zone per objective

## Next Steps

Learn how to [capture objectives](./capturing-objectives.md) and manage the [logistics system](./logistics.md).
