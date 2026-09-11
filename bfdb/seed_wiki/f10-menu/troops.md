# Troop Transport

Move infantry units to capture objectives, reinforce positions, and support ground operations.

![Infantry squad loaded into a transport helicopter for an objective capture run](/api/wiki/images/a645d067-42f4-4059-a31a-e2e5e378cfa0)

## Overview

Transport infantry via helicopter or ground vehicle:
- Load troops at friendly objectives
- Move to target location
- Unload for capture or defense
- Critical for objective capture

## Requirements

**Transport Capability**:
- Helicopters: Mi-8, UH-60, CH-47, etc.
- APCs/IFVs: BTR, BMP, Bradley, etc.
- Check vehicle specifications

**Proximity**: Must be near troops/objective

## Troops Menu

Access via F10 → Troops

**Menu Options**:
- **Load**: Pick up infantry
- **Unload**: Drop off troops
- **Status**: Check loaded troops

## Loading Troops

**Steps**:
1. Position near objective/FARP
2. F10 → Troops → Load
3. Select troop type
4. Troops board automatically
5. Confirmation message

**Troop Types**:
- Infantry squads (can capture)
- Assault troops
- Special forces
- Anti-tank teams
- Support units

**Capacity**:
- **C-130J**: 20 troop slots
- **CH-47**: 10 troop slots (largest helo)
- **Mi-8**: 6 troop slots
- **UH-1H**: 3 troop slots
- **SA342L / SA342 Minigun / Mi-24P**: 1 troop slot each
- Each slot = one infantry squad
- APCs vary by type

> **Gazelle variants**: only the **SA342L** (scout) and **SA342 Minigun** carry
> troops/cargo — they fly logistics lives. The **SA342M** (HOT/Viviane) and
> **SA342 Mistral** have no usable cabin space and fly attack lives; they cannot
> load troops or crates.

**Squad weights** (added to the aircraft as internal cargo):

| Squad | Weight |
|-------|-------:|
| MANPAD (Igla / Stinger) | 150 kg |
| Standard (rifle) | 700 kg |
| Anti Tank | 750 kg |
| Mortar | 900 kg |

A half-fuel SA342L can realistically lift a single MANPAD team. A rifle or
mortar squad needs a Huey or larger.

## Transporting Troops

**Flight Operations**:
- Troops count as weight
- May affect performance
- Stay low for safety
- Fast ingress/egress

**Ground Movement**:
- APCs/IFVs can transport troops overland

## Unloading Troops

**Steps**:
1. Land/stop at destination
2. **Objective must NOT be threatened** ⚠️
3. F10 → Troops → Unload
4. Select troops or "Unload All"
5. Troops dismount
6. Troops now on ground

**Critical Restriction**:
- ⚠️ Cannot unload at **threatened objectives**!
- Error: "you can't deploy troops here while enemies are near"
- Objective stays threatened for **5 minutes** after enemies leave

**Location Matters**:
- For capture: Inside capture zone (must be unthreatened)
- For defense: Strategic positions
- For assault: Covered approaches

## Troop Management

### After Unloading

**Troop Control**:
- Troops become deployed group
- Assigned group ID
- Control via commands or F10

**Movement**:
```
-bind <troop-id>
```
Then use F10 → Actions>> → Move (Units/Troops) — the cost is in the menu label

**Deletion**:
```
-delete <troop-id>
```
Gives 50% refund

## See Also

- [Deployable Units Reference](../reference/deployables.md) - Troop types and costs
- [Capturing Objectives](../gameplay/capturing-objectives.md)
- [Cargo Operations](./cargo.md)
