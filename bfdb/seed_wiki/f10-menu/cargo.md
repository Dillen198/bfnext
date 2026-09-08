# Cargo Operations

Transport supplies and equipment using the cargo system to support your team's war effort.

![Transport helicopter carrying a slung supply crate over the front line](/api/wiki/images/fe035065-f175-4f73-babe-9681513a3b93)

## Overview

The cargo system lets you:
- Load crates at objectives
- Transport to other locations
- Unload at friendly objectives
- Resupply forward positions

## Requirements

**Aircraft**: Must have cargo capability
- Helicopters: Mi-8, UH-60, CH-47, etc.
- Fixed-wing: C-130 (if available)
- Check aircraft specifications

**Location**: Must be near objective/FARP to load

## Cargo Menu

Access via F10 → Cargo

**Menu Options**:
- **Load**: Pick up cargo crates
- **Unload**: Deliver cargo
- **Status**: Check loaded cargo

## Loading Cargo

**Steps**:
1. Land near objective/FARP (within ~50m)
2. F10 → Cargo → Load
3. Select cargo type
4. Cargo loads automatically
5. Message confirms load

**Cargo Types**:
- Supply crates
- Equipment crates
- Fuel bladders
- Special cargo (server-dependent)

**Capacity**:
- **CH-47**: 6 crate slots, 10 troop slots
- **Mi-8**: 3 crate slots, 6 troop slots
- **UH-1H**: 2 crate slots, 3 troop slots
- **SA342L / SA342 Minigun**: 1 crate slot, 1 troop slot
- **Mi-24P**: 1 crate slot, 1 troop slot

> **SA342M** and **SA342 Mistral** cannot carry crates or troops — no cabin space.
> Use the **SA342L** or **SA342 Minigun** for Gazelle logistics runs.

## Transporting Cargo

**During Flight**:
- Cargo stays loaded
- Affects aircraft weight
- May impact performance
- Don't crash!

**Tips**:
- Plan flight route
- Check weather/threats
- Have escorts if needed
- Stay low and fast in danger areas

## Unloading Cargo

**Steps**:
1. Land at destination objective
2. Must be friendly objective
3. **Objective must NOT be threatened** ⚠️
4. F10 → Cargo → Unload
5. Select cargo or "Unload All"
6. Cargo transfers to objective

**Restrictions**:
- ⚠️ Cannot unload at **threatened objectives**
- Error: "you can't deploy troops here while enemies are near"
- Wait 5 minutes after enemies leave for cooldown
- Ensure area is clear before unloading

**Effects**:
- Increases objective supply
- Contributes to logistics
- Supports operations

## See Also

- [Deployable Units Reference](../reference/deployables.md) - Complete crate list
- [Logistics System](../gameplay/logistics.md)
- [Troop Transport](./troops.md)
