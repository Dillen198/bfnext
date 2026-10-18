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

Access via **F10 → Cargo**. Which menu you get depends on your airframe.

**Heavy helicopters (Mi-8, UH-1H, CH-47, Mi-24P on the live mission)** carry
crates as DCS cargo: you load and unload them through the ground crew, not
the F10 menu.

```
F10 → Cargo
├── Unpack Nearby Crate(s)   build the deployable from crates on the ground
├── List Nearby Crates
├── List Cargo               what's aboard
├── Destroy Nearby Crate     clear a mis-dropped or unwanted crate
├── Pack Up Fat Cow          (only where the server has Fat Cows)
├── CSAR >>                  downed pilots (see Combat Search & Rescue)
└── Crates
    ├── Base Supply          supply crates, and the Emergency Repair crate
    │                        where the server enables it
    └── <category> → <unit>(<cost> pts) → <crate> xN → 1 … 9
```

**Other cargo airframes (such as the Gazelle SA342L)** use the classic crate
menu, which loads and unloads through F10:

```
F10 → Cargo
├── Unpack Nearby Crate(s)
├── Load Nearby Crate
├── Unload Crate
├── List Nearby Crates
├── List Cargo
├── Destroy Nearby Crate
└── Crates → Base Supply, and <category> → <unit>(<cost> pts) → <crate>
```

A unit that needs more than one of a crate shows the count after the crate
name in brackets, for example `<crate>(2)`. Where there's an **xN** submenu, pick
1–9 to spawn that many at once.

The C-130 gets its own **C-130 Cargo** menu instead, with `List Nearby Crates`,
`Delete Nearby Crate`, `Delete All Nearby Crates` and a **Vehicles** category —
see [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md). See
[Emergency repair crates](../gameplay/logistics.md#emergency-repair-crates) and
the [Fat Cow](../advanced/deployables-guide.md#fat-cow-heavy-lift-farp).

## Loading Cargo

1. Spawn crates from **Crates** while you're at a friendly objective, or find
   ones already on the ground.
2. Land near the crate: within **{{cfg:crate_load_distance|100}} m**.
3. **Heavy helicopters:** load it through **F8 → Ground Crew → Cargo**.
   **Classic menu:** **F10 → Cargo → Load Nearby Crate**.
4. A message confirms the load. **List Cargo** shows what's aboard.

**Crate limit**: you may have up to **{{cfg:max_crates|8}}** crate sets
outstanding at once. Spawned crates are scattered within
{{cfg:crate_spread|250}} m.

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

1. Land where you want the crate. To build a unit, that's where it will go;
   supply crates go to a friendly objective.
2. **Heavy helicopters:** unload through **F8 → Ground Crew → Cargo**.
   **Classic menu:** **F10 → Cargo → Unload Crate**.
3. With every crate the unit needs on the ground, use **F10 → Cargo →
   Unpack Nearby Crate(s)**.

**Restrictions**:
- ⚠️ You can't deploy at a **threatened objective**: "you can't deploy troops
  here while enemies are near". Wait for it to clear
  ({{cfg:threatened_cooldown|300}} s after enemies leave).
- Each crate has a maximum drop height and speed; exceed them and the drop
  fails, with the limits reported back to you.

## See Also

- [Deployable Units Reference](../reference/deployables.md) - Complete crate list
- [Logistics System](../gameplay/logistics.md)
- [Troop Transport](./troops.md)
