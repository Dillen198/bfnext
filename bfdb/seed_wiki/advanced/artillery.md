# Artillery Missions

Coordinate devastating artillery strikes using the JTAC system.

![HIMARS rocket artillery firing on a target](/api/wiki/images/b7c7cf5d-7559-4a43-b97e-b304cc4d8ccb)

**Also via chat**: `-jtac <id> arty <id|all> <n>` directs a specific gun or every friendly battery in range to fire N rounds at the JTAC's current target — same system as the F10 menu flow below, just faster once you know the syntax.

## Overview

Artillery provides:
- Indirect fire support
- Area suppression
- Precision strikes (with JTAC)
- Cost-effective firepower

## Requirements

1. **JTAC unit** with target
2. **Artillery battery** in range — this includes **missile TELs** (Scud,
   Iskander/9K720, Silkworm) as well as tube guns and MLRS
3. **Clear line of fire**
4. **Ammunition**

Each unit type has its own minimum/maximum range in the server config, so a
Scud won't accept a target inside its ~50 km minimum, and a 122 mm gun won't
accept one 40 km away. If a battery is out of range the JTAC menu tells you
which one and why.

## Requesting Fire

**Via F10 Menu**:
```
F10 → JTAC → [JTAC ID] → Artillery → [Battery ID] → [Rounds]
```

**Process**:
1. JTAC identifies target
2. Select a battery — or **Fire All Groups Together** to salvo every battery
   in range at once
3. Choose rounds: **1 / 3 / 5 / 10 / all ammo** ("all ammo" dumps each gun's
   full remaining load)
4. The battery turns to face the target if it needs to, then fires
5. Rounds impact near target

> **Turning to fire**: fixed- or limited-traverse launchers (Grad, Smerch,
> Scud, Silkworm…) spawn facing north. When you call fire they now reposition
> a few metres to bring the launcher onto the target bearing before firing,
> so shots that used to silently fail now land.

## Fire Adjustment

**After Initial Volley**:
```
F10 → JTAC → [JTAC ID] → Artillery → [Battery] → Adjust Fire
```

**Adjustments**:
- **Short**: Increase range
- **Long**: Decrease range
- **Left**: Shift left
- **Right**: Shift right

**Typical**: 50-100m per adjustment

## Fire Missions

### Registration Fire

**Purpose**: Establish accuracy

**Process**:
1. Fire 1-3 rounds
2. Observe impacts
3. Adjust as needed
4. Note corrections

### Fire for Effect

**Purpose**: Maximum damage

**Process**:
1. After registration
2. Fire 5-10+ rounds
3. Saturate area
4. Assess damage

### Suppression

**Purpose**: Keep enemy pinned

**Process**:
1. Continuous fire
2. Area coverage
3. Prevents movement
4. Supports friendly advance

## Ammunition Management

### Checking Ammo

**JTAC Status shows**:
```
available artillery: [54321(25)]
```
= Battery 54321 with 25 rounds

### Conservation

- Use minimum effective rounds
- Save for critical moments
- Resupply via logistics
- Don't waste on low-value targets

## Artillery Specifications

**Range**: Varies hugely by unit — towed/self-propelled guns and MLRS reach roughly 4-70km, while tactical ballistic systems (e.g. ATACMS, Iskander, Scud) can range into the hundreds of kilometres but have a large *minimum* range too. The JTAC menu enforces each battery's real min/max and tells you which guns are too close or too far for the current target.

**Fire-for-Effect Scatter**: Impacts scatter across roughly a 200m radius around the target point.

**Simultaneous Batteries**: Up to 3 friendly artillery groups can fire simultaneously per mission.

**Adjustment**: 50-100m per adjustment (Short/Long/Left/Right)
**Time of Flight**: Varies by range (~30-60 seconds)

## Current Mission Deployable Artillery & Range

Every artillery battery and missile TEL deployable via crates on this server, with
the exact min/max fire range the JTAC menu enforces — pulled live from the server's
`ODFv2_CFG`. Battery names below match what you'll see in the F10 → Deploy menu and
the JTAC ammo listing. See [Deployable Units Reference](../reference/deployables.md)
for crate weights and deploy limits on every deployable, not just artillery.

### RED

| Battery | Type | Min Range | Max Range | Crates to Deploy | Limit |
|---|---|---|---|---|---|
| SPH 2S19 Msta | Tube, 152mm | 30 m | 23.5 km | 2 | 15 |
| PLZ-05 | Tube, 152mm | 60 m | 23.5 km | 2 | 15 |
| MLRS Smerch CM | MLRS | 20 km | 70 km | 3 | 10 |
| MLRS Smerch HE | MLRS | 20 km | 70 km | 3 | 10 |
| MLRS TOS-1A | Thermobaric MLRS | 400 m | 6 km | 2 | 15 |
| MLRS GRAD | MLRS | 5 km | 19 km | 2 | 15 |
| MLRS 9K57 (Uragan) | MLRS | 11.5 km | 36 km | 2 | 15 |
| SRBM 9K720 CM (Iskander) | Ballistic TEL | 75 km | 400 km | 4 | 10 |
| SRBM 9K720 HE (Iskander) | Ballistic TEL | 75 km | 400 km | 4 | 10 |
| SSM Scud | Ballistic TEL | 50 km | 285 km | 3 | 10 |

### BLUE

| Battery | Type | Min Range | Max Range | Crates to Deploy | Limit |
|---|---|---|---|---|---|
| Firtina 155MM | Tube, 155mm | 0 m | 41 km | 2 | 15 |
| Dana 152MM | Tube, 152mm | 30 m | 18.5 km | 2 | 15 |
| MLRS M270 | MLRS | 10 km | 32 km | 2 | 15 |
| Himars GMLRS HE | MLRS | 15 km | 70 km | 3 | 10 |
| Himars GMLRS CM | MLRS | 15 km | 70 km | 3 | 10 |
| Himars ATACMS HE | Ballistic TEL | 50 km | 300 km | 3 | 10 |
| Himars ATACMS CM | Ballistic TEL | 50 km | 300 km | 3 | 10 |

> Any battery not in these lists (e.g. mission-placed statics like a Silkworm coastal
> launcher) isn't spawnable via crates but will still take JTAC fire missions if one
> happens to be on the map. Ranges shown are the config's `min_range_m`/`max_range_m`
> per unit; anything not explicitly listed falls back to the server default of 4-30 km.

## See Also

- [JTAC System](../f10-menu/jtac.md)
