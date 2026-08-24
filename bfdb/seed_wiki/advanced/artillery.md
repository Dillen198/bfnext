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
2. **Artillery battery** in range
3. **Clear line of fire**
4. **Ammunition**

## Requesting Fire

**Via F10 Menu**:
```
F10 → JTAC → [JTAC ID] → Artillery → [Battery ID] → [Rounds]
```

**Process**:
1. JTAC identifies target
2. Select artillery battery
3. Choose rounds (1, 3, 5, 10, All)
4. Battery fires
5. Rounds impact near target

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

**Range**: Varies hugely by unit — towed/self-propelled guns and MLRS reach roughly 4-70km, while tactical ballistic systems (e.g. ATACMS, Iskander) can range into the hundreds of kilometres. Check what's actually in range before calling it in — don't assume every battery listed can reach a distant target.

**Fire-for-Effect Scatter**: Impacts scatter across roughly a 200m radius around the target point.

**Simultaneous Batteries**: Up to 3 friendly artillery groups can fire simultaneously per mission.

**Adjustment**: 50-100m per adjustment (Short/Long/Left/Right)
**Time of Flight**: Varies by range (~30-60 seconds)

## See Also

- [JTAC System](../f10-menu/jtac.md)
