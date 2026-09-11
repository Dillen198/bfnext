# GCI / EWR

Your coalition's radar picture, on demand, plus the settings for the
[live GCI voice net](../gameplay/gci.md). The menu is called **`GCI/EWR`** and
it is available from every slot.

![EWR radar operator screen tracking enemy and friendly aircraft contacts](/api/wiki/images/b68ad72b-f981-4957-986d-b301edcc4216)

## Overview

The EWR system provides:
- Enemy aircraft positions
- Altitude and heading information
- Distance and bearing from your position
- Friendly aircraft locations
- Tactical intelligence

## The menu

```
F10 → GCI/EWR
├── Report                enemy air picture
├── Toggle                turn automatic EWR calls on/off for you
├── Friendly Report       where your own side's aircraft are
├── Units to Imperial     feet / nautical miles
├── Units to Metric       metres / kilometres
├── Ground Intel          known enemy ground units and SAMs
└── GCI Voice
    ├── Toggle GCI Calls          voice net on/off for you
    ├── Toggle Auto Callouts      unprompted calls on/off
    ├── Units: Imperial / Metric / Server Default
    └── Reference: BRAA / Bullseye / Clock
```

### Ground Intel

Not an air-picture report — this is what your coalition **knows** about enemy
ground units and SAM sites, from the ELINT/SIGINT intel database. Contacts decay
over time and are only as good as your sensors: a SAM nobody has detected is not
on it.

Feed it by flying [recon passes](./recon.md), keeping an AWACS up, keeping JTACs
alive, and uploading [TARPS photos](../advanced/recon-intel-map.md).

### GCI Voice

Settings for the AWACS controller that talks to you over SRS. `Toggle Auto
Callouts` is the one most people want — it turns off unprompted calls while
keeping the net. Reference mode (BRAA / bullseye / clock) changes how positions
are spoken to you, and unit preference changes feet-vs-metres.

The same settings are available in chat as `-gci` — see
[Live GCI](../gameplay/gci.md).

## Enemy Report

**Request Report**:
```
F10 → GCI/EWR → Report
```

**Report Format**:
```
BRAA: Bearing, Range, Altitude, Aspect

BRAA 045/25/15000/HOT
BRAA 180/40/5000/FLANK
BRAA 270/15/20000/COLD
```

**Reading**:
- **Bearing**: Direction from you (degrees)
- **Range**: Distance (nm or km)
- **Altitude**: Height (feet or meters)
- **Aspect**:
  - HOT: Coming toward you
  - COLD: Going away from you
  - FLANK: Crossing left/right
  - BEAM: 90° to you

**Example**:
```
BRAA 045/25/15000/HOT
```
= Enemy at 045°, 25nm away, 15,000ft, coming toward you

## Friendly Report

**Request Report**:
```
F10 → GCI/EWR → Friendly Report
```

Shows same format for friendly aircraft:
- Your team's aircraft positions
- Coordination information

## Toggle EWR

**Enable/Disable**:
```
F10 → GCI/EWR → Toggle
```

**Effect**:
- ON: Automatic periodic reports
- OFF: Manual request only
- Preference per player

**Use Cases**:
- ON: High-threat environment
- OFF: Reduce message spam

## Unit Systems

**Imperial**:
```
F10 → GCI/EWR → Units to Imperial
```
- Feet (altitude)
- Nautical miles (range)
- Standard for aviation

**Metric**:
```
F10 → GCI/EWR → Units to Metric
```
- Meters (altitude)
- Kilometers (range)
- International standard

**Changes apply immediately**

## Limitations — read this one

**The picture is delayed.** This server runs EWR in
**{{cfg:ewr_mode|Delayed}}** mode with a
**{{cfg:ewr_delay|60}} second** delay on track updates. A contact's position is
where it was, not where it is. At 480 knots that is roughly 8 nm of error —
enough to turn a "he's at 20 miles" into a merge. Treat every EWR line as a cue
to look, not as a firing solution.

**It is a snapshot.** The report is generated when you ask for it; nothing
updates on the page.

**Coverage is earned.** The picture comes from your side's ground EWR radars,
airborne EWR-capable aircraft, and any AWACS you have paid for. Lose them and
you go blind:

- Deploy **AN/FPS-117 / EWR** ground radars from crates —
  see [Deployable Units](../reference/deployables.md).
- Keep an **AWACS** up — `Actions>> → E-3A AWACS`.
- Many fighters count as airborne EWR themselves just by being on station.

**Terrain and altitude matter.** Ground radars miss low contacts; a target in
the weeds behind a ridge is not in the report. The engine models radar cross
section by aspect and notching, so a beaming contact can drop out entirely.

**Contacts are generic.** The report does not hand you a type. Identification
ripens over time and with better sensors — the
[GCI voice net](../gameplay/gci.md) will upgrade a call from "bogey" to a type
as confidence grows.

## See Also

- [Live GCI (AWACS Calls)](../gameplay/gci.md) — the voice side of the same picture
- [Reconnaissance](./recon.md) — feeding the ground intel database
- [Recon Intel Map (TARPS)](../advanced/recon-intel-map.md)
- [Actions Menu](./actions.md) — deploying AWACS and drones
- [Deployable Units](../reference/deployables.md) — ground EWR radars
