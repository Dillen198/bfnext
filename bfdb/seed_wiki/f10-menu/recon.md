# Reconnaissance (Recon Pass)

Fly a recon-capable aircraft near an enemy objective and run a timed **Recon Pass**
to reveal the enemy units around it — SAM and AAA sites included — on the F10 map
for your whole coalition.

## Overview

A Recon Pass:
- Only works from a **recon-capable airframe** (set by the server — e.g. the
  L-39ZA, MB-339A).
- Must be started while within range of an **enemy-owned objective** (default 25 km).
- Takes a fixed **dwell time** to complete (default 120 s) during which you must
  stay within range.
- Only reveals units your aircraft has **terrain line-of-sight** to — units hidden
  behind ridgelines stay hidden.
- Feeds the shared intel picture, so contacts appear on the F10 map and in the
  **EWR → Ground Intel** radio report, then fade as the intel ages.

## Recon Menu

Access via **F10 → Recon** (only shown in recon-capable aircraft).

**Menu Options**:
- **Start Recon Pass** — begins a pass against the nearest enemy objective in range.
- **Cancel Recon Pass** — aborts the current pass (any point cost is refunded).
- **Recon Status** — shows pass progress, or the nearest enemy objective and its
  distance.

## Running a pass

1. Slot a recon-capable aircraft.
2. Fly toward an enemy objective (a SAM belt, an airfield, a staging area).
3. When **Recon Status** shows "Ready", select **Start Recon Pass**.
4. Hold station within range for the dwell time. Orbit, or hover if you are in a
   helicopter. Leaving the range ring or (if the server sets a ceiling) climbing
   too high aborts the pass.
5. In progressive mode you get partial reveals at 25 / 50 / 75 %, and a full
   sweep on completion: `Recon complete -- N contact(s) on the F10 map`.

## Reading the results

Detected units are clustered by type and drawn as dashed boxes with a label such
as `[INTEL/RED] 4xADS | 80% +/-1.5km`:
- **ADS** = air defense (SAM / AAA), **ARMOR**, **ARTY**, **INF**, **NAVAL**, **UNK**.
- The percentage is the current **confidence** — it decays over roughly ten
  minutes and the mark disappears when it gets too stale. Re-fly the pass to
  refresh it.
- A dotted square shows the area that was scanned.

## Limits

- **Line of sight matters** — a single high pass will miss units in dead ground.
  Multiple passes from different angles build a fuller picture.
- **Intel ages** — the map is only as current as your last pass.
- **Cooldown** — there is a short per-pilot cooldown between passes.
- Aborting or completing a pass both start the cooldown.

## Other ways contacts reach the map

A **JTAC** with eyes-on a target (any type — ground, drone, or player) feeds it
into this same intel picture automatically, no menu action needed, and keeps it
marked for a long time after losing sight of it. See
[JTAC → F10 Map Intel](./jtac.md#f10-map-intel).

## See Also

- [Early Warning Radar](./ewr.md) — the **Ground Intel** report reads out the same contacts
- [JTAC System](./jtac.md) — JTAC eyes-on targets also mark the map
- [Actions Menu](./actions.md) — the AI **Recon** action scouts a map-mark without a player flying it
