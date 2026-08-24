# Changelog — v2.0

What's changed since the original open-source Fowl Engine. This page only lists what's **different from stock Fowl Engine** — not a full commit history, and not core mechanics (F10 menus, JTAC, cargo, points/lives, capturing objectives) that were already part of the base engine and are simply documented elsewhere in this wiki.

## New Campaign Systems

**IADN — Integrated Air Defence Network**
SAM sites are no longer isolated units. They share a live sensor picture across the coalition, keep search radars dark until a real threat is confirmed, and actively defend each other: fire a HARM or other anti-radiation missile at one and it goes dark to deny the lock, while short-range point-defense systems (Pantsir, Shilka) stay alert specifically to shoot the missile down. A new **Command Center** objective ties into overall network integrity.

**Advanced Radar Physics**
Detection is no longer a simple binary "in range = detected." Probability now depends on target aspect (hot/flank/beam/cold), altitude, and closure rate — flying beam-aspect with low closure rate ("the notch") makes you nearly invisible to pulse-Doppler radars, the way it does in reality. Radar tracks are smoothed instead of snapping between raw returns. AWACS and ground-based EWR are properly differentiated (AWACS gets look-down capability, ground radar degrades at low altitude).

**ELINT/SIGINT Intel Database**
Recon flights now build a persistent, decaying intel picture instead of a one-shot report: detected enemy ground units are classified, clustered, and tracked with a confidence score that fades over time (faster for older/lower-quality sources). Shown as F10 map markers and in a radio "intel picture" report.

**Mechanized Infantry**
Infantry squads can board and dismount IFVs/APCs for overland transport, not just helicopters — troops embark near a friendly vehicle and can be dropped off anywhere it can drive.

**CSAR — Combat Search & Rescue**
Ejecting no longer costs a life outright. A downed pilot unit spawns at the crash site; a friendly helicopter can locate and extract them via the F10 → CSAR menu, restoring the lost life and earning the rescuer bonus points.

**C-130 Hercules & Airdrop System**
A dedicated logistics role: the C-130 can deliver cargo, troops, and vehicles via parachute or LAPES (low-altitude extraction) runs to unprepared landing zones, with automatic detection-and-unpack on delivery — no manual trigger needed. See [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md).

**SAM Site & Naval Carrier capture mechanics**
SAM site positions are classified (not shown on F10 map or dashboard) and capture instantly once the zone is held, with no timer. Naval Carrier Groups can't be boarded while combat-effective — disable one by sinking its logistics to zero, then capture it directly or by taking its linked naval base. See [Capturing Objectives](../gameplay/capturing-objectives.md).

**Last Stand**
A coalition reduced to its last primary objective (airbase, naval base, or FARP) gets a do-or-die countdown timer instead of being able to turtle indefinitely.

**Automated Convoys & Strategic Infrastructure**
Ground supply convoys run automatically between logistics hubs and the front — and can be interdicted. Real map buildings (warehouses, fuel depots, industrial complexes) near objectives are marked on the F10 map and, when destroyed, permanently degrade that objective's logistics — a second, independent way to attack enemy supply. **Factories** are a new objective type that passively produces ground units for their owner.

**Live Weather Sync**
Server weather now syncs automatically from the running DCS mission via `bftools`/DCSServerBot integration, instead of being fixed at mission build time.

## New Tools & Interfaces

**In-DCS Cockpit UI**
An overlay panel that renders inside the DCS window itself (EWR reports, a C-130J CARP bombing-solution planner, crate spawning) for players who install it — strictly additive, the F10 menu is unchanged and always available alongside it.

**bfweb Ops Dashboard**
A full web dashboard: live tactical map with pilot names and NATO-style unit symbology, A/A / A/G / Logistics leaderboard tabs, objective Health/Logistics/Supply/Fuel status with repair-progress percentages, engine performance history and hardware monitoring, and Discord OAuth login.

**bfwiki**
This wiki — an admin-editable player reference, separate from the web dashboard, backed by the same login.

**Discord Integration**
Kill-streak achievement announcements, capture alerts with pilot attribution, and bot plugins for announcements, FAQ, rules, tickets, and server administration.

**In-game Help Menu**
A built-in F10 help menu for quick reference without leaving the game.

## See Also

- [Introduction](../introduction.md)
- [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md)
- [Capturing Objectives](../gameplay/capturing-objectives.md)
