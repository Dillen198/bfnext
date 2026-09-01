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
Ejecting no longer costs a life outright. A downed pilot unit spawns at the crash site; a friendly helicopter can locate and extract them via the F10 → CSAR menu, restoring the lost life and earning the rescuer bonus points. Eject over a friendly airbase, FARP or carrier and the pilot walks in on their own — no helo needed. A downed pilot is held by an enemy-capture countdown (server-configurable, 30 min by default), but as long as a friendly rescue helicopter is sitting on top of them the timer can't take them, so a pickup that's a few seconds late still works.

**C-130 Hercules & Airdrop System**
A dedicated logistics role: the C-130 can deliver cargo, troops, and vehicles via parachute or LAPES (low-altitude extraction) runs to unprepared landing zones, with automatic detection-and-unpack on delivery — no manual trigger needed. See [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md).

**Capture mechanics — health/infantry gate, troop scaling, consolidation**
A base is capturable once its **health is ≤ 20% and every infantry defender is dead** — not when its logistics hit zero. The hold timer is **180 s base, divided by how many troop squads you have in the zone** (more squads = faster, floored at 30 s). After the timer completes the base flips, but your troops must then **hold for a ~5-minute consolidation window** while the garrison moves in — if they're wiped out first, the base goes **Neutral** and has to be retaken. On a successful capture the new owner's garrison is revived immediately so a freshly-taken base doesn't sit at 0% health waiting to be lost again. SAM sites still capture instantly (no timer). See [Capturing Objectives](../gameplay/capturing-objectives.md).

**Carrier groups change hands**
Disable an enemy carrier (sink its escorts to knock its logistics to 0) and take the naval base it's linked to, and the carrier group now **passes to the captor as their own ships** — the enemy task force is replaced by your coalition's carrier group in the same spot, with your deck slots. It comes across at 50% and can be brought back up with **carrier repair crates** (air-dropped or flown in by helo — stack several to repair faster, ~30 min at one crate) or the **Repair / Respawn Carrier** actions off the naval-base menu. An `-admin capture <objective> <blue|red|neutral>` command was also added to force any objective to change hands.

**Artillery — missile TELs, salvos, auto-turn**
The JTAC artillery menu now also commands **ballistic/cruise missile launchers** (Scud, Iskander, Silkworm), enforces each unit type's real min/max range, and adds a **"Fire All Groups Together"** salvo and an **"all ammo"** option. Batteries that spawn facing the wrong way now **reposition to bring the launcher onto the target bearing** before firing, so hull-traverse systems that used to silently no-op actually shoot. The **Move** action follows roads instead of driving cross-country. See [Artillery Missions](../advanced/artillery.md).

**ATIS & weather**
The airfield ATIS now reports the **real DCS runway designator** (it was deriving the number from runway heading and sometimes naming a runway that doesn't exist), fixes a mirrored surface-wind bearing, and adds field elevation, QFE, cloud base AGL, visibility and precipitation — all in **both metric and imperial**. Live weather sync also corrects the wind-speed units and syncs the upper (2000 m / 8000 m) wind layers, not just the surface.

**Last Stand**
A coalition reduced to its last primary objective (airbase, naval base, or FARP) gets a do-or-die countdown timer instead of being able to turtle indefinitely.

**Automated Convoys & Strategic Infrastructure**
Ground supply convoys run automatically between logistics hubs and the front — and can be interdicted. Real map buildings (warehouses, fuel depots, industrial complexes) near objectives, when destroyed, permanently degrade that objective's logistics — a second, independent way to attack enemy supply. Each objective's logistics buildings are pinned on the F10 map for the owning coalition while a friendly aircraft is nearby (they cull with the objective's units so the map stays clean). **Factories** are a new objective type that passively produces ground units for their owner. An objective flagged **fully detached** from the supply chain gets no automatic resupply at all — convoy, cargo plane, or otherwise — and has to be sustained by hand with transport crates and C-130 drops.

**Frontline on the F10 map**
When enabled, the contour that separates blue-held ground from red-held ground is drawn as a dashed line — computed from the objectives themselves (Delaunay triangulation, marched for the blue/red boundary), so a theatre with an island and two land borders shows up as several distinct fronts and nothing is drawn where one side holds everything. Each stretch is coloured by which side's bases along it are healthier, and it redraws whenever an objective changes hands.

**Live Weather Sync**
Server weather now syncs automatically from the running DCS mission via `bftools`/DCSServerBot integration, instead of being fixed at mission build time.

## New Tools & Interfaces

**In-DCS Cockpit UI**
An overlay panel that renders inside the DCS window itself (EWR reports, a C-130J CARP bombing-solution planner, crate spawning) for players who install it — strictly additive, the F10 menu is unchanged and always available alongside it.

**bfweb Ops Dashboard**
A full web dashboard: live tactical map with pilot names and NATO-style unit symbology (and a light/dark theme), A/A / A/G / Logistics leaderboard tabs, objective Health/Logistics/Supply/Fuel status with colour-graded bars and repair-progress percentages, sortable columns (ascending/descending) on the Objectives, Kill Feed and Admin tables, engine performance history and hardware monitoring, and Discord OAuth login. Carrier groups appear in the Objectives list with full status but no map position (their location stays hidden). Map objective icons are clean lucide symbols with no coloured glow, and the Pilots page flight/kill/deploy logs scroll cleanly on mobile.

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
