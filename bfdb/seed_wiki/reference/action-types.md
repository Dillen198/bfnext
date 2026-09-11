# Action Types Reference

Every kind of action the engine can offer under `F10 → Actions>>`, what it
spawns, and how to use it. **Which of these your server actually offers is
config** — this is the catalogue, not the menu. The menu is the authority for
what you can call and what it costs.

> The live mission's action set is listed in [Actions Menu](../f10-menu/actions.md).

## Support aircraft

### AWACS
Spawns an airborne early-warning aircraft on station at your map mark. Once it
is up, its frequency and TACAN show in `F10 → Info → Support & Radios`, and it
feeds the coalition radar picture that [GCI](../gameplay/gci.md) and
[EWR reports](./../f10-menu/ewr.md) are built on.

*Carries a penalty — losing it early costs you again. Orbit it behind the line.*

### Tanker
A refuelling aircraft on station. Boom and basket are separate actions; bring
the one your airframe can use. Frequency and TACAN in `Support & Radios`.

*Also penalised on early loss.*

### Waypoint actions
`AWACS Waypoint`, `Air Refuelers Waypoint`, `DRONE Waypoint`,
`Fighters/Attackers/SEAD Waypoint`, `Cruise Missile Waypoint`,
`Carrier Waypoint`. Each moves an asset you already have to a new mark, for a
fraction of the price of calling another one.

### RTB
Sends an asset home. Frees whatever slot or limit it was occupying.

## Strike and fighter packages

### Bomber
A heavy bomber strike. It does **not** take a map mark — it expands into a list
of your coalition's live JTACs, and hits whatever the JTAC you pick is currently
tracking. So the workflow is: get a JTAC on the target first, then call the
bomber.

### Fighters (CAP)
An AI fighter patrol at your mark. Useful as a screen over a corridor you need
to keep open, or over a base you expect to be hit.

### Attackers
AI ground-attack aircraft against a marked area.

### SEAD
AI anti-radiation aircraft against enemy emitters in an area. The usual reason
to call one is that something is painting your strike package and nobody wants
to go shopping for it themselves.

### Drone
An orbiting drone over your mark. In practice this is a **JTAC you can put
anywhere** — it sees, it lases, and it feeds targets to `F10 → JTAC` and to
bomber missions. The cheapest sensor in the game.

### Cruise missile platform / Naval strike
`CruiseMissileSpawn` spawns an ALCM carrier — see
[Air-Launched Cruise Missiles](../advanced/alcm.md).
`NavalCruiseMissileStrike` fires from your nearest carrier in range at an enemy
objective you pick from a list. See [Carrier Operations](../gameplay/carrier-ops.md).

### Nuke
Exists in the engine; not configured on normal servers.

## Ground forces

### Paratrooper
Air-drops a squad at a mark. A way to get capture troops onto ground you cannot
land on — check the [Deployables reference](./deployables.md) for which squad
types can actually take an objective.

### Deployable
Spawns a ground unit directly, rather than through the crate system. Where a
server offers both, crates are usually cheaper and deployables are faster.

### Move (Units/Troops)
Sends one of your existing ground groups or squads to a mark. Cheap, and
penalised if the group dies on the way. This is how you walk capture troops the
last few hundred metres into a zone when the
[Capture Advisor](../f10-menu/objectives.md) says they are short of the edge.

### Artillery
Player-callable indirect fire from your side's guns. On the live mission this
appears as **`Request Fires`** in the Actions menu whenever your coalition has
artillery alive, with per-battery control under `F10 → JTAC`. See
[Artillery Missions](../advanced/artillery.md).

## Logistics

### Logistics Repair
Flies in a repair to an objective's logistics infrastructure. Pays
{{cfg:points.logistics_repair|350}} points on completion — one of the
best-paying things in the campaign, and one of the least flown.

### Logistics Transfer
Moves supply between objectives. Pays
{{cfg:points.logistics_transfer|350}}.

Both are worth roughly an air kill. See
[Logistics & Supply](../gameplay/logistics.md) and
[Materiel & the War Economy](../gameplay/war-economy.md).

## Carrier

### Carrier Waypoint
Sails a carrier group to a mark. Free on the live mission.

### Carrier Repair / Carrier Respawn
Pays for a carrier's repair, or re-floats a sunk one, out of its linked naval
base's supplies. **Not configured on the live mission** — use carrier repair
crates and the automatic repair instead. See
[Carrier Operations](../gameplay/carrier-ops.md).

## Intelligence

### Recon
Dispatches an AI recon aircraft to scan an area and feed what it finds into the
coalition intel picture. Distinct from the **player** recon pass, which you fly
yourself from `F10 → Recon` — see [Reconnaissance](../f10-menu/recon.md).

## Coalition coordination

### Add Task
Posts a task to the coalition tasking board at a map mark, or against an
objective. Drawn on the F10 map for everyone on your side, ranked into the
briefing, announced on the GCI net.

### Remove Task
Takes one back off the board.

Both are free. See [The Tasking Board](../gameplay/tasking-board.md).

## How costs and limits work

| | |
| --- | --- |
| **Cost** | Deducted when the action starts. Shown in the menu label. |
| **Penalty** | Charged *again* if the asset is lost early. |
| **Limit** | Some actions cap how many of a thing a side may have up at once. |
| **Geo limit** | Some actions can only be called within a distance of friendly territory. |

An action you cannot afford, or that is at its limit, answers with a panel
message saying so rather than silently failing.

## See Also

- [Actions Menu](../f10-menu/actions.md) — the live set and how to drive it
- [Deployable Units](./deployables.md) — the crate-built ground units
- [AI Helo Missions](../advanced/helo-missions.md)
- [Points and Lives](../gameplay/points-and-lives.md)
