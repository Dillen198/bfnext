# Action Types Reference

Every kind of action the engine can offer under `F10 → Actions>>`, what it
does, what it needs from you, and what it says when it can't. **Which of these
your server actually offers is config**: this is the catalogue, not the menu.
The menu, and the table below, are the authority for what you can call and
what it costs. How to drive the menu itself is on the
[Actions Menu](../f10-menu/actions.md) page.

Nothing an action creates appears out of thin air. Aircraft start on the ground
at the nearest friendly field more than 10 km from the target that has room
for them (heavies need a large stand; helicopters can use FARPs and FOBs) and
take off. Ground units drive out from a friendly base. If no field can launch
it, the action is refused and costs nothing.

## What this server offers

Generated from the campaign config of **the server selected in the top bar**:
switch servers there and these lists change.

### Blue

{{list:actions.Blue|_This server publishes no action list. Check F10 → Actions in game._}}

### Red

{{list:actions.Red|_This server publishes no action list. Check F10 → Actions in game._}}

The **Type** column is the engine's own name for the action kind, and matches
the headings below. `Cost` is charged when the action starts; see
[How costs and limits work](#how-costs-and-limits-work).

## At a glance

| Type | You pick | Puts on the map |
| --- | --- | --- |
| `Awacs` | a map mark | an AWACS on a race track |
| `Tanker` | a map mark | a tanker on a race track |
| `Fighters` | a map mark | an AI fighter flight orbiting the mark |
| `Attackers` | a map mark | an AI attack flight orbiting the mark |
| `Sead` | a map mark | an AI SEAD flight orbiting the mark |
| `Drone` | a map mark | a drone that is also a JTAC |
| `Recon` | a map mark | a recon flight; the area is scanned at once |
| `CruiseMissileSpawn` | a map mark | an ALCM platform orbiting the mark |
| `Bomber` | a friendly JTAC | a bomber flying to that JTAC's target |
| `Strike` | an enemy objective, or a map mark | a strike flight |
| `Paratrooper` | a map mark | a transport that drops a squad |
| `Deployable` | a map mark | a deployable, by air or straight away |
| `FatCow` | a map mark | an AI heavy-lift helo that sets up a FARP |
| `LogisticsRepair` | one of your objectives | a cargo flight that repairs it |
| `LogisticsTransfer` | (chat only) two of your objectives | a cargo flight moving supply |
| `Reinforce` | one of your objectives that lost groups | a convoy of transporters |
| `Nuke` | a map mark | a very large explosion |
| `Artillery` | a map mark | nothing; your guns fire |
| `NavalCruiseMissileStrike` | an enemy objective | nothing; your ships fire |
| `*Waypoint` | your flight, then a mark | nothing; the flight moves |
| `Rtb` | your flight, then a mark | nothing; the flight goes home |
| `Move` | your ground group, then a mark | nothing; the group drives |
| `CarrierWaypoint` | your carrier group, then a mark | nothing; the carrier sails |
| `CarrierRepair`, `CarrierRespawn` | (chat only) your carrier group | the lost ships |
| `AddTask`, `RemoveTask` | a task type and a mark or base / a task | a task on the board |

---

## Support aircraft

### AWACS
`Awacs`. Spawns an airborne early-warning aircraft that flies a race track
centred on your mark, laid across the line to the nearest enemy objective
(two race points, 30 km either side). It is an EWR in the air: it feeds the
coalition radar picture that [GCI](../gameplay/gci.md) and
[EWR reports](../f10-menu/ewr.md) are built on. Its frequency, TACAN and
callsign (where the server sets them) are listed in
`F10 → Info → Support & Radios`.

### Tanker
`Tanker`. A refuelling aircraft on a race track at your mark, like the AWACS.
Boom and basket are usually separate actions; bring the one your airframe can
use. Frequency and TACAN are in `Support & Radios`.

### Waypoint actions
`AwacsWaypoint`, `TankerWaypoint`, `FighersWaypoint`, `AttackersWaypoint`,
`SeadWaypoint`, `DroneWaypoint`, `CruiseMissileWaypoint`. Each moves a flight
your side already has to a new mark, usually for much less than a new one. The
menu lists only flights of the matching kind. Ordering a flight that is still on
the ground is fine: the new station is flown once it is airborne.

Refusals: *"this move action is not compatible with the selected group"*,
*"can't move the other team's awacs"* (any flight that isn't yours).

### RTB
`Rtb`. Sends one of your side's AI flights home: it flies to the friendly
airbase nearest the mark you pick and is removed when it is back within
10 km. Whoever paid for it gets **a quarter of the action's cost** back
(rounded up), and your side is told:

```dcsmessage
Viper's BAWACS-5012 has RTB'd. points refunded: 25
```

RTB frees the flight's slot against the action's limit. Refusals:
*"no bases to rtb!"* (your side holds no airbase), *"can't rtb the other team's
aircraft"*.

## Strike and fighter packages

### Bomber
`Bomber`. A heavy bomber strike. It does **not** take a map mark: it expands
into a list of your coalition's live JTACs, then `Yes, do it!`. The bomber
flies to what that JTAC is tracking (or the JTAC's position if it has no
target). Within 10 km of it, the strike lands on up to the action's target
count of your JTACs' contacts within 15 km, each hit placed at random inside
the action's accuracy radius. Then it flies home.

```dcsmessage
BOMBER MISSION STARTED
targeting by jtac 1187 near Gori
started by Viper
```

Get a JTAC on the target first, then call the bomber. Also
`-jtac <id> bomber [mission]` in chat.

### Strike
`Strike`. An AI flight that attacks with its **own weapons**, for example an
F-117 on a precision strike. It needs no JTAC. Each Strike action has one
mission: **Precision** (an enemy base's buildings), **Runway** (an enemy
airbase's runways), **Anti-ship** (an enemy carrier group), **Interdiction**
(the enemy column nearest your mark) or **Carpet** (a line of bombs through
your mark). Details, refusals and messages:
[AI Strike Missions](../advanced/strike-missions.md).

### Fighters
`Fighters`. An AI fighter flight orbiting your mark, engaging enemy aircraft
and helicopters within 30 km. Use it to screen a corridor or a base.

### Attackers
`Attackers`. AI ground-attack aircraft orbiting your mark, engaging aircraft
and ground units within 15 km.

### SEAD
`Sead`. AI anti-radiation aircraft orbiting your mark, engaging SAM sites, EWR
and AAA within 15 km. It stays until you send it home (RTB) or its lifetime
runs out.

### Drone
`Drone`. An orbiting drone over your mark that is also a **JTAC**: it finds,
lases and feeds targets to `F10 → JTAC`, bomber missions and artillery. Its
lasing range and whether it needs line of sight are set by the server for each
drone action.

### Cruise missile platform
`CruiseMissileSpawn`. An ALCM carrier aircraft orbiting your mark. You fire its
missiles through a JTAC; see
[Air-Launched Cruise Missiles](../advanced/alcm.md).

### Naval cruise missile strike
`NavalCruiseMissileStrike`. Pick an enemy objective; the nearest of your
carrier task forces in range with enough land-attack cruise missiles on one
ship group fires them at it. Refusals:

- *"No friendly carrier group in range"*
- *"None of our ships in range carries land-attack cruise missiles (a
  Ticonderoga or an Arleigh Burke does)"*
- *"Our ships in range have only 4 cruise missiles between them, not 6 on one
  ship group"*
- *"Cannot strike a friendly objective"*

See [Carrier Operations](../gameplay/carrier-ops.md).

### Nuke
`Nuke`. A very large explosion 500 m above your mark. Every nuke used divides
the next one's price by the action's scale, for both sides, down to 1 point.
Not on normal servers.

## Ground forces

### Paratrooper
`Paratrooper`. A transport flies to your mark and, within 800 m of it, drops
a squad that is yours. You pay the action cost plus the squad's own cost. A way
to get capture troops onto ground you cannot land on; check the
[Deployables reference](./deployables.md) for which squads can take an
objective. If the drop fails: `paratroop mission failed ...` with the reason
(for example *"the maximum number of Standard troops are already deployed"*).

### Deployable
`Deployable`. A deployable without the crate run. You pay the action cost plus
the deployable's own cost. If the action has an aircraft, a transport flies it
to your mark and sets it down within 800 m; if not, it appears at the mark
straight away. Deployable limits apply as for crates: *"the max number of
"Firtina 155MM" are already deployed"*. If an airborne delivery fails at the
mark: `deploy mission failed ...`.

### Move
`Move`. Sends one of your side's deployed units or troop squads to a mark. It
drives the road network where there is one, then off-road onto the exact spot,
and engages anything within 2 km on the way.

- **Price:** the action cost per step of distance (the step is set separately
  for troops and deployables, usually 1 km), rounded up, never less than one
  step. The menu shows the price next to every mark.
- **Penalty:** charged only if you move **someone else's** unit and it is later
  destroyed: `for the death of 1051 which was deployed by Hawk and moved by you`.
  Moving your own costs nothing extra. The unit's map label then also names you
  as responsible party.
- Refusals: *"can't move an enemy unit"*, *"can't move this unit type"*.

Use it to walk capture troops the last few hundred metres into a zone when the
[Capture Advisor](../f10-menu/objectives.md) says they are short of the edge.

### Reinforce
`Reinforce`. Sends a convoy of tank transporters to one of your objectives
that has lost garrison groups, to replace them. The menu lists only the bases
with something destroyed, and how many groups each has lost
(`Kobuleti (2 down)`).

- It sets off from **your nearest objective** within the action's range that
  isn't under attack and can spare the materiel, and drives the roads there.
  It is slow, and the enemy can see and hit it.
- When it arrives, up to the action's group count of destroyed garrison groups
  are rebuilt, armour first. Each transporter lost on the way means less
  arrives.
- One convoy per objective at a time. A convoy on the road when the server
  restarts carries on from where it was. One stuck on terrain is recalled and
  refunded.
- The action's penalty is charged if the convoy is destroyed.

Messages your side sees:

```dcsmessage
Reinforcements for Kobuleti leaving Batumi: 2 group(s) on transporters, 34 km by road, about 51 min. Keep the road open.
```

```dcsmessage
Reinforcements reached Kobuleti: 2 group(s) back in the line.
```

Also: *"Reinforcements reached Kobuleti: 1 group(s) back in the line (1
transporter(s) lost on the way)."*, *"Reinforcements reached Kobuleti, but
nothing there needed replacing."*, *"The reinforcement convoy for Kobuleti was
destroyed on the road. Nothing arrives."* (the enemy sees *"Enemy reinforcement
convoy destroyed -- its armour never reaches the front."*), *"The reinforcement
convoy for Kobuleti is stuck and has been recalled. Points refunded."* and
*"Kobuleti fell before the reinforcements got there."*

Refusals: *"Kobuleti is not ours to reinforce"*, *"a carrier group can't be
reinforced by road"*, *"Kobuleti is under attack -- reinforcements can go in
once it is clear"*, *"reinforcements are already on the road to Kobuleti"*,
*"Kobuleti has no destroyed ground units to replace"*, *"no friendly objective
within 80 km of Kobuleti can send reinforcements (it must not be under
attack)"* (or *"(needs 40 materiel and not be under attack)"* when the convoy
costs materiel).

### Artillery
`Artillery`. Player-called indirect fire from your side's guns and launchers
at your mark. The server can also switch on a free **`Request Fires`** entry at
the bottom of the Actions menu that does the same whenever your coalition has
artillery alive. Per-battery control is under `F10 → JTAC`. See
[Artillery Missions](../advanced/artillery.md).

## Logistics

### Logistics Repair
`LogisticsRepair`. Pick one of your objectives; a cargo flight flies to it
and, within 800 m, repairs one step of its logistics. Pays
{{cfg:points.logistics_repair|25}} points on completion. If the drop is not near
a friendly objective any more: `repair mission failed ... no friendly objective
near drop off point`.

### Logistics Transfer
`LogisticsTransfer`. Moves supply from one of your objectives to another. It
has **no Actions menu entry**: call it in chat with
`-action <name> <from> <to>`. The flight takes off from `<from>`. Pays
{{cfg:points.logistics_transfer|15}}. Refusals: *"a transfer needs two
different objectives"*, *"<from> is not in friendly hands"*, *"<from> has no
free parking to launch this flight from"*.

See [Logistics & Supply](../gameplay/logistics.md) and
[Materiel & the War Economy](../gameplay/war-economy.md).

### Fat Cow (AI)
`FatCow`. An AI heavy-lift helicopter that sets up a temporary forward arming
and refuelling point at your mark. It takes off from the nearest friendly
field, lands at the mark, and a minute after touchdown the
[Fat Cow](../advanced/deployables-guide.md#fat-cow-heavy-lift-farp) is laid out
around it: pad 100 m behind the helicopter, fuel point, fire point and guards.
It flies with the [AI helo mission](../advanced/helo-missions.md) settings.

- It stays as long as the action says (engine default 20 minutes), then packs
  up and flies home.
- It carries the same fixed load as a player-built Fat Cow. Nothing resupplies
  it.
- The mark must be clear of friendly objectives, like any FARP, and within
  {{cfg:helo_insertion.max_range_m|150000}} m of a friendly field.
- If the helicopter is shot down on station, the Fat Cow closes with it. If it
  is lost, can't set up, or isn't airborne within 15 minutes, you get the cost
  back.
- The action's limit caps how many your side can have out at once, and the
  Fat Cow deployable's own limit applies too.

```dcsmessage
The Fat Cow is set up at fat cow 38T 0 for 20 minutes. Land on the pad 100m behind the helicopter to rearm and refuel.
```

Other messages: *"A Fat Cow is lifting from Senaki for the marked site; it will
be set up for 20 minutes."*, *"The Fat Cow at fat cow 38T 0 has packed up."*,
and to you on failure *"your Fat Cow was lost before it was set up, refunding 50
points"*, *"your Fat Cow could not set up: ..."*, *"your Fat Cow never made it
to the site and was recalled"*. Refusals: *"a Fat Cow can't be set up there --
2140m from Senaki, it has to be {{cfg:logistics_exclusion|10000}}m from friendly
objectives"*, *"Blue already has 1 Fat Cow up"*, *"no friendly field able to
launch a Fat Cow"*, *"the nearest friendly field (Senaki) is 162km away, past
the 150km max range"*.

## Carrier

### Carrier Waypoint
`CarrierWaypoint`. Sails one of your carrier groups to a mark. The menu lists
your carrier groups as `5501(<name> Carrier)`. Refusal: *"5501 is not one of
your carrier groups"*.

### Carrier Repair / Carrier Respawn
`CarrierRepair` repairs a damaged task force and refloats its lost escorts;
`CarrierRespawn` brings back a sunk one, whose replacement ships put to sea off
its naval base. Both are paid in **materiel from the task force's naval base**
({{cfg:carrier.repair_cost|5000}} materiel to repair, {{cfg:carrier.respawn_cost|15000}} to respawn), not just points. They show an **empty
submenu** in the Actions menu: run them in chat with
`-action <name> <carrier group>`, or as a commander order. Refusals:

- *"the carrier is destroyed, it needs a respawn not a repair"*
- *"the carrier is not damaged"*
- *"Carrier is not destroyed (health: 40%)"*
- *"Not enough supplies at Naval Base to repair carrier (need 5000, have 1200)"*
  (or *respawn*)
- *"Batumi Naval is not in friendly hands, it can't supply the carrier"*

See [Carrier Operations](../gameplay/carrier-ops.md).

## Intelligence

### Recon
`Recon`. Dispatches an AI recon aircraft to orbit your mark. As soon as it is
up, every enemy unit within the action's scan radius (engine default 10 km) of
the mark goes into your coalition's intel picture, no line of sight needed. A
recon flight is not respawned after a server restart. Distinct from the
**player** recon pass you fly yourself from `F10 → Recon`; see
[Reconnaissance](../f10-menu/recon.md).

## Coalition coordination

### Add Task / Remove Task
`AddTask` posts a task to the coalition tasking board at a map mark, or against
an objective; `RemoveTask` takes one back off. Usually free. See
[The Tasking Board](../gameplay/tasking-board.md).

## How costs and limits work

| | |
| --- | --- |
| **Cost** | Deducted when the action starts, shown in the menu label. Paratrooper and Deployable add the unit's own cost; Move is per step of distance; Nuke gets cheaper with each use. 15% off at war effort tier 2 and above. |
| **Penalty** | Charged to the responsible party when an action group is destroyed. For Move, charged to the mover only when they moved someone else's unit and it dies. |
| **Limit** | For actions that put a group on the map: how many your side may have up at once. For the rest (waypoints, RTB, Move, nuke, carrier orders, fires, tasks): how many uses per server session. |
| **Geo limit** | Some actions can only be aimed within a distance of a friendly objective. |
| **Duration** | Loitering flights can have a lifetime in hours, after which they are removed. |

Every refusal answers with a panel message rather than silently failing:

| Message | Means |
| --- | --- |
| `Viper: this action costs 100 points and you have 40 points` | not enough points |
| `blue is out of AWACS actions` | at the action's limit |
| `this action point is 41230 meters but can only be targeted within 20000 meters of a friendly objective` | geo limit |
| `no friendly airfield more than 10 km from the target has room to launch this flight` | nowhere to take off from (helicopters: `airbase, FARP or FOB`) |
| `Senaki has no free parking to launch this flight from` | the chosen field is full |
| `you are not authorized for actions` | the server's rules exclude you |

Refunds: RTB gives back a quarter of the cost; an undelivered bomber, strike,
paradrop, deployable delivery or logistics run is refunded in full if the
server restarts; AI helo missions and the Fat Cow refund when they fail.

## See Also

- [Actions Menu](../f10-menu/actions.md) — the menu itself, step by step
- [Deployable Units](./deployables.md) — the crate-built ground units
- [AI Helo Missions](../advanced/helo-missions.md)
- [Points and Lives](../gameplay/points-and-lives.md)
