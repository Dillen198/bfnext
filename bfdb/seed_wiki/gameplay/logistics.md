# Logistics & Supply

The logistics system adds strategic depth to the campaign. Understanding supply flows is key to sustained operations — cutting the enemy's supply is often more decisive than destroying their aircraft.

## Overview

Logistics simulates the supply chain required to maintain military operations. Without adequate supplies, objectives cannot function effectively.

## Convoys

![Supply convoy under aerial attack](/api/wiki/images/7d6453ff-8e35-4304-8a34-d61e680b7f83)

Automated ground convoys run between logistics hubs and forward objectives — no player action needed to keep them moving, but they **can be interdicted**:

- **Interdict enemy supply**: Destroying enemy convoys degrades their objective supply over time. Sustained interdiction can neutralize entire sectors.
- **Supply score**: Destroying enemy logistics convoys earns campaign score — every truck counts.
- **Cargo runs**: Player-piloted cargo missions (via F10 → Cargo, or the [C-130 airdrop system](../advanced/c130-airdrop.md)) can accelerate resupply when automated convoys are destroyed.

## Strategic Infrastructure

Real map buildings near objectives — warehouses, fuel depots, industrial complexes — are marked with **orange boxes** on the F10 map. Destroy them and that objective's logistics rating drops **permanently**, on top of anything convoy interdiction achieves. This is a second, independent way to attack enemy supply beyond hitting convoys.

## Factories

Factories are a distinct objective type (see [Objectives](./objectives.md)) that actively produce ground units over time for their owning coalition. An active enemy factory will keep replenishing ground forces at nearby objectives — strike them early to limit spawns before the enemy builds up a defense you can't crack.

## The Logistics System

### What is Logistics?

**Logistics tracks two main resources**:
1. **Equipment** (weapons, vehicles, parts)
2. **Fuel** (aviation fuel, vehicle fuel)

**Three levels of infrastructure**:
1. **Logistics (Logi)** - Infrastructure health
2. **Supply** - Equipment inventory level
3. **Fuel** - Fuel inventory level

## Supply Flow

### Logistics Hubs

**Central Distribution**:
- Logistics hubs are special objectives
- They distribute supplies to connected objectives
- Form the backbone of supply network

**Hub Connections**:
- Each hub connects to multiple objectives
- Supply flows automatically
- Captured objectives reconnect supply lines

### Supply Routes

Supplies flow from:
```
Logistics Hub → Frontline Objectives → FARPs
```

**Route Characteristics**:
- Automatic distribution every tick interval
- Prioritizes objectives with lowest supply
- Distance affects delivery amount
- Broken routes halt supply flow

### Supply routing follows the front line

A hub is **only a candidate supplier for a base if the ground between them is
clear of enemy-held objectives**. A base whose road is cut can be resupplied by
air, by player crate runs, or by an
[AI helo run](../advanced/helo-missions.md) — and by nothing else.

Enemy-held objectives interdict a belt
**{{cfg:warehouse.route_block_margin_m|12000}} m** wide either side of
themselves, on top of their own zone radius. That is wide enough that a handful
of objectives forms a genuine **supply corridor**, and taking the objectives
astride an enemy road starves everything behind it without you ever attacking
the base itself.

### A "logistics detached" base is on its own

An objective the engine has flagged **fully detached** from the supply chain
gets **no automatic resupply at all** — no convoy, no cargo aircraft, nothing.
Its base card says so:

```
NOTE: logistics detached -- no automatic resupply
```

It has to be sustained by hand: player crate runs, C-130 drops, or an
[AI helo resupply run](../advanced/helo-missions.md). Know this before you take
an objective deep behind the line — you are signing up to feed it.

### Hubs keep a reserve

A hub will not ship its last drop forward: **{{cfg:warehouse.hub_reserve_percent|20}}%**
of its capacity is held back from automatic distribution, so one lost convoy
doesn't empty the theatre. The reserve is still available for hub-to-hub
balancing and for the player-driven supply transfer.

### How supplies actually move

Three carriers, each with its own strengths and its own vulnerability:

| Carrier | Live? | Speed | Notes |
| --- | --- | --- | --- |
| Ground convoys | {{cfg:warehouse.convoy.enabled|yes}} | {{cfg:warehouse.convoy.speed_kph|60}} km/h | {{cfg:warehouse.convoy.trucks_per_convoy|5}} trucks each, up to {{cfg:warehouse.convoy.max_concurrent_convoys|10}} at once. Follows roads, blocked by the front line, killable. |
| Air logistics | {{cfg:warehouse.air_logistics.enabled|yes}} | {{cfg:warehouse.air_logistics.speed_kph|400}} km/h | Crosses the front line. Up to {{cfg:warehouse.air_logistics.max_concurrent_routes|6}} routes, triggered when a base drops below {{cfg:warehouse.air_logistics.supply_threshold|50}}% supply. Killable, and a fat target at {{cfg:warehouse.air_logistics.altitude_m|2500}} m. |
| Sea logistics | {{cfg:warehouse.sea_logistics.enabled|no}} | {{cfg:warehouse.sea_logistics.speed_kph|30}} km/h | Naval resupply, where the map and the server enable it. |

**Every one of these is a target.** Killing an enemy transport aircraft or a
convoy truck earns points *and* removes supply that was about to arrive
somewhere. See `F10 → Info → Supply Convoys` for what is in transit on your
side; the enemy has the same page for theirs.

Ground convoys despawn on arrival, so a convoy still on the map is still
carrying something.

### Supply Ticks

The system runs on a **tick cycle**:

| | |
| --- | --- |
| Tick interval | every **{{cfg:warehouse.tick|10}} minutes** |
| Ticks per outside delivery | **{{cfg:warehouse.ticks_per_delivery|12}}** |

Distribution happens on every tick; a fresh delivery from outside the theatre
arrives every `ticks_per_delivery` ticks. No player action is needed for either.

**During each tick**:
1. The engine assesses every objective.
2. It works out who is short.
3. It ships from the hubs that can legally reach them (see **Supply routing**
   below) — holding back the hub's operational reserve.
4. Convoys, transport aircraft or ships actually carry it, and it lands when
   they arrive.

## Supply & Fuel Levels

### Supply Percentage

Represents equipment and munitions:

- **100%**: Fully stocked
- **75-99%**: Good condition
- **50-74%**: Adequate supplies
- **25-49%**: Low stocks
- **0-24%**: Critical shortage

**Effects of Low Supply**:
- Reduced repair speeds
- Limited deployments available
- Decreased operational tempo
- Warehouse capacity reduced

### Fuel Percentage

Represents aviation fuel stocks:

- **100%**: Full fuel reserves
- **75-99%**: Good fuel stocks
- **50-74%**: Adequate fuel
- **25-49%**: Low fuel
- **0-24%**: Fuel emergency

**Effects of Low Fuel**:
- Aircraft cannot rearm
- Helicopter operations limited
- May prevent takeoffs
- Logistics vehicles affected

## Supply Priorities

### Automatic Distribution

The system prioritizes:
1. **Lowest supply first** - Most desperate get priority
2. **Connected objectives** - Must have supply route
3. **Available inventory** - Hub must have supplies

**Example Priority**:
```
Objective A: 20% supply → Gets first priority
Objective B: 45% supply → Gets second priority
Objective C: 80% supply → Gets last priority
```

## Logistics Infrastructure (Logi)

### What is Logi?

Logi represents the physical infrastructure:
- Buildings
- Roads and railways
- Communications
- Support facilities

### Logi Percentage

- **100%**: Perfect condition
- **75-99%**: Minor damage
- **50-74%**: Moderate damage
- **25-49%**: Heavy damage
- **1-24%**: Critical damage
- **0%**: Destroyed (objective capturable!)

### Logi Effects

**High Logi (75-100%)**:
- Fast repair times
- Efficient supply processing
- Normal operations

**Medium Logi (25-74%)**:
- Slower operations
- Reduced efficiency
- Still functional

**Low Logi (1-24%)**:
- Severely impaired
- Very slow repairs
- Minimal functionality

**Zero Logi (0%)**:
- **OBJECTIVE CAN BE CAPTURED**
- No supply processing
- No repairs possible
- Critical vulnerability

## Repairing Logistics

### Automatic repair

An objective with working logistics repairs itself on a pulse, every
**{{cfg:repair_time|1800}} seconds**, and each repair **costs the base
supplies** — {{cfg:repair_supply_cost|5}}% of stock per group repaired, plus
{{cfg:warehouse.materiel.repair_cost|250}} materiel where the server runs the
materiel economy.

**If the base doesn't have it, the repair doesn't happen.** It stays broken
until something reaches it. This is the single most important consequence of the
supply system, in both directions:

- A target of yours that stops healing has run dry. **That is the moment to take
  it** — check `F10 → Objectives → Capture Advisor`, whose `REPAIR:` line tells
  you whether the base is still repairing and when the next pulse lands.
- A base of yours that won't repair is not bugged. It is starving. Fix the
  supply line.

### Repair kits (crates)

The player-flown fix. Request a **Logistics Repair Kit** crate from the cargo
menu, fly it to the objective, and unpack it there:

```
F10 → Cargo → Crates → Logistics → Logistics Repair Kit
```

One crate is enough ({{cfg:repair_crate.Blue.required|1}} required). Delivering
one also earns **{{cfg:points.logistics_repair|350}} points** — comparable to an
air kill — and at a base that has just been captured it pushes the
[consolidation](./capturing-objectives.md) clock forward.

Carrier repairs work the same way with a **Carrier Repair** crate; see
[Carrier Operations](./carrier-ops.md).

### Supply transfer crates

**Base Fuel Resupply** and **Base Ammo Resupply** crates move
{{cfg:warehouse.supply_transfer_size|25}}% of a base's supply to another one.
Delivering one pays {{cfg:points.logistics_transfer|350}} points. This is the
manual way to push stock into a base the automatic distribution cannot reach —
a cut-off objective, or one you have just taken.

## Warehouse System

Every objective has a warehouse, and what it holds is what you can actually fly
and build from there.

### What's in it

| Line | Meaning |
| --- | --- |
| `S:` Supply | Equipment and munitions |
| `F:` Fuel | Aviation and vehicle fuel |
| `A:` Airframes | Aircraft physically on the ramp |
| Materiel | The generic war-stock that pays for repairs and deployments |

Read them from `F10 → Objectives → Friendly Status`, or the per-base card.

### Stock depth

| | Hub | Forward base |
| --- | --- | --- |
| Supplies | {{cfg:warehouse.hub_max|1}} × delivery | {{cfg:warehouse.airbase_max|1}} × delivery |
| Airframes | {{cfg:warehouse.airframe_max[0]|3}} × source count | {{cfg:warehouse.airframe_max[1]|1}} × source count |

Airframes are capped separately from ammunition on purpose — sharing the supply
depth would leave a base with a squadron parked on it that never runs short
however badly the war was going.

**So the airframe you want may not be there.** That isn't a bug; it is the base
being out of that type. Fly something else, or fly from somewhere else.

### The economy underneath

Materiel, production scaling, captured airframes and the front-line routing
rules have a page of their own:
**[Materiel & the War Economy](./war-economy.md)**. If you want to understand
why a round is being won or lost, that is the page.

### Deployment costs

Unpacking a crate draws {{cfg:deploy_supply_cost|3}}% supply — plus
{{cfg:warehouse.materiel.deploy_cost|100}} materiel where enabled — from the
**crate's origin objective**, not from where you unpack it. Building a SAM ring
in the desert spends the stock of the base you picked the crates up from.

## Supply Strategies

### Offensive Strategy

**Attacking Enemy Supply**:
1. **Target logistics hubs** - Cut off multiple objectives
2. **Interdict supply routes** - Attack connecting objectives
3. **Reduce frontline supply** - Weaken enemy operations

**Maintaining Your Supply**:
1. **Protect logistics hubs** - Heavy air defense
2. **Secure supply routes** - Defend connecting objectives
3. **Keep logi above 0%** - Prevents captures

### Defensive Strategy

**Supply Line Defense**:
- Deploy SAMs at logistics hubs
- Maintain CAP over critical objectives
- Repair logi quickly when damaged
- Keep fuel reserves high

**Emergency Response**:
- If logi falls to 0%, immediate priority repair
- Rush fighters to defend against capture
- Deploy ground units to contest zone

## Reading Supply Information

### F10 Map Markers

Typical format:
```
Musa Airbase
Health: 85
Logi: 42
Supply: 75
Fuel: 100
Points: 0
```

- **Health**: 85 - Facility condition
- **Logi**: 42 - Infrastructure (safe from capture, above 0)
- **Supply**: 75 - Equipment stocks (good level)
- **Fuel**: 100 - Fuel stocks (full)
- **Points**: 0 - Capture point value

**Note**: Values are whole numbers 0-100.

### In-Game Notifications

System messages for supply events:
- "Objective supply critical" - Below 25%
- "Objective fuel emergency" - Below 25%
- "Logistics damaged" - Logi falling
- "Objective capturable" - Logi at 0%

## Logistics Transfers

### Manual Transfers

Admin or special actions can transfer supplies:

**Command**:
```
-admin transfer <from-objective> <to-objective>
```

**Use Cases**:
- Emergency supply to starved objective
- Balancing supply distribution
- Preparing for major operations

**Restrictions**:
- Requires admin privileges (for `-admin` variant)
- Limited by warehouse capacity
- Both objectives must be owned

## Advanced Topics

### Supply Line Optimization

**Efficient Network**:
- Capture objectives in logical order
- Maintain control of connecting objectives
- Don't overextend supply lines

**Example Bad Strategy**:
```
Hub → A → (enemy) → B → Front
```
Objective B is cut off!

**Example Good Strategy**:
```
Hub → A → B → Front
```
Clear supply line maintained.

### Logistics as Weapon

**Starve Enemy Objectives**:
1. Identify their logistics hubs
2. Strike them repeatedly
3. Target connecting objectives
4. Wait for supply depletion
5. Attack when weakened

**Siege Warfare**:
- Surround enemy objective
- Cut off supply routes
- Wait for supplies to deplete
- Capture when logistics fail

### Supply Consumption

Different operations consume supplies:

**High Consumption**:
- Repairing damaged aircraft
- Deploying heavy armor
- Sustained combat operations
- Large-scale actions

**Low Consumption**:
- CAP flights
- Basic repairs
- Small unit deployments

## Troubleshooting

### "Why is my objective low on supply?"

Possible causes:
- Logistics hub captured by enemy
- Supply route broken
- High consumption rate
- Insufficient tick intervals passed

**Solution**:
- Check supply route integrity
- Protect logistics hubs
- Wait for next supply tick
- Reduce unnecessary deployments

### "Logi won't repair"

Possible causes:
- Supply level too low
- Recent damage faster than repair
- Server settings

**Solution**:
- Wait for supply delivery
- Use manual repair action
- Defend objective from attacks

## See Also

- [Materiel & the War Economy](./war-economy.md) — production, routing, captured airframes
- [AI Helo Missions](../advanced/helo-missions.md) — the fastest resupply you can order
- [Cargo Operations](../f10-menu/cargo.md) — flying crates yourself
- [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md)
- [The Tasking Board](./tasking-board.md) — posting a SUPPLY task and letting it close itself
- [Points and Lives](./points-and-lives.md)
