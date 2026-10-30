# Logistics & Supply

Every base runs on what is in its warehouse. Supply pays for its repairs, the
aircraft you can slot into, the weapons on its ramp and the crates and troops
it can hand out. Cutting the enemy's supply is often more decisive than
destroying their aircraft, and keeping your own flowing is the quiet job that
wins rounds.

![How supply reaches the front: outside production feeds the logistics hubs every few ticks; each tick the hubs ship to bases that are short by convoy, airlift or ship, all of which can be shot up; players spawn crates and troops from a friendly base's stock and fly them by helicopter or C-130 to build deployables, transfer supply, repair logistics, make emergency repairs or capture bases; a base's stock pays for automatic repairs, aircraft, weapons and fuel, and the crates and troops it issues](/api/wiki/images/3f6c2a91-8d4e-4b7a-9c15-62e0d8a4b7f3)

## The supply chain at a glance

1. **Production.** Fresh stock arrives in the theatre every
   **{{cfg:warehouse.ticks_per_delivery|6}} ticks**, one tick being
   **{{cfg:warehouse.tick|10}} minutes**. How much depends on what your side
   holds; see [Materiel & the War Economy](./war-economy.md).
2. **Hubs.** Logistics hubs receive it and, every tick, ship to the bases that
   are short and that they can legally reach.
3. **Carriers.** Convoys, transport aircraft and ships carry it. Every one of
   them can be killed on the way.
4. **Bases.** What arrives sits in the base's warehouse until it is spent.
5. **Players.** Crates and troops are issued from a friendly base's stock and
   flown wherever they are needed: see [Cargo Operations](../f10-menu/cargo.md).

## Warehouses

Every objective has a warehouse, and what it holds is what you can actually fly
and build from there.

| Line | Meaning |
| --- | --- |
| `S:` Supply | Munitions and equipment |
| `F:` Fuel | Aviation and vehicle fuel |
| `A:` Airframes | Aircraft physically on the ramp |
| Materiel | The generic war stock that pays for repairs and crates, where the server runs the materiel economy |

Read them from `F10 → Objectives → Friendly Status`, or the per-base card.

### Stock depth

| | Hub | Forward base |
| --- | --- | --- |
| Supplies | {{cfg:warehouse.hub_max|25}} × delivery | {{cfg:warehouse.airbase_max|5}} × delivery |
| Airframes | {{cfg:warehouse.airframe_max[0]|3}} × source count | {{cfg:warehouse.airframe_max[1]|1}} × source count |

Airframes are capped separately from ammunition on purpose: sharing the supply
depth would leave a base with a squadron parked on it that never runs short
however badly the war was going.

**So the airframe you want may not be there.** Slotting into an aircraft the
base has run out of is refused:

```dcsmessage
Objective does not have any F-16C_50 in stock
```

That isn't a bug; the base is out of that type. Fly something else, or fly
from somewhere else. A base with no working logistics can't launch anything:
the slot answers `Objective is capturable`.

## How supplies move

### Hubs keep a reserve

A hub will not ship its last drop forward:
**{{cfg:warehouse.hub_reserve_percent|20}}%** of its capacity is held back from
automatic distribution, so one lost convoy doesn't empty the theatre. The
reserve is still available for hub-to-hub balancing and for supply transfers.

### Three carriers

| Carrier | On this server | Speed | Notes |
| --- | --- | --- | --- |
| Ground convoys | {{cfg:warehouse.convoy.enabled|yes}} | {{cfg:warehouse.convoy.speed_kph|60}} km/h | {{cfg:warehouse.convoy.trucks_per_convoy|5}} trucks each, up to {{cfg:warehouse.convoy.max_concurrent_convoys|10}} at once. Follows roads, blocked by the front line, killable. |
| Air logistics | {{cfg:warehouse.air_logistics.enabled|yes}} | {{cfg:warehouse.air_logistics.speed_kph|780}} km/h | Crosses the front line. Up to {{cfg:warehouse.air_logistics.max_concurrent_routes|6}} routes, sent when a base drops below {{cfg:warehouse.air_logistics.supply_threshold|50}}% supply. Killable, and a fat target at {{cfg:warehouse.air_logistics.altitude_m|2500}} m. |
| Sea logistics | {{cfg:warehouse.sea_logistics.enabled|no}} | {{cfg:warehouse.sea_logistics.speed_kph|30}} km/h | Naval resupply, where the map and the server enable it. |

Each base gets **one** convoy or airlift at a time, carrying weapons and fuel
together.

**Every one of these is a target.** Killing an enemy transport aircraft or a
convoy truck earns points *and* removes supply that was about to arrive
somewhere. `F10 → Info → Supply Convoys` shows what is in transit on your side;
the enemy has the same page for theirs.

Ground convoys despawn on arrival, so a convoy still on the map is still
carrying something. They keep driving under fire: they don't scatter off the
road when hit, so the column stays on its route, and stays a target. A convoy
that wedges somewhere (a wreck on the road, a bridge the AI won't cross) is
noticed after a few minutes and given a fresh road route, then a
cross-country one; if it is still stuck after that, it is withdrawn and its
load goes back to the hub. One that stalls where its road ends, close to the
base, counts as delivered.

### Routing follows the front line

A hub is **only a candidate supplier for a base if the ground between them is
clear of enemy-held objectives**. A base whose road is cut can be resupplied by
air, by player crate runs, or by an
[AI helo run](../advanced/helo-missions.md), and by nothing else.

Enemy-held objectives block a belt
**{{cfg:warehouse.route_block_margin_m|12000}} m** wide either side of
themselves, on top of their own zone radius. That is wide enough that a handful
of objectives forms a real **supply corridor**, and taking the objectives
astride an enemy road starves everything behind it without you ever attacking
the base itself.

The belt is relaxed at the **destination end**: an enemy objective whose belt
only reaches the base being supplied cuts the road only if the road runs
through its zone proper. So a base you've just taken on the front line can
still be supplied by road, as long as the road to it doesn't pass through the
enemy base next door.

### A "logistics detached" base is on its own

An objective flagged **detached** from the supply chain gets **no automatic
resupply at all**: no convoy, no cargo aircraft, nothing. Its base card says
so:

```dcsmessage
NOTE: logistics detached -- no automatic resupply
```

It has to be sustained by hand: player crate runs, C-130 drops, or an
[AI helo resupply run](../advanced/helo-missions.md). Know this before you take
an objective deep behind the line: you are signing up to feed it.

### Supply running out

When a base's supply, fuel or aircraft stock falls below
**{{cfg:supply_alert_threshold|20}}%**, its status hexes on the F10 map turn
red; there is no separate chat message. If it stays that low for
{{cfg:supply_auto_convoy_delay_secs|300}} seconds and nothing is already on the
way, the engine sends a relief load from its hub on its own (by road, or by air
if the road is cut), within the same convoy and airlift limits as everything
else.

## What a base's stock pays for

| Spent on | How |
| --- | --- |
| **Repairs** | Each automatic repair of a group costs {{cfg:repair_supply_cost|5}}% of the base's stock, or {{cfg:warehouse.materiel.repair_cost|250}} materiel where the materiel economy is on. A base that can't pay doesn't repair. |
| **Aircraft and weapons** | Taking off with an airframe, fuel and weapons takes them out of the warehouse (and landing back at a friendly base returns what you didn't use). |
| **Crates** | Each crate unpacked charges the base it was **spawned at**, not where you unpack it: {{cfg:deploy_supply_cost|3}}% of its stock per crate, or {{cfg:warehouse.materiel.deploy_cost|100}} materiel per crate with the materiel economy. Building a SAM site in the desert spends the stock of the base you picked the crates up from. |
| **Issuing crates and troops** | With the materiel economy on, a base with less than {{cfg:warehouse.materiel.deploy_cost|100}} materiel refuses to issue any: `Ramat David is out of materiel (40 on hand, 100 needed) -- it can't issue supply until a convoy gets through`. |

A base whose logistics are at 0%, or that has been knocked down to
capturable, issues nothing at all: no crates, no troops, no repair kits.

## Repairing a base

### Automatic repair

An objective with working logistics repairs itself one group at a time, on a
pulse of **{{cfg:repair_time|1800}} seconds** at full logistics, longer as its
logistics drop (at 50% logistics it takes twice as long). The countdown only
runs while the base is **left alone**: every time an enemy is in sight of it
the countdown starts over, so a base under sustained attack does not heal.

**If the base can't pay, the repair doesn't happen.** It stays broken until
supply reaches it. This is the single most important consequence of the supply
system, in both directions:

- A target of yours that stops healing has run dry. **That is the moment to
  take it.** `F10 → Objectives → Capture Advisor` has a `REPAIR:` line that
  tells you whether the base is still repairing and when the next pulse lands.
- A base of yours that won't repair is not bugged. It is starving. Fix the
  supply line.

### Logistics repair kits

The player-flown fix for a base's **logistics** (its infrastructure). Spawn a
**{{cfg:repair_crate.Blue.name|Logistics Repair}}** crate from
**Cargo → Crates → Base Supply**, fly it to the base and unpack it there. Each
kit raises the base's logistics one step:

```dcsmessage
Dillen repaired logistics at Megiddo to %80
```

- From the classic F10 cargo menu it has to be unpacked **inside the zone of a
  friendly base other than the one it was loaded at**.
- A dynamic (helicopter) or C-130 kit goes to the **nearest friendly
  objective** when it lands, and that may be the base it came from.
- At a base whose logistics are already full it is refused (the crate stays
  where it is), unless that base is still consolidating after a capture: then
  the kit buys {{cfg:consolidation_crate_progress_secs|120}} seconds of
  consolidation instead. See [Capturing Objectives](./capturing-objectives.md).

A kit delivered from the F10 menu pays
**{{cfg:points.logistics_repair|25}} points**, more for a long haul or a
front-line base, shared with whoever flew it if you only unpacked it.

### Emergency repair crates

Where the server enables them, **Base Supply** also offers an
**{{cfg:emergency_repair.crate.name|Emergency Repair}}** crate. It skips the
countdown: unpacked **inside the zone** of a damaged friendly base, it rebuilds
one destroyed group on the spot and restarts the repair countdown, just like an
automatic pulse would:

```dcsmessage
Emergency repair at Megiddo: AAA group rebuilt (health 70% -> 80%)
```

It works by any delivery (F10 menu, sling, ground-crew cargo or C-130 drop),
but only on a base that is **left alone**:

- friendly, damaged (under 100%) and not wiped out
- **not under attack**, **no enemy capture timer** running, and not still
  consolidating after its own capture
- **not the base the crate was loaded at**: fly it somewhere
- not a carrier group (carriers have their own repair crate)
- at most one every
  **{{cfg:emergency_repair.cooldown_secs_per_objective|600}} seconds** per
  base, so a stack of crates can't rebuild a base instantly

The repair is paid out of the **target base's own stores** exactly like an
automatic one, so a starving base can't be crate-repaired either: resupply it
first. Where the server sets a points price
({{cfg:emergency_repair.cost_points|0}} points) it is taken from the pilot who
delivered it, only when the repair goes through.

If the crate can't be used, **nothing is spent**: the crate stays on the ground
and you are told why. A dropped (dynamic or C-130) crate keeps retrying about
once a minute, so one that lands during an attack does its job as soon as the
base goes quiet. The reasons, word for word:

| Reason |
| --- |
| `an emergency repair crate has to be unpacked INSIDE the zone of the friendly base it is for` |
| `Megiddo is Neutral -- it has to be retaken with troops, it can't be repaired` |
| `Megiddo is an enemy base` |
| `Megiddo is a carrier group -- use the carrier repair crate there` |
| `Megiddo is already at full health` |
| `Megiddo is still consolidating after its capture -- nothing to repair yet` |
| `Megiddo has an enemy capture in progress -- clear the enemy troops first` |
| `Megiddo is under attack -- repairs can't start while enemy units are in sight of it` |
| `Megiddo's garrison is wiped out -- the base is falling to Neutral` |
| `this crate was loaded at Megiddo -- fly it to a different friendly base` |
| `Megiddo had an emergency repair recently -- next one possible in 4m 10s` |
| `Megiddo can't pay for the repair: 120 materiel on hand, 250 needed -- it needs a convoy or supply run first` |
| `Megiddo can't pay for the repair: supply 3% is under the 5% a repair costs -- resupply it first` |
| `an emergency repair costs 50 points and you have 20` |
| `Megiddo has no destroyed group an emergency repair can rebuild` |
| `emergency repairs are switched off on this server` |

### Reinforcement convoys

Where the server offers the **Reinforce** action, a convoy of transporters
drives replacement vehicles by road from the nearest friendly base that can
spare the materiel, and rebuilds destroyed garrison groups (armour first) when
it arrives. Fewer transporters arriving means fewer groups rebuilt. It can't go
to a base under attack, and the enemy is told when they destroy one.

```dcsmessage
Reinforcements for Megiddo leaving Ramat David: 2 group(s) on transporters, 38 km by road, about 42 min. Keep the road open.
```

On arrival your side reads `Reinforcements reached Megiddo: 2 group(s) back in
the line.`; a lost convoy reads `The reinforcement convoy for Megiddo was
destroyed on the road. Nothing arrives.` See
[Action Types](../reference/action-types.md#reinforce).

### Carriers

Carrier groups repair with a **Carrier Repair** crate, from the dynamic or
C-130 cargo menus. Each crate delivered to a damaged carrier starts or speeds
up its repair. See [Carrier Operations](./carrier-ops.md).

## Supply transfer crates

**Fuel Transfer** and **Weapons Transfer** crates (the names vary by server)
move stock from the base you load them at to another friendly base:
{{cfg:warehouse.supply_transfer_size|25}}% of the source's stock, never
airframes. This is the manual way to push stock into a base the automatic
distribution can't reach: a cut-off objective, or one you have just taken.
Unpacked from the F10 menu at a base that is still consolidating after a capture, it also speeds the consolidation up.

| Delivered by | Where it goes | What moves |
| --- | --- | --- |
| Classic F10 menu | Unpack it inside the zone of a friendly base other than its origin | Either crate moves weapons, equipment and fuel together |
| Dynamic or C-130 cargo | The nearest friendly objective when it lands, which must not be its origin | The fuel crate moves only fuel, the weapons crate only weapons and equipment |

A transfer delivered from the F10 menu pays
**{{cfg:points.logistics_transfer|15}} points**, scaled the same way as a
repair kit.

If you would rather pay points than fly, the **Logistics Repair** and
**Logistics Transfer** actions send an AI cargo flight to do the same jobs; see
[Action Types](../reference/action-types.md#logistics).

## Strategic infrastructure

Real map buildings near objectives (warehouses, fuel depots, industrial
complexes) are marked with **`logi:` pins** on the F10 map. Destroy them and
that objective's logistics rating drops, and it stays down until a logistics
repair kit or a capture brings it back. This works on top of anything convoy
interdiction achieves, as a second way to attack enemy supply.
[Buildings at an Objective](./objective-buildings.md) shows what to look for.

## Factories

Factories are a distinct objective type (see [Objectives](./objectives.md))
that produce ground units over time for their owner. An active enemy factory
keeps replenishing ground forces at nearby objectives: strike them early,
before the enemy builds up a defence you can't crack.

## Reading supply on the map

Supply state is read off the **status hexes** beside each base. The order
never changes:

```
   Health  Logi  Supply  Fuel
     ⬢      ⬢      ⬢      ⬢
```

Green is above 66%, amber 33 to 66%, red below 33%. A **gold outline** means
that resource is unlimited and will never run dry.

For logistics the two that matter are **Logi** (the infrastructure that lets a
base repair and issue crates) and **Supply** (munitions). A base with a red
logi hex has had its infrastructure flattened. Full key:
[Reading the F10 Map](./reading-the-map.md).

## Strategy

**Starving the enemy**

1. Identify the hubs and the roads out of them.
2. Take or neutralise the objectives astride those roads: the routing belt
   does the rest.
3. Hunt convoys and transport aircraft.
4. Flatten the `logi:` buildings at the target base.
5. Attack when the base stops repairing.

**Keeping your own side fed**

- Cover the hubs with air defence and CAP; losing one starves a sector.
- Don't take a base you can't feed. A detached base needs a crate run for every
  repair.
- Fly logistics repair kits to damaged bases before they hit 0%: a base at 0%
  logistics issues nothing and can be captured.
- Push supply transfer crates into fresh captures and cut-off bases.

**Bad and good lines**

```
Hub → A → (enemy) → B → Front     B is cut off: no road supply
Hub → A → B → Front               clear line, road supply flows
```

## Troubleshooting

**"Why is my base low on supply?"** Its road from the hub is cut, its hub is
short or lost, transports keep getting killed, or it is detached. Check
`F10 → Info → Supply Convoys`, and fly a supply transfer crate in the meantime.

**"The base won't repair."** It is under attack (the countdown restarts while
enemies are in sight), or it can't pay for the repair. Clear the area, then
resupply it. An emergency repair crate tells you which, word for word.

**"I can't spawn crates here."** You are outside the zone, the base's logistics
are at 0% or it is capturable, or it is out of materiel.

Admins have a `-admin transfer <from-objective> <to-objective>` command for
moving supply by hand; players use supply transfer crates.

## See Also

- [Materiel & the War Economy](./war-economy.md) - production, routing, captured airframes
- [Cargo Operations](../f10-menu/cargo.md) - flying crates yourself
- [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md)
- [AI Helo Missions](../advanced/helo-missions.md) - the fastest resupply you can order
- [The Tasking Board](./tasking-board.md) - posting a SUPPLY task and letting it close itself
- [Points and Lives](./points-and-lives.md)
