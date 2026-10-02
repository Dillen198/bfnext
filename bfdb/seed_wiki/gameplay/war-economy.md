# Materiel & the War Economy

Under the supply percentages you see on the F10 map there is a second economy:
a single generic war-stock commodity called **materiel**. It is what repairs and
deployments are actually paid for, it is produced at a rate that depends on how
the war is going, and it has to physically reach the base that wants to spend it.

This is the layer that decides long rounds. You can win every engagement and
still lose because your bases cannot afford to repair themselves.

> Every figure on this page is read from the engine config of the server
> selected at the top of the page. All of these systems are **on** for the live
> mission; a server can run with any of them off, in which case the mechanic
> described doesn't apply there even though the numbers still render.

## Materiel

Think fuel drums, spares, engineering plant, generic ammunition — everything
that isn't a specific missile on a specific pylon.

| | |
| --- | --- |
| Produced per delivery, per side | {{cfg:warehouse.materiel.hub_production|2000}} |
| Hub stockpile cap | {{cfg:warehouse.materiel.hub_capacity|6}} × production |
| Forward-base stockpile cap | {{cfg:warehouse.materiel.airbase_capacity|3}} × production |
| Cost to repair one group | {{cfg:warehouse.materiel.repair_cost|250}} |
| Cost per crate unpacked | {{cfg:warehouse.materiel.deploy_cost|100}} |

**The rule that matters: if the objective doesn't have the materiel on hand, the
repair does not happen.** The base stays broken until a convoy gets through.
That is the whole point of running a supply line — and the whole point of
cutting the enemy's.

Unpacking a deployable draws materiel from the **crate's origin objective**, not
from where you unpack it. Building a SAM ring out in the desert still spends the
stock of the base you picked the crates up from.

### Why you care as a pilot

- A base you are grinding down that suddenly stops repairing is out of materiel.
  It will not come back until something reaches it. **That is the moment to take
  it.**
- One of your bases that won't repair is not bugged. Check its supply. See
  [Logistics & Supply](./logistics.md).
- Crate spam has a cost the whole coalition pays. Twenty crates unpacked is
  {{cfg:warehouse.materiel.deploy_cost|100}} × 20 of materiel that is now not
  repairing anything.

## Production scaling

Production is not a constant. Each side's output is tied to **how much of the
map it holds and how healthy that territory is**.

Every objective a side owns contributes its weight, scaled by that objective's
logistics health, and the side's output is the ratio of its current score to the
score it started the round with:

| Objective type | Weight |
| --- | --- |
| Logistics hub | {{cfg:warehouse.production_scaling.logistics_weight|3}} |
| Factory | {{cfg:warehouse.production_scaling.factory_weight|4}} |
| Command centre | {{cfg:warehouse.production_scaling.command_center_weight|2}} |
| Airbase | {{cfg:warehouse.production_scaling.airbase_weight|1}} |

Output is clamped between **{{cfg:warehouse.production_scaling.floor_percent|35}}%**
and **{{cfg:warehouse.production_scaling.ceiling_percent|125}}%** of nominal.
The floor exists so a side that is losing can still come back; the ceiling
exists so a side that is winning doesn't run away with it.

**The consequence:** a factory is worth four airbases to the enemy's war
production, and a logistics hub three. Striking a factory is not a side quest —
it is the highest-leverage ground target on the map, and flattening its
logistics is nearly as good as taking it.

It also means damage, not just ownership, moves the needle: an objective at 40%
logi contributes 40% of its weight. Sustained interdiction degrades enemy
production without anyone having to capture anything.

## Front-line supply routing

Supply does not teleport. A hub is only a candidate supplier for a base **if the
ground between them is clear of enemy-held objectives**. A base whose road is
cut can only be resupplied by air.

Enemy-held objectives interdict a belt
**{{cfg:warehouse.route_block_margin_m|12000}} m** wide either side of
themselves, on top of their own zone radius — a garrison denies the ground
around it, not just the runway.

This turns a handful of objectives into a **supply corridor**, and gives you a
way to strangle a base without ever attacking it:

- Take or hold the objectives between the enemy's hub and their front-line base.
  The base starves whether or not you ever go near it.
- Conversely, an isolated objective of yours deep in enemy ground is a permanent
  air-resupply problem. Know that before you take it.
- [AI helo resupply](../advanced/helo-missions.md) and player crate runs cross
  the front line freely. They are the answer to a cut road.

## Operational reserve

A hub will not ship its last drop forward. **{{cfg:warehouse.hub_reserve_percent|20}}%**
of a hub's capacity is held back from automatic distribution.

A depot that empties itself into the first convoy that asks has no reserve: one
lost convoy and the whole theatre is dry until the next production delivery.
The reserve is still available to hub-to-hub balancing and to the player-driven
supply transfer — it just isn't handed out automatically.

## Airframe depth

| | Hub | Forward base |
| --- | --- | --- |
| Supplies | {{cfg:warehouse.hub_max|1}} × delivery | {{cfg:warehouse.airbase_max|1}} × delivery |
| Airframes | {{cfg:warehouse.airframe_max[0]|3}} × source count | {{cfg:warehouse.airframe_max[1]|1}} × source count |

Aircraft and ammunition want different depths, so airframes get their own cap
rather than inheriting the supply one. Set them equal and a base ends up with a
carrier's worth of jets parked on it that never runs short however badly the war
is going.

**What this means in the cockpit:** the airframe you want may not be available
at the base you want it at. That is not a bug, it is the base being out of that
type. Check `Aircraft` on the base's detail card, or `A:` in
`F10 → Objectives → Friendly Status`, and fly something else or fly from
somewhere else.

## Captured airframes

When a base changes hands, some of the losing side's parked aircraft come with it.

| | |
| --- | --- |
| Fraction salvaged | {{cfg:warehouse.captured_airframes.salvage_percent|25}}% |
| Hard cap per type | {{cfg:warehouse.captured_airframes.max_per_type|4}} |
| Base health needed before they can be slotted | {{cfg:warehouse.captured_airframes.min_health|100}}% |

The rest is assumed destroyed on the ramp, sabotaged, or flown out ahead of the
assault.

They are **not flyable the instant the last defender dies** — the base has to be
consolidated and repaired to the health threshold first. So a freshly taken
airbase full of enemy jets is a promise, not a prize: hold it, repair it, then
fly them.

## Reading the economy

| Where | What it tells you |
| --- | --- |
| `F10 → Objectives → Friendly Status` | `S:` supply, `F:` fuel, `A:` airframes per base |
| `F10 → Info → Situation → 5. Logistics` | Which of your bases are starving and why |
| `F10 → Info → Supply Convoys` | What is in transit, and how far out |
| `F10 → Objectives → Capture Advisor` | The `REPAIR:` line — whether a target can heal faster than you can hurt it |
| Dashboard → Briefing | The same picture on a map |

## See Also

- [Logistics & Supply](./logistics.md) — convoys, hubs, air/sea logistics, crates
- [Objectives](./objectives.md) — what factories, hubs and command centres do
- [AI Helo Missions](../advanced/helo-missions.md)
- [Capturing Objectives](./capturing-objectives.md)
