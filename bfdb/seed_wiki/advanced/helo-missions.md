# AI Helo Missions

You can order the campaign to fly a logistics helo for you. It cold-starts at a
real friendly field, flies a real route, **lands for real** at the destination,
and only then delivers — troops on the ground, or supply into the warehouse.
Shoot it down en route and nothing arrives.

```
F10 → Actions>> → AI Helo Missions
├── Insert Troops: Nearest Capturable
├── Insert Troops: Capturable Now     → [base]
├── Insert Troops: Any Objective      → [base]
├── Resupply: Nearest Friendly
└── Resupply: Friendly Base           → [base]
```

Any slot can order one — you do not have to be in a helo. It costs points, not
your time.

## Why you would use it

**Troop insertion** puts capture-capable infantry into an objective's zone
without anybody flying a 40-minute Mi-8 round trip. That is the single biggest
tax on capturing anything, and this pays it for you.

**Resupply** moves supply from your nearest hub into a base that is running dry
— the thing that stops a front-line base generating sorties, repairing itself,
or holding after a capture. It is also the fastest way to close a `SUPPLY` task
on the [tasking board](../gameplay/tasking-board.md).

Neither is free of risk. The helo flies at {{cfg:helo_insertion.altitude_m|500}}
m and {{cfg:helo_insertion.speed_kph|220}} km/h in a straight line. If that line
crosses a live SAM, you have bought an expensive fireball.

## Troop insertion

Pick the target base. The engine:

1. Finds the **nearest friendly launch field** — an airbase, FARP or FOB with a
   live DCS pad. Not a hub with no pad, not a carrier.
2. Checks the range. Beyond
   **{{cfg:helo_insertion.max_range_m|150000}} m** from the nearest such field,
   the mission is refused with the distance in the message.
3. Charges you the troop's own cost plus the mission fee
   ({{cfg:helo_insertion.troop_mission_cost|0}} points).
4. Spawns the helo and flies it.
5. On landing within
   **{{cfg:helo_insertion.landing_radius_m|200}} m** of the objective, deploys a
   **{{cfg:helo_insertion.troop_name|Standard}}** squad there and despawns.

The squad belongs to you, exactly as if you had unloaded it from your own
aircraft — it holds the zone, it counts toward the capture timer, it speeds up
consolidation.

**Refusals you will see:**

- *"X is already yours and isn't under threat"* — troop insertion is for taking
  ground or defending ground under attack, not for garrisoning quiet bases.
- *"no friendly field able to launch the mission"* — nothing with a live pad in
  reach. Take a closer base, or build a FARP.
- *"nearest friendly launch field (X) is N km away, past the M km max range"* —
  self-explanatory. Shorten the problem first.

### Which list to use

- **`Insert Troops: Nearest Capturable`** — one click, no picking. What you want
  90% of the time.
- **`Insert Troops: Capturable Now`** — only bases that are actually takeable
  this minute. If this list is empty, nothing is ready and troops would be
  wasted; go read the [Capture Advisor](../f10-menu/objectives.md) instead.
- **`Insert Troops: Any Objective`** — every enemy base, for softening one up
  ahead of the assault.

## Resupply

Pick a friendly base. The engine finds the nearest field with **surplus supply
on hand**, loads
{{cfg:helo_insertion.supply_amount_per_item|50}} of each item it can spare,
charges **{{cfg:helo_insertion.supply_mission_cost|50}} points**, and flies it in.

On landing the supply lands in that base's warehouse. It also counts as a
delivery for anything waiting on one — a `SUPPLY` task, and
[consolidation progress](../gameplay/capturing-objectives.md) at a base that has
just been taken.

**Refusals:**

- *"X has no surplus supply on hand to send"* — the origin is as dry as the
  destination. Fix the supply line, not the symptom; see
  [Logistics & Supply](../gameplay/logistics.md).
- Range refusals work the same as troop insertion.

## How it behaves in the world

- **It is a real group.** It shows on the F10 map, it shows on radar, the enemy
  can see and engage it.
- **It flies a straight line** from launch field to destination. Look at that
  line before you spend the points.
- **Delivery happens on landing, not on arrival overhead.** A helo that is shot
  down on short final delivers nothing.
- **Progress is polled every 10 seconds**, so expect a few seconds between the
  wheels touching and the delivery message.
- **It despawns after delivering.** You do not have to clean it up.

## Using it well

- **Escort it when it matters.** A CAP or SEAD pass down the route, or a
  [`CAS` task posted](../gameplay/tasking-board.md) on the threat, turns a coin
  flip into a delivery.
- **Insert troops *after* the base is eligible, not before.** Troops sitting in
  a zone that still has six infantry defenders and 60% health just die. Read the
  Capture Advisor first.
- **Stack it with your own flying.** Order the insertion, then go do the
  grinding. The helo arrives while you are working.
- **Resupply the base that is about to consolidate.** A delivery during the
  post-capture hold pushes the consolidation clock forward — the fastest way to
  lock a base you have just taken.

## See Also

- [Actions Menu](../f10-menu/actions.md)
- [Capturing Objectives](../gameplay/capturing-objectives.md)
- [The Tasking Board](../gameplay/tasking-board.md)
- [Logistics & Supply](../gameplay/logistics.md)
- [Troop Transport](../f10-menu/troops.md) — doing it yourself
