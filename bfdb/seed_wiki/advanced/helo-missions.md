# AI Helo Missions

You can order the campaign to fly a logistics helo for you. It starts cold on
the ground at a real friendly field — engines off, AI runs its own startup —
flies a real route, **lands for real** at the destination,
and only then delivers — troops on the ground, or supply into the warehouse.
If a troop helo fails, the squad is **driven in by road** instead; if that
fails too, or it was a supply run, you get your points back.

`AI Helo Missions` sits at the end of the [Actions menu](../f10-menu/actions.md)
whenever the server has these missions on. Bases are listed nearest first with
their range from you in nautical miles; `[CAP]` marks one that can be captured
right now. Long lists are split into alphabetical ranges.

```f10menu
{"root":"F10 Other","items":[
 {"label":"Actions","items":[
  {"label":"AI Helo Missions","items":[
   {"label":"Insert Troops: Nearest Capturable","says":"Helo troop insertion dispatched (HELO_blue_3_1761830412).","note":"Picks the nearest enemy or neutral base, preferring one that is capturable now."},
   {"label":"Insert Troops: Capturable Now","note":"Only present when something is capturable or being captured.","items":[
    {"label":"Gori 23nm [CAP]","says":"Helo troop insertion dispatched (HELO_blue_4_1761830470)."},
    {"label":"Khashuri 41nm [CAP]","says":"Could not dispatch: not enough points for a helo troop insertion (60 needed, 35 available)"}
   ]},
   {"label":"Insert Troops: Any Objective","items":[
    {"label":"Gori 23nm [CAP]","says":"Helo troop insertion dispatched (HELO_blue_5_1761830502)."},
    {"label":"Tskhinvali 31nm","says":"Helo troop insertion dispatched (HELO_blue_6_1761830530)."},
    {"label":"Beslan 95nm","says":"Could not dispatch: nearest friendly launch field (Senaki) is 162km away, past the 150km max range"}
   ]},
   {"label":"Resupply: Nearest Friendly","says":"Helo resupply run dispatched (HELO_blue_7_1761830560).","note":"Sends supply to the friendly base nearest you."},
   {"label":"Resupply: Friendly Base","items":[
    {"label":"Senaki 12nm","says":"Helo resupply run dispatched (HELO_blue_8_1761830600)."},
    {"label":"Kobuleti 30nm","says":"Could not dispatch: Batumi has nothing on hand that Kobuleti has room for"}
   ]}
  ]}
 ]}
]}
```

The text version: `Actions → AI Helo Missions`, then `Insert Troops: Nearest
Capturable`, `Insert Troops: Capturable Now → [base]`, `Insert Troops: Any
Objective → [base]`, `Resupply: Nearest Friendly` or `Resupply: Friendly Base
→ [base]`.

Any slot can order one: you do not have to be in a helo. It costs points, not
your time.

## Why you would use it

**Troop insertion** puts capture-capable infantry into an objective's zone
without anybody flying a 40-minute Mi-8 round trip. That is the single biggest
tax on capturing anything, and this pays it for you.

**Resupply** moves supply from your nearest hub into a base that is running dry
— the thing that stops a front-line base generating sorties, repairing itself,
or holding after a capture. It is also the fastest way to close a `SUPPLY` task
on the [tasking board](../gameplay/tasking-board.md).

Neither is free of risk. The helo cruises at
{{cfg:helo_insertion.speed_kph|220}} km/h on a route planned around the terrain
**and around the air defence your side knows about** — see
[Staying alive](#staying-alive). Air defence nobody has reported is still a
surprise, and a target inside its own defences can only be flown into low and
fast. The points come back if it dies; the time does not.

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
5. On landing inside the objective's zone, deploys a
   **{{cfg:helo_insertion.troop_name|Standard}}** squad where it put down and
   despawns. It lands at the safest spot in the zone it can find, not
   necessarily the middle — see [Staying alive](#staying-alive).

The squad belongs to you, exactly as if you had unloaded it from your own
aircraft — it holds the zone, it counts toward the capture timer, it speeds up
consolidation.

**Refusals you will see** (after `Could not dispatch:`):

- *"Senaki is already yours and isn't under threat"*: troop insertion is for
  taking ground or defending ground under attack, not for garrisoning quiet
  bases.
- *"no friendly field able to launch the mission (a launch field has to be an
  airbase, FARP or FOB with a live DCS pad)"*: nothing with a live pad in
  reach. Take a closer base, or build a FARP.
- *"nearest friendly launch field (Senaki) is 162km away, past the 150km max
  range"*: shorten the problem first.
- *"not enough points for a helo troop insertion (60 needed, 35 available)"*.
- *"AI helo missions are not enabled"*, or a missing helicopter for your side:
  a server configuration problem; tell an admin.

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

```dcsmessage
your helo delivered its supply run to Senaki
```

If the base fell while the helo was inbound: *"your helo landed at Senaki, but
it is no longer ours -- the supply went back"*.

**Refusals** (after `Could not dispatch:`):

- *"Batumi has nothing on hand that Senaki has room for"*: the launch field has
  nothing to spare, or the destination is already full. Fix the supply line,
  not the symptom; see [Logistics & Supply](../gameplay/logistics.md).
- *"nearest friendly hub (Batumi) is 162km away, past the 150km max range"*.
- *"not enough points for a helo supply run (50 needed, 35 available)"*.
- *"the launch field had nothing left to load"*.

## How it behaves in the world

- **It is a real group.** It shows on the F10 map, it shows on radar, the enemy
  can see and engage it.
- **It flies a terrain-aware route** — see below. It is not a straight line any
  more, so check the map rather than assuming the ruler.
- **Delivery happens on landing, not on arrival overhead.** A helo that is shot
  down on short final delivers nothing — a troop run then goes by road (see
  below), a supply run is refunded.
- **Progress is polled every 10 seconds**, so expect a few seconds between the
  wheels touching and the delivery message.
- **It despawns after delivering.** You do not have to clean it up.

## Staying alive

The helo plans its route around the enemy air defence **your side knows
about** — nothing more. It never cheats: a SAM nobody on your side has found is
as invisible to it as it is to you. What it does know:

- **Your intel picture.** Every air-defence contact on your side's map — from
  recon passes, JTACs, special forces, AWACS and EWR fusion. Each is treated as a
  ring as wide as what is actually there can reach (or
  {{cfg:helo_insertion.threat_avoidance.unknown_radius_m|10000}} m when the
  contact says nothing about that), plus how unsure the contact's position is.
- **Enemy bases on the F10 map.** Every enemy objective whose garrison is still
  standing is assumed to have MANPADS and guns out to
  {{cfg:helo_insertion.threat_avoidance.garrison_radius_m|3000}} m beyond its
  zone. You can see those bases; so can it.
- **What it sees itself.** A SAM that launches or guns that open up near it, a
  hit, or a radar on its own warning receiver (if the airframe has one).

Every ring is widened by ×{{cfg:helo_insertion.threat_avoidance.margin|1.25}}
for safety. Then:

1. **Route.** It takes the shortest route that stays out of every ring. If there
   is none within about 1.6× the direct distance — the target sits inside its own
   defences, or a line of SAMs is too wide to go round — it takes the route that
   spends the **least time inside** them, preferring the weaker ones. You are
   told when the route bends around threats, and when there was no clean way in.
2. **Height.** Near known threats and on the final approach it flies
   **nap-of-the-earth**, about
   **{{cfg:helo_insertion.threat_avoidance.noe_agl_m|40}} m** over the ground.
   Everywhere else it keeps the normal terrain clearance (below).
3. **Landing spot.** It puts down at the spot in the zone **farthest from known
   enemies** that is flat, on land and clear of units — still well inside the
   zone, so the troops count for the capture. With nothing known about, that is
   the middle, as always.
4. **In flight.** If it comes under fire, or new air defence is reported across
   the rest of its route, it **re-plans from where it is** and you are told. At
   most {{cfg:helo_insertion.threat_avoidance.max_replans|4}} times per mission,
   and not more than once every
   {{cfg:helo_insertion.threat_avoidance.min_replan_secs|20}} s. Inside the last
   3 km it is committed and just lands.
5. **Behaviour.** It holds its fire (it is there to land, not to fight), jinks
   when shot at and then carries on, drops flares whenever it is inside a SAM's
   reach, and does not turn for home on low fuel.

If your side knows of nothing anywhere near the route, it flies exactly the
terrain route described next.

## How it routes

**Without known threats it flies straight at the objective.** What matters is
the altitude: it climbs and descends with the ground underneath it, instead of
holding one fixed height and meeting the first ridge taller than that — which is
how these used to be lost.

When you call the mission the engine samples the terrain along the track and
builds a profile from it:

1. The route is cut into steps of about
   **{{cfg:helo_insertion.waypoint_spacing_m|2500}} m**, and each step is given
   the highest ground under it.
2. Each waypoint is placed **{{cfg:helo_insertion.terrain_clearance_m|250}} m**
   above the higher of the two steps meeting there, and never below
   {{cfg:helo_insertion.altitude_m|500}} m. Because both ends of a step sit
   above everything in it, the line actually flown clears the ground the whole
   way, not just at the waypoints.
3. Steps of similar height are merged, so flat desert gets two or three
   waypoints and rolling ground gets one per contour — the helo climbs for the
   ridge and comes back down the far side rather than staying at the height of
   the highest thing on the whole route.
4. Each climb is then given **only the distance it needs** at a helicopter's
   climb rate. It stays low and starts up near the high ground, rather than
   easing up from the moment it took off, and it comes back down as soon as the
   ground drops away instead of gliding down the whole leg.
5. It rolls out on final short of the objective and flies a normal approach down
   to the landing point.

The one thing this cannot fix is terrain a loaded helicopter simply cannot
out-climb. The route still goes **over** it, because flying under a ridge is not
an option, and the server log says so when the top of the route goes past
**{{cfg:helo_insertion.max_altitude_m|3500}} m**. That usually means you are
asking for a delivery across a range no loaded helo should be crossing — take a
closer launch field instead.

*(Server admins: `lateral_avoidance` will additionally let the planner dogleg
around ground that high rather than climbing it. It is off here — the straight
track is shorter, more predictable, and spends less time exposed, and nothing on
this map is tall enough to need it.)*

## If it does not arrive

### Troop insertion: the road fallback

If the helo lets you down — it never starts up, it is shot down, or it never
gets down at the target — the engine does not give up on the squad. It sends
the **same squad by road** instead:

1. It picks the **nearest objective your side owns** (anything but a carrier)
   within **{{cfg:helo_insertion.ground_fallback.max_range_m|60000}} m** of the
   target that has a **road to it**.
2. One vehicle group sets off from there at
   {{cfg:helo_insertion.ground_fallback.speed_kph|50}} km/h, on the roads, and
   leaves the road for the last stretch into the zone. You are told where it is
   coming from and roughly how long it will take.
3. Once it is within
   **{{cfg:helo_insertion.ground_fallback.arrive_m|400}} m** of the zone — or
   has stopped for a minute within twice that — your squad dismounts inside the
   zone and the vehicle goes away. The squad is yours, exactly as if the helo
   had landed.

It is a real vehicle on a real road: it shows on the map, and the enemy can
ambush it. Covering the road does for it what escorting the helo does.

You are **refunded** instead if there is no friendly objective in range with a
road to the target, if the vehicle is destroyed, if it gets stuck, or if it
runs out of time (about twice the planned drive). A server restart also
refunds a road insertion that is still under way — the vehicle does not survive
the restart.

### Resupply

Losing a supply helo — to terrain, to a SAM, to a fighter — refunds what the
mission cost you. You are told in the panel when it happens. Nothing is
delivered, so the base is no better off; you have only lost the round trip.

## What you will be told

Everything goes to the player who called the mission.

```dcsmessage
your helo landed at Gori, troops are on the ground -- keep them alive in the zone to capture it
```

| Message | When |
| --- | --- |
| `Helo troop insertion dispatched (HELO_blue_3_1761830412).` / `Helo resupply run dispatched (...)` | accepted; the id names the mission |
| `your helo to Gori is routed around 2 known air-defence threat(s) (+14 km), flying low near them` | the route bends around known air defence |
| `no clean way into Gori past known air defence -- your helo takes the least-defended gap, flying low` | there was no threat-free route |
| `your helo to Gori is under fire -- re-routing` | it was shot at and re-planned |
| `your helo to Gori is re-routing around newly reported air defence` | new air defence was reported across its route |
| `your helo landed at Gori, troops are on the ground -- ...` | troops delivered |
| `your helo landed at Gori but the troops could not be deployed: ...` | landed, but the squad could not spawn (for example the squad limit) |
| `your helo delivered its supply run to Senaki` | supply delivered |
| `helo mission to Gori lost en route` | shot down or crashed on the way |
| `helo mission to Gori lost on the ground at Senaki before it took off` | destroyed on the ramp |
| `your helo to Gori never took off from Senaki: ..., mission cancelled` | it never got airborne (the reason is given) |
| `helo mission to Gori never made it down, recalled` | it ran out of time without landing |
| `helo mission target lost` | the destination no longer exists |

For a supply run (or a troop run with the road fallback off) these failure
messages end in `, refunding 50 points` when there is something to refund.
A failed troop run goes by road instead:

```dcsmessage
helo mission to Gori lost en route -- sending the squad to Gori by road from Kareli instead, ETA ~18 min
```

and then either
`your squad dismounted at Gori after the drive from Kareli -- keep them alive in the zone to capture it`,
or `road insertion to Gori failed: the vehicle carrying the squad was destroyed -- refunding 60 points`
(other reasons: `the vehicle got stuck 2.4 km short of Gori`,
`the vehicle didn't reach Gori in 46 min`, `its target no longer exists`,
`the squad could not be deployed: ...`). If no road trip is possible the first
message says why instead: `...; the squad can't go by road either: nothing of
ours within 60 km has a road to Gori -- refunding 60 points`.

## Using it well

- **Find the threats first.** The helo only avoids what your side has reported.
  A [recon pass](../f10-menu/recon.md) or a JTAC over the route before you call
  it is the difference between a detour and a surprise.
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
