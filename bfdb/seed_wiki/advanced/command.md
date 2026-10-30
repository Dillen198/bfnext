# Command: the Commander's Map

The dashboard's **COMMAND** page is your coalition's war on one map: the
ground war's formations, bases and battles, and every other asset your side
has in the field, live, with the enemy you can see. Commanders give orders from
it; everyone on the side can watch.

The page is locked to your coalition: log in, and make sure your Discord is
linked (`-linkme` in DCS chat) and that you have taken a slot on the server
this campaign. Otherwise it shows **NO COALITION**.

## Becoming a commander

Command is earned. It unlocks at rank tier
{{cfg:command.commander_rank|4}} (Major, a campaign score of 50 at the
default): the same score and ranks the leaderboard shows, so captures,
logistics and kills all count toward it. An admin can also make you a
commander, or take it away.

Rank is campaign-wide, so it counts on every server, for whichever side you
are flying for there. Switch sides and you command your new side. On
Discord, commanders get that server's **Commander** role; your Blue or Red role still
shows your side, server by server, and follows you when you switch.

The page tells you where you stand: what unlocks command, and how far off you
are. In game, an order from someone who isn't a commander is refused with:

```dcsmessage
orders need a commander: reach Major (a campaign score of 50), or ask an admin.
```

The rank named is the server's commander rank. Server admins can always
order. Some servers don't require a commander at all, and then anyone on a
side can command it; and until the dashboard has told the game server who
the commanders are (just after a restart, or on a server running without
it), orders are let through.

## What is on the map

- **Our ground formations**, bases, battles and the front, as on the
  [Ground War](./ground-war.md) page.
- **Our AI flights**: tankers, AWACS, drones, fighters, attackers, SEAD,
  bombers, recon and transports, each drawn where it really is, with its
  altitude and speed.
- **Supply convoys** on the road, with the base they are headed for.
- **Deployed units and troops** players have put down, units nobody owns
  (put down by the HQ or an admin), and **Dismounts**: infantry squads that
  got off their vehicles.
- **Batteries**: artillery in our bases and among deployed units.
- **Carrier groups**.
- **Enemy aircraft** our radar and AWACS hold, and the enemy formations our
  forces can see. Nothing else of the enemy: the command map has the same fog
  of war as everyone else.

Zoom in and groups break up into their vehicles. Assets that are not spawned
in DCS right now are drawn faded. The **AIR / GND / SEA / LOG / HOS** buttons
on the right turn each layer on and off.

## Giving orders

Select an asset with a click, then use the order buttons in its panel, a
hotkey, or right-click the map for its main order:

| Asset | Orders |
|---|---|
| Ground formations | **Attack** (**A**, then click an enemy base), **Defend** (**D**, then one of your bases), **Hold** (**H**), **Withdraw** (**W**: home, or the nearest base you hold), **Release** (**X**: hand back to the AI), **Raise** (**R**: a new formation at the base under the cursor), and **Move** (**M**) to any point that isn't an enemy base. A Move is a road march: the column drives on past enemies it only sees and returns fire, stopping to fight only when the enemy is within about 3 km. To go looking for a fight, give an Attack. The same orders as F10 → Ground Forces (see [Ground War](./ground-war.md)), under the same rules |
| AI flights | **Station** (**S**): go to a point and work there (CAP station, tanker or AWACS orbit, attack area); given while the flight is still on the ramp, it takes effect once it is airborne. **RTB** (**B**) |
| Deployed units, troops | **Move** (**M**) |
| Units nobody owns, dismount squads | **DISBAND**: removed for good, nothing refunded. Not a squad holding a base it has just captured, and never a unit a player owns: only they can remove it |
| Batteries | **Fire** (**G**) on a point inside the orange ring |
| Carrier groups | **Sail** (**M**) to open water |
| Every battery in range | **Barrage** (**V**) |

**L** opens the command panel: the side's treasury, the logistics orders for
the selected base (**convoy**, **helicopter supply**, **helicopter troops**),
the operations the [Theatre HQ](./theatre-hq.md) can run right now, each
with its price, and **BUY A FORMATION**. Click one to launch it.

Press **?** on the page for every key. The rest of the controls: **Shift +
click** adds or removes a formation, **Shift + drag** box-selects (Ctrl +
Shift adds), a **double-click** selects every formation of that kind on
screen, **Ctrl / Alt + 1-9** makes the selection a group and **1-9** selects
it (twice to centre on it), **Space** centres on the selection, **Tab** steps
through your formations, **F** follows your own aircraft and **Esc** cancels
the order, then deselects.

Every order you give is told to your whole side:

```dcsmessage
COMMAND Viper: barrage on the marked point ({{cfg:smart_commander.hq.costs.artillery|150}} from the treasury)
```

The price shown is what that order cost on this server (the HQ's artillery
price, times its cost scale where the server sets one). A Move reads
"COMMAND Viper: 3rd Mech Coy (Senaki): moving to a position 12 km away by
road, returning fire only". Attack, Defend, Hold, Withdraw and Raise are
announced the way F10 orders are, with "(dashboard)" after your name.

## Reading the map

The command map shows your side's whole situation, under the units:

- **Zones of control**: every base's ground (what is nearer to it than to any
  other base) washed in its owner's colour, so the front is where the two
  colours meet. **TERR** turns them off.
- **Supply (SUP)**: yellow dashes from each logistics hub to the bases it
  feeds; red where the route is cut (an enemy base on it, or the hub wrecked).
- **Coverage (COV)**: how far your SAM and AAA sites reach (bright when they
  are live in DCS) and your radar coverage.
- **Threats (THR)**: enemy air defences your side has found, with a dotted
  ring for how sure you are of where they are. Old or uncertain reports fade.

## Moving formations

**M** then click the map to lay waypoints, one click each (up to 12); the
route is drawn as you go. **Right-click** or **Enter** sends it (a right-click
also adds that point as the destination). A plain right-click on the map with
formations selected sends them straight there.

The formation panel sets how it fights and drives, at once in DCS:

- **Rules of engagement**: **AUTO** (the doctrine: weapons free, return fire
  only on a road march), **FREE**, **RETURN** (shoot only when shot at, and keep
  moving), **HOLD** (hold fire).
- **Pace**: **SLOW** (0.55 of normal, keeps together), **NORMAL**
  ({{cfg:ground_war.speed_kph|35}} km/h by road at most), **FAST** (about
  1.65 times normal: flat out, never faster than the slowest vehicle).

You can't move, or change the posture of, a formation another commander is
directing while their order stands: "3rd Mech Coy (Senaki) is carrying out
Viper's order". A route has at most 12 waypoints, none in the water and none
inside an enemy base ("Gori is an enemy base: order an attack to take it").

## Buying a formation

Where the ground war is on, commanders can buy a whole new formation from the
treasury, under **BUY A FORMATION** on the command panel (**L**):

1. Pick the unit type (each shows its price per copy) and set how many with
   **−** / **+**: {{cfg:ground_war.purchase.min_count|2}} to
   {{cfg:ground_war.purchase.max_count|8}}.
2. Press **ROUTE**, click up to 12 waypoints, and **right-click** or press
   **Enter** to send.
3. Confirm the price.

The formation is put together at your nearest base within
{{cfg:command.deploy_range_m|25000}} m of the route's first point and sets
out along your route. That base can't be under threat, still consolidating
after a capture, a carrier, a naval base or a SAM site. Radar and GCI units
can't be bought this way, and nor can anything that can't drive.

It is named like any other, by its unit type and base, for example "6th
T-72B3 Coy (Kutaisi)". Each copy costs the deployable's own price times
{{cfg:ground_war.purchase.cost_factor|1}}. The nearest base is the one used:
if it is consolidating, the order is refused rather than sent from the next
one.

A bought formation counts towards your side's formation limit
({{cfg:ground_war.max_formations_per_side|6}}), and towards the
{{cfg:ground_war.max_live_formations|6}} formations both sides together may
have in DCS. It starts with its base's supply. When it stands down, its survivors stay where they are as the buying
commander's deployed units.

## The order catalogue

The **COMMAND** panel (**L**) lists everything your side can order on this
server, grouped by Air, Fires, Ground, Naval, Logistics and Intel, each with
its price. Pick one, then click where it goes: a point, a point at sea, one of
your bases, an enemy base, or two bases for a transfer. Greyed entries say why
they can't be ordered right now.

Everything happens in DCS. Nothing is decided on the map instead:

- **Air**: AWACS, tankers, fighters, attack and SEAD flights take off from a
  friendly airfield (one must be within 250 km of the point) and fly there.
  **Bombers** go after the target a JTAC of yours is lasing nearest the point
  (there has to be one within 15 km of it).
  **Strikes** attack with the flight's own weapons, no JTAC needed (see
  [AI Strike Missions](./strike-missions.md)).
- **Fires**: every battery in range fires (**Barrage**); deployed missile
  launchers fire a **missile strike**; ALCM bombers launch cruise missiles.
- **Ground**: new units (**deployments**) are put together at your nearest
  base within {{cfg:command.deploy_range_m|25000}} m and **drive there by road**:
  a convoy the enemy can find and kill on the way. **Ambush convoy** sends a
  force out from your nearest base to cut off the enemy convoy nearest the
  point (within 20 km of it). **Reinforcements** rebuild a base's lost units
  by transporter convoy.
- **Naval**: a **naval strike** has your ships launch their own cruise
  missiles at an enemy base: only ships that carry them can (a Ticonderoga or
  an Arleigh Burke). A **hunter group** (Red: a Type 093 submarine; Blue: an
  Arleigh Burke surface action group, since DCS has no modern Western
  submarine) sails out from your nearest naval base or carrier group to the
  point and attacks the enemy ships it finds with its own anti-ship missiles.
  It costs {{cfg:command.hunters.cost|300}} before the HQ's cost scale,
  sails up to {{cfg:command.hunters.range_m|400000}} m from where it set out,
  and a side can have {{cfg:command.hunters.max_per_side|2}} at sea at once.
  It shows on your map; **Sail** sends it elsewhere. It heads home after
  {{cfg:command.hunters.lifetime_secs|5400}} seconds. If it is sunk, your
  side hears "COMMAND: Surface action group 1 has been lost."
- **Logistics**: repair flights, base-to-base transfers, carrier repair and
  respawn.

## What it costs, and what you can't do

Commanding spends the side's treasury, not your points, and the server checks
every order:

- Anything that makes firepower or supplies (fire missions, barrages, convoys,
  helicopter runs, HQ operations) is paid from the treasury at the HQ's
  prices. A battery still has to reload between fire missions.
- Moving a player's deployed units or troops costs what the Move action costs
  for that distance.
- You can only launch an operation the HQ itself could run there right now,
  within its limits.
- You can only order your own side's assets, and only commanders can.
- A formation can't be moved into an enemy base: taking one is an Attack, with
  its assault and capture rules.
- What you put on the map from the panel (deployments by road, troop
  insertions, the HQ operations you launch, and actions paid from the
  treasury) is **yours**: you get the credit, and you can `-delete` it, but
  nothing is refunded, since your points didn't pay for it.
- Orders are rate limited: one every 1.5 seconds and at most 20 a minute
  per commander ("one order at a time"), and orders to ground formations (and
  buying one) one every
  {{cfg:ground_war.player_order_cooldown_secs|30}} seconds, shared with your
  F10 orders.
- The whole side is told who ordered what.
