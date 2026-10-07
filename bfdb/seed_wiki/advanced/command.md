# Command: the Commander's Map

The dashboard's **COMMAND** page is your coalition's war on one map: the
ground war's formations, bases and battles, and every other asset your side
has in the field, live, with the enemy you can see. Commanders give orders from
it; everyone on the side can watch.

## Becoming a commander

Command is earned. It unlocks at rank tier
{{cfg:command.commander_rank|4}} (Major, a campaign score of 50 at the
default): the same score and ranks the leaderboard shows, so captures,
logistics and kills all count toward it. An admin can also make you a
commander, or take it away.

Rank is campaign-wide, so it counts on every server, for whichever side you
are flying for there. Switch sides and you command your new side. On
Discord, commanders get the **Commander** role; your Blue or Red role still
shows your side, server by server, and follows you when you switch.

The page tells you where you stand: what unlocks command, and how far off you
are.

## What is on the map

- **Our ground formations**, bases, battles and the front, as on the
  [Ground War](./ground-war.md) page.
- **Our AI flights**: tankers, AWACS, drones, fighters, attackers, SEAD,
  bombers, recon and transports, each drawn where it really is, with its
  altitude and speed.
- **Supply convoys** on the road, with the base they are headed for.
- **Deployed units and troops** players have put down.
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
| Ground formations | Attack, defend, withdraw, hold (as before), and **Move** (**M**) to any point that isn't an enemy base |
| AI flights | **Station** (**S**): go to a point and work there (CAP station, tanker or AWACS orbit, attack area). **RTB** (**B**) |
| Deployed units, troops | **Move** (**M**) |
| Batteries | **Fire** (**G**) on a point inside the orange ring |
| Carrier groups | **Sail** (**M**) to open water |
| Every battery in range | **Barrage** (**V**) |

**L** opens the command panel: the side's treasury, the logistics orders for
the selected base (**convoy**, **helicopter supply**, **helicopter troops**),
and the operations the [Theatre HQ](./theatre-hq.md) can run right now, each
with its price. Click one to launch it.

Press **?** on the page for every key.

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
- Orders are rate limited.
- The whole side is told who ordered what.
