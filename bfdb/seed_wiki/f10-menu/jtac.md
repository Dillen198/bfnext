# JTAC System

A JTAC (Joint Terminal Attack Controller) finds enemy ground units, picks one,
and holds a laser on it for your weapons. It also reads you a 9-line, smokes the
target, lases logistics buildings, and calls friendly artillery and cruise
missiles onto whatever it is lasing. Close air support worked through a JTAC is
one of the fastest ways to break an enemy ground push.

![JTAC calling in a 9-line CAS brief](/api/wiki/images/1e34aa38-f253-4652-b725-c30cc1553a38)

For a step-by-step attack run, see the playbook
[Flying CAS With a JTAC](../playbooks/cas-with-jtac.md).

## Where JTACs come from

| Kind | How it gets there | Shown in menus as |
|---|---|---|
| Drone | Bought from the [Actions menu](./actions.md) (a drone action) | its callsign and group id, e.g. `Reaper 1234` |
| Ground vehicle | A JTAC-capable [deployable](../advanced/deployables-guide.md) unpacked from crates | group id plus what it is and who built it, e.g. `1240(JTAC Humvee Viper 1-1)` |
| Troops | A JTAC-capable [troop](./troops.md) squad | group id plus squad and owner |
| Player | Flying an airframe the server lists as an airborne JTAC | `sl` plus the slot id, the type and the pilot, e.g. `sl57(SA342L Viper 1-2)` |

Every JTAC has a detection range set by the server for its type. Some types (most
drones) see through terrain; the rest need line of sight to each unit. Enemy
aircraft in flight are never JTAC targets, but helicopters are.

### Seeing is not lasing

A JTAC **sees** out to its detection range but **lases** no further than
**18.5 km (10 nm)**, or its own range if that is shorter. Contacts beyond that
are still reported (under "Seen, out of laser range") and never lased. The
Status report always says how far this JTAC lases.

![Diagram of a drone JTAC: an outer detection ring, an inner 18.5 km laser ring, and a 5 km focus circle around a player's map mark, showing which contacts get lased](/api/wiki/images/abcdcd8d-500e-453d-a7f6-07596af01c71)

## Opening the JTAC menu

The JTAC menu loads in steps, because building every JTAC's menu at once would
be slow:

1. **F10 → `JTAC>>`**. This is a command, so DCS closes the F10 menu when you
   click it. Re-open F10: the entry is now the submenu **`JTAC`**.
2. **`JTAC`** lists **places, not JTACs**: one entry per objective with JTACs
   near it, such as `Batumi (2 JTAC)>>`. Pinned JTACs sit at the top of the
   list with their full menu.
3. Click a place. F10 closes again and the server tells you:

```dcsmessage
2 JTAC near Batumi loaded.
Re-open F10 and go to JTAC > Batumi to work them.
```

4. Re-open F10 and go to **`JTAC → Batumi`**. Each JTAC there is a submenu
   named with its laser code on the end, e.g. `Reaper 1234 [1688]`.

`Refresh Locations` (top of the list) rebuilds the list when JTACs have come
and gone. Long lists continue on `More >>` pages. Who gets the JTAC menu at all
is set by the server's rules.

```f10menu
{"root":"F10 Other","items":[
 {"label":"JTAC>>","note":"First click only: builds the list and closes F10. Re-open F10 and it is now the JTAC submenu below."},
 {"label":"JTAC","items":[
  {"label":"Refresh Locations","note":"Rebuilds this list. No message; F10 closes."},
  {"label":"Kobuleti (1 JTAC)>>","says":"1 JTAC near Kobuleti loaded.\nRe-open F10 and go to JTAC > Kobuleti to work them."},
  {"label":"Senaki (3 JTAC)>>","says":"3 JTAC near Senaki loaded.\nRe-open F10 and go to JTAC > Senaki to work them."},
  {"label":"Batumi","note":"What Batumi (2 JTAC)>> turns into after you click it.","items":[
   {"label":"Reaper 1234 [1688]","items":[
    {"label":"Status","says":"JTAC Reaper (1234) [1688] status\nlasing T-72B code 1688 -- 312°M 6.4 km from the JTAC\nposition 045°M 5.2km from Batumi, lases out to 18.5 km\n\nIn laser range: T-72B x3, BMP-3 x2\nSeen, out of laser range: Tor 9A331\n\nmode: auto, IR pointer: off\nfilter: []\navailable artillery: [1301,1302,1310]\navailable ALCM: [1401(8)]\nRadio: 251.000 MHz AM (comms menu for coordinates)","note":"Also drops a pin on the target for your group only, replaced on your next click."},
    {"label":"9-Line","says":"== 9-LINE CAS - JTAC Reaper (1234) ==\n1. IP: Batumi\n2. HDG: 062°M IP to target\n3. DIST: 6.4km / 3.5nm\n4. ELEV: 33ft MSL\n5. DESC: T-72B (6 contact(s) visible)\n6. LOC: N41°38.21' E041°44.87'\n    MGRS 37TGG 27411 13552\n    062°M 6.4km from Batumi\n7. MARK: LASER 1688\n8. FRIENDLIES: JTAC 228°M 4.1km from target\n9. EGRESS: 242°M"},
    {"label":"Shift","says":"JTAC SHIFTED NOW TARGETING BMP-3\nauto shift is now disabled\njtac 1234 near Batumi\nrequested by Viper 1-1"},
    {"label":"Smoke Current Target","says":"SMOKE DEPLOYED ON TARGET\njtac 1234 near Batumi\nlaser code 1688\nrequested by Viper 1-1","note":"60 s cooldown per JTAC. Blue JTACs smoke red, Red JTACs smoke blue."},
    {"label":"Code","items":[
     {"label":"Xxxx","items":[{"label":"1","says":"JTAC CODE CHANGED TO 1688\njtac 1234 near Batumi\nchanged by Viper 1-1"}]},
     {"label":"xXxx","items":[{"label":"1"},{"label":"2"},{"label":"3"},{"label":"4"},{"label":"5","says":"JTAC CODE CHANGED TO 1588\njtac 1234 near Batumi\nchanged by Viper 1-1"},{"label":"6"},{"label":"7"}]},
     {"label":"xxXx","items":[{"label":"1"},{"label":"2"},{"label":"3"},{"label":"4"},{"label":"5"},{"label":"6"},{"label":"7"},{"label":"8"}]},
     {"label":"xxxX","items":[{"label":"1"},{"label":"2"},{"label":"3"},{"label":"4"},{"label":"5"},{"label":"6"},{"label":"7","says":"JTAC CODE CHANGED TO 1587\njtac 1234 near Batumi\nchanged by Viper 1-1\nWARNING: JTAC 1240 also lase on 1587"},{"label":"8"}]}
    ]},
    {"label":"Filter","items":[
     {"label":"Clear","says":"JTAC FILTER CLEARED\njtac 1234 near Batumi\ncleared by Viper 1-1"},
     {"label":"SAM","says":"JTAC FILTER CHANGED TO SAM\njtac 1234 near Batumi\nchanged by Viper 1-1"},
     {"label":"AAA"},{"label":"Armor","says":"JTAC FILTER CHANGED TO SAM | Armor\njtac 1234 near Batumi\nchanged by Viper 1-1","note":"Filters add up: each pick adds a type."},
     {"label":"APC"},{"label":"Logistics"},{"label":"Infantry"},{"label":"EWR"},{"label":"Aircraft"},
     {"label":"More >>","items":[
      {"label":"Helicopter"},{"label":"LR"},{"label":"SR"},{"label":"MR"},{"label":"IRGuided"},{"label":"RadarGuided"},{"label":"OpticallyGuided"},{"label":"EngagesWeapons"},{"label":"Unguided"},
      {"label":"More >>","items":[
       {"label":"TrackRadar"},{"label":"SearchRadar"},{"label":"AuxRadarUnit"},{"label":"ControlUnit"},{"label":"Launcher"},{"label":"ATGM"},{"label":"Artillery"},{"label":"LightCannon"},{"label":"HeavyCannon"},
       {"label":"More >>","items":[
        {"label":"RPG"},{"label":"SmallArms"},{"label":"Unarmed"},{"label":"Invincible"},{"label":"Driveable"},{"label":"AWACS"},{"label":"Link16"},{"label":"Boat"},{"label":"ALCM"},
        {"label":"More >>","items":[{"label":"NavalSpawnPoint"},{"label":"CAP"},{"label":"HotStart"},{"label":"Jammer"},{"label":"Recon"},{"label":"ColdStart"},{"label":"EventSpawn"}]}
       ]}
      ]}
     ]}
    ]},
    {"label":"Artillery","items":[
     {"label":"Fire All Groups Together","note":"Only with two or more guns in range.","items":[
      {"label":"1 round each","says":"GROUP FIRE MISSION: 3 guns firing 1 rounds each\njtac 1234 near Batumi\nrequested by Viper 1-1"},
      {"label":"3 rounds each"},{"label":"5 rounds each"},{"label":"10 rounds each"},
      {"label":"all ammo","says":"GROUP FIRE MISSION: 2 guns firing all remaining rounds each\njtac 1234 near Batumi\nrequested by Viper 1-1\nskipped 1310 (Kobuleti Arty 2) too far from target: 16200m (max 15000m)"}
     ]},
     {"label":"Batumi (2)","items":[
      {"label":"Fire Battery Together","items":[
       {"label":"1 round each"},{"label":"3 rounds each"},
       {"label":"5 rounds each","says":"GROUP FIRE MISSION (Batumi battery): 2 guns firing 5 rounds each\njtac 1234 near Batumi\nrequested by Viper 1-1"},
       {"label":"10 rounds each"},{"label":"all ammo"}
      ]},
      {"label":"1301","items":[
       {"label":"Relay Target","says":"TARGET RELAYED to 1301\ndirected by jtac 1234 near Batumi\nrequested by Viper 1-1"},
       {"label":"Get Ammo","says":"Artillery Group 1301 has 36 rounds total"},
       {"label":"Fire One","says":"ARTILLERY MISSION STARTED for 1301\ndirected by jtac 1234 near Batumi\nrequested by Viper 1-1"},
       {"label":"Fire For Effect","items":[{"label":"5"},{"label":"10","says":"ARTILLERY MISSION STARTED for 1301\ndirected by jtac 1234 near Batumi\nrequested by Viper 1-1"},{"label":"20"},{"label":"40"}]},
       {"label":"Fire All","says":"ARTILLERY FIRE ALL MISSION STARTED for 1301\ndirected by jtac 1234 near Batumi\nrequested by Viper 1-1"},
       {"label":"Target Group","items":[
        {"label":"Fire 1 per","items":[{"label":"Target 2 units"},{"label":"Target 3 units"},{"label":"Target 4 units"},{"label":"Target 5 units"},{"label":"Target 6 units"}]},
        {"label":"Fire 2 per","items":[{"label":"Target 2 units"},{"label":"Target 3 units"},{"label":"Target 4 units","says":"Artillery Combo Mission: Viper 1-1 firing 2 rounds per target at 4 targets"},{"label":"Target 5 units"},{"label":"Target 6 units"}]},
        {"label":"Fire 5 per","items":[{"label":"Target 2 units"},{"label":"Target 3 units"},{"label":"Target 4 units"},{"label":"Target 5 units"},{"label":"Target 6 units"}]},
        {"label":"Fire 10 per","items":[{"label":"Target 2 units"},{"label":"Target 3 units"},{"label":"Target 4 units"},{"label":"Target 5 units"},{"label":"Target 6 units"}]}
       ]}
      ]},
      {"label":"1302","note":"Same menu as 1301."}
     ]},
     {"label":"Kobuleti (1)","items":[{"label":"1310","note":"Same menu as 1301."}]}
    ]},
    {"label":"ALCM","items":[
     {"label":"1401(8)","note":"Group id and missiles left.","items":[
      {"label":"Fire Quarter","items":[{"label":"Fire One per target","says":"ALCM MISSION STARTED for 1401\ndirected by jtac 1234 near Batumi\nrequested by Viper 1-1"},{"label":"Fire Two per target"},{"label":"Fire Four per target"}]},
      {"label":"Fire Half","items":[{"label":"Fire One per target"},{"label":"Fire Two per target"},{"label":"Fire Four per target"}]},
      {"label":"Fire All","items":[{"label":"Fire One per target"},{"label":"Fire Two per target"},{"label":"Fire Four per target"}]}
     ]}
    ]},
    {"label":"Focus on My Latest Mark","says":"JTAC 1234 near Batumi: focusing on Viper 1-1's mark, 3 contact(s) within 5 km, lasing the nearest on code 1688","note":"Once a focus is set this reads Focus: Move to My Latest Mark, and Clear Focus is added."},
    {"label":"More >>","items":[
     {"label":"Toggle Auto Shift","says":"AUTO SHIFT NOW false\nfor jtac 1234 near Batumi\nchanged by Viper 1-1"},
     {"label":"Toggle IR Pointer","says":"IR POINTER NOW true\nfor jtac 1234 near Batumi\nchanged by Viper 1-1"},
     {"label":"Designate Building","says":"JTAC 1234 designating building: Warehouse (logistics target) 118°M / 3.4km from the JTAC, code 1688"},
     {"label":"Pin","note":"Puts this JTAC at the top of your JTAC list and sends you its callouts. Reads Unpin once pinned."}
    ]}
   ]},
   {"label":"1240(JTAC Humvee Viper 1-1) [1711]","note":"Same menu as Reaper 1234."}
  ]}
 ]}
]}
```

## The per-JTAC menu

The first page holds what you work a target with; the set-and-forget toggles are
on `More >>`.

| Entry | What it does |
|---|---|
| `Status` | Full report (below). If the JTAC has a target, also drops an F10 pin on it for your group only, labelled like `JTAC 1234 — T-72B`; your next Status click replaces it. |
| `9-Line` | A CAS 9-line for the current target. |
| `Shift` | Lase the next target in laser range and switch to manual mode. |
| `Smoke Current Target` | Smoke on the target. |
| `Code` | Change the laser code one digit at a time. |
| `Filter` | Only lase units with one of the chosen types. |
| `Artillery` | Friendly guns in range of the JTAC, and their fire missions. |
| `ALCM` | Friendly cruise-missile shooters in range, and their launch options. |
| `Focus on My Latest Mark` | Lase only near your newest F10 map mark. |
| `Clear Focus` | Back to the JTAC's whole area (only shown while a focus is set). |
| `Toggle Auto Shift` | Switch between auto (always lase the top-priority target) and manual. |
| `Toggle IR Pointer` | Add or remove an IR pointer on the target, for NVGs. |
| `Designate Building` | Lase the next logistics building at the JTAC's objective. |
| `Pin` / `Unpin` | Keep this JTAC at the top of your list and hear its callouts. |

Settings you change (code, filter, auto shift, IR pointer) apply to the JTAC for
everyone, and are kept for deployed, troop and drone JTACs across a server
restart.

## Status report

```dcsmessage
JTAC Reaper (1234) [1688] status
lasing T-72B code 1688 -- 312°M 6.4 km from the JTAC
position 045°M 5.2km from Batumi, lases out to 18.5 km

In laser range: T-72B x3, BMP-3 x2
Seen, out of laser range: Tor 9A331

mode: auto, IR pointer: off
filter: []
available artillery: [1301,1302,1310]
available ALCM: [1401(8)]
Radio: 251.000 MHz AM (comms menu for coordinates)
```

- **Line 2** is the target, its laser code, and where it is from the JTAC
  (magnetic bearing, km). If two JTACs at the same place share a code it adds
  `(code conflicts with [1240])`.
- **position** is where the JTAC is, from the nearest objective, and how far it
  lases.
- **In laser range** and **Seen, out of laser range** split what it can see.
  With a focus set the second line reads `Seen, out of range / outside focus`.
- **mode** is `auto` or `manual (Shift cycles targets)`; `, focused on a mark`
  is added while a focus is set.
- **Radio** appears for drones that have a radio: tune it and DCS's own comms
  menu reads you the target coordinates.

When it sees enemies but none it can lase, the status says why instead of a bare
"no target":

```dcsmessage
JTAC Reaper (1234) [1688] status
no target: 3 contact(s) in view, none in laser range. Nearest is Tor 9A331 087°M 23.4 km from the JTAC -- it lases out to 18.5 km
move it closer: -action DRONE Waypoint 1234 <mark text>
```

The second line names the server's drone waypoint action (here `DRONE
Waypoint`) and is only shown for drones; a player JTAC is told `fly closer to
lase`. With nothing in sight at all it says `No enemies in sight`.

## Targeting

### Auto and manual

In **auto** mode (the default) the JTAC lases the best target and moves on by
itself when that target dies or leaves. It ranks targets:

1. In laser range, and inside the focus area if one is set.
2. By the server's target priority list (SAM radars first by default, then other
   air defence, ATGM carriers, logistics, infantry, armour).
3. Nearest first: to the focus mark if set, otherwise to the JTAC.

It stays on its current target while that is still as important as the best one,
so it does not hop between two equal tanks.

`Shift` puts it in **manual** mode and moves the laser to the next target in
laser range. Press `Toggle Auto Shift` to go back to auto. If there is nothing to
shift to, only you are told:

```dcsmessage
JTAC 1234: nothing to shift to
no target: 3 contact(s) in view, none in laser range. Nearest is Tor 9A331 087°M 23.4 km from the JTAC -- it lases out to 18.5 km
```

### Filter

`Filter → <type>` keeps only units carrying **any** of the chosen types. Each
pick adds a type (`SAM | Armor`), and `Clear` removes them all. The list is every
unit tag the engine knows, so it runs over four `More >>` pages; the useful ones
are on the first page (`SAM`, `AAA`, `Armor`, `APC`, `Logistics`, `Infantry`).

### Focus on a map mark

Place an F10 map mark near the targets you want, then `Focus on My Latest Mark`.
The JTAC then lases only inside **5 km** of the mark, nearest the mark first.
Without a mark you are told `Place an F10 map mark near the targets first, then
use Focus on My Mark`. The answer depends on what it finds:

```dcsmessage
JTAC 1234 near Batumi: focusing on Viper 1-1's mark. It sees 2 contact(s) there, but the nearest is 21.3 km away and it lases out to 18.5 km -- move it closer (Actions > DRONE Waypoint) and it will lase on code 1688
```

`Clear Focus` answers `JTAC 1234 near Batumi: focus cleared, back to working its
whole area`.

## Laser codes

- Valid codes are **1111 to 1788**: first digit 1, second 1-7, last two 1-8.
  These are the codes DCS weapons can be set to.
- Each new JTAC picks a code no other JTAC on its side uses: the first one gets
  its default (1688 unless the server sets another), the next ones walk upward
  (1711, 1712, ...). So two JTACs over one fight do not share a spot by default.
- `Code` has one submenu per digit: `Xxxx` (thousands), `xXxx` (hundreds),
  `xxXx` (tens), `xxxX` (ones). Picking `5` under `xXxx` turns 1688 into 1588.
- If another JTAC on your side already uses the new code, the confirmation ends
  with `WARNING: JTAC 1240 also lase on 1587`.

The code is also on the end of every JTAC's menu label, on the map label next to
its target, and in every callout, so you can read it off without opening Status.
Set your weapon to the same code.

## Smoke and IR pointer

`Smoke Current Target` drops smoke within 10 m of the target: **red smoke from a
Blue JTAC, blue smoke from a Red one**. Each JTAC can smoke once every 60
seconds; asking early tells only you:

```dcsmessage
COULD NOT SMOKE TARGET
jtac 1234
smoke will not be ready for another 42s
```

`Toggle IR Pointer` adds an infrared pointer on the target (visible through
NVGs). It does not change the laser. The 9-line shows it as
`7. MARK: LASER 1688 + IR pointer`.

## Designate Building

`Designate Building` lases the next logistics building (the ones with logistics
markers on the map) at the objective **nearest the JTAC**, and pins it on the map
for your side. Each press moves to the next building. See
[Objective Buildings](../gameplay/objective-buildings.md).

```dcsmessage
JTAC 1234 designating building: Warehouse (logistics target) 118°M / 3.4km from the JTAC, code 1688
```

If there are none it says either `JTAC 1234: Batumi has no logistics buildings on
the map to lase (it is the objective nearest the JTAC)` or `JTAC 1234: all 4
logistics buildings at Batumi are destroyed`.

## Fire support

The `Artillery` and `ALCM` submenus are always there. With nothing in range they
hold one line, `(no guns in range — deploy artillery nearby)` or `(no ALCM
shooters in range)`, which answers:

```dcsmessage
JTAC 1234: no friendly artillery or ALCM shooters are in range of it. The menu refreshes when some are.
```

### Artillery

Guns are listed when they are within **{{cfg:artillery_mission_range|15000}} m**
of the JTAC (the server's artillery mission range). A gun fires only at targets
between its minimum and maximum range; the default minimum is
{{cfg:artillery_min_range|1500}} m, and some types (ballistic launchers) have
their own. Guns are grouped by the objective they sit at, such as `Batumi (2)`,
when they are spread over several places.

| Entry | Effect |
|---|---|
| `Fire All Groups Together` | Every listed gun fires the same salvo at the target: `1 round each`, `3 rounds each`, `5 rounds each`, `10 rounds each` or `all ammo`. Guns out of range or empty are skipped and named. |
| `Fire Battery Together` | The same, for just the guns at that one objective. |
| `<gun id> → Relay Target` | Hands the JTAC's target to that group as a direct attack order. |
| `<gun id> → Get Ammo` | `Artillery Group 1301 has 36 rounds total`. |
| `<gun id> → Fire One` | One round. |
| `<gun id> → Fire For Effect` | `5`, `10`, `20` or `40` rounds. |
| `<gun id> → Fire All` | Everything that group has left. |
| `<gun id> → Target Group` | Spread fire over several JTAC contacts: `Fire 1 per` / `2` / `5` / `10 per`, then `Target 2 units` to `Target 6 units`. |

More detail: [Artillery Operations](../advanced/artillery.md).

### ALCM

Cruise-missile shooters within **{{cfg:alcm_mission_range|600000}} m** are
listed as group id and missiles left, e.g. `1401(8)`. Pick how much of the
magazine to use (`Fire Quarter`, `Fire Half`, `Fire All`), then how many
missiles per target (`Fire One per target`, `Fire Two per target`, `Fire Four
per target`). The salvo is spread over the JTAC's contacts. See
[ALCM Operations](../advanced/alcm.md).

Bomber missions are not in the JTAC menu. They are in the
[Actions menu](./actions.md) (each one asks which JTAC to work with) and in chat
as `-jtac <id> bomber`.

## Who hears a JTAC

JTAC messages go to the player who asked, plus every player on that side who
**pinned** the JTAC or **expanded its place** in the JTAC menu. Those players also
get its callouts:

```dcsmessage
JTAC Reaper (1234) [1688] near Batumi: lasing T-72B
```

```dcsmessage
JTAC Reaper (1234) [1688]: target T-72B destroyed
```

`JTAC Reaper (1234) [1688]: target lost` means it lost sight of the target. Two
messages go to the whole side: `JTAC Reaper (1234) is no longer available` when a
JTAC is killed or gone, and `JTAC Reaper (1234) [1688] is online 045°M 5200
meters from Batumi` when a saved JTAC comes back after a restart.

## What JTACs draw on the F10 map

- **Drone, ground and troop JTACs** draw, for your side only: a dot-dash laser
  line from the JTAC to its target, a pin on the target reading
  `JTAC 1234`, `lasing T-72B`, `Code: 1688`, and the laser code as a label just
  south of the target. They follow a moving target.
- **Player JTACs** drop a pin for the side: `JTAC sl57 target T-72B marked by
  code 1688`.
- **Status** drops a pin for your group only (see above).
- **Designate Building** pins the building for the side.

### F10 map intel

On servers that run the intel picture (ELINT or player
[recon](./recon.md) enabled), every unit a JTAC can see is also fed into your
side's intel picture and marked on the F10 map as an `[INTEL/RED]` marker. While
the JTAC keeps watching it the marker stays at full confidence. Once the JTAC
loses it, confidence halves every {{cfg:elint.half_life_jtac_secs|3600}} seconds,
so it stays on the map for hours. A unit that is destroyed has its marker
removed at once, and a target that drives off moves its marker with it. This
works for every JTAC type and needs nothing from you.

## Chat commands

Everything in the menu also works from chat, which is handy for Combined Arms
commanders who have no F10 menu. The id is the number in the JTAC's label.

| Command | Does |
|---|---|
| `-jtac list` | Every JTAC on your side: id, where, laser code, what it is lasing |
| `-jtac help` | The command list |
| `-jtac <id> status` | Status report |
| `-jtac <id> shift` | Next target (switches auto off) |
| `-jtac <id> autoshift` | Toggle auto |
| `-jtac <id> smoke` | Smoke the target |
| `-jtac <id> focus [<mark text>\|clear]` | Focus on your latest (or the named) map mark, or clear it |
| `-jtac <id> move <mark text>` | Fly a drone JTAC to a map mark (the drone waypoint action, costs points) |
| `-jtac <id> building` | Lase the next logistics building |
| `-jtac <id> 9line` | The 9-line |
| `-jtac <id> filter <type>\|clear` | Add a type to the filter, e.g. `filter SAM`, or clear it |
| `-jtac <id> code <code>` | Set a whole code, e.g. `code 1688` |
| `-jtac <id> pointer` | Toggle the IR pointer |
| `-jtac <id> arty <gun id\|all> <rounds>` | Artillery on the current target |
| `-jtac <id> bomber [mission]` | A bomber mission on the current target |

`-jtac list` answers one line per JTAC, such as
` Reaper (1234) near Batumi, code 1688: lasing T-72B`. Full list:
[Chat Commands](../gameplay/chat-commands.md).

## Common issues

**"My drone isn't in the list."** The JTAC list shows places. Open the place it
is near (the count says how many JTACs are behind it), or use
`Refresh Locations` if it was just deployed.

**"The menu closed when I clicked."** `JTAC>>` and each place are commands that
build a submenu; DCS closes F10 after any command. Re-open F10.

**No target.** Read the status line under the header. Usual reasons: everything
it sees is beyond its laser range (move a drone closer with the waypoint
action), a filter or focus excludes everything (`Filter → Clear`, `Clear Focus`),
terrain blocks line of sight, or it simply sees no enemies.

**Bombs ignore the spot.** Your weapon code does not match the JTAC's. Check the
code on the menu label: a second JTAC on your side lases on a different code.

**Artillery skipped.** The group message lists each skipped gun with the reason,
such as `too far from target: 16200m (max 15000m)` or `is out of ammunition`.

## See Also

- [Flying CAS With a JTAC](../playbooks/cas-with-jtac.md) - the attack run step by step
- [Artillery Operations](../advanced/artillery.md) - fire missions in detail
- [ALCM Operations](../advanced/alcm.md) - cruise missile strikes
- [Reconnaissance](./recon.md) - the other way to put enemy units on the map
- [Actions Menu](./actions.md) - drones, drone waypoints and bomber missions
