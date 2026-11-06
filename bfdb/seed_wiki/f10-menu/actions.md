# Actions Menu

`F10 → Actions>>` is where you spend your points. Everything your coalition can
buy or order from the radio lives here: AWACS and tankers, fighter, attack and
SEAD flights, bombers and strikes, JTAC drones, paratroopers and AI-delivered
deployables, logistics runs, reinforcement convoys, moves for ground units you
already own, carrier orders, artillery fire, the coalition
[tasking board](../gameplay/tasking-board.md) and the
[AI helo missions](../advanced/helo-missions.md).

![Illustration of a gloved hand on a glowing map table, picking aircraft and vehicles to send out](/api/wiki/images/ccf9a836-dfdb-4bf6-a6cd-1fb77a55ac9a)

What is on the menu, and what it costs, is **server configuration**. Each
server names its own actions and sets its own prices. This page explains how
the menu works and what every kind of entry does; the
[Action Types reference](../reference/action-types.md) has the full catalogue.

## What this server offers

Generated from the campaign config of the server selected in the top bar.

### Blue

{{list:actions.Blue|_This server publishes no action list. Check F10 → Actions in game._}}

### Red

{{list:actions.Red|_This server publishes no action list. Check F10 → Actions in game._}}

**Cost** is what the menu label shows and what you are charged when the action
starts. **Type** tells you which section of
[Action Types](../reference/action-types.md) describes it. **Limit** is
explained under [Limits](#costs-limits-and-refunds).

## Opening the menu

You only get the Actions menu if your account passes the server's rule for
actions (most servers allow everyone). It appears at the root of the F10 menu
as **`Actions>>`**, which is a command, not a folder:

1. Select `Actions>>`. The engine builds the full **`Actions`** submenu from the
   world as it is right now: your map marks, your side's JTACs, its groups and
   objectives, the open tasks on the board.
2. Open F10 again and go into `Actions`.

The submenu goes back to being `Actions>>` when you place a new map mark and
after an action you picked has started, so the next time you open it the lists
are fresh. If a list looks stale, select `Actions>>` again.

Where an entry needs a place, it lists **your own F10 map marks**, nearest
first, each with its bearing and range from you (`CAS1 045/12km`, or
`Mark 1 045/12km` for an unnamed mark). How to drop and name marks is covered
in [F10 Menu Overview: map marks](./overview.md#map-marks-are-your-targeting-system).
The mark you pick is deleted once the action has started.

Lists longer than a menu page end in **`More >>`**, which opens the rest.

## The menu tree

This replica uses typical action names and prices. Your server's names and
prices are in the table above; the shape of each submenu depends only on the
kind of action, so it is the same everywhere.

```f10menu
{"root":"F10 Other","items":[
 {"label":"Actions","note":"Built when you select Actions>>. Actions appear in config order, nine to a page.","items":[
  {"label":"AWACS(100 pts)","note":"A spawn action: lists your map marks.","items":[
   {"label":"CAS1 045/12km","says":"action AWACS started","note":"A second message shows the charge: 1150(-100) points perform action AWACS"},
   {"label":"Mark 1 310/38km","says":"action AWACS started"},
   {"label":"Mark 2 122/61km","says":"could not start AWACS, Viper: this action costs 100 points and you have 40 points"}
  ]},
  {"label":"AWACS Waypoint(10 pts)","note":"A waypoint action: lists your side's AWACS, then your marks.","items":[
   {"label":"* 5012(AWACS)","note":"* marks a group you own. The number is the group id.","items":[
    {"label":"CAS1 045/12km","says":"action AWACS Waypoint started for 5012"},
    {"label":"Mark 1 310/38km","says":"action AWACS Waypoint started for 5012"}
   ]},
   {"label":"5044(AWACS)","items":[
    {"label":"CAS1 045/12km","says":"action AWACS Waypoint started for 5044"}
   ]}
  ]},
  {"label":"Tanker(50 pts)","items":[
   {"label":"CAS1 045/12km","says":"action Tanker started"},
   {"label":"Mark 1 310/38km","says":"could not start Tanker, calling tanker\n\nCaused by:\n    no friendly airfield more than 10 km from the target has room to launch this flight"}
  ]},
  {"label":"Bomber(100 pts)","note":"Lists your side's JTACs by callsign or id. The bomber hits what that JTAC is tracking.","items":[
   {"label":"Axeman 11","items":[
    {"label":"Yes, do it!","says":"BOMBER MISSION STARTED\ntargeting by jtac 1187 near Gori\nstarted by Viper"}
   ]},
   {"label":"1203","items":[
    {"label":"Yes, do it!","says":"bomber mission could not start blue is out of Bomber actions"}
   ]}
  ]},
  {"label":"JTAC Drone(25 pts)","items":[
   {"label":"CAS1 045/12km","says":"action JTAC Drone started"},
   {"label":"Mark 1 310/38km","says":"action JTAC Drone started"}
  ]},
  {"label":"RTB","note":"Lists your side's AI flights that can be sent home.","items":[
   {"label":"* 5012(AWACS)","items":[
    {"label":"CAS1 045/12km","says":"action RTB started for 5012","note":"It lands at the friendly airbase nearest the mark you pick."}
   ]},
   {"label":"5020(Tanker)","items":[
    {"label":"CAS1 045/12km","says":"action RTB started for 5020"}
   ]}
  ]},
  {"label":"Move(10 pts/km)","note":"Lists your side's deployed units and troops (yours first), then your marks priced for that group.","items":[
   {"label":"* 1043(Standard Troop)","items":[
    {"label":"CAS1 3km (30 pts)","says":"action Move started for 1043"},
    {"label":"Mark 1 18km (180 pts)","says":"action Move started for 1043"}
   ]},
   {"label":"1051(Firtina 155MM)","items":[
    {"label":"CAS1 9km (90 pts)","says":"action Move started for 1051"}
   ]}
  ]},
  {"label":"Reinforce(150 pts)","note":"Lists only your objectives that have lost garrison groups, with how many.","items":[
   {"label":"Kobuleti (2 down)","says":"action Reinforce started","note":"Your side also sees: Reinforcements for Kobuleti leaving Batumi: 2 group(s) on transporters, 34 km by road, about 51 min. Keep the road open."},
   {"label":"Senaki (1 down)","says":"could not start Reinforce, calling reinforcements\n\nCaused by:\n    Senaki is under attack -- reinforcements can go in once it is clear"}
  ]},
  {"label":"Add Task","note":"The tasking board. Position task types list your marks; objective types list bases.","items":[
   {"label":"CAP","items":[
    {"label":"CAS1 045/12km","says":"CAP task posted"},
    {"label":"Mark 1 310/38km","says":"CAP task posted"}
   ]},
   {"label":"CAS","items":[
    {"label":"CAS1 045/12km","says":"CAS task posted"}
   ]},
   {"label":"CAPTURE","note":"Enemy-held bases you can see.","items":[
    {"label":"Gori","says":"CAPTURE task posted"},
    {"label":"Tskhinvali","says":"could not post CAPTURE task, posting objective task\n\nCaused by:\n    Tskhinvali is already on the board as task 9"}
   ]},
   {"label":"SUPPLY","note":"Your own bases.","items":[
    {"label":"Senaki","says":"SUPPLY task posted"}
   ]}
  ]},
  {"label":"More >>","items":[
   {"label":"Remove Task","note":"Lists every open task on your side's board.","items":[
    {"label":"CAP 14 (12km NE of Batumi)","says":"task 14 removed"},
    {"label":"CAPTURE 9 (Tskhinvali)","says":"task 9 removed"}
   ]},
   {"label":"Precision Strike(300 pts)","note":"Strikes aimed at a base list the enemy objectives you can see.","items":[
    {"label":"Gori","says":"action Precision Strike started"},
    {"label":"Tskhinvali","says":"action Precision Strike started"}
   ]},
   {"label":"Carpet Bombing(250 pts)","note":"Interdiction and carpet strikes list your marks.","items":[
    {"label":"CAS1 045/12km","says":"action Carpet Bombing started"}
   ]},
   {"label":"Naval Strike(200 pts)","items":[
    {"label":"Gori","says":"could not start Naval Strike, naval cruise missile strike\n\nCaused by:\n    No friendly carrier group in range"}
   ]},
   {"label":"Carrier Waypoint","items":[
    {"label":"5501(Blue CSG Carrier)","items":[
     {"label":"Mark 1 250/40km","says":"action Carrier Waypoint started for 5501"}
    ]}
   ]},
   {"label":"Fat Cow(50 pts)","items":[
    {"label":"CAS1 045/12km","says":"action Fat Cow started","note":"Your side also sees: A Fat Cow is lifting from Senaki for the marked site; it will be set up for 20 minutes."}
   ]},
   {"label":"Paratroopers(50 pts)","items":[
    {"label":"CAS1 045/12km","says":"action Paratroopers started"}
   ]},
   {"label":"Request Fires","note":"Added when the server has artillery fire support on and your side has live guns or launchers. Free.","items":[
    {"label":"CAS1 045/12km","says":"fire mission requested"},
    {"label":"Mark 1 310/38km","says":"request fires failed: calling artillery fire support\n\nCaused by:\n    no friendly artillery/missiles in range of that position"}
   ]},
   {"label":"AI Helo Missions","note":"Added when the server has AI helo missions on.","items":[
    {"label":"Insert Troops: Nearest Capturable","says":"Helo troop insertion dispatched (HELO_blue_3_1761830412)."},
    {"label":"Insert Troops: Capturable Now","items":[
     {"label":"Gori 23nm [CAP]","says":"Helo troop insertion dispatched (HELO_blue_4_1761830470)."}
    ]},
    {"label":"Insert Troops: Any Objective","items":[
     {"label":"Gori 23nm [CAP]","says":"Helo troop insertion dispatched (HELO_blue_5_1761830502)."},
     {"label":"Tskhinvali 31nm","says":"Could not dispatch: nearest friendly launch field (Senaki) is 162km away, past the 150km max range"}
    ]},
    {"label":"Resupply: Nearest Friendly","says":"Helo resupply run dispatched (HELO_blue_6_1761830560)."},
    {"label":"Resupply: Friendly Base","items":[
     {"label":"Senaki 12nm","says":"Helo resupply run dispatched (HELO_blue_7_1761830600)."},
     {"label":"Kobuleti 30nm","says":"Could not dispatch: Batumi has nothing on hand that Kobuleti has room for"}
    ]}
   ]}
  ]}
 ]}
]}
```

In game the two menus above read the same: `Actions` then the entry, then the
mark, group, JTAC, objective or task the entry asks for.

## How each kind of entry works

The submenu under an entry depends on what kind of action it is.

| Entry kind | You pick | Action kinds |
| --- | --- | --- |
| Spawn at a mark | one of your map marks | AWACS, Tanker, Fighters, Attackers, SEAD, Drone, Recon, cruise missile platform, Paratrooper, Deployable, Fat Cow, Nuke, Artillery, and Interdiction or Carpet strikes |
| Order an existing flight | the flight, then a mark | AWACS / Tanker / Fighters / Attackers / SEAD / Drone / cruise missile waypoints, and RTB |
| Move a ground group | the group, then a mark (each priced) | Move |
| Carrier order | your carrier group, then a mark | Carrier Waypoint |
| Bomber | one of your side's JTACs, then `Yes, do it!` | Bomber |
| Strike a base | an enemy objective you can see | Precision, Runway and Anti-ship strikes, Naval cruise missile strike |
| Your own base | one of your objectives | Logistics Repair, Reinforce (only bases that have lost groups) |
| Tasking board | a task type, then a mark or a base | Add Task |
| Open task | a task on your board | Remove Task |

### Group lists

Waypoint, RTB, Move and Carrier entries list groups as `id(name)`, for example
`5012(AWACS)` or `1043(Standard Troop)`. A leading `*` means the group is yours,
and yours are listed first. Each waypoint entry lists only the flights it can
actually move: `AWACS Waypoint` lists AWACS, `Tanker Waypoint` lists tankers,
and so on. RTB lists every AI flight that can fly home (tankers, AWACS,
bombers, strikes, cruise missile platforms, drones, recon, SEAD, fighters,
attackers) that is not already on its way.

Move lists every friendly deployed unit and troop squad. Each mark under a
group shows the distance and the price for that group, for example
`CAS1 3km (30 pts)`, because a move is charged per step of distance. The menu
title says so: `Move(10 pts/km)`.

### Orbit marks

A flight sent to a mark leaves its own marks on your side's F10 map: an orbit
point for fighters, attackers, SEAD, drones, recon and cruise missile
platforms, or two race-track points 30 km either side of the mark for AWACS
and tankers (laid across the line to the nearest enemy objective). Each one is
labelled with the group id and its **responsible party**:

```dcsmessage
5012 race point 1
responsible party: Viper
```

The responsible party is who pays the penalty if that flight is lost. It
starts as the buyer; a later waypoint order can hand it to whoever gave it.

### Flights start on the ground

Every action aircraft takes off from a real friendly field: the nearest one
more than 10 km from the target with room for it (heavy aircraft need a large
stand; helicopters can use FARPs and FOBs). It is never air-started. If no
field can launch it, the action is refused and costs nothing. A waypoint order
given while the flight is still taxiing is held and flown as soon as it is
airborne.

## Costs, limits and refunds

**Cost.** Shown in the menu label as `(N pts)` and charged the moment the
action starts. You are told twice: the panel line from the menu, and a points
line:

```dcsmessage
1150(-100) points perform action AWACS
```

A few prices are worked out at the time:

- **Paratrooper and Deployable** charge the squad's or deployable's own cost on
  top of the action's.
- **Move** charges its cost once per step of distance, rounded up, never less
  than one step.
- **Nuke** gets cheaper each time either side uses one (the price is divided by
  the nuke's scale for every nuke used, never below 1).
- When your side has reached the second tier of the
  [war effort](../gameplay/war-effort.md) bar, every action costs 15% less
  (the Move prices in the menu do not show this discount; the charge does).

If you cannot afford it:

```dcsmessage
could not start AWACS, Viper: this action costs 100 points and you have 40 points
```

**Limit.** For actions that put a group on the map (aircraft, convoys, Fat
Cows) the limit is how many your side may have up **at once**; a slot frees the
moment one is destroyed, lands or is removed. For actions that put nothing on
the map (waypoints, RTB, Move, nuke, carrier orders, fires, tasks) it is how
many times your side may use it before the server restarts. At the limit:

```dcsmessage
could not start Fat Cow, blue is out of Fat Cow actions
```

**Geo limit.** Some actions can only be aimed within a set distance of a
friendly objective:

```dcsmessage
could not start Paratroopers, this action point is 41230 meters but can only be targeted within 20000 meters of a friendly objective
```

**Penalty.** Charged to the flight's responsible party when an action group is
destroyed, on top of the cost already paid:

```dcsmessage
875(-100) points for the loss of action group 5012
```

A Move penalty works differently: it is charged to you only if you moved
**somebody else's** unit and it is later destroyed ("for the death of 1051
which was deployed by Hawk and moved by you").

**Refunds.**

- **RTB.** A flight sent home with RTB refunds a quarter of the action's cost
  (rounded up) when it gets back to within 10 km of its field. Your side sees
  `Viper's BAWACS-5012 has RTB'd. points refunded: 25`.
- **Server restart.** A bomber, strike, paradrop, deployable delivery or
  logistics run that had not delivered yet when the server restarted gives the
  payer their points back. A reinforcement convoy still on the road carries on
  from where it was (refunded if it can't). AWACS, tankers, fighters,
  attackers, SEAD, drones and cruise missile platforms are put back in the
  air. A recon flight is simply gone.
- **Refused actions** (no field, no target, out of range) charge nothing.
- AI helo missions and the Fat Cow refund on their own terms; see
  [AI Helo Missions](../advanced/helo-missions.md).

**Duration.** Loitering flights (AWACS, tankers, fighters, attackers, SEAD,
drones, recon, cruise missile platforms) can have a lifetime in hours set by
the server. When it runs out the flight is removed, with no refund.

## What happens after you buy

Point-to-point actions report back when they arrive:

| Action | On arrival | If it fails there |
| --- | --- | --- |
| Bomber (within 10 km of the target) | strikes up to its target count among your side's JTAC contacts within 15 km, then flies home | — |
| Logistics Repair | repairs one step of the objective's logistics | `repair mission failed ...` to your side |
| Logistics Transfer | moves supply between the two objectives | `transfer mission failed ...` to your side |
| Paratrooper | drops the squad, which is yours | `paratroop mission failed ...` to you |
| Deployable (by air) | sets the deployable down, yours | `deploy mission failed ...` to you |
| Reinforce | rebuilds lost garrison groups | see [Action Types: Reinforce](../reference/action-types.md#reinforce) |

For the logistics, paratroop and deployable flights, delivery happens when any
aircraft of the flight is within 800 m of the target point.

## Entries with no menu items

A few action kinds are configured but cannot be driven from the menu:

- **Logistics Transfer** has no entry at all. Use the chat command
  `-action <name> <from> <to>` with two of your objectives.
- **Carrier Repair** and **Carrier Respawn** show an empty submenu. Use
  `-action <name> <carrier group>` in chat, or the commander's orders on the
  [command map](../advanced/command.md).

## The same thing in chat

Every action can also be run from chat, which is handy when the mark list is
long:

- `-action help` lists your side's actions as ready-to-type commands with their
  costs.
- `-action <name> <key>` runs a spawn action at the map mark whose text is
  `<key>` (marks you named; it must be unique).
- `-action <name> <group> <key>` for waypoints, Move and RTB.
- `-action <name> <objective>` for actions aimed at a base.
- `-jtac <id> bomber [mission]` calls a bomber on a JTAC's current target.

The reply comes back in chat: `action JTAC Drone M1 started`, or
`could not run action ...` with the reason. See
[Chat Commands](../reference/chat-commands.md).

## Troubleshooting

**"you are not permitted to use actions on this server"** — the server's rules
exclude your account from actions; that is also why you have no `Actions>>`.

**An entry is empty.** It has nothing to list: no marks placed (place one, then
select `Actions>>` again), no JTACs alive for a bomber, no flight of that kind
for a waypoint, no damaged base for Reinforce, or a Carrier Repair/Respawn
entry (see above).

**"could not start ..., calling ...  Caused by: ..."** — the line after
`Caused by:` is the real reason. The common ones are listed on
[Action Types](../reference/action-types.md).

## See Also

- [Action Types](../reference/action-types.md) — every action kind in detail
- [Playbook: Calling AWACS, Tankers & CAP](../playbooks/calling-support.md)
- [The Tasking Board](../gameplay/tasking-board.md)
- [AI Helo Missions](../advanced/helo-missions.md)
- [AI Strike Missions](../advanced/strike-missions.md)
- [Artillery Missions](../advanced/artillery.md)
- [Points and Lives](../gameplay/points-and-lives.md)
