# AI Strike Missions

A **Strike** action sends an AI flight from a friendly field to attack with
its own weapons, for example an F-117 on a precision strike. Unlike the Bomber
action, it needs no JTAC on the target. DCS does the damage, so kills,
destroyed buildings and the target base's logistics update just as they would
if a player had dropped the bombs.

| Mission | You aim it at | What the flight does | Default weapons |
| --- | --- | --- | --- |
| **Precision** | an enemy objective | Attacks the base's tracked buildings (its logistics), nearest the centre first; the base centre if it has none | guided bombs, two per aim point |
| **Runway** | an enemy airbase | Bombs its runways | any bomb, all of them |
| **Anti-ship** | an enemy carrier group | Attacks the group's ships | anti-ship missiles, half |
| **Interdiction** | a map mark | Attacks the live enemy ground group nearest the mark, within 15 km | the AI's choice, half |
| **Carpet** | a map mark | Lays a line of bombs through the mark | any bomb, all of them |

Each action attacks up to its configured number of aim points (engine default
2, at most 8): buildings, runways or ships. A carpet line is the configured
length (engine default 1500 m). The server can also pick the weapons.

## Calling one

```
F10 → Actions>> → <strike action> → [enemy objective or your map mark]
```

Precision, Runway and Anti-ship strikes list the enemy objectives you can see.
Interdiction and Carpet list your map marks, so drop a mark first (see
[map marks](../f10-menu/overview.md#map-marks-are-your-targeting-system)).

```f10menu
{"root":"F10 Other","items":[
 {"label":"Actions","items":[
  {"label":"Precision Strike(300 pts)","items":[
   {"label":"Gori","says":"action Precision Strike started"},
   {"label":"Tskhinvali","says":"action Precision Strike started"}
  ]},
  {"label":"Runway Attack(250 pts)","items":[
   {"label":"Gori","says":"could not start Runway Attack, calling strike\n\nCaused by:\n    pick an enemy airbase for a runway attack"},
   {"label":"Beslan","says":"action Runway Attack started"}
  ]},
  {"label":"Anti-ship Strike(300 pts)","items":[
   {"label":"Red CSG","says":"action Anti-ship Strike started"},
   {"label":"Gori","says":"could not start Anti-ship Strike, calling strike\n\nCaused by:\n    pick an enemy carrier group for an anti-ship strike"}
  ]},
  {"label":"Interdiction(200 pts)","items":[
   {"label":"ROAD1 060/24km","says":"action Interdiction started"},
   {"label":"Mark 1 015/40km","says":"could not start Interdiction, calling strike\n\nCaused by:\n    no enemy convoy or column within 15 km of the mark"}
  ]},
  {"label":"Carpet Bombing(250 pts)","items":[
   {"label":"ROAD1 060/24km","says":"action Carpet Bombing started"}
  ]}
 ]}
]}
```

The names and prices above are examples: the
[Actions Menu](../f10-menu/actions.md#what-this-server-offers) page lists this
server's strike actions and costs. In chat: `-action <name> <key>`, where
`<key>` is the text of your map mark (for a base strike, a mark within 8 km of
the enemy base).

## How a strike flies

1. It takes off from the nearest friendly field more than 10 km from the
   target with room for it (a carpet bomber needs a large stand), with weapons
   held and only evading fire, so a stealth aircraft stays quiet on the way in.
2. At the **initial point**, 20 km short of the target (halfway, on a short
   trip), it goes weapons free and attacks.
3. It flies home and is removed once it is back within 10 km of its field.

Strikes against ships aim at the ships themselves, so a moving task force is
still hit.

## When it can't go

| Message after `Caused by:` | Means |
| --- | --- |
| `pick an enemy objective to strike` | no enemy objective within 8 km of the aim point |
| `pick an enemy airbase for a runway attack` | the base you picked is not an airbase |
| `pick an enemy carrier group for an anti-ship strike` | no enemy carrier group within 40 km |
| `Red CSG has no ships in the water to attack` | the task force is already sunk |
| `no enemy convoy or column within 15 km of the mark` | nothing to interdict near your mark |
| `no friendly airfield more than 10 km from the target has room to launch this flight` | nowhere to take off from |

A refused strike costs nothing.

## Refunds and losses

- If the server restarts while a strike is still on its way to the target,
  whoever paid for it gets back what they paid. Once it has reached the
  target, there is no refund.
- `RTB` sends a strike flight home early and refunds a quarter of the action's
  cost when it lands.
- If the flight is shot down, its responsible party pays the action's penalty.

Commanders can also order strikes from the [command map](./command.md).

## See Also

- [Action Types](../reference/action-types.md)
- [Actions Menu](../f10-menu/actions.md)
- [Air-Launched Cruise Missiles](./alcm.md)
