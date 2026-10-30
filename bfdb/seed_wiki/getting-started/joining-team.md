# Joining a Team

Before you can fly or fight you belong to a coalition, Blue or Red. You join one
simply by taking a slot.

## Connecting

The server may turn a connection away before you reach the slot screen. The
reason shows in the DCS disconnect box:

| Message | Why |
| --- | --- |
| `The server is not finished loading the mission` | The mission is still loading; try again in a moment |
| `The server is initializing ETA 40s` | The campaign is starting up |
| `name must match <pattern>` | The server only accepts callsigns of a certain form |
| `your callsign is already taken by another player` | Someone on the server has your name |
| `another player is already connected from your ip address` | One player per IP address is enforced ({{cfg:one_player_per_ip|yes}} here) |
| `you are banned until <date>` / `you are banned forever` | A ban |

## Registering

There is **no command to type**. Pick a coalition and a slot on the normal DCS
slot screen, and the first slot you take registers you to that coalition:

```steps
[
  {"title": "Open the slot screen", "text": "Connect to the server and open the role/slot selection screen."},
  {"title": "Choose Blue or Red", "text": "Pick the coalition you want to fly for. Your first slot decides it, so choose deliberately."},
  {"title": "Pick a slot at a friendly base", "text": "Take an aircraft slot (or a Combined Arms slot) at a base your coalition holds. The slot list shows where each one sits."},
  {"title": "Read the welcome", "text": "The server confirms your registration in chat: **Welcome to the Blue team. You may only occupy slots belonging to your team. Good luck!**"},
  {"title": "Wait out the hold and read the briefing", "text": "If the server runs a takeoff hold, wait for **Takeoff hold cleared -- happy hunting!** before you get airborne. The slot briefing arrives a few seconds after you spawn, with your task and the war effort."},
  {"title": "Check your account", "text": "Type `-lives` and `-balance` in chat, or open **F10 → Info → My Status**, to see your lives and points."}
]
```

The server confirms it to you:

```dcsmessage
Welcome to the Blue team. You may only occupy slots belonging to your team. Good luck!
```

and tells everyone else `Viper21 has joined Blue team`.

- From then on you can only take slots of your coalition (unless you switch, below).
- Your registration lasts for the round. A new round starts everyone fresh.
- You start with a points balance; see [Points and Lives](../gameplay/points-and-lives.md).

## Taking a slot

Every time you pick an aircraft slot, the server checks it. A refused slot puts
you back on the slot screen with one of these in chat:

| Message | What it means |
| --- | --- |
| `You are on Blue in this campaign and sides are locked on this server -- pick a Blue slot` | Wrong coalition, and the server doesn't allow switching by slot |
| `You are on Blue in this campaign and you have no side switches left -- pick a Blue slot` | Wrong coalition, and you've used your switches |
| `Blue does not own the objective associated with this slot` | The base this slot sits at isn't yours (or is neutral) |
| `Objective is still consolidating (120s left) -- hold it with troops, or land a logistics crate to speed it up` | The base was just captured and isn't secure yet |
| `Objective is capturable` | The base is battered down to capture state; nothing flies from it until it's repaired |
| `Objective does not have any F-16C_50 in stock` | The base is out of that airframe; fly something else or from somewhere else |
| `Su-27 was captured here -- it will be flyable once this objective is repaired` | A captured enemy airframe, not usable until the base is repaired |
| `F-16C_50 costs 300, you have 120` | You and the base fund together can't afford the airframe |
| `you have no Standard lives remaining. standard 0/3 resetting in 03:10:44` | That role's lives are used up |
| `F-14B is not available in the current era (1980s)` | The server's era rules exclude this airframe |
| `access to slot is denied` | An instructor slot (admins only), or a Combined Arms slot you are barred from |

A Combined Arms commander slot also prints a short guide to commanding from
chat; see [Chat Commands](../gameplay/chat-commands.md#combined-arms-commanders).

## After you take a slot

- **Takeoff hold.** Where the server runs one ({{cfg:takeoff_delay_secs|0}} s
  here, shorter for a side high on the [war effort bar](../gameplay/war-effort.md)),
  a ground start must wait before getting airborne. You get
  `Takeoff hold: 25s remaining` every few seconds, then
  `Takeoff hold cleared -- happy hunting!`. Take off early and the aircraft is
  removed:

```dcsmessage
You took off 12s before your ground hold expired -- returning to spectators.
```

- **Briefings.** The GCI frequency, the situation briefing with
  [your task](../gameplay/your-task.md) and the war effort arrive in the first
  seconds, where the server has them on.
- **Taxi.** Where airframes and weapons cost points, the `FLIGHT COST` panel
  appears when you start to roll; see
  [Points and Lives](../gameplay/points-and-lives.md#your-aircraft-and-weapons).
- **Takeoff.** Fixed-wing aircraft (Harriers excepted) are expected to use the
  runway. Lifting off from a taxiway or apron gets a warning, not a penalty:
  `You departed from a taxiway or apron. Fixed-wing aircraft must take off from the active runway.`

## Side switching

Changing coalition costs one **side switch**. This server allows
**{{cfg:side_switches|unlimited}}** per round, and sides locked:
**{{cfg:lock_sides|yes}}**.

From spectators, type:

```
-switch blue
-switch red
```

The whole server sees `Viper21 has switched to Red`. You must leave your slot
first, and you need a switch left.

Where sides are **not** locked, simply taking a slot of the other coalition
also switches you, and uses up a switch the same way. A slot that is refused
for another reason doesn't cost one. Where sides **are** locked, a slot of the
other coalition is always refused.

`F10 → Info → My Status` shows the server's rule: `Sides are LOCKED this round`
or `Side switches allowed: 1/round`.

## Checking your status

| Command | Shows |
| --- | --- |
| `-lives` | Lives left per role and when they refill |
| `-balance` | Your points |
| `-status` | Side, points, kill streak, career kills, objectives held, your side's convoys |
| `F10 → Info → My Status` | All of the above in the cockpit, plus the side-switch rule |

## Common issues

### "you must be in spectators to switch sides"
Leave your slot and return to spectators, then `-switch`.

### "you can't switch sides again this round"
You've used all your side switches. You stay on your coalition until the
round resets.

### A slot of the other coalition is refused
You are registered to the other side. Pick one of your own slots, or (with a
switch left) go to spectators and `-switch`.

## Next Steps

See [Understanding the Menus](./hud-and-menus.md) for the interface.
