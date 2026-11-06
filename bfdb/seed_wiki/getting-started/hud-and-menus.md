# Understanding the Menus

You talk to Fowl Engine in three places: **chat** commands, the **F10 map**
and the **F10 radio menu**. The server talks back in the DCS message box. This
page covers each one; the [F10 Menu Overview](../f10-menu/overview.md) has the
full menu tree.

![Illustration of a gloved hand resting on a kneeboard map in the cockpit](/api/wiki/images/ed912e81-5a74-493c-a691-2bfff1541054)

## Chat commands

Open the DCS multiplayer chat, type a command and press Enter.

- Every command starts with a dash: `-help`, `-lives`, `-balance`.
- Type them in **lower case**. A few (`-lives`, `-time`, `-weather`,
  `-switch`, a bare `-gci` or `-hq`) work in any case; the rest only match
  lower case.
- The **answer comes back to you only**. The line you typed still shows in
  the chat channel you sent it to, like any other message, so use team chat
  if you don't want the other side reading it.
- A mistyped command gets the help list back, starting with:

```dcsmessage
-hepl is not a valid command. Valid commands follow.
```

The ones you will use most:

| Command | What it does |
| --- | --- |
| `-help` | the list of player commands |
| `-brief` | the short situation briefing |
| `-status` | campaign status: points, kill streak, objectives held, your convoys |
| `-balance` | your points |
| `-lives` | your lives left this round, per role |
| `-weather` | the full weather report for your slot, including winds aloft |
| `-time` | time until the server restarts |
| `-gci` | your GCI voice settings |

All of them, with what the server answers, are on
[Chat Commands](../gameplay/chat-commands.md). The same list is in game under
**F10 → Info → Help → Chat Commands**.

## The F10 map

Press `F10` for the map. Fowl Engine draws the campaign on it for your
coalition:

- **Objectives**: a ring for the zone, a type icon, four status hexes (Health,
  Logistics, Supply, Fuel) and a label with the name and anything happening
  there: capturable, being captured, consolidating, repairing.
- **Warnings**: a thick yellow ring on a threatened base, and markings for
  captures and alerts.
- **Pins** you click for detail: convoys, JTACs (with the full 9-line and laser
  code), tasks on the board, deployed groups, crates, logistics buildings.
- **Intel**: enemy units your side has found by recon, ELINT or JTACs.

The map is your coalition's picture, not the truth: enemy positions appear
only once your side has found them. How to read all of it is on
[Reading the F10 Map](../gameplay/reading-the-map.md).

### Your map marks

The marks **you** place on the map are how you give the engine a position:
where to send an AWACS, a drone, a flight, a ground group or a task.

- Place them **while you are in your aircraft**. Marks made from the spectator
  screen are not yours as far as the engine is concerned.
- Name them short (24 characters at most) and unique, or leave them blank;
  blank marks are listed as `Mark 1`, `Mark 2`, ... nearest first.
- In the menu each mark shows its bearing and range from you, e.g.
  `CAS1 045/12km`.

The full rules are under
[Map marks are your targeting system](../f10-menu/overview.md#map-marks-are-your-targeting-system).

## The F10 radio menu

Open the DCS radio (communications) menu and pick **F10. Other**. That is
where the engine's menus live. **F1** to **F10** pick an entry, **F11** goes
back a level, **F12** closes the menu.

The menus are built for your aircraft when it **spawns**, and what you get
depends on the airframe and the server:

| Menu | Who gets it |
| --- | --- |
| **GCI/EWR** | everyone: radar picture, ground intel, GCI voice settings |
| **Recon** / **Wingman** | recon-capable airframes / servers with AI wingmen; inside one **Flight** menu when you get both |
| **Cargo** or **C-130 Cargo** | cargo-capable airframes: crates and base supply |
| **CSAR** | servers with CSAR on; inside the cargo menu when you have one |
| **Troops** | troop-capable airframes: load and unload infantry |
| **JTAC>>** | JTAC targeting, laser codes, fire missions |
| **Actions>>** | AWACS, tankers, drones and other paid actions, the tasking board, AI helo missions |
| **Ground Forces>>** | servers running the ground war, for those allowed to command it |
| **Objectives** | everyone: base reports and the Capture Advisor |
| **Info** | everyone: your status, the situation briefing, radios, navaids, weather, help |

The full tree, with which slot sees what, is in the
[F10 Menu Overview](../f10-menu/overview.md#the-root-menu).

### `>>` at the end means "select me first"

`JTAC>>`, `Actions>>` and `Ground Forces>>` are **commands**, not folders.
Select one: the menu closes and the engine builds the real submenu from the
world as it is right now. Open F10 again and go in.

This is also how you refresh them. `Actions` folds back to `Actions>>` by
itself when you place a new map mark or after an action has started; select
it again for fresh lists.

### `More >>`: the thing that trips people up

**DCS silently drops anything past the tenth entry in a menu.** No error, no
warning. Fowl Engine spends the last slot of a full page on **`More >>`**,
which opens the next page of the same list.

So if something you expect is not in a list, **look for `More >>` at the
bottom before assuming it isn't there.** Long base lists split into numbered
ranges such as `1. Abu al-Duh - Hama` instead; pick the range your base falls
in.

### A typical tree

```f10menu
{"root":"F10 Other","items":[
 {"label":"GCI/EWR","items":[
  {"label":"Report","note":"Enemy air contacts, BRAA from you."},
  {"label":"Toggle","says":"ewr reports are disabled"},
  {"label":"Units to Imperial","says":"EWR units are now Imperial"},
  {"label":"Units to Metric","says":"EWR units are now Metric"},
  {"label":"GCI Voice","items":[
   {"label":"Units: Imperial","says":"GCI calls will use imperial units"},
   {"label":"Units: Metric","says":"GCI calls will use metric units"},
   {"label":"Units: Server Default","says":"GCI calls will use the server default units"}]}]},
 {"label":"Actions>>","note":"Select once; the menu closes. Open F10 again and Actions is a submenu."},
 {"label":"Actions","note":"What Actions>> turns into. Names and prices are the server's own.","items":[
  {"label":"AWACS(100 pts)","items":[
   {"label":"CAS1 045/12km","says":"action AWACS started"},
   {"label":"Mark 1 310/38km","says":"action AWACS started"}]},
  {"label":"Add Task"},
  {"label":"More >>","items":[{"label":"AI Helo Missions"}]}]},
 {"label":"Objectives","items":[
  {"label":"Capture Advisor: Nearest","note":"Why that base won't flip yet."}]},
 {"label":"Info","items":[
  {"label":"My Status","says":"=== Viper ===\nSide: Blue\nPoints: 1250\nKill streak: 3\nCareer kills: 87\nSide switches allowed: 1/round"},
  {"label":"Situation","items":[{"label":"1. Overview"},{"label":"2. Tasking"}]},
  {"label":"Time & Server","says":"Server restarts in 02:14:37"}]}
]}
```

### Map or radio menu?

| F10 map | F10 radio menu |
| --- | --- |
| The campaign picture: bases, front, pins, intel | Commands and reports |
| Open it any time | Only once your aircraft has spawned |
| Place marks | Use your marks |

**Common workflow**:

1. Spawn, then open the **F10 map** and look at the picture.
2. Place a mark where you want something to happen.
3. Radio menu → **F10. Other** → `Actions>>`.
4. Open it again → **Actions** → the action → your mark.

### The cockpit panel

The optional [Vector Strike cockpit](./cockpit-overlay.md) shows the whole F10
menu on a panel you can read and click, alongside your cargo, status, the EWR
picture, a log of every server message and this wiki. It also adds a radial
wheel for one-glance commands. The radio menu keeps working either way.

## Server messages

The server answers in the DCS message box (top right) and, for chat commands
and slot notices, in chat:

- **Replies to you**: the result of a menu command or chat command, e.g.
  `action AWACS started` or `could not start AWACS, ...` with the reason.
  Menu replies stay up about 10 seconds; long reports such as the Situation
  pages stay longer.
- **Side-wide notices**: captures, threats to your bases, crates unpacked,
  troops landed, task updates.
- **Slot-entry panels**: the airfield ATIS and the situation briefing a few
  seconds after you spawn, where the server has them on; see
  [Welcome](./welcome.md#first-steps).

Missed one? Run the command again,
open **Info → My Status**, or use the cockpit panel's **LOG** tab.

## Units

- **EWR text reports**: **F10 → GCI/EWR → Units to Imperial / Units to
  Metric**. They start in your airframe's own units (metric for the Russian
  types) until you pick.
- **GCI voice calls**: **F10 → GCI/EWR → GCI Voice → Units: Imperial / Metric
  / Server Default**, or `-gci imperial` / `-gci metric` in chat. See
  [GCI](../gameplay/gci.md).

## Quick reference

**Must-know commands**:

- `-help`: the command list
- `-brief`: the short situation briefing
- `-status`: campaign status, your points, objectives held, your convoys
- `-balance` and `-lives`: points and lives
- `-weather`: weather at your slot

**Must-know menus**:

- `F10 → Info → Situation`: the full six-page briefing
- `F10 → Objectives → Capture Advisor: Nearest`: why that base won't flip
- `F10 → Info → Support & Radios`: is there a tanker up, and on what frequency
- `F10 → Info → Help`: short guides to cargo, capturing, carriers and JTACs

## See Also

- [F10 Menu Overview](../f10-menu/overview.md): every menu, in detail
- [Reading the F10 Map](../gameplay/reading-the-map.md)
- [Chat Commands](../gameplay/chat-commands.md)
- [Your First Sortie](../playbooks/first-sortie.md)
