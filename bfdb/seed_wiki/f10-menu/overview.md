# F10 Menu Overview

The F10 radio menu is where you actually play Fowl Engine. Everything the
engine offers a pilot (reports, capture advice, cargo, JTAC, AI dispatches,
the tasking board, ground formations) hangs off it.

![F10 radio menu open in the cockpit showing the Fowl Engine command tree](/api/wiki/images/441295e3-48c3-4737-b308-e5f91812884c)

## Opening it

Open the DCS radio (communications) menu and pick **F10. Other**. The engine's
menus sit there, next to anything the mission itself adds. In any menu,
**F1** to **F10** pick an entry, **F11** goes back one level and **F12**
closes the menu.

The menus belong to your **aircraft group** and are built when your aircraft
**spawns**, not when you click the slot. The server builds a few slots' menus
each second, so after a restart, when everyone spawns at once, yours can take a
few seconds to appear. A menu that fails to build is retried a few times on
its own.

No group, no menu: spectators, the Combined Arms commander, forward observer
and observer slots and instructor slots get no engine menus. A Combined Arms
commander slot gets a short guide in chat instead, because every one of these
things can also be done from [chat](../gameplay/chat-commands.md).

If you have the [Vector Strike cockpit](../getting-started/cockpit-overlay.md)
installed, its **MENU** tab shows this same tree on a panel you can read and
click.

If a menu looks wrong after something changed in the world, leave and re-enter
the slot. Every menu is rebuilt when you spawn.

## The root menu

Each menu appears only when your airframe can use it, the server has the
feature on and your account passes the server's rule for it. This replica is a
recon-capable cargo helicopter on a server with everything on; it is about as
full as the root gets.

```f10menu
{"root":"F10 Other","items":[
 {"label":"GCI/EWR","note":"Every slot.","items":[
  {"label":"Report","note":"Enemy air contacts, BRAA from you. See the GCI/EWR page."},
  {"label":"Toggle","says":"ewr reports are disabled","note":"Clicking again: ewr reports are enabled"},
  {"label":"Friendly Report","note":"Friendly air contacts, BRAA from you."},
  {"label":"Units to Imperial","says":"EWR units are now Imperial"},
  {"label":"Units to Metric","says":"EWR units are now Metric"},
  {"label":"Ground Intel","note":"Enemy ground contacts your side knows about."},
  {"label":"GCI Voice","items":[
   {"label":"Toggle GCI Calls","says":"GCI voice calls are disabled"},
   {"label":"Toggle Auto Callouts"},
   {"label":"Units: Imperial","says":"GCI calls will use imperial units"},
   {"label":"Units: Metric","says":"GCI calls will use metric units"},
   {"label":"Units: Server Default","says":"GCI calls will use the server default units"},
   {"label":"Reference: BRAA","says":"GCI calls will use BRAA from your aircraft"},
   {"label":"Reference: Bullseye","says":"GCI calls will use bullseye reference"},
   {"label":"Reference: Clock","says":"GCI calls will use clock position"}]}]},
 {"label":"Flight","note":"Only when the slot gets both Recon and Wingman. With just one of them, that menu sits here at the top level instead.","items":[
  {"label":"Recon","note":"Recon-capable airframes, on servers with player recon on.","items":[
   {"label":"Start Recon Pass","note":"Starts on the nearest enemy objective in range. See the Reconnaissance page."},
   {"label":"Cancel Recon Pass"},
   {"label":"Recon Status","says":"Recon on Senaki: 45% (54/120s)"}]},
  {"label":"Wingman","note":"Every aircraft slot, on servers with AI wingmen on.","items":[
   {"label":"Request Wingman","note":"Only once you are airborne. See Wingmen, AI Packages & Civil Traffic."},
   {"label":"Release Wingman","says":"F-16C_50 wingman released, RTB."},
   {"label":"Wingman Status","says":"F-16C_50 wingman: 2/2 aircraft, 3.4 nm from you, 52 min left."}]}]},
 {"label":"Cargo","note":"Airframes the server gives crate slots (or lists for helicopter dynamic cargo). A C-130 gets C-130 Cargo here instead. This is the classic menu.","items":[
  {"label":"Unpack Nearby Crate(s)","says":"KillerDog unpacked a Hawk System"},
  {"label":"Load Nearby Crate","says":"Hawk Launcher crate loaded\n1 of 4 Hawk System deployed, unpacking will delete oldest, 4471 when the limit is exceeded"},
  {"label":"Unload Crate","says":"Hawk Launcher crate unloaded"},
  {"label":"List Nearby Crates","says":"Hawk Launcher crate, bearing 271, 32 meters away\nHawk CC crate, bearing 268, 41 meters away"},
  {"label":"List Cargo","note":"Troops, crates and pilots aboard, and their weight."},
  {"label":"Destroy Nearby Crate"},
  {"label":"Crates","note":"Base Supply plus the server's deployable categories. See Cargo Operations.","items":[
   {"label":"Base Supply","note":"Repair kit, fuel and weapons transfer crates."},
   {"label":"Air Defence","note":"An example deployable category."}]},
  {"label":"CSAR","note":"Inside Cargo whenever the slot has a cargo menu; at the top level otherwise. Only when CSAR is on.","items":[
   {"label":"List Downed Pilots","says":"No downed pilots on your side"},
   {"label":"Request Nearest Pilot Smoke"},
   {"label":"Pick Up Downed Pilot (manual)"},
   {"label":"Deliver Rescued Pilots (manual)"}]}]},
 {"label":"Troops","note":"Airframes the server gives troop slots.","items":[
  {"label":"Unload","says":"KillerDog dropped Standard troops into the field"},
  {"label":"Extract"},
  {"label":"List","note":"Same cargo list as Cargo > List Cargo."},
  {"label":"Return","says":"KillerDog returned Standard troops"},
  {"label":"Squads","items":[
   {"label":"Load Standard squad","says":"KillerDog loaded Standard\n3 of 10 Standard deployed, unloading will delete oldest, 5521 when the limit is exceeded"},
   {"label":"Load Mortar squad (5 pts)"}]}]},
 {"label":"JTAC>>","note":"A command, not a folder. Select it and F10 closes; open F10 again and it is the JTAC submenu (shown in the next replica)."},
 {"label":"Actions>>","note":"A command, not a folder. Select it and the Actions submenu is built from the world as it is now (next replica)."},
 {"label":"Ground Forces>>","note":"Only with the ground war on and for accounts that may command it. A command that opens into the Ground Forces submenu (next replica)."},
 {"label":"Objectives","note":"Every slot.","items":[
  {"label":"Nearest Base (detail)"},
  {"label":"Capturable / Contested"},
  {"label":"Capture Advisor: Nearest"},
  {"label":"Bases Under Threat"},
  {"label":"Friendly Status","items":[{"label":"Show (First Page)"},{"label":"Next Page >>"},{"label":"<< Previous Page"}]},
  {"label":"Enemy Status","items":[{"label":"Show (First Page)"},{"label":"Next Page >>"},{"label":"<< Previous Page"}]},
  {"label":"Neutral Status","items":[{"label":"Show (First Page)"},{"label":"Next Page >>"},{"label":"<< Previous Page"}]},
  {"label":"Base Detail","note":"Your own bases.","items":[{"label":"Abu al-Duhur"},{"label":"Hama"}]},
  {"label":"Capture Advisor","note":"Enemy and neutral bases.","items":[{"label":"1. Aleppo - Jirah"},{"label":"2. Kuweires - Taftanaz"}]}]},
 {"label":"Info","note":"Every slot.","items":[
  {"label":"My Status","says":"=== Viper ===\nSide: Blue\nPoints: 1250\nKill streak: 3\nCareer kills: 87\nSide switches allowed: 1/round"},
  {"label":"Situation","items":[
   {"label":"1. Overview"},{"label":"2. Tasking"},{"label":"3. Hotspots"},
   {"label":"4. Threats & Air"},{"label":"5. Logistics"},{"label":"6. Comms"}]},
  {"label":"Support & Radios"},
  {"label":"Supply Convoys"},
  {"label":"Navaids Directory"},
  {"label":"HQ","note":"Only on servers that run the Theatre HQ for your side.","items":[
   {"label":"Commander's Intent"},{"label":"Operations"},{"label":"Request Support (nearest objective)"}]},
  {"label":"Time & Server","says":"Server restarts in 02:14:37"},
  {"label":"Weather"},
  {"label":"Help","items":[
   {"label":"Getting Started"},{"label":"Chat Commands"},{"label":"Cargo & Logistics"},
   {"label":"Objectives & Capturing"},{"label":"Carrier Groups"},{"label":"Combat & JTAC"}]}]}
]}
```

`JTAC>>`, `Actions>>` and `Ground Forces>>` are built when you select them.
After that, the same root entries are submenus:

```f10menu
{"root":"F10 Other","items":[
 {"label":"JTAC","items":[
  {"label":"Refresh Locations","note":"Rebuilds this list. No message; F10 closes."},
  {"label":"Kobuleti (1 JTAC)>>","says":"1 JTAC near Kobuleti loaded.\nRe-open F10 and go to JTAC > Kobuleti to work them."},
  {"label":"Senaki (3 JTAC)>>","says":"3 JTAC near Senaki loaded.\nRe-open F10 and go to JTAC > Senaki to work them."},
  {"label":"Batumi","note":"A place you already loaded. Each JTAC carries its laser code in brackets.","items":[
   {"label":"Reaper 1234 [1688]","note":"Status, 9-Line, Shift, Smoke, Code, Filter, Artillery and more. See the JTAC page."}]}]},
 {"label":"Actions","note":"Names and prices are this server's own; see the Actions page for its list.","items":[
  {"label":"AWACS(100 pts)","items":[
   {"label":"CAS1 045/12km","says":"action AWACS started"},
   {"label":"Mark 1 310/38km","says":"could not start AWACS, Viper: this action costs 100 points and you have 40 points"}]},
  {"label":"Tanker(50 pts)"},
  {"label":"JTAC Drone(25 pts)"},
  {"label":"RTB"},
  {"label":"Move(10 pts/km)"},
  {"label":"Add Task","note":"The coalition tasking board."},
  {"label":"More >>","items":[
   {"label":"Remove Task"},
   {"label":"Request Fires"},
   {"label":"AI Helo Missions"}]}]},
 {"label":"Ground Forces","items":[
  {"label":"Status report","note":"Everyone on your side can read it."},
  {"label":"Refresh this menu"},
  {"label":"3rd Mech Coy (Senaki) (moving, 92%)","note":"One entry per formation: Attack, Defend / move to, Hold position, Withdraw, Hand back to AI command."},
  {"label":"Raise a formation"}]}
]}
```

**Which ones you get:**

| Menu | Appears when | Page |
| --- | --- | --- |
| GCI/EWR | always | [Early Warning Radar](./ewr.md) |
| Recon | your airframe carries the server's Recon tag and player recon is on. Inside **Flight** if you also get Wingman | [Reconnaissance](./recon.md) |
| Wingman | the server offers AI wingmen (every aircraft slot). Inside **Flight** if you also get Recon | [Wingmen: the Wingman menu](../advanced/air-life.md#the-wingman-menu) |
| Cargo | your airframe has crate slots in the server's cargo table, or is on its helicopter dynamic-cargo list, and you pass the cargo rule | [Cargo Operations](./cargo.md) |
| C-130 Cargo | your airframe is on the server's C-130 cargo list (instead of Cargo) | [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md) |
| CSAR | CSAR is on and you pass the cargo rule. Inside Cargo / C-130 Cargo when you have one, at the top level otherwise | [Combat Search & Rescue](./csar.md) |
| Troops | your airframe has troop slots and you pass the troops rule | [Troop Transport](./troops.md) |
| JTAC>> | you pass the server's JTAC rule | [JTAC System](./jtac.md) |
| Actions>> | you pass the server's actions rule | [Actions Menu](./actions.md) |
| Ground Forces>> | the ground war is on and you pass its command rule | [Ground War: Commanding a formation yourself](../advanced/ground-war.md#commanding-a-formation-yourself) |
| Objectives | always | [Objectives Menu](./objectives.md) |
| Info | always | [Info Menu](./info.md) |

The menus that depend on the airframe (Recon, Cargo, Troops) are
capabilities of the aircraft type, not permissions for you. Most servers let
everyone through the JTAC, Actions, cargo and troops rules.

A fast jet on a server with wingmen and CSAR on therefore sees GCI/EWR,
Wingman, CSAR, JTAC>>, Actions>>, Objectives and Info, plus Ground Forces>>
where the ground war allows it.

### `JTAC>>`, `Actions>>` and `Ground Forces>>` are buttons, not folders

These three start life as a **command**, not a submenu. Select one and DCS
closes the menu (it always does after a command) while the engine replaces the
entry with the full submenu, built from the world as it is *right now*: your
current map marks, the JTACs currently alive, the formations in the field.
Open F10 again and go in. Nothing is printed when you do this.

That is also how they stay fresh:

- **Actions** turns back into `Actions>>` by itself after an action you picked
  has started, and whenever you place a new map mark. Select it again to see
  the new lists.
- **JTAC** has **Refresh Locations** at the top of its list.
- **Ground Forces** has **Refresh this menu**.

## Paging: `More >>`

**DCS silently drops anything past the tenth entry in a menu.** No error, no
warning: the eleventh JTAC, the eleventh deployable, the eleventh mark simply
is not there.

Fowl Engine works around this with a pager. When a list outgrows a page, the
tenth slot becomes **`More >>`**, which opens the next page of the same list:
nine entries per page, then `More >>`, as deep as it needs to go.

```
F10 → JTAC
  1. Refresh Locations
  2. Batumi (2 JTAC)>>
  ...
  9. Senaki (1 JTAC)>>
 10. More >>   ← the rest of the places live in here
```

So: **if you cannot find something in a list, look for `More >>` at the bottom
before assuming it doesn't exist.**

Long base lists in the Objectives menu are split differently, into numbered
range submenus named after the first and last base in each (up to ten
characters of each name):

```
F10 → Objectives → Base Detail
  1. Abu al-Duh - Hama
  2. Jirah - Kuweires
  3. Masyaf - Tartus
```

Pick the range your base falls in. On very large maps the ranges split again
one level down. The Friendly / Enemy / Neutral Status reports page with their
own `Next Page >>` and `<< Previous Page` commands instead.

## The one hard limit

The root menu itself is **not** paged: each top-level menu is removed and
rebuilt by its fixed name from several places in the engine, and a `More >>`
page would move it out from under them. DCS shows ten entries there, so the
engine keeps to a budget: CSAR goes inside the cargo menu when you have one,
and Recon and Wingman share a **Flight** menu when you get both. That is why
new features land *inside* Actions or Info rather than as new root entries.

## Map marks are your targeting system

Anything that needs a position takes it from an F10 **map mark** you placed:
spawning an AWACS or a drone, sending a flight somewhere, moving a ground group,
posting a task on the board, aiming a fire mission.

1. **Spawn first.** A mark only counts as yours if you placed it while sitting
   in your aircraft. Marks made from the spectator screen or before you spawned
   are not listed.
2. Open the F10 **map** (not the radio menu) and add a mark where you want the
   thing to happen. Name it, or leave it blank.
3. Back in the radio menu, select `Actions>>` (or re-select it if it was
   already open) and go into the action. Your marks are listed under it.

How the list reads:

| You placed | It shows as |
| --- | --- |
| A mark named `CAS1` | `CAS1 045/12km`: true bearing and range from your aircraft |
| A mark with no text | `Mark 1 310/38km`, `Mark 2 ...`, numbered nearest first |
| Two marks with the same text | neither. Use `CAS1` and `CAS2`, not `CAS` twice |
| A name longer than 24 characters | nothing. Keep names short |

- Marks are listed **per player**: you only ever see your own.
- The list is **nearest first**, so the top entry is usually the one you just
  dropped.
- Under **Move**, each mark shows the distance from that group and the price
  of the move instead: `Mark 1 18km (180 pts)`.
- **The mark you pick is deleted** once the action has started, so the next
  list is not cluttered with spent marks.
- Placing a mark folds an open Actions menu back to `Actions>>`, so select it
  again before looking for the new mark.

Marks have two other uses: a JTAC's **Focus on My Latest Mark** lases near your
newest mark, and `-action <name> <mark text>` in chat runs an action at a
**named** mark (see [Chat Commands](../gameplay/chat-commands.md)).

## Costs

Entries that cost points carry the price in the label, for example
`AWACS(100 pts)`, `Move(10 pts/km)`, `Load Mortar squad (5 pts)` or a
deployable crate's `Hawk System(35 pts)`. Each server sets its own prices; the
[Actions Menu](./actions.md) page lists the ones on the server selected at the
top.

Reports, the Capture Advisor, the situation briefing, navaid listings, the
tasking board and CSAR cost nothing. See
[Points and Lives](../gameplay/points-and-lives.md) for how points are earned
and spent.

## Menu-by-menu

| Menu | Page |
| --- | --- |
| GCI/EWR | [Early Warning Radar](./ewr.md), and [GCI](../gameplay/gci.md) for the voice controller |
| Flight → Recon | [Reconnaissance](./recon.md) |
| Flight → Wingman | [Wingmen, AI Packages & Civil Traffic: the Wingman menu](../advanced/air-life.md#the-wingman-menu) |
| Cargo | [Cargo Operations](./cargo.md) |
| C-130 Cargo | [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md) and [Cargo Operations](./cargo.md) |
| CSAR | [Combat Search & Rescue](./csar.md) |
| Troops | [Troop Transport](./troops.md) |
| JTAC | [JTAC System](./jtac.md) |
| Actions | [Actions Menu](./actions.md), [Tasking Board](../gameplay/tasking-board.md), [AI Helo Missions](../advanced/helo-missions.md) |
| Ground Forces | [Ground War: Commanding a formation yourself](../advanced/ground-war.md#commanding-a-formation-yourself) |
| Objectives | [Objectives Menu](./objectives.md) |
| Info | [Info Menu](./info.md), [The Auto-Generated Briefing](../gameplay/briefing.md) for Situation, [Theatre HQ](../advanced/theatre-hq.md) for HQ |

## Troubleshooting

**"The menu I want isn't there."**
Wrong airframe for that feature, the server has it off, or the server's rules
leave you out. Check the table above. Cargo, Troops and Recon in particular
come with the aircraft type.

**"There are no Fowl Engine menus at all."**
You are not in an aircraft group (spectator or a Combined Arms slot), or your
aircraft has only just spawned. Give it a few seconds; if they still don't
come, leave and re-enter the slot.

**"The list is missing entries I know exist."**
Look for `More >>`. Then check you are not looking at a filtered list: the
Objectives menu's Base Detail only lists bases *you own*, and Capture Advisor
only lists ones you don't.

**"I picked an action and nothing happened."**
Commands answer in your message box: `action AWACS started`, or
`could not start AWACS, ...` with the reason. The exceptions are the entries
that only rebuild a menu (`JTAC>>`, `Actions>>`, `Ground Forces>>`, Refresh
Locations, Refresh this menu), which close F10 silently. If a real command
says nothing at all, leave and re-enter the slot.

**"My map mark isn't in the list."**
Select `Actions>>` again so the list is rebuilt. If it is still missing, you
placed it before you spawned, its name is over 24 characters, or another of
your marks has the same name.

## See Also

- [Understanding the Menus](../getting-started/hud-and-menus.md): chat, map and radio menu basics
- [Vector Strike Cockpit](../getting-started/cockpit-overlay.md): the same menu on an in-game panel
- [The Auto-Generated Briefing](../gameplay/briefing.md): what the Situation pages contain
- [Chat Commands](../gameplay/chat-commands.md): the things that aren't on the F10 menu
