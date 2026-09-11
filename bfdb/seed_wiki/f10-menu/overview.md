# F10 Menu Overview

The F10 radio menu is where you actually play Fowl Engine. Everything the
engine offers a pilot — reports, capture advice, cargo, JTAC, AI dispatches,
the tasking board — hangs off it.

![F10 radio menu open in the cockpit showing the Fowl Engine command tree](/api/wiki/images/441295e3-48c3-4737-b308-e5f91812884c)

## Opening it

Slot into an aircraft, press `F10` ("Other" / the communications menu), and the
engine's menus appear alongside the stock DCS ones. Spectators and Combined Arms
slots get nothing — the menus are built per *aircraft group*, when you occupy
the slot.

If a menu looks stale after something changed in the world, leave and re-enter
the slot. The engine rebuilds the whole tree on slot entry.

## The root menu

Up to nine top-level entries, depending on what you are flying and what the
server has enabled:

```
F10
├── GCI/EWR      radar picture, ground intel, GCI voice settings   (always)
├── Recon        player recon pass                    (recon-tagged airframes)
├── Cargo        crates & base supply             (crate-capable helos, Gazelle)
│              ...or "C-130 Cargo" instead, if you are in the C-130J-30
├── CSAR         downed pilots                             (server-enabled)
├── Troops       infantry load/unload             (troop-capable airframes)
├── JTAC>>       laser, codes, fire missions, filters
├── Actions>>    AWACS/tanker/bomber/drone, tasking board, AI helo missions
├── Objectives   base reports, Capture Advisor, threat list      (always)
└── Info         your status, situation briefing, navaids, weather, help (always)
```

**Which ones you get:**

| Menu | Appears when |
| --- | --- |
| GCI/EWR | always |
| Recon | your airframe is tagged Recon (see [Aircraft Roster](../reference/aircraft-roster.md)) |
| C-130 Cargo | you are in the C-130J-30 |
| Cargo | your airframe has crate slots (Mi-8MT, UH-1H, CH-47F, Mi-24P, Gazelle L/Minigun) |
| CSAR | CSAR is enabled on the server (it is on the live mission) |
| Troops | your airframe has troop slots |
| JTAC / Actions | your account passes the server's rules for that feature |
| Objectives / Info | always |

A fixed-wing fast jet therefore sees a short tree (GCI/EWR, JTAC, Actions,
Objectives, Info). A Mi-8 sees nearly all of it.

### `JTAC>>` and `Actions>>` are buttons, not folders

These two start life as a **command**, not a submenu — you will see
`Actions>>` sitting at the root with no arrow into it. Select it once and it
rebuilds itself into the full `Actions` submenu, populated with the world as it
is *right now*: your current map marks, the JTACs currently alive, the
objectives currently capturable.

That is the mechanism, and it is also the fix for most "the menu is out of
date" problems: **re-open the slot's `Actions>>` / `JTAC>>` to rebuild the
list.** (If the submenu is already built, leave and re-enter the slot.)

## Paging: `More >>`

**DCS silently drops anything past the tenth entry in a menu.** No error, no
warning — the eleventh JTAC, the eleventh deployable, the forty-first objective
simply is not there.

Fowl Engine works around this with a pager. When a list outgrows a page, the
last slot becomes **`More >>`**, which opens the next page of the same list.
Chains as deep as it needs to.

```
F10 → JTAC
  1. FF11 ...
  ...
  9. FF19 ...
 10. More >>   ← the rest of the JTACs live in here
```

So: **if you cannot find something in a list, look for `More >>` at the bottom
before assuming it doesn't exist.**

Long base lists are chunked differently — into alphabetical range submenus:

```
F10 → Objectives → Base Detail
  1. Abu Su - Damascu
  2. Deir ez - Kiryat
  3. King Hu - Ramat D
```

Pick the range your base falls in. On very large maps the ranges split again one
level down.

## The one hard limit

The root menu itself is **not** paged — its entries have fixed paths the engine
rebuilds from several places. There is room for ten top-level menus and nine are
in use. That is a server-configuration concern, not a player one, but it is why
new features land *inside* Actions or Info rather than as new root entries.

## Map marks are your targeting system

Several actions take a position, and the way you give them one is an F10 **map
mark**:

1. Open the F10 **map** (not the radio menu).
2. Right-click where you want the thing to happen → Add mark.
3. Give it a short name (24 characters or less).
4. Back in the cockpit, `F10` → the action → your mark appears in the list.

Notes that actually matter:

- Marks are listed **per player** — you only see your own.
- Unnamed marks show up as `Mark 1`, `Mark 2`, … in menu order, so you can skip
  naming them entirely if you are in a hurry.
- Duplicate names collide. Use `CAS1`, `CAS2`, not `CAS` twice.
- Delete marks you are done with — the list is easier to fly with when it is short.
- The mark list is captured when the Actions menu is *built*. Add a mark, then
  re-open `Actions>>` to refresh it.

## Costs

Menu entries that cost points show the cost in the label:

```
E-3A AWACS(100 pts)
```

Everything else is free. Reports, the Capture Advisor, the situation briefing,
navaid listings, cargo, troops, CSAR and the tasking board cost nothing — see
[Points and Lives](../gameplay/points-and-lives.md) for what the paid items run.

## Menu-by-menu

| Menu | Page |
| --- | --- |
| GCI/EWR | [Early Warning Radar](./ewr.md) |
| Recon | [Reconnaissance](./recon.md) |
| Cargo | [Cargo Operations](./cargo.md) |
| CSAR | [Combat Search & Rescue](./csar.md) |
| Troops | [Troop Transport](./troops.md) |
| JTAC | [JTAC System](./jtac.md) |
| Actions | [Actions Menu](./actions.md) |
| Objectives | [Objectives Menu](./objectives.md) |
| Info | [Info Menu](./info.md) |

## Troubleshooting

**"The menu I want isn't there."**
Wrong airframe for that feature, or the server has it off. Check the table
above. Cargo and Troops in particular are per-airframe capabilities, not
per-player permissions.

**"The list is missing entries I know exist."**
Look for `More >>`. Then check you are not looking at a filtered list — the
Objectives menu's Base Detail only lists bases *you own*, and Capture Advisor
only lists ones you don't.

**"I picked an action and nothing happened."**
The engine answers every action with a panel message, including failures
("could not start …", "no such mark"). If you saw no message at all, you were
probably not recognised as being in a slot — leave and re-enter the slot.

**"My map mark isn't in the list."**
The menu was built before you placed it. Re-open `Actions>>`.

## See Also

- [Understanding the Menus](../getting-started/hud-and-menus.md) — chat, map, and radio menu basics
- [The Auto-Generated Briefing](../gameplay/briefing.md) — what the Situation pages contain
- [Chat Commands](../gameplay/chat-commands.md) — the things that aren't on the F10 menu
