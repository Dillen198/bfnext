# Vector Strike Cockpit: In-Game Panel & Wheel

The F10 radio menu works, but it makes you dig: open comms, pick a submenu, page
down, pick again, and the answer flashes in the corner of the screen for a few
seconds. The **Vector Strike cockpit** gives you two better ways in, both drawn
inside DCS:

- the **panel** — the whole F10 menu, your cargo, status, EWR picture, every
  message the server sends you and this wiki, on a window you can read;
- the **wheel** — a ring you point at with your head or the mouse, for doing
  things in a second without looking away from flying.

It is an **optional add-on you install yourself**. The F10 menu keeps working
exactly as before, so nothing breaks if you skip it, and nothing changes for the
people you fly with.

*The pictures on this page show the real layout and colours with example data.*

## Install

**[Download the installer](/api/cockpit/plugin/installer)**
(`VectorStrike-Cockpit-Install.exe`, always the current version). Close DCS,
run it, press **Install**, and start DCS again.

Every version is also on the project's
[GitHub releases](https://github.com/Dillen198/bfnext/releases?q=cockpit-v&expanded=true),
with a `.zip` for installing by hand or with a mod manager. Windows may warn
that the installer is from an unknown publisher (it isn't code-signed): choose
**More info → Run anyway**.

The installer finds every DCS profile on your machine (`DCS`, `DCS.openbeta`,
anything else named `DCS*`) and ticks them all — installing into the wrong one
is the most common reason it "doesn't work". A Saved Games folder moved to
another drive or into OneDrive is still found; if yours is somewhere stranger,
**Add profile...** lets you point at it. **Uninstall** is next to it and keeps
your settings.

**Updates install themselves** (from version 3.2). When the server has a
newer version, the panel says *"Version … is downloaded — it installs when you
close DCS"*: it has fetched the installer in the background, and the next time
you start DCS you're on the new version. Nothing to click. If you'd rather
update by hand, untick **Update automatically** in the
[settings](#settings); the panel then tells you when there's a new version.
If you're on 3.1, run the installer once more to get this.

There is **no login, no pairing code and no account linking** — it knows who you
are from the connection you're already flying on. You do **not** need to edit
`MissionScripting.lua`, and nothing in your DCS installation folder is touched.

## Opening it

| Key | What it does |
| --- | --- |
| your **Comms / radio-menu** key | opens the panel |
| `Ctrl+Alt+C` | shows / hides the panel, from anywhere |
| `Ctrl+Alt+W` | opens the wheel |

All three can be changed in **Options → Special → Vector Strike** (see
[Settings](#settings)). Changes apply within a few seconds, even mid-mission.

## The panel

Every tab shares the same frame: the **tabs** across the top (MENU, CARGO,
STATUS, EWR, LOG, WIKI, WHEEL), **-** / **+** to
make the panel more see-through or more solid, **X** to hide it, and a **status
line** at the bottom that shows the result of what you just did (or your keys).
Drag the panel by its title bar and resize it from its edges; its position,
size and opacity are remembered.

### MENU — your whole F10 menu

![The MENU tab inside the Cargo submenu, with the server's answer at the top](/api/wiki/images/2ca24e67-39ef-4941-8713-3195728bd999)

- Every F10 item for your slot, read live from the server: Actions, Cargo,
  Troops, JTAC, Objectives, Info, Recon — and anything added later.
- **Bright green rows ending in `>`** are submenus: click to go in. **< Back**
  goes up one level; the trail next to it shows where you are.
- **Plain rows** are commands: click to run them — exactly what F10 runs.
- **The box at the top** is what the server answered ("SA-3 Launcher crate
  spawned. 3 more needed…", "you must land to spawn crates"), so you don't
  have to catch it in the corner of the screen.
- **☆ / ★** pins a command to your [wheel](#the-wheel) (★ means it's pinned).
- No `More >>` pages: long lists just scroll.

Here is the MENU tab inside **Cargo** in a Huey (on the web you can click the
rows; the replies are the server's real wording):

```cockpitpanel
{"tab":"MENU","crumb":"F10 > Cargo","rows":[
 {"text":"Hawk Launcher crate loaded","style":"answer"},
 {"button":"Unpack Nearby Crate(s) ☆","says":"KillerDog unpacked a Hawk System"},
 {"button":"Load Nearby Crate ☆","says":"Hawk Launcher crate loaded"},
 {"button":"Unload Crate ☆","says":"Hawk Launcher crate unloaded"},
 {"button":"List Nearby Crates ☆","says":"Hawk Launcher crate, bearing 271, 32 meters away"},
 {"button":"List Cargo ☆","says":"Current Cargo  ·  crates: 1 of 1  ·  Hawk Launcher crate weighing 1000 kg"},
 {"button":"Destroy Nearby Crate ☆"},
 {"button":"Crates >","sub":true},
 {"button":"CSAR >","sub":true}
],"status":"Clicked Load Nearby Crate"}
```

The full Cargo tree and every reply it can give are on
[Cargo Operations](../f10-menu/cargo.md); the other menus have their own pages
under [F10 Menu Systems](../f10-menu/overview.md).

### CARGO — what to carry, and how much of it

![The CARGO tab: on board, nearby crates, quick actions and categories](/api/wiki/images/56eaa9cf-0803-4eae-9226-802ad10c2eeb)

- **ON BOARD** — crates and troops you're carrying, against how many your
  aircraft can take.
- **NEARBY CRATES** — crates close enough to load or unpack, with range and
  bearing.
- **Quick buttons** — Unpack, Load, Unload, List Cargo, Destroy: the same F10
  commands, one click away. Only the ones your aircraft has are shown.
- **BASE SUPPLY** — repair kits and fuel / weapons transfer crates.
- **BUILD SOMETHING** — every deployable, grouped by category. Pick one:

![A cargo category: each deployable with the crates it needs, weights and cost](/api/wiki/images/de33e85d-f738-4cb7-b515-9fe24e15592e)

- Each deployable shows its **points cost** and **how many crates of each kind
  it needs in all** — the number you need to bring and unpack together.
- Each crate shows **how many are needed** and **what one weighs**.
- **SPAWN 1** spawns one crate; **SPAWN ALL n** spawns the full set at once
  (where your aircraft can do that). They are the F10 commands, so the same
  rules apply — land first, spawn at a friendly base — and if something stops
  you, the reason shows up in the panel.

### STATUS — who and where you are

![The STATUS tab: pilot, points, lives, aircraft, position and nearest objective](/api/wiki/images/9171f745-71d3-44e9-93fc-4c91f11aa070)

Your name, side, **points**, **lives** and crates in hand; then your aircraft,
**position** (degrees and decimal minutes, ready for a waypoint), altitude,
heading and speed, airborne or on the ground, your **home base**, the base
you're parked at, the **nearest objective** with range and bearing, and any
**takeoff hold** still counting down.

### EWR — the radar picture

![The EWR tab showing threats with BRAA calls](/api/wiki/images/9e5839e7-b4cc-46a0-a092-497663354263)

- **THREATS / FRIENDLIES / GROUND INTEL** — the early-warning network's
  picture of enemy aircraft, your own side, or known enemy ground units.
- **EWR CALLS ON/OFF** — turns the automatic EWR radio calls on or off for you.
- **IMPERIAL / METRIC** — the units those calls and this picture use.

### LOG — everything the server told you

![The LOG tab: messages newest first, tagged YOU, SIDE and ALL](/api/wiki/images/05732d53-90e8-402b-9e50-f333720a706b)

Every message sent to you, newest first, tagged **YOU** (just you), **SIDE**
(your coalition), **ALL** (everyone) or **CHAT**. When something arrives while
you're on another tab, the tab shows a count — **LOG 3**.

### WIKI — this wiki, in the cockpit

The **WIKI** tab is this wiki (the same pages as wiki.vectorstrike.org),
inside DCS, so you can look something up on the ramp or in a hover without
leaving the sim. It works in single player too.

- **Sections, then pages.** The tab opens on the wiki's sections. Pick a
  section, then a page. **< Back** and the trail at the top
  (`WIKI > Section > Title`) work as they do in MENU.
- **Search.** The box at the top filters page titles as you type, then lists
  the pages that mention your words. While you type, the keyboard is the
  panel's: click outside the box, or switch tab, to give it back to the
  cockpit.
- **Links are buttons.** A link to another page appears as a button under its
  paragraph. **NEXT** at the bottom opens the following page in the section.
- **The numbers are your server's.** Points, timings and costs are the ones
  the server you're flying on uses, the same as the web wiki shows for it.
- **Pictures** load after a moment and are then kept on your PC, in
  `Saved Games\DCS\Mods\Services\BFCockpit\cache\wiki\` (safe to delete; they
  are downloaded again when needed). Screenshots and the drawn diagrams both
  show as pictures. Pictures need the 3.5 installer: on an older install you
  see their captions instead.
- **Mockups show as text.** The interactive mockups on the web can't be
  clicked in DCS, so they are written out: an F10 menu replica becomes an
  indented tree with each command's answer under it, a walkthrough becomes a
  numbered list, and a panel or wheel replica becomes its rows and labels.
  Every page is written to make sense that way; for the clickable version, open
  the same page on the web.
- **On the wheel.** Add the WIKI tab to your wheel with **+ PANEL** in the
  [WHEEL editor](#building-your-own-wheel).

The panel is at least 500 pixels wide (at 100% panel size) so pages stay
readable.

A page open in the WIKI tab looks like this (an illustration: the rows depend
on the page):

```cockpitpanel
{"tab":"WIKI","crumb":"WIKI > F10 Menu Systems > Actions Menu","rows":[
 {"text":"Search the wiki...","style":"dim"},
 {"heading":"Actions Menu"},
 {"text":"Actions>> starts as a single command. Select it once and it rebuilds itself into the full Actions submenu, built from the world as it is right now."},
 {"button":"F10 Menu Overview","says":"F10 Menu Overview"},
 {"text":"[picture: the Actions menu open in the cockpit]","style":"dim"},
 {"text":"F10 Other\n  Actions\n    AWACS(100 pts)\n      Mark 1 310/38km\n        -> action AWACS started","style":"mono"},
 {"gap":true},
 {"button":"NEXT: JTAC System","says":"JTAC System"}
],"status":"Actions Menu"}
```

### WHEEL — build your own wheel

See [Building your own wheel](#building-your-own-wheel) below.

## The wheel

The wheel is a ring of buttons you **point at** instead of clicking through
menus — like Flow for MSFS. Tap **Ctrl+Alt+W** and it opens in front of you.

![The wheel: CARGO is pointed at, its commands fan out on an arc, UNLOAD is picked](/api/wiki/images/cb4d087b-8805-487e-a4ff-e1a95386dc24)

**Pointing.** Three ways, all at once — whichever you used last wins:

- **Your head** (TrackIR or VR): where you were looking when the wheel opened
  is the middle; turn your head toward a segment to light it up. A **dot**
  shows where you are pointing. Turning the aircraft doesn't move it.
- **The mouse** (or a VR controller's laser): hover a segment.
- **Keys or HOTAS buttons only** — no mouse, no head; see
  [Keys only](#keys-only-no-mouse-no-head-tracking) below.

**Groups fan out.** Point at a group — an F10 menu like **Cargo**, marked with
a small dot near its edge — and its commands appear on an **arc** just outside
it. Push further out (turn your head a bit more, or move the mouse out) onto
the arc to pick one. No clicking into submenus.

**Firing.** **Click**, or **tap the wheel key again**, to run what's lit up.
Pick a group itself to open it as the whole ring (handy for long lists). The
**middle** shows what you're pointing at; clicking it goes back a level, or
closes the wheel at the top. Tapping the key with nothing lit up closes it too.

**Feedback.** The server's answer appears in the strip under the wheel.
Anything that destroys or deletes is drawn in red and has to be **picked
twice**.

### Keys only (no mouse, no head tracking)

Everything the wheel does can be done from keys — ideal in VR, or with a hat
switch on your stick. These keys are **only taken while the wheel is open**, so
they never interfere with flying.

| Arrow set (default) | Numpad set | What it does |
| --- | --- | --- |
| **← / →** | **4 / 6** | step to the previous / next segment (or along the arc) |
| **↑** | **8** | move **out** onto the highlighted group's arc |
| **↓** | **2** | come back **in** from the arc to the ring |
| **Enter** (or tap the wheel key) | **5** | **fire** what's highlighted — or open a group as the whole ring |
| **Backspace** | **0** | **back** a level, or close the wheel at the top |

A third set uses **Ctrl+Alt + the arrows / Enter / Backspace**, for when the
plain arrows are bound in your aircraft. Pick the set (or turn it off) with
**Wheel keys** in the [settings](#settings). The middle of the wheel shows the
keys while you use them.

**On a HOTAS:** use your stick's software (VKB, Virpil, Thrustmaster TARGET,
Joystick Gremlin…) to map a 4-way hat to the four arrows, one button to
**Enter**, one to **Backspace** and one to the **wheel key**. Then: wheel key
to open, hat to move, button to fire — without letting go of the stick.

While you're using keys, small head movements are ignored, so a VR headset
can't steal your selection; turn your head clearly (a few degrees) to hand
pointing back to the head.

### Live info

![A live-info slot pointed at: the middle shows the nearest objective in full](/api/wiki/images/9d90d6b9-13b8-4052-8684-12d5c5605742)

Some segments show a value instead of running a command. Point at one and the
middle spells it out; pick it to open the panel where it comes from.

| Slot | Shows | Picking it opens |
| --- | --- | --- |
| **Nearest objective** | range and bearing, e.g. `12NM 012°` | STATUS |
| **Crates aboard** | crates carried against room, e.g. `1/2 CRATES` | CARGO |
| **Takeoff** | `CLEARED`, or the time left on a takeoff hold | STATUS |
| **Points** | your points | STATUS |
| **Last message** | the latest thing the server told you | LOG |

### What's on it to start with

Until you change it, each aircraft type gets a preset, after any commands you
pinned with ☆. A slot whose F10 menu has **Cargo** or **C-130 Cargo** gets the
transport set: **Cargo** (or **C-130 Cargo**), **Troops**, **crates aboard**,
**Objectives**, **Actions**, the **nearest objective** and a shortcut to the
**F10 MENU** tab. Everything else gets the combat set: **JTAC**, **GCI/EWR**,
the **EWR** tab, the **nearest objective**, **Objectives**, **Actions**,
**Flight** and **F10 MENU**. Menus your slot doesn't have are left out.

The combat preset, as a fast jet sees it (on the web, point at a group to fan
it out; the replies are the server's real wording):

```wheel
{"theme":"green","title":"VECTOR STRIKE","subtitle":"combat preset","items":[
 {"label":"JTAC","icon":"jtac","items":[
  {"label":"Refresh Locations"},
  {"label":"Kobuleti (2 JTAC)>>"},
  {"label":"Gori (1 JTAC)>>"}]},
 {"label":"GCI/EWR","icon":"ewr","items":[
  {"label":"Report","says":"Bandits BRAA\n   BRG°M      RNG      ALT      SPD      HDG°M      AGE  ASPECT    SRC   TYPE\n     42     38nm    A22.0    500kts     222      6s HOT       [GA]   Flanker"},
  {"label":"Toggle","says":"ewr reports are disabled"},
  {"label":"Friendly Report"},
  {"label":"Ground Intel"}]},
 {"label":"EWR","icon":"ewr","says":"opens the panel's EWR tab"},
 {"label":"12NM 012°","icon":"compass","says":"Gori (Red) 12.0 nm @ 012°"},
 {"label":"Objectives","icon":"objective","items":[
  {"label":"Nearest Base (detail)"},
  {"label":"Capturable / Contested"},
  {"label":"Capture Advisor: Nearest"},
  {"label":"Bases Under Threat"}]},
 {"label":"Actions","icon":"actions"},
 {"label":"Flight","icon":"flight"},
 {"label":"F10 MENU","icon":"panel","says":"opens the panel's MENU tab"}
]}
```

The exact replies of each menu are on its own page: [JTAC](../f10-menu/jtac.md),
[GCI / EWR](../f10-menu/ewr.md), [Objectives](../f10-menu/objectives.md),
[Actions](../f10-menu/actions.md).

![The wheel in the blue colour scheme, as set up for a jet](/api/wiki/images/f7e01aaf-8460-48c5-a6dc-81ab6f36fb17)

### Building your own wheel

![The WHEEL tab: the wheel's items with move, icon and remove buttons, and the add buttons](/api/wiki/images/e260230b-1070-4e58-896a-cad801e27529)

The panel's **WHEEL** tab edits the wheel for the aircraft type you're sitting
in — each type keeps its own.

| Button | What it does |
| --- | --- |
| **▲ ▼** | move an item round the ring (or within its group) |
| **ICON** | change the item's icon (click again for the next one) |
| **RENAME** | rename one of your groups |
| **→ GROUP** | move a pinned command into your group |
| **OUT** | take an item back out of its group |
| **✕** | remove it from the wheel |
| **RESET TO DEFAULT** | throw your changes away and go back to the preset |
| **+ F10 MENU** | add any of your F10 menus as a group that fans out |
| **+ LIVE INFO** | add a live-info slot |
| **+ PANEL** | add a shortcut that opens a panel tab |
| **+ NEW GROUP** | add your own group to collect pinned commands in |

To put a **single command** on the wheel, use the **☆** next to it in the MENU
tab. The ring holds up to **10** items; anything beyond that sits behind
**MORE**.

### Icons

![The Vector Strike icon set](/api/wiki/images/da866270-2d36-4ebb-8582-49420aaa561a)

Commands get an icon from what they do — crates, troops, CSAR, JTAC, smoke,
objectives and so on. Change any of them with **ICON** in the WHEEL tab.

## Settings

![Options, Special, Vector Strike: every setting numbered](/api/wiki/images/e6ad2b68-688a-4002-8be6-02f83031c5fb)

Everything is in **Options → Special → Vector Strike**, and changes apply
within a few seconds, even in the middle of a mission.

| # | Setting | Default | What it's for |
| --- | --- | --- | --- |
| 1 | **Show / hide key** | Ctrl+Alt+C | The key that opens and closes the panel from anywhere. Pick any letter, digit, F1–F12, numpad digit, Insert, Delete, Home, End or Pause, with any of Ctrl / Alt / Shift. Choose a combination none of your aircraft use. |
| 2 | **Also open with the Comms key** | on | The panel also opens when you open the radio menu, so the key you already use for campaign things gets you there. |
| 3 | **Update automatically** | on | When the server has a newer version, it downloads in the background and installs when you close DCS. Untick it to update by hand; the panel then tells you when there's a new version. |
| 4 | **Panel size** | Auto | How big the whole panel is: the window, its buttons and its text, from 60% to 200%. On a small screen or a laptop, try 70–80%. Auto picks 80% on screens under 820 pixels tall, 90% under 1000, 100% on anything bigger and 135% in VR. |
| 5 | **Text size** | 100% | Just the writing, from 70% to 130%, without changing the layout. Make it smaller to fit more on a small panel, or bigger if it's hard to read. |
| 6 | **Wheel key** | Ctrl+Alt+W | The key that opens the wheel, and (tapped again) fires what you're pointing at. |
| 7 | **Point with head tracking** | on | Turn off if you only want the mouse to point, e.g. if your head moves the pointer when you don't mean it to. |
| 8 | **Head turn to reach an item** | Medium (10°) | How far you turn your head to reach the ring; the outer arc is twice that. Short suits TrackIR with high curves, Long suits VR or relaxed head movement. |
| 9 | **Fire by looking at an item** | Off | Fire an item just by looking at it for 0.8–2 seconds — fully hands-free. Off by default so a glance never fires anything. |
| 10 | **Wheel size** | 100% | How big the wheel is (60–150%). On a small screen, try 60–70%. |
| 11 | **Wheel colour** | Vector Strike green | Green, blue, amber, red or white. |
| 12 | **Wheel opacity** | 90% | How see-through the wheel's segments are. |
| 13 | **Open the wheel at** | Centre of the screen | Or **where the mouse is**. In VR it always opens where you're looking. |
| 14 | **Wheel keys (while it is open)** | Arrow keys, Enter, Backspace | Drive the wheel with keys only — see [Keys only](#keys-only-no-mouse-no-head-tracking). Choose the arrows, the numpad, Ctrl+Alt + arrows, or Off. |

**HOTAS buttons:** DCS doesn't let add-ons add their own entry to the
**Controls** page, so to open the panel or wheel — or drive the wheel — from
your stick, map buttons and a hat to the keys with your stick's software (VKB,
Virpil, Thrustmaster TARGET, Joystick Gremlin…).

A few rarer settings live in a file the cockpit writes the first time it runs,
`%USERPROFILE%\Saved Games\DCS\Config\BFCockpit.lua`:

```lua
cockpit = {
    -- The campaign API. Only change this if you're told to.
    api = "https://api.vectorstrike.org",

    -- Pin to one server if it ever shows the wrong one's war, e.g. "vs1".
    instance = nil,

    -- "auto" reads VR from your own DCS options; "on" / "off" force it.
    vr = "auto",
}
```

## Using it in VR

Both the panel and the wheel show up in the headset like DCS's own menus, and
VR is detected from your own DCS options:

- the panel and wheel are drawn **larger** — a headset cursor lands *near* a
  target far more often than on it;
- the wheel always opens **in the middle of your view**;
- **head pointing** works with the headset exactly as with TrackIR — and with
  **Fire by looking at an item** turned on, you can use the wheel without
  touching anything;
- or skip pointing altogether and drive it with **keys or a HOTAS hat** (see
  [Keys only](#keys-only-no-mouse-no-head-tracking)) — the most reliable way
  in a headset;
- a VR controller's **laser** works like the mouse.

If things are too small or too large, change **Panel size** (150–200% suits
dense headsets) and **Wheel size**.

## Flying on more than one of our servers

Nothing to do. The cockpit tells the campaign which server you're on, so the
test server shows the test server's war and the main server the main one. If it
ever shows the wrong one, set `instance` in the settings file as above.

## If it doesn't work

The line along the bottom of the panel says what went wrong in plain words.
Everything is also logged — open
`%USERPROFILE%\Saved Games\DCS\Logs\dcs.log` and search for `BFCOCKPIT`. Bring
those lines to the Discord.

- **No "Vector Strike" under Options → Special** — DCS only looks for add-ons
  when it starts: quit DCS completely and start it again.
- **Nothing happens and nothing is in the log** — it isn't installed in the
  profile DCS is using. Re-run the installer.
- **"bfcockpit_net.dll is missing"** or **"the wheel's art is missing"** —
  re-run the installer with DCS closed.
- **"can't reach the campaign server"** — the message says why. Usually a
  firewall, or a wrong `api` in the settings file.
- **"not supported by this server yet"** — that server hasn't been updated to
  the version the CARGO and LOG tabs need yet. Everything else still works.
- **The WIKI tab shows captions instead of pictures** — pictures need the 3.5
  installer. Run it again with DCS closed.
- **The keyboard stopped working in the cockpit** — the WIKI search box still
  has it. Click outside the box, or switch tab.
