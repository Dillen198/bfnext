# The Cockpit Overlay (In-Game UI)

The F10 radio menu works, but it makes you dig: open comms, pick a submenu, page
down, pick again, and hope you remembered which branch the thing you wanted
lives under. The **cockpit overlay** puts the same functions on a real panel
drawn on top of DCS — buttons you click, fields you type numbers into, lists you
can actually read.

It is an **optional add-on you install yourself**. The F10 menu is not going
anywhere and keeps working exactly as before, so nothing breaks if you skip
this, and nothing breaks for the people you fly with if they do.

## What's on it

**MENU** is the whole F10 menu — Actions, Cargo, Troops, JTAC, Objectives,
Info, Recon, all of it — as a clickable list with a breadcrumb trail instead
of a tree you have to keep re-opening. It is not a copy: it reads the live
menu out of the engine and clicking an item runs the exact same thing F10
runs, so it can never say something different from the real menu, and
anything added to the campaign later turns up here on its own.

Two things are better than F10:

- **No `More >>` pages.** Those exist only because DCS throws away a menu's
  eleventh entry. The panel scrolls, so forty deployables are one list.
- **You can see where you are.** The strip along the top shows your aircraft,
  altitude, heading, groundspeed, points, crates in hand and the nearest
  objective with its bearing and range — read from the engine, never typed in
  by you.

**EWR** gives BRAA calls and ground intel, **CARP** is the C-130J release-point
computer, and **CARGO** spawns crates by name and free-typed quantity — the one
thing a fixed F10 click-list can't do.

## Install

Download the overlay from the Discord, run **`bfcockpit-install.exe`**, press
**Install**, and restart DCS. That's it.

A window opens listing every DCS profile on your machine — `DCS`,
`DCS.openbeta`, anything else named `DCS*` — all ticked. Having two profiles
and installing into the wrong one is the single most common reason this
"doesn't work", so it doesn't make you choose. It also finds your Saved Games
folder where Windows actually says it is, so a Saved Games moved to another
drive or into OneDrive is still found; if yours is somewhere stranger than
that, **Add profile...** lets you point at it.

**Uninstall** is right next to it, and leaves your settings alone so
reinstalling keeps your keys, opacity and window position.

If you'd rather do it by hand, it is one file:

```
%USERPROFILE%\Saved Games\DCS\Scripts\Hooks\bfcockpit.lua
```

There is **no login, no pairing code and no account linking** — the script asks
DCS who you are on the connection you're already flying, and the server matches
that to your pilot record. It works the moment you join.

### Do I need to edit MissionScripting.lua?

**No.** You may have seen that file mentioned for other DCS mods. It sanitizes
the *mission scripting* Lua state — the one mission files run in, which is
locked down because a mission can come from a server you don't control.

The overlay doesn't run there. It runs in the GUI/hooks state, which DCS never
sanitizes. Nothing in your DCS installation folder is touched by this plugin.

## Opening it

**Press your normal Comms / radio-menu key.** That opens the overlay.

This is deliberate: it's the key your hands already go to when you want to do
campaign things, and DCS lets you rebind it in *Options → Controls → UI Layer*
like any other key, so "which key opens the cockpit UI" stays your choice.

Once the window is on screen it has its own keys:

| Key | What it does |
| --- | --- |
| `Ctrl+Alt+C` | Show / hide the overlay |
| `Ctrl+Alt+O` / `Ctrl+Alt+L` | Opacity up / down |
| `Ctrl+Alt+T` | Click-through on / off |
| `Ctrl+Alt+R` | Reload the panel |
| `Escape` | Hide |

**Click-through** is the one worth learning. With it on, the panel stays
visible but the mouse passes straight through it to the aircraft — so you can
leave a BRAA picture up on the corner of your screen while you keep flying,
instead of choosing between seeing it and using your cockpit.

The window is draggable and resizable by its edges, and where you put it,
how big you made it and how transparent you set it are all remembered for
next time.

## Using it in VR

The overlay is a normal DCS window, so it shows up in the headset the same way
the briefing and ESC menu do — and it detects VR on its own by reading your own
DCS options.

In VR it automatically:

- **Centres itself** and opens much larger, instead of tucking into a corner
  that sits outside your comfortable field of view.
- **Scales the whole panel up** and enlarges every button, field and scrollbar,
  because a headset cursor lands *near* a target far more often than on it.

If it's still too small or now too large, set `scale` in the settings file
below — `1.6` and `2.0` are reasonable for dense headsets.

## Settings

The first time it runs, the script writes:

```
%USERPROFILE%\Saved Games\DCS\Config\BFCockpit.lua
```

Edit **that** file, not the script — that way updating the script never wipes
your setup. Everything is optional; anything you leave out keeps its default.

```lua
cockpit = {
    -- The campaign API. Only change this if you're told to.
    url = "https://api.vectorstrike.org/cockpit",

    -- 0.15 (ghost) .. 1.0 (solid). Ctrl+Alt+O / L change it live.
    opacity = 0.92,

    -- Open the panel when you open the comms menu.
    open_on_radio_menu = true,

    -- Have it up from mission start.
    open_on_start = false,

    -- "auto" reads VR from your own DCS options; "on" / "off" force it.
    vr = "auto",

    -- UI magnification. Leave unset for 1.0 flat / 1.35 in VR.
    scale = nil,

    hotkeys = {
        toggle        = "ctrl+alt+c",
        hide          = "escape",
        opacity_up    = "ctrl+alt+o",
        opacity_down  = "ctrl+alt+l",
        click_through = "ctrl+alt+t",
        reload        = "ctrl+alt+r",
    },
}
```

## Flying on more than one of our servers

Nothing to do. The overlay tells the campaign which server you're actually
connected to, so joining the test server shows you the test server's war and
joining the main server shows you the main one. The server's name is printed
along the top of the panel so you can see at a glance which picture you're
reading.

If it ever shows the wrong one, pin it by adding the server's short id to your
settings file:

```lua
cockpit = { instance = "vs1" }
```

## If it doesn't appear

Everything the script does is logged. Open:

```
%USERPROFILE%\Saved Games\DCS\Logs\dcs.log
```

and search for `BFCOCKPIT`. You'll see whether it loaded, which settings it
read, whether VR was detected, and the exact address it tried to open. Bring
those lines to the Discord and they'll tell us immediately what went wrong.

Common causes:

- **Nothing in the log at all** — the file isn't in `Scripts\Hooks`, or it's
  named something DCS skips. It must end in `.lua`.
- **Window opens but stays blank** — your DCS can't reach the campaign API.
  Check the `loading ...` line in the log and try that address in a browser.
- **Opens on the wrong server's data** — set `instance` as above.
