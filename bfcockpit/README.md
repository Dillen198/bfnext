# BFNext Cockpit Overlay

An in-game panel for the Fowl Engine campaign — the F10 radio menu as
something you click instead of something you dig through, drawn on top of DCS
by the sim's own GUI layer. Works in VR.

This is a **client-side DCS plugin**. You install it; the server doesn't, and
can't — DCS's mission-scripting state lives on the server and has no route to
a connecting player's screen. A script in your own `Scripts\Hooks` folder does.

## Install

Run **`bfcockpit-install.exe`**, press **Install**, restart DCS. That's it.

A window opens listing every DCS profile it found on your machine — `DCS`,
`DCS.openbeta`, a server profile, anything else named `DCS*`. They all start
ticked, because having more than one profile and installing into only one of
them is the usual reason the overlay "doesn't work". Untick any you don't want.

It looks for your Saved Games folder where Windows actually says it is, so a
Saved Games moved onto another drive or into OneDrive is still found. If yours
is somewhere it can't guess — you launch DCS with `--write-dir`, say — use
**Add profile...** and point at the folder with `Config` and `Logs` in it.

**Uninstall** is the button next to it. Your settings are left in place, so
reinstalling keeps your keys, opacity and window position.

The overlay is embedded in the exe: nothing to unzip, nothing to keep next to
it, no runtime to install, and no DCS game file touched.

### Scripted installs

`bfcockpit-install-cli.exe` is the same thing without the window — for rolling
it out across machines, or for seeing exactly where it looked when the window
found nothing.

```
bfcockpit-install-cli.exe --list                        just show what it finds
bfcockpit-install-cli.exe --yes                         don't ask, do all of them
bfcockpit-install-cli.exe --write-dir "D:\DCS profile"   a profile in an odd place
bfcockpit-install-cli.exe --search   "D:\Saved Games"    another place to look
bfcockpit-install-cli.exe --url http://192.168.1.50:8880/cockpit
bfcockpit-install-cli.exe --uninstall
```

`--write-dir` wants the folder DCS *writes* to — the one with `Config` and
`Logs` in it — not the folder DCS is installed in.

### Installing by hand, or with a mod manager

The `Scripts` folder next to the exe mirrors a DCS profile, so this package is
a valid OvGME / mod-manager mod as-is. By hand it is one file:

```
bfcockpit\Scripts\Hooks\bfcockpit.lua
      ->  %USERPROFILE%\Saved Games\DCS\Scripts\Hooks\bfcockpit.lua
```

## Do I need to change MissionScripting.lua?

**No.** That file sanitizes the *mission scripting* Lua state, which is where
mission `.miz` code runs — and mission code can come from a server, which is
why DCS strips `io`, `lfs`, `require` and `package` from it.

The overlay does not run there. It runs in the GUI/hooks state, which DCS never
sanitizes, and that is exactly why it can use `require('dxgui')` to draw a
window and `lfs` to read your settings. Nothing in your DCS installation
folder is modified by this plugin, `MissionScripting.lua` included.

(Server admins: your *server* does need the sanitize block removed, because the
campaign engine is loaded with `require("bflib")` from the mission. That is a
separate, pre-existing server-side requirement and has nothing to do with this
plugin.)

## Using it

Press your normal **Comms / radio-menu key**. That opens the panel.

DCS gives a hooks script no global keyboard hook of its own, and the UI-layer
input profile has no user-definable command — so rather than invent a key that
can't exist, the overlay rides the one key DCS does report to it. That key is
already yours to rebind in *Options → Controls → UI Layer*, and it's the key
your hand goes to for campaign things anyway.

Once the window is up:

| Key | |
| --- | --- |
| `Ctrl+Alt+C` | Show / hide |
| `Ctrl+Alt+O` / `Ctrl+Alt+L` | Opacity up / down |
| `Ctrl+Alt+T` | Click-through on / off |
| `Ctrl+Alt+R` | Reload |
| `Escape` | Hide |

**Click-through** leaves the panel on screen while the mouse passes through it
to the aircraft — a BRAA picture in the corner while you keep flying.

Drag and resize the window by its edges. Position, size and opacity are
remembered.

## Settings

First run writes `%USERPROFILE%\Saved Games\DCS\Config\BFCockpit.lua`. Edit
that, not the script, so an update never wipes your setup. Every key is
optional and documented in the header of `bfcockpit.lua`.

```lua
cockpit = {
    url                = "https://api.vectorstrike.org/cockpit",
    opacity            = 0.92,
    open_on_radio_menu = true,
    open_on_start      = false,
    vr                 = "auto",   -- "on" / "off" to force
    scale              = nil,      -- UI magnification; 1.35 in VR by default
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

## VR

Detected automatically from your own DCS options. The window centres itself,
opens much larger, and the panel is rendered with bigger type and hit targets,
because a headset cursor lands near a target more often than on it. Turn
`scale` up if your headset wants more.

## Several servers

Nothing to configure. The overlay tells the campaign which server you're on,
and the server's name is printed across the top of the panel so you can see
which war you're looking at.

## If it doesn't show up

Everything it does is logged. Open

```
%USERPROFILE%\Saved Games\DCS\Logs\dcs.log
```

and search for `BFCOCKPIT`. You'll see the version that loaded, the settings it
read, whether VR was detected, and the exact address it tried to open. Bring
those lines to the Discord.

- **Nothing in the log** — the file isn't in `Scripts\Hooks`. Re-run the
  installer.
- **Window opens but stays blank** — your DCS can't reach the campaign API.
  Check the `loading ...` line and try that address in a browser.

## What it touches

Only `<DCS profile>\Scripts\Hooks\bfcockpit.lua` and, if you don't already have
one, `<DCS profile>\Config\BFCockpit.lua`. No DCS install files are modified,
nothing is written to the registry, and nothing is added to startup.
