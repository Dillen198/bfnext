# Understanding the Menus

Fowl Engine provides several ways to interact with the campaign system. This page covers the essential interface elements you'll use constantly.

![Cockpit kneeboard page showing campaign status and objective information](/api/wiki/images/ed912e81-5a74-493c-a691-2bfff1541054)

## Chat System

### In-Game Chat
The chat system is your primary way to issue commands and communicate.

**Opening Chat**:
- Press `Shift+Tab` to open chat
- Type your command
- Press Enter to send

**Chat Visibility**:
- Commands starting with `-` are processed by the system
- Most commands are only visible to you
- Regular chat messages are visible to your team

### Command Prefix
Almost all system commands start with a dash `-`:
```
-help
-lives
-time
-balance
```

See the [Chat Commands](../gameplay/chat-commands.md) section for a complete list.

## F10 Map Menu

The F10 map is your tactical hub in DCS, and Fowl Engine extends it with powerful new features.

### Accessing the F10 Map
1. Press `F10` to open the map
2. You'll see standard DCS map features plus Fowl Engine additions
3. Right-click to access context menus

### Map Markers
Fowl Engine adds several types of markers:

**Objective Markers**:
- Show ownership (Blue/Red)
- Display health and logistics status
- Indicate supply levels

**JTAC Markers**:
- Show active JTAC targets
- Include laser codes for precision strikes
- Updated in real-time

**Player Markers**:
- Your F10 marks become menu targets
- Used for spawning units and waypoints
- Limited to 24 characters for menu display

## F10 Radio Menu

The F10 radio menu (not to be confused with the map) provides access to coalition commands while in your aircraft.

### Main Menu Categories

Fowl Engine adds up to nine top-level menus:

1. **GCI/EWR** – radar picture, ground intel, GCI voice settings *(always)*
2. **Recon** – fly a recon pass *(recon-tagged airframes)*
3. **Cargo** / **C-130 Cargo** – crates and base supply *(cargo-capable airframes)*
4. **CSAR** – downed pilots *(server-enabled)*
5. **Troops** – load/unload infantry *(troop-capable airframes)*
6. **JTAC>>** – targeting, laser codes, fire missions
7. **Actions>>** – support aircraft, tasking board, AI helo missions
8. **Objectives** – base reports and the Capture Advisor *(always)*
9. **Info** – your status, the situation briefing, navaids, weather, help *(always)*

Each is context-sensitive and only appears if your aircraft has the capability
and the server has the feature enabled.

**`JTAC>>` and `Actions>>` are buttons, not folders.** They start as a single
command; select it once and it builds itself into the full menu from the world
as it is right now. That is also how you refresh a stale list — re-open
`Actions>>` after placing a new map mark.

The full tree is documented in
[F10 Menu Overview](../f10-menu/overview.md).

## F10 Map vs F10 Radio Menu

It's important to understand the difference:

| F10 Map | F10 Radio Menu |
|---------|----------------|
| Shows battlefield overview | In-aircraft commands |
| Always accessible | Only in slotted aircraft |
| Place markers | Execute actions |
| Strategic planning | Tactical execution |

**Common workflow**:
1. Open **F10 Map** to plan
2. Place markers at target locations
3. Slot into aircraft
4. Open **F10 Radio Menu** → Actions
5. Select action linked to your map marker

## Menu Navigation Tips

### Using Number Keys
- Each menu item has a number (1-9)
- Press the number to select instantly
- Faster than mouse navigation in combat

### Menu Structure
Menus are hierarchical:
```
F10
├── Actions>>              (select once to build it)
│   └── Actions
│       ├── E-3A AWACS(100 pts)
│       │   └── [your map marks]
│       ├── Add Task
│       └── More >>
├── JTAC>>
└── GCI/EWR
    ├── Report
    └── Toggle
```

### `More >>` — the thing that trips people up
**DCS silently drops anything past the tenth entry in a menu.** No error, no
warning. Fowl Engine works around it by spending the last slot on **`More >>`**,
which opens the next page of the same list.

So if something you expect is not in a list, **look for `More >>` at the bottom
before assuming it isn't there.**

Long base lists are chunked differently — into alphabetical submenus like
`1. Abu Su - Damascu`. Pick the range your base falls in.

## On-Screen Messages

Fowl Engine sends you messages through the DCS message system:

**Message Types**:
- **White text**: Information and confirmations
- **Command responses**: Results of your actions
- **System announcements**: Team-wide notifications

**Message Duration**:
- Most messages display for 5-10 seconds
- Critical messages may persist longer
- Check chat log to review missed messages

## Accessibility Options

### Units System
Toggle between Imperial and Metric units:
- F10 → GCI/EWR → "Units to Imperial" / "Units to Metric"
- F10 → GCI/EWR → GCI Voice → "Units: Imperial / Metric / Server Default"
  (for the spoken calls), or `-gci imperial` / `-gci metric` in chat

## Quick Reference

**Must-know commands**:
- `-help` — command list
- `-status` — campaign status: points, objectives held, your convoys
- `-brief` — the short situational briefing
- `-balance` — check points
- `-weather` — weather at your position

**Must-know menus**:
- `F10 → Info → Situation` — the full six-page briefing
- `F10 → Objectives → Capture Advisor: Nearest` — why that base won't flip
- `F10 → Info → Support & Radios` — is there a tanker up, and on what frequency

## See Also

- [F10 Menu Overview](../f10-menu/overview.md) — every menu, in detail
- [Core Gameplay](../gameplay/objectives.md) — campaign mechanics
- [Chat Commands](../gameplay/chat-commands.md)
