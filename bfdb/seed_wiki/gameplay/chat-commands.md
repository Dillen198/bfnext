# Chat Commands

Chat commands are your primary interface for interacting with the Fowl Engine system. This page covers the most commonly used commands.

## Command Format

All system commands start with a dash `-`:
```
-command [arguments]
```

**Important**:
- Commands are **case-insensitive**
- Spaces matter in arguments
- Most commands are silent (only you see response)

## Essential Commands

### Registration & Status

#### `blue` / `red`
Register for a team (first-time only).

```
blue
```
```
red
```

**Response**:
```
Welcome to the Blue team. You may only occupy slots
belonging to your team. Good luck!
```

**Usage**:
- First time joining server
- Must be in spectator mode
- Permanent choice (unless server allows switching)

---

#### `-switch blue` / `-switch red`
Switch teams (if allowed by server).

```
-switch blue
```
```
-switch red
```

**Requirements**:
- Must be in spectator mode
- Must have side switches remaining
- Server must allow switching

**Response**:
```
[Your name] has switched to Blue
```

---

#### `-lives`
Check your current status.

```
-lives
```

**Response**:
```
Team: Blue
Lives: 5
Points: 1250
Side Switches: 1
```

Shows:
- Current team
- Remaining lives, per role
- Points balance
- Available side switches

**Note**: lives are a campaign setting and are **switched off on the live
mission** — no life is taken on takeoff or death. When a server runs with lives
off, there is nothing for this command to count. See
[Points and Lives](./points-and-lives.md).

---

#### `-balance`
Check your points balance.

```
-balance
```

**Response**:
```
You have 1250 points
```

Quick way to check points without full status.

---

### Time & Server

#### `-time`
Check server restart time.

```
-time
```

**Response**:
```
The server will shutdown in 02:45:30
```

Shows time remaining until:
- Automatic server restart
- Campaign save
- Scheduled maintenance

If no restart scheduled:
```
The server isn't configured to restart automatically
```

---

#### `-status`
Campaign status at a glance — yours and the war's.

```
-status
```

**Response**:
```
=== CAMPAIGN STATUS ===
Side: Blue | Points: 1250 | Streak: 2 | Career Kills: 41
Objectives — Blue: 17 | Red: 14
Active Blue Convoys: 3
```

The objective count is the scoreboard that actually matters, and the convoy
count tells you whether your supply system is moving. Details of what's in
transit: `F10 → Info → Supply Convoys`.

---

#### `-weather`
Weather brief for where you are.

```
-weather
```

On the live mission weather is pulled from real-world METAR, so it changes
between rounds. Worth checking before a low-level run or a carrier recovery.
Same data as `F10 → Info → Weather`.

---

#### `-brief`
The condensed situational briefing on demand — the same text the slot-entry
panel shows about 20 seconds after you take a slot.

```
-brief
```

Gives you the headline, your coalition's top tasks, the nearest known threat and
the GCI frequency. The full six-page report is `F10 → Info → Situation`; the
whole system is explained in
[The Auto-Generated Briefing](./briefing.md).

---

### Help

#### `-help`
Display available commands.

```
-help
```

**Response**: Lists all available commands with brief descriptions.

**Tip**: Keep this page bookmarked for detailed explanations!

---

## Unit Management Commands

### Troop Operations

#### `-bind <troop-id>`
Bind troops to your user for movement commands.

```
-bind 12345
```

**Usage**:
- After deploying troops
- Enables movement commands
- Troops follow your orders

**Finding Troop ID**:
- Check F10 map markers
- System message after deployment
- Format: usually numeric

---

#### `-delete <group-id>`
Delete deployed troops/groups.

```
-delete 12345
```

**Important**:
- Gives **50% refund** of deployment cost!
- Permanent action
- Clean removal

**Use Cases**:
- Remove stuck troops
- Clean up after mission
- Delete accidental deployments
- Get some points back

---

### JTAC Commands

#### `-jtac <id> <command>`
Control JTAC units via chat.

**Available Commands**:
```
-jtac <id> status      - Show JTAC status
-jtac <id> shift       - Shift to next target
-jtac <id> autoshift   - Toggle auto-shift
-jtac <id> pointer     - Toggle IR pointer
-jtac <id> smoke       - Smoke target
-jtac <id> code <code> - Set laser code
-jtac <id> arty <aid> <n> - Artillery fire mission
-jtac <id> bomber [mission] - Bomber mission
```

**Example**:
```
-jtac 12345 status
-jtac 12345 arty 54321 5
```

See [JTAC System](../f10-menu/jtac.md) for detailed usage.

**Note**: Most players use F10 menu for JTAC instead of chat commands.

---

### Action Commands

#### `-action <name> <args>`
Control deployed action units via chat.

```
-action help                    - List available actions
-action <name> <key>            - Execute action at map marker
-action <name> <group> <key>    - Execute action on group
```

**Examples**:
```
-action help
-action fighters CAP1
-action rtb 12345 HOME
```

**Note**: Most players use F10 menu for actions instead of chat commands.

---

### GCI / AWACS Voice

#### `-gci [option]`
Tune the [Live GCI](gci.md) voice controller to your preference. Also available
under **F10 → GCI/EWR → GCI Voice**. Your choice is saved to your pilot.

```
-gci             - show your current GCI settings
-gci on / off    - unmute / mute all GCI calls to you
-gci imperial    - range in nautical miles, altitude in thousands of feet
-gci metric      - range in kilometres, altitude in metres
-gci braa        - contact position from your own jet (default)
-gci bulls       - contact position from the bullseye
-gci clock       - contact position as a clock code + high / low
-gci auto        - follow the server defaults
```

On servers with speech recognition you can also key up on the GCI frequency and
ask — *"Magic, bogey dope"*, *"Magic, picture"*, *"Magic, declare"*,
*"Magic, commit"*. See [Live GCI](gci.md) for the full phrasebook.

---

## Administrative Commands

These commands require admin privileges.

### Admin Access

Only players with admin privileges can use these commands. Check with server staff if you think you need admin access.

#### `-admin help`
List all admin commands.

```
-admin help
```

**Response**: Complete list of admin commands with syntax.

---

### Common Admin Commands

#### `-admin reduce <objective> <percent>`
Reduce supplies at objective.

```
-admin reduce Batumi 50
```

Reduces objective supplies by specified percentage.

---

#### `-admin tick`
Force logistics tick immediately.

```
-admin tick
```

Triggers supply distribution without waiting for scheduled tick.

---

#### `-admin deliver`
Force logistics delivery.

```
-admin deliver
```

Triggers supply delivery cycle.

---

#### `-admin repair <airbase>`
Repair one step at airbase.

```
-admin repair Batumi
```

Increases logi by one step.

---

#### `-admin capture <objective> <blue|red|neutral>`
Force an objective to change hands, with all the normal capture side effects (logistics/services repair, garrison revival, warehouse and supply-line transfer, markers). Works on carrier groups too. Objective names with spaces are fine.

```
-admin capture Mersin red
-admin capture Blue Strike Group red
```

---

#### `-admin tim <key> [size] [alt]`
Create explosions at every F10 map mark whose text is `<key>`. `size` defaults to 3000; `alt` is an optional altitude.

```
-admin tim boom
-admin tim boom 500
```

Place one or more map markers all labelled `boom`, then run the command to detonate them.

---

#### `-admin spawn <key>`
Spawn units at F10 mark.

```
-admin spawn MARK1 deployable blue 180 TankPlatoon
```

**Format**: `<key> <troop|deployable> <side> <heading> <name>`

---

#### `-admin switch <side> <player>`
Force player side switch.

```
-admin switch blue Viper21
```

Switches player to specified side regardless of restrictions.

---

#### `-admin ban <duration> <player>`
Ban player from server.

```
-admin ban 7days Griefer123
```
```
-admin ban forever Hacker456
```

**Duration examples**:
- `10minutes`
- `2hours`
- `7days`
- `forever`

---

#### `-admin unban <player>`
Unban player.

```
-admin unban Viper21
```

---

#### `-admin kick <player>`
Kick player from server.

```
-admin kick IdlePlayer
```

Removes player without banning.

---

#### `-admin reset-lives <player>`
Reset player's lives.

```
-admin reset-lives Viper21
```

Restores lives to starting amount.

---

#### `-admin connected`
List connected players.

```
-admin connected
```

Shows all currently connected players with IDs.

---

#### `-admin banned`
List banned players.

```
-admin banned
```

Shows all banned players and ban durations.

---

#### `-admin search <regex>`
Search player database.

```
-admin search Viper.*
```

Searches players using regular expressions.

---

#### `-admin balance <player>`
Check player's point balance.

```
-admin balance Viper21
```

---

#### `-admin set-points <amount> <player>`
Set player's points.

```
-admin set-points 5000 Viper21
```

**Use Cases**:
- Compensate for bugs
- Event rewards
- Balance adjustments

---

#### `-admin delete <group-id>`
Delete deployed group cleanly.

```
-admin delete 12345
```

Better than player `-delete` for complex cleanups.

---

#### `-admin deslot <player>`
Force player to spectators.

```
-admin deslot Viper21
```

Removes player from their slot.

---

#### `-admin reset [winner]`
Reset campaign (dangerous!).

```
-admin reset
```
```
-admin reset blue
```

**Warning**:
- Resets entire campaign state
- Usually requires confirmation
- Use for new campaigns only

---

#### `-admin shutdown`
Shutdown server.

```
-admin shutdown
```

Graceful server shutdown with save.

---

## Point Transfer Commands

If enabled by server:

#### `-transfer <amount> <player|objective>`
Transfer points to another player or objective.

```
-transfer 500 Viper21
-transfer 100 objective:Batumi
```

**Requirements**:
- Sufficient points
- Valid player name or objective name
- For players: same team only

---

## Command Tips

### Finding Player Names

**For Commands**:
- Use exact name as shown in player list
- Case-sensitive in some cases
- Use quotes if name has spaces: `"Player Name"`
- Some commands accept UCID or Player ID

**Getting Player Info**:
```
-admin connected
```
Lists all players with IDs.

### Command Errors

**"Command not found"**:
- Check spelling
- Verify dash `-` prefix
- Ensure server supports command

**"Permission denied"**:
- Command requires admin
- You don't have privileges
- Contact server staff

**"Invalid syntax"**:
- Check argument format
- Verify spacing
- Re-read command examples

## Command Aliases

Some commands have shortcuts:

| Full Command | Alias | Notes |
|--------------|-------|-------|
| `blue` | (none) | Registration only |
| `red` | (none) | Registration only |
| `help` | `-help` | Both work |
| `points` | `-balance` | System interprets |
| `credits` | `-balance` | System interprets |

## Server-Specific Commands

Some servers add custom commands:
- Check server Discord
- Read MOTD (Message of the Day)
- Ask in chat
- Check server documentation

## Next Steps

Now explore the [F10 Menu Systems](../f10-menu/overview.md) for more advanced interactions!
