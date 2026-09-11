# Complete Chat Command Reference

Quick reference for all Fowl Engine chat commands.

## Player Commands

### Registration & Status

| Command | Description | Example |
|---------|-------------|---------|
| `blue` | Register for Blue team | `blue` |
| `red` | Register for Red team | `red` |
| `-switch blue` | Switch to Blue team | `-switch blue` |
| `-switch red` | Switch to Red team | `-switch red` |
| `-lives` | Check lives, points, status | `-lives` |
| `-balance` | Check point balance | `-balance` |
| `-status` | Campaign status: your points/streak/kills, objectives held per side, your side's convoys in transit | `-status` |
| `-time` | Mission time and restart schedule | `-time` |
| `-weather` | Weather brief for your position | `-weather` |
| `-brief` | Auto-generated situational briefing (full report: F10 → Info → Situation) | `-brief` |
| `-help` | Show command list | `-help` |

### Unit Management

| Command | Description | Example |
|---------|-------------|---------|
| `-bind <id>` | Bind troop for control | `-bind 12345` |
| `-delete <id>` | Delete deployed group | `-delete 12345` |

### JTAC Commands

| Command | Description | Example |
|---------|-------------|---------|
| `-jtac status` | Get JTAC status | `-jtac status` |

### GCI Voice

| Command | Description | Example |
|---------|-------------|---------|
| `-gci` | Show your current GCI settings | `-gci` |
| `-gci on` \| `off` | GCI voice on or off entirely | `-gci off` |
| `-gci callouts` \| `quiet` | Unprompted calls on or off | `-gci quiet` |
| `-gci metric` \| `imperial` | Spoken units | `-gci imperial` |
| `-gci braa` \| `bulls` \| `clock` | Position reference | `-gci bulls` |
| `-gci auto` | Follow the server defaults | `-gci auto` |

### Actions

| Command | Description | Example |
|---------|-------------|---------|
| `-action <name> <args>` | Run an action from chat instead of the F10 menu | `-action help` |

### Point Transfers (if enabled)

| Command | Description | Example |
|---------|-------------|---------|
| `-transfer <pts> <player>` | Transfer points to player | `-transfer 500 Viper21` |
| `-transfer <pts> objective:<name>` | Transfer points to objective | `-transfer 100 objective:Batumi` |

## Admin Commands

Admin only. `-admin help` lists them in game; the full explanations are in the
[Chat Commands guide](../gameplay/chat-commands.md).

| Command | Description |
|---------|-------------|
| `-admin help` | List admin commands |
| `-admin connected` / `-admin banned` | Who is on, who is banned |
| `-admin search <regex>` | Find a player by name |
| `-admin kick <player>` | Kick |
| `-admin ban <duration\|forever> <player>` / `-admin unban <player>` | Ban / unban |
| `-admin deslot <player>` | Force a player out of their slot |
| `-admin switch <side> <player>` | Move a player to the other coalition |
| `-admin add-admin <player>` / `-admin remove-admin <player>` | Grant / revoke admin |
| `-admin balance <player>` / `-admin set-points <amount> <player>` | Inspect / set points |
| `-admin reset-lives <player>` / `-admin reset-lives-all` | Restore lives |
| `-admin capture <objective> <blue\|red\|neutral>` | Force an objective's owner |
| `-admin repair <airbase>` | Repair an objective |
| `-admin reduce <airbase> <amount>` | Reduce an objective's inventory |
| `-admin transfer <from> <to>` | Transfer supply between objectives |
| `-admin tick` / `-admin deliver` | Force a logistics tick / delivery |
| `-admin spawn <key>` | Spawn a group by template key |
| `-admin tim <key> [size] [alt]` | Spawn a timed explosion |
| `-admin delete <group-id>` | Delete a group |
| `-admin remark <text>` | Post a remark |
| `-admin log-warehouse <objective\|dcs> <airbase>` | Dump a warehouse to the log |
| `-admin log-logistics` / `-admin logdesc` | Dump logistics state to the log |
| `-admin reset [winner]` | End the round |
| `-admin shutdown` | Shut the server down |

## Command Syntax Notes

- `<required>` = Required parameter
- `[optional]` = Optional parameter
- `<option1|option2>` = Choose one
- Case-insensitive for most commands
- Player names may be case-sensitive

## See Also

- [Chat Commands Guide](../gameplay/chat-commands.md) - Detailed explanations
