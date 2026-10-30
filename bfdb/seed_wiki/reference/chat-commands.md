# Complete Chat Command Reference

Every chat command the engine understands, one line each. What they do and
what the replies look like is explained in the
[Chat Commands guide](../gameplay/chat-commands.md).

Type a command in the DCS chat (team or all). It starts with a dash. Use
lower case: `-lives`, `-time`, `-weather`, `-switch`, and a bare `-gci` or
`-hq` accept any case, but the others only match in lower case. A line that
starts with a dash and a letter but isn't a command gets
`<line> is not a valid command. Valid commands follow.` and the `-help` list.

The reply goes only to you. The line you typed still shows in the chat channel
you typed it into, like any message.

## Status and account

| Command | What it does |
| --- | --- |
| `-help` | List the player commands (admins also get the `-admin` line) |
| `-lives` | Lives left per role, and when each pool refills |
| `-balance` | Your points balance |
| `-status` | Side, points, kill streak, career kills, objectives held per side, your side's convoys in transit |
| `-time` | Time left until the scheduled server restart |
| `-weather` | Weather report for your slot (needs a slot, unless the server publishes its own weather) |
| `-brief` | The condensed situation briefing (full report: F10 → Info → Situation) |
| `-switch blue` / `-switch red` | Change coalition, from spectators, if you have a side switch left |
| `-bind <uuid>` | Link your pilot to the web dashboard, with the UUID from the dashboard's login page |

## Points and units

| Command | What it does |
| --- | --- |
| `-transfer <amount> <player>` | Give points to a pilot on your side (name, start of a name, or player id) |
| `-transfer <amount> objective:<objective>` | Pay points into a base your side holds |
| `-delete <group id>` | Delete a group you deployed, for half its cost back. Commanders can also disband unowned groups (no refund) |
| `-action help` | List your side's actions with their chat syntax and cost |
| `-action <name> <key>` | Run an action at the F10 map mark whose text is `<key>` |
| `-action <name> <group> <key>` | Run a waypoint/move action on a group |
| `-action <name> <objective>` | Logistics repair or reinforcement to an objective |
| `-action <name> <from> <to>` | Logistics transfer between two objectives |
| `-action <name> <type> <key>` | Add Task: post a task of `<type>` at a mark |
| `-action <name> <task id>` | Remove Task |

## JTAC

| Command | What it does |
| --- | --- |
| `-jtac help` | Print the JTAC commands |
| `-jtac list` | Your side's JTACs: id, nearest objective, laser code, what it is lasing |
| `-jtac <id> status` | Targets, laser range, what it can see |
| `-jtac <id> shift` | Lase the next target in range (turns auto-shift off) |
| `-jtac <id> autoshift` | Toggle auto-shift (always lase the top-priority target) |
| `-jtac <id> smoke` | Smoke the current target |
| `-jtac <id> focus` / `focus <mark>` / `focus clear` | Lase near your latest map mark, a named mark, or stop focusing |
| `-jtac <id> move <mark>` | Fly a drone JTAC to a map mark (the drone waypoint action, costs points) |
| `-jtac <id> building` | Lase the next logistics building at its objective |
| `-jtac <id> 9line` | The 9-line for the current target (`9-line` and `nineline` also work) |
| `-jtac <id> filter <type>` / `filter clear` | Only lase that unit type, e.g. `filter SAM`; repeat to add types |
| `-jtac <id> code <code>` | Set the laser code, 1111-1788 |
| `-jtac <id> pointer` | Toggle the IR pointer |
| `-jtac <id> arty <gun id\|all> <rounds>` | Artillery on the current target |
| `-jtac <id> bomber [mission]` | Call a bomber mission on the current target |

## GCI voice

| Command | What it does |
| --- | --- |
| `-gci` (or `-gci status`) | Your current GCI settings and the options |
| `-gci on` / `-gci off` | GCI voice calls to you on or off |
| `-gci callouts` / `-gci quiet` | Unprompted calls on or off (`callouts on`, `loud`, `callouts off`, `silent` also work) |
| `-gci metric` / `-gci imperial` | Spoken units |
| `-gci braa` / `-gci bulls` / `-gci clock` | Position reference (`self` = `braa`, `bullseye` = `bulls`) |
| `-gci auto` (or `-gci default`) | Back to the server defaults for units and reference |

## Theatre HQ

| Command | What it does |
| --- | --- |
| `-hq` | Your side's commander's intent |
| `-hq ops` | What the HQ has under way, and the requests waiting (`operations`, `status` also work) |
| `-hq help` | Print the HQ commands |
| `-request <cas\|cap\|sead\|recon\|fires\|supply\|troops> [objective]` | Ask the HQ for support (nearest objective if none named) |
| `-request cancel <id>` | Withdraw your request |
| `-hq posture <offensive\|balanced\|defensive>` | HQ commanders only: set the posture |
| `-hq effort <objective>` | HQ commanders only: set the main effort (`main`, `attack` also work) |
| `-hq defend <objective>` | HQ commanders only: add a base to hold (`hold` also works) |
| `-hq supply <objective>` | HQ commanders only: add a supply priority |
| `-hq avoid <objective>` | HQ commanders only: keep the HQ away from a base |
| `-hq pause` / `-hq resume` | HQ commanders only: stop or restart the HQ's own planning |
| `-hq cancel <op id>` | HQ commanders only: call off an operation |
| `-hq clear` | HQ commanders only: hand command back to the HQ (`release` also works) |

See [Theatre HQ](../advanced/theatre-hq.md) for what these do.

## Not engine commands

| Command | What it is |
| --- | --- |
| `-linkme <token>` | DCSServerBot's Discord link (get the token with `/linkme` in Discord). Needed for `/fe_card` and the Discord-linked web features |
| `-cockpitkey ...` | Sent by the cockpit overlay on its own to register itself. Never shown in chat; you never need to type it |

## Admin commands

Admins only; anyone else gets no reply. `-admin help` lists them in game.

| Command | What it does |
| --- | --- |
| `-admin help` | List the admin commands |
| `-admin connected` / `-admin banned` | Who is on; who is banned |
| `-admin search <regex>` | Search the player list by regular expression |
| `-admin kick <player>` | Kick a player |
| `-admin ban <duration\|forever> <player>` | Kick and ban, e.g. `ban 10days D4n` |
| `-admin unban <player>` | Lift a ban |
| `-admin deslot <player>` | Force a player to spectators |
| `-admin switch <side> <player>` | Force a player onto a side (sends them to spectators if slotted) |
| `-admin add-admin <player>` / `-admin remove-admin <player>` | Grant / revoke admin |
| `-admin blacklist <rule> <player>` / `-admin whitelist <rule> <player>` | Deny / allow `actions`, `cargo`, `troops`, `jtac` or `ca` |
| `-admin balance <player>` | Show a player's points |
| `-admin set-points <n> <player>` | Set a player's points |
| `-admin addpoints <player> <n>` | Add points (negative takes them away) |
| `-admin lives <player> <standard\|intercept\|logistics\|attack\|recon> <n>` | Set how many lives of a type a player has left |
| `-admin reset-lives <player>` / `-admin reset-lives-all` | Restore one player's / everyone's lives |
| `-admin capture <objective> <blue\|red\|neutral>` | Force an objective to change hands |
| `-admin repair <airbase>` | Repair one step |
| `-admin reduce <objective> <percent>` | Cut an objective's supplies by a percentage |
| `-admin transfer <from-objective> <to-objective>` | Move supplies between objectives |
| `-admin reinit-warehouse <airbase>` | Reinitialise a warehouse |
| `-admin tick` / `-admin deliver` | Run a logistics tick / delivery now |
| `-admin logistics` | Dump the whole logistics picture to the log |
| `-admin log-warehouse <objective\|dcs> <airbase>` | Write a warehouse's contents to the log |
| `-admin log-desc` | Write the getDesc of your current aircraft to the log |
| `-admin whythreat <objective>` | Why an objective isn't repairing |
| `-admin ghosts` / `-admin purge-ghosts` | List / remove units the campaign thinks are alive but DCS doesn't |
| `-admin spawn <key>` | Spawn at an F10 mark: `<key> <troop\|deployable> <side> <heading> <name>` |
| `-admin tim <key> [size] [alt]` | Explosions at every F10 mark with text `<key>` (size default 3000) |
| `-admin jammer <key>` | Spawn a GPS/radio jammer at every F10 mark `<key> <side>` |
| `-admin jammer_mode <group> <gps> <glonass> <radio> [start_mhz end_mhz]` | Set a jammer's modes |
| `-admin delete <group id>` | Delete a group |
| `-admin remark <objective>` | Redraw an objective's map markup |
| `-admin broadcast <text>` | Send text to everyone as chat and a panel |
| `-admin reset [winner]` | End the round and reset the campaign (needs `confirm`) |
| `-admin shutdown` | Shut the server down (needs `confirm`) |

`reset` and `shutdown` must be said twice: send the command, then the same
command with `confirm` on the end within 30 seconds.

## Syntax notes

- `<required>`, `[optional]`, `a|b` = pick one.
- `<key>` / `<mark>` is the text of an F10 map mark you placed.
- Player names match case-insensitively, exactly or by a unique start of the
  name; a player id number also works.

## See Also

- [Chat Commands guide](../gameplay/chat-commands.md)
- [Points and Lives](../gameplay/points-and-lives.md)
- [JTAC](../f10-menu/jtac.md)
- [Live GCI](../gameplay/gci.md)
