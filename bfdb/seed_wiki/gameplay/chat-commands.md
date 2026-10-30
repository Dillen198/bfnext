# Chat Commands

Chat commands do what the F10 menu can't, or do it faster: check your lives
and points, switch sides, give points away, delete a unit you deployed, or run
a JTAC from a Combined Arms slot. This page explains the commands a player
uses and shows what the server answers. The one-line list of every command,
admin commands included, is the
[Chat Command Reference](../reference/chat-commands.md).

## How to type them

- Every command starts with a dash: `-lives`, `-transfer 500 Viper21`.
- Type commands in **lower case**. `-lives`, `-time`, `-weather`, `-switch`
  and a bare `-gci` or `-hq` work in any case; the rest only match lower case.
- The answer comes back to **you only**. The line you typed still appears in
  the chat channel you sent it to, so use team chat if you don't want the
  other side reading it.
- A mistyped command gets the help list back:

```dcsmessage
-hepl is not a valid command. Valid commands follow.
```

Plain words such as `help`, `points` or `credits` (without a dash) get the same
answer. They are not shortcuts for anything.

## Getting help

`-help` prints the player commands:

```dcsmessage
-switch <color>: side switch to <color>
-lives: display your current lives
-time: how long until server restart
-weather: full weather report for your slot, including winds/temp aloft
-balance: show your points balance
-status: show campaign status (objectives, convoys, streak)
-brief: auto-generated situational briefing (full report: F10 > Info > Situation)
-transfer <amount> [<player> | objective:<objective>]: transfer points to another player or objective
-delete <groupid>: delete a group you deployed for a partial refund
-action <name> <args>: perform an action, -action help for a list of actions
-bind <uuid>: link your account to the web dashboard (uuid from its login page)
-jtac list | -jtac <id> <cmd>: work a JTAC from chat, -jtac help for the commands
-gci [on|off|metric|imperial|auto]: control your live GCI voice calls
-hq [ops|help]: your side's commander's intent and operations, -request <kind>: ask HQ for support
-help: show this help message
```

## Your account

### Joining a side

There is no join command. Taking your first slot registers you to that slot's
coalition, and the server tells you:

```dcsmessage
Welcome to the Blue team. You may only occupy slots belonging to your team. Good luck!
```

Everyone else sees `Viper21 has joined Blue team`. See
[Joining a Team](../getting-started/joining-team.md).

### `-switch blue` / `-switch red`

Moves you to the other coalition. You must be **in spectators** and have a
side switch left this round ({{cfg:side_switches|unlimited}} on this server).
The whole server is told:

```dcsmessage
Viper21 has switched to Red
```

When it can't, you get one of:

```dcsmessage
you must be in spectators to switch sides
you can't switch sides again this round
you are already on the requested side
You are not registered. Take a slot on the side you want to fly
```

### `-lives`

One line per role: lives left, the pool size, and how long until a pool you
have dipped into refills completely.

```dcsmessage
standard 2/3 resetting in 04:12:09
intercept 4/4
attack 4/4
logistics 6/6
recon 6/6
```

Lives are only enforced where the server turns them on
(**{{cfg:limited_lives|yes}}** here). With them off, nothing is ever taken, so
every line reads full. See [Points and Lives](./points-and-lives.md).

### `-balance`

```dcsmessage
You have 1250 points
```

### `-status`

Your numbers and the war's, in one message:

```dcsmessage
=== CAMPAIGN STATUS ===
Side: Blue | Points: 1250 | Streak: 2 | Career Kills: 41
Objectives — Blue: 17 | Red: 14
Active Blue Convoys: 3
```

The streak counts kills since your last takeoff. The convoy count says whether
your supply system is moving; `F10 → Info → Supply Convoys` lists what is in
transit.

### `-bind <uuid>`

Links your pilot to the web dashboard. The dashboard's login page shows a UUID;
paste it once:

```
-bind 3f2a91c4-8d1e-4b77-9a02-5c6e1d84b0f3
```

```dcsmessage
Bind request sent -- reload the dashboard to confirm your account is linked
```

It only takes a UUID in the `8-4-4-4-12` form. **It does not bind groups.**
Anything else is answered with:

```dcsmessage
Invalid token -- -bind takes the UUID from the web dashboard login page
it does not bind groups. Your own groups are listed first under F10 > Actions
```

## Server and situation

### `-time`

```dcsmessage
The server will shutdown in 02:45:30
```

or, on a server with no scheduled restart,
`The server isn't configured to restart automatically`. The server also warns
everyone at 30, 10, 5 and 1 minutes before a scheduled restart.

### `-weather`

A weather report. Where the server publishes its own weather (live
real-world weather), you get it whether or not you are flying:

```dcsmessage
SERVER WEATHER
Temp: 14°C / 57°F
Surface wind: 270° at 12 kt
Visibility: 10 km / 6 SM
Clouds: base 3500 ft AGL, 4/10
QNH: 1013 hPa / 29.91 inHg
```

Otherwise it is the full report for your slot, winds and temperatures aloft
included, and you must be in one:
`You must be in a slot to request a weather report`. The same data is under
`F10 → Info → Weather`.

### `-brief`

The condensed situation briefing on demand, the same text the slot-entry panel
shows: the headline, your side's top tasks, the nearest known threat and the
GCI frequency. The full six-page report is `F10 → Info → Situation`; see
[The Auto-Generated Briefing](./briefing.md). Before you have taken a slot the
answer is
`you aren't registered yet -- take any slot on the side you want to fly`.

## Points and units

### `-transfer`

```
-transfer 500 Viper21
-transfer 100 objective:Batumi
```

Gives points to a pilot **on your side**, or pays them into the fund of a base
**your side holds** (bases pay for flights, troops and deployables when a
pilot's own points run short). A pilot can be named in full, by a unique start
of the name, in any case, or by player id. On success you get
`transfer complete`, and the other pilot sees:

```dcsmessage
1750(+500) you received points from Viper11
```

When it can't go through:

```dcsmessage
transfer failed insufficient balance, you have 300, you requested 500
transfer failed you can transfer 200 -- the other 800 is your starting grant, which you can spend but not give away
Viper21 isn't on your side
Batumi isn't held by your side
could not transfer to Vip, 2 players match Vip (Viper11, Viper21), type more of the name
```

A late joiner's head start can be spent but not transferred; see
[Points and Lives](./points-and-lives.md#joining-late).

### `-delete <group id>`

Removes a group you deployed and hands back **half its cost**: to you for the
part you paid, and to the base fund for the part it paid, while your side still
holds that base. A unit the treasury paid for refunds nothing. The answer is
`deleted 12345`, and a `reclaimed 12345` points message for the refund.

```dcsmessage
group 12345 wasn't deployed by you
group 12345 belongs to the other side
can't delete an action group
can't delete an objective group
can't delete a downed pilot this way
```

**Commanders** can also clear away ground units nobody owns (put down by the HQ
or an admin) and dismount squads. They are disbanded with nothing refunded:
`disbanded 12345 (unowned, nothing refunded)`. A dismount squad holding a base
it has just captured can't be deleted.

Moving something you deployed needs no command: `F10 → Actions>> → Move`,
pick the group, then pick a map mark. Your own groups are listed first.

## Actions from chat

Everything in the F10 Actions menu can be run from chat. Start with
`-action help`, which lists your side's actions, ready to copy:

```dcsmessage
<key> is the text of an F10 map mark you placed, e.g. -action JTAC Drone M1
```

Each line after it has the form
`-action <name> <key> | <what it does>. cost <points>`, for example
`-action E-3A AWACS <key> | Spawn an awacs at key, a mark point.` followed by
this server's cost. Waypoint actions take a group id too
(`-action <name> <group> <key>`). The answer is `action <...> started`, or
`could not run action <...>: <reason>`. A player barred from actions gets
`you are not permitted to use actions on this server`.

## JTAC from chat

`-jtac list` gives each JTAC's id, the number in brackets:

```dcsmessage
JTACs (the number in brackets is the id for -jtac <id> ...):
 Axeman (12345) near Gori, code 1688: lasing T-72B
 12377 near Tskhinvali, code 1511: sees enemies, none in laser range
```

Then `-jtac <id> <command>`:

```
-jtac <id> status            targets, laser range, what it can see
-jtac <id> shift             lase the next target in range (auto off)
-jtac <id> autoshift         toggle auto (lase the top-priority target)
-jtac <id> smoke             smoke the current target
-jtac <id> focus [<mark>|clear]  lase near your latest / the named map mark
-jtac <id> move <mark>       fly a drone JTAC to a map mark (costs points)
-jtac <id> building          lase the next logistics building at its objective
-jtac <id> 9line             9-line for the current target
-jtac <id> filter <type>|clear  only lase that type, e.g. filter SAM
-jtac <id> code <code>       set the laser code (1111-1788)
-jtac <id> pointer           toggle the IR pointer
-jtac <id> arty <gun|all> <n>  artillery fire mission
-jtac <id> bomber [mission]  bomber mission
```

`-jtac help` prints the same list in game. A drone JTAC sees far but only
lases out to 18.5 km (10 nm). See [JTAC System](../f10-menu/jtac.md).

### Combined Arms commanders

A Combined Arms commander with no vehicle selected has no F10 menu at all, so
chat is the way in. Taking one of those slots prints:

```dcsmessage
Combined Arms: with no vehicle selected DCS has no F10 menu for you, so command from chat:
 -action help: AWACS, drones, JTACs, AI helos, tankers and the rest of your side's actions, with costs
   most take the text of an F10 map mark you placed, e.g. -action JTAC Drone M1
 -jtac list, then -jtac <id> status | shift | smoke | move <mark> | focus <mark> -- -jtac help for all
 -brief: the situation, -balance: your points, -help: everything else
```

## GCI voice

`-gci` shows your settings and the options; your choices are saved to your
pilot. The same controls are under `F10 → GCI/EWR → GCI Voice`.

```dcsmessage
GCI voice: ON | auto callouts: ON | units: server default | reference: BRAA (own jet)
  -gci on | off              GCI on or off entirely
  -gci callouts | quiet      unprompted calls on or off
  -gci metric | imperial     spoken units
  -gci braa | bulls | clock   position reference
  -gci auto                  follow the server defaults
```

With `-gci quiet` the controller still answers when you key up and ask; it
just never speaks first. See [Live GCI](./gci.md).

## Theatre HQ

`-hq` is your side's commander's intent, `-hq ops` what the HQ has under way,
and `-request cas Gori` asks it for support (cas, cap, sead, recon, fires,
supply or troops; the nearest objective if you name none). `-request cancel
<id>` withdraws it. On a server with no HQ the answer is
`there is no HQ on this server`. Commanders can also give the HQ orders; see
[Theatre HQ](../advanced/theatre-hq.md).

## Admin commands

Only admins get an answer; for anyone else the line is ignored. `-admin help`
prints them all, and the [reference](../reference/chat-commands.md#admin-commands)
lists them. Ending the round and shutting the server down must be said twice:

```dcsmessage
-admin reset blue will shut the server down AND reset the whole campaign. Send -admin reset blue confirm within 30s to go ahead
```

## Other commands you may see

`-linkme <token>` belongs to DCSServerBot, not the engine: it links your
Discord account (get the token with `/linkme` in Discord). The cockpit overlay
registers itself with a hidden chat line of its own; you never type it.

## Next Steps

Now explore the [F10 Menu Systems](../f10-menu/overview.md).
