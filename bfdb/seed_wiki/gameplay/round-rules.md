# Rear Bases, Era Weapons & Round Length

Each server sets these rules for itself, and each one only applies where the
server's config turns it on. Check the server's comms and rules card, or ask
staff.

## Protected rear bases

Each side can have a few protected rear airfields, so nobody spawns into a
fight they never chose. While its own side still holds the base, an **enemy
aircraft within {{cfg:rear_base_protection.radius_m|27780}} m** (15 nm by
default) of it gets **{{cfg:rear_base_protection.grace_secs|60}} seconds in
total** in that airspace. After that it is destroyed and the pilot goes to
spectators.

```dcsmessage
PROTECTED AIRSPACE: Kutaisi is a rear base. Leave now -- 45s before you are removed.
```

You are warned every 10 seconds. The time is a running total for the sortie,
not a fresh allowance each visit; it only resets when you leave the slot. When
it runs out:

```dcsmessage
You stayed in Kutaisi's protected airspace -- returning to spectators. Staff have been notified.
```

A server can exempt aircraft flying at or below
{{cfg:rear_base_protection.min_agl_m|0}} m above the ground (0 = no
exemption), such as a helicopter crossing at treetop level.

## Era rules

A Cold War or early-era round can limit both airframes and weapons to the era.

**Airframes** outside the era's list can't be slotted:

```dcsmessage
F-14B is not available in the current era (1980s)
```

**Weapons** on the era's ban list, for example AMRAAMs in an 80s round, can
still be loaded at the rearm screen, so check your loadout before you taxi.

- **If you take off carrying one,** you get a warning and
  {{cfg:era.banned_weapon_land_secs|300}} seconds (never less than 30) to land:

```dcsmessage
BANNED IN THE 1980S ERA: AIM-120C AMRAAM. Land and remove it within 5:00, and do not fire it -- or you will be removed.
```

- **If you fire one, or are still flying when time is up,** your aircraft is
  destroyed and you go to spectators:

```dcsmessage
You fired AIM-120C AMRAAM, which is banned in this era -- returning to spectators.
```

Landing in time forgives it; the next takeoff checks again.

## How a round ends

A round ends in one of six ways. The first five are announced to everyone
with a countdown before the server resets for the next round.

- **Map victory.** Where the server sets one, a side that holds
  {{cfg:auto_reset.condition.MapOwned.fraction|the set share}} of all
  objectives (neutral ones count in the total) wins, after
  {{cfg:auto_reset.delay|30}} seconds' notice.
- **Last stand.** Where the server runs it, a side down to
  {{cfg:last_stand.trigger_count|1}} primary objective(s) (airbases, naval
  bases and FARPs) has {{cfg:last_stand.countdown_secs|3600}} seconds to take
  something back:

```dcsmessage
Red is down to 1 primary objective(s) — Last Stand! Blue wins in 3600s if not recaptured.
```

  The countdown repeats at intervals
  (`Red is making their last stand! Blue wins in 45m 00s unless Red recaptures.`)
  and stops with `Red has recaptured objectives. Last stand cancelled.`
- **War will.** Where the server runs it, each side has a **will to fight**,
  0-100, starting at {{cfg:war_will.start|70}}. Losses lower it: a carrier
  group hit ({{cfg:war_will.carrier_damaged|8}}) or sunk
  ({{cfg:war_will.carrier_destroyed|25}}), a command centre lost
  ({{cfg:war_will.command_center_lost|10}}), a factory destroyed or taken
  ({{cfg:war_will.factory_lost|5}}), an airbase lost
  ({{cfg:war_will.airbase_lost|4}}), aircraft, ships and ground formations,
  and fuel stocks running under {{cfg:war_will.fuel_low_pct|30}}% of
  capacity. Every enemy loss raises yours by half as much, and captures raise
  the captor's. It recovers slowly while nothing goes wrong. A side whose
  will stays under {{cfg:war_will.threshold|20}} for
  {{cfg:war_will.hold_hours|6}} hours **asks for terms**, and the other side
  wins. The side is warned when it drops under the line, and on a countdown
  after that:

```dcsmessage
WILL TO FIGHT 18/100, below 20: we ask for terms in 5h 00m unless it recovers.
```

  The dashboard shows both sides' will, its trend and the biggest recent
  causes.
- **A story ending.** Where the campaign writes [story events](#story-events),
  one of them can end the war: on the Southern Flank, Moscow asks for terms
  once Soviet fuel has run short and its will has collapsed. Like every story
  event, it only happens because of what was done in the round.
- **Round length.** A server can cap a round at
  {{cfg:max_round_days|0}} real-world days (0 = no cap). If nobody has won by
  then, the round ends in a **draw**. This lets a theatre rotation move on from
  a front that has frozen.
- **Admin reset.** Staff can end a round by hand; naming a winner announces it
  and counts down first.

When it happens:

```dcsmessage
ROUND OVER: Blue has won the war! The server will reset in 10m 00s
```

or, for the round-length cap,
`ROUND OVER: the round has run its full length and ends in a draw.`

A new round starts the map, everyone's points, lives and side switches, and the
[war effort bar](./war-effort.md) from scratch. What you keep across rounds is
your XP, level and achievements; see [XP, Levels & Squadrons](./progression.md).

## Story events

A campaign can write **story events**. Each one waits for something players
(or the AI) do: an objective destroyed, captured or held, a type of aircraft
shot down in numbers, supply convoys cut, a side's fuel or will to fight
crossing a line. None of them runs on a clock or in a fixed order, so which
fire, and when, is up to the round. When one fires, everyone is told, the
pilots who caused it are named (and may be paid a bonus), and the war
changes: will to fight, fuel or equipment output, stocks at a base, an action
withdrawn, unlocked or repriced, the AI headquarters' orders, intel handed to
a side, or the end of the round. Each fires once a round unless the campaign
lets it repeat, and some rule each other out, so whichever side gets there
first decides which way the story goes:

```dcsmessage
TUAPSE BURNS: The Tuapse refinery is burning after a strike by Shade and Kilo.
```

The dashboard lists the events that have fired, and the daily war news leads
with them.

A story can also give a losing side a last card: a nuclear strike flown by
an AI aircraft, which the other side is warned about and can shoot down. See
[The Last Stand](../advanced/campaign-events.md#the-last-stand-nuclear-strike).

## Scheduled restarts

Restarts are not the end of a round: the campaign is saved and carries on.
Where the server restarts on a schedule, everyone is warned at 30, 10, 5 and 1
minutes:

```dcsmessage
The server will restart in 10 minutes
```

`-time` tells you how long is left at any moment.

## Who may use what

A server can restrict, per player, who may use the F10 Actions menu, the cargo
menu, the troops menu, JTACs and the Combined Arms slots. Blocked players see
the menu missing, a Combined Arms slot refused with `access to slot is denied`,
or one of:

```dcsmessage
you are not permitted to use actions on this server
you are not permitted to command JTACs on this server
```

## Empty-server protection

Where the server has it on, a side with nobody (or almost nobody) in a slot is
harder to roll up overnight: captures of its bases take longer, and its bases
can repair faster. See
[Capturing Objectives](./capturing-objectives.md).

## See Also

- [Joining a Team](../getting-started/joining-team.md): slot rules, takeoff hold, side switching
- [Points and Lives](./points-and-lives.md)
- [FAQ](../reference/faq.md)
