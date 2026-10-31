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

A round ends in one of four ways. The first three are announced to everyone
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
