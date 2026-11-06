# Campaign Events

Campaign events are the things the war does on its own, between and around
what the players do: artillery barrages on a contested base, missile strikes
on the enemy's rear, ambushes laid for supply convoys, and AI fighters and
helicopters scrambled to meet you. They are all real units in DCS, and you
can fight every one of them.

A server turns them on with `campaign_events.enabled`. Not every server does,
and each kind can be switched off on its own.

## Who starts them

| Event | Started by | Who is told |
|---|---|---|
| [Barrage](#barrage) | The AI commander | Everyone: a message and an F10 circle |
| [Missile strike](#missile-strike) | The AI commander, the [Theatre HQ](./theatre-hq.md) or a [player commander](./command.md) | Everyone: a message and an F10 circle |
| [Convoy ambush](#convoy-ambush) | The AI commander, the Theatre HQ or a player commander | Everyone: an F10 circle, no message |
| [Reactive CAP and helo patrols](./ai-opposition.md) | Automatic, when enemy players push | The side being scrambled against |
| [Commander CAP](#commander-cap) | The AI commander | The other side: an F10 circle |

The **AI commander** here is the Smart Commander running without the Theatre
HQ. It looks at each side every
{{cfg:campaign_events.check_interval_secs|300}} seconds (every 30 seconds
while one of that side's bases is threatened or being captured), and buys at
most one event per look with the side's treasury: whichever gives the best
value for the money. A side can have up to
{{cfg:campaign_events.max_concurrent_events|3}} events running at once; a base
actually being captured gets one through even past that limit. If nothing
worth buying turns up, the treasury is not charged.

| Event | Treasury cost |
|---|---|
| Barrage | {{cfg:smart_commander.barrage_cost|150}} |
| Missile strike | one and a half times the barrage cost |
| Convoy ambush | {{cfg:smart_commander.ambush_cost|100}} |
| Commander CAP | {{cfg:smart_commander.cap_cost|250}} |

On a server running the [Theatre HQ](./theatre-hq.md), the HQ plans these
itself as part of its operations, and the rules above step aside.

## Barrage

A friendly base's garrison (its armour and its medium and long-range groups)
opens fire on the enemy base it is best placed to hit: one within gun
range (and at least 5 km away), weak, short of supply, close, or about to be
captured. Logistics hubs and factories are worth more. Up to
{{cfg:campaign_events.barrage_max_groups|5}} groups fire, scattered over
{{cfg:campaign_events.barrage_radius_m|500}} m of the target. A group out of
range drives toward the target until it is in range, then fires.

Everyone on the server reads:

```dcsmessage
INTEL: Red fire-support mission in progress — Senaki is under artillery fire! (5 min)
```

The minutes are the barrage's length on this server
({{cfg:campaign_events.barrage_duration_secs|300}} seconds). A dashed circle
about 2 km across on the F10 map, labelled
`Fire Support [Red] — 4 units firing`, marks the target. When it is over:

```dcsmessage
INTEL: Red fire-support mission has ended
```

**What you can do:** if it is your base under fire, the guns are at an enemy
base within gun range of it, or driving toward one. Kill them, or get your own
units out of the circle. If it is the enemy's base, the shelling softens it up: it is a good
time to push troops in.

## Missile strike

Deployed long-range launchers (ALCM, Scud or HIMARS groups) fire at a
high-value enemy target: logistics hubs first, then factories, then whatever
is weak or about to be captured. Only one runs per side at a time.

```dcsmessage
INTEL: Blue missile strike inbound — Gudauta is the target!
```

A dashed circle about 3 km across marks the target, labelled
`Missile Strike [Blue] — 2 launchers firing`. After the same
{{cfg:campaign_events.barrage_duration_secs|300}} seconds:
`INTEL: Blue missile strike mission has ended`.

**What you can do:** the launchers are deployed units on the map. Hunt them
down before the next strike; nothing else stops it.

## Convoy ambush

A force drives out by road from one of the ambushing side's bases to a point
on an enemy supply convoy's way that it can reach first, sets up there, and
attacks the convoy when it arrives. It picks a spot at least 2 km ahead of the
convoy and not right at its destination, needs about two minutes to get off
the road and into position, and will not drive more than about 40 km to get
there. If no road gets it there in time, the ambush is called off and nothing
spawns (the treasury is not refunded).

There is no message, but an F10 circle about 1.5 km across, labelled
`AMBUSH [Red]`, marks the ambush point **for both sides**. Once the force is in
place it waits {{cfg:campaign_events.ambush_duration_secs|600}} seconds for the
convoy, then goes.

**What you can do:** if it is your convoy heading into the circle, escort it
or clear the ambush from the air first; the force is a normal ground group.
The ambushing force is also driving there by road from its base, so you can
catch it on the way. See [Logistics](../gameplay/logistics.md) for how convoys
run.

## Commander CAP

When the enemy has jets up and your side does not (at least three jets up on
one side, and the enemy at least
{{cfg:smart_commander.cap_min_friendly_pilots|2}} ahead), the AI commander can
buy a CAP flight for your side. It launches cold from the friendly airbase
nearest your most threatened base, holds a station over the contacts your
radar is painting, and goes home after
{{cfg:campaign_events.cap_duration_secs|600}} seconds. After one ends, the
side waits {{cfg:smart_commander.cap_cooldown_secs|300}} seconds before buying
another.

The other side sees it like a reactive CAP: a solid circle about 25 km across
around the launch field, labelled `Enemy CAP [Blue] — ACTIVE`. It uses the
same flight templates as the [reactive CAP](./ai-opposition.md), and needs
`enemy_cap_enabled`.

## Night and weather

Two optional modifiers sit next to the events in the config:

- **Night kill bonus.** Kills made at night on the mission clock (from
  {{cfg:time_of_day_effects.night_start_hour|22}}:00 to
  {{cfg:time_of_day_effects.night_end_hour|6}}:00) are worth
  {{cfg:time_of_day_effects.night_kill_bonus|1.5}} times the usual points, air
  and ground alike.
- **Slower convoys.** With weather effects on, AI supply convoys drive at the
  slowest of the rain, snow and storm speed factors
  ({{cfg:weather_effects.snow_speed_multiplier|0.5}} of normal at the
  defaults), whatever the weather in the mission actually is.

## Wreck clearing

While campaign events are on, the server clears wrecks and debris every
{{cfg:campaign_events.junk_removal_interval_secs|300}} seconds. A smoking hulk
you are using as a reference point will not be there for long.

## Server setup

All of it lives under `campaign_events`:

- `enabled` turns new events on. Events already running are still flown and
  cleaned up with it off.
- `barrage_enabled` covers barrages and missile strikes; `ambush_enabled`
  covers ambushes; `enemy_cap_enabled` and `enemy_helo_enabled` the reactive
  CAP and helo patrols and the Commander CAP (see
  [AI Opposition](./ai-opposition.md#server-setup) for the templates).
- The same block holds the capture timing (`capture_time_secs`,
  `capture_cooldown_secs` and the garrison revive settings); see
  [Capturing Objectives](../gameplay/capturing-objectives.md).
- The AI commander's prices and timing are under `smart_commander`.
- `time_of_day_effects` and `weather_effects` are separate top-level blocks.
- `event_probability`, `vip_reward_points`,
  `evacuation_reward_per_civilian` and `cap_orbit_radius_m` are read by
  nothing; the engine warns if they are set.
