# Points and Lives

Points are the campaign's currency. You earn them by doing useful things and
spend them on your aircraft and weapons, support actions, troops and
deployables. Lives are a separate, optional limit on how often you can fly
each kind of aircraft.

> The numbers on this page come from the engine config of the server selected
> at the top of the page. Different servers run different economies. Numbers
> inside the example messages are only examples.

![Diagram of how points flow: kills, captures, logistics, convoy kills, CSAR, holding pay and task-zone kills earn points into your balance, which underdog pay raises and the high-balance rate slows; your balance pays flight costs, actions, troops, deployables, recon passes, AI wingmen and transfers, with the base fund covering what you can't; landing refunds, -delete, returned troops and failed missions give points back, and team kills, lost action groups, lost moved units and civilian shoot-downs take them away.](/api/wiki/images/5f3c8a2e-9b14-4d7e-a6c1-2e8f0b7d4c93)

## Checking your balance

```
-balance                   chat: just the number
-status                    chat: points, streak, career kills, the war
F10 → Info → My Status     in the cockpit
```

`My Status` shows:

```dcsmessage
=== Viper21 ===
Side: Blue
Points: 1250
Kill streak: 2
Career kills: 41
Sides are LOCKED this round

Lives:
standard 2/3 resetting in 04:12:09
intercept 4/4
attack 4/4
logistics 6/6
recon 6/6
```

The side line is the server's rule, not your own count: either
`Sides are LOCKED this round` or `Side switches allowed: 1/round`. The lives
block only appears where lives are enforced.

Every change to your balance arrives as a short message: your new balance, the
change in brackets, and why.

```dcsmessage
1840(48) points for logistics repair (37 km, front line) [x1.30 underdog]
```

## Starting balance

A new pilot starts with {{cfg:points.new_player_join|25}} points, or more if
they join late (see [Joining late](#joining-late)). Your balance belongs to the
round: a new round starts everyone again.

## Earning points

| What | Points |
| --- | --- |
| Air kill (aircraft or helicopter) | {{cfg:points.air_kill|25}} |
| Ground kill | {{cfg:points.ground_kill|2}} × what it was (see below) |
| Long-range SAM tracking radar, on top of its ground kill | {{cfg:points.lr_sam_bonus|5}} |
| Capturing an objective, split between the capturing pilots | {{cfg:points.capture|15}} |
| Logistics repair delivered | {{cfg:points.logistics_repair|25}} and up |
| Supply transfer delivered | {{cfg:points.logistics_transfer|15}} and up |
| Killing a supply convoy vehicle | {{cfg:points.convoy_interdiction_points|10}} |
| CSAR: delivering someone else's downed pilot | {{cfg:csar.rescue_reward|0}} |
| Holding pay, every {{cfg:smart_commander.tick_period_secs|60}} s in a slot | up to {{cfg:smart_commander.holding_bonus_per_objective|5}} |
| Periodic award, every {{cfg:points.periodic_point_gain[1]|0}} s in a slot | {{cfg:points.periodic_point_gain[0]|0}} |

A few things worth reading off that table:

- **Ground work pays.** A capture is worth several kills, and the campaign pays
  you for moving the front line, not for padding a K:D.
- **Logistics pays like combat**, and pays more the further you haul it (below).
- **Convoy interdiction is small per truck but constant.** It is also what
  starves an enemy sector; see [Materiel & the War Economy](./war-economy.md).
- **Flying your own ejected pilot home** gets your life back but no reward.

A kill message looks like this. An AI victim says `kill`, a player victim
names them:

```dcsmessage
1275(+25) points, killed Viper21
1278(+3) [x1.5 streak] points, kill
```

### What a kill is worth

An air kill is a flat {{cfg:points.air_kill|25}}. A ground kill is the ground
kill rate times what you destroyed. The unit takes the highest multiplier among
its types, and anything not listed counts ×1:

| Target | Multiplier |
| --- | --- |
| Infantry, unarmed vehicles | ×{{cfg:economy.ground_kill_values[0][1]|0.5}} |
| Logistics vehicles | ×{{cfg:economy.ground_kill_values[2][1]|0.75}} |
| APCs, AAA | ×{{cfg:economy.ground_kill_values[3][1]|1}} |
| Armour, artillery, SAM launchers | ×{{cfg:economy.ground_kill_values[5][1]|1.5}} |
| Ships, early-warning and search radars | ×{{cfg:economy.ground_kill_values[8][1]|2}} |
| SAM tracking radars | ×{{cfg:economy.ground_kill_values[11][1]|2.5}} |

Every ground kill is worth at least 1 point. The long-range SAM bonus is
added on top for a long-range SAM's tracking radar. Where the server runs time-of-day
effects, a kill at night (mission time, {{cfg:time_of_day_effects.night_start_hour|22}}:00 to
{{cfg:time_of_day_effects.night_end_hour|6}}:00) is worth
×{{cfg:time_of_day_effects.night_kill_bonus|1.5}}.

**Kill streaks.** Where the server sets streak bonuses, your own kills since
your last takeoff raise the value of the next one; the message shows
`[x1.5 streak]`. Taking off again starts the streak from zero.

**Your task zone.** A kill you make yourself inside the zone of the task you
were given at slot-in pays ×1.5. See [Your Task & Rookie Mode](./your-task.md).

### Shared kills

A kill several pilots worked on is split by **hits**. The hit that killed it
counts {{cfg:economy.killing_blow_weight|1}} extra. The shares add up to the
kill's value, so nobody gets a full kill for tagging a target someone else
destroyed. If nobody registered a hit, everyone who fired at it in the last
3 minutes shares it evenly.

### Kills by your deployed AI

SAMs, troops and action groups you put down earn you a share when they kill:
×{{cfg:economy.owned_ai_kill_fraction|0.5}} while you're in a slot and
×{{cfg:economy.owned_ai_unattended_fraction|0.25}} while you aren't. The
message says `your deployed ai kill`. These don't count toward your streak or
your task zone.

A Combined Arms commander driving a ground vehicle is paid for its kills in
full, like a pilot.

### Bailing out of a fight

Leaving your slot in the air does not save you. Anyone who hit you in the last
10 minutes, or fired at you in the last 90 seconds, gets the kill. If nobody
did but an enemy aircraft is within {{cfg:slot_leave_kill_radius_m|15000}} m,
that pilot is credited with it. Leaving on the ground outside a friendly
objective only counts as a loss if you were hit in the last 10 minutes.

### Provisional points

Where the server runs provisional points ({{cfg:points.provisional|no}} here),
kill points you earn in the air are held until you **land at a friendly
objective**. Die, eject or leave the slot first and they are lost. The kill
message says `provisional`, and the landing debrief shows what you banked or
what is still pending:

```dcsmessage
Kill points pending: 50 -- land at a friendly objective to bank them
```

### Logistics pays the pilot who flew it

Repair and supply-transfer pay goes to the pilot who **loaded** the crate, not
whoever pressed unpack. If someone else on your side unpacks it, they get
{{cfg:economy.unpacker_share|0.25}} of the pay and the hauler gets the rest.

The pay grows with the haul. It starts at the base rate and rises by up to
×{{cfg:economy.delivery_max_bonus|1}} more for a haul of
{{cfg:economy.delivery_ref_km|40}} km or longer. Delivering to a
**front-line** base adds another
+{{cfg:economy.front_line_bonus|0.5}}: one that's under threat, or within
{{cfg:economy.front_line_km|25}} km of an enemy base. The message shows the
distance and whether it counted as front line.

### Underdog pay

A side that is behind gets paid more for the same work: kills, logistics,
captures, CSAR, convoy kills, holding pay and periodic awards. "Behind" means
holding less than half the contested map, or being outnumbered in the air once
at least {{cfg:economy.underdog_min_players|4}} pilots are in a slot. The raise
goes up to +{{cfg:economy.underdog_max_bonus|0.5}} (×1.5). The AI commander's
treasury income gets the same raise. When it applies, the message says so,
e.g. `[x1.30 underdog]`.

### High balances earn at a lower rate

Once your balance passes {{cfg:economy.wealth_cap_ratio|4}}× your side's
typical balance (the median of its pilots who have earned or spent anything),
anything you earn above that line is paid at
{{cfg:economy.wealth_taper|0.5}}×. Nothing is taken away; a big lead just grows
more slowly. The message tags it `[high-balance rate]`.

### Joining late

A new pilot starts with the larger of the join grant and
{{cfg:economy.late_joiner_fraction|0.5}}× their side's typical balance, so
joining on day three doesn't mean starting from nothing. The part above the
normal join grant can be spent but **not transferred**.

### Holding pay

Every {{cfg:smart_commander.tick_period_secs|60}} seconds each pilot **in a
slot** gets holding pay: {{cfg:smart_commander.holding_bonus_per_objective|5}}
points times the share of the map their side holds, rounded, and at least 1
for a side that holds anything. It arrives silently. Spectators get nothing.
It only runs on servers with the smart commander on.

## Spending points

### Your aircraft and weapons

Where the server prices airframes and weapons, a flight costs its airframe plus
every priced store on board. When you start to taxi you get the bill:

```dcsmessage
FLIGHT COST
  F-16C_50 airframe: 100
  2 x AIM-120C AMRAAM: 60  (30 each)
  TOTAL: 160
Your points: 1250  |  Kutaisi base fund: 4000
After takeoff: 1090 points.
```

- It is **charged at takeoff** from a friendly objective. Your own points pay
  first and the base's fund covers what they can't.
- **Land at a friendly objective** and you get back what is still on the
  aircraft (airframe and unfired stores), to whoever paid it. What you fired
  stays paid for. The landing debrief shows both numbers.
- Taking a slot already checks the airframe price against your points plus the
  base fund. If you can't cover it:

```dcsmessage
F-16C_50 costs 300, you have 120 -- lifeline: Su-25T, TF-51D fly free while you're short
```

- Where the cost is **strict** ({{cfg:points.strict|no}} here), the taxi panel
  warns `!! NOT ENOUGH POINTS -- DO NOT TAKE OFF !!` when you can't pay, and
  taking off anyway removes the aircraft:
  `Not enough points to pay for this flight (see the FLIGHT COST panel from taxi) -- aircraft removed.`
  Where it isn't strict, your balance simply goes negative.

### Lifeline

Where the server has it on, running out of points does not ground you. While
your **own** balance (base funds don't count) is below the lifeline line, the
aircraft is **free**, with a free weapons budget; load past it and the extra is
charged as usual (under strict points: unload it at the rearm menu or you can't
take off). Only the listed airframes qualify, and aircraft priced out of the
campaign's era stay locked. The taxi panel says so:

```dcsmessage
LIFELINE FLIGHT
You have 40 points, so this Su-25T flies free, weapons included.
Kills, logistics and captures earn your way back.
```

### Actions

Costs are shown in the F10 menu label, e.g. `E-3A AWACS(100 pts)`; that label
is the authority. A side high on the [war effort bar](./war-effort.md) pays 15%
less than the label. This server's actions:

{{list:actions.Blue|_The action list is unavailable for this server; check the F10 Actions menu._}}

{{list:actions.Red|_The action list is unavailable for this server; check the F10 Actions menu._}}

Several actions carry a **penalty** as well as a cost. If the group you called
is destroyed, the pilot responsible for it (the last one to give it orders) is
charged the penalty: `for the loss of action group 12345`. A Move action can
carry one too: move someone else's unit and if it dies, you pay.

### Troops and deployables

Troop squads cost points when you load them, and deployables can carry a
points price as well as their crates and materiel; both are charged like a
flight (your points first, then the base fund). Prices per squad:

{{list:troops.Blue|_The troop list is unavailable for this server._}}

{{list:troops.Red|_The troop list is unavailable for this server._}}

Deployables are listed in [Deployable Units](../reference/deployables.md).

### Other costs

| | Cost |
| --- | --- |
| AI helo troop insertion | {{cfg:helo_insertion.troop_mission_cost|0}} + the troop's own cost |
| AI helo resupply run | {{cfg:helo_insertion.supply_mission_cost|50}} |
| Player recon pass | {{cfg:player_recon.cost|0}} |
| AI wingman | {{cfg:air_life.wingman.cost|0}} |
| Cargo crates, CSAR, all reports | free |

### Getting points back

- **Landing** at a friendly objective: the unspent part of your flight cost.
- **`-delete <group id>`** on a unit you deployed: half its cost, see
  [Chat Commands](./chat-commands.md).
- **Returning troops** to a base: what the squad is still worth.
- **An AI flight you paid for and sent home with an RTB action**: a quarter of
  its cost once it gets there.
- **Missions that never deliver**: an [AI helo mission](../advanced/helo-missions.md)
  shot down or lost, a reinforcement convoy stuck on the road, or anything a
  server restart cut off before it delivered, refunds what you paid.

## Penalties

### Team kills

Kill something on your own side and you lose the kill's full value. Earlier
team kills make it worse: each one you made in the last
{{cfg:points.tk_window|24}} hours adds the full value again, and older ones
count half as much for every further {{cfg:points.tk_window|24}} hours. After
eight such windows they are forgotten.

```dcsmessage
1180(-25) points, you have killed a friendly unit
```

Team-killing a **player** in the air also costs you lives, starting with the
role your victim was flying, and more if you have done it recently. If you run
out of lives to lose, you are sent to spectators 30 seconds later:

```dcsmessage
1100(-50) points, you have team killed Viper21.
You have lost
1 standard life
```

Team kills earn no XP either.

### Civilian traffic

Where the server runs civilian air traffic, shooting an airliner down costs you
{{cfg:air_life.civil_traffic.shootdown_penalty_points|500}} points, and your
side's treasury pays too. Everyone is told who did it.

## Point transfers

```
-transfer <amount> <player>
-transfer <amount> objective:<objective>
```

Transfers go to another pilot on your coalition, or **into a base's fund** (a
base your side holds). Base funds pay for flights, troops and deployables when
a pilot's own points run short, and the AI commander tops them up. See
[Chat Commands](./chat-commands.md) for the replies.

Use it to:

- Pool for something expensive nobody can afford alone.
- Hand points to a new player who has just burned their starting balance.
- Fund a base that is about to be fought over.

You cannot transfer to the other coalition, and a late joiner's head start
can't be transferred.

A base's fund belongs to whoever holds the base. Where the server sets a
per-side budget, each side's bases start the round with it split between them
({{cfg:objective_start_points.Blue|not set}} for Blue,
{{cfg:objective_start_points.Red|not set}} for Red); otherwise each base starts
with {{cfg:smart_commander.objective_start_points|500}}. When a base is
captured, the new owner keeps {{cfg:economy.capture_fund_keep|0.25}} of its
fund (capped at what their own bases of that kind hold). The rest is lost with
the base.

## Lives

**Lives enforced on this server: {{cfg:limited_lives|yes}}.**

This differs between campaigns, so check the server selector at the top of the
page before planning around it. When it reads **no**, nothing is taken on
takeoff and the lives block does not appear in `My Status`; the cost of dying
is the points and the time, not a quota. When it reads **yes**, everything
below applies.

### How it works

Lives are tracked **separately per role**, so running dry in a fighter does not
ground you from flying logistics:

| Role | Lives | Refill period |
| --- | --- | --- |
| Standard | {{cfg:default_lives.Standard[0]|3}} | {{cfg:default_lives.Standard[1]|21600}} s |
| Intercept | {{cfg:default_lives.Intercept[0]|4}} | {{cfg:default_lives.Intercept[1]|21600}} s |
| Attack | {{cfg:default_lives.Attack[0]|4}} | {{cfg:default_lives.Attack[1]|21600}} s |
| Recon | {{cfg:default_lives.Recon[0]|6}} | {{cfg:default_lives.Recon[1]|21600}} s |
| Logistics | {{cfg:default_lives.Logistics[0]|6}} | {{cfg:default_lives.Logistics[1]|21600}} s |

Which role an airframe belongs to is per aircraft; see the
[Aircraft Roster](../reference/aircraft-roster.md).

- A life is taken **on takeoff from a friendly objective**. A takeoff from
  anywhere else takes nothing. The cockpit shows:

```dcsmessage
life taken
standard 2/3 resetting in 05:59:59
```

- **Land at any friendly objective** and you get it back. Die, eject, or leave
  the slot away from a friendly objective and it is gone.
- A role's pool refills **completely** once its refill period has passed since
  the first life you took from it. `-lives` shows the clock.
- With no lives left for a role, its slots are refused:

```dcsmessage
you have no Standard lives remaining. standard 0/3 resetting in 03:10:44
```

### Getting a life back

[CSAR](../f10-menu/csar.md) is the other way. A downed pilot who is recovered,
by a helo or by reaching a friendly zone themselves, gets the life back for the
role they lost it in. If that role is already full, the credit cascades down a
tier rather than being wasted.

## Side switching

How many times you can change side, and whether picking the other side's slot
does it for you, is covered in
[Joining a Team](../getting-started/joining-team.md#side-switching).

## Admin commands

```
-admin balance <player>
-admin set-points <amount> <player>
-admin addpoints <player> <n>
-admin lives <player> <standard|intercept|logistics|attack|recon> <n>
-admin reset-lives <player>
-admin reset-lives-all
```

## See Also

- [Chat Commands](./chat-commands.md)
- [Actions Menu](../f10-menu/actions.md): what the points buy
- [Materiel & the War Economy](./war-economy.md): the economy points *don't* pay for
- [Combat Search & Rescue](../f10-menu/csar.md)
- [Aircraft Roster](../reference/aircraft-roster.md): role per airframe
- [XP, Levels & Squadrons](./progression.md): what you keep between rounds
