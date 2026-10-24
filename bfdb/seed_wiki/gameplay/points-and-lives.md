# Points and Lives

Points are the campaign's currency. You earn them by doing useful things and
spend them on support assets, deployables and AI missions. Lives are a separate,
optional throttle on how often you can fly — **switched off on this server right
now** (see below).

> The numbers on this page come from the engine config of the server selected at
> the top of the page. Different servers run different economies.

## Earning points

| What | Points |
| --- | --- |
| Air kill | {{cfg:points.air_kill|25}} |
| Ground kill | {{cfg:points.ground_kill|2}} |
| Long-range SAM bonus (on top of the ground kill) | {{cfg:points.lr_sam_bonus|5}} |
| Capturing an objective | {{cfg:points.capture|15}} |
| Logistics repair | {{cfg:points.logistics_repair|25}} |
| Logistics transfer | {{cfg:points.logistics_transfer|15}} |
| Killing a supply convoy truck | {{cfg:points.convoy_interdiction_points|10}} |
| CSAR pilot delivered | {{cfg:csar.rescue_reward|50}} |
| Starting balance, new player | {{cfg:points.new_player_join|30000}} |

A few things worth reading off that table:

- **Ground work pays.** A ground kill is a large fraction of an air kill, and a
  capture is worth several of either. The campaign pays you for moving the front
  line, not for padding a K:D.
- **Logistics pays like combat.** Repairing and transferring supply are worth
  about what an air kill is. The crate run nobody wants to fly is not charity.
- **Convoy interdiction is small per truck but constant.** It is also the thing
  that actually starves an enemy sector — see
  [Materiel & the War Economy](./war-economy.md).

### What a kill is worth

An air kill is a flat {{cfg:points.air_kill|25}}. A ground kill is priced by
what you destroyed, as a multiple of the ground kill rate: infantry and
unarmed trucks ×0.5, logistics vehicles ×0.75, APCs and AAA ×1, armour,
artillery and SAM launchers ×1.5, ships, early-warning and search radars ×2,
SAM tracking radars ×2.5 (and the long-range SAM bonus on top of that).

A shared kill is split by **hits**. The hit that actually killed it counts
double. The shares add up to the kill's value, so nobody gets a full kill for
tagging a target someone else destroyed.

Kills made by **your deployed AI** (SAMs, troops, action groups) pay
{{cfg:economy.owned_ai_kill_fraction|0.5}}× while you're in a slot and
{{cfg:economy.owned_ai_unattended_fraction|0.25}}× while you aren't.

### Logistics pays the pilot who flew it

Repair and supply-transfer pay goes to the pilot who **loaded** the crate, not
whoever pressed unpack. If someone else unpacks it, they get
{{cfg:economy.unpacker_share|0.25}} of the pay and the hauler gets the rest.

The pay grows with the haul. The base rate is for a delivery made where the
crate was loaded, and it rises to double for a haul of
{{cfg:economy.delivery_ref_km|40}} km or more. Delivering to a **front-line**
base adds another +50%: one that's under threat, or within
{{cfg:economy.front_line_km|25}} km of an enemy base. The points message shows
the distance and whether it counted as front line.

### Underdog pay

A side that is behind gets paid more for the same work: kills, logistics,
captures, CSAR, convoy kills and holding pay. "Behind" means holding less than
half the contested map, or being outnumbered in the air once at least
{{cfg:economy.underdog_min_players|4}} pilots are up. The raise goes up to
+{{cfg:economy.underdog_max_bonus|0.5}} (×1.5). The AI commander's treasury
income gets the same raise. When it applies, the points message says so, e.g.
`[x1.30 underdog]`.

### High balances earn at a lower rate

Once your balance passes {{cfg:economy.wealth_cap_ratio|4}}× your side's
typical balance (the median of its active pilots), anything you earn above that
line is paid at {{cfg:economy.wealth_taper|0.5}}×. Nothing is taken away; a big
lead just grows more slowly. The message tags it `[high-balance rate]`.

### Joining late

A new pilot starts with the larger of the join grant and
{{cfg:economy.late_joiner_fraction|0.5}}× their side's typical balance, so
joining on day three doesn't mean starting from nothing. The part above the
normal join grant can be spent but **not transferred**.

### Holding pay

Every few minutes each pilot **in a slot** gets holding pay, scaled by how much
of the map their side holds. A side that holds anything at all gets at least 1
point. Spectators and AFK players get nothing.

### Team kills

Shoot your own side and you lose the kill's full value in points. Repeat
offences cost more: earlier team kills are remembered, and their weight halves
every {{cfg:points.tk_window|5}} hours.

## Spending points

Costs are always shown in the F10 menu label, e.g. `E-3A AWACS(100 pts)`. That
label is the authority — it is generated from the same config this page reads.

Typical costs on the live action set:

| Action | Cost |
| --- | --- |
| E-3A AWACS | 100 |
| KC-135 tanker (boom or basket) | 50 |
| B-1B bomber attack | 100 |
| JTAC drone | 25 |
| Naval cruise missile strike | 50 |
| Move units/troops | 10 |
| AWACS / tanker / drone waypoint | 5–10 |
| Carrier waypoint | free |
| Add / Remove Task | free |

Plus things that aren't in the action list:

| | Cost |
| --- | --- |
| AI helo troop insertion | {{cfg:helo_insertion.troop_mission_cost|0}} + the troop's own cost |
| AI helo resupply run | {{cfg:helo_insertion.supply_mission_cost|50}} |
| Player recon pass | {{cfg:player_recon.cost|0}} |
| Artillery fire mission | free — see [Artillery](../advanced/artillery.md) |
| Cargo, troops, CSAR, all reports | free |

**Penalties.** Several actions carry a penalty as well as a cost — you are
charged again if the asset you called is lost early. An AWACS shot down shortly
after launch costs you twice.

**Refunds.** An [AI helo mission](../advanced/helo-missions.md) that never
delivers — shot down, or lost to the terrain — hands its points back, the
troop cost included. Nothing arrives, but you are not charged for nothing.

**Lifeline.** Where the server has it switched on, running out of points does
not ground you. While your own balance is below the server's lifeline
threshold, the aircraft you slot is **free**, with a small **free weapons
budget** — load past it and the extra is charged as usual (under strict points:
unload it at the rearm menu or you can't take off). Airframes priced out of the
campaign's era stay locked. The taxi panel says `LIFELINE FLIGHT` when it
applies; kills, logistics and captures earn your way back above the line.

Deployables are paid for in **crates and materiel**, not points. See
[Deployable Units](../reference/deployables.md) and
[Materiel & the War Economy](./war-economy.md).

## Checking your balance

```
-balance                   chat
-status                    chat, with more context
F10 → Info → My Status     in the cockpit
```

`My Status` shows your balance, kill streak, career kills, and how many side
switches you have left.

## Point transfers

```
-transfer <amount> <player>
-transfer <amount> <objective>
```

Transfers to another player on your coalition, or **into an objective's own
balance** — objectives fund the AI commander's actions, so this is a way for
players to pay for something the commander can't yet afford. Each side's
objectives start the round with
{{cfg:objective_start_points.Blue|500000}} between them.

Use it to:

- Pool for something expensive nobody can afford alone.
- Hand points to a new player who has just burned their starting balance.
- Fund the objective that is about to be attacked.

You cannot transfer to the other coalition, and a late joiner's head start
can't be transferred (see above).

A base's balance belongs to whoever holds the base. When it is captured, the
new owner keeps {{cfg:economy.capture_fund_keep|0.25}} of it (capped at what
their own bases of that kind hold). The rest is lost with the base.

## Lives

**Lives enforced on this server: {{cfg:limited_lives|no}}.**

This differs between campaigns, so check the server selector at the top of the
page before planning around it. The 2008 Caucasus campaign runs with lives
**on** — losing an airframe costs you a slot in that role for the rest of the
refill window. The modern Syria campaign runs with them **off**.

When it reads **no**, nothing is deducted on takeoff or death and the lives
block does not appear in `My Status`. Fly as often as you like; the cost of
dying is the points and the time, not a quota.

When it reads **yes**, everything below applies.

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

Which role an airframe belongs to is per-aircraft — see the
[Aircraft Roster](../reference/aircraft-roster.md).

Each role's pool refills on its own **rolling timer** from when the first life
was taken, not at round start. `-lives` shows the current count and the clock.

A life is consumed **on takeoff from a friendly objective**, not on death — so
an aborted sortie still costs one.

### Getting a life back

[CSAR](../f10-menu/csar.md) is how. A downed pilot who is recovered — by a helo,
or by reaching a friendly zone themselves — gets the life back for the role they
lost it in. If that role is already full, the credit cascades down a tier rather
than being wasted.

## Side switching

You get **{{cfg:side_switches|1}}** side switch per round
(`-switch blue` / `-switch red`).

A server can also lock sides entirely, in which case you fly the side you
registered with until the round resets and `-switch` is refused.
`F10 → Info → My Status` states which policy is in force right now — it prints
either "Sides are LOCKED this round" or how many switches you have left.

## Admin commands

```
-admin balance <player>
-admin set-points <amount> <player>
-admin reset-lives <player>
-admin reset-lives-all
```

## See Also

- [Chat Commands](./chat-commands.md)
- [Actions Menu](../f10-menu/actions.md) — what the points buy
- [Materiel & the War Economy](./war-economy.md) — the economy points *don't* pay for
- [Combat Search & Rescue](../f10-menu/csar.md)
- [Aircraft Roster](../reference/aircraft-roster.md) — role per airframe
