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
| Air kill | {{cfg:points.air_kill|350}} |
| Ground kill | {{cfg:points.ground_kill|200}} |
| Long-range SAM bonus (on top of the ground kill) | {{cfg:points.lr_sam_bonus|50}} |
| Capturing an objective | {{cfg:points.capture|1000}} |
| Logistics repair | {{cfg:points.logistics_repair|350}} |
| Logistics transfer | {{cfg:points.logistics_transfer|350}} |
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

### Team kills

Friendly fire is penalised, with a short grace window
({{cfg:points.tk_window|5}} seconds) so a shared kill isn't misattributed. Shoot
your own side and you lose points.

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

You cannot transfer to the other coalition.

## Lives

**Lives are not being enforced on this server.** `limited_lives` is off, which
means **no life is taken when you take off or when you die**, and the lives
block does not appear in `My Status`. Fly as often as you like; the cost of
dying is the points and the time, not a quota.

The system below is what happens when a server turns it on.

### How it works when enabled

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
