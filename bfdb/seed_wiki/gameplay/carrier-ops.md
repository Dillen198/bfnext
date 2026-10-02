# Carrier Operations

A carrier group is an objective like any other — it can be damaged, repaired,
captured and lost — but it is the only one you can **sail**. This page is about
operating one. For taking one off the enemy, see
[Capturing Objectives](./capturing-objectives.md).

## Finding it

```
F10 → Info → Navaids Directory     TACAN / ICLS / ACLS / Link-4, per deck
F10 → Objectives → Base Detail     position, health, supply, airframes, BRC
F10 → Info → Weather               the recovery case
```

The carrier's card carries a **`BRC:`** line — the base recovery course — and
points you at `Info → Weather` for the recovery case. Read both before you
depart, not on the way back.

Each **ship** in the task force gets its own navaid set, so a group with two
decks has two TACANs. A US CVN gets the full CATOBAR suite — TACAN, ICLS, ACLS
and Link-4 on {{cfg:navaids.carrier_link4_mhz|336}} MHz. An LHA/LHD gets TACAN
and ICLS only; it has no cats or traps, so there is nothing for ACLS to fly.

**Check the channels before you launch.** They are generated per round, not
fixed per ship.

## Sailing it

```
F10 → Actions>> → Carrier Waypoint → [your map mark]
```

Place an F10 map mark where you want the group, then send it. Free.

The group transits at **{{cfg:carrier.movement_speed|25}} m/s**. Plan ahead —
this is not a teleport, and a carrier that needs to be somewhere in 40 minutes
needs to be told now.

Practical use:

- **Move it in before a push**, so the strike package isn't spending half its
  fuel in transit.
- **Move it out when it's threatened.** A carrier inside an enemy anti-ship
  envelope is a very expensive target, and a sunk one costs the coalition its
  deck slots.
- **Everyone shares it.** Coordinate before you move a deck other people are
  recovering to.

## Striking from it

```
F10 → Actions>> → Naval Strike → [enemy objective]
```

Cruise missiles off your nearest friendly carrier in range with ammunition
aboard, against an enemy objective you pick from a list. It costs points — the
figure is in the menu label.

It needs a **live** carrier (health above 0) with rounds left. A sunk or
stripped task force cannot shoot.

## Keeping it alive

A carrier's health falls when it takes hits. Below 75% it needs help, and there
are three ways to give it.

### Auto-repair from its naval base

Every carrier auto-links to the nearest friendly **naval base** at mission
start. If that base is still yours and holding at least
**{{cfg:carrier.repair_cost|5000}}** materiel, repair starts on its own when the
carrier drops below 75%, paid out of the base's stock.

This is the main mechanism, and it is a reason to care about naval bases you
otherwise never visit: **lose the base and the carrier stops healing.**

### Carrier repair crates

Request a **Carrier Repair** crate from the cargo menu, then fly or air-drop it
to the ship. A single crate runs the repair over about
**{{cfg:carrier.repair_time|1800}} seconds** — roughly 30 minutes. Each
additional crate delivered while a repair is running divides that time (floored
at a minute), so **stacking crates is how you get a deck back in a hurry**.

See [Cargo Operations](../f10-menu/cargo.md) and
[C-130 Hercules & Airdrop](../advanced/c130-airdrop.md).

### Repair / Respawn Carrier actions

Some servers configure explicit `Repair Carrier` and `Respawn Carrier` actions
on the linked naval base, paid out of that base's supplies — respawn costs
**{{cfg:carrier.respawn_cost|15000}}** and requires the carrier to be fully sunk
first. They are **not** on this server's action list unless you can see them in
`Actions>>`; when they are absent, use crates and auto-repair.

## Flying off it

- **Deck slots follow ownership.** Capture an enemy carrier and its slots become
  yours.
- **A captured carrier keeps the aircraft it had aboard**, including types your
  coalition doesn't otherwise operate — but those airframes stay grounded until
  the repairs finish. Trying to slot one early gets you a "flyable once carrier
  repairs finish" message.
- **Airframe counts are finite.** The `A:` figure in the base status line is
  what's actually on the deck. See
  [Materiel & the War Economy](./war-economy.md).

## Cargo from the deck

Carriers have their own crate spawn point, so crate requests made on the deck
land somewhere you can actually pick them up rather than in the water. Crate
handling otherwise works exactly as it does ashore —
see [Cargo Operations](../f10-menu/cargo.md).

## Losing it

A carrier group at 0% health is sunk: no slots, no strikes, no navaids. It comes
back only through a configured `Respawn Carrier` action, or by the round
resetting.

The enemy takes a carrier by **sinking its SUPPLY ship** — the eligibility rule
for a carrier group is logistics at 0%, not health at 20% — and then either
putting capture troops aboard or taking the naval base it is linked to. The
[Capture Advisor](../f10-menu/objectives.md) states which of those is
outstanding. Read the full sequence in
[Capturing Objectives](./capturing-objectives.md).

## See Also

- [Capturing Objectives](./capturing-objectives.md) — taking and losing a carrier
- [Navaids & Approaches](./navaids.md) — TACAN/ICLS/ACLS channels
- [Actions Menu](../f10-menu/actions.md) — Carrier Waypoint and Naval Strike
- [Materiel & the War Economy](./war-economy.md) — why the naval base matters
