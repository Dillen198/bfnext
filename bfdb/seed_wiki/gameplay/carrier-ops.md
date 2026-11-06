# Carrier Operations

A carrier group is an objective like any other: it can be damaged, repaired,
captured and lost. But it is the only one you can **sail**. This page is about
operating one. For taking one off the enemy, see
[Capturing Objectives](./capturing-objectives.md).

Carrier groups are drawn on the F10 map for their owner only. The enemy has to
find yours.

## Finding it

| Where | What it tells you |
| --- | --- |
| Slot-entry **carrier ATIS** | BRC, recovery case, wind and altimeter, 15 seconds after you take a deck slot |
| **Info → Navaids Directory** | TACAN, ICLS, ACLS and Link-4, one line per deck |
| **Objectives → Base Detail → (carrier)** | Position, health, supplies, `BRC:` line and the deck navaids |
| **Info → Weather** | The carrier ATIS again, with winds aloft |

```f10menu
{"root":"F10 Other","items":[{"label":"Info","items":[
 {"label":"My Status"},
 {"label":"Situation"},
 {"label":"Support & Radios"},
 {"label":"Supply Convoys"},
 {"label":"Navaids Directory","says":"=== Friendly Navaids ===\nCVN Group [BCARRIER-1] (231°/88nm): TACAN 3Y BCA  ICLS 1  Link4 336.0  ACLS\nCVN Group [BCARRIER-3] (231°/88nm): TACAN 4Y BC1  ICLS 2\nFARP Kilo (047°/23nm): TACAN 5Y FAR  NDB 205","note":"Sorted by range. A task force with a CVN and an LHA has two lines: the LHA gets TACAN and ICLS only."},
 {"label":"Time & Server"},
 {"label":"Weather","says":"CARRIER ATIS - CVN GROUP\nBRC: 084°M\nRecovery: CASE I\nTemperature: +18.0°C (+64°F)\nSurface Wind: 219° (213°M) @ 12 kts / 6 m/s\nVisibility: 10 km (+) / 6 SM (+)\nClouds: base 4,921 ft MSL | 984 ft thick | density 3/10\nQNH: 1015 hPa | 29.97 inHg | 761 mmHg\nWinds aloft AGL (°T/kt): 1000ft 225/14, 2000ft 230/17, 6600ft 245/24, 26000ft 260/48","note":"From a deck slot. The winds-aloft table at standard levels follows."},
 {"label":"Help","items":[{"label":"Carrier Groups","note":"A short help page on boarding a carrier."}]}
]}]}
```

The slot-entry version, without winds aloft:

```dcsmessage
CARRIER ATIS - CVN GROUP
BRC: 084°M
Recovery: CASE I
Temperature: +18.0°C (+64°F)
Surface Wind: 219° (213°M) @ 12 kts / 6 m/s
Visibility: 10 km (+) / 6 SM (+)
Clouds: base 4,921 ft MSL | 984 ft thick | density 3/10
QNH: 1015 hPa | 29.97 inHg | 761 mmHg
```

- **BRC** in the ATIS is the ship's heading, **magnetic**. The `BRC:` line on
  the Base Detail card is the same heading in **true** degrees, so on most maps
  the two differ by the magnetic variation. Fly the ATIS figure.
- **Recovery** is worked out from the cloud base: no cloud or a base at
  3,000 ft MSL or above is CASE I, 1,000 to 3,000 ft is CASE II, below 1,000 ft
  is CASE III. See [Weather, ATIS & Time](./weather.md).

Each **ship** in the task force gets its own navaid set, so a group with two
decks has two TACANs. A US CVN gets the full CATOBAR suite: TACAN, ICLS, ACLS
and Link-4 on {{cfg:navaids.carrier_link4_mhz|336}} MHz. An LHA/LHD gets
TACAN and ICLS only; it has no cats or traps, so there is nothing for ACLS to
fly. The TACAN band is the server's ground-beacon band
({{cfg:navaids.tacan_band|Y}} by default), not necessarily X.

**Check the channels before you launch.** They are generated from who owns
what, so they can change when bases change hands. See
[Navaids & Approaches](./navaids.md).

## Sailing it

The carrier waypoint action in the **Actions** menu sails your task force:

1. Place an F10 map mark where you want the group.
2. Open the carrier waypoint action, pick your task force (listed as
   `<name> Carrier`), then the mark. The cost, if any, is in the menu label.

Only your own carrier groups are listed. The group transits at
**{{cfg:carrier.movement_speed|5}} m/s**. Plan ahead: this is not a teleport,
and a carrier that needs to be somewhere in 40 minutes needs to be told now.
After a server restart the task force is put back where it was.

Practical use:

- **Move it in before a push**, so the strike package isn't spending half its
  fuel in transit.
- **Move it out when it's threatened.** A carrier inside an enemy anti-ship
  envelope is a very expensive target, and a sunk one costs the coalition its
  deck slots.
- **Everyone shares it.** Coordinate before you move a deck other people are
  recovering to.

## Striking from it

The naval strike action in **Actions** fires land-attack cruise missiles from
your nearest task force in range at an enemy objective you pick from a list.
It costs points; the figure is in the menu label.

The ships really launch: the engine looks through every ship of the task
force for land-attack cruise missiles (a Ticonderoga or an Arleigh Burke
carries them). It needs a **live** task force (health above 0) in range with
enough missiles on one ship group. When it cannot fire it tells you why:

- `No friendly carrier group in range`
- `None of our ships in range carries land-attack cruise missiles (a Ticonderoga or an Arleigh Burke does)`.
  In DCS the red fleet's missiles are anti-ship only, so a red task force may
  never be able to.
- `Our ships in range have only 4 cruise missiles between them, not 6 on one ship group`

See [Actions Menu](../f10-menu/actions.md) and
[Action Types](../reference/action-types.md).

## Keeping it alive

A carrier's health falls when its ships take hits. There are three ways to
bring it back.

### Auto-repair from its naval base

Every carrier is linked to a friendly **naval base**. When the carrier drops
below 75% health, and that base is still yours and holds at least
**{{cfg:carrier.repair_cost|5000}}** materiel, a repair starts on its own:

```dcsmessage
NAVAL: CVN Group auto-repair initiated (62% health). Supplies deducted from naval base.
```

It takes **{{cfg:carrier.repair_time|1800}} seconds**. When it finishes the
carrier is back to full health and its sunk ships are refloated:

```dcsmessage
CVN Group has been fully repaired and is operational
```

If the carrier is at 25% or less and the base cannot pay, your side is warned
instead:

```dcsmessage
WARNING: CVN Group critically damaged (21% health)! Insufficient supplies at naval base for auto-repair.
```

This is the main mechanism, and it is a reason to care about naval bases you
otherwise never visit: **lose the base, or let its materiel run down, and the
carrier stops healing.**

### Carrier repair crates

Fly or air-drop your side's **Carrier Repair** crate to the ship. A delivered
crate goes to your nearest carrier group and starts a repair there:

```dcsmessage
Carrier repair crate delivered to CVN Group (1 crate(s)) - ~30m remaining
```

One crate runs the repair over **{{cfg:carrier.repair_time|1800}} seconds**.
Each more crate delivered while it runs divides that time by the crate count
(two crates halve it, three take a third), with a floor of one minute, so
**stacking crates is how you get a deck back in a hurry**. A repair the naval
base started counts as no crates: the first crate you add to it does not
shorten it, the second halves it.

Carriers have their own crate spawn point on the deck, so crates you spawn
there land where you can pick them up rather than in the water. See
[Cargo Operations](../f10-menu/cargo.md) and
[C-130 Hercules & Airdrop](../advanced/c130-airdrop.md).

### Repair Carrier / Respawn Carrier actions

Some servers configure explicit carrier repair and respawn actions, paid out
of the linked naval base's materiel rather than your points. Both need the
carrier and the naval base to be yours.

- **Repair** costs {{cfg:carrier.repair_cost|5000}} materiel and finishes at
  once: health back to 100% and lost escorts refloated. It refuses with
  `the carrier is not damaged`, or `the carrier is destroyed, it needs a respawn not a repair`.
- **Respawn** costs {{cfg:carrier.respawn_cost|15000}} materiel and needs the
  carrier fully sunk first (`Carrier is not destroyed (health: 40%)` otherwise).
  The replacement ships put to sea off the naval base and sail back to their
  station, so they are not on station until they get there.

Short of materiel, either says
`Not enough supplies at Naval Base to repair carrier (need 5000, have 3200)`
(or `... to respawn carrier ...`). If you cannot see these actions in your
Actions menu, the server does not offer them; use crates and auto-repair.

## Flying off it

- **Deck slots follow ownership.** Capture an enemy carrier and its slots
  become yours.
- **A captured carrier keeps the aircraft it had aboard**, including types your
  coalition doesn't otherwise operate, but those airframes stay grounded until
  the carrier is back to 100% health. Slotting one early gets you, in chat:

```dcsmessage
FA-18C_hornet was captured here -- it will be flyable once this objective is repaired
```

- **Airframe counts are finite.** The aircraft figure on the Base Detail card
  is what's actually on the deck. See
  [Materiel & the War Economy](./war-economy.md).

## Losing it

A carrier group at 0% health is sunk: no slots, no strikes, no navaids. It
comes back through a configured respawn action, or when the round resets.

The enemy takes a carrier by **sinking its SUPPLY ship**: a carrier group is
only eligible once its logistics are at 0%, not when its health is low. Then
they either put capture troops aboard or take the naval base it is linked to.
When it goes, both sides are told:

```dcsmessage
You have captured CVN Group with its aircraft! Carrier at 50% health
```

```dcsmessage
CVN Group has been captured by the enemy!
```

The [Capture Advisor](../f10-menu/objectives.md) states what is outstanding.
Read the full sequence in [Capturing Objectives](./capturing-objectives.md).

## See Also

- [Capturing Objectives](./capturing-objectives.md): taking and losing a carrier
- [Navaids & Approaches](./navaids.md): TACAN, ICLS, ACLS and Link-4 channels
- [Weather, ATIS & Time](./weather.md): the carrier ATIS and recovery case
- [Actions Menu](../f10-menu/actions.md): carrier waypoint and naval strike
- [Materiel & the War Economy](./war-economy.md): why the naval base matters
