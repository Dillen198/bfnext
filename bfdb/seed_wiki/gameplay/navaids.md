# Navaids & Approaches

DCS gives a stock airbase its own ILS, TACAN, VOR and NDB — baked into the
terrain. It gives a FARP, a FOB, a logistics hub, a naval base or a carrier
**nothing at all**. Fowl Engine fills that gap: every round it generates a
navaid set for those objectives, lights the beacons, and publishes the list.

You do not have to do anything to make this work. You just have to know the
channel.

## Finding the channel

```
F10 → Info → Navaids Directory
```

Every navaid your coalition currently owns, with its Morse ident and its
bearing/range from you. Also on the per-base card:

```
F10 → Objectives → Nearest Base (detail)
```

…which lists the navaids that base broadcasts, under its status block.

Two things worth knowing:

- **Real airbases are deliberately excluded.** The terrain already carries their
  approach aids, and the scripting API cannot read them to avoid a channel
  collision — so the directory only lists generated ones. For an airbase, use
  the DCS kneeboard or F10 map as usual.
- **The list is your side's.** You see what you own. Capture a FARP and it
  re-lights on your side's channel pool at the next reallocation.

## What each objective type gets

| Objective | Blue-owned | Red-owned |
| --- | --- | --- |
| FARP | TACAN + NDB | NDB only |
| Logistics Hub | TACAN + NDB | NDB only |
| Naval Base | TACAN + NDB | NDB only |
| FOB | nothing (off by default) | nothing |
| Carrier group | full suite, per deck | full suite, per deck |
| Airbase | — (terrain provides it) | — |

**Why red gets NDB only:** Russian-pattern aircraft home on ADF/ARK, not TACAN.
A red-owned ground objective broadcasts a homer its own airframes can actually
use. This follows *current ownership* — take a red FARP and it starts putting
out TACAN for you; lose one of yours and it goes NDB.

### Carrier groups

A carrier is handled per **ship**, not per task force, so a group with several
decks gets an independent set on each:

| Deck class | Gets |
| --- | --- |
| US CVN / Stennis / Forrestal | TACAN + ICLS + ACLS + Link-4 ({{cfg:navaids.carrier_link4_mhz|336}} MHz) |
| LHA / LHD (Tarawa and friends) | TACAN + ICLS — no ACLS or Link-4, they have no cats or traps |

See [Carrier Operations](./carrier-ops.md) for the rest of the carrier picture.

## Channel allocation

| | |
| --- | --- |
| Blue TACAN pool | channels {{cfg:navaids.blue_tacan.lo|2}}–{{cfg:navaids.blue_tacan.hi|62}} |
| Red TACAN pool | channels {{cfg:navaids.red_tacan.lo|63}}–{{cfg:navaids.red_tacan.hi|125}} |
| Band | {{cfg:navaids.tacan_band|Y}} |
| NDB pool | {{cfg:navaids.ndb_khz.lo|200}}–{{cfg:navaids.ndb_khz.hi|1400}} kHz |
| Minimum separation | {{cfg:navaids.min_separation_nm|150}} nm between two navaids on the same channel |

The two coalitions use **separate TACAN pools**, so a channel identifies a side
as well as a station. If you are blue and you are tracking a high-numbered
TACAN, it is not one of ours.

Allocation is deterministic: objectives are sorted and handed the lowest free
channel nothing within the separation radius already holds. Re-running it after
the map changes hands is safe — an objective keeps its channel unless something
forces a reshuffle.

## Morse idents

Each station identifies with the **first three alphanumeric characters of the
objective name**, uppercased. `Al Dhafra` → `ALD`. `FARP Kilo` → `FAR`. Idents
are de-duplicated between stations that are close enough to be confused.

If you are tuning a TACAN and hearing the wrong ident, you are locked onto a
different station on the same channel — check the directory for the one you
actually want.

## Using them

**TACAN.** Tune the channel and band from the directory. On a FARP or hub this
is usually the only way to find the place at night or in weather — a FARP is
four pads and a fuel bladder, and it is invisible from 5 nm.

**NDB.** Set the frequency on your ADF/ARK. The needle points at the station.
Mi-8, Mi-24, Huey and the Russian fast jets all home on this.

**Carrier recovery.** TACAN for the join, ICLS for the needles on final, ACLS /
Link-4 if your airframe supports it. Same as any DCS carrier — the difference
is that the channels are generated per round, so **check the directory before
you launch**, not on the way back with 1,500 lb of gas.

## Practical notes

- **Write it down before you take off.** The Navaids Directory is a cockpit
  menu, which is a poor place to be reading when you are low on fuel in weather.
- **Channels change between rounds.** They are generated per campaign, not
  fixed per airfield. A channel you memorised last week is probably somebody
  else's now.
- **A captured base changes its beacon.** If a FARP you have been using goes
  quiet or moves channel, check who owns it now.
- **A beacon needs a host.** Ground navaids are broadcast by a group at the
  objective. Flatten the objective and the beacon goes with it — which is also
  a legitimate thing to do to the enemy's.

## See Also

- [Info Menu](../f10-menu/info.md) — where the directory lives
- [Carrier Operations](./carrier-ops.md)
- [Comms Plan](./comms-plan.md) — radio frequencies, which is a different problem
- [Objectives](./objectives.md) — what a FARP / FOB / hub actually is
