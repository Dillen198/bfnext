# Navaids & Approaches

DCS gives a stock airbase its own ILS, TACAN, VOR and NDB, built into the
terrain. It gives a FARP, a FOB, a logistics hub, a naval base or a carrier
**nothing at all**. Fowl Engine fills that gap: it generates a navaid set for
those objectives, lights the beacons, and lists them for your side.

You do not have to do anything to make this work. You just have to know the
channel.

## Finding the channel

Two places in the F10 menu:

- **Info → Navaids Directory**: every generated navaid your coalition owns,
  nearest first, with bearing and range from you.
- **Objectives → Nearest Base (detail)** or **Objectives → Base Detail → (base)**:
  the base card ends with a `Navaids:` block for that base. It is shown on
  your own bases only, so an enemy FARP's TACAN is never handed to you as a
  homing beacon for a strike.

```f10menu
{"root":"F10 Other","items":[{"label":"Info","items":[
 {"label":"My Status"},
 {"label":"Situation","items":[{"label":"1. Overview"},{"label":"2. Tasking"},{"label":"3. Hotspots"},{"label":"4. Threats & Air"},{"label":"5. Logistics"},{"label":"6. Comms"}]},
 {"label":"Support & Radios"},
 {"label":"Supply Convoys"},
 {"label":"Navaids Directory","says":"=== Friendly Navaids ===\nFARP Kilo (047°/23nm): TACAN 5Y FAR  NDB 205\nKutaisi Logistics (112°/41nm): TACAN 2Y KUT  NDB 200\nCVN Group [BCARRIER-1] (231°/88nm): TACAN 3Y BCA  ICLS 1  Link4 336.0  ACLS\nCVN Group [BCARRIER-2] (231°/88nm): TACAN 4Y BC1  ICLS 2","note":"Your side's generated navaids, nearest first. Carriers get one line per ship. Shown for 45 seconds."},
 {"label":"Time & Server"},
 {"label":"Weather"},
 {"label":"Help","items":[{"label":"Getting Started"},{"label":"Chat Commands"},{"label":"Cargo & Logistics"},{"label":"Objectives & Capturing"},{"label":"Carrier Groups"},{"label":"Combat & JTAC"}]}
]}]}
```

Each line reads `TACAN <channel><band> <ident>`, then `NDB <kHz>`, then for a
carrier deck `ICLS <channel>`, `Link4 <MHz>` and `ACLS`. If the server has
switched the feature off you get `Auto-navaids are disabled on this server.`
instead, and before the first allocation `No generated navaids yet.`

On a base card the same summary appears under the status block:

```dcsmessage
Navaids:
TACAN 5Y FAR  NDB 205
```

or `Navaids: none assigned` for a base that gets nothing (an airbase, or a FOB).

Two things worth knowing:

- **Real airbases are deliberately excluded.** The terrain already carries
  their approach aids, and the scripting API cannot read those to avoid a
  channel clash, so the directory only lists generated ones. For an airbase,
  use the DCS kneeboard or F10 map as usual.
- **The list is your side's.** You see what you own. Navaids are also never
  written on the F10 map labels, because those labels are drawn for both
  coalitions.

## What each objective type gets

| Objective | Blue-owned | Red-owned |
| --- | --- | --- |
| FARP | TACAN + NDB | NDB only |
| Logistics Hub | TACAN + NDB | NDB only |
| Naval Base | TACAN + NDB | NDB only |
| FOB | NDB only, and only if the server turns on `navaids.ndb_on_fob` (off by default) | same |
| Carrier group | a full set per deck (see below) | a full set per deck |
| Airbase | nothing generated (the terrain provides it) | nothing generated |

**Why red gets NDB only:** Russian-pattern aircraft home on ADF/ARK, not
TACAN. A red-owned ground objective broadcasts a homer its own airframes can
use. This follows *current ownership*: take a red FARP and it starts putting
out TACAN for you; lose one of yours and it goes NDB only. (A server can turn
this off with `navaids.red_ground_ndb_only`, and can turn NDBs off entirely.)

### Carrier groups

A carrier is handled per **ship**, not per task force, so a group with several
decks gets an independent set on each. What a deck gets depends on the ship
type, not on who owns it:

| Deck class | Gets |
| --- | --- |
| US CVN, Stennis, Forrestal | TACAN + ICLS + ACLS + Link-4 on {{cfg:navaids.carrier_link4_mhz|336}} MHz |
| LHA / LHD (Tarawa class) | TACAN + ICLS only. No cats or traps, so no ACLS or Link-4 |

The carrier TACAN comes out of the owning side's TACAN pool, on the same band
as the ground beacons ({{cfg:navaids.tacan_band|Y}} by default), so do not
assume the usual carrier X band. Carriers get no NDB. ICLS channels run from
1 to 20 and are never reused anywhere on the map.

See [Carrier Operations](./carrier-ops.md) for the rest of the carrier picture.

## Channel allocation

| | |
| --- | --- |
| Blue TACAN pool | channels {{cfg:navaids.blue_tacan.lo|2}}–{{cfg:navaids.blue_tacan.hi|62}} |
| Red TACAN pool | channels {{cfg:navaids.red_tacan.lo|63}}–{{cfg:navaids.red_tacan.hi|125}} |
| TACAN band | {{cfg:navaids.tacan_band|Y}} |
| NDB pool | {{cfg:navaids.ndb_khz.lo|200}}–{{cfg:navaids.ndb_khz.hi|1400}} kHz, in 5 kHz steps |
| ICLS | channels 1–20, one per carrier deck |
| Minimum separation | {{cfg:navaids.min_separation_nm|150}} nm between two navaids on the same TACAN channel or NDB frequency |

The two coalitions use **separate TACAN pools**, so a channel identifies a side
as well as a station. If you are blue and you are tracking a channel from the
red pool, it is not one of ours.

Allocation is deterministic: the eligible objectives are taken in a fixed
order and each is handed the lowest free channel that nothing within the
separation radius already holds. It is rerun whenever an objective changes
hands. Because the whole map is reallocated in order, **a capture anywhere can
move channels elsewhere**, not just at the base that flipped. Check the
directory again after a base changes hands.

## Morse idents

A ground station identifies with the **first three letters or digits of the
objective name**, uppercased: `Kutaisi Logistics` → `KUT`, `FARP Kilo` →
`FAR`. A carrier deck uses the first three of the **ship's unit name** in the
mission instead.

When two stations inside the separation radius would share an ident, the later
one keeps its first two characters and gets a digit: `FAR` and `FA1`, or `BCA`
and `BC1` for two decks of one task force.

If you are tuning a TACAN and hearing the wrong ident, you are locked onto a
different station on the same channel. Check the directory for the one you
actually want.

## When a beacon is on the air

A ground beacon is broadcast by one of the objective's own groups: the
logistics group first, then AAA, armour, the SAM groups and so on. Infantry and
services groups never host one.

- **A FARP's beacon is always up.** FARP groups are never despawned.
- **A Logistics Hub or Naval Base can go quiet.** Like other ground objectives,
  their garrison is despawned to save server load while no enemy is near, and
  it is re-spawned when one comes close. The beacon lives on that garrison, so
  it may only be on the air while the base is awake. It also goes off if the
  host group is destroyed, and comes back when the group is repaired.
- **Carrier decks are always present** (carrier groups are never despawned).
  Each ship lights its own set once it is up, and again after a capture or a
  respawn.

## Using them

**TACAN.** Tune the channel and band from the directory. On a FARP this is
usually the only way to find the place at night or in weather: a FARP is four
pads and a fuel bladder, and it is invisible from 5 nm.

**NDB.** Set the frequency on your ADF/ARK. The needle points at the station.
Mi-8, Mi-24, Huey and the Russian fast jets all home on this.

**Carrier recovery.** TACAN for the join, ICLS for the needles on final, ACLS
and Link-4 if your airframe supports them. The difference from a stock mission
is that the channels are generated, so **check the directory before you
launch**, not on the way back with 1,500 lb of gas.

## Practical notes

- **Write it down before you take off.** The Navaids Directory is a radio menu
  message, which is a poor thing to be reading when you are low on fuel in
  weather.
- **Channels follow the map, not the airfield.** They depend on who owns what,
  so a channel you memorised last week is probably somebody else's now.
- **A captured base changes its beacon.** If a FARP you have been using goes
  quiet or moves channel, check who owns it now.
- **A beacon needs a host.** Kill the group hosting the enemy's beacon and the
  beacon goes with it until they repair.

## See Also

- [Info Menu](../f10-menu/info.md): where the directory lives
- [Carrier Operations](./carrier-ops.md)
- [Weather, ATIS & Time](./weather.md): airfield ATIS, runway in use, altimeter
- [Comms Plan](./comms-plan.md): radio frequencies, which is a different problem
- [Objectives](./objectives.md): what a FARP, FOB or hub actually is
