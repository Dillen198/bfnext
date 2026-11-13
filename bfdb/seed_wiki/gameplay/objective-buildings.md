# Buildings at an Objective: What to Look For

Every objective has things you can destroy, but they don't all do the same job.
This page shows what to look for when you fly over a base, what each kind of
thing does for its owner, and what happens when you destroy it.

There are two kinds:

| What | Where it shows | What destroying it does |
| --- | --- | --- |
| **Logistics buildings**: warehouses, industrial buildings, fuel and storage depots | **Logi** hex, and a `logi:` pin on the F10 map | The base repairs more slowly, sends less supply down the chain and makes less in the war economy. At 0 it can't supply crates or troops at all |
| **The garrison**: vehicles, guns, SAM launchers, infantry | **Health** hex | Brings the base towards capture. Health at or below 20% with no infantry left = capturable |

So **buildings break the base, and the garrison holds it.** Destroy the
buildings to stop a base from working; kill the garrison to take it.

---

## Finding the buildings: the `logi:` pins

You don't have to guess which buildings count. Each logistics building has
an F10 map pin that names it. For example:

```
Aleppo logi: 12345678 (AIRBASE_WAREHOUSE_01)
```

That's the base, then the building's map name, then its DCS type in brackets.

![F10 map zoomed in on an airbase, showing its logi: pins and the four status hexes](/api/wiki/images/c772fdcf-ef17-4a95-8d69-4cca9a2298b3)

- **Both sides see the pins.** Attackers use them to find targets, and
  defenders use them to see what they've lost.
- **Pins only show while the base is awake.** A base wakes up when an enemy
  aircraft comes within about {{cfg:unit_cull_distance|37040}} m (or is
  heading there fast enough to arrive within a minute), an enemy ground unit
  within {{cfg:ground_vehicle_cull_distance|10000}} m, or a player-deployed
  enemy unit within 10 km. A quiet base deep behind the lines shows none.
  Special SAM sites never get pins.
- **A pin disappears when its building is destroyed.** If a base has fewer
  pins than it used to, the missing ones are already destroyed.
- **The engine counts only up to four buildings per base**: the warehouses,
  industrial buildings, fuel tanks and storage depots closest to the centre
  of the zone. Other buildings in the town around a base don't count.
  Hitting them does nothing.
- **A JTAC can lase them for you.** Use `F10 → JTAC → Designate Building`, or
  type `-jtac <id> building` in chat. The JTAC works through the standing
  buildings one at a time and tells you when none are left. See
  [JTAC System](../f10-menu/jtac.md).

---

## Logistics buildings

On **Modern – Syria** a base's logistics is the real buildings on the DCS map
inside its zone. Nothing extra is placed for it. Every building that counts
has a DCS type name containing one of five words:

| Word in the type name | What you're looking at |
| --- | --- |
| `WAREHOUSE` | Warehouses and store sheds |
| `INDUSTRIAL` | Factory and works buildings |
| `DEPOT` | Depots |
| `FUEL` | Fuel tanks and fuel stores |
| `STORAGE` | Storage buildings and tanks |

### Warehouses

![A warehouse at a Syria airbase](/api/wiki/images/1076c909-d162-42c1-af6a-cbfd0cec57f2)

**Look for:** big shed-like store buildings. Most airbases have them, and
the base's `logi:` pins tell you which ones count.

### Industrial buildings

![An industrial building at a factory objective](/api/wiki/images/ac9d7ea1-c739-4c4b-9901-bebdfdeee7c3)

**Look for:** factory halls and works buildings. They're most common at
**Factory** objectives, which are placed on real industrial sites such as
mines and oil fields.

### Fuel, depot and storage buildings

![Fuel and storage tanks at an objective](/api/wiki/images/106369a9-2019-4c29-b7a3-36bb568e34ca)

**Look for:** fuel tanks and storage compounds. A single tank is a small
target, so check the pin before you drop.

### What destroying one does

Logi is simply the share of the base's counted buildings still standing:

| Buildings standing | Logi | Self-repair pulse |
| --- | --- | --- |
| 4 of 4 | 100% | every {{cfg:repair_time|1800}} s |
| 3 of 4 | 75% | about a third slower |
| 2 of 4 | 50% | twice as slow |
| 1 of 4 | 25% | four times as slow |
| 0 of 4 | **0%** | **none** |

A base with three counted buildings drops 100 → 67 → 33 → 0, and so on. A
base with **no** counted buildings in its zone always reads
**{{cfg:logi_from_scenery.fallback_logi|100}}% logi**, so you can't hurt it
this way. Go after its garrison instead.

As logi falls:

- **It repairs itself more slowly**, and not at all at 0. Auto-repair is how
  a base rebuilds the garrison you've been killing, so this is what turns a
  raid into lasting damage.
- **It feeds the supply chain less.** A Logistics Hub receives new supply in
  proportion to its logi. A hub at 0 drops out of the network entirely.
- **It produces less.** A Factory produces in proportion to its logi, and
  nothing at 0. Every base also adds to its side's
  [war economy](./war-economy.md) according to its logi. Hubs and factories
  count most, then Command Centers, then airbases, FOBs, FARPs and naval
  bases.
- **At 0 it is cut off.** Nobody can load crates or troops there. Players get
  *"not near friendly logistics (or this base is too damaged to supply
  from)"*. An enemy base at 0 also stops blocking your supply routes, and the
  ATIS reports the field closed.

![The same warehouse after it has been destroyed](/api/wiki/images/3df33ccf-08ac-4df1-b0b2-257650fdf76f)

**Low logi doesn't make a base capturable.** Capture depends on Health and
infantry. The exception is a base with no garrison units at all, such as some
hubs and depots: its Health simply follows its logi, so bombing its buildings
is how you take it.

### Getting it back

The rubble stays on the map, but the rating can recover:

- **Logistics Repair Kit.** Fly one in from a *different* friendly base and
  unpack it inside the damaged base's zone. Each kit brings back half the
  lost buildings plus one, and pays
  {{cfg:points.logistics_repair|25}} points. See
  [Repairing Logistics](./logistics.md).
- **Capturing the base** counts as one repair step for the new owner.
- **Auto-repair doesn't bring buildings back.** It only rebuilds the
  garrison.

Losses carry over when the server restarts.

---

## The garrison

The garrison is everything at the base that counts towards **Health**: armour,
AAA, SAM launchers, infantry and any other units the mission placed there. It
is only in the DCS world while someone is near the base, so a quiet base can
look empty from a distance. Its units still count, and a base that has been
threatened for a minute brings any missing garrison groups back in.

Where the server leaves it on (`garrison`, on by default), the garrison does
not just stand there:

- **It moves out to meet you.** An enemy ground unit within
  {{cfg:garrison.react_m|5000}} m of the base's edge sets the armour and
  infantry driving out to fight it, up to {{cfg:garrison.leash_m|2500}} m past
  the edge. A unit that fires on the base gives itself away and becomes a
  target too. The owner is told `Saraqib: enemy armour at the perimeter, the
  garrison is moving to engage.`
- **It calls for help.** If the intruder is still there after
  {{cfg:garrison.qrf_delay_secs|60}} seconds, a nearby formation may be sent as
  a quick reaction force: `GROUND COMMAND: 3rd Armoured Bn is the quick reaction
  force for Saraqib.`
- **It goes home** {{cfg:garrison.clear_secs|120}} seconds after the last
  intruder, and patrols inside the base while it is quiet (not at airfields, where armour would block the taxiways).
- **Mobile SAM sites move.** A SAM group whose vehicles all drive relocates
  inside its base after it stops firing and now and then while quiet, and the
  new spot is saved, so a site is rarely where it was last round.

When the base is down to 20% health with its last few infantry alive, the
attacking side gets a pin on each survivor: `Last defenders of Saraqib:
infantry here (3 left)`. See
[Capturing Objectives](./capturing-objectives.md#step-1-knock-it-down).

On capture, the old owner's garrison is removed and the new owner gets a light
one, without SAMs. The rest comes back one group per repair pulse.

---

## Command Centers

![A Command Center: the star on the F10 map, and the SAM sites it networks](/api/wiki/images/bab0f39f-5eab-4196-a350-0832f2d996b5)

**On the F10 map:** a **five-point star**. On Modern – Syria, for example,
*SyADF 159th Regiment*.

**On the ground:** there is no special Command Center building. A Command
Center is an objective zone with a garrison, and it has logistics buildings
if the zone covers any.

**What it does:** at mission start every SAM site links to its nearest
friendly Command Center. While that Command Center still has Health above 0
and belongs to the same side, its SAM sites are networked:

- they share one radar picture,
- their radars stay dark until a real threat appears,
- they shut down when an anti-radiation missile is inbound.

**When it is destroyed or captured:** every SAM site linked to it falls back
to plain DCS AI, with radars on all the time and no coordination. Nobody has
to touch the SAM sites themselves. A Command Center never goes Neutral by
itself when its Health reaches 0, so troops have to capture it. See
[Objectives → Command Centers](./objectives.md).

---

## FARPs

![A crate-built FARP: tent, ammo dump and fuel depot](/api/wiki/images/cba2754e-1658-4577-b0f4-49a73b4eafe6)

On servers that offer FARP crates, a FARP built from crates appears about a
minute after you unpack it, with:

| Piece | DCS type | Looks like |
| --- | --- | --- |
| Tent | `FARP Tent` | An army tent |
| Ammo dump | `FARP Ammo Dump Coating` | Ammunition under camouflage netting |
| Fuel depot | `FARP Fuel Depot` | A small fuel store |
| Landing pads | `Invisible FARP` | Nothing visible. The pads can't be destroyed |

Its defence vehicles and the pieces above are what you shoot. A FARP whose
logi reaches 0 is removed from the map. A FARP whose Health reaches 0 goes
Neutral.

---

## FOBs, Airbases and Naval Bases

These have no building of their own that matters. Their **Health** is their
garrison, and their **logi** is the map buildings described above, if their
zone covers any.

Some objectives also contain **neutral decoration statics**, props the
mission maker placed. These can't be killed for good: they respawn and count
for nothing.

---

## Servers without map-building logistics

A base with no `logi:` pins is either asleep (no enemy nearby) or on a server
that places its logistics as statics instead of using map buildings. In that
case, look for this cluster at the base:

![Placed logistics statics: command centre, ammunition depot and trucks](/api/wiki/images/7eaf037f-a014-495e-90c6-46f3c0cd2d68)

| Piece | DCS type | Looks like |
| --- | --- | --- |
| Command centre | `.Command Center` | A command building |
| Ammunition depot | `.Ammunition depot` | An ammunition store |
| Logistics trucks | Ural / ATZ / HEMTT / M939 and similar | Parked fuel and cargo trucks |

There, logi is the share of this logistics group still alive. Losing it has
the same effects as losing buildings above. Other placed statics, such as a
lone fuel tank, count towards Health instead.

---

## See Also

- [Objectives](./objectives.md): objective types, Health, Logi, Supply and Fuel
- [Reading the F10 Map](./reading-the-map.md): the four status hexes and every pin
- [Logistics & Supply](./logistics.md): supply chains, auto-repair and repair kits
- [Capturing Objectives](./capturing-objectives.md): bringing a base down to capturable and taking it
- [Materiel & the War Economy](./war-economy.md): why hubs and factories decide the round
