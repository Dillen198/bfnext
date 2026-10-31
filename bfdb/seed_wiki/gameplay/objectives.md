# Objectives

Objectives are the places the campaign is fought over: airbases, FARPs, FOBs,
logistics hubs, factories, naval bases, carrier groups, SAM sites and command
centers. Each one has an owner, a zone, a garrison and a set of numbers that
say how it is doing. This page explains all of it.

![Overview of objective types across the campaign map](/api/wiki/images/934a41dc-d2ad-4b52-8dfd-60cbdc4deb78)

## Objective types

| Type | F10 icon | Menu tag | What it is for |
| --- | --- | --- | --- |
| **Airbase** | none of ours (DCS draws the airfield) | `AB` | Fixed-wing and helicopter slots, warehouse, repair and rearm |
| **FARP** | an "H" | `FARP` | Forward helicopter base. A FARP whose logistics reach 0 is removed from the map |
| **FOB** | a tent | `FOB` | Forward ground base, a zone with a garrison |
| **Logistics Hub** | a hexagon | `LOGI` | Feeds supply down the chain to the bases it is linked to (grey arrows on the map) |
| **Factory** | a factory with a sawtooth roof | `FACT` | Produces for its side's war economy in proportion to its logistics |
| **Naval Base** | an anchor | `NAVAL` | Port; each carrier group links to its nearest friendly naval base |
| **Carrier Group** | a ship hull | `CVN` | A moving task force with deck slots. Drawn for its owner only |
| **Special SAM Site** | a boxed missile | `SAM` | A long-range air defence site. Drawn for its owner only |
| **Command Center** | a five-point star | `CC` | Keeps nearby SAM sites networked |

Airbases, naval bases and FARPs are **primary objectives**: they are what the
[Last Stand](./round-rules.md) counts.

The F10 map labels each objective with its name and type, for example
`Saraqib Logistics Hub` (a special SAM site shows its name only). See
[Reading the F10 Map](./reading-the-map.md) for the rings and hexes around it,
and [Buildings at an Objective](./objective-buildings.md) for what to shoot.

![Naval carrier strike group at sea](/api/wiki/images/f2ad53a9-2e98-4925-bced-de97267fc7e6)

### Carrier groups and SAM sites are secret

Two kinds are drawn on the F10 map **for their owner only**: carrier groups,
because they move, and special SAM sites, because they are meant to be found.
Every menu, report and briefing follows the same rule, so the enemy's carrier
or SAM site never appears in your lists until your side owns it. A Neutral one
is hidden from both sides. The dashboard follows the same rule.

### SAM sites

![Hidden SAM site tucked into rocky terrain](/api/wiki/images/0ce0f6d9-ab3e-4f5b-87e3-799b0e2964cd)

High-value air defence sites, networked with other sensors:

- Search radars stay dark until a real threat is confirmed, then power up.
- The enemy has to find them: they are not on the enemy's F10 map or
  dashboard (EWR tracks, recon and ELINT help).
- Ownership can flip either way, including from Neutral. A captured site
  re-arms under its new owner and defends *their* airspace.
- Troops can capture one at any time, with the normal capture timer. See
  [Capturing Objectives](./capturing-objectives.md#special-sam-sites).

**Taking a base does not give you air defence.** Objectives carry point defence
only: guns and short-range launchers that cover their own perimeter. Everything
with real reach lives in a SAM site, which stands off several kilometres from
whatever it protects, so it has to be found and fought for separately. Capture
the airbase and the battery covering it is still out there.

That cuts both ways. Your own strategic cover comes from the SAM sites your
coalition holds and from what your pilots deploy by crate. Lose the sites and
the bases behind them are naked however many you own.

**Early-warning radars are their own sites.** They sit further back than the
SAM batteries, they feed the networked picture, and they are capturable on the
same terms. Killing one blinds a sector without fighting the batteries in it.

**Warning**: a "quiet" site isn't necessarily undefended. Sites share
detections across the coalition's sensor network and only light up for a
confirmed threat. Fire an anti-radiation missile at one and it goes dark, and
some keep a short-range point-defence system alert specifically to shoot your
missile down. Don't treat radar silence as an all-clear.

### Command Centers

The network node behind the Integrated Air Defence Network. A Command Center is
a trigger-zone objective with a garrison: no airfield, no pad.

- At mission start, every SAM site links to its **nearest friendly Command
  Center**.
- While that Command Center is alive and still owned by the same side, its SAM
  sites get the full networked behaviour: shared picture, radars dark until a
  confirmed threat, anti-radiation missile defence.
- **If the Command Center is destroyed or captured, every SAM site linked to it
  falls back to plain DCS AI**: always-on radar, no coordination.
- A Command Center never drops to Neutral at health 0. Troops have to capture
  it.

Taking one out quietly de-fangs every SAM site it covered, often a cheaper way
to open a corridor than finding each site. Defend yours as hard as the sites.

## Ownership

Every objective belongs to **Blue**, **Red** or **Neutral**. A base becomes
Neutral when:

- its garrison is wiped out completely (health 0), or
- it was just captured and the enemy killed every one of the troops holding it
  during [consolidation](./capturing-objectives.md#step-5-consolidation).

A Neutral base has no garrison, locks its slots, never repairs, and is taken by
whichever side gets capture troops into it first.

## The numbers

Each objective tracks these. The first four are the hexes under it on the F10
map.

```statushexes
{"health":82,"logi":58,"supply":14,"fuel":95,"caption":"A base in trouble: its garrison is mostly standing and its logistics half there, but it is nearly out of munitions."}
```

### Health

The share of the base's garrison still alive: every unit at the objective
except indestructible decoration. It is what decides whether the base can be
captured: **20% or less, with no infantry left**.

- A base with no garrison units at all reads 100%, unless its health comes from
  its logistics buildings (see
  [Buildings at an Objective](./objective-buildings.md)).
- A Neutral base reads 0% until someone captures it.
- Health 0 on a Blue or Red base drops it to Neutral (not carriers, command
  centers or special SAM sites).

### Logistics (Logi)

Whether the base *works*: its logistics buildings, or on servers without
map-building logistics, its logistics group. At 0% it can't issue crates or
troops and can't repair itself; in between it repairs more slowly. On a carrier
group, Logi is the SUPPLY ship: sink it and Logi drops to 0%. Logistics does
**not** gate capture. See [Buildings at an Objective](./objective-buildings.md).

### Supply

Munitions and materiel in the warehouse, weighted by what the side actually
uses. Low supply takes loadouts off the rearm menu and stops repairs: each
repair pulse needs {{cfg:repair_supply_cost|5}}% supply. See
[Logistics & Supply](./logistics.md).

### Fuel

Fuel in the warehouse.

### Aircraft and infantry

Two more numbers, shown on your own bases' cards in the
[Objectives Menu](../f10-menu/objectives.md):

- **Aircraft**: airframes on the ramp. Low means the type you want may not be
  slottable.
- **Infantry**: the share of the garrison's infantry still alive (not a head
  count). The F10 label shows it to everyone once health is down to 20%.

### Hex colours

| Colour | Value |
| --- | --- |
| Green | above 66% |
| Amber | 34% to 66% |
| Red | 33% or less |

```hexlegend
```

A **gold outline on the supply hex** means unlimited supply: the base never
runs out of weapons, vehicles or fuel. A **gold owner ring** means unlimited
aircraft.

### Base fund

Each base also carries a fund of points, seeded at the start of a map
(`objective_start_points`, {{cfg:objective_start_points.Blue|0}} for Blue on
this server) and topped up with `-transfer <amt> objective:<name>`. When a base
changes hands most of its fund is lost; a base that falls Neutral loses all of
it. See [Materiel & the War Economy](./war-economy.md).

## States

### Threatened

An objective is **threatened** while:

- an enemy **pilot** is within threat range of it (set per airframe) with line
  of sight, or
- an armed enemy **ground unit** is within
  {{cfg:ground_vehicle_cull_distance|10000}} m.

Enemy AI aircraft wake a base up but never threaten it. It stays threatened
until {{cfg:threatened_cooldown|300}} seconds after the last enemy left.

What it does:

- **Repairs freeze**, and the repair countdown restarts from the last moment
  an enemy was seen.
- The garrison is put on the map (if it was asleep) and moves out to meet enemy
  ground units.
- Troops can't be unloaded inside your own threatened base: `you can't deploy
  troops here while enemies are near`.
- The F10 map draws the **yellow ring**, and the owner ring and icon flash. If
  the server turns on under-attack alerts, the owner also gets a red ring.
- It shows as `[THREAT]` in the status lists and under `Bases Under Threat`.

### Capturable

Health at or below 20% and no infantry left. The **inner ring turns white** and
the label reads `>> UNDEFENDED - BLUE captures: land troops inside the ring`
(or `>> NEUTRAL - either side captures: ...` on a Neutral base). You can't slot
in at your own base while it is capturable, and you can't load crates or
troops there. It still repairs if left alone, burning supply to do it.

### Being taken

A capture timer is running: `Capturing: 50%` on the label, `[BEING TAKEN]` in
the lists. Repairs are frozen.

### Consolidating

For a while after a capture the base is held only by the troops that took it.
The label counts it down (`>> CONSOLIDATING 42% (2m54s left) - hold the zone
with troops`), nobody can slot in, and the enemy can't start a capture timer:
they have to kill the holding troops. See
[Capturing Objectives](./capturing-objectives.md#step-5-consolidation).

### Commander's priority

A base the command has flagged shows `[PRIORITY]` in your status lists and
`- COMMANDER PRIORITY` on its card. Only its owner sees the flag.

### Logistics detached

A base set up with no automatic resupply. Its card says `NOTE: logistics
detached -- no automatic resupply`: crates, C-130 drops and helo runs are all
it gets.

## The zone

Each objective has one zone, a circle or a four-sided area set in the mission.
On the F10 map it is the **dashed owner ring**. Capture troops count only while
they are alive inside it; its radius is printed on every card (`Zone radius:
2500 m`).

![Diagram of an objective on the F10 map. The dashed owner ring marks the zone; a white inner ring at nine-tenths of its size means capturable; a thick yellow ring, as wide as the logistics exclusion distance or 1.1 times the zone, appears when the base is threatened. Up to four logistics buildings inside the zone carry logi pins. A row of four status hexes, Health, Logi, Supply and Fuel, sits south of the centre, the type icon in the middle, and the label with the name and capture state to the north-east.](/api/wiki/images/ffa68774-fadf-4d8c-a09b-dcb6814d6691)

Around it, {{cfg:logistics_exclusion|10000}} m from the centre (the logistics
exclusion), you can't unpack deployables near your own objectives. The yellow
threatened ring is drawn at that distance, or 1.1 times the zone if that is
bigger.

## See Also

- [Capturing Objectives](./capturing-objectives.md): the full capture sequence
- [Buildings at an Objective](./objective-buildings.md): what to look for and what each building does
- [Reading the F10 Map](./reading-the-map.md): rings, hexes, labels and pins
- [Objectives Menu](../f10-menu/objectives.md): the in-cockpit reports and the Capture Advisor
- [Logistics & Supply](./logistics.md): keeping what you hold
- [Materiel & the War Economy](./war-economy.md): why factories and hubs decide the round
- [Navaids & Approaches](./navaids.md): finding a FARP or carrier
- [Carrier Operations](./carrier-ops.md)
