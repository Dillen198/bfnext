# Reading the F10 Map

Everything the campaign knows is on the F10 map. Almost none of it is text any
more — the engine draws **shapes and colours** so you can read a base's state
without stopping to read a paragraph.

This page is the key to every symbol.

## The two rules that explain the whole map

**1. Colour means state.** Violet is Red coalition, azure is Blue, white is
neutral. Green / amber / red always mean good / marginal / critical, never a
side. Gold always means "special — pay attention".

**2. Shapes shrink when you zoom out, labels don't.** The status shapes are
drawn on the ground, so they fade away when you pull back to the whole theatre
and come back as you zoom in. That is deliberate: at theatre zoom you want the
front line, not four hundred little markers.

---

## An objective at a glance

Every base, FOB, factory, hub and naval base is drawn the same way: rings
for the zone and its state, a type icon in the middle, a row of four status
hexes to the south and a label to the north-east.

![Diagram of an objective on the F10 map. The dashed owner ring marks the zone; a white inner ring at nine-tenths of its size means capturable; a thick yellow ring, as wide as the logistics exclusion distance or 1.1 times the zone, appears when the base is threatened. Up to four logistics buildings inside the zone carry logi pins. A row of four status hexes, Health, Logi, Supply and Fuel, sits south of the centre, the type icon in the middle, and the label with the name and capture state to the north-east.](/api/wiki/images/ffa68774-fadf-4d8c-a09b-dcb6814d6691)

Carrier groups and special SAM sites are drawn for their owner only. The enemy
has to find them.

### The rings

```mapsymbols
[
 {"icon":"ring-owner","side":"Blue","name":"Outer ring, dashed",
  "text":"The zone, in the owner's colour: azure is Blue, violet is Red, white is neutral. Capture troops count only inside it. It and the type icon flash while the base is threatened or a capture timer is running there."},
 {"icon":"ring-unlimited","name":"Outer ring, gold",
  "text":"This base has unlimited aircraft. It will keep producing airframes no matter how many you lose out of it, so it is a main operating base — yours or theirs."},
 {"icon":"ring-capturable","side":"Blue","name":"Inner ring, white",
  "text":"Capturable right now: health 20% or less and no infantry left, or just captured and still held only by its assault troops. Land capture troops inside the zone. On a special SAM site this ring is in the owner's colour while the site stands and turns white once it is destroyed."},
 {"icon":"ring-threatened","name":"Outer ring, thick yellow",
  "text":"Threatened: an enemy pilot in sight of the base, or an armed enemy ground unit near it. Repairs are frozen, and the owner can't unload troops inside it. It clears a few minutes after the last enemy leaves. The ring is drawn at the logistics exclusion distance."}
]
```

If you see a white inner ring, the base is takeable this second. That is the
single most useful thing on the map. Read the label to see **who** can take
it: the ring is the same colour for both sides.

### The four status hexes

Beside every objective is a row of four hexagons. They are always in the same
order:

```statushexes
{"health":82,"logi":58,"supply":14,"fuel":95,
 "caption":"A base in trouble: still standing and holding out, but nearly out of munitions."}
```

Left to right: **Health, Logi, Supply, Fuel**.

### The colours

```hexlegend
```

### What each of the four means

**1. Health — how much of the base is still standing.**

```statushexes
{"health":82,"logi":58,"supply":14,"fuel":95,"focus":["health"]}
```

Every unit of its garrison counts toward it: vehicles, guns, launchers,
infantry. This is the one that decides whether you can take the place: health
has to fall to **20% or less**, with the last infantry dead, before the inner
white ring appears and troops can capture it. Bombing a base with full supply
and full fuel still works: you are killing health.

**2. Logi — its logistics buildings.**

```statushexes
{"health":82,"logi":58,"supply":14,"fuel":95,"focus":["logi"]}
```

Warehouses, fuel tanks, the infrastructure that makes the base a base. Kill
these and it stops repairing itself **and** stops feeding its neighbours down
the supply chain. [Buildings at an Objective](./objective-buildings.md) shows
what they look like. Hitting logi is how you make a whole region wither instead of
one airfield. A base whose logi is red will not come back on its own.

**3. Supply — munitions.**

```statushexes
{"health":82,"logi":58,"supply":14,"fuel":95,"focus":["supply"]}
```

What is in the warehouse for people flying out of it. When this runs down the
loadouts you can actually take start disappearing from the rearm menu. Red
supply on your own base is the usual reason "my jet won't take that bomb".

**4. Fuel.**

```statushexes
{"health":82,"logi":58,"supply":14,"fuel":95,"focus":["fuel"]}
```

Exactly what it says.

**Unlimited supply** is shown on the **supply** hex: a **gold outline**. Such a
base never runs out of weapons, vehicles or fuel, so both its supply and fuel
hexes stay green. Your rear airfields often look like this; a forward FOB
almost never does.

```statushexes
{"health":100,"logi":100,"supply":100,"fuel":100,"unlimited":["supply"],"focus":["supply"],
 "caption":"A rear base with unlimited supply: gold outline on the supply hex."}
```

### Reading the row at a glance

Four red hexes on an enemy base means it is beaten — stop bombing it and go
take it. Four red hexes on *yours* means you have a logistics problem, not a
flying problem: somebody needs to run crates or repair the chain, and no
number of sorties will fix it.

### The label

The name and type (`Saraqib Logistics Hub`), in the owner's colour on a dark
plate, and then only things you have to act on:

| Label line | Meaning |
| --- | --- |
| `>> UNDEFENDED - BLUE captures: land troops inside the ring` | A Red base Blue can take now (and the reverse for a Blue base). |
| `>> NEUTRAL - either side captures: land troops inside the ring` | A Neutral base. |
| `Infantry: 30% — clear all defenders to capture` | Health is down, but infantry is still alive. |
| `Capturing: 60%` | A capture timer is running. |
| `>> CONSOLIDATING 40% (2m10s left) - hold the zone with troops` | Just captured, consolidating. |
| `>> CONSOLIDATION PAUSED - no troops in the zone` | Just captured, holding troops out of the zone. |
| `>> NOT CONSOLIDATED - hold with troops or it goes Neutral` | Just captured, no progress figure yet. |
| `Repairing: 25% (ETA 4m00s)` | Healing toward its next repaired group. |

The health and supply numbers are **not** in the label: they are the hexes.
Navaids are not either, since the label is drawn for both sides; read them
from `Info → Navaids Directory`.

---

## Lines and routes

```mapsymbols
[
 {"icon":"supply-arrow","side":"Blue","name":"Supply arrow",
  "text":"A faint grey arrow from a logistics hub to each base it supplies. This is the chain that keeps the far end stocked. A naval base's arrow to its carrier is drawn for the carrier's owner only."},
 {"icon":"frontline","name":"Front line",
  "text":"The boundary between the two coalitions: a dotted white centre line, with a dashed azure line on Blue's side and a dashed violet line on Red's. It moves as objectives change hands, so it is the quickest read on who is winning where."},
 {"icon":"jtac-line","side":"Blue","name":"JTAC bearing line + diamond",
  "text":"A dot-dash line from a JTAC to whatever it is currently lasing, with a diamond on the target and the laser code printed beside it. Follow the line and you are looking at the thing you were asked to kill."},
 {"icon":"convoy-chevron","side":"Blue","name":"Convoy chevron",
  "text":"A supply convoy on the road, with the chevron showing which way it is travelling. Kill one and the base it was feeding goes short."},
 {"icon":"task-star","name":"Gold star on a circle",
  "text":"A task somebody has posted to the tasking board. Someone on your side has asked for work here — CAS, CAP, a capture or a supply run."}
]
```

Cut a supply arrow by taking the base at its source and everything downstream
starves.

---

## Pins (the little markers you click)

Pins stay collapsed until you click them, so they cost you nothing until you
want the detail.

| Pin | Carries |
| --- | --- |
| Convoy | Where it came from, where it is going |
| Air / sea logistics | Same, for air and sea runs |
| JTAC | The full 9-line, including the **laser code** |
| Tasking board | Who posted the task and what it is |
| Deployed group | What it is and who deployed it |
| Crate | What is inside it |
| Logistics building | `Saraqib logi: 12345678 (AIRBASE_WAREHOUSE_01)`: which building, at which base |
| Last defenders | `Last defenders of Saraqib: infantry here (3 left)`, attackers only, within about 150 m of each survivor |
| Captured base | `Saraqib captured by Blue`, for 30 minutes after a capture |

---

## Warnings and events

```mapsymbols
[
 {"icon":"ring-threatened","name":"Thick yellow ring",
  "text":"The objective is threatened: an enemy is in sight of it. Its repairs are frozen until it clears."},
 {"icon":"ring-attacked","name":"Thick red ring",
  "text":"Your objective is under attack. Drawn for the owner only, on servers that turn under-attack alerts on, and it fades after a couple of minutes. This is where to go."},
 {"icon":"arty-burst","name":"Burst star in a circle",
  "text":"An artillery fire mission is landing there. Do not be under it, and do not expect anything soft to survive it."},
 {"icon":"recon-contacts","name":"Diamonds in a dotted box",
  "text":"A recon pass found enemy units here — one diamond per five contacts, with the exact count in the pin. The box is roughly where they were when they were seen, not where they are now."},
 {"icon":"csar","name":"White circle with a ripening hex",
  "text":"A downed pilot waiting for pickup. The hex runs green to amber to red as the enemy capture timer counts down — red means you are nearly out of time to get them."},
 {"icon":"cap-threat","side":"Red","name":"Large circle at a base",
  "text":"A CAP threat: enemy fighters are airborne from there. Treat the circle as the area they are covering."},
 {"icon":"ambush","name":"Small circle on a road",
  "text":"An ambush party has driven out from an enemy base and set up on that stretch of road, ahead of a convoy. Convoys routed through it are going to lose vehicles."},
 {"icon":"missile","name":"Circle on open ground",
  "text":"A missile strike is inbound to that point. It is a warning with a clock on it, not a target marker."}
]
```

---

## The ground war

Ground formations add three things to the map, and one to the world. The
dashboard's COMMAND page has its own, richer set: see
[Ground War](../advanced/ground-war.md#reading-the-battlefield).

```mapsymbols
[
 {"icon": "f10-pin", "name": "Formation pin (your side only)", "text": "One per formation in the field, in your side's colour: its name on the first line, then its orders and roughly how strong it is, in quarters (\"attack Gori | ~75% strength\"). Click it for the text. The enemy never sees your pins.", "side": "Blue"},
 {"icon": "f10-attack-arrow", "name": "Arrow from a pin", "text": "The formation is attacking a base, and this is the way it is heading. Drawn only when the base is more than 3 km off; formations moving or defending get no arrow.", "side": "Blue"},
 {"icon": "f10-battle", "name": "Dashed orange ring, \"GROUND BATTLE near …\"", "text": "Formations are fighting here, or a formation is fighting a base's garrison. Everyone sees it, both sides: a battle is not a secret. It moves with the fight and goes when the fighting stops. This is where CAS and attack helicopters are wanted."},
 {"icon": "world-smoke", "name": "Smoke and fire on the ground (in the 3D world)", "text": "Not on the map: in the world. A battle sends up a column of smoke you can see from the air long before you can see the vehicles, and every vehicle killed in it burns for a while. Follow the smoke."}
]
```

---

## The same picture on the dashboard

The **Live Ops map** on the dashboard draws objectives the same way, on purpose
— so what you learn on the F10 map transfers straight across:

- the same **violet / azure** coalition colours,
- the same **four status hexes** under each base, in the same health / logi /
  supply / fuel order and the same green / amber / red buckets,
- the same **gold outline** for an unlimited resource, and a gold ring for a
  base with unlimited aircraft,
- a white ring for capturable, yellow for threatened.

The dashboard shows you your own coalition's supply and fuel only — enemy
stocks are fog-of-war, so those two hexes are simply absent on enemy bases
rather than guessed at.

## Intel contacts

A contact's **shape tells you what it is**, without reading anything:

```mapsymbols
[
 {"icon":"contact-diamond","side":"Red","name":"Diamond — air defence",
  "text":"A SAM or AAA contact. The shape you most want to know about before you plan a run."},
 {"icon":"contact-square","side":"Red","name":"Square — armour",
  "text":"Tanks and armoured vehicles."},
 {"icon":"contact-triangle","side":"Red","name":"Triangle — infantry",
  "text":"Troops, including the squads that capture objectives."},
 {"icon":"contact-hex","side":"Red","name":"Hexagon — artillery",
  "text":"Guns and rocket batteries. Whatever has been shelling you is one of these."},
 {"icon":"contact-hex-large","side":"Red","name":"Large hexagon — naval or airbase",
  "text":"A ship or an airfield: the big, fixed things."},
 {"icon":"contact-octagon","name":"Octagon — unidentified",
  "text":"Something is there and has not been classified yet. Worth a recon pass before you commit to it."}
]
```

Around it is a **dashed uncertainty ring**. That ring is the engine telling you
how sure it is about the position: a tight ring is a good fix, a wide one means
the contact is somewhere in there. Fly against the ring, not the shape.

Class, count, source, confidence and age are all still there — click the pin.

---

## Quick answers

**"Which base should I hit?"** One with a white inner ring and a label naming
your side, or one whose logi hex has gone red: that one is already starving.

**"Why won't this base capture?"** Read the label: no capture line means health
is too high, `Infantry:` means defenders are left, and a consolidation line
means it was just taken. For the full answer, `F10 → Objectives → Capture
Advisor` or the base's drawer on the dashboard. See
[Capturing Objectives](./capturing-objectives.md#why-wont-it-capture).

**"Where do I take crates?"** Anywhere with red supply or fuel hexes on your own
side.

**"Why did the markers disappear?"** You zoomed out. The ground-drawn shapes
scale with the map. Zoom back in.
