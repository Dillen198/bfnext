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

Every base, FOB, factory, depot and naval base is drawn the same way.

### The rings

```mapsymbols
[
 {"icon":"ring-owner","side":"Blue","name":"Outer ring, dashed",
  "text":"Who owns the base. Azure is Blue, violet is Red, white is neutral. This is the only ring that is a coalition colour — every other colour on an objective means a state, not a side."},
 {"icon":"ring-unlimited","name":"Outer ring, gold",
  "text":"This base has unlimited aircraft. It will keep producing airframes no matter how many you lose out of it, so it is a main operating base — yours or theirs."},
 {"icon":"ring-capturable","side":"Blue","name":"Inner ring, white",
  "text":"Capturable right now. Its health is under 20% and it has no infantry left: land troops in the zone and it is yours. This is the single most useful thing on the map."},
 {"icon":"ring-threatened","name":"Outer ring, thick yellow",
  "text":"Threatened — an enemy is inside the exclusion zone. While it is yellow you cannot deploy crates or troops there. It clears once they leave and the cooldown runs out."}
]
```

If you see a white inner ring, the base is takeable this second. That is the
single most useful thing on the map.

### The four status hexes

Beside every objective is a row of four hexagons. They are always in the same
order:

```statushexes
{"health":82,"logi":58,"supply":14,"fuel":95,"unlimited":["fuel"],
 "caption":"A base in trouble: still standing and holding out, but nearly out of munitions. Its fuel is gold — unlimited — so it will never run dry of that."}
```

### The colours

```hexlegend
```

### What each of the four means

**1. Health — how much of the base is still standing.**

```statushexes
{"health":82,"logi":58,"supply":14,"fuel":95,"focus":["health"]}
```

Every building, vehicle and gun at the objective counts toward it. This is the
one that decides whether you can take the place: health has to fall **below
20%** before the inner white ring appears and troops can capture it. Bombing a
base with full supply and full fuel still works — you are killing health.

**2. Logi — its logistics buildings.**

```statushexes
{"health":82,"logi":58,"supply":14,"fuel":95,"focus":["logi"]}
```

Warehouses, fuel tanks, the infrastructure that makes the base a base. Kill
these and it stops repairing itself **and** stops feeding its neighbours down
the supply chain. Hitting logi is how you make a whole region wither instead of
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
{"health":82,"logi":58,"supply":14,"fuel":95,"unlimited":["fuel"],"focus":["fuel"]}
```

Exactly what it says. Shown here with the **gold outline**: unlimited, so it
never depletes however much you take. Your rear airfields usually look like
this; a forward FOB almost never does.

### Reading the row at a glance

Four red hexes on an enemy base means it is beaten — stop bombing it and go
take it. Four red hexes on *yours* means you have a logistics problem, not a
flying problem: somebody needs to run crates or repair the chain, and no
number of sorties will fix it.

### The label

The name, and then only things you have to act on:

- `>> CAPTURABLE - land troops in the zone`
- `>> CONSOLIDATING 40% (2m10s left) - hold the zone with troops`
- `>> CONSOLIDATION PAUSED - no troops in the zone`
- `Infantry: 30% — clear all defenders to capture`
- `Capturing: 60%` / `Repairing: 25% (ETA 4m)`
- the base's navaids (TACAN / VOR / ILS)

The health and supply numbers are **not** in the label any more. They are the
hexes.

---

## Lines and routes

```mapsymbols
[
 {"icon":"supply-arrow","side":"Blue","name":"Supply arrow",
  "text":"A supply link between two bases, pointing the way the materiel flows. This is the chain that keeps the far end stocked."},
 {"icon":"frontline","name":"Front line, dashed white",
  "text":"The boundary between the two coalitions. It moves as objectives change hands, so it is the quickest read on who is winning where."},
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
| Logistics building | Which building, at which base |

---

## Warnings and events

```mapsymbols
[
 {"icon":"ring-threatened","name":"Thick yellow ring",
  "text":"An enemy has entered this objective's exclusion zone. Deployment there is locked until it clears."},
 {"icon":"ring-attacked","name":"Thick red ring",
  "text":"The objective is being actively attacked right now. If it is yours, this is where to go."},
 {"icon":"arty-burst","name":"Burst star in a circle",
  "text":"An artillery fire mission is landing there. Do not be under it, and do not expect anything soft to survive it."},
 {"icon":"recon-contacts","name":"Diamonds in a dotted box",
  "text":"A recon pass found enemy units here — one diamond per five contacts, with the exact count in the pin. The box is roughly where they were when they were seen, not where they are now."},
 {"icon":"csar","name":"White circle with a ripening hex",
  "text":"A downed pilot waiting for pickup. The hex runs green to amber to red as the enemy capture timer counts down — red means you are nearly out of time to get them."},
 {"icon":"cap-threat","side":"Red","name":"Large circle at a base",
  "text":"A CAP threat: enemy fighters are airborne from there. Treat the circle as the area they are covering."},
 {"icon":"ambush","name":"Small circle on a road",
  "text":"An ambush has spawned on that stretch of road. Convoys routed through it are going to lose vehicles."},
 {"icon":"missile","name":"Circle on open ground",
  "text":"A missile strike is inbound to that point. It is a warning with a clock on it, not a target marker."}
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

**"Which base should I hit?"** One with a white inner ring, or one whose logi
hex has gone red — that one is already starving.

**"Why won't this base capture?"** Read the label. It tells you: health too
high, infantry still alive, or a consolidation hold running.

**"Where do I take crates?"** Anywhere with red supply or fuel hexes on your own
side.

**"Why did the markers disappear?"** You zoomed out. The ground-drawn shapes
scale with the map. Zoom back in.
