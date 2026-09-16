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

| Ring | Colour | Means |
| --- | --- | --- |
| Outer, dashed | Violet / azure / white | Who owns it |
| Outer, **gold** | Gold | This base has **unlimited aircraft** |
| Inner | White | **Capturable right now** — land troops |
| Outer, thick | Yellow | **Threatened** — enemy inside the exclusion zone |

If you see a white inner ring, the base is takeable this second. That is the
single most useful thing on the map.

### The four status hexes

Beside every objective is a row of four hexagons. They are always in the same
order:

```
   Health   Logi   Supply   Fuel
      ⬢       ⬢       ⬢       ⬢
```

| Colour | Value | Read it as |
| --- | --- | --- |
| Green | above 66% | Fine |
| Amber | 33–66% | Getting thin |
| Red | below 33% | Critical |

**Health** is how much of the base is still standing — it has to be under 20%
before the base can be captured. **Logi** is its logistics buildings; kill those
and the base stops repairing and stops supplying its neighbours. **Supply** is
munitions, **Fuel** is fuel.

A hex with a **gold outline** means that resource is *unlimited* — it will never
run dry, no matter how much you take. Those are your main operating bases.

Four red hexes on an enemy base means it is beaten. Four red hexes on *yours*
means you have a logistics problem, not a flying problem.

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

| Symbol | Means |
| --- | --- |
| Arrow between two bases | A supply link — the chain that keeps a base stocked |
| Dashed front line | The boundary between the coalitions |
| Dot-dash line to a target | A **JTAC** bearing line, from the JTAC to what it is lasing |

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

| Symbol | Means |
| --- | --- |
| Thick **yellow** ring | Enemy has entered an objective's exclusion zone |
| Thick **red** ring | An objective is actively being attacked |
| `FIRES` + circle | An artillery fire mission is landing there |
| `RECON` | A recon pass found enemy units |
| `CSAR` + circle | A downed pilot, with the countdown to capture |
| Large circle at a base | A **CAP threat** — enemy fighters are up |
| Small circle on a road | An **ambush** has spawned |
| Circle on open ground | A missile strike is inbound to that point |

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

Contacts found by ELINT, SIGINT or a recon pass appear where the engine thinks
they are — which is not always exactly where they are. Treat an intel mark as a
lead, not a guarantee, and check the age on the label before you commit.

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
