# Playbook: Capturing a Base

A complete objective assault, from picking the target to holding it. This is the
task-focused version — for the rules and every edge case see
[Capturing Objectives](../gameplay/capturing-objectives.md).

**Roles involved:** strike/CAS to soften the base, a transport helo for the
assault troops, ideally a fighter for top cover. One person can do it across
several sorties; a coordinated group does it in one.

## Step 0 — Pick the target

On the [Live Ops dashboard](./reading-live-ops.md) → OBJECTIVES, sort enemy
bases by health. You want one that is:

- **Low health already**, or lightly defended enough that you can grind it down.
- **Close to your own friendly base** — you have to stage troops from somewhere
  that *isn't* the target.
- **Worth taking** — a logistics hub or an airbase on the front line shifts the
  war; a dead-end FARP doesn't.

## Step 1 — Grind it to ≤ 20% health, zero infantry

The base becomes capturable only when **health ≤ 20% AND every infantry squad
in the zone is dead.** Logistics is *not* the trigger.

- CAS with bombs, rockets, guns, AGMs, GBUs. Sweep the whole zone for infantry —
  **one surviving squad blocks the capture.**
- JTAC-directed [artillery](../advanced/artillery.md) or
  [cruise missiles](../advanced/alcm.md) are excellent against dug-in troops.
- Watch the F10 map marker: it shows **Health %** and **Infantry %**. The
  **inner ring turns white** when the base is actually capturable.
- If your troops are down there and it isn't flipping, the game tells you why —
  *"health still above 20% (34%)"* or *"enemy infantry still defending."*

> A base bombed all the way to **Health 0** goes **Neutral** by itself — you
> still need troops to take the Neutral base afterward.

## Step 2 — Load capture troops from a *different* base

You **cannot pull troops from a base that is itself threatened or capturable.**
Stage from a friendly airbase/FARP further back.

Troops that **can capture** (Vector Strike / PG Tempest values):

| Squad | Cost | Notes |
|---|---|---|
| Standard Infantry | 0 pts | the workhorse — bring these |
| Anti-Tank Infantry | 2 pts | can capture, also kills armor |
| Mortar Infantry | 5 pts | can capture, indirect fire |

MANPADS (Igla / Stinger) **cannot capture.**

`F10 → Troops → Load → [squad type]`. A Huey lifts one squad; a Mi-8 six; a
CH-47 ten. Squad weights matter on light helos — see
[Troop Transport](../f10-menu/troops.md).

## Step 3 — Fly it in and unload inside the zone

- The capture zone is the ring around the objective center on the F10 map.
- Land inside it. The base must **not be threatened** at the moment you unload
  (no enemies within range) or you'll get *"you can't deploy troops here while
  enemies are near."* Clear them, or wait ~5 minutes after they leave.
- `F10 → Troops → Unload`. Troops must be **on the ground and alive** — squads
  still in the helo don't count.

## Step 4 — Hold for the timer

- Base timer is **180 seconds** with one squad in the zone.
- **More squads = faster:** 2 squads ≈ 90 s, 3 ≈ 60 s, floored at 30 s.
- The timer **pauses or resets** if enemy troops enter the zone, your troops
  die or leave, or logistics repairs the base back above 20% health.

## Step 5 — Consolidate (don't fly away yet)

The base flips owner when the timer completes, but it is **not fully yours** for
a **~5-minute consolidation window**. The F10 label runs a live countdown —
**"CONSOLIDATING 42% (3m left) — hold the zone with troops"** — and you can't
slot in at the base until it finishes.

**Keep the troops in the zone.** Fly them out and the clock stops
(**"CONSOLIDATION PAUSED"**); progress is held, not lost, and resumes when they
return.

**Don't just orbit — shorten it.** Bring a second squad into the zone (each
extra squad adds 50% to the rate), or land a **Logistics Repair Kit** or
**supply crate** at the base for an outright **~2-minute jump**. The logistics
sortie is faster than waiting.

- The enemy **cannot start a fresh capture timer** during this window — they
  have to physically kill your holding troops to take it back.
- If your holding troops die **and** the base is still shot up (≤ 20% health),
  it drops to **Neutral** and you start over.
- If your **new garrison** has come up above 20% health, it consolidates and
  stays yours even if the assault troops die.

## Step 6 — Hold what you took

A freshly captured base is a **soft target**: you get a light AAA + infantry
garrison at ~25% health and **no SAMs**. Everything heavier rebuilds slowly
through auto-repair or has to be flown in.

- Deliver [crates](./running-crates.md) — SAMs and armor — to harden it.
- Keep [logistics](../gameplay/logistics.md) flowing so the garrison rebuilds
  and repairs run.
- Expect an air counter-attack in the first few minutes. Top cover earns its
  keep here.

## Special cases

- **SAM site** — captures **instantly** when troops hold the zone, no timer.
  But its position is hidden (not on the map) and its launchers/radar/infantry
  are live until you kill them.
- **Naval carrier group** — disable it by sinking its escorts to drop its
  logistics to 0, then either land troops aboard or take its linked naval base.

Both are detailed in [Capturing Objectives](../gameplay/capturing-objectives.md).

## See also

- [Running Crates & Building a Base](./running-crates.md)
- [Flying CAS With a JTAC](./cas-with-jtac.md)
- [Deployables Guide](../advanced/deployables-guide.md)
