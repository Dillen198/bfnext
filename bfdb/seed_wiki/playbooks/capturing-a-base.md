# Playbook: Capturing a Base

A complete objective assault, from picking the target to holding it. This is the
task-focused version. For the rules, every message and every edge case see
[Capturing Objectives](../gameplay/capturing-objectives.md).

**Roles involved:** strike/CAS to knock the base down, a transport helo for the
assault troops, ideally a fighter for top cover and a logistics pilot to speed
up consolidation. One person can do it across several sorties; a coordinated
group does it in one.

## The walkthrough

```steps
[
 {"title":"0. Pick the target","text":"Open the dashboard's **OBJECTIVES** page or the Live Ops map, or in the cockpit `F10 → Objectives → Enemy Status` and `Neutral Status`. You want a base that is low already (red health hex), close to a friendly base you can stage troops from, and worth taking: a logistics hub or front-line airbase shifts the war, a dead-end FARP doesn't. Neutral bases (`[CAP]` in `Neutral Status`) are the cheapest captures on the map."},
 {"title":"1. Ask the Capture Advisor","text":"`F10 → Objectives → Capture Advisor`, then the base by name. The `STATUS` lines are your to-do list: `objective health must be <=20% (now 34%)`, `clear the infantry defenders`. The `REPAIR` line tells you how long before it heals: `WILL self-repair -- next pulse ~12m; act fast or it heals back above 20%`."},
 {"title":"2. Knock it down","text":"Destroy the garrison until health is **20% or less** and **every infantry defender is dead**. CAS, JTAC-directed artillery and cruise missiles all work. Once health is down, the label adds `Infantry: 30% — clear all defenders to capture`, and when eight or fewer infantry are left your side gets `Last defenders of Kuweires: infantry here (3 left)` pins. Stay in sight of the base: while you are there it can't repair."},
 {"title":"3. Watch for the white ring","text":"When it is capturable the inner ring turns **white**, the label reads `>> UNDEFENDED - BLUE captures: land troops inside the ring`, and everyone sees `Kuweires (Red) is undefended -- Blue can capture it with troops in the zone.` Check the label names **your** side: the ring looks the same to both."},
 {"title":"4. Load capture troops elsewhere","text":"Land at a friendly base that still has logistics and is not itself capturable. `F10 → Troops → Squads → Load Standard squad`. Standard, Anti Tank and Mortar squads can capture; MANPADS (Igla, Stinger) **cannot**. More squads make the capture faster, so bring two or three if you can carry them."},
 {"title":"5. Unload inside the ring","text":"Land inside the dashed owner ring and `F10 → Troops → Unload`. Read the hint: `troops are in Kuweires's ring -- keep them alive to capture it` is what you want. `these troops are 340m outside Kuweires's ring` means move them; `Igla troops can't capture` means wrong squad."},
 {"title":"6. Hold for the timer","text":"Your side sees `Capturing Kuweires (90 sec)`, theirs `Kuweires is being captured! Eliminate enemy troops!` The timer is {{cfg:campaign_events.capture_time_secs|180}} s divided by your squads in the zone, never under 30 s. If all your squads die or leave for 10 s, it resets. Cover them: this is when the enemy comes."},
 {"title":"7. Consolidate","text":"`BASE CAPTURE: Kuweires has been taken by Blue!` The base is yours, but held only by your troops while it consolidates ({{cfg:capture_consolidation_secs|300}} s of progress). Keep them **inside the ring**: out for more than {{cfg:consolidation_zone_grace_secs|15}} s and the clock pauses. Land a logistics repair kit or supply crate there to jump it forward {{cfg:consolidation_crate_progress_secs|120}} s, or bring another squad (each adds {{cfg:consolidation_squad_bonus|0.5}} to the rate). If every holding squad dies, the base goes Neutral."},
 {"title":"8. Hold what you took","text":"`Kuweires consolidated -- garrison moving in, assault troops stood down`. You now have a light garrison, about a quarter strength and **no SAMs**. Fly in deployable crates, keep logistics flowing so it repairs, and expect a counter-attack from the air."}
]
```

The same steps in more detail follow.

## Step 0: Pick the target

On the [Live Ops dashboard](./reading-live-ops.md) or the OBJECTIVES page,
sort enemy bases by health. Click one: its drawer carries a capture report
that lists exactly what is still standing. You want a base that is:

- **Low already**, or lightly defended enough that you can grind it down.
- **Close to a friendly base** you can stage troops from. You can't load troops
  at a base that is itself capturable or has lost its logistics.
- **Worth taking**: a logistics hub or an airbase on the front line shifts the
  war; a dead-end FARP doesn't.

Neutral bases need no softening at all: health is already 0. Check
`F10 → Objectives → Neutral Status`.

## Step 1: Knock it down to 20%, zero infantry

The base becomes capturable only when **health is 20% or less AND every
infantry defender is dead.** Logistics is *not* the trigger.

- CAS with bombs, rockets, guns and guided weapons. Sweep the whole zone for
  infantry: **one surviving soldier blocks the capture.**
- JTAC-directed [artillery](../advanced/artillery.md) or
  [cruise missiles](../advanced/alcm.md) are excellent against dug-in troops.
- Expect the garrison's armour to drive out at your ground units, and a quick
  reaction force if you linger.
- Watch the health hex and the label. The **inner ring turns white** when the
  base is actually capturable.
- If the advisor says `REPAIR: WILL self-repair`, keep someone in sight of the
  base: a threatened base can't repair, and its countdown restarts every time
  an enemy is seen.

> A base bombed all the way to **health 0** goes **Neutral** by itself. You
> still need troops to take it afterwards, but so does the enemy.

## Step 2: Load capture troops from a different base

`F10 → Troops → Squads → Load Standard squad` at a friendly base that still has
logistics and isn't capturable. Squads that **can capture** on the example
configuration:

| Squad | Cost | Weight | Notes |
|---|---|---|---|
| Standard | {{cfg:troops.Blue[0].cost|0}} pts | {{cfg:troops.Blue[0].weight|800}} kg | the workhorse: bring these |
| Anti Tank | {{cfg:troops.Blue[1].cost|1}} pts | {{cfg:troops.Blue[1].weight|1000}} kg | also kills armour |
| Mortar | {{cfg:troops.Blue[2].cost|5}} pts | {{cfg:troops.Blue[2].weight|1200}} kg | indirect fire |

MANPADS (Igla / Stinger) **cannot capture.** The
[Capture Advisor](../f10-menu/objectives.md#the-capture-advisor) flags a
squad that can't with `<-- CANNOT capture (troop type)`. How many squads your
helo lifts, and the weight limits, are on
[Troop Transport](../f10-menu/troops.md).

No helo? An [AI helo mission](../advanced/helo-missions.md) can insert a squad
for you.

## Step 3: Fly in and unload inside the ring

- The zone is the dashed ring around the objective on the F10 map. The Capture
  Advisor prints its radius (`Zone radius: 2000 m`).
- Land inside it and `F10 → Troops → Unload`. Troops must be **on the ground
  and alive**; squads still in the helo don't count.
- Enemies at the target don't stop you unloading. (The only threat block on
  unloading is inside your **own** threatened base.)
- Read the hint you get back. It tells you at once whether the drop can take
  the base.

## Step 4: Hold for the timer

- {{cfg:campaign_events.capture_time_secs|180}} seconds with one squad in the
  zone, divided by the number of squads (2 squads half, 3 a third), never under
  30 s.
- Longer when the defenders have nobody online, on servers with empty-server
  protection: the message ends `(defenders offline: capture takes 2x longer)`.
- It **resets** if all your squads die or are out of the zone for 10 s, or if
  another side's capture troops come in (at a Neutral base).
- `Capturing: 60%` on the label shows progress.

## Step 5: Consolidate (don't fly away yet)

The base flips owner when the timer completes, but for
{{cfg:capture_consolidation_secs|300}} seconds of progress it is held only by
your assault troops. The F10 label counts it down: `>> CONSOLIDATING 42%
(2m54s left) - hold the zone with troops`. Nobody can slot in there until it
finishes.

**Keep the troops in the zone.** Out for more than
{{cfg:consolidation_zone_grace_secs|15}} seconds and the clock stops
(`>> CONSOLIDATION PAUSED - no troops in the zone`); progress is held, not
lost. You can't extract them until it is settled.

**Don't just orbit: shorten it.** Each extra squad in the zone adds
{{cfg:consolidation_squad_bonus|0.5}} to the rate, and a **logistics repair
kit** or **supply crate** landed at the base jumps it forward
{{cfg:consolidation_crate_progress_secs|120}} seconds. The logistics sortie is
faster than waiting.

- The enemy **cannot start a capture timer** during this window. They have to
  kill your holding troops.
- If every holding squad dies, the base drops to **Neutral**
  (`Kuweires: the assault force was wiped out -- the base is contested
  (Neutral)`) and either side can take it again after a
  {{cfg:campaign_events.capture_cooldown_secs|120}}-second cooldown.

## Step 6: Hold what you took

A freshly captured base is a **soft target**: a light garrison of AAA and
infantry, about a quarter strength, and **no SAMs**. The rest rebuilds one
group per repair pulse, as long as the base has logistics and supply.

- Deliver [crates](./running-crates.md): SAMs and armour to harden it.
- Keep [logistics](../gameplay/logistics.md) flowing so it repairs.
- Expect an air counter-attack in the first few minutes. Top cover earns its
  keep here.

## Special cases

- **Special SAM site**: no softening needed. Troops in its zone start the
  normal timer at any health, but its launchers are live while you do it, and
  only its owner sees it on the F10 map.
- **Carrier group**: sink the escort with `SUPPLY` in its name to drop its Logi
  to 0, then board it with capture troops (full-length timer, no
  consolidation), or take its linked naval base.

Both are covered in [Capturing Objectives](../gameplay/capturing-objectives.md#special-cases).

## See also

- [Objectives Menu](../f10-menu/objectives.md): the Capture Advisor
- [Running Crates & Building a Base](./running-crates.md)
- [Flying CAS With a JTAC](./cas-with-jtac.md)
- [Deployables Guide](../advanced/deployables-guide.md)
