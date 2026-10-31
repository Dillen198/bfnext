# Playbook: Flying CAS With a JTAC

A JTAC is a ground unit, drone or player that finds enemy targets, tracks them
and holds a laser on one for your weapons. Working a JTAC is the fastest way to
be useful in a strike jet, and it is how you break an enemy ground push. Full
reference: [JTAC System](../f10-menu/jtac.md).

## What you need

- A slot that carries air-to-ground weapons. Laser-guided bombs and laser
  Mavericks use the JTAC's spot; rockets, guns and dumb bombs still benefit from
  its smoke and coordinates.
- A JTAC near the fight. If your side has none there, someone deploys one: a
  drone from the [Actions menu](../f10-menu/actions.md), or a JTAC vehicle from
  [crates](../advanced/deployables-guide.md).

## The run, step by step

```steps
[
 {"title":"1. Find your JTAC","text":"Press **F10 → `JTAC>>`**. F10 closes; re-open it and open **`JTAC`**. The list shows *places*, e.g. `Batumi (2 JTAC)>>`. Click the place nearest your target, re-open F10 again and go to **`JTAC → Batumi`**. Each JTAC there ends with its laser code, e.g. `Reaper 1234 [1688]`."},
 {"title":"2. Read its Status before the threat ring","text":"**`<JTAC> → Status`**. Line 2 is the target and laser code, with its bearing and distance from the JTAC. `In laser range` is what it can lase; `Seen, out of laser range` is what else is out there. Look for SAMs and AAA in either list: they are threats to you. Status also drops a pin on the target for your flight."},
 {"title":"3. Match the laser code","text":"Set your weapon's laser code to the JTAC's (the number in brackets). Each JTAC on your side has its own code, so check this one. If your jet cannot change code, change the JTAC's under **`Code`** one digit at a time: `Xxxx`, `xXxx`, `xxXx`, `xxxX`. Valid codes are 1111 to 1788."},
 {"title":"4. Copy the 9-line","text":"**`<JTAC> → 9-Line`**. The lines that matter in the cockpit: **1-3** run-in from the IP (heading and distance from the objective), **6** target position in lat/long and MGRS, **7** the laser code (and IR pointer if on), **8** where the JTAC is from the target, **9** the egress heading."},
 {"title":"5. Get eyes on","text":"Can't find it? **`Smoke Current Target`** puts smoke on it (red from a Blue JTAC, blue from a Red one; once a minute). At night use **`More >> → Toggle IR Pointer`**. Want a different group of targets? Place an F10 map mark on them and choose **`Focus on My Latest Mark`**: the JTAC lases only within 5 km of your mark."},
 {"title":"6. Attack","text":"Release inside your weapon's parameters; the JTAC holds the spot. Respect the egress in line 9 and the threats you noted in step 2. Friendly ground units may be close, and friendly fire costs you."},
 {"title":"7. Next target","text":"When the target dies, players following the JTAC see `JTAC Reaper (1234) [1688]: target T-72B destroyed` and, in auto mode, it lases the next one by itself. **`Shift`** moves the laser now (and switches to manual; **`More >> → Toggle Auto Shift`** goes back to auto). **`Filter → SAM`** (or `Armor`, `APC`...) keeps it on the type you are loaded for."},
 {"title":"8. Escalate","text":"If the JTAC's **`Artillery`** or **`ALCM`** submenu lists shooters, put them on the same target: a gun's `Fire For Effect`, `Fire All Groups Together`, or an ALCM `Fire Quarter → Fire One per target`. See the Artillery and ALCM pages for detail."}
]
```

If the JTAC is not lasing, its Status says why on the line under the header. The
common one on drones is range: a drone sees far but lases only out to 18.5 km.
Move it closer with the drone waypoint action (`Actions` menu, or
`-jtac <id> move <mark text>` in chat).

## Pin your JTAC

**`More >> → Pin`** puts the JTAC at the top of your JTAC list with its full
menu, so you skip the place list in step 1 next time, and you hear its callouts:

```dcsmessage
JTAC Reaper (1234) [1688] near Batumi: lasing BMP-3
```

## Working from chat

Every step has a chat command, handy when you are busy in the cockpit:
`-jtac list` for the ids, then `-jtac 1234 status`, `-jtac 1234 9line`,
`-jtac 1234 smoke`, `-jtac 1234 shift`, `-jtac 1234 code 1511`. `-jtac help`
lists the rest.

## Bonus: JTAC targets feed the map

On servers that run the intel picture, everything a JTAC can see is marked on
your coalition's F10 map as `[INTEL/RED]` markers, and they stay for hours after
the JTAC loses sight. A JTAC is a recon asset for the whole team.

## See also

- [JTAC System](../f10-menu/jtac.md) - full menu reference
- [Artillery Operations](../advanced/artillery.md)
- [ALCM Operations](../advanced/alcm.md)
- [Your First Sortie](./first-sortie.md)
- [Capturing a Base](./capturing-a-base.md)
