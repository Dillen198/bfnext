# Playbook: Flying CAS With a JTAC

A JTAC (Joint Terminal Attack Controller) is a ground unit, drone, or player
that finds enemy targets, tracks them, and lasers them for your weapons. Working
a JTAC is the fastest way to be useful in a strike jet and it's how you break an
enemy ground push. Full system reference: [JTAC System](../f10-menu/jtac.md).

## What you need

- A slot that can carry air-to-ground ordnance with a laser or GPS capability —
  LGBs, laser Mavericks, rockets, or just guns and dumb bombs in a pinch.
- A JTAC in the area. If none is listed, someone has to deploy one first:
  `F10 → Actions → Recon Drone` (Small, 50 pts, 12 km / Large MQ-9, 100 pts,
  18 km — neither needs line of sight), or a ground
  [JTAC/recon vehicle crate](../advanced/deployables-guide.md).

## Step 1 — Check the JTAC before you're in the threat ring

`F10 → JTAC → [JTAC ID] → Status`. Read back:

```
lasing T-72B code 1688 marker M123
position bearing 045 for 5.2km from Batumi
Visual On: T-72Bx3, BMP-3x2, SA-13
```

That tells you: **what** it's on, the **laser code** (1688 here), **where** it
is relative to a known objective, and **what else** is down there — note the
SA-13, that's a threat to you.

## Step 2 — Match the laser code

Set your aircraft's laser code to the JTAC's code (1688 by default).

- If you can't change your jet's code, change the JTAC's instead:
  `F10 → JTAC → [ID] → Code → [hundreds / tens / ones]`.
- Multiple aircraft can ride the same code.

## Step 3 — Copy the 9-line and run in

Requesting support passes a 9-line brief on the tactical freq. The ones that
matter in the cockpit:

- **Line 6 — target location** (get eyes/TGP on it)
- **Line 7 — mark type** (laser / smoke / IR)
- **Line 8 — friendlies** — where your own troops are relative to the target
- **Line 9 — egress** — which way to leave

Ask for **smoke** (`F10 → JTAC → [ID] → Smoke Target`) or the **IR pointer**
(night) if you can't find it visually.

## Step 4 — Attack the lased target

- Release inside your weapon's parameters; the JTAC holds the laser.
- Don't overfly the target into the SA-13 you noted in step 1 — respect line 9.
- **Deconflict:** never drop without positive JTAC clearance. Friendlies may be
  in the target area and **friendly fire carries a life penalty.**

## Step 5 — BDA and shift

Report battle damage after each pass. Then:

- `F10 → JTAC → [ID] → Shift Target` to move the laser to the next contact, or
- `F10 → JTAC → [ID] → Toggle Auto-Shift` to have it cycle automatically, or
- `F10 → JTAC → [ID] → Filter → [Tank / SAM / APC / …]` to make it prioritize
  the target type you're loaded for.

## Escalate: artillery and cruise missiles

If the JTAC has friendly artillery or an ALCM platform in range (shown in its
Status), you can task them on the same target:

- `F10 → JTAC → [ID] → Artillery → [battery] → [rounds]` — then **Adjust Fire**
  short/long/left/right to walk rounds on.
- `F10 → JTAC → [ID] → ALCM → [platform] → [settings]` for standoff strikes on
  hardened targets.

See [Artillery Missions](../advanced/artillery.md) and
[ALCM](../advanced/alcm.md).

## Bonus: JTAC targets feed the map

Anything a JTAC has eyes-on is automatically pushed to your coalition's F10 map
and stays marked for about an hour after the JTAC loses it — so a JTAC is also a
recon asset for the whole team, no extra steps.

## See also

- [JTAC System](../f10-menu/jtac.md) — full menu reference
- [Your First Sortie](./first-sortie.md)
- [Capturing a Base](./capturing-a-base.md)
