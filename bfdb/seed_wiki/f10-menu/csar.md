# Combat Search & Rescue (CSAR)

When a pilot goes down, they don't just vanish into the scoreboard — a **downed
pilot** spawns on the ground where they came out, and somebody has to go get
them.

![Rescue helicopter hoisting a downed pilot](/api/wiki/images/f0af8d1d-8ae3-4a78-a361-27d14e55aa33)

```
F10 → CSAR
├── List Downed Pilots
├── Request Nearest Pilot Smoke
├── Pick Up Downed Pilot (manual)
└── Deliver Rescued Pilots (manual)
```

The menu appears for every slot when CSAR is enabled on the server (it is, on
the live mission). Actually *carrying* a pilot needs an airframe with pilot
slots — see the capacity table below.

## What happens when a pilot goes down

1. A downed-pilot ground unit spawns at the crash position, on the pilot's own
   coalition.
2. Every friendly helo pilot gets a notification with the location, repeated
   every **{{cfg:csar.renotify_interval|5}} minutes** while the pilot is still
   out there.
3. A **{{cfg:csar.capture_timer|30}} minute** clock starts. If nobody reaches
   them, the pilot is captured and gone.
4. Enemy ground units within **{{cfg:csar.enemy_capture_radius|50}} m** capture
   them immediately — so a pilot down on top of an enemy position is usually
   unrecoverable.

### Two things that save you a sortie

**A pilot inside a friendly objective's zone recovers themselves.** Eject over
your own airfield, or walk/drive into any friendly zone, and the base picks them
up — no helicopter, no points, message to the whole coalition. So if you are
going to lose the jet, **glide toward friendly ground first.**

**A friendly helo parked on the pilot pauses the capture clock.** The engine
treats a helo on top of them as a pickup in progress, so a recovery that runs a
few minutes long still counts. Land and stay landed.

## Getting them out

**Find them.** `List Downed Pilots` gives you every one your side knows about,
with bearing and range.

**Get eyes on.** `Request Nearest Pilot Smoke` has the nearest pilot pop smoke.
There is a **{{cfg:csar.smoke_cooldown|300}} second cooldown** per pilot, so don't burn it while you are still
20 nm out — call for it on final.

**Land.** Put down within **{{cfg:csar.pickup_radius|100}} m** of them. The pilot stands up and walks toward
your aircraft, firing a flare. You must actually be on the ground — the pickup
is refused in the air.

**They board.** Inside **{{cfg:csar.board_radius|20}} m** the pilot boards
automatically. `Pick Up Downed
Pilot (manual)` is there for when the automatic pickup doesn't trigger — same
rules, you still have to be landed and within pickup range.

**Fly them home.** Land inside the zone of any **friendly objective**. Delivery
is automatic; `Deliver Rescued Pilots (manual)` forces it. Again — you have to
be on the ground.

## What you get

**+{{cfg:csar.rescue_reward|50}} points** to the rescuing pilot, per pilot
delivered.

If the server is enforcing lives, delivery also **gives the rescued pilot their
life back** — cascading down a tier if they are already topped up at the tier
they lost. Lives are not enforced on the live mission right now, so today the
reward is the points and the fact that a teammate keeps flying. See
[Points and Lives](../gameplay/points-and-lives.md).

## Who can carry pilots

| Airframe | Pilots |
| --- | --- |
| CH-47F Chinook | 4 |
| Mi-8MTV2 | 3 |
| UH-1H Huey | 2 |
| Mi-24P | 1 |
| SA342L / SA342 Minigun | 1 |
| C-130J-30 | — (troops and crates only) |

Pilots share the airframe's total cargo capacity with crates and troops. A
Huey already carrying crates may not have room for two pilots.

## Practical notes

- **CSAR is a helo job, and it is the best-paying thing a helo can do between
  crate runs.** Keep the CSAR menu open when you are transiting.
- **Post a CSAR task.** `Actions → Add Task → CSAR` at a map mark puts a pin and
  a circle on the F10 map for your whole coalition. See
  [The Tasking Board](../gameplay/tasking-board.md).
- **Don't go alone into a hot area.** A downed pilot is usually down *because*
  something shot them down, and that something is still there. Bring a shooter,
  or clear it first.
- **The capture clock is short.** A pickup 60 nm behind the line in an Mi-8 is
  roughly 20 minutes each way. Decide fast.
- **If you ejected, say where.** The notification goes to helo pilots, but a
  callout on comms with a bullseye reference gets somebody moving quicker.

## See Also

- [Cargo Operations](./cargo.md) — the rest of what a transport helo does
- [The Tasking Board](../gameplay/tasking-board.md) — posting a CSAR task
- [Comms Plan](../gameplay/comms-plan.md) — the helo/ground net frequencies
- [Points and Lives](../gameplay/points-and-lives.md)
