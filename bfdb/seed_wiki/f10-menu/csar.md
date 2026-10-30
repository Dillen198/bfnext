# Combat Search & Rescue (CSAR)

When a pilot ejects, they don't just vanish into the scoreboard: a **downed
pilot** lands where they came down, and somebody has to go and get them.

![Rescue helicopter hoisting a downed pilot](/api/wiki/images/f0af8d1d-8ae3-4a78-a361-27d14e55aa33)

## Where the menu is

The CSAR menu appears for every slot when CSAR is enabled on the server. In a
slot that has a **Cargo** or **C-130 Cargo** menu it sits inside it (DCS only
shows ten top-level menus, and this keeps **Info** on screen); other slots
have it at the top level.

```f10menu
{"root":"F10 Other","items":[{"label":"Cargo","items":[
 {"label":"Unpack Nearby Crate(s)","note":"See Cargo Operations."},
 {"label":"List Nearby Crates"},
 {"label":"List Cargo"},
 {"label":"Destroy Nearby Crate"},
 {"label":"Crates","items":[{"label":"Base Supply","note":"See Cargo Operations."}]},
 {"label":"CSAR","items":[
  {"label":"List Downed Pilots","says":"Downed Pilots\n----------------------------\nCobra 2-1 | Mike: 47° / 8350m\nHawg 1-2 | Sam: 112° / 21900m","note":"Every downed pilot on your side, with bearing and distance from you. \"No downed pilots on your side\" when there are none."},
  {"label":"Request Nearest Pilot Smoke","says":"Cobra 2-1 | Mike popped green smoke — 1240m away","note":"The nearest one only. Each pilot can smoke once per cooldown: \"Cobra 2-1 | Mike smoke on cooldown — 212s remaining\". \"No downed pilots nearby\" when there are none."},
  {"label":"Pick Up Downed Pilot (manual)","says":"Dillen picked up downed pilot Cobra 2-1 | Mike","note":"Shown to your side. For when the automatic pickup doesn't happen."},
  {"label":"Deliver Rescued Pilots (manual)","says":"Dillen delivered 1 rescued pilot to safety","note":"Shown to your side. For when the automatic delivery doesn't happen."}]}]}]}
```

## What happens when a pilot ejects

1. A downed pilot spawns where the pilot came down, on their own side. Only an
   **ejection** creates one: a pilot killed in the cockpit leaves nobody to
   rescue.
2. Every friendly pilot in an airframe that can carry downed pilots gets told
   where, with bearing and distance from their own aircraft:

```dcsmessage
CSAR: Cobra 2-1 | Mike is down — bearing 47°, 8350m
```

   The reminder repeats every **{{cfg:csar.renotify_interval|5}} minutes**
   while the pilot is still out there:
   `CSAR reminder: Cobra 2-1 | Mike still down — bearing 47°, 8350m`.
3. A **{{cfg:csar.capture_timer|15}} minute** clock starts. If nobody gets
   there, the pilot is captured.
4. Any live enemy unit within **{{cfg:csar.enemy_capture_radius|50}} m**
   captures them at once. A pilot who comes down on an enemy position is
   usually lost.
5. If the server runs search parties and an enemy base is close enough, the
   enemy sends ground units out of it, by road where there is one, to reach
   the pilot first. It is a race: get there first, or stop them on the way.

When the clock runs out your side sees
`CSAR: Cobra 2-1 | Mike was captured — rescue window expired`, and an enemy
capture reads `CSAR: Cobra 2-1 | Mike was captured by enemy forces!`.

### Two things that save you a sortie

**A pilot inside a friendly objective's zone recovers themselves.** Eject over
your own airfield, or get walked or driven into any friendly zone, and the
base picks them up. No helicopter, no points:

```dcsmessage
CSAR: Cobra 2-1 | Mike made it to Ramat David — recovered
```

So if you are going to lose the jet, **glide toward friendly ground first.**

**A friendly helicopter near the pilot holds the capture clock.** While a
CSAR-capable helicopter of your side is within
{{cfg:csar.pickup_radius|100}} m of the pilot, the timer can't capture them,
so a pickup that runs late still counts. Land and stay landed.

## Getting them out

**Find them.** **List Downed Pilots** gives every one your side knows about,
with bearing and range from you.

**Get eyes on.** **Request Nearest Pilot Smoke** puts green smoke on the
nearest one. The same pilot can smoke again only after
**{{cfg:csar.smoke_cooldown|300}} seconds**, so call for it on final, not
20 nm out.

**Land close.** When a friendly CSAR-capable helicopter comes within
**{{cfg:csar.pickup_radius|100}} m**, the pilot fires a green flare (once). If
you are **on the ground** with room aboard, they then walk to your aircraft.

**They board.** Inside **{{cfg:csar.board_radius|20}} m** the pilot climbs in
on their own:

```dcsmessage
Dillen picked up downed pilot Cobra 2-1 | Mike — auto-boarding!
```

**Pick Up Downed Pilot (manual)** is there for when that doesn't happen. You
must be landed, and within {{cfg:crate_load_distance|50}} m of the pilot (the
cargo load distance, which is closer than the walk-in radius). Failures:
`you must land to pick up a downed pilot`, `no downed pilots in range`,
`you already have a full load onboard`, and in an airframe with no pilot
seats, `this aircraft cannot carry downed pilots`.

**Fly them home.** Land at **any friendly objective**. Delivery is automatic:

```dcsmessage
Dillen auto-delivered 1 rescued pilot at base
```

**Deliver Rescued Pilots (manual)** forces it. You must be on the ground
inside a friendly objective's zone
(`you must be at a friendly objective to deliver pilots`).

## What you get

**+{{cfg:csar.rescue_reward|0}} points** to the rescuer for each pilot
delivered (0 means the server pays nothing for rescues). Flying your **own**
ejected pilot home earns no reward: getting the life back is the payoff.

If the server limits lives, delivery also **gives the rescued pilot a life
back**, and they are told:

```dcsmessage
your pilot was rescued by Dillen and delivered — you have 3 Standard lives
```

A pilot who is captured gets `Your downed pilot was captured — no Standard
life restored`. See [Points and Lives](../gameplay/points-and-lives.md).

## Who can carry pilots

Pilot seats are set per airframe by the server, and they share the airframe's
total with crates and troops; the full table is on
[Cargo Operations](./cargo.md#what-each-airframe-carries). **List Cargo**
shows a `pilots: 0 of N` line when your airframe has seats. Only those
airframes get the CSAR alerts, the flare and the automatic pickup.

## Practical notes

- **CSAR is a helicopter job, and one of the most useful things a helicopter
  can do between crate runs.** Keep an eye on the alerts while you transit.
- **Post a CSAR task.** **Actions → Add Task → CSAR** at a map mark puts a pin
  and a circle on the F10 map for your whole side. See
  [The Tasking Board](../gameplay/tasking-board.md).
- **Don't go alone into a hot area.** A pilot is usually down *because*
  something shot them down, and it is still there. Bring a shooter, or clear
  it first.
- **The clock is short.** Check the bearing and range against your speed
  before you commit.
- **If you ejected, say where.** A call on comms with a bullseye reference gets
  somebody moving faster than the alert alone.

## See Also

- [Cargo Operations](./cargo.md) - the rest of what a transport helicopter does
- [The Tasking Board](../gameplay/tasking-board.md) - posting a CSAR task
- [Comms Plan](../gameplay/comms-plan.md) - the helicopter and ground net frequencies
- [Points and Lives](../gameplay/points-and-lives.md)
