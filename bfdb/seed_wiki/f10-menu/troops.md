# Troop Transport

Infantry is what captures a base. Aircraft and artillery can strip an
objective down, but only a squad standing inside its ring takes it. Troops
are also how you hold a base you have just captured while it consolidates.

![Infantry squad loaded into a transport helicopter for an objective capture run](/api/wiki/images/a645d067-42f4-4059-a31a-e2e5e378cfa0)

## Who gets the Troops menu

Any airframe the server gives troop slots, as long as the server has squads
configured for your side. The slot counts per airframe are in the table on
[Cargo Operations](./cargo.md#what-each-airframe-carries). Troops, crates and
rescued pilots share one **total**, so check **List** before you plan a mixed
load.

## The Troops menu

```f10menu
{"root":"F10 Other","items":[{"label":"Troops","items":[
 {"label":"Unload","says":"KillerDog dropped Standard troops into the field","note":"Shown to your side. Straight after, only you get a hint about the nearest enemy or neutral objective, e.g. \"troops are in Megiddo's ring -- keep them alive to capture it\"."},
 {"label":"Extract","says":"KillerDog extracted Standard troops from the field (5 of 8 left)","note":"Picks up a friendly squad within the load distance. The (alive of total) part appears only when it has taken casualties."},
 {"label":"List","says":"Current Cargo\n----------------------------\ntroops: 1 of 1\ncrates: 0 of 1\npilots: 0 of 2\ntotal : 1 of 2\n----------------------------\nStandard troop weighing 800 kg\n----------------------------\ntotal cargo weight: 800 kg"},
 {"label":"Return","says":"KillerDog returned Standard troops","note":"Hands the last squad back to the base and refunds it."},
 {"label":"Squads","items":[
  {"label":"Load Standard squad","says":"KillerDog loaded Standard\n3 of 10 Standard deployed, unloading will delete oldest, 5521 when the limit is exceeded","note":"Free squads have no (pts) suffix."},
  {"label":"Load Anti Tank squad (1 pts)","says":"KillerDog loaded Anti Tank\n0 of 10 Anti Tank deployed, unloading will delete oldest when the limit is exceeded"},
  {"label":"Load Mortar squad (5 pts)","says":"KillerDog loaded Mortar\n1 of 10 Mortar deployed, unloading will delete oldest, 5530 when the limit is exceeded"},
  {"label":"Load Stinger squad (5 pts)","says":"KillerDog loaded Stinger\n2 of 10 Stinger deployed, unloading will delete oldest, 5502 when the limit is exceeded","note":"MANPAD teams can't capture."}]}]}]}
```

The squad names, prices and limits come from the server's config; the list
above is an example. **Squads** pages with a `More >>` entry if a side has
more than nine squad types. This server's squads, with their weights and
whether they can capture, are on the
[Deployable Units Reference](../reference/deployables.md#troop-types).

## Loading troops

1. Sit inside the zone of a **friendly objective** that still has logistics
   (above 0%) and is **not capturable**. A base bombed down to capturable
   can't send its own troops out.
2. **F10 → Troops → Squads → Load ... squad**.
3. The squad is paid for now: from your points first, then from the base's
   fund. The base also needs materiel to issue it, where the server runs the
   materiel economy.

The confirmation goes to your whole side and tells you how many of that squad
type your side already has out, and what happens at the limit: either the
oldest squad is deleted when you unload, or the unload is refused.

| Failure | Why |
| --- | --- |
| `not near friendly logistics (or this base is too damaged to supply from)` | Not inside a working friendly base. |
| `there are 3 points available, this troop costs 5 points` | You and the base together can't pay. |
| `you already have a full load onboard` | No troop slot, or the shared total is used up. |

The squad's weight is added to your aircraft as internal cargo, so it changes
how your helicopter flies. A heavy squad in a small helicopter on a hot day
may be more than you can hover with.

## Unloading troops

Land, then **F10 → Troops → Unload**. The last squad you loaded gets out next
to your aircraft. You have to be on the ground: `you must land to unload troops`.

The squad then tells you whether it can do anything where you put it:

```dcsmessage
these troops are 340m outside Megiddo's ring -- they only capture from inside it
```

The other hints are:

- `troops are in Megiddo's ring -- keep them alive to capture it`
- `troops are in Megiddo's ring, but it isn't capturable yet -- see Objectives > Capture Advisor`
- `Stinger troops can't capture -- bring a squad that can to take Megiddo`

No hint means there is no enemy or neutral objective within about 5 km.

**Refusals**

- `you can't deploy troops here while enemies are near`: you are inside one of
  your **own** bases and it is threatened. Wait for the threat to clear
  ({{cfg:threatened_cooldown|300}} s after the last enemy is out of sight), or
  unload outside it. This check applies only inside your own base, not at the
  enemy objective you are attacking.
- `the maximum number of Mortar troops are already deployed`: that squad type
  refuses at the limit instead of replacing the oldest.

## Extract and Return

**Extract** picks up a friendly squad of your side that is within
{{cfg:crate_load_distance|50}} m of you, whoever dropped it. A squad that has
lost men comes aboard at its reduced strength and stays that way when you drop
it again. Squads holding a base that is still consolidating after its capture
stay put:

```dcsmessage
those troops are holding Megiddo while it consolidates -- they can't be extracted until the capture is settled
```

They are not lost: once the base consolidates your squads are released where
they stand and can be extracted again.

Otherwise the failure is `no troops in range`.

**Return** hands the last squad aboard back to a friendly base and refunds
what it is still worth (a squad at half strength refunds half its price, and
a squad nobody paid for refunds nothing). You must be landed inside a working
friendly base: `you are not close enough to friendly logistics to return troops`.

If you leave your slot or the server while parked at a working friendly base,
the squads aboard are returned and refunded for you. Squads lost with an
aircraft that crashes or is shot down are gone.

## Troops in IFVs, APCs and trucks

Every BTR, BMP, Bradley, Stryker and troop truck the server lists as carrying
infantry has **one squad aboard**, for as long as the vehicle lives. You can
put it down:

- **Driving it yourself** (Combined Arms): the vehicle gets an **F10 → Troops**
  menu with **Dismount squad**, **Embark nearby squad** and **What is aboard**.
  The same works from chat: `-dismount`, `-embark`.
- **One you deployed** (AI driven): `-dismount <groupid>` and
  `-embark <groupid>` in chat. `-carriers` lists your vehicles that carry a
  squad and what each has aboard.

The vehicle has to be **stopped**. The squad gets out behind it and is yours:
it counts against the squad limits and captures if its type can.

One squad per vehicle, never more:

- To put it down again, **embark it first**: drive within 60 m of it, stop,
  and use Embark. It comes back aboard at the strength it has left.
- If the squad is **killed**, deleted or lifted out by a helicopter, the
  vehicle has **nobody left** and can't deploy troops again.
- A squad that took a base can't embark while the base consolidates. Once it
  has, the squad is released where it stands and can embark again.
- A vehicle that is **destroyed** lets out whoever is aboard, and nobody if
  its squad is already out or lost.
- An AI formation that sends a vehicle's squad into a base uses it up the
  same way.

```dcsmessage
a Standard squad is out of the BTR-82A as group 4120 -- inside an enemy base's ring they capture it
the BTR-82A's squad is already out as group 4120 -- embark it before it can deploy again
the BTR-82A's squad was lost -- it has no troops to deploy
```

## Capturing with troops

The squad has to be **inside the ring** of an objective that is
**capturable**, and it has to stay alive until the capture timer finishes.
After the capture the squads in the zone hold the base while it consolidates.
More squads go faster. The details, including what makes a base capturable,
are on [Capturing Objectives](../gameplay/capturing-objectives.md) and in the
[Capturing a Base](../playbooks/capturing-a-base.md) playbook.

## After the drop

- Your squads show up in **F10 → Actions → Move (Units/Troops)**, marked with
  a `*`, if you want to walk them somewhere. See [Actions](./actions.md).
- Loading or unloading a squad also pins it, and the objective it is near, in
  your [JTAC](./jtac.md) menu.
- `-delete <groupid>` in chat removes a group you deployed for a partial
  refund. See [Chat Commands](../reference/chat-commands.md).

## See Also

- [Deployable Units Reference](../reference/deployables.md) - squad types and costs
- [Capturing Objectives](../gameplay/capturing-objectives.md)
- [Cargo Operations](./cargo.md)
- [Combat Search & Rescue](./csar.md)
