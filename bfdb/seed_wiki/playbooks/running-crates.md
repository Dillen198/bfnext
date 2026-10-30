# Playbook: Running Crates & Building a Base

Logistics wins campaigns. Every SAM, tank, gun, radar and JTAC vehicle on the
map got there because someone flew the crates out, and every base that keeps
repairing does so because someone kept it fed. It is also one of the easiest
ways for a new player to earn points and matter. Reference pages:
[Cargo Operations](../f10-menu/cargo.md),
[Deployables Guide](../advanced/deployables-guide.md),
[Deployable Units Reference](../reference/deployables.md).

## Three different jobs

| Job | What it does |
|---|---|
| **Build a deployable** | Fly every crate a unit needs to a spot between or ahead of your bases, then unpack them into the unit. |
| **Fix a base** | Fly a logistics repair kit (logistics up one step) or an emergency repair crate (one destroyed group rebuilt) to a damaged base. |
| **Feed a base** | Fly a fuel or weapons transfer crate from a well-stocked base to a hungry one: a cut-off base, or one you have just captured. |

## What can carry crates

Any airframe with a **Cargo** or **C-130 Cargo** menu. Slot counts per airframe
are on [Cargo Operations](../f10-menu/cargo.md#what-each-airframe-carries);
remember that crates, troops and rescued pilots share one total. The C-130
carries the most and can drop without landing: see
[C-130 & Airdrop](../advanced/c130-airdrop.md).

## Building a deployable, step by step

The walkthrough below uses the classic F10 menu. On the helicopter dynamic
menu you load and set crates down with **F8 → Ground Crew → Cargo** instead of
**Load Nearby Crate** and **Unload Crate**; everything else is the same.

```steps
[
 {"title":"1. Pick what and where","text":"Decide what the position needs: a radar SAM to cover a gap, an EWR on an approach lane, a JTAC vehicle to open up CAS, armour for an assault. See the [Deployables Guide](../advanced/deployables-guide.md).\n\nThen pick the spot. It must be **at least {{cfg:logistics_exclusion|10000}} m from the centre of every friendly objective**. Between two of your bases, a road junction, or right next to the enemy base you are attacking are all good."},
 {"title":"2. Check the bill","text":"The [Deployable Units Reference](../reference/deployables.md) lists the crates, weights, price and limit for each unit on this server. The price is also in the menu label, e.g. `Hawk System(35 pts)`. You pay at unpack, from your points first and then from the fund of the base the crates came from."},
 {"title":"3. Spawn the crates","text":"Land inside the zone of a friendly base with working logistics. **F10 → Cargo → Crates → category → unit → crate**. A crate marked `(2)` is needed twice.\n\nThe classic menu answers `2 of 4 crates spawned, ...`: you may have only {{cfg:max_crates|4}} spawned crates out at once, and one more deletes your oldest."},
 {"title":"4. Load","text":"**F10 → Cargo → Load Nearby Crate** within {{cfg:crate_load_distance|50}} m of the crate. The answer tells you how many of the unit your side already has and what happens at the limit:\n\n`Hawk Launcher crate loaded`\n`1 of 4 Hawk System deployed, unpacking will delete oldest, 4471 when the limit is exceeded`"},
 {"title":"5. Fly and set down","text":"Fly to the site. Land and use **Unload Crate**, or drop from a slow, low hover (each crate has its own height and speed limit, and the refusal tells you both). Repeat until every crate is on the ground, all within {{cfg:crate_spread|250}} m of each other."},
 {"title":"6. Unpack","text":"Land next to the crates and press **F10 → Cargo → Unpack Nearby Crate(s)**. Your side sees `KillerDog unpacked a Hawk System` and the unit appears at the crates, clear of your aircraft.\n\nIf it fails, the message says why: a missing crate, `too close to friendly objective`, not enough points, or the unit limit."},
 {"title":"7. Reposition if you want to","text":"**F10 → Actions → Move (Units/Troops)**, pick the group, then a map mark. Your own groups are at the top, marked `*`, and the cost is in the label. See [Actions](../f10-menu/actions.md)."}
]
```

## Fixing and feeding bases

1. **Find the base that needs it.** A base that won't repair is starving; a
   red logi hex means its infrastructure is flattened. Check
   [Live Ops → OBJECTIVES](./reading-live-ops.md) or the F10 map.
2. **Spawn the crate at a different friendly base.** **Cargo → Crates → Base
   Supply** has the repair kit, the transfer crates and (where enabled) the
   emergency repair crate. Supply crates from the F10 menu do nothing at the
   base they were loaded at.
3. **Deliver it.**
   - Classic menu: land **inside the target base's zone**, unload and unpack.
     `KillerDog repaired logistics at Megiddo to %80` or
     `KillerDog transfered supplies from Ramat David to Megiddo`.
   - Dynamic or C-130 cargo: set it down or drop it on the base. Transfer
     crates and repair kits go to the **nearest** friendly objective, so put
     them right on top of the one you mean. An emergency repair crate has to
     land inside the zone.
4. **Get paid.** A repair kit is worth {{cfg:points.logistics_repair|25}}
   points and a transfer {{cfg:points.logistics_transfer|15}}, more for a long
   haul or a front-line base.

The emergency repair crate is picky about when it works (not under attack, no
capture running, not on its cooldown). If it is refused, nothing is spent and
the message says why: see
[Emergency repair crates](../gameplay/logistics.md#emergency-repair-crates).

## Where to build

- **Ahead of a fresh capture.** A captured base has a skeleton garrison and no
  SAMs of yours, and you can't build inside its exclusion ring; cover the
  approaches to it instead, and fly supply transfer crates into it.
- **Around the enemy base you are attacking.** The exclusion rule only counts
  your own bases. Armour and AAA next to the target support the troops that
  will take it.
- **Approach lanes.** An EWR well forward gives your side early warning.
- **Gaps between your bases.** A radar SAM backed by AAA covers ground no
  garrison does.

## Common mistakes

- **Building too close to your own base.** The unpack is refused with the
  distance you still need to move.
- **Spreading the crates out.** They have to be within
  {{cfg:crate_spread|250}} m of each other.
- **Forgetting the count.** Two crates of three do nothing; the failure lists
  what is missing.
- **Spawning at a capturable base.** A base knocked down to capturable, or at
  0% logistics, issues nothing; stage from further back.
- **Delivering a supply crate to its own base.** Fly it somewhere else.
- **Building a radar SAM with no AAA backup.** One SEAD flight and it's gone.

## See also

- [Cargo Operations](../f10-menu/cargo.md)
- [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md)
- [Logistics & Supply](../gameplay/logistics.md)
- [Capturing a Base](./capturing-a-base.md)
